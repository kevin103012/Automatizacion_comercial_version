;; ============================================================================
;; GM_MODELO.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


;; E10: *UBI_ACTIVE_VP* se deriva del ML_VP_NUM persistente del viewport detectado (1-5); por defecto 1.
(defun GM:ActivateTemplatePaper ()
  (if (not (and *GM_AUTO_LAYOUT_TAB* (member *GM_AUTO_LAYOUT_TAB* (layoutlist))))
    (GM:LayoutFail "No hay Layout plantilla valido memorizado"))
  (UBI_RestoreSpace *GM_AUTO_LAYOUT_TAB* 1)
  T)

;; nil es normal en algunas rutinas manuales (princ); solo las rutinas con
;; contrato booleano requieren T. Toda excepcion bloquea la etapa siguiente.
(defun GM:AutoStage (code stage fn args require_true / stage_result stage_error previous_message)
  (setq stage_error *error* previous_message *GM_AUTO_LAST_MESSAGE*)
  (setq stage_result (vl-catch-all-apply fn args) *error* stage_error)
  (if (and (vl-catch-all-error-p stage_result)
           (wcmatch (strcase (vl-catch-all-error-message stage_result)) "*CANCEL*,*BREAK*"))
    (setq *GM_AUTO_CANCELLED* T))
  (if (or (vl-catch-all-error-p stage_result) (and require_true (not (eq stage_result T))))
    (progn
      (if (= *GM_AUTO_LAST_MESSAGE* previous_message)
        (setq *GM_AUTO_LAST_MESSAGE* (strcat stage ": "
          (if (vl-catch-all-error-p stage_result) (vl-catch-all-error-message stage_result)
            "La rutina no confirmo exito"))))
      (GM:AutoTrace code stage "ERROR" *GM_AUTO_LAST_MESSAGE*)
      nil)
    (progn (GM:AutoTrace code stage "OK" "Etapa completada") T)))

;; Purga unica al inicio del lote: elimina la grilla estatica/antigua (GRILLA y
;; GRILLA-VPn) para que no interfiera con la generada por cada VP. No se repite
;; por ciclo porque cada VP genera su propia grilla. Colores 7/8 intactos.
(defun GM:AutoPurgeStaticGrid (/ layers lay ss i e)
  (setq layers '("GRILLA" "GRILLA-VP1" "GRILLA-VP2" "GRILLA-VP3" "GRILLA-VP4" "GRILLA-VP5"))
  (foreach lay layers
    (if (tblsearch "LAYER" lay)
      (progn
        ;; COMMAND no se puede pasar a apply: se invoca directo dentro de lambda.
        (vl-catch-all-apply '(lambda (l) (command "_.-LAYER" "_T" l "_ON" l "_U" l "")) (list lay))
        (if (setq ss (ssget "X" (list (cons 8 lay))))
          (progn
            (setq i 0)
            (while (< i (sslength ss))
              (setq e (ssname ss i))
              (if (entget e) (vl-catch-all-apply 'entdel (list e)))
              (setq i (1+ i))
            )
          )
        )
      )
    )
  )
  (GM:AutoTrace "LOTE" "GRILLA_BASE" "OK" "Grilla estatica purgada una vez al inicio")
  (princ)
)

;; Rev AH: auditoria PE/UC preservados in-place (conteo visible, sin borrados).
(defun GM:TracePEUC (code / ss i ent obj hl n_vis item typ ed)
  (setq n_vis 0)
  ;; OPT6 2026-09-27 (F2): con indice de lote solo se revisan textos del indice;
  ;; sin indice se conserva el barrido COM original.
  (if (and *UBI_ENT_INDEX_READY* *UBI_ENT_INDEX*)
    (foreach item *UBI_ENT_INDEX*
      (setq ent (car item) hl (cadr item) typ (cadddr item) ed nil)
      (if (and hl (/= hl "") (or (= typ "AcDbText") (= typ "AcDbMText"))
               (vl-string-search "ETIQUETAS PE UC" (strcase hl)))
        ;; P0-6 2026-09-29: una sola lectura DXF por entidad (antes dos entget).
        (progn
          (setq ed (entget ent))
          (if (and ed (/= (cdr (assoc 60 ed)) 1))
            (setq n_vis (1+ n_vis))))))
    (progn
  (setq ss (ssget "X" (list (cons 0 "TEXT,MTEXT") (cons 410 "Model"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        ;; FIX-AV: un solo wrapper por iteracion, liberado antes de seguir.
        (setq ent (ssname ss i) hl nil obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (vl-catch-all-error-p obj)
          (setq obj nil)
          (setq hl (UBI_GetHyperlinkString obj)))
        (GM:ReleaseObjectSafe obj)
        (setq obj nil)
        (if (and hl (vl-string-search "ETIQUETAS PE UC" (strcase hl)))
          ;; P0-6 2026-09-29: una sola lectura DXF por entidad.
          (progn
            (setq ed (entget ent))
            (if (and ed (/= (cdr (assoc 60 ed)) 1))
              (setq n_vis (1+ n_vis)))))
        (setq i (1+ i)))))
    )
  )
  (GM:AutoTrace code "PEUC" (if (> n_vis 0) "OK" "AVISO")
    (strcat "N=" (itoa n_vis) " PE/UC Model no ocultos por entidad (DXF 60); no certifica visibilidad en viewport; Rev AI"))
  n_vis
)

(defun GM:AutoFinishModel (code vp / grid_result fmt_ok audit_result t0)
  (setq t0 (GM:PerfMs))
  (setq grid_result (vl-catch-all-apply 'UBI_GenerateViewportGrid (list vp *GM_AUTO_SCALE_STR*)))
  (GM:PerfMark code "GRILLA" t0)
  (if (not (eq grid_result T))
    (progn
      (setq *GM_AUTO_LAST_MESSAGE* (strcat "GRILLA: "
        (cond (*UBI_GRID_LAST_ERROR* *UBI_GRID_LAST_ERROR*)
              ((vl-catch-all-error-p grid_result) (vl-catch-all-error-message grid_result))
              (T "La rutina no confirmo generacion"))))
      (GM:AutoTrace code "GRILLA" "ERROR" *GM_AUTO_LAST_MESSAGE*)
      nil)
    (progn
      (GM:AutoTrace code "GRILLA" "OK"
        (strcat "Lineas/textos=" (vl-princ-to-string *UBI_GRID_COUNTS*)))
      ;; Formato y visibilidad actuan en la pestana Model, sin MSPACE ni CVPORT.
      (setvar "CTAB" "Model")
      (setq t0 (GM:PerfMs))
      (setq fmt_ok (GM:AutoStage code "VISIBILIDAD" 'UBI_CleanByHyperlinks (list code) nil))
      (GM:PerfMark code "VISIBILIDAD" t0)
      (if fmt_ok
        (progn
          (setq t0 (GM:PerfMs))
          (setq fmt_ok (GM:AutoStage code "FORMATO_MODELO" 'UBI_ApplyAutoFormat (list code *GM_AUTO_SCALE_STR* *GM_AUTO_SCALE_VAL*) nil))
          (GM:PerfMark code "FORMATO" t0)
          ;; OPT5 (F0): sub-tiempos y conteos del formato (MAPA/TEXTOS/ORDEN/CIERRE).
          (GM:FmtStatsReport code)))
      ;; Auditoria informativa: incluso un fallo de escritura no aborta Model.
      (if fmt_ok
        (progn
          (setq audit_result (vl-catch-all-apply 'GM:TracePEUC (list code)))
          (if (vl-catch-all-error-p audit_result)
            (vl-catch-all-apply 'GM:AutoTrace
              (list code "PEUC" "AVISO"
                (strcat "Auditoria no completada: " (vl-catch-all-error-message audit_result)))))))
      fmt_ok)))

;; Restaurar contexto tambien si centrado/grilla/formato fallan dentro de catch-all.
(defun GM:AutoModelForCode (code row / model_result restore_result model_error)
  (setq model_error *error* *ML_TARGET_CODE* code *GM_AUTO_MODE* T)
  (GM:ActivateTemplatePaper)
  (setq model_result (vl-catch-all-apply 'GM:AutoModelWork (list code row)) *error* model_error)
  (if (and (vl-catch-all-error-p model_result)
           (wcmatch (strcase (vl-catch-all-error-message model_result)) "*CANCEL*,*BREAK*"))
    (setq *GM_AUTO_CANCELLED* T))
  (setq restore_result (vl-catch-all-apply 'GM:ActivateTemplatePaper '()))
  (cond
    ((vl-catch-all-error-p restore_result)
      (setq *GM_AUTO_LAST_MESSAGE* "No se pudo restaurar el Layout despues de Model")
      (GM:AutoTrace code "CONTEXTO_LAYOUT" "ERROR" *GM_AUTO_LAST_MESSAGE*) nil)
    ((vl-catch-all-error-p model_result)
      (setq *GM_AUTO_LAST_MESSAGE* (vl-catch-all-error-message model_result))
      (GM:AutoTrace code "MODELO" "ERROR" *GM_AUTO_LAST_MESSAGE*) nil)
    (T (eq model_result T))))

(defun GM:AutoModelWork (code row / vp vp_num center reference polygon result fit fitidx fitcenter)
  (GM:AutoTrace code "INICIO_MODELO" "INICIO" (strcat "Inicio del procesamiento DWG=" (getvar "DWGNAME")))
  (GM:AutoTrace code "VERSION_CONTEXTO" "INFO"
    (strcat *GM_VER_AUTO* " CTAB=" (getvar "CTAB") " PLANTILLA=" *GM_AUTO_LAYOUT_TAB*))
  (vlax-ldata-put "ML_SUITE" "TARGET_CODE" code)
  (setq *UBI_TARGET_CODE* code *UBI_TARGET_REFERENCE* "" *UBI_TARGET_POLYGON* nil *UBI_ACTIVE_VP* 1 *UBI_VP_DATA_MEM* nil *UBI_VP_DICT* nil)
  (setq vp (UBI_FindViewportInCurrentLayout))
  ;; E10: reutilizar el numero persistente del viewport para la capa GRILLA-VP<n> (manual usa 1-5).
  (if vp
    (progn
      (setq vp_num (vl-catch-all-apply 'vlax-ldata-get (list vp "ML_VP_NUM")))
      (if (and (not (vl-catch-all-error-p vp_num)) (numberp vp_num) (>= vp_num 1) (<= vp_num 5))
        (setq *UBI_ACTIVE_VP* (fix vp_num))
      )
    )
  )
  (if (not vp)
    (progn
      (GM:AutoTrace code "VIEWPORT" "ERROR" "No se detecto viewport real")
      (setq *GM_AUTO_LAST_MESSAGE* "No se detecto un viewport real en el Layout activo")
      nil
    )
    (progn
      (setq reference (if (and row (> (length row) 13) (= (type (nth 13 row)) 'STR)) (nth 13 row) ""))
      (setq polygon (UBI_FindPolygonByCode code))
      (if (and (not polygon) (/= reference ""))
        (progn
          (setq *UBI_TARGET_REFERENCE* reference)
          (setq polygon (UBI_FindPolygonByReference reference))
        )
      )
      (if (not polygon)
        (progn
          (GM:AutoTrace code "POLIGONO" "ERROR" "No se encontro codigo ni referencia U.C.")
          (setq *UBI_TARGET_POLYGON* polygon)
          (setq *GM_AUTO_LAST_MESSAGE* (strcat "No se encontro poligono para " code
            (if (/= reference "") (strcat " ni para la referencia " reference) "")))
          nil
        )
        (progn
          (setq *UBI_TARGET_POLYGON* polygon)
          (GM:AutoTrace code "POLIGONO" "OK" "Poligono localizado")
          ;; Auto-escala por tamano del terreno (escanea matriz+AF antes del VP).
          (setq *GM_AUTO_SCALE_IDX* (UBI_AutoScaleIndex polygon)
                *GM_AUTO_SCALE_STR* (nth (atoi *GM_AUTO_SCALE_IDX*) *UBI_SCALE_LIST*)
                *GM_AUTO_SCALE_VAL* (atof *GM_AUTO_SCALE_STR*))
          (GM:AutoTrace code "AUTOESCALA" "INFO" (strcat "Terreno -> 1/" *GM_AUTO_SCALE_STR*))
          (GM:AutoTrace code "PREPARACION_VP" "INICIO" "Reiniciando memoria del viewport")
          (setq result (vl-catch-all-apply 'vlax-ldata-put (list vp "DYN_VP_CENTER" nil)))
          (if (vl-catch-all-error-p result)
            (progn
              (GM:AutoTrace code "PREPARACION_VP" "ERROR" (vl-catch-all-error-message result))
              (setq *GM_AUTO_LAST_MESSAGE* (vl-catch-all-error-message result))
              nil
            )
            (progn
              (setq result (vl-catch-all-apply 'vlax-ldata-put (list vp "DYN_VP_SCALE_IDX" *GM_AUTO_SCALE_IDX*)))
              (if (vl-catch-all-error-p result)
                (progn
                  (GM:AutoTrace code "PREPARACION_VP" "ERROR" (vl-catch-all-error-message result))
                  (setq *GM_AUTO_LAST_MESSAGE* (vl-catch-all-error-message result))
                  nil
                )
                (progn
                  (GM:AutoTrace code "CENTRADO" "INICIO" "Procesando viewport seleccionado")
                (UBI_ProcessSelectedVP vp)
                (GM:AutoTrace code "CENTRADO" "OK" "Viewport centrado")
                (setq center (vlax-ldata-get vp "DYN_VP_CENTER"))
                ;; BV 2026-09-22: ajuste centro+escala contra ESQUEMA_AF/LOC/ESCALA_VP.
                ;; UBI_AutoViewportFit devuelve (idx centro), "NOFIT" o nil.
                ;; nil = sin datos para calcular (se conserva 55%+centro); NOFIT = fail-closed.
                (setq fit (vl-catch-all-apply 'UBI_AutoViewportFit (list polygon vp)))
                (if (vl-catch-all-error-p fit) (setq fit nil))
                (if (= fit "NOFIT")
                  (progn
                    (GM:AutoTrace code "AUTOESCALA" "ERROR" "Sin composicion libre hasta 1/25000; predio invade ESQUEMA_AF/LOC/ESCALA_VP")
                    (setq *GM_AUTO_LAST_MESSAGE* "Sin composicion libre: el predio invade zonas de esquemas aun en 1/25000")
                    nil
                  )
                  (progn
                    (if (and fit (/= fit "NOFIT"))
                      (progn
                        (setq fitidx (car fit) fitcenter (cadr fit))
                        (if (and fitidx fitcenter)
                          (progn
                            (setq *GM_AUTO_SCALE_IDX* fitidx *GM_AUTO_SCALE_STR* (nth (atoi fitidx) *UBI_SCALE_LIST*) *GM_AUTO_SCALE_VAL* (atof *GM_AUTO_SCALE_STR*))
                            (vlax-ldata-put vp "DYN_VP_CENTER" fitcenter)
                            (vlax-ldata-put vp "DYN_VP_SCALE_IDX" fitidx)
                            (setq center fitcenter)
                            (GM:AutoTrace code "AUTOESCALA" "INFO" (strcat "Ajuste centro+escala -> 1/" *GM_AUTO_SCALE_STR* " sin invadir esquemas"))
                          )
                        )
                      )
                    )
                  (if (and *UBI_VP_DATA_MEM* center)
                  (progn
                    (GM:AutoTrace code "ESCALA" "INICIO" "Actualizando viewport y escala")
                    (setq result (vl-catch-all-apply 'UBI_ExecuteVPUpdate (list 0 *GM_AUTO_SCALE_IDX*)))
                    (if (vl-catch-all-error-p result)
                      (progn
                        (GM:AutoTrace code "ESCALA" "ERROR" (vl-catch-all-error-message result))
                        (setq *GM_AUTO_LAST_MESSAGE* (vl-catch-all-error-message result))
                        nil
                      )
                      (progn
                        (GM:AutoTrace code "ESCALA" "OK" (strcat "Escala 1/" *GM_AUTO_SCALE_STR* " aplicada"))
                          (GM:AutoFinishModel code vp)
                      )
                    )
                  )
                  (progn
                    (GM:AutoTrace code "CENTRADO" "ERROR" "No se pudo guardar el centro del viewport")
                    (setq *GM_AUTO_LAST_MESSAGE* (strcat "Se encontro el predio pero no se pudo centrar el viewport para " code))
                    nil
                  )
                  )
                  )
                )
                )
              )
            )
          )
        )
      )
    )
  )
)