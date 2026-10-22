;; ============================================================================
;; UBI_LIMPIEZA.lsp - Sistema GM Ingenieros (modulo ubi)
;; Movido SIN CAMBIOS desde PLANO_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


;; ==============================================================================
;; VISIBILIDAD POR HIPERV\\U+00DANculos (CORREGIDA PARA NO OCULTAR COLINDANTES/SUNARP/GORE Y WCMATCH)
;; ==============================================================================
(defun UBI_ShouldKeepForUbicacion (hl_up code_up lay_up / padded_hl prefix reference_up)
  (setq padded_hl (strcat " " hl_up " ") prefix (UBI_GetCodePrefix code_up)
        reference_up (UBI_NormalizeSearchKey *UBI_TARGET_REFERENCE*))
  (cond
    ((or (null hl_up) (= hl_up "")) nil)
    ((vl-string-search "VIV" hl_up) nil) ; Ocultar VIVIENDAS / VIV
    ;; Rev AH: contexto PE/UC siempre visible, primera rama prioritaria.
    ((vl-string-search "ETIQUETAS PE UC" hl_up) T)
    ((or (= hl_up code_up)
         (and (/= reference_up "") (= (UBI_NormalizeSearchKey hl_up) reference_up))) T)
    ((and (vl-string-search code_up hl_up) (vl-string-search "-AF-" hl_up)) T) 
    ((and (vl-string-search code_up hl_up) (vl-string-search "-PF-" hl_up)) T) 
    ((or (vl-string-search "-AF-" hl_up) (vl-string-search "-PF-" hl_up)) nil) 
    ((or (vl-string-search "PROGRESIVA" hl_up) (vl-string-search "ESTACA" hl_up) (vl-string-search "ETIQUETAS PE UC" hl_up) (vl-string-search "DERECHO DE VIA" hl_up) (vl-string-search "EJE DE VIA" hl_up) (vl-string-search "VIA EXISTENTE" hl_up)) T)
    ((or (vl-string-search "GORE" hl_up) (vl-string-search "SUNARP" hl_up) (vl-string-search "PREDIO" hl_up) (vl-string-search "COLINDANTE" hl_up) (vl-string-search "PROPIETARIO" hl_up) (vl-string-search "TERCERO" hl_up) (vl-string-search "PC-" hl_up)
         (vl-string-search "SUNARP" lay_up) (vl-string-search "COLINDANTE" lay_up) (vl-string-search "GORE" lay_up)) T)
    ((or (vl-string-search " RIO " padded_hl) (vl-string-search "QUEBRADA" hl_up)) T)
    ((or (and (/= prefix "") (vl-string-search prefix hl_up)) (wcmatch hl_up "*-*-*#*")) T) ; <-- CORRECCI\\U+00D3N: RECONOCIMIENTO ESTRUCTURAL
    (T nil)
  )
)

(defun UBI_CleanByHyperlinks (target_code / app doc ms ent vla_obj hl_str code_up keep hide_count restore_count lay_name lay_up item keep_list hide_list delta_on new_mask iidx nm pm want was delta_ok applied_ok)
  (if (or (= target_code "") (not target_code)) (progn (alert "Debe definir un c\\U+00F3digo primero.") (exit)))
  ;; T2.2-FIX1: el delta de visibilidad vive entre codigos; la restauracion
  ;; completa por codigo solo corre sin delta (manual o indice no listo).
  (setq delta_on (and *GM_AUTO_MODE* *UBI_ENT_INDEX_READY* *UBI_ENT_INDEX*))
  (if delta_on
    (progn
      (if (not *UBI_VIS_PREV_READY*) (UBI_VisResyncPrev))
      (setq delta_on (and *UBI_VIS_PREV_READY* *UBI_VIS_PREV*
                          (= (length *UBI_VIS_PREV*) (length *UBI_ENT_INDEX*))))
    )
  )
  (setq restore_count (if delta_on 0 (UBI_RestoreHiddenByGM))
        code_up (vl-string-trim " \t\n\r" (strcase target_code)) hide_count 0)
  (if (and *UBI_ENT_INDEX_READY* *UBI_ENT_INDEX*)
    (progn
      ;; PASO 1 (Lisp puro): clasificar cada entidad y construir la mascara
      ;; nueva alineada al indice (mismo orden); sin COM ni LData aqui.
      ;; T2.2-FIX1: el estado previo se invalida de forma pesimista y solo un
      ;; recorrido completo sin error lo vuelve a armar (fail-closed).
      (setq keep_list nil hide_list nil new_mask nil *UBI_VIS_PREV_READY* nil)
      (foreach item *UBI_ENT_INDEX*
        (setq ent (car item) hl_str (cadr item) lay_name (caddr item) keep nil)
        ;; T1.6: un solo strcase por entidad (antes se repetia hasta 4 veces).
        (setq lay_up (strcase lay_name))
        ;; A\\U+00D1ADIMOS SUNARP, COLINDANTE Y GORE A LA LISTA DE CAPAS PROTEGIDAS
        (if (or (vl-string-search "GRILLA" lay_up) (vl-string-search "MEMBRETE" lay_up) (vl-string-search "VISTA" lay_up)
                (vl-string-search "SUNARP" lay_up) (vl-string-search "COLINDANTE" lay_up) (vl-string-search "GORE" lay_up)
                (UBI_ContainsString *UBI_LAYER_LIST* lay_name))
          (setq keep T)
          (if (and *UBI_TARGET_POLYGON* (= ent *UBI_TARGET_POLYGON*))
            (setq keep T)
            (if (/= hl_str "") (setq keep (UBI_ShouldKeepForUbicacion (strcase hl_str) code_up lay_up)) (setq keep nil))
          )
        )
        (setq new_mask (cons (if keep T nil) new_mask))
        (if keep (setq keep_list (cons ent keep_list)) (setq hide_list (cons ent hide_list)))
      )
      (setq new_mask (reverse new_mask))
      ;; PASO 2: aplicar.
      (if delta_on
        (progn
          ;; T2.2-FIX1: recorrido paralelo indice/mascara nueva/mascara previa;
          ;; solo se toca la entidad cuando su estado cambia (O(delta)).
          ;; F: delta fail-closed: si SetEntityVisible no confirma el cambio,
          ;; LData/hide_count no avanzan y la mascara previa queda invalidada.
          (setq iidx *UBI_ENT_INDEX* nm new_mask pm *UBI_VIS_PREV* delta_ok T)
          (while (and iidx nm pm)
            (setq ent (car (car iidx)) want (car nm) was (car pm))
            (if (not (equal (if want T nil) (if was T nil)))
              (progn
                (setq applied_ok (and ent (UBI_SetEntityVisible ent want)))
                (if applied_ok
                  (progn
                    (setq *UBI_VIS_MODIFIED* (cons ent *UBI_VIS_MODIFIED*))
                    (if want
                      (if (= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1) (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 0))
                      (progn
                        (if (/= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1) (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 1))
                        (setq hide_count (1+ hide_count)))))
                  (setq delta_ok nil))
              )
            )
            (setq iidx (cdr iidx) nm (cdr nm) pm (cdr pm))
          )
          (if delta_ok
            (setq *UBI_VIS_PREV* new_mask *UBI_VIS_PREV_READY* T))
        )
        ;; Sin delta (manual o indice sin mascaras): pasada completa como antes
        ;; de T2.2; la base de restauracion queda sembrada al indexar.
        (progn
          (foreach ent keep_list
            (if (UBI_SetEntityVisible ent T) (setq *UBI_VIS_MODIFIED* (cons ent *UBI_VIS_MODIFIED*)))
            (if (= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1) (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 0)))
          (foreach ent hide_list
            (if (UBI_SetEntityVisible ent nil) (setq *UBI_VIS_MODIFIED* (cons ent *UBI_VIS_MODIFIED*)))
            (if (/= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1) (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 1))
            (setq hide_count (1+ hide_count)))
        )
      )
    )
    (progn
  ;; F15-R1: app/doc/ms con catch y liberados al salir de la rama de respaldo.
  (setq app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (vl-catch-all-error-p app) (setq app nil))
  (setq doc (if app (vl-catch-all-apply 'vla-get-ActiveDocument (list app)) nil))
  (if (vl-catch-all-error-p doc) (setq doc nil))
  (setq ms (if doc (vl-catch-all-apply 'vla-get-ModelSpace (list doc)) nil))
  (if (vl-catch-all-error-p ms) (setq ms nil))
  (if ms
    (vlax-for vla_obj ms
    (setq ent (vlax-vla-object->ename vla_obj) lay_name (vla-get-Layer vla_obj))
    ;; A\\U+00D1ADIMOS SUNARP, COLINDANTE Y GORE A LA LISTA DE CAPAS PROTEGIDAS
    (if (or (vl-string-search "GRILLA" (strcase lay_name)) (vl-string-search "MEMBRETE" (strcase lay_name)) (vl-string-search "VISTA" (strcase lay_name)) 
            (vl-string-search "SUNARP" (strcase lay_name)) (vl-string-search "COLINDANTE" (strcase lay_name)) (vl-string-search "GORE" (strcase lay_name))
            (UBI_ContainsString *UBI_LAYER_LIST* lay_name))
      (setq keep T)
       (if (and *UBI_TARGET_POLYGON* (= ent *UBI_TARGET_POLYGON*))
         (setq keep T)
         (progn
         (setq hl_str (UBI_GetHyperlinkString vla_obj))
         (if (and hl_str (/= hl_str "")) (setq keep (UBI_ShouldKeepForUbicacion (strcase hl_str) code_up (strcase lay_name))) (setq keep nil))
         )
       )
    )
    ;; OPT2 2026-09-26 (Fase 3): escribir DXF 60 y LData solo cuando cambia.
    (if keep
      (progn
        (if (UBI_SetEntityVisible ent T) (setq *UBI_VIS_MODIFIED* (cons ent *UBI_VIS_MODIFIED*)))
        (if (= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1) (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 0)))
      (progn
        (if (UBI_SetEntityVisible ent nil) (setq *UBI_VIS_MODIFIED* (cons ent *UBI_VIS_MODIFIED*)))
        (if (/= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1) (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 1))
        (setq hide_count (1+ hide_count))))
      )
    )
    (GM:ReleaseObjectSafe ms)
    (setq ms nil)
    (GM:ReleaseObjectSafe doc)
    (GM:ReleaseObjectSafe app)
    )
  )
  (princ (strcat "\n[VISIBILIDAD UBICACION] Restituidos: " (itoa restore_count) " | Ocultados: " (itoa hide_count)))
)

;; ==============================================================================
;; ESCALADO DE TEXTOS Y FORMATO PE UC
;; ==============================================================================
(defun UBI_GetEndpoints (obj tipo / startpt endpt pnts converted lista_p)
  (setq lista_p nil)
  (cond
    ((= tipo "LINE") 
     (setq startpt (vlax-get obj 'StartPoint) endpt (vlax-get obj 'EndPoint) lista_p (list startpt endpt))
    )
    ((= tipo "LWPOLYLINE")
      ;; Civil 3D 2021 puede devolver Coordinates como lista o SAFEARRAY.
      (setq pnts (vlax-get obj 'Coordinates))
      (setq converted (vl-catch-all-apply 'vlax-safearray->list (list pnts)))
      (if (not (vl-catch-all-error-p converted)) (setq pnts converted))
      (setq lista_p (list (list (car pnts) (cadr pnts) 0.0) (list (nth (- (length pnts) 2) pnts) (nth (- (length pnts) 1) pnts) 0.0)))
    )
  )
  lista_p
)

(defun UBI_FormatPEUC (obj map_text / str pos p1 p2 obj_name clean orig nw cur)
  ;; POR QUE in-place y sin borrados: la limpieza del lote elimina entidades
  ;; nuevas por diff de handles; si aqui se borrara el texto original y se creara
  ;; otro, el original se perderia (bug HS-MC-003). Se preservan handle, capa,
  ;; rotacion e hipervinculos; el estilo se fuerza a ARIAL para legibilidad.
  ;; TEXT queda en 1 linea; MTEXT admite 2 lineas con \P. Altura fija map_text.
  ;; OPT7 2026-09-27 (F3): diferencial; capa/rotacion eran autoescrituras (leian
  ;; y devolvian el mismo valor) y se eliminaron.
  (setq orig (vla-get-TextString obj) str orig obj_name (vla-get-ObjectName obj))
  ;; LIMPIEZA DE SALTOS DE LINEA PREVIOS Y DOBLE ESPACIOS
  (while (vl-string-search "\\P" str) (setq str (vl-string-subst " " "\\P" str)))
  (while (vl-string-search "  " str) (setq str (vl-string-subst " " "  " str)))
  (setq str (vl-string-trim " " str) pos nil clean str)
  ;; BUSQUEDA DE PALABRAS CLAVE AMPLIADA (CON Y SIN PUNTO FINAL)
  (cond
    ((setq pos (vl-string-search "SN / U.C." (strcase clean))))
    ((setq pos (vl-string-search "SN / U.C" (strcase clean))))
    ((setq pos (vl-string-search "U.C." (strcase clean))))
    ((setq pos (vl-string-search "U.C" (strcase clean))))
  )
  (if pos
    (progn
      (setq p1 (vl-string-trim " " (substr clean 1 pos))) (setq p2 (vl-string-trim " " (substr clean (1+ pos))))
      (if (= obj_name "AcDbMText")
        (setq str (strcat p1 "\\P" p2))
        (setq str (strcat p1 " " p2)))
    )
  )
  (setq nw 0)
  (if (and *GM_FMT_DIFF_WRITE* (= str orig))
    (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
    (progn (vl-catch-all-apply 'vla-put-TextString (list obj str)) (setq nw (1+ nw))))
  (setq cur (vl-catch-all-apply 'vla-get-Height (list obj)))
  (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (UBI_FmtNear cur map_text))
    (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
    (progn (vl-catch-all-apply 'vla-put-Height (list obj map_text)) (setq nw (1+ nw))))
  (setq cur (vl-catch-all-apply 'vla-get-Color (list obj)))
  (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (= cur 1))
    (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
    (progn (vl-catch-all-apply 'vla-put-Color (list obj 1)) (setq nw (1+ nw))))
  (if (= obj_name "AcDbMText")
    (progn
      (setq cur (vl-catch-all-apply 'vla-get-Width (list obj)))
      (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (UBI_FmtNear cur 0.0))
        (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
        (progn (vl-catch-all-apply 'vla-put-Width (list obj 0.0)) (setq nw (1+ nw))))))
  (setq cur (vl-catch-all-apply 'vla-get-StyleName (list obj)))
  (if (and *GM_FMT_DIFF_WRITE* (= (type cur) 'STR) (= (strcase cur) "ARIAL"))
    (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
    (progn (vl-catch-all-apply 'vla-put-StyleName (list obj "ARIAL")) (setq nw (1+ nw))))
  (if (> nw 0)
    (vl-catch-all-apply 'vla-Update (list obj))
    (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*)))
  (setq *GM_FMT_TX_WRITES* (+ *GM_FMT_TX_WRITES* nw))
)

;; OPT6 (F2): ramas por hipervinculo extraidas para que el barrido del indice y
;; el barrido COM compartan exactamente la misma logica.
;; OPT7 2026-09-27 (F3): diferencial (altura/estilo/update) con contadores.
;; Devuelve la entrada (ename InsertionPoint "PROG") si el texto es PROGRESIVA.
(defun UBI_ProcessHlText (obj hl_up map_prog map_peuc map_dist / res nw cur)
  (setq res nil)
  (cond
    ((vl-string-search "PROGRESIVA" hl_up)
     (setq nw 0)
     (setq cur (vl-catch-all-apply 'vla-get-Height (list obj)))
     (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (UBI_FmtNear cur map_prog))
       (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
       (progn (vl-catch-all-apply 'vla-put-Height (list obj map_prog)) (setq nw (1+ nw))))
     (setq cur (vl-catch-all-apply 'vla-get-StyleName (list obj)))
     (if (and *GM_FMT_DIFF_WRITE* (= (type cur) 'STR) (= (strcase cur) "ARIAL"))
       (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
       (progn (vl-catch-all-apply 'vla-put-StyleName (list obj "ARIAL")) (setq nw (1+ nw))))
     (if (> nw 0)
       (vl-catch-all-apply 'vla-Update (list obj))
       (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*)))
     (setq *GM_FMT_TX_WRITES* (+ *GM_FMT_TX_WRITES* nw))
     ;; Fase 1B (H-5): guardar el ename, no el wrapper COM.
     (setq res (list (vlax-vla-object->ename obj) (vlax-get obj 'InsertionPoint) "PROG"))
    )
    ((vl-string-search "ETIQUETAS PE UC" hl_up)
     (UBI_FormatPEUC obj map_peuc)
    )
    ((vl-string-search "DISTANCIA" hl_up) ;; BQ: Altura Distancias Excel (nth 2); no-op si no hay hipervinculos DISTANCIA
     (setq nw 0)
     (setq cur (vl-catch-all-apply 'vla-get-Height (list obj)))
     (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (UBI_FmtNear cur map_dist))
       (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
       (progn (vl-catch-all-apply 'vla-put-Height (list obj map_dist)) (setq nw (1+ nw))))
     (setq cur (vl-catch-all-apply 'vla-get-StyleName (list obj)))
     (if (and *GM_FMT_DIFF_WRITE* (= (type cur) 'STR) (= (strcase cur) "ARIAL"))
       (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
       (progn (vl-catch-all-apply 'vla-put-StyleName (list obj "ARIAL")) (setq nw (1+ nw))))
     (if (> nw 0)
       (vl-catch-all-apply 'vla-Update (list obj))
       (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*)))
     (setq *GM_FMT_TX_WRITES* (+ *GM_FMT_TX_WRITES* nw))
    )
    ((= hl_up "VIV")
     (setq nw 0)
     (setq cur (vl-catch-all-apply 'vla-get-StyleName (list obj)))
     (if (and *GM_FMT_DIFF_WRITE* (= (type cur) 'STR) (= (strcase cur) "ROMANS"))
       (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
       (progn (vl-catch-all-apply 'vla-put-StyleName (list obj "ROMANS")) (setq nw (1+ nw))))
     (if (> nw 0)
       (vl-catch-all-apply 'vla-Update (list obj))
       (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*)))
     (setq *GM_FMT_TX_WRITES* (+ *GM_FMT_TX_WRITES* nw))
    )
  )
  res
)
;; Escala la estaca y devuelve su centro (o nil). Mismas condiciones del flujo.
;; OPT7 2026-09-27 (F3): desde el segundo codigo la estaca ya tiene la escala
;; aplicada (esc=1); se omite scaleentity+update conservando el centro para el
;; reposicionamiento de progresivas.
(defun UBI_ProcessHlLine (obj hl_up map_vw tipo / pnts p1 p2 old_len minpt maxpt centro esc res)
  (setq res nil)
  (if (vl-string-search "ESTACA" hl_up)
    (progn
      (setq pnts (UBI_GetEndpoints obj tipo))
      (if (= (length pnts) 2)
        (progn
          (setq p1 (car pnts) p2 (cadr pnts) old_len (distance p1 p2))
          (if (> old_len 0.0001)
            (progn
              (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minp 'maxp))
              (setq minpt (vlax-safearray->list minp) maxpt (vlax-safearray->list maxp))
              (setq centro (list (/ (+ (car minpt) (car maxpt)) 2.0) (/ (+ (cadr minpt) (cadr maxpt)) 2.0) 0.0))
              (setq esc (* (/ map_vw old_len) (atof *UBI_STAKE_SCALE_FACTOR*)))
              (if (and *GM_FMT_DIFF_WRITE* (UBI_FmtNear esc 1.0))
                (setq *GM_FMT_STK_SKIP* (1+ *GM_FMT_STK_SKIP*))
                (progn
                  (vla-scaleentity obj (vlax-3d-point centro) esc)
                  ;; REDIBUJAR PARA VISUALIZACION INMEDIATA
                  (vl-catch-all-apply 'vla-Update (list obj))))
              (setq res centro)
            )
          )
        )
      )
    )
  )
  res
)

(defun UBI_ProcessHyperlinks (map_vw map_prog map_peuc map_dist / ss_lines ss_texts i obj txt_list pnts p1 p2 old_len minp maxp centro esc stake_mids min_dist best_mid d txt_obj txt_ins t_item smid hl_str hl_up minpt maxpt acad_doc ts_col new_style text_mult ang off_dist new_pt item typ _app)
  (setq txt_list nil stake_mids nil text_mult (atof *UBI_TEXT_SCALE_MULT*))
  (setq map_vw (* map_vw text_mult) map_prog (* map_prog text_mult) map_peuc (* map_peuc text_mult) map_dist (* (if (numberp map_dist) map_dist map_peuc) text_mult)) ;; BQ: map_dist nil-safe
  ;; AUD1: app/doc con catch y la app liberada en el acto (antes anonima: fugaba
  ;; 1 por codigo aunque el doc si se liberaba al final).
  (setq _app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (vl-catch-all-error-p _app) (setq _app nil))
  (setq acad_doc (if _app (vl-catch-all-apply 'vla-get-ActiveDocument (list _app)) nil))
  (if (vl-catch-all-error-p acad_doc) (setq acad_doc nil))
  (GM:ReleaseObjectSafe _app)
  (setq _app nil)
  (UBI_create_style_st_coorp) 
  
  (if (not (tblsearch "STYLE" "ARIAL"))
    (progn
      (setq ts_col (if acad_doc (vl-catch-all-apply 'vla-get-TextStyles (list acad_doc)) nil))
      (if (vl-catch-all-error-p ts_col) (setq ts_col nil))
      (setq new_style (if ts_col (vl-catch-all-apply 'vla-Add (list ts_col "ARIAL")) nil))
      (if (not (vl-catch-all-error-p new_style))
        (vl-catch-all-apply 'vla-put-fontfile (list new_style "arial.ttf"))
      )
      ;; Fase 1B (H-5): liberar el estilo creado y la coleccion de estilos.
      (vl-catch-all-apply 'vlax-release-object (list new_style))
      (vl-catch-all-apply 'vlax-release-object (list ts_col))
      (setq new_style nil ts_col nil)
    )
  )
  
  (if *UBI_ENT_INDEX_READY*
    ;; OPT8: sub-indice de textos con hipervinculo (sin barrer las 12k entradas).
    (foreach item *UBI_HLTEXT_IDX*
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list (car item))))
      (if (not (vl-catch-all-error-p obj))
        (progn
          (setq t_item (UBI_ProcessHlText obj (strcase (cadr item)) map_prog map_peuc map_dist))
          (if t_item (setq txt_list (cons t_item txt_list)))
          (vl-catch-all-apply 'vlax-release-object (list obj))
          (setq obj nil)
        )
      )
    )
    (progn
      (if (setq ss_texts (ssget "X" (list (cons 0 "TEXT,MTEXT") (cons 410 "Model"))))
        (progn
          (setq i 0)
          (repeat (sslength ss_texts)
            ;; AUD1: creacion con catch; entidad caida = se salta.
            (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list (ssname ss_texts i))))
            (if (vl-catch-all-error-p obj) (setq obj nil))
            (setq hl_str (if obj (UBI_GetHyperlinkString obj) nil))
            (if (and hl_str (/= (vl-string-trim " " hl_str) ""))
              (progn
                (setq hl_up (strcase hl_str))
                (setq t_item (UBI_ProcessHlText obj hl_up map_prog map_peuc map_dist))
                (if t_item (setq txt_list (cons t_item txt_list)))
              )
            )
            ;; Fase 1B (H-5): un wrapper por texto, liberado siempre.
            (vl-catch-all-apply 'vlax-release-object (list obj))
            (setq i (1+ i))
          )
          (setq ss_texts nil) (gc)
        )
      )
    )
  )

  (if *UBI_ENT_INDEX_READY*
    ;; OPT8: sub-indice de lineas con hipervinculo (sin barrer las 12k entradas).
    (foreach item *UBI_HLLINE_IDX*
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list (car item))))
      (if (not (vl-catch-all-error-p obj))
        (progn
          (setq smid (UBI_ProcessHlLine obj (strcase (cadr item)) map_vw (if (= (caddr item) "AcDbLine") "LINE" "LWPOLYLINE")))
          (if smid (setq stake_mids (cons smid stake_mids)))
          (vl-catch-all-apply 'vlax-release-object (list obj))
          (setq obj nil)
        )
      )
    )
  )
  (if (and (not *UBI_ENT_INDEX_READY*) (setq ss_lines (ssget "X" (list (cons 0 "LINE,LWPOLYLINE") (cons 410 "Model")))))
    (progn 
      (setq i 0)
      (repeat (sslength ss_lines)
        ;; AUD1: creacion con catch; entidad caida = se salta.
        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list (ssname ss_lines i))))
        (if (vl-catch-all-error-p obj) (setq obj nil))
        (setq hl_str (if obj (UBI_GetHyperlinkString obj) nil))
        (if (and hl_str (/= (vl-string-trim " " hl_str) ""))
          (progn
            (setq hl_up (strcase hl_str))
            (if (vl-string-search "ESTACA" hl_up)
              (progn
                (setq pnts (UBI_GetEndpoints obj (cdr (assoc 0 (entget (vlax-vla-object->ename obj))))))
                (if (= (length pnts) 2)
                  (progn
                    (setq p1 (car pnts) p2 (cadr pnts) old_len (distance p1 p2))
                    (if (> old_len 0.0001)
                      (progn
                        (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'minp 'maxp))
                        (setq minpt (vlax-safearray->list minp) maxpt (vlax-safearray->list maxp))
                        (setq centro (list (/ (+ (car minpt) (car maxpt)) 2.0) (/ (+ (cadr minpt) (cadr maxpt)) 2.0) 0.0))
                        
                        (setq esc (* (/ map_vw old_len) (atof *UBI_STAKE_SCALE_FACTOR*))) 
                        (vla-scaleentity obj (vlax-3d-point centro) esc)
                        
                        ;; REDIBUJAR PARA VISUALIZACION INMEDIATA
                        (vl-catch-all-apply 'vla-Update (list obj))
                        
                        (setq stake_mids (cons centro stake_mids))
                      )
                    )
                  )
                )
              )
            )
          )
        )
        ;; Fase 1B (H-5): un wrapper por linea, liberado siempre.
        (vl-catch-all-apply 'vlax-release-object (list obj))
        (setq i (1+ i))
      )
      (setq ss_lines nil) (gc)
    )
  )

  (foreach t_item txt_list
    (if (= (caddr t_item) "PROG")
      (progn
        (setq txt_ins (cadr t_item))
        (setq min_dist 1000000.0 best_mid nil)
        
        (foreach smid stake_mids
          (setq d (distance (list (car smid) (cadr smid)) (list (car txt_ins) (cadr txt_ins))))
          (if (< d min_dist)
            (progn
              (setq min_dist d best_mid smid)
            )
          )
        )
        
        (if (and best_mid (< min_dist 1000.0))
          (progn
            (setq ang (angle best_mid txt_ins))
            (setq off_dist (* map_prog (atof *UBI_PROG_OFFSET_MULT*)))
            (setq new_pt (polar best_mid ang off_dist))
            ;; OPT8: no reescribir la posicion si la progresiva ya esta ahi.
            (if (and *GM_FMT_DIFF_WRITE* (UBI_FmtNear (car new_pt) (car txt_ins)) (UBI_FmtNear (cadr new_pt) (cadr txt_ins)))
              (setq *GM_FMT_TX_SKIP* (1+ *GM_FMT_TX_SKIP*))
              (progn
                ;; Fase 1B (H-5): wrapper temporal por item, liberado al usarlo.
                (setq txt_obj (vl-catch-all-apply 'vlax-ename->vla-object (list (car t_item))))
                (if (not (vl-catch-all-error-p txt_obj))
                  (progn
                    (vlax-put txt_obj 'InsertionPoint (list (car new_pt) (cadr new_pt) (caddr txt_ins)))
                    (vl-catch-all-apply 'vlax-release-object (list txt_obj))
                    (setq txt_obj nil)))
              )
            )
          )
        )
      )
    )
  )
  ;; Fase 1B (H-5): liberar el documento al cerrar.
  (vl-catch-all-apply 'vlax-release-object (list acad_doc))
  (setq acad_doc nil)
)