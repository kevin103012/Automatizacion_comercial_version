;; ============================================================================
;; GM_PLANTILLA.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


(defun GM:LayoutFail (message)
  ;; POR QUE exit: aborta la etapa actual como excepcion no local; el llamador
  ;; (AutoProcessCode / EXEC_ESQUEMAS) la captura, limpia entidades nuevas y
  ;; registra LAYOUT;ERROR. No retornar nil aqui: el fallo debe ser visible.
  (setq *GM_AUTO_LAST_MESSAGE* message *GM_AUTO_LAYOUT_FAILED* T)
  (GM:AutoTrace *ML_TARGET_CODE* "LAYOUT" "ERROR" message)
  (exit)
)

;; Fija visibilidad por DXF ((60 . 1) oculta, sin 60 visible) y lo verifica
;; releyendo la entidad. Retorna T si quedo en el estado pedido.
;; CHG1 2026-10-07: para ACAD_TABLE (leyenda de plantilla nueva) el grupo 60 no
;; es fiable por entmod; si el DXF no verifica, reintenta por COM (Visible) y
;; verifica releyendo la propiedad. La entidad desconocida sigue fallando.
(defun GM:LegendSetVisible (ent visible / data pair ok obj res got)
  (if (not (and ent (setq data (entget ent))))
    nil
    (progn
      (if visible
        (if (setq pair (assoc 60 data))
          (setq data (subst '(60 . 0) pair data))
        )
        (if (setq pair (assoc 60 data))
          (setq data (subst '(60 . 1) pair data))
          (setq data (append data '((60 . 1))))))
      (if (entmod data)
        (progn
          (entupd ent)
          (setq data (entget ent))
          (if visible (not (= (cdr (assoc 60 data)) 1)) (= (cdr (assoc 60 data)) 1))
        )
        nil))))
;; CHG1: version con respaldo COM para leyendas que no exponen el grupo 60.
(defun GM:LegendSetVisibleEx (ent visible / ok obj res got)
  (setq ok (GM:LegendSetVisible ent visible))
  (if (not ok)
    (progn
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (not (vl-catch-all-error-p obj))
        (progn
          (setq res (vl-catch-all-apply 'vla-put-Visible (list obj (if visible :vlax-true :vlax-false))))
          (if (not (vl-catch-all-error-p res))
            (progn
              (setq got (vl-catch-all-apply 'vla-get-Visible (list obj)))
              (if (not (vl-catch-all-error-p got))
                (setq ok (eq (if (eq got :vlax-false) nil T) (if visible T nil))))))
          (GM:ReleaseObjectSafe obj)
          (setq obj nil))))
    )
  (if ok T nil)
)
(defun GM:LayoutRestoreLegend (/ restored cur obj set_item)
  ;; Restore the exact original DXF, including visibility; never restyle the base.
  ;; CHG1 2026-10-07: leyenda ACAD_TABLE: no hay snapshot DXF (solo se toco la
  ;; visibilidad); se restaura visible por COM y se verifica.
  (if *GM_AUTO_LEGEND_TABLE_P*
    (progn
      (if (not (GM:LegendSetVisibleEx *GM_AUTO_LEGEND_TABLE_P* T))
        (progn (setq *GM_AUTO_CANCELLED* T) (GM:LayoutFail "No se pudo restaurar la leyenda base (tabla)")))
      (setq *GM_AUTO_LEGEND_TABLE_P* nil))
    (if *GM_AUTO_LEGEND_DATA*
      (progn
        (setq restored (cdr (assoc -1 *GM_AUTO_LEGEND_DATA*)))
        (if (not (entmod *GM_AUTO_LEGEND_DATA*))
          (progn (setq *GM_AUTO_CANCELLED* T) (GM:LayoutFail "No se pudo restaurar la leyenda base")))
        (entupd restored)
        ;; Fase 1A (H-10): se relee el estado REAL. Si la original venia visible
        ;; SIN grupo 60, entmod no siempre elimina el (60 . 1) temporal que puso
        ;; la calibracion; depender del snapshot dejaba la leyenda oculta y el
        ;; siguiente codigo registraba "Original oculta rehabilitada".
        (setq cur (entget restored))
        (if (= (cdr (assoc 60 cur)) 1)
          (if (not (GM:LegendSetVisible restored T))
            (progn (setq *GM_AUTO_CANCELLED* T) (GM:LayoutFail "Leyenda restaurada pero oculta"))))
        (setq *GM_AUTO_LEGEND_DATA* nil))))
  ;; CHG3-FIX 2026-10-10: restaurar las entidades del conjunto (simbologias y
  ;; wipeout) desde su snapshot DXF y borrar sus copias. H-10: si el original
  ;; venia visible SIN grupo 60, el entmod puede no eliminar el (60 . 1)
  ;; temporal; se re-verifica y se desoculta antes de seguir.
  (foreach set_item *GM_AUTO_LEGEND_SET_DATA*
    (setq restored (cdr (assoc -1 set_item)))
    (if (and restored (entget restored))
      (progn
        (if (not (entmod set_item))
          (progn (setq *GM_AUTO_CANCELLED* T) (GM:LayoutFail "No se pudo restaurar una simbologia de la leyenda base")))
        (entupd restored)
        (setq cur (entget restored))
        (if (= (cdr (assoc 60 cur)) 1)
          (if (not (GM:LegendSetVisible restored T))
            (progn (setq *GM_AUTO_CANCELLED* T) (GM:LayoutFail "Simbologia de leyenda restaurada pero oculta")))))))
  (setq *GM_AUTO_LEGEND_SET_DATA* nil)
  (foreach set_item *GM_AUTO_LEGEND_SET_COPIES*
    (if (and set_item (entget set_item))
      (if (not (entdel set_item))
        (progn
          (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list set_item)))
          (if (not (vl-catch-all-error-p obj))
            (progn (vl-catch-all-apply 'vla-Delete (list obj)) (GM:ReleaseObjectSafe obj)))
          (setq obj nil)))))
  (setq *GM_AUTO_LEGEND_SET_COPIES* nil)
  (if (and *GM_AUTO_LEGEND_COPY* (entget *GM_AUTO_LEGEND_COPY*))
    (progn
      (if (not (entdel *GM_AUTO_LEGEND_COPY*))
        (progn
          ;; CHG1: la copia de una tabla puede resistir entdel; respaldo COM.
          (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list *GM_AUTO_LEGEND_COPY*)))
          (if (not (vl-catch-all-error-p obj))
            (progn (vl-catch-all-apply 'vla-Delete (list obj)) (GM:ReleaseObjectSafe obj)))
          (setq obj nil)))))
  (setq *GM_AUTO_LEGEND_COPY* nil)
)

;; BX: la plantilla puede conservar un cuadro de escala creado por el flujo
;; manual. Se oculta solo durante la exportacion para no superponerlo con el
;; cuadro automatico y se restaura con su DXF exacto al limpiar la corrida.
(defun GM:LayoutRestoreScaleBase (/ item ent failed remaining)
  (setq failed nil remaining nil)
  (foreach item *GM_AUTO_SCALE_BASE_DATA*
    (setq ent (cdr (assoc -1 item)))
    (if (or (not ent) (not (entmod item)))
      (setq failed T remaining (cons item remaining))
      (entupd ent)))
  ;; Conservar los DXF fallidos permite reintentar en la limpieza exterior.
  (setq *GM_AUTO_SCALE_BASE_DATA* (reverse remaining))
  (not failed)
)

(defun GM:LayoutHideScaleBase (/ zone ss i ent data box hidden failed restored)
  (setq restored (GM:LayoutRestoreScaleBase))
  (setq zone (cdr (assoc "ESCALA_VP" *GM_AUTO_ZONES*)) hidden 0 failed (not restored))
  (if (and restored zone)
    (progn
      (setq ss (ssget "X" (list (cons 0 "TEXT,MTEXT,LWPOLYLINE,POLYLINE,HATCH,SOLID")
                                (cons 8 "Membrete") (cons 410 (getvar "CTAB")))))
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq ent (ssname ss i) data (entget ent) box (GM:LayoutEntityBox ent))
            (if (and data box
                     (< (car box) (nth 2 zone)) (> (nth 2 box) (car zone))
                     (< (cadr box) (nth 3 zone)) (> (nth 3 box) (cadr zone)))
              (progn
                (setq *GM_AUTO_SCALE_BASE_DATA* (cons data *GM_AUTO_SCALE_BASE_DATA*))
                (if (GM:LegendSetVisible ent nil)
                  (setq hidden (1+ hidden))
                  (setq failed T))))
            (setq i (1+ i)))))))
  (GM:AutoTrace *ML_TARGET_CODE* "ESCALA_BASE" (if failed "ERROR" "OK")
    (strcat "Entidades de escala preexistentes ocultas=" (itoa hidden)))
  (not failed)
)

;; Dimensiones de referencia Huancasancos (solo para escalar zonas, no para bloquear).
;; El viewport real se acepta con w,h > 50 (contrato GM v1, SPEC_PLANTILLA_GM.md).
(if (not (boundp '*GM_VP_W*)) (setq *GM_VP_W* 278.94963536 *GM_VP_H* 257.70882797))
(if (not (boundp '*GM_VP_REF_W*)) (setq *GM_VP_REF_W* 278.94963536 *GM_VP_REF_H* 257.70882797))

;; --- Contrato GM v1: roles en vez de handles fijos ---
;; Rol guardado con ldata "GM_ROL": VP_PRINCIPAL / LEYENDA / MAPA_GEO / PROYECTO.
;; POR QUE duplicado: LAYOUT trae los mismos 3 helpers para operar autocontenido
;; sin AUTO; al cargar AUTO al final gana esta version (misma semantica).
(defun GM:EntRole (ent / r)
  (setq r (vl-catch-all-apply 'vlax-ldata-get (list ent "GM_ROL")))
  (if (or (vl-catch-all-error-p r) (not (= (type r) 'STR))) nil r)
)
(defun GM:SetEntRole (ent rol)
  (vl-catch-all-apply 'vlax-ldata-put (list ent "GM_ROL" rol))
  rol
)
;; Busca en el layout activo la primera entidad del tipo pedido con ese rol.
(defun GM:FindByRole (rol dtype / ss i e)
  (setq ss (ssget "X" (list (cons 0 dtype) (cons 410 (getvar "CTAB")))))
  (if (not ss) nil
    (progn
      (setq i 0 e nil)
      (while (and (< i (sslength ss)) (not e))
        (if (= (GM:EntRole (ssname ss i)) rol) (setq e (ssname ss i)))
        (setq i (1+ i))
      )
      e
    )
  )
)
;; Viewport real mas grande del layout (excluye papel 69=1). Fallback generico.
(defun GM:LargestLayoutViewport (/ ss i e ed best bestarea w h a)
  (setq ss (ssget "X" (list (cons 0 "VIEWPORT") (cons 410 (getvar "CTAB")))))
  (if (not ss) nil
    (progn
      (setq i 0 best nil bestarea 0.0)
      (while (< i (sslength ss))
        (setq e (ssname ss i) ed (entget e))
        (if (and ed (/= (cdr (assoc 69 ed)) 1))
          (progn
            (setq w (cdr (assoc 40 ed)) h (cdr (assoc 41 ed)))
            (if (and (numberp w) (numberp h))
              (progn (setq a (* w h)) (if (> a bestarea) (setq best e bestarea a)))
            )
          )
        )
        (setq i (1+ i))
      )
      best
    )
  )
)
;; CHG1 2026-10-07: entidad con rol sin importar su tipo admitido (para el
;; diagnostico; la busqueda operativa usa GM:FindByRole por tipo).
(defun GM:RoleEntity (rol / e)
  (setq e (GM:FindByRole rol "VIEWPORT"))
  (if (not e) (setq e (GM:FindByRole rol "INSERT")))
  (if (not e) (setq e (GM:FindByRole rol "ACAD_TABLE")))
  (if (not e) (setq e (GM:FindByRole rol "LWPOLYLINE")))
  (if (not e) (setq e (GM:FindByRole rol "POLYLINE")))
  (if (not e) (setq e (GM:FindByRole rol "IMAGE")))
  e
)
;; CHG1: linea de diagnostico por rol: tipo, handle y caja medida (solo lectura).
(defun GM:RoleInfo (nombre ent / ed box typ hnd)
  (if (not (and ent (setq ed (entget ent))))
    (strcat nombre "=AUSENTE")
    (progn
      (setq typ (cdr (assoc 0 ed)) hnd (cdr (assoc 5 ed)) box (GM:LayoutEntityBox ent))
      (strcat nombre "=" typ " h=" (if hnd hnd "?")
              " caja=" (if box
                         (strcat "(" (rtos (car box) 2 1) "," (rtos (cadr box) 2 1) ")-("
                                 (rtos (nth 2 box) 2 1) "," (rtos (nth 3 box) 2 1) ")")
                         "s/d"))
    )
  )
)
;; CHG1: linea de diagnostico de una caja resuelta (rol/legacy/geometria).
(defun GM:BoxInfo (nombre box)
  (if box
    (strcat nombre "=(" (rtos (car box) 2 1) "," (rtos (cadr box) 2 1) ")-("
            (rtos (nth 2 box) 2 1) "," (rtos (nth 3 box) 2 1) ")")
    (strcat nombre "=AUSENTE"))
)
;; Diagnostico de roles para GM_VALIDAR_PLANTILLA. Retorna lista de faltantes.
(defun GM:ValidarPlantilla (/ falt vp leg mapb proyb)
  (setq falt nil)
  (setq vp (GM:FindLayoutViewport))
  (if (not vp) (setq falt (cons "FALTA_ROL:VP_PRINCIPAL (marque un viewport con GM_MARCAR_PLANTILLA o GM_MARCAR_ROL)" falt)))
  (setq leg (GM:FindLayoutLegend))
  (if (not leg) (setq falt (cons "FALTA_ROL:LEYENDA (bloque con LEYENDA en el nombre o tabla marcada con GM_MARCAR_ROL)" falt)))
  (setq mapb (GM:FindLayoutBox "167F4D" "MAP"))
  (if (not mapb) (setq falt (cons "FALTA_ROL:MAPA_GEO (marco geografico; marque con GM_MARCAR_ROL)" falt)))
  (setq proyb (GM:FindLayoutBox "15BBFB" "PROJECT"))
  (if (not proyb) (setq falt (cons "FALTA_ROL:PROYECTO (franja de proyecto; marque con GM_MARCAR_ROL)" falt)))
  (reverse falt)
)
(defun c:GM_VALIDAR_PLANTILLA (/ falt msg f mkbox mkxf mk)
  (setq falt (GM:ValidarPlantilla))
  (setq msg (strcat "DWG=" (getvar "DWGNAME") " CTAB=" (getvar "CTAB") "\n" (GM:CalibDiag)))
  ;; CHG1: detalle por rol (tipo/handle/caja) para diagnosticar plantillas nuevas.
  (setq msg (strcat msg "\n" (GM:RoleInfo "ROL_VP" (GM:FindLayoutViewport))
                          "\n" (GM:RoleInfo "ROL_LEY" (GM:FindLayoutLegend))
                          "\n" (GM:RoleInfo "ROL_MAP" (GM:RoleEntity "MAPA_GEO"))
                          "\n" (GM:RoleInfo "ROL_PROY" (GM:RoleEntity "PROYECTO"))
                          "\n" (GM:BoxInfo "MAP_BOX" (GM:FindLayoutBox "167F4D" "MAP"))
                          "\n" (GM:BoxInfo "PROY_BOX" (GM:FindLayoutBox "15BBFB" "PROJECT"))))
  ;; PRY1-MULTI F5 2026-10-14: clasificacion del modelo de mapa (MAPA_KIND):
  ;; la validacion dice de inmediato que fuente de localizacion esperar
  ;; (LAYOUT-TICKS / BLOQUE / BLOQUE-VP / XREF / SIN_MARCO) antes de un piloto.
  (if (and (boundp 'GM:MapContentBox) (boundp 'GM:MapKind) (boundp 'GM:GeoUnifiedXf))
    (progn
      (setq mkbox (vl-catch-all-apply 'GM:MapContentBox '()))
      (if (vl-catch-all-error-p mkbox) (setq mkbox nil))
      (setq mkxf (if mkbox (vl-catch-all-apply 'GM:GeoUnifiedXf (list mkbox)) nil))
      (if (vl-catch-all-error-p mkxf) (setq mkxf nil))
      (setq mk (GM:MapKind mkbox mkxf))
      (setq msg (strcat msg "\nMAPA_KIND=" mk
                        (if mkxf (strcat " XF=" (nth 4 mkxf)) "")))))
  (if falt
    (progn
      (foreach f falt (setq msg (strcat msg "\n" f)))
      (alert (strcat msg "\n\nVer SPEC_PLANTILLA_GM.md"))
      (princ (strcat "\n" msg))
    )
    (progn (alert (strcat msg "\n\nPLANTILLA OK: lista para GM_PLANOS_LOTE")) (princ "\nPLANTILLA OK")))
  (princ)
)
;; Marca roles una vez: reusa legacy D5CF2/166D8E si existen, si no usa el
;; viewport mas grande e INSERT con LEYENDA en el nombre. POR QUE incompleto:
;; MAPA_GEO/PROYECTO quedan al respaldo geometrico; GM_VALIDAR los exige igual,
;; asi que una plantilla sin esos marcos fallara en calibracion aunque marque 2/2.
(defun c:GM_MARCAR_PLANTILLA (/ vp leg ss i e ed nm n)
  (setq vp (handent "D5CF2"))
  (if (not (and vp (entget vp))) (setq vp (GM:LargestLayoutViewport)))
  (if vp (GM:SetEntRole vp "VP_PRINCIPAL"))
  (setq leg (handent "166D8E"))
  (if (not (GM:LegendCandidateOk leg))
    (progn
      (setq leg nil ss (ssget "X" (list (cons 0 "INSERT") (cons 410 (getvar "CTAB")))))
      (if ss (progn (setq i 0)
        (while (and (< i (sslength ss)) (not leg))
          (setq e (ssname ss i) ed (entget e) nm (cdr (assoc 2 ed)))
          (if (and nm (wcmatch (strcase nm) "*LEYENDA*")) (setq leg e))
          (setq i (1+ i)))))
    )
  )
  (if leg (GM:SetEntRole leg "LEYENDA"))
  ;; MAPA_GEO/PROYECTO se marcan por seleccion manual futura o quedan por
  ;; respaldo geometrico; el minimo para correr es VP+LEYENDA.
  (setq n 0)
  (if vp (setq n (1+ n))) (if leg (setq n (1+ n)))
  (alert (strcat "Roles marcados: " (itoa n) "/2 minimos (VP+LEYENDA). Re-ejecute GM_VALIDAR_PLANTILLA."))
  (princ)
)

;; CHG1 2026-10-07: tipos de entidad admitidos por rol (contrato de recepcion).
(defun GM:RoleTypeOk (rol typ)
  (cond
    ((= rol "VP_PRINCIPAL") (= typ "VIEWPORT"))
    ((= rol "LEYENDA") (member typ '("INSERT" "ACAD_TABLE")))
    ((= rol "MAPA_GEO") (member typ '("LWPOLYLINE" "POLYLINE" "INSERT" "IMAGE")))
    ((= rol "PROYECTO") (member typ '("LWPOLYLINE" "POLYLINE" "INSERT" "IMAGE")))
    (T nil)
  )
)
;; CHG1 2026-10-07: marcado explicito de un rol por seleccion (plantillas
;; nuevas donde la leyenda es una tabla ACAD_TABLE o el mapa es un bloque sin
;; nombre/deteccion geometrica). Complementa a GM_MARCAR_PLANTILLA.
;; CHG1-FIX1 2026-10-07: seleccion NUMERICA. El getkword con palabras que
;; contenian guion bajo fue rechazado en AutoCAD ("Invalid option keyword");
;; los keywords de initget no admiten '_'. 1=VP_PRINCIPAL, 2=LEYENDA,
;; 3=MAPA_GEO, 4=PROYECTO.
(defun c:GM_MARCAR_ROL (/ n rol ent ed typ ok)
  (initget 1)
  (setq n (getint "\nRol a marcar [1=VP_PRINCIPAL 2=LEYENDA 3=MAPA_GEO 4=PROYECTO]: "))
  (setq rol (cond ((= n 1) "VP_PRINCIPAL") ((= n 2) "LEYENDA") ((= n 3) "MAPA_GEO") ((= n 4) "PROYECTO") (T nil)))
  (if (not rol)
    (alert "Opcion fuera de rango: use 1 (viewport), 2 (leyenda), 3 (mapa) o 4 (proyecto).")
    (progn
      (setq ent (car (entsel (strcat "\nSeleccione la entidad para " rol ": "))))
      (if (not ent)
        (princ "\nSin entidad seleccionada.")
        (progn
          (setq ed (entget ent) typ (cdr (assoc 0 ed)))
          (if (not (= (cdr (assoc 410 ed)) (getvar "CTAB")))
            (alert (strcat "La entidad no pertenece al layout activo (" (getvar "CTAB") ")."))
            (if (not (GM:RoleTypeOk rol typ))
              (alert (strcat "Tipo no admitido para " rol ": " typ
                             ".\nAdmitidos: " (vl-princ-to-string
                               (if (= rol "VP_PRINCIPAL") '("VIEWPORT")
                                 (if (= rol "LEYENDA") '("INSERT" "ACAD_TABLE")
                                   '("LWPOLYLINE" "POLYLINE" "INSERT" "IMAGE"))))))
              (progn
                (GM:SetEntRole ent rol)
                (alert (strcat "Rol " rol " marcado en " typ " (handle " (cdr (assoc 5 ed))
                               ").\nRe-ejecute GM_VALIDAR_PLANTILLA.")))))))))
  (princ)
)

;; OPT5 2026-09-27 (F1): cache de calibracion del lote. Resuelve una sola vez
;; viewport/leyenda/marcos (rol o legacy) y reutiliza el resultado entre codigos;
;; los avisos LEGACY-MIGRADO y el CALIB_DIAG se emiten solo al resolver (primer
;; codigo). El flujo manual no cachea (gate *GM_AUTO_MODE*) y el lote reinicia la
;; cache al arrancar.
(defun GM:CalibCacheGet (key / v)
  (setq v (cdr (assoc key *GM_AUTO_CALIB_CACHE*)))
  (if v v nil)
)
(defun GM:CalibCachePut (key val)
  (if (and *GM_AUTO_MODE* key val)
    (setq *GM_AUTO_CALIB_CACHE*
      (cons (cons key val) (vl-remove (assoc key *GM_AUTO_CALIB_CACHE*) *GM_AUTO_CALIB_CACHE*))))
  val
)
(defun GM:CalibCacheEntOk (ent dtype / ed)
  (setq ed (if ent (entget ent) nil))
  (and ed (= (cdr (assoc 0 ed)) dtype) (= (cdr (assoc 410 ed)) (getvar "CTAB")))
)
;; La cache esta completa cuando los cuatro roles base ya se resolvieron y las
;; entidades siguen vigentes en el layout activo.
(defun GM:CalibCacheReady ()
  (if (and *GM_AUTO_MODE*
           (GM:CalibCacheEntOk (GM:CalibCacheGet "VP") "VIEWPORT")
           (GM:LegendCandidateOk (GM:CalibCacheGet "LEG"))
           (GM:CalibCacheGet "BOX_MAP")
           (GM:CalibCacheGet "BOX_PROJECT"))
    T nil)
)

;; Busca el viewport principal: 1) rol VP_PRINCIPAL, 2) legacy D5CF2, 3) mas grande.
;; Retorna ename o nil. No falla: el llamador decide el mensaje.
;; OPT6 2026-09-27 (F1b): la cache se lee siempre (validada por entidad+tipo+CTAB)
;; y el aviso LEGACY-MIGRADO solo se emite al resolver dentro del lote (modo auto);
;; los sondeos silenciosos de snapshots/restauracion ya no lo repiten.
(defun GM:FindLayoutViewport (/ vp)
  (setq vp (GM:CalibCacheGet "VP"))
  (if (GM:CalibCacheEntOk vp "VIEWPORT")
    vp
    (progn
      ;; 1) rol VP_PRINCIPAL
      (setq vp (GM:FindByRole "VP_PRINCIPAL" "VIEWPORT"))
      (if (GM:CalibCacheEntOk vp "VIEWPORT")
        (GM:CalibCachePut "VP" vp)
        (progn
          ;; 2) legacy D5CF2 (Huancasancos) con aviso de migracion
          (setq vp (handent "D5CF2"))
          (if (GM:CalibCacheEntOk vp "VIEWPORT")
            (progn
              (if *GM_AUTO_MODE* (GM:AutoTrace *ML_TARGET_CODE* "CALIBRACION" "AVISO" "LEGACY-MIGRADO: viewport D5CF2 sin rol; marque con GM_MARCAR_PLANTILLA"))
              (GM:CalibCachePut "VP" vp)
            )
            ;; 3) viewport real mas grande del layout
            (setq vp (GM:LargestLayoutViewport))
          )
        )
      )
      (GM:CalibCachePut "VP" vp)
      vp
    )
  )
)

;; Viewport principal para anotaciones y pie (misma resolucion que la calibracion).
(defun GM:AutoMainViewport (/ vp)
  (setq vp (GM:FindLayoutViewport))
  (if (and vp (entget vp)) vp nil)
)

;; Valida un candidato a leyenda: INSERT (plantilla Huancasancos) o
;; ACAD_TABLE (plantilla Chagual), en el layout activo y con tamano acotado
;; (<= 65 x 48). CHG1 2026-10-07: el nombre ya NO decide aqui; la politica de
;; deteccion por nombre/legacy la aplica GM:FindLayoutLegend y el rol explicito
;; (GM_MARCAR_ROL) exime del nombre. La medicion COM se libera por candidato.
(defun GM:LegendCandidateOk (ent / ed typ obj lo hi w h ok)
  (if (not (and ent (setq ed (entget ent))))
    nil
    (if (not (and (member (setq typ (cdr (assoc 0 ed))) '("INSERT" "ACAD_TABLE"))
                  (= (cdr (assoc 410 ed)) (getvar "CTAB"))))
      nil
      (progn
        ;; F15-R1: wrapper con catch y liberado antes de devolver (el barrido
        ;; de leyenda corre por codigo y fugaba uno por candidato).
        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (vl-catch-all-error-p obj)
          nil
          (progn
            (setq ok (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'lo 'hi)))))
            (if ok
              (setq lo (vlax-safearray->list lo) hi (vlax-safearray->list hi)
                    w (- (car hi) (car lo)) h (- (cadr hi) (cadr lo))))
            (GM:ReleaseObjectSafe obj)
            (if ok (and (<= w 65.0) (<= h 48.0)) nil)
          )
        )
      )
    )
  )
)

;; Busca la leyenda: 1) rol LEYENDA (INSERT o ACAD_TABLE), 2) legacy 166D8E,
;; 3) barrido por nombre (bloque con LEYENDA en el nombre, preferencia exacta).
;; Un rol marcado que no valide es fail-closed: NO se cae al legacy/barrido
;; (CHG1 2026-10-07). Prefiere la oculta (60 . 1): original de corrida cortada.
;; Retorna ename o nil.
(defun GM:FindLayoutLegend (/ legend cached role_ent ss i e ed nm best_visible exact)
  (setq cached (if *GM_AUTO_MODE* (GM:CalibCacheGet "LEG") nil))
  (if (GM:LegendCandidateOk cached)
    cached
    (progn
      (setq role_ent (GM:FindByRole "LEYENDA" "INSERT"))
      (if (not role_ent) (setq role_ent (GM:FindByRole "LEYENDA" "ACAD_TABLE")))
      (cond
        (role_ent
          (if (GM:LegendCandidateOk role_ent)
            (setq legend (GM:CalibCachePut "LEG" role_ent))
            (setq legend nil)))
        (T
          (setq legend (handent "166D8E"))
          (if (GM:LegendCandidateOk legend)
            (progn
              (if *GM_AUTO_MODE* (GM:AutoTrace *ML_TARGET_CODE* "CALIBRACION" "AVISO" "LEGACY-MIGRADO: leyenda 166D8E sin rol"))
              (GM:CalibCachePut "LEG" legend)
            )
            (progn
              (setq legend nil best_visible nil exact nil
                    ss (ssget "X" (list (cons 0 "INSERT") (cons 410 (getvar "CTAB")))))
              (if ss
                (progn
                  (setq i 0)
                  (while (< i (sslength ss))
                    (setq e (ssname ss i))
                    (if (GM:LegendCandidateOk e)
                      (progn
                        (setq ed (entget e) nm (cdr (assoc 2 ed)))
                        (if (and nm (wcmatch (strcase nm) "*LEYENDA*"))
                          (progn
                            (if (and nm (= (strcase nm) "LEYENDA DE UBICACION")) (setq exact e))
                            (if (= (cdr (assoc 60 ed)) 1)
                              (setq legend e)
                              (if (not best_visible) (setq best_visible e))
                            )
                          )
                        )
                      )
                    )
                    (setq i (1+ i))
                  )
                )
              )
              ;; Preferencia: oculta con LEYENDA, luego nombre exacto, luego visible.
              (setq legend (if legend legend (if exact exact best_visible)))
              (GM:CalibCachePut "LEG" legend)
            )
          )
        )
      )
      legend
    )
  )
)

;; BBox de una entidad del layout como (left bottom right top), o nil si no es medible.
(defun GM:LayoutEntityBox (ent / obj lo hi result)
  (if (not (and ent (entget ent)))
    nil
    (progn
      ;; F15-R1: wrapper con catch y liberado (se llama en barridos por codigo).
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p obj)
        nil
        (progn
          (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'lo 'hi))))
            (progn
              (setq lo (vlax-safearray->list lo) hi (vlax-safearray->list hi))
              (setq result (list (car lo) (cadr lo) (car hi) (cadr hi)))))
          (GM:ReleaseObjectSafe obj)
          result
        )
      )
    )
  )
)

;; Busca el marco del mapa geografico o la franja de proyecto.
;; 1) rol MAPA_GEO/PROYECTO, 2) handle legacy, 3) respaldo geometrico.
;; mode "MAP" (ancho>=150, 2<=alto<=12, el mas alto) o "PROJECT" (ancho>=100, 8<=alto<=25, el mas bajo).
;; Retorna (left bottom right top) o nil.
(defun GM:FindLayoutBox (handle mode / ent box ss i e bx w h best bestval v rol cached)
  (setq rol (if (= mode "MAP") "MAPA_GEO" "PROYECTO"))
  (setq cached (if *GM_AUTO_MODE* (GM:CalibCacheGet (strcat "BOX_" mode)) nil))
  (if (and cached (listp cached) (= (length cached) 4))
    cached
    (progn
  (setq ent (GM:FindByRole rol "LWPOLYLINE"))
  (if (not ent) (setq ent (GM:FindByRole rol "POLYLINE")))
  (if (not ent) (setq ent (GM:FindByRole rol "INSERT")))
  (if (not ent) (setq ent (GM:FindByRole rol "IMAGE")))
  (if (and ent (entget ent) (= (cdr (assoc 410 (entget ent))) (getvar "CTAB")))
    (setq box (GM:LayoutEntityBox ent))
    (progn
      (setq ent (handent handle))
      (if (and ent (entget ent) (= (cdr (assoc 410 (entget ent))) (getvar "CTAB")))
        (progn
          (if *GM_AUTO_MODE* (GM:AutoTrace *ML_TARGET_CODE* "CALIBRACION" "AVISO" (strcat "LEGACY-MIGRADO: marco " handle " sin rol")))
          (setq box (GM:LayoutEntityBox ent))
        )
        (progn
      (setq box nil best nil bestval nil
            ss (ssget "X" (list (cons 0 "LWPOLYLINE,POLYLINE,INSERT,IMAGE") (cons 410 (getvar "CTAB")))))
      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq e (ssname ss i) bx (GM:LayoutEntityBox e))
            (if bx
              (progn
                (setq w (- (nth 2 bx) (car bx)) h (- (nth 3 bx) (cadr bx)))
                (if (= mode "MAP")
                  (if (and (>= w 150.0) (>= h 2.0) (<= h 12.0))
                    (progn (setq v (nth 3 bx)) (if (or (not bestval) (> v bestval)) (setq best bx bestval v)))
                  )
                  (if (and (>= w 100.0) (>= h 8.0) (<= h 25.0) (<= (cadr bx) 60.0))
                    (progn (setq v (cadr bx)) (if (or (not bestval) (< v bestval)) (setq best bx bestval v)))
                  )
                )
              )
            )
            (setq i (1+ i))
          )
        )
      )
      (if best
        (progn
          (GM:AutoTrace *ML_TARGET_CODE* "CALIBRACION" "AVISO"
            (strcat "Handle " handle " no hallado; respaldo geometrico (" mode ")"))
          (setq box best)
        )
      )
        )
      )
    )
  )
  ;; OPT5 2026-09-27 (F1): cachear el marco resuelto (una sola resolucion por lote).
  (GM:CalibCachePut (strcat "BOX_" mode) box)
  box
    )
  )
)

;; Texto seguro para diagnostico (nunca rompe strcat).
(defun GM:DiagStr (x) (if (= (type x) 'STR) x "<nil>"))
(defun GM:DiagNum (x) (if (numberp x) (vl-princ-to-string x) "<nil>"))

;; Foto forense de la plantilla para el reporte: con esto se sabe exactamente que comparacion falla.
(defun GM:CalibDiag (/ ctab out vp vped leg leged nss)
  (setq ctab (getvar "CTAB")
        out (strcat "DWG=" (getvar "DWGNAME") " CTAB=" ctab))
  (setq vp (handent "D5CF2"))
  (if (and vp (setq vped (entget vp)))
    (setq out (strcat out " D5CF2_tipo=" (GM:DiagStr (cdr (assoc 0 vped)))
                      "_410=" (GM:DiagStr (cdr (assoc 410 vped)))
                      "_W=" (GM:DiagNum (cdr (assoc 40 vped)))
                      "_H=" (GM:DiagNum (cdr (assoc 41 vped)))
                      "_69=" (GM:DiagNum (cdr (assoc 69 vped)))))
    (setq out (strcat out " D5CF2=" (if vp "sin_DXF" "AUSENTE")))
  )
  (setq nss (ssget "X" (list (cons 0 "VIEWPORT") (cons 410 ctab))))
  (setq out (strcat out " VPs_con_410=" (if nss (itoa (sslength nss)) "0")))
  (setq leg (handent "166D8E"))
  (if (and leg (setq leged (entget leg)))
    (setq out (strcat out " LEG_tipo=" (GM:DiagStr (cdr (assoc 0 leged)))
                      "_bloque=" (GM:DiagStr (cdr (assoc 2 leged)))
                      "_410=" (GM:DiagStr (cdr (assoc 410 leged)))
                      "_60=" (GM:DiagNum (cdr (assoc 60 leged)))))
    (setq out (strcat out " LEG166D8E=" (if leg "sin_DXF" "AUSENTE")))
  )
  (setq out (strcat out " BLK_LEYENDA=" (if (tblsearch "BLOCK" "LEYENDA DE UBICACION") "SI" "NO")))
  (setq out (strcat out " ROL_VP=" (if (GM:FindByRole "VP_PRINCIPAL" "VIEWPORT") "SI" "NO")))
  (setq out (strcat out " ROL_LEY=" (if (GM:FindByRole "LEYENDA" "INSERT") "SI" "NO")))
  (setq out (strcat out " VP_W=" (GM:DiagNum (if (boundp '*GM_VP_W*) *GM_VP_W* nil))))
  out
)

(defun GM:LayoutBaseBox (handle / box mode)
  (setq mode (if (= (strcase handle) "167F4D") "MAP" "PROJECT"))
  (setq box (GM:FindLayoutBox handle mode))
  (if (not box)
    (GM:LayoutFail (strcat "FALTA_ROL:" (if (= mode "MAP") "MAPA_GEO" "PROYECTO")
      ". Ejecute GM_VALIDAR_PLANTILLA y GM_MARCAR_PLANTILLA. Ver SPEC_PLANTILLA_GM.md")))
  box
)

;; CHG3-FIX 2026-10-10: entidades que acompanan a la leyenda-tabla (WIPEOUT de
;; fondo y simbologias independientes): misma capa de la tabla o capas
;; "LEYENDA*", con caja dentro de la caja medida +margen 1. Excluye la tabla.
;; Se recolecta ANTES de copiar para no capturar la copia recien movida.
(defun GM:LayoutLegendSet (legend lo hi / lay_table ss i e ed lay box pad result)
  (setq lay_table (strcase (cdr (assoc 8 (entget legend))))
        pad 1.0 result nil
        ss (ssget "X" (list (cons 410 (getvar "CTAB")))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i) ed (entget e) lay (strcase (cdr (assoc 8 ed))))
        (if (and (not (eq e legend))
                 (or (= lay lay_table) (wcmatch lay "LEYENDA*")))
          (progn
            (setq box (GM:LayoutEntityBox e))
            (if (and box
                     (< (car box) (+ (car hi) pad)) (> (nth 2 box) (- (car lo) pad))
                     (< (cadr box) (+ (cadr hi) pad)) (> (nth 3 box) (- (cadr lo) pad)))
              (setq result (cons e result)))))
        (setq i (1+ i)))))
  (reverse result)
)

;; CHG3-FIX 2026-10-10: copia cada entidad del conjunto, la mueve con el MISMO
;; delta que la tabla (lo -> dest), ordena el dibujo (WIPEOUT al fondo, simbolos
;; al frente; la tabla copia queda en medio) y oculta el original guardando su
;; DXF para la restauracion. Fail-closed: sin leyenda ordenada no se exporta.
(defun GM:LayoutLegendSetMove (set_ents lo dest / e obj cp cp_ename ed data bad)
  (setq bad nil)
  (foreach e set_ents
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
    (if (vl-catch-all-error-p obj)
      (setq bad T)
      (progn
        (setq cp (vl-catch-all-apply 'vla-Copy (list obj)))
        (if (vl-catch-all-error-p cp)
          (setq bad T)
          (progn
            (setq cp_ename (vlax-vla-object->ename cp))
            (vl-catch-all-apply 'vla-Move (list cp (vlax-3d-point lo) (vlax-3d-point dest)))
            (setq *GM_AUTO_LEGEND_SET_COPIES* (cons cp_ename *GM_AUTO_LEGEND_SET_COPIES*))
            ;; CHG3-FIX: la copia debe quedar visible aunque el original llegara
            ;; oculto (vla-Copy hereda el estado); mismo contrato que la tabla.
            (if (not (GM:LegendSetVisibleEx cp_ename T)) (setq bad T))
            (setq ed (entget e))
            (if (= (cdr (assoc 0 ed)) "WIPEOUT")
              (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" cp_ename "" "_B"))
              (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" cp_ename "" "_F")))
            (setq data (entget e))
            (if (and data (GM:LegendSetVisible e nil))
              (setq *GM_AUTO_LEGEND_SET_DATA* (cons data *GM_AUTO_LEGEND_SET_DATA*))
              (setq bad T))))
        (GM:ReleaseObjectSafe obj)
        (GM:ReleaseObjectSafe cp)
        (setq obj nil cp nil))))
  (if bad
    (GM:LayoutFail "No se pudo mover/ocultar una simbologia de la leyenda base")
    T)
)

(defun GM:LayoutCalibrate (/ vp ed c w h x y legend obj lo hi cx z copy data mapbox projectbox tabletop kw kh legend_is_table legend_set legend_dest)
  (GM:LayoutRestoreLegend)
  ;; Contrato GM v1: zonas relativas al viewport real (SPEC_PLANTILLA_GM.md).
  ;; Offsets historicos escalados por kw/kh para compatibilidad Huancasancos.
  (setq *GM_AUTO_ZONES* nil)
  ;; Foto forense al reporte antes de buscar: revela handle/CTAB/dimensiones reales.
  ;; OPT5 2026-09-27 (F1): solo al resolver (primer codigo del lote); despues se
  ;; reutiliza la cache y se deja una traza corta en lugar de repetir la foto.
  (if (GM:CalibCacheReady)
    (GM:AutoTrace *ML_TARGET_CODE* "CALIB_CACHE" "INFO" "Calibracion reutilizada del primer codigo del lote")
    (GM:AutoTrace *ML_TARGET_CODE* "CALIB_DIAG" "INFO" (GM:CalibDiag)))
  (setq vp (GM:FindLayoutViewport))
  (if (not vp)
    (GM:LayoutFail "FALTA_ROL:VP_PRINCIPAL. Ejecute GM_VALIDAR_PLANTILLA y GM_MARCAR_PLANTILLA."))
  (setq legend (GM:FindLayoutLegend))
  (if (not legend)
    (GM:LayoutFail "FALTA_ROL:LEYENDA. No borre la leyenda base; marque con GM_MARCAR_PLANTILLA."))
  (setq ed (entget vp) c (cdr (assoc 10 ed)) w (cdr (assoc 40 ed)) h (cdr (assoc 41 ed)))
  (if (or (not (numberp w)) (not (numberp h)) (< w 50.0) (< h 50.0))
    (GM:LayoutFail "Viewport demasiado chico para el contrato GM v1 (w,h > 50)"))
  (setq *GM_VP_W* w *GM_VP_H* h)
  ;; P0-3 2026-09-29: referencias de plantilla no numericas/cero no deben dividir.
  (setq kw (if (and (numberp *GM_VP_REF_W*) (> *GM_VP_REF_W* 0.0)) (/ w *GM_VP_REF_W*) 1.0)
        kh (if (and (numberp *GM_VP_REF_H*) (> *GM_VP_REF_H* 0.0)) (/ h *GM_VP_REF_H*) 1.0))
  (setq x (- (car c) (/ w 2.0)) y (- (cadr c) (/ h 2.0)) lo nil hi nil)
  ;; P0-1 2026-09-29: bbox de leyenda con catch + release determinista.
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list legend)))
  (if (or (vl-catch-all-error-p obj) (not obj))
    (GM:LayoutFail "No se pudo abrir la leyenda para medir su bounding box"))
  (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'lo 'hi)))
    (progn
      (GM:ReleaseObjectSafe obj)
      (setq obj nil)
      (GM:LayoutFail "Bounding box de leyenda no medible")))
  (setq lo (vlax-safearray->list lo) hi (vlax-safearray->list hi)
        cx (/ (+ (car lo) (car hi)) 2.0))
  (if (or (> (- (car hi) (car lo)) 65.0) (> (- (cadr hi) (cadr lo)) 48.0)
          (< (car lo) (+ x w)) (> (car hi) (+ x w 118.0)))
    (progn
      (GM:ReleaseObjectSafe obj)
      (setq obj nil)
      (GM:LayoutFail "Bounding box de leyenda fuera del perfil calibrado")))
  (setq mapbox (GM:LayoutBaseBox "167F4D") projectbox (GM:LayoutBaseBox "15BBFB")
        tabletop (- (cadr mapbox) 4.0))
  ;; Zones are (left bottom right top). Offsets escalados por kw/kh (contrato v1).
  (setq *GM_AUTO_ZONES*
    (list
      (cons "ESQUEMA_AF" (list (+ x (* 187.0 kw)) (+ y (* 27.0 kh)) (+ x (* 257.0 kw)) (+ y (* 94.5 kh))))
      (cons "ESQUEMA_LOC" (list (+ x (* 16.0 kw)) (+ y (* 147.0 kh)) (+ x (* 96.0 kw)) (+ y (* 214.5 kh))))
      (cons "TABLAS" (list (- cx 25.0) (- tabletop 51.0) (+ cx 25.0) tabletop))
      (cons "LEYENDA" (list (- cx 32.5) (- tabletop 102.0) (+ cx 32.5) (- tabletop 54.0)))
      (cons "MEMBRETE" (list (+ x w 1.0) y (+ x w 118.0) (+ y 65.0)))
      (cons "ESCALA_VP" (list (+ x w (* -34.26 kw)) (+ y (* 4.0 kh)) (+ x w (* -3.0 kw)) (+ y (* 10.31 kh))))
      (cons "PIE_MEMBRETE" (list (+ (nth 2 projectbox) 1.0) (+ (cadr projectbox) 1.0)
                                 (+ x w 118.0) (nth 3 projectbox)))))
  (setq z (cdr (assoc "PIE_MEMBRETE" *GM_AUTO_ZONES*)))
  (if (or (< (- (nth 2 z) (car z)) 100.0) (< (- (nth 3 z) (cadr z)) 15.0)
          (>= (nth 3 z) y) (<= (cadr z) (- y 19.0))
          (< (- tabletop 102.0) (+ y 66.0)))
    (progn
      (GM:ReleaseObjectSafe obj)
      (setq obj nil)
      (GM:LayoutFail "Mapa/proyecto fuera del perfil: no hay espacio libre para leyenda y pie")))
  (setq z (cdr (assoc "LEYENDA" *GM_AUTO_ZONES*)))
  (if (> (- (cadr hi) (cadr lo)) (- (nth 3 z) (cadr z)))
    (progn
      (GM:ReleaseObjectSafe obj)
      (setq obj nil)
      (GM:LayoutFail "La leyenda excede la altura de su zona")))
  ;; CHG1 2026-10-07: la leyenda puede ser INSERT (Huancasancos) o ACAD_TABLE
  ;; (Chagual). La tabla no expone grupo 60 fiable: su ciclo usa COM.
  (setq legend_is_table (= (cdr (assoc 0 (entget legend))) "ACAD_TABLE"))
  ;; CHG3-FIX 2026-10-10: recolectar el conjunto (wipeout + simbologias) ANTES
  ;; de copiar: asi la copia recien creada no entra en la captura.
  (setq legend_set nil *GM_AUTO_LEGEND_SET_DATA* nil *GM_AUTO_LEGEND_SET_COPIES* nil)
  (if legend_is_table
    (setq legend_set (GM:LayoutLegendSet legend lo hi)))
  ;; Auto-rehabilitacion: la original pudo quedar oculta por una sesion interrumpida;
  ;; desocultarla antes de copiar para no arrastrar el estado.
  (if legend_is_table
    (progn
      (if (not (GM:LegendSetVisibleEx legend T))
        (GM:LayoutFail "No se pudo visibilizar la leyenda base (tabla)")))
    (if (= (cdr (assoc 60 (entget legend))) 1)
      (progn
        (GM:LegendSetVisible legend T)
        (GM:AutoTrace *ML_TARGET_CODE* "LEYENDA" "AVISO" "Original oculta rehabilitada"))))
  (setq copy (vla-Copy obj) *GM_AUTO_LEGEND_COPY* (vlax-vla-object->ename copy))
  ;; P0-1 2026-09-29: la original ya no se usa; soltar su wrapper (la copia vive).
  (GM:ReleaseObjectSafe obj)
  (setq obj nil)
  ;; La copia hereda visibilidad: forzarla visible en cualquier caso.
  (if (not (GM:LegendSetVisibleEx *GM_AUTO_LEGEND_COPY* T))
    (progn
      (GM:ReleaseObjectSafe copy)
      (setq copy nil)
      (GM:LayoutFail "No se pudo visibilizar la copia de leyenda")))
  ;; CHG3-FIX: el conjunto viaja al mismo destino (mismo delta) que la tabla.
  (setq legend_dest (list (car lo) (- (nth 3 z) (- (cadr hi) (cadr lo))) (caddr lo)))
  (vla-Move copy (vlax-3d-point lo) (vlax-3d-point legend_dest))
  ;; P0-1 2026-09-29: soltar el wrapper de la copia (la entidad permanece).
  (GM:ReleaseObjectSafe copy)
  (setq copy nil)
  ;; CHG3-FIX: copiar/mover el conjunto y ocultar sus originales (fail-closed).
  (if legend_is_table (GM:LayoutLegendSetMove legend_set lo legend_dest))
  (if legend_is_table
    (progn
      (if (not (GM:LegendSetVisibleEx legend nil))
        (GM:LayoutFail "No se pudo ocultar la leyenda original (tabla)"))
      (setq *GM_AUTO_LEGEND_TABLE_P* legend *GM_AUTO_LEGEND_DATA* nil))
    (progn
      (setq *GM_AUTO_LEGEND_DATA* (entget legend) data *GM_AUTO_LEGEND_DATA*)
      (if (assoc 60 data) (setq data (subst '(60 . 1) (assoc 60 data) data))
        (setq data (append data '((60 . 1)))))
      (if (not (entmod data)) (GM:LayoutFail "No se pudo ocultar la leyenda original"))
      (entupd legend)
      (setq *GM_AUTO_LEGEND_TABLE_P* nil)))
  (GM:AutoTrace *ML_TARGET_CODE* "CALIBRACION" "OK" "Contrato v1: zonas relativas a viewport/leyenda; kw/kh aplicados")
  (GM:AutoTrace *ML_TARGET_CODE* "LEYENDA_ESTADO" "INFO"
    (strcat "Original oculta, copia visible en zona"
            (if legend_is_table (strcat "; conjunto=" (itoa (length legend_set))) "")))
  T
)

(defun GM:LayoutZonePoint (role w h index mode / z top left sep)
  (setq z (cdr (assoc role *GM_AUTO_ZONES*)))
  (if (not z) (GM:LayoutFail (strcat "Zona no calibrada: " role)))
  (setq sep (if (numberp *GM_TABLE_SEP*) *GM_TABLE_SEP* 3.0))
  (setq top (- (nth 3 z) (* index (+ h sep))) left (+ (car z) (/ (- (nth 2 z) (car z) w) 2.0)))
  (if (or (< index 0) (> w (+ 0.001 (- (nth 2 z) (car z)))) (< (- top h) (- (cadr z) 0.001)))
    (progn
      (GM:AutoTrace *ML_TARGET_CODE* role "ERROR" "Capacidad de zona excedida; no se exporta un plano incompleto")
      (GM:LayoutFail (strcat "Capacidad de zona excedida: " role)))
    (if (= mode "SCHEMA")
      (list (+ left (/ w 2.0)) (- top 30.0) 0.0)
      (list left top 0.0)))
)