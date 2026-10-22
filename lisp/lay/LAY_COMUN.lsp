;; ============================================================================
;; LAY_COMUN.lsp - Sistema GM Ingenieros (modulo lay)
;; Movido SIN CAMBIOS desde LAYOUT_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================
;;; ==============================================================================
;;;  SISTEMA INDEPENDIENTE: ETIQUETADO LAYOUT - PLANOS DE UBICACI\\U+00D3N
;;;  Comando Principal: LAYOUT_UBICACION (Esquemas PU + L\\U+00F3gica Original Restaurada)
;;; ==============================================================================

(vl-load-com)

;; Version del modulo (verificada por GM_PLANOS_LOTE / GM_DIAGNOSTICO).
(setq *GM_VER_LAYOUT* nil)

;; --- VARIABLES GLOBALES DE MEMORIA ---
(if (not *ML_TARGET_CODE*) (setq *ML_TARGET_CODE* ""))
(if (not *Membrete_CSV_Path*) (setq *Membrete_CSV_Path* nil))
(if (not *GL_DATOS_CSV*) (setq *GL_DATOS_CSV* nil))
(if (vlax-ldata-get "ML_SUITE" "CSV_PATH") (setq *Membrete_CSV_Path* (vlax-ldata-get "ML_SUITE" "CSV_PATH")))
(if (not *ML_ESQ_ACTIVO*) (setq *ML_ESQ_ACTIVO* "1"))
(if (not *ML_ESQ_MODO*) (setq *ML_ESQ_MODO* "0"))
(if (not *ML_ESQ_PE_TOG*) (setq *ML_ESQ_PE_TOG* "1"))
(if (not *ML_ESQ_PE_VAL*) (setq *ML_ESQ_PE_VAL* ""))
(if (not *ML_ESQ_LOC_TIPO*) (setq *ML_ESQ_LOC_TIPO* "0"))
(if (not *GL_DATA_AFECTACIONES*) (setq *GL_DATA_AFECTACIONES* nil))
(if (not *GM_AUTO_MODE*) (setq *GM_AUTO_MODE* nil))

;; --- FUNCIONES AUXILIARES ---
;; POR QUE duplicadas: MENU_PLANOS.lsp trae las mismas para operar solo. Con el
;; orden oficial gana esta version; se mantienen ambas y no se cambia una sin la otra.
(defun count-quotes (str / cnt pos) (setq cnt 0 pos 0) (while (setq pos (vl-string-search "\"" str pos)) (setq cnt (1+ cnt) pos (1+ pos))) cnt)
(defun _str-split (str del / pos lst) (while (setq pos (vl-string-search del str)) (setq lst (cons (substr str 1 pos) lst) str (substr str (+ pos 1 (strlen del))))) (reverse (cons str lst)))
(defun _clean_val (txt) (if (= (type txt) 'STR) (progn (setq txt (vl-string-trim " \t\n\r" txt)) (if (and (> (strlen txt) 1) (= (substr txt 1 1) "\"") (= (substr txt (strlen txt) 1) "\"")) (setq txt (substr txt 2 (- (strlen txt) 2)))) (while (vl-string-search "\"\"" txt) (setq txt (vl-string-subst "\"" "\"\"" txt))) (while (vl-string-search "\n" txt) (setq txt (vl-string-subst "\\P" "\n" txt))) (vl-string-trim " " txt)) ""))
(defun str-starts-with (str pat) (if (and str pat (>= (strlen str) (strlen pat))) (= (substr str 1 (strlen pat)) pat) nil))

;; Fase B 2026-10-12: GM:AutoTrace vive en AUTOMATIZACION; si LAYOUT se carga
;; solo (kit manual sin el 4to LISP) este no-op evita "no function definition".
;; Con AUTOMATIZACION cargado, su defun real reemplaza a este no-op.
(if (not (boundp 'GM:AutoTrace))
  (defun GM:AutoTrace (code stage status message) nil))

;; No interactive fallback or invented geometry in a batch.
(defun GM:LayoutOmit (role)
  (GM:AutoTrace *ML_TARGET_CODE* role "OMITIDO" "Sin proyeccion geometrica validada; no se inventan puntos")
  nil
)
;; OPT1/BZ-OPT1 2026-09-26: liberacion determinista de wrappers COM locales (hl/hlinks/obj).
;; vlax-release-object no borra la entidad; solo suelta la referencia COM.
;; AUD1 2026-09-29: con *GM_AUTO_WRAP_DEBUG* T lleva el contador del lote
;; (misma instrumentacion que la copia de AUTOMATIZACION, gane quien gane la carga).
(defun GM:ReleaseObjectSafe (obj)
  (if (and obj (not (vl-catch-all-error-p obj)))
    (progn
      (vl-catch-all-apply 'vlax-release-object (list obj))
      (if (and (boundp '*GM_AUTO_WRAP_DEBUG*) *GM_AUTO_WRAP_DEBUG*)
        (setq *GM_AUTO_WRAP_RELEASES*
              (1+ (if (boundp '*GM_AUTO_WRAP_RELEASES*) *GM_AUTO_WRAP_RELEASES* 0)))))
  )
  nil
)
;; F15-R1 2026-09-29: aplica una propiedad COM a una entidad creando y liberando
;; el wrapper en la misma llamada. Los esquemas usaban
;; (vla-put-* (vlax-ename->vla-object X) ...) ~24 veces por codigo: cada sitio
;; fugaba un wrapper (vector del AV 0xC0000005 tras decenas de codigos).
(defun GM:EntPut (ent prop value / o res)
  (setq o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p o)
    nil
    (progn
      (setq res (vl-catch-all-apply 'vlax-put (list o prop value)))
      (GM:ReleaseObjectSafe o)
      (not (vl-catch-all-error-p res))
    )
  )
)
;; DIAG1 2026-10-03: lectura segura de Handle VLA para trazas; nunca aborta la
;; etapa (wrapper muerto o propiedad no disponible devuelve "ERR").
(defun GM:SafeHandle (o / h)
  (if o
    (progn
      (setq h (vl-catch-all-apply 'vla-get-Handle (list o)))
      (if (vl-catch-all-error-p h) "ERR" h))
    "NO")
)
;; OPT1/BZ-OPT1 2026-09-26: registro de definiciones temporales por codigo para purga
;; dirigida (TEMP_AF_/ESQ_AF_FIN_/TEMP_LO_/TEMP_LO_ZOOM_/TEMP_LO_AF_/ESQ_LO_FIN_).
;; La limpieza por handles no ve definiciones; sin registro se acumulan entre codigos.
(if (not (boundp '*GM_TEMP_BLOCKS*)) (setq *GM_TEMP_BLOCKS* nil))
(defun GM:RegisterTempBlock (name)
  (if (and name (/= name ""))
    (setq *GM_TEMP_BLOCKS* (cons name *GM_TEMP_BLOCKS*))
  )
  nil
)
(defun GM:ResetTempBlocks () (setq *GM_TEMP_BLOCKS* nil) T)

;; PRY1-MULTI F4 2026-10-14: marco unico del circulo grande del zoom del predio
;; (11 de radio + 0.6 de aire). Una sola matematica para LAY_ESQUEMAS (dibujo)
;; y GM_ROTULO (exclusion): antes estaba duplicada y divergia en el corte de
;; ancho (27.2 vs 28). center = centro del predio; mbox = marco del mapa
;; (GM:MapContentBox). Devuelve (x y 0.0); sin marco cae al respaldo historico
;; del lado izquierdo (solo si hay center).
;; ROT1 2026-10-21: el circulo grande baja a 9.9 de radio, asi que el margen
;; de sujetamiento baja de 11.6 a 10.1 (9.9 + 0.2 de aire). El desplazamiento
;; lateral (+/-25) NO cambia: el conjunto mantiene la composicion del plano.
(defun GM:ZoomFrameFor (center mbox / midx side cx1 cx2 cy1 cy2 frame)
  (if (and center mbox)
    (progn
      (setq midx (/ (+ (car mbox) (nth 2 mbox)) 2.0)
            side (if (< (car center) midx) 25.0 -25.0)
            cx1 (+ (car mbox) 2.0) cx2 (- (nth 2 mbox) 2.0)
            cy1 (+ (cadr mbox) 2.0) cy2 (- (nth 3 mbox) 2.0)
            frame (list (+ (car center) side) (+ (cadr center) 5.0) 0.0))
      (if (> (- cx2 cx1) 24.0)
        (setq frame (list (max (+ cx1 10.1) (min (- cx2 10.1) (car frame))) (cadr frame) 0.0)))
      (if (> (- cy2 cy1) 24.0)
        (setq frame (list (car frame) (max (+ cy1 10.1) (min (- cy2 10.1) (cadr frame))) 0.0)))
      frame)
    (if center (list (- (car center) 25.0) (+ (cadr center) 5.0) 0.0) nil))
)

(defun GM:GetLayoutPoint (message /)
  (if *GM_AUTO_MODE* (GM:LayoutOmit message) (getpoint message))
)

(defun GM:SchemaAnchor (ent base dest scale / point)
  ;; The inset uses this same translation and uniform scale for its copied curves.
  (setq point (if ent (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list ent base))))
  (if (and point (not (vl-catch-all-error-p point)))
    (progn
      (setq point (list (+ (car dest) (* scale (- (car point) (car base))))
                        (+ (cadr dest) (* scale (- (cadr point) (cadr base)))) 0.0))
      (if (and (< (abs (- (car point) (car dest))) 39.0)
               (< (abs (- (cadr point) (cadr dest))) 29.0))
        point
        (GM:LayoutOmit "ANCLA_ESQUEMA_FUERA_DEL_RECORTE")))
    (GM:LayoutOmit "ANCLA_ESQUEMA_SIN_CURVA")))

;; MOTOR DE HIPERVINCULOS
(defun ML_GetHyperlinkString (obj / hlinks cnt i hl match url desc)
  (setq match nil)
  (if (not (vl-catch-all-error-p (setq hlinks (vl-catch-all-apply 'vla-get-Hyperlinks (list obj)))))
    (progn
      ;; AUD1: Count e Item con catch (entidad muerta a mitad del escaneo ya no
      ;; aborta el lote con hlinks vivo).
      (setq cnt (vl-catch-all-apply 'vla-get-Count (list hlinks)) i 0)
      (if (or (vl-catch-all-error-p cnt) (not (numberp cnt))) (setq cnt 0))
      (while (and (< i cnt) (not match))
        (setq hl (vl-catch-all-apply 'vla-Item (list hlinks i)))
        (if (vl-catch-all-error-p hl) (setq hl nil))
        (setq url (if hl (vl-catch-all-apply 'vla-get-URL (list hl)) nil))
        (if (or (vl-catch-all-error-p url) (not url) (= url "#")) (setq url ""))
        (setq desc (vl-catch-all-apply 'vla-get-URLDescription (list hl)))
        (if (or (vl-catch-all-error-p desc) (not desc) (= desc "#")) (setq desc ""))
        (setq url (vl-string-trim " \t\n\r" url) desc (vl-string-trim " \t\n\r" desc))
        (if (/= desc "") (setq match desc) (if (/= url "") (setq match url)))
        (GM:ReleaseObjectSafe hl)
        (setq hl nil i (1+ i))
      )
      (GM:ReleaseObjectSafe hlinks)
      (setq hlinks nil)
    )
  )
  match
)

;; Fase B 2026-10-12: el motor UBI_* vive en PLANO_UBICACION; si LAYOUT se
;; cargara sin ese modulo, este alias condicionado evita "no function
;; definition" (mismo lector local; con PLANO cargado no se redefine nada).
(if (not (boundp 'UBI_GetHyperlinkString))
  (defun UBI_GetHyperlinkString (obj) (ML_GetHyperlinkString obj)))

;; Cuenta geometria vinculada al codigo de via para el modo de lote.
;; El modo manual conserva sus cantidades introducidas por el usuario.
(defun LUBI_CountLinkedEntities (kind / ss i ent vobj hl hl_up result)
  (setq result 0)
  (setq ss (ssget "X" (list (cons 0 "LINE,LWPOLYLINE,POLYLINE") (cons 410 "Model"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        ;; AUD1: creacion con catch; entidad caida = se salta.
        (setq vobj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (vl-catch-all-error-p vobj) (setq vobj nil))
        (setq hl (if vobj (ML_GetHyperlinkString vobj) nil)
              hl_up (if hl (strcase (vl-string-trim " \t\n\r" hl)) ""))
        (GM:ReleaseObjectSafe vobj)
        (setq vobj nil)
        (if (or (and (= kind "DV")
                     (vl-string-search "DERECHO" hl_up)
                     (vl-string-search "VIA" hl_up))
                (and (= kind "EJE")
                     (vl-string-search "EJE" hl_up)
                     (vl-string-search "VIA" hl_up)))
          (setq result (1+ result))
        )
        (setq i (1+ i))
      )
    )
  )
result
)

;; ==============================================================================
;; HELPERS AUTOM\\U+00C1TICOS: Proyecci\\U+00F3n Model?Paper y Anclas para Lote
;; ==============================================================================

;; Contrato GM v1 en LAYOUT (autocontenido para flujo manual sin AUTO cargado).
;; AUTO redefine identico al cargar al final; misma semantica.
(defun GM:EntRole (ent / r)
  (setq r (vl-catch-all-apply 'vlax-ldata-get (list ent "GM_ROL")))
  (if (or (vl-catch-all-error-p r) (not (= (type r) 'STR))) nil r)
)
(defun GM:FindByRole (rol dtype / ss i e)
  (setq ss (ssget "X" (list (cons 0 dtype) (cons 410 (getvar "CTAB")))))
  (if (not ss) nil
    (progn (setq i 0 e nil)
      (while (and (< i (sslength ss)) (not e))
        (if (= (GM:EntRole (ssname ss i)) rol) (setq e (ssname ss i)))
        (setq i (1+ i)))
      e))
)
(defun GM:LargestLayoutViewport (/ ss i e ed best bestarea w h a)
  (setq ss (ssget "X" (list (cons 0 "VIEWPORT") (cons 410 (getvar "CTAB")))))
  (if (not ss) nil
    (progn (setq i 0 best nil bestarea 0.0)
      (while (< i (sslength ss))
        (setq e (ssname ss i) ed (entget e))
        (if (and ed (/= (cdr (assoc 69 ed)) 1))
          (progn (setq w (cdr (assoc 40 ed)) h (cdr (assoc 41 ed)))
            (if (and (numberp w) (numberp h))
              (progn (setq a (* w h)) (if (> a bestarea) (setq best e bestarea a))))))
        (setq i (1+ i)))
      best))
)
;; Obtiene el viewport principal del lote por contrato GM v1:
;; 1) rol VP_PRINCIPAL, 2) legacy D5CF2, 3) viewport real mas grande.
;; Sin dimensiones fijas (SPEC_PLANTILLA_GM.md).
(defun GM:GetMainViewport ( / vp ed ss i e w h best bestarea a)
  (setq vp (GM:FindByRole "VP_PRINCIPAL" "VIEWPORT"))
  (if (and vp (entget vp)
           (= (cdr (assoc 0 (entget vp))) "VIEWPORT")
           (= (cdr (assoc 410 (entget vp))) (getvar "CTAB")))
    vp
    (progn
      (setq vp (handent "D5CF2"))
      (if (and vp (entget vp)
               (= (cdr (assoc 0 (entget vp))) "VIEWPORT")
               (= (cdr (assoc 410 (entget vp))) (getvar "CTAB")))
        vp
        (GM:LargestLayoutViewport)
      )
    ))
)

;; DCS/PSDCS requieren el viewport realmente activo. Cambiar CVPORT desde
;; Paper (1) no basta: entrar a MSpace antes y restaurar al terminar.
;; F15-R1: doc/app se liberan SIEMPRE (la ruta se ejecuta 100-300 veces por
;; codigo; eran los wrappers mas fugados del lote y el vector del AV 0xC0000005).
;; HOTFIX 2026-10-20 (AUDIT4-F): unwind-protect NO existe en AutoLISP (es
;; Common Lisp). En AutoCAD 2021 la llamada abortaba la proyeccion con
;; "no function definition: UNWIND-PROTECT" y todos los planos caian en
;; LAYOUT;ERROR. Se reemplaza por vl-catch-all-apply + liberacion
;; garantizada: doc/app se sueltan SIEMPRE (salida feliz, error o fallo de
;; viewport) y el error viaja como texto para que el llamante lo formatee
;; una sola vez (sin exit anidado que "Proyeccion: quit / exit abort").
;; El nucleo va en una funcion aparte (no lambda citada): asi el catch-all
;; no depende del alcance dinamico y el arnés de QA lo ejecuta de verdad.
(defun GM:ProjectPointCore (point vp inverse doc / vpn)
  (setq vpn (cdr (assoc 69 (entget vp))))
  (vla-put-MSpace doc :vlax-true)
  (setvar "CVPORT" vpn)
  (if (/= (getvar "CVPORT") vpn)
    (list 'GM_FAIL "Proyeccion: no se activo el viewport de proyeccion")
    (if inverse
      (trans (trans point 3 2) 2 0)
      (trans (trans point 0 2) 2 3))))

(defun GM:ProjectPointWork (point vp inverse / app doc err)
  (setq app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (vl-catch-all-error-p app) (setq app nil))
  (setq doc (if app (vl-catch-all-apply 'vla-get-ActiveDocument (list app)) nil))
  (if (vl-catch-all-error-p doc) (setq doc nil))
  (setq err
    (if (not doc)
      "Proyeccion: sin ActiveDocument"
      (vl-catch-all-apply 'GM:ProjectPointCore (list point vp inverse doc))))
  (GM:ReleaseObjectSafe doc)
  (GM:ReleaseObjectSafe app)
  (cond
    ((vl-catch-all-error-p err)
     (strcat "Proyeccion: " (vl-catch-all-error-message err)))
    ((and (listp err) (eq (car err) 'GM_FAIL)) (cadr err))
    (T err)))

(defun GM:ProjectPoint (point inverse / vp saved_tab saved_cv projected restored)
  (setq saved_tab (getvar "CTAB") saved_cv (getvar "CVPORT") vp (GM:GetMainViewport))
  (if (and vp point (numberp (car point)) (numberp (cadr point)))
    (progn
      (setq projected (vl-catch-all-apply 'GM:ProjectPointWork (list point vp inverse)))
      (setq restored (vl-catch-all-apply 'UBI_RestoreSpace (list saved_tab saved_cv)))
      (if (vl-catch-all-error-p restored) (GM:LayoutFail "No se restauro Paper despues de proyectar"))
      (cond
        ((vl-catch-all-error-p projected)
         (GM:LayoutFail (strcat "Proyeccion: " (vl-catch-all-error-message projected))))
        ;; AutoLISP no tiene stringp: el tipo de una cadena es 'STR.
        ((= (type projected) 'STR) (GM:LayoutFail projected))
        (T projected)))
    nil))

(defun GM:ModelToPaper (point) (GM:ProjectPoint point nil))
(defun GM:PaperToModel (point) (GM:ProjectPoint point T))

;; AU: activar/restaurar el viewport una vez por curva, no por muestra.
;; F15-R1: doc/app liberados al salir (una llamada por lote de puntos).
(defun GM:ProjectPointsWork (points vp / app doc vpn result point)
  (setq app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (vl-catch-all-error-p app) (setq app nil))
  (setq doc (if app (vl-catch-all-apply 'vla-get-ActiveDocument (list app)) nil))
  (if (vl-catch-all-error-p doc) (setq doc nil))
  (if (not doc)
    (progn
      ;; HOTFIX 2026-10-20: se liberan antes de abortar (mismo criterio que
      ;; GM:ProjectPointWork: doc/app nunca quedan vivos en la ruta de error).
      (GM:ReleaseObjectSafe doc)
      (GM:ReleaseObjectSafe app)
      (GM:LayoutFail "Proyeccion vial: sin ActiveDocument")))
  (setq vpn (cdr (assoc 69 (entget vp))))
  (vla-put-MSpace doc :vlax-true)
  (setvar "CVPORT" vpn)
  (if (/= (getvar "CVPORT") vpn)
    (progn
      (GM:ReleaseObjectSafe doc)
      (GM:ReleaseObjectSafe app)
      (GM:LayoutFail "No se activo el viewport de proyeccion")))
  (foreach point points
    ;; Una curva puede devolver nil en un parametro extremo. Separa ese tramo,
    ;; pero no cancela toda la deteccion de vias del propietario.
    (setq result (cons
      (if (and point (numberp (car point)) (numberp (cadr point)))
        (vl-catch-all-apply '(lambda (p) (trans (trans p 0 2) 2 3)) (list point))
        nil)
      result)))
  (setq result (mapcar '(lambda (p) (if (vl-catch-all-error-p p) nil p)) result))
  (GM:ReleaseObjectSafe doc)
  (GM:ReleaseObjectSafe app)
  (reverse result))

(defun GM:ModelPointsToPaper (points / vp saved_tab saved_cv projected restored)
  (setq saved_tab (getvar "CTAB") saved_cv (getvar "CVPORT") vp (GM:GetMainViewport))
  (if (not vp) (GM:LayoutFail "Sin viewport para proyeccion vial"))
  (setq projected (vl-catch-all-apply 'GM:ProjectPointsWork (list points vp))
        restored (vl-catch-all-apply 'UBI_RestoreSpace (list saved_tab saved_cv)))
  (if (vl-catch-all-error-p restored) (GM:LayoutFail "No se restauro Paper despues de proyectar vias"))
  (if (vl-catch-all-error-p projected)
    (GM:LayoutFail (strcat "Proyeccion vial: " (vl-catch-all-error-message projected)))
    projected))

;; Obtiene ancla Paper real sobre una curva del Model visible en el viewport principal
;; ent: entidad Model (curva/polil\\U+00EDnea); base: punto WCS de referencia en la curva
;; Retorna punto Paper o llama GM:LayoutFail
(defun GM:AnnotationAnchor (ent base_wcs / pt_wcs pt_paper vla_o)
  (if (not (and ent (entget ent)))
    (GM:LayoutFail "GM:AnnotationAnchor: entidad inv\\U+00E1lida"))
  ;; FIX-AV: wrapper nominal liberado tras medir (una sola entidad).
  (setq vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p vla_o) (setq vla_o nil))
  (setq pt_wcs (if vla_o (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla_o base_wcs)) nil))
  (GM:ReleaseObjectSafe vla_o)
  (if (or (not pt_wcs) (vl-catch-all-error-p pt_wcs))
    (GM:LayoutFail "GM:AnnotationAnchor: no se pudo obtener punto sobre curva"))
  (setq pt_paper (GM:ModelToPaper pt_wcs))
  (if (not pt_paper)
    (GM:LayoutFail "GM:AnnotationAnchor: fallo proyecci\\U+00F3n Model->Paper"))
  pt_paper
)

;; E4: verifica si un punto Paper cae dentro del viewport principal (con margen).
;; Retorna T si visible, nil si fuera del recorte o sin viewport.
(defun GM:IsPaperPointInMainViewport (pt_paper / vp ed c w h x y)
  (if (not (and pt_paper (numberp (car pt_paper)) (numberp (cadr pt_paper))))
    nil
    (progn
      (setq vp (GM:GetMainViewport))
      (if (not vp)
        nil
        (progn
          (setq ed (entget vp) c (cdr (assoc 10 ed)) w (cdr (assoc 40 ed)) h (cdr (assoc 41 ed)))
          (if (not (and c (numberp w) (numberp h))) nil
            (progn
              (setq x (- (car c) (/ w 2.0)) y (- (cadr c) (/ h 2.0)))
              (and (>= (car pt_paper) x) (<= (car pt_paper) (+ x w))
                   (>= (cadr pt_paper) y) (<= (cadr pt_paper) (+ y h)))
            )
          )
        )
      )
    )
  )
)

;; E4: ancla visible sobre una curva: prueba inicio/cuartos/medio/fin y devuelve el primer
;; punto Paper que caiga dentro del viewport; nil si ninguno es visible/recortado.
(defun GM:VisibleAnchorOnCurve (ent / vla_o end_p params p frac pt_wcs pt_paper)
  (if (not (and ent (entget ent)))
    nil
    (progn
      ;; AUD1: creacion con catch; entidad caida => end_p falla y retorna nil.
      (setq vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p vla_o) (setq vla_o nil))
      (setq end_p (vl-catch-all-apply 'vlax-curve-getEndParam (list vla_o)))
      (if (or (vl-catch-all-error-p end_p) (not (numberp end_p)))
        (progn (GM:ReleaseObjectSafe vla_o) nil)
        (progn
          (setq params (list 0.0 (/ end_p 4.0) (/ end_p 2.0) (/ (* end_p 3.0) 4.0) end_p)
                pt_paper nil)
          (while (and params (not pt_paper))
            (setq frac (car params) params (cdr params))
            (setq pt_wcs (vl-catch-all-apply 'vlax-curve-getPointAtParam (list vla_o frac)))
            (if (and pt_wcs (not (vl-catch-all-error-p pt_wcs)))
              (progn
                (setq pt_wcs (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla_o pt_wcs)))
                (if (and pt_wcs (not (vl-catch-all-error-p pt_wcs)))
                  (progn
                    (setq pt_paper (GM:ModelToPaper pt_wcs))
                    (if (and pt_paper (not (GM:IsPaperPointInMainViewport pt_paper)))
                      (setq pt_paper nil)
                    )
                  )
                )
              )
            )
          )
          pt_paper
          ;; FIX-AV: soltar el wrapper de la curva antes de retornar.
          (GM:ReleaseObjectSafe vla_o)
          (setq vla_o nil)
          pt_paper
        )
      )
    )
  )
)

;; Rectangulo util del viewport principal en Paper como (l b r t); nil si no hay.
(defun GM:MainViewportRect (/ vp ed c w h)
  (setq vp (GM:GetMainViewport))
  (if (not (and vp (setq ed (entget vp))))
    nil
    (progn
      (setq c (cdr (assoc 10 ed)) w (cdr (assoc 40 ed)) h (cdr (assoc 41 ed)))
      (if (not (and c (numberp w) (numberp h)))
        nil
        (list (- (car c) (/ w 2.0)) (- (cadr c) (/ h 2.0))
              (+ (car c) (/ w 2.0)) (+ (cadr c) (/ h 2.0)))))))
;; Holgura entre la huella (ax1 ay1 ax2 ay2) y la caja b: maxima separacion
;; direccional; negativa si se solapan.
(defun GM:GapToBox (ax1 ay1 ax2 ay2 b)
  (max (- (car b) ax2) (- ax1 (nth 2 b)) (- (cadr b) ay2) (- ay1 (nth 3 b))))
;; Roles criticos: la caja nunca los pisa y el leader tampoco puede cruzarlos.
;; OPT10 2026-09-27 (F5): ESQUEMA_AF/ESQUEMA_LOC entran aqui; ninguna flecha
;; externa (AF, propietario, viales) puede atravesar los esquemas. Las lineas
;; internas de los esquemas no pasan por este validador y quedan exentas.
(defun GM:CriticalRole (role)
  (or (= role "MEMBRETE") (= role "PIE_MEMBRETE") (= role "ESCALA_VP")
      (= role "ESQUEMA_AF") (= role "ESQUEMA_LOC")))
;; Muestras del borde para separacion contra el poligono real (no su bbox):
;; puntos Paper del contorno + distancia minima al cuadrado exigida.
(setq *GM_LABEL_SAMPLES* nil *GM_LABEL_DIST2* nil)
;; N puntos del contorno proyectados a Paper; salta los no proyectables.
(defun GM:PaperBoundarySamples (ent n / vla_o end_p i frac pt_wcs pt_paper out)
  (setq out nil)
  (if (and ent (entget ent) (numberp n) (> n 2))
    (progn
      ;; CHUNK 2026-09-29: creacion del wrapper con catch; una entidad caida
      ;; devuelve lista vacia en vez de abortar el grid.
      (setq vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p vla_o)
        (setq vla_o nil))
      (setq end_p (if vla_o (vl-catch-all-apply 'vlax-curve-getEndParam (list vla_o)) nil))
      (if (and (not (vl-catch-all-error-p end_p)) (numberp end_p) (> end_p 0.0))
        (progn
          (setq i 0)
          (while (< i n)
            (setq frac (* end_p (/ (float i) (float n))))
            (setq pt_wcs (vl-catch-all-apply 'vlax-curve-getPointAtParam (list vla_o frac)))
            (if (and pt_wcs (not (vl-catch-all-error-p pt_wcs)))
              (progn
                (setq pt_wcs (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla_o pt_wcs)))
                (if (and pt_wcs (not (vl-catch-all-error-p pt_wcs)))
                  (progn
                    (setq pt_paper (vl-catch-all-apply 'GM:ModelToPaper (list pt_wcs)))
                    (if (and pt_paper (not (vl-catch-all-error-p pt_paper))
                             (numberp (car pt_paper)) (numberp (cadr pt_paper)))
                      (setq out (cons pt_paper out)))))))
            (setq i (1+ i)))))))
  ;; FIX-AV: soltar el wrapper del contorno antes de retornar.
  (GM:ReleaseObjectSafe vla_o)
  out)
;; T si la huella queda a la distancia exigida de todas las muestras.
(defun GM:SamplesClear (ax1 ay1 ax2 ay2 / need ok s dx dy)
  (setq need (if (numberp *GM_LABEL_DIST2*) *GM_LABEL_DIST2* nil) ok T)
  (if (and need *GM_LABEL_SAMPLES*)
    (foreach s *GM_LABEL_SAMPLES*
      (if ok
        (progn
          (setq dx (max (- ax1 (car s)) 0.0 (- (car s) ax2))
                dy (max (- ay1 (cadr s)) 0.0 (- (cadr s) ay2)))
          (if (< (+ (* dx dx) (* dy dy)) need) (setq ok nil))))))
  ok)
;; Cajas extra a evitar al aterrizar etiquetas (p.ej. bbox Paper del predio
;; se\\U+00F1alado): se tratan como obstaculos junto a zonas y cajas colocadas.
(setq *GM_LABEL_EXCLUDE* nil)
;; BBox Paper de una entidad Model (4 esquinas proyectadas) expandida con
;; margen; nil si no proyectable. Para que el texto no caiga sobre el predio.
(defun GM:PaperBBoxOfEnt (ent margin / vla_o lo hi corners pt pp xs ys)
  (if (not (and ent (entget ent) (numberp margin)))
    nil
    (progn
      (setq vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p vla_o)
        nil
        (progn
          (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list vla_o 'lo 'hi)))
            ;; FIX-AV: soltar el wrapper tambien en la rama de fallo.
            (progn (GM:ReleaseObjectSafe vla_o) nil)
            (progn
              ;; FIX-AV: el wrapper ya no se necesita tras extraer la caja.
              (GM:ReleaseObjectSafe vla_o)
              (setq vla_o nil)
              (setq lo (vlax-safearray->list lo) hi (vlax-safearray->list hi)
                    corners (list lo hi (list (car lo) (cadr hi) 0.0) (list (car hi) (cadr lo) 0.0))
                    xs nil ys nil)
              (foreach pt corners
                (setq pp (vl-catch-all-apply 'GM:ModelToPaper (list pt)))
                (if (and pp (not (vl-catch-all-error-p pp)) (numberp (car pp)) (numberp (cadr pp)))
                  (setq xs (cons (car pp) xs) ys (cons (cadr pp) ys))))
              (if (and xs ys)
                (list (- (apply 'min xs) margin) (- (apply 'min ys) margin)
                      (+ (apply 'max xs) margin) (+ (apply 'max ys) margin))
                nil))))))))
;; Holgura minima de una caja contra zonas calibradas y cajetines colocados.
;; Iteradores localizados POR QUE: sin declarar, foreach los filtraba al entorno
;; dinamico y podia pisar variables del llamador.
(defun GM:BoxClearance (ax1 ay1 ax2 ay2 / cl zone_pair placed_box excl_box)
  (setq cl 1000000.0)
  (foreach zone_pair *GM_AUTO_ZONES*
    (setq cl (min cl (GM:GapToBox ax1 ay1 ax2 ay2 (cdr zone_pair)))))
  (foreach placed_box *GM_PLACED_BOXES*
    (setq cl (min cl (GM:GapToBox ax1 ay1 ax2 ay2 placed_box))))
  (if *GM_LABEL_EXCLUDE*
    (foreach excl_box *GM_LABEL_EXCLUDE*
      (setq cl (min cl (GM:GapToBox ax1 ay1 ax2 ay2 excl_box)))))
  cl)
;; Nombre del primer obstaculo que toca la caja; "" si esta libre.
(defun GM:BoxBlocker (ax1 ay1 ax2 ay2 / name zone_pair placed_box excl_box)
  (setq name "")
  (foreach zone_pair *GM_AUTO_ZONES*
    (if (and (= name "") (< (GM:GapToBox ax1 ay1 ax2 ay2 (cdr zone_pair)) 0))
      (setq name (strcat "Z:" (car zone_pair)))))
  (foreach placed_box *GM_PLACED_BOXES*
    (if (and (= name "") (< (GM:GapToBox ax1 ay1 ax2 ay2 placed_box) 0))
      (setq name "BOX")))
  (if *GM_LABEL_EXCLUDE*
    (foreach excl_box *GM_LABEL_EXCLUDE*
      (if (and (= name "") (< (GM:GapToBox ax1 ay1 ax2 ay2 excl_box) 0))
        (setq name "PREDIO"))))
  (if (and (= name "") *GM_LABEL_SAMPLES* (numberp *GM_LABEL_DIST2*)
           (not (GM:SamplesClear ax1 ay1 ax2 ay2)))
    (setq name "BORDE"))
  name)
;; OPT10 (F5): interseccion EXACTA segmento-rectangulo (Liang-Barsky) con
;; margen minimo. El AABB anterior rechazaba diagonales que solo rozaban la
;; esquina y dejaba pasar el contacto real; aqui el contacto de borde cuenta
;; como choque (el margen cubre el redondeo flotante).
(defun GM:SegHitsBox (x1 y1 x2 y2 box margin / eps xo1 yo1 xo2 yo2 dx dy t0 t1 ok p q tmp)
  (setq eps (if (numberp margin) margin 0.01)
        xo1 (- (car box) eps) yo1 (- (cadr box) eps)
        xo2 (+ (nth 2 box) eps) yo2 (+ (nth 3 box) eps)
        dx (- x2 x1) dy (- y2 y1) t0 0.0 t1 1.0 ok T)
  (foreach pq (list (list (- 0 dx) (- x1 xo1)) (list dx (- xo2 x1))
                    (list (- 0 dy) (- y1 yo1)) (list dy (- yo2 y1)))
    (if ok
      (progn
        (setq p (car pq) q (cadr pq))
        (if (= p 0.0)
          (if (< q 0.0) (setq ok nil))
          (progn
            (setq tmp (/ q p))
            (if (< p 0.0)
              (if (> tmp t1) (setq ok nil) (if (> tmp t0) (setq t0 tmp)))
              (if (< tmp t0) (setq ok nil) (if (< tmp t1) (setq t1 tmp)))))))))
  ok)
;; Primer rol critico que bloquea el segmento p1->(x2,y2); nil si pasa.
(defun GM:LeaderBlocker (p1 x2 y2 / res zone_pair)
  (setq res nil)
  (foreach zone_pair *GM_AUTO_ZONES*
    (if (and (not res) (GM:CriticalRole (car zone_pair))
             (GM:SegHitsBox (car p1) (cadr p1) x2 y2 (cdr zone_pair) 0.01))
      (setq res (car zone_pair))))
  res)
;; El leader (segmento) solo debe evitar roles criticos: es una linea fina y en
;; el plano objetivo cruza contenido libremente.
(defun GM:LeaderClear (p1 x2 y2)
  (if (GM:LeaderBlocker p1 x2 y2) nil T))
;; Primer rol critico que bloquea cualquiera de los tramos de una ruta; nil si pasa.
(defun GM:LeaderRouteBlocker (pts / res rest a b)
  (setq res nil rest pts)
  (while (and rest (cdr rest) (not res))
    (setq a (car rest) b (cadr rest) rest (cdr rest))
    (setq res (GM:LeaderBlocker a (car b) (cadr b))))
  res)
;; OPT10 (F5): la ruta completa (no solo la diagonal nominal) debe pasar.
(defun GM:LeaderRouteClear (pts)
  (if (GM:LeaderRouteBlocker pts) nil T))
;; OPT10 (F5): ruta real del leader suelto (AF/propietario): p1 -> codo (extremo
;; del texto mas cercano al ancla) -> extremo opuesto. Misma eleccion que
;; GM:DrawAfectacionWithWidth y GM:DrawPropietarioSuelto.
(defun GM:LeaderRouteFree (p1 p2 box_w / p3)
  (setq p3 (list (+ (car p2) box_w) (cadr p2) 0.0))
  (if (<= (abs (- (car p1) (car p3))) (abs (- (car p1) (car p2))))
    (list p1 p3 p2)
    (list p1 p2 p3)))
;; OPT10 (F5): ruta real del leader con caja (viales): p1 -> codo -> amarre,
;; reutilizando GM:LeaderAttach/GM:LeaderElbow igual que GM:DrawLeaderBox. Los
;; rotulos de via (2 lineas x 1.8) nunca hacen crecer la caja (h_up+h_dn=10).
(defun GM:LeaderRouteBoxed (p1 p2 w up dn / bl tr attach elbow)
  (setq bl (list (car p2) (- (cadr p2) dn) 0.0)
        tr (list (+ (car p2) w) (+ (cadr p2) up) 0.0)
        attach (GM:LeaderAttach bl tr p1)
        elbow (GM:LeaderElbow p1 attach))
  (list p1 elbow attach))
;; Separacion estandar etiqueta-predio (unidades Paper): distancia base entre el
;; ancla y el aterrizaje. 5.0 por defecto; el propietario usa 15.0.
(setq *GM_LANDING_GAP* 5.0)
;; Barrido AF en todo el viewport (Revision AM): rejilla sobre el rectangulo
;; util con puntaje = aire + lejania al propietario - largo de flecha.
;; El sur es ultimo recurso. Editables sin reeditar codigo; en campo, antes
;; del lote: (setq *GM_AF_SCAN_STEP* "8.0").
;; La flecha larga puede cruzar predios y via; solo evita roles criticos
;; (MEMBRETE/PIE_MEMBRETE/ESCALA_VP).
(if (not *GM_AF_SCAN_STEP*) (setq *GM_AF_SCAN_STEP* "10.0"))
(if (not *GM_AF_MIN*) (setq *GM_AF_MIN* "3.0"))
(if (not *GM_AF_SOUTH_FRAC*) (setq *GM_AF_SOUTH_FRAC* "0.35"))
(if (not *GM_AF_AIR_W*) (setq *GM_AF_AIR_W* "2.0"))
(if (not *GM_AF_FAR_W*) (setq *GM_AF_FAR_W* "0.001"))
;; *GM_AF_LEADER_W* retirado: se configuro pero ningun puntaje lo lee.
;; Veredicto forense del barrido (lo compone BestAnchorLandingAF).
(if (not *GM_AF_SCAN_DIAG*) (setq *GM_AF_SCAN_DIAG* ""))
;; Numero desde variable editable (numero directo o cadena); default si vacio/invalido.
(defun GM:NumVar (val default / n)
  (if (numberp val) val
    (progn
      (setq n (vl-catch-all-apply 'atof (list (if val val ""))))
      (if (and (not (vl-catch-all-error-p n)) (numberp n) (> n 0.0)) n default))))
;; Candidatos (etiqueta x y): derecha, izquierda y variantes arriba/abajo.
(defun GM:LandingCandidates (p1 w up dn / x1 y1 gap step)
  (setq x1 (car p1) y1 (cadr p1)
        gap (if (numberp *GM_LANDING_GAP*) *GM_LANDING_GAP* 5.0)
        step (+ up dn 4.0))
  (list (list "D" (+ x1 w gap) y1) (list "I" (- x1 w gap) y1)
        (list "D+" (+ x1 w gap) (+ y1 step)) (list "D-" (+ x1 w gap) (- y1 step))
        (list "I+" (- x1 w gap) (+ y1 step)) (list "I-" (- x1 w gap) (- y1 step))))
;; Caja Paper estimada del rotulo de propietario (obstaculo BLANDO del AF):
;; mismo ancho/alto del dibujo (GM:PropBoxWidth), centrada bajo la matriz con
;; gap 15 como PropLowerLanding. Nil sin datos (el puntaje usa aire+flecha).
(defun GM:PropEstimateBox (/ row nombre condicion w up dn matbox cx x1 x2 y2 y1)
  (setq row *GL_DATOS_CSV*
        nombre (if (and row (nth 0 row)) (nth 0 row) "SIN NOMBRE")
        condicion (if (and row (nth 2 row)) (nth 2 row) "")
        w (GM:PropBoxWidth nombre condicion)
        up (+ 1.7 (* 2.65 (length (GM:PropNameLines nombre))))
        dn 5.0
        matbox (if (and *GM_LABEL_EXCLUDE* (cadr *GM_LABEL_EXCLUDE*)) (cadr *GM_LABEL_EXCLUDE*) nil))
  (if (and (numberp w) (> w 0.0) matbox (numberp (car matbox)) (numberp (cadr matbox)))
    (progn
      (setq cx (/ (+ (car matbox) (nth 2 matbox)) 2.0)
            x1 (- cx (/ w 2.0)) x2 (+ cx (/ w 2.0))
            y2 (- (cadr matbox) 15.0) y1 (- y2 up dn))
      (list x1 y1 x2 y2))
    nil))
;; Distancia al cuadrado entre centros de cajas (sin sqrt: solo ordena).
(defun GM:BoxDist2 (ax1 ay1 ax2 ay2 bx1 by1 bx2 by2 / dx dy)
  (setq dx (- (/ (+ ax1 ax2) 2.0) (/ (+ bx1 bx2) 2.0))
        dy (- (/ (+ ay1 ay2) 2.0) (/ (+ by1 by2) 2.0)))
  (+ (* dx dx) (* dy dy)))
;; Sur = centro de la caja bajo fondo+FRAC*altura (territorio del propietario).
(defun GM:BoxIsSouth (by1 by2 rect / frac)
  (setq frac (GM:NumVar *GM_AF_SOUTH_FRAC* 0.35))
  (< (/ (+ by1 by2) 2.0) (+ (cadr rect) (* frac (- (nth 3 rect) (cadr rect))))))
;; Huella del candidato: caja a la derecha de p2 (estricta contra todo) mas
;; quiebre dentro del viewport util. El leader solo evita roles criticos.
;; Retorna holgura de la caja o nil.
(defun GM:CandidateClearance (p1 x2 y2 w up dn rect / bx1 by1 bx2 by2 cl)
  (setq bx1 x2 by1 (- y2 dn) bx2 (+ x2 w) by2 (+ y2 up))
  (if (and rect (or (< x2 (car rect)) (< by1 (cadr rect))
                     (> bx2 (nth 2 rect)) (> by2 (nth 3 rect))))
    nil
    (progn
      (setq cl (GM:BoxClearance bx1 by1 bx2 by2))
      ;; OPT10 (F5): se valida la ruta real del leader con caja (p1->codo->amarre).
      (if (or (< cl 0)
              (not (GM:LeaderRouteClear (GM:LeaderRouteBoxed p1 (list x2 y2 0.0) w up dn)))
              (not (GM:SamplesClear bx1 by1 bx2 by2)))
        nil cl))))
;; Bloqueador del candidato para el veredicto forense.
(defun GM:CandidateBlocker (p1 x2 y2 w up dn rect / bx1 by1 bx2 by2 name)
  (setq bx1 x2 by1 (- y2 dn) bx2 (+ x2 w) by2 (+ y2 up))
  (if (and rect (or (< x2 (car rect)) (< by1 (cadr rect))
                    (> bx2 (nth 2 rect)) (> by2 (nth 3 rect))))
    "FUERA"
    (progn
      (setq name (GM:BoxBlocker bx1 by1 bx2 by2))
      (if (/= name "")
        name
        (progn
          ;; OPT10 (F5): el veredicto nombra el rol que bloquea la ruta real.
          (setq name (GM:LeaderRouteBlocker (GM:LeaderRouteBoxed p1 (list x2 y2 0.0) w up dn)))
          (if name (strcat "L:" name) ""))))))
;; Veredicto compacto de todos los candidatos para el reporte forense.
(defun GM:CandidatesVerdict (p1 cands w up dn rect / diag c cl)
  (setq diag "")
  (foreach c cands
    (setq cl (GM:CandidateClearance p1 (cadr c) (caddr c) w up dn rect))
    (setq diag (strcat diag (car c) ":"
                 (if cl (strcat "OK:" (rtos cl 2 1))
                   (GM:CandidateBlocker p1 (cadr c) (caddr c) w up dn rect)) " ")))
  diag)
;; Holgura minima exigida para preferir el aterrizaje mas cercano: entre los
;; candidatos que la cumplen se elige el leader mas corto (etiqueta pegada al
;; predio); si ninguno la cumple, se usa el de mayor holgura aunque quede lejos.
(setq *GM_LANDING_MIN_GAP* 3.0)
;; Largo al cuadrado del leader p1->p2 como proxy de cercania (sin sqrt/expt).
(defun GM:LeaderLen (p1 x2 y2)
  (+ (* (- x2 (car p1)) (- x2 (car p1))) (* (- y2 (cadr p1)) (- y2 (cadr p1)))))
;; Mejor aterrizaje como (p2 holgura); nil si ninguno cabe.
(defun GM:LandingBestAt (p1 w up dn / rect cands c cl best bestclear bestlen L min_gap)
  ;; Umbral local (los setq globales no existen en todos los contextos).
  (setq min_gap (if (numberp *GM_LANDING_MIN_GAP*) *GM_LANDING_MIN_GAP* 3.0)
        rect (GM:MainViewportRect)
        cands (GM:LandingCandidates p1 w up dn) best nil bestclear nil bestlen nil)
  ;; Pasada 1: holgura suficiente + leader mas corto.
  (foreach c cands
    (setq cl (GM:CandidateClearance p1 (cadr c) (caddr c) w up dn rect))
    (if cl
      (progn
        (setq L (GM:LeaderLen p1 (cadr c) (caddr c)))
        (if (and (>= cl min_gap) (or (not bestlen) (< L bestlen)))
          (setq best (list (cadr c) (caddr c) 0.0) bestclear cl bestlen L)))))
  ;; Pasada 2 (fallback): mayor holgura.
  (if (not best)
    (foreach c cands
      (setq cl (GM:CandidateClearance p1 (cadr c) (caddr c) w up dn rect))
      (if (and cl (or (not bestclear) (> cl bestclear)))
        (setq best (list (cadr c) (caddr c) 0.0) bestclear cl))))
  (if best (list best bestclear) nil))
;; Barrido AF place-first (Revision AN/AQ): primero el mejor LUGAR (celda),
;; despues el enlace (ancla al PF cercano). AQ: la region nace de la VIA
;; (todo sale del DWG): lado opuesto a la matriz, mas alla del corredor +
;; ROAD_GAP; la via es obstaculo (margen ROAD_MARGIN) para no intervenirla.
;; Cascada: ESTE/OESTE -> TOTAL sin sur -> TOTAL con sur -> nil (ERROR).
;; Celda plana: (x2 y2 aire dist2 sur puntaje). Margen anti-marco FRAME_M.
(if (not *GM_AF_AIR_CAP*) (setq *GM_AF_AIR_CAP* "15.0"))
(if (not *GM_AF_DIST_CAP*) (setq *GM_AF_DIST_CAP* "10000.0"))
(if (not *GM_AF_NEAR_W*) (setq *GM_AF_NEAR_W* "0.00001"))
(if (not *GM_AF_ROAD_GAP*) (setq *GM_AF_ROAD_GAP* "8.0"))
(if (not *GM_AF_ROAD_MARGIN*) (setq *GM_AF_ROAD_MARGIN* "2.0"))
(if (not *GM_AF_FRAME_M*) (setq *GM_AF_FRAME_M* "5.0"))
;; Propietario estilo AQ (Revision AR): mismo corredor vial, lado opuesto,
;; gap 20 y clearance 3. Editables sin reeditar codigo; en campo, antes
;; del lote: (setq *GM_PROP_GAP* "25.0").
(if (not *GM_PROP_GAP*) (setq *GM_PROP_GAP* "20.0"))
(if (not *GM_PROP_MIN*) (setq *GM_PROP_MIN* "3.0"))
(if (not *GM_PROP_SCAN_DIAG*) (setq *GM_PROP_SCAN_DIAG* ""))