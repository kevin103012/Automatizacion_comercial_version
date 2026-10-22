;; ============================================================================
;; GM_GEO.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


;; El circulo pequeno debe senalar el predio real (el del esquema izquierdo
;; ESQUEMA DE LOCALIZACION DEL PREDIO EN POSESION), no un punto arbitrario.
;; Orden: 1) U.C. exacta dentro del mapa regional, 2) DISTRITO/SECTOR del CSV
;; dentro del mapa (el mapa rotula distritos, no U.C.), 3) ruta roja por
;; progresiva solo como ultimo recurso y filtrada al marco del mapa.
;; FIX plano2 (circulo grande en blanco): la captura nunca queda vacia.
(defun GM:AutoLocateDigits (value / result pos ch)
  (setq result "" pos 1)
  (if (= (type value) 'STR)
    (while (<= pos (strlen value))
      (setq ch (substr value pos 1))
      (if (wcmatch ch "[0-9]") (setq result (strcat result ch)))
      (setq pos (1+ pos))))
  result
)
;; T si el punto cae dentro de la caja (x1 y1 x2 y2).
(defun GM:LocInBox (pt box)
  (and pt box (numberp (car pt)) (numberp (cadr pt))
       (>= (car pt) (car box)) (<= (car pt) (nth 2 box))
       (>= (cadr pt) (cadr box)) (<= (cadr pt) (nth 3 box))))
;; PRY1-MULTI F2 (GEO8) 2026-10-14: normalizacion CON separadores -> espacios
;; unicos y acentos plegados a ASCII. UBI_NormalizeSearchKey elimina espacios y
;; el search plano producia falsos positivos de borde ("SANTA" matcheaba
;; "SANTANDER"); esta forma conserva las palabras para el match por token.
(defun GM:GeoSpacedNorm (value / s pr i n c out prev_out)
  (setq s (if (= (type value) 'STR) (strcase (vl-string-trim " \t\n\r" value)) ""))
  (foreach pr '(("\U+00C1" . "A") ("\U+00C9" . "E") ("\U+00CD" . "I")
                ("\U+00D3" . "O") ("\U+00DA" . "U") ("\U+00DC" . "U")
                ("\U+00D1" . "N") ("\U+00C0" . "A") ("\U+00C8" . "E")
                ("\U+00CC" . "I") ("\U+00D2" . "O") ("\U+00D9" . "U"))
    (while (vl-string-search (car pr) s)
      (setq s (vl-string-subst (cdr pr) (car pr) s))))
  (setq out "" i 1 n (strlen s) prev_out nil)
  (while (<= i n)
    (setq c (substr s i 1))
    (if (wcmatch c "[A-Z0-9]")
      (progn (setq out (strcat out c) prev_out T))
      (if prev_out (progn (setq out (strcat out " ")) (setq prev_out nil))))
    (setq i (1+ i)))
  (vl-string-trim " " out)
)
;; T si needle aparece en hay con bordes de palabra (ambos GeoSpacedNorm).
(defun GM:GeoSpacedMatchP (needle hay)
  (and (= (type needle) 'STR) (= (type hay) 'STR)
       (/= needle "") (/= hay "")
       (vl-string-search (strcat " " needle " ") (strcat " " hay " "))))
;; PRY1-MULTI F2 (GEO8): peso de la fuente que gano (la cascada del localizador
;; va en el mismo orden de pesos; la traza PREDIO_LOC_SCORE lo deja auditable).
(defun GM:GeoMethodScore (method / hit)
  (setq hit (assoc method '(("TICKS" . 1.0) ("TICKS+ROJA" . 1.0)
                            ("ESQUEMA_VP" . 0.9) ("ESQUEMA" . 0.8)
                            ("UC" . 0.7) ("UC+ROJA" . 0.7)
                            ("DISTRITO" . 0.5) ("DISTRITO+ROJA" . 0.5)
                            ("PROG" . 0.2))))
  (if hit (cdr hit) 0.0)
)
;; Centro papel de un texto (bbox; si falla, insercion). Nil si no medible.
(defun GM:LocTextCenter (ent / obj lo hi p10 ed result)
  (setq result nil)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
  (if (vl-catch-all-error-p obj)
    nil
    (progn
      (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'lo 'hi)))
        (progn
          (setq ed (entget ent) p10 (cdr (assoc 10 ed)))
          (if (and p10 (numberp (car p10)) (numberp (cadr p10)))
            (setq result (list (car p10) (cadr p10) 0.0))))
        (progn
          (setq lo (vlax-safearray->list lo) hi (vlax-safearray->list hi))
          (setq result (list (/ (+ (car lo) (car hi)) 2.0) (/ (+ (cadr lo) (cadr hi)) 2.0) 0.0))))
      ;; FIX-AV: soltar el wrapper del texto en todas las salidas.
      (GM:ReleaseObjectSafe obj)))
  result)
;; Marco COMPLETO del mapa regional (se abre todo el mapa, no una ventana
;; fija): rectangulo sobre el titulo 167F4D. Retorna (l b r t) o nil.
(defun GM:MapFrameBox (/ title l0 b0 r0 t0 ss i ent ed bb bl bb2 br bt w h typ fl vxs ok area best bestarea d)
  ;; FindLayoutBox no falla (retorna nil); LayoutBaseBox marcaria FAILED lateral.
  (setq title (vl-catch-all-apply 'GM:FindLayoutBox (list "167F4D" "MAP")))
  (if (vl-catch-all-error-p title) (setq title nil))
  (if (not title)
    nil
    (progn
      (setq l0 (car title) b0 (cadr title) r0 (nth 2 title) t0 (nth 3 title)
            ss (ssget "X" (list (cons 0 "LWPOLYLINE,POLYLINE,INSERT,IMAGE") (cons 410 (getvar "CTAB"))))
            i 0 best nil bestarea 0.0)
      (while (and ss (< i (sslength ss)))
        (setq ent (ssname ss i) ed (entget ent) bb (GM:LayoutEntityBox ent) ok nil)
        (if (and bb ed)
          (progn
            (setq bl (car bb) bb2 (cadr bb) br (nth 2 bb) bt (nth 3 bb)
                  w (- br bl) h (- bt bb2))
            (if (and (> w 80.0) (> h 25.0)
                     (< (abs (- bb2 t0)) 15.0) (< (abs (- bl l0)) 20.0) (< (abs (- br r0)) 20.0)
                     (> bt (+ t0 20.0)))
              (progn
                (setq typ (cdr (assoc 0 ed)))
                (if (or (= typ "INSERT") (= typ "IMAGE"))
                  (setq ok T)
                  (progn
                    (setq fl (cdr (assoc 70 ed)) vxs 0)
                    (foreach d ed (if (= (car d) 10) (setq vxs (1+ vxs))))
                    (if (and (numberp fl) (= (logand fl 1) 1) (or (= vxs 4) (= vxs 5)))
                      (setq ok T))))))))
        (if ok
          (progn
            (setq area (* w h))
            (if (> area bestarea) (setq best bb bestarea area))))
        (setq i (1+ i)))
      best)))
;; Transformacion UTM->papel con los ticks del marco (p.ej. 550000/600000 y
;; 8450000/8500000). Retorna (sx ox sy oy) o nil.
(defun GM:TickTransform (mbox / box ss i ent obj val dig num cen xs ys xa xb ya yb ltext)
  (setq box (if mbox (list (- (car mbox) 6.0) (- (cadr mbox) 6.0) (+ (nth 2 mbox) 6.0) (+ (nth 3 mbox) 6.0)) nil)
        xs nil ys nil)
  (if *GM_AUTO_LTEXT_READY*
    ;; PA-3 (H7): pasada pura sobre el indice del lote (sin COM por texto).
    (foreach ltext *GM_AUTO_LTEXT_INDEX*
      (setq ent (car ltext) val (cadr ltext) dig (GM:AutoLocateDigits val) num (atof dig) cen nil)
      (if (and (= (strlen dig) 6) (>= num 400000.0) (<= num 800000.0))
        (progn
          (setq cen (GM:LocTextCenter ent))
          (if (and cen (or (not box) (GM:LocInBox cen box)))
            (setq xs (cons (list num (car cen)) xs)))))
      (if (and (= (strlen dig) 7) (>= num 8000000.0) (<= num 9000000.0))
        (progn
          (setq cen (GM:LocTextCenter ent))
          (if (and cen (or (not box) (GM:LocInBox cen box)))
            (setq ys (cons (list num (cadr cen)) ys))))))
    (progn
      (setq ss (ssget "X" (list (cons 0 "TEXT,MTEXT") (cons 410 (getvar "CTAB")))) i 0)
      (while (and ss (< i (sslength ss)))
        (setq ent (ssname ss i))
        ;; AUD1: creacion con catch; entidad caida = se salta (antes abortaba).
        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (vl-catch-all-error-p obj) (setq obj nil))
        (setq val (if obj (vl-catch-all-apply 'vla-get-TextString (list obj)) nil))
        (if (and (not (vl-catch-all-error-p val)) val)
          (progn
            (setq dig (GM:AutoLocateDigits val) num (atof dig) cen nil)
            (if (and (= (strlen dig) 6) (>= num 400000.0) (<= num 800000.0))
              (progn
                (setq cen (GM:LocTextCenter ent))
                (if (and cen (or (not box) (GM:LocInBox cen box)))
                  (setq xs (cons (list num (car cen)) xs)))))
            (if (and (= (strlen dig) 7) (>= num 8000000.0) (<= num 9000000.0))
              (progn
                (setq cen (GM:LocTextCenter ent))
                (if (and cen (or (not box) (GM:LocInBox cen box)))
                  (setq ys (cons (list num (cadr cen)) ys)))))))
        ;; FIX-AV: soltar el wrapper del texto antes de la siguiente iteracion.
        (GM:ReleaseObjectSafe obj)
        (setq i (1+ i)))))
  (if (and xs (cdr xs) ys (cdr ys))
    (progn
      (setq xs (vl-sort xs '(lambda (a b) (< (car a) (car b))))
            ys (vl-sort ys '(lambda (a b) (< (car a) (car b))))
            xa (car xs) xb (car (reverse xs)) ya (car ys) yb (car (reverse ys)))
      (if (and (> (- (car xb) (car xa)) 5000.0) (> (- (car yb) (car ya)) 5000.0)
               (/= (- (cadr xb) (cadr xa)) 0.0) (/= (- (cadr yb) (cadr ya)) 0.0))
        (list (/ (- (cadr xb) (cadr xa)) (- (car xb) (car xa)))
              (- (cadr xa) (* (/ (- (cadr xb) (cadr xa)) (- (car xb) (car xa))) (car xa)))
              (/ (- (cadr yb) (cadr ya)) (- (car yb) (car ya)))
              (- (cadr ya) (* (/ (- (cadr yb) (cadr ya)) (- (car yb) (car ya))) (car ya))))
        nil))
    nil))
;; Centroide UTM (Model) del AF del esquema izquierdo (hilo CODE-AF-/PF-,
;; si no la matriz). Es "donde esta el predio afectado" segun el esquema.
(defun GM:ModelAFCenter (code / code_up ss i ent obj hl hlu afEnt afObj lo hi)
  (setq code_up (if (= (type code) 'STR) (strcase code) "")
        ss (ssget "X" (list (cons 0 "HATCH,LWPOLYLINE,POLYLINE") (cons 410 "Model"))) i 0 afEnt nil)
  (while (and ss (< i (sslength ss)) (not afEnt))
    (setq ent (ssname ss i))
    ;; AUD1: creacion con catch; entidad caida = se salta.
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (vl-catch-all-error-p obj) (setq obj nil))
    (setq hl (if obj (UBI_GetHyperlinkString obj) nil) hlu (if hl (strcase hl) ""))
    (if (and (/= code_up "") (or (str-starts-with hlu (strcat code_up "-AF-")) (str-starts-with hlu (strcat code_up "-PF-"))))
      (setq afEnt ent))
    ;; FIX-AV: soltar el wrapper del escaneo antes de seguir.
    (GM:ReleaseObjectSafe obj)
    (setq i (1+ i)))
  (if (and (not afEnt) (/= code_up ""))
    (setq afEnt (UBI_FindPolygonByCode code_up)))
  (if (and afEnt (entget afEnt))
    (progn
      ;; FIX-AV: wrapper nominal del bounding box, liberado tras medir.
      (setq afObj (vl-catch-all-apply 'vlax-ename->vla-object (list afEnt))
            lo nil hi nil)
      (if (vl-catch-all-error-p afObj) (setq afObj nil))
      (if (and afObj (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list afObj 'lo 'hi)))))
        (progn
          (GM:ReleaseObjectSafe afObj)
          (setq afObj nil
                lo (vlax-safearray->list lo) hi (vlax-safearray->list hi))
          (list (/ (+ (car lo) (car hi)) 2.0) (/ (+ (cadr lo) (cadr hi)) 2.0) 0.0))
        (progn
          (GM:ReleaseObjectSafe afObj)
          (setq afObj nil)
          nil)))
    nil))
;; Caja util de TODO el mapa regional para buscar el predio del esquema.
;; Retorna (x1 y1 x2 y2) o nil.
;; PA-2 (H3) 2026-09-28: marco del mapa + obstaculos base cacheados por lote.
;; El layout base no cambia entre codigos (la limpieza restaura); el localizador,
;; el zoom de predio y el rotulo GEO reutilizan la misma medicion. En manual las
;; banderas estan apagadas y todo se calcula como antes.
(if (not (boundp '*GM_AUTO_MAPBOX_LOTE*)) (setq *GM_AUTO_MAPBOX_LOTE* nil))
(if (not (boundp '*GM_AUTO_MAPBOX_LOTE_READY*)) (setq *GM_AUTO_MAPBOX_LOTE_READY* nil))
(if (not (boundp '*GM_AUTO_GEO_BOXES*)) (setq *GM_AUTO_GEO_BOXES* nil))
(if (not (boundp '*GM_AUTO_GEO_BOXES_READY*)) (setq *GM_AUTO_GEO_BOXES_READY* nil))
;; PA-3 (H7) 2026-09-28: indice de textos del Layout (ename + cadena) medido una
;; vez por lote; el localizador y TickTransform hacen sus pasadas en Lisp puro y
;; solo piden bbox (COM) al texto que coincide. En manual cae al barrido vivo.
(if (not (boundp '*GM_AUTO_LTEXT_INDEX*)) (setq *GM_AUTO_LTEXT_INDEX* nil))
(if (not (boundp '*GM_AUTO_LTEXT_READY*)) (setq *GM_AUTO_LTEXT_READY* nil))
;; CHG3-FIX2 2026-10-10: datos del mapa-en-Modelo (VP anidado) por codigo:
;; transformacion del VP y punto del predio sobre la ruta del mapa.
(if (not (boundp '*GM_AUTO_MAPVP_INFO*)) (setq *GM_AUTO_MAPVP_INFO* nil))
(if (not (boundp '*GM_AUTO_MAPVP_MODELPT*)) (setq *GM_AUTO_MAPVP_MODELPT* nil))
;; PRY1-MULTI F1 (GEO7) 2026-10-14: transform UTM->papel unificado del lote
;; (TICKS primero; VP anidado como observacion secundaria solo para kind).
;; Formato: (sx ox sy oy tipo) con tipo "TICKS" | "VP"; nil si no calibrable.
(if (not (boundp '*GM_AUTO_GEO_XF*)) (setq *GM_AUTO_GEO_XF* nil))
;; PRY1-MULTI F4 2026-10-14: marco del circulo grande publicado por el
;; localizador (GM:ZoomFrameFor); LAY_ESQUEMAS y GM_ROTULO lo consumen para
;; que el dibujo y la exclusion usen SIEMPRE la misma posicion.
(if (not (boundp '*GM_AUTO_ZOOM_FRAME*)) (setq *GM_AUTO_ZOOM_FRAME* nil))
;; CHG3-FIX2: handle del texto ancla de la localizacion (no debe copiarse al zoom).
(if (not (boundp '*GM_AUTO_PREDIO_LOC_ANCHOR*)) (setq *GM_AUTO_PREDIO_LOC_ANCHOR* nil))
;; PRY1-GEO7 2026-10-19 (F1 plan B): desc del zoom cuando el mapa vive DENTRO
;; de un bloque: (ent insx insy sx sy rot inshandle len param x_ancla y_ancla).
;; LAY_ESQUEMAS la consuma para recrear el tramo de ruta (GM:MapBlockZoomContent).
(if (not (boundp '*GM_AUTO_PREDIO_ZOOM_BLK*)) (setq *GM_AUTO_PREDIO_ZOOM_BLK* nil))
;; PRY1-GEO7 (F1): ename del ultimo texto del bloque matcheado por
;; GM:MapBlockTextFind (para excluir el rotulo ancla del contenido del zoom).
(if (not (boundp '*GM_MAPBLOCK_LAST_TEXT*)) (setq *GM_MAPBLOCK_LAST_TEXT* nil))
(defun GM:LayoutTextIndex (/ ss i ent ed str result)
  (setq result nil ss (ssget "X" (list (cons 0 "TEXT,MTEXT") (cons 410 (getvar "CTAB")))) i 0)
  (while (and ss (< i (sslength ss)))
    (setq ent (ssname ss i) ed (entget ent) str (if ed (cdr (assoc 1 ed)) nil))
    ;; CHG3-FIX2: los rotulos DEP/PROV/DIST del propio sistema (incluida una
    ;; plantilla contaminada de una corrida previa) no son fuentes validas de
    ;; localizacion ni del mapa: quedan fuera del indice.
    (if (and (= (type str) 'STR)
             (/= (cdr (assoc 8 ed)) "0. DEP. PROV. DIST."))
      (setq result (cons (list ent str) result)))
    (setq i (1+ i)))
  (reverse result))
;; Mide UNA vez el marco MAPA_GEO y las letras base que lo pisan (una sola
;; pasada de textos del Layout en todo el lote). CHG5: suma el contenido
;; legible DENTRO del bloque del mapa (rotulos + ruta roja/magenta).
(defun GM:PrimeGeoBoxesCache (/ mbox bx bb)
  (setq *GM_AUTO_MAPBOX_LOTE* nil *GM_AUTO_MAPBOX_LOTE_READY* nil
        *GM_AUTO_GEO_BOXES* nil *GM_AUTO_GEO_BOXES_READY* nil
        *GM_AUTO_LTEXT_INDEX* nil *GM_AUTO_LTEXT_READY* nil
        *GM_AUTO_GEO_XF* nil)
  (setq mbox (GM:MapContentBox) bx nil bb nil)
  (if mbox
    (progn
      (setq bx (GM:GeoMapTextBoxes mbox) bb (GM:MapBlockBoxesSafe mbox))
      (setq *GM_AUTO_MAPBOX_LOTE* mbox *GM_AUTO_MAPBOX_LOTE_READY* T
            *GM_AUTO_GEO_BOXES* (append bx bb) *GM_AUTO_GEO_BOXES_READY* T)))
  ;; PA-3 (H7): el indice de textos se arma con el layout base (sin entidades
  ;; del codigo), que es cuando corre el primado.
  (setq *GM_AUTO_LTEXT_INDEX* (GM:LayoutTextIndex) *GM_AUTO_LTEXT_READY* T)
  ;; PRY1-MULTI F1 (GEO7): transform unificado + clasificacion del modelo
  ;; (una vez por lote; fail-open a nil y el localizador cae a la cascada).
  (if mbox
    (progn
      (setq *GM_AUTO_GEO_XF* (vl-catch-all-apply 'GM:GeoUnifiedXf (list mbox)))
      (if (vl-catch-all-error-p *GM_AUTO_GEO_XF*) (setq *GM_AUTO_GEO_XF* nil))
      (GM:AutoTrace "LOTE" "MAPA_KIND" "INFO"
        (strcat "kind=" (GM:MapKind mbox *GM_AUTO_GEO_XF*)))
      (if *GM_AUTO_GEO_XF*
        (GM:AutoTrace "LOTE" "MAPA_XF" "INFO"
          (strcat "tipo=" (nth 4 *GM_AUTO_GEO_XF*)
                  " sx=" (rtos (nth 0 *GM_AUTO_GEO_XF*) 2 6)
                  " sy=" (rtos (nth 2 *GM_AUTO_GEO_XF*) 2 6))))))
  (GM:AutoTrace "LOTE" "GEO_BOXES" (if *GM_AUTO_GEO_BOXES_READY* "OK" "AVISO")
    (if *GM_AUTO_GEO_BOXES_READY*
      (strcat "marco y obstaculos base medidos una vez (boxes=" (itoa (length *GM_AUTO_GEO_BOXES*))
              " bloque=" (itoa (length bb))
              " textos=" (itoa (length *GM_AUTO_LTEXT_INDEX*)) ")")
      "sin marco MAPA_GEO; el rotulo GEO medira por codigo"))
  (if *GM_AUTO_GEO_BOXES_READY* T nil))

;; CHG4 2026-10-12: la plantilla puede traer el rotulo DEP/PROV/DIST de una
;; corrida anterior guardada en el DWG (contaminacion observada en Chagual:
;; 6 textos viejos que el lote no borraba y el PDF mostraba duplicados). Se
;; borra UNA vez por lote, antes de medir obstaculos: con el toggle ON el
;; plano lleva solo el rotulo fresco del codigo; con OFF queda sin ningun
;; rotulo viejo. Solo toca la capa exclusiva del sistema (TEXT/MTEXT/ATTRIB
;; del layout activo), nunca cartografia ni otras capas. La copia de trabajo
;; es desechable; el master no se toca. Traza GEO_BASE_LIMPIA;N=n.
(defun GM:CleanGeoBase (/ ss i ent n res)
  (setq n 0 i 0
        ss (ssget "X" (list (cons 0 "TEXT,MTEXT,ATTRIB") (cons 410 (getvar "CTAB"))
                            (cons 8 "0. DEP. PROV. DIST."))))
  (while (and ss (< i (sslength ss)))
    (setq ent (ssname ss i)
          res (vl-catch-all-apply 'entdel (list ent)))
    (if (and (not (vl-catch-all-error-p res)) res)
      (setq n (1+ n)))
    (setq i (1+ i)))
  (GM:AutoTrace "LOTE" "GEO_BASE_LIMPIA" "OK"
    (strcat "N=" (itoa n) " rotulos viejos del modelo base eliminados"))
  n)
(defun GM:MapContentBox (/ title frame)
  (if (and *GM_AUTO_MAPBOX_LOTE_READY* *GM_AUTO_MODE*)
    *GM_AUTO_MAPBOX_LOTE*
    (progn
      (setq title (vl-catch-all-apply 'GM:FindLayoutBox (list "167F4D" "MAP")))
      (if (vl-catch-all-error-p title) (setq title nil))
      (if (and title (> (- (nth 3 title) (cadr title)) 20.0))
        ;; CHG1 2026-10-07: mapa combinado (bloque INSERT/IMAGE o marco grande
        ;; que incluye su franja de titulo; p.ej. ESQUEMA DE LOCALIZACION).
        ;; El contenido es el propio marco medido, con margen interior de 2.
        (list (+ (car title) 2.0) (+ (cadr title) 2.0) (- (nth 2 title) 2.0) (- (nth 3 title) 2.0))
        (progn
          (setq frame (GM:MapFrameBox))
          (if (and title frame)
            (list (+ (car frame) 2.0) (+ (nth 3 title) 2.0) (- (nth 2 frame) 2.0) (- (nth 3 frame) 2.0))
            (if title
              (list (+ (car title) 2.0) (+ (nth 3 title) 2.0) (- (nth 2 title) 2.0) (+ (nth 3 title) 110.0))
              nil)))))))
;; PRY1-MULTI F1 (GEO7) 2026-10-14: transform UTM->papel unificado del lote.
;; Prioridad TICKS (calibracion exacta por ticks del marco) y, como observacion
;; secundaria, el VP anidado (afin equivalente kf=s*h/vh). El tipo "VP" NO
;; alimenta la proyeccion del AF en esta fase (la cartografia del VP puede no
;; ser UTM real; se usa solo para clasificar el modelo y para el diagnostico
;; PREDIO_LOC_XF_CHECK). Fail-open: sin observacion devuelve nil.
(defun GM:GeoUnifiedXf (mbox / tform vpinfo kf ox oy)
  (cond
    ((setq tform (GM:TickTransform mbox))
     (list (car tform) (cadr tform) (caddr tform) (cadddr tform) "TICKS"))
    ((setq vpinfo (GM:FindNestedMapVP mbox))
     (setq kf (GM:MapVPKf vpinfo))
     (if (and (numberp kf) (> kf 0.0))
       (progn
         ;; misma matematica que GM:MapVPToPaper: papel = kf*modelo + (ix+s*cx-kf*vcx)
         (setq ox (- (+ (nth 7 vpinfo) (* (nth 9 vpinfo) (nth 0 vpinfo))) (* kf (nth 5 vpinfo)))
               oy (- (+ (nth 8 vpinfo) (* (nth 9 vpinfo) (nth 1 vpinfo))) (* kf (nth 6 vpinfo))))
         (list kf ox kf oy "VP"))
       nil))
    (T nil))
)
;; PRY1-MULTI F1 (GEO7): clasificacion del modelo de mapa para diagnostico.
;; Precedencia espejo del localizador: VP anidado -> TICKS -> bloque -> XREF.
(defun GM:MapKind (mbox xf / ins)
  (cond
    ((null mbox) "SIN_MARCO")
    ((and xf (listp xf) (equal (nth 4 xf) "VP")) "BLOQUE-VP")
    ((and xf (listp xf) (equal (nth 4 xf) "TICKS")) "LAYOUT-TICKS")
    ((setq ins (GM:MapBlockInsert)) "BLOQUE")
    ((GM:FindMapXref) "XREF")
    (T "LAYOUT"))
)

;; T si la entidad es roja: ACI 1, TrueColor RGB 255/0/0 o capa ROJ/RED/RUTA.
(defun GM:IsRedEnt (obj / c ok tc r g b lay)
  (setq ok nil)
  (setq c (vl-catch-all-apply 'vla-get-Color (list obj)))
  (if (and (not (vl-catch-all-error-p c)) (= c 1)) (setq ok T))
  (if (not ok)
    (progn
      (setq tc (vl-catch-all-apply 'vla-get-TrueColor (list obj)))
      (if (and (not (vl-catch-all-error-p tc)) tc)
        (progn
          (setq r (vl-catch-all-apply 'vla-get-Red (list tc)))
          (setq g (vl-catch-all-apply 'vla-get-Green (list tc)))
          (setq b (vl-catch-all-apply 'vla-get-Blue (list tc)))
          ;; FIX-AV: el TrueColor temporal tambien es un objeto COM.
          (GM:ReleaseObjectSafe tc)
          (if (and (not (vl-catch-all-error-p r)) (not (vl-catch-all-error-p g)) (not (vl-catch-all-error-p b))
                   (= r 255) (= g 0) (= b 0))
            (setq ok T))))))
  (if (not ok)
    (progn
      (setq lay (vl-catch-all-apply 'vla-get-Layer (list obj)))
      (if (and (not (vl-catch-all-error-p lay)) (= (type lay) 'STR)
               (or (vl-string-search "ROJ" (strcase lay)) (vl-string-search "RED" (strcase lay)) (vl-string-search "RUTA" (strcase lay))))
        (setq ok T))))
  ok)
;; Largo de curva universal (LINE/ARC/SPLINE/polilineas). Nil si no es curva.
(defun GM:CurveLen (obj / L)
  (setq L (vl-catch-all-apply '(lambda (o) (vlax-curve-getDistAtParam o (vlax-curve-getEndParam o))) (list obj)))
  (if (or (vl-catch-all-error-p L) (not (numberp L))) nil L))
;; Ruta roja del mapa regional (linea del medio): la mas larga dentro del marco.
;; Retorna (entidad longitud handle) o nil.
(defun GM:LocRegionalRoute (mbox / ss i ent obj candidate_len best bestlen besthand cand_mid)
  (setq best nil bestlen 0.0 besthand nil
        ss (ssget "X" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE") (cons 410 (getvar "CTAB")))) i 0)
  (while (and ss (< i (sslength ss)))
    (setq ent (ssname ss i))
    ;; AUD1: creacion con catch; entidad caida = se salta.
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (vl-catch-all-error-p obj) (setq obj nil))
    (setq candidate_len (if obj (GM:CurveLen obj) nil))
    (if (and obj candidate_len (> candidate_len bestlen) (GM:IsRedEnt obj))
      (progn
        (setq cand_mid (vl-catch-all-apply 'vlax-curve-getPointAtDist (list obj (/ candidate_len 2.0))))
        (if (vl-catch-all-error-p cand_mid) (setq cand_mid nil))
        (if (or (not mbox) (not cand_mid) (GM:LocInBox cand_mid mbox))
          (setq best ent bestlen candidate_len besthand (cdr (assoc 5 (entget ent)))))))
    ;; FIX-AV: soltar el wrapper del candidato antes de seguir.
    (GM:ReleaseObjectSafe obj)
    (setq i (1+ i)))
  (if best (list best bestlen besthand) nil))
;; Fraccion del AF a lo largo del EJE del Model (lo que dibuja el ESQUEMA DE
;; LOCALIZACION DEL PREDIO EN PROPIEDAD: su rosada es la roja del mapa).
;; Retorna (fraccion longitud-eje) o nil.
(defun GM:EjeFractionT (code / ss i ent obj hl hlu code_up pref_af pref_pf bestEje bestLen elen afEnt afC lo hi cen clo cdist frac afObj ejeObj)
  (setq bestEje nil bestLen 0.0 afEnt nil
        code_up (if (= (type code) 'STR) (strcase code) "")
        pref_af (strcat code_up "-AF-") pref_pf (strcat code_up "-PF-")
        ss (ssget "X" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE") (cons 410 "Model"))) i 0)
  (while (and ss (< i (sslength ss)))
    (setq ent (ssname ss i))
    ;; AUD1: creacion con catch; entidad caida = se salta.
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
    (if (vl-catch-all-error-p obj) (setq obj nil))
    (setq hl (if obj (UBI_GetHyperlinkString obj) nil) hlu (if hl (strcase hl) ""))
    (if (and (/= hlu "") (vl-string-search "EJE" hlu) (vl-string-search "VIA" hlu))
      (progn
        (setq elen (GM:CurveLen obj))
        (if (and elen (> elen bestLen))
          (setq bestEje ent bestLen elen))))
    (if (and (not afEnt) (/= code_up "") (str-starts-with hlu pref_af))
      (setq afEnt ent))
    (if (and (not afEnt) (/= code_up "") (str-starts-with hlu pref_pf))
      (setq afEnt ent))
    ;; FIX-AV: soltar el wrapper del escaneo antes de seguir.
    (GM:ReleaseObjectSafe obj)
    (setq i (1+ i)))
  (if (and (not afEnt) (/= code_up ""))
    (setq afEnt (UBI_FindPolygonByCode code_up)))
  (if (and bestEje afEnt (entget afEnt) (> bestLen 0.0))
    (progn
      ;; FIX-AV: wrappers nominales, liberados tras medir (una sola vez).
      (setq afObj (vl-catch-all-apply 'vlax-ename->vla-object (list afEnt))
            ejeObj (vl-catch-all-apply 'vlax-ename->vla-object (list bestEje)))
      (if (vl-catch-all-error-p afObj) (setq afObj nil))
      (if (vl-catch-all-error-p ejeObj) (setq ejeObj nil))
      (setq afC nil)
      (if (and afObj (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list afObj 'lo 'hi)))))
        (progn
          (setq lo (vlax-safearray->list lo) hi (vlax-safearray->list hi))
          (setq afC (list (/ (+ (car lo) (car hi)) 2.0) (/ (+ (cadr lo) (cadr hi)) 2.0) 0.0))))
      (if (and afC ejeObj)
        (progn
          (setq clo (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list ejeObj afC)))
          (if (vl-catch-all-error-p clo) (setq clo nil))
          (if clo
            (progn
              (setq cdist (vl-catch-all-apply 'vlax-curve-getDistAtPoint (list ejeObj clo)))
              (if (vl-catch-all-error-p cdist) (setq cdist nil))
              (if (and cdist (numberp cdist))
                (progn
                  (setq frac (max 0.02 (min 0.98 (/ cdist bestLen))))
                  (GM:ReleaseObjectSafe afObj)
                  (GM:ReleaseObjectSafe ejeObj)
                  (list frac bestLen))
                (progn
                  (GM:ReleaseObjectSafe afObj)
                  (GM:ReleaseObjectSafe ejeObj)
                  nil)))
            (progn
              (GM:ReleaseObjectSafe afObj)
              (GM:ReleaseObjectSafe ejeObj)
              nil)))
        (progn
          (GM:ReleaseObjectSafe afObj)
          (GM:ReleaseObjectSafe ejeObj)
          nil)))
    nil))
;; CHG3-FIX2 2026-10-10: mapa regional en el MODELO visto por un VIEWPORT
;; anidado en un bloque del layout (Chagual). Transformacion rigida 2D
;; (rot 0, sx=sy). Devuelve (cx cy w h vh vcx vcy ix iy s) o nil:
;;  cx,cy=centro VP (unid. bloque); w,h=tamano VP; vh=alto de vista (modelo);
;;  vcx,vcy=centro de vista (modelo); ix,iy=insercion del bloque; s=escala.
(defun GM:FindNestedMapVP (mbox / ss i e ed nm bd be bed typ ok best bestarea
                              vpx vpw vph v45 v12 vcx vcy sx sy rot)
  (setq best nil bestarea 0.0
        ss (ssget "X" (list (cons 0 "INSERT") (cons 410 (getvar "CTAB")))) i 0)
  (while (and ss (< i (sslength ss)))
    (setq e (ssname ss i) ed (entget e) nm (cdr (assoc 2 ed)) ok nil)
    ;; Sin GetBoundingBox del INSERT: un bloque con VIEWPORT anidado dispara el
    ;; computo de la vista completa y colgaba el prime del lote en /b. El VP
    ;; interno (area mayor) es el discriminante suficiente.
    (setq rot (cdr (assoc 50 ed)) sx (cdr (assoc 41 ed)) sy (cdr (assoc 42 ed)))
    (if (and (or (not rot) (equal rot 0.0)) (numberp sx) (numberp sy)
             (> sx 0.0) (equal sx sy 0.0001))
      (progn
        (setq bd (tblobjname "BLOCK" nm))
        (if bd
          (progn
            (setq be (entnext bd))
            (while (and be (not ok))
              (setq bed (entget be))
              (if (not bed)
                (setq be nil)
                (progn
                  (setq typ (cdr (assoc 0 bed)))
                  (if (= typ "ENDBLK")
                    (setq be nil)
                    (progn
                      (if (= typ "VIEWPORT")
                        (progn
                          (setq vpx (cdr (assoc 10 bed)) vpw (cdr (assoc 40 bed))
                                vph (cdr (assoc 41 bed)) v45 (cdr (assoc 45 bed))
                                v12 (cdr (assoc 12 bed)))
                          (if (and vpx vpw vph v45 v12
                                   (numberp vpw) (numberp vph) (numberp v45)
                                   (> vpw 10.0) (> vph 10.0) (> v45 100.0)
                                   (numberp (car v12)) (numberp (cadr v12)))
                            (progn
                              (setq vcx (car v12) vcy (cadr v12))
                              (if (> (* vpw vph) bestarea)
                                (setq bestarea (* vpw vph)
                                      best (list (car vpx) (cadr vpx) vpw vph v45
                                                 vcx vcy
                                                 (car (cdr (assoc 10 ed)))
                                                 (cadr (cdr (assoc 10 ed)))
                                                 sx)))))))
                      (setq be (entnext be)))))))))
    (setq i (1+ i)))
  )
  )
  best)

;; CHG3-FIX2: modelo -> papel con el VP anidado del mapa. nil si no calculable.
(defun GM:MapVPToPaper (info mx my / cx cy h vh vcx vcy ix iy s kf)
  (if (or (not info) (not (numberp mx)) (not (numberp my)))
    nil
    (progn
      (setq cx (nth 0 info) cy (nth 1 info) h (nth 3 info)
            vh (nth 4 info) vcx (nth 5 info) vcy (nth 6 info)
            ix (nth 7 info) iy (nth 8 info) s (nth 9 info))
      (if (or (<= vh 0.0) (<= h 0.0) (not (numberp s)) (<= s 0.0))
        nil
        (progn
          (setq kf (* s (/ h vh)))
          (list (+ ix (* s cx) (* (- mx vcx) kf))
                (+ iy (* s cy) (* (- my vcy) kf))
                0.0))))))

;; CHG3-FIX2: rectangulo de MODELO visible por el VP anidado (x1 y1 x2 y2).
(defun GM:MapVPModelRect (info / w h vh vcx vcy hw hh)
  (if (not info)
    nil
    (progn
      (setq w (nth 2 info) h (nth 3 info) vh (nth 4 info)
            vcx (nth 5 info) vcy (nth 6 info))
      (if (or (<= h 0.0) (<= w 0.0) (<= vh 0.0))
        nil
        (progn
          (setq hh (/ vh 2.0) hw (* hh (/ w h)))
          (list (- vcx hw) (- vcy hh) (+ vcx hw) (+ vcy hh)))))))

;; CHG3-FIX2: ruta del proyecto dibujada en el MODELO dentro del mapa regional:
;; la curva mas larga en color rojo (ACI 1 / RGB 255,0,0) o magenta (ACI 6 /
;; RGB 255,0,255) con su punto medio dentro del rect visible. (ename . longitud).
(defun GM:LocMapRouteModel (info / rect ss i ent ed col tc r g b obj clen mid best bestlen ok)
  (setq rect (GM:MapVPModelRect info) best nil bestlen 0.0)
  (if rect
    (progn
      (setq ss (ssget "X" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE") (cons 410 "Model"))) i 0)
      (while (and ss (< i (sslength ss)))
        (setq ent (ssname ss i) ed (entget ent) col (cdr (assoc 62 ed)) ok nil)
        (if (or (equal col 1) (equal col 6))
          (setq ok T)
          (progn
            (setq tc (cdr (assoc 420 ed)))
            (if (numberp tc)
              (progn
                (setq r (logand 255 (/ tc 65536))
                      g (logand 255 (/ tc 256))
                      b (logand 255 tc))
                (if (or (and (= r 255) (= g 0) (= b 0))
                        (and (= r 255) (= g 0) (= b 255)))
                  (setq ok T))))))
        (if ok
          (progn
            (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
            (if (vl-catch-all-error-p obj)
              (setq obj nil))
            (setq clen (if obj (GM:CurveLen obj) nil))
            (if (and obj clen (> clen bestlen))
              (progn
                (setq mid (vl-catch-all-apply 'vlax-curve-getPointAtDist (list obj (/ clen 2.0))))
                (if (vl-catch-all-error-p mid) (setq mid nil))
                (if (and mid (GM:LocInBox mid rect))
                  (setq best ent bestlen clen))))
            (GM:ReleaseObjectSafe obj)
            (setq obj nil)))
        (setq i (1+ i)))))
  (if best (list best bestlen) nil))

;; CHG3-FIX2: punto de la ruta del MODELO a la fraccion del eje del proyecto
;; (misma convencion que el metodo ESQUEMA: la punta norte es 0+000).
(defun GM:MapRoutePointAtFrac (route frac / obj rp0 rp1 d0 dlen pt)
  (setq pt nil)
  (if (and route frac (numberp frac))
    (progn
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list route)))
      (if (not (vl-catch-all-error-p obj))
        (progn
          (setq dlen (GM:CurveLen obj)
                rp0 (vl-catch-all-apply 'vlax-curve-getStartPoint (list obj))
                rp1 (vl-catch-all-apply 'vlax-curve-getEndPoint (list obj)))
          (if (vl-catch-all-error-p rp0) (setq rp0 nil))
          (if (vl-catch-all-error-p rp1) (setq rp1 nil))
          (if (and dlen rp0 rp1 (numberp (cadr rp0)) (numberp (cadr rp1)))
            (progn
              (if (>= (cadr rp0) (cadr rp1))
                (setq d0 (* frac dlen))
                (setq d0 (* (- 1.0 frac) dlen)))
              (setq d0 (max 1.0 (min (- dlen 1.0) d0)))
              (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtDist (list obj d0)))
              (if (vl-catch-all-error-p pt) (setq pt nil))))
          (GM:ReleaseObjectSafe obj)
          (setq obj nil))))
  pt))

;; CHG3-FIX2: factor modelo->papel del VP anidado (s * h / vh). 0 si invalido.
(defun GM:MapVPKf (info / s h vh)
  (if (not info)
    0.0
    (progn
      (setq s (nth 9 info) h (nth 3 info) vh (nth 4 info))
      (if (and (numberp s) (> s 0.0) (numberp h) (numberp vh) (> vh 0.0))
        (* s (/ h vh))
        0.0))))

;; CHG3-FIX2: caja barata (solo DXF) de una curva del modelo para el filtro de
;; ventana del zoom: recorre los grupos 10/11 (LINE usa 10 y 11). nil si no hay.
(defun GM:ModelEntBoxDXF (ed / pr p x1 y1 x2 y2 got)
  (setq x1 nil y1 nil x2 nil y2 nil got nil)
  (foreach pr ed
    (if (or (= (car pr) 10) (= (car pr) 11))
      (progn
        (setq p (cdr pr))
        (if (and (numberp (car p)) (numberp (cadr p)))
          (progn
            (if (or (not x1) (< (car p) x1)) (setq x1 (car p)))
            (if (or (not x2) (> (car p) x2)) (setq x2 (car p)))
            (if (or (not y1) (< (cadr p) y1)) (setq y1 (cadr p)))
            (if (or (not y2) (> (cadr p) y2)) (setq y2 (cadr p)))
            (setq got T))))))
  (if got (list x1 y1 x2 y2) nil))

;; CHG3-FIX2: par (10 . punto) para el entmake de las curvas del zoom.
(defun GM:MapVtxPair (p) (cons 10 p))

;; CHG3-FIX2: contenido del zoom cuando el mapa vive en el MODELO (VP anidado):
;; recrea en PAPEL (bubble 5.18x) las curvas del modelo cercanas al punto del
;; predio, con su capa y color. Devuelve un selection set (puede venir vacio).
(defun GM:MapVPZoomContent (vpinfo mpt zoom_frame / kf rect ss i ent ed box2 obj dlen paso plist d cur p p0 n pts col tc lay e)
  (setq kf (GM:MapVPKf vpinfo) p0 (GM:MapVPToPaper vpinfo (car mpt) (cadr mpt)))
  (if (or (<= kf 0.0) (not p0))
    nil
    (progn
      ;; Ventana del modelo: radio del bubble (9.5 papel) / 5.18x, con 20% de aire.
      (setq rect (list (- (car mpt) (/ 2.2 kf)) (- (cadr mpt) (/ 2.2 kf))
                       (+ (car mpt) (/ 2.2 kf)) (+ (cadr mpt) (/ 2.2 kf)))
            ss (ssget "X" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE") (cons 410 "Model"))) i 0
            e (ssadd))
      (while (< i (sslength ss))
        (setq ent (ssname ss i) ed (entget ent) box2 (GM:ModelEntBoxDXF ed))
        (if (and box2 (GM:GeoBoxesOverlap box2 rect 0.0))
          (progn
            (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
            (if (not (vl-catch-all-error-p obj))
              (progn
                (setq dlen (GM:CurveLen obj))
                (if (and dlen (> dlen 0.05))
                  (progn
                    (setq paso (max 6.0 (/ dlen 140.0)) plist nil d 0.0)
                    (while (<= d dlen)
                      (setq cur (vl-catch-all-apply 'vlax-curve-getPointAtDist (list obj d)))
                      (if (not (vl-catch-all-error-p cur))
                        (progn
                          (setq p (GM:MapVPToPaper vpinfo (car cur) (cadr cur)))
                          (if p
                            (setq plist (cons (list (+ (car zoom_frame) (* (- (car p) (car p0)) 5.18))
                                                    (+ (cadr zoom_frame) (* (- (cadr p) (cadr p0)) 5.18))
                                                    0.0)
                                              plist)))))
                      (setq d (+ d paso)))
                    (setq pts (reverse plist) n (length pts))
                    (if (>= n 2)
                      (progn
                        (setq lay (cdr (assoc 8 ed)) col (cdr (assoc 62 ed)) tc (cdr (assoc 420 ed)))
                        (if (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                                                   (cons 8 lay) (cons 410 (getvar "CTAB")))
                                             (if (numberp col) (list (cons 62 col)) nil)
                                             (if (and (numberp tc) (not (numberp col))) (list (cons 420 tc)) nil)
                                             (list '(100 . "AcDbPolyline") (cons 90 n) '(70 . 0))
                                             (mapcar 'GM:MapVtxPair pts)))
                          (ssadd (entlast) e))))))
                (GM:ReleaseObjectSafe obj)
                (setq obj nil))))
        (setq i (1+ i)))
      )
      e)))

;; CHG3-FIX2: nombre del primer XREF insertado (layout o Model) o nil. Un mapa
;; en XREF no tiene cartografia/ruta/textos internos legibles: sin bind no hay
;; fuente de localizacion ni contenido de zoom.
(defun GM:FindMapXref (/ ss i e ed nm bd res)
  (setq res nil i 0
        ss (ssget "X" (list (cons 0 "INSERT"))))
  (while (and ss (< i (sslength ss)) (not res))
    (setq e (ssname ss i) ed (entget e) nm (cdr (assoc 2 ed)) bd (tblsearch "BLOCK" nm))
    (if (and bd (numberp (cdr (assoc 70 bd))) (= 4 (logand 4 (cdr (assoc 70 bd)))))
      (setq res nm))
    (setq i (1+ i)))
  res)

;; PRY1-GEO6 2026-10-14: lectura de la cartografia empotrada DENTRO del bloque
;; MAPA_GEO (caso Chagual: "ESQUEMA DE LOCALIZACION"). El bloque no es legible
;; con ssget del layout; se transforman sus entidades con el INSERT del rol
;; (misma matematica que GM:MapXfBox). Fail-open: sin bloque o sin coincidencia
;; devuelve nil y el flujo conserva el AVISO honesto; no se inventan posiciones.
(defun GM:MapXfPt (insx insy sx sy rot x y / c s)
  (if (or (not (numberp insx)) (not (numberp insy))
          (not (numberp sx)) (not (numberp sy)) (not (numberp rot))
          (not (numberp x)) (not (numberp y)))
    nil
    (progn
      (setq c (cos rot) s (sin rot))
      (list (+ insx (- (* (* x sx) c) (* (* y sy) s)))
            (+ insy (+ (* (* x sx) s) (* (* y sy) c)))
            0.0))))

;; PRY1-GEO6: INSERT del rol MAPA_GEO (o INSERT con ESQUEMA/LOCALIZACION en el
;; nombre como respaldo). nil si no existe.
(defun GM:MapBlockInsert (/ ins ss i e ed nm)
  (setq ins (vl-catch-all-apply 'GM:FindByRole (list "MAPA_GEO" "INSERT")))
  (if (vl-catch-all-error-p ins) (setq ins nil))
  (if (not (and ins (entget ins)))
    (progn
      (setq ins nil
            ss (ssget "X" (list (cons 0 "INSERT") (cons 410 (getvar "CTAB")))) i 0)
      (while (and ss (< i (sslength ss)))
        (setq e (ssname ss i) ed (entget e) nm (cdr (assoc 2 ed)))
        (if (and nm (or (wcmatch (strcase nm) "*ESQUEMA*")
                        (wcmatch (strcase nm) "*LOCALIZACION*")))
          (setq ins e))
        (setq i (1+ i)))))
  ins)

;; PRY1-GEO6: texto (TEXT/MTEXT/ATTDEF) dentro del bloque MAPA_GEO que contiene
;; `target` (clave ya normalizada); devuelve el punto transformado por el INSERT
;; (centrado aproximado con alto/ancho del texto) o nil. Solo lectura.
;; PRY1-GEO7 2026-10-19: publica *GM_MAPBLOCK_LAST_TEXT* con el ename del
;; texto que produjo el match (el localizador lo usa como ancla excluida
;; del zoom); nil si no hubo match.
(defun GM:MapBlockTextFind (mbox target / ins ed nm insx insy sx sy rot bd be bed typ str p th px py pts norm res)
  (setq *GM_MAPBLOCK_LAST_TEXT* nil res nil)
  (if (and (= (type target) 'STR) (/= target ""))
    (progn
      (setq ins (GM:MapBlockInsert))
      (if (and ins (setq ed (entget ins)))
        (progn
          (setq nm (cdr (assoc 2 ed))
                insx (car (cdr (assoc 10 ed))) insy (cadr (cdr (assoc 10 ed)))
                sx (if (numberp (cdr (assoc 41 ed))) (cdr (assoc 41 ed)) 1.0)
                sy (if (numberp (cdr (assoc 42 ed))) (cdr (assoc 42 ed)) 1.0)
                rot (if (numberp (cdr (assoc 50 ed))) (cdr (assoc 50 ed)) 0.0))
          (if (< (abs sx) 0.000001) (setq sx 1.0))
          (if (< (abs sy) 0.000001) (setq sy 1.0))
          (setq bd (tblobjname "BLOCK" nm))
          (if bd
            (progn
              (setq be (entnext bd))
              (while (and be (not res) (/= (cdr (assoc 0 (entget be))) "ENDBLK"))
                (setq bed (entget be) typ (cdr (assoc 0 bed)))
                (if (and (member typ '("TEXT" "MTEXT" "ATTDEF"))
                         (/= (cdr (assoc 60 bed)) 1))
                  (progn
                    (setq str (cdr (assoc 1 bed)))
                    (if (and (= (type str) 'STR) (/= str ""))
                      (progn
                        (setq norm (UBI_NormalizeSearchKey str))
                        (if (and (/= norm "") (vl-string-search target norm))
                          (progn
                            (setq p (cdr (assoc 10 bed)) th (cdr (assoc 40 bed)))
                            (if (not (numberp th)) (setq th 1.4))
                            (if (and p (numberp (car p)) (numberp (cadr p)))
                              (progn
                                 (setq px (+ (car p) (* 0.5 0.62 th (strlen str)))
                                       py (+ (cadr p) (* 0.5 th)))
                                 (setq pts (GM:MapXfPt insx insy sx sy rot px py))
                                 (if (and pts (or (not mbox) (GM:LocInBox pts mbox)))
                                    (progn
                                      (setq *GM_MAPBLOCK_LAST_TEXT* be)
                                      (setq res pts)))))))))))
                  (setq be (entnext be)))))))))
  res)

;; PRY1-GEO6: color de ruta dentro del bloque: 2=magenta (ACI 6 / TC 255,0,255),
;; 1=rojo (ACI 1 / TC 255,0,0 o capa RUTA/ROJ/RED); nil si no es ruta.
(defun GM:MapBlockRouteColor (ed / col tc r g b lay)
  (setq col (cdr (assoc 62 ed)) tc (cdr (assoc 420 ed)))
  (cond
    ((and (numberp tc)
          (progn
            (setq r (logand 255 (/ tc 65536)) g (logand 255 (/ tc 256)) b (logand 255 tc))
            (or (and (= r 255) (= g 0) (= b 255)) (and (= r 255) (= g 0) (= b 0)))))
     2)
    ((equal (abs (if (numberp col) col 0)) 6) 2)
    ((equal (abs (if (numberp col) col 0)) 1) 1)
    (T
      (setq lay (if (= (type (cdr (assoc 8 ed))) 'STR) (strcase (cdr (assoc 8 ed))) ""))
      (if (or (vl-string-search "RUTA" lay) (vl-string-search "ROJ" lay) (vl-string-search "RED" lay))
        1
        nil))))

;; PRY1-GEO6: ruta del mapa dentro del bloque: la curva ABIERTA de ruta mas
;; apta (magenta antes que rojo; a igualdad, la mas larga). Devuelve
;; (ename insx insy sx sy rot inshandle len) o nil. Solo lectura.
(defun GM:MapBlockRouteFind (mbox / ins ed nm insx insy sx sy rot ibh bd be bed typ g70 col len best bestlen bestc)
  (setq best nil bestlen 0.0 bestc 0)
  (setq ins (GM:MapBlockInsert))
  (if (and ins (setq ed (entget ins)))
    (progn
      (setq nm (cdr (assoc 2 ed))
            insx (car (cdr (assoc 10 ed))) insy (cadr (cdr (assoc 10 ed)))
            sx (if (numberp (cdr (assoc 41 ed))) (cdr (assoc 41 ed)) 1.0)
            sy (if (numberp (cdr (assoc 42 ed))) (cdr (assoc 42 ed)) 1.0)
            rot (if (numberp (cdr (assoc 50 ed))) (cdr (assoc 50 ed)) 0.0)
            ibh (cdr (assoc 5 ed)))
      (if (< (abs sx) 0.000001) (setq sx 1.0))
      (if (< (abs sy) 0.000001) (setq sy 1.0))
      (setq bd (tblobjname "BLOCK" nm))
      (if bd
        (progn
          (setq be (entnext bd))
          (while (and be (/= (cdr (assoc 0 (entget be))) "ENDBLK"))
            (setq bed (entget be) typ (cdr (assoc 0 bed)) g70 (cdr (assoc 70 bed)))
            (if (and (member typ '("LINE" "LWPOLYLINE" "POLYLINE" "SPLINE" "ARC"))
                     (not (and (numberp g70) (= 1 (logand 1 g70)))))
              (progn
                (setq col (GM:MapBlockRouteColor bed))
                (if col
                  (progn
                    (setq len (vl-catch-all-apply
                                '(lambda (x) (vlax-curve-getDistAtParam x (vlax-curve-getEndParam x)))
                                (list be)))
                    (if (vl-catch-all-error-p len) (setq len nil))
                    (if (and (numberp len) (> len 15.0)
                             (or (> col bestc) (and (= col bestc) (> len bestlen))))
                      (setq best (list be insx insy sx sy rot ibh len) bestlen len bestc col))))))
            (setq be (entnext be)))))))
  best)

;; PRY1-GEO6: punto de la ruta del bloque a la fraccion `frac` (0..1) de su
;; longitud, transformado a coordenadas del layout. nil si no calculable.
(defun GM:MapBlockRouteFracPt (desc frac / d p)
  (if (and desc (numberp frac) (car desc) (numberp (nth 7 desc)) (> (nth 7 desc) 0.0))
    (progn
      (setq d (* (max 0.0 (min 1.0 frac)) (nth 7 desc)))
      (setq p (vl-catch-all-apply 'vlax-curve-getPointAtDist (list (car desc) d)))
      (if (vl-catch-all-error-p p) (setq p nil))
      (if p (GM:MapXfPt (nth 1 desc) (nth 2 desc) (nth 3 desc) (nth 4 desc) (nth 5 desc) (car p) (cadr p)) nil))
    nil))

;; PRY1-GEO7 2026-10-19 (F1 plan B): inversa de GM:MapXfPt. Dado un punto en
;; PAPEL devuelve su equivalente en coordenadas DE BLOQUE. nil si invalida.
(defun GM:MapXfPtInv (insx insy sx sy rot px py / c s dx dy)
  (if (or (not (numberp insx)) (not (numberp insy))
          (not (numberp sx)) (not (numberp sy)) (not (numberp rot))
          (not (numberp px)) (not (numberp py))
          (< (abs sx) 0.000001) (< (abs sy) 0.000001))
    nil
    (progn
      (setq c (cos rot) s (sin rot)
            dx (- px insx) dy (- py insy))
      (list (/ (+ (* dx c) (* dy s)) sx)
            (/ (- (* dy c) (* dx s)) sy)
            0.0))))

;; PRY1-GEO7 2026-10-19: contenido del circulo grande cuando el mapa vive
;; DENTRO de un bloque (Chagual): recrea en PAPEL el tramo de la ruta del
;; bloque alrededor del punto del predio. desc4 =
;; (ent insx insy sx sy rot ins_handle len param x_ancla y_ancla)
;; (param en unidades de bloque; ancla en papel). Los puntos creados son
;; RELATIVOS al centro de la ventana: el -INSERT a escala 5.18 del pipeline
;; los coloca alrededor de zoom_frame.
(defun GM:MapBlockZoomContent (desc4 zoom_center / ent ed insx insy sx sy rot dlen dpar x_a y_a
                                         dw dst d p pp pts lay col tc e ok)
  (if (and (= (type desc4) 'LIST)
           (car desc4) (entget (car desc4))
           (numberp (nth 3 desc4)) (> (nth 3 desc4) 0.0)
           (numberp (nth 7 desc4)) (> (nth 7 desc4) 0.0)
           (numberp (nth 8 desc4)) (= (type zoom_center) 'LIST)
           (numberp (car zoom_center)) (numberp (cadr zoom_center))
           (numberp (nth 9 desc4)) (numberp (nth 10 desc4)))
    (progn
      (setq ent (car desc4) ed (entget ent)
            insx (nth 1 desc4) insy (nth 2 desc4)
            sx (nth 3 desc4) sy (nth 4 desc4) rot (nth 5 desc4)
            dlen (nth 7 desc4) dpar (nth 8 desc4)
            x_a (nth 9 desc4) y_a (nth 10 desc4)
            lay (cdr (assoc 8 ed))
            col (cdr (assoc 62 ed)) tc (cdr (assoc 420 ed))
            pts nil ok nil)
      ;; ventana en unidades de bloque: radio de burbuja (9.5)/5.18x + 15% aire.
      (setq dw (/ (* 9.5 1.15) 5.18 sx))
      ;; muestra del tramo: paso denso (dw/6) de lado a lado del centro.
      (setq dst (* dw 0.16)
            d (max 0.0 (- dpar (* 1.2 dw))))
      (while (<= d (min dlen (+ dpar (* 1.2 dw))))
        (setq p (vl-catch-all-apply 'vlax-curve-getPointAtDist (list ent d)))
        (if (and (not (vl-catch-all-error-p p)) (numberp (car p)) (numberp (cadr p)))
          (progn
            (setq pp (GM:MapXfPt insx insy sx sy rot (car p) (cadr p)))
            (if pp
              (progn
                (setq pts (cons (list (+ (car zoom_center) (- (car pp) x_a))
                                      (+ (cadr zoom_center) (- (cadr pp) y_a))
                                      0.0)
                                pts))))))
        (setq d (+ d dst)))
      (if (and pts (>= (length pts) 2))
        (progn
          (if (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                                     (cons 8 (if lay lay "0"))
                                     (cons 410 (getvar "CTAB")))
                               (if (numberp col) (list (cons 62 col)) nil)
                               (if (and (numberp tc) (not (numberp col))) (list (cons 420 tc)) nil)
                               (list '(100 . "AcDbPolyline") (cons 90 (length pts)) '(70 . 0))
                               (mapcar 'GM:MapVtxPair (reverse pts))))
            (progn (setq e (ssadd (entlast)) ok T))
            nil))
        nil))
    nil)
  (if ok e nil)
)


(defun GM:AutoLocatePredioAffected (row / reference target target_digits dist_targs sect_targ prov_targ
                                     mapbox mbox method ss i ent obj value normval center anchor p1 p2
                                     source_ss source_handles source_ent route route_len route_color
                                     route_dist route_pt prog_ini prog_fin prog_mid max_prog
                                     candidate_len candidate_color route_handle text_handle truecol red_true
                                     cand_mid cand_box rinfo snap_pt curdist newdist delta_paper
                                      tinfo frac rp0 rp1 d0 afutm routeObj ltext route_obj vpinfo mroute mpt xref
                                      bpt mapins route_blk xf_chk
                                      anchor_blk bk_pt delta_blk
                                      target_sp dist_raws sect_raw prov_raw cand_raw cand_sp val_sp win_k)
  (setq *GM_AUTO_PREDIO_LOC_RECT* nil *GM_AUTO_PREDIO_LOC_ENT_HANDLES* nil
        reference (nth 13 row) target (if reference (UBI_NormalizeSearchKey reference) "")
        target_sp (GM:GeoSpacedNorm reference)
        target_digits (GM:AutoLocateDigits reference)
        ;; PRY1-GEO4 2026-10-13: la col 12 admite dos distritos (el poligono AF
        ;; puede cruzar distritos); el localizador prueba cada uno como candidato.
        dist_targs (mapcar 'UBI_NormalizeSearchKey (GM:GeoSplitDistritos (if (nth 11 row) (nth 11 row) "")))
        sect_targ (UBI_NormalizeSearchKey (if (nth 6 row) (nth 6 row) ""))
        prov_targ (UBI_NormalizeSearchKey (if (nth 9 row) (nth 9 row) ""))
        route_handle nil text_handle nil method "NINGUNO" mbox nil
        *GM_AUTO_MAPVP_INFO* nil *GM_AUTO_MAPVP_MODELPT* nil *GM_AUTO_PREDIO_LOC_ANCHOR* nil
        *GM_AUTO_ZOOM_FRAME* nil
        ;; PRY1-GEO7 (F1): estado del mapa-en-bloque por codigo (zoom y ancla).
        *GM_AUTO_PREDIO_ZOOM_BLK* nil *GM_MAPBLOCK_LAST_TEXT* nil)
  ;; CHG3-FIX2: descartar referencias de relleno ('t', '-', cortas o sin
  ;; digitos): no son una U.C. utilizable y ensuciaban la busqueda de textos.
  (if (or (< (strlen target) 4) (= (GM:AutoLocateDigits target) ""))
    (setq target ""))
  ;; Se abre TODO el mapa: marco completo sobre el titulo (no ventana fija).
  (setq mbox (GM:MapContentBox) *GM_AUTO_MAPCONTENT_BOX* mbox)
  (if (= *GM_AUTO_SHOW_PREDIO_LOC* "1")
    (progn
      ;; CHG3-FIX2: mapa regional en el MODELO (VP anidado en un bloque): el
      ;; predio se localiza por la fraccion del eje del proyecto sobre la ruta
      ;; dibujada en el mapa, transformada a papel con el propio VP. La ruta se
      ;; busca en rojo o magenta (trazado esquematico sobre la cartografia).
      (if (not center)
        (progn
          (setq vpinfo (GM:FindNestedMapVP mbox))
          (if vpinfo
            (progn
              (setq mroute (GM:LocMapRouteModel vpinfo))
              (if mroute
                (progn
                  (setq tinfo (GM:EjeFractionT *ML_TARGET_CODE*))
                  (if (and tinfo (numberp (car tinfo)) (> (cadr tinfo) 0.0))
                    (progn
                      (setq mpt (GM:MapRoutePointAtFrac (car mroute) (car tinfo)))
                      (if mpt
                        (progn
                          (setq center (GM:MapVPToPaper vpinfo (car mpt) (cadr mpt)))
                          (if (and center (GM:LocInBox center mbox))
                            (progn
                              (setq method "ESQUEMA_VP"
                                    *GM_AUTO_MAPVP_INFO* vpinfo
                                    *GM_AUTO_MAPVP_MODELPT* mpt)
                              ;; PRY1-MULTI F1 (GEO7): diagnostico de confianza
                              ;; del VP anidado (AF UTM proyectado vs el punto
                              ;; elegido sobre la ruta); INFO, no altera nada.
                              (setq afutm (GM:ModelAFCenter *ML_TARGET_CODE*))
                              (if afutm
                                (progn
                                  (setq xf_chk (GM:MapVPToPaper vpinfo (car afutm) (cadr afutm)))
                                  (if xf_chk
                                    (GM:AutoTrace *ML_TARGET_CODE* "PREDIO_LOC_XF_CHECK" "INFO"
                                      (strcat "metodo=ESQUEMA_VP delta="
                                              (rtos (distance xf_chk center) 2 2)
                                              " mm (afutm vs ruta)"))))))
                            (setq center nil))))))))
                ;; CHG3-FIX2: mapa sin ruta interna legible (p.ej. XREF): sin
                ;; bind no hay fuente de localizacion; se traza la causa.
                (progn
                  (setq xref (GM:FindMapXref))
                  (if xref
                    (GM:AutoTrace *ML_TARGET_CODE* "MAPA_XREF" "AVISO"
                      (strcat "mapa en XREF (" xref "); bindeelo para habilitar circulos y zoom"))))))))
      ;; T) Ticks UTM del marco: el AF del esquema (UTM del Model) se proyecta
      ;; exacto en TODO el mapa (arriba/abajo segun el CSV, no siempre al medio).
      ;; PRY1-MULTI F1 (GEO7): usa el transform unificado primado por lote
      ;; (tipo TICKS); en manual/sin primado cae al calculo vivo equivalente.
      (if (not center)
        (progn
          (setq tinfo (if (and *GM_AUTO_GEO_XF* (listp *GM_AUTO_GEO_XF*))
                        *GM_AUTO_GEO_XF*
                        (GM:GeoUnifiedXf mbox))
                afutm nil)
          (if (and tinfo (equal (nth 4 tinfo) "TICKS"))
            (progn
              (setq afutm (GM:ModelAFCenter *ML_TARGET_CODE*))
              (if afutm
                (progn
                  (setq center (list (+ (cadr tinfo) (* (car tinfo) (car afutm)))
                                     (+ (nth 3 tinfo) (* (nth 2 tinfo) (cadr afutm))) 0.0))
                  (if (and mbox (not (GM:LocInBox center (list (- (car mbox) 15.0) (- (cadr mbox) 15.0) (+ (nth 2 mbox) 15.0) (+ (nth 3 mbox) 15.0)))))
                    (setq center nil)
                    (if center (setq method "TICKS")))))))))
      ;; 0) GUIA DEL ESQUEMA: la rosada del esquema es la roja del mapa.
      ;; Fraccion del AF sobre el EJE del Model -> mismo punto sobre la roja
      ;; regional medido desde la punta norte (0+000 arriba). Asi el predio
      ;; de la punta (p.ej. HS-MC-005) cae en la punta y no en el medio.
      (if (not center)
        (progn
          (setq rinfo (GM:LocRegionalRoute mbox))
          (if rinfo
            (progn
              (setq route (car rinfo) route_len (cadr rinfo) route_handle (caddr rinfo))))
          (if (and route (> route_len 0.0))
            (progn
              (setq tinfo (GM:EjeFractionT *ML_TARGET_CODE*))
              (if (and tinfo (numberp (car tinfo)) (> (cadr tinfo) 0.0))
                (progn
                  (setq frac (car tinfo)
                        routeObj (vl-catch-all-apply 'vlax-ename->vla-object (list route)))
                  (if (vl-catch-all-error-p routeObj) (setq routeObj nil))
                  (setq rp0 (if routeObj (vl-catch-all-apply 'vlax-curve-getStartPoint (list routeObj)) nil)
                        rp1 (if routeObj (vl-catch-all-apply 'vlax-curve-getEndPoint (list routeObj)) nil))
                  ;; FIX-AV: soltar el wrapper de la ruta tras medir sus puntas.
                  (GM:ReleaseObjectSafe routeObj)
                  (setq routeObj nil)
                  (if (vl-catch-all-error-p rp0) (setq rp0 nil))
                  (if (vl-catch-all-error-p rp1) (setq rp1 nil))
                  (if (and rp0 rp1 (numberp (cadr rp0)) (numberp (cadr rp1)))
                    (progn
                      (if (>= (cadr rp0) (cadr rp1))
                        (setq d0 (* frac route_len))
                        (setq d0 (* (- 1.0 frac) route_len)))
                      (setq d0 (max 1.0 (min (- route_len 1.0) d0)))
                      (setq center (vl-catch-all-apply 'vlax-curve-getPointAtDist (list route d0)))
                      (if (vl-catch-all-error-p center) (setq center nil))
                      (if center (setq method "ESQUEMA"))))))))))
      ;; 1) U.C. exacta, pero SOLO dentro del mapa (membrete/cuadros no valen).
      (if (and (/= target "") (not center))
        (progn
          (if *GM_AUTO_LTEXT_READY*
            ;; PA-3 (H7): pasada pura sobre el indice del lote.
            (foreach ltext *GM_AUTO_LTEXT_INDEX*
              (if (not center)
                (progn
                  (setq ent (car ltext) value (cadr ltext) normval (UBI_NormalizeSearchKey value)
                        val_sp (GM:GeoSpacedNorm value))
                  ;; PRY1-MULTI F2 (GEO8): match estricto (exacto, por token o
                  ;; digitos identicos); el search superstring quedaba expuesto
                  ;; a falsos positivos ("124623" no es la U.C. "24623").
                  (if (or (= target normval)
                          (GM:GeoSpacedMatchP target_sp val_sp)
                          (and (/= target_digits "")
                               (= target_digits (GM:AutoLocateDigits value))))
                    (progn
                      (setq center (GM:LocTextCenter ent))
                      (if (and center (or (not mbox) (GM:LocInBox center mbox)))
                        (setq text_handle (cdr (assoc 5 (entget ent))) method "UC")
                        (setq center nil)))))))
            (progn
              (setq ss (ssget "X" (list (cons 0 "TEXT,MTEXT") (cons 410 (getvar "CTAB")))) i 0)
              (while (and ss (< i (sslength ss)) (not center))
                (setq ent (ssname ss i))
                ;; AUD1: creacion con catch; entidad caida = se salta.
                (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
                (if (vl-catch-all-error-p obj) (setq obj nil))
                (setq value (if obj (vl-catch-all-apply 'vla-get-TextString (list obj)) nil))
                (if (and (not (vl-catch-all-error-p value)) value)
                  (progn
                    (setq normval (UBI_NormalizeSearchKey value) val_sp (GM:GeoSpacedNorm value))
                    ;; PRY1-MULTI F2 (GEO8): mismo match estricto del indice.
                    (if (or (= target normval)
                            (GM:GeoSpacedMatchP target_sp val_sp)
                            (and (/= target_digits "")
                                 (= target_digits (GM:AutoLocateDigits value))))
                      (progn
                        (setq center (GM:LocTextCenter ent))
                        (if (and center (or (not mbox) (GM:LocInBox center mbox)))
                          (setq text_handle (cdr (assoc 5 (entget ent))) method "UC")
                          (setq center nil))))))
                ;; FIX-AV: soltar el wrapper del texto antes de seguir.
                (GM:ReleaseObjectSafe obj)
                (setq i (1+ i)))))))
      ;; 2) El predio del esquema izquierdo se identifica en el mapa por su
      ;; DISTRITO (rotulado en el regional), luego SECTOR, luego PROVINCIA.
      ;; PRY1-GEO4: los distritos de la fila (uno o dos) entran como candidatos.
      ;; PRY1-MULTI F2 (GEO8): candidatos desde el valor crudo para el match
      ;; por token (cand_sp) y longitud minima (placeholders tipo "t" fuera).
      (if (not center)
        (progn
          (setq dist_raws (GM:GeoSplitDistritos (if (nth 11 row) (nth 11 row) ""))
                sect_raw (if (nth 6 row) (nth 6 row) "")
                prov_raw (if (nth 9 row) (nth 9 row) ""))
          (foreach cand_raw (append dist_raws (list sect_raw prov_raw))
            (setq cand_targ (UBI_NormalizeSearchKey cand_raw) cand_sp (GM:GeoSpacedNorm cand_raw))
            (if (and (not center) (>= (strlen cand_targ) 3))
              (if *GM_AUTO_LTEXT_READY*
                ;; PA-3 (H7): pasada pura sobre el indice del lote.
                (foreach ltext *GM_AUTO_LTEXT_INDEX*
                  (if (not center)
                    (progn
                      (setq ent (car ltext) value (cadr ltext) val_sp (GM:GeoSpacedNorm value))
                      (if (or (GM:GeoSpacedMatchP cand_sp val_sp)
                              (and (>= (strlen val_sp) 4) (GM:GeoSpacedMatchP val_sp cand_sp)))
                        (progn
                          (setq center (GM:LocTextCenter ent))
                          (if (and center mbox (not (GM:LocInBox center mbox)))
                            (setq center nil)
                            (if center (setq text_handle (cdr (assoc 5 (entget ent))) method "DISTRITO")))))))) 
                (progn
                  (setq ss (ssget "X" (list (cons 0 "TEXT,MTEXT") (cons 410 (getvar "CTAB")))) i 0)
                  (while (and ss (< i (sslength ss)) (not center))
                    (setq ent (ssname ss i))
                    ;; AUD1: creacion con catch; entidad caida = se salta.
                    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
                    (if (vl-catch-all-error-p obj) (setq obj nil))
                    (setq value (if obj (vl-catch-all-apply 'vla-get-TextString (list obj)) nil))
                    (if (and (not (vl-catch-all-error-p value)) value)
                      (progn
                        (setq val_sp (GM:GeoSpacedNorm value))
                        (if (or (GM:GeoSpacedMatchP cand_sp val_sp)
                                (and (>= (strlen val_sp) 4) (GM:GeoSpacedMatchP val_sp cand_sp)))
                          (progn
                            (setq center (GM:LocTextCenter ent))
                            (if (and center mbox (not (GM:LocInBox center mbox)))
                              (setq center nil)
                              (if center (setq text_handle (cdr (assoc 5 (entget ent))) method "DISTRITO")))))))
                    ;; FIX-AV: soltar el wrapper del texto antes de seguir.
                    (GM:ReleaseObjectSafe obj)
                    (setq i (1+ i)))))))))
      ;; 2c) PRY1-GEO6 2026-10-14: el mapa regional puede vivir DENTRO de un
      ;; bloque (Chagual: "ESQUEMA DE LOCALIZACION"); sus textos no estan en el
      ;; layout. Se busca DISTRITO/SECTOR/PROVINCIA en los textos del bloque,
      ;; transformados por el INSERT del rol. Fail-open: sin coincidencia queda
      ;; el AVISO honesto.
      (if (not center)
        (progn
          (setq bpt nil)
          (foreach cand_targ (append dist_targs (list sect_targ prov_targ))
            (if (and (not bpt) (/= cand_targ "") (not center))
              (setq bpt (GM:MapBlockTextFind mbox cand_targ))))
          (if bpt
            (progn
              (setq center bpt method "DISTRITO")
              ;; PRY1-GEO7 (F1): el rotulo ancla vive DENTRO del bloque; su
              ;; handle sale al zoom para que LAY lo excluya de la copia.
              (if *GM_MAPBLOCK_LAST_TEXT*
                (setq text_handle (cdr (assoc 5 (entget *GM_MAPBLOCK_LAST_TEXT*)))))
              (GM:AutoTrace *ML_TARGET_CODE* "PREDIO_LOC_MAPA" "INFO"
                "distrito/sector/provincia localizado en textos del bloque MAPA_GEO")))))
      ;; 2b) El esquema izquierdo identifica el predio (ancla en center);
      ;; se clava el circulo sobre la linea roja del medio del mapa regional.
      ;; Mas un leve desfase por progresiva para distinguir predios vecinos.
      (if (and center (or (= method "UC") (= method "DISTRITO") (= method "TICKS")))
        (progn
          (setq anchor center rinfo (GM:LocRegionalRoute mbox))
          (if rinfo
            (progn
              (setq route (car rinfo) route_len (cadr rinfo) route_handle (caddr rinfo))
              ;; F15-R1: wrapper unico de la ruta con catch, liberado al final
              ;; del bloque (antes se creaba dos veces y nunca se soltaba).
              (setq route_obj (vl-catch-all-apply 'vlax-ename->vla-object (list route)))
              (if (vl-catch-all-error-p route_obj) (setq route_obj nil))
              (if route_obj
                (progn
                  (setq snap_pt (vl-catch-all-apply 'vlax-curve-getClosestPointTo
                                 (list route_obj anchor)))
                  (if (vl-catch-all-error-p snap_pt) (setq snap_pt nil))
                  ;; TICKS ya es exacto: solo se clava si la roja esta cerca.
                  (if (and snap_pt (= method "TICKS") (> (distance center snap_pt) 6.0))
                    (setq snap_pt nil))
                  (if (and snap_pt (not (vl-catch-all-error-p snap_pt)))
                    (progn
                      (setq center snap_pt method (strcat method "+ROJA"))
                      ;; TICKS queda exacto: desfase 0 (rem 20/40 = 20).
                      (setq prog_mid (if (= method "TICKS+ROJA") 20.0 (GM:AutoProgressValue (nth 12 row))))
                      (if (not (numberp prog_mid)) (setq prog_mid 0.0))
                      (setq delta_paper (* (- (rem prog_mid 40.0) 20.0) 0.1))
                      (setq curdist (vl-catch-all-apply 'vlax-curve-getDistAtPoint
                                      (list route_obj center)))
                      (if (and (not (vl-catch-all-error-p curdist)) (numberp curdist))
                        (progn
                          (setq newdist (max 2.0 (min (- route_len 2.0) (+ curdist delta_paper))))
                          (setq snap_pt (vl-catch-all-apply 'vlax-curve-getPointAtDist
                                          (list route newdist)))
                          (if (and snap_pt (not (vl-catch-all-error-p snap_pt)))
                            (setq center snap_pt))))))
              (GM:ReleaseObjectSafe route_obj)
              (setq route_obj nil))))
            ;; PRY1-GEO7 2026-10-19 (F1 plan B): mapa en bloque (Chagual) sin
            ;; ruta de layout: se clava sobre la ruta empotrada en el bloque,
            ;; transformada a papel por el INSERT del rol MAPA_GEO. Se invierte
            ;; la ancla a coordenadas de bloque, punto mas cercano de la ruta,
            ;; desfase por progresiva (+ delta/sx) y de vuelta a papel.
            (progn
              (setq route_blk (GM:MapBlockRouteFind mbox))
              (if (and route_blk (nth 0 route_blk)
                       (numberp (nth 7 route_blk)) (> (nth 7 route_blk) 0.0))
                (progn
                  (setq anchor_blk (GM:MapXfPtInv (nth 1 route_blk) (nth 2 route_blk)
                                                  (nth 3 route_blk) (nth 4 route_blk)
                                                  (nth 5 route_blk) (car anchor) (cadr anchor)))
                  (if (and anchor_blk (numberp (car anchor_blk)) (numberp (cadr anchor_blk)))
                    (progn
                      (setq route_obj (vl-catch-all-apply 'vlax-ename->vla-object
                                        (list (nth 0 route_blk))))
                      (if (vl-catch-all-error-p route_obj) (setq route_obj nil))
                      (if route_obj
                        (progn
                          (setq snap_pt (vl-catch-all-apply 'vlax-curve-getClosestPointTo
                                           (list route_obj anchor_blk)))
                          (if (vl-catch-all-error-p snap_pt) (setq snap_pt nil))
                          (if (and snap_pt (numberp (car snap_pt)) (numberp (cadr snap_pt)))
                            (progn
                              (setq prog_mid (if (= method "TICKS") 20.0
                                               (GM:AutoProgressValue (nth 12 row))))
                              (if (not (numberp prog_mid)) (setq prog_mid 0.0))
                              (setq delta_paper (* (- (rem prog_mid 40.0) 20.0) 0.1)
                                    delta_blk (/ delta_paper (nth 3 route_blk)))
                              (setq curdist (vl-catch-all-apply 'vlax-curve-getDistAtPoint
                                              (list route_obj snap_pt)))
                              (if (and (not (vl-catch-all-error-p curdist)) (numberp curdist))
                                (progn
                                  (setq newdist (max 2.0 (min (- (nth 7 route_blk) 2.0)
                                                              (+ curdist delta_blk))))
                                  (setq bk_pt (vl-catch-all-apply 'vlax-curve-getPointAtDist
                                                  (list (nth 0 route_blk) newdist)))
                                  (if (and bk_pt (not (vl-catch-all-error-p bk_pt)))
                                    (progn
                                      (setq center (GM:MapXfPt (nth 1 route_blk) (nth 2 route_blk)
                                                               (nth 3 route_blk) (nth 4 route_blk)
                                                               (nth 5 route_blk) (car bk_pt) (cadr bk_pt)))
                                      (if (and center (GM:LocInBox center mbox))
                                        (progn
                                          (setq method (strcat method "+ROJA")
                                                route_handle (nth 6 route_blk)
                                                *GM_AUTO_PREDIO_ZOOM_BLK*
                                                  (append route_blk
                                                          (list newdist (car center) (cadr center))))
                                          (GM:AutoTrace *ML_TARGET_CODE* "PREDIO_LOC_BLOQUE" "INFO"
                                            (strcat "snap a ruta del bloque d="
                                                    (rtos newdist 2 1)
                                                    " len=" (rtos (nth 7 route_blk) 2 1))))
                                        ;; fuera del mapa (borde): ancla vuelve al
                                        ;; rotulo; el zoom muestra la ruta del entorno.
                                        (progn
                                          (setq center anchor
                                                *GM_AUTO_PREDIO_ZOOM_BLK*
                                                  (append route_blk
                                                          (list curdist (car anchor) (cadr anchor))))))))))))))
                      (GM:ReleaseObjectSafe route_obj)
                      (setq route_obj nil)))))))))
      ;; 3) Ultimo recurso: ruta roja del marco por progresiva (sin ancla textual).
      (if (not center)
        (progn
          (setq prog_ini (GM:AutoProgressValue (nth 10 row))
                prog_fin (GM:AutoProgressValue (nth 12 row))
                prog_mid (/ (+ prog_ini prog_fin) 2.0)
                max_prog 0.0)
          (foreach source_ent *GM_AUTO_PREVIEW_ROWS*
            (setq max_prog (max max_prog (GM:AutoProgressValue (nth 10 source_ent))
                                (GM:AutoProgressValue (nth 12 source_ent)))))
          (if (<= max_prog 0.0) (setq max_prog (max prog_fin prog_mid 1.0)))
          (setq rinfo (GM:LocRegionalRoute mbox))
          (if rinfo
            (progn
              (setq route (car rinfo) route_len (cadr rinfo) route_handle (caddr rinfo))))
          ;; PRY1-GEO6 2026-10-14: ultimo recurso con la ruta empotrada en el
          ;; bloque MAPA_GEO (solo si no hay ruta legible en el layout).
          (if (and (not route) (not route_blk))
            (setq route_blk (GM:MapBlockRouteFind mbox)))
          (if (and route_blk (not route) (nth 7 route_blk))
            (setq route_len (nth 7 route_blk)))
          (if (and (or route route_blk) (> route_len 50.0) (> max_prog 0.0))
            (progn
              (setq route_dist (* route_len (max 0.05 (min 0.95 (/ prog_mid max_prog)))))
              (if route
                (setq route_pt (vl-catch-all-apply 'vlax-curve-getPointAtDist (list route route_dist)))
                (setq route_pt (GM:MapBlockRouteFracPt route_blk (max 0.05 (min 0.95 (/ prog_mid max_prog))))))
              (if (and route_pt (not (vl-catch-all-error-p route_pt)))
                (progn
                  (setq center route_pt method "PROG")
                  ;; PRY1-GEO7 (F1): si el punto salio de la ruta del bloque,
                  ;; el zoom reproduce el tramo en vez de copiar el INSERT x6.
                  (if (and route_blk (not route))
                    (setq *GM_AUTO_PREDIO_ZOOM_BLK*
                      (append route_blk
                              (list (* (max 0.05 (min 0.95 (/ prog_mid max_prog)))
                                       (nth 7 route_blk))
                                     (car route_pt) (cadr route_pt)))))))))))
      (if center
        (progn
          ;; Ventana ajustada al recorte del zoom (11/4 = 2.75): solo lo visible.
          ;; PRY1-MULTI F2 (GEO8): en mapas anchos (>150) la ventana crece
          ;; proporcional (k hasta 1.6); en los mapas validados (Huanca 110,
          ;; Chagual 103) k=1.0 conserva la ventana historica intacta.
          (setq win_k (if (and mbox (> (- (nth 2 mbox) (car mbox)) 150.0))
                        (min 1.6 (/ (- (nth 2 mbox) (car mbox)) 100.0))
                        1.0)
                p1 (list (- (car center) (* 3.0 win_k)) (- (cadr center) (* 2.5 win_k)) 0.0)
                p2 (list (+ (car center) (* 3.0 win_k)) (+ (cadr center) (* 2.5 win_k)) 0.0)
                source_ss (ssget "_C" p1 p2) source_handles nil i 0)
          (if source_ss
            (while (< i (sslength source_ss))
              (setq source_ent (ssname source_ss i))
              (if (entget source_ent)
                (setq source_handles (cons (cdr (assoc 5 (entget source_ent))) source_handles)))
              (setq i (1+ i))))
          (if (not source_handles)
            (progn
              (if text_handle (setq source_handles (cons text_handle source_handles)))
              (if (and route_handle (not (member route_handle source_handles)))
                (setq source_handles (cons route_handle source_handles)))
              ;; PRY1-GEO6: mapa dentro de un bloque: el zoom copia el INSERT
              ;; del rol MAPA_GEO (sus entidades internas no son seleccionables).
              (setq mapins (GM:MapBlockInsert))
              (if (and mapins (entget mapins)
                       (not (member (cdr (assoc 5 (entget mapins))) source_handles)))
                (setq source_handles (cons (cdr (assoc 5 (entget mapins))) source_handles)))))
          (if (not source_handles)
            (progn
              (setq source_ss (ssget "_C"
                               (list (- (car center) (* 6.0 win_k)) (- (cadr center) (* 5.0 win_k)) 0.0)
                               (list (+ (car center) (* 6.0 win_k)) (+ (cadr center) (* 5.0 win_k)) 0.0))
                    i 0)
              (if source_ss
                (while (< i (sslength source_ss))
                  (setq source_ent (ssname source_ss i))
                  (if (entget source_ent)
                    (setq source_handles (cons (cdr (assoc 5 (entget source_ent))) source_handles)))
                  (setq i (1+ i))))))
          (setq *GM_AUTO_PREDIO_LOC_RECT* (list p1 p2)
                *GM_AUTO_PREDIO_LOC_ENT_HANDLES* source_handles
                *GM_AUTO_PREDIO_LOC_ANCHOR* text_handle
                ;; PRY1-MULTI F4 2026-10-14: marco unico del zoom publicado
                ;; (center del RECT == center hallado); consumidores: LAY y
                ;; el rotulo GEO. Paridad exacta con la matematica historica.
                *GM_AUTO_ZOOM_FRAME* (if (boundp 'GM:ZoomFrameFor)
                                      (GM:ZoomFrameFor center mbox)
                                      (if (and mbox center)
                                        (list (- (car center) 25.0) (+ (cadr center) 5.0) 0.0)
                                        nil)))
          (GM:AutoTrace *ML_TARGET_CODE* "PREDIO_LOC_METODO" "OK"
            (strcat "metodo=" method " entidades=" (itoa (length source_handles))))
          ;; PRY1-MULTI F2 (GEO8): peso de la fuente ganadora (auditoria; la
          ;; cascada va en el mismo orden de pesos, sin umbral de rechazo).
          (GM:AutoTrace *ML_TARGET_CODE* "PREDIO_LOC_SCORE" "INFO"
            (strcat "metodo=" method " score=" (rtos (GM:GeoMethodScore method) 2 2)))
          T)
        nil))
    nil)
)
