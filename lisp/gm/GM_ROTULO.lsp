;; ============================================================================
;; GM_ROTULO.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


;; AO: orden mayor-area-primero (copia ordenada; el original conserva sufijos).
(defun GM:OrdenarAfectaciones (recs)
  (if (and recs (cdr recs))
    (vl-sort (append recs nil)
             '(lambda (a b) (> (GM:AF_AreaM2 a) (GM:AF_AreaM2 b))))
    recs))

(defun GM:GeoFijoOverlap (bx1 by1 bx2 by2 cx cy r)
  (not (or (< bx2 (- cx r)) (> bx1 (+ cx r)) (< by2 (- cy r)) (> by1 (+ cy r)))))
(defun GM:GeoBoxesOverlap (a b margin / m)
  (setq m (if (numberp margin) margin 0.0))
  (not (or (<= (nth 2 a) (- (car b) m)) (>= (car a) (+ (nth 2 b) m))
           (<= (nth 3 a) (- (cadr b) m)) (>= (cadr a) (+ (nth 3 b) m)))))
;; PA-1 (H1) 2026-09-28: valor CSV saneado para el rotulo geografico.
;; #N/A / vacio / "-" -> respaldo historico (una plantilla sin esos datos
;; conserva el rotulo anterior); nunca inventa un dato a medias.
(defun GM:GeoSafeValue (value fallback / s)
  (setq s (if (= (type value) 'STR) (vl-string-trim " \t\r\n'" value) ""))
  (if (or (= s "") (= (strcase s) "#N/A") (= s "-")) fallback s)
)
;; PRY1-GEO4 2026-10-13: la col 12 del CSV admite uno o DOS distritos (el
;; poligono AF puede cruzar distritos): "CHUGAY - COCHORCO". Separadores
;; validos: - / | (con o sin espacios). Devuelve la lista de partes limpias;
;; sin separadores devuelve una sola parte. Los segmentos vacios se descartan.
(defun GM:GeoSplitDistritos (value / s i c out tok)
  (setq s (if (= (type value) 'STR) value "") i 1 out nil tok "")
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (member c '("-" "/" "|"))
      (progn
        (setq tok (vl-string-trim " \t\r\n" tok))
        (if (/= tok "") (setq out (cons tok out)))
        (setq tok ""))
      (setq tok (strcat tok c)))
    (setq i (1+ i)))
  (setq tok (vl-string-trim " \t\r\n" tok))
  (if (/= tok "") (setq out (cons tok out)))
  (reverse out)
)
;; PA-1 (H1) 2026-09-28: parte un texto en lineas de <= maxc caracteres
;; aproximados, por palabras; devuelve ("") si el texto queda vacio.
(defun GM:GeoWrapText (txt maxc / rest pos word line out)
  (setq rest (if (= (type txt) 'STR) (vl-string-trim " \t\r\n" txt) "")
        line "" out nil)
  (while (/= rest "")
    (setq pos (vl-string-search " " rest))
    (if pos
      (setq word (substr rest 1 pos) rest (vl-string-trim " " (substr rest (+ pos 1))))
      (setq word rest rest ""))
    (if (/= word "")
      (if (= line "")
        (setq line word)
        (if (<= (+ (strlen line) 1 (strlen word)) maxc)
          (setq line (strcat line " " word))
          (setq out (cons line out) line word)))))
  (if (/= line "") (setq out (cons line out)))
  (if out (reverse out) (list ""))
)
;; CHG5 2026-10-12: paso vertical del rotulo GEO. Con el factor 1.35 el aire
;; entre lineas de altura 1.5 quedaba en ~0.3 y las fuentes TTF se encimaban
;; (detalle reportado en Chagual). 1.7, con la MISMA formula en la caja y en
;; el dibujo, evita que caja y trazo diverjan.
(defun GM:GeoLinePitch (h)
  (max 1.7 (* (if (numberp h) h 1.5) 1.7))
)
;; PA-1 (H1) 2026-09-28: alto total de una caja de lineas del rotulo
;; (misma formula de avance que usa el dibujo: GM:GeoLinePitch).
(defun GM:GeoItemsHeight (items / total item)
  (setq total 1.2)
  (foreach item items (setq total (+ total (GM:GeoLinePitch (nth 1 item)))))
  (+ total 0.25)
)
;; PRY1-MULTI F3 (GEO9) 2026-10-14: medio ancho del rotulo medido con
;; GM:TextMeasure por linea (estilo bold real); respaldo 0.62*h*len si la
;; medicion no esta disponible. Clamp [8.0, 14.0]: la caja que se evita es la
;; que se dibuja, sin encoger el texto ni romper el diseno historico.
(defun GM:GeoLabelHalfWidth (items / mx w anyres)
  (setq mx 0.0)
  (foreach item items
    (setq anyres (vl-catch-all-apply 'GM:TextMeasure (list (nth 0 item) (nth 1 item) (nth 3 item)))
          w (if (and anyres (not (vl-catch-all-error-p anyres)) (numberp anyres) (> anyres 0.0))
              anyres
              (* 0.62 (nth 1 item) (strlen (nth 0 item)))))
    (if (and (numberp w) (> w mx)) (setq mx w)))
  (max 8.0 (min 14.0 (+ (/ mx 2.0) 0.5)))
)
(defun GM:GeoMapTextBoxes (mbox / ss i ent ed box lay boxes vpinfo mrect mss ment med mpt mp mh mw kf)
  ;; Solo letras visibles del mapa. CHG3-FIX2: los rotulos DEP/PROV/DIST de una
  ;; plantilla contaminada por una corrida previa SI cuentan como obstaculo:
  ;; evita que el rotulo nuevo se dibuje encima del viejo (doble texto).
  (setq boxes nil ss (ssget "X" (list (cons 0 "TEXT,MTEXT,ATTRIB") (cons 410 (getvar "CTAB")))) i 0)
  (while (and ss (< i (sslength ss)))
    (setq ent (ssname ss i) ed (entget ent) lay (if ed (cdr (assoc 8 ed)) nil)
          box (if ed (GM:LayoutEntityBox ent) nil))
    (if (and box (/= (cdr (assoc 60 ed)) 1)
             (GM:GeoBoxesOverlap box mbox 0.0))
      (setq boxes (cons box boxes)))
    (setq i (1+ i)))
  ;; CHG3-FIX2: textos del mapa dibujado en el MODELO (VP anidado) proyectados
  ;; a papel: el rotulo GEO debe evitar los nombres reales del mapa.
  (setq vpinfo (GM:FindNestedMapVP mbox) mrect (GM:MapVPModelRect vpinfo))
  (if (and mrect (< (length boxes) 120))
    (progn
      (setq kf (GM:MapVPKf vpinfo))
      (setq mss (ssget "X" (list (cons 0 "TEXT,MTEXT") (cons 410 "Model"))) i 0)
      (while (and mss (< i (sslength mss)) (< (length boxes) 120))
        (setq ment (ssname mss i) med (entget ment) mpt (cdr (assoc 10 med)))
        (if (and mpt (numberp (car mpt)) (numberp (cadr mpt))
                 (/= (cdr (assoc 60 med)) 1)
                 (>= (car mpt) (- (car mrect) 60.0)) (<= (car mpt) (+ (nth 2 mrect) 60.0))
                 (>= (cadr mpt) (- (cadr mrect) 60.0)) (<= (cadr mpt) (+ (nth 3 mrect) 60.0)))
          (progn
            (setq mp (GM:MapVPToPaper vpinfo (car mpt) (cadr mpt)))
            (if (and mp (> kf 0.0) (GM:LocInBox mp mbox))
              (progn
                (setq mh (cdr (assoc 40 med)))
                (if (not (numberp mh)) (setq mh 2.0))
                (setq mw (* 0.62 mh kf (strlen (GM:GeoSafeValue (cdr (assoc 1 med)) "X"))))
                (if (< mw 0.2) (setq mw 0.2))
                (setq box (list (- (car mp) mw) (- (cadr mp) (* mh kf 0.7))
                                (+ (car mp) mw) (+ (cadr mp) (* mh kf 0.7))))
                (setq boxes (cons box boxes))))))
        (setq i (1+ i)))))
  boxes)
;; CHG5 2026-10-12: contenido del MAPA_GEO como obstaculo. En mapas combinados
;; (Chagual: ESQUEMA DE LOCALIZACION) la cartografia vive DENTRO del bloque:
;; rotulos TEXT/MTEXT y curvas de ruta/borde en rojo (ACI 1) o magenta (ACI 6
;; o TrueColor). El ssget del layout no los ve (boxes=0) y el rotulo GEO caia
;; al centro encima de ellos. Se recorre la definicion UNA vez por lote
;; (prime), se transforma con el INSERT del rol MAPA_GEO y se recorta al
;; marco. Las lineas finas de contexto (miles de LINE grises) NO bloquean.
;; Cap y dedupe por grilla; fallo = nil (fail-open, como CHG4).
(defun GM:MapRouteColorP (ed / col tc r g b lay lc lcache hit)
  (setq col (cdr (assoc 62 ed)))
  (if (not (numberp col))
    (progn
      (setq lay (if (= (type (cdr (assoc 8 ed))) 'STR) (cdr (assoc 8 ed)) nil))
      (if lay
        (progn
          (setq lcache *GM_MAP_LAYCOL_CACHE* hit (assoc lay lcache))
          (if hit
            (setq lc (cdr hit))
            (progn
              (setq lc (cdr (assoc 62 (tblsearch "LAYER" lay))))
              (if (numberp lc)
                (setq *GM_MAP_LAYCOL_CACHE* (cons (cons lay lc) lcache))))))
        )
      (if (numberp lc) (setq col (abs lc)))))
  (if (or (equal col 1) (equal col 6))
    T
    (progn
      (setq tc (cdr (assoc 420 ed)))
      (if (numberp tc)
        (progn
          (setq r (logand 255 (/ tc 65536)) g (logand 255 (/ tc 256)) b (logand 255 tc))
          (if (or (and (= r 255) (= g 0) (= b 0)) (and (= r 255) (= g 0) (= b 255))) T nil))
        nil))))
(defun GM:MapDefTextBox (ed ent / p h str w obj lo hi got res)
  ;; PRY1-MULTI F3 (GEO9) 2026-10-14: caja REAL del texto dentro del bloque
  ;; (bbox medida y transformada con el INSERT) cuando es medible; la
  ;; estimacion 0.62*h*len queda SOLO como respaldo (fail-open). Asi la caja
  ;; que se evita coincide con la que se dibuja.
  (setq p (cdr (assoc 10 ed))
        h (cdr (assoc 40 ed))
        str (cdr (assoc 1 ed))
        res nil got nil)
  (if (and p (numberp (car p)) (numberp (cadr p)))
    (progn
      (if ent
        (progn
          (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
          (if (vl-catch-all-error-p obj) (setq obj nil))
          (if obj
            (progn
              (setq lo nil hi nil)
              (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'lo 'hi))))
                (progn
                  (setq lo (vlax-safearray->list lo) hi (vlax-safearray->list hi))
                  (if (and lo hi (numberp (car lo)) (numberp (cadr lo))
                           (numberp (car hi)) (numberp (cadr hi)))
                    (setq got T))))
              (GM:ReleaseObjectSafe obj)
              (setq obj nil)))))
      (if got
        (setq res (list (- (car lo) 0.3) (- (cadr lo) 0.3) (+ (car hi) 0.3) (+ (cadr hi) 0.3)))
        (progn
          (if (not (numberp h)) (setq h 2.0))
          (setq w (* 0.62 h (if (= (type str) 'STR) (min 60 (strlen str)) 4)))
          (setq res (list (- (car p) 0.3) (- (cadr p) (* h 0.35))
                          (+ (car p) w 0.3) (+ (cadr p) (* h 1.05))))))))
  res
)
(defun GM:MapCurvePts (ed / typ pts p)
  (setq typ (cdr (assoc 0 ed)) pts nil)
  (cond
    ((= typ "LINE")
     (setq p (cdr (assoc 10 ed))) (if (and p (numberp (car p))) (setq pts (cons p pts)))
     (setq p (cdr (assoc 11 ed))) (if (and p (numberp (car p))) (setq pts (cons p pts))))
    ((= typ "ARC")
     (setq p (cdr (assoc 10 ed))) (if (and p (numberp (car p))) (setq pts (cons p pts))))
    ((= typ "CIRCLE")
     (setq p (cdr (assoc 10 ed))) (if (and p (numberp (car p))) (setq pts (cons p pts))))
    ((or (= typ "LWPOLYLINE") (= typ "POLYLINE"))
     (foreach item ed
       (if (and (= (car item) 10) (listp (cdr item)) (numberp (car (cdr item))))
         (setq pts (cons (cdr item) pts))))))
  (reverse pts)
)
(defun GM:MapXfBox (box ins sx sy rot / x1 y1 x2 y2 c s ax ay bx by)
  (setq x1 (car box) y1 (cadr box) x2 (nth 2 box) y2 (nth 3 box)
        c (cos rot) s (sin rot))
  (setq ax (+ (car ins) (- (* (* x1 sx) c) (* (* y1 sy) s)))
        ay (+ (cadr ins) (+ (* (* x1 sx) s) (* (* y1 sy) c)))
        bx (+ (car ins) (- (* (* x2 sx) c) (* (* y2 sy) s)))
        by (+ (cadr ins) (+ (* (* x2 sx) s) (* (* y2 sy) c))))
  (list (min ax bx) (min ay by) (max ax bx) (max ay by))
)
(defun GM:MapBoxKey (box)
  (strcat (itoa (fix (/ (+ (car box) (nth 2 box)) 5.0)))
          ":" (itoa (fix (/ (+ (cadr box) (nth 3 box)) 5.0))))
)
(defun GM:MapBlockContentBoxes (mbox / ent ed nm bd be bed typ ins sx sy rot
                                boxes n cap seen b p pk pts)
  (setq boxes nil n 0 cap 150 seen nil *GM_MAP_LAYCOL_CACHE* nil)
  (setq ent (vl-catch-all-apply 'GM:FindByRole (list "MAPA_GEO" "INSERT")))
  (if (vl-catch-all-error-p ent) (setq ent nil))
  (if (and mbox ent (= (type ent) 'ENAME))
    (setq ed (entget ent))
    (setq ed nil))
  (if (and ed (= (cdr (assoc 0 ed)) "INSERT"))
    (setq ins (cdr (assoc 10 ed)) nm (cdr (assoc 2 ed))
          sx (cdr (assoc 41 ed)) sy (cdr (assoc 42 ed)) rot (cdr (assoc 50 ed)))
    (setq ins nil))
  (if (and ins (numberp (car ins)) (numberp (cadr ins)) (= (type nm) 'STR))
    (progn
      (if (not (numberp sx)) (setq sx 1.0))
      (if (not (numberp sy)) (setq sy 1.0))
      (if (not (numberp rot)) (setq rot 0.0))
      (if (< (abs sx) 0.000001) (setq sx 1.0))
      (if (< (abs sy) 0.000001) (setq sy 1.0))
      (setq bd (tblobjname "BLOCK" nm))
      (if bd
        (progn
          (setq be (entnext bd))
          (while (and be (< n cap) (/= (cdr (assoc 0 (entget be))) "ENDBLK"))
            (setq bed (entget be) typ (cdr (assoc 0 bed)))
            (cond
              ((member typ '("TEXT" "MTEXT" "ATTDEF"))
               (setq b (GM:MapDefTextBox bed be))
               (if b
                 (progn
                   (setq b (GM:MapXfBox b ins sx sy rot))
                   (if (and (GM:GeoBoxesOverlap b mbox 0.0)
                            (not (member (setq pk (GM:MapBoxKey b)) seen)))
                     (setq seen (cons pk seen) boxes (cons b boxes) n (1+ n))))))
              ((and (member typ '("LWPOLYLINE" "LINE" "ARC" "CIRCLE" "POLYLINE"))
                    (GM:MapRouteColorP bed))
               (setq pts (GM:MapCurvePts bed))
               (foreach p pts
                 (if (and p (numberp (car p)) (numberp (cadr p)) (< n cap))
                   (progn
                     (setq b (list (- (car p) 1.4) (- (cadr p) 1.4) (+ (car p) 1.4) (+ (cadr p) 1.4)))
                     (setq b (GM:MapXfBox b ins sx sy rot))
                     (if (and (GM:GeoBoxesOverlap b mbox 0.0)
                              (not (member (setq pk (GM:MapBoxKey b)) seen)))
                       (setq seen (cons pk seen) boxes (cons b boxes) n (1+ n))))))))
            (setq be (entnext be)))))))
  boxes
)
(defun GM:MapBlockBoxesSafe (mbox / r)
  (setq r (vl-catch-all-apply 'GM:MapBlockContentBoxes (list mbox)))
  (if (vl-catch-all-error-p r) nil r)
)
;; PRY1-GEO5 2026-10-13: el rotulo se pega a la ESQUINA superior derecha del
;; mapa (pedido del usuario); la distancia se mide contra el centro ideal de
;; la celda mas esquinada (x=cx2-hw, y_top=cy2) en vez del cuadrante. El resto
;; (hueco libre, exclusiones, minimo solape) no cambia.
;; ROT1 2026-10-21: la exclusion del circulo grande pasa de 14.0 a 12.5 para
;; seguir al radio nuevo del circulo (9.9 exterior + 2.6 de aire, el mismo
;; margen que antes). La exclusion del circulo pequeno (4.5) no cambia.
(defun GM:GeoFindFreeSpot (cx1 cy1 cx2 cy2 hw hh obstacles zcx zcy zfx zfy / x y_top box ob hits prefx prefy dist best bestdist step)
  (setq prefx (- cx2 hw) prefy (- cy2 (/ hh 2.0))
        step 1.0 best nil bestdist 1e99 x (+ cx1 hw))
  (while (<= x (- cx2 hw))
    (setq y_top (+ cy1 hh))
    (while (<= y_top cy2)
      (setq box (list (- x hw) (- y_top hh) (+ x hw) y_top) hits 0)
      (if (and (not (GM:GeoFijoOverlap (car box) (cadr box) (nth 2 box) (nth 3 box) zcx zcy 4.5))
               (not (GM:GeoFijoOverlap (car box) (cadr box) (nth 2 box) (nth 3 box) zfx zfy 12.5)))
        (foreach ob obstacles
          (if (GM:GeoBoxesOverlap box ob 0.5) (setq hits (1+ hits))))
        (setq hits 1))
      (if (= hits 0)
        (progn
          (setq dist (+ (* (- x prefx) (- x prefx))
                        (* (- (- y_top (/ hh 2.0)) prefy) (- (- y_top (/ hh 2.0)) prefy))))
          (if (< dist bestdist) (setq bestdist dist best (list x y_top)))))
      (setq y_top (+ y_top step)))
    (setq x (+ x step)))
  best)
;; PRY1-GEO2 2026-10-13: colocacion garantizada del rotulo DEP/PROV/DIST.
;; Cuando no existe un hueco 100% libre (mapa denso), esta pasada elige el
;; punto con MENOS solapes: prioriza no pisar los circulos del predio (zoom)
;; y, si no hay opcion limpia, acepta tapar letras del mapa. El tamano del
;; texto nunca se degrada.
(defun GM:GeoFindLeastOverlapSpot (cx1 cy1 cx2 cy2 hw hh obstacles zcx zcy zfx zfy / x y_top box ob ob_hits zoom_ok dist prefx prefy best best_hits bestdist bestz bestz_hits bestzdist step)
  (setq prefx (- cx2 hw) prefy (- cy2 (/ hh 2.0))
        step 1.0 best nil best_hits 32767 bestdist 1e99
        bestz nil bestz_hits 32767 bestzdist 1e99
        x (+ cx1 hw))
  (while (<= x (- cx2 hw))
    (setq y_top (+ cy1 hh))
    (while (<= y_top cy2)
      (setq box (list (- x hw) (- y_top hh) (+ x hw) y_top)
            ob_hits 0
            zoom_ok (and (not (GM:GeoFijoOverlap (car box) (cadr box) (nth 2 box) (nth 3 box) zcx zcy 4.5))
                         (not (GM:GeoFijoOverlap (car box) (cadr box) (nth 2 box) (nth 3 box) zfx zfy 12.5))))
      (foreach ob obstacles
        (if (GM:GeoBoxesOverlap box ob 0.5) (setq ob_hits (1+ ob_hits))))
      (setq dist (+ (* (- x prefx) (- x prefx))
                    (* (- (- y_top (/ hh 2.0)) prefy) (- (- y_top (/ hh 2.0)) prefy))))
      (if (or (< ob_hits best_hits) (and (= ob_hits best_hits) (< dist bestdist)))
        (setq best_hits ob_hits bestdist dist best (list x y_top)))
      (if (and zoom_ok (or (< ob_hits bestz_hits) (and (= ob_hits bestz_hits) (< dist bestzdist))))
        (setq bestz_hits ob_hits bestzdist dist bestz (list x y_top)))
      (setq y_top (+ y_top step)))
    (setq x (+ x step)))
  (if bestz bestz best))
(defun GM:AutoGeoFijoSmallMap (code row / ctab mbox left bottom right top cx1 cx2 cy1 cy2 hw hh
                                 zc zf zcx zcy zfx zfy x y_top curr_y e best
                                 obstacles items item formato dep prov dist dist_parts distp
                                 solapado geo_font geo_font_alt
                                 geo_bold_font geo_bold_alt)
  (setq ctab (getvar "CTAB"))
  (if (not (tblsearch "LAYER" "0. DEP. PROV. DIST."))
    (entmake '((0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord") (2 . "0. DEP. PROV. DIST.") (70 . 0))))
  ;; PRY1-GEO2 2026-10-13: si el TTF del rotulo no esta instalado en Windows
  ;; se crea el estilo con un TTF de respaldo (evita sustituciones silenciosas
  ;; y el texto "feo"); los estilos preexistentes de la plantilla no se tocan.
  ;; ROT1 2026-10-21: el rotulo recupera el estilo de la version anterior
  ;; (UBICACION = Dutch801 XBd BT, serif pesada tipo Times). Si esa fuente no
  ;; esta instalada, el respaldo pasa de arial.ttf a timesbd.ttf (Times New
  ;; Roman Bold): con Arial el DEPARTAMENTO/PROVINCIA se ven sans y mas
  ;; delgado que la referencia. Solo cambia el estilo NUEVO; uno que ya exista
  ;; en la plantilla no se toca (puede usarlo el mapa).
  ;; OJO: el archivo real de Dutch801 XBd BT en Windows es dutcheb.ttf; el
  ;; nombre de familia ("Dutch801 XBd BT.ttf") no es un archivo, asi que se
  ;; buscaba un nombre inexistente y el respaldo salia siempre. Por eso el
  ;; E2E del 21-10 lo vio con timesbd.ttf en una plantilla limpia.
  (setq geo_font "dutcheb.ttf" geo_font_alt nil)
  (if (not (or (findfile geo_font)
               (and (getenv "WINDIR") (findfile (strcat (getenv "WINDIR") "\\Fonts\\" geo_font)))))
    (setq geo_font_alt T))
  (if (not (tblsearch "STYLE" "UBICACION"))
    (progn
      (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                     '(2 . "UBICACION") '(70 . 0) '(40 . 0.0)
                     (cons 3 (if geo_font_alt "timesbd.ttf" geo_font))))
      (if geo_font_alt
        (GM:AutoTrace code "GEO_FUENTE" "AVISO" "dutcheb.ttf (Dutch801 XBd BT) no instalada; rotulo UBICACION con timesbd.ttf"))))
  (if (not (tblsearch "STYLE" "DISTRITO"))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                   '(2 . "DISTRITO") '(70 . 0) '(40 . 0.0)
                   (cons 3 (if (and (getenv "WINDIR") (findfile (strcat (getenv "WINDIR") "\\Fonts\\arialuni.ttf"))) "arialuni.ttf" "arial.ttf")))))
  ;; PRY1-GEO3 2026-10-13: estilo bold del rotulo esperado (arialbd.ttf con
  ;; fallback arial.ttf + traza); los estilos UBICACION/DISTRITO se conservan
  ;; intactos para el resto del dibujo.
  (setq geo_bold_font "arialbd.ttf" geo_bold_alt nil)
  (if (not (or (findfile geo_bold_font)
               (and (getenv "WINDIR") (findfile (strcat (getenv "WINDIR") "\\Fonts\\" geo_bold_font)))))
    (setq geo_bold_alt T))
  (if (not (tblsearch "STYLE" "GEO_BOLD"))
    (progn
      (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord")
                     '(2 . "GEO_BOLD") '(70 . 0) '(40 . 0.0)
                     (cons 3 (if geo_bold_alt "arial.ttf" geo_bold_font))))
      (if geo_bold_alt
        (GM:AutoTrace code "GEO_FUENTE" "AVISO" "arialbd.ttf no instalada; rotulo GEO_BOLD con arial.ttf"))))
  ;; Todo el mapa: marco completo (no ventana fija).
  (setq mbox (GM:MapContentBox))
  (if (not mbox)
    (GM:LayoutFail "Sin marco MAPA_GEO para colocar el texto DEP/PROV/DIST")
    (progn
      (setq left (car mbox) bottom (cadr mbox) right (nth 2 mbox) top (nth 3 mbox))
      ;; Area util de todo el mapa pequeno, con margen interior.
      (setq cx1 (+ left 2.0) cx2 (- right 2.0) cy1 (+ bottom 3.0) cy2 (- top 3.0))
      (if (< (- cx2 cx1) 30.0) (setq cx1 (- (/ (+ left right) 2.0) 15.0) cx2 (+ (/ (+ left right) 2.0) 15.0)))
      ;; Circulos del predio (el marco del zoom sale del helper compartido).
      (setq zc nil zf nil)
      (if (and *GM_AUTO_PREDIO_LOC_RECT* (car *GM_AUTO_PREDIO_LOC_RECT*) (cadr *GM_AUTO_PREDIO_LOC_RECT*))
        (progn
          (setq zc (list (/ (+ (car (car *GM_AUTO_PREDIO_LOC_RECT*)) (car (cadr *GM_AUTO_PREDIO_LOC_RECT*))) 2.0)
                         (/ (+ (cadr (car *GM_AUTO_PREDIO_LOC_RECT*)) (cadr (cadr *GM_AUTO_PREDIO_LOC_RECT*))) 2.0) 0.0))
          ;; PRY1-MULTI F4 2026-10-14: MISMA matematica que dibuja LAY_ESQUEMAS
          ;; (GM:ZoomFrameFor en LAY_COMUN); antes duplicada y con corte 27.2
          ;; vs 28. La exclusion coincide siempre con el circulo grande real.
          (setq zf (if (and (boundp '*GM_AUTO_ZOOM_FRAME*) *GM_AUTO_ZOOM_FRAME*)
                     *GM_AUTO_ZOOM_FRAME*
                     (GM:ZoomFrameFor zc mbox)))))
      (setq zcx (if zc (car zc) 1e12) zcy (if zc (cadr zc) 1e12)
            zfx (if zf (car zf) 1e12) zfy (if zf (cadr zf) 1e12))
      ;; PA-1 (H1) 2026-09-28: DEP/PROV/DIST salen del CSV (nth 7/9/11) con
      ;; respaldo historico si el campo falta; antes iban fijos a Huancasancos
      ;; (SANCHEZ CARRION / ANDRES AVELINO CACERES DORREGARAY) mientras el set
      ;; real trae CANGALLO/VICTOR FAJARDO/HUANCA SANCOS y otros distritos.
      ;; El alto de la caja se deriva de las lineas reales.
      (setq dep (GM:GeoSafeValue (if row (nth 7 row) nil) "AYACUCHO")
            prov (GM:GeoSafeValue (if row (nth 9 row) nil) "SANCHEZ CARRION")
            dist (GM:GeoSafeValue (if row (nth 11 row) nil) "ANDRES AVELINO CACERES DORREGARAY"))
      ;; PRY1-GEO3 2026-10-13: estilo esperado del rotulo (imagen de
      ;; referencia): DEPARTAMENTO rojo con valor mas grande; PROVINCIA negro;
      ;; DISTRITO magenta; todo en negrita (GEO_BOLD) y pegado a la esquina
      ;; superior derecha (PRY1-GEO5).
      ;; ROT1 2026-10-21: DEPARTAMENTO y PROVINCIA vuelven al estilo serif
      ;; UBICACION (el de la version anterior; mas grueso que Arial Bold) y
      ;; la PROVINCIA pasa de negro 7 a gris 250, como en el plano de
      ;; referencia. DISTRITO sigue en GEO_BOLD (sans bold) magenta 6: es lo
      ;; que muestra la referencia. Las alturas no cambian.
      ;; PRY1-GEO4: la col 12 admite dos distritos ("CHUGAY - COCHORCO").
      ;; PA-2 (H3): obstaculos base ya medidos por lote; sin cache se mide aqui.
      ;; CHG5: sin cache tambien se suma el contenido del bloque del mapa.
      (setq obstacles (if *GM_AUTO_GEO_BOXES_READY* *GM_AUTO_GEO_BOXES*
                        (append (GM:GeoMapTextBoxes mbox) (GM:MapBlockBoxesSafe mbox)))
            dist_parts (GM:GeoSplitDistritos dist)
            items (list (list "DEPARTAMENTO" 2.0 10 "UBICACION"))
            formato "AMPLIO" solapado nil)
      (if (not dist_parts) (setq dist_parts (list dist)))
      (foreach item (GM:GeoWrapText dep 16) (setq items (append items (list (list item 3.0 10 "UBICACION")))))
      (setq items (append items (list (list "PROVINCIA" 1.5 250 "UBICACION"))))
      (foreach item (GM:GeoWrapText prov 16) (setq items (append items (list (list item 1.8 250 "UBICACION")))))
      (setq items (append items (list (list "DISTRITO" 1.5 6 "GEO_BOLD"))))
      (foreach distp dist_parts
        (foreach item (GM:GeoWrapText distp 16) (setq items (append items (list (list item 1.5 6 "GEO_BOLD"))))))
      (setq hh (GM:GeoItemsHeight items)
            ;; PRY1-MULTI F3 (GEO9): medio ancho real del rotulo medido por
            ;; linea (GM:TextMeasure con el estilo bold); clamp [8, 14] para no
            ;; romper el diseno historico. Antes era 14.0 fijo.
            hw (GM:GeoLabelHalfWidth items)
            best (GM:GeoFindFreeSpot cx1 cy1 cx2 cy2 hw hh obstacles zcx zcy zfx zfy))
      ;; Sin hueco libre: minimo solape (prioriza no pisar los circulos); si
      ;; ni existe grilla valida (mapa menor que la caja), se centra.
      (if (not best)
        (setq best (GM:GeoFindLeastOverlapSpot cx1 cy1 cx2 cy2 hw hh obstacles zcx zcy zfx zfy)
              solapado T formato "AMPLIO-SOLAPADO"))
      (if (not best)
        (setq best (list (/ (+ cx1 cx2) 2.0) (+ (/ (+ cy1 cy2) 2.0) (/ hh 2.0)))
              solapado T formato "AMPLIO-SOLAPADO"))
      ;; La primera linea tambien queda dentro de la caja validada; TEXT
      ;; centrado sobresale media altura por encima de su punto de insercion.
      (setq x (car best) y_top (cadr best) curr_y (- y_top 1.2))
      ;; PRY1-GEO5 2026-10-13: sin fondo WIPEOUT (pedido del usuario: "sin
      ;; fondo blanco"): el rotulo va directo sobre el mapa y los TEXT quedan
      ;; al frente con DRAWORDER _F. En modo solapado puede cruzar letras del
      ;; mapa, aceptado a cambio de no tapar el dibujo con una caja blanca.
      (foreach item items
        (setq e (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                               (cons 8 "0. DEP. PROV. DIST.") (cons 410 ctab)
                               (cons 62 (nth 2 item)) (cons 10 (list x curr_y 0.0))
                               (cons 40 (nth 1 item)) (cons 1 (nth 0 item))
                               (cons 7 (nth 3 item)) '(72 . 1) (cons 11 (list x curr_y 0.0)) '(73 . 2))))
        (if e (progn (setq e (entlast)) (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" e "" "_F"))))
        (setq curr_y (- curr_y (GM:GeoLinePitch (nth 1 item)))))
      (GM:AutoTrace code "GEO_FIJO" "OK"
        (strcat "Texto " formato " en (" (rtos x 2 1) "," (rtos y_top 2 1) "); obstaculos=" (itoa (length obstacles))
                "; distritos=" (itoa (length dist_parts))))
      T)))