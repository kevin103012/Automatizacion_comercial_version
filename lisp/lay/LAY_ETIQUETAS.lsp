;; ============================================================================
;; LAY_ETIQUETAS.lsp - Sistema GM Ingenieros (modulo lay)
;; Movido SIN CAMBIOS desde LAYOUT_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


(defun GM:PropSpaceLandingGrid (ent w up dn roads reds step mode allowRed / *GM_LABEL_EXCLUDE* *GM_LABEL_SAMPLES* *GM_LABEL_DIST2*
    rect matbox x y air cells c p1 result rejected valid center_y south left score d2
    bestlen attempts redHit redMargin rb gapNow evaln redList)
  (setq *GM_LABEL_EXCLUDE* nil *GM_LABEL_SAMPLES* nil *GM_LABEL_DIST2* nil
        rect (GM:MainViewportRect) matbox (GM:PaperBBoxOfEnt ent 0.0)
        cells nil rejected 0 valid 0 result nil evaln 0)
  ;; F15-R3: forense al entrar a cada grid (sobrevive un AV dentro del barrido).
  (if (boundp 'GM:AutoForensic)
    (GM:AutoForensic *ML_TARGET_CODE* "PROP_GRID"
      (strcat "modo=" mode " paso=" (rtos step 2 1) " rojo=" (if allowRed "PISA" "EVITA"))))
  ;; El borde real evita castigar entrantes de matrices concavas por su bbox.
  (setq *GM_LABEL_SAMPLES* (GM:PaperBoundarySamples ent 16)
        *GM_LABEL_DIST2* 225.0
        gapNow (GM:NumVar *GM_PROP_GAP* 15.0)
        center_y (if matbox (/ (+ (cadr matbox) (nth 3 matbox)) 2.0) 0.0))
  (if (and rect matbox (> step 0.0) (> w 0.0))
    (progn
      (setq x (+ (car rect) 5.0))
      (while (<= (+ x w) (- (nth 2 rect) 5.0))
        (setq y (+ (cadr rect) dn 5.0))
        (while (<= (+ y up) (- (nth 3 rect) 5.0))
          ;; F15-R3: latido cada 250 celdas; la ultima linea forense dice donde
          ;; murio el barrido (el reporte solo registra el fin del grid).
          (setq evaln (1+ evaln))
          (if (and (boundp 'GM:AutoForensic) (= (rem evaln 250) 0))
            (GM:AutoForensic *ML_TARGET_CODE* "PROP_CELDA"
              (strcat "evaluadas=" (itoa evaln) " validas=" (itoa valid)
                      " x=" (rtos x 2 1) " y=" (rtos y 2 1))))
          (setq air (GM:PropSpaceAir x y w up dn matbox roads)
                air (min air (- x (car rect) 5.0) (- (- y dn) (cadr rect) 5.0)
                             (- (nth 2 rect) (+ x w) 5.0) (- (nth 3 rect) (+ y up) 5.0)))
          (setq south (GM:BoxIsSouth (- y dn) (+ y up) rect)
                left (<= (+ x (/ w 2.0)) (car matbox))
                redMargin (GM:NumVar *GM_PROP_RED_M* 3.0)
                redHit 0)
          ;; AUD1: con rojo=EVITA (allowRed=nil) basta el primer choque: la celda
          ;; ya quedo descalificada y el score solo cuenta rojos cuando allowRed=T.
          ;; Corta hasta ~125 GapToBox por celda en el caso 017 (misma salida).
          (setq redList reds)
          (while redList
            (setq rb (car redList) redList (cdr redList))
            (if (< (GM:GapToBox x (- y dn) (+ x w) (+ y up) rb) redMargin)
              (setq redHit (1+ redHit)))
            (if (and *GM_PROP_EARLY_RED* (not allowRed) (> redHit 0))
              (setq redList nil)))
          ;; BW: IZQUIERDA exige izquierda Y no-sur; el sur queda reservado a
          ;; SUR-RECURSO para no aceptar de entrada una esquina inferior lejana
          ;; habiendo espacio superior libre (TOTAL ya cubre izquierda no-sur).
          (if (and (>= air 0.0)
                   (or allowRed (= redHit 0))
                   (GM:SamplesClear x (- y dn) (+ x w) (+ y up))
                   (or (and (= mode "IZQUIERDA") left (not south))
                       (and (= mode "TOTAL") (not south))
                       (and (= mode "SUR-RECURSO") south)))
            (progn
              (setq valid (1+ valid))
              (setq d2 (GM:BoxDist2 x (- y dn) (+ x w) (+ y up)
                         (car matbox) (cadr matbox) (nth 2 matbox) (nth 3 matbox)))
              (setq score (* (min air (GM:NumVar *GM_PROP_AIR* 15.0)) 2.0))
              (setq score (+ score (* -0.05 (abs (- (+ y (/ (- up dn) 2.0)) center_y)))))
              (setq score (+ score (* -0.00001 d2)))
              (if (> redHit 0) (setq score (- score (* 5.0 redHit))))
              (setq cells (cons (list x y air d2 left score) cells)))
            (setq rejected (1+ rejected)))
          (setq y (+ y step)))
        (setq x (+ x step)))
      (GM:AutoTrace *ML_TARGET_CODE* "PROP_GRID_FIN" "INFO"
        (strcat "AU modo=" mode " gap=" (rtos gapNow 2 1) " paso=" (rtos step 2 1)
                " rojo=" (if allowRed "PISA" "EVITA") " rojos=" (itoa (length reds))
                " evaluadas=" (itoa (+ valid rejected)) " validas=" (itoa valid)))
      ;; CHUNK 2026-09-29: grid sin celdas validas alimenta el fail-fast de
      ;; GM:PropSpaceLanding (evita agotar 24 grids hasta el AV).
      (if (= valid 0) (setq *GM_PROP_GRIDS_0* (1+ *GM_PROP_GRIDS_0*)))
      (setq cells (if cells (vl-sort cells 'GM:PropCellBetter) nil))
      (setq bestlen nil attempts 0)
      ;; OPT10 (F5): mas intentos si los esquemas descartan rutas; tope 40 para
      ;; acotar el costo (antes 12, insuficiente con ESQUEMA_AF/LOC bloqueando).
      (while (and cells (< attempts 40) (not result))
        (setq c (car cells) cells (cdr cells) attempts (1+ attempts)
              p1 (GM:PropSpaceAnchor ent c w up dn))
        (if (and p1 (GM:LeaderRouteClear (GM:LeaderRouteFree p1 (list (car c) (cadr c) 0.0) w)))
          (setq bestlen (GM:LeaderLen p1 (car c) (cadr c))
                result (list p1 (list (car c) (cadr c) 0.0)))))
      (GM:AutoTrace *ML_TARGET_CODE* "PROP_CANDIDATOS" "INFO"
        (strcat "AT validos=" (itoa valid) " rechazados=" (itoa rejected)
          (if result (strcat " region=" mode
            " FINAL=" (vl-princ-to-string (cadr result)) " aireExtra=" (rtos (nth 2 c) 2 2)
            " intentos=" (itoa attempts) " largo2=" (rtos bestlen 2 0)
            " gap=" (rtos gapNow 2 1)
            (if allowRed " ROJO-RECURSO" ""))
            " SIN_POSICION; restricciones conservadas")))))
  (if result (setq *GM_PROP_USED_GAP* gapNow))
  result)

;; Ancho del rotulo de propietario por textbox (misma formula del dibujo).
(defun GM:PropNameLines (nombre / lines pos)
  (setq nombre (if (and nombre (/= nombre "")) nombre "SIN NOMBRE") lines nil)
  (while (setq pos (vl-string-search "\\P" nombre))
    (setq lines (cons (substr nombre 1 pos) lines)
          nombre (substr nombre (+ pos 3))))
  (reverse (cons nombre lines)))

(defun GM:PropBoxWidth (nombre condicion / st_name st_cond box_w span)
  (setq st_name (if (tblsearch "STYLE" "ARIAL BLACK") "ARIAL BLACK" "ROMANS")
        st_cond (if (tblsearch "STYLE" "ROMANS") "ROMANS" "STANDARD")
        box_w 40.0)
  ;; P0-6 2026-09-29: medicion memoizada (GM:TextMeasure) en vez de textbox directo.
  (foreach line (GM:PropNameLines nombre)
    (setq span (GM:TextMeasure line 1.8 st_name))
    (if span (setq box_w (max box_w (+ span 4.0)))))
  (setq span (GM:TextMeasure (if condicion condicion "") 1.5 st_cond))
  (if span (setq box_w (max box_w (+ span 4.0))))
  box_w)

;; Primero etiqueta bajo la matriz; despues ancla real mas cercana.
;; La bbox completa es conservadora: el texto queda fuera del predio con 15
;; unidades de separacion incluso entre vertices no muestreados.
(defun GM:PropLowerLanding (ent w up dn gap / rect box x y p2 endpt modelpt anchor pp best score bestscore row col vla_o)
  (setq rect (GM:MainViewportRect) box (GM:PaperBBoxOfEnt ent 0.0)
        best nil bestscore nil
        ;; FIX-AV: un solo wrapper para toda la rejilla 5x5 (se libera al final).
        vla_o (if ent (vl-catch-all-apply 'vlax-ename->vla-object (list ent)) nil))
  (if (vl-catch-all-error-p vla_o) (setq vla_o nil))
  (if (and rect box)
    (foreach row '(0.0 10.0 20.0 35.0 50.0)
      (foreach col '(0.0 10.0 20.0 35.0 50.0)
        (setq x (max (+ (car rect) 3.0) (- (car box) (/ w 2.0) col))
              y (- (cadr box) gap up row)
              p2 (list x y 0.0)
              endpt (list (+ x w) y 0.0))
        (if (and (>= (- y dn) (+ (cadr rect) 3.0))
                 (<= (+ x w) (- (nth 2 rect) 3.0))
                 (>= (GM:BoxClearance x (- y dn) (+ x w) (+ y up)) 0.0))
          (progn
            (setq modelpt (GM:PaperToModel endpt)
                  anchor (if (and modelpt vla_o) (vl-catch-all-apply 'vlax-curve-getClosestPointTo
                           (list vla_o modelpt)) nil))
            (if (and anchor (not (vl-catch-all-error-p anchor)))
              (progn
                (setq pp (GM:ModelToPaper anchor))
                (if (and pp (GM:IsPaperPointInMainViewport pp)
                         (GM:LeaderRouteClear (GM:LeaderRouteFree pp p2 w)))
                  (progn
                    (setq score (GM:LeaderLen pp (car endpt) (cadr endpt)))
                    (if (or (not bestscore) (< score bestscore))
                      (setq best (list pp p2) bestscore score)))))))))))
  ;; FIX-AV: soltar el wrapper de la rejilla antes de retornar.
  (GM:ReleaseObjectSafe vla_o)
  best)
;; Aterrizaje lateral para propietario: ancla y texto del mismo lado del predio
;; (el propietario puede se\\U+00F1alar cualquier parte, a diferencia del AF).
;; Criterio unico: entre todos los candidatos de ambos lados con holgura
;; suficiente gana el leader mas corto (empate exacto a la izquierda); si
;; ninguno cumple, el de mayor holgura. Cada candidato nace hacia afuera del
;; lado de su ancla, asi que el lado comun esta garantizado por construccion.
;; Requiere *GM_LABEL_SAMPLES* con el contorno en Paper para medir huecos y
;; evitar el borde. Retorna (ancla aterrizaje) o nil.
(defun GM:PropSideLanding (samples w up dn gap / rect vis s n sx sy cx off step side px py cl L min_gap best bestside bestlen bestcl dy)
  (setq rect (GM:MainViewportRect))
  (if (not (and rect samples (numberp w) (> w 0.0) (numberp up) (numberp dn) (numberp gap)))
    nil
    (progn
      ;; Muestras visibles + centroide (define el lado de cada muestra).
      (setq vis nil sx 0.0 sy 0.0 n 0)
      (foreach s samples
        (if (and (numberp (car s)) (numberp (cadr s)) (GM:IsPaperPointInMainViewport s))
          (setq vis (cons s vis) sx (+ sx (car s)) sy (+ sy (cadr s)) n (1+ n))))
      (if (< n 1)
        nil
        (progn
          (setq cx (/ sx n)
                off (+ w gap) step (+ up dn 4.0)
                min_gap (if (numberp *GM_LANDING_MIN_GAP*) *GM_LANDING_MIN_GAP* 3.0)
                best nil bestside "" bestlen nil bestcl nil)
          ;; Pasada 1: leader mas corto entre los holgados; empate a la izquierda.
          (foreach s vis
            (setq side (if (<= (car s) cx) "I" "D"))
            (foreach dy (list 0.0 (- step) step)
              (setq px (if (= side "I") (- (car s) off) (+ (car s) off))
                    py (+ (cadr s) dy)
                    cl (GM:CandidateClearance s px py w up dn rect))
              (if cl
                (progn
                  (setq L (GM:LeaderLen s px py))
                  (if (and (>= cl min_gap)
                           (or (not bestlen) (< L bestlen)
                               (and (= L bestlen) (= side "I") (= bestside "D"))))
                    (setq best (list s (list px py 0.0)) bestside side bestlen L))))))
          ;; Pasada 2 (fallback): mayor holgura.
          (if (not best)
            (foreach s vis
              (setq side (if (<= (car s) cx) "I" "D"))
              (foreach dy (list 0.0 (- step) step)
                (setq px (if (= side "I") (- (car s) off) (+ (car s) off))
                      py (+ (cadr s) dy)
                      cl (GM:CandidateClearance s px py w up dn rect))
                (if (and cl (or (not bestcl) (> cl bestcl)))
                  (setq best (list s (list px py 0.0)) bestside side bestcl cl)))))
          best)))))


;; Punto del borde de la caja mas cercano al ancla (izq/sup/inf/der al medio).
(defun GM:LeaderAttach (p_box_bl p_box_tr p1 / bx1 by1 bx2 by2 cx cy cands c best bestd d)
  (setq bx1 (car p_box_bl) by1 (cadr p_box_bl) bx2 (car p_box_tr) cy (/ (+ by1 (cadr p_box_tr)) 2.0) cx (/ (+ bx1 bx2) 2.0)
        cands (list (list bx1 cy 0.0) (list cx (cadr p_box_tr) 0.0) (list cx by1 0.0) (list bx2 cy 0.0))
        best (car cands) bestd nil)
  (foreach c cands
    (setq d (distance p1 c))
    (if (or (not bestd) (< d bestd)) (setq best c bestd d)))
  best)
;; Codo horizontal hacia el amarre, del lado del ancla (tramo final horizontal).
(defun GM:LeaderElbow (p1 attach / gap)
  (setq gap 8.0)
  (if (< (car p1) (car attach))
    (list (- (car attach) gap) (cadr attach) 0.0)
    (list (+ (car attach) gap) (cadr attach) 0.0)))
;; Dibuja leader con caja de texto en Paper (para anotaciones autom\\U+00E1ticas)
;; p1: punta flecha Paper; p2: quiebre Paper; w: ancho caja; h_up/h_dn: alturas; msg: lista l\\U+00EDneas; style/height: estilo texto
;; E3: todo entmake lleva (410 . layout) para forzar PaperSpace aunque CTAB cambie (incluido el MTEXT).
;; Mejora de senalizacion: msg admite N lineas (unidas con \P), la caja crece si no caben,
;; el texto se centra en la caja y la caja queda registrada para no encimar etiquetas.
;; Retorna T si ok, nil si entradas invalidas.
;; LEGACY (CHG3 2026-10-10): el rotulo vial del lote usa GM:DrawRoadLabel (sin
;; marco, texto negro con fondo tenue); esta caja con marco se conserva como
;; utilidad del kit manual.
(defun GM:DrawLeaderBox (p1 p2 w h_up h_dn msg style height / p_box_bl p_box_tr lay nlines need have f text line cx cy e_solid e_frame e_text e_leader ss_front e_data attach_pt elbow_pt)
  (if (not (and p1 p2 (numberp (car p1)) (numberp (car p2)) (numberp w) (> w 0.0) msg (car msg)))
    (progn (princ "\n[GM:DrawLeaderBox] Puntos o mensaje invalidos") nil)
    (progn
  ;; La caja crece proporcionalmente si las lineas no caben en h_up+h_dn.
  (setq nlines (length msg) need (+ (* nlines height 1.35) 2.0) have (+ h_up h_dn))
  (if (> need have)
    (progn (setq f (/ need have) h_up (* h_up f) h_dn (* h_dn f))))
  (setq lay (getvar "CTAB")
        p_box_bl (list (car p2) (- (cadr p2) h_dn) 0.0)
        p_box_tr (list (+ (car p2) w) (+ (cadr p2) h_up) 0.0)
        attach_pt (GM:LeaderAttach p_box_bl p_box_tr p1)
        elbow_pt (GM:LeaderElbow p1 attach_pt))
  ;; OPT10 (F5): en modo lote la ruta real (con el crecimiento de caja ya
  ;; aplicado) no puede cruzar roles criticos.
  (if (and *GM_AUTO_MODE* (not (GM:LeaderRouteClear (list p1 elbow_pt attach_pt))))
    (progn (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETA_VIA" "ERROR" "Leader cruzaria ESQUEMA/membrete; etiqueta no dibujada") nil)
    (progn
  ;; Fondo opaco
  (entmake (append (list '(0 . "SOLID") '(100 . "AcDbEntity") '(8 . "Membrete") '(420 . 16777215)
                 '(100 . "AcDbTrace")
                 (cons 10 (list (car p_box_bl) (cadr p_box_bl) 0.0))
                 (cons 11 (list (car p_box_tr) (cadr p_box_bl) 0.0))
                 (cons 12 (list (car p_box_bl) (cadr p_box_tr) 0.0))
                 (cons 13 (list (car p_box_tr) (cadr p_box_tr) 0.0)))
              (list (cons 410 lay))))
  (setq e_solid (entlast))
  ;; Marco
  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                 '(90 . 4) '(70 . 1) (cons 8 "Membrete") (cons 62 0)
                 (cons 10 p_box_bl) (cons 10 (list (car p_box_tr) (cadr p_box_bl) 0.0))
                 (cons 10 p_box_tr) (cons 10 (list (car p_box_bl) (cadr p_box_tr) 0.0)))
              (list (cons 410 lay))))
  (setq e_frame (entlast))
  ;; MTEXT por entmake con el patron de los titulos de esquema (insercion explicita,
  ;; justificacion media 71/5): la geometria queda determinada por la caja calculada.
  (setq text (car msg))
  (foreach line (cdr msg) (setq text (strcat text "\\P" line)))
  (setq cx (+ (car p_box_bl) (/ w 2.0)) cy (/ (+ (cadr p_box_bl) (cadr p_box_tr)) 2.0))
  (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                 (cons 8 "Membrete") (cons 410 lay)
                 (cons 10 (list cx cy 0.0)) (cons 40 height) (cons 41 w)
                 (cons 71 5) (cons 7 style) (cons 1 text)))
  ;; El texto entra al grupo frontal; si entlast no lo devuelve, se omite
  ;; sin romper el orden del resto.
  (setq e_text (entlast)
        e_data (if e_text (entget e_text) nil))
  (if (not (and e_data (= (cdr (assoc 0 e_data)) "MTEXT"))) (setq e_text nil))
  ;; Leader (linea + flecha simple; quiebre/amarre ya calculados arriba)
  (entmake (append (list '(0 . "LEADER") '(100 . "AcDbEntity") '(8 . "Membrete") '(100 . "AcDbLeader")
                 (cons 10 p1) (cons 10 elbow_pt) (cons 10 attach_pt)
                 '(70 . 0) '(40 . 4.0) '(370 . 50) '(71 . 1) '(72 . 0) '(73 . 1) '(74 . 1))
              (list (cons 410 lay))))
  ;; P0-5 2026-09-29: sin entupd tras entmake (entmake ya actualiza; el entupd
  ;; extra sumaba un registro UNDO/regen por etiqueta).
  (setq e_leader (entlast))
  ;; Orden frontal del cajetin sobre esquemas y viewport (apilado garantizado;
  ;; fallo silencioso para no invalidar una etiqueta ya dibujada).
  ;; P0-4 2026-09-29: vl-cmdf (capturable) en vez de command dentro de catch-all.
  (vl-catch-all-apply
    '(lambda () (setq ss_front (ssadd)) (ssadd e_solid ss_front) (ssadd e_frame ss_front)
                (if e_text (ssadd e_text ss_front)) (ssadd e_leader ss_front)
                (vl-cmdf "_.DRAWORDER" ss_front "" "_F"))
    nil)
  ;; Registrar la caja para que las siguientes etiquetas no la encimen.
  (setq *GM_PLACED_BOXES* (cons (list (car p_box_bl) (cadr p_box_bl) (car p_box_tr) (cadr p_box_tr)) *GM_PLACED_BOXES*))
  T
    )
    )
    )
  )
)

;; CHG3-FIX 2026-10-10: estilo y medida unicos del rotulo vial. Fuente negra
;; ancha como los titulos de AF/propietario (ARIAL BLACK; ROMANS con factor si
;; esa fuente no existe) para que el rotulo se note en el plano.
(defun GM:RoadLabelStyle (/ st wf)
  (if (tblsearch "STYLE" "ARIAL BLACK")
    (setq st "ARIAL BLACK" wf 1.0)
    (setq st "ROMANS" wf 1.25))
  (list st wf 2.0)
)

;; CHG3-FIX: ancho de reserva real (span x factor + margen). Lote y dibujo usan
;; el MISMO valor para que la huella anti-encime coincida con lo dibujado.
;; Fallback 16 si la medicion no esta disponible.
(defun GM:RoadLabelWidth (txt / spec st wf span)
  (setq spec (GM:RoadLabelStyle)
        st (car spec) wf (cadr spec)
        span (GM:TextMeasure txt (nth 2 spec) st))
  (if (and (numberp span) (> span 0.0)) (+ (* span wf) 3.0) 16.0)
)

;; CHG3 2026-10-10 / CHG3-FIX: rotulo vial con el lenguaje grafico de
;; propietario/AF: texto con la fuente negra ancha, ARRIBA de la linea, sobre
;; fondo blanco tenue y SIN marco. El leader llega al BORDE inferior cercano
;; (nunca atraviesa las letras). w es el ancho ya medido por GM:RoadLabelWidth.
(defun GM:DrawRoadLabel (p1 p2 txt w / lay spec st wf h mid y0 elb far e_bg e_lead e_text ss_front nearx)
  (if (not (and p1 p2 txt (= (type txt) 'STR) (numberp w) (> w 0.0)))
    (progn (princ "\n[GM:DrawRoadLabel] Puntos o texto invalidos") nil)
    (progn
      (setq spec (GM:RoadLabelStyle) st (car spec) wf (cadr spec) h (nth 2 spec)
            lay (getvar "CTAB")
            y0 (cadr p2)
            mid (+ (car p2) (/ w 2.0))
            nearx (if (< (car p1) mid) (car p2) (+ (car p2) w)))
      ;; Codo fuera de la caja y amarre en el borde inferior cercano: el tramo
      ;; horizontal muere en el borde; las letras suben por encima.
      (setq elb (list (if (< (car p1) mid) (- nearx 3.0) (+ nearx 3.0)) y0 0.0)
            far (list nearx y0 0.0))
      ;; OPT10 (F5): en modo lote la ruta real no puede cruzar roles criticos.
      (if (and *GM_AUTO_MODE* (not (GM:LeaderRouteClear (list p1 elb far))))
        (progn (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETA_VIA" "ERROR" "Leader cruzaria ESQUEMA/membrete; etiqueta no dibujada") nil)
        (progn
          ;; Fondo blanco tenue (mismo 30% que las etiquetas de AF/propietario).
          (entmake (append (list '(0 . "SOLID") '(100 . "AcDbEntity") '(8 . "Membrete")
                               '(420 . 16777215) (cons 440 33554508) '(100 . "AcDbTrace")
                               (cons 10 (list (- (car p2) 0.5) (+ y0 0.15) 0.0))
                               (cons 11 (list (+ (car p2) w 0.5) (+ y0 0.15) 0.0))
                               (cons 12 (list (- (car p2) 0.5) (+ y0 2.75) 0.0))
                               (cons 13 (list (+ (car p2) w 0.5) (+ y0 2.75) 0.0)))
                        (list (cons 410 lay))))
          (setq e_bg (entlast))
          (entmake (append (list '(0 . "LEADER") '(100 . "AcDbEntity") '(8 . "Membrete") '(100 . "AcDbLeader")
                           (cons 10 p1) (cons 10 elb) (cons 10 far)
                           '(70 . 0) '(40 . 4.0) '(370 . 50) '(71 . 1) '(72 . 0) '(73 . 3) '(74 . 1) '(75 . 0))
                        (list (cons 410 lay))))
          (setq e_lead (entlast))
          (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                         (cons 8 "Membrete") (cons 410 lay)
                         (cons 10 (list mid (+ y0 1.45) 0.0)) (cons 11 (list mid (+ y0 1.45) 0.0))
                         (cons 40 h) (cons 41 wf) (cons 1 txt) (cons 7 st) '(62 . 7) '(72 . 1) '(73 . 2)))
          (setq e_text (entlast))
          ;; Al frente, como el resto de etiquetas (fallo silencioso: la
          ;; etiqueta ya esta dibujada y el orden por creacion ya apila bien).
          (setq ss_front (vl-catch-all-apply 'ssadd (list)))
          (if (not (vl-catch-all-error-p ss_front))
            (progn
              (vl-catch-all-apply 'ssadd (list e_bg ss_front))
              (if e_text (vl-catch-all-apply 'ssadd (list e_text ss_front)))
              (vl-catch-all-apply 'ssadd (list e_lead ss_front))
              (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" ss_front "" "_F"))))
          ;; Huella real (texto + fondo) para que la siguiente etiqueta no la encime.
          (setq *GM_PLACED_BOXES* (cons (list (- (car p2) 0.5) (+ y0 0.15) (+ (car p2) w 0.5) (+ y0 2.75)) *GM_PLACED_BOXES*))
          T
        )
      )
    )
  )
)

;; E8: formato canonico *GL_DATA_AFECTACIONES* = (area_m2 area_ha perimetro sufijo pf_ent).
;; Los registros antiguos de 3 elementos (area_m2 area_ha perimetro) siguen leyendose: sufijo="" y pf=nil.
;; Los 3 primeros campos nunca cambian de posicion para no romper consumidores manuales.
(defun GM:AF_AreaM2 (rec) (nth 0 rec))
(defun GM:AF_AreaHa (rec) (nth 1 rec))
(defun GM:AF_Perim (rec) (nth 2 rec))
(defun GM:AF_Suffix (rec) (if (>= (length rec) 4) (nth 3 rec) ""))
(defun GM:AF_PfEnt (rec) (if (>= (length rec) 5) (nth 4 rec) nil))

;; Lineas de se\\U+00F1alizacion formato imagen: sin tildes, Ha/m2/m sin punto ni coma.
;; titulo, codigo, hectareas (4 decimales), m2 y perimetro. Funcion pura (testeable).
(defun GM:AF_LabelLines (code sufijo area_m2 area_ha perimetro / old_dimzin a2 h4 p2)
  (setq old_dimzin (getvar "DIMZIN")) (setvar "DIMZIN" 0)
  (setq a2 (rtos (if (numberp area_m2) area_m2 0.0) 2 2)
        h4 (rtos (if (numberp area_ha) area_ha 0.0) 2 4)
        p2 (rtos (if (numberp perimetro) perimetro 0.0) 2 2))
  (setvar "DIMZIN" old_dimzin)
  (list (strcat "AREA AFECTADA " sufijo) (if code code "")
        (strcat "AREA = " h4 " Ha") (strcat "AREA = " a2 " m2") (strcat "PERIMETRO = " p2 " m")))

;; Se\\U+00F1alizacion suelta formato imagen/antiguo: LEADER + 5x TEXT sin caja.
;; Ancho dinamico max+6 min 35, 2 tama\\U+00F1os (3.0 titulo/codigo, 1.8 medidas).
;; El texto se extiende siempre hacia +x desde p2, igual que la huella que
;; valida CandidateClearance, para no salirse del viewport en candidatos I.
;; Registra huella invisible en *GM_PLACED_BOXES* para anti-encimado.
;; LEGACY (conservado): el lote llama directo a GM:DrawAfectacionWithWidth con el
;; ancho ya reservado; se mantiene por compatibilidad manual.
(defun GM:DrawAfectacionSuelto (p1 p2 lines / measured)
  (setq measured (GM:ResolveAfectacionWidth lines))
  (GM:DrawAfectacionWithWidth p1 p2 lines (car measured)))

;; AO: fondo blanco opacidad 30% (como la version manual) para que el texto no se
;; cruce con linderos del mapa; va al frente con leader+textos.
(defun GM:DrawAfectacionWithWidth (p1 p2 lines box_w / lay p3 mid_x st_title st_body y0 i h pt elb far e_bg e_leader e_texts e_tmp ss_front)
  (if (not (and p1 p2 (numberp (car p1)) (numberp (car p2)) lines (= (length lines) 5)
                (numberp box_w) (> box_w 0.0)))
    (progn (princ "\n[GM:DrawAfectacionSuelto] Puntos o lineas invalidos") nil)
    (progn
      ;; OPT10 (F5): en modo lote la ruta real no puede cruzar roles criticos
      ;; (ESQUEMA_AF/LOC incluidos); fail-closed para no dibujar cruzadas.
      (if (and *GM_AUTO_MODE* (not (GM:LeaderRouteClear (GM:LeaderRouteFree p1 p2 box_w))))
        (progn (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETA_AFECTACION" "ERROR" "Leader cruzaria ESQUEMA/membrete; etiqueta no dibujada") nil)
        (progn
      (setq lay (getvar "CTAB")
            st_title (if (tblsearch "STYLE" "ARIAL BLACK") "ARIAL BLACK" "ROMANS")
            st_body (if (tblsearch "STYLE" "VERTICES") "VERTICES" "ROMANS"))
      ;; AK: usar exactamente el ancho validado (tambien si fue fallback 45).
      (setq p3 (list (+ (car p2) box_w) (cadr p2) 0.0)
            mid_x (+ (car p2) (/ box_w 2.0)) y0 (cadr p2))
      ;; Fondo blanco opacidad 30% con la huella validada ([x2 y0-8.3 x2+box y0+6.7]).
      (entmake (append (list '(0 . "SOLID") '(100 . "AcDbEntity") '(8 . "Membrete") '(420 . 16777215)
                       (cons 440 33554508) '(100 . "AcDbTrace")
                       (cons 10 (list (car p2) (- y0 8.3) 0.0))
                       (cons 11 (list (+ (car p2) box_w) (- y0 8.3) 0.0))
                       (cons 12 (list (car p2) (+ y0 6.7) 0.0))
                       (cons 13 (list (+ (car p2) box_w) (+ y0 6.7) 0.0)))
                    (list (cons 410 lay))))
      (setq e_bg (entlast))
      ;; Codo en el extremo mas cercano al ancla: la diagonal no cruza el texto.
      (if (<= (abs (- (car p1) (car p3))) (abs (- (car p1) (car p2))))
        (setq elb p3 far p2)
        (setq elb p2 far p3))
      ;; LEADER suelto de 3 puntos, sin anotacion asociada (73=3) ni hook (75=0).
      (entmake (append (list '(0 . "LEADER") '(100 . "AcDbEntity") '(8 . "Membrete") '(100 . "AcDbLeader")
                       (cons 10 p1) (cons 10 elb) (cons 10 far)
                       '(70 . 0) '(40 . 4.0) '(370 . 50) '(71 . 1) '(72 . 0) '(73 . 3) '(74 . 1) '(75 . 0))
                    (list (cons 410 lay))))
      ;; P0-5 2026-09-29: sin entupd tras entmake (ya actualiza).
      (setq e_leader (entlast) e_texts nil)
      ;; 5 TEXT sueltos centrados, tamano manual 2.5/1.5.
      (setq i 0)
      (while (< i 5)
        (setq h (if (< i 2) 2.5 1.5)
              pt (list mid_x (+ y0 (nth i '(5.4 2.1 -2.1 -4.4 -6.7))) 0.0))
        (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                       (cons 8 "Membrete") (cons 410 lay) (cons 10 pt) (cons 11 pt)
                       (cons 40 h) (cons 1 (nth i lines))
                       (cons 7 (if (< i 2) st_title st_body)) '(72 . 1) '(73 . 2)))
        (setq e_texts (cons (entlast) e_texts)
              i (1+ i))
      )
      ;; Conjunto frontal: fondo + textos + leader sobre mapa y viewport.
      ;; Cada paso con su propio catch-all para no invalidar una etiqueta ya
      ;; dibujada (sin lambda citada: los strings deben evaluarse afuera).
      ;; AP: vl-cmdf en vez de command (command no admite apply en CAD).
      ;; El orden de creacion (fondo->leader->textos) ya apila bien por si
      ;; el DRAWORDER fallara en alguna instalacion.
      (setq ss_front (vl-catch-all-apply 'ssadd (list)))
      (if (not (vl-catch-all-error-p ss_front))
        (progn
          (vl-catch-all-apply 'ssadd (list e_bg ss_front))
          (foreach e_tmp e_texts (vl-catch-all-apply 'ssadd (list e_tmp ss_front)))
          (vl-catch-all-apply 'ssadd (list e_leader ss_front))
          (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" ss_front "" "_F"))))
      ;; Huella invisible para siguientes etiquetas (alto +6.7/-8.3 aprox).
      ;; Coincide con la huella validada por CandidateClearance ([x2 x2+box]).
      (setq *GM_PLACED_BOXES* (cons (list (car p2) (- y0 8.3) (+ (car p2) box_w) (+ y0 6.7)) *GM_PLACED_BOXES*))
      T
    )
    )
    )
  )
)
;; AJ: misma anchura medida para buscar sitio y dibujar (antes se reservaba 45).
(defun GM:AfectacionWidth (lines / w i h style span valid)
  (setq w 30.0 i 0 valid T)
  (foreach line lines
    (setq h (if (< i 2) 2.5 1.5)
          style (if (< i 2) "ARIAL BLACK" "VERTICES"))
    (if (not (tblsearch "STYLE" style)) (setq style "ROMANS"))
    ;; P0-6 2026-09-29: medicion memoizada.
    (setq span (GM:TextMeasure line h style))
    (if (and span (numberp span))
      (progn
        (if (> span 0.0) (setq w (max w (+ span 6.0))) (setq valid nil)))
      (setq valid nil))
    (setq i (1+ i)))
  (if (and valid (= i 5)) w nil))

;; Retorna (ancho origen). Una sola linea no medible invalida la medicion total.
(defun GM:ResolveAfectacionWidth (lines / measured)
  (setq measured (vl-catch-all-apply 'GM:AfectacionWidth (list lines)))
  (if (and (not (vl-catch-all-error-p measured)) (numberp measured) (> measured 0.0))
    (list measured "MEDIDO")
    (list 38.0 "FALLBACK_MEDICION")))

;; Variables dinamicamente locales: no afectan al propietario ni a las vias,
;; y se restauran tambien si la busqueda lanza una excepcion.
(defun GM:AfectacionLanding (pf matriz w / *GM_LABEL_EXCLUDE* *GM_LANDING_GAP* *GM_LANDING_MIN_GAP* pfbox matbox roadboxes roadmargin roadbox)
  (setq *GM_LANDING_GAP* 5.0 *GM_LANDING_MIN_GAP* 3.0
        pfbox (GM:PaperBBoxOfEnt pf 3.0)
        matbox (GM:PaperBBoxOfEnt matriz 3.0)
        *GM_LABEL_EXCLUDE* (list pfbox matbox))
  ;; BW: la via tambien es obstaculo duro del AF (mismo lector por tramos que el
  ;; propietario). Solo la huella del texto; el leader puede cruzarla.
  (setq roadboxes (GM:CachedRoadBoxes)
        roadmargin (GM:NumVar *GM_AF_ROAD_MARGIN* 2.0))
  (foreach roadbox roadboxes
    (setq *GM_LABEL_EXCLUDE* (cons (GM:ExpandBox roadbox roadmargin) *GM_LABEL_EXCLUDE*)))
  ;; POR QUE no se quitan las exclusiones: forzar un resultado encima del predio
  ;; produciria una etiqueta ilegible; si no hay sitio, el codigo falla cerrado.
  (if (and pfbox matbox)
    (GM:BestAnchorLandingAF pf w 8.5 8.5 "AFECTACION")
    nil))

;; Etiqueta autom\\U+00E1tica: ancla real PF, texto fuera de PF y matriz.
;; record via accesores GM:AF_* (compatibles 3 y 5 elementos). E4/E5: mejor ancla + aterrizaje.
(defun GM:AutoLabelAfectacion (record / area_m2 area_ha perimetro sufijo pf_ent placed p1_paper p2_paper lines width measured)
  (setq area_m2 (GM:AF_AreaM2 record) area_ha (GM:AF_AreaHa record) perimetro (GM:AF_Perim record)
        sufijo (GM:AF_Suffix record) pf_ent (GM:AF_PfEnt record))
  (if (not (and pf_ent (entget pf_ent)))
    (progn (GM:AutoTrace *ML_TARGET_CODE* (strcat "ETIQUETA_AFECTACION_" sufijo) "ERROR" "PF entidad invalida") nil)
    (progn
      (setq lines (GM:AF_LabelLines *ML_TARGET_CODE* sufijo area_m2 area_ha perimetro)
            measured (GM:ResolveAfectacionWidth lines)
            width (car measured)
            placed (GM:AfectacionLanding pf_ent *UBI_TARGET_POLYGON* width))
      (GM:AutoTrace *ML_TARGET_CODE* (strcat "AF_POSICION_" sufijo) "INFO"
        (strcat "2026-09-15-AQ; PF+3 matriz+3 barridoVP paso="
                (vl-princ-to-string (if *GM_AF_SCAN_STEP* *GM_AF_SCAN_STEP* "?"))
                " min=" (vl-princ-to-string (if *GM_AF_MIN* *GM_AF_MIN* "?"))
                " surFrac=" (vl-princ-to-string (if *GM_AF_SOUTH_FRAC* *GM_AF_SOUTH_FRAC* "?"))
                "; ancho=" (rtos width 2 2) " origen=" (cadr measured)))
      (if (not placed)
        (progn (GM:AutoTrace *ML_TARGET_CODE* (strcat "ETIQUETA_AFECTACION_" sufijo) "ERROR" "Sin posicion AF libre fuera de PF/matriz o geometria no proyectable") nil)
        (progn
          (setq p1_paper (car placed) p2_paper (cadr placed))
          (GM:AutoTrace *ML_TARGET_CODE* (strcat "AF_ANCLA_" sufijo) "INFO"
            (strcat "P1=" (rtos (car p1_paper) 2 2) "," (rtos (cadr p1_paper) 2 2)))
          (GM:AutoTrace *ML_TARGET_CODE* (strcat "AF_CANDIDATOS_" sufijo) "INFO"
            (if *GM_LANDING_DIAG* *GM_LANDING_DIAG* "sin datos"))
          (if (not p2_paper)
            (progn (GM:AutoTrace *ML_TARGET_CODE* (strcat "ETIQUETA_AFECTACION_" sufijo) "ERROR" "Sin espacio libre para etiqueta") nil)
            (progn
              (if (GM:DrawAfectacionWithWidth p1_paper p2_paper lines width)
                (progn
                  (GM:AutoTrace *ML_TARGET_CODE* (strcat "ETIQUETA_AFECTACION_" sufijo) "OK" "Etiqueta afectaci\\U+00F3n autom\\U+00E1tica")
                  T
                )
                (progn (GM:AutoTrace *ML_TARGET_CODE* (strcat "ETIQUETA_AFECTACION_" sufijo) "ERROR" "Fallo dibujo leader") nil)
              )
            )
          )
        )
      )
    )
  )
)

;; Propietario suelto formato antiguo: LEADER horizontal + nombre en negro
;; (MTEXT ARIAL BLACK 1.8 arriba de la linea) + condicion (TEXT ROMANS 1.5 debajo).
;; Ancho dinamico max+4 min 40. Texto siempre hacia +x desde p2, igual que la
;; huella validada por CandidateClearance. Sin caja ni fondo.
(defun GM:DrawPropietarioSuelto (p1 p2 nombre condicion / lay box_w p3 mid_x st_name st_cond elb far e_bg)
  (if (not (and p1 p2 (numberp (car p1)) (numberp (car p2)) nombre))
    (progn (princ "\n[GM:DrawPropietarioSuelto] Puntos o nombre invalidos") nil)
    (progn
      (setq lay (getvar "CTAB")
            st_name (if (tblsearch "STYLE" "ARIAL BLACK") "ARIAL BLACK" "ROMANS")
            st_cond (if (tblsearch "STYLE" "ROMANS") "ROMANS" "STANDARD")
            nombre (if (and nombre (/= nombre "")) nombre "SIN NOMBRE")
            condicion (if condicion condicion ""))
      (setq box_w (GM:PropBoxWidth nombre condicion))
      ;; OPT10 (F5): en modo lote la ruta real no puede cruzar roles criticos.
      (if (and *GM_AUTO_MODE* (not (GM:LeaderRouteClear (GM:LeaderRouteFree p1 p2 box_w))))
        (progn (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETA_PROPIETARIO" "ERROR" "Leader cruzaria ESQUEMA/membrete; etiqueta no dibujada") nil)
        (progn
      (setq p3 (list (+ (car p2) box_w) (cadr p2) 0.0)
            mid_x (+ (car p2) (/ box_w 2.0)))
      ;; Sabana blanca opacidad 30% para que linderos verdes no estorben al texto.
      (entmake (append (list '(0 . "SOLID") '(100 . "AcDbEntity") '(8 . "Membrete")
                           '(420 . 16777215) (cons 440 33554508) '(100 . "AcDbTrace")
                           (cons 10 (list (car p2) (- (cadr p2) 5.0) 0.0))
                           (cons 11 (list (+ (car p2) box_w) (- (cadr p2) 5.0) 0.0))
                           (cons 12 (list (car p2) (+ (cadr p2) 1.7 (* 2.65 (length (GM:PropNameLines nombre)))) 0.0))
                           (cons 13 (list (+ (car p2) box_w) (+ (cadr p2) 1.7 (* 2.65 (length (GM:PropNameLines nombre)))) 0.0)))
                      (list (cons 410 lay))))
      (setq e_bg (entlast))
      ;; Codo en el extremo mas cercano al ancla: la diagonal no cruza el texto.
      (if (<= (abs (- (car p1) (car p3))) (abs (- (car p1) (car p2))))
        (setq elb p3 far p2)
        (setq elb p2 far p3))
      (entmake (append (list '(0 . "LEADER") '(100 . "AcDbEntity") '(8 . "Membrete") '(100 . "AcDbLeader")
                       (cons 10 p1) (cons 10 elb) (cons 10 far)
                       '(70 . 0) '(40 . 4.0) '(370 . 50) '(71 . 1) '(72 . 0) '(73 . 3) '(74 . 1) '(75 . 0))
                    (list (cons 410 lay))))
      ;; P0-5 2026-09-29: sin entupd tras entmake (ya actualiza).
      ;; Nombre arriba de la linea, tamano manual 1.8.
      (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                     (cons 10 (list mid_x (+ (cadr p2) 0.8) 0.0)) '(40 . 1.8) '(41 . 0.0)
                     '(71 . 8) (cons 7 st_name) (cons 1 nombre) '(8 . "Membrete") (cons 410 lay)))
      ;; Condicion juridica debajo de la linea, tamano manual 1.5.
      (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                     (cons 8 "Membrete") (cons 410 lay)
                     (cons 10 (list mid_x (- (cadr p2) 2.3) 0.0)) (cons 11 (list mid_x (- (cadr p2) 2.3) 0.0))
                     '(40 . 1.5) (cons 1 condicion) (cons 7 st_cond) '(72 . 1) '(73 . 2)))
      (setq *GM_PLACED_BOXES*
        (cons (list (car p2) (- (cadr p2) 5.0)
                    (+ (car p2) box_w) (+ (cadr p2) 1.7 (* 2.65 (length (GM:PropNameLines nombre)))))
              *GM_PLACED_BOXES*))
      T
    )
    )
    )
  )
)

;; Etiqueta automatica de propietario/condicion: LEADER + textos sueltos.
;; POR QUE el algoritmo: busca aire a la izquierda incluyendo el sur antes que
;; cercania; la via solo bloquea el texto (la flecha puede cruzarla). Sin
;; fallback legacy: si no hay sitio, falla cerrado para no exportar incompleto.
;; LEGACY: las rutinas AR/AS (PropScan*, BestAnchorLandingPROP, PropLower/Side)
;; se conservan solo por compatibilidad manual; el lote usa PropSpaceLanding.
(defun GM:AutoLabelPropietario (row / nombre condicion matriz_ent placed p1_paper p2_paper prop_w prop_up prop_dn prop_gap)
  (setq nombre (if (and row (nth 0 row)) (nth 0 row) "SIN NOMBRE")
        condicion (if (and row (nth 2 row)) (nth 2 row) "")
        prop_w (GM:PropBoxWidth nombre condicion)
        prop_up (+ 1.7 (* 2.65 (length (GM:PropNameLines nombre))))
        prop_dn 5.0
        prop_gap (GM:NumVar *GM_PROP_GAP* 20.0)
        *GM_PROP_USED_GAP* nil)
  (setq matriz_ent *UBI_TARGET_POLYGON*)
  (if (not (and matriz_ent (entget matriz_ent)))
    (progn (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETA_PROPIETARIO" "ERROR" "Matriz no disponible") nil)
    (progn
      (setq *GM_LABEL_SAMPLES* nil *GM_LABEL_DIST2* nil *GM_LABEL_EXCLUDE* nil
            placed (GM:PropSpaceLanding matriz_ent prop_w prop_up prop_dn))
      (setq *GM_LABEL_SAMPLES* nil *GM_LABEL_DIST2* nil *GM_LABEL_EXCLUDE* nil)
      (if (not placed)
        (progn (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETA_PROPIETARIO" "ERROR" "Sin espacio propietario fuera de matriz/via con gap 15/10") nil)
        (progn
          (setq p1_paper (car placed) p2_paper (cadr placed))
          (GM:AutoTrace *ML_TARGET_CODE* "PROP_POSICION" "INFO"
            (strcat "2026-09-17-AZ6 espacioIzquierdo gap=" (if *GM_PROP_USED_GAP* (rtos *GM_PROP_USED_GAP* 2 1) "15/10")
                    " min=" (rtos (GM:NumVar *GM_PROP_MIN* 3.0) 2 1)
                    "; ancho=" (rtos prop_w 2 2)
                    "; alto_superior=" (rtos prop_up 2 2)
                    "; ancla=" (vl-princ-to-string p1_paper)
                    "; texto=" (vl-princ-to-string p2_paper)))
          (if (not p2_paper)
            (progn (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETA_PROPIETARIO" "ERROR" "Sin espacio libre para etiqueta") nil)
            (progn
          (if (GM:DrawPropietarioSuelto p1_paper p2_paper nombre condicion)
            (progn (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETA_PROPIETARIO" "OK" "Etiqueta propietario autom\\U+00E1tica") T)
            (progn (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETA_PROPIETARIO" "ERROR" "Fallo dibujo leader") nil)
          )
            )
          )
        )
      )
    )
  )
)

;; Puntos Paper visibles de un tramo de via desde su wrapper (nil si no visible).
;; Un solo wrapper por entidad; lo libera el llamador tras usarlo.
(defun GM:RoadLabelSegmentPoints (vla_o / end_p mid_p p1_wcs p2_wcs p1_p p2_p)
  (setq end_p (vl-catch-all-apply 'vlax-curve-getEndParam (list vla_o)))
  (if (and (not (vl-catch-all-error-p end_p)) (numberp end_p))
    (progn
      (setq mid_p (vl-catch-all-apply 'vlax-curve-getPointAtParam (list vla_o (/ end_p 2.0))))
      (if (and mid_p (not (vl-catch-all-error-p mid_p)))
        (progn
          (setq p1_wcs (vl-catch-all-apply 'vlax-curve-getStartPoint (list vla_o)))
          (if (not (vl-catch-all-error-p p1_wcs))
            (progn
              (setq p1_wcs (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla_o p1_wcs)))
              (setq p2_wcs (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla_o mid_p)))
              (if (and p1_wcs (not (vl-catch-all-error-p p1_wcs))
                       p2_wcs (not (vl-catch-all-error-p p2_wcs)))
                (progn
                  (setq p1_p (GM:ModelToPaper p1_wcs) p2_p (GM:ModelToPaper p2_wcs))
                  ;; E6: ambos extremos deben ser visibles en el viewport principal.
                  (if (and p1_p p2_p
                           (GM:IsPaperPointInMainViewport p1_p)
                           (GM:IsPaperPointInMainViewport p2_p))
                    (list p1_p p2_p)))))))))))
;; Segmentos de via visibles localmente para rotulos automaticos.
;; Retorna lista de (p1_paper p2_paper) para cada tramo a rotular.
;; E6: solo tramos con ambos extremos proyectables Y visibles en el viewport; tope 2 por tipo.
(defun GM:RoadLabelSegments (kind / ss i ent hl hl_up result vla_o seg cached cand scanned t0 ncand)
  (setq result nil scanned 0 t0 (getvar "MILLISECS") ncand 0)
  ;; FIX-AV: si PROP_VIA ya detecto las vias, procesar solo esas entidades
  ;; (sin reescanear ~9k polilineas ni crear miles de wrappers COM).
  (setq cached (if (and (boundp '*GM_ROAD_BOXES_CACHE_READY*) *GM_ROAD_BOXES_CACHE_READY*
                        (boundp '*GM_ROAD_LABEL_ENTS*) *GM_ROAD_LABEL_ENTS*)
                 *GM_ROAD_LABEL_ENTS* nil)
        ss nil)
  (if cached
    (progn
      (setq ncand (length cached))
      (foreach cand cached
        (if (< (length result) 2)
          (progn
            (setq ent (car cand))
            (if (and (= (cadr cand) kind) ent (entget ent))
              (progn
                (setq scanned (1+ scanned)
                      vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
                (if (vl-catch-all-error-p vla_o) (setq vla_o nil))
                (if vla_o
                  (progn
                    (setq seg (GM:RoadLabelSegmentPoints vla_o))
                    (if seg (setq result (cons seg result)))
                    (GM:ReleaseObjectSafe vla_o)
                    (setq vla_o nil)))))))))
    (progn
      (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE,POLYLINE") (cons 410 "Model"))))
      (if ss (setq ncand (sslength ss)))))
  (if ss
    (progn
      (setq i 0)
      (while (and (< i (sslength ss)) (< (length result) 2))
        (setq ent (ssname ss i) scanned (1+ scanned) vla_o nil hl nil hl_up "")
        (setq vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (vl-catch-all-error-p vla_o) (setq vla_o nil))
        (if vla_o
          (setq hl (ML_GetHyperlinkString vla_o)
                hl_up (if hl (strcase (vl-string-trim " \t\n\r" hl)) "")))
        (if (or (and (= kind "DV") (vl-string-search "DERECHO" hl_up) (vl-string-search "VIA" hl_up))
                (and (= kind "EJE") (vl-string-search "EJE" hl_up) (vl-string-search "VIA" hl_up)))
          (progn
            (setq seg (GM:RoadLabelSegmentPoints vla_o))
            (if seg (setq result (cons seg result)))
          )
        )
        ;; FIX-AV: soltar el unico wrapper de la iteracion antes de seguir.
        (GM:ReleaseObjectSafe vla_o)
        (setq vla_o nil i (1+ i))
      )
    )
  )
  (GM:AutoTrace *ML_TARGET_CODE* "VIA_SCAN" "INFO"
    (strcat "tipo=" kind " candidatos=" (itoa ncand) " procesados=" (itoa scanned)
            " segmentos=" (itoa (length result))
            " ms=" (itoa (- (getvar "MILLISECS") t0))
            (if cached " via=CACHE" " via=SCAN")))
  (reverse result)
)

;; Etiquetas autom\\U+00E1ticas de v\\U+00EDa (DV y EJE) - CHG3: leader + texto negro ancho sin marco
;; E6: segmentos ya filtrados por visibilidad; landing validado; sin TEXT suelto ni retornos falsos.
(defun GM:AutoLabelVias ( / dv_segments eje_segments seg p1 p2 mid_pt txt_pt n_ok n_om w_lab)
  (setq dv_segments (GM:RoadLabelSegments "DV")
        eje_segments (GM:RoadLabelSegments "EJE")
        n_ok 0 n_om 0
        w_lab (GM:RoadLabelWidth "DERECHO DE V\\U+00CDA"))
  (foreach seg dv_segments
    (setq p1 (car seg) p2 (cadr seg))
    (if (and p1 p2)
      (progn
        (setq mid_pt (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
        ;; Punta en punto medio del segmento visible; aterrizaje validado contra zonas
        (setq txt_pt (GM:AnnotationLanding mid_pt w_lab 2.8 0.4 "VIA_DV"))
        (if (and txt_pt (GM:DrawRoadLabel mid_pt txt_pt "DERECHO DE V\\U+00CDA" w_lab))
          (setq n_ok (1+ n_ok))
          ;; OPT10 (F5): segmento visible omitido (p.ej. toda ruta cruza ESQUEMA).
          (progn (setq n_om (1+ n_om))
                 (GM:AutoTrace *ML_TARGET_CODE* "VIA_SCAN" "AVISO"
                   (strcat "DV sin posicion: " (if *GM_LANDING_DIAG* *GM_LANDING_DIAG* "sin datos"))))
        )
      )
    )
  )
  (setq w_lab (GM:RoadLabelWidth "EJE DE V\\U+00CDA"))
  (foreach seg eje_segments
    (setq p1 (car seg) p2 (cadr seg))
    (if (and p1 p2)
      (progn
        (setq mid_pt (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
        (setq txt_pt (GM:AnnotationLanding mid_pt w_lab 2.8 0.4 "VIA_EJE"))
        (if (and txt_pt (GM:DrawRoadLabel mid_pt txt_pt "EJE DE V\\U+00CDA" w_lab))
          (setq n_ok (1+ n_ok))
          (progn (setq n_om (1+ n_om))
                 (GM:AutoTrace *ML_TARGET_CODE* "VIA_SCAN" "AVISO"
                   (strcat "EJE sin posicion: " (if *GM_LANDING_DIAG* *GM_LANDING_DIAG* "sin datos"))))
        )
      )
    )
  )
  ;; Sin tramos visibles no es fallo: se traza y se retorna T (nada que rotular).
  (GM:AutoTrace *ML_TARGET_CODE* "ETIQUETAS_VIA" "OK"
    (strcat "Etiquetas vias automaticas DV=" (itoa (length dv_segments))
            " EJE=" (itoa (length eje_segments)) " dibujadas=" (itoa n_ok)
            " omitidas=" (itoa n_om)))
  T
)

;; ==============================================================================
;; MOTOR DE VISTA PREVIA AVANZADA (GHOSTING)
;; ==============================================================================
;; POR QUE duplicado: ver nota en PLANO_UBICACION.lsp; esta version gana con el
;; orden oficial y es la que usan los esquemas (registra omision en AUTO).
(defun PREVIEW_SCHEMA (w h h_caj msg / gr code pt_curr loop p1 p2 p3 p4 p5 p6 res)
  (if *GM_AUTO_MODE* (GM:LayoutOmit "PREVIEW_SCHEMA_SIN_ZONA")
    (progn
      (princ (strcat "\n" msg)) (setq loop T res nil)
  (while loop
    (setq gr (grread t 15 0) code (car gr) pt_curr (cadr gr))
    (cond
      ((= code 5)
        (redraw)
        (setq p1 (list (- (car pt_curr) (/ w 2.0)) (- (cadr pt_curr) (/ h 2.0)))
              p2 (list (+ (car pt_curr) (/ w 2.0)) (- (cadr pt_curr) (/ h 2.0)))
              p3 (list (+ (car pt_curr) (/ w 2.0)) (+ (cadr pt_curr) (/ h 2.0)))
              p4 (list (- (car pt_curr) (/ w 2.0)) (+ (cadr pt_curr) (/ h 2.0)))
              p5 (list (+ (car pt_curr) (/ w 2.0)) (- (- (cadr pt_curr) (/ h 2.0)) h_caj))
              p6 (list (- (car pt_curr) (/ w 2.0)) (- (- (cadr pt_curr) (/ h 2.0)) h_caj)))
        (grdraw p1 p2 6) (grdraw p2 p3 6) (grdraw p3 p4 6) (grdraw p4 p1 6)
        (grdraw p1 p6 6) (grdraw p6 p5 6) (grdraw p5 p2 6)
        (grdraw p1 p3 6) (grdraw p2 p4 6)
      )
      ((= code 3) (redraw) (setq res pt_curr loop nil))
      ((or (= code 2) (= code 25)) (redraw) (setq res nil loop nil))
    )
  )
      res
    )
  )
)

(defun PREVIEW_TABLE (w h h_hdr msg / gr code pt_curr loop p1 p2 p3 p4 p5 p6 res)
  (if *GM_AUTO_MODE* (GM:LayoutOmit "PREVIEW_TABLE_SIN_ZONA")
    (progn
      (princ (strcat "\n" msg)) (setq loop T res nil)
  (while loop
    (setq gr (grread t 15 0) code (car gr) pt_curr (cadr gr))
    (cond
      ((= code 5)
        (redraw)
        (setq p1 pt_curr p2 (list (+ (car pt_curr) w) (cadr pt_curr)) p3 (list (+ (car pt_curr) w) (- (cadr pt_curr) h)) p4 (list (car pt_curr) (- (cadr pt_curr) h))
              p5 (list (car pt_curr) (- (cadr pt_curr) h_hdr)) p6 (list (+ (car pt_curr) w) (- (cadr pt_curr) h_hdr)))
        (grdraw p1 p2 6) (grdraw p2 p3 6) (grdraw p3 p4 6) (grdraw p4 p1 6) (grdraw p5 p6 6) (grdraw p1 p3 6) (grdraw p2 p4 6)
      )
      ((= code 3) (redraw) (setq res pt_curr loop nil))
      ((or (= code 2) (= code 25)) (redraw) (setq res nil loop nil))
    )
  )
      res
    )
  )
)

;; VERSI\\U+00D3N ORIGINAL (6 PAR\\U+00C1METROS)
(defun PREVIEW_LEADER_BOX (p_start w_box h_box_up h_box_dn msg p_link / gr code pt_curr loop p1 p2 p3 p4 dir res offset_x)
  (if *GM_AUTO_MODE* (GM:LayoutOmit "LEADER_SIN_ZONA")
    (progn
      (princ (strcat "\n" msg)) (setq loop T res nil offset_x w_box)
  (while loop
    (setq gr (grread t 15 0) code (car gr) pt_curr (cadr gr))
    (cond
      ((= code 5)
        (redraw)
        (if (>= (car pt_curr) (car p_start)) (setq dir 1.0) (setq dir -1.0))
        (setq p1 (list (car pt_curr) (+ (cadr pt_curr) h_box_up)) p2 (list (+ (car pt_curr) (* offset_x dir)) (+ (cadr pt_curr) h_box_up)) p3 (list (+ (car pt_curr) (* offset_x dir)) (- (cadr pt_curr) h_box_dn)) p4 (list (car pt_curr) (- (cadr pt_curr) h_box_dn)))
        (grdraw p_start pt_curr 6) (grdraw pt_curr (list (+ (car pt_curr) (* offset_x dir)) (cadr pt_curr)) 6)
        (grdraw p1 p2 6) (grdraw p2 p3 6) (grdraw p3 p4 6) (grdraw p4 p1 6)
        (if p_link (grdraw p_link pt_curr 1)) 
      )
      ((= code 3) (redraw) (setq res pt_curr loop nil))
      ((or (= code 2) (= code 25)) (redraw) (setq res nil loop nil))
    )
  )
      res
    )
  )
)

(defun PREVIEW_OFFSET_TEXT (pmid ang_line p1 p2 margen msg / gr code pt_curr loop dir offset_pnt res p_txt1 p_txt2)
  (if *GM_AUTO_MODE* (polar pmid (+ ang_line (/ pi 2.0)) margen)
    (progn
      (princ (strcat "\n" msg)) (setq loop T res nil)
  (while loop
    (setq gr (grread t 15 0) code (car gr) pt_curr (cadr gr))
    (cond
      ((= code 5)
        (redraw)
        (grdraw p1 p2 6)
        (if (>= (sin (- (angle pmid pt_curr) ang_line)) 0) (setq offset_pnt (polar pmid (+ ang_line (/ pi 2.0)) margen)) (setq offset_pnt (polar pmid (- ang_line (/ pi 2.0)) margen)))
        (setq p_txt1 (polar offset_pnt ang_line 15.0) p_txt2 (polar offset_pnt (+ ang_line pi) 15.0))
        (grdraw p_txt1 p_txt2 6) (grdraw pmid offset_pnt 6)
      )
      ((= code 3) (redraw) (setq res pt_curr loop nil))
      ((or (= code 2) (= code 25)) (redraw) (setq res nil loop nil))
    )
  )
      res
    )
  )
)

;; ==============================================================================
;; MOTOR DE EXTRACCI\\U+00D3N Y LECTURA DE CSV
;; ==============================================================================
(defun LUBI_ParseCSV ( / openfile linea lista_datos_temp next_line cond_jur part_num)
  (if (and *Membrete_CSV_Path* *ML_TARGET_CODE* (/= *ML_TARGET_CODE* ""))
    (progn
      (setq openfile (open *Membrete_CSV_Path* "r") *GL_DATOS_CSV* nil)
      (if openfile
        (progn
          (while (and (not *GL_DATOS_CSV*) (setq linea (read-line openfile)))
            (while (= (rem (count-quotes linea) 2) 1) (setq linea (strcat linea (if (setq next_line (read-line openfile)) (strcat "\\P" next_line) "\""))))
            (setq lista_datos_temp (mapcar '_clean_val (_str-split linea (if (vl-string-search ";" linea) ";" ","))))
            (if (and (> (length lista_datos_temp) 14) (= (strcase (nth 14 lista_datos_temp)) (strcase *ML_TARGET_CODE*)))
              (setq *GL_DATOS_CSV* lista_datos_temp)
            )
          )
          (close openfile)
          
          (if *GL_DATOS_CSV*
            (progn
              (setq cond_jur (strcase (if (and *GL_DATOS_CSV* (nth 2 *GL_DATOS_CSV*)) (nth 2 *GL_DATOS_CSV*) "")))
              (setq part_num (strcase (if (and *GL_DATOS_CSV* (nth 4 *GL_DATOS_CSV*)) (nth 4 *GL_DATOS_CSV*) "")))
              (while (vl-string-search "P.E." part_num) (setq part_num (vl-string-subst "" "P.E." part_num)))
              (while (vl-string-search "PE." part_num) (setq part_num (vl-string-subst "" "PE." part_num)))
              (while (vl-string-search "P.E" part_num) (setq part_num (vl-string-subst "" "P.E" part_num)))
              (while (vl-string-search "PE" part_num) (setq part_num (vl-string-subst "" "PE" part_num)))
              (setq part_num (vl-string-trim " \t:-N\\U+00D1N" part_num))
              
              (cond
                ((vl-string-search "PROPIETARIO" cond_jur)
                  (setq *ML_ESQ_LOC_TIPO* "0") 
                  (if (or (= part_num "") (= part_num "---") (= part_num "-"))
                    (setq *ML_ESQ_PE_TOG* "0" *ML_ESQ_PE_VAL* "")
                    (setq *ML_ESQ_PE_TOG* "1" *ML_ESQ_PE_VAL* part_num)
                  )
                )
                ((vl-string-search "POSESIONARIO" cond_jur)
                  (setq *ML_ESQ_LOC_TIPO* "1" *ML_ESQ_PE_TOG* "0" *ML_ESQ_PE_VAL* "")
                )
                 ((vl-string-search "OCUPANTE" cond_jur)
                   (setq *ML_ESQ_LOC_TIPO* "2" *ML_ESQ_PE_TOG* "0" *ML_ESQ_PE_VAL* "")
                 )
                 (T
                   (setq *ML_ESQ_LOC_TIPO* "0" *ML_ESQ_PE_TOG* "0" *ML_ESQ_PE_VAL* "")
                 )
               )
              T
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)