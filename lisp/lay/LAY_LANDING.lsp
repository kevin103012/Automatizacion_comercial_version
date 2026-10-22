;; ============================================================================
;; LAY_LANDING.lsp - Sistema GM Ingenieros (modulo lay)
;; Movido SIN CAMBIOS desde LAYOUT_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================

;; Corredor vial en Paper: puntos de las DV (hipervinculo DERECHO+VIA)
;; muestreados (20/curva) y proyectados, solo visibles en el viewport.
;; Bbox (l b r t) o nil si no hay DV visible. Sin imagen ni lado fijo.
(defun GM:RoadCorridorBox (/ ss i ent vla_o hl hl_up endp n k ptWCS points ptPaper xs ys)
  (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE,POLYLINE") (cons 410 "Model")))
        xs nil ys nil)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        ;; AUD1: creacion con catch; entidad caida = se salta.
        (setq vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (vl-catch-all-error-p vla_o) (setq vla_o nil))
        (setq hl (if vla_o (ML_GetHyperlinkString vla_o) nil)
              hl_up (if hl (strcase (vl-string-trim " \t\n\r" hl)) ""))
        (if (and (vl-string-search "DERECHO" hl_up) (vl-string-search "VIA" hl_up))
          (progn
            (setq endp (vl-catch-all-apply 'vlax-curve-getEndParam (list vla_o)))
            (if (and (not (vl-catch-all-error-p endp)) (numberp endp) (> endp 0.0))
              (progn
                (setq n 20 k 0 points nil)
                (while (<= k n)
                  (setq ptWCS (vl-catch-all-apply 'vlax-curve-getPointAtParam
                                (list vla_o (/ (* endp k) n))))
                  (if (and ptWCS (not (vl-catch-all-error-p ptWCS)))
                    (setq points (cons ptWCS points)))
                  (setq k (1+ k)))
            ;; Una sola activacion por curva, igual que PropRoadBoxes.
            (if points
              (progn
                (setq points (vl-catch-all-apply 'GM:ModelPointsToPaper (list (reverse points))))
                (if (not (vl-catch-all-error-p points))
                  (foreach ptPaper points
                    (if (and ptPaper (numberp (car ptPaper)) (numberp (cadr ptPaper))
                             (GM:IsPaperPointInMainViewport ptPaper))
                      (setq xs (cons (car ptPaper) xs)
                            ys (cons (cadr ptPaper) ys)))))))))))
        ;; OPT1/BZ-OPT1: soltar el wrapper de la curva antes de la siguiente iteracion.
        (GM:ReleaseObjectSafe vla_o)
        (setq vla_o nil i (1+ i)))))
  (if (and xs ys)
    (list (apply 'min xs) (apply 'min ys) (apply 'max xs) (apply 'max ys))
    nil))
;; Una pasada de rejilla en [xLo,xMax] (xMax = maximo x2+w).
;; southOk=nil ignora el sur (ni lo colecta). Retorna (norte sur libres surN).
(defun GM:AFScanGrid (w up dn xLo xMax southOk / rect step frame need airW farW airCap distCap nearW
    propbox mbox x2 y2 bx1 by1 bx2 by2 cl d2 mboxd2 south freeN southN north southL c)
  (setq rect (GM:MainViewportRect)
        step (GM:NumVar *GM_AF_SCAN_STEP* 10.0)
        frame (GM:NumVar *GM_AF_FRAME_M* 5.0)
        need (GM:NumVar *GM_AF_MIN* 3.0)
        airW (GM:NumVar *GM_AF_AIR_W* 2.0)
        farW (GM:NumVar *GM_AF_FAR_W* 0.001)
        airCap (GM:NumVar *GM_AF_AIR_CAP* 15.0)
        distCap (GM:NumVar *GM_AF_DIST_CAP* 10000.0)
        nearW (GM:NumVar *GM_AF_NEAR_W* 0.00001)
        propbox (GM:PropEstimateBox)
        mbox (if (and *GM_LABEL_EXCLUDE* (cadr *GM_LABEL_EXCLUDE*)) (cadr *GM_LABEL_EXCLUDE*) nil)
        freeN 0 southN 0 north nil southL nil)
  (if (and rect (> step 0.0) (numberp w) (> w 0.0) (numberp up) (numberp dn)
           (numberp xLo) (numberp xMax) (<= (+ xLo w) xMax))
    (progn
      (setq x2 xLo)
      (while (<= (+ x2 w) xMax)
        (setq y2 (+ (cadr rect) dn frame))
        (while (<= (+ y2 up) (- (nth 3 rect) frame))
          (setq bx1 x2 by1 (- y2 dn) bx2 (+ x2 w) by2 (+ y2 up)
                cl (GM:BoxClearance bx1 by1 bx2 by2))
          (if (and (>= cl need) (GM:SamplesClear bx1 by1 bx2 by2))
            (progn
              (setq freeN (1+ freeN)
                    d2 (if propbox (GM:BoxDist2 bx1 by1 bx2 by2 (car propbox) (cadr propbox) (nth 2 propbox) (nth 3 propbox)) 0.0)
                    south (GM:BoxIsSouth by1 by2 rect)
                    mboxd2 (if mbox (GM:BoxDist2 bx1 by1 bx2 by2 (car mbox) (cadr mbox) (nth 2 mbox) (nth 3 mbox)) 0.0)
                    c (list x2 y2 cl d2 south (+ (* (if (> cl airCap) airCap cl) airW)
                                                 (* (if (and propbox (> d2 distCap)) distCap d2) farW)
                                                 (* -1.0 mboxd2 nearW))))
              (if south (if southOk (setq southN (1+ southN) southL (cons c southL)))
                          (setq north (cons c north)))))
          (setq y2 (+ y2 step)))
        (setq x2 (+ x2 step))))
    nil)
  (list north southL freeN southN))
;; Envoltorio con cascada de regiones. Retorna celdas ordenadas norte-primero
;; (vacia = nada cabe) y deja veredicto en *GM_AF_SCAN_DIAG*.
(defun GM:AFScanCells (w up dn / rect frame roadbox roadGap roadMargin matbox side
    xLo0 xMax0 xLo xMax region grid north southL freeN southN ordered top)
  (setq rect (GM:MainViewportRect)
        frame (GM:NumVar *GM_AF_FRAME_M* 5.0)
        roadbox (vl-catch-all-apply 'GM:CachedRoadCorridorBox (list))
        roadGap (GM:NumVar *GM_AF_ROAD_GAP* 8.0)
        roadMargin (GM:NumVar *GM_AF_ROAD_MARGIN* 2.0)
        matbox (if (and *GM_LABEL_EXCLUDE* (cadr *GM_LABEL_EXCLUDE*)) (cadr *GM_LABEL_EXCLUDE*) nil)
        region nil north nil southL nil freeN 0 southN 0)
  (if (vl-catch-all-error-p roadbox) (setq roadbox nil))
  (if (not rect)
    nil
    (progn
      ;; La via como obstaculo: la caja nunca pisa la via.
      (if (and roadbox (numberp (car roadbox)))
        (setq *GM_LABEL_EXCLUDE*
          (append *GM_LABEL_EXCLUDE*
                  (list (list (- (car roadbox) roadMargin) (- (cadr roadbox) roadMargin)
                              (+ (nth 2 roadbox) roadMargin) (+ (nth 3 roadbox) roadMargin))))))
      ;; Lado = opuesto a la matriz; sin via: este de la matriz; sin nada: total.
      (setq side
        (cond ((and roadbox matbox (numberp (car matbox)))
               (if (< (/ (+ (car matbox) (nth 2 matbox)) 2.0)
                      (/ (+ (car roadbox) (nth 2 roadbox)) 2.0)) 1 -1))
              (matbox 1)
              (T nil))
            xLo0 (+ (car rect) frame) xMax0 (- (nth 2 rect) frame))
      ;; R1: lado de la via (respaldo oeste incluido por construccion).
      (if side
        (progn
          (if (= side 1)
            (setq xLo (max xLo0 (+ (if roadbox (nth 2 roadbox) (nth 2 matbox)) roadGap)) xMax xMax0)
            (setq xLo xLo0 xMax (min xMax0 (- (if roadbox (car roadbox) (car matbox)) roadGap))))
          (setq grid (GM:AFScanGrid w up dn xLo xMax nil))
          (if (car grid)
            (setq region (if (= side 1) "ESTE" "OESTE")
                  north (car grid) southL (cadr grid) freeN (nth 2 grid) southN (nth 3 grid)))))
      ;; R2: total sin sur.
      (if (not region)
        (progn
          (setq grid (GM:AFScanGrid w up dn xLo0 xMax0 nil))
          (if (car grid)
            (setq region "TOTAL" north (car grid) southL (cadr grid) freeN (nth 2 grid) southN (nth 3 grid)))))
      ;; R3: total con sur (ultimo recurso).
      (if (not region)
        (progn
          (setq grid (GM:AFScanGrid w up dn xLo0 xMax0 T))
          (if (or (car grid) (cadr grid))
            (setq region "SUR" north (car grid) southL (cadr grid) freeN (nth 2 grid) southN (nth 3 grid)))))
      (if (not region)
        (progn (setq *GM_AF_SCAN_DIAG* "SCAN:region=VACIO") nil)
        (progn
          (setq north (if north (vl-sort north '(lambda (a b) (> (nth 5 a) (nth 5 b)))) nil)
                southL (if southL (vl-sort southL '(lambda (a b) (> (nth 5 a) (nth 5 b)))) nil)
                ordered (append north southL)
                top (car ordered))
          (setq *GM_AF_SCAN_DIAG*
            (strcat "SCAN:region=" region " libres=" (itoa freeN) " norte=" (itoa (- freeN southN)) " sur=" (itoa southN)
                    (if top
                      (strcat " top=(" (rtos (car top) 2 1) "," (rtos (cadr top) 2 1) ")"
                              " aire=" (rtos (nth 2 top) 2 1)
                              " distProp2=" (rtos (nth 3 top) 2 0)
                              " score=" (rtos (nth 5 top) 2 1)
                              (if (nth 4 top) " SUR-RECURSO" ""))
                      " VACIO")))
          (if ordered ordered nil))))))
;; Enlaza la celda (estilo propietario): centro de la caja -> modelo ->
;; punto mas cercano del PF -> papel. Nil si no proyectable/visible o si
;; el leader cruzaria un rol critico.
;; F15-R1: vla_pf creado con catch y liberado antes de salir en ambas ramas
;; (hasta 40 intentos por codigo sin release era fuga directa).
(defun GM:AFCeldaAncla (pf c w up dn / x2 y2 cx cy modelpt vla_pf anchorWCS p1)
  (setq x2 (car c) y2 (cadr c)
        cx (+ x2 (/ w 2.0)) cy (+ y2 (/ (- up dn) 2.0))
        modelpt (GM:PaperToModel (list cx cy 0.0)))
  (if (not modelpt) nil
    (progn
      (setq vla_pf (vl-catch-all-apply 'vlax-ename->vla-object (list pf)))
      (if (vl-catch-all-error-p vla_pf)
        (setq vla_pf nil)
        (setq anchorWCS (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla_pf modelpt))))
      (GM:ReleaseObjectSafe vla_pf)
      (if (or (vl-catch-all-error-p anchorWCS) (not anchorWCS)) nil
        (progn
          (setq p1 (GM:ModelToPaper anchorWCS))
          ;; OPT10 (F5): ruta real p1->codo->extremo, no la diagonal nominal.
          (if (and p1 (GM:IsPaperPointInMainViewport p1)
                   (GM:LeaderRouteClear (GM:LeaderRouteFree p1 (list x2 y2 0.0) w)))
            p1 nil))))))
;; Calcula punto de quiebre/aterrizaje para leader en Paper, reservando espacio
;; p1_paper: punta de flecha en Paper; w: ancho caja; up/dn: alturas arriba/abajo; role: identificador
;; Retorna p2_paper (quiebre) o nil si no cabe / solapa zona protegida.
;; E5 + mejora de senalizacion: 6 candidatos; la caja evita todo, el leader
;; solo roles criticos; se elige la mayor holgura. Deja el veredicto por
;; candidato en *GM_LANDING_DIAG* para el reporte forense.
(defun GM:AnnotationLanding (p1_paper w up dn role / rect cands res)
  ;; E5: entradas invalidas o sin ancla => sin aterrizaje (no inventar posiciones).
  (if (not (and p1_paper (numberp (car p1_paper)) (numberp (cadr p1_paper))
                (numberp w) (> w 0.0) (numberp up) (numberp dn)))
    nil
    (progn
      (setq rect (GM:MainViewportRect)
            cands (GM:LandingCandidates p1_paper w up dn))
      (setq *GM_LANDING_DIAG* (GM:CandidatesVerdict p1_paper cands w up dn rect))
      (setq res (GM:LandingBestAt p1_paper w up dn))
      (if res (car res) nil)
    )
  )
)
;; LEGACY (conservado, fuera de la ruta del lote): mejor par ancla/aterrizaje
;; sobre una curva con 5 muestras. Solo lo usan pruebas/manual antiguo; el lote
;; usa GM:BestAnchorLandingAF y PropSpaceLanding. role se conserva por aridad.
(defun GM:BestAnchorLanding (ent w up dn role / vla_o end_p fracs frac pt_wcs pt_paper land bestp1 bestp2 bestcl)
  (if (not (and ent (entget ent)))
    nil
    (progn
      ;; F15-R1: wrapper con catch y liberado en ambas salidas (uso manual).
      (setq vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p vla_o) (setq vla_o nil))
      (setq end_p (if vla_o (vl-catch-all-apply 'vlax-curve-getEndParam (list vla_o)) nil))
      (if (or (not vla_o) (vl-catch-all-error-p end_p) (not (numberp end_p)))
        (progn (GM:ReleaseObjectSafe vla_o) nil)
        (progn
          (setq fracs (list 0.0 (/ end_p 4.0) (/ end_p 2.0) (/ (* end_p 3.0) 4.0) end_p)
                bestp1 nil bestp2 nil bestcl nil)
          (foreach frac fracs
            (setq pt_wcs (vl-catch-all-apply 'vlax-curve-getPointAtParam (list vla_o frac)))
            (if (and pt_wcs (not (vl-catch-all-error-p pt_wcs)))
              (progn
                (setq pt_wcs (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla_o pt_wcs)))
                (if (and pt_wcs (not (vl-catch-all-error-p pt_wcs)))
                  (progn
                    (setq pt_paper (GM:ModelToPaper pt_wcs))
                    (if (and pt_paper (GM:IsPaperPointInMainViewport pt_paper))
                      (progn
                        (setq land (GM:LandingBestAt pt_paper w up dn))
                        (if (and land (or (not bestcl) (> (cadr land) bestcl)))
                          (progn
                            (setq bestp1 pt_paper bestp2 (car land) bestcl (cadr land))
                            (setq *GM_LANDING_DIAG* (GM:CandidatesVerdict bestp1 (GM:LandingCandidates bestp1 w up dn) w up dn (GM:MainViewportRect))))))))))))
          (GM:ReleaseObjectSafe vla_o)
          (if bestp1 (list bestp1 bestp2) nil)
        )
      )
    )
  )
)
;; Variante AF place-first (Revision AN/AO, como el propietario): mejor CELDA
;; global del barrido y despues enlace al punto visible del PF mas cercano.
;; AO: gana la celda de FLECHA MAS CORTA (como la imagen manual: cada AF pegada
;; a su poligono). OPT10: examina hasta 12 ANCLAS validas (tope 40 celdas)
;; porque los esquemas duros descartan candidatos. Retorna (ancla quiebre) o nil.
(defun GM:BestAnchorLandingAF (ent w up dn role / cells c p1 bestp1 bestp2 bestc bestlen L tries nattempt)
  (if (not (and ent (entget ent)))
    nil
    (progn
      (setq cells (GM:AFScanCells w up dn) bestp1 nil bestp2 nil bestc nil bestlen nil
            tries 0 nattempt 0)
      ;; OPT10 (F5): con ESQUEMA_AF/LOC como obstaculo duro hay mas celdas
      ;; descartadas por flecha; se sigue examinando hasta juntar 12 anclas
      ;; validas (tope duro 40 celdas para acotar el costo).
      (while (and cells (< nattempt 12) (< tries 40))
        (setq c (car cells) cells (cdr cells) tries (1+ tries)
              p1 (GM:AFCeldaAncla ent c w up dn))
        (if p1
          (progn
            (setq nattempt (1+ nattempt)
                  L (GM:LeaderLen p1 (car c) (cadr c)))
            (if (or (not bestlen) (< L bestlen))
              (setq bestp1 p1 bestp2 (list (car c) (cadr c) 0.0) bestc c bestlen L)))))
      (if bestp1
        (progn
          (setq *GM_LANDING_DIAG*
            (strcat (if *GM_AF_SCAN_DIAG* *GM_AF_SCAN_DIAG* "sin datos")
                    " ANCLA=(" (rtos (car bestp1) 2 1) "," (rtos (cadr bestp1) 2 1) ")"
                    " top12=" (itoa nattempt) " largo2=" (rtos bestlen 2 0)
                    (if (nth 4 bestc) " CELDA-SUR" "")))
          (list bestp1 bestp2))
        nil))))

;; LEGACY (conservado, fuera de la ruta del lote): barrido propietario estilo AQ
;; (Revision AR). Solo pruebas/manual antiguo; el lote usa PropSpaceLanding.
(defun GM:PropScanGrid (w up dn xLo xMax southOk matbox / rect step frame need airW airCap nearW
    x2 y2 bx1 by1 bx2 by2 cl mboxd2 south freeN southN north southL c)
  (setq rect (GM:MainViewportRect)
        step (GM:NumVar *GM_AF_SCAN_STEP* 10.0)
        frame (GM:NumVar *GM_AF_FRAME_M* 5.0)
        need (GM:NumVar *GM_PROP_MIN* 3.0)
        airW (GM:NumVar *GM_AF_AIR_W* 2.0)
        airCap (GM:NumVar *GM_AF_AIR_CAP* 15.0)
        nearW (GM:NumVar *GM_AF_NEAR_W* 0.00001)
        freeN 0 southN 0 north nil southL nil)
  (if (and rect (> step 0.0) (numberp w) (> w 0.0) (numberp up) (numberp dn)
           (numberp xLo) (numberp xMax) (<= (+ xLo w) xMax))
    (progn
      (setq x2 xLo)
      (while (<= (+ x2 w) xMax)
        (setq y2 (+ (cadr rect) dn frame))
        (while (<= (+ y2 up) (- (nth 3 rect) frame))
          (setq bx1 x2 by1 (- y2 dn) bx2 (+ x2 w) by2 (+ y2 up)
                cl (GM:BoxClearance bx1 by1 bx2 by2))
          (if (and (>= cl need) (GM:SamplesClear bx1 by1 bx2 by2))
            (progn
              (setq freeN (1+ freeN)
                    south (GM:BoxIsSouth by1 by2 rect)
                    mboxd2 (if (and matbox (numberp (car matbox))) (GM:BoxDist2 bx1 by1 bx2 by2 (car matbox) (cadr matbox) (nth 2 matbox) (nth 3 matbox)) 0.0)
                    c (list x2 y2 cl 0.0 south (+ (* (if (> cl airCap) airCap cl) airW)
                                                 (* -1.0 mboxd2 nearW))))
              (if south (if southOk (setq southN (1+ southN) southL (cons c southL)))
                          (setq north (cons c north)))))
          (setq y2 (+ y2 step)))
        (setq x2 (+ x2 step))))
    nil)
  (list north southL freeN southN))
;; Envoltorio propietario con cascada de regiones (Revision AR). La matriz
;; expandida con PROP_GAP y la via expandida con ROAD_MARGIN entran como
;; obstaculos duros: la caja nunca las pisa. Retorna celdas ordenadas
;; norte-primero (vacia = nada cabe) y deja veredicto en *GM_PROP_SCAN_DIAG*.
(defun GM:PropScanCells (w up dn mat_ent / rect frame roadbox roadMargin propGap matbox matExp roadExp side
    xLo0 xMax0 xLo xMax region grid north southL freeN southN ordered top)
  (setq rect (GM:MainViewportRect)
        frame (GM:NumVar *GM_AF_FRAME_M* 5.0)
        roadbox (vl-catch-all-apply 'GM:CachedRoadCorridorBox (list))
        roadMargin (GM:NumVar *GM_AF_ROAD_MARGIN* 2.0)
        propGap (GM:NumVar *GM_PROP_GAP* 20.0)
        matbox (if (and mat_ent (entget mat_ent)) (GM:PaperBBoxOfEnt mat_ent 0.0) nil)
        region nil north nil southL nil freeN 0 southN 0)
  (if (vl-catch-all-error-p roadbox) (setq roadbox nil))
  (if (not rect)
    nil
    (progn
      (if (and matbox (numberp (car matbox)))
        (progn
          (setq matExp (list (- (car matbox) propGap) (- (cadr matbox) propGap)
                             (+ (nth 2 matbox) propGap) (+ (nth 3 matbox) propGap)))
          (setq *GM_LABEL_EXCLUDE* (append *GM_LABEL_EXCLUDE* (list matExp)))))
      (if (and roadbox (numberp (car roadbox)))
        (progn
          (setq roadExp (list (- (car roadbox) roadMargin) (- (cadr roadbox) roadMargin)
                              (+ (nth 2 roadbox) roadMargin) (+ (nth 3 roadbox) roadMargin)))
          (setq *GM_LABEL_EXCLUDE* (append *GM_LABEL_EXCLUDE* (list roadExp)))))
      (setq side
        ;; AS: espejo del AF. El propietario va del MISMO lado que la matriz
        ;; (lejos de la via), no mas alla de ella: matriz oeste -> OESTE.
        (cond ((and roadbox matbox (numberp (car matbox)) (numberp (car roadbox)))
               (if (< (/ (+ (car matbox) (nth 2 matbox)) 2.0)
                      (/ (+ (car roadbox) (nth 2 roadbox)) 2.0)) -1 1))
              (matbox 1)
              (T nil))
            xLo0 (+ (car rect) frame) xMax0 (- (nth 2 rect) frame))
      (if side
        (progn
          (if (= side 1)
            (setq xLo (max xLo0 (+ (if roadbox (nth 2 roadbox) (nth 2 matbox)) (if roadbox (GM:NumVar *GM_AF_ROAD_GAP* 8.0) propGap))) xMax xMax0)
            (setq xLo xLo0 xMax (min xMax0 (- (if roadbox (car roadbox) (car matbox)) (if roadbox (GM:NumVar *GM_AF_ROAD_GAP* 8.0) propGap)))))
          (setq grid (GM:PropScanGrid w up dn xLo xMax nil matbox))
          (if (car grid)
            (setq region (if (= side 1) "ESTE" "OESTE")
                  north (car grid) southL (cadr grid) freeN (nth 2 grid) southN (nth 3 grid)))))
      (if (not region)
        (progn
          (setq grid (GM:PropScanGrid w up dn xLo0 xMax0 nil matbox))
          (if (car grid)
            (setq region "TOTAL" north (car grid) southL (cadr grid) freeN (nth 2 grid) southN (nth 3 grid)))))
      (if (not region)
        (progn
          (setq grid (GM:PropScanGrid w up dn xLo0 xMax0 T matbox))
          (if (or (car grid) (cadr grid))
            (setq region "SUR" north (car grid) southL (cadr grid) freeN (nth 2 grid) southN (nth 3 grid)))))
      (if (not region)
        (progn (setq *GM_PROP_SCAN_DIAG* "PSCAN:region=VACIO") nil)
        (progn
          (setq north (if north (vl-sort north '(lambda (a b) (> (nth 5 a) (nth 5 b)))) nil)
                southL (if southL (vl-sort southL '(lambda (a b) (> (nth 5 a) (nth 5 b)))) nil)
                ordered (append north southL)
                top (car ordered))
          (setq *GM_PROP_SCAN_DIAG*
            (strcat "PSCAN:region=" region " libres=" (itoa freeN) " norte=" (itoa (- freeN southN)) " sur=" (itoa southN)
                    (if top
                      (strcat " top=(" (rtos (car top) 2 1) "," (rtos (cadr top) 2 1) ")"
                              " aire=" (rtos (nth 2 top) 2 1)
                              (if (nth 4 top) " SUR-RECURSO" ""))
                      " VACIO")))
          (if ordered ordered nil))))))
;; Variante propietario place-first (Revision AR): mejor CELDA global del
;; barrido y despues enlace al punto visible de la matriz mas cercano.
;; Ancla hasta 12 celdas y gana la de FLECHA MAS CORTA. Retorna (ancla quiebre).
(defun GM:BestAnchorLandingPROP (ent w up dn / cells c p1 bestp1 bestp2 bestc bestlen L tries nattempt)
  (if (not (and ent (entget ent)))
    nil
    (progn
      (setq cells (GM:PropScanCells w up dn ent) bestp1 nil bestp2 nil bestc nil bestlen nil
            tries 0 nattempt 0)
      (while (and cells (< tries 12))
        (setq c (car cells) cells (cdr cells) tries (1+ tries)
              p1 (GM:AFCeldaAncla ent c w up dn))
        (if p1
          (progn
            (setq nattempt (1+ nattempt)
                  L (GM:LeaderLen p1 (car c) (cadr c)))
            (if (or (not bestlen) (< L bestlen))
              (setq bestp1 p1 bestp2 (list (car c) (cadr c) 0.0) bestc c bestlen L)))))
      (if bestp1
        (progn
          (setq *GM_LANDING_DIAG*
            (strcat (if *GM_PROP_SCAN_DIAG* *GM_PROP_SCAN_DIAG* "sin datos")
                    " ANCLA=(" (rtos (car bestp1) 2 1) "," (rtos (cadr bestp1) 2 1) ")"
                    " top12=" (itoa nattempt) " largo2=" (rtos bestlen 2 0)
                    (if (nth 4 bestc) " CELDA-SUR" "")))
          (list bestp1 bestp2))
        nil))))

;; AT: propietario por espacio, independiente del lado de la via.
;; Parametros exclusivos del propietario, persistentes durante el lote.
(if (not *GM_PROP_ROAD_MIN*) (setq *GM_PROP_ROAD_MIN* 5.0))
(if (not *GM_PROP_AIR*) (setq *GM_PROP_AIR* 15.0))
(if (not *GM_PROP_STEP*) (setq *GM_PROP_STEP* 10.0))
(if (not *GM_PROP_RED_M*) (setq *GM_PROP_RED_M* 3.0))
;; CHUNK 2026-09-29: fail-fast del propietario. Si los primeros N grids
;; consecutivos terminan con 0 celdas validas (caso 017/045: 12-24 grids de
;; 420-1880 celdas hasta el AV), la busqueda se corta con traza PROP_AGOTADO y
;; el codigo falla cerrado (ETIQUETA_PROPIETARIO;ERROR), como en sesion fresca.
;; 0 = sin tope (comportamiento F15).
(if (not (boundp '*GM_PROP_MAX_GRIDS*)) (setq *GM_PROP_MAX_GRIDS* 8))
(if (not (boundp '*GM_PROP_GRIDS_0*)) (setq *GM_PROP_GRIDS_0* 0))
;; AUD1 2026-09-29: en modo EVITA (allowRed=nil) el barrido de rojos corta al
;; primer choque: la celda ya quedo descalificada y el score solo cuenta rojos
;; cuando allowRed=T. Misma salida; ~mitad de costo en el peor caso. nil = F15.
(if (not (boundp '*GM_PROP_EARLY_RED*)) (setq *GM_PROP_EARLY_RED* T))
;; Rectangulo Model del viewport principal (+10 margen) para pre-filtrar por
;; bbox antes de muestrear: 4 proyecciones descartan, no 201-801.
(defun GM:ModelRectOfViewport (/ rect corners pp pts xs ys)
  (setq rect (GM:MainViewportRect) pts nil)
  (if rect
    (progn
      (foreach corners (list (list (car rect) (cadr rect) 0.0)
                             (list (nth 2 rect) (cadr rect) 0.0)
                             (list (nth 2 rect) (nth 3 rect) 0.0)
                             (list (car rect) (nth 3 rect) 0.0))
        (setq pp (vl-catch-all-apply 'GM:PaperToModel (list corners)))
        (if (and pp (not (vl-catch-all-error-p pp))
                 (numberp (car pp)) (numberp (cadr pp)))
          (setq pts (cons pp pts))))
      (if pts
        (progn
          (setq xs (mapcar 'car pts) ys (mapcar 'cadr pts))
          (list (- (apply 'min xs) 10.0) (- (apply 'min ys) 10.0)
                (+ (apply 'max xs) 10.0) (+ (apply 'max ys) 10.0)))
        nil))
    nil))
;; Mismo lector y categorias que UBI_ApplyAutoFormat. Incluye LINE/ARC/SPLINE,
;; no solo polilineas. Tramos cortos en Paper, nunca bbox unica de toda la via.
(defun GM:PropRoadBoxes (/ ss i ent ed obj hl ep k count prev pt box rect mRect lo hi bb boxes found points started errors
    curveMs nBBox nHl maxMs maxEnt labelKind)
  (setq started (getvar "MILLISECS"))
  (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA_INICIO" "INICIO" "AU proyeccion agrupada por curva")
  (setq ss (ssget "X" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE") (cons 410 "Model")))
        i 0 boxes nil found 0 rect (GM:MainViewportRect) mRect (GM:ModelRectOfViewport)
        nBBox 0 nHl 0 maxMs 0 maxEnt 0)
  (if ss
    (repeat (sslength ss)
      (setq ent (ssname ss i) i (1+ i) ed (entget ent) bb nil)
      ;; CHUNK 2026-09-29: wrapper con catch (si la entidad cayo entre ssget y
      ;; la conversion, el catch devuelve error y se salta sin abortar).
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p obj) (setq obj nil))
      ;; Prefiltro barato: bbox Model vs viewport Model, antes del hipervinculo.
      (if mRect
        (progn
          (setq bb (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'lo 'hi)))
          (if (and (not (vl-catch-all-error-p bb))
                   (progn (setq lo (vlax-safearray->list lo) hi (vlax-safearray->list hi)) T)
                   (> (GM:GapToBox (car lo) (cadr lo) (car hi) (cadr hi) mRect) 0.0))
             ;; OPT1/BZ-OPT1: el wrapper del prefiltrado tambien se suelta; si no, se
             ;; acumula un wrapper por entidad descartada en cada codigo.
             (progn (GM:ReleaseObjectSafe obj) (setq nBBox (1+ nBBox) obj nil)))))
      (if obj
        (progn
          (setq hl (UBI_GetHyperlinkString obj) hl (if hl (strcase hl) ""))
          (if (and (/= (cdr (assoc 60 ed)) 1)
                   (or (vl-string-search "DERECHO DE VIA" hl)
                       (vl-string-search "EJE DE VIA" hl)
                       (vl-string-search "VIA EXISTENTE" hl)))
            (progn
              ;; BC forense: traza previa a cualquier metodo de curva. Si
              ;; AutoCAD cae con AV, la cola del reporte identifica la entidad
              ;; (AutoTrace hace flush por linea).
              (setq found (1+ found))
              ;; FIX-AV: guardar la entidad vial para rotulos sin reescanear.
              ;; Solo DV/EJE se rotulan; EXISTENTE no entra a la cache.
              (setq labelKind (cond ((vl-string-search "DERECHO DE VIA" hl) "DV")
                                    ((vl-string-search "EJE DE VIA" hl) "EJE")
                                    (T nil)))
              (if labelKind
                (setq *GM_ROAD_LABEL_ENTS* (cons (list ent labelKind) *GM_ROAD_LABEL_ENTS*)))
              (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA_CAND" "INFO"
                (strcat "entidad=" (itoa found)
                        " handle=" (if (cdr (assoc 5 ed)) (cdr (assoc 5 ed)) "?")
                        " tipo=" (if (cdr (assoc 0 ed)) (cdr (assoc 0 ed)) "?")
                        " hl=" (cond ((vl-string-search "DERECHO DE VIA" hl) "DERECHO")
                                     ((vl-string-search "EJE DE VIA" hl) "EJE")
                                     (T "EXISTENTE"))))
              (setq ep (vl-catch-all-apply 'vlax-curve-getEndParam (list obj))
                    count (if (and (numberp ep) (> ep 0.0))
                            (min 801 (max 64 (fix (* ep 8))))
                            0)
                    k 0 prev nil points nil errors 0 curveMs (getvar "MILLISECS"))
              (if (vl-catch-all-error-p ep) (setq ep nil))
              (if (and ep (> count 0))
                (progn
                  (repeat (1+ count)
                    (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtParam
                                (list obj (/ (* ep k) count))))
                    (if (vl-catch-all-error-p pt)
                      (setq pt nil errors (1+ errors)))
                    (setq points (cons pt points) k (1+ k)))
                   (setq points (GM:ModelPointsToPaper (reverse points)))))
              ;; OPT1/BZ-OPT1: fases por candidata (muestreo WCS y proyeccion a Paper).
              (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA_MUESTRA" "INFO"
                (strcat "entidad=" (itoa found) " muestras=" (itoa (length points))))
              (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA_PROY" "INFO"
                (strcat "entidad=" (itoa found) " proyectados=" (itoa (length points))))
              (setq curveMs (- (getvar "MILLISECS") curveMs))
              (if (> curveMs maxMs) (setq maxMs curveMs maxEnt found))
              ;; Detalle solo para atipicas; el resumen va al final.
              (if (or (> errors 0) (> curveMs 1500) *GM_DEBUG_VIA*)
                (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA_CURVA" "INFO"
                  (strcat "entidad=" (itoa found) " muestras=" (itoa (length points))
                          " limite=801 nil=" (itoa errors)
                          " ms=" (itoa curveMs))))
              (foreach pt points
                (if (not pt)
                  (setq prev nil)
                  (progn
                    (if prev
                      (progn
                        (setq box (list (min (car prev) (car pt)) (min (cadr prev) (cadr pt))
                                        (max (car prev) (car pt)) (max (cadr prev) (cadr pt))))
                        (if (<= (GM:GapToBox (car box) (cadr box) (nth 2 box) (nth 3 box) rect) 0.0)
                          (setq boxes (cons box boxes)))))
                    (setq prev pt))))
              ;; OPT1/BZ-OPT1: fase TRANS por candidata + liberacion del wrapper COM.
              (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA_TRANS" "INFO"
                (strcat "entidad=" (itoa found) " tramosVP=" (itoa (length boxes))))
              (GM:ReleaseObjectSafe obj)
              (setq obj nil)
            ;; No vial: tambien se suelta el wrapper antes de seguir.
            (progn (GM:ReleaseObjectSafe obj) (setq obj nil nHl (1+ nHl)))))))))
  (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA_RESUMEN" "INFO"
    (strcat "ss=" (itoa (if ss (sslength ss) 0)) " bboxSkip=" (itoa nBBox)
            " hlSkip=" (itoa nHl) " viales=" (itoa found)
            " rotulos=" (itoa (length *GM_ROAD_LABEL_ENTS*))
            " tramosVP=" (itoa (length boxes)) " maxCurvaMs=" (itoa maxMs)))
  (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA" (if boxes "INFO" "AVISO")
    (strcat "AU entidades=" (itoa found) " tramosVP=" (itoa (length boxes))
            " ms=" (itoa (- (getvar "MILLISECS") started))
            (if boxes " lector=UBI; corredor aproximado por tramos" " SIN_GEOMETRIA_VIAL_VISIBLE; proteccion no confirmada")))
  ;; OPT1/BZ-OPT1: soltar el selection set al salir; no arrastrar referencias entre codigos.
  (setq ss nil)
  boxes)
;; BW 2026-09-22: la via es obstaculo DURO compartido AF/propietario para la
;; huella del texto (el leader si puede cruzarla: GM:LeaderClear no la mira).
;; DERECHO+EJE+EXISTENTE, tipos LINE/ARC/SPLINE/LWPOLYLINE/POLYLINE, por tramos
;; Paper via GM:PropRoadBoxes. Una sola deteccion por codigo (cache en
;; *GM_ROAD_BOXES_CACHE*, reiniciada por codigo en GM:AutoLayoutForCode).
(if (not (boundp '*GM_ROAD_BOXES_CACHE*)) (setq *GM_ROAD_BOXES_CACHE* nil))
;; OPT1/BZ-OPT1: READY distingue "sin calcular" de "calculado sin vias" (nil cacheable).
(if (not (boundp '*GM_ROAD_BOXES_CACHE_READY*)) (setq *GM_ROAD_BOXES_CACHE_READY* nil))
;; FIX-AV: entidades viales del codigo para rotulos, sin reescanear el Model.
;; Lista de (ent kind) con kind "DV"/"EJE"; valida solo con READY en T.
(if (not (boundp '*GM_ROAD_LABEL_ENTS*)) (setq *GM_ROAD_LABEL_ENTS* nil))
;; OPT2 2026-09-26 (Fase 2): indice estatico de vias del lote en WCS. Cada via
;; se muestrea una sola vez al iniciar el lote; por codigo solo se reproyecta a
;; Paper con el viewport vigente. Entradas (kind ent puntosWCS). El indice NO
;; guarda bboxes Paper ni resultados dependientes del viewport.
(if (not (boundp '*GM_AUTO_ROAD_WCS*)) (setq *GM_AUTO_ROAD_WCS* nil))
(if (not (boundp '*GM_AUTO_ROADS_INDEX_READY*)) (setq *GM_AUTO_ROADS_INDEX_READY* nil))
;; PA-2 (H4) 2026-09-28: corredor DV cacheado por codigo. En lote se deriva del
;; indice WCS del lote (sin reescanear el Model ni leer COM); en manual cae al
;; barrido clasico de GM:RoadCorridorBox (que queda puro para su contrato).
(if (not (boundp '*GM_ROAD_CORRIDOR_CACHE*)) (setq *GM_ROAD_CORRIDOR_CACHE* nil))
(if (not (boundp '*GM_ROAD_CORRIDOR_READY*)) (setq *GM_ROAD_CORRIDOR_READY* nil))
;; P1-1 2026-09-29: cache de cajas rojas P.E./U.C. por codigo (espejo del cache
;; vial). El conjunto de rojos depende del codigo (el formato cambia colores de
;; PE/UC), por eso NO se cachea entre codigos: se mide una vez por codigo y se
;; reutiliza dentro del mismo (si dos etiquetas piden landing, no se repite).
(if (not (boundp '*GM_RED_BOXES_CACHE*)) (setq *GM_RED_BOXES_CACHE* nil))
(if (not (boundp '*GM_RED_BOXES_CACHE_READY*)) (setq *GM_RED_BOXES_CACHE_READY* nil))
;; P1-4 2026-09-29: memo de bbox Model por entidad dentro del lote. Evita repetir
;; vla-GetBoundingBox sobre las mismas entidades entre consumidores; solo se
;; guarda en modo lote y se libera al cerrar el codigo (ResetRoadCache/reset).
(if (not (boundp '*GM_MODEL_BBOX_CACHE*)) (setq *GM_MODEL_BBOX_CACHE* nil))
(defun GM:ResetRoadCache () (setq *GM_ROAD_BOXES_CACHE* nil *GM_ROAD_BOXES_CACHE_READY* nil *GM_ROAD_LABEL_ENTS* nil *GM_ROAD_CORRIDOR_CACHE* nil *GM_ROAD_CORRIDOR_READY* nil *GM_RED_BOXES_CACHE* nil *GM_RED_BOXES_CACHE_READY* nil *GM_MODEL_BBOX_CACHE* nil) T)
;; Corredor DV desde el indice WCS: prefiltro por bbox Model contra el viewport,
;; proyeccion a Paper y recorte a la ventana visible (misma matematica del
;; corredor clasico, sin COM por entidad).
(defun GM:RoadCorridorBoxFromIndex (/ rect mRect entry bb points pt prev xs ys)
  (setq rect (GM:MainViewportRect) mRect (GM:ModelRectOfViewport) xs nil ys nil)
  (foreach entry *GM_AUTO_ROAD_WCS*
    (if (= (car entry) "DV")
      (progn
        (setq bb (nth 3 entry))
        (if (and bb mRect (> (GM:GapToBox (car bb) (cadr bb) (nth 2 bb) (nth 3 bb) mRect) 0.0))
          nil
          (progn
            (setq points (GM:ModelPointsToPaper (caddr entry)) prev nil)
            (foreach pt points
              (if (and pt (numberp (car pt)) (numberp (cadr pt)) (GM:IsPaperPointInMainViewport pt))
                (setq xs (cons (car pt) xs) ys (cons (cadr pt) ys)))))))))
  (if (and xs ys)
    (list (apply 'min xs) (apply 'min ys) (apply 'max xs) (apply 'max ys))
    nil))
;; Cache por codigo (solo lote): el primer consumidor mide, el segundo reutiliza.
;; PA-3: deja traza de la fuente (CACHE/INDICE/CLASICO) para el reporte del lote.
(defun GM:CachedRoadCorridorBox (/ src value)
  (if *GM_AUTO_MODE*
    (progn
      (if *GM_ROAD_CORRIDOR_READY*
        (setq src "CACHE" value *GM_ROAD_CORRIDOR_CACHE*)
        (if (and (boundp '*GM_AUTO_ROADS_INDEX_READY*) *GM_AUTO_ROADS_INDEX_READY* *GM_AUTO_ROAD_WCS*)
          (setq src "INDICE" value (GM:RoadCorridorBoxFromIndex))
          (setq src "CLASICO" value (GM:RoadCorridorBox))))
      (if (not *GM_ROAD_CORRIDOR_READY*)
        (setq *GM_ROAD_CORRIDOR_CACHE* value *GM_ROAD_CORRIDOR_READY* T))
      (GM:AutoTrace *ML_TARGET_CODE* "CORREDOR_FUENTE" "INFO" src)
      value)
    (GM:RoadCorridorBox)))
(defun GM:CachedRoadBoxes (/ boxes)
  (if (and (boundp '*GM_ROAD_BOXES_CACHE_READY*) *GM_ROAD_BOXES_CACHE_READY*)
    *GM_ROAD_BOXES_CACHE*
    (progn
      (setq boxes (if (and (boundp '*GM_AUTO_ROADS_INDEX_READY*) *GM_AUTO_ROADS_INDEX_READY*)
                    (GM:PropRoadBoxesFromIndex)
                    (GM:PropRoadBoxes)))
      (setq *GM_ROAD_BOXES_CACHE* boxes *GM_ROAD_BOXES_CACHE_READY* T)
      boxes)))
;; P1-1 2026-09-29: wrapper de rojos con cache por codigo (solo lote). En manual
;; recalcula en cada llamada, identico al comportamiento previo.
(defun GM:CachedRedBoxes (/ boxes)
  (if (and *GM_AUTO_MODE* (boundp '*GM_RED_BOXES_CACHE_READY*) *GM_RED_BOXES_CACHE_READY*)
    *GM_RED_BOXES_CACHE*
    (progn
      (setq boxes (GM:PropRedBoxes))
      (if *GM_AUTO_MODE*
        (setq *GM_RED_BOXES_CACHE* boxes *GM_RED_BOXES_CACHE_READY* T))
      boxes)))
;; OPT2 2026-09-26 (Fase 2): muestreo unico de vias en WCS al inicio del lote.
(defun GM:BatchIndexRoads (/ ss i ent ed obj hl ep count k points kind pt bb n_dv n_eje n_exi)
  (setq *GM_AUTO_ROAD_WCS* nil *GM_AUTO_ROADS_INDEX_READY* nil *GM_ROAD_LABEL_ENTS* nil)
  (GM:AutoTrace "LOTE" "INDICE_VIAS" "INICIO" "Indexando vias del Model una sola vez")
  (setq ss (ssget "X" (list (cons 0 "LINE,ARC,SPLINE,LWPOLYLINE,POLYLINE") (cons 410 "Model"))) i 0
        n_dv 0 n_eje 0 n_exi 0)
  (if ss
    (repeat (sslength ss)
      (setq ent (ssname ss i) i (1+ i) ed (entget ent)
            ;; CHUNK 2026-09-29: wrapper con catch (entidad caida = se salta).
            obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p obj) (setq obj nil))
      (setq hl (if obj (UBI_GetHyperlinkString obj) "")
            hl (if hl (strcase hl) ""))
      (if (and (/= (cdr (assoc 60 ed)) 1)
               (or (vl-string-search "DERECHO DE VIA" hl)
                   (vl-string-search "EJE DE VIA" hl)
                   (vl-string-search "VIA EXISTENTE" hl)))
        (progn
          (setq kind (cond ((vl-string-search "DERECHO DE VIA" hl) "DV")
                           ((vl-string-search "EJE DE VIA" hl) "EJE")
                           (T "EXISTENTE")))
          (setq ep (vl-catch-all-apply 'vlax-curve-getEndParam (list obj))
                count (if (and (numberp ep) (> ep 0.0)) (min 801 (max 64 (fix (* ep 8)))) 0)
                k 0 points nil)
          (if (and (numberp ep) (not (vl-catch-all-error-p ep)) (> count 0))
            (repeat (1+ count)
              (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtParam (list obj (/ (* ep k) count))))
              (if (vl-catch-all-error-p pt) (setq pt nil))
              (setq points (cons pt points) k (1+ k))))
          (setq points (reverse points))
          ;; OPT2 correccion: guardar bbox Model para prefiltrar por viewport
          ;; sin proyectar las 500+ vias que quedan fuera de la vista.
          (setq bb (GM:ModelBBoxOfEnt ent))
          (if points
            (setq *GM_AUTO_ROAD_WCS* (cons (list kind ent points bb) *GM_AUTO_ROAD_WCS*)))
          (if (= kind "DV") (setq n_dv (1+ n_dv)))
          (if (= kind "EJE") (setq n_eje (1+ n_eje)))
          (if (= kind "EXISTENTE") (setq n_exi (1+ n_exi)))
          (if (or (= kind "DV") (= kind "EJE"))
            (setq *GM_ROAD_LABEL_ENTS* (cons (list ent kind) *GM_ROAD_LABEL_ENTS*)))))
      (GM:ReleaseObjectSafe obj)
      (setq obj nil)))
  (setq ss nil *GM_AUTO_ROADS_INDEX_READY* T)
  (GM:AutoTrace "LOTE" "INDICE_VIAS" "OK"
    (strcat "vias=" (itoa (length *GM_AUTO_ROAD_WCS*))
            " dv=" (itoa n_dv) " eje=" (itoa n_eje) " existente=" (itoa n_exi)
            " rotulos=" (itoa (length *GM_ROAD_LABEL_ENTS*))))
  T)
;; OPT2 2026-09-26 (Fase 2): por codigo solo reproyecta el indice WCS a Paper.
;; Correccion: prefiltro por bbox Model contra el viewport; solo se proyectan
;; las vias visibles (antes se proyectaban las 500+ existentes de todo el Model).
(defun GM:PropRoadBoxesFromIndex (/ rect mRect entry bb points pt prev box boxes found candidatas started)
  (setq started (getvar "MILLISECS")
        rect (GM:MainViewportRect) mRect (GM:ModelRectOfViewport) boxes nil found 0 candidatas 0)
  (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA_INICIO" "INICIO" "OPT2 indice WCS (sin reescaneo)")
  ;; Repoblar rotulos de vias desde el indice para este codigo.
  (setq *GM_ROAD_LABEL_ENTS* nil)
  (foreach entry *GM_AUTO_ROAD_WCS*
    (if (or (= (car entry) "DV") (= (car entry) "EJE"))
      (setq *GM_ROAD_LABEL_ENTS* (cons (list (cadr entry) (car entry)) *GM_ROAD_LABEL_ENTS*))))
  (if (and *GM_AUTO_ROAD_WCS* rect)
    (foreach entry *GM_AUTO_ROAD_WCS*
      (setq points (caddr entry) bb (nth 3 entry) prev nil)
      ;; Prefiltro: descartar la via si su bbox Model no toca el viewport.
      (if (and bb mRect (> (GM:GapToBox (car bb) (cadr bb) (nth 2 bb) (nth 3 bb) mRect) 0.0))
        nil
        (if points
          (progn
            (setq candidatas (1+ candidatas))
            (setq points (GM:ModelPointsToPaper points) found (1+ found))
            (foreach pt points
              (if (not pt)
                (setq prev nil)
                (progn
                  (if prev
                    (progn
                      (setq box (list (min (car prev) (car pt)) (min (cadr prev) (cadr pt))
                                      (max (car prev) (car pt)) (max (cadr prev) (cadr pt))))
                      (if (<= (GM:GapToBox (car box) (cadr box) (nth 2 box) (nth 3 box) rect) 0.0)
                        (setq boxes (cons box boxes)))))
                  (setq prev pt)))))))))
  (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA_RESUMEN" "INFO"
    (strcat "indexTotal=" (itoa (length *GM_AUTO_ROAD_WCS*))
            " candidatasVP=" (itoa candidatas) " proyectadas=" (itoa found)
            " rotulos=" (itoa (length *GM_ROAD_LABEL_ENTS*))
            " tramosVP=" (itoa (length boxes)) " index=1"))
  (GM:AutoTrace *ML_TARGET_CODE* "PROP_VIA" (if boxes "INFO" "AVISO")
    (strcat "OPT2 indice candidatasVP=" (itoa candidatas) " proyectadas=" (itoa found)
            " tramosVP=" (itoa (length boxes))
            (if boxes " lector=UBI; corredor aproximado por tramos" " SIN_GEOMETRIA_VIAL_VISIBLE; proteccion no confirmada")))
  (GM:AutoTrace *ML_TARGET_CODE* "TIEMPO_PROP_VIA" "INFO"
    (strcat "ms=" (itoa (- (getvar "MILLISECS") started))))
  boxes)
(defun GM:ExpandBox (box margin / m)
  (setq m (if (numberp margin) margin 0.0))
  (list (- (nth 0 box) m) (- (nth 1 box) m) (+ (nth 2 box) m) (+ (nth 3 box) m)))
;; Aire sobre el minimo: matriz GAP, via ROAD_MIN, otras cajas MIN, marco 5.
;; Caja entera evaluada. Un resultado negativo rechaza el candidato.
(defun GM:PropSpaceAir (x y w up dn matbox roads / clearance road)
  (setq clearance (min
    (- (GM:GapToBox x (- y dn) (+ x w) (+ y up) matbox) (GM:NumVar *GM_PROP_GAP* 20.0))
    (- (GM:BoxClearance x (- y dn) (+ x w) (+ y up)) (GM:NumVar *GM_PROP_MIN* 3.0))))
  ;; Una caja ya rechazada no necesita medir el resto de la cartografia.
  (while (and roads (>= clearance 0.0))
    (setq road (car roads) roads (cdr roads))
    (setq clearance (min clearance (- (GM:GapToBox x (- y dn) (+ x w) (+ y up) road)
                                     (GM:NumVar *GM_PROP_ROAD_MIN* 5.0)))))
  clearance)
;; Orden de celdas: izquierda primero, luego aire, puntaje y cercania.
;; La region (IZQUIERDA/TOTAL/SUR-RECURSO) ya filtro; el sur nunca gana si hay
;; posicion valida en regiones anteriores.
(defun GM:PropCellBetter (a b / aa ba sa sb)
  (if (/= (nth 4 a) (nth 4 b))
    (nth 4 a)
    (progn
      (setq aa (min (nth 2 a) 15.0) ba (min (nth 2 b) 15.0)
            sa (if (numberp (nth 5 a)) (nth 5 a) 0.0)
            sb (if (numberp (nth 5 b)) (nth 5 b) 0.0))
      (if (/= aa ba)
        (> aa ba)
        (if (and (/= sa 0.0) (/= sb 0.0) (/= sa sb))
          (> sa sb)
          (if (/= (nth 3 a) (nth 3 b))
            (< (nth 3 a) (nth 3 b))
            (> (nth 2 a) (nth 2 b))))))))

;; Bbox Model (l b r t) de una entidad o nil. Sin cambios de espacio.
;; P1-4 2026-09-29: memo por lote (assoc por ename) para no repetir COM sobre las
;; mismas entidades; solo almacena en modo lote y se libera con ResetRoadCache.
(defun GM:ModelBBoxOfEnt (ent / vla_o lo hi result hit)
  (if (and ent (entget ent))
    (progn
      (setq hit (if *GM_AUTO_MODE* (assoc ent *GM_MODEL_BBOX_CACHE*) nil))
      (if hit
        (cdr hit)
        (progn
          (setq vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
          (if (vl-catch-all-error-p vla_o)
            nil
            (progn
              (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list vla_o 'lo 'hi))))
                (progn
                  (setq lo (vlax-safearray->list lo) hi (vlax-safearray->list hi))
                  (if (and lo hi (numberp (car lo)) (numberp (car hi)))
                    (setq result (list (car lo) (cadr lo) (car hi) (cadr hi))))))
              ;; Fase 1B (H-5): liberar el wrapper COM antes de salir.
              (GM:ReleaseObjectSafe vla_o)
              (if *GM_AUTO_MODE*
                (setq *GM_MODEL_BBOX_CACHE* (cons (cons ent result) *GM_MODEL_BBOX_CACHE*)))
              result)))))
    nil))
;; Bboxes Paper de varias entidades con UNA sola activacion de viewport.
;; Entrada: lista de enames y margen. Salida: lista de (l b r t) visibles.
(defun GM:PaperBBoxesOfEnts (ents margin / rect all pts boxes ent box corners pp xs ys)
  (setq rect (GM:MainViewportRect) all nil boxes nil)
  (if (and rect ents (numberp margin))
    (progn
      (foreach ent ents
        (setq box (GM:ModelBBoxOfEnt ent))
        (if box (setq all (cons (list ent box) all))))
      (if all
        (progn
          (setq pts nil all (reverse all))
          (foreach ent all
            (setq box (cadr ent))
            (setq pts (append pts (list (list (car box) (cadr box) 0.0)
                                        (list (nth 2 box) (cadr box) 0.0)
                                        (list (nth 2 box) (nth 3 box) 0.0)
                                        (list (car box) (nth 3 box) 0.0)))))
          (setq pts (GM:ModelPointsToPaper pts))
          (if (and pts (not (vl-catch-all-error-p pts)))
            (foreach ent all
              (setq corners (list (car pts) (cadr pts) (caddr pts) (car (cdr (cdr (cdr pts)))))
                    pts (cdr (cdr (cdr (cdr pts))))
                    xs nil ys nil)
              (foreach pp corners
                (if (and pp (numberp (car pp)) (numberp (cadr pp)))
                  (setq xs (cons (car pp) xs) ys (cons (cadr pp) ys))))
              (if (and xs ys)
                (progn
                  (setq box (list (- (apply 'min xs) margin) (- (apply 'min ys) margin)
                                  (+ (apply 'max xs) margin) (+ (apply 'max ys) margin)))
                  (if (<= (GM:GapToBox (car box) (cadr box) (nth 2 box) (nth 3 box) rect) 0.0)
                    (setq boxes (cons box boxes)))))))))))
  boxes)
;; Codigos rojos P.E./U.C. como obstaculo blando del propietario: se evitan con
;; margen 3, pero se permite pisarlos como ultimo recurso con penalidad.
;; Proyeccion barata por bbox (una vez por texto), sin muestreo denso.
(defun GM:PropRedBoxes (/ ss i ent ed str cands boxes started)
  (setq started (getvar "MILLISECS")
        ss (ssget "X" (list (cons 0 "TEXT,MTEXT") (cons 410 "Model")))
        i 0 cands nil boxes nil)
  (if ss
    (repeat (sslength ss)
      (setq ent (ssname ss i) i (1+ i) ed (entget ent))
      (if (and (= (cdr (assoc 62 ed)) 1) (/= (cdr (assoc 60 ed)) 1))
        (progn
          (setq str (cdr (assoc 1 ed)) str (if str (strcase str) ""))
          (if (or (vl-string-search "P.E." str) (vl-string-search "U.C." str))
            (setq cands (cons ent cands)))))))
  (if cands (setq boxes (GM:PaperBBoxesOfEnts (reverse cands) 0.0)))
  (GM:AutoTrace *ML_TARGET_CODE* "PROP_SUAVE" "INFO"
    (strcat "rojos=" (itoa (length boxes)) " ms=" (itoa (- (getvar "MILLISECS") started))))
  boxes)
(defun GM:PropSpaceAnchor (ent c w up dn / modelpt anchor p1 vla_o)
  (setq modelpt (GM:PaperToModel (list (+ (car c) (/ w 2.0)) (+ (cadr c) (/ (- up dn) 2.0)) 0.0)))
  (if modelpt
    (progn
      ;; F15-R3: rastro forense antes del COM del ancla (entidad/punto/celda).
      (if (boundp 'GM:AutoForensic)
        (GM:AutoForensic *ML_TARGET_CODE* "PROP_ANCLA"
          (strcat "ent=" (if (and ent (entget ent)) (cdr (assoc 5 (entget ent))) "?")
                  " celda=" (vl-princ-to-string c))))
      ;; FIX-AV: wrapper nominal liberado tras medir (una sola entidad).
      (setq vla_o (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p vla_o) (setq vla_o nil))
      (setq anchor (if vla_o (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla_o modelpt)) nil)
            p1 (if (and anchor (not (vl-catch-all-error-p anchor))) (GM:ModelToPaper anchor) nil))
      (GM:ReleaseObjectSafe vla_o)
      (if (and p1 (GM:IsPaperPointInMainViewport p1))
        ;; OPT10 (F5): ruta real (diagonal al extremo cercano + linea base).
        (if (GM:LeaderRouteClear (GM:LeaderRouteFree p1 (list (car c) (cadr c) 0.0) w)) p1 nil)
        nil))
    nil))
(defun GM:PropSpaceLanding (ent w up dn / *GM_PROP_GAP* roads reds step result started mode phase abort max_g)
  (setq started (getvar "MILLISECS") roads (GM:CachedRoadBoxes) reds (GM:CachedRedBoxes)
        step (GM:NumVar *GM_PROP_STEP* 10.0)
        result nil abort nil
        max_g (if (and (numberp *GM_PROP_MAX_GRIDS*) (> *GM_PROP_MAX_GRIDS* 0)) *GM_PROP_MAX_GRIDS* 0))
  (setq *GM_PROP_GRIDS_0* 0)
  ;; Fase limpia con gap 15, luego gap 10; al final se permite pisar rojos.
  ;; Nunca se retiran matriz, via, zonas, marco ni borde real.
  (foreach phase '((15.0 nil) (10.0 nil) (15.0 T) (10.0 T))
    (if (and (not result) (not abort))
      (progn
        (setq *GM_PROP_GAP* (car phase))
        (if (and (cadr phase) (= *GM_PROP_GAP* 15.0))
          (GM:AutoTrace *ML_TARGET_CODE* "PROP_ROJO_RECURSO" "INFO" "Sin posicion limpia; se permite pisar codigos rojos"))
        (foreach mode '("IZQUIERDA" "TOTAL" "SUR-RECURSO")
          (if (and (not result) (not abort))
            (progn
              (setq result (GM:PropSpaceLandingGrid ent w up dn roads reds step mode (cadr phase)))
              ;; CHUNK1-FIX1 2026-09-29: el tope se evalua POR GRID (no al final
              ;; de la fase): el caso 017 agotaba 11-12 grids de 420-1680 celdas
              ;; antes de que la revision por fase pudiera cortar y moria con AV.
              (if (and (not result) (> max_g 0) (>= *GM_PROP_GRIDS_0* max_g))
                (setq abort T)))))
        (if (and (not result) (> step 5.0) (not abort))
          (progn
            (GM:AutoTrace *ML_TARGET_CODE* "PROP_GRID_REINTENTO" "INFO" "Paso 5; mismas restricciones y vias")
            (foreach mode '("IZQUIERDA" "TOTAL" "SUR-RECURSO")
              (if (and (not result) (not abort))
                (progn
                  (setq result (GM:PropSpaceLandingGrid ent w up dn roads reds 5.0 mode (cadr phase)))
                  (if (and (not result) (> max_g 0) (>= *GM_PROP_GRIDS_0* max_g))
                    (setq abort T)))))))
        ;; CHUNK1-FIX1: traza unica del corte (solo puede activarse una vez: al
        ;; quedar abort=T las fases siguientes se saltan con el guard de arriba).
        (if abort
          (GM:AutoTrace *ML_TARGET_CODE* "PROP_AGOTADO" "AVISO"
            (strcat "0 validas en " (itoa *GM_PROP_GRIDS_0*) " grids iniciales; se corta la busqueda (fail-fast)"))))))
  (GM:AutoTrace *ML_TARGET_CODE* "PROP_TIEMPO" "INFO"
    (strcat "AU ms=" (itoa (- (getvar "MILLISECS") started))))
  result)