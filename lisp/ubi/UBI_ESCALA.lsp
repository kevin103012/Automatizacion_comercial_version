;; ============================================================================
;; UBI_ESCALA.lsp - Sistema GM Ingenieros (modulo ubi)
;; Movido SIN CAMBIOS desde PLANO_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


(defun UBI_SuggestScaleIndex (poly_ent / minpt maxpt result max_dim scales suggested vla_p)
  ;; Regla inicial basada en la mayor dimension del bounding box, en unidades del dibujo.
  ;; H6 (PA-1): "10" = 1/1000 es el default del flujo manual (indice valido de *UBI_SCALE_LIST*).
  (setq suggested "10")
  (if (and poly_ent (entget poly_ent))
    (progn
      ;; CHUNK 2026-09-29: wrapper con catch + release (antes fugaba por codigo).
      (setq vla_p (vl-catch-all-apply 'vlax-ename->vla-object (list poly_ent)))
      (if (vl-catch-all-error-p vla_p)
        (setq vla_p nil))
      (if vla_p
        (progn
          (setq result (vl-catch-all-apply 'vla-getboundingbox (list vla_p 'minpt 'maxpt)))
          (GM:ReleaseObjectSafe vla_p)
          (if (not (vl-catch-all-error-p result))
            (progn
              (setq minpt (vlax-safearray->list minpt) maxpt (vlax-safearray->list maxpt)
                    max_dim (max (- (car maxpt) (car minpt)) (- (cadr maxpt) (cadr minpt)))
                    scales '("20" "25" "50" "75" "100" "125" "200" "250" "500" "750" "1000" "1250" "2000" "2500" "5000" "7500" "10000" "12500" "20000" "25000"))
              (cond
                ((<= max_dim 20.0) (setq suggested "50"))
                ((<= max_dim 50.0) (setq suggested "250"))
                ((<= max_dim 100.0) (setq suggested "500"))
                ((<= max_dim 200.0) (setq suggested "1000"))
                ((<= max_dim 400.0) (setq suggested "2000"))
                ((<= max_dim 800.0) (setq suggested "5000"))
                ((<= max_dim 1500.0) (setq suggested "10000"))
                (T (setq suggested "25000"))
              )
              (setq suggested (itoa (vl-position suggested scales)))
              (princ (strcat "\nEscala sugerida por dimension del predio (" (rtos max_dim 2 2) "): 1/" (nth (atoi suggested) scales)))
            )
          )
        )
      )
    )
  )
  suggested
)

;; Lista unica de escalas del sistema (denominadores); indice 11 = 1/1250.
(if (not (boundp '*UBI_SCALE_LIST*))
  (setq *UBI_SCALE_LIST* '("20" "25" "50" "75" "100" "125" "200" "250" "500" "750" "1000" "1250" "2000" "2500" "5000" "7500" "10000" "12500" "20000" "25000")))
;; BW 2026-09-22: minimo de AUTOESCALA 1/1000 (solo lote automatico).
;; El selector manual conserva 1/20...1/750; el lote nunca baja de 1/1000.
(if (not (boundp '*GM_AUTO_SCALE_MIN*)) (setq *GM_AUTO_SCALE_MIN* "1000"))
(defun GM:AutoScaleMinIndex (/ i n minv)
  (setq i 0 n (length *UBI_SCALE_LIST*) minv (atof *GM_AUTO_SCALE_MIN*))
  (if (not (and (numberp minv) (> minv 0.0))) (setq minv 1000.0))
  (while (and (< i (1- n)) (< (atof (nth i *UBI_SCALE_LIST*)) minv))
    (setq i (1+ i)))
  i
)
;; Auto-escala del lote por tamano del terreno (contrato GM v1, auto por viewport):
;; need=(maxDim*1000)/(vpH*0.55); se elige el primer denominador >= need en toda
;; la lista, acotado por abajo a *GM_AUTO_SCALE_MIN* (1/1000 en lote).
;; Retorna el indice como cadena; "11" ante cualquier fallo.
(defun UBI_AutoScaleIndex (poly_ent / minpt maxpt bbox result max_dim need idx n vph mini vla_p)
  ;; H6 (PA-1): "11" = 1/1250 es el fallback historico solo si el poligono no es
  ;; valido; el flujo corta antes con POLIGONO ERROR y nunca depende de este valor.
  (setq result "11")
  (if (and poly_ent (entget poly_ent))
    (progn
      ;; CHUNK 2026-09-29: wrapper con catch + release (antes fugaba por codigo).
      (setq vla_p (vl-catch-all-apply 'vlax-ename->vla-object (list poly_ent)))
      (if (vl-catch-all-error-p vla_p)
        (setq vla_p nil))
      (if vla_p
        (progn
          (setq bbox (vl-catch-all-apply 'vla-getboundingbox (list vla_p 'minpt 'maxpt)))
          (GM:ReleaseObjectSafe vla_p)
          (if (not (vl-catch-all-error-p bbox))
            (progn
              (setq minpt (vlax-safearray->list minpt) maxpt (vlax-safearray->list maxpt)
                    max_dim (max (- (car maxpt) (car minpt)) (- (cadr maxpt) (cadr minpt))))
              (setq vph (if (and (boundp '*GM_VP_H*) (numberp *GM_VP_H*) (> *GM_VP_H* 50.0)) *GM_VP_H* 257.70882797))
              (setq need (/ (* max_dim 1000.0) (* vph 0.55)))
              (setq result (GM:SnapScaleIndex need))
              (setq mini (GM:AutoScaleMinIndex))
              (if (< (atoi result) mini) (setq result (itoa mini)))
              (princ (strcat "\nAuto-escala por terreno (" (rtos max_dim 2 2) " m): 1/" (nth (atoi result) *UBI_SCALE_LIST*)))
            )
          )
        )
      )
    )
  )
  (if (not (and (>= (atoi result) 0) (< (atoi result) (length *UBI_SCALE_LIST*))))
    (setq result "11"))
  result
)
;; Primer indice con denominador >= need, desde el inicio de la lista (contrato v1).
(defun GM:SnapScaleIndex (need / idx n)
  (setq idx 0 n (length *UBI_SCALE_LIST*))
  (while (and (< idx (1- n)) (< (atof (nth idx *UBI_SCALE_LIST*)) need))
    (setq idx (1+ idx)))
  (itoa idx)
)
;; BV 2026-09-22: ajuste combinado centro+escala para no tapar esquemas.
;; Las tres cajas replican exactamente GM:LayoutCalibrate (AUTOMATIZACION.lsp):
;; ESQUEMA_AF (187.0 27.0 257.0 94.5), ESQUEMA_LOC (16.0 147.0 96.0 214.5) y
;; ESCALA_VP (-34.26 4.0 -3.0 10.31). Si cambian alla, cambiar aqui igual.
;; Margen Paper y paso de barrido editables sin reeditar codigo.
(if (not (boundp '*GM_FIT_MARGIN*)) (setq *GM_FIT_MARGIN* 2.0))
(if (not (boundp '*GM_FIT_STEP*)) (setq *GM_FIT_STEP* 5.0))
(defun UBI_ReservedBoxesPure (x y w h kw kh / af loc esc)
  (setq af (list (+ x (* 187.0 kw)) (+ y (* 27.0 kh)) (+ x (* 257.0 kw)) (+ y (* 94.5 kh))))
  (setq loc (list (+ x (* 16.0 kw)) (+ y (* 147.0 kh)) (+ x (* 96.0 kw)) (+ y (* 214.5 kh))))
  (setq esc (list (+ x w (* -34.26 kw)) (+ y (* 4.0 kh)) (+ x w (* -3.0 kw)) (+ y (* 10.31 kh))))
  (list af loc esc)
)
(defun UBI_BoxOverlapsPure (a b margin / m a1 a2 a3 a4 b1 b2 b3 b4)
  (setq m (if (numberp margin) margin 0.0))
  (setq a1 (nth 0 a) a2 (nth 1 a) a3 (nth 2 a) a4 (nth 3 a))
  (setq b1 (nth 0 b) b2 (nth 1 b) b3 (nth 2 b) b4 (nth 3 b))
  (not (or (<= (+ a3 m) b1) (<= (+ b3 m) a1) (<= (+ a4 m) b2) (<= (+ b4 m) a2)))
)
(defun UBI_FitOffsetForScale (vx vy vw vh pw ph kw kh margin step / vcx vcy boxes r1 r2 r3 dxmin dxmax dymin dymax dx dy cands d2 cx1 cy1 cx2 cy2 ok best cand)
  (if (or (not (numberp step)) (<= step 0.0)) (setq step 5.0))
  (if (not (numberp margin)) (setq margin 2.0))
  (setq vcx (+ vx (/ vw 2.0)) vcy (+ vy (/ vh 2.0)))
  (setq boxes (UBI_ReservedBoxesPure vx vy vw vh kw kh))
  (setq r1 (nth 0 boxes) r2 (nth 1 boxes) r3 (nth 2 boxes))
  (if (or (> pw (- vw (* 2.0 margin))) (> ph (- vh (* 2.0 margin))))
    nil
    (progn
      (setq dxmin (- (+ vx margin (/ pw 2.0)) vcx) dxmax (- (- (+ vx vw) margin (/ pw 2.0)) vcx))
      (setq dymin (- (+ vy margin (/ ph 2.0)) vcy) dymax (- (- (+ vy vh) margin (/ ph 2.0)) vcy))
      (setq dx dxmin cands nil)
      (while (<= dx (+ dxmax 0.0001))
        (setq dy dymin)
        (while (<= dy (+ dymax 0.0001))
          (setq d2 (+ (* dx dx) (* dy dy)))
          (setq cands (cons (list d2 dx dy) cands))
          (setq dy (+ dy step))
        )
        (setq dx (+ dx step))
      )
      (if (and (>= 0.0 dxmin) (<= 0.0 dxmax) (>= 0.0 dymin) (<= 0.0 dymax))
        (setq cands (cons (list 0.0 0.0 0.0) cands))
      )
      (setq cands (vl-sort cands '(lambda (a b) (< (car a) (car b)))))
      (setq best nil)
      (foreach cand cands
        (if (not best)
          (progn
            (setq dx (nth 1 cand) dy (nth 2 cand))
            (setq cx1 (- (+ vcx dx) (/ pw 2.0)) cy1 (- (+ vcy dy) (/ ph 2.0)) cx2 (+ (+ vcx dx) (/ pw 2.0)) cy2 (+ (+ vcy dy) (/ ph 2.0)))
            (setq ok (not (or (UBI_BoxOverlapsPure (list cx1 cy1 cx2 cy2) r1 margin) (UBI_BoxOverlapsPure (list cx1 cy1 cx2 cy2) r2 margin) (UBI_BoxOverlapsPure (list cx1 cy1 cx2 cy2) r3 margin))))
            (if ok (setq best (list dx dy)))
          )
        )
      )
      best
    )
  )
)
(defun UBI_AutoViewportFit (poly_ent vp_ent / minpt maxpt bbox pcx pcy pwm phm ed c vw vh vx vy kw kh startidx n idx denom k pw ph off dx dy ncx ncy margin step vla_p)
  (if (not (and poly_ent (entget poly_ent) vp_ent (entget vp_ent)))
    nil
    (progn
      ;; AUD1: wrapper del poligono con catch y liberacion (antes anonimo:
      ;; fugaba 1 por cada ajuste de viewport).
      (setq vla_p (vl-catch-all-apply 'vlax-ename->vla-object (list poly_ent)))
      (if (vl-catch-all-error-p vla_p) (setq vla_p nil))
      (setq bbox (if vla_p (vl-catch-all-apply 'vla-getboundingbox (list vla_p 'minpt 'maxpt)) nil))
      (GM:ReleaseObjectSafe vla_p)
      (if (vl-catch-all-error-p bbox)
        nil
        (progn
          (setq minpt (vlax-safearray->list minpt) maxpt (vlax-safearray->list maxpt))
          (setq pcx (/ (+ (car minpt) (car maxpt)) 2.0) pcy (/ (+ (cadr minpt) (cadr maxpt)) 2.0))
          (setq pwm (- (car maxpt) (car minpt)) phm (- (cadr maxpt) (cadr minpt)))
          (setq ed (entget vp_ent) c (cdr (assoc 10 ed)) vw (cdr (assoc 40 ed)) vh (cdr (assoc 41 ed)))
          (if (not (and c (numberp vw) (numberp vh) (> vw 50.0) (> vh 50.0) (numberp pwm) (numberp phm) (> pwm 0.0) (> phm 0.0)))
            nil
            (progn
              (setq vx (- (car c) (/ vw 2.0)) vy (- (cadr c) (/ vh 2.0)))
              (setq kw (if (and (boundp '*GM_VP_REF_W*) (numberp *GM_VP_REF_W*) (> *GM_VP_REF_W* 0.0)) (/ vw *GM_VP_REF_W*) 1.0))
              (setq kh (if (and (boundp '*GM_VP_REF_H*) (numberp *GM_VP_REF_H*) (> *GM_VP_REF_H* 0.0)) (/ vh *GM_VP_REF_H*) 1.0))
              (setq margin (if (and (boundp '*GM_FIT_MARGIN*) (numberp *GM_FIT_MARGIN*) (> *GM_FIT_MARGIN* 0.0)) *GM_FIT_MARGIN* 2.0))
              (setq step (if (and (boundp '*GM_FIT_STEP*) (numberp *GM_FIT_STEP*) (> *GM_FIT_STEP* 0.0)) *GM_FIT_STEP* 5.0))
              (setq startidx (atoi (UBI_AutoScaleIndex poly_ent)) n (length *UBI_SCALE_LIST*) idx startidx off nil dx 0.0 dy 0.0)
              (if (< startidx (GM:AutoScaleMinIndex)) (setq startidx (GM:AutoScaleMinIndex)))
              (if (not (and (>= startidx 0) (< startidx n))) (setq startidx (GM:AutoScaleMinIndex)))
              (setq idx startidx)
              (while (and (< idx n) (not off))
                (setq denom (atof (nth idx *UBI_SCALE_LIST*)))
                (if (and (numberp denom) (> denom 0.0))
                  (progn
                    (setq k (/ 1000.0 denom) pw (* pwm k) ph (* phm k))
                    (setq off (UBI_FitOffsetForScale vx vy vw vh pw ph kw kh margin step))
                    (if off
                      (progn
                        (setq dx (nth 0 off) dy (nth 1 off))
                        (setq ncx (- pcx (/ dx k)) ncy (- pcy (/ dy k)))
                        (setq off (list (itoa idx) (list ncx ncy 0.0)))
                      )
                    )
                  )
                )
                (if (not off) (setq idx (1+ idx)))
              )
              (if off off "NOFIT")
            )
          )
        )
      )
    )
  )
)