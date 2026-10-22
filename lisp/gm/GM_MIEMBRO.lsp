;; ============================================================================
;; GM_MIEMBRO.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


(defun GM:MemberLbl (xa ya text)
  ;; BT: etiqueta manual (MENU_PLANOS.lsp:335-340): ROMANS 0.94 alineada izquierda.
  (GM:AutoTextC (list (+ xa 1.0) (- ya 2.2) 0.0) 0.94 text "ROMANS" "Membrete" nil nil)
)

(defun GM:MemberVal (xa xb ya yb val height / h)
  ;; BT: valor manual (_draw_val): ST-COORP-GM centrado en la celda.
  (setq h (if (numberp height) height 1.5))
  (GM:MemberCellEx xa (- (/ (+ ya yb) 2.0) 0.8) (- xb xa) val h "ST-COORP-GM" nil)
)

;; LEGACY (conservado): celda ROMANS/negro. El codigo vigente usa MemberCellEx.
(defun GM:MemberCell (x y w text height)
  (GM:MemberCellEx x y w text height "ROMANS" nil)
)

;; BS 2026-09-21: celda con estilo y color ACI opcionales (espejo del cuadro
;; manual: titulo rojo 2.0 WMF-Arial0, subtitulo 1.25 Memoir). Defaults
;; ROMANS/negro: los llamadores existentes de 5 args quedan identicos.
(defun GM:MemberCellEx (x y w text height style color / measured st)
  (setq st (if (and style (/= style "")) style "ROMANS"))
  (setq text (if text text ""))
  (while (vl-string-search "\\P" text) (setq text (vl-string-subst " " "\\P" text)))
  ;; P0-6 2026-09-29: medicion memoizada (GM:TextMeasure, en PLANO_UBICACION).
  ;; Si textbox falla devuelve nil y el texto se dibuja con su altura original.
  (setq measured (GM:TextMeasure text height st))
  (if (and measured (> measured (- w 2.0))) (setq height (* height (/ (- w 2.0) measured))))
  (if (< height 0.8)
    (GM:LayoutFail "Texto de membrete excede celda a altura legible")
    (GM:AutoTextC (list (+ x (/ w 2.0)) y 0.0) height text st "Membrete" T color))
)

;; F15 2026-09-29: parte el nombre del membrete en lineas reales. Los duenos
;; llegan separados por \P desde el CSV (multilinea tolerado por el lector);
;; cada segmento se ajusta por palabras al ancho medido con GM:TextMeasure.
;; Devuelve la lista de lineas o nil si alguna palabra no cabe sola (el
;; llamador prueba alturas menores y falla cerrado si ninguna sirve).
(defun GM:MemberNameWrap (nombre height maxw / rest pat segs seg words word pos line test out ok tw)
  (setq ok T out nil pat "\\P")
  (setq rest (if (= (type nombre) 'STR) nombre ""))
  (setq segs nil)
  (while (vl-string-search pat rest)
    (setq pos (vl-string-search pat rest)
          segs (cons (substr rest 1 pos) segs)
          rest (substr rest (+ pos (strlen pat) 1))))
  (setq segs (reverse (cons rest segs)))
  (foreach seg segs
    (setq seg (vl-string-trim " \t" seg))
    (if (/= seg "")
      (progn
        (setq words nil rest seg)
        (while (/= rest "")
          (setq pos (vl-string-search " " rest))
          (if pos
            (setq word (substr rest 1 pos) rest (vl-string-trim " " (substr rest (+ pos 1))))
            (setq word rest rest ""))
          (if (/= word "") (setq words (cons word words))))
        (setq words (reverse words) line nil)
        (foreach word words
          (setq test (if line (strcat line " " word) word)
                tw (GM:TextMeasure test height "ST-COORP-GM"))
          (if (and tw (> tw maxw) line)
            (setq out (cons line out) line word)
            (progn
              (if (and tw (> tw maxw)) (setq ok nil))
              (setq line test))))
        (if line (setq out (cons line out))))))
  (if (and ok out) (reverse out) nil))

(defun GM:GenerateMemberAuto (row / w h x y z w_izq w_mitad h_fila h_cab h_cuerpo y_base i yc yn val_L val_R old_h tb_lst labels_left labels_right name_txt name_cand name_line name_lines name_h name_step name_yc)
  ;; BT 2026-09-21: espejo del membrete manual (MENU_PLANOS.lsp:331-383).
  ;; Cabecera + DENOMINACION ancha + 6 dobles (derecha TRAMO..CENTRO <- 3 5 7 9 11 13).
  ;; Etiquetas ROMANS 0.94 Left, valores ST-COORP-GM 1.5 (casos 1.20), VB ROMANS 2.41.
  ;; Capa Membrete (recomendada; el manual hereda la activa). Sin cambios de zonas.
  (if (not (tblsearch "LAYER" "Membrete")) (command "_.-LAYER" "_M" "Membrete" ""))
  (GM:AutoEnsureStyle "ROMANS" "romans.shx")
  (GM:AutoEnsureStyle "ST-COORP-GM" "romans.shx")
  (GM:AutoEnsureStyle "WMF-Arial0" "Arial")
  (GM:AutoEnsureStyle "TREBUCHET" "Trebuchet MS")
  (setq z (cdr (assoc "MEMBRETE" *GM_AUTO_ZONES*))
        w (- (nth 2 z) (car z)) h (- (nth 3 z) (cadr z)) x (car z) y (nth 3 z))
  (setq w_izq 67.78 w_mitad 33.89 h_fila 6.89 h_cab 16.68 h_cuerpo 48.23
        y_base (- y h_cab))
  ;; BU 2026-09-21: un solo marco exterior (espejo del manual, sin marco
  ;; interior pegado al subtitulo; el interior ademas salia 1.0 por fuera abajo).
  (GM:AutoRect (list x y 0.0) (list (+ x w) (- y h) 0.0) "Membrete")
  (GM:AutoRect (list x y 0.0) (list (+ x w) y_base 0.0) "Membrete")
  (GM:MemberLbl x y "APELLIDOS Y NOMBRES / RAZON SOCIAL")
  ;; F15 2026-09-29: nombre adaptativo. Un solo renglon a 1.5 conserva el dibujo
  ;; historico (ruta MemberCellEx); un nombre con varios duenos o texto largo
  ;; pasa a bloque multilinea centrado con la mayor altura que quepa. Si ni a
  ;; 0.8 cabe, falla cerrado (no se trunca el dato en silencio).
  (setq name_txt (if (and (nth 0 row) (/= (nth 0 row) "")) (nth 0 row) "--"))
  (setq name_cand (GM:MemberNameWrap name_txt 1.5 (- w 8.0)))
  (if (and name_cand (= (length name_cand) 1))
    (GM:MemberCellEx x (+ y_base 7.0) (- w 6.0) (car name_cand) 1.5 "ST-COORP-GM" nil)
    (progn
      (setq name_lines nil name_h nil)
      (foreach name_step '(1.5 1.35 1.2 1.05 0.9 0.8)
        (if (not name_lines)
          (progn
            (setq name_cand (GM:MemberNameWrap name_txt name_step (- w 8.0)))
            (if (and name_cand (<= (* (length name_cand) (* name_step 1.35)) (- h_cab 3.2)))
              (setq name_lines name_cand name_h name_step)))))
      (if (not name_lines)
        (GM:LayoutFail "Nombre de membrete excede el espacio disponible")
        (progn
          (setq name_yc (- y (+ 3.4 (/ (* name_h 1.35) 2.0))))
          (foreach name_line name_lines
            (GM:AutoTextC (list (+ x (/ w 2.0)) name_yc 0.0) name_h name_line "ST-COORP-GM" "Membrete" T nil)
            (setq name_yc (- name_yc (* name_h 1.35))))
          T))))
  ;; Columna VB (ancho manual = w - 67.78).
  (GM:AutoRect (list (+ x w_izq) y_base 0.0) (list (+ x w) (- y_base h_cuerpo) 0.0) "Membrete")
  (GM:AutoTextC (list (+ x w_izq 1.0) (- y_base 3.7) 0.0) 2.41 "V\U+00B0B\U+00B0" "ROMANS" "Membrete" nil nil)
  ;; 7 filas: DENOMINACION ancha + 6 dobles.
  (setq labels_left '("DENOMINACI\U+00D3N" "CONDICI\U+00D3N" "PARTIDA" "SECTOR" "TIPO DE PREDIO" "PROGRESIVA INICIAL" "PROGRESIVA FINAL")
        labels_right '("" "TRAMO" "LADO" "DEPARTAMENTO" "PROVINCIA" "DISTRITO" "CENTRO URBANO/UNIDAD CATASTRAL") i 0)
  (while (< i 7)
    (setq yc (- y_base (* i h_fila)) yn (- y_base (* (1+ i) h_fila))
          val_L (nth (if (= i 0) 1 (* i 2)) row)
          val_L (if (or (not val_L) (= val_L "")) "--" val_L)
          val_R (if (> i 0) (nth (1+ (* i 2)) row) nil)
          val_R (if (or (not val_R) (= val_R "")) "--" val_R))
    (if (= i 0)
      (progn
        (GM:AutoRect (list x yc 0.0) (list (+ x w_izq) yn 0.0) "Membrete")
        (GM:MemberLbl x yc (nth i labels_left))
        (GM:MemberVal x (+ x w_izq) yc yn val_L 1.5))
      (progn
        (GM:AutoRect (list x yc 0.0) (list (+ x w_mitad) yn 0.0) "Membrete")
        (GM:AutoRect (list (+ x w_mitad) yc 0.0) (list (+ x w_izq) yn 0.0) "Membrete")
        (GM:MemberLbl x yc (nth i labels_left))
        (GM:MemberLbl (+ x w_mitad) yc (nth i labels_right))
        (setq old_h 1.5)
        (if (and (= i 1) (= (strcase val_L) "POSESIONARIO MAYOR A 10 A\U+00D1OS")) (setq old_h 1.2))
        (if (and (or (= i 5) (= i 6)) (/= val_L "--"))
          ;; P0-6 2026-09-29: medicion memoizada (devuelve ancho o nil).
          (if (setq tb_lst (GM:TextMeasure val_L 1.5 "ST-COORP-GM"))
            (if (> tb_lst (- w_mitad 2.0)) (setq old_h 1.2))))
        (GM:MemberVal x (+ x w_mitad) yc yn val_L old_h)
        (GM:MemberVal (+ x w_mitad) (+ x w_izq) yc yn val_R 1.5)))
    (setq i (1+ i)))
  (princ)
)

(defun GM:LayoutScaleText (customscale / denominator match)
  ;; Model units are metres, paper units millimetres, as in UBI_ExecuteVPUpdate.
  ;; Acepta la auto-escala elegida por terreno (lista del sistema), no solo 1/1250.
  (if (or (not (numberp customscale)) (<= customscale 0.0))
    (GM:LayoutFail "Escala del viewport no valida"))
  (setq denominator (/ 1000.0 customscale) match nil)
  (foreach s *UBI_SCALE_LIST*
    (if (<= (abs (- denominator (atof s))) 0.51) (setq match s)))
  (if (not match)
    (GM:LayoutFail "El viewport no tiene una escala valida del lote"))
  (strcat "1/" match)
)

(defun GM:GenerateLayoutFooter (row / vp vp_obj scale z x y w h date tit_style esc_style kw kh w_f1 w_f2 w_f3 h_sup h_inf hms x_foot yf xf_next tit2 num2 esc_lab midy)
  ;; El viewport se resuelve dinamicamente (igual que la calibracion), no por handle fijo.
  (setq vp (GM:AutoMainViewport))
  (if (not vp) (GM:LayoutFail "Pie no generado: no se hallo el viewport principal en el layout activo."))
  ;; F15-R1: wrapper del VP con catch y liberado tras leer CustomScale.
  (setq vp_obj (vl-catch-all-apply 'vlax-ename->vla-object (list vp)))
  (if (vl-catch-all-error-p vp_obj) (GM:LayoutFail "Pie sin viewport COM medible"))
  (setq scale (GM:LayoutScaleText (vl-catch-all-apply 'vla-get-CustomScale (list vp_obj))))
  (GM:ReleaseObjectSafe vp_obj)
  (setq z (cdr (assoc "ESCALA_VP" *GM_AUTO_ZONES*)) x (car z) y (cadr z)
        w (- (nth 2 z) x) h (- (nth 3 z) y))
  ;; Opaque paper-space background keeps model lines from crossing the small scale box.
  ;; E3: owner 410 explicito al Layout activo.
  (entmake (list '(0 . "SOLID") '(100 . "AcDbEntity") '(8 . "Membrete") (cons 410 (getvar "CTAB")) '(420 . 16777215)
    '(100 . "AcDbTrace") (cons 10 (list x y 0.0)) (cons 11 (list (+ x w) y 0.0))
    (cons 12 (list x (+ y h) 0.0)) (cons 13 (list (+ x w) (+ y h) 0.0))))
  ;; BS 2026-09-21: cuadro espejo del cuadro manual (PLANO_UBICACION.lsp:159-170):
  ;; marco ancho 0.2, titulo rojo 2.0 WMF-Arial0 con tilde, subtitulo 1.25 Memoir.
  ;; Excels sin norma de fuentes: estilos de la plantilla con fallback.
  (GM:AutoRectW (list x y 0.0) (list (+ x w) (+ y h) 0.0) "Membrete" 0.2)
  ;; Sin cond (el harness estatico solo evalua if anidados): cadena de if.
  (setq tit_style (if (tblsearch "STYLE" "WMF-Arial0") "WMF-Arial0"
                    (if (tblsearch "STYLE" "ARIAL BLACK") "ARIAL BLACK"
                      (if (tblsearch "STYLE" "Arial") "Arial" "ROMANS")))
        esc_style (if (tblsearch "STYLE" "Memoir") "Memoir" "ROMANS"))
  (GM:MemberCellEx x (+ y 4.0) w "PLANO DE UBICACI\U+00D3N" 2.0 tit_style 1)
  (GM:MemberCellEx x (+ y 1.5) w (strcat "ESCALA: " scale) 1.25 esc_style nil)
  ;; BT 2026-09-21: pie espejo del manual (MENU_PLANOS.lsp:385-415).
  ;; Anchos manuales xkw a la zona (104.2 -> w); altos proporcionales a h.
  ;; Etiquetas 0.94 ROMANS Left, valores 1.5 ST-COORP-GM, titulo 2.5 WMF-Arial0,
  ;; numero 6.0 TREBUCHET, indice 1.0 Right. ESCALA = literal CSV[15].
  (setq z (cdr (assoc "PIE_MEMBRETE" *GM_AUTO_ZONES*)) x (car z) y (cadr z)
        w (- (nth 2 z) x) h (- (nth 3 z) y)
        kw (/ w 104.2) kh (/ h 16.9274)
        w_f1 (* 23.0177 kw) w_f2 (* 24.3433 kw) w_f3 (* 36.5027 kw)
        h_sup (* 11.3319 kh) h_inf (* 5.5955 kh) hms (/ (* 11.3319 kh) 2.0)
        x_foot x yf (+ y h) xf_next (+ x_foot w_f1 w_f2)
        tit2 (if (tblsearch "STYLE" "WMF-Arial0") "WMF-Arial0"
               (if (tblsearch "STYLE" "ARIAL BLACK") "ARIAL BLACK"
                 (if (tblsearch "STYLE" "Arial") "Arial" "ROMANS")))
        num2 (if (tblsearch "STYLE" "TREBUCHET") "TREBUCHET" "ROMANS")
        esc_lab (if (or (not (nth 15 row)) (= (nth 15 row) "")) "--" (nth 15 row))
        date (if (nth 16 row) (vl-string-left-trim "' " (nth 16 row)) ""))
  ;; F6 2026-10-05: si el CSV declara una escala concreta ("1/N") distinta de
  ;; la real aplicada por el ajuste centro+escala, el pie rotula la real para
  ;; que la hoja no muestre dos escalas. Etiquetas no numericas ("INDICADA")
  ;; se conservan tal cual.
  (if (and (not (= esc_lab "--"))
           (vl-string-search "/" esc_lab)
           (not (= (strcase (vl-string-trim " " esc_lab)) (strcase scale))))
    (setq esc_lab scale))
  ;; Bloque izquierdo: PLANO: + titulo + PROYECCION: | ZONA:.
  (GM:AutoRect (list x_foot yf 0.0) (list (+ x_foot w_f1 w_f2) (- yf h_sup) 0.0) "Membrete")
  (GM:MemberLbl x_foot yf "PLANO:")
  (setq midy (/ (+ yf (- yf h_sup)) 2.0))
  (GM:MemberCellEx x_foot (- midy 1.7) (+ w_f1 w_f2) "UBICACI\U+00D3N" 2.5 tit2 nil)
  (GM:AutoRect (list x_foot (- yf h_sup) 0.0) (list (+ x_foot w_f1) (- yf h_sup h_inf) 0.0) "Membrete")
  (GM:AutoRect (list (+ x_foot w_f1) (- yf h_sup) 0.0) (list (+ x_foot w_f1 w_f2) (- yf h_sup h_inf) 0.0) "Membrete")
  (GM:MemberLbl x_foot (- yf h_sup) "PROYECCI\U+00D3N:")
  (GM:MemberVal x_foot (+ x_foot w_f1) (- yf h_sup) (- yf h_sup h_inf) (if (nth 18 row) (nth 18 row) "--") 1.5)
  (GM:MemberLbl (+ x_foot w_f1) (- yf h_sup) "ZONA:")
  (GM:MemberVal (+ x_foot w_f1) (+ x_foot w_f1 w_f2) (- yf h_sup) (- yf h_sup h_inf) (if (nth 17 row) (nth 17 row) "--") 1.5)
  ;; Bloque medio: CODIGO: / ESCALA: (literal CSV) / FECHA:.
  (GM:AutoRect (list xf_next yf 0.0) (list (+ xf_next w_f3) (- yf hms) 0.0) "Membrete")
  (GM:MemberLbl xf_next yf "C\U+00D3DIGO:")
  (GM:MemberVal xf_next (+ xf_next w_f3) yf (- yf hms) (if (nth 14 row) (nth 14 row) "--") 1.5)
  (GM:AutoRect (list xf_next (- yf hms) 0.0) (list (+ xf_next w_f3) (- yf h_sup) 0.0) "Membrete")
  (GM:MemberLbl xf_next (- yf hms) "ESCALA:")
  (GM:MemberVal xf_next (+ xf_next w_f3) (- yf hms) (- yf h_sup) esc_lab 1.5)
  (GM:AutoRect (list xf_next (- yf h_sup) 0.0) (list (+ xf_next w_f3) (- yf h_sup h_inf) 0.0) "Membrete")
  (GM:MemberLbl xf_next (- yf h_sup) "FECHA:")
  (GM:MemberVal xf_next (+ xf_next w_f3) (- yf h_sup) (- yf h_sup h_inf) (if (= date "") "--" date) 1.5)
  ;; Bloque derecho: N PLANO + 01 + 01 / 04 fijo (plano ubicacion).
  (GM:AutoRect (list (+ xf_next w_f3) yf 0.0) (list (+ x w) (- yf h_sup h_inf) 0.0) "Membrete")
  (GM:MemberLbl (+ xf_next w_f3) yf "N\U+00B0 PLANO")
  (setq midy (/ (+ yf (- yf h_sup h_inf)) 2.0))
  (GM:MemberCellEx (+ xf_next w_f3) (- midy 2.1) (- (+ x w) (+ xf_next w_f3)) "01" 6.0 num2 nil)
  (GM:AutoTextJ (list (- (+ x w) 1.5) (+ (- yf h_sup h_inf) 1.5) 0.0) 1.0 "01 / 04" "ROMANS" "Membrete" nil "R")
  (GM:AutoTrace *ML_TARGET_CODE* "ESCALA_Y_PIE" "OK" (strcat "Escala real " scale "; rotulo pie " esc_lab "; hoja 01/04; fecha/zona/datum del CSV"))
)