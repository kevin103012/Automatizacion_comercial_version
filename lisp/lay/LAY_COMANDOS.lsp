;; ============================================================================
;; LAY_COMANDOS.lsp - Sistema GM Ingenieros (modulo lay)
;; Movido SIN CAMBIOS desde LAYOUT_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


;; ==============================================================================
;; FUNCIONES N\\U+00DACLEO (HERRAMIENTAS LAYOUT - C\\U+00C1LCULO AL VUELO)
;; ==============================================================================
(defun EXEC_ETIQUETA_AFEC_AL_VUELO ( / target_prefix ss temp_dict i ent hl suf cur_list ents e vla_e tmp_a tmp_p pf_data a_m2 p_m2 chosen_data dcl_file f dcl_id sel_idx res a_ha title_text cod capa est_sup est_inf h_sup h_inf old_dimzin str_ha str_m2 str_p p1 p2 dir offset_x p3 dim_style mid_x old_layer)
  (if (or (= *ML_TARGET_CODE* "") (not *ML_TARGET_CODE*))
    (progn (alert "?? NO HAY C\\U+00D3DIGO OBJETIVO DEFINIDO. Selecci\\U+00F3nalo en el panel principal primero.") (exit))
  )
  
  (setq target_prefix (strcat (strcase *ML_TARGET_CODE*) "-PF-"))
  (setq ss (ssget "X" (list '(0 . "LWPOLYLINE,POLYLINE") '(410 . "Model"))))
  (setq pf_data '() temp_dict '())
  
  (if ss
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i))))
      ;; FIX-AV: wrapper nominal liberado por iteracion.
      (setq vla_e (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p vla_e) (setq vla_e nil))
      (setq hl (if vla_e (ML_GetHyperlinkString vla_e) nil))
      (GM:ReleaseObjectSafe vla_e)
      (setq vla_e nil)
      (if (and hl (str-starts-with (strcase hl) target_prefix))
        (progn
          (setq suf (substr (strcase hl) (1+ (strlen target_prefix))))
          (setq cur_list (cdr (assoc suf temp_dict)))
          (setq temp_dict (vl-remove (assoc suf temp_dict) temp_dict))
          (setq temp_dict (cons (cons suf (cons ent cur_list)) temp_dict))
        )
      )
    )
  )
  
  (foreach item temp_dict
    (setq suf (car item) ents (cdr item) a_m2 0.0 p_m2 0.0)
    (foreach e ents
      ;; AUD1: creacion con catch; entidad caida = se salta.
      (setq vla_e (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
      (if (vl-catch-all-error-p vla_e) (setq vla_e nil))
      (setq tmp_a (if vla_e (vl-catch-all-apply 'vla-get-Area (list vla_e)) nil))
      (if (not (vl-catch-all-error-p tmp_a)) (setq a_m2 (+ a_m2 (if (numberp tmp_a) tmp_a 0.0))))
      (setq tmp_p (if vla_e (vl-catch-all-apply 'vla-get-Length (list vla_e)) nil))
      (if (not (vl-catch-all-error-p tmp_p)) (setq p_m2 (+ p_m2 (if (numberp tmp_p) tmp_p 0.0))))
      (GM:ReleaseObjectSafe vla_e)
    )
    (setq pf_data (cons (list suf a_m2 p_m2) pf_data))
  )
  
  (if (not pf_data)
    (progn (alert (strcat "?? No se encontraron pol\\U+00EDgonos de afectaci\\U+00F3n con el hiperv\\U+00EDnculo: " target_prefix "* en el Model Space.")) (exit))
  )
  
  (setq pf_data (vl-sort pf_data '(lambda (a b) (< (atoi (car a)) (atoi (car b))))))
  
  (setq chosen_data nil)
   (if (and (> (length pf_data) 1) (not *GM_AUTO_MODE*))
    (progn
      (setq dcl_file (vl-filename-mktemp "pf_sel.dcl") f (open dcl_file "w"))
      (write-line "pfsel : dialog { label=\"M\\U+00FAltiples \\U+00E1reas Afectadas\";" f)
      (write-line "  : text { label=\"Seleccione a qu\\U+00E9 \\U+00E1rea PF desea generarle la etiqueta:\"; }" f)
      (write-line "  : popup_list { key=\"cbx_pf\"; label=\"Pol\\U+00EDgono PF:\"; }" f)
      (write-line "  ok_cancel; }" f)
      (close f)
      
      (setq dcl_id (load_dialog dcl_file))
      (new_dialog "pfsel" dcl_id)
      (start_list "cbx_pf")
      (foreach p pf_data (add_list (strcat "PF-" (car p) "  (\\U+00E1rea: " (rtos (cadr p) 2 2) " m\\U+00B2)")))
      (end_list)
      (setq sel_idx 0)
      (action_tile "cbx_pf" "(setq sel_idx (atoi $value))")
      (setq res (start_dialog))
      (unload_dialog dcl_id)
      (vl-file-delete dcl_file)
      
      (if (= res 1) (setq chosen_data (nth sel_idx pf_data)) (exit))
    )
    (setq chosen_data (car pf_data))
  )
  
  (setq suf (car chosen_data) a_m2 (cadr chosen_data) p_m2 (caddr chosen_data) a_ha (/ a_m2 10000.0))
  (setq title_text (if (> (length pf_data) 1) (strcat "\\U+00C1REA AFECTADA " (if (< (atoi suf) 10) "0" "") (itoa (atoi suf))) "\\U+00C1REA AFECTADA"))
  
  (setq cod (if (and *GL_DATOS_CSV* (nth 14 *GL_DATOS_CSV*)) (nth 14 *GL_DATOS_CSV*) *ML_TARGET_CODE*))
  (setq capa "Membrete" est_sup "ARIAL BLACK" est_inf "VERTICES" h_sup 3.0 h_inf 1.8)

  (if (not (tblsearch "STYLE" est_sup)) (command "_-STYLE" est_sup "ariblk.ttf" "0" "1" "0" "_N" "_N"))
  (if (not (tblsearch "STYLE" est_inf)) (command "_-STYLE" est_inf "arial.ttf" "0" "1" "0" "_N" "_N"))
  (setq old_layer (getvar "CLAYER"))
  (if (not (tblsearch "LAYER" capa)) (command "_-LAYER" "_M" capa "_C" "7" "" "") (setvar "CLAYER" capa))

  (setq old_dimzin (getvar "DIMZIN")) (setvar "DIMZIN" 0)
  (setq str_ha (if a_ha (rtos a_ha 2 4) "0.0000") str_m2 (if a_m2 (rtos a_m2 2 2) "0.00") str_p (if p_m2 (rtos p_m2 2 2) "0.00"))
  (setvar "DIMZIN" old_dimzin)

       (setq p1 (GM:GetLayoutPoint (strcat "\nSeleccione punto para la flecha de la Etiqueta [" title_text "] o ENTER para cancelar: ")))
  (if p1
    (progn
      (setq p2 (PREVIEW_LEADER_BOX p1 35.0 10.0 10.0 (strcat "\nMueva el cursor para ubicar la Etiqueta de [" title_text "] y haga CLIC: ") nil))
      (if p2
        (progn
          (if (>= (car p2) (car p1)) (setq dir 1.0) (setq dir -1.0))
          (setq offset_x (* 35.0 dir) p3 (list (+ (car p2) offset_x) (cadr p2) 0.0))

          (setq dim_style (getvar "DIMSTYLE"))
          (entmake (list '(0 . "LEADER") '(100 . "AcDbEntity") (cons 8 capa) '(100 . "AcDbLeader") (cons 3 dim_style) '(71 . 1) '(72 . 0) '(73 . 3) '(74 . 0) '(75 . 0) '(76 . 3) (cons 10 p1) (cons 10 p2) (cons 10 p3)))
          
          (setq mid_x (+ (car p2) (* 17.5 dir)))

          (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (+ (cadr p2) 6.5) 0)) (cons 40 h_sup) (cons 1 title_text) (cons 7 est_sup) '(72 . 1) (cons 11 (list mid_x (+ (cadr p2) 6.5) 0)) '(73 . 2) (cons 8 capa)))
          (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (+ (cadr p2) 2.5) 0)) (cons 40 h_sup) (cons 1 cod) (cons 7 est_sup) '(72 . 1) (cons 11 (list mid_x (+ (cadr p2) 2.5) 0)) '(73 . 2) (cons 8 capa)))

          (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (- (cadr p2) 2.5) 0)) (cons 40 h_inf) (cons 1 (strcat "\\U+00C1REA = " str_ha " ha.")) (cons 7 est_inf) '(72 . 1) (cons 11 (list mid_x (- (cadr p2) 2.5) 0)) '(73 . 2) (cons 8 capa)))
          (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (- (cadr p2) 5.3) 0)) (cons 40 h_inf) (cons 1 (strcat "\\U+00C1REA = " str_m2 " m\\U+00B2.")) (cons 7 est_inf) '(72 . 1) (cons 11 (list mid_x (- (cadr p2) 5.3) 0)) '(73 . 2) (cons 8 capa)))
          (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (- (cadr p2) 8.1) 0)) (cons 40 h_inf) (cons 1 (strcat "PER\\U+00CDMETRO = " str_p " m.")) (cons 7 est_inf) '(72 . 1) (cons 11 (list mid_x (- (cadr p2) 8.1) 0)) '(73 . 2) (cons 8 capa)))
          
          (princ (strcat "\n> Etiqueta [" title_text "] generada con \\U+00E9xito."))
        )
      )
    )
  )
  ;; F15-R2: CLAYER vuelve a su estado previo (antes quedaba en Membrete).
  (setvar "CLAYER" old_layer)
)

(defun EXEC_ETIQUETA_AFEC ( / p1 p2 p3 mid_x h_sup h_inf capa est_sup est_inf dir offset_x dim_style cod a_m2 a_ha p str_ha str_m2 str_p old_dimzin idx num_afectaciones title_text suf data old_layer)
  ;; E: CLAYER se captura siempre; si el dato falta, no hay setvar con nil.
  (setq old_layer (getvar "CLAYER"))
  (if (not *GL_DATA_AFECTACIONES*)
    (alert "?? Primero debes generar los Esquemas PU para extraer los datos de \\U+00E1rea y Per\\U+00EDmetro en memoria.")
    (progn
      (setq cod (if (and *GL_DATOS_CSV* (nth 14 *GL_DATOS_CSV*)) (nth 14 *GL_DATOS_CSV*) "C\\U+00D3DIGO"))
      (setq capa "Membrete" est_sup "ARIAL BLACK" est_inf "VERTICES" h_sup 3.0 h_inf 1.8)

      (if (not (tblsearch "STYLE" est_sup)) (command "_-STYLE" est_sup "ariblk.ttf" "0" "1" "0" "_N" "_N"))
      (if (not (tblsearch "STYLE" est_inf)) (command "_-STYLE" est_inf "arial.ttf" "0" "1" "0" "_N" "_N"))
      (if (not (tblsearch "LAYER" capa)) (command "_-LAYER" "_M" capa "_C" "7" "" "") (setvar "CLAYER" capa))

      (setq idx 1)
      (setq num_afectaciones (length *GL_DATA_AFECTACIONES*))
      
      (foreach data *GL_DATA_AFECTACIONES*
        ;; E8: accesores compatibles 3/5 elementos; titulo con sufijo real cuando exista.
        (setq a_m2 (GM:AF_AreaM2 data) a_ha (GM:AF_AreaHa data) p (GM:AF_Perim data))

        (setq old_dimzin (getvar "DIMZIN")) (setvar "DIMZIN" 0)
        (setq str_ha (if a_ha (rtos a_ha 2 4) "0.0000") str_m2 (if a_m2 (rtos a_m2 2 2) "0.00") str_p  (if p (rtos p 2 2) "0.00"))
        (setvar "DIMZIN" old_dimzin)

        (setq suf (GM:AF_Suffix data))
        (if (and suf (/= suf ""))
          (setq title_text (strcat "\\U+00C1REA AFECTADA " suf))
          (if (> num_afectaciones 1) (setq title_text (strcat "\\U+00C1REA AFECTADA " (if (< idx 10) "0" "") (itoa idx))) (setq title_text "\\U+00C1REA AFECTADA")))

         (setq p1 (GM:GetLayoutPoint (strcat "\nSeleccione punto para la flecha [" title_text "] o ENTER para saltar: ")))
        (if p1
          (progn
            (setq p2 (PREVIEW_LEADER_BOX p1 35.0 10.0 10.0 (strcat "\nMueva el cursor para previsualizar [" title_text "] y haga CLIC para ubicarlo: ") nil))
            (if p2
              (progn
                (if (>= (car p2) (car p1)) (setq dir 1.0) (setq dir -1.0))
                (setq offset_x (* 35.0 dir) p3 (list (+ (car p2) offset_x) (cadr p2) 0.0))

                (setq dim_style (getvar "DIMSTYLE"))
                (entmake (list '(0 . "LEADER") '(100 . "AcDbEntity") (cons 8 capa) '(100 . "AcDbLeader") (cons 3 dim_style) '(71 . 1) '(72 . 0) '(73 . 3) '(74 . 0) '(75 . 0) '(76 . 3) (cons 10 p1) (cons 10 p2) (cons 10 p3)))
                
                (setq mid_x (+ (car p2) (* 17.5 dir)))

                (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (+ (cadr p2) 6.5) 0)) (cons 40 h_sup) (cons 1 title_text) (cons 7 est_sup) '(72 . 1) (cons 11 (list mid_x (+ (cadr p2) 6.5) 0)) '(73 . 2) (cons 8 capa)))
                (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (+ (cadr p2) 2.5) 0)) (cons 40 h_sup) (cons 1 cod) (cons 7 est_sup) '(72 . 1) (cons 11 (list mid_x (+ (cadr p2) 2.5) 0)) '(73 . 2) (cons 8 capa)))

                (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (- (cadr p2) 2.5) 0)) (cons 40 h_inf) (cons 1 (strcat "\\U+00C1REA = " str_ha " ha.")) (cons 7 est_inf) '(72 . 1) (cons 11 (list mid_x (- (cadr p2) 2.5) 0)) '(73 . 2) (cons 8 capa)))
                (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (- (cadr p2) 5.3) 0)) (cons 40 h_inf) (cons 1 (strcat "\\U+00C1REA = " str_m2 " m\\U+00B2.")) (cons 7 est_inf) '(72 . 1) (cons 11 (list mid_x (- (cadr p2) 5.3) 0)) '(73 . 2) (cons 8 capa)))
                (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (- (cadr p2) 8.1) 0)) (cons 40 h_inf) (cons 1 (strcat "PER\\U+00CDMETRO = " str_p " m.")) (cons 7 est_inf) '(72 . 1) (cons 11 (list mid_x (- (cadr p2) 8.1) 0)) '(73 . 2) (cons 8 capa)))
                
                (princ (strcat "\n> Etiqueta [" title_text "] generada con \\U+00E9xito."))
              )
              (princ (strcat "\n> Omitido [" title_text "]."))
            )
          )
          (princ (strcat "\n> Omitido [" title_text "]."))
        )
        (setq idx (1+ idx))
      )
      (princ "\n> Proceso de Etiquetas de Afectaci\\U+00F3n finalizado.")
    )
  )
  ;; E: CLAYER vuelve a su estado previo solo si hubo captura valida.
  (if old_layer (setvar "CLAYER" old_layer))
)

(defun EXEC_ETIQUETA_PROP ( / p1 p2 p3 mid_x h_texto capa est_norm est_neg dir_x dist_linea old_dimasz nombre_full cond_jur_bruta cond_jur_fin name_lines max_name_w w_cond max_w y_cond tb w)
  (if (not *GL_DATOS_CSV*)
    (alert "?? No hay datos cargados en memoria. Aseg\\U+00FArate de asignar un CSV y un C\\U+00F3digo v\\U+00E1lidos en la ventana principal.")
    (progn
      (setq nombre_full (if (and *GL_DATOS_CSV* (nth 0 *GL_DATOS_CSV*)) (nth 0 *GL_DATOS_CSV*) "SIN NOMBRE"))
      
      (setq cond_jur_bruta (strcase (if (and *GL_DATOS_CSV* (nth 2 *GL_DATOS_CSV*)) (nth 2 *GL_DATOS_CSV*) "PROPIETARIO")))
      (cond 
        ((vl-string-search "PROPIETARIO ESTATAL" cond_jur_bruta) (setq cond_jur_fin "PROPIETARIO"))
        ((vl-string-search "PROPIETARIO" cond_jur_bruta) (setq cond_jur_fin "PROPIETARIO"))
        ((vl-string-search "POSESIONARIO" cond_jur_bruta) (setq cond_jur_fin "POSESIONARIO MAYOR A 10 A\\U+00D1OS"))
        ((vl-string-search "OCUPANTE" cond_jur_bruta) (setq cond_jur_fin "OCUPANTE"))
        (T (setq cond_jur_fin cond_jur_bruta))
      )

      (setq capa "0" est_norm "ROMANS" est_neg "NEGRITA" h_texto 1.8 dist_linea 60.0)

      (if (not (tblsearch "STYLE" est_norm)) (command "_-STYLE" est_norm "romans.shx" "0" "1" "0" "_N" "_N"))
      (if (not (tblsearch "STYLE" est_neg)) (command "_-STYLE" est_neg "arialbd.ttf" "0" "1" "0" "_N" "_N"))

      (defun get-text-width (txt hyt style / tb)
        (if (and txt (/= txt ""))
          (progn (setq tb (textbox (list '(0 . "TEXT") (cons 1 txt) (cons 40 hyt) (cons 7 style)))) (if tb (- (car (cadr tb)) (car (car tb))) 0.0))
          0.0
        )
      )

      (setq name_lines (_str-split nombre_full "\\P"))
      (setq max_name_w 0.0)
      (foreach nl name_lines
        (setq w (get-text-width nl h_texto est_norm))
        (if (> w max_name_w) (setq max_name_w w))
      )
      
      (setq w_cond (get-text-width cond_jur_fin h_texto est_neg) max_w (max max_name_w w_cond))

      (if (> max_w (- dist_linea 4.0)) (setq dist_linea (+ max_w 8.0)))

      (setq y_cond -2.5)

       (setq p1 (GM:GetLayoutPoint "\nSeleccione punto para la punta de la flecha (Propietario) o ENTER para cancelar: "))
      (if p1
        (progn
          (setq p2 (PREVIEW_LEADER_BOX p1 dist_linea 7.0 3.0 "\nMueva el cursor para previsualizar el quiebre y haga CLIC para ubicarlo: " nil))
          (if p2
            (progn
              (if (>= (car p2) (car p1)) (setq dir_x 1.0) (setq dir_x -1.0))
              (setq p3 (list (+ (car p2) (* dist_linea dir_x)) (cadr p2) 0.0))

              (setq old_dimasz (getvar "DIMASZ")) (setvar "DIMASZ" 2.5)
              (command "_.LEADER" p1 p2 p3 "" "" "_N")
              (setvar "DIMASZ" old_dimasz)

              (setq mid_x (+ (car p2) (* (/ dist_linea 2.0) dir_x)))

              (entmake 
                (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                      (cons 10 (list mid_x (+ (cadr p2) 1.0) 0)) 
                      (cons 40 h_texto) 
                      (cons 41 0.0) 
                      (cons 71 8) 
                      (cons 7 est_norm) 
                      (cons 1 nombre_full) 
                      (cons 8 capa))
              )

              (entmake (list '(0 . "TEXT") (cons 10 (list mid_x (+ (cadr p2) y_cond) 0)) (cons 40 h_texto) (cons 1 cond_jur_fin) (cons 7 est_neg) '(72 . 1) (cons 11 (list mid_x (+ (cadr p2) y_cond) 0)) '(73 . 2) (cons 8 capa)))

              (princ "\n> Etiqueta de Condici\\U+00F3n Jur\\U+00EDdica generada con \\U+00E9xito.")
            )
          )
        )
      )
    )
  )
)

(defun EXEC_ETIQUETAS_VIA (/ cant_der cant_eje margen capa estilo h_texto i p1 p2 pmid p3 ang_line ang_text ang_click dir offset_pnt old_layer)
  (setq old_layer (getvar "CLAYER")) 
  (setq capa "0" estilo "ROMANS" h_texto 1.8 margen 3.0)
  
  (if (not (tblsearch "STYLE" estilo)) (command "_-STYLE" estilo "romans.shx" "0" "1.2" "0" "_N" "_N"))
  (setvar "CLAYER" capa) 
  
   (setq cant_der (if *GM_AUTO_MODE* (LUBI_CountLinkedEntities "DV") (getint "\nCANTIDAD DE ETIQUETAS PARA DERECHO DE VIA <2>: ")))
  (if (not cant_der) (setq cant_der 2)) 
  
   (setq cant_eje (if *GM_AUTO_MODE* (LUBI_CountLinkedEntities "EJE") (getint "\nCANTIDAD DE ETIQUETAS PARA EJE DE VIA <1>: ")))
  (if (not cant_eje) (setq cant_eje 1)) 

  (defun insertar_etiqueta (texto / p1 p2 pmid p3 ang_line ang_text ang_click dir offset_pnt)
     (setq p1 (GM:GetLayoutPoint (strcat "\n[" texto "] Seleccione PRIMER punto sobre la l\\U+00EDnea (o ENTER para saltar): ")))
    (if p1
      (progn
         (setq p2 (if *GM_AUTO_MODE* nil (getpoint p1 (strcat "\n[" texto "] Seleccione SEGUNDO punto sobre la l\\U+00EDnea: "))))
        (if p2
          (progn
            (setq pmid (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
            (setq ang_line (angle p1 p2))
            
            (setq ang_text ang_line)
            (if (and (> ang_text (/ pi 2.0)) (<= ang_text (* 1.5 pi)))
              (setq ang_text (+ ang_text pi))
            )
            
            (setq p3 (PREVIEW_OFFSET_TEXT pmid ang_line p1 p2 margen (strcat "\nMueva el mouse hacia qu\\U+00E9 LADO colocar [" texto "] y haga CLIC: ")))
            (if p3
              (progn
                (setq ang_click (angle pmid p3))
                (setq dir (sin (- ang_click ang_line)))
                
                (if (>= dir 0)
                  (setq offset_pnt (polar pmid (+ ang_line (/ pi 2.0)) margen))
                  (setq offset_pnt (polar pmid (- ang_line (/ pi 2.0)) margen))
                )
                
                (entmake
                  (list '(0 . "TEXT") (cons 10 offset_pnt) (cons 40 h_texto)  (cons 1 texto) (cons 7 estilo) (cons 8 capa) (cons 62 0) (cons 41 1.2) (cons 50 ang_text) '(72 . 1) (cons 11 offset_pnt) '(73 . 2))
                )
              )
              (princ "\nOmitido.")
            )
          )
        )
      )
    )
  )

  (setq i 0)
  (while (< i cant_der) (insertar_etiqueta "DERECHO DE V\\U+00CDA") (setq i (1+ i)))
  (setq i 0)
  (while (< i cant_eje) (insertar_etiqueta "EJE DE V\\U+00CDA") (setq i (1+ i)))

  (setvar "CLAYER" old_layer)
  (princ "\n> Etiquetas de V\\U+00CDa finalizadas.")
)

(defun EXEC_UBICACION (/ p1 p2 p_edge p3 dir offset_x mid_x h_texto capa estilo rad_cir ang ent_cir old_osmode old_layer)
  (setq capa "UBI" estilo "Arial" h_texto 1.6525 rad_cir 1.75 offset_x 18.0)
  
  (if (not (tblsearch "STYLE" estilo)) (command "_-STYLE" estilo "arial.ttf" "0" "1" "0" "_N" "_N"))
  (setq old_layer (getvar "CLAYER"))
  (if (not (tblsearch "LAYER" capa)) (command "_-LAYER" "_M" capa "_C" "7" "" "") (setvar "CLAYER" capa))
  
   (setq p1 (if *GM_AUTO_MODE* nil (getpoint "\nSeleccione el punto central del C\\U+00CDRCULO de ubicaci\\U+00F3n (ENTER = centro autom\\U+00E1tico): ")))
  (if (not p1)
    (setq p1 (if *GM_AUTO_MODE* (GM:LayoutOmit "UBICADOR") (LUBI_GetTargetPolygonCenter)))
  )
  (if (not p1)
     (setq p1 (if *GM_AUTO_MODE* nil (getpoint "\nNo se pudo localizar el predio. Seleccione el punto central manualmente (o ENTER para cancelar): ")))
  )
  (if p1
    (progn
      (setq p2 (PREVIEW_LEADER_BOX p1 18.0 4.0 4.0 "\nMueva el cursor para previsualizar el quiebre y haga CLIC: " nil))
      (if p2
        (progn
          (setq old_osmode (getvar "OSMODE"))
          (setvar "OSMODE" 0)

          (entmake (list '(0 . "CIRCLE") (cons 8 capa) (cons 10 p1) (cons 40 rad_cir)))
          (setq ent_cir (entlast))

          (command "_.-HATCH" "_P" "ANSI31" 0.25 0 "_S" ent_cir "" "")

          (setq ang (angle p1 p2))
          (setq p_edge (polar p1 ang rad_cir)) 

          (if (>= (car p2) (car p1)) (setq dir 1.0) (setq dir -1.0))
          (setq p3 (list (+ (car p2) (* offset_x dir)) (cadr p2) 0.0))

          (command "_.LEADER" p_edge p2 p3 "" "" "_N")

          (setq mid_x (+ (car p2) (* (/ offset_x 2.0) dir)))
          (entmake
            (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 capa) '(100 . "AcDbMText") (cons 10 (list mid_x (+ (cadr p2) 0.5) 0.0)) (cons 40 h_texto) '(71 . 8) (cons 1 "UBICACI\\U+00D3N\\PDEL PREDIO\\PAFECTADO") (cons 7 estilo))
          )

          (setvar "OSMODE" old_osmode)
          (princ "\n> Ubicaci\\U+00F3n generada con \\U+00E9xito.")
        )
      )
    )
  )
  ;; F15-R2: CLAYER vuelve a su estado previo.
  (setvar "CLAYER" old_layer)
)

(defun LUBI_GetTargetPolygonCenter (/ ss i ent obj hl target minpt maxpt result vobj)
  ;; El centro automatico se basa en el poligono matriz del codigo activo.
  (setq target (if (and *ML_TARGET_CODE* (/= *ML_TARGET_CODE* "")) (strcase *ML_TARGET_CODE*) ""))
  (if (/= target "")
    (progn
      (setq ss (ssget "X" (list (cons 0 "LWPOLYLINE,POLYLINE") (cons 410 "Model"))))
      (if ss
        (progn
          (setq i 0)
          (while (and (< i (sslength ss)) (not ent))
            ;; FIX-AV: un solo wrapper por iteracion, liberado antes de seguir.
            (setq obj (ssname ss i) vobj (vl-catch-all-apply 'vlax-ename->vla-object (list obj)))
            (if (vl-catch-all-error-p vobj)
              (setq vobj nil)
              (setq hl (ML_GetHyperlinkString vobj)))
            (GM:ReleaseObjectSafe vobj)
            (setq vobj nil)
            (if (and hl (= (strcase (vl-string-trim " \t\n\r" hl)) target)) (setq ent obj))
            (setq i (1+ i))
          )
          (setq ss nil) (gc)
        )
      )
    )
  )
  (if ent
    (progn
      ;; OPT1/BZ-OPT1: el wrapper del bounding box se suelta tras medir (una sola entidad).
      ;; AUD1: creacion con catch; entidad caida = result nil controlado.
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
      (if (vl-catch-all-error-p obj) (setq obj nil))
      (setq result (if obj (vl-catch-all-apply 'vla-getboundingbox (list obj 'minpt 'maxpt)) nil))
      (GM:ReleaseObjectSafe obj)
      (setq obj nil)
      (if (not (vl-catch-all-error-p result))
        (progn
          (setq minpt (vlax-safearray->list minpt) maxpt (vlax-safearray->list maxpt))
          (list (/ (+ (car minpt) (car maxpt)) 2.0) (/ (+ (cadr minpt) (cadr maxpt)) 2.0) 0.0)
        )
      )
    )
  )
)

(defun EXEC_UBICACION_GEOGRAFICA (/ pt x curr_y max_w dep_val prov_val dist_val dep_lines prov_lines dist_lines crear-txt get-text-width wrap-text line)
  (if (not *GL_DATOS_CSV*)
    (alert "?? No hay datos del CSV cargados. Se usar\\U+00E1n valores por defecto de demostraci\\U+00F3n.")
  )
  
  (setq dep_val  (strcase (if (and *GL_DATOS_CSV* (> (length *GL_DATOS_CSV*) 7)) (nth 7 *GL_DATOS_CSV*) "AYACUCHO")))
  (setq prov_val (strcase (if (and *GL_DATOS_CSV* (> (length *GL_DATOS_CSV*) 9)) (nth 9 *GL_DATOS_CSV*) "S\\U+00C1NCHEZ CARRI\\U+00D3N")))
  (setq dist_val (strcase (if (and *GL_DATOS_CSV* (> (length *GL_DATOS_CSV*) 11)) (nth 11 *GL_DATOS_CSV*) "ANDRES AVELINO CACERES DORREGARAY")))

  (if (not (tblsearch "LAYER" "0. DEP. PROV. DIST."))
    (entmake '((0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord") (2 . "0. DEP. PROV. DIST.") (70 . 0)))
  )

  (if (not (tblsearch "STYLE" "UBICACION"))
    (entmake '((0 . "STYLE") (100 . "AcDbSymbolTableRecord") (100 . "AcDbTextStyleTableRecord") (2 . "UBICACION") (70 . 0) (40 . 0.0) (3 . "Dutch801 XBd BT.ttf")))
  )
  (if (not (tblsearch "STYLE" "DISTRITO"))
    (entmake '((0 . "STYLE") (100 . "AcDbSymbolTableRecord") (100 . "AcDbTextStyleTableRecord") (2 . "DISTRITO") (70 . 0) (40 . 0.0) (3 . "arialuni.ttf")))
  )

  (defun get-text-width (txt hyt style / tb)
    (if (and txt (/= txt ""))
      (progn 
        (setq tb (vl-catch-all-apply 'textbox (list (list '(0 . "TEXT") (cons 1 txt) (cons 40 hyt) (cons 7 style)))))
        (if (and (not (vl-catch-all-error-p tb)) tb) (- (car (cadr tb)) (car (car tb))) (* (strlen txt) (* hyt 0.8)))
      )
      0.0
    )
  )

  ;; POR QUE defun anidado: en AutoLISP queda global tras ejecutar la rutina
  ;; contenedora; se conserva el nombre por compatibilidad manual.
  (defun wrap-text (str max_width hyt style / words res current_line test_line w)
    (setq words (vl-remove "" (_str-split str " ")))
    (setq res nil current_line "")
    (foreach w words
      (if (= current_line "")
        (setq current_line w)
        (progn
          (setq test_line (strcat current_line " " w))
          (if (<= (get-text-width test_line hyt style) max_width)
            (setq current_line test_line)
            (progn (setq res (append res (list current_line))) (setq current_line w))
          )
        )
      )
    )
    (if (/= current_line "") (setq res (append res (list current_line))))
    (if (not res) (setq res (list str)))
    res
  )

   (setq pt (if *GM_AUTO_MODE*
              (GM:LayoutOmit "GEOGRAFIA_CONSERVADA_EN_BASE")
              (GM:GetLayoutPoint "\nHaz clic en el punto de inserci\\U+00F3n (centro superior para DEPARTAMENTO): ")))
  
  (if pt
    (progn
      (setq x (car pt))
      (setq curr_y (cadr pt))
      
      (setq max_w (get-text-width "DEPARTAMENTO" 2.0 "UBICACION"))
      (if (or (not max_w) (<= max_w 0.0)) (setq max_w 20.0)) 

      (setq dep_lines  (wrap-text dep_val  max_w 2.0 "UBICACION"))
      (setq prov_lines (wrap-text prov_val max_w 1.5 "UBICACION"))
      (setq dist_lines (wrap-text dist_val max_w 1.5 "DISTRITO"))

      (defun crear-txt (texto punto altura color estilo)
        (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") '(8 . "0. DEP. PROV. DIST.") (cons 62 color) (cons 10 punto) (cons 40 altura) (cons 1 texto) (cons 7 estilo) '(72 . 4) (cons 11 punto)))
      )
      
      (crear-txt "DEPARTAMENTO" (list x curr_y (caddr pt)) 2.0 10 "UBICACION")
      (foreach line dep_lines
        (setq curr_y (- curr_y 2.5))
        (crear-txt line (list x curr_y (caddr pt)) 2.0 10 "UBICACION")
      )
      
      (setq curr_y (- curr_y 3.0))
      (crear-txt "PROVINCIA" (list x curr_y (caddr pt)) 1.5 250 "UBICACION")
      (foreach line prov_lines
        (setq curr_y (- curr_y 2.0))
        (crear-txt line (list x curr_y (caddr pt)) 1.5 250 "UBICACION")
      )

      (setq curr_y (- curr_y 2.5))
      (crear-txt "DISTRITO" (list x curr_y (caddr pt)) 1.5 230 "DISTRITO")
      (foreach line dist_lines
        (setq curr_y (- curr_y 2.0))
        (crear-txt line (list x curr_y (caddr pt)) 1.5 230 "DISTRITO")
      )
    )
  )
  (princ "\nTextos geogr\\U+00E1ficos auto-ajustados generados correctamente.")
  (princ)
)

;; ==============================================================================
;; COMANDO PRINCIPAL: LAYOUT_UBICACION
;; ==============================================================================
(defun c:LAYOUT_UBICACION (/ olderr old_filedia old_cmdecho step dcl_id dcl_file file ret
                           sel_txt obj_txt txt_val old_ctab temp_path tmp_code)
  
  (setq old_filedia (getvar "FILEDIA") old_cmdecho (getvar "CMDECHO")) (setvar "FILEDIA" 1) (setvar "CMDECHO" 0)
  
  ;; ---> LECTURA AUTOM\\U+00C1TICA DEL C\\U+00D3DIGO DESDE PLANO_UBICACION <---
  (setq tmp_code (vlax-ldata-get "ML_SUITE" "TARGET_CODE"))
  (if (and tmp_code (/= tmp_code "")) 
    (setq *ML_TARGET_CODE* (vl-string-trim " \t\n\r" (strcase tmp_code)))
  )

  ;; E: guarda el *error* previo; el local no debe declararse *error* o el
  ;; setq capturaria nil y la restauracion seria falsa.
  (setq olderr *error*)

  (defun *error* (msg) 
    (if old_filedia (setvar "FILEDIA" old_filedia)) 
    (setvar "CMDECHO" (if old_cmdecho old_cmdecho 1)) 
    (setq *error* olderr)
    (princ "\nComando Layout Ubicaci\\U+00F3n cancelado o finalizado.") 
    (princ)
  )

  (defun save_ui_state ()
    (setq *ML_ESQ_ACTIVO* (get_tile "tg_esq_activo")
          *ML_ESQ_MODO* (get_tile "cbx_modo")
          *ML_ESQ_PE_TOG* (get_tile "tog_pe")
          *ML_ESQ_PE_VAL* (get_tile "val_pe")
          *ML_ESQ_LOC_TIPO* (get_tile "cbx_tipo_loc")
          *ML_TARGET_CODE* (vl-string-trim " \t\n\r" (strcase (get_tile "eb_codigo"))))
  )

  (setq step 1)

  (while (> step 0)
    (cond
      ((= step 1)
        (if (and *ML_TARGET_CODE* (/= *ML_TARGET_CODE* "") *Membrete_CSV_Path*)
          (LUBI_ParseCSV)
        )

        (setq dcl_file (vl-filename-mktemp "Dlg_UBI.dcl") file (open dcl_file "w"))
        (write-line "dlg_ubi : dialog { label = \"Etiquetado Layout - Ubicaci\\U+00F3n\"; width = 50;" file)
        
        ;; 1. Configuraci\\U+00F3n de Datos
        (write-line "  : boxed_column { label = \"1. Configuraci\\U+00F3n de Datos\";" file)
        (write-line "    : row { : button { key = \"btn_csv\"; label = \"Seleccionar CSV...\"; width = 15; } : text { key = \"txt_csv_status\"; value = \"FALTA CSV\"; width = 30; } }" file)
        (write-line "    : row { : edit_box { key = \"eb_codigo\"; label = \"C\\U+00F3digo:\"; edit_width = 15; } : button { key = \"btn_pick_code\"; label = \"?? Pick\"; } : button { key = \"btn_cargar\"; label = \"Cargar Datos\"; } }" file)
        (write-line "  }" file)
        
        ;; 2. Generador de Esquemas
        (write-line "  : boxed_column { label = \"2. GENERAR ESQUEMAS PU (EXTRACCI\\U+00D3N AUTO)\";" file)
        (write-line "    : toggle { key = \"tg_esq_activo\"; label = \"Activar Generaci\\U+00F3n de Esquemas\"; }" file)
        (write-line "    : popup_list { key = \"cbx_modo\"; label = \"Modo:\"; list = \"Ambos Esquemas\\nSolo Afectaci\\U+00F3n (70x60)\\nSolo Localizaci\\U+00F3n (80x60)\"; }" file)
        (write-line "    : row { : toggle { key = \"tog_pe\"; label = \"\\U+00BFTiene P.E.?\"; } : edit_box { key = \"val_pe\"; label = \"N\\U+00B0 P.E.:\"; edit_width = 15; } }" file)
        (write-line "    : popup_list { key = \"cbx_tipo_loc\"; label = \"Condici\\U+00F3n Localizaci\\U+00F3n:\"; list = \"PROPIETARIO\\nPOSEEDOR\\nOCUPANTE\"; }" file)
        (write-line "    spacer; : button { key = \"btn_ejecutar_esq\"; label = \"EJECUTAR ESQUEMAS PU\"; is_default = true; }" file)
        (write-line "  }" file)

        ;; 3. Etiquetas Individuales
        (write-line "  : boxed_column { label = \"3. ETIQUETAS INDIVIDUALES\";" file)
        (write-line "    : row { : button { key = \"btn_etiq_afec\"; label = \"Etiqueta Afectaci\\U+00F3n\"; } : button { key = \"btn_etiq_prop\"; label = \"Etiqueta Propietario\"; } }" file)
        (write-line "    : row { : button { key = \"btn_etiq_via\"; label = \"Etiqueta DV y Eje\"; } : button { key = \"btn_ubicacion\"; label = \"Ubicador Predio\"; } }" file)
        (write-line "    : row { : button { key = \"btn_ubi_geo\"; label = \"Ubicaci\\U+00F3n Geogr\\U+00E1fica\"; } }" file)
        (write-line "  }" file)
        
        (write-line "  spacer; : row { : button { key = \"cancel\"; label = \"Cerrar\"; is_cancel = true; } }" file)
        (write-line "}" file)
        (close file)

        (setq dcl_id (load_dialog dcl_file))
        (new_dialog "dlg_ubi" dcl_id)
        
        ;; Carga de valores en UI
        (if *Membrete_CSV_Path* (set_tile "txt_csv_status" (strcat "BD: " (vl-filename-base *Membrete_CSV_Path*) ".csv")))
        (set_tile "eb_codigo" *ML_TARGET_CODE*)
        (set_tile "tg_esq_activo" *ML_ESQ_ACTIVO*)
        (set_tile "cbx_modo" *ML_ESQ_MODO*)
        (set_tile "tog_pe" *ML_ESQ_PE_TOG*)
        (set_tile "val_pe" *ML_ESQ_PE_VAL*)
        (set_tile "cbx_tipo_loc" *ML_ESQ_LOC_TIPO*)

        (defun actualizar_ui_esquemas ()
          (if (= (get_tile "tg_esq_activo") "0")
            (progn (mode_tile "cbx_modo" 1) (mode_tile "tog_pe" 1) (mode_tile "val_pe" 1) (mode_tile "cbx_tipo_loc" 1) (mode_tile "btn_ejecutar_esq" 1))
            (progn 
              (mode_tile "cbx_modo" 0) (mode_tile "btn_ejecutar_esq" 0)
              (cond 
                ((= (get_tile "cbx_modo") "0") (mode_tile "tog_pe" 0) (if (= (get_tile "tog_pe") "1") (mode_tile "val_pe" 0) (mode_tile "val_pe" 1)) (mode_tile "cbx_tipo_loc" 0))
                ((= (get_tile "cbx_modo") "1") (mode_tile "tog_pe" 0) (if (= (get_tile "tog_pe") "1") (mode_tile "val_pe" 0) (mode_tile "val_pe" 1)) (mode_tile "cbx_tipo_loc" 1))
                ((= (get_tile "cbx_modo") "2") (mode_tile "tog_pe" 1) (mode_tile "val_pe" 1) (mode_tile "cbx_tipo_loc" 0))
              )
            )
          )
        )
        
        (actualizar_ui_esquemas)
        (action_tile "tg_esq_activo" "(actualizar_ui_esquemas)")
        (action_tile "cbx_modo" "(actualizar_ui_esquemas)")
        (action_tile "tog_pe" "(if (= $value \"0\") (mode_tile \"val_pe\" 1) (mode_tile \"val_pe\" 0))")

        ;; Botones de Accion
         (action_tile "btn_csv" "(setq temp_path (getfiled \"Seleccionar CSV\" \"\" \"csv;txt\" 8)) (if temp_path (progn (setq *Membrete_CSV_Path* temp_path) (vlax-ldata-put \"ML_SUITE\" \"CSV_PATH\" temp_path) (set_tile \"txt_csv_status\" (strcat \"BD: \" (vl-filename-base *Membrete_CSV_Path*) \".csv\"))))")
        (action_tile "btn_pick_code" "(save_ui_state) (done_dialog 2)")
        (action_tile "btn_cargar" "(save_ui_state) (done_dialog 3)")
        
        (action_tile "btn_ejecutar_esq" "(save_ui_state) (done_dialog 9)")
        (action_tile "btn_etiq_afec" "(save_ui_state) (done_dialog 4)")
        (action_tile "btn_etiq_prop" "(save_ui_state) (done_dialog 5)")
        (action_tile "btn_etiq_via" "(save_ui_state) (done_dialog 6)")
        (action_tile "btn_ubicacion" "(save_ui_state) (done_dialog 7)")
        (action_tile "btn_ubi_geo" "(save_ui_state) (done_dialog 8)")
        (action_tile "cancel" "(done_dialog 0)")
        
        (setq ret (start_dialog)) 
        (unload_dialog dcl_id) 
        (vl-file-delete dcl_file)
        
        (setq step (cond ((= ret 2) 2) ((= ret 3) 3) ((= ret 4) 4) ((= ret 5) 5) ((= ret 6) 6) ((= ret 7) 7) ((= ret 8) 8) ((= ret 9) 9) (t 0)))
      )

      ;; PICK CODE
      ((= step 2)
       (setq old_ctab (getvar "CTAB")) 
       (setvar "CTAB" "Model") 
       (if (setq sel_txt (car (entsel "\nSeleccione el Texto o MText con el c\\U+00F3digo: ")))
         (progn
           ;; F15-R1: wrapper con catch y liberado tras capturar el codigo.
           (setq obj_txt (vl-catch-all-apply 'vlax-ename->vla-object (list sel_txt)))
           (if (vl-catch-all-error-p obj_txt)
             (alert "El objeto seleccionado no es un texto v\\U+00E1lido.")
             (progn
               (if (or (= (vla-get-ObjectName obj_txt) "AcDbText") (= (vla-get-ObjectName obj_txt) "AcDbMText"))
                 (progn
                   (setq txt_val (vla-get-TextString obj_txt))
                   (while (vl-string-search "\\P" txt_val) (setq txt_val (vl-string-subst "" "\\P" txt_val)))
                   (setq *ML_TARGET_CODE* (vl-string-trim " \r\n" txt_val))
                   (princ (strcat "\nC\\U+00F3digo capturado: " *ML_TARGET_CODE*))
                   
                   (if *Membrete_CSV_Path* (LUBI_ParseCSV))
                 )
                 (alert "El objeto seleccionado no es un texto v\\U+00E1lido.")
               )
               (GM:ReleaseObjectSafe obj_txt)
             )
           )
         )
       )
       (setvar "CTAB" old_ctab)
       (setq step 1)
      )

      ;; PARSE CSV (CARGAR DATOS)
      ((= step 3)
        (if (LUBI_ParseCSV)
          (princ (strcat "\n> Datos cargados correctamente para el c\\U+00F3digo: " *ML_TARGET_CODE*))
          (alert "?? No se encontraron datos. Verifica que el CSV est\\U+00E1 seleccionado y el c\\U+00F3digo exista.")
        )
        (setq step 1)
      )

      ;; RUN: ETIQUETA AFECTACION (Con soporte a ambos m\\U+00E9todos)
      ((= step 4)
        (if *GL_DATA_AFECTACIONES*
          (EXEC_ETIQUETA_AFEC)
          (EXEC_ETIQUETA_AFEC_AL_VUELO)
        )
        (setq step 1)
      )

      ;; RUN: ETIQUETA PROPIETARIO
      ((= step 5)
        (if (not *GL_DATOS_CSV*) (LUBI_ParseCSV))
        (EXEC_ETIQUETA_PROP)
        (setq step 1)
      )

      ;; RUN: ETIQUETAS VIA
      ((= step 6)
        (EXEC_ETIQUETAS_VIA)
        (setq step 1)
      )

      ;; RUN: UBICADOR
      ((= step 7)
        (EXEC_UBICACION)
        (setq step 1)
      )

      ;; RUN: UBICACION GEOGRAFICA
      ((= step 8)
        (if (not *GL_DATOS_CSV*) (LUBI_ParseCSV))
        (EXEC_UBICACION_GEOGRAFICA)
        (setq step 1)
      )

      ;; RUN: ESQUEMAS PU FULL
      ((= step 9)
        (if (not *GL_DATOS_CSV*) (LUBI_ParseCSV)) 
        (EXEC_ESQUEMAS_PU_FULL)
        (setq step 1)
      )
    )
  )

  (setq *error* olderr)
  (if old_filedia (setvar "FILEDIA" old_filedia)) 
  (setvar "CMDECHO" (if old_cmdecho old_cmdecho 1)) 
  (princ)
)

(setq *GM_VER_LAYOUT* "2026-10-10-CHG3")
(princ "\nSISTEMA ETIQUETADO LAYOUT CARGADO. Escribe LAYOUT_UBICACION para iniciar. Revision 2026-10-10-CHG3.")
(princ)
