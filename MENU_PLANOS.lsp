;;; ==============================================================================
;;;  SISTEMA MAESTRO: MENU_PLANOS (HUB CENTRAL + MEMBRETE + LAYOUT + MODEL)
;;;  ACTUALIZADO: INTERFAZ DE ESQUEMAS ELIMINADA, EJECUCI\\U+00D3N DIRECTA DE COMANDOS
;;; ==============================================================================

(vl-load-com)

;; Version del modulo (verificada por GM_PLANOS_LOTE / GM_DIAGNOSTICO).
(setq *GM_VER_MENU* nil)

;; VARIABLES GLOBALES DE DATOS Y ESTADO DE VENTANA
(if (not *GL_DATOS_CSV*) (setq *GL_DATOS_CSV* nil))
(if (not *GL_CODIGO_BUSCADO*) (setq *GL_CODIGO_BUSCADO* ""))
(if (not *GL_OPCION_PLANO*) (setq *GL_OPCION_PLANO* 1))
(if (not *GL_SUB_OPCION*) (setq *GL_SUB_OPCION* 1))
(if (not *Membrete_CSV_Path*) (setq *Membrete_CSV_Path* nil))

;; --- RECUPERAR MEMORIA PERSISTENTE DEL DWG ---
(if (vlax-ldata-get "ML_SUITE" "TARGET_CODE")
  (setq *GL_CODIGO_BUSCADO* (vlax-ldata-get "ML_SUITE" "TARGET_CODE"))
)
(if (vlax-ldata-get "ML_SUITE" "CSV_PATH")
  (setq *Membrete_CSV_Path* (vlax-ldata-get "ML_SUITE" "CSV_PATH"))
)

;; --- FUNCIONES AUXILIARES DE TEXTO ---
;; POR QUE duplicadas: LAYOUT_UBICACION.lsp define las mismas dos funciones para
;; operar autocontenido. Con el orden oficial MENU->PLANO->LAYOUT->AUTO gana la
;; version de LAYOUT; se mantienen aqui para que MENU funcione tambien cargado
;; solo. No cambiar una sin la otra.
(defun count-quotes (str / cnt pos) (setq cnt 0 pos 0) (while (setq pos (vl-string-search "\"" str pos)) (setq cnt (1+ cnt) pos (1+ pos))) cnt)
(defun _str-split (str del / pos lst) (while (setq pos (vl-string-search del str)) (setq lst (cons (substr str 1 pos) lst) str (substr str (+ pos 1 (strlen del))))) (reverse (cons str lst)))

;; Normaliza un campo CSV: recorta espacios, quita comillas envolventes, desescapa
;; dobles comillas, convierte saltos en \P y quita el apostrofe inicial que Excel
;; antepone a textos numericos. POR QUE: el CSV Huanca mezcla ; y , y trae
;; multilinea; sin esto el nth 14 (codigo) no coincide.
(defun ML_CleanVal (txt) 
  (if (= (type txt) 'STR) 
    (progn 
      (setq txt (vl-string-trim " \t\n\r" txt)) 
      (if (and (> (strlen txt) 1) (= (substr txt 1 1) "\"") (= (substr txt (strlen txt) 1) "\"")) 
        (setq txt (substr txt 2 (- (strlen txt) 2)))
      ) 
      (while (vl-string-search "\"\"" txt) (setq txt (vl-string-subst "\"" "\"\"" txt))) 
      (while (vl-string-search "\n" txt) (setq txt (vl-string-subst "\\P" "\n" txt))) 
      (setq txt (vl-string-trim " " txt))
      (if (and (> (strlen txt) 0) (= (substr txt 1 1) "'")) (setq txt (substr txt 2)))
      txt
    ) 
    ""
  )
)

(defun _count_lines (txt / count pos) (setq count 1 pos 0) (while (setq pos (vl-string-search "\\P" txt pos)) (setq count (1+ count) pos (+ pos 2))) count)

;; Funci\\U+00F3n para forzar la altura de un estilo a 0.0
(defun fix-style-height (styleName / st st_ent st_data)
  (setq st (tblsearch "STYLE" styleName))
  (if (and st (/= (cdr (assoc 40 st)) 0.0))
    (progn
      (setq st_ent (tblobjname "STYLE" styleName)
            st_data (entget st_ent)
            st_data (subst '(40 . 0.0) (assoc 40 st_data) st_data))
      (entmod st_data)
    )
  )
)

(defun GM:GetAutoCajetinPoint (/ limmin limmax margin_x margin_y)
  ;; Coordenada fija relativa al origen del Layout base de 420 x 297 unidades.
  ;; El membrete queda dentro del panel derecho y encima del pie institucional.
  (setq limmin (getvar "LIMMIN") limmax (getvar "LIMMAX") margin_x 5.0 margin_y 42.0)
  (list (- (car limmax) margin_x) (+ (cadr limmin) margin_y) 0.0)
)

;; Funci\\U+00F3n para obtener el comando del LISP de Modelado
(defun GM:Get-Model-Command (opcion_plano)
  (cdr
    (assoc opcion_plano
      '(
        (1 . "PLANO_UBICACION")
        (2 . "PLANO_AFECTACION")
        (3 . "PLANO_PERIMETRICO")
        (4 . "PLANO_MATRIZ")
        (5 . "PLANO_DISTRIBUCION")
        (6 . "PLANO_ELEVACION")
        (7 . "PLANO_OBRAS_COMPLEMENTARIAS")
        (8 . "PLANO_DIAGNOSTICO")
        (9 . "PLANO_PERIMETRICO_2")
      )
    )
  )
)

;; Funci\\U+00F3n para obtener el comando del LISP de Layout
(defun GM:Get-Layout-Command (opcion_plano)
  (cdr
    (assoc opcion_plano
      '(
        (1 . "LAYOUT_UBICACION")
        (2 . "LAYOUT_AFECTACION")
        (3 . "LAYOUT_PERIMETRICO")
        (4 . "LAYOUT_MATRIZ")
        (5 . "LAYOUT_DISTRIBUCION")
        (6 . "LAYOUT_ELEVACION")
        (7 . "LAYOUT_OBRAS_COMPLEMENTARIAS")
        (8 . "LAYOUT_DIAGNOSTICO")
        (9 . "LAYOUT_PERIMETRICO_2")
      )
    )
  )
)


;; ==============================================================================
;; COMANDO PRINCIPAL: MENU_PLANOS
;; ==============================================================================
;; Contrato CSV (indices 0-18): 14=codigo, 15=escala rotulo, 16=fecha, 17=zona,
;; 18=datum/proyeccion; 13=referencia U.C. alternativa. El membrete manual lee por
;; posicion fija, sin cabecera.
;; Codigos DCL ret/step: 0=salir, 2=layout, 3=membrete, 9=model, 10=appload, 11=pick.
(defun c:MENU_PLANOS (/ *error* old_filedia old_cmdecho step dcl_id dcl_file file ret plan_list sub_list
                        opcion_plano sub_opcion codigo_buscado pto_click txt_plano cmd acad_doc acad_app
                        openfile linea lista_datos_temp datos_encontrados temp_path next_line
                        w_total h_cabecera h_cabecera_def w_izq h_cuerpo h_fila y_base_cuerpo
                        w_mitad gap_v gap_h shift_user w_f1 w_f2 w_f3 w_f4
                        h_f_inf h_f_sup txt_h val_h old_h tb_lst
                        i val_L val_R
                        style_data style_lbl style_tit style_romant style_num
                        txt_n_plano txt_index txt_diag_linea2
                        mx my
                        csv_path
                        x y yc yn labels_left labels_right xf_next hms yf x_foot
                        sel_txt obj_txt txt_val old_ctab)

  (vl-load-com)
  ;; POR QUE se guardan: FILEDIA/CMDECHO se fuerzan a 1/0 para DCL y COMMAND sin
  ;; dialogos; deben volver a su valor previo para no alterar la sesion.
  (setq old_filedia (getvar "FILEDIA") old_cmdecho (getvar "CMDECHO"))
  (setvar "FILEDIA" 1) (setvar "CMDECHO" 0)

  (defun *error* (msg)
    (if old_filedia (setvar "FILEDIA" old_filedia))
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if (and old_ctab (/= (getvar "CTAB") old_ctab)) (setvar "CTAB" old_ctab))
    (princ "\nComando cancelado o finalizado.")
    (princ)
  )

  ;; Inicializar las variables locales con la memoria global guardada
  (setq opcion_plano *GL_OPCION_PLANO*)
  (setq sub_opcion *GL_SUB_OPCION*)
  (setq codigo_buscado *GL_CODIGO_BUSCADO*)

  ;; --- ESTILOS DE MEMBRETE ---
  (setq style_lbl "ROMANS" style_data "ST-COORP-GM" style_tit "WMF-Arial0" style_romant "ROMANT" style_num "TREBUCHET") 
  (if (not (tblsearch "STYLE" style_lbl)) (command "_.-STYLE" style_lbl "romans.shx" "0.0" "1.15" "0" "_N" "_N" "_N"))
  (if (not (tblsearch "STYLE" style_data)) (command "_.-STYLE" style_data "romans.shx" "0.0" "1.00" "0" "_N" "_N" "_N"))
  (if (not (tblsearch "STYLE" style_tit)) (command "_.-STYLE" style_tit "Arial" "0.0" "1.00" "0" "_N" "_N"))
  (if (not (tblsearch "STYLE" style_romant)) (command "_.-STYLE" style_romant "romant.shx" "0.0" "1.10" "0" "_N" "_N" "_N"))
  (if (not (tblsearch "STYLE" style_num)) (command "_.-STYLE" style_num "Trebuchet MS" "0.0" "1.00" "0" "_N" "_N"))

  (mapcar 'fix-style-height (list style_lbl style_data style_tit style_romant style_num))

  (setq plan_list '("1. UBICACI\\U+00D3N" "2. AFECTACI\\U+00D3N" "3. PERIM\\U+00C9TRICO" "4. MATRIZ" "5. DISTRIBUCI\\U+00D3N" "6. ELEVACI\\U+00D3N" "7. OBRAS COMPLEMENTARIAS" "8. DIAGN\\U+00D3STICO" "9. PERIM\\U+00C9TRICO (N\\U+00B002)"))
  (setq sub_list '("1. PROPIETARIO" "2. PROPIETARIO ESTATAL" "3. POSEEDOR" "4. OCUPANTE"))

  (setq step 1)

  (while (> step 0)
    (cond
      ((= step 1)
        (setq dcl_file (vl-filename-mktemp "Dlg1.dcl") file (open dcl_file "w"))
        (write-line "dlg_1 : dialog { label = \"Generador de Membretes GM\";" file)
        (write-line "  : boxed_column { label = \"Base de Datos (CSV)\";" file)
        (write-line "    : row { : button { key = \"btn_csv\"; label = \"Seleccionar CSV...\"; width = 15; }" file)
        (write-line "            : text { key = \"txt_csv_status\"; value = \"FALTA SELECCIONAR BASE DE DATOS\"; width = 45; } } }" file)
        (write-line "  : boxed_column { label = \"Configuraci\\U+00F3n Principal\";" file)
        (write-line "    : row {" file)
        (write-line "      : edit_box { key = \"eb_codigo\"; label = \"C\\U+00F3digo a buscar:\"; width = 30; edit_width = 25; }" file)
        (write-line "      : button { key = \"btn_pick_code\"; label = \"?? Seleccionar\"; width = 12; fixed_width = true; }" file)
        (write-line "    }" file)
        (write-line "    : popup_list { key = \"cb_plano\"; label = \"Tipo de Plano:\"; width = 30; edit_width = 35; }" file)
        (write-line "    : popup_list { key = \"cb_sub_diag\"; label = \"Sub-opci\\U+00F3n (Diagn\\U+00F3stico):\"; width = 30; edit_width = 35; is_enabled = false; } }" file)
        (write-line "  : row {" file)
        (write-line "    : button { key = \"btn_membrete\"; label = \"1. GENERAR MEMBRETE\"; is_default = true; }" file)
        (write-line "    : button { key = \"btn_model\"; label = \"2. MODEL\"; }" file)
        (write-line "    : button { key = \"btn_layout\"; label = \"3. LAYOUT\"; }" file)
        (write-line "    : button { key = \"btn_appload\"; label = \"APPLOAD\"; }" file)
        (write-line "  }" file)
        (write-line "  : row { : button { key = \"cancel\"; label = \"Salir\"; is_cancel = true; alignment = right; } } }" file)
        (close file)

        (setq dcl_id (load_dialog dcl_file))
        (new_dialog "dlg_1" dcl_id)
        
        (start_list "cb_plano") (mapcar 'add_list plan_list) (end_list)
        (start_list "cb_sub_diag") (mapcar 'add_list sub_list) (end_list)
        
        ;; Cargar valores desde la memoria local/global
        (set_tile "cb_plano" (itoa (1- opcion_plano)))
        (set_tile "cb_sub_diag" (itoa (1- sub_opcion)))
        (set_tile "eb_codigo" codigo_buscado)
        
        (if (= opcion_plano 8) (mode_tile "cb_sub_diag" 0))
        (if (and *Membrete_CSV_Path* (findfile *Membrete_CSV_Path*))
          (set_tile "txt_csv_status" (strcat "BD SELECCIONADA: " (vl-filename-base (findfile *Membrete_CSV_Path*)) ".csv"))
          (if *Membrete_CSV_Path* (set_tile "txt_csv_status" "RUTA CSV INVALIDA: SELECCIONAR NUEVAMENTE"))
        )
        
        (action_tile "btn_csv" "(setq temp_path (getfiled \"Seleccionar CSV\" \"\" \"csv;txt\" 8)) (if temp_path (progn (setq *Membrete_CSV_Path* temp_path) (vlax-ldata-put \"ML_SUITE\" \"CSV_PATH\" temp_path) (set_tile \"txt_csv_status\" (strcat \"BD SELECCIONADA: \" (vl-filename-base *Membrete_CSV_Path*) \".csv\"))))")
        (action_tile "cb_plano" "(if (= $value \"7\") (mode_tile \"cb_sub_diag\" 0) (mode_tile \"cb_sub_diag\" 1))")
        
        ;; BOTON PARA SELECCIONAR TEXTO DE PANTALLA
        (action_tile "btn_pick_code" "(setq codigo_buscado (get_tile \"eb_codigo\") opcion_plano (1+ (atoi (get_tile \"cb_plano\"))) sub_opcion (1+ (atoi (get_tile \"cb_sub_diag\")))) (done_dialog 11)")

        ;; APPLOAD B\\U+00E1sico
        (action_tile "btn_appload" "(done_dialog 10)")

        ;; Al hacer clic en un bot\\U+00F3n, guardamos los valores en las variables globales para el futuro
        (action_tile "btn_membrete" "(cond ((or (not *Membrete_CSV_Path*) (not (findfile *Membrete_CSV_Path*))) (alert \"?? La ruta del CSV no existe. Selecciona nuevamente el archivo.\")) ((= (get_tile \"eb_codigo\") \"\") (alert \"?? Ingresa el c\\U+00F3digo.\")) (T (setq opcion_plano (1+ (atoi (get_tile \"cb_plano\"))) sub_opcion (1+ (atoi (get_tile \"cb_sub_diag\"))) codigo_buscado (get_tile \"eb_codigo\") *GL_OPCION_PLANO* opcion_plano *GL_SUB_OPCION* sub_opcion *GL_CODIGO_BUSCADO* codigo_buscado) (vlax-ldata-put \"ML_SUITE\" \"TARGET_CODE\" codigo_buscado) (done_dialog 3)))")
        
        (action_tile "btn_model" "(cond ((= (get_tile \"eb_codigo\") \"\") (alert \"?? Ingresa el c\\U+00F3digo.\")) (T (setq opcion_plano (1+ (atoi (get_tile \"cb_plano\"))) sub_opcion (1+ (atoi (get_tile \"cb_sub_diag\"))) codigo_buscado (get_tile \"eb_codigo\") *GL_OPCION_PLANO* opcion_plano *GL_SUB_OPCION* sub_opcion *GL_CODIGO_BUSCADO* codigo_buscado) (vlax-ldata-put \"ML_SUITE\" \"TARGET_CODE\" codigo_buscado) (done_dialog 9)))")
        
        (action_tile "btn_layout" "(cond ((= (get_tile \"eb_codigo\") \"\") (alert \"?? Ingresa el c\\U+00F3digo.\")) (T (setq opcion_plano (1+ (atoi (get_tile \"cb_plano\"))) sub_opcion (1+ (atoi (get_tile \"cb_sub_diag\"))) codigo_buscado (get_tile \"eb_codigo\") *GL_OPCION_PLANO* opcion_plano *GL_SUB_OPCION* sub_opcion *GL_CODIGO_BUSCADO* codigo_buscado) (vlax-ldata-put \"ML_SUITE\" \"TARGET_CODE\" codigo_buscado) (done_dialog 2)))")
        
        (action_tile "cancel" "(done_dialog 0)")
        
        (setq ret (start_dialog)) (unload_dialog dcl_id) (vl-file-delete dcl_file)
        (setq step (cond ((= ret 3) 3) ((= ret 9) 9) ((= ret 2) 2) ((= ret 10) 10) ((= ret 11) 11) (t 0)))
      )

      ;; ========================================================
      ;; BOT\\U+00D3N: SELECCIONAR TEXTO EN PANTALLA
      ;; ========================================================
      ((= step 11)
        (setq old_ctab (getvar "CTAB"))
        (if (/= old_ctab "Model") (setvar "CTAB" "Model")) ; Salto al Model
        
        (if (setq sel_txt (car (entsel "\nSeleccione el Texto o MText con el c\\U+00F3digo en el MODEL (clic izquierdo): ")))
          (progn
            ;; F15-R1: wrapper con catch y liberado tras capturar el codigo.
            (setq obj_txt (vl-catch-all-apply 'vlax-ename->vla-object (list sel_txt)))
            (if (vl-catch-all-error-p obj_txt)
              (alert "El objeto seleccionado no es un texto v\\U+00E1lido.")
              (progn
                (if (or (= (vla-get-ObjectName obj_txt) "AcDbText") (= (vla-get-ObjectName obj_txt) "AcDbMText"))
                  (progn
                    (setq txt_val (vla-get-TextString obj_txt))
                    ;; Eliminar saltos de l\\U+00EDnea propios del MTEXT (\P)
                    (while (vl-string-search "\\P" txt_val) (setq txt_val (vl-string-subst "" "\\P" txt_val)))
                    ;; Limpiar espacios iniciales y finales
                    (setq codigo_buscado (vl-string-trim " \r\n" txt_val))
                    (setq *GL_CODIGO_BUSCADO* codigo_buscado)
                    (vlax-ldata-put "ML_SUITE" "TARGET_CODE" codigo_buscado)
                  )
                  (alert "El objeto seleccionado no es un texto v\\U+00E1lido.")
                )
                (if (boundp 'GM:ReleaseObjectSafe) (GM:ReleaseObjectSafe obj_txt))
              )
            )
          )
        )
        
        (if (/= (getvar "CTAB") old_ctab) (setvar "CTAB" old_ctab)) ; Regreso al Layout
        (setq step 1) ;; Regresa autom\\U+00E1ticamente a mostrar el panel principal
      )

      ;; ========================================================
      ;; BOT\\U+00D3N: APPLOAD DEDICADO
      ;; ========================================================
      ((= step 10)
        (command "_.APPLOAD")
        (setq step 1)
      )

      ;; ========================================================
      ;; BOT\\U+00D3N: 2. MODEL
      ;; ========================================================
      ((= step 9)
        ;; POR QUE sendcommand: el comando MODEL corre como comando AutoCAD
        ;; independiente tras cerrar este dialogo; una llamada directa Lisp
        ;; anidaria contextos DCL/COM. Solo UBICACION existe en este paquete;
        ;; el resto son nombres reservados para modulos externos.
        (setq cmd (GM:Get-Model-Command opcion_plano))
        (if cmd
          (progn
            ;; F15-R1: doc/app liberados tras el sendcommand (cero uso posterior).
            (setq acad_app (vl-catch-all-apply 'vlax-get-acad-object '()))
            (if (vl-catch-all-error-p acad_app) (setq acad_app nil))
            (setq acad_doc (if acad_app (vl-catch-all-apply 'vla-get-ActiveDocument (list acad_app)) nil))
            (if (vl-catch-all-error-p acad_doc) (setq acad_doc nil))
            (if acad_doc (vla-sendcommand acad_doc (strcat cmd "\r")))
            (if (boundp 'GM:ReleaseObjectSafe) (progn (GM:ReleaseObjectSafe acad_doc) (GM:ReleaseObjectSafe acad_app)))
            (setq step 0) ;; Termina LISP actual para ceder el control
          )
          (progn
            (alert "?? No existe un comando MODEL configurado para este tipo de plano.")
            (setq step 1) ;; Vuelve al men\\U+00FA principal
          )
        )
      )

      ;; ========================================================
      ;; BOT\\U+00D3N: 3. LAYOUT (EJECUCI\\U+00D3N DIRECTA DEL COMANDO ASIGNADO)
      ;; ========================================================
      ((= step 2)
        (setq cmd (GM:Get-Layout-Command opcion_plano))
        (if cmd
          (progn
            ;; F15-R1: doc/app liberados tras el sendcommand.
            (setq acad_app (vl-catch-all-apply 'vlax-get-acad-object '()))
            (if (vl-catch-all-error-p acad_app) (setq acad_app nil))
            (setq acad_doc (if acad_app (vl-catch-all-apply 'vla-get-ActiveDocument (list acad_app)) nil))
            (if (vl-catch-all-error-p acad_doc) (setq acad_doc nil))
            (if acad_doc (vla-sendcommand acad_doc (strcat cmd "\r")))
            (if (boundp 'GM:ReleaseObjectSafe) (progn (GM:ReleaseObjectSafe acad_doc) (GM:ReleaseObjectSafe acad_app)))
            (setq step 0) ;; Termina LISP actual para ceder el control al comando llamado
          )
          (progn
            (alert "?? No existe un comando LAYOUT configurado para este tipo de plano.")
            (setq step 1) ;; Vuelve al men\\U+00FA principal
          )
        )
      )

      ;; ========================================================
      ;; BOT\\U+00D3N: 1. GENERAR MEMBRETE
      ;; ========================================================
      ((= step 3)
        (setq txt_diag_linea2 "")
        (cond
          ((= opcion_plano 1) (setq txt_plano "UBICACI\\U+00D3N" txt_n_plano "01" txt_index "01 / 04"))
          ((= opcion_plano 2) (setq txt_plano "AFECTACI\\U+00D3N" txt_n_plano "02" txt_index "02 / 04"))
          ((= opcion_plano 3) (setq txt_plano "PERIM\\U+00C9TRICO" txt_n_plano "03" txt_index "03 / 04"))
          ((= opcion_plano 4) (setq txt_plano "MATRIZ" txt_n_plano "04" txt_index "04 / 04"))
          ((= opcion_plano 5) (setq txt_plano "DISTRIBUCI\\U+00D3N" txt_n_plano "05" txt_index "05 / 07"))
          ((= opcion_plano 6) (setq txt_plano "ELEVACI\\U+00D3N" txt_n_plano "06" txt_index "06 / 07"))
          ((= opcion_plano 7) (setq txt_plano "OBRAS COMPLEMENTARIAS" txt_n_plano "07" txt_index "07 / 07"))
          ((= opcion_plano 8) (setq txt_plano "DIAGN\\U+00D3STICO" txt_n_plano "01" txt_index "01 / 02" txt_diag_linea2 (cond ((= sub_opcion 1) "PROPIETARIO") ((= sub_opcion 2) "PROPIETARIO ESTATAL") ((= sub_opcion 3) "POSEEDOR") ((= sub_opcion 4) "OCUPANTE") (t "---"))))
          ((= opcion_plano 9) (setq txt_plano "PERIM\\U+00C9TRICO" txt_n_plano "02" txt_index "02 / 02"))
        )

        (setq csv_path (if (and *Membrete_CSV_Path* (findfile *Membrete_CSV_Path*)) (findfile *Membrete_CSV_Path*) nil)
              openfile (if csv_path (open csv_path "r") nil)
              datos_encontrados nil *GL_DATOS_CSV* nil)
        (if openfile
          (progn
            (while (and (not datos_encontrados) (setq linea (read-line openfile)))
              (while (= (rem (count-quotes linea) 2) 1) (setq linea (strcat linea (if (setq next_line (read-line openfile)) (strcat "\\P" next_line) "\""))))
               (setq lista_datos_temp (mapcar 'ML_CleanVal (_str-split linea (if (vl-string-search ";" linea) ";" ","))))
              (if (and (nth 14 lista_datos_temp) (= (strcase (nth 14 lista_datos_temp)) (strcase codigo_buscado))) 
                (setq datos_encontrados lista_datos_temp *GL_DATOS_CSV* lista_datos_temp)
              )
            )
            (close openfile)

            (if datos_encontrados
              (progn
                (setq pto_click (getpoint (strcat "\nC\\U+00F3digo [" codigo_buscado "] ENCONTRADO. Seleccione esquina INFERIOR DERECHA (ENTER = posici\\U+00F3n autom\\U+00E1tica): ")))
              (if (not pto_click) (setq pto_click (GM:GetAutoCajetinPoint)))
              (if pto_click
                (progn
                  (setq w_total 117.06 h_cabecera_def 16.68 h_cuerpo 48.23 gap_v 1.4 gap_h 1.8 shift_user 3.6 h_f_sup 11.3319 h_f_inf 5.5955 w_izq 67.78 w_mitad 33.89 h_fila 6.89 val_h 1.50 txt_h 0.94)
                  (setq h_cabecera (if (> (+ (* (_count_lines (if (nth 0 datos_encontrados) (nth 0 datos_encontrados) "")) 2.5) 4.5) h_cabecera_def) (+ (* (_count_lines (if (nth 0 datos_encontrados) (nth 0 datos_encontrados) "")) 2.5) 4.5) h_cabecera_def))
                  (setq y_base_cuerpo (+ (cadr pto_click) h_f_inf h_f_sup gap_v h_cuerpo) x (- (car pto_click) (- (+ w_total shift_user) gap_h)) y (+ y_base_cuerpo h_cabecera))

                  (defun _draw_lbl (pa pb txt) 
                    (command "_RECTANG" "_non" pa "_non" pb) 
                    (if (and txt (/= txt "")) 
                      (command "_TEXT" "_STYLE" style_lbl "_J" "_L" "_non" (list (+ (car pa) 1.0) (- (cadr pa) 2.2)) txt_h "0.0" txt)
                    )
                  )
                  
                  (defun _draw_val (pa pb val) 
                    (if (and val (/= val "")) 
                      (command "_TEXT" "_STYLE" style_data "_J" "_MC" "_non" (list (/ (+ (car pa) (car pb)) 2.0) (- (/ (+ (cadr pa) (cadr pb)) 2.0) 0.8)) val_h "0.0" val)
                    )
                  )

                  (_draw_lbl (list x y) (list (+ x w_total) y_base_cuerpo) "APELLIDOS Y NOMBRES/RAZON SOCIAL")
                  (if (and (nth 0 datos_encontrados) (/= (nth 0 datos_encontrados) "")) 
                    (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText") (cons 7 style_data) (cons 10 (list (+ x (/ w_total 2.0)) (+ y_base_cuerpo (/ h_cabecera 2.0)) 0.0)) (cons 40 val_h) (cons 41 (- w_total 6.0)) (cons 71 5) (cons 1 (nth 0 datos_encontrados))))
                  )
                  
                  (command "_RECTANG" "_non" (list (+ x w_izq) y_base_cuerpo) "_non" (list (+ x w_total) (- y_base_cuerpo h_cuerpo)))
                  (command "_TEXT" "_STYLE" style_lbl "_J" "_L" "_non" (list (+ x w_izq 1.0) (- y_base_cuerpo 3.7)) 2.41 "0.0" "V\\U+00B0B\\U+00B0")

                  (setq labels_left '("DENOMINACI\\U+00D3N" "CONDICI\\U+00D3N" "PARTIDA" "SECTOR" "TIPO DE PREDIO" "PROGRESIVA INICIAL" "PROGRESIVA FINAL")
                        labels_right '("" "TRAMO" "LADO" "DEPARTAMENTO" "PROVINCIA" "DISTRITO" "CENTRO URBANO/UNIDAD CATASTRAL") i 0)
                  
                  (repeat 7
                    (setq yc (- y_base_cuerpo (* i h_fila)) yn (- y_base_cuerpo (* (1+ i) h_fila)) val_L (nth (cond ((= i 0) 1) (t (* i 2))) datos_encontrados) val_R (if (> i 0) (nth (1+ (* i 2)) datos_encontrados) nil))
                    (if (= i 0) 
                      (progn 
                        (_draw_lbl (list x yc) (list (+ x w_izq) yn) (nth i labels_left)) 
                        (_draw_val (list x yc) (list (+ x w_izq) yn) val_L)
                      )
                      (progn 
                        (_draw_lbl (list x yc) (list (+ x w_mitad) yn) (nth i labels_left)) 
                        
                        (setq old_h val_h)
                        (if (and (= i 1) (= (strcase val_L) "POSESIONARIO MAYOR A 10 A\\U+00D1OS")) (setq val_h 1.20))
                        (if (and (or (= i 5) (= i 6)) val_L (/= val_L "")) 
                          (if (setq tb_lst (textbox (list (cons 0 "TEXT") (cons 1 val_L) (cons 40 val_h) (cons 7 style_data)))) 
                            (if (> (- (car (cadr tb_lst)) (car (car tb_lst))) (- w_mitad 2.0)) (setq val_h 1.20))
                          )
                        )
                        (_draw_val (list x yc) (list (+ x w_mitad) yn) val_L) (setq val_h old_h)

                        (_draw_lbl (list (+ x w_mitad) yc) (list (+ x w_izq) yn) (nth i labels_right)) 
                        (_draw_val (list (+ x w_mitad) yc) (list (+ x w_izq) yn) val_R)
                      )
                    )
                    (setq i (1+ i))
                  )

                  (setq yf (- y_base_cuerpo h_cuerpo gap_v) w_f1 23.0177 w_f2 24.3433 w_f3 36.5027 w_f4 20.34 x_foot (+ x w_total (* -1 gap_h) (* -1 (+ w_f1 w_f2 w_f3 w_f4)) shift_user))
                  (_draw_lbl (list x_foot yf) (list (+ x_foot w_f1 w_f2) (- yf h_f_sup)) "PLANO:") (setq mx (/ (+ x_foot (+ x_foot w_f1 w_f2)) 2.0) my (/ (+ yf (- yf h_f_sup)) 2.0))
                  
                  (if (= opcion_plano 8) 
                    (progn 
                      (command "_TEXT" "_STYLE" style_tit "_J" "_MC" "_non" (list mx (+ my 0.2)) 2.50 "0.0" txt_plano) 
                      (command "_TEXT" "_STYLE" style_romant "_J" "_MC" "_non" (list mx (- my 2.4)) 0.91 "0.0" (strcat "DIAGN\\U+00D3STICO: " txt_diag_linea2))
                    ) 
                    (command "_TEXT" "_STYLE" style_tit "_J" "_MC" "_non" (list mx (- my 0.8)) 2.50 "0.0" txt_plano)
                  )
                  
                  (_draw_lbl (list x_foot (- yf h_f_sup)) (list (+ x_foot w_f1) (- yf h_f_sup h_f_inf)) "PROYECCI\\U+00D3N:") 
                  (_draw_val (list x_foot (- yf h_f_sup)) (list (+ x_foot w_f1) (- yf h_f_sup h_f_inf)) (nth 18 datos_encontrados))
                  
                  (_draw_lbl (list (+ x_foot w_f1) (- yf h_f_sup)) (list (+ x_foot w_f1 w_f2) (- yf h_f_sup h_f_inf)) "ZONA:") 
                  (_draw_val (list (+ x_foot w_f1) (- yf h_f_sup)) (list (+ x_foot w_f1 w_f2) (- yf h_f_sup h_f_inf)) (nth 17 datos_encontrados))
                  
                  (setq xf_next (+ x_foot w_f1 w_f2) hms (/ h_f_sup 2.0))
                  (_draw_lbl (list xf_next yf) (list (+ xf_next w_f3) (- yf hms)) "C\\U+00D3DIGO:") 
                  (_draw_val (list xf_next yf) (list (+ xf_next w_f3) (- yf hms)) (nth 14 datos_encontrados))
                  
                  (_draw_lbl (list xf_next (- yf hms)) (list (+ xf_next w_f3) (- yf h_f_sup)) "ESCALA:") 
                  (_draw_val (list xf_next (- yf hms)) (list (+ xf_next w_f3) (- yf h_f_sup)) (nth 15 datos_encontrados))
                  
                  (_draw_lbl (list xf_next (- yf h_f_sup)) (list (+ xf_next w_f3) (- yf h_f_sup h_f_inf)) "FECHA:") 
                  (_draw_val (list xf_next (- yf h_f_sup)) (list (+ xf_next w_f3) (- yf h_f_sup h_f_inf)) (nth 16 datos_encontrados))
                  
                  (_draw_lbl (list (+ xf_next w_f3) yf) (list (+ xf_next w_f3 w_f4) (- yf h_f_sup h_f_inf)) "N\\U+00B0 PLANO") 
                  (command "_TEXT" "_STYLE" style_num "_J" "_MC" "_non" (list (/ (+ (+ xf_next w_f3) (+ xf_next w_f3 w_f4)) 2.0) (/ (+ yf (- yf h_f_sup h_f_inf)) 2.0)) 6.00 "0.0" txt_n_plano)
                  
                  (command "_TEXT" "_STYLE" style_lbl "_J" "_R" "_non" (list (- (+ xf_next w_f3 w_f4) 1.5) (+ (- yf h_f_sup h_f_inf) 1.5)) 1.00 "0" txt_index)
                  
                  (princ "\nMembrete finalizado correctamente.")
                )
              )
              )
              (alert (strcat "?? No se encontr\\U+00F3 el c\\U+00F3digo \"" codigo_buscado "\" en el CSV."))
            )
          )
          (alert (strcat "?? No se pudo abrir la Base de Datos.\n\nRuta: " (if *Membrete_CSV_Path* *Membrete_CSV_Path* "(vacia)") "\n\nVerifica que el archivo exista, que tengas permisos y que no este bloqueado por otro programa."))
        )
        (setq step 1) ;; Vuelve al men\\U+00FA principal
      )
    )
  )

  (if old_filedia (setvar "FILEDIA" old_filedia))
  (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
  (princ)
)

(setq *GM_VER_MENU* "2026-09-30-FIX3")
(princ "\nSISTEMA GM CARGADO. Escribe MENU_PLANOS para iniciar.")
(princ)
