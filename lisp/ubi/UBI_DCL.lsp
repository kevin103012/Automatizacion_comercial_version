;; ============================================================================
;; UBI_DCL.lsp - Sistema GM Ingenieros (modulo ubi)
;; Movido SIN CAMBIOS desde PLANO_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


;; ==============================================================================
;; COMANDO PRINCIPAL INDESTRUCTIBLE
;; ==============================================================================
(defun c:PLANO_UBICACION (/ input_vp dcl_id dcl_file fn dcl_status old_ctab olderr old_ctab_main scales vp_info new_idx current_scale_str map_to_use map_data old_rect_handle pl_ent sel_txt obj_txt txt_val scl_val i launch_gm input_catch txt_catch dcl_file_param fn_param dcl_id_param vp_hl_name found_rect ss_model j tmp_ent tmp_obj hl_str)
  (UBI_RestoreDWGMemory)
  (setq old_ctab_main (getvar "CTAB") olderr *error*)

  (defun *error* (msg)
    (if (and old_ctab_main (/= (getvar "CTAB") old_ctab_main)) (vl-catch-all-apply 'setvar (list "CTAB" old_ctab_main)))
    (vl-catch-all-apply 'setvar (list "SELECTIONEFFECT" 1))
    (setq *error* olderr) 
    (if (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")
      (princ "\n[ACCI\\U+00D3N CANCELADA POR EL USUARIO (ESC)]")
      (princ (strcat "\n[ALERTA DE SISTEMA EVITADA]: " msg))
    )
    (UBI_Nuke-Memory)
(princ)
)

  (initget 4)
  (setq input_catch (vl-catch-all-apply 'getint (list (strcat "\n\\U+00BFCon qu\\U+00E9 Viewport trabajar? (1-5) <" (itoa *UBI_ACTIVE_VP*) ">: "))))
  (if (or (vl-catch-all-error-p input_catch) (not input_catch)) (setq input_vp *UBI_ACTIVE_VP*) (setq input_vp input_catch))
  (if (> input_vp 5) (setq input_vp 5)) (if (< input_vp 1) (setq input_vp 1))
  (setq *UBI_ACTIVE_VP* input_vp *UBI_VP_DATA_MEM* (cdr (assoc *UBI_ACTIVE_VP* *UBI_VP_DICT*)))
  (UBI_AutoUpdateFactor)
  (setq scales '("20" "25" "50" "75" "100" "125" "200" "250" "500" "750" "1000" "1250" "2000" "2500" "5000" "7500" "10000" "12500" "20000" "25000"))

  (setq dcl_file (vl-filename-mktemp "gestor_maestro.dcl") fn (open dcl_file "w"))
  (write-line (strcat "gestor_master : dialog { label = \"Centro de Comando: PLANO UBICACION (VP Activo: " (itoa *UBI_ACTIVE_VP*) ")\"; width = 60;") fn)
  (write-line "  : boxed_column { label = \"1. ESCALA VIEWPORT Y AUTOMATIZACI\\U+00D3N\"; : row { : button { label = \"?? Configurar VP Din\\U+00E1mico\"; key = \"btn_mod_vp\"; } : button { label = \"?? Act.\"; key = \"btn_upd_vp\"; width = 6; fixed_width = true; } } }" fn)
  (write-line "  : boxed_column { label = \"2. GRILLA AVANZADA\"; : row { : button { label = \"?? Configurar Grilla\"; key = \"btn_mod_gri\"; } : button { label = \"?? Act.\"; key = \"btn_upd_gri\"; width = 6; fixed_width = true; } } }" fn)
  (write-line "  : boxed_column { label = \"3. FORMATO M\\U+00C1GICO (POR C\\U+00D3DIGO)\"; : row { : edit_box { key = \"txt_codigo\"; label = \"C\\U+00F3digo:\"; edit_width = 18; } : button { label = \"?? Selecc.\"; key = \"btn_pick_code\"; width = 10; fixed_width = true; } } : row { : button { label = \"? Aplicar Formato y Limpiar\"; key = \"btn_upd_formato\"; is_default = true; } : button { label = \"?? Param.\"; key = \"btn_param_formato\"; width = 8; fixed_width = true; } } }" fn)
  (write-line "  : boxed_column { label = \"4. CAJET\\U+00D3N Y ETIQUETADO LAYOUT\"; : row { : button { label = \"?? Generar Cajet\\U+00E1n\"; key = \"btn_cajetin\"; } : button { label = \"?? Ejecutar LAYOUT UBIC.\"; key = \"btn_layout_ubi\"; } } }" fn)
  (write-line "  spacer; : row { : button { label = \"RESTAURAR\"; key = \"btn_clear\"; fixed_width = true; } : button { label = \"Cerrar\"; key = \"cancel\"; is_cancel = true; fixed_width = true; } } }" fn)
  (close fn)

  (setq dcl_status 5 launch_gm nil) 
  (while (> dcl_status 1)
    (setq dcl_id (load_dialog dcl_file))
    (if (not (new_dialog "gestor_master" dcl_id)) (exit))

    (set_tile "txt_codigo" *UBI_TARGET_CODE*)
    (action_tile "btn_mod_vp" "(setq *UBI_TARGET_CODE* (get_tile \"txt_codigo\")) (vlax-ldata-put \"ML_SUITE\" \"TARGET_CODE\" *UBI_TARGET_CODE*) (done_dialog 11)") 
    (action_tile "btn_upd_vp" "(done_dialog 18)") 
    (action_tile "btn_mod_gri" "(done_dialog 13)") (action_tile "btn_upd_gri" "(done_dialog 19)") 
    (action_tile "txt_codigo" "(setq *UBI_TARGET_CODE* $value) (vlax-ldata-put \"ML_SUITE\" \"TARGET_CODE\" *UBI_TARGET_CODE*)")
    (action_tile "btn_pick_code" "(done_dialog 21)")
    (action_tile "btn_upd_formato" "(setq *UBI_TARGET_CODE* (get_tile \"txt_codigo\")) (vlax-ldata-put \"ML_SUITE\" \"TARGET_CODE\" *UBI_TARGET_CODE*) (done_dialog 22)")
    (action_tile "btn_param_formato" "(setq *UBI_TARGET_CODE* (get_tile \"txt_codigo\")) (done_dialog 29)")
    (action_tile "btn_cajetin" "(setq *UBI_TARGET_CODE* (get_tile \"txt_codigo\")) (done_dialog 24)")
    (action_tile "btn_layout_ubi" "(done_dialog 25)")
    (action_tile "btn_clear" "(done_dialog 4)") (action_tile "cancel" "(done_dialog 0)")      

    (setq dcl_status (start_dialog))
    (unload_dialog dcl_id)

    (cond
      ((= dcl_status 11) (vl-catch-all-apply 'UBI_RunVPDinamico) (UBI_AutoUpdateFactor) (setq dcl_status 5))
       ((= dcl_status 18)
        (if (and *UBI_VP_DATA_MEM*
                 (setq vp_info (car *UBI_VP_DATA_MEM*))
                 (vlax-ldata-get (car vp_info) "DYN_VP_CENTER"))
         (progn (setq i 0) (foreach vp_info *UBI_VP_DATA_MEM* (setq new_idx (caddr vp_info)) (if (not new_idx) (setq new_idx "10")) (vl-catch-all-apply 'UBI_ExecuteVPUpdate (list i new_idx)) (setq i (1+ i))))
         (alert "No hay Viewport configurado en este slot.")
       )
       (UBI_AutoUpdateFactor) (setq dcl_status 5)
      )
      ((= dcl_status 13) 
       (UBI_Nuke-Memory)
       (vl-catch-all-apply 'UBI_RunGrilla)
       (setq dcl_status 5)
      )
      ((= dcl_status 19)
       (UBI_Nuke-Memory)
       (if (and *UBI_VP_DATA_MEM* (setq vp_info (car *UBI_VP_DATA_MEM*)))
         (progn
           (setq new_idx (caddr vp_info))
           (if (not new_idx) (setq new_idx (vlax-ldata-get (car vp_info) "DYN_VP_SCALE_IDX")))
           (if (not new_idx) (setq new_idx "10"))
           (setq current_scale_str (nth (atoi new_idx) scales) scl_val (atof current_scale_str))
           
           ;; \\U+00BFAUTO APLICAR PARAMETROS DE ESCALA!
           (setq map_to_use (if (= *UBI_VP_TYPE* "1") *UBI_DATA_MAP_NORMAL* *UBI_DATA_MAP_UBICACION*))
           (setq map_data (assoc current_scale_str map_to_use))
           (if (and map_data (>= (length map_data) 7))
             (progn
               (setq *UBI_sg_intervalo* (rtos (nth 4 map_data) 2 2) 
                     *UBI_sg_altura* (rtos (nth 5 map_data) 2 2) 
                     *UBI_sg_gap* (rtos (nth 6 map_data) 2 2))
             )
           )

           ;; Escaneo e inyecci\\U+00F3n autom\\U+00E1tica por VISTA-X en el Model
           (setq vp_hl_name (strcat "VISTA-" (itoa *UBI_ACTIVE_VP*)))
           (setq old_ctab (getvar "CTAB"))
           (vl-catch-all-apply 'setvar (list "CTAB" "Model"))
           
            (setq found_rect nil ss_model (ssget "X" (list (cons 410 "Model") (cons 0 "LWPOLYLINE"))))
            (if ss_model
              (progn
                (setq j 0)
                (while (and (< j (sslength ss_model)) (not found_rect))
                  (setq tmp_ent (ssname ss_model j) hl_str nil)
                  ;; FIX-AV: wrapper nominal del escaneo, liberado por iteracion.
                  (setq tmp_obj (vl-catch-all-apply 'vlax-ename->vla-object (list tmp_ent)))
                  (if (vl-catch-all-error-p tmp_obj)
                    (setq tmp_obj nil)
                    (setq hl_str (UBI_GetHyperlinkString tmp_obj)))
                  (vl-catch-all-apply 'vlax-release-object (list tmp_obj))
                  (setq tmp_obj nil)
                  (if (and hl_str (= (strcase hl_str) (strcase vp_hl_name)))
                    (setq found_rect tmp_ent))
                  (setq j (1+ j))
                )
                (setq ss_model nil)
              )
            )

           (if found_rect
             (progn
               ;; Forzar la creaci\\U+00F3n y selecci\\U+00F3n predeterminada de ST-COORP
               (setq *UBI_sg_idx_style* (UBI_GetStyleIndex "ST-COORP"))
               (vl-catch-all-apply 'UBI_ExecuteGridGeneration (list found_rect scl_val current_scale_str))
             )
             (alert (strcat "No se encontr\\U+00F3 el recuadro con hiperv\\U+00EDnculo [" vp_hl_name "] en el Model."))
           )
           
           (vl-catch-all-apply 'setvar (list "CTAB" old_ctab))
         )
         (alert "No hay Viewport configurado. Usa 'Configurar VP Din\\U+00E1mico' primero.")
       )
       (setq dcl_status 5)
      )
      ((= dcl_status 21)
       (setq old_ctab (getvar "CTAB")) (vl-catch-all-apply 'setvar (list "CTAB" "Model"))
       (setq txt_catch (vl-catch-all-apply 'entsel (list "\nSeleccione el Texto o MText con el c\\U+00F3digo (ESC para volver): ")))
       (if (and (not (vl-catch-all-error-p txt_catch)) (setq sel_txt (car txt_catch)))
         (progn
           ;; F15-R1: wrapper con catch y liberado tras capturar el codigo.
           (setq obj_txt (vl-catch-all-apply 'vlax-ename->vla-object (list sel_txt)))
           (if (vl-catch-all-error-p obj_txt)
             (alert "El objeto seleccionado no es un texto v\\U+00E1lido.")
             (progn
               (if (or (= (vla-get-ObjectName obj_txt) "AcDbText") (= (vla-get-ObjectName obj_txt) "AcDbMText"))
                 (progn (setq txt_val (vla-get-TextString obj_txt)) (while (vl-string-search "\\P" txt_val) (setq txt_val (vl-string-subst "" "\\P" txt_val))) (setq *UBI_TARGET_CODE* (vl-string-trim " \r\n" txt_val)) (vlax-ldata-put "ML_SUITE" "TARGET_CODE" *UBI_TARGET_CODE*) (princ (strcat "\nC\\U+00F3digo capturado: " *UBI_TARGET_CODE*)))
                 (alert "El objeto seleccionado no es un texto v\\U+00E1lido."))
               (GM:ReleaseObjectSafe obj_txt)))
         )
       )
       (vl-catch-all-apply 'setvar (list "CTAB" old_ctab)) (setq dcl_status 5)
      )
      ((= dcl_status 22)
       (if (or (= *UBI_TARGET_CODE* "") (not *UBI_TARGET_CODE*))
         (alert "\\U+00BFDebes escribir o seleccionar un C\\U+00F3digo primero!")
         (progn
           (if (and *UBI_VP_DATA_MEM* (setq vp_info (car *UBI_VP_DATA_MEM*)))
             (progn
               (setq new_idx (caddr vp_info)) (if (not new_idx) (setq new_idx "10"))
               (setq current_scale_str (nth (atoi new_idx) scales) scl_val (atof current_scale_str))
               (setq old_ctab (getvar "CTAB")) (vl-catch-all-apply 'setvar (list "CTAB" "Model"))
               (vl-catch-all-apply 'UBI_CleanByHyperlinks (list *UBI_TARGET_CODE*))
               (vl-catch-all-apply 'UBI_ApplyAutoFormat (list *UBI_TARGET_CODE* current_scale_str scl_val))
               (vl-catch-all-apply 'setvar (list "CTAB" old_ctab))
             )
             (alert "No hay Viewport configurado. Inicia 'Configurar VP Din\\U+00E1mico' para obtener la escala.")
           )
         )
       )
       (setq dcl_status 5)
      )

      ;; --- NUEVA SECCI\\U+00D3N DE PAR\\U+00C1METROS ESPECIALES DE FORMATO ---
      ((= dcl_status 29)
       (setq dcl_file_param (vl-filename-mktemp "param_formato_ubi.dcl") fn_param (open dcl_file_param "w"))
       (write-line "format_params : dialog { label = \"?? PAR\\U+00C1METROS AVANZADOS DE FORMATO\";" fn_param)
       (write-line "  : edit_box { key = \"txt_prog_offset\"; label = \"Dist. Progresiva a Estaca (x Altura Texto):\"; edit_width = 8; }" fn_param)
       (write-line "  : edit_box { key = \"txt_stake_scale\"; label = \"Tama\\U+00F1o de Estacas (Multiplicador):\"; edit_width = 8; }" fn_param)
       (write-line "  : edit_box { key = \"txt_text_scale\"; label = \"Tama\\U+00F1o Global Textos (Multiplicador):\"; edit_width = 8; }" fn_param)
       (write-line "  ok_cancel;" fn_param)
       (write-line "}" fn_param)
       (close fn_param)
       
       (setq dcl_id_param (load_dialog dcl_file_param))
       (if (new_dialog "format_params" dcl_id_param)
         (progn
           (set_tile "txt_prog_offset" *UBI_PROG_OFFSET_MULT*)
           (set_tile "txt_stake_scale" *UBI_STAKE_SCALE_FACTOR*)
           (set_tile "txt_text_scale" *UBI_TEXT_SCALE_MULT*)
           (action_tile "txt_prog_offset" "(setq *UBI_PROG_OFFSET_MULT* $value)")
           (action_tile "txt_stake_scale" "(setq *UBI_STAKE_SCALE_FACTOR* $value)")
           (action_tile "txt_text_scale" "(setq *UBI_TEXT_SCALE_MULT* $value)")
           (action_tile "accept" "(done_dialog 1)")
           (action_tile "cancel" "(done_dialog 0)")
           (start_dialog)
         )
       )
       (unload_dialog dcl_id_param)
       (vl-file-delete dcl_file_param)
       (setq dcl_status 5)
      )

      ((= dcl_status 24) (setq launch_gm T dcl_status 0))
      ((= dcl_status 25)
       (if (type c:LAYOUT_UBICACION)
         (c:LAYOUT_UBICACION)
         (alert "El comando LAYOUT_UBICACION no est\\U+00E1 cargado o no existe en el sistema.")
       )
       (setq dcl_status 5)
      )
      ((= dcl_status 4) (UBI_RestoreHiddenByGM) (setq *UBI_VP_DICT* '() *UBI_VP_DATA_MEM* '() *UBI_SAFE_LAYERS* '() *UBI_TARGET_CODE* "") (vlax-ldata-put "ML_SUITE" "TARGET_CODE" "") (princ "\nVisibilidad restaurada y memoria limpia.") (setq dcl_status 5))
    )
  )
  
  (setq *error* olderr) (vl-catch-all-apply 'setvar (list "SELECTIONEFFECT" 1))
  (vl-file-delete dcl_file) (UBI_Nuke-Memory)
  
  (if launch_gm
    (if (not (type c:MENU_PLANOS))
      (progn (princ "\n[SISTEMA] Cargue el LISP (MENU_PLANOS)...\n") (command "_.APPLOAD") (if (type c:MENU_PLANOS) (c:MENU_PLANOS)))
      (c:MENU_PLANOS)
    )
  )
  (princ)
)

(defun c:PLANO_UBICACION_AUTO (/ code vp_ent vp_info scale_idx current_scale_str scale_val scales)
  ;; Encadena las operaciones mecanicas y deja las posiciones visuales para Layout.
  (setq code (getstring T "\nCodigo del predio para AUTO <memoria>: "))
  (if (= code "") (setq code (vlax-ldata-get "ML_SUITE" "TARGET_CODE")))
  (setq code (if (= (type code) 'STR) (vl-string-trim " \t\n\r" code) ""))
  (if (= code "")
    (alert "Debe indicar un codigo de predio.")
    (progn
      (vlax-ldata-put "ML_SUITE" "TARGET_CODE" code)
      (setq *UBI_TARGET_CODE* code)
      (UBI_RestoreDWGMemory)
       (if (= (getvar "CTAB") "Model")
         (alert "PLANO_UBICACION_AUTO debe ejecutarse desde un Layout.")
         (progn
           ;; AUTO parte de una configuracion nueva para no reutilizar el codigo anterior.
           (setq *UBI_VP_DATA_MEM* nil *UBI_VP_DICT* nil)
           (setq vp_ent (UBI_FindViewportInCurrentLayout))
          (if (not vp_ent)
             (alert "No se pudo detectar un viewport unico o persistente en el Layout activo.")
             (progn
               (vlax-ldata-put vp_ent "DYN_VP_CENTER" nil)
               (vlax-ldata-put vp_ent "DYN_VP_SCALE_IDX" "11")
               (UBI_ProcessSelectedVP vp_ent)
               (if *UBI_VP_DATA_MEM*
                 (progn
                   ;; La escala preliminar del lote es fija: indice 11 = 1/1250.
                   (setq vp_info (car *UBI_VP_DATA_MEM*) scale_idx "11")
                   (UBI_ExecuteVPUpdate 0 scale_idx)
                  (setq scales '("20" "25" "50" "75" "100" "125" "200" "250" "500" "750" "1000" "1250" "2000" "2500" "5000" "7500" "10000" "12500" "20000" "25000")
                        current_scale_str (nth (atoi scale_idx) scales)
                        scale_val (atof current_scale_str))
                  (UBI_CleanByHyperlinks code)
                  (UBI_ApplyAutoFormat code current_scale_str scale_val)
                  (princ "\nFases mecanicas completadas. Se abrira el etiquetado Layout para las posiciones de criterio.")
                  (if (type c:LAYOUT_UBICACION)
                    (c:LAYOUT_UBICACION)
                    (princ "\nLAYOUT_UBICACION no esta cargado; ejecutelo manualmente.")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(setq *GM_VER_PLANO* "2026-10-10-CHG3")
(princ "\nSUITE MAESTRA UBICACI\\U+00D3N V9.8 CARGADA (Auto-Update de Escala en Cuadro). Escribe PLANO_UBICACION para iniciar.")
(princ)
