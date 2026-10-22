;; ============================================================================
;; UBI_VP.lsp - Sistema GM Ingenieros (modulo ubi)
;; Movido SIN CAMBIOS desde PLANO_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


;; ==============================================================================
;; VP DIN\\U+00C1MICO REESCRITO AL METODO MATRIZ (VISTA-1 HIPERVINCULADA)
;; ==============================================================================

(defun UBI_ProcessSelectedVP (vp_ent / vp_obj cenpt scale_idx ans poly poly_catch reference)
  ;; AUD1: creacion con catch (VP invalido no aborta el comando manual).
  (setq vp_obj (vl-catch-all-apply 'vlax-ename->vla-object (list vp_ent)))
  (if (vl-catch-all-error-p vp_obj) (setq vp_obj nil))
  (if vp_obj (vla-put-DisplayLocked vp_obj :vlax-false))
  (vlax-ldata-put vp_ent "ML_VP_NUM" *UBI_ACTIVE_VP*)
  (setq cenpt (vlax-ldata-get vp_ent "DYN_VP_CENTER") scale_idx (vlax-ldata-get vp_ent "DYN_VP_SCALE_IDX")) 
  
  (if cenpt (progn (initget "Si No") (setq ans (vl-catch-all-apply 'getkword (list "\n\\U+00BFSeleccionar NUEVO pol\\U+00EDgono centro? [Si/No] <No>: "))) (if (and (not (vl-catch-all-error-p ans)) (= ans "Si")) (setq cenpt nil))))
  
  (if (not cenpt)
    (progn 
      (command "_.mspace")
      ;; P0-3 2026-09-29: entget nil-safe (viewport borrado/invalido no debe abortar).
      (if (and vp_ent (entget vp_ent)) (setvar "CVPORT" (cdr (assoc 69 (entget vp_ent)))))
      
      ;; BUSQUEDA AUTOM\\U+00C1TICA
      (setq poly nil)
      (if (and *UBI_TARGET_CODE* (/= *UBI_TARGET_CODE* ""))
        (progn
          (princ (strcat "\nBuscando autom\\U+00E1ticamente el pol\\U+00EDgono (" *UBI_TARGET_CODE* ") en el Model..."))
           (setq reference *UBI_TARGET_REFERENCE*)
           (setq poly (if (and *UBI_TARGET_POLYGON* (entget *UBI_TARGET_POLYGON*)) *UBI_TARGET_POLYGON*
                       (if (/= reference "")
                        (UBI_FindPolygonByCodeOrReference *UBI_TARGET_CODE* reference)
                        (UBI_FindPolygonByCode *UBI_TARGET_CODE*))))
        )
      )
      
       (if (and (not poly) (not *GM_AUTO_MODE*))
         (progn
           (setq poly_catch (vl-catch-all-apply 'entsel (list "\nSeleccione el Pol\\U+00EDgono para centrar (ESC para omitir): ")))
           (if (and (not (vl-catch-all-error-p poly_catch)) poly_catch) (setq poly (car poly_catch)))
         )
         (if poly (princ "\n\\U+00BFPol\\U+00EDgono detectado y centrado autom\\U+00E1ticamente!")
           (princ "\nNo se encontr\\U+00F3 el pol\\U+00EDgono del c\\U+00F3digo activo.")
         )
       )

      (if poly
        (if (setq cenpt (UBI_GetPolygonCenter poly))
          (progn
             (vlax-ldata-put vp_ent "DYN_VP_CENTER" cenpt)
             (setq *UBI_TARGET_POLYGON* poly)
            (if (not scale_idx) (setq scale_idx (UBI_SuggestScaleIndex poly)))
          )
          (princ "\nNo se pudo calcular el centro del poligono; se requiere seleccion manual.")
        )
      )
      (command "_.pspace") 
    )
  )
  ;; H6 (PA-1): respaldo manual 1/1000 documentado; el lote usa UBI_AutoScaleIndex.
  (if (not scale_idx) (setq scale_idx "10"))
  (setq *UBI_VP_DATA_MEM* (list (list vp_ent cenpt scale_idx)) *UBI_VP_DICT* (vl-remove (assoc *UBI_ACTIVE_VP* *UBI_VP_DICT*) *UBI_VP_DICT*) *UBI_VP_DICT* (append *UBI_VP_DICT* (list (cons *UBI_ACTIVE_VP* *UBI_VP_DATA_MEM*))))
  ;; Fase 1B (H-5): liberar el wrapper del viewport.
  (vl-catch-all-apply 'vlax-release-object (list vp_obj))
  (setq vp_obj nil)
)

(defun UBI_ExecuteVPUpdate (update_vp_idx new_scale_idx / vp_info vp_ent cenpt vp_obj scales current_scale_str scale_val old_rect_handle e_rect ps_w ps_h ms_w ms_h p1_ms p2_ms p3_ms p4_ms old_osmode old_cecolor ms_rect vp_hl_name temp_ctab handle_cuadro ent_cuadro vla_cuadro ms_rect_obj ms_rect_hls ok)
  (setq vp_info (nth update_vp_idx *UBI_VP_DATA_MEM*) vp_ent (car vp_info) cenpt (vlax-ldata-get vp_ent "DYN_VP_CENTER"))
  ;; AUD1: creacion con catch; el release de cierre ya era nil-safe.
  (setq vp_obj (vl-catch-all-apply 'vlax-ename->vla-object (list vp_ent)))
  (if (vl-catch-all-error-p vp_obj) (setq vp_obj nil))
  (setq scales '("20" "25" "50" "75" "100" "125" "200" "250" "500" "750" "1000" "1250" "2000" "2500" "5000" "7500" "10000" "12500" "20000" "25000"))
  (setq current_scale_str (nth (atoi new_scale_idx) scales) scale_val (atof current_scale_str))
  
  (setq *UBI_VP_DATA_MEM* (subst (list vp_ent cenpt new_scale_idx) vp_info *UBI_VP_DATA_MEM*))
  (vlax-ldata-put vp_ent "DYN_VP_SCALE_IDX" new_scale_idx) (if cenpt (vlax-ldata-put vp_ent "DYN_VP_CENTER" cenpt))
  (setq *UBI_VP_DICT* (vl-remove (assoc *UBI_ACTIVE_VP* *UBI_VP_DICT*) *UBI_VP_DICT*) *UBI_VP_DICT* (append *UBI_VP_DICT* (list (cons *UBI_ACTIVE_VP* *UBI_VP_DATA_MEM*))))
  
  (setq old_rect_handle (vlax-ldata-get vp_ent "DYN_VP_RECT")) 
  (if (and old_rect_handle (setq e_rect (handent old_rect_handle)) (entget e_rect)) (entdel e_rect))
  
  (if (and cenpt vp_obj)
    (progn
      ;; AUD1: lecturas con catch (VP caido = se conserva el flujo de cierre).
      (setq ps_w (vl-catch-all-apply 'vla-get-Width (list vp_obj))
            ps_h (vl-catch-all-apply 'vla-get-Height (list vp_obj)))
      (if (or (vl-catch-all-error-p ps_w) (vl-catch-all-error-p ps_h)
              (not (numberp ps_w)) (not (numberp ps_h)))
        (setq ps_w 278.95 ps_h 257.709))
      (setq ms_w (* ps_w (/ scale_val 1000.0)) ms_h (* ps_h (/ scale_val 1000.0)))
      (setq p1_ms (list (- (car cenpt) (/ ms_w 2.0)) (- (cadr cenpt) (/ ms_h 2.0)) 0.0)
            p2_ms (list (+ (car cenpt) (/ ms_w 2.0)) (- (cadr cenpt) (/ ms_h 2.0)) 0.0)
            p3_ms (list (+ (car cenpt) (/ ms_w 2.0)) (+ (cadr cenpt) (/ ms_h 2.0)) 0.0)
            p4_ms (list (- (car cenpt) (/ ms_w 2.0)) (+ (cadr cenpt) (/ ms_h 2.0)) 0.0))

      (command "_.mspace") 
      ;; P0-3 2026-09-29: entget nil-safe (viewport invalido no debe abortar el zoom).
      (if (and vp_ent (entget vp_ent)) (setvar "CVPORT" (cdr (assoc 69 (entget vp_ent)))))
      (command "_.zoom" "_c" "_NON" cenpt (strcat "1000/" current_scale_str "xp"))
      
      (setq old_osmode (getvar "OSMODE") old_cecolor (getvar "CECOLOR")) 
      (setvar "OSMODE" 0) (setvar "CECOLOR" "BYBLOCK")
      
      (setq vp_hl_name (strcat "VISTA-" (itoa *UBI_ACTIVE_VP*)))
      (if (not (tblsearch "LAYER" vp_hl_name)) (command "_.-layer" "_m" vp_hl_name "_color" "1" "" ""))
      
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") 
                     (cons 8 vp_hl_name) '(90 . 4) '(70 . 1) (cons 43 0.0) (cons 62 0) 
                     (list 10 (car p1_ms) (cadr p1_ms)) (list 10 (car p2_ms) (cadr p2_ms)) 
                     (list 10 (car p3_ms) (cadr p3_ms)) (list 10 (car p4_ms) (cadr p4_ms))))
      
      (setq ms_rect (entlast))
      ;; Fase 1B (H-5): wrappers nominales del rectangulo y sus hyperlinks,
      ;; liberados al terminar de usarlos. AUD1: cadena con catch.
      (setq ms_rect_obj (vl-catch-all-apply 'vlax-ename->vla-object (list ms_rect)))
      (if (vl-catch-all-error-p ms_rect_obj) (setq ms_rect_obj nil))
      (setq ms_rect_hls (if ms_rect_obj (vl-catch-all-apply 'vla-get-hyperlinks (list ms_rect_obj)) nil))
      (if (vl-catch-all-error-p ms_rect_hls) (setq ms_rect_hls nil))
      (if ms_rect_hls (vla-add ms_rect_hls vp_hl_name vp_hl_name vp_hl_name))
      (vl-catch-all-apply 'vlax-release-object (list ms_rect_hls))
      (vl-catch-all-apply 'vlax-release-object (list ms_rect_obj))
      (setq ms_rect_hls nil ms_rect_obj nil)
      
      (vlax-ldata-put vp_ent "DYN_VP_RECT" (cdr (assoc 5 (entget ms_rect))))
      
      (setvar "OSMODE" old_osmode) (setvar "CECOLOR" old_cecolor)
      (command "_.pspace")
      
      ;; POR QUE este bloque: fuerza REGENALL tras actualizar el VP para que el
      ;; usuario vea la escala aplicada de inmediato. No recalcula textos aqui;
      ;; el formato de textos/estacas vive solo en UBI_ApplyAutoFormat.
      ;; Fase 1D (H-8): en modo lote se omite; el PDF depende de la definicion
      ;; del viewport (zoom xp) y no del regen en pantalla.
      (if (not *GM_AUTO_MODE*)
        (progn
          (setq temp_ctab (getvar "CTAB"))
          (vl-catch-all-apply 'setvar (list "CTAB" "Model"))
          (vl-catch-all-apply 'setvar (list "CTAB" temp_ctab))
          (command "_.REGENALL")))
      
      ;; --- REESCRITURA AUTOM\\U+00C1TICA DEL CUADRO DE ESCALA ---
      (setq handle_cuadro (vlax-ldata-get vp_ent "DYN_VP_CUADRO_TXT"))
      (if (and handle_cuadro (setq ent_cuadro (handent handle_cuadro)) (entget ent_cuadro))
        (progn
          ;; AUD1: creacion con catch; entidad caida = se omite la reescritura.
          (setq vla_cuadro (vl-catch-all-apply 'vlax-ename->vla-object (list ent_cuadro)))
          (if (vl-catch-all-error-p vla_cuadro) (setq vla_cuadro nil))
          (if vla_cuadro
            (progn
              (vl-catch-all-apply 'vla-put-TextString (list vla_cuadro (strcat "ESCALA: 1/" current_scale_str)))
              (vl-catch-all-apply 'vla-Update (list vla_cuadro))
              ;; FIX-AV: soltar el wrapper del cuadro tras actualizarlo.
              (vl-catch-all-apply 'vlax-release-object (list vla_cuadro))))
        )
      )
      (setq ok T)
    )
  )
  ;; Fase 1B (H-5): liberar el wrapper del viewport.
  (vl-catch-all-apply 'vlax-release-object (list vp_obj))
  (setq vp_obj nil)
  ;; F: mensaje honesto; solo se anuncia sincronizado si realmente se ejecuto
  ;; el bloque de escala/recuadro. El retorno T/nil lo usan la UI y el lote.
  (if ok
    (princ (strcat "\nVP sincronizado con recuadro a escala 1/" current_scale_str))
    (princ "\nVP NO actualizado: falto el centro o el viewport valido."))
  ok
)

(defun UBI_FindViewportInCurrentLayout (/ ss i ent ed vp_num candidates tagged)
  ;; Ignora el viewport papel (numero 1) y solo revisa el layout activo.
  (setq candidates '() tagged nil)
  (setq ss (ssget "X" (list (cons 0 "VIEWPORT") (cons 410 (getvar "CTAB")))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i) ed (entget (ssname ss i)) vp_num (cdr (assoc 69 ed)))
        (if (/= vp_num 1)
          (progn
            (setq candidates (cons ent candidates))
            (if (= (UBI_SafeLDataGet ent "ML_VP_NUM" 0) *UBI_ACTIVE_VP*)
              (setq tagged ent)
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (cond (tagged tagged) ((= (length candidates) 1) (car candidates)) (T nil))
)

(defun UBI_RunVPDinamico ( / app doc prev_ctab scales dcl_file fn dcl_id loop status vp_ent ans new_scale_idx vp_sel current_scale_str)
  ;; F15-R1: doc/app con catch y liberados antes de salir (flujo manual).
  (setq app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (vl-catch-all-error-p app) (setq app nil))
  (setq doc (if app (vl-catch-all-apply 'vla-get-activedocument (list app)) nil))
  (if (vl-catch-all-error-p doc) (setq doc nil))
  (setq prev_ctab (getvar "CTAB"))
  (if (= prev_ctab "Model") (progn (alert "VP Din\\U+00E1mico debe ejecutarse en un Layout (Paper Space).") (exit)))
  (if (/= (getvar "CVPORT") 1) (command "_.pspace"))
  
  (setq scales '("20" "25" "50" "75" "100" "125" "200" "250" "500" "750" "1000" "1250" "2000" "2500" "5000" "7500" "10000" "12500" "20000" "25000"))
  
  ;; === L\\U+00D3GICA DE SELECCI\\U+00D3N DIRECTA REVERTIDA ===
  (if *UBI_VP_DATA_MEM* (progn 
      (initget "Si No") 
      (setq ans (vl-catch-all-apply 'getkword (list "\nYa hay config. \\U+00BFDesea usar la misma? [Si/No] <Si>: "))) 
      (if (or (vl-catch-all-error-p ans) (not ans) (= ans "Si")) (setq ans "Si") (setq *UBI_VP_DATA_MEM* '()))
    )
  )
  
  (if (not *UBI_VP_DATA_MEM*)
    (progn 
      (princ (strcat "\n--- Configurando Viewport " (itoa *UBI_ACTIVE_VP*) " ---"))
      (setq vp_ent (UBI_FindViewportInCurrentLayout))
      (if vp_ent
        (progn
          (princ "\nViewport detectado automaticamente en el layout activo.")
          (UBI_ProcessSelectedVP vp_ent)
        )
        (progn
          (setq vp_sel (vl-catch-all-apply 'entsel (list "\nSeleccione el Viewport (ESC para omitir): ")))
          (if (and (not (vl-catch-all-error-p vp_sel)) vp_sel (setq vp_ent (car vp_sel)) (= (cdr (assoc 0 (entget vp_ent))) "VIEWPORT"))
            (UBI_ProcessSelectedVP vp_ent)
          )
        )
      )
    )
  )

  (if *UBI_VP_DATA_MEM*
    (progn
      (setq dcl_file (vl-filename-mktemp "vpdyn.dcl"))
      (setq fn (open dcl_file "w"))
      (write-line "VPDyn : dialog { label = \"Gestor de Escalas y Viewport\"; width = 45;" fn)
      (write-line "  : boxed_column { label = \"Configurar Escala\"; " fn)
      (write-line "    : radio_row { label = \"Tipo:\"; : radio_button { key = \"rb_norm\"; label = \"ESCALAS <2500\"; } : radio_button { key = \"rb_ubic\"; label = \"ESCALAS >=2500\"; } }" fn)
      (write-line "    : row { : popup_list { key = \"scale_vp_0\"; edit_width = 10; } : button { key = \"btn_cen_0\"; label = \"Actualizar\"; is_default = true; } }" fn)
      (write-line "  }" fn)
      (write-line "  : boxed_row { label = \"Etiquetado\"; " fn)
      (write-line "    : button { key = \"btn_insert_esc\"; label = \"??? Insertar Cuadro Escala\"; alignment = centered; } " fn)
      (write-line "  } ok_only; }" fn)
      (close fn)

      (setq loop T)
      (while loop
        (setq dcl_id (load_dialog dcl_file))
        (if (not (new_dialog "VPDyn" dcl_id)) (exit))
        
        (if (= *UBI_VP_TYPE* "1") (set_tile "rb_norm" "1") (set_tile "rb_ubic" "1"))
        (action_tile "rb_norm" "(setq *UBI_VP_TYPE* \"1\")") 
        (action_tile "rb_ubic" "(setq *UBI_VP_TYPE* \"2\")")
        
        (start_list "scale_vp_0") (mapcar 'add_list scales) (end_list) 
        (set_tile "scale_vp_0" (caddr (car *UBI_VP_DATA_MEM*)))
        
        (action_tile "scale_vp_0" "(setq new_scale_idx $value)")
        (action_tile "btn_cen_0" "(setq new_scale_idx (get_tile \"scale_vp_0\")) (done_dialog 3)")
        (action_tile "btn_insert_esc" "(setq new_scale_idx (get_tile \"scale_vp_0\")) (done_dialog 6)")
        
        (setq status (start_dialog))
        (unload_dialog dcl_id)
        
        (cond 
          ((or (= status 0) (= status 1)) (setq loop nil)) 
          ((= status 3) 
           (if (UBI_ExecuteVPUpdate 0 new_scale_idx)
             (alert "Viewport y Textos/Estacas actualizado correctamente.")
             (alert "No se pudo actualizar el viewport. Revisa el centro y la escala."))
          )
          ((= status 6) ;; Cuadro de Escala
           (setq current_scale_str (nth (atoi new_scale_idx) scales))
           (UBI_InsertarCuadroEscala (car (car *UBI_VP_DATA_MEM*)) current_scale_str)
          )
        )
      )
      (vl-file-delete dcl_file)
    )
  )
  (GM:ReleaseObjectSafe doc)
  (GM:ReleaseObjectSafe app)
)