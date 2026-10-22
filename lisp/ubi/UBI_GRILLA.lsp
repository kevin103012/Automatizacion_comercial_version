;; ============================================================================
;; UBI_GRILLA.lsp - Sistema GM Ingenieros (modulo ubi)
;; Movido SIN CAMBIOS desde PLANO_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


(defun UBI_AutoUpdateFactor ( / vp_info vp_obj c_scale)
  (if (and *UBI_VP_DATA_MEM* (> (length *UBI_VP_DATA_MEM*) 0) (setq vp_info (car *UBI_VP_DATA_MEM*)))
    (if (and (car vp_info) (entget (car vp_info)))
      (progn 
        ;; F15-R1: wrapper con catch y liberado al leer la escala.
        (setq vp_obj (vl-catch-all-apply 'vlax-ename->vla-object (list (car vp_info))))
        (if (not (vl-catch-all-error-p vp_obj))
          (progn
            ;; G: CustomScale debe ser numero y > 0 antes de dividir 1.0/c_scale.
            (if (not (vl-catch-all-error-p (setq c_scale (vl-catch-all-apply 'vla-get-CustomScale (list vp_obj)))))
              (if (and (numberp c_scale) (> c_scale 0.0))
                (setq *UBI_FACTOR* (rtos (/ 1.0 c_scale) 2 4))))
            (GM:ReleaseObjectSafe vp_obj)))
      )
    )
  )
)

;; ==============================================================================
;; MOTOR DE GRILLA AVANZADO (VISTA-1 HIPERVINCULADA)
;; ==============================================================================
(defun UBI_get-poly-vertices (ename / elist vertices item)
  (setq elist (entget ename))
  (foreach item elist 
    (if (= (car item) 10) 
      (setq vertices (cons (list (cadr item) (caddr item) 0.0) vertices))
    )
  )
  (reverse vertices)
)

(defun UBI_get-all-intersections (p1 p2 verts / i v1 v2 pt pts)
  (setq pts '() i 0)
  (repeat (length verts)
    (setq v1 (nth i verts) v2 (nth (rem (1+ i) (length verts)) verts) pt (inters p1 p2 v1 v2 T))
    (if pt 
      (if (not (vl-member-if '(lambda (x) (< (distance x pt) 0.001)) pts)) 
        (setq pts (cons pt pts))
      )
            )
            (setq i (1+ i))
  )
  pts
)

(defun UBI_create_style_st_coorp ()
  (if (not (tblsearch "STYLE" "ST-COORP"))
    (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") '(2 . "ST-COORP") '(70 . 0) '(40 . 0.0) '(41 . 1.1) '(50 . 0.0) '(71 . 0) '(3 . "romans.shx") '(4 . "")))
  )
)

(defun UBI_GetStyleIndex (style_name / item styles_list n found_idx s)
  (UBI_create_style_st_coorp)
  (setq styles_list '() item (tblnext "STYLE" T))
  (while item (setq styles_list (cons (cdr (assoc 2 item)) styles_list) item (tblnext "STYLE")))
  (setq styles_list (vl-sort styles_list '<) n 0 found_idx "0")
  (foreach s styles_list (if (= (strcase s) (strcase style_name)) (setq found_idx (itoa n))) (setq n (1+ n)))
  found_idx
)

(defun UBI_GenerateGridCore (pl_ent scl_val final_intervalo final_altura final_gap all_grid_lines all_grid_texts / pl_verts all_x all_y minx maxx miny maxy final_estilo final_layer off_vert_line off_horiz_line off_poly_border safe_buffer curr_x curr_y pt_list p_start p_end styles_list n found_idx item style_item draw_coord_text draw_line failed_creations)
  ;; E1: scl_val es el denominador de escala (p.ej. 1250.0). Se valida fail-closed;
  ;; no se usa para el trazado porque la grilla deriva de final_intervalo/altura/gap del mapa.
  (if (or (not pl_ent) (not (and (numberp scl_val) (> scl_val 0.0))) (not (and final_intervalo (> final_intervalo 0.0)))
          (not (and final_altura (> final_altura 0.0)))
          (not (and final_gap (>= final_gap 0.0))))
    (progn
      (princ "\n[ERROR] Par\\U+00E1metros de grilla inv\\U+00E1lidos: intervalo y altura deben ser positivos.")
      nil)
    (progn
      (setq pl_verts (UBI_get-poly-vertices pl_ent))
      (if (< (length pl_verts) 3)
        (progn
          (princ "\n[ERROR] El contorno no tiene suficientes v\\U+00E9rtices.")
          nil)
        (progn
          (setq all_x (mapcar 'car pl_verts) all_y (mapcar 'cadr pl_verts))
          (setq minx (apply 'min all_x) maxx (apply 'max all_x) miny (apply 'min all_y) maxy (apply 'max all_y))
  
  (setq styles_list '() item (tblnext "STYLE" T))
  (while item (setq styles_list (cons (cdr (assoc 2 item)) styles_list) item (tblnext "STYLE")))
  (setq styles_list (vl-sort styles_list '<) n 0 found_idx "0")
  (foreach style_item styles_list (if (= (strcase style_item) "ST-COORP") (setq found_idx (itoa n))) (setq n (1+ n)))
  (if (not *UBI_sg_idx_style*) (setq *UBI_sg_idx_style* found_idx))
  
  (setq final_estilo (if *GM_AUTO_MODE* "ST-COORP" (or (nth (atoi *UBI_sg_idx_style*) styles_list) "ST-COORP")) final_layer *UBI_sg_layer_name*)
  (setq off_vert_line (* 1.6 final_altura) off_horiz_line (* 0.8 final_altura) off_poly_border final_gap safe_buffer 1000000.0)
  
  (defun draw_coord_text (pt val rotation align) 
    (if (entmake (list (cons 0 "TEXT") (cons 8 final_layer) (cons 10 pt) (cons 40 final_altura) (cons 1 (rtos val 2 0)) (cons 50 rotation) (cons 7 final_estilo) (cons 62 *UBI_sg_col_txt*) (cons 72 align) (cons 73 0) (cons 11 pt)))
      (ssadd (entlast) all_grid_texts)
      (setq failed_creations T))
  ) 
  
  (defun draw_line (pt1 pt2) 
    (if (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                   (cons 8 final_layer) '(90 . 2) '(70 . 0)
                    (cons 370 13) (cons 43 0.0) (cons 62 *UBI_sg_col_grid*)
                    (cons 10 (list (car pt1) (cadr pt1)))
                    (cons 10 (list (car pt2) (cadr pt2)))))
      (ssadd (entlast) all_grid_lines)
      (setq failed_creations T))
  )
  
  (setq curr_x (* (fix (/ minx final_intervalo)) final_intervalo))
  (if (< curr_x minx) (setq curr_x (+ curr_x final_intervalo)))
  (while (<= curr_x maxx)
    (setq pt_list (UBI_get-all-intersections (list curr_x (- miny safe_buffer) 0.0) (list curr_x (+ maxy safe_buffer) 0.0) pl_verts) pt_list (vl-sort pt_list '(lambda (a b) (< (cadr a) (cadr b)))))
    (if (>= (length pt_list) 2)
      (progn 
          (setq p_start (car pt_list) p_end (last pt_list))
        (draw_line p_start p_end)
        (if (= *UBI_sg_chk_inf* "1") (draw_coord_text (list (+ (car p_start) off_vert_line) (+ (cadr p_start) off_poly_border) 0.0) curr_x (* 0.5 pi) 0))
        (if (= *UBI_sg_chk_sup* "1") (draw_coord_text (list (+ (car p_end) off_vert_line) (- (cadr p_end) off_poly_border) 0.0) curr_x (* 0.5 pi) 2))
      )
    )
    (setq curr_x (+ curr_x final_intervalo))
  )
  
  (setq curr_y (* (fix (/ miny final_intervalo)) final_intervalo))
  (if (< curr_y miny) (setq curr_y (+ curr_y final_intervalo)))
  (while (<= curr_y maxy)
    (setq pt_list (UBI_get-all-intersections (list (- minx safe_buffer) curr_y 0.0) (list (+ maxx safe_buffer) curr_y 0.0) pl_verts) pt_list (vl-sort pt_list '(lambda (a b) (< (car a) (car b)))))
    (if (>= (length pt_list) 2)
      (progn 
          (setq p_start (car pt_list) p_end (last pt_list))
        (draw_line p_start p_end)
        (if (= *UBI_sg_chk_izq* "1") (draw_coord_text (list (+ (car p_start) off_poly_border) (+ (cadr p_start) off_horiz_line) 0.0) curr_y 0.0 0))
        (if (= *UBI_sg_chk_der* "1") (draw_coord_text (list (- (car p_end) off_poly_border) (+ (cadr p_end) off_horiz_line) 0.0) curr_y 0.0 2))
      )
    )
    (setq curr_y (+ curr_y final_intervalo))
  )
          (not failed_creations)
        )
      )
    )
  )
)

(defun UBI_ExecuteGridGeneration (manual_pl_ent current_scl_val current_scale_str / all_grid_lines all_grid_texts vp_info old_rect_handle ss_old e_rect target_poly)
  (setvar "CTAB" "Model")
  (UBI_create_style_st_coorp)
  (setq *UBI_sg_layer_name* (strcat "GRILLA-VP" (itoa *UBI_ACTIVE_VP*)))
  
  (setq target_poly nil)
  (if manual_pl_ent
    (setq target_poly (list manual_pl_ent current_scl_val current_scale_str (atof *UBI_sg_intervalo*) (atof *UBI_sg_altura*) (atof *UBI_sg_gap*)))
    (progn
      (if (and *UBI_VP_DATA_MEM* (setq vp_info (car *UBI_VP_DATA_MEM*)))
        (progn
          (setq old_rect_handle (vlax-ldata-get (car vp_info) "DYN_VP_RECT"))
          (if (and old_rect_handle (setq e_rect (handent old_rect_handle)))
            (setq target_poly (list e_rect current_scl_val current_scale_str (atof *UBI_sg_intervalo*) (atof *UBI_sg_altura*) (atof *UBI_sg_gap*)))
          )
        )
      )
    )
  )

  (if target_poly
    (progn
      (if (tblsearch "LAYER" *UBI_sg_layer_name*)
        (progn
          (command "_.-layer" "_thaw" *UBI_sg_layer_name* "_on" *UBI_sg_layer_name* "_unlock" *UBI_sg_layer_name* "")
          (setq ss_old (ssget "X" (list (cons 8 *UBI_sg_layer_name*))))
          (if ss_old (progn (command "_.ERASE" ss_old "") (setq ss_old nil) (gc)))
        )
        (command "_.-layer" "_new" *UBI_sg_layer_name* "_color" (itoa *UBI_sg_col_grid*) *UBI_sg_layer_name* "")
      )

      (command "._undo" "_begin")
      (setq all_grid_lines (ssadd) all_grid_texts (ssadd))

      (UBI_GenerateGridCore (nth 0 target_poly) (nth 1 target_poly) (nth 3 target_poly) (nth 4 target_poly) (nth 5 target_poly) all_grid_lines all_grid_texts)

      (if (> (sslength all_grid_lines) 0) (command "._draworder" all_grid_lines "" "_Back"))
      (if (> (sslength all_grid_texts) 0) (command "._draworder" all_grid_texts "" "_Front"))

      (command "._undo" "_end")
      (princ (strcat "\nGrilla generada a escala 1/" (nth 2 target_poly)))
    )
    (alert "No se encontr\\U+00F3 el recuadro del VP. Actualiza el VP Din\\U+00E1mico primero.")
  )
)

;; Motivo especifico del ultimo fallo de grilla (para el reporte del lote).
;; La deja en nil al iniciar y con texto en cada rama de fallo.
(if (not (boundp '*UBI_GRID_LAST_ERROR*)) (setq *UBI_GRID_LAST_ERROR* nil))

;; Reconstruye el recuadro del viewport en Model con la misma matematica de UBI_ExecuteVPUpdate.
;; Retorna el ename del rectangulo temporal (capa 0) o nil. No guarda ldata: el llamador lo borra tras usarlo.
(defun UBI_BuildTempVpRect (vp_ent scale_val / vp_obj cenpt ps_w ps_h ms_w ms_h p1 p2 p3 p4 result)
  (if (not (and vp_ent (entget vp_ent) (numberp scale_val) (> scale_val 0.0)))
    nil
    (progn
      (setq cenpt (vl-catch-all-apply 'vlax-ldata-get (list vp_ent "DYN_VP_CENTER")))
      (if (or (vl-catch-all-error-p cenpt) (not cenpt))
        nil
        (progn
          ;; AUD1: el wrapper del viewport se libera en todas las ramas (antes
          ;; fugaba 1 por grilla generada).
          (setq vp_obj (vl-catch-all-apply 'vlax-ename->vla-object (list vp_ent)))
          (if (vl-catch-all-error-p vp_obj) (setq vp_obj nil))
          (if vp_obj
            (progn
              (setq ps_w (vl-catch-all-apply 'vla-get-Width (list vp_obj))
                    ps_h (vl-catch-all-apply 'vla-get-Height (list vp_obj)))
              (if (or (vl-catch-all-error-p ps_w) (vl-catch-all-error-p ps_h)
                      (not (numberp ps_w)) (not (numberp ps_h)))
                (setq result nil)
                (progn
                  (setq ms_w (* ps_w (/ scale_val 1000.0)) ms_h (* ps_h (/ scale_val 1000.0)))
                  (setq p1 (list (- (car cenpt) (/ ms_w 2.0)) (- (cadr cenpt) (/ ms_h 2.0)) 0.0)
                        p2 (list (+ (car cenpt) (/ ms_w 2.0)) (- (cadr cenpt) (/ ms_h 2.0)) 0.0)
                        p3 (list (+ (car cenpt) (/ ms_w 2.0)) (+ (cadr cenpt) (/ ms_h 2.0)) 0.0)
                        p4 (list (- (car cenpt) (/ ms_w 2.0)) (+ (cadr cenpt) (/ ms_h 2.0)) 0.0))
                  (setq result (if (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                     '(8 . "0") '(90 . 4) '(70 . 1)
                                     (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p2))
                                     (list 10 (car p3) (cadr p3)) (list 10 (car p4) (cadr p4))))
                                 (entlast)
                                 nil))))
              (GM:ReleaseObjectSafe vp_obj)
              (setq vp_obj nil)
              result)
            nil)
        )
      )
    )
  )
)

;; Restauracion comun: PSPACE no cambia de la pestana Model a un Layout.
;; F15-R1: doc/app se liberan SIEMPRE; esta ruta corre 100-300 veces por codigo
;; (cada proyeccion) y era el mayor acumulador de wrappers del lote.
(defun UBI_RestoreSpace (tab cv / app doc)
  (setvar "CTAB" tab)
  (if (/= tab "Model")
    (progn
      (setq app (vl-catch-all-apply 'vlax-get-acad-object '()))
      (if (vl-catch-all-error-p app) (setq app nil))
      (setq doc (if app (vl-catch-all-apply 'vla-get-ActiveDocument (list app)) nil))
      (if (vl-catch-all-error-p doc) (setq doc nil))
      (if doc
        (progn
          (vl-catch-all-apply 'vla-put-MSpace (list doc (if (= cv 1) :vlax-false :vlax-true)))
          (if (/= cv 1) (setvar "CVPORT" cv))))
      (GM:ReleaseObjectSafe doc)
      (GM:ReleaseObjectSafe app)))
  (if (or (/= (getvar "CTAB") tab) (/= (getvar "CVPORT") cv))
    (progn (princ "\nNo se pudo restaurar CTAB/CVPORT") (exit)))
  T)

(defun UBI_GridFail (message)
  (setq *UBI_GRID_LAST_ERROR* (strcat *UBI_GRID_STAGE* ": " message))
  (exit))

;; Worker sin comandos interactivos. temp_rect/all_grid_lines/all_grid_texts se
;; enlazan por alcance dinamico a los locales del wrapper UBI_GenerateViewportGrid
;; (1301) para poder borrar el recuadro temporal y medir conteos aun con error.
;; POR QUE no se declaran aqui (revision P0-7 2026-09-29): si fueran locales del
;; worker, el wrapper perderia la referencia y el recuadro temporal quedaria vivo.
(defun UBI_ViewportGridWork (vp_ent scale_str / map_data scl_num rect_handle e_rect layers layer core_ok _app _doc)
  (setq *UBI_GRID_STAGE* "VALIDACION")
  (if (not (and vp_ent (entget vp_ent) (= (cdr (assoc 0 (entget vp_ent))) "VIEWPORT")))
    (UBI_GridFail "Viewport invalido"))
  (setq map_data (assoc scale_str (if (= *UBI_VP_TYPE* "2") *UBI_DATA_MAP_UBICACION* *UBI_DATA_MAP_NORMAL*)) scl_num (atof scale_str)) ;; BQ: grilla por tabla segun tipo
  (if (not map_data) (setq map_data (assoc scale_str *UBI_DATA_MAP_NORMAL*))) ;; BQ fallback NORMAL
  (if (not (and map_data (> scl_num 0.0))) (UBI_GridFail "Escala sin parametros de grilla"))
  (setvar "CTAB" "Model")
  (setq *UBI_GRID_STAGE* "RECUADRO")
  (setq rect_handle (vlax-ldata-get vp_ent "DYN_VP_RECT"))
  (if (= (type rect_handle) 'STR) (setq e_rect (handent rect_handle)))
  (if (not (and e_rect (entget e_rect)
                (= (cdr (assoc 0 (entget e_rect))) "LWPOLYLINE")
                (= (cdr (assoc 410 (entget e_rect))) "Model")))
    (setq temp_rect (UBI_BuildTempVpRect vp_ent scl_num) e_rect temp_rect))
  (if (not e_rect) (UBI_GridFail "Sin recuadro ni centro valido para reconstruirlo"))
  (setq *UBI_GRID_STAGE* "CAPA_ESTILO")
  (UBI_create_style_st_coorp)
  ;; AUD1: la cadena app/doc/layers va con catch y se suelta en el acto (antes
  ;; app y doc anonimos fugaban 2 por codigo); Item/Add tambien con catch.
  (setq _app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (vl-catch-all-error-p _app) (setq _app nil))
  (setq _doc (if _app (vl-catch-all-apply 'vla-get-ActiveDocument (list _app)) nil))
  (if (vl-catch-all-error-p _doc) (setq _doc nil))
  (GM:ReleaseObjectSafe _app)
  (setq layers (if _doc (vl-catch-all-apply 'vla-get-Layers (list _doc)) nil))
  (if (vl-catch-all-error-p layers) (setq layers nil))
  (GM:ReleaseObjectSafe _doc)
  (setq _app nil _doc nil)
  (setq layer (if layers
                (if (tblsearch "LAYER" *UBI_sg_layer_name*)
                  (vl-catch-all-apply 'vla-Item (list layers *UBI_sg_layer_name*))
                  (vl-catch-all-apply 'vla-Add (list layers *UBI_sg_layer_name*)))
                nil))
  (if (vl-catch-all-error-p layer) (setq layer nil))
  (if (not layer) (UBI_GridFail "Capa de grilla no disponible"))
  (vla-put-Freeze layer :vlax-false)
  (vla-put-LayerOn layer :vlax-true)
  (vla-put-Lock layer :vlax-false)
  (vla-put-Color layer *UBI_sg_col_grid*)
  ;; F15-R1: soltar los wrappers de capa al terminar de usarlos (por codigo).
  (GM:ReleaseObjectSafe layer)
  (GM:ReleaseObjectSafe layers)
  (setq layer nil layers nil)
  ;; No ERASE por capa: la limpieza del lote solo debe borrar lo creado en la corrida.
  (setq *UBI_GRID_STAGE* "DIBUJO")
  (setq core_ok (UBI_GenerateGridCore e_rect scl_num (nth 4 map_data)
                  (nth 5 map_data) (nth 6 map_data) all_grid_lines all_grid_texts))
  (setq *UBI_GRID_COUNTS* (list (sslength all_grid_lines) (sslength all_grid_texts)))
  (if (not (and core_ok (> (car *UBI_GRID_COUNTS*) 0) (> (cadr *UBI_GRID_COUNTS*) 0)))
    (UBI_GridFail "El motor no genero una grilla completa (lineas/textos)"))
  T)

;; T o nil; el motivo se conserva en *UBI_GRID_LAST_ERROR*. No retorna cadenas
;; verdaderas en caso de error. Restauracion y limpieza se ejecutan tras el catch.
(defun UBI_GenerateViewportGrid (vp_ent scale_str / grid_tab grid_cv grid_result restore_result cleanup_result temp_rect
    all_grid_lines all_grid_texts *UBI_sg_layer_name* *UBI_sg_idx_style*
    *UBI_sg_chk_inf* *UBI_sg_chk_sup* *UBI_sg_chk_izq* *UBI_sg_chk_der*)
  (setq grid_tab (getvar "CTAB") grid_cv (getvar "CVPORT")
        *UBI_GRID_LAST_ERROR* nil *UBI_GRID_COUNTS* nil *UBI_GRID_STAGE* "INICIO"
        all_grid_lines (ssadd) all_grid_texts (ssadd)
        *UBI_sg_layer_name* (strcat "GRILLA-VP" (itoa *UBI_ACTIVE_VP*))
        *UBI_sg_chk_inf* "1" *UBI_sg_chk_sup* "1" *UBI_sg_chk_izq* "1" *UBI_sg_chk_der* "1")
  (setq grid_result (vl-catch-all-apply 'UBI_ViewportGridWork (list vp_ent scale_str)))
  (if (and (vl-catch-all-error-p grid_result)
           (wcmatch (strcase (vl-catch-all-error-message grid_result)) "*CANCEL*,*BREAK*"))
    (setq *GM_AUTO_CANCELLED* T))
  (if (and (not *UBI_GRID_LAST_ERROR*) (or (vl-catch-all-error-p grid_result) (not (eq grid_result T))))
    (setq *UBI_GRID_LAST_ERROR* (strcat *UBI_GRID_STAGE* ": "
      (if (vl-catch-all-error-p grid_result) (vl-catch-all-error-message grid_result)
        "La rutina termino sin confirmar generacion"))))
  (if (and temp_rect (entget temp_rect))
    (progn
      (setq cleanup_result (vl-catch-all-apply 'entdel (list temp_rect)))
      ;; G: no pisa el error original de la grilla; agrega la limpieza como sufijo.
      (if (or (vl-catch-all-error-p cleanup_result) (not cleanup_result))
        (setq *UBI_GRID_LAST_ERROR*
          (if *UBI_GRID_LAST_ERROR*
            (strcat *UBI_GRID_LAST_ERROR* " | LIMPIEZA: No se pudo borrar el recuadro temporal")
            "LIMPIEZA: No se pudo borrar el recuadro temporal"))))
  )
  (setq restore_result (vl-catch-all-apply 'UBI_RestoreSpace (list grid_tab grid_cv)))
  (if (vl-catch-all-error-p restore_result)
    (setq *UBI_GRID_LAST_ERROR*
      (if *UBI_GRID_LAST_ERROR*
        (strcat *UBI_GRID_LAST_ERROR* " | RESTAURACION: " (vl-catch-all-error-message restore_result))
        (strcat "RESTAURACION: " (vl-catch-all-error-message restore_result)))))
  (not *UBI_GRID_LAST_ERROR*))

(defun UBI_RunGrilla ( / dcl_file f dcl_id status sel pl_ent styles_list temp_col vp_info scale_idx current_scale_str current_scl_val map_data map_to_use scales orig_tab item)
  (setq orig_tab (getvar "CTAB"))
  (setvar "CTAB" "Model")
  
  (if (or (not *UBI_sg_idx_style*) (= *UBI_sg_idx_style* "0")) (setq *UBI_sg_idx_style* (UBI_GetStyleIndex "ST-COORP")))

  (setq scales '("20" "25" "50" "75" "100" "125" "200" "250" "500" "750" "1000" "1250" "2000" "2500" "5000" "7500" "10000" "12500" "20000" "25000"))
  (setq current_scl_val 1000.0 current_scale_str "1000")

  (if (and *UBI_VP_DATA_MEM* (> (length *UBI_VP_DATA_MEM*) 0))
    (progn
      (setq vp_info (car *UBI_VP_DATA_MEM*) scale_idx (vlax-ldata-get (car vp_info) "DYN_VP_SCALE_IDX"))
      (if (not scale_idx) (setq scale_idx (caddr vp_info)))
      (if scale_idx
        (progn
          (setq current_scale_str (nth (atoi scale_idx) scales))
          (setq current_scl_val (atof current_scale_str))
          (setq map_to_use (if (= *UBI_VP_TYPE* "1") *UBI_DATA_MAP_NORMAL* *UBI_DATA_MAP_UBICACION*))
          (setq map_data (assoc current_scale_str map_to_use))
          (if (and map_data (>= (length map_data) 7))
            (progn
              (setq *UBI_sg_intervalo* (rtos (nth 4 map_data) 2 2) *UBI_sg_altura* (rtos (nth 5 map_data) 2 2) *UBI_sg_gap* (rtos (nth 6 map_data) 2 2))
            )
          )
        )
      )
    )
  )

  (setq styles_list '() item (tblnext "STYLE" T))
  (while item (setq styles_list (cons (cdr (assoc 2 item)) styles_list) item (tblnext "STYLE")))
  (setq styles_list (vl-sort styles_list '<))

  (setq *UBI_sg_layer_name* (strcat "GRILLA-VP" (itoa *UBI_ACTIVE_VP*)))

  (setq dcl_file (strcat (getenv "TEMP") "\\GrillaTempUbi.dcl") f (open dcl_file "w"))
  (write-line "GrillaUbi : dialog { label = \"Generador de Grilla NATIVA\";" f)
  (write-line "  : row { : column { label = \"Datos (Base)\"; : edit_box { label = \"Capa Destino:\"; key = \"layer_name\"; edit_width = 15; } : edit_box { label = \"Intervalo:\"; key = \"intervalo\"; edit_width = 10; } : edit_box { label = \"Altura Texto:\"; key = \"altura\"; edit_width = 10; } : edit_box { label = \"Margen Borde:\"; key = \"gap\"; edit_width = 10; } }" f)
  (write-line "  : boxed_column { label = \"BORDES\"; : toggle { label = \"Izquierda\"; key = \"chk_izq\"; } : toggle { label = \"Derecha\"; key = \"chk_der\"; } : toggle { label = \"Superior\"; key = \"chk_sup\"; } : toggle { label = \"Inferior\"; key = \"chk_inf\"; } }" f)
  (write-line "  : boxed_column { label = \"Estilos y Color\"; : popup_list { key = \"list_estilos\"; edit_width = 20; } spacer_1; : row { : button { label = \"Color Txt >\"; key = \"btn_col_txt\"; width = 12; } : text { key = \"txt_col_txt_val\"; width = 4; } } : row { : button { label = \"Color Grid >\"; key = \"btn_col_grid\"; width = 12; } : text { key = \"txt_col_grid_val\"; width = 4; } } } } spacer_1;" f)
  (write-line "  : row { : button { label = \"Pol\\U+00EDgono Manual (Opcional) <\"; key = \"btn_pick\"; width = 25; alignment = centered; } ok_cancel; } }" f)
  (close f)

  (setq status 2 pl_ent nil)
  (while (= status 2)
    (setq dcl_id (load_dialog dcl_file))
    (if (not (new_dialog "GrillaUbi" dcl_id)) (exit))
    (set_tile "layer_name" *UBI_sg_layer_name*) (set_tile "intervalo" *UBI_sg_intervalo*) (set_tile "altura" *UBI_sg_altura*) (set_tile "gap" *UBI_sg_gap*)
    (set_tile "chk_izq" *UBI_sg_chk_izq*) (set_tile "chk_der" *UBI_sg_chk_der*) (set_tile "chk_sup" *UBI_sg_chk_sup*) (set_tile "chk_inf" *UBI_sg_chk_inf*)
    (start_list "list_estilos") (mapcar 'add_list styles_list) (end_list)
    (set_tile "list_estilos" *UBI_sg_idx_style*) (set_tile "txt_col_txt_val" (itoa *UBI_sg_col_txt*)) (set_tile "txt_col_grid_val" (itoa *UBI_sg_col_grid*))

    (action_tile "btn_col_txt" "(setq temp_col (acad_colordlg *UBI_sg_col_txt* t)) (if temp_col (progn (setq *UBI_sg_col_txt* temp_col) (set_tile \"txt_col_txt_val\" (itoa *UBI_sg_col_txt*))))")
    (action_tile "btn_col_grid" "(setq temp_col (acad_colordlg *UBI_sg_col_grid* t)) (if temp_col (progn (setq *UBI_sg_col_grid* temp_col) (set_tile \"txt_col_grid_val\" (itoa *UBI_sg_col_grid*))))")
    (action_tile "btn_pick" "(done_dialog 2)") (action_tile "accept" "(done_dialog 1)") (action_tile "cancel" "(done_dialog 0)")
    (action_tile "layer_name" "(setq *UBI_sg_layer_name* $value)") (action_tile "intervalo" "(setq *UBI_sg_intervalo* $value)") (action_tile "altura" "(setq *UBI_sg_altura* $value)") (action_tile "gap" "(setq *UBI_sg_gap* $value)")
    (action_tile "chk_izq" "(setq *UBI_sg_chk_izq* $value)") (action_tile "chk_der" "(setq *UBI_sg_chk_der* $value)") (action_tile "chk_sup" "(setq *UBI_sg_chk_sup* $value)") (action_tile "chk_inf" "(setq *UBI_sg_chk_inf* $value)")
    (action_tile "list_estilos" "(setq *UBI_sg_idx_style* $value)")

    (setq status (start_dialog)) 
    (unload_dialog dcl_id)
    (if (= status 2)
      (progn 
        (setq sel (entsel "\nSeleccione pol\\U+00EDgono (Polil\\U+00EDnea cerrada): "))
        (if (and sel (= (cdr (assoc 0 (entget (car sel)))) "LWPOLYLINE"))
          (setq pl_ent (car sel))
          (alert "Debe ser una Polil\\U+00EDnea.")
        )
      )
    )
  )

  (if (findfile dcl_file) (vl-file-delete dcl_file))

  (if (= status 1)
    (UBI_ExecuteGridGeneration pl_ent current_scl_val current_scale_str)
  )
  (setvar "CTAB" orig_tab)
)