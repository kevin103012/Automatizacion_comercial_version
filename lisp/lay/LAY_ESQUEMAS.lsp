;; ============================================================================
;; LAY_ESQUEMAS.lsp - Sistema GM Ingenieros (modulo lay)
;; Movido SIN CAMBIOS desde LAYOUT_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


;; ==============================================================================
;; MOTOR DE DIBUJO DE ESQUEMAS PU (Afectaci\\U+00F3n y Localizaci\\U+00F3n)
;; ==============================================================================
;; POR QUE el fondo del esquema AF: el rectangulo lleva un hatch SOLID blanco al
;; fondo del grupo para tapar el mapa del viewport sin tapar su propio contenido;
;; el titulo lleva su caja melocoton encima. hatch_bg_main_af es ese fondo.
(defun EXEC_ESQUEMAS_PU_FULL ( / orig_tab olderr modo_num str_texto matriz i dv_list minpt maxpt minp maxp pt_base w_orig h_orig pt_dest_af scale_x scale_y sf pt_rect_1 pt_rect_2 rect_main_af hatch_bg_main_af pt_box_tl pt_box_br box_ent hatch_bg pt_text_center str_line1 str_line2 text_ent ss_scale mat_copy hatch_ents h_copy pt_dest_lo ss_all hatch_mat dv_copy rect_main_ent hatch_bg_main_lo pt_arr1 pt_txt1 pt_arr2 pt_txt2 idx pt_tbl X Y a_ha str_ha str_m2 str_p str_header box_hdr h_hdr old_dimzin block_name ss_blk_af blk_name_af blk_name_lo e_l1 e_t1 e_l2 e_t2 blk_af_inserted blk_lo_inserted ss obj vla_obj hl_str hl_up code_up pref_af pref_pf af_list pf_list suffix a p data match_af match_pf a_m2 p_m2 p_obj a_obj box_main h_body all_suffixes data_afectaciones item dv h suf tbl_n tbl_h tbl_k tbl_cap ss_blk_lo detail_min detail_max detail_lo detail_hi detail_base detail_w detail_h detail_sf detail_ss detail_copy detail_block detail_insert detail_p1 detail_p2 detail_center detail_bg detail_rect detail_text detail_sources detail_sel selected_af_rect zoom_bg zoom_ss zoom_handle zoom_source zoom_copy zoom_block zoom_insert zoom_center zoom_frame zoom_clip zoom_circle zoom_source_circle zoom_circle_inner zoom_circle_outer zoom_source_inner zoom_source_outer zoom_i zoom_leader1 zoom_label zoom_angle zoom_fb_ss zoom_fb_i zoom_fb_ent zoom_fb_p1 zoom_fb_p2 zoom_mapbox zoom_mbox zoom_route_info zoom_before drw_ss mat_obj _detail_obj zoom_ss2)
  (setq orig_tab (getvar "CTAB"))
  (setq olderr *error*)
  (defun *error* (msg)
    (if (and orig_tab (/= (getvar "CTAB") orig_tab)) (setvar "CTAB" orig_tab))
    (if *GM_AUTO_MODE*
      (progn
        (setq *GM_AUTO_LAYOUT_FAILED* T *GM_AUTO_LAST_MESSAGE* msg)
        (GM:LayoutRestoreLegend)))
    (setq *error* olderr)
    (princ "\nEsquemas Cancelados.")
    (princ)
  )

  (if (or (= *ML_TARGET_CODE* "") (not *ML_TARGET_CODE*))
    (progn (alert "?? NO HAY C\\U+00D3DIGO OBJETIVO DEFINIDO. Ingresa o selecciona uno en el panel de configuraci\\U+00F3n primero.") (exit))
  )

  (setq modo_num (atoi *ML_ESQ_MODO*) data_afectaciones nil) 
  (setq str_texto (nth (atoi *ML_ESQ_LOC_TIPO*) '("ESQUEMA DE LOCALIZACI\\U+00D3N DEL PREDIO EN PROPIEDAD" "ESQUEMA DE LOCALIZACI\\U+00D3N DEL PREDIO EN POSESI\\U+00D3N" "ESQUEMA DE LOCALIZACI\\U+00D3N DEL PREDIO OCUPADO")))
  
  (setq *GL_DATA_AFECTACIONES* nil)
  ;; FIX2 30-09-2026: fallback defensivo alineado con AUTOMATIZACION: sin
  ;; toggle definido la senalizacion del predio queda ACTIVADA ("1").
  (if (not *GM_AUTO_SHOW_PREDIO_LOC*) (setq *GM_AUTO_SHOW_PREDIO_LOC* "1"))

  (if (not (tblsearch "STYLE" "TEXTO RESALTADO")) (command "_.-STYLE" "TEXTO RESALTADO" "calibri.ttf" "0.0" "1.0" "0" "_N" "_N"))
  (if (not (tblsearch "STYLE" "SANSSERIF")) (command "_.-STYLE" "SANSSERIF" "sansserif.ttf" "0.0" "1.0" "0" "_N" "_N"))
  (if (not (tblsearch "STYLE" "ROMANS")) (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbTextStyleTableRecord") '(2 . "ROMANS") '(70 . 0) '(40 . 0.0) '(41 . 1.15) '(3 . "romans.shx") '(4 . ""))))

  (if (/= (getvar "CTAB") "Model") (setvar "CTAB" "Model"))
  
  (princ "\n=============================================")
  (princ "\n   ESCANEO INTELIGENTE DE MODEL SPACE...     ")
  (princ "\n=============================================")

  ;; EXTRACCI\\U+00D3N AUTOM\\U+00C1TICA DE ENTIDADES POR HIPERV\\U+00DANculos
  (setq code_up (strcase *ML_TARGET_CODE*))
  (setq pref_af (strcat code_up "-AF-"))
  (setq pref_pf (strcat code_up "-PF-"))
  
  (setq matriz nil dv_list nil af_list nil pf_list nil)
  ;; PA-2 (H2) 2026-09-28: en lote, clasificar desde *UBI_ENT_INDEX* (los
  ;; hipervinculos ya se leyeron UNA vez por lote en UBI_BatchIndexEnts); el
  ;; barrido clasico queda para manual o si el indice no esta listo. Mismo
  ;; orden de base (la cons invierte igual) y mismas guardas por entidad.
  (if (and *GM_AUTO_MODE* *UBI_ENT_INDEX_READY* *UBI_ENT_INDEX*)
    (foreach item *UBI_ENT_INDEX*
      (setq obj (car item) hl_str (cadr item))
      (if (and hl_str (/= hl_str ""))
        (progn
          (setq hl_up (strcase hl_str))
          (cond
            ((or (= hl_up code_up)
                 (and (and *UBI_TARGET_REFERENCE* (/= *UBI_TARGET_REFERENCE* ""))
                      (= (UBI_NormalizeSearchKey *UBI_TARGET_REFERENCE*)
                         (UBI_NormalizeSearchKey hl_up))))
             (setq matriz obj))
            ((and (vl-string-search "DERECHO" hl_up) (vl-string-search "VIA" hl_up))
             (setq dv_list (cons obj dv_list)))
            ((str-starts-with hl_up pref_af)
             (setq suffix (substr hl_up (1+ (strlen pref_af))))
             (setq af_list (cons (list suffix obj) af_list)))
            ((str-starts-with hl_up pref_pf)
             (setq suffix (substr hl_up (1+ (strlen pref_pf))))
             (setq pf_list (cons (list suffix obj) pf_list)))))))
    (progn
  (setq ss (ssget "X" (list (cons 410 "Model"))))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (ssname ss i))
        ;; AUD1: creacion con catch; una entidad caida se salta sin abortar el
        ;; clasificador AF/PF (antes el wrapper se creaba directo).
        (setq vla_obj (vl-catch-all-apply 'vlax-ename->vla-object (list obj)))
        (if (vl-catch-all-error-p vla_obj) (setq vla_obj nil))
        (setq hl_str (if vla_obj (ML_GetHyperlinkString vla_obj) nil))
        
        (if (and hl_str (/= hl_str ""))
          (progn
            (setq hl_up (strcase hl_str))
            (cond
              ((or (= hl_up code_up)
                   (and (and *UBI_TARGET_REFERENCE* (/= *UBI_TARGET_REFERENCE* ""))
                        (= (UBI_NormalizeSearchKey *UBI_TARGET_REFERENCE*)
                           (UBI_NormalizeSearchKey hl_up))))
                (setq matriz obj))
              ((and (vl-string-search "DERECHO" hl_up) (vl-string-search "VIA" hl_up)) 
               (setq dv_list (cons obj dv_list)))
              ((str-starts-with hl_up pref_af) 
               (setq suffix (substr hl_up (1+ (strlen pref_af))))
               (setq af_list (cons (list suffix obj) af_list)))
               ((str-starts-with hl_up pref_pf)
                (setq suffix (substr hl_up (1+ (strlen pref_pf))))
                (setq pf_list (cons (list suffix obj) pf_list)))
             )
           )
         )
        ;; OPT1/BZ-OPT1: soltar el wrapper del escaneo AF/PF por iteracion.
        (GM:ReleaseObjectSafe vla_obj)
        (setq vla_obj nil i (1+ i))
      )
    )
  )
    )
  )

  ;; PA-3 (H2): fuente de clasificacion del lote para el reporte.
  (if *GM_AUTO_MODE*
    (GM:AutoTrace *ML_TARGET_CODE* "ESQUEMAS_FUENTE" "INFO"
      (if (and *UBI_ENT_INDEX_READY* *UBI_ENT_INDEX*) "INDICE" "CLASICO")))

  ;; Si el codigo se resolvio mediante una referencia U.C., reutilizar
  ;; exactamente la polilinea validada por el centrado del viewport.
  (if (and (not matriz) *UBI_TARGET_POLYGON* (entget *UBI_TARGET_POLYGON*))
    (setq matriz *UBI_TARGET_POLYGON*)
  )

  (if (not matriz)
    (progn (alert (strcat "?? NO SE ENCONTR\\U+00D3 EL POL\\U+00CDGONO MATRIZ.\nAseg\\U+00FArate de que haya un objeto en el Model con el hiperv\\U+00EDnculo exacto: " code_up)) (exit))
  )

  (princ (strcat "\n> Se encontr\\U+00F3 la Matriz: " code_up))
  (princ (strcat "\n> Derechos de V\\U+00CDa encontrados: " (itoa (length dv_list))))

  ;; EMPAREJAMIENTO DE AF Y PF
  (setq all_suffixes nil)
  (foreach item af_list (if (not (member (car item) all_suffixes)) (setq all_suffixes (cons (car item) all_suffixes))))
  (foreach item pf_list (if (not (member (car item) all_suffixes)) (setq all_suffixes (cons (car item) all_suffixes))))
  (setq all_suffixes (vl-sort all_suffixes '<))
  (if *GM_AUTO_MODE*
    (GM:AutoTrace *ML_TARGET_CODE* "AF_PF_INVENTARIO" "OK"
      (strcat "AF=" (itoa (length af_list)) "; PF=" (itoa (length pf_list))
              "; sufijos=" (vl-princ-to-string all_suffixes))))

  (setq hatch_ents nil)

  (foreach suf all_suffixes
    (setq match_af (assoc suf af_list))
    (setq match_pf (assoc suf pf_list))
    
    (setq a_m2 0.0 p_m2 0.0 a_obj nil p_obj nil)
    
    ;; F15-R1: wrappers con catch y liberados al cerrar el tramo (todas las
    ;; ramas pasan por aqui; el trace de Handle corre antes del release).
    (if match_af
      (progn
        (setq a_obj (vl-catch-all-apply 'vlax-ename->vla-object (list (cadr match_af))))
        (if (vl-catch-all-error-p a_obj) (setq a_obj nil))))
    (if match_pf
      (progn
        (setq p_obj (vl-catch-all-apply 'vlax-ename->vla-object (list (cadr match_pf))))
        (if (vl-catch-all-error-p p_obj) (setq p_obj nil))))

    (if a_obj (setq a_m2 (vl-catch-all-apply 'vla-get-Area (list a_obj))))
    (if (vl-catch-all-error-p a_m2) (setq a_m2 0.0))
    
    (if p_obj (setq p_m2 (vl-catch-all-apply 'vla-get-Length (list p_obj))))
    (if (vl-catch-all-error-p p_m2) (setq p_m2 0.0))

    (if (and (= p_m2 0.0) a_obj) 
      (progn
        (setq p_m2 (vl-catch-all-apply 'vla-get-Length (list a_obj)))
        (if (vl-catch-all-error-p p_m2) (setq p_m2 0.0))
      )
    )
    (if (and (= a_m2 0.0) p_obj) 
      (progn
        (setq a_m2 (vl-catch-all-apply 'vla-get-Area (list p_obj)))
        (if (vl-catch-all-error-p a_m2) (setq a_m2 0.0))
      )
    )

;; Keep the real suffix with its measurements; the table index is only a slot.
    (setq data_afectaciones (append data_afectaciones (list (list a_m2 p_m2 suf))))
    (setq *GL_DATA_AFECTACIONES* (append *GL_DATA_AFECTACIONES* (list (list a_m2 (/ a_m2 10000.0) p_m2 suf (if match_pf (cadr match_pf) nil)))))
    
    (if a_obj (setq hatch_ents (cons (cadr match_af) hatch_ents)))
    (if (and (not a_obj) p_obj) (setq hatch_ents (cons (cadr match_pf) hatch_ents))) 
    
    (princ (strcat "\n> Procesado Tramo AF-" suf " -> \\U+00E1rea: " (rtos a_m2 2 2) " m2, Per\\U+00EDmetro: " (rtos p_m2 2 2) " m."))
    (if *GM_AUTO_MODE*
      (GM:AutoTrace *ML_TARGET_CODE* "AF_PF_TRAMO" "OK"
        (strcat "Sufijo=" suf "; AF=" (GM:SafeHandle a_obj)
                "; PF=" (GM:SafeHandle p_obj)
                "; area_m2=" (rtos a_m2 2 4) "; perimetro_m=" (rtos p_m2 2 4))))
    ;; F15-R1: liberar los wrappers del tramo (antes quedaban vivos por codigo).
    (GM:ReleaseObjectSafe a_obj)
    (GM:ReleaseObjectSafe p_obj)
    (setq a_obj nil p_obj nil)
  )

  ;; P0-1 2026-09-29: bbox de la matriz con catch + release (una matriz borrada
  ;; o corrupta abortaba la etapa con fuga COM por codigo).
  (if (boundp 'GM:AutoForensic)
    (GM:AutoForensic *ML_TARGET_CODE* "ESQ_MAT_BBOX" "bbox matriz INICIO"))
  (setq mat_obj (vl-catch-all-apply 'vlax-ename->vla-object (list matriz)))
  (if (or (vl-catch-all-error-p mat_obj) (not mat_obj)
          (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetBoundingBox (list mat_obj 'minpt 'maxpt))))
    (progn
      (GM:ReleaseObjectSafe mat_obj)
      (GM:LayoutFail "No se pudo medir la matriz del esquema (bbox no disponible)")))
  (setq minp (vlax-safearray->list minpt) maxp (vlax-safearray->list maxpt))
  (GM:ReleaseObjectSafe mat_obj)
  (setq pt_base (list (/ (+ (car minp) (car maxp)) 2.0) (/ (+ (cadr minp) (cadr maxp)) 2.0) (/ (+ (caddr minp) (caddr maxp)) 2.0)))
  (setq w_orig (- (car maxp) (car minp)) h_orig (- (cadr maxp) (cadr minp)))
  (if (boundp 'GM:AutoForensic)
    (GM:AutoForensic *ML_TARGET_CODE* "ESQ_MAT_BBOX"
      (strcat "w=" (rtos w_orig 2 2) " h=" (rtos h_orig 2 2))))
  ;; P0-3 2026-09-29: sin dimensiones reales los esquemas dividirian por cero.
  (if (or (not (numberp w_orig)) (<= w_orig 0.0) (not (numberp h_orig)) (<= h_orig 0.0))
    (GM:LayoutFail "Matriz sin dimensiones medibles para los esquemas"))

  ;; --- 1. ESQUEMA AFECTACI\\U+00D3N (70x60) ---
  (if (or (= modo_num 0) (= modo_num 1))
    (progn
      (princ "\n--- INICIANDO ESQUEMA DE AFECTACI\\U+00D3N (70x60) ---")
      (setvar "CTAB" orig_tab)
      (setq pt_dest_af (if *GM_AUTO_MODE* (GM:LayoutZonePoint "ESQUEMA_AF" 70.0 67.5 0 "SCHEMA") (PREVIEW_SCHEMA 70.0 60.0 7.5 "\nMueva el cursor para ubicar el Esquema de Afectaci\\U+00F3n y haga CLIC: ")))
      
      (if (boundp 'GM:AutoForensic)
  (GM:AutoForensic *ML_TARGET_CODE* "ESQ_AF_ZONA" (if pt_dest_af "zona AF OK" "zona AF NIL")))
(if pt_dest_af
        (progn
          ;; Fase 1D (H-8): el zoom/REGEN era refresco visual para uso manual;
          ;; en modo lote (*GM_AUTO_MODE*) no aporta al resultado y cuesta tiempo.
          (if (not *GM_AUTO_MODE*) (command "_.ZOOM" "_C" pt_dest_af 200))
          (setq scale_x (/ 65.0 w_orig) scale_y (/ 55.0 h_orig) sf (if (< scale_x scale_y) scale_x scale_y))
          
(if (boundp 'GM:AutoForensic)
  (GM:AutoForensic *ML_TARGET_CODE* "ESQ_AF_FONDO" "fondo AF INICIO"))
(setq pt_rect_1 (list (- (car pt_dest_af) 35.0) (- (cadr pt_dest_af) 30.0) 0.0) pt_rect_2 (list (+ (car pt_dest_af) 35.0) (+ (cadr pt_dest_af) 30.0) 0.0))
           (command "_.RECTANG" "_NON" pt_rect_1 "_NON" pt_rect_2) (setq rect_main_af (entlast)) (GM:EntPut rect_main_af 'Color 0) 
           
           ;; Fondo blanco de la ventana del esquema (patron del esquema LO): tapa el mapa
           ;; del viewport sin tapar el contenido propio (va al fondo del grupo).
           (command "_.-HATCH" "_P" "SOLID" "_S" rect_main_af "" "") (setq hatch_bg_main_af (entlast)) (command "_.CHPROP" hatch_bg_main_af "" "_C" "_T" "255,255,255" "")
           ;; Caja titulo melocoton sobre el fondo blanco
           (setq pt_box_tl (list (- (car pt_dest_af) 35.0) (- (cadr pt_dest_af) 30.0) 0.0) pt_box_br (list (+ (car pt_dest_af) 35.0) (- (cadr pt_dest_af) 37.5) 0.0))
           (command "_.RECTANG" "_NON" pt_box_tl "_NON" pt_box_br) (setq box_ent (entlast)) (GM:EntPut box_ent 'Color 0) 
           (command "_.-HATCH" "_P" "SOLID" "_S" box_ent "" "") (setq hatch_bg (entlast)) (command "_.CHPROP" hatch_bg "" "_C" "_T" "255,210,180" "")

          (if (boundp 'GM:AutoForensic)
            (GM:AutoForensic *ML_TARGET_CODE* "ESQ_AF_FONDO" "fondo AF OK"))
          (setq pt_text_center (list (car pt_dest_af) (- (cadr pt_dest_af) 33.75) 0.0) str_line1 "ESQUEMA DEL \\U+00C1REA AFECTADA DENTRO DE LA MATRIZ")
          (if (= *ML_ESQ_PE_TOG* "1") (setq str_line2 (strcat "INSCRITA EN LA P.E. " *ML_ESQ_PE_VAL*)) (setq str_line2 "NO INSCRITA"))
          (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText") (cons 10 pt_text_center) (cons 40 1.8) (cons 41 70.0) (cons 71 5) (cons 7 "TEXTO RESALTADO") (cons 1 (strcat "{\\fCalibri|b1;" str_line1 "\\P" str_line2 "}"))))
          (setq text_ent (entlast))

          (setvar "CTAB" "Model")
                     (if (boundp 'GM:AutoForensic)
             (GM:AutoForensic *ML_TARGET_CODE* "ESQ_AF_COPIAS"
               (strcat "copias=" (itoa (1+ (length hatch_ents))))))
           (setq ss_scale (ssadd))
          (command "_.COPY" matriz "" "_NON" pt_base "_NON" pt_base) (setq mat_copy (entlast)) 
          (GM:EntPut mat_copy 'Color 34)
          (GM:EntPut mat_copy 'Linetype "Continuous") 
          (ssadd mat_copy ss_scale)
          
          (foreach h hatch_ents
            (command "_.COPY" h "" "_NON" pt_base "_NON" pt_base) (setq h_copy (entlast)) 
            (GM:EntPut h_copy 'Color 1)
            (GM:EntPut h_copy 'Linetype "Continuous") 
            (ssadd h_copy ss_scale)
          )
          (if (boundp 'GM:AutoForensic)
            (GM:AutoForensic *ML_TARGET_CODE* "ESQ_AF_COPIAS" "copias OK"))
          
          (setq block_name (strcat "TEMP_AF_" (rtos (getvar "TDUSRTIMER") 2 6)))
          (setq block_name (vl-string-subst "" "." block_name))
          ;; OPT1/BZ-OPT1: registrar el temporal para la purga dirigida entre codigos.
          (GM:RegisterTempBlock block_name)
          (command "_.-BLOCK" block_name "_NON" pt_base ss_scale "")
          (setvar "CTAB" orig_tab)
          
          (command "_.-INSERT" block_name "_NON" pt_dest_af sf sf "0")
          (setq blk_af_inserted (entlast))
          
(if (boundp 'GM:AutoForensic)
  (GM:AutoForensic *ML_TARGET_CODE* "ESQ_AF_XCLIP" "XCLIP AF INICIO"))
(command "_.XCLIP" blk_af_inserted "" "_N" "_R" "_NON" pt_rect_1 "_NON" pt_rect_2)

           ;; T1.1 REVERTIDO (revision 28-09-2026): el DRAWORDER agrupado no
           ;; garantiza el orden de la cascada (preserva el orden actual de la
           ;; base); se conservan los comandos individuales por apilado exacto.
           (command "_.DRAWORDER" blk_af_inserted "" "_F")
           (command "_.DRAWORDER" rect_main_af "" "_F")     
           (command "_.DRAWORDER" hatch_bg "" "_F")
           (command "_.DRAWORDER" box_ent "" "_F")
           (command "_.DRAWORDER" text_ent "" "_F")
           (command "_.DRAWORDER" hatch_bg_main_af "" "_B")
           (if (boundp 'GM:AutoForensic)
             (GM:AutoForensic *ML_TARGET_CODE* "ESQ_AF_XCLIP" "XCLIP+DRAWORDER AF OK"))
           (if (not *GM_AUTO_MODE*) (command "_.REGEN"))

           (setq ss_blk_af (ssadd))
           (ssadd blk_af_inserted ss_blk_af)
           (ssadd rect_main_af ss_blk_af)
           (ssadd hatch_bg_main_af ss_blk_af)
           (ssadd hatch_bg ss_blk_af)
           (ssadd box_ent ss_blk_af)
           (ssadd text_ent ss_blk_af)
          
          (if (boundp 'GM:AutoForensic)
            (GM:AutoForensic *ML_TARGET_CODE* "ESQ_AF_FIN" "cierre AF INICIO"))
          (setq blk_name_af (vl-string-subst "" "." (strcat "ESQ_AF_FIN_" (rtos (getvar "TDUSRTIMER") 2 6))))
          (GM:RegisterTempBlock blk_name_af)
          (command "_.-BLOCK" blk_name_af "_NON" pt_dest_af ss_blk_af "")
          (command "_.-INSERT" blk_name_af "_NON" pt_dest_af "1" "1" "0")
          (if (boundp 'GM:AutoForensic)
            (GM:AutoForensic *ML_TARGET_CODE* "ESQ_AF_FIN" "cierre AF OK"))

          (princ "\n> Esquema de Afectaci\\U+00F3n finalizado.")

          (if data_afectaciones
            (progn
              ;; Con 3+ afectaciones los cuadros se compactan (50x15 sep 2);
              ;; con 1-2 quedan identicos a la version aprobada (50x24 sep 3).
              (setq tbl_n (length data_afectaciones)
                    tbl_h (if (> tbl_n 2) 15.0 24.0)
                    tbl_k (/ tbl_h 24.0)
                    *GM_TABLE_SEP* (if (> tbl_n 2) 2.0 3.0))
              (if (> tbl_n 2)
                (GM:AutoTrace *ML_TARGET_CODE* "TABLAS" "INFO"
                  (strcat "Compactas 50x15 sep 2 por " (itoa tbl_n) " afectaciones")))
              ;; FIX 30-09: capacidad evaluada ANTES de dibujar (24+3 => 2
              ;; tablas; 15+2 => 3). El exceso falla suave (LAYOUT_FAILED, sin
              ;; exit): la limpieza por diferencia del lote borra todo y no
              ;; quedan tablas residuales en los codigos siguientes.
              (setq tbl_cap (if (> tbl_n 2) 3 2))
              (if (boundp 'GM:AutoForensic)
                (GM:AutoForensic *ML_TARGET_CODE* "ESQ_TABLAS"
                  (strcat "n=" (itoa tbl_n) " cap=" (itoa tbl_cap))))
              (if (> tbl_n tbl_cap)
                (progn
                  (GM:AutoTrace *ML_TARGET_CODE* "TABLAS" "ERROR"
                    (strcat "Capacidad de zona excedida, no se exporta un plano incompleto (" (itoa tbl_n) " afectaciones; caben " (itoa tbl_cap) ")"))
                  (setq *GM_AUTO_LAYOUT_FAILED* T
                        *GM_AUTO_LAST_MESSAGE* (strcat "Capacidad de zona excedida: TABLAS (" (itoa tbl_n) " afectaciones)"))
                )
              )
              (if (<= tbl_n tbl_cap)
                (progn
              (setq idx 1)
              (foreach data data_afectaciones
                (setq a (car data) p (cadr data) a_ha (/ a 10000.0))
                (setq old_dimzin (getvar "DIMZIN")) (setvar "DIMZIN" 0) 
                (setq str_ha (rtos a_ha 2 4) str_m2 (rtos a 2 2) str_p (rtos p 2 2)) (setvar "DIMZIN" old_dimzin)

                (if (setq pt_tbl (if *GM_AUTO_MODE* (GM:LayoutZonePoint "TABLAS" 50.0 tbl_h (1- idx) "TABLE") (PREVIEW_TABLE 50.0 24.0 8.64 (strcat "\nMueva el cursor para ubicar el CUADRO DE \\U+00C1REA " (if (> (length data_afectaciones) 1) (strcat (if (< idx 10) "0" "") (itoa idx)) "") " y haga CLIC: "))))
                  (progn
                    (setq X (car pt_tbl) Y (cadr pt_tbl))
                    
                    (command "_.RECTANG" "_NON" pt_tbl "_NON" (list (+ X 50.0) (- Y tbl_h) 0.0)) (setq box_main (entlast))
                    (command "_.-HATCH" "_P" "SOLID" "_S" box_main "" "") (setq h_body (entlast)) (command "_.CHPROP" h_body "" "_C" "_T" "255,255,255" "")
                    
                    (command "_.RECTANG" "_NON" pt_tbl "_NON" (list (+ X 50.0) (- Y (* 8.64 tbl_k)) 0.0)) (setq box_hdr (entlast))
                    (command "_.-HATCH" "_P" "SOLID" "_S" box_hdr "" "") (setq h_hdr (entlast)) (command "_.CHPROP" h_hdr "" "_C" "_T" "255,210,180" "") 
                    
                    ;; T1.1 REVERTIDO: cascada individual por apilado exacto
                    ;; (el set agrupado dejaria el marco debajo del hatch).
                    (command "_.DRAWORDER" h_body "" "_B")
                    (command "_.DRAWORDER" box_main "" "_F")
                    (command "_.DRAWORDER" h_hdr "" "_F")
                    (command "_.DRAWORDER" box_hdr "" "_F")

                    (command "_.LINE" "_NON" (list X (- Y (* 13.76 tbl_k)) 0.0) "_NON" (list (+ X 50.0) (- Y (* 13.76 tbl_k)) 0.0) "") 
                    (command "_.LINE" "_NON" (list X (- Y (* 18.88 tbl_k)) 0.0) "_NON" (list (+ X 50.0) (- Y (* 18.88 tbl_k)) 0.0) "") 
                    (command "_.LINE" "_NON" (list (+ X 30.0) (- Y (* 8.64 tbl_k)) 0.0) "_NON" (list (+ X 30.0) (- Y tbl_h) 0.0) "")
                    
                    (setq str_header (strcat "CUADRO DE \\U+00C1REA\\P(\\U+00C1REA AFECTADA " (caddr data) ")"))
                    (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText") (cons 10 (list (+ X 25.0) (- Y (* 4.32 tbl_k)) 0.0)) (cons 40 (* 2.0 tbl_k)) (cons 41 50.0) (cons 71 5) (cons 7 "TEXTO RESALTADO") (cons 1 (strcat "{\\fCalibri|b1;" str_header "}"))))

                    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(370 . 9) '(100 . "AcDbText") (cons 10 (list (+ X 15.0) (- Y (* 11.20 tbl_k)) 0.0)) (cons 40 (* 1.5 tbl_k)) (cons 41 1.1) (cons 1 "\\U+00C1REA (ha)") (cons 7 "ROMANS") '(72 . 1) (cons 11 (list (+ X 15.0) (- Y (* 11.20 tbl_k)) 0.0)) '(100 . "AcDbText") '(73 . 2)))
                    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(370 . 9) '(100 . "AcDbText") (cons 10 (list (+ X 40.0) (- Y (* 11.20 tbl_k)) 0.0)) (cons 40 (* 1.5 tbl_k)) (cons 41 1.1) (cons 1 str_ha) (cons 7 "ROMANS") '(72 . 1) (cons 11 (list (+ X 40.0) (- Y (* 11.20 tbl_k)) 0.0)) '(100 . "AcDbText") '(73 . 2)))
                    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(370 . 9) '(100 . "AcDbText") (cons 10 (list (+ X 15.0) (- Y (* 16.32 tbl_k)) 0.0)) (cons 40 (* 1.5 tbl_k)) (cons 41 1.1) (cons 1 "\\U+00C1REA (m\\U+00B2)") (cons 7 "ROMANS") '(72 . 1) (cons 11 (list (+ X 15.0) (- Y (* 16.32 tbl_k)) 0.0)) '(100 . "AcDbText") '(73 . 2)))
                    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(370 . 9) '(100 . "AcDbText") (cons 10 (list (+ X 40.0) (- Y (* 16.32 tbl_k)) 0.0)) (cons 40 (* 1.5 tbl_k)) (cons 41 1.1) (cons 1 str_m2) (cons 7 "ROMANS") '(72 . 1) (cons 11 (list (+ X 40.0) (- Y (* 16.32 tbl_k)) 0.0)) '(100 . "AcDbText") '(73 . 2)))
                    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(370 . 9) '(100 . "AcDbText") (cons 10 (list (+ X 15.0) (- Y (* 21.44 tbl_k)) 0.0)) (cons 40 (* 1.5 tbl_k)) (cons 41 1.1) (cons 1 "PER\\U+00CDMETRO (m)") (cons 7 "ROMANS") '(72 . 1) (cons 11 (list (+ X 15.0) (- Y (* 21.44 tbl_k)) 0.0)) '(100 . "AcDbText") '(73 . 2)))
                    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(370 . 9) '(100 . "AcDbText") (cons 10 (list (+ X 40.0) (- Y (* 21.44 tbl_k)) 0.0)) (cons 40 (* 1.5 tbl_k)) (cons 41 1.1) (cons 1 str_p) (cons 7 "ROMANS") '(72 . 1) (cons 11 (list (+ X 40.0) (- Y (* 21.44 tbl_k)) 0.0)) '(100 . "AcDbText") '(73 . 2)))
                  )
                )
                (setq idx (1+ idx))
              )
                )
              )
            )
          )
        )
      )
    )
  )

  ;; DIAG1 2026-10-03: rama AF cerrada (tablas incluidas) antes de la rama LO.
  (if (boundp 'GM:AutoForensic)
    (GM:AutoForensic *ML_TARGET_CODE* "ESQ_TABLAS" "rama AF cerrada"))

  ;; --- 2. ESQUEMA LOCALIZACI\\U+00D3N (80x60) ---
  (if (or (= modo_num 0) (= modo_num 2))
    (progn
      (princ "\n--- INICIANDO ESQUEMA DE LOCALIZACI\\U+00D3N (80x60) ---")
      (if (/= (getvar "CTAB") orig_tab) (setvar "CTAB" orig_tab))

      (setq pt_dest_lo (if *GM_AUTO_MODE* (GM:LayoutZonePoint "ESQUEMA_LOC" 80.0 67.5 0 "SCHEMA") (PREVIEW_SCHEMA 80.0 60.0 7.5 "\nMueva el cursor para ubicar el Esquema de Localizaci\\U+00F3n y haga CLIC: ")))
      
      (if pt_dest_lo
        (progn
          (if (not *GM_AUTO_MODE*) (command "_.ZOOM" "_C" pt_dest_lo 200))
          (setq scale_x (/ 40.0 w_orig) scale_y (/ 25.0 h_orig) sf (if (< scale_x scale_y) scale_x scale_y))
          
          (setvar "CTAB" "Model")
          (setq ss_all (ssadd))
          
          (foreach dv dv_list
            (command "_.COPY" dv "" "_NON" pt_base "_NON" pt_base) (setq dv_copy (entlast)) 
            (GM:EntPut dv_copy 'Color 230)
            (GM:EntPut dv_copy 'Linetype "Continuous") 
            (ssadd dv_copy ss_all)
          )
          
          (command "_.COPY" matriz "" "_NON" pt_base "_NON" pt_base) (setq mat_copy (entlast)) 
          (GM:EntPut mat_copy 'Color 5)
          (GM:EntPut mat_copy 'Linetype "Continuous") 
          (ssadd mat_copy ss_all)
          
          (command "_.-HATCH" "_P" "SOLID" "_S" mat_copy "" "") (setq hatch_mat (entlast))
          (GM:EntPut hatch_mat 'Color 5)
          (ssadd hatch_mat ss_all)

          (command "_.DRAWORDER" mat_copy hatch_mat "" "_F")

          ;; La geometria roja tambien se incorpora al mapa general de localizacion.
          (foreach h hatch_ents
            (command "_.COPY" h "" "_NON" pt_base "_NON" pt_base) (setq h_copy (entlast))
            (GM:EntPut h_copy 'Color 1)
            (GM:EntPut h_copy 'Linetype "Continuous")
            (ssadd h_copy ss_all)
          )

          (if (boundp 'GM:AutoForensic)
            (GM:AutoForensic *ML_TARGET_CODE* "ESQ_LO_XCLIP" "TEMP_LO INICIO"))
          (setq block_name (strcat "TEMP_LO_" (rtos (getvar "TDUSRTIMER") 2 6)))
          (setq block_name (vl-string-subst "" "." block_name))
          (GM:RegisterTempBlock block_name)
          (command "_.-BLOCK" block_name "_NON" pt_base ss_all "")
          
          (setvar "CTAB" orig_tab)

          (command "_.-INSERT" block_name "_NON" pt_dest_lo sf sf "0")
          (setq blk_lo_inserted (entlast))

          (setq pt_rect_1 (list (- (car pt_dest_lo) 40.0) (- (cadr pt_dest_lo) 30.0) 0.0) pt_rect_2 (list (+ (car pt_dest_lo) 40.0) (+ (cadr pt_dest_lo) 30.0) 0.0))
          (command "_.RECTANG" "_NON" pt_rect_1 "_NON" pt_rect_2) (setq rect_main_ent (entlast)) (GM:EntPut rect_main_ent 'Color 0)

          (command "_.-HATCH" "_P" "SOLID" "_S" rect_main_ent "" "") (setq hatch_bg_main_lo (entlast)) 
          (command "_.CHPROP" hatch_bg_main_lo "" "_C" "_T" "255,255,255" "")
          
          (command "_.XCLIP" blk_lo_inserted "" "_N" "_R" "_NON" pt_rect_1 "_NON" pt_rect_2)
          (if (boundp 'GM:AutoForensic)
            (GM:AutoForensic *ML_TARGET_CODE* "ESQ_LO_XCLIP" "XCLIP LO OK"))

          (setq pt_box_tl (list (- (car pt_dest_lo) 40.0) (- (cadr pt_dest_lo) 30.0) 0.0) pt_box_br (list (+ (car pt_dest_lo) 40.0) (- (cadr pt_dest_lo) 37.5) 0.0))
          (command "_.RECTANG" "_NON" pt_box_tl "_NON" pt_box_br) (setq box_ent (entlast)) (GM:EntPut box_ent 'Color 0)
          (command "_.-HATCH" "_P" "SOLID" "_S" box_ent "" "") (setq hatch_bg (entlast)) (command "_.CHPROP" hatch_bg "" "_C" "_T" "255,210,180" "")

          (setq pt_text_center (list (car pt_dest_lo) (- (cadr pt_dest_lo) 33.75) 0.0))
          (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText") (cons 10 pt_text_center) (cons 40 1.8) (cons 41 80.0) (cons 71 5) (cons 7 "TEXTO RESALTADO") (cons 1 (strcat "{\\fCalibri|b1;" str_texto "}")))) 
          (setq text_ent (entlast))

          ;; T1.1 REVERTIDO: cascada individual por apilado exacto.
          (command "_.DRAWORDER" hatch_bg_main_lo "" "_B")
          (command "_.DRAWORDER" blk_lo_inserted "" "_F")
          (command "_.DRAWORDER" rect_main_ent "" "_F")
          (command "_.DRAWORDER" hatch_bg "" "_F")         
          (command "_.DRAWORDER" box_ent "" "_F")
          (command "_.DRAWORDER" text_ent "" "_F")         
          
          ;; T1.2 2026-09-28: REGEN solo manual (en lote era costo puro).
          (if (not *GM_AUTO_MODE*) (command "_.REGEN"))

             ;; El CSV localiza la U.C. del predio en el mapa base; el circulo
             ;; pequeno lo marca y el grande muestra la misma zona ampliada.
             (setq selected_af_rect nil)
             (if (and (= *GM_AUTO_SHOW_PREDIO_LOC* "1") *GM_AUTO_PREDIO_LOC_RECT*)
               (progn
                 (setq zoom_center (list (/ (+ (car (car *GM_AUTO_PREDIO_LOC_RECT*)) (car (cadr *GM_AUTO_PREDIO_LOC_RECT*))) 2.0)
                                         (/ (+ (cadr (car *GM_AUTO_PREDIO_LOC_RECT*)) (cadr (cadr *GM_AUTO_PREDIO_LOC_RECT*))) 2.0) 0.0))
                 ;; Llamada compacta: el circulo pequeno solo marca el predio.
                 ;; ROT1 2026-10-21: los radios del circulo pequeno bajan en la
                 ;; misma proporcion del circulo grande (factor 0.864 = 9.5/11):
                 ;; 1.6 -> 1.38, 1.3 -> 1.12, 1.9 -> 1.64.
                 (command "_.CIRCLE" "_NON" zoom_center "1.38")
                (setq zoom_source_circle (entlast) selected_af_rect zoom_source_circle)
                 (GM:EntPut zoom_source_circle 'Color 94)
                 (GM:EntPut zoom_source_circle 'Lineweight 200)
                 ;; Anillos concentricos: superan visualmente el maximo de 2.11 mm.
                 ;; ROT1: con el radio al 86% el lineweight 2.11 los cerraba en
                 ;; un punto macizo; bajan a 2.00 mm (el estandar mas cercano
                 ;; al 1.82 proporcional) para que sigan leyendose como anillos.
                 (command "_.CIRCLE" "_NON" zoom_center "1.12") (setq zoom_source_inner (entlast))
                 (command "_.CIRCLE" "_NON" zoom_center "1.64") (setq zoom_source_outer (entlast))
                  ;; El zoom se coloca al lado opuesto del predio sobre la roja
                  ;; y clamped dentro del mapa para que el grande muestre la
                  ;; linea roja en vez de caer fuera del marco.
                  ;; Todo el mapa: marco completo para que el grande no caiga fuera.
                  (setq zoom_mbox (GM:MapContentBox) zoom_mapbox zoom_mbox)
                  ;; PRY1-MULTI F4 2026-10-14: marco unico compartido con el
                  ;; rotulo GEO (GM:ZoomFrameFor en LAY_COMUN); paridad exacta
                  ;; con la matematica que antes vivia inline en este bloque.
                  (setq zoom_frame (if (and (boundp '*GM_AUTO_ZOOM_FRAME*) *GM_AUTO_ZOOM_FRAME*)
                                     *GM_AUTO_ZOOM_FRAME*
                                     (GM:ZoomFrameFor zoom_center zoom_mbox)))
                 ;; ROT1 2026-10-21: el circulo grande baja a 9.5 (antes 11) con los anillos
                 ;; a la misma distancia (9.1 interior, 9.9 exterior); el relleno
                 ;; blanco y el recorte XCLIP comparten el radio 9.5.
                 (command "_.CIRCLE" "_NON" zoom_frame "9.5")
                 (setq zoom_circle (entlast))
                (command "_.-HATCH" "_P" "SOLID" "_S" zoom_circle "" "")
                (setq zoom_bg (entlast))
                 (command "_.CHPROP" zoom_bg "" "_C" "_T" "255,255,255" "")
                 (command "_.DRAWORDER" zoom_bg "" "_F")
                 (command "_.CIRCLE" "_NON" zoom_frame "9.1") (setq zoom_circle_inner (entlast))
                 (command "_.CIRCLE" "_NON" zoom_frame "9.9") (setq zoom_circle_outer (entlast))
                                     (if (boundp 'GM:AutoForensic)
                     (GM:AutoForensic *ML_TARGET_CODE* "ESQ_ZOOM_CLIP" "zoom INICIO"))
                     (setq zoom_ss (ssadd))
                     ;; PRY1-GEO7 2026-10-19 (F2): mapa DENTRO de un bloque
                     ;; (Chagual): el localizador publica *GM_AUTO_PREDIO_ZOOM_BLK*
                     ;; y aqui se recrea el tramo de la ruta (entidades nuevas,
                     ;; nunca el INSERT x6). Si no aplica, siguen los respaldos.
                     (if *GM_AUTO_PREDIO_ZOOM_BLK*
                       (progn
                         (setq zoom_ss2 (GM:MapBlockZoomContent *GM_AUTO_PREDIO_ZOOM_BLK* zoom_center))
                         (if zoom_ss2 (setq zoom_ss zoom_ss2))
                         (setq zoom_ss2 nil)
                         (if *GM_AUTO_MODE*
                           (GM:AutoTrace *ML_TARGET_CODE* "PREDIO_ZOOM_FUENTE" "INFO"
                             (strcat "ruta del bloque=" (itoa (sslength zoom_ss)))))))
                     ;; CHG3-FIX2: si el mapa vive en el MODELO (VP anidado), el
                     ;; contenido del zoom se recrea en papel (bubble 5.18x) desde
                     ;; las curvas del modelo; sin datos del VP, captura clasica.
                     ;; PRY1-GEO7 (F2): solo si el tramo del bloque no produjo nada.
                     (if (and (= (sslength zoom_ss) 0)
                              (boundp '*GM_AUTO_MAPVP_INFO*) *GM_AUTO_MAPVP_INFO*
                              (boundp '*GM_AUTO_MAPVP_MODELPT*) *GM_AUTO_MAPVP_MODELPT*)
                       (progn
                         (setq zoom_ss2 (GM:MapVPZoomContent *GM_AUTO_MAPVP_INFO* *GM_AUTO_MAPVP_MODELPT* zoom_frame))
                         (if zoom_ss2 (setq zoom_ss zoom_ss2))
                         (setq zoom_ss2 nil)))
                    (if (= (sslength zoom_ss) 0)
                      (foreach zoom_handle *GM_AUTO_PREDIO_LOC_ENT_HANDLES*
                       (if (and zoom_handle (setq zoom_source (handent zoom_handle)) (entget zoom_source)
                                (/= (cdr (assoc 8 (entget zoom_source))) "0. DEP. PROV. DIST.")
                                ;; PRY1-GEO7 (F2): el INSERT del mapa nunca se
                                ;; copia entero (era el mapa gigante en el zoom).
                                (/= "INSERT" (cdr (assoc 0 (entget zoom_source))))
                                (/= (cdr (assoc 5 (entget zoom_source)))
                                    (if (boundp '*GM_AUTO_PREDIO_LOC_ANCHOR*) *GM_AUTO_PREDIO_LOC_ANCHOR* "")))
                        (progn
                          ;; Copia verificada: si COPY falla (capa bloqueada),
                          ;; entlast no cambia y no se agrega basura al zoom.
                          (setq zoom_before (entlast))
                          (command "_.COPY" zoom_source "" "_NON" zoom_center "_NON" zoom_center)
                          (setq zoom_copy (entlast))
                          (if (and zoom_copy (not (equal zoom_copy zoom_before)) (entget zoom_copy))
                            (ssadd zoom_copy zoom_ss))))))
                  ;; Respaldo: captura directa ajustada al recorte (9.5/5.18 = 1.83).
                  ;; Ventana chica para no arrastrar textos gigantes al zoom.
                  (if (= (sslength zoom_ss) 0)
                    (progn
                      (setq zoom_fb_p1 (list (- (car zoom_center) 3.0) (- (cadr zoom_center) 2.5) 0.0)
                            zoom_fb_p2 (list (+ (car zoom_center) 3.0) (+ (cadr zoom_center) 2.5) 0.0)
                            zoom_fb_ss (ssget "_C" zoom_fb_p1 zoom_fb_p2)
                            zoom_fb_i 0)
                      (if (not zoom_fb_ss)
                        (progn
                          (setq zoom_fb_p1 (list (- (car zoom_center) 6.0) (- (cadr zoom_center) 5.0) 0.0)
                                zoom_fb_p2 (list (+ (car zoom_center) 6.0) (+ (cadr zoom_center) 5.0) 0.0)
                                zoom_fb_ss (ssget "_C" zoom_fb_p1 zoom_fb_p2))))
                      (if zoom_fb_ss
                        (while (< zoom_fb_i (sslength zoom_fb_ss))
                          (setq zoom_fb_ent (ssname zoom_fb_ss zoom_fb_i))
                          ;; No copiar los propios circulos/fondo del zoom ni
                          ;; los rotulos DEP/PROV/DIST (CHG3-FIX2: evita letras
                          ;; gigantes del rotulo en el circulo).
                          (if (and zoom_fb_ent (entget zoom_fb_ent)
                                   (not (member zoom_fb_ent (list zoom_circle zoom_source_circle zoom_source_inner zoom_source_outer zoom_bg)))
                                   ;; PRY1-GEO7 (F2): el INSERT del mapa nunca
                                   ;; entra al respaldo (saliria gigante).
                                   (/= "INSERT" (cdr (assoc 0 (entget zoom_fb_ent))))
                                   (/= (cdr (assoc 8 (entget zoom_fb_ent))) "0. DEP. PROV. DIST.")
                                   (/= (cdr (assoc 5 (entget zoom_fb_ent)))
                                       (if (boundp '*GM_AUTO_PREDIO_LOC_ANCHOR*) *GM_AUTO_PREDIO_LOC_ANCHOR* "")))
                            (progn
                              (setq zoom_before (entlast))
                              (command "_.COPY" zoom_fb_ent "" "_NON" zoom_center "_NON" zoom_center)
                              (setq zoom_copy (entlast))
                              (if (and zoom_copy (not (equal zoom_copy zoom_before)) (entget zoom_copy))
                                (ssadd zoom_copy zoom_ss))))
                          (setq zoom_fb_i (1+ zoom_fb_i))))
                      (if *GM_AUTO_MODE*
                        (GM:AutoTrace *ML_TARGET_CODE* "PREDIO_ZOOM_RESPALDO" "INFO"
                          (strcat "captura directa=" (itoa (sslength zoom_ss)))))))
                  ;; Garantia final lote (planos 2-5 en blanco): la roja pasa
                  ;; por el centro; si nada se capturo se copia la roja para
                  ;; que el grande nunca salga vacio.
                  (if (= (sslength zoom_ss) 0)
                    (progn
                      (setq zoom_route_info
                        (if (and (boundp '*GM_AUTO_MAPCONTENT_BOX*) *GM_AUTO_MAPCONTENT_BOX*)
                          (vl-catch-all-apply 'GM:LocRegionalRoute (list *GM_AUTO_MAPCONTENT_BOX*))
                          (vl-catch-all-apply 'GM:LocRegionalRoute (list (GM:MapContentBox)))))
                      (if (vl-catch-all-error-p zoom_route_info) (setq zoom_route_info nil))
                      (if (and zoom_route_info (car zoom_route_info)
                               (setq zoom_source (handent (caddr zoom_route_info)))
                               (entget zoom_source))
                        (progn
                          (setq zoom_before (entlast))
                          (command "_.COPY" zoom_source "" "_NON" zoom_center "_NON" zoom_center)
                          (setq zoom_copy (entlast))
                          (if (and zoom_copy (not (equal zoom_copy zoom_before)) (entget zoom_copy))
                            (ssadd zoom_copy zoom_ss))))
                      (if *GM_AUTO_MODE*
                        (GM:AutoTrace *ML_TARGET_CODE* "PREDIO_ZOOM_ROJA" "INFO"
                          (strcat "forzado roja=" (itoa (sslength zoom_ss)))))))
                 (if (> (sslength zoom_ss) 0)
                  (progn
                    (setq zoom_block (vl-string-subst "" "." (strcat "TEMP_LO_ZOOM_" (rtos (getvar "TDUSRTIMER") 2 6))) )
                    (GM:RegisterTempBlock zoom_block)
                    (command "_.-BLOCK" zoom_block "_NON" zoom_center zoom_ss "")
                    ;; Zoom grande: escala 5.18 (antes 6.0). ROT1 2026-10-21: el
                    ;; circulo bajo a 9.5 y la escala baja en la misma proporcion
                    ;; (0.864), de modo que la ventana real del zoom sigue siendo
                    ;; la misma (9.5/5.18 = 11/6 = 1.83) y no se recorta el
                    ;; circulo del predio.
                    (command "_.-INSERT" zoom_block "_NON" zoom_frame "5.18" "5.18" "0")
                    (setq zoom_insert (entlast))
                    ;; XCLIP requiere una polilinea cerrada; 24 lados produce
                    ;; una ventana circular al imprimir sin depender de XCLIPFRAME.
                    (command "_.PLINE")
                    (setq zoom_i 0)
                    (repeat 24
                       (command "_NON" (polar zoom_frame (* 2.0 pi (/ zoom_i 24.0)) 9.5))
                      (setq zoom_i (1+ zoom_i)))
                    (command "_C")
                    (setq zoom_clip (entlast))
                    (command "_.XCLIP" zoom_insert "" "_N" "_S" zoom_clip "")
                    (command "_.DRAWORDER" zoom_insert "" "_F")
                    (if (boundp 'GM:AutoForensic)
                      (GM:AutoForensic *ML_TARGET_CODE* "ESQ_ZOOM_CLIP"
                        (strcat "zoom n=" (itoa (sslength zoom_ss))))))
                  ;; Fallo cerrado de raiz: jamas exportar el grande en blanco.
                  ;; Si no hubo contenido, el plano se marca ERROR en el reporte
                  ;; en vez de salir con el circulo vacio.
                  (if (and *GM_AUTO_MODE* (= (sslength zoom_ss) 0))
                    (progn
                      (GM:AutoTrace *ML_TARGET_CODE* "PREDIO_ZOOM" "ERROR" "Circulo grande sin contenido")
                      (setq *GM_AUTO_LAYOUT_FAILED* T
                            *GM_AUTO_LAST_MESSAGE* "Zoom del circulo grande vacio; plano incompleto"))))
                 ;; Circulos y conectores verdes, separados de la cartografia roja.
                 (GM:EntPut zoom_circle 'Color 94)
                 (GM:EntPut zoom_circle 'Lineweight 211)
                 (setq zoom_angle (angle zoom_center zoom_frame))
                 (command "_.LINE" "_NON" (polar zoom_center zoom_angle 1.64) "_NON" (polar zoom_frame (+ zoom_angle pi) 9.9) "")
                 (setq zoom_leader1 (entlast))
                 ;; Los conectores se mandan detras; los circulos gruesos quedan
                 ;; encima y los recortan exactamente en sus bordes.
                 (foreach h (list zoom_leader1)
                   (if h (progn
                     (GM:EntPut h 'Color 94)
                     (GM:EntPut h 'Lineweight 211)
                     (GM:EntPut h 'Linetype "Continuous")
                     (command "_.DRAWORDER" h "" "_F"))))
                 (foreach h (list zoom_circle zoom_circle_inner zoom_circle_outer zoom_source_circle zoom_source_inner zoom_source_outer)
                   (if h (progn
                     (GM:EntPut h 'Color 94)
                     ;; ROT1 2026-10-21: los anillos pequenos (radios al 86%)
                     ;; bajan su lineweight a 2.00 mm; con 2.11 se cerraban en
                     ;; un punto macizo. El circulo grande conserva 2.11.
                     (GM:EntPut h 'Lineweight (if (member h (list zoom_source_circle zoom_source_inner zoom_source_outer)) 200 211))
                     (command "_.DRAWORDER" h "" "_F"))))
                 (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                                '(62 . 0) (cons 10 (list (car zoom_frame) (- (cadr zoom_frame) 11.3) 0.0))
                                (cons 40 1.35) (cons 41 30.0) (cons 71 5) (cons 7 "TEXTO RESALTADO")
                                (cons 1 "{\\fCalibri|b1|i1;PREDIO AFECTADO}")))
                 (setq zoom_label (entlast))
                 (command "_.DRAWORDER" zoom_label "" "_F")))

             ;; El detalle rectangular anterior queda desactivado: el unico
             ;; detalle es el circulo ampliado del mapa de localizacion.
            (setq detail_sources nil)
            (setq detail_sel nil)
             (if nil
              (progn
                 (setq detail_sel nil)))
            ;; Los mapas cartograficos de plantilla no siempre conservan
            ;; hipervinculos. Con una sola afectacion, su detalle es inequ\\U+00EDvoco.
             (if nil
              (setq detail_sel (car all_suffixes)))
            (if detail_sel
              (progn
                (setq match_af (assoc detail_sel af_list) match_pf (assoc detail_sel pf_list))
                (if match_af (setq detail_sources (cons (cadr match_af) detail_sources)))
                (if (and match_pf (not (member (cadr match_pf) detail_sources)))
                  (setq detail_sources (cons (cadr match_pf) detail_sources)))))
             (if nil
             (progn
               (setq detail_min nil detail_max nil)
               (foreach h detail_sources
                 ;; AUD1: rama desactivada (if nil); si se reactivara, el
                 ;; wrapper ya no fuga: catch + release inmediato.
                 (setq _detail_obj (vl-catch-all-apply 'vlax-ename->vla-object (list h)))
                 (if (vl-catch-all-error-p _detail_obj)
                   (setq _detail_obj nil))
                 (vla-getboundingbox _detail_obj 'detail_lo 'detail_hi)
                 (GM:ReleaseObjectSafe _detail_obj)
                 (setq detail_lo (vlax-safearray->list detail_lo) detail_hi (vlax-safearray->list detail_hi))
                 (if (not detail_min)
                   (setq detail_min detail_lo detail_max detail_hi)
                   (setq detail_min (list (min (car detail_min) (car detail_lo)) (min (cadr detail_min) (cadr detail_lo)) 0.0)
                         detail_max (list (max (car detail_max) (car detail_hi)) (max (cadr detail_max) (cadr detail_hi)) 0.0)))
               )
               (setq detail_base (list (/ (+ (car detail_min) (car detail_max)) 2.0) (/ (+ (cadr detail_min) (cadr detail_max)) 2.0) 0.0)
                     detail_w (- (car detail_max) (car detail_min))
                     detail_h (- (cadr detail_max) (cadr detail_min)))
               (if (< detail_w 0.001) (setq detail_w 1.0))
               (if (< detail_h 0.001) (setq detail_h 1.0))
               (setq detail_sf (min (/ 26.0 detail_w) (/ 14.0 detail_h)) detail_ss (ssadd))
               (setvar "CTAB" "Model")
               (foreach h detail_sources
                 (command "_.COPY" h "" "_NON" detail_base "_NON" detail_base) (setq detail_copy (entlast))
                 (GM:EntPut detail_copy 'Color 1)
                 (GM:EntPut detail_copy 'Linetype "Continuous")
                 (ssadd detail_copy detail_ss))
               (setq detail_block (vl-string-subst "" "." (strcat "TEMP_LO_AF_" (rtos (getvar "TDUSRTIMER") 2 6))))
               (GM:RegisterTempBlock detail_block)
               (command "_.-BLOCK" detail_block "_NON" detail_base detail_ss "")
               (setvar "CTAB" orig_tab)
               (setq detail_p1 (list (- (car pt_dest_lo) 37.0) (- (cadr pt_dest_lo) 27.0) 0.0)
                     detail_p2 (list (- (car pt_dest_lo) 7.0) (- (cadr pt_dest_lo) 7.0) 0.0)
                     detail_center (list (/ (+ (car detail_p1) (car detail_p2)) 2.0) (/ (+ (cadr detail_p1) (cadr detail_p2)) 2.0) 0.0))
               (command "_.RECTANG" "_NON" detail_p1 "_NON" detail_p2) (setq detail_rect (entlast)) (GM:EntPut detail_rect 'Color 1)
               (command "_.-HATCH" "_P" "SOLID" "_S" detail_rect "" "") (setq detail_bg (entlast)) (command "_.CHPROP" detail_bg "" "_C" "_T" "255,255,255" "")
               (command "_.-INSERT" detail_block "_NON" detail_center detail_sf detail_sf "0") (setq detail_insert (entlast))
               (command "_.XCLIP" detail_insert "" "_N" "_R" "_NON" detail_p1 "_NON" detail_p2)
               (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText") (cons 10 (list (car detail_center) (- (cadr detail_p2) 1.5) 0.0)) (cons 40 1.0) (cons 41 28.0) (cons 71 5) (cons 7 "SANSSERIF") (cons 1 "DETALLE \\U+00C1REA AFECTADA")))
               (setq detail_text (entlast))
                ;; T1.1 REVERTIDO: cascada individual por apilado exacto.
                (command "_.DRAWORDER" detail_bg "" "_B")
                (command "_.DRAWORDER" detail_insert "" "_F")
                (command "_.DRAWORDER" detail_rect "" "_F")
                (command "_.DRAWORDER" detail_text "" "_F"))
           )

          (setq e_l1 nil e_t1 nil e_l2 nil e_t2 nil)
           (setq pt_arr1 (if *GM_AUTO_MODE* (GM:SchemaAnchor matriz pt_base pt_dest_lo sf) (GM:GetLayoutPoint "\nHaz clic en el pol\\U+00EDgono azul (Inicio de flecha) o ENTER para omitir: ")))
           (if pt_arr1
              (if (setq pt_txt1 (if *GM_AUTO_MODE* (list (+ (car pt_dest_lo) 22.0) (+ (cadr pt_dest_lo) 18.0) 0.0) (getpoint pt_arr1 "\nHaz clic para ubicar 'PREDIO AFECTADO': ")))
              (progn
                 (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(370 . 13) '(100 . "AcDbLine") (cons 10 pt_arr1) (cons 11 pt_txt1)))
                 (setq e_l1 (entlast))
                 (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(370 . 13) '(100 . "AcDbMText") (cons 10 pt_txt1) (cons 40 1.15) (cons 41 0.0) (cons 71 5) (cons 7 "SANSSERIF") (cons 1 "PREDIO AFECTADO") '(90 . 19) '(45 . 1.25)))
                 ;; T1.1: par linea+texto con un solo DRAWORDER (antes 2).
                 (setq e_t1 (entlast) drw_ss (ssadd))
                 (ssadd e_l1 drw_ss) (ssadd e_t1 drw_ss)
                 (command "_.DRAWORDER" drw_ss "" "_F")
               )
            )
          )
           (setq pt_arr2 (if *GM_AUTO_MODE* (if (= (length dv_list) 1) (GM:SchemaAnchor (car dv_list) pt_base pt_dest_lo sf) (GM:LayoutOmit "DV_ESQUEMA_AMBIGUO")) (GM:GetLayoutPoint "\nHaz clic en el Derecho de V\\U+00EDa color 230 (Inicio de flecha) o ENTER para omitir: ")))
           (if pt_arr2
              (if (setq pt_txt2 (if *GM_AUTO_MODE* (list (+ (car pt_dest_lo) 22.0) (- (cadr pt_dest_lo) 18.0) 0.0) (getpoint pt_arr2 "\nHaz clic para ubicar 'DERECHO DE V\\U+00CDA': ")))
              (progn
                 (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(370 . 13) '(100 . "AcDbLine") (cons 10 pt_arr2) (cons 11 pt_txt2)))
                 (setq e_l2 (entlast))
                 (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(370 . 13) '(100 . "AcDbMText") (cons 10 pt_txt2) (cons 40 1.15) (cons 41 0.0) (cons 71 5) (cons 7 "SANSSERIF") (cons 1 "DERECHO\\PDE V\\U+00CDA") '(90 . 19) '(45 . 1.25)))
                 ;; T1.1: par linea+texto con un solo DRAWORDER (antes 2).
                 (setq e_t2 (entlast) drw_ss (ssadd))
                 (ssadd e_l2 drw_ss) (ssadd e_t2 drw_ss)
                 (command "_.DRAWORDER" drw_ss "" "_F")
               )
            )
          )

          (setq ss_blk_lo (ssadd))
          (ssadd hatch_bg_main_lo ss_blk_lo)
          (ssadd blk_lo_inserted ss_blk_lo)
          (ssadd rect_main_ent ss_blk_lo)
          (ssadd hatch_bg ss_blk_lo)
          (ssadd box_ent ss_blk_lo)
          (ssadd text_ent ss_blk_lo)
          (if e_l1 (ssadd e_l1 ss_blk_lo))
          (if e_t1 (ssadd e_t1 ss_blk_lo))
          (if e_l2 (ssadd e_l2 ss_blk_lo))
          (if e_t2 (ssadd e_t2 ss_blk_lo))
          (if selected_af_rect (ssadd selected_af_rect ss_blk_lo))
          (if zoom_bg (ssadd zoom_bg ss_blk_lo))
          (if zoom_insert (ssadd zoom_insert ss_blk_lo))
          (if (and zoom_clip (entget zoom_clip)) (ssadd zoom_clip ss_blk_lo))
          (if zoom_circle (ssadd zoom_circle ss_blk_lo))
          (if zoom_circle_inner (ssadd zoom_circle_inner ss_blk_lo))
          (if zoom_circle_outer (ssadd zoom_circle_outer ss_blk_lo))
          (if zoom_source_inner (ssadd zoom_source_inner ss_blk_lo))
          (if zoom_source_outer (ssadd zoom_source_outer ss_blk_lo))
          (if zoom_leader1 (ssadd zoom_leader1 ss_blk_lo))
          (if zoom_label (ssadd zoom_label ss_blk_lo))
          (if detail_bg (ssadd detail_bg ss_blk_lo))
          (if detail_insert (ssadd detail_insert ss_blk_lo))
          (if detail_rect (ssadd detail_rect ss_blk_lo))
          (if detail_text (ssadd detail_text ss_blk_lo))

          ;; DIAG1 2026-10-03: cierre del bloque LO con sondas antes/despues.
          (if (boundp 'GM:AutoForensic)
            (GM:AutoForensic *ML_TARGET_CODE* "ESQ_LO_FIN" "cierre LO INICIO"))
          (setq blk_name_lo (vl-string-subst "" "." (strcat "ESQ_LO_FIN_" (rtos (getvar "TDUSRTIMER") 2 6))))
          (GM:RegisterTempBlock blk_name_lo)
          (command "_.-BLOCK" blk_name_lo "_NON" pt_dest_lo ss_blk_lo "")
          (command "_.-INSERT" blk_name_lo "_NON" pt_dest_lo "1" "1" "0")
          (if (boundp 'GM:AutoForensic)
            (GM:AutoForensic *ML_TARGET_CODE* "ESQ_LO_FIN" "cierre LO OK"))

          (princ "\n> Esquema de Localizaci\\U+00F3n finalizado.")
        )
      )
    )
  )
  ;; DIAG1 2026-10-03: latido final de esquemas; si falta en el forense tras un
  ;; corte, la muerte ocurrio dentro de EXEC_ESQUEMAS_PU_FULL.
  (if (boundp 'GM:AutoForensic)
    (GM:AutoForensic *ML_TARGET_CODE* "ESQ_FIN" "esquemas completos"))
  (setq *error* olderr)
)
