;; ============================================================================
;; GM_LOTE.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================



(defun GM:AutoBatchPanel (/ dcl_file file dcl_id status)
  ;; CHUNK 2026-09-29: en modo continuacion (GM_CONTINUAR_LOTE) el panel no se
  ;; muestra; el estado ya validado queda en *GM_AUTO_PANEL_STATUS* (1/2).
  (if *GM_AUTO_SKIP_PANEL*
    (progn (setq *GM_AUTO_SKIP_PANEL* nil) *GM_AUTO_PANEL_STATUS*)
    (progn
  (setq dcl_file (vl-filename-mktemp "gm_lote_panel.dcl")
        file (open dcl_file "w"))
  (if file
    (progn
      (write-line "gm_lote_panel : dialog { label = \"GM Ingenieros - Generacion por lote\"; width = 58;" file)
      (write-line "  : boxed_column { label = \"1. Entradas\";" file)
      (write-line "    : row { : button { key = \"btn_csv\"; label = \"Seleccionar CSV...\"; width = 18; } : text { key = \"txt_csv\"; label = \"No seleccionado\"; width = 34; } }" file)
      (write-line "    : row { : button { key = \"btn_folder\"; label = \"Carpeta de salida...\"; width = 18; } : text { key = \"txt_folder\"; label = \"No seleccionada\"; width = 34; } }" file)
      (write-line "    : boxed_row { label = \"Deseas visualizar el predio afectado en el esquema de localizacion\"; : button { key = \"btn_show_predio\"; label = \"MOSTRAR PREDIO AFECTADO\"; width = 26; } : button { key = \"btn_hide_predio\"; label = \"NO MOSTRAR PREDIO AFECTADO\"; width = 26; } : text { key = \"txt_predio_loc\"; label = \"Mostrar automaticamente\"; width = 18; } }" file)
      (write-line "    : boxed_row { label = \"Deseas dibujar el rotulo DEPARTAMENTO/PROVINCIA/DISTRITO en el mapa\"; : button { key = \"btn_show_geo\"; label = \"DIBUJAR ROTULO GEO\"; width = 26; } : button { key = \"btn_hide_geo\"; label = \"NO DIBUJAR ROTULO GEO\"; width = 26; } : text { key = \"txt_geo_txt\"; label = \"Dibujar automaticamente\"; width = 18; } }" file)
      (write-line "  }" file)
      (write-line "  : boxed_column { label = \"2. Previsualizacion del CSV\";" file)
      (write-line "    : text { key = \"txt_count\"; label = \"Codigos validos: 0\"; }" file)
      (write-line "    : list_box { key = \"lst_codes\"; width = 48; height = 12; fixed_width = true; fixed_height = true; }" file)
      (write-line "  }" file)
      (write-line "  : boxed_column { label = \"3. Salidas\";" file)
      (write-line "    : text { label = \"DWG principal siempre. Elegir salida con el boton.\"; }" file)
      (write-line "    : row { : button { key = \"btn_solo_dwg\"; label = \"GENERAR SOLO DWG\"; width = 22; } : button { key = \"btn_dwg_pdf\"; label = \"GENERAR DWG + PDF\"; width = 22; } }" file)
      (write-line "  }" file)
      (write-line "  spacer; : row { : button { key = \"cancel\"; label = \"Cancelar\"; is_cancel = true; } }" file)
      (write-line "}" file)
      (close file)
      (setq dcl_id (load_dialog dcl_file))
      (if (and dcl_id (> dcl_id 0) (new_dialog "gm_lote_panel" dcl_id))
        (progn
          (set_tile "txt_csv" (if *GM_AUTO_CSV_PATH* (strcat "CSV: " (vl-filename-base *GM_AUTO_CSV_PATH*)) "No seleccionado"))
          (set_tile "txt_folder" (if *GM_AUTO_OUTPUT_FOLDER* *GM_AUTO_OUTPUT_FOLDER* "No seleccionada"))
          (set_tile "txt_predio_loc" (if (= *GM_AUTO_SHOW_PREDIO_LOC* "1") "Mostrar automaticamente" "No mostrar"))
          (set_tile "txt_geo_txt" (if (= (GM:ResolveShowGeoTxt *GM_AUTO_SHOW_GEO_TXT*) "1") "Dibujar automaticamente" "No dibujar"))
          (GM:AutoSetPreviewList)
          (action_tile "btn_csv" "(GM:AutoSelectCsvFromPanel)")
          (action_tile "btn_folder" "(GM:AutoSelectOutputFolderFromPanel)")
          (action_tile "btn_show_predio" "(setq *GM_AUTO_SHOW_PREDIO_LOC* \"1\") (set_tile \"txt_predio_loc\" \"Mostrar automaticamente\")")
          (action_tile "btn_hide_predio" "(setq *GM_AUTO_SHOW_PREDIO_LOC* \"0\") (set_tile \"txt_predio_loc\" \"No mostrar\")")
          (action_tile "btn_show_geo" "(setq *GM_AUTO_SHOW_GEO_TXT* \"1\") (set_tile \"txt_geo_txt\" \"Dibujar automaticamente\")")
          (action_tile "btn_hide_geo" "(setq *GM_AUTO_SHOW_GEO_TXT* \"0\") (set_tile \"txt_geo_txt\" \"No dibujar\")")
          (action_tile "btn_solo_dwg" "(if (and *GM_AUTO_CSV_PATH* (findfile *GM_AUTO_CSV_PATH*) *GM_AUTO_OUTPUT_FOLDER* (vl-file-directory-p *GM_AUTO_OUTPUT_FOLDER*)) (done_dialog 1) (alert \"Selecciona un CSV existente y una carpeta de salida valida.\"))")
          (action_tile "btn_dwg_pdf" "(if (and *GM_AUTO_CSV_PATH* (findfile *GM_AUTO_CSV_PATH*) *GM_AUTO_OUTPUT_FOLDER* (vl-file-directory-p *GM_AUTO_OUTPUT_FOLDER*)) (done_dialog 2) (alert \"Selecciona un CSV existente y una carpeta de salida valida.\"))")
          (action_tile "cancel" "(done_dialog 0)")
          (setq status (start_dialog))
        )
      )
      (if (and dcl_id (> dcl_id 0)) (unload_dialog dcl_id))
      (vl-file-delete dcl_file)
      status
    )
    0
  )
    )
  )
)

(defun GM:AutoApplyCsvDefaults (row / cond_jur part_num)
  ;; Replica las reglas de LUBI_ParseCSV cuando el CSV no esta disponible
  ;; como ruta global durante una corrida ya iniciada.
  (setq cond_jur (strcase (if (nth 2 row) (nth 2 row) ""))
        part_num (strcase (if (nth 4 row) (nth 4 row) "")))
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
        (setq *ML_ESQ_PE_TOG* "1" *ML_ESQ_PE_VAL* part_num)))
    ((vl-string-search "POSESIONARIO" cond_jur)
      (setq *ML_ESQ_LOC_TIPO* "1" *ML_ESQ_PE_TOG* "0" *ML_ESQ_PE_VAL* ""))
    ((vl-string-search "OCUPANTE" cond_jur)
      (setq *ML_ESQ_LOC_TIPO* "2" *ML_ESQ_PE_TOG* "0" *ML_ESQ_PE_VAL* ""))
    (T (setq *ML_ESQ_LOC_TIPO* "0" *ML_ESQ_PE_TOG* "0" *ML_ESQ_PE_VAL* ""))
  )
  T
)

(defun GM:AutoFileExists (path / file)
  (if (and path (setq file (open path "r")))
    (progn (close file) T)
    nil
  )
)

(defun GM:AutoExportPdf (output_path / acad_doc acad_app plot old_bg old_filedia old_textfill result ok)
  ;; PlotToFile usa el Layout activo; el ejecutor debe dejarlo preparado antes.
  (if (and output_path (/= output_path ""))
    (progn
      ;; AUD1: toda la cadena COM con catch y un unico punto de liberacion; un
      ;; fallo de COM deja mensaje y ok=nil sin tumbar el DWG (contrato BB).
      (setq acad_app (vl-catch-all-apply 'vlax-get-acad-object '()))
      (if (vl-catch-all-error-p acad_app) (setq acad_app nil))
      (setq acad_doc (if acad_app (vl-catch-all-apply 'vla-get-ActiveDocument (list acad_app)) nil))
      (if (vl-catch-all-error-p acad_doc) (setq acad_doc nil))
      (setq plot (if acad_doc (vl-catch-all-apply 'vla-get-Plot (list acad_doc)) nil))
      (if (vl-catch-all-error-p plot) (setq plot nil))
      (setq old_bg (getvar "BACKGROUNDPLOT")
            old_filedia (getvar "FILEDIA")
            old_textfill (getvar "TEXTFILL"))
      (if plot
        (progn
          (setvar "BACKGROUNDPLOT" 0)
          (setvar "FILEDIA" 0)
          ;; ROT1 2026-10-21: TEXTFILL=1 asegura que el texto TrueType (el
          ;; rotulo DEP/PROV/DIST) se plasma RELLENO en el PDF. El DWG se
          ;; genera con TEXTFILL=1, pero la sesion que plotea puede venir con
          ;; 0 y el texto salia delineado (mas delgado). Se restaura el valor
          ;; original al terminar el plot.
          (setvar "TEXTFILL" 1)
          (if (GM:AutoFileExists output_path) (vl-file-delete output_path))
          (setq result (vl-catch-all-apply 'vla-PlotToFile (list plot output_path "DWG To PDF.pc3")))
          (setvar "BACKGROUNDPLOT" old_bg)
          (setvar "FILEDIA" old_filedia)
          (setvar "TEXTFILL" old_textfill)
          (if (vl-catch-all-error-p result)
            (setq *GM_AUTO_LAST_MESSAGE* (strcat "No se pudo exportar el PDF: " (vl-catch-all-error-message result)))
          )
          ;; FASE1 2026-10-21: el veredicto del PDF ya no es "existe". Un PDF
          ;; vacio o truncado pasaba como OK y el cliente recibia un archivo
          ;; roto. Se valida cabecera y tamano (igual que el DWG) y, si no
          ;; vale, se borra el archivo invalido: no puede quedar un PDF que
          ;; parezca recien generado.
          (if (and (not (vl-catch-all-error-p result)) (GM:AutoFileExists output_path))
            (if (GM:AutoPdfValid output_path)
              (setq ok T)
              (progn
                (setq *GM_AUTO_LAST_MESSAGE* (strcat "PDF exportado invalido (cabecera/tamano): " output_path))
                (setq ok nil)
                (if (GM:AutoFileExists output_path) (vl-file-delete output_path))))
            (progn
              (setq ok nil)
              (if (not (vl-catch-all-error-p result))
                (setq *GM_AUTO_LAST_MESSAGE* (strcat "El plot no dejo archivo PDF: " output_path))))))
        (progn
          (setq *GM_AUTO_LAST_MESSAGE* "No se pudo preparar la exportacion PDF (COM no disponible)"
                ok nil)))
      ;; Fase 1B (H-5): liberar Plot, documento y aplicacion.
      (vl-catch-all-apply 'vlax-release-object (list plot))
      (vl-catch-all-apply 'vlax-release-object (list acad_doc))
      (vl-catch-all-apply 'vlax-release-object (list acad_app))
      (setq plot nil acad_doc nil acad_app nil)
      ok
    )
    nil
  )
)

(defun GM:AutoWriteTest (path / f ok)
  ;; Pre-flight: comprueba escritura real en la carpeta destino.
  (setq ok nil)
  (if (and path (/= path "") (setq f (open path "w")))
    (progn (write-line "GM" f) (close f) (setq ok T)))
  ok)

(defun GM:AutoMarkFile (path text / f)
  ;; Marcador lateral: si el interprete muere dentro del export, el .tmp
  ;; huerfano lo delata (el reporte no podria contarlo).
  (if (and path (/= path "") (setq f (open path "w")))
    (progn (write-line text f) (close f) T)))

(defun GM:AutoExportDwg (output_path / filedia cmdecho cmddia tmp ok wargs)
  ;; BE: WBLOCK por comando ("*" = dibujo entero). El metodo COM anterior no
  ;; existe en este Civil (no function definition) y su llamada abortaba al
  ;; interprete por fuera de todas las redes Lisp. Sin ActiveX en esta ruta.
  ;; BG: CMDDIA a 0 + auto-respuesta Map configurable (ver *GM_AUTO_MAPA_ANS*).
  (if (and (boundp '*GM_AUTO_DWG_METODO*) (/= *GM_AUTO_DWG_METODO* "WBLOCK"))
    (progn (setq *GM_AUTO_LAST_MESSAGE* (strcat "Metodo DWG no soportado: " *GM_AUTO_DWG_METODO*)) nil)
  (if (not (getcname "_-WBLOCK"))
    ;; PA-3 (H8) 2026-09-28: pre-vuelo sin efectos; si el CAD no expone el
    ;; comando, no se desactiva FILEDIA ni se deja .tmp huerfano (fail-closed).
    (progn (setq *GM_AUTO_LAST_MESSAGE* (strcat "Pre-vuelo: comando _-WBLOCK no disponible en este CAD: " output_path) ok nil) nil)
  (if (or (not output_path) (= output_path ""))
    (progn (setq *GM_AUTO_LAST_MESSAGE* "Ruta DWG vacia") nil)
    (progn
      (if (not (GM:AutoWriteTest (strcat output_path ".writetest")))
        (progn (setq *GM_AUTO_LAST_MESSAGE* (strcat "Carpeta no escribible: " output_path)) nil)
        (progn
          (vl-file-delete (strcat output_path ".writetest"))
          (setq tmp (strcat output_path ".tmp"))
          (GM:AutoMarkFile tmp (strcat "DWG_ENTER " output_path))
          (if (GM:AutoFileExists output_path) (vl-file-delete output_path))
          (setq filedia (getvar "FILEDIA") cmdecho (getvar "CMDECHO") cmddia (getvar "CMDDIA"))
          (setvar "FILEDIA" 0) (setvar "CMDECHO" 0) (setvar "CMDDIA" 0)
          ;; BF: COMMAND fuera de todo catch-all (command dentro de catch-all
          ;; abortaba al interprete con .tmp huerfano). POR QUE apply: wargs
          ;; lleva 2 o 3 args segun *GM_AUTO_MAPA_ANS* ("_Yes"/"_No"/nil);
          ;; apply evita duplicar la llamada. Veredicto solo por archivo.
          ;; FIX6 2026-10-06 (AUD4): se retira el unwind-protect de FIX4. En
          ;; AutoCAD 2021, tras el WBLOCK, la salida del unwind-protect
          ;; abortaba la evaluacion en silencio: el cleanup corria completo
          ;; pero el veredicto y el resto de la funcion se perdian ("No
          ;; procesado", sin OK/PDF/RESULTADO y con reintento del lanzador).
          ;; Esta es la forma SYS1 de la corrida 202/202 del 05-10; el restore
          ;; de FILEDIA/CMDECHO/CMDDIA ante un error real ya lo hacen los
          ;; *error* por codigo y del lote, y el .tmp queda como marcador.
          (setq wargs (list "_.-WBLOCK" output_path "*"))
          (if (and (boundp '*GM_AUTO_MAPA_ANS*) *GM_AUTO_MAPA_ANS* (/= *GM_AUTO_MAPA_ANS* ""))
            (setq wargs (append wargs (list *GM_AUTO_MAPA_ANS*))))
          (apply 'command wargs)
          (setvar "FILEDIA" filedia) (setvar "CMDECHO" cmdecho) (setvar "CMDDIA" cmddia)
          (if (GM:AutoFileExists output_path)
            (if (GM:AutoDwgValid output_path)
              (setq ok T)
              (setq *GM_AUTO_LAST_MESSAGE* (strcat "DWG exportado invalido (cabecera/tamano): " output_path) ok nil))
            (setq *GM_AUTO_LAST_MESSAGE* (strcat "WBLOCK sin archivo de salida: " output_path) ok nil))
          (vl-file-delete tmp)
          ok)))))))

(defun GM:AutoCsvValue (value)
  (if (= (type value) 'STR)
    (progn
      (setq value (vl-string-trim " \t\n\r" value))
      (if (and (> (strlen value) 1) (= (substr value 1 1) "\"") (= (substr value (strlen value) 1) "\""))
        (setq value (substr value 2 (- (strlen value) 2)))
      )
      (while (vl-string-search "\"\"" value) (setq value (vl-string-subst "\"" "\"\"" value)))
      (while (vl-string-search "\n" value) (setq value (vl-string-subst "\\P" "\n" value)))
      value
    )
    ""
  )
)

(defun GM:AutoCountQuotes (text / count pos)
  (setq count 0 pos 0)
  (while (setq pos (vl-string-search "\"" text pos))
    (setq count (1+ count) pos (1+ pos))
  )
  count
)

(defun GM:AutoSplit (text delimiter / pos result)
  (setq result nil)
  (while (setq pos (vl-string-search delimiter text))
    (setq result (cons (substr text 1 pos) result)
          text (substr text (+ pos 1 (strlen delimiter))))
  )
  (reverse (cons text result))
)

;; F2 2026-10-05: contrato CSV en el LISP (19 columnas + codigo [A-Za-z0-9_-]).
;; Las filas con codigo real que no cumplen se descartan y se cuentan; el lote
;; lo traza como ETAPA;LOTE;CSV_CONTRATO;AVISO (fail-soft con evidencia).
(if (not (boundp '*GM_AUTO_CSV_BAD*)) (setq *GM_AUTO_CSV_BAD* 0))
(if (not (boundp '*GM_AUTO_CSV_BAD_MSG*)) (setq *GM_AUTO_CSV_BAD_MSG* ""))

;; F2: valida el contrato de codigo del lote (^[A-Za-z0-9_-]+$ en ASCII).
(defun GM:AutoValidCode (code / i n ch ok)
  (setq ok (and (= (type code) 'STR) (> (strlen code) 0)) i 1 n (strlen code))
  (while (and ok (<= i n))
    (setq ch (ascii (substr code i 1)))
    (if (not (or (and (>= ch 48) (<= ch 57)) (and (>= ch 65) (<= ch 90))
                 (and (>= ch 97) (<= ch 122)) (= ch 95) (= ch 45)))
      (setq ok nil))
    (setq i (1+ i))
  )
  ok
)

;; F2: separa respetando comillas ("a;b" no se parte) para el contrato CSV.
(defun GM:AutoSplitQuoted (text delimiter / i n ch cur parts inq next)
  (setq parts nil cur "" inq nil i 1 n (strlen text))
  (while (<= i n)
    (setq ch (substr text i 1))
    (cond
      ((= ch "\"")
        (setq next (if (< i n) (substr text (1+ i) 1) ""))
        (if (= next "\"")
          (progn (setq cur (strcat cur "\"") i (1+ i)))
          (setq inq (not inq))
        )
      )
      ((and (= ch delimiter) (not inq))
        (setq parts (cons cur parts) cur "")
      )
      (T (setq cur (strcat cur ch)))
    )
    (setq i (1+ i))
  )
  (reverse (cons cur parts))
)

(defun GM:AutoReadCsv (path / file line next sep row code rows seen)
  (setq rows nil seen nil *GM_AUTO_CSV_BAD* 0 *GM_AUTO_CSV_BAD_MSG* "")
  (if (and path (findfile path) (setq file (open (findfile path) "r")))
    (progn
      (while (setq line (read-line file))
        (while (= (rem (GM:AutoCountQuotes line) 2) 1)
          (setq next (read-line file))
          (if next (setq line (strcat line "\\P" next)) (setq next nil line (strcat line "\"")))
        )
        (setq sep (if (vl-string-search ";" line) ";" ","))
        (setq row (mapcar 'GM:AutoCsvValue (GM:AutoSplitQuoted line sep))
              code (if (> (length row) 14) (vl-string-trim " \t\r\n" (nth 14 row)) ""))
        (cond
          ((or (= code "") (= (strcase code) "CODIGO")) nil)
          ((not (GM:AutoValidCode code))
            (setq *GM_AUTO_CSV_BAD* (1+ *GM_AUTO_CSV_BAD*))
            (if (= *GM_AUTO_CSV_BAD_MSG* "")
              (setq *GM_AUTO_CSV_BAD_MSG* (strcat "codigo invalido '" code "'")))
          )
          ((< (length row) 19)
            (setq *GM_AUTO_CSV_BAD* (1+ *GM_AUTO_CSV_BAD*))
            (if (= *GM_AUTO_CSV_BAD_MSG* "")
              (setq *GM_AUTO_CSV_BAD_MSG* (strcat code ": " (itoa (length row)) " columnas (se requieren 19)")))
          )
          ((member (strcase code) seen) nil)
          (T (setq rows (cons row rows) seen (cons (strcase code) seen)))
        )
      )
      (close file)
    )
  )
  (reverse rows)
)

(defun GM:AutoEnsureStyle (name font)
  (if (not (tblsearch "STYLE" name))
    (command "_.-STYLE" name font "0.0" "1.0" "0" "_N" "_N" "_N")
  )
)

;; E3: todo entmake del lote lleva (410 . CTAB) para forzar el Layout activo aunque el espacio cambie.
;; Negro puro (420 . 0) + peso grueso (370 . 50) para trazado fuerte del membrete/pie.
(defun GM:AutoRect (p1 p2 layer)
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer)
                 (cons 410 (getvar "CTAB")) '(420 . 0) '(370 . 50)
                 '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                 (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p1))
                 (list 10 (car p2) (cadr p2)) (list 10 (car p1) (cadr p2))))
)

;; LEGACY (conservado): texto sin color. El codigo vigente usa GM:AutoTextJ/C.
(defun GM:AutoText (point height text style layer align / data)
  (setq data (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                   (cons 8 layer) (cons 410 (getvar "CTAB")) '(420 . 0)
                   (cons 10 point) (cons 40 height) (cons 1 text) (cons 7 style)))
   (if align (setq data (append data (list '(72 . 1) (cons 11 point) '(73 . 2)))))
  (entmake data)
)

;; BS 2026-09-21: variante con color ACI (espejo del cuadro manual
;; PLANO_UBICACION.lsp:161, titulo rojo 62.1). Sin 420 cuando hay color.
(defun GM:AutoTextJ (point height text style layer color just / data)
  ;; BT 2026-09-21: just = "MC" (72.1/73.2), "R" (72.2), otro = Left (sin 72/73).
  ;; Espejo del manual (MENU_PLANOS.lsp:338-344,390-415).
  (setq data (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                   (cons 8 layer) (cons 410 (getvar "CTAB"))
                   (cons 10 point) (cons 40 height) (cons 1 text) (cons 7 style)))
  (if (numberp color)
    (setq data (append data (list (cons 62 color))))
    (setq data (append data (list '(420 . 0)))))
  (if (= just "MC") (setq data (append data (list '(72 . 1) (cons 11 point) '(73 . 2)))))
  (if (= just "R") (setq data (append data (list '(72 . 2) (cons 11 point)))))
  (entmake data)
)

(defun GM:AutoTextC (point height text style layer align color)
  (GM:AutoTextJ point height text style layer color (if align "MC" "L"))
)

;; BS 2026-09-21: marco con ancho constante (grupo 43) como el cuadro manual
;; (PLANO_UBICACION.lsp:140, 43 . 0.2): siempre visible, sin depender de LWDISPLAY.
(defun GM:AutoRectW (p1 p2 layer width)
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer)
                 (cons 410 (getvar "CTAB")) '(420 . 0)
                 '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) (cons 43 width)
                 (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p1))
                 (list 10 (car p2) (cadr p2)) (list 10 (car p1) (cadr p2))))
)