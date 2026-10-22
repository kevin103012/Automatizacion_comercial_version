;; ============================================================================
;; GM_REPORTE.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


(defun GM:AutoReportValue (value / result)
  (setq result (if (= (type value) 'STR) value ""))
  (while (vl-string-search ";" result)
    (setq result (vl-string-subst "," ";" result))
  )
  (while (vl-string-search "\n" result)
    (setq result (vl-string-subst " " "\n" result))
  )
  result
)

;; F8 2026-10-05: metrica de ingenieria por codigo para la memoria tecnica del
;; lote. Escribe una linea cruda (los parsers del lanzador solo leen RESULTADO
;; y ETAPA) con:
;; METRICA;codigo;area_ha;area_m2;perim_m;escala;centro_e;centro_n;zona;datum;u.c.
;; Nunca tumba el codigo: todo va envuelto en catch-all y sin poligono no escribe.
(defun GM:AutoMetrica (code row / poly o area perim cen eh en zona datum uc)
  (setq poly *UBI_TARGET_POLYGON*)
  (if (and poly (= (type poly) 'ENAME))
    (progn
      (setq o (vl-catch-all-apply 'vlax-ename->vla-object (list poly)))
      (if (not (vl-catch-all-error-p o))
        (progn
          (setq area (vl-catch-all-apply 'vla-get-Area (list o))
                perim (vl-catch-all-apply 'vla-get-Length (list o)))
          (if (vl-catch-all-error-p area) (setq area nil))
          (if (vl-catch-all-error-p perim) (setq perim nil))
          (GM:ReleaseObjectSafe o)
          (setq cen (vl-catch-all-apply 'UBI_GetPolygonCenter (list poly)))
          (if (vl-catch-all-error-p cen) (setq cen nil))
          (if (and cen (= (type cen) 'LIST)) (setq eh (car cen) en (cadr cen)))
          (setq zona (if (nth 17 row) (nth 17 row) "")
                datum (if (nth 18 row) (nth 18 row) "")
                uc (if (nth 13 row) (nth 13 row) ""))
          (GM:AutoReportLine
            (strcat "METRICA;" code ";"
              (if area (rtos (/ area 10000.0) 2 4) "") ";"
              (if area (rtos area 2 2) "") ";"
              (if perim (rtos perim 2 2) "") ";"
              "1/" (if (= (type *GM_AUTO_SCALE_STR*) 'STR) *GM_AUTO_SCALE_STR* "") ";"
              (if eh (rtos eh 2 2) "") ";" (if en (rtos en 2 2) "") ";"
              zona ";" datum ";" uc))
        )
      )
    )
  )
  T
)

;; Fase 1C (H-14): si hay descriptor persistente se escribe en el (sin abrir ni
;; cerrar por traza); si no, se cae al modo abrir/anexar/cerrar anterior.
(defun GM:AutoReportLine (line / file)
  (if *GM_AUTO_REPORT_PATH*
    (progn
      (if (and *GM_AUTO_REPORT_H* (= (type *GM_AUTO_REPORT_H*) 'FILE))
        (write-line line *GM_AUTO_REPORT_H*)
        (if (setq file (open *GM_AUTO_REPORT_PATH* "a"))
          (progn (write-line line file) (close file))
        )
      )
    )
  )
  (princ)
)

;; Fase 1C (H-14): abre una sola vez el reporte del lote y lo mantiene abierto.
(defun GM:AutoReportOpen (/ h)
  (if (and *GM_AUTO_REPORT_PATH*
           (not (and *GM_AUTO_REPORT_H* (= (type *GM_AUTO_REPORT_H*) 'FILE))))
    (setq *GM_AUTO_REPORT_H* (open *GM_AUTO_REPORT_PATH* "a")))
  *GM_AUTO_REPORT_H*
)

;; Fase 1C (H-14): cierra el descriptor del reporte (exito, error o cancelacion).
(defun GM:AutoReportClose ()
  (if (and *GM_AUTO_REPORT_H* (= (type *GM_AUTO_REPORT_H*) 'FILE))
    (progn (close *GM_AUTO_REPORT_H*) (setq *GM_AUTO_REPORT_H* nil)))
  (princ)
)

;; F15-R3 2026-09-29: vaciado forzado del reporte en bordes de etapa. Un AV
;; mata al interprete sin cerrar el descriptor y la cola se perdia (reporte
;; cortado a mitad del codigo 045). close+reopen en los puntos seguros.
(defun GM:AutoReportFlush ()
  (if (and *GM_AUTO_REPORT_PATH* *GM_AUTO_REPORT_H* (= (type *GM_AUTO_REPORT_H*) 'FILE))
    (progn
      (close *GM_AUTO_REPORT_H*)
      (setq *GM_AUTO_REPORT_H* (open *GM_AUTO_REPORT_PATH* "a"))))
  (princ)
)

;; F15-R3 2026-09-29: forense a prueba de crash. open/write/close POR EVENTO
;; (como el progreso): sobrevive al 0xC0000005. Solo en lote y con kill switch;
;; tope por codigo para no inflar el disco (aviso FORENSE_TOPE una vez).
(if (not (boundp '*GM_AUTO_FORENSIC*)) (setq *GM_AUTO_FORENSIC* T))
(if (not (boundp '*GM_AUTO_FORENSIC_PATH*)) (setq *GM_AUTO_FORENSIC_PATH* nil))
(if (not (boundp '*GM_AUTO_FORENSIC_N*)) (setq *GM_AUTO_FORENSIC_N* 0))
(if (not (boundp '*GM_AUTO_FORENSIC_MAX*)) (setq *GM_AUTO_FORENSIC_MAX* 500))
(defun GM:AutoForensic (code stage detail / f)
  (if (and *GM_AUTO_FORENSIC* *GM_AUTO_FORENSIC_PATH* *GM_AUTO_MODE*)
    (progn
      (setq *GM_AUTO_FORENSIC_N* (1+ *GM_AUTO_FORENSIC_N*))
      (setq f (open *GM_AUTO_FORENSIC_PATH* "a"))
      (if f
        (progn
          (if (<= *GM_AUTO_FORENSIC_N* *GM_AUTO_FORENSIC_MAX*)
            (write-line (strcat (GM:AutoReportValue code) ";"
                                (GM:AutoReportValue stage) ";"
                                "ms=" (itoa (getvar "MILLISECS")) " "
                                (GM:AutoReportValue detail)) f)
            (if (= *GM_AUTO_FORENSIC_N* (1+ *GM_AUTO_FORENSIC_MAX*))
              (write-line (strcat (GM:AutoReportValue code)
                                  ";FORENSE_TOPE;INFO;eventos>" (itoa *GM_AUTO_FORENSIC_MAX*)
                                  " en este codigo; resta del codigo sin detalle") f)))
          (close f))))
  )
  nil
)

(defun GM:AutoWriteReport (code status message output_path)
  (GM:AutoReportLine
    (strcat "RESULTADO;" (GM:AutoReportValue code) ";FIN;"
            (GM:AutoReportValue status) ";"
            (GM:AutoReportValue message) ";"
            (GM:AutoReportValue output_path)))
)

;; OPT8 2026-09-27 (checkpoint/compactacion): archivo de progreso por carpeta de
;; salida. Cada codigo OK se anota; al re-ejecutar se omiten los ya exportados
;; (reanudacion tras cerrar y reabrir el DWG para compactar la sesion).
;; SYS1 2026-10-05: la carpeta de salida queda legible (planos + REPORTE +
;; PENDIENTES). Los archivos de trabajo del sistema (progreso, sidecar,
;; parcial, forense) viven en la subcarpeta _sistema; se crea sola.
(defun GM:AutoStateFolder (folder / dir)
  (setq dir (strcat (vl-string-right-trim "\\/" folder) "\\_sistema"))
  (if (not (vl-file-directory-p dir)) (vl-mkdir dir))
  dir
)
(defun GM:AutoProgressPath (folder)
  (strcat (GM:AutoStateFolder folder) "\\GM_PLANOS_LOTE_PROGRESO.csv")
)
(defun GM:AutoProgressLoad (path / f line lst)
  (setq lst nil)
  (if (and path (findfile path))
    (progn
      (setq f (open path "r"))
      (if f
        (progn
          (while (setq line (read-line f))
            (setq line (vl-string-trim " \t\r\n" line))
            (if (/= line "") (setq lst (cons line lst))))
          (close f))
      )
    )
  )
  lst
)
(defun GM:AutoProgressAppend (path code / f)
  (if (and path (= (type code) 'STR) (/= code ""))
    (progn
      (setq f (open path "a"))
      (if f (progn (write-line code f) (close f)))
    )
  )
  nil
)

;; ============================================================================
;; PEND1 2026-09-29: CSV de pendientes al finalizar el lote.
;; GM_PLANOS_LOTE_PENDIENTES.csv (CODIGO;ETAPA;ESTADO;MOTIVO;ARCHIVO) lista los
;; codigos que no salieron OK con su motivo (y, en cierre normal/cancelacion,
;; los no intentados). Se escribe tambien en el handler *error* (crash), y en
;; continuacion los pendientes previos se fusionan: se conservan salvo que el
;; codigo ya este en el progreso (resuelto) o vuelva a fallar en esta corrida.
;; N=0 se escribe como cabecera sola (senal explicita de lote sin pendientes).
;; ============================================================================
(defun GM:AutoPendPath (folder)
  (strcat (vl-string-right-trim "\\/" folder) "\\GM_PLANOS_LOTE_PENDIENTES.csv")
)
(defun GM:AutoPendCodeOf (line / pos)
  (setq pos (vl-string-search ";" line))
  (if pos (substr line 1 pos) line)
)
(defun GM:AutoPendLine (code etapa estado motivo arch)
  (strcat (GM:AutoReportValue code) ";"
          (GM:AutoReportValue etapa) ";"
          (GM:AutoReportValue estado) ";"
          (GM:AutoReportValue motivo) ";"
          (GM:AutoReportValue arch))
)
(defun GM:AutoPendRecord (code etapa estado motivo)
  (if (and (= (type code) 'STR) (/= code ""))
    (if (not (assoc code *GM_AUTO_FAILED*))
      (setq *GM_AUTO_FAILED* (cons (list code etapa estado motivo) *GM_AUTO_FAILED*))))
  nil
)
(defun GM:AutoPendWrite (folder rows add_unattempted / path f line old done out cod row2 code_old arch seen tmp)
  (setq path (if (and folder (= (type folder) 'STR) (/= folder "")) (GM:AutoPendPath folder) nil))
  (if path
    (progn
      ;; pendientes previos (continuacion), en orden de archivo
      (setq old nil)
      (if (findfile path)
        (progn
          (setq f (open path "r"))
          (if f
            (progn
              (while (setq line (read-line f))
                (setq line (vl-string-trim " \t\r\n" line))
                (if (and (/= line "") (/= line "CODIGO;ETAPA;ESTADO;MOTIVO;ARCHIVO"))
                  (setq old (cons line old))))
              (close f)))))
      (setq old (reverse old)
            done (GM:AutoProgressLoad (GM:AutoProgressPath folder))
            out nil seen nil)
      ;; 1) fallos de esta corrida (orden cronologico)
      (foreach cod (reverse *GM_AUTO_FAILED*)
        (setq arch (GM:AutoOutputPath folder (car cod)))
        (if (not (findfile arch)) (setq arch ""))
        (setq out (append out (list (GM:AutoPendLine (car cod) (cadr cod) (caddr cod) (nth 3 cod) arch)))
              seen (cons (car cod) seen)))
      ;; 2) pendientes previos vigentes (no resueltos ni re-fallados)
      (foreach line old
        (setq code_old (GM:AutoPendCodeOf line))
        (if (and (/= code_old "")
                 (not (GM:AutoCodeDone code_old done))
                 (not (assoc code_old *GM_AUTO_FAILED*))
                 (not (member code_old seen)))
          (setq out (append out (list line))
                seen (cons code_old seen))))
      ;; 3) no intentados (solo cierre normal/cancelacion; en crash no hay lista
      ;; fiable de filas y solo se conservan los fallos ya registrados). Un
      ;; pendiente viejo que tampoco se intento ya esta en out (paso 2): no se
      ;; duplica con NO_INTENTADO.
      (if (and add_unattempted rows)
        (foreach row2 rows
          (setq code_old (nth 14 row2))
          (if (and (= (type code_old) 'STR) (/= code_old "")
                   (not (GM:AutoCodeDone code_old done))
                   (not (assoc code_old *GM_AUTO_FAILED*))
                   (not (member code_old seen)))
            (setq out (append out (list (GM:AutoPendLine code_old "NO_INTENTADO" "AVISO"
                                        "corrida interrumpida; codigo no intentado" "")))
                  seen (cons code_old seen)))))
      ;; FIX3: escritura por archivo temporal + copia. Si un AV mata a AutoCAD
      ;; a mitad del volcado, el PENDIENTES anterior queda intacto (antes el
      ;; open "w" truncaba el archivo real y un AV lo dejaba en 0 bytes:
      ;; incidente 30-09-2026). N=0 = cabecera sola (lote sin pendientes).
      (setq tmp (strcat path ".tmp"))
      (setq f (open tmp "w"))
      (if f
        (progn
          (write-line "CODIGO;ETAPA;ESTADO;MOTIVO;ARCHIVO" f)
          (foreach line out (write-line line f))
          (close f)
          (if (findfile path) (vl-file-delete path))
          (vl-file-copy tmp path)
          (vl-file-delete tmp)))
      (GM:AutoTrace "LOTE" "PENDIENTES" (if out "AVISO" "OK")
        (strcat "N=" (itoa (length out)) " archivo=" path))
    )
  )
  (length out)
)

;; DIAG2 2026-10-03: volcado PARCIAL de re-fallos en cada CHUNK_FIN. Archivo
;; aparte (GM_PLANOS_LOTE_PENDIENTES_PARCIAL.csv) para no interferir con la
;; fusion final de GM:AutoPendWrite; escritura por temporal + copia, sobrevive
;; al 0xC0000005 (el PENDIENTES final solo se escribe al cierre y un AV lo pierde).
(defun GM:AutoPendPartialWrite (folder / path tmp f it)
  (if (and folder (= (type folder) 'STR) (/= folder ""))
    (progn
      (setq path (strcat (GM:AutoStateFolder folder) "\\GM_PLANOS_LOTE_PENDIENTES_PARCIAL.csv")
            tmp (strcat path ".tmp")
            f (open tmp "w"))
      (if f
        (progn
          (write-line "CODIGO;ETAPA;ESTADO;MOTIVO" f)
          (foreach it (reverse *GM_AUTO_FAILED*)
            (write-line (strcat (GM:AutoReportValue (car it)) ";"
                                (GM:AutoReportValue (cadr it)) ";"
                                (GM:AutoReportValue (caddr it)) ";"
                                (GM:AutoReportValue (nth 3 it))) f))
          (close f)
          (if (findfile path) (vl-file-delete path))
          (vl-file-copy tmp path)
          (vl-file-delete tmp)))
      (GM:AutoTrace "LOTE" "PENDIENTES_PARCIAL" "INFO"
        (strcat "N=" (itoa (if *GM_AUTO_FAILED* (length *GM_AUTO_FAILED*) 0))
                " archivo=" path)))
  )
  nil
)

;; ============================================================================
;; FIX3 2026-09-30: sidecar de configuracion del lote.
;; La config efectiva (CSV, carpeta, modo, circulos, sello) se persiste en
;; GM_PLANOS_LOTE_CONFIG.csv dentro de la carpeta de salida, no solo en
;; ML_SUITE (LData del DWG). Causa raiz del salto a 167: al reabrir el DWG,
;; GM_CONTINUAR_LOTE leyo la huella vieja del DWG (CSV de Fase A y
;; SHOW_PREDIO_LOC "0") y la seleccion correcta en memoria se perdio.
;; GM_CONTINUAR_LOTE lee el sidecar primero (fail-closed: si el CSV no
;; contiene los codigos del progreso, no inicia).
;; ============================================================================
(defun GM:SplitSemi (line / lst idx)
  (setq lst nil)
  (while (and (= (type line) 'STR) (setq idx (vl-string-search ";" line)))
    (setq lst (cons (substr line 1 idx) lst)
          line (substr line (+ idx 2))))
  (reverse (cons (if (= (type line) 'STR) line "") lst))
)
(defun GM:AutoConfigPath (folder)
  (strcat (GM:AutoStateFolder folder) "\\GM_PLANOS_LOTE_CONFIG.csv")
)
(defun GM:AutoConfigWrite (folder csv mk_pdf show_loc show_geo tipo vp / path f)
  ;; CHG2 2026-10-07: se agrega SHOW_GEO_TXT (rotulo DEP/PROV/DIST) al sidecar.
  ;; PRY1 2026-10-12: se agregan TIPO_PLANO (estilo) y VP_TYPE (datos de
  ;; grilla); un sidecar viejo sin los campos se resuelve a UBICACION/"1" en
  ;; GM_CONTINUAR_LOTE. Lectores viejos ignoran los campos nuevos.
  (setq path (if (and folder (= (type folder) 'STR) (/= folder "")) (GM:AutoConfigPath folder) nil))
  (if (and path csv (= (type csv) 'STR) (/= csv ""))
    (progn
      (setq f (open path "w"))
      (if f
        (progn
          (write-line "CSV_PATH;CSV_FOLDER;MAKE_PDF;SHOW_PREDIO_LOC;SELLO;SHOW_GEO_TXT;TIPO_PLANO;VP_TYPE" f)
          (write-line (strcat (GM:AutoReportValue csv) ";"
                              (GM:AutoReportValue folder) ";"
                              (if mk_pdf "2" "1") ";"
                              (if (equal show_loc "1") "1" "0") ";"
                              *GM_VER_AUTO* ";"
                              (if (equal (GM:ResolveShowGeoTxt show_geo) "1") "1" "0") ";"
                              (GM:ResolvePlanTipo tipo) ";"
                              (GM:ResolveVpType vp)) f)
          (close f)))))
  nil
)
(defun GM:AutoConfigRead (path / f line parts)
  (setq parts nil)
  (if (and path (= (type path) 'STR) (findfile path))
    (progn
      (setq f (open path "r"))
      (if f
        (progn
          (read-line f) ;; cabecera
          (setq line (read-line f))
          (close f)
          (if (and line (/= (vl-string-trim " \t\r\n" line) ""))
            (setq parts (GM:SplitSemi line)))))))
  (if (and parts (>= (length parts) 4)) parts nil)
)
;; FIX3: todo codigo ya exportado (PROGRESO) debe existir en el CSV que se
;; va a continuar. Devuelve la lista de codigos ofensivos (nil = compatible);
;; evita saltos silenciosos a otro CSV (incidente HS-CP-022 -> HS-HU-167 con
;; CAJETIN_FASE_A_167-234.csv).
(defun GM:AutoCodeInRows (code rows / c hit)
  (setq hit nil c (if (= (type code) 'STR) (strcase (vl-string-trim " \t\r\n" code)) ""))
  (if (/= c "")
    (foreach row rows
      (if (not hit)
        (if (= c (strcase (vl-string-trim " \t\r\n" (nth 14 row)))) (setq hit T)))))
  hit
)
(defun GM:AutoProgressCompat (done rows / bad)
  (setq bad nil)
  (if (and done rows)
    (foreach c done
      (if (not (GM:AutoCodeInRows c rows)) (setq bad (cons c bad)))))
  (reverse bad)
)
;; FIX3: une una lista en texto para las alertas (primeros N + "... si hay mas").
(defun GM:AutoListJoin (lst n / out i)
  (setq out "" i 0)
  (foreach x lst
    (if (< i n)
      (setq out (strcat out (if (= out "") "" ", ") (if (= (type x) 'STR) x (vl-princ-to-string x)))
            i (1+ i))))
  (if (and (> n 0) (> (length lst) n)) (setq out (strcat out ", ...")))
  out
)

;; ============================================================================
;; AUD1 2026-09-29: GM:AutoFreshCheck - aviso de copia no fresca.
;; Se llama ANTES de escribir el CSV_PATH de esta corrida: si el DWG ya tiene
;; ML_SUITE.CSV_PATH, viene de una corrida anterior. Solo avisa (alert una vez)
;; y deja nota para la traza FRESH_CHECK del reporte; el lote continua.
;; Kill switch *GM_AUTO_FRESH_CHECK* nil = sin alerta (la traza queda).
;; ============================================================================
(defun GM:AutoFreshCheck (csv / prev)
  (setq prev (vl-catch-all-apply 'vlax-ldata-get (list "ML_SUITE" "CSV_PATH")))
  (if (vl-catch-all-error-p prev) (setq prev nil))
  (if (and prev (= (type prev) 'STR) (/= prev ""))
    (progn
      (setq *GM_AUTO_FRESH_DIRTY* T
            *GM_AUTO_FRESH_NOTE* (strcat "huellas de corrida previa: ML_SUITE.CSV_PATH=" prev "; se recomienda copia fresca del modelo"))
      (if *GM_AUTO_FRESH_CHECK*
        (alert (strcat "Aviso: este DWG ya corrio un lote antes (huella ML_SUITE).\n"
                       "Para maxima estabilidad use una COPIA FRESCA del modelo.\n"
                       "El lote continuara en este archivo."))))
    (setq *GM_AUTO_FRESH_DIRTY* nil
          *GM_AUTO_FRESH_NOTE* "sin huellas de corrida previa"))
  nil
)

(defun GM:AutoCodeDone (code done / c hit)
  (setq hit nil c (if (= (type code) 'STR) (strcase (vl-string-trim " \t\r\n" code)) ""))
  (if (/= c "")
    (foreach item done
      (if (not hit)
        (if (= c (strcase (vl-string-trim " \t\r\n" item))) (setq hit T)))))
  ;; F5 2026-10-05: PROGRESO solo cuenta como hecho si el DWG existe y es un
  ;; DWG real (cabecera/tamano). Un archivo borrado o truncado por un AV
  ;; obliga a regenerar el codigo en la reanudacion.
  (if (and hit (boundp '*GM_AUTO_OUTPUT_FOLDER*) *GM_AUTO_OUTPUT_FOLDER*
           (= (type *GM_AUTO_OUTPUT_FOLDER*) 'STR) (/= *GM_AUTO_OUTPUT_FOLDER* ""))
    (if (not (GM:AutoDwgValid (GM:AutoOutputPath *GM_AUTO_OUTPUT_FOLDER* code)))
      (setq hit nil)))
  hit
)

;; RECYCLE1: diferencia entre progreso real (OK) y filas que el supervisor
;; reservo para otro proceso. Las filas diferidas no cuentan como intentadas.
(defun GM:AutoLauncherSkipCodeP (code / c hit item)
  (setq hit nil c (if (= (type code) 'STR) (strcase (vl-string-trim " \t\r\n" code)) ""))
  (if (/= c "")
    (foreach item *GM_AUTO_LAUNCHER_SKIP*
      (if (and (not hit) (= c (strcase (vl-string-trim " \t\r\n" item))))
        (setq hit T))))
  hit
)

(defun GM:AutoTrace (code stage status message)
  ;; PEND1 2026-09-29: la primera etapa en ERROR del codigo en curso alimenta
  ;; el CSV de pendientes (el bucle la reinicia antes de cada codigo; ResetPerCodeState
  ;; no la limpia porque el fallo suave se lee despues del retorno).
  (if (and (= (type status) 'STR) (= status "ERROR") (not *GM_AUTO_LAST_ERR_STAGE*))
    (setq *GM_AUTO_LAST_ERR_STAGE* (if (= (type stage) 'STR) stage "")))
  (GM:AutoReportLine
    (strcat "ETAPA;" (GM:AutoReportValue code) ";"
            (GM:AutoReportValue stage) ";"
            (GM:AutoReportValue status) ";"
            (GM:AutoReportValue message) ";"))
)

;; FASE 0: marca de tiempo y registro por etapa. Devuelve los ms transcurridos.
;; No escribe nada si no hay marca inicial valida.
(defun GM:PerfMs () (getvar "MILLISECS"))
(defun GM:PerfElapsed (t0 / now el)
  (if (and t0 (numberp t0))
    (progn (setq now (getvar "MILLISECS") el (- now t0)) (if (< el 0) (setq el 0)) el)
    0
  )
)
;; OPT5 2026-09-27 (F0): registra una etapa con un tiempo ya calculado (lo usan
;; tanto las marcas por t0 como los sub-tiempos del formato venidos de PLANO).
(defun GM:PerfAdd (stage el)
  (if (and stage (numberp el)) (setq *GM_PERF_CUR* (cons (cons stage el) *GM_PERF_CUR*)))
  nil
)
(defun GM:PerfMark (code stage t0 / el)
  (if (and t0 (numberp t0))
    (progn
      (setq el (GM:PerfElapsed t0))
      (GM:PerfAdd stage el)
      (GM:AutoTrace code (strcat "TIEMPO_" stage) "INFO" (strcat "ms=" (itoa el)))
    )
  )
  nil
)
;; OPT5 (F0): lee un numero de una lista de estadisticas (0 si ausente).
(defun GM:StatNum (al key / v)
  (setq v (cdr (assoc key al)))
  (if (numberp v) v 0)
)
;; OPT5 (F0): publica los sub-tiempos del formato como etapas del resumen, los
;; conteos en una traza unica y acumula los totales del lote. Solo lee
;; *GM_FMT_STATS* (no altera el formato ni la salida).
(defun GM:FmtStatsReport (code / st k v)
  (setq st (if (boundp '*GM_FMT_STATS*) *GM_FMT_STATS* nil))
  (if st
    (progn
      (foreach k '("MAPA" "TEXTOS" "ORDEN" "CIERRE" "HL_MS")
        (setq v (cdr (assoc k st)))
        (if (numberp v) (GM:PerfAdd (strcat "FMT_" k) v)))
      (GM:AutoTrace code "FORMATO_STATS" "INFO"
        (strcat "escaneadas=" (itoa (GM:StatNum st "N_SCAN"))
                " conHL=" (itoa (GM:StatNum st "N_HL"))
                " aplicadas=" (itoa (GM:StatNum st "N_APPLY"))
                " yaColor=" (itoa (GM:StatNum st "N_SKIPCOL"))
                " escritas=" (itoa (GM:StatNum st "N_WRITES"))
                " sealSkip=" (itoa (GM:StatNum st "N_SEALSKIP"))
                " skipLtype=" (itoa (GM:StatNum st "N_SKIPLT"))
                " skipLts=" (itoa (GM:StatNum st "N_SKIPLTS"))
                " skipLw=" (itoa (GM:StatNum st "N_SKIPLW"))
                " skipWidth=" (itoa (GM:StatNum st "N_SKIPW"))
                " skipHatch=" (itoa (GM:StatNum st "N_SKIPH"))
                " skipTC=" (itoa (GM:StatNum st "N_SKIPTC"))
                " skipLTGen=" (itoa (GM:StatNum st "N_SKIPLTGEN"))
                " writeLTGen=" (itoa (GM:StatNum st "N_WRITELTGEN"))
                " skipUpdate=" (itoa (GM:StatNum st "N_SKIPU"))
                " textSkip=" (itoa (GM:StatNum st "N_TXSKIP"))
                " textWr=" (itoa (GM:StatNum st "N_TXWR"))
                " stkSkip=" (itoa (GM:StatNum st "N_STKSKIP"))
                " hlRead=" (itoa (GM:StatNum st "HL_N"))
                " msMapa=" (itoa (GM:StatNum st "MAPA"))
                " msTextos=" (itoa (GM:StatNum st "TEXTOS"))
                " msOrden=" (itoa (GM:StatNum st "ORDEN"))
                " msCierre=" (itoa (GM:StatNum st "CIERRE"))
                " msHL=" (itoa (GM:StatNum st "HL_MS"))))
      (foreach k '("N_SCAN" "N_HL" "N_APPLY" "N_SKIPCOL" "N_WRITES" "N_SEALSKIP" "N_TXSKIP" "N_STKSKIP" "HL_N" "N_SKIPLTGEN" "N_WRITELTGEN")
        (setq v (cdr (assoc k st)))
        (if (not (numberp v)) (setq v 0))
        (setq *GM_FMT_TOTALS*
          (cons (cons k (+ v (GM:StatNum *GM_FMT_TOTALS* k)))
                (vl-remove (assoc k *GM_FMT_TOTALS*) *GM_FMT_TOTALS*))))
    )
  )
  T
)
(defun GM:PerfRecordCode ()
  (if (and *GM_PERF_CUR* (mapcar 'car *GM_PERF_CUR*))
    (setq *GM_PERF_ROWS*
      (cons (cons (if *GM_AUTO_PERF_CODE* *GM_AUTO_PERF_CODE* "?") (reverse *GM_PERF_CUR*))
            *GM_PERF_ROWS*))
  )
  (setq *GM_PERF_CUR* nil *GM_PERF_CODE_START* nil)
  nil
)
;; FASE 0: resumen de lote: por etapa, primer/ultimo tiempo entre codigos validos
;; y factor de degradacion (ultimo/primero). Traza RESUMEN;LOTE para cada etapa.
(defun GM:PerfSummarizeBatch (/ rows stages stage vals v_first v_last factor n)
  (setq rows (reverse *GM_PERF_ROWS*))
  (setq stages '("MODELO" "GRILLA" "VISIBILIDAD" "FORMATO"
                 "FMT_MAPA" "FMT_TEXTOS" "FMT_ORDEN" "FMT_CIERRE" "FMT_HL_MS"
                 "LAYOUT"
                 "EXPORT_DWG" "EXPORT_PDF" "SNAPSHOT_CAPTURA" "LIMPIEZA"
                 "SNAPSHOT_VERIFY" "RESTAURACION" "TOTAL"))
  (if (= (length rows) 0)
    (GM:AutoTrace "LOTE" "RESUMEN_LOTE" "INFO" "sin filas instrumentadas")
    (progn
      (GM:AutoTrace "LOTE" "RESUMEN_LOTE" "INFO"
        (strcat "codigos=" (itoa (length rows))
                " primero=" (GM:AutoReportValue (car (car rows)))
                " ultimo=" (GM:AutoReportValue (car (car (reverse rows))))))
      (foreach stage stages
        (setq vals (vl-remove nil (mapcar '(lambda (r) (cdr (assoc stage (cdr r)))) rows)))
        (setq n (length vals))
        (if (> n 0)
          (progn
            (setq v_first (car vals) v_last (car (reverse vals)))
            (setq factor (if (and v_first (numberp v_first) (> v_first 0)) (/ (float v_last) v_first) 0.0))
            (GM:AutoTrace "LOTE" "RESUMEN" "INFO"
              (strcat "etapa=" stage " primero_ms=" (itoa v_first) " ultimo_ms=" (itoa v_last)
                      " factor=" (rtos factor 2 2) " n=" (itoa n))))))
      ;; OPT5 (F0): acumulado de conteos del formato de todo el lote.
      (GM:AutoTrace "LOTE" "FORMATO_TOTAL" "INFO"
        (strcat "codigos=" (itoa (length rows))
                " escaneadas=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_SCAN"))
                " conHL=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_HL"))
                " aplicadas=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_APPLY"))
                " yaColor=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_SKIPCOL"))
                " escritas=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_WRITES"))
                " sealSkip=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_SEALSKIP"))
                " skipLTGen=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_SKIPLTGEN"))
                " writeLTGen=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_WRITELTGEN"))
                " textSkip=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_TXSKIP"))
                " stkSkip=" (itoa (GM:StatNum *GM_FMT_TOTALS* "N_STKSKIP"))
                " hlRead=" (itoa (GM:StatNum *GM_FMT_TOTALS* "HL_N"))))
      (princ "\n[PERF] Resumen de rendimiento escrito en el reporte del lote.")
    )
  )
  (setq *GM_PERF_ROWS* nil *GM_PERF_CUR* nil *GM_FMT_TOTALS* nil)
  (princ)
)

(defun GM:AutoSetPreviewList (/ row code)
  (if (and (get_tile "lst_codes") (get_tile "txt_count"))
    (progn
      (start_list "lst_codes")
      (foreach row *GM_AUTO_PREVIEW_ROWS*
        (setq code (nth 14 row))
        (if code (add_list code))
      )
      (end_list)
      (set_tile "txt_count" (strcat "Codigos validos: " (itoa (length *GM_AUTO_PREVIEW_ROWS*))))
    )
  )
  (princ)
)

(defun GM:AutoSelectCsvFromPanel (/ selected)
  (setq selected (getfiled "Seleccionar CSV" (if *GM_AUTO_CSV_PATH* *GM_AUTO_CSV_PATH* "") "csv;txt" 8))
  (if selected
    (progn
      (setq *GM_AUTO_CSV_PATH* selected *GM_AUTO_PREVIEW_ROWS* (GM:AutoReadCsv selected))
      (set_tile "txt_csv" (strcat "CSV: " (vl-filename-base selected)))
      (GM:AutoSetPreviewList)
    )
    (progn
      (setq *GM_AUTO_CSV_PATH* nil *GM_AUTO_PREVIEW_ROWS* nil)
      (set_tile "txt_csv" "No seleccionado")
      (GM:AutoSetPreviewList)
    )
  )
  (princ)
)

(defun GM:AutoSelectOutputFolderFromPanel (/ shell picked self result)
  ;; BrowseForFolder evita que el usuario tenga que escribir una ruta.
  (setq result nil)
  (setq shell (vl-catch-all-apply 'vlax-create-object (list "Shell.Application")))
  (if (not (vl-catch-all-error-p shell))
    (progn
      (setq picked (vl-catch-all-apply 'vlax-invoke-method (list shell 'BrowseForFolder 0 "Seleccionar carpeta de salida DWG" 0)))
      (if (and (not (vl-catch-all-error-p picked)) picked)
        (progn
          (setq self (vl-catch-all-apply 'vlax-get-property (list picked 'Self)))
          (if (and (not (vl-catch-all-error-p self)) self)
            (setq result (vl-catch-all-apply 'vlax-get-property (list self 'Path)))
          )
          (if (vl-catch-all-error-p result) (setq result nil))
          (if self (vl-catch-all-apply 'vlax-release-object (list self)))
          (vl-catch-all-apply 'vlax-release-object (list picked))
        )
      )
      (vl-catch-all-apply 'vlax-release-object (list shell))
    )
  )
  (if (and result (/= result ""))
    (progn
      (setq *GM_AUTO_OUTPUT_FOLDER* result)
      (set_tile "txt_folder" result)
    )
    (progn
      (setq *GM_AUTO_OUTPUT_FOLDER* nil)
      (set_tile "txt_folder" "No seleccionada")
    )
  )
  (princ)
)

(defun GM:AutoProgressValue (value / pos km mt)
  (setq value (if (= (type value) 'STR) (vl-string-trim " \t\r\n" value) "")
        pos (vl-string-search "+" value))
  (if pos
    (progn
      (setq km (atof (substr value 1 pos)) mt (atof (substr value (+ pos 2))))
      (+ (* km 1000.0) mt))
    (atof value))
)