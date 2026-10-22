;; ============================================================================
;; GM_COMANDOS.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


(defun c:GM_PLANOS_LOTE (/ csv folder rows row result total success old_ctab olderr report_path report_file panel_status model_before layout_before batcherr ver_msgs _clean _verify _rm _rl t_idx _nents _cb prog_path done pausa_lim procesados skipped launcher_skipped fallos vis_rest pausa fx ln found_res corte_prev rep_append rep_mode was_skipped chunk_lim desde_chunk _pt_stage _pt_msg)
  (setq old_ctab (getvar "CTAB") olderr *error*
        rep_append (and *GM_AUTO_REPORT_APPEND* T))
  ;; CHUNK: la bandera de anexado es de un solo uso (continuacion).
  (setq *GM_AUTO_REPORT_APPEND* nil)
  ;; FIX2 30-09-2026: el toggle de la senalizacion del predio nace en "1"
  ;; (un "0" explicito se conserva); asi el panel y el lote nunca arrancan
  ;; apagados por un valor nil/vacio heredado.
  (setq *GM_AUTO_SHOW_PREDIO_LOC* (GM:ResolveShowPredioLoc *GM_AUTO_SHOW_PREDIO_LOC*))
  ;; CHG2 2026-10-07: misma normalizacion para el rotulo DEP/PROV/DIST.
  (setq *GM_AUTO_SHOW_GEO_TXT* (GM:ResolveShowGeoTxt *GM_AUTO_SHOW_GEO_TXT*))
  ;; QUIET1 2026-10-05: recordar si el panel fue saltado (lanzador externo o
  ;; GM_CONTINUAR_LOTE) ANTES de que GM:AutoBatchPanel consuma la bandera de un
  ;; solo uso; con was_skipped o *GM_AUTO_QUIET* el modo de salida se informa
  ;; por consola/traza y no con un alert modal que congelaria la corrida /b.
  (setq was_skipped (and *GM_AUTO_SKIP_PANEL* T))
(defun *error* (msg)
     (setq *GM_AUTO_MODE* nil *GM_AUTO_CANCELLED* T *GM_AUTO_LAUNCHER_SKIP* nil)
    ;; OPT2 2026-09-26 (Fase 5): restaurar UNDO tambien al cancelar/fallar.
    (GM:AutoUndoControlEnd)
    (if (and (boundp 'model_before) model_before (boundp 'layout_before) layout_before)
      (vl-catch-all-apply 'GM:AutoCaptureRun (list model_before layout_before))
      (if (and (boundp '*GM_AUTO_BATCH_MODEL_BEFORE*) *GM_AUTO_BATCH_MODEL_BEFORE*
               (boundp '*GM_AUTO_BATCH_LAYOUT_BEFORE*) *GM_AUTO_BATCH_LAYOUT_BEFORE*)
        (vl-catch-all-apply 'GM:AutoCaptureRun (list *GM_AUTO_BATCH_MODEL_BEFORE* *GM_AUTO_BATCH_LAYOUT_BEFORE*))))
    (vl-catch-all-apply 'GM:AutoCleanRun '())
    (vl-catch-all-apply 'GM:PurgeTempBlocks '())
    ;; T2.2-FIX1 (28-09-2026): la visibilidad vuelve a su estado original
    ;; (mascara base) ANTES de liberar el indice; el legado es fallback dentro
    ;; de UBI_RestoreVisibilityBase. UBI_RestoreHiddenByGM conserva la higiene
    ;; de capas OBRAS COMPLEMENTARIAS y la invalidacion de la zona previa.
    (setq vis_rest (vl-catch-all-apply 'UBI_RestoreVisibilityBase '()))
    (vl-catch-all-apply 'GM:AutoTrace
      (list "LOTE" "VISIBILIDAD_FINAL"
        (if (vl-catch-all-error-p vis_rest) "ERROR" "OK")
        (if (vl-catch-all-error-p vis_rest)
          (vl-catch-all-error-message vis_rest)
          (strcat "restauradas=" (if (numberp vis_rest) (itoa vis_rest) "0") " (cancelacion)"))))
    (vl-catch-all-apply 'UBI_RestoreHiddenByGM '())
    (vl-catch-all-apply 'GM:RestoreLayerState '())
    (vl-catch-all-apply 'GM:RestoreViewportState '())
    ;; OPT6 (F2): soltar el indice de entidades del lote tambien al cancelar.
    (vl-catch-all-apply 'UBI_ResetEntIndex '())
    (setq *UBI_VP_DATA_MEM* nil *GL_DATA_AFECTACIONES* nil *UBI_TARGET_REFERENCE* "" *UBI_TARGET_POLYGON* nil *GM_PLACED_BOXES* nil *GM_LABEL_EXCLUDE* nil *GM_LANDING_GAP* nil *GM_LABEL_SAMPLES* nil *GM_LABEL_DIST2* nil *GM_ROAD_BOXES_CACHE* nil *GM_ROAD_BOXES_CACHE_READY* nil *GM_TEMP_BLOCKS* nil *GM_AUTO_CLEAN_FAILED* nil *GM_AUTO_SCALE_RESTORE_OK* T *GM_CLEANUP_IN_PROGRESS* nil *GM_AUTO_MARK_LOTE* nil *GM_AUTO_ROADS_INDEX_READY* nil *GM_AUTO_ROAD_WCS* nil *GM_AUTO_CALIB_CACHE* nil *GM_RED_BOXES_CACHE* nil *GM_RED_BOXES_CACHE_READY* nil *GM_MODEL_BBOX_CACHE* nil *GM_TEXT_MEASURE_CACHE* nil)
    ;; Fase 1C (H-14): cerrar el descriptor del reporte tambien al cancelar.
    ;; PEND1: antes de cerrar, dejar el CSV de pendientes con los fallos ya
    ;; registrados (sin no-intentados: la lista de filas puede estar incompleta
    ;; en un crash; con folder nil la funcion no hace nada).
    ;; FIX3: si el error ocurre antes del panel, cae a la carpeta en memoria.
    (vl-catch-all-apply 'GM:AutoPendWrite
      (list (if (and folder (= (type folder) 'STR) (/= folder "")) folder *GM_AUTO_OUTPUT_FOLDER*) nil nil))
    (vl-catch-all-apply 'GM:AutoReportClose '())
    (if old_ctab (setvar "CTAB" old_ctab))
    (setq *error* olderr)
    (princ (strcat "\nLote cancelado: " msg))
    (princ)
  )
  ;; Puerta de versiones: impide mezclar modulos viejos con nuevos (causa de errores fantasma).
  (setq ver_msgs (GM:CheckVersions))
  (if ver_msgs
    (progn
      (alert (strcat "Modulos desactualizados o sin cargar. Recargue con APPLOAD en este orden: MENU_PLANOS, PLANO_UBICACION, LAYOUT_UBICACION, AUTOMATIZACION."
                     (apply 'strcat (mapcar '(lambda (m) (strcat "\n- " m)) ver_msgs))))
      (princ "\n[GM_PLANOS_LOTE] Modulos desactualizados; lote no iniciado.")
      (setq panel_status 0 csv nil folder nil)
    )
  (if (= old_ctab "Model")
    (progn
      (alert "GM_PLANOS_LOTE debe iniciarse desde el Layout plantilla activo.")
      (setq panel_status 0)
    )
    (if ver_msgs
      (setq panel_status 0)
      (setq panel_status (GM:AutoBatchPanel))
    )
  )
  )
  ;; BL: decision por botones del panel (el toggle no registraba el clic y la
  ;; pregunta de consola se perdia en este entorno: el NO nunca llegaba).
  ;; 1 = SOLO DWG, 2 = DWG+PDF; otro codigo = lote no iniciado.
  ;; BN: alerta imposible de pasar por alto con el modo elegido (el eco de
  ;; consola no aparece en este entorno); el reporte tambien lo registra.
  ;; QUIET1: si el panel fue saltado o *GM_AUTO_QUIET* esta activo, el aviso va
  ;; a consola (princ) para no bloquear un arranque desatendido; el flujo manual
  ;; interactivo conserva el alert.
  (cond
    ((= panel_status 1)
      (setq csv *GM_AUTO_CSV_PATH* folder *GM_AUTO_OUTPUT_FOLDER*
            *GM_AUTO_MAKE_PDF* nil)
      (princ "\n[PANEL] make_pdf=0 (SOLO DWG)")
      (if (or was_skipped *GM_AUTO_QUIET*)
        (princ "\n[GM_PLANOS_LOTE] Modo de salida: SOLO DWG (sin PDF)")
        (alert "Modo de salida: SOLO DWG (sin PDF)")))
    ((= panel_status 2)
      (setq csv *GM_AUTO_CSV_PATH* folder *GM_AUTO_OUTPUT_FOLDER*
            *GM_AUTO_MAKE_PDF* T)
      (princ "\n[PANEL] make_pdf=1 (DWG+PDF)")
      (if (or was_skipped *GM_AUTO_QUIET*)
        (princ "\n[GM_PLANOS_LOTE] Modo de salida: DWG + PDF")
        (alert "Modo de salida: DWG + PDF")))
    (T (setq csv nil folder nil))
  )
  (if (and csv folder (/= folder "") (vl-file-directory-p folder))
    (progn
      ;; AUD1: aviso de copia no fresca ANTES de escribir ML_SUITE de esta corrida.
      (GM:AutoFreshCheck csv)
      (setq rows (GM:AutoReadCsv csv) total (length rows) success 0)
      (if (= total 0)
        (alert "No se encontraron codigos validos en la columna 15 del CSV.")
        (progn
           (vlax-ldata-put "ML_SUITE" "CSV_PATH" csv)
            ;; CHUNK 2026-09-29: persistir tambien carpeta y modo de salida para
            ;; que GM_CONTINUAR_LOTE retome la corrida sin volver a pedir datos.
            (vl-catch-all-apply 'vlax-ldata-put (list "ML_SUITE" "CSV_FOLDER" folder))
            ;; FIX 30-09: el contrato persistido es 1=SOLO DWG, 2=DWG+PDF (el
            ;; lector de GM_CONTINUAR_LOTE ya lo esperaba asi; el escritor
            ;; guardaba 1 para DWG+PDF y cada continuacion invertia el modo).
            (vl-catch-all-apply 'vlax-ldata-put (list "ML_SUITE" "MAKE_PDF" (if *GM_AUTO_MAKE_PDF* 2 1)))
            (vl-catch-all-apply 'vlax-ldata-put (list "ML_SUITE" "SHOW_PREDIO_LOC" *GM_AUTO_SHOW_PREDIO_LOC*))
            (vl-catch-all-apply 'vlax-ldata-put (list "ML_SUITE" "SHOW_GEO_TXT" (GM:ResolveShowGeoTxt *GM_AUTO_SHOW_GEO_TXT*)))
            ;; FIX3 2026-09-30: sidecar en disco con la config efectiva de la
            ;; corrida (fuente de verdad para GM_CONTINUAR_LOTE; el DWG puede
            ;; traer huellas viejas tras un crash o una reapertura).
            ;; PRY1: el tipo de plano y los datos de VP viajan al sidecar para
            ;; que la continuacion manual conserve el estilo del lote.
            (vl-catch-all-apply 'GM:AutoConfigWrite (list folder csv *GM_AUTO_MAKE_PDF* *GM_AUTO_SHOW_PREDIO_LOC* *GM_AUTO_SHOW_GEO_TXT* (if (boundp '*GM_PLANO_TIPO*) *GM_PLANO_TIPO* "UBICACION") (if (boundp '*UBI_VP_TYPE*) *UBI_VP_TYPE* "1")))
            (setq *Membrete_CSV_Path* csv
                  *GM_AUTO_LAYOUT_TAB* old_ctab
                  report_path (strcat (vl-string-right-trim "\\/" folder) "\\GM_PLANOS_LOTE_REPORTE.csv")
                  *GM_AUTO_REPORT_PATH* report_path)
             (princ (strcat "\nReporte del lote: " *GM_AUTO_REPORT_PATH*))
             ;; F15-R3: forense por evento + deteccion de corte previo (el
             ;; reporte se trunca al iniciar; si el anterior no tiene
             ;; RESUMEN_LOTE, la corrida pasada murio sin cierre).
             ;; SYS1: el forense vive en _sistema y solo se crea en modo
             ;; diagnostico (*GM_AUTO_FORENSIC*; el lanzador lo apaga por
             ;; defecto). El gate de escritura ya esta en GM:AutoForensic.
             (setq *GM_AUTO_FORENSIC_PATH* (strcat (GM:AutoStateFolder folder) "\\GM_PLANOS_LOTE_FORENSE.csv")
                   *GM_AUTO_FORENSIC_N* 0
                   corte_prev nil)
             (if *GM_AUTO_FORENSIC*
               (progn
                 (if (and rep_append (findfile *GM_AUTO_FORENSIC_PATH*))
                   (princ) ;; CHUNK: continuacion; se conserva la cola forense previa
                   (progn
                     (if (findfile *GM_AUTO_FORENSIC_PATH*) (vl-file-delete *GM_AUTO_FORENSIC_PATH*))
                     (if (setq fx (open *GM_AUTO_FORENSIC_PATH* "w"))
                       (progn (write-line "CODIGO;FASE;DETALLE" fx) (close fx)))))))
             (if (findfile report_path)
               (progn
                 (setq fx (open report_path "r") found_res nil)
                 (if fx
                   (progn
                     (while (and (not found_res) (setq ln (read-line fx)))
                       (if (vl-string-search "RESUMEN_LOTE" ln) (setq found_res T)))
                     (close fx)))
                 (setq corte_prev (not found_res))))
             (setq *GM_AUTO_BATCH_MODEL_BEFORE* (GM:AutoEntitySnapshot "Model")
                   *GM_AUTO_BATCH_LAYOUT_BEFORE* (GM:AutoEntitySnapshot "Layout"))
             ;; CHUNK 2026-09-29: en continuacion el reporte se abre en modo
             ;; anexar ("a") para no perder la cola de la corrida anterior; en
             ;; corrida normal se trunca ("w") como siempre.
             (setq rep_mode (if (and rep_append (findfile *GM_AUTO_REPORT_PATH*)) "a" "w"))
             (if (setq report_file (open *GM_AUTO_REPORT_PATH* rep_mode))
               (progn
                 (if (= rep_mode "w")
                   (write-line "TIPO;CODIGO;ETAPA;ESTADO;MENSAJE;ARCHIVO" report_file))
                 (if (and corte_prev (= rep_mode "w"))
                   (write-line "ETAPA;LOTE;REANUDACION;CORTE;reporte previo sin RESUMEN_LOTE (corte detectado en la corrida anterior);" report_file))
                 (close report_file)
                 ;; Fase 1C (H-14): un solo descriptor para todo el lote.
                 (GM:AutoReportOpen)
                 (if rep_append
                   (GM:AutoTrace "LOTE" "CONTINUAR" "INFO" "reporte anexado; corrida de continuacion en 1 clic"))
                  ;; BN: decision del panel como primera linea del reporte.
                  (GM:AutoTrace "LOTE" "MODO_SALIDA" "INFO"
                    (if *GM_AUTO_MAKE_PDF* "DWG+PDF" "SOLO_DWG"))
                  ;; FIX2: senalizacion del predio efectiva de esta corrida.
                  (GM:AutoTrace "LOTE" "PREDIO_LOC_CONFIG" "INFO"
                    (if (= *GM_AUTO_SHOW_PREDIO_LOC* "1")
                      "circulos verdes del predio ACTIVADOS"
                      "circulos verdes del predio DESACTIVADOS (eleccion explicita)"))
                  ;; CHG2: rotulo DEP/PROV/DIST efectivo de esta corrida.
                  (GM:AutoTrace "LOTE" "GEO_TXT_CONFIG" "INFO"
                    (if (= (GM:ResolveShowGeoTxt *GM_AUTO_SHOW_GEO_TXT*) "1")
                      "rotulo DEP/PROV/DIST ACTIVADO"
                      "rotulo DEP/PROV/DIST DESACTIVADO (eleccion explicita)"))
                  ;; PRY1 2026-10-12: tipo de plano efectivo de la corrida; la
                  ;; biblioteca del lanzador lee esta traza para etiquetar cada
                  ;; plano generado (los RESULTADO siguientes heredan el tipo).
                  (GM:AutoTrace "LOTE" "TIPO_PLANO" "INFO"
                    (GM:ResolvePlanTipo (if (boundp '*GM_PLANO_TIPO*) *GM_PLANO_TIPO* nil)))
                 ;; AUD1: nota del chequeo de copia fresca (AVISO si hay huellas).
                 (if *GM_AUTO_FRESH_NOTE*
                   (GM:AutoTrace "LOTE" "FRESH_CHECK" (if *GM_AUTO_FRESH_DIRTY* "AVISO" "OK") *GM_AUTO_FRESH_NOTE*))
                 ;; F2: filas con codigo real descartadas por contrato CSV
                 ;; (columna corta o codigo fuera de ^[A-Za-z0-9_-]+$).
                 (if (> *GM_AUTO_CSV_BAD* 0)
                   (GM:AutoTrace "LOTE" "CSV_CONTRATO" "AVISO"
                     (strcat "descartadas=" (itoa *GM_AUTO_CSV_BAD*) "; " *GM_AUTO_CSV_BAD_MSG*)))
               )
             )
             (setq *GM_AUTO_CANCELLED* nil)
             ;; OPT8: reanudacion por progreso y tope de compactacion por corrida.
             ;; CHUNK 2026-09-29: chunk_lim = compactacion en caliente cada N
             ;; codigos (sin cerrar el DWG); pausa_lim = tope duro legado opcional.
             (setq prog_path (GM:AutoProgressPath folder)
                   done (if *GM_AUTO_RESUME* (GM:AutoProgressLoad prog_path) nil)
                   pausa_lim (if (and (numberp *GM_AUTO_MAX_CODES*) (> *GM_AUTO_MAX_CODES* 0)) *GM_AUTO_MAX_CODES* 0)
                   chunk_lim (if (and (numberp *GM_AUTO_CHUNK*) (> *GM_AUTO_CHUNK* 0)) *GM_AUTO_CHUNK* 0)
                   procesados 0 skipped 0 launcher_skipped 0 fallos 0 pausa nil desde_chunk 0)
             (if (> chunk_lim 0)
               (GM:AutoTrace "LOTE" "CHUNK_CONFIG" "INFO"
                 (strcat "compactacion en caliente cada " (itoa chunk_lim) " codigos; sin cierre de DWG")))
             (if done (GM:AutoTrace "LOTE" "REANUDACION" "INFO"
                (strcat "progreso=" (itoa (length done)) " codigos ya exportados se omiten")))
             ;; DIAG1 2026-10-03: cabecera de corrida (id/hora/entorno). Resuelve
             ;; sesiones mezcladas y correlaciona reporte/forense con el monitor
             ;; externo de recursos y el dump de Windows. En diagnostico el
             ;; contador de releases COM va activo (costo despreciable).
             (setq *GM_AUTO_WRAP_DEBUG* T)
             (setq *GM_AUTO_RUN_ID* (rtos (getvar "CDATE") 2 8))
             (GM:AutoTrace "LOTE" "CORRIDA" "INFO"
               (strcat "id=" *GM_AUTO_RUN_ID*
                       " acadver=" (getvar "ACADVER")
                       " dwg=" (getvar "DWGNAME")
                       " csv=" (if (and csv (= (type csv) 'STR)) csv "")
                       " sello=" (if (boundp '*GM_VER_AUTO*) *GM_VER_AUTO* "?")
                       " chunk=" (itoa chunk_lim)
                       " wrap_debug=1"))
              ;; FASE 0: arranque limpio de la instrumentacion de rendimiento.
             ;; OPT5 2026-09-27 (F0/F1): totales del formato y cache de calibracion
             ;; tambien arrancan limpios (la cache vive lo que dura el lote).
             (setq *GM_PERF_ROWS* nil *GM_PERF_CUR* nil *GM_FMT_TOTALS* nil *GM_AUTO_CALIB_CACHE* nil)
            ;; PEND1: arranque limpio del registro de pendientes de la corrida.
            (setq *GM_AUTO_FAILED* nil *GM_AUTO_LAST_ERR_STAGE* nil *GM_AUTO_WRAP_RELEASES* 0)
             ;; OPT1/BZ-OPT1 (Fase 3 completa): foto de lote ANTES de purgar la
             ;; grilla estatica; al cierre se restaura (ver fin del lote).
             (vl-catch-all-apply 'GM:SnapshotBatchState '())
             (GM:AutoPurgeStaticGrid)
             ;; OPT2 2026-09-26 (Fase 2): indice estatico de vias del Model (WCS),
             ;; una sola vez para todo el lote; por codigo solo se reproyecta.
             (if *GM_AUTO_INDEX_ROADS*
               (progn
                 (setq t_idx (getvar "MILLISECS"))
                 (vl-catch-all-apply 'GM:BatchIndexRoads '())
                 (GM:AutoTrace "LOTE" "TIEMPO_INDICE_VIAS" "INFO"
                   (strcat "ms=" (itoa (- (getvar "MILLISECS") t_idx))))))
             ;; OPT6 2026-09-27 (F2): indice de entidades del Model una sola vez
             ;; por lote (hyperlink/capa/tipo); los consumidores caen al barrido
             ;; original si el indice no esta listo.
             (if *GM_AUTO_INDEX_ENTS*
               (progn
                 (setq t_idx (getvar "MILLISECS"))
                 (setq _nents (vl-catch-all-apply 'UBI_BatchIndexEnts '()))
                 (GM:AutoTrace "LOTE" "INDICE_ENTIDADES"
                   (if (vl-catch-all-error-p _nents) "ERROR" "OK")
                   (strcat "ent=" (if (vl-catch-all-error-p _nents) "0" (itoa _nents))
                           " hyperlink/capa/tipo listos por lote"))
                 (GM:AutoTrace "LOTE" "TIEMPO_INDICE_ENTIDADES" "INFO"
                   (strcat "ms=" (itoa (- (getvar "MILLISECS") t_idx))))))
             ;; CHG4 2026-10-12: lavar el rotulo DEP/PROV/DIST viejo del modelo
             ;; base UNA vez por lote (ON: solo el fresco del codigo; OFF:
             ;; ninguno). Debe correr ANTES de PrimeGeoBoxesCache para que la
             ;; medicion de obstaculos no cuente textos contaminados.
             ;; Fallo = AVISO y el flujo continua como hoy (fallback).
             (setq _cb (vl-catch-all-apply 'GM:CleanGeoBase '()))
             (if (vl-catch-all-error-p _cb)
               (GM:AutoTrace "LOTE" "GEO_BASE_LIMPIA" "AVISO"
                 (vl-catch-all-error-message _cb)))
             ;; PA-2 (H3) 2026-09-28: marco del mapa y obstaculos base del layout
             ;; se miden UNA vez por lote (el layout base no cambia entre codigos).
             (vl-catch-all-apply 'GM:PrimeGeoBoxesCache '())
             ;; OPT2 2026-09-26 (Fase 5): desactivar UNDO durante el lote si es
             ;; seguro; se restaura al cierre y en el handler de cancelacion.
             (GM:AutoUndoControlBegin)
             (foreach row rows
               (if (not *GM_AUTO_CANCELLED*)
                 (cond
                   ((GM:AutoCodeDone (nth 14 row) done)
                     (setq skipped (1+ skipped))
                     (GM:AutoTrace "LOTE" "REANUDACION" "INFO" (strcat "omitido OK " (GM:AutoReportValue (nth 14 row)))))
                   ((GM:AutoLauncherSkipCodeP (nth 14 row))
                     (setq launcher_skipped (1+ launcher_skipped)))
                   (T
                    (progn
                  (setq model_before (GM:AutoEntitySnapshot "Model")
                        layout_before (GM:AutoEntitySnapshot "Layout") batcherr *error*)
                 ;; PEND1: la etapa en ERROR es por codigo; se limpia antes de
                 ;; procesar (la fija GM:AutoTrace en el primer ERROR).
                 (setq *GM_AUTO_LAST_ERR_STAGE* nil)
                 (setq result (vl-catch-all-apply 'GM:AutoProcessCode (list row folder)))
                  (setq *error* batcherr)
                   (if (vl-catch-all-error-p result)
                     (progn
                       ;; PEND1: capturar etapa/motivo antes de ResetPerCodeState.
                       (setq _pt_stage (if (and *GM_AUTO_LAST_ERR_STAGE* (/= *GM_AUTO_LAST_ERR_STAGE* "")) *GM_AUTO_LAST_ERR_STAGE* "FIN")
                             _pt_msg (vl-catch-all-error-message result))
                       ;; catch-all can bypass local *error* handlers. Clean before another row.
                       (GM:AutoCaptureRun model_before layout_before)
                       (setq _clean (GM:AutoCleanRun))
                       (vl-catch-all-apply 'GM:PurgeTempBlocks '())
                       (setq _verify (GM:AutoVerifyClean model_before layout_before))
                       (setq _rm (car _verify) _rl (cadr _verify))
                       (if (or (not (numberp _rm)) (not (numberp _rl))) (setq _rm 0 _rl 0))
                       (vl-catch-all-apply 'UBI_RestoreHiddenByGM '())
                       (vl-catch-all-apply 'GM:RestoreLayerState '())
                       (vl-catch-all-apply 'GM:RestoreViewportState '())
                       (GM:ResetPerCodeState)
                       (gc)
                       (setq *GM_AUTO_CANCELLED* T *GM_AUTO_MODE* nil)
                      ;; BE: la rama de excepcion tambien deja voz (traza + consola).
                      (GM:AutoTrace (nth 14 row) "LIMPIEZA"
                        (if (> (+ _rm _rl) 0) "ERROR" "OK")
                        (strcat "restM=" (itoa _rm) " restL=" (itoa _rl) " (ruta de excepcion)"))
                      (princ (strcat "\n[" (GM:AutoReportValue (nth 14 row)) "] ERROR: " (vl-catch-all-error-message result)))
                      (GM:AutoWriteReport (nth 14 row) "ERROR"
                        (strcat (vl-catch-all-error-message result)
                                (if (> (+ _rm _rl) 0)
                                  (strcat " Limpieza parcial: restM=" (itoa _rm) " restL=" (itoa _rl))
                                  ""))
                        "")
                      ;; PEND1: el codigo que muere por excepcion tambien entra
                      ;; al CSV de pendientes (motivo = mensaje de la excepcion).
                      (GM:AutoPendRecord (nth 14 row) _pt_stage "ERROR" _pt_msg))
                    (if result (progn (setq success (1+ success)) (GM:AutoProgressAppend prog_path (nth 14 row))))
                  )
                  ;; OPT1/BZ-OPT1 (Fase 1): frontera de recoleccion entre codigos.
                  (gc)
                  (setq procesados (1+ procesados))
                  ;; PEND1: fallo suave (result nil) = codigo incompleto; entra al
                  ;; CSV de pendientes con la primera etapa en ERROR y el motivo.
                  (if (not result)
                    (progn
                      (setq fallos (1+ fallos))
                      (GM:AutoPendRecord (nth 14 row)
                        (if (and *GM_AUTO_LAST_ERR_STAGE* (/= *GM_AUTO_LAST_ERR_STAGE* "")) *GM_AUTO_LAST_ERR_STAGE* "FIN")
                        "ERROR" *GM_AUTO_LAST_MESSAGE*)))
                  ;; CHUNK 2026-09-29: compactacion EN CALIENTE cada chunk_lim
                  ;; codigos (sin cerrar el DWG). No se ejecuta en el ultimo
                  ;; codigo por visitar (el cierre ya restaura todo) ni tras una
                  ;; cancelacion/excepcion (el cierre de lote se encarga).
                  (setq desde_chunk (1+ desde_chunk))
                  (if (and (> chunk_lim 0) (>= desde_chunk chunk_lim)
                           (not *GM_AUTO_CANCELLED*)
                           (< (+ procesados skipped) total))
                    (progn
                      (GM:CompactaChunk)
                      (setq desde_chunk 0)
                      (GM:AutoTrace "LOTE" "CHUNK_RESUMEN" "INFO"
                        (strcat "procesados_corrida=" (itoa procesados)
                                " compactado_cada=" (itoa chunk_lim)))))
                  ;; OPT8: tope de compactacion: pausa tras N procesados en la corrida.
                  (if (and (not pausa) (> pausa_lim 0) (>= procesados pausa_lim))
                    (progn
                      (setq pausa T *GM_AUTO_CANCELLED* T)
                      (GM:AutoTrace "LOTE" "PAUSA_COMPACTACION" "OK"
                        (strcat "procesados=" (itoa procesados) " en esta corrida; cierre y reabra el DWG y re-ejecute: reanudacion automatica"))))
                )
                )
              )
              )
              )
            (GM:AutoTrace "LOTE" "REANUDACION" "INFO"
              (strcat "omitidos_ok=" (itoa skipped) " diferidos_lanzador=" (itoa launcher_skipped)
                      " procesados=" (itoa procesados) " fallos=" (itoa fallos)))
           ;; PEND1 2026-09-29: CSV de pendientes al finalizar (fallos + no
           ;; intentados). add_unattempted solo en cierre normal o cancelacion;
           ;; en pausa legado los faltantes se retomaran con la reanudacion.
            (vl-catch-all-apply 'GM:AutoPendWrite (list folder rows (not pausa)))
            (setq *GM_AUTO_LAUNCHER_SKIP* nil)
           ;; AUD1: contador de liberaciones de wrappers (solo con debug activo).
           (if *GM_AUTO_WRAP_DEBUG*
             (GM:AutoTrace "LOTE" "WRAP_DEBUG" "INFO" (strcat "releases=" (itoa *GM_AUTO_WRAP_RELEASES*))))
           (if pausa
             (princ (strcat "\nPausa de compactacion: " (itoa procesados) " codigos procesados. Cierre y reabra el DWG y re-ejecute GM_CONTINUAR_LOTE (1 clic, sin panel) para continuar (reanudacion automatica)."))
             (if *GM_AUTO_CANCELLED*
               (princ (strcat "\nLote cancelado: " (itoa success) "/" (itoa total) " DWG preliminares generados."))
               (princ (strcat "\nLote finalizado: " (itoa success) "/" (itoa total) " DWG preliminares generados."))
             )
           )
            ;; OPT8: el progreso se conserva si queda trabajo (pausa, cancelacion o fallos).
            ;; FIX 30-09: una corrida que SOLO omite codigos ya hechos (procesados=0)
            ;; no borra el progreso; antes un CSV de 5 codigos todos omitidos lo
            ;; eliminaba y se perdia la lista de reanudacion del lote completo.
            (if (and (not pausa) (not *GM_AUTO_CANCELLED*) (> procesados 0) (= fallos 0) (>= (+ success skipped) total) prog_path (findfile prog_path))
              (vl-file-delete prog_path))
           ;; OPT2 2026-09-26 (Fase 5): restaurar UNDO antes de devolver la plantilla.
           (GM:AutoUndoControlEnd)
           ;; OPT1/BZ-OPT1 (Fase 3 completa): devolver la plantilla a su estado
           ;; previo al lote (capas + viewport exacto), aun con lote cancelado,
           ;; para que otro lote arranque intacto sin recargar el DWG.
           (vl-catch-all-apply 'GM:RestoreBatchState '())
           ;; OPT2 2026-09-26 (Fase 2): soltar el indice WCS de vias al cerrar.
           (setq *GM_AUTO_ROADS_INDEX_READY* nil *GM_AUTO_ROAD_WCS* nil)
            (GM:AutoTrace "LOTE" "RESTAURACION_LOTE" (if *GM_AUTO_CLEAN_FAILED* "AVISO" "OK")
              "Plantilla devuelta al estado previo al lote")
            ;; T2.2-FIX1 (28-09-2026): una sola pasada final contra la mascara
            ;; base devuelve cada entidad indexada a su visibilidad original;
            ;; debe correr ANTES de liberar el indice (fallback legado si el
            ;; indice no se construyo). Si falla queda traza ERROR.
            (setq vis_rest (vl-catch-all-apply 'UBI_RestoreVisibilityBase '()))
            (vl-catch-all-apply 'GM:AutoTrace
              (list "LOTE" "VISIBILIDAD_FINAL"
                (if (vl-catch-all-error-p vis_rest) "ERROR" "OK")
                (if (vl-catch-all-error-p vis_rest)
                  (vl-catch-all-error-message vis_rest)
                  (strcat "restauradas=" (if (numberp vis_rest) (itoa vis_rest) "0")))))
            ;; OPT6 2026-09-27 (F2/F1b): liberar indice de entidades y cache de
            ;; calibracion al cerrar el lote (sesion limpia para el siguiente).
            (vl-catch-all-apply 'UBI_ResetEntIndex '())
           (setq *GM_AUTO_CALIB_CACHE* nil)
           ;; P1-1/P1-4/P0-6 2026-09-29: soltar caches del lote (espejos de vias).
           (setq *GM_RED_BOXES_CACHE* nil *GM_RED_BOXES_CACHE_READY* nil
                 *GM_MODEL_BBOX_CACHE* nil *GM_TEXT_MEASURE_CACHE* nil)
           ;; PA-2 (H3): soltar las medidas base del layout al cerrar el lote.
           (setq *GM_AUTO_MAPBOX_LOTE* nil *GM_AUTO_MAPBOX_LOTE_READY* nil
                 *GM_AUTO_GEO_BOXES* nil *GM_AUTO_GEO_BOXES_READY* nil)
           ;; PA-3 (H7): soltar el indice de textos del Layout al cerrar el lote.
           (setq *GM_AUTO_LTEXT_INDEX* nil *GM_AUTO_LTEXT_READY* nil)
           ;; FASE 0: resumen de rendimiento del lote (primer/ultimo/factor por etapa).
           (vl-catch-all-apply 'GM:PerfSummarizeBatch '())
           ;; Fase 1C (H-14): cerrar el descriptor persistente al terminar el lote.
           (vl-catch-all-apply 'GM:AutoReportClose '())
        )
      )
    )
    (alert "Selecciona un CSV y una carpeta de salida valida.")
  )
  (setq *error* olderr)
  (if old_ctab (setvar "CTAB" old_ctab))
  (princ)
)

;; ============================================================================
;; CHUNK 2026-09-29: GM_CONTINUAR_LOTE ? reanudacion en 1 clic.
;; Retoma el lote anterior sin volver a pedir CSV/carpeta/modo: usa el progreso
;; (GM_PLANOS_LOTE_PROGRESO.csv), el sidecar GM_PLANOS_LOTE_CONFIG.csv de la
;; carpeta de salida (fuente de verdad) y el reporte en modo anexar.
;; Si falta el progreso o el CSV, avisa con alert + traza y no inicia.
;; FIX2 2026-09-30: si la huella no trae SHOW_PREDIO_LOC (o llega invalido),
;; el default es "1" (mostrar circulos verdes); un "0" guardado a proposito
;; se respeta. Antes el default era "0" y la reanudacion los perdia.
;; FIX3 2026-09-30: lee el sidecar ANTES que ML_SUITE (el DWG reabierto
;; conservaba el CSV de Fase A y el toggle "0": incidente del salto a 167);
;; valida que el CSV contenga los codigos del progreso (fail-closed) y muestra
;; una alerta pre-vuelo con CSV/circulos/modo antes de arrancar.
;; ============================================================================
(defun c:GM_CONTINUAR_LOTE (/ folder csv prog_path done mk_pdf show_loc show_geo tipo vp cfg cfg_src rows bad _pv)
  ;; FIX3 2026-09-30: la configuracion efectiva se lee del sidecar en disco
  ;; (GM_PLANOS_LOTE_CONFIG.csv) y no de la huella ML_SUITE del DWG: una
  ;; continuacion tras crash/reabrir ya no hereda CSV/toggle viejos (causa del
  ;; salto a 167 sin circulos). Sin sidecar se cae a ML_SUITE con aviso.
  (setq folder (cond
                 ((and *GM_AUTO_OUTPUT_FOLDER* (/= *GM_AUTO_OUTPUT_FOLDER* "") (vl-file-directory-p *GM_AUTO_OUTPUT_FOLDER*))
                   *GM_AUTO_OUTPUT_FOLDER*)
                 (T (vl-catch-all-apply 'vlax-ldata-get (list "ML_SUITE" "CSV_FOLDER")))))
  (if (vl-catch-all-error-p folder) (setq folder nil))
  (setq cfg_src "ML_SUITE (sin sidecar)" cfg nil)
  (if (and folder (= (type folder) 'STR) (/= folder ""))
    (setq prog_path (GM:AutoProgressPath folder)
          done (GM:AutoProgressLoad prog_path)
          cfg (vl-catch-all-apply 'GM:AutoConfigRead (list (GM:AutoConfigPath folder)))))
  (if (vl-catch-all-error-p cfg) (setq cfg nil))
  (if (and cfg (= (type cfg) 'LIST) (>= (length cfg) 4))
    (progn
      ;; Sidecar valido: manda el disco, no el DWG.
      (setq cfg_src (strcat "SIDECAR " (if (= (type (nth 4 cfg)) 'STR) (nth 4 cfg) ""))
            csv (nth 0 cfg)
            mk_pdf (nth 2 cfg)
            show_loc (nth 3 cfg)
            ;; CHG2: sidecar viejo sin SHOW_GEO_TXT -> nil -> default activo.
            show_geo (nth 5 cfg)
            ;; PRY1: sidecar viejo sin TIPO_PLANO/VP_TYPE -> nil -> defaults.
            tipo (nth 6 cfg)
            vp (nth 7 cfg))
      (if (and (nth 1 cfg) (= (type (nth 1 cfg)) 'STR) (/= (nth 1 cfg) "") (vl-file-directory-p (nth 1 cfg)))
        (setq folder (nth 1 cfg))))
    (progn
      ;; Sin sidecar: huella del DWG como respaldo.
      (setq csv (vl-catch-all-apply 'vlax-ldata-get (list "ML_SUITE" "CSV_PATH")))
      (if (vl-catch-all-error-p csv) (setq csv nil))
      (setq mk_pdf (vl-catch-all-apply 'vlax-ldata-get (list "ML_SUITE" "MAKE_PDF")))
      (if (vl-catch-all-error-p mk_pdf) (setq mk_pdf 2))
      (setq show_loc (vl-catch-all-apply 'vlax-ldata-get (list "ML_SUITE" "SHOW_PREDIO_LOC")))
      (if (vl-catch-all-error-p show_loc) (setq show_loc nil))
      (setq show_geo (vl-catch-all-apply 'vlax-ldata-get (list "ML_SUITE" "SHOW_GEO_TXT")))
      (if (vl-catch-all-error-p show_geo) (setq show_geo nil))))
  (if (and csv (= (type csv) 'STR) (not (findfile csv))) (setq csv nil))
  ;; Modo persistido (1 = SOLO DWG, 2 = DWG+PDF); default DWG+PDF.
  ;; FIX 30-09: el escritor siembra 2/1 desde esta revision; el default de
  ;; lectura tambien es 2 (antes 1) para no degradar un DWG sin huella.
  (setq mk_pdf (if (or (equal mk_pdf 1) (equal mk_pdf "1")) 1 2))
  (setq rows (if csv (GM:AutoReadCsv csv) nil))
  ;; FIX3 fail-closed: todo codigo del progreso debe existir en el CSV.
  (setq bad (if (and done rows) (GM:AutoProgressCompat done rows) nil))
  ;; Prevalidacion con mensajes claros: nunca iniciar a medias.
  (cond
    ((not (and folder (= (type folder) 'STR) (/= folder "") (vl-file-directory-p folder)))
      (alert "GM_CONTINUAR_LOTE: no hay carpeta de salida valida (ni en memoria ni en ML_SUITE). Ejecuta GM_PLANOS_LOTE una vez.")
      (princ "\n[GM_CONTINUAR_LOTE] carpeta invalida; no iniciado."))
    ((not (and done (listp done) (> (length done) 0)))
      (alert "GM_CONTINUAR_LOTE: no hay progreso que continuar en la carpeta de salida.")
      (princ "\n[GM_CONTINUAR_LOTE] sin progreso; no iniciado."))
    ((not csv)
      (alert "GM_CONTINUAR_LOTE: no se pudo resolver el CSV de esta corrida (sidecar ni ML_SUITE). Ejecuta GM_PLANOS_LOTE una vez.")
      (princ "\n[GM_CONTINUAR_LOTE] CSV sin resolver; no iniciado."))
    ((not rows)
      (alert "GM_CONTINUAR_LOTE: el CSV resuelto no tiene codigos validos.")
      (princ "\n[GM_CONTINUAR_LOTE] CSV vacio; no iniciado."))
    (bad
      (alert (strcat "GM_CONTINUAR_LOTE: el CSV no corresponde al progreso.\n"
                     "Codigos ya exportados ausentes en " (vl-filename-base csv) ":\n"
                     (GM:AutoListJoin bad 8)
                     "\nNo se inicio nada. Revisa CSV/carpeta o reinicia el lote."))
      (princ "\n[GM_CONTINUAR_LOTE] CSV incompatible con el progreso; no iniciado."))
    (T
      ;; FIX2 30-09-2026: sin huella el default es "1" (mostrar); el resolver
      ;; conserva un "0" explicito y sanea nil/vacio/basura hacia "1".
      (setq show_loc (GM:ResolveShowPredioLoc show_loc))
      ;; CHG2 2026-10-07: idem para el rotulo DEP/PROV/DIST (sidecar viejo o
      ;; huella sin el campo -> "1"; un "0" deliberado se respeta).
      (setq show_geo (GM:ResolveShowGeoTxt show_geo))
      ;; PRY1 2026-10-12: restaura tipo de plano y datos de VP del sidecar; sin
      ;; esto la continuacion manual perderia el estilo del lote en silencio.
      (setq tipo (GM:ResolvePlanTipo tipo)
            vp (GM:ResolveVpType vp))
      ;; FIX3 pre-vuelo: el eco de consola no se ve en este entorno; la alerta
      ;; muestra que se va a correr ANTES de arrancar (CSV, circulos, modo).
      ;; QUIET1 2026-10-05: en modo desatendido el pre-vuelo va a consola/traza
      ;; para no bloquear el arranque /b; el flujo manual conserva el alert.
      (setq _pv (strcat "GM_CONTINUAR_LOTE\n"
                     "CSV: " (vl-filename-base csv) " (" (itoa (length rows)) " codigos)\n"
                     "Omitidos ya exportados: " (itoa (length done)) "\n"
                     "Circulos verdes del predio: " (if (= show_loc "1") "SI" "NO") "\n"
                     "Rotulo DEP/PROV/DIST: " (if (= show_geo "1") "SI" "NO") "\n"
                     "Tipo de plano: " tipo " (VP " (if (= vp "2") "UBICACION" "NORMAL") ")\n"
                     "Modo: " (if (= mk_pdf 1) "SOLO DWG" "DWG + PDF") "\n"
                     "Config: " cfg_src))
      (if *GM_AUTO_QUIET*
        (princ (strcat "\n" _pv))
        (alert _pv))
      (setq *GM_AUTO_CSV_PATH* csv
            *GM_AUTO_OUTPUT_FOLDER* folder
            *GM_AUTO_MAKE_PDF* (if (= mk_pdf 1) nil T)
            *GM_AUTO_SHOW_PREDIO_LOC* show_loc
            *GM_AUTO_SHOW_GEO_TXT* show_geo
            *GM_PLANO_TIPO* tipo
            *UBI_VP_TYPE* vp
            *GM_AUTO_RESUME* T
            *GM_AUTO_PANEL_STATUS* (if *GM_AUTO_MAKE_PDF* 2 1)
            *GM_AUTO_SKIP_PANEL* T
            *GM_AUTO_REPORT_APPEND* T)
      (princ (strcat "\n[GM_CONTINUAR_LOTE] continuando " (itoa (length done))
                     " codigos ya exportados desde " folder " (" cfg_src ")"))
      (c:GM_PLANOS_LOTE)
    )
  )
  (princ)
)

;; ============================================================================
;; GM_DIAGNOSTICO: vuelca el estado de la plantilla a GM_DIAGNOSTICO.txt junto al DWG.
;; Ejecutar desde el layout plantilla y compartir el TXT para recalibrar sin adivinar.
;; ============================================================================
(defun GM:DiagPut (f label value / s)
  (setq s (cond ((= (type value) 'STR) value) ((= value nil) "<nil>") (T (vl-princ-to-string value))))
  (write-line (strcat label ": " s) f)
)

(defun c:GM_DIAGNOSTICO (/ f path old_ctab lay ss i e ed nm n_vp n_all msg_ver)
  (setq old_ctab (getvar "CTAB")
        path (strcat (getvar "DWGPREFIX") "GM_DIAGNOSTICO.txt")
        f (open path "w"))
  (if (not f) (progn (alert "No se pudo escribir GM_DIAGNOSTICO.txt junto al DWG.") (exit)))
  (write-line "=== GM_DIAGNOSTICO PLANTILLA ===" f)
  (GM:DiagPut f "FECHA_CDATE" (rtos (getvar "CDATE") 2 0))
  (GM:DiagPut f "DWG" (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
  (GM:DiagPut f "CTAB_ACTIVO" old_ctab)
  (GM:DiagPut f "CVPORT" (vl-princ-to-string (getvar "CVPORT")))
  (GM:DiagPut f "VER_MENU" (if (boundp '*GM_VER_MENU*) *GM_VER_MENU* nil))
  (GM:DiagPut f "VER_PLANO" (if (boundp '*GM_VER_PLANO*) *GM_VER_PLANO* nil))
  (GM:DiagPut f "VER_LAYOUT" (if (boundp '*GM_VER_LAYOUT*) *GM_VER_LAYOUT* nil))
  (GM:DiagPut f "VER_AUTO" (if (boundp '*GM_VER_AUTO*) *GM_VER_AUTO* nil))
  (setq msg_ver (GM:CheckVersions))
  (GM:DiagPut f "VERSIONES_OK" (if msg_ver (apply 'strcat (mapcar '(lambda (m) (strcat "[" m "]")) msg_ver)) "SI"))
  (GM:DiagPut f "LAYOUTS" (apply 'strcat (mapcar '(lambda (l) (strcat l " ")) (layoutlist))))
  (GM:DiagPut f "BLOQUE_LEYENDA_DEF" (if (tblsearch "BLOCK" "LEYENDA DE UBICACION") "SI" "NO"))
  (foreach lay (layoutlist)
    (setvar "CTAB" lay)
    (write-line (strcat "--- LAYOUT " lay " ---") f)
    (setq ss (ssget "X" (list (cons 0 "VIEWPORT") (cons 410 lay)))
          n_vp 0)
    (if ss
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq e (ssname ss i) ed (entget e) n_vp (1+ n_vp))
          (write-line (strcat "VP handle=" (cdr (assoc 5 ed))
                              " num69=" (vl-princ-to-string (cdr (assoc 69 ed)))
                              " W=" (vl-princ-to-string (cdr (assoc 40 ed)))
                              " H=" (vl-princ-to-string (cdr (assoc 41 ed)))
                              " centro=" (vl-princ-to-string (cdr (assoc 10 ed)))
                              " capa=" (cdr (assoc 8 ed))
                              " ML_VP_NUM=" (vl-princ-to-string (vl-catch-all-apply 'vlax-ldata-get (list e "ML_VP_NUM")))
                              " DYN_VP_CENTER=" (vl-princ-to-string (vl-catch-all-apply 'vlax-ldata-get (list e "DYN_VP_CENTER")))
                              " DYN_VP_RECT=" (vl-princ-to-string (vl-catch-all-apply 'vlax-ldata-get (list e "DYN_VP_RECT")))) f)
          (setq i (1+ i))
        )
      )
    )
    (GM:DiagPut f "VIEWPORTS_EN_LAYOUT" n_vp)
    (setq ss (ssget "X" (list (cons 0 "VIEWPORT")))
          n_all (if ss (sslength ss) 0))
    (GM:DiagPut f "VIEWPORTS_TOTAL_DWG" n_all)
    (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 410 lay)))
          i 0)
    (if ss
      (while (< i (sslength ss))
        (setq e (ssname ss i) ed (entget e) nm (cdr (assoc 2 ed)))
        (if (and nm (wcmatch (strcase nm) "*LEYENDA*"))
          (write-line (strcat "LEYENDA handle=" (cdr (assoc 5 ed))
                              " bloque=" nm
                              " oculto60=" (vl-princ-to-string (cdr (assoc 60 ed)))) f)
        )
        (setq i (1+ i))
      )
    )
  )
  (setvar "CTAB" old_ctab)
  (foreach lay '("D5CF2" "166D8E" "167F4D" "15BBFB")
    (setq e (handent lay))
    (GM:DiagPut f (strcat "HANDLE_" lay) (if (and e (entget e)) (cdr (assoc 0 (entget e))) "AUSENTE"))
  )
  (GM:DiagPut f "CSV_ML_SUITE" (vl-catch-all-apply 'vlax-ldata-get (list "ML_SUITE" "CSV_PATH")))
  (GM:DiagPut f "LAYOUT_TAB_MEM" (if (boundp '*GM_AUTO_LAYOUT_TAB*) *GM_AUTO_LAYOUT_TAB* nil))
  (GM:DiagPut f "UBI_ACTIVE_VP" (if (boundp '*UBI_ACTIVE_VP*) *UBI_ACTIVE_VP* nil))
  (close f)
  (alert (strcat "Diagnostico escrito en:\n" path))
  (princ (strcat "\nDiagnostico: " path))
  (princ)
)

(setq *GM_VER_AUTO* "2026-10-21-FIX1")
(princ "\nInfraestructura de automatizacion GM cargada. Revision 2026-10-21-FIX1.")
(princ)



