;; ============================================================================
;; GM_TAREA.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================



;; E9: cada etiqueta se envuelve en vl-catch-all Y se verifica su retorno; nil => ERROR + FAILED que bloquea la exportacion.
(defun GM:AutoLayoutForCode (code row / old_tab layout_before layout_after result rec afectaciones)
  (GM:ActivateTemplatePaper)
   (GM:AutoTrace code "INICIO_LAYOUT" "INICIO" "Inicio de generacion Layout")
   ;; F15-R3: vaciado antes de calibracion/esquemas (fase con mas COM y la que
   ;; mas riesgo de AV concentra).
   (GM:AutoReportFlush)
  (setq old_tab (getvar "CTAB") *ML_TARGET_CODE* code *GL_DATOS_CSV* row
         *ML_ESQ_ACTIVO* "1" *ML_ESQ_MODO* "0" *GM_AUTO_MODE* T)
  (setq *GM_AUTO_LAYOUT_FAILED* nil *GM_PLACED_BOXES* nil *GM_LABEL_EXCLUDE* nil *GM_LANDING_GAP* nil *GM_LABEL_SAMPLES* nil *GM_LABEL_DIST2* nil)
  ;; BW: una deteccion vial por codigo; AF y propietario comparten los tramos.
  (GM:ResetRoadCache)
  (GM:LayoutCalibrate)
  (if (not (GM:LayoutHideScaleBase))
    (GM:LayoutFail "No se pudo ocultar el cuadro de escala preexistente"))
  ;; Fase 1C (H-13): la fila ya viene del lote y *GL_DATOS_CSV* ya es row; se
  ;; aplican las reglas de PE/UC directo sin reabrir ni recorrer el CSV por
  ;; codigo. GM:AutoApplyCsvDefaults replica exactamente el cond de LUBI_ParseCSV.
  (GM:AutoApplyCsvDefaults row)
  ;; El esquema izquierdo identifica el predio (U.C. col 14 / distrito col 12);
  ;; el localizador lo busca dentro del mapa regional para los circulos.
  ;; La busqueda ocurre para cada fila, no una vez para todo el lote.
  (if (= *GM_AUTO_SHOW_PREDIO_LOC* "1")
    (if (GM:AutoLocatePredioAffected row)
      (GM:AutoTrace code "PREDIO_LOCALIZACION" "OK" (strcat "Predio ubicado en mapa: " (nth 13 row)))
      (progn
        (GM:AutoTrace code "PREDIO_LOCALIZACION" "AVISO" (strcat "No se ubico el predio en el mapa: " (nth 13 row)))
        ;; FIX3: un plano que exporta OK sin los circulos verdes ya no queda en
        ;; silencio: entra al CSV de pendientes (revisar/regenerar a mano).
        (GM:AutoPendRecord code "PREDIO_LOCALIZACION" "AVISO"
          "predio no ubicado en el mapa; plano sin senalizacion verde"))))
  (setq layout_before (GM:AutoEntitySnapshot "Layout"))
  (princ (strcat "\n[" code "] Generando membrete y Layout..."))
  (GM:GenerateMemberAuto row)
  (GM:AutoTrace code "MEMBRETE" "OK" "Membrete generado")
  ;; FIX 30-09: en lote EXEC se envuelve para que un aborto interno
  ;; (LayoutFail/exit, p.ej. capacidad de zona) degrade a fallo suave con la
  ;; limpieza por diferencia de AutoProcessCode; en manual se conserva el
  ;; contrato original (una excepcion aborta el comando, comportamiento E9).
  (if *GM_AUTO_MODE*
    (progn
      (setq result (vl-catch-all-apply 'EXEC_ESQUEMAS_PU_FULL '()))
      (if (vl-catch-all-error-p result)
        (progn
          (GM:AutoTrace code "ESQUEMAS" "ERROR" (vl-catch-all-error-message result))
          (if (= *GM_AUTO_LAST_MESSAGE* "No procesado")
            (setq *GM_AUTO_LAST_MESSAGE* (vl-catch-all-error-message result)))
          (setq *GM_AUTO_LAYOUT_FAILED* T))
        (GM:AutoTrace code "ESQUEMAS" "OK" "Esquemas PU ejecutados")))
    (progn
      (EXEC_ESQUEMAS_PU_FULL)
      (GM:AutoTrace code "ESQUEMAS" "OK" "Esquemas PU ejecutados")))
  ;; Texto DEP/PROV/DIST obligatorio: una excepcion o retorno nil marca el
  ;; Layout incompleto para impedir que se exporte nuevamente sin el rotulo.
  ;; CHG2 2026-10-07: el rotulo es desactivable por configuracion; apagado
  ;; explicito deja GEO_FIJO;OMITIDO y el plano exporta sin el bloque.
  (if (= (GM:ResolveShowGeoTxt *GM_AUTO_SHOW_GEO_TXT*) "1")
    (progn
      (setq result (vl-catch-all-apply 'GM:AutoGeoFijoSmallMap (list code row)))
      (if (or (vl-catch-all-error-p result) (not result))
        (progn
          (if (vl-catch-all-error-p result)
            (GM:AutoTrace code "GEO_FIJO" "ERROR" (vl-catch-all-error-message result)))
          (setq *GM_AUTO_LAYOUT_FAILED* T)
          (if (= *GM_AUTO_LAST_MESSAGE* "No procesado")
            (setq *GM_AUTO_LAST_MESSAGE* "No se pudo colocar el texto DEP/PROV/DIST sin solapes")))))
    (GM:AutoTrace code "GEO_FIJO" "OMITIDO" "Rotulo DEP/PROV/DIST desactivado por configuracion"))
  (GM:GenerateLayoutFooter row)
  ;; Etiquetas autom\\U+00E1ticas en lugar de omisiones - con captura de errores y verificacion de retorno.
  ;; E9: un retorno nil (sin ancla visible / sin espacio) es fallo controlado: se registra ERROR y
  ;; se marca *GM_AUTO_LAYOUT_FAILED* para que el codigo no se exporte como completo.
  ;; Las funciones ya trazan su OK/ERROR especifico; el envoltorio solo registra
  ;; excepciones, propaga el motivo real a LAST_MESSAGE y marca FAILED (politica estricta).
  ;; AO: mayor area primero para que el AF grande elija sitio y los chicos se
  ;; acomoden despues (multi-AF con poco espacio, como la version manual).
  (setq afectaciones (GM:OrdenarAfectaciones *GL_DATA_AFECTACIONES*))
  (foreach rec afectaciones
    (setq result (vl-catch-all-apply 'GM:AutoLabelAfectacion (list rec)))
    (if (or (vl-catch-all-error-p result) (not result))
      (progn
        (if (vl-catch-all-error-p result)
          (GM:AutoTrace code "ETIQUETA_AFECTACION" "ERROR" (vl-catch-all-error-message result)))
        (if (= *GM_AUTO_LAST_MESSAGE* "No procesado")
          (setq *GM_AUTO_LAST_MESSAGE* (strcat "Etiqueta AF-" (GM:AF_Suffix rec) " sin colocar; plano incompleto")))
        (setq *GM_AUTO_LAYOUT_FAILED* T)
      )
    )
  )
  (GM:AutoTrace code "PROP_INICIO" "INICIO" "AU inicio de etiqueta propietario")
  ;; F15-R3: ultimo vaciado antes del barrido PROP (donde murio el lote de 44).
  (GM:AutoReportFlush)
  (setq result (vl-catch-all-apply 'GM:AutoLabelPropietario (list row)))
  (if (or (vl-catch-all-error-p result) (not result))
    (progn
      (if (vl-catch-all-error-p result)
        (GM:AutoTrace code "ETIQUETA_PROPIETARIO" "ERROR" (vl-catch-all-error-message result)))
      (if (= *GM_AUTO_LAST_MESSAGE* "No procesado")
        (setq *GM_AUTO_LAST_MESSAGE* "Etiqueta de propietario sin colocar; plano incompleto"))
      (setq *GM_AUTO_LAYOUT_FAILED* T)
    )
  )
  (GM:AutoTrace code "VIA_INICIO" "INICIO" "Inicio de etiquetas viales")
  (setq result (vl-catch-all-apply 'GM:AutoLabelVias (list)))
  (if (or (vl-catch-all-error-p result) (not result))
    (progn
      (if (vl-catch-all-error-p result)
        (GM:AutoTrace code "ETIQUETAS_VIA" "ERROR" (vl-catch-all-error-message result)))
      (if (= *GM_AUTO_LAST_MESSAGE* "No procesado")
        (setq *GM_AUTO_LAST_MESSAGE* "Etiquetas de via sin colocar; plano incompleto"))
      (setq *GM_AUTO_LAYOUT_FAILED* T)
    )
  )
  ;; UBICADOR: omitido - requiere georreferenciaci\\U+00F3n del mapa regional
  (GM:LayoutOmit "UBICADOR")
   ;; El modelo base ya contiene el mapa geografico y sus textos.
   (GM:AutoTrace code "UBICACION_GEOGRAFICA" "OK" "Conservada del modelo base")
  (if (/= (getvar "CTAB") old_tab) (setvar "CTAB" old_tab))
  (setq layout_after (GM:AutoEntitySnapshot "Layout"))
   (if (and (not *GM_AUTO_LAYOUT_FAILED*) (> (length (GM:AutoNewEntities layout_before layout_after)) 0))
    (progn
      (princ (strcat "\n[" code "] Layout generado correctamente."))
      T
    )
    (progn
      (princ (strcat "\n[" code "] No se generaron entidades nuevas del Layout."))
      nil
    )
  )
)

(defun GM:AutoProcessCode (row folder / code model_before layout_before output_path pdf_path dwg_path ok layout_ok olderr plot_started dwg_started make_pdf pdf_ok _clean _verify _rest _locks _blocks _blkfail t0 t_model t_layout model_ok wrap0 m_after)
   ;; FASE 0: inicio de cronometro del codigo (instrumentacion solamente).
   (setq *GM_PERF_CODE_START* (GM:PerfMs) *GM_PERF_CUR* nil)
   ;; F15-R3: contador forense por codigo (cada codigo arranca su ventana).
   (setq *GM_AUTO_FORENSIC_N* 0)
   ;; DIAG1 2026-10-03: base del contador de releases COM para el delta del codigo.
   (setq wrap0 (if (boundp '*GM_AUTO_WRAP_RELEASES*) *GM_AUTO_WRAP_RELEASES* 0))
   (GM:ActivateTemplatePaper)
   ;; OPT1/BZ-OPT1 (Fase 3): snapshots de capas y viewport antes de tocar la plantilla.
   (vl-catch-all-apply 'GM:SnapshotLayerState '())
   (vl-catch-all-apply 'GM:SnapshotViewportState '())
   (setq *GM_AUTO_CLEAN_FAILED* nil *GM_AUTO_SCALE_RESTORE_OK* T *GM_CLEANUP_IN_PROGRESS* nil)
   ;; OPT2 2026-09-26 (Fase 4): marcador previo a la generacion del codigo.
   (setq *GM_AUTO_MARK_LOTE* (GM:AutoMarkCapture))
   (GM:ResetTempBlocks)
    (setq olderr *error*
          ;; P0-2 2026-09-29: fila corta no debe producir code nil silencioso.
          code (if (>= (length row) 15) (nth 14 row) nil)
          model_before (GM:AutoEntitySnapshot "Model") layout_before (GM:AutoEntitySnapshot "Layout") ok nil)
    ;; PA-3 (F5) 2026-09-28: tamano de la base al entrar el codigo (control de
    ;; deriva por registros borrados entre codigos; solo instrumentacion).
    (GM:AutoTrace code "BASE_MODELO" "INFO"
      (strcat "model_before=" (itoa (length model_before)) " layout_before=" (itoa (length layout_before))))
    ;; F15-R3: vaciado del reporte antes de la fase pesada del codigo.
    (GM:AutoReportFlush)
   (setq *GM_AUTO_PERF_CODE* code)
   (setq *GM_AUTO_LAST_STATUS* "ERROR" *GM_AUTO_LAST_MESSAGE* "No procesado")
   (defun *error* (msg)
     ;; OPT1/BZ-OPT1 (Fase 2): restaurar el handler antes de limpiar (anti-reentrada).
     (setq *error* olderr *GM_CLEANUP_IN_PROGRESS* nil)
      (GM:AutoCaptureRun model_before layout_before)
      (GM:AutoCleanRun)
      (vl-catch-all-apply 'GM:PurgeTempBlocks '())
      (if *GM_AUTO_LAYOUT_TAB* (setvar "CTAB" *GM_AUTO_LAYOUT_TAB*))
      ;; BF: si el export murio con FILEDIA/CMDECHO a 0, devolver interactivos.
      (if (= (getvar "FILEDIA") 0) (setvar "FILEDIA" 1))
      (if (= (getvar "CMDECHO") 0) (setvar "CMDECHO" 1))
      (if (= (getvar "CMDDIA") 0) (setvar "CMDDIA" 1))
      (if (/= (getvar "CVPORT") 1) (command "_.pspace"))
     (vl-catch-all-apply 'UBI_RestoreHiddenByGM '())
     (if (wcmatch (strcase msg) "*CANCEL*,*BREAK*") (setq *GM_AUTO_CANCELLED* T))
     ;; Fase 1A (H-11): reportar el estado REAL de limpieza/purga antes de
     ;; cualquier reset. Antes se borraban *GM_TEMP_BLOCKS*,
     ;; *GM_AUTO_CLEAN_FAILED* y *GM_AUTO_SCALE_RESTORE_OK* aqui mismo y se
     ;; perdia la evidencia de un fallo de limpieza o de una purga parcial.
     (vl-catch-all-apply 'GM:AutoTrace
       (list code "LIMPIEZA_ERROR" "ERROR"
         (strcat "blkRestos=" (if *GM_TEMP_BLOCKS* (apply 'strcat (mapcar '(lambda (n) (strcat n ",")) *GM_TEMP_BLOCKS*)) "0")
                 " cleanFailed=" (if *GM_AUTO_CLEAN_FAILED* "1" "0")
                 " scaleOk=" (if *GM_AUTO_SCALE_RESTORE_OK* "1" "0"))))
         (setq *GM_AUTO_LAST_STATUS* "ERROR" *GM_AUTO_LAST_MESSAGE* msg
                 *GM_AUTO_MODE* nil *GL_DATA_AFECTACIONES* nil *UBI_VP_DATA_MEM* nil *UBI_TARGET_REFERENCE* "" *UBI_TARGET_POLYGON* nil *GM_PLACED_BOXES* nil *GM_LABEL_EXCLUDE* nil *GM_LANDING_GAP* nil *GM_LABEL_SAMPLES* nil *GM_LABEL_DIST2* nil *GM_PROP_USED_GAP* nil *GM_ROAD_BOXES_CACHE* nil *GM_ROAD_BOXES_CACHE_READY* nil *GM_CLEANUP_IN_PROGRESS* nil *error* olderr)
     (vl-catch-all-apply 'GM:RestoreLayerState '())
     (vl-catch-all-apply 'GM:RestoreViewportState '())
      (vl-catch-all-apply 'GM:AutoTrace (list code "ERROR" "ERROR" msg))
     (vl-catch-all-apply 'GM:AutoWriteReport (list code "ERROR" msg ""))
     (princ (strcat "\nCodigo " code " omitido: " msg))
    (princ)
  )
   (setq t_model (GM:PerfMs))
   (setq model_ok (and code (/= code "") (GM:AutoModelForCode code row)))
   (GM:PerfMark code "MODELO" t_model)
   (if model_ok
     (progn
        (setq t_layout (GM:PerfMs))
        (setq layout_ok (GM:AutoLayoutForCode code row))
        (if (or (not layout_ok)
                (<= (length (GM:AutoNewEntities layout_before (GM:AutoEntitySnapshot "Layout"))) 0))
          (progn
             (if (= *GM_AUTO_LAST_MESSAGE* "No procesado")
               (setq *GM_AUTO_LAST_MESSAGE* "No se generaron entidades nuevas del Layout")
             )
            (setq layout_ok nil)
          )
        )
        (GM:PerfMark code "LAYOUT" t_layout)
          ;; BO: el panel siempre fija el global (1/2); el guard boundp con
          ;; default T era vestigio del toggle y apunta a la direccion
          ;; insegura. Snapshot directo: un nil solo da OMITIDO, nunca PDF.
          (GM:AutoTrace code "EXPORTACION_PDF_GLOBAL" "INFO"
            (strcat "global=" (if *GM_AUTO_MAKE_PDF* "1" "0")
                    " bound=" (if (boundp '*GM_AUTO_MAKE_PDF*) "1" "0")))
          (setq dwg_path (GM:AutoOutputPath folder code)
                pdf_path (GM:AutoPdfOutputPath folder code)
                output_path dwg_path
                make_pdf *GM_AUTO_MAKE_PDF*)
          (if layout_ok
            (progn
              (setq dwg_started (getvar "MILLISECS"))
              (GM:AutoTrace code "EXPORTACION_DWG_INICIO" "INICIO" dwg_path)))
          (if (and layout_ok (GM:AutoExportDwg dwg_path))
            (progn
              (GM:AutoTrace code "EXPORTACION_DWG" "OK" dwg_path)
              (GM:AutoTrace code "EXPORTACION_DWG_TIEMPO" "INFO"
                (strcat "ms=" (itoa (- (getvar "MILLISECS") dwg_started))))
              (GM:PerfMark code "EXPORT_DWG" dwg_started)
              (setq ok T *GM_AUTO_LAST_STATUS* "OK" *GM_AUTO_LAST_MESSAGE* "DWG preliminar; etiquetas automaticas generadas, revisar trazas")
              ;; BH forense: valor resuelto del toggle por codigo.
              (GM:AutoTrace code "EXPORTACION_PDF_ESTADO" "INFO" (strcat "make_pdf=" (if make_pdf "1" "0")))
              ;; F8: metrica de ingenieria del predio en el reporte (area,
              ;; perimetro, centro UTM, escala, zona, datum, U.C.).
              (vl-catch-all-apply 'GM:AutoMetrica (list code row))
              ;; BB: PDF auxiliar opcional. Su fallo no tumba el DWG principal.
              (if make_pdf
                (progn
                  (setq plot_started (getvar "MILLISECS"))
                  (GM:AutoTrace code "EXPORTACION_PDF_INICIO" "INICIO" pdf_path)
                  (setq pdf_ok (GM:AutoExportPdf pdf_path))
                  (if pdf_ok
                    (progn
                      (GM:AutoTrace code "EXPORTACION_PDF" "OK" pdf_path)
                      (GM:AutoTrace code "EXPORTACION_PDF_TIEMPO" "INFO"
                        (strcat "ms=" (itoa (- (getvar "MILLISECS") plot_started))))
                      (GM:PerfMark code "EXPORT_PDF" plot_started)
                      (setq *GM_AUTO_LAST_MESSAGE* (strcat *GM_AUTO_LAST_MESSAGE* " + PDF auxiliar")))
                    (progn
                      (GM:AutoTrace code "EXPORTACION_PDF" "ERROR" *GM_AUTO_LAST_MESSAGE*)
                      (setq *GM_AUTO_LAST_MESSAGE* (strcat "DWG preliminar; etiquetas automaticas generadas, revisar trazas [PDF auxiliar fallo]")))
                  )
                )
                (progn
                  (GM:AutoTrace code "EXPORTACION_PDF" "OMITIDO" "PDF auxiliar desactivado en el panel")
                  ;; BG: con el auxiliar desactivado, un PDF viejo del codigo
                  ;; pareceria recien generado; se elimina y se deja traza.
                  (if (GM:AutoFileExists pdf_path)
                    (progn
                      (vl-file-delete pdf_path)
                      (GM:AutoTrace code "EXPORTACION_PDF" "OMITIDO" "PDF auxiliar desactivado; PDF anterior eliminado"))))
              )
            )
            (if layout_ok
              (if (= *GM_AUTO_LAST_MESSAGE* "No procesado")
                (setq *GM_AUTO_LAST_MESSAGE* "No se pudo exportar el DWG")
              )
             (if (= *GM_AUTO_LAST_MESSAGE* "No procesado")
               (setq *GM_AUTO_LAST_MESSAGE* "No se genero contenido del Layout")
             )
            )
         )
         (if (not layout_ok) (GM:AutoTrace code "LAYOUT" "ERROR" *GM_AUTO_LAST_MESSAGE*))
     )
      (if (= *GM_AUTO_LAST_MESSAGE* "No procesado")
        (setq *GM_AUTO_LAST_MESSAGE* (if (and code (/= code "")) "MODELO: la rutina no confirmo exito y no informo un motivo" "Codigo vacio"))
      )
   )
     ;; DIAG1 2026-10-03: vaciado tras la exportacion para no perder la cola si
     ;; el AV llega durante snapshot/limpieza (hueco 3550->3831 del FIX3).
     (GM:AutoReportFlush)
     (setq t0 (GM:PerfMs))
     (GM:AutoCaptureRun model_before layout_before)
     (GM:PerfMark code "SNAPSHOT_CAPTURA" t0)
     (setq t0 (GM:PerfMs))
     (setq _clean (GM:AutoCleanRun))
     ;; OPT1/BZ-OPT1 (Fase 1): purga dirigida de bloques temporales creados por esquemas.
     (setq _blocks (GM:PurgeTempBlocks))
     (GM:PerfMark code "LIMPIEZA" t0)
     (setq _blkfail (cadr _blocks))
    (GM:AutoTrace code "BLOQUES_TEMP"
      (if (= _blkfail 0) "OK" "ERROR")
      (strcat "borrados=" (itoa (car _blocks))
              " fallos=" (itoa _blkfail)
              (if (caddr _blocks)
                (strcat " restos=" (apply 'strcat (mapcar '(lambda (n) (strcat n ",")) (caddr _blocks))))
                " restos=0")))
    (setq t0 (GM:PerfMs))
    (setq _verify (GM:AutoVerifyClean model_before layout_before))
    (GM:PerfMark code "SNAPSHOT_VERIFY" t0)
    (setq _rest (+ (car _verify) (cadr _verify)))
    (setq _locks (GM:JoinLocked (append (nth 6 _clean) (nth 7 _clean))))
    (GM:AutoTrace code "LIMPIEZA"
      (if (and (= _rest 0) (= _blkfail 0) (not *GM_AUTO_CLEAN_FAILED*) *GM_AUTO_SCALE_RESTORE_OK*) "OK" "ERROR")
      (strcat "creM=" (itoa (car _clean)) " delM=" (itoa (nth 2 _clean))
              " restM=" (itoa (car _verify)) " creL=" (itoa (cadr _clean))
              " delL=" (itoa (nth 3 _clean)) " restL=" (itoa (cadr _verify))
              " blkDel=" (itoa (car _blocks)) " blkFail=" (itoa _blkfail)
              (if (> (+ (nth 4 _clean) (nth 5 _clean)) 0)
                (strcat " fallos=" _locks)
                "")))
    ;; DIAG1 2026-10-03: delta de releases COM del codigo y base Model tras la
    ;; limpieza. Si releases crece codigo a codigo hay retencion de wrappers;
    ;; model_after estable confirma que la limpieza de entidades no deriva.
    (setq m_after (GM:AutoEntitySnapshot "Model"))
    (GM:AutoTrace code "WRAP_DELTA" "INFO"
      (strcat "releases=" (itoa (- (if (boundp '*GM_AUTO_WRAP_RELEASES*) *GM_AUTO_WRAP_RELEASES* 0) wrap0))
              " model_after=" (itoa (length m_after))))
    ;; OPT1/BZ-OPT1 (Fase 2 estricta): limpieza o purga parcial invalida el codigo y
    ;; detiene el lote para no contaminar el siguiente plano.
    (if (or (> _rest 0) (> _blkfail 0) *GM_AUTO_CLEAN_FAILED* (not *GM_AUTO_SCALE_RESTORE_OK*))
      (progn
        (setq ok nil *GM_AUTO_LAST_STATUS* "ERROR" *GM_AUTO_CANCELLED* T)
        (setq *GM_AUTO_LAST_MESSAGE* (strcat *GM_AUTO_LAST_MESSAGE* " [Limpieza parcial]"))))
   (setq t0 (GM:PerfMs))
   (if *GM_AUTO_LAYOUT_TAB* (setvar "CTAB" *GM_AUTO_LAYOUT_TAB*))
   (if (/= (getvar "CVPORT") 1) (command "_.pspace"))
   (vl-catch-all-apply 'UBI_RestoreHiddenByGM '())
   ;; OPT1/BZ-OPT1 (Fase 1/3): frontera de recoleccion entre codigos + reset central.
   (GM:ResetPerCodeState)
   (gc)
   (setq *error* olderr)
    (GM:PerfMark code "RESTAURACION" t0)
    (if (not ok) (setq *GM_AUTO_LAST_STATUS* "ERROR"))
    (GM:AutoWriteReport code *GM_AUTO_LAST_STATUS* *GM_AUTO_LAST_MESSAGE* output_path)
    (GM:PerfMark code "TOTAL" *GM_PERF_CODE_START*)
    (GM:PerfRecordCode)
    (princ (strcat "\n[" code "] " *GM_AUTO_LAST_STATUS* ": " *GM_AUTO_LAST_MESSAGE*))
    ok
)

;; ============================================================================
;; CHUNK 2026-09-29: compactacion EN CALIENTE entre codigos (sin cerrar el DWG).
;; Se ejecuta SOLO en la frontera entre codigos (tras AutoCleanRun +
;; ResetPerCodeState del codigo anterior), nunca dentro de un codigo. Secuencia
;; segura (ver AUDITORIA/F15 y plan de chunks):
;;   1. UBI_RestoreVisibilityBase: devuelve cada entidad indexada a su
;;      visibilidad original ANTES de liberar el indice (orden obligatorio
;;      VISIBILIDAD > ResetEntIndex).
;;   2. UBI_ResetEntIndex + soltar caches por codigo (rojos, bbox Model, medidas,
;;      geo/ltext del layout); la calibracion de lote se conserva (handles vivos).
;;   3. Reconstruir indices del lote con catch: entidades y vias. Un fallo deja
;;      AVISO y los consumidores caen al barrido COM por codigo (correcto).
;;   4. (gc) + flush del reporte/forense.
;; Todo fallo degrada a AVISO/ERROR en el reporte; nunca aborta el lote.
;; ============================================================================
(defun GM:CompactaChunk (/ vis_rest _r _e _g)
  (GM:AutoTrace "LOTE" "CHUNK_INICIO" "INFO" "compactacion en caliente entre codigos")
  ;; 1) visibilidad base (orden obligatorio antes de resetear el indice)
  (setq vis_rest (vl-catch-all-apply 'UBI_RestoreVisibilityBase '()))
  (GM:AutoTrace "LOTE" "COMPACT_VIS" (if (vl-catch-all-error-p vis_rest) "ERROR" "OK")
    (if (vl-catch-all-error-p vis_rest)
      (vl-catch-all-error-message vis_rest)
      (strcat "restauradas=" (if (numberp vis_rest) (itoa vis_rest) "0"))))
  ;; 2) indice de entidades fuera + caches por codigo
  (vl-catch-all-apply 'UBI_ResetEntIndex '())
  (setq *GM_RED_BOXES_CACHE* nil *GM_RED_BOXES_CACHE_READY* nil
        *GM_MODEL_BBOX_CACHE* nil *GM_TEXT_MEASURE_CACHE* nil
        *GM_AUTO_MAPBOX_LOTE* nil *GM_AUTO_MAPBOX_LOTE_READY* nil
        *GM_AUTO_GEO_BOXES* nil *GM_AUTO_GEO_BOXES_READY* nil
        *GM_AUTO_LTEXT_INDEX* nil *GM_AUTO_LTEXT_READY* nil
        *GM_AUTO_GEO_XF* nil)
  ;; 3) reconstruir indices del lote (degradan a AVISO, no abortan)
  (setq _r (vl-catch-all-apply 'GM:BatchIndexRoads '()))
  (GM:AutoTrace "LOTE" "COMPACT_VIAS" (if (vl-catch-all-error-p _r) "AVISO" "OK")
    (if (vl-catch-all-error-p _r)
      (strcat "fallback a barrido por codigo: " (vl-catch-all-error-message _r))
      "indice WCS de vias reconstruido"))
  (setq _e (vl-catch-all-apply 'UBI_BatchIndexEnts '()))
  (GM:AutoTrace "LOTE" "COMPACT_ENTIDADES" (if (vl-catch-all-error-p _e) "AVISO" "OK")
    (if (vl-catch-all-error-p _e)
      (strcat "fallback a barrido COM: " (vl-catch-all-error-message _e))
      (strcat "ent=" (if (numberp _e) (itoa _e) "0"))))
  (setq _g (vl-catch-all-apply 'GM:PrimeGeoBoxesCache '()))
  (GM:AutoTrace "LOTE" "COMPACT_GEO" (if (vl-catch-all-error-p _g) "AVISO" "OK")
    (if (vl-catch-all-error-p _g)
      (strcat "medicion por codigo: " (vl-catch-all-error-message _g))
      "geo/textos del layout reconstruidos"))
  ;; 4) liberar memoria y asegurar la cola del reporte
  (gc)
  (vl-catch-all-apply 'GM:AutoReportFlush '())
  (GM:AutoTrace "LOTE" "CHUNK_FIN" "OK"
    (strcat "compactacion completada; corrida continua sin cierre"
            (if *GM_AUTO_WRAP_DEBUG*
              (strcat " releases=" (itoa *GM_AUTO_WRAP_RELEASES*))
              "")))
  ;; DIAG2 2026-10-03: re-fallos parciales a disco (sobreviven al AV).
  (vl-catch-all-apply 'GM:AutoPendPartialWrite (list *GM_AUTO_OUTPUT_FOLDER*))
  T
)