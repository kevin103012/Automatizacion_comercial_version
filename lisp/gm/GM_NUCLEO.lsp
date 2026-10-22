;; ============================================================================
;; GM_NUCLEO.lsp - Sistema GM Ingenieros (modulo gm)
;; Movido SIN CAMBIOS desde AUTOMATIZACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================
(vl-load-com)

;;; ============================================================================
;;; Infraestructura de corridas automaticas
;;; Trabaja por diferencia de handles: elimina entidades nuevas de cada corrida.
;;; No restaura propiedades de originales ni la grilla base purgada al inicio
;;; (ver GM:AutoPurgeStaticGrid); correr sobre copia hasta validar restauracion.
;;; ============================================================================

;; Version del modulo y versiones requeridas de los otros tres (orden de carga:
;; MENU_PLANOS, PLANO_UBICACION, LAYOUT_UBICACION, AUTOMATIZACION). Si el lote
;; detecta un modulo ausente o desactualizado, se niega a correr en vez de
;; mezclar codigo viejo con nuevo.
(setq *GM_VER_AUTO* nil)
;; RECYCLE1 2026-10-04: el lanzador externo recicla el proceso entre bloques y
;; pasa los codigos diferidos en *GM_AUTO_LAUNCHER_SKIP*; el flujo manual no cambia.
;; QUIET1 2026-10-05: en modo desatendido (lanzador / reanudacion silenciosa) los
;; avisos informativos van a consola/traza; los fail-closed conservan su alert.
;; SYS1 2026-10-05: los archivos de trabajo (progreso/sidecar/parcial/forense)
;; viven en la subcarpeta _sistema de la salida; el forense solo en diagnostico.
;; FIX4 2026-10-05: blindaje del lote (WBLOCK con unwind-protect) y cortacircuitos
;; del lanzador (CrashCounts persistente, reintento marcado al ejecutar).
;; FIX5 2026-10-05 (AUD2): contrato CSV en el LISP, gate de DWG valido,
;; reanudacion PROGRESO+DWG, reset/escala por codigo, escala unica en hoja y
;; linea METRICA de ingenieria. Lanzador: re-hash CSV por bloque, Save atomico,
;; stall-kill opt-in, poda de historiales/pilotos.
(setq *GM_VER_REQUIRED* '(("*GM_VER_MENU*" . "2026-09-30-FIX3") ("*GM_VER_PLANO*" . "2026-10-10-CHG3") ("*GM_VER_LAYOUT*" . "2026-10-10-CHG3") ("*GM_VER_AUTO*" . "2026-10-21-FIX1")))

;; Retorna lista de mensajes de modulos faltantes o desactualizados (vacia = todo OK).
(defun GM:CheckVersions (/ msgs pair sym want got)
  (setq msgs nil)
  (foreach pair *GM_VER_REQUIRED*
    (setq sym (car pair) want (cdr pair) got nil)
    (if (boundp (read sym)) (setq got (eval (read sym))))
    (if (not (equal got want))
      (setq msgs (cons (strcat sym " esperado " want " hallado " (if got got "<no cargado>")) msgs))
    )
  )
  (reverse msgs)
)
;; Fase B 2026-10-12: str-starts-with vive en LAYOUT_UBICACION; si AUTOMATIZACION
;; se cargara sin ese modulo, este fallback evita "no function definition"
;; (misma implementacion; con LAYOUT cargado no se redefine nada).
(if (not (boundp 'str-starts-with))
  (defun str-starts-with (str pat) (if (and str pat (>= (strlen str) (strlen pat))) (= (substr str 1 (strlen pat)) pat) nil)))
;; Estado del lote (handles creados, pestana, reporte, CSV/carpeta, cancelacion).
;; Las globales de leyenda/zonas/escala se inicializan abajo y en LayoutCalibrate.
(if (not *GM_AUTO_CREATED_MODEL*) (setq *GM_AUTO_CREATED_MODEL* nil))
(if (not *GM_AUTO_CREATED_LAYOUT*) (setq *GM_AUTO_CREATED_LAYOUT* nil))
(if (not *GM_AUTO_LAYOUT_TAB*) (setq *GM_AUTO_LAYOUT_TAB* nil))
(if (not *GM_AUTO_REPORT_PATH*) (setq *GM_AUTO_REPORT_PATH* nil))
;; Fase 1C (H-14): descriptor persistente del reporte; evita abrir/cerrar el
;; CSV en cada traza (~80-90 por codigo).
(if (not (boundp '*GM_AUTO_REPORT_H*)) (setq *GM_AUTO_REPORT_H* nil))
(if (not *GM_AUTO_LAST_STATUS*) (setq *GM_AUTO_LAST_STATUS* ""))
(if (not *GM_AUTO_LAST_MESSAGE*) (setq *GM_AUTO_LAST_MESSAGE* ""))
(if (not *GM_AUTO_CSV_PATH*) (setq *GM_AUTO_CSV_PATH* nil))
(if (not *GM_AUTO_OUTPUT_FOLDER*) (setq *GM_AUTO_OUTPUT_FOLDER* nil))
(if (not *GM_AUTO_PREVIEW_ROWS*) (setq *GM_AUTO_PREVIEW_ROWS* nil))
(if (not *GM_AUTO_CANCELLED*) (setq *GM_AUTO_CANCELLED* nil))
;; Visualizacion automatica del predio afectado en el mapa de localizacion.
;; La U.C. del CSV localiza el predio; nunca se solicita un clic sobre la linea roja.
;; FIX2 30-09-2026: nace ACTIVADA ("1"); con el default viejo "0" la reanudacion
;; de Fase A (167-234) genero 57 planos sin los circulos verdes. Un "0"
;; explicito del usuario se respeta (ver GM:ResolveShowPredioLoc).
(if (not *GM_AUTO_SHOW_PREDIO_LOC*) (setq *GM_AUTO_SHOW_PREDIO_LOC* "1"))
;; CHG2 2026-10-07: rotulo geografico DEP/PROV/DIST del mapa regional.
;; "1" = dibujarlo (default; comportamiento historico); "0" explicito = omitirlo
;; con traza GEO_FIJO;OMITIDO (el plano exporta sin el bloque). Misma semantica
;; que *GM_AUTO_SHOW_PREDIO_LOC*.
(if (not *GM_AUTO_SHOW_GEO_TXT*) (setq *GM_AUTO_SHOW_GEO_TXT* "1"))
(if (not *GM_AUTO_PREDIO_LOC_RECT*) (setq *GM_AUTO_PREDIO_LOC_RECT* nil))
(if (not *GM_AUTO_PREDIO_LOC_ENT_HANDLES*) (setq *GM_AUTO_PREDIO_LOC_ENT_HANDLES* nil))
(if (not (boundp '*GM_AUTO_MAPCONTENT_BOX*)) (setq *GM_AUTO_MAPCONTENT_BOX* nil))

;; FIX2 30-09-2026: normaliza el toggle de la senalizacion del predio.
;; "1" = mostrar circulos verdes (default); "0" = no mostrar, solo si es
;; explicito; cualquier otro valor (nil, "", basura) cae al default activo.
(defun GM:ResolveShowPredioLoc (value)
  (if (or (equal value "0") (equal value 0))
    "0"
    "1"))

;; CHG2 2026-10-07: normaliza el toggle del rotulo DEP/PROV/DIST con la misma
;; regla (default activo; "0"/0 explicito lo apaga; nil/vacio/basura -> "1").
(defun GM:ResolveShowGeoTxt (value)
  (if (or (equal value "0") (equal value 0))
    "0"
    "1"))

;; PRY1 2026-10-12: tipo de plano del lote (estilo de formato: UBICACION,
;; MATRIZ, DIAGNOSTICO, PERIMETRICO) y datos de grilla del VP ("1" NORMAL,
;; "2" UBICACION). Valores fuera de catalogo caen al default historico; la
;; continuacion manual los restaura del sidecar para no cambiar el estilo.
(defun GM:ResolvePlanTipo (value / v)
  (setq v (if (= (type value) 'STR) (strcase (vl-string-trim " \t\r\n" value)) ""))
  (if (member v '("UBICACION" "MATRIZ" "DIAGNOSTICO" "PERIMETRICO")) v "UBICACION")
)
(defun GM:ResolveVpType (value / v)
  (setq v (if (= (type value) 'STR) (vl-string-trim " \t\r\n" value) ""))
  (if (= v "2") "2" "1")
)

;; BB: PDF auxiliar opcional (T = generar junto al DWG principal).
(if (not (boundp '*GM_AUTO_MAKE_PDF*)) (setq *GM_AUTO_MAKE_PDF* T))
;; BE: metodo de exportacion DWG ("WBLOCK" por comando; el metodo COM anterior
;; no existe en este Civil y se elimino).
(if (not (boundp '*GM_AUTO_DWG_METODO*)) (setq *GM_AUTO_DWG_METODO* "WBLOCK"))
;; BG: auto-respuesta al dialogo Map de WBLOCK ("_Yes" conserva datos Map,
;; "_No" exporta magro, nil = confirmacion manual por codigo).
(if (not (boundp '*GM_AUTO_MAPA_ANS*)) (setq *GM_AUTO_MAPA_ANS* "_Yes"))
;; OPT1/BZ-OPT1 2026-09-26 (Fase 1): cache lista para nil + registro de bloques temporales.
(if (not (boundp '*GM_ROAD_BOXES_CACHE_READY*)) (setq *GM_ROAD_BOXES_CACHE_READY* nil))
(if (not (boundp '*GM_TEMP_BLOCKS*)) (setq *GM_TEMP_BLOCKS* nil))
;; OPT1/BZ-OPT1 2026-09-26 (Fase 2/3): estado de limpieza y restauracion integral.
(if (not (boundp '*GM_CLEANUP_IN_PROGRESS*)) (setq *GM_CLEANUP_IN_PROGRESS* nil))
(if (not (boundp '*GM_AUTO_CLEAN_FAILED*)) (setq *GM_AUTO_CLEAN_FAILED* nil))
(if (not (boundp '*GM_AUTO_SCALE_RESTORE_OK*)) (setq *GM_AUTO_SCALE_RESTORE_OK* T))
(if (not (boundp '*GM_AUTO_LAYER_SNAPSHOT*)) (setq *GM_AUTO_LAYER_SNAPSHOT* nil))
(if (not (boundp '*GM_AUTO_VP_SNAPSHOT*)) (setq *GM_AUTO_VP_SNAPSHOT* nil))
;; OPT2 2026-09-26 (Fases 2/4/5): indice estatico de vias del lote, deteccion de
;; entidades nuevas por marcador entlast/entnext y control de UNDO. Todo opt-in
;; con fallback al comportamiento previo: sin la condicion, nada cambia.
(if (not (boundp '*GM_AUTO_INDEX_ROADS*)) (setq *GM_AUTO_INDEX_ROADS* T))
(if (not (boundp '*GM_AUTO_ROADS_INDEX_READY*)) (setq *GM_AUTO_ROADS_INDEX_READY* nil))
(if (not (boundp '*GM_AUTO_ROAD_WCS*)) (setq *GM_AUTO_ROAD_WCS* nil))
(if (not (boundp '*GM_AUTO_SNAPSHOT_MARKER*)) (setq *GM_AUTO_SNAPSHOT_MARKER* T))
(if (not (boundp '*GM_AUTO_MARK_LOTE*)) (setq *GM_AUTO_MARK_LOTE* nil))
(if (not (boundp '*GM_AUTO_MARKER_PARITY*)) (setq *GM_AUTO_MARKER_PARITY* nil))
(if (not (boundp '*GM_AUTO_LAST_CREATED_M*)) (setq *GM_AUTO_LAST_CREATED_M* nil))
(if (not (boundp '*GM_AUTO_LAST_CREATED_L*)) (setq *GM_AUTO_LAST_CREATED_L* nil))
(if (not (boundp '*GM_AUTO_UNDO_CONTROL*)) (setq *GM_AUTO_UNDO_CONTROL* T))
(if (not (boundp '*GM_AUTO_UNDO_STATE*)) (setq *GM_AUTO_UNDO_STATE* nil))
(if (not (boundp '*GM_AUTO_UNDO_APPLIED*)) (setq *GM_AUTO_UNDO_APPLIED* nil))
(if (not (boundp '*GM_AUTO_UNDO_SKIP_REASON*)) (setq *GM_AUTO_UNDO_SKIP_REASON* nil))
;; OPT5 2026-09-27 (F0/F1): sub-tiempos/conteos del formato del Model (F0, los
;; llena PLANO_UBICACION) y cache de calibracion del lote (F1: viewport/leyenda/
;; marcos resueltos una sola vez; avisos LEGACY-MIGRADO solo al resolver). Todo
;; opt-in: sin *GM_AUTO_MODE* no se cachea nada y el flujo manual no cambia.
(if (not (boundp '*GM_FMT_TOTALS*)) (setq *GM_FMT_TOTALS* nil))
(if (not (boundp '*GM_AUTO_CALIB_CACHE*)) (setq *GM_AUTO_CALIB_CACHE* nil))
;; OPT6 2026-09-27 (F2): indice estatico de entidades del Model por lote
;; (hyperlink/capa/tipo; lo construye PLANO_UBICACION y lo consumen visibilidad,
;; formato, textos, auditoria PE/UC y busqueda de poligono).
(if (not (boundp '*GM_AUTO_INDEX_ENTS*)) (setq *GM_AUTO_INDEX_ENTS* T))
;; OPT8 2026-09-27 (checkpoint/compactacion): reanudacion por progreso y tope de
;; codigos por corrida. *GM_AUTO_MAX_CODES* 0 = sin tope; al alcanzarlo el lote
;; pausa para cerrar/reabrir el DWG (compactar la sesion) y continuar.
(if (not (boundp '*GM_AUTO_RESUME*)) (setq *GM_AUTO_RESUME* T))
(if (not (boundp '*GM_AUTO_MAX_CODES*)) (setq *GM_AUTO_MAX_CODES* 0))
;; CHUNK 2026-09-29: compactacion EN CALIENTE cada N codigos sin cerrar el DWG
;; (visibilidad base + indices/caches + gc). 0 = desactivado (comportamiento F15).
(if (not (boundp '*GM_AUTO_CHUNK*)) (setq *GM_AUTO_CHUNK* 12))
;; CHUNK: reanudacion en 1 clic (c:GM_CONTINUAR_LOTE). Bandera interna del panel
;; y modo de reporte en modo continuacion.
(if (not (boundp '*GM_AUTO_SKIP_PANEL*)) (setq *GM_AUTO_SKIP_PANEL* nil))
(if (not (boundp '*GM_AUTO_PANEL_STATUS*)) (setq *GM_AUTO_PANEL_STATUS* nil))
(if (not (boundp '*GM_AUTO_REPORT_APPEND*)) (setq *GM_AUTO_REPORT_APPEND* nil))
;; QUIET1 2026-10-05: modo desatendido. T = los avisos informativos (modo de
;; salida elegido, pre-vuelo de la continuacion) salen por consola/traza en vez
;; de alert modal, para que el lanzador externo o /b nunca quede esperando un
;; clic. Los errores fail-closed (versiones, CSV incompatible, carpeta invalida)
;; conservan su alert. Lo fija el lanzador en el script de arranque; el flujo
;; manual nace en nil y no cambia.
(if (not (boundp '*GM_AUTO_QUIET*)) (setq *GM_AUTO_QUIET* nil))
;; RECYCLE1 2026-10-04: codigos pospuestos por el supervisor externo durante
;; este bloque. No se escriben en PROGRESO: solo los RESULTADO OK son progreso.
(if (not (boundp '*GM_AUTO_LAUNCHER_SKIP*)) (setq *GM_AUTO_LAUNCHER_SKIP* nil))
;; PEND1 2026-09-29: CSV de pendientes (codigos que no salieron OK con su
;; motivo). *GM_AUTO_FAILED* guarda (codigo etapa estado motivo) de la corrida;
;; *GM_AUTO_LAST_ERR_STAGE* recuerda la primera etapa en ERROR del codigo en
;; curso (lo alimenta GM:AutoTrace y lo reinicia el bucle por codigo).
(if (not (boundp '*GM_AUTO_FAILED*)) (setq *GM_AUTO_FAILED* nil))
(if (not (boundp '*GM_AUTO_LAST_ERR_STAGE*)) (setq *GM_AUTO_LAST_ERR_STAGE* nil))
;; AUD1 2026-09-29: chequeo de copia fresca. Un DWG que ya corrio un lote trae
;; huellas persistentes (ML_SUITE.CSV_PATH de la corrida anterior) y la
;; restauracion no revierte formato/colores/PEUC: cada corrida sobre el mismo
;; DWG arranca mas degradada. Avisa y traza; no bloquea (kill switch abajo).
(if (not (boundp '*GM_AUTO_FRESH_CHECK*)) (setq *GM_AUTO_FRESH_CHECK* T))
(if (not (boundp '*GM_AUTO_FRESH_DIRTY*)) (setq *GM_AUTO_FRESH_DIRTY* nil))
(if (not (boundp '*GM_AUTO_FRESH_NOTE*)) (setq *GM_AUTO_FRESH_NOTE* nil))
;; AUD1 2026-09-29: contador de diagnostico de wrappers COM (opt-in). Con
;; *GM_AUTO_WRAP_DEBUG* T, GM:ReleaseObjectSafe cuenta liberaciones y el cierre
;; del lote (y cada CHUNK_FIN) reporta WRAP_DEBUG/releases=. Sin costo si nil.
(if (not (boundp '*GM_AUTO_WRAP_DEBUG*)) (setq *GM_AUTO_WRAP_DEBUG* nil))
(if (not (boundp '*GM_AUTO_WRAP_RELEASES*)) (setq *GM_AUTO_WRAP_RELEASES* 0))
;; DIAG1 2026-10-03: identificador de corrida (CDATE) para correlacionar
;; reporte/forense con el monitor externo de recursos y el dump de Windows.
(if (not (boundp '*GM_AUTO_RUN_ID*)) (setq *GM_AUTO_RUN_ID* ""))
;; OPT1/BZ-OPT1: liberacion determinista de wrappers COM (no borra entidades).
;; AUD1: con *GM_AUTO_WRAP_DEBUG* T lleva el contador de liberaciones del lote.
(defun GM:ReleaseObjectSafe (obj)
  (if (and obj (not (vl-catch-all-error-p obj)))
    (progn
      (vl-catch-all-apply 'vlax-release-object (list obj))
      (if (and (boundp '*GM_AUTO_WRAP_DEBUG*) *GM_AUTO_WRAP_DEBUG*)
        (setq *GM_AUTO_WRAP_RELEASES*
              (1+ (if (boundp '*GM_AUTO_WRAP_RELEASES*) *GM_AUTO_WRAP_RELEASES* 0)))))
  )
  nil
)
;; OPT1/BZ-OPT1: registro de bloques temporales por codigo (purga dirigida, nunca PURGE ALL).
(defun GM:RegisterTempBlock (name)
  (if (and name (/= name ""))
    (setq *GM_TEMP_BLOCKS* (cons name *GM_TEMP_BLOCKS*))
  )
  nil
)
(defun GM:ResetTempBlocks () (setq *GM_TEMP_BLOCKS* nil) T)
;; OPT2 2026-09-26 (Fase 5): control de UNDO durante el lote. UNDOCTL es de solo
;; lectura; se manipula con el comando UNDO. Se usa vl-cmdf (sincrono y con error
;; capturable) en lugar de command crudo: en campo `command` dejo el cierre del
;; lote sin ejecutar (reporte sin RESTAURACION_LOTE ni RESUMEN).
;; OPT4 2026-09-26: UNDOCTL es bitcode (1=UNDO on, 4=Auto, 8=grupo, 16=zoom,
;; 32=capas). Con Control=None el comando UNDO NO muestra el menu principal sino
;; directamente "UNDO control option [All/None/One/Combine/Layer]"; por eso enviar
;; "_Control" antes de la opcion fallaba y dejaba UNDO apagado (reporte OPT3:
;; 53 -> 48 y AVISO al cierre). La restauracion ahora es adaptativa y verificada:
;; 1) si UNDO esta apagado se envia "_All" directo; 2) se repone Auto si el estado
;; original lo tenia; 3) se exige coincidencia exacta con el UNDOCTL inicial.
(defun GM:AutoUndoCtl (/ c)
  (setq c (vl-catch-all-apply 'getvar (list "UNDOCTL")))
  (if (and (not (vl-catch-all-error-p c)) (numberp c)) c nil)
)
(defun GM:AutoUndoRaw (args / res)
  (setq res (vl-catch-all-apply 'vl-cmdf (cons "_.UNDO" args)))
  (not (vl-catch-all-error-p res))
)
(defun GM:AutoUndoCmd (opt / res)
  (setq res (vl-catch-all-apply 'vl-cmdf (list "_.UNDO" "_Control" opt)))
  (not (vl-catch-all-error-p res))
)
(defun GM:AutoUndoControlBegin (/ ctl grp ctl2)
  (setq *GM_AUTO_UNDO_APPLIED* nil *GM_AUTO_UNDO_SKIP_REASON* nil)
  (if (not *GM_AUTO_UNDO_CONTROL*)
    (progn (setq *GM_AUTO_UNDO_SKIP_REASON* "control desactivado por bandera")
           (GM:AutoTrace "LOTE" "UNDO_LOTE" "OMITIDO" *GM_AUTO_UNDO_SKIP_REASON*) nil)
    (progn
      (setq ctl (GM:AutoUndoCtl))
      (if (not (numberp ctl))
        (progn (setq *GM_AUTO_UNDO_SKIP_REASON* "UNDOCTL no disponible")
               (GM:AutoTrace "LOTE" "UNDO_LOTE" "OMITIDO" *GM_AUTO_UNDO_SKIP_REASON*) nil)
        (progn
          (setq *GM_AUTO_UNDO_STATE* ctl)
          ;; Bit 8 = grupo UNDO activo: no tocar.
          (setq grp (logand ctl 8))
          (if (/= grp 0)
            (progn (setq *GM_AUTO_UNDO_SKIP_REASON* "grupo UNDO activo; no se desactiva")
                   (GM:AutoTrace "LOTE" "UNDO_LOTE" "OMITIDO" *GM_AUTO_UNDO_SKIP_REASON*) nil)
            (progn
              (GM:AutoUndoCmd "_None")
              (setq ctl2 (GM:AutoUndoCtl))
              (if (and (numberp ctl2) (= (logand ctl2 1) 0))
                (progn (setq *GM_AUTO_UNDO_APPLIED* T)
                       (GM:AutoTrace "LOTE" "UNDO_LOTE" "OK"
                         (strcat "UNDO desactivado durante el lote (UNDOCTL " (itoa ctl) " -> " (itoa ctl2) ")"))
                       T)
                (progn
                  (GM:AutoUndoCmd "_All")
                  (setq *GM_AUTO_UNDO_SKIP_REASON* "UNDO no quedo desactivado; restaurado")
                  (GM:AutoTrace "LOTE" "UNDO_LOTE" "OMITIDO" *GM_AUTO_UNDO_SKIP_REASON*) nil)))))))))
(defun GM:AutoUndoControlEnd (/ want ctl ctl2 auto_was)
  (if *GM_AUTO_UNDO_APPLIED*
    (progn
      (setq want *GM_AUTO_UNDO_STATE*)
      (setq auto_was (and (numberp want) (= (logand want 4) 4)))
      ;; Limpiar la bandera ANTES del comando para evitar reentrada del *error*.
      (setq *GM_AUTO_UNDO_APPLIED* nil)
      ;; Paso 1: reactivar UNDO. Si esta apagado (Control=None) el comando pide la
      ;; opcion de Control directamente, asi que NO se antepone "_Control".
      (setq ctl (GM:AutoUndoCtl))
      (if (and (numberp ctl) (= (logand ctl 1) 0))
        (GM:AutoUndoRaw (list "_All"))
        (GM:AutoUndoRaw (list "_Control" "_All")))
      (setq ctl2 (GM:AutoUndoCtl))
      ;; Paso 1b: si aun sigue apagado, reintento por el menu principal.
      (if (and (numberp ctl2) (= (logand ctl2 1) 0))
        (progn (GM:AutoUndoRaw (list "_Control" "_All"))
               (setq ctl2 (GM:AutoUndoCtl))))
      ;; Paso 2: reponer modo Auto si el original lo tenia y se perdio.
      (if (and auto_was (numberp ctl2) (= (logand ctl2 4) 0))
        (progn (GM:AutoUndoRaw (list "_Auto" "_ON"))
               (setq ctl2 (GM:AutoUndoCtl))))
      ;; Paso 3: exigir coincidencia exacta con el estado original.
      (if (and (numberp ctl2)
               (= (logand ctl2 1) 1)
               (or (not (numberp want)) (= ctl2 want)))
        (GM:AutoTrace "LOTE" "UNDO_LOTE" "OK"
          (strcat "UNDO restaurado al cierre del lote (UNDOCTL "
                  (if (numberp want) (itoa want) "?") " -> " (itoa ctl2) ")"))
        (GM:AutoTrace "LOTE" "UNDO_LOTE" "AVISO"
          (strcat "UNDO no confirmado al cierre; cierre la copia sin guardar (UNDOCTL "
                  (if (numberp ctl2) (itoa ctl2) "?")
                  (if (numberp want) (strcat " esperado " (itoa want)) "") ")")))
      T)
    (progn (setq *GM_AUTO_UNDO_APPLIED* nil) nil))
)
;; OPT2 2026-09-26 (Fase 4): lectura por marcador sin comparar listas de handles
;; (H-4 O(n^2)). El marcador se toma antes de generar; aqui solo se recorre lo
;; posterior y se filtra por espacio. Layout = todo lo que no es "Model".
(defun GM:AutoMarkCapture (/ m)
  (setq m (vl-catch-all-apply 'entlast '()))
  (if (vl-catch-all-error-p m) nil m)
)
(defun GM:AutoEntitiesAfter (mark space / ent ed g result)
  (setq ent (if mark (entnext mark) (entlast)) result nil)
  (while ent
    (setq ed (entget ent) g (if ed (cdr (assoc 410 ed)) nil))
    (if (and ed
             (if (= space "Model")
               (= g "Model")
               (and g (/= g "Model"))))
      (setq result (cons ent result)))
    (setq ent (entnext ent)))
  (reverse result)
)
;; Estado de composicion por codigo; se fijan en LayoutCalibrate/ModelWork.
;; Se declaran aqui para que las rutas de error puedan leerlas sin boundp.
(if (not (boundp '*GM_AUTO_LEGEND_DATA*)) (setq *GM_AUTO_LEGEND_DATA* nil))
(if (not (boundp '*GM_AUTO_LEGEND_COPY*)) (setq *GM_AUTO_LEGEND_COPY* nil))
;; CHG1 2026-10-07: cuando la leyenda base es una ACAD_TABLE (plantilla nueva),
;; la restauracion usa visibilidad COM en vez del snapshot DXF del INSERT.
(if (not (boundp '*GM_AUTO_LEGEND_TABLE_P*)) (setq *GM_AUTO_LEGEND_TABLE_P* nil))
;; CHG3-FIX 2026-10-10: la leyenda-tabla (Chagual) trae su fondo (WIPEOUT) y las
;; simbologias como entidades independientes en capas "LEYENDA*". Se copian,
;; mueven y ocultan como conjunto junto a la tabla (snapshot DXF + copias).
(if (not (boundp '*GM_AUTO_LEGEND_SET_DATA*)) (setq *GM_AUTO_LEGEND_SET_DATA* nil))
(if (not (boundp '*GM_AUTO_LEGEND_SET_COPIES*)) (setq *GM_AUTO_LEGEND_SET_COPIES* nil))
(if (not (boundp '*GM_AUTO_SCALE_BASE_DATA*)) (setq *GM_AUTO_SCALE_BASE_DATA* nil))
(if (not (boundp '*GM_AUTO_ZONES*)) (setq *GM_AUTO_ZONES* nil))
(if (not (boundp '*GM_AUTO_LAYOUT_FAILED*)) (setq *GM_AUTO_LAYOUT_FAILED* nil))
(if (not (boundp '*GM_AUTO_SCALE_IDX*)) (setq *GM_AUTO_SCALE_IDX* nil))
(if (not (boundp '*GM_AUTO_SCALE_STR*)) (setq *GM_AUTO_SCALE_STR* nil))
(if (not (boundp '*GM_AUTO_SCALE_VAL*)) (setq *GM_AUTO_SCALE_VAL* nil))

;; FASE 0 (2026-09-26): instrumentacion de rendimiento solo-lectura. No altera
;; geometria ni salida; agrega trazas TIEMPO_* por codigo y un resumen de lote
;; con primer/ultimo tiempo y factor de degradacion por etapa.
(if (not (boundp '*GM_AUTO_PERF_CODE*)) (setq *GM_AUTO_PERF_CODE* nil))
(if (not (boundp '*GM_PERF_CODE_START*)) (setq *GM_PERF_CODE_START* nil))
(if (not (boundp '*GM_PERF_CUR*)) (setq *GM_PERF_CUR* nil))
(if (not (boundp '*GM_PERF_ROWS*)) (setq *GM_PERF_ROWS* nil))

(defun GM:AutoEntitySnapshot (space / ss i ent result)
  (setq result nil)
  (if (= space "Model")
    (setq ss (ssget "X" '((410 . "Model"))))
    (setq ss (ssget "X" (list (cons 410 (getvar "CTAB")))))
  )
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (entget ent) (setq result (cons ent result)))
        (setq i (1+ i))
      )
    )
  )
  result
)

(defun GM:AutoEntityHandles (entities / result ent data handle)
  (setq result nil)
  (foreach ent entities
    (if (and ent (setq data (entget ent)) (setq handle (cdr (assoc 5 data))))
      (setq result (cons handle result))
    )
  )
  result
)

(defun GM:AutoNewEntities (before after / result ent data handle old_handles)
  (setq old_handles (GM:AutoEntityHandles before))
  (setq result nil)
  (foreach ent after
    (setq data (entget ent) handle (if data (cdr (assoc 5 data)) nil))
    (if (and handle (not (member handle old_handles)))
      (setq result (cons ent result))
    )
  )
  result
)

;; LEGACY (conservado): borrado simple sin conteos. El lote usa
;; GM:AutoDeleteEntitiesEx con reintento y reporte handle:capa:motivo.
(defun GM:AutoDeleteEntities (entities / ent)
  (foreach ent entities
    (if (and ent (entget ent)) (entdel ent))
  )
  (princ)
)

;; Borrado con conteos honestos: retorna (list borradas fallidas bloqueos).
;; bloqueos es lista de "handle:capa:motivo". Reintenta tras thaw/on/unlock.
;; Solo toca handles de la corrida; nunca la plantilla.
(defun GM:AutoDeleteEntitiesEx (entities / ent data lay entry c62 motivo ok fail locked)
  (setq ok 0 fail 0 locked nil)
  (foreach ent entities
    (if (not (and ent (entget ent)))
      (setq ok (1+ ok))
      (progn
        (entdel ent)
        (if (entget ent)
          (progn
            (setq lay (cdr (assoc 8 (entget ent))))
            ;; P0-4 2026-09-29: vl-cmdf (capturable) en vez de command dentro de catch-all.
            (vl-catch-all-apply
              '(lambda (l) (vl-cmdf "_.-LAYER" "_T" l "_ON" l "_U" l ""))
              (list lay))
            (if (entget ent) (entdel ent))
            (if (entget ent)
              (progn
                (setq fail (1+ fail) motivo "desconocido")
                (setq entry (tblsearch "LAYER" lay))
                (if entry
                  (progn
                    (setq c62 (cdr (assoc 62 entry)))
                    (if (and (numberp c62) (< c62 0)) (setq motivo "apagada") (setq motivo "bloqueada/congelada"))
                  )
                )
                (setq data (entget ent))
                (setq locked (cons (strcat (if (cdr (assoc 5 data)) (cdr (assoc 5 data)) "?") ":" (if lay lay "?") ":" motivo) locked))
              )
              (setq ok (1+ ok))
            )
          )
          (setq ok (1+ ok))
        )
      )
    )
  )
  (list ok fail locked)
)

;; Une lista de textos con ", " para el reporte. Sin lambdas con strings.
(defun GM:JoinLocked (items / s first ent)
  (setq s "" first T)
  (foreach ent items
    (if (not first) (setq s (strcat s ", ")))
    (setq s (strcat s ent) first nil)
  )
  s
)

;; LEGACY (conservado): reinicio manual de listas creadas. El lote vigente usa
;; GM:AutoCaptureRun/GM:AutoCleanRun por codigo; se mantiene por consola.
(defun GM:AutoBeginRun ()
  (setq *GM_AUTO_CREATED_MODEL* nil
        *GM_AUTO_CREATED_LAYOUT* nil)
  (princ)
)

(defun GM:AutoCaptureRun (model_before layout_before / model_after layout_after current_tab mark_m mark_l)
  ;; OPT2 2026-09-26 (Fase 4): lectura por marcador entlast/entnext (O(n)) en vez
  ;; de comparar listas de ~9k handles (O(n^2)). Paridad demostrada 5/5 en campo.
  (if (and *GM_AUTO_SNAPSHOT_MARKER* *GM_AUTO_MARK_LOTE*)
    (progn
      (setq mark_m (GM:AutoEntitiesAfter *GM_AUTO_MARK_LOTE* "Model")
            mark_l (GM:AutoEntitiesAfter *GM_AUTO_MARK_LOTE* "Layout"))
      (setq *GM_AUTO_CREATED_MODEL* mark_m
            *GM_AUTO_CREATED_LAYOUT* mark_l)
      ;; Paridad opcional (flag) contra el diff de handles.
      (if *GM_AUTO_MARKER_PARITY*
        (GM:AutoMarkerParity model_before layout_before))
    )
    (progn
      (setq current_tab (getvar "CTAB") model_after (GM:AutoEntitySnapshot "Model"))
      (if (and *GM_AUTO_LAYOUT_TAB* (/= (getvar "CTAB") *GM_AUTO_LAYOUT_TAB*))
        (setvar "CTAB" *GM_AUTO_LAYOUT_TAB*)
      )
      (setq layout_after (GM:AutoEntitySnapshot "Layout"))
      (if (and current_tab (/= (getvar "CTAB") current_tab)) (setvar "CTAB" current_tab))
      (setq *GM_AUTO_CREATED_MODEL* (GM:AutoNewEntities model_before model_after)
            *GM_AUTO_CREATED_LAYOUT* (GM:AutoNewEntities layout_before layout_after))
    )
  )
  (princ)
)
;; OPT2 2026-09-26 (Fase 4): paridad opcional marcador vs diff de handles.
;; deltaM/deltaL = 0 significa que ambas tecnicas detectan exactamente lo mismo.
(defun GM:AutoMarkerParity (model_before layout_before / model_after layout_after current_tab d_m d_l delta_m delta_l)
  (setq current_tab (getvar "CTAB") model_after (GM:AutoEntitySnapshot "Model"))
  (if (and *GM_AUTO_LAYOUT_TAB* (/= (getvar "CTAB") *GM_AUTO_LAYOUT_TAB*))
    (setvar "CTAB" *GM_AUTO_LAYOUT_TAB*)
  )
  (setq layout_after (GM:AutoEntitySnapshot "Layout"))
  (if (and current_tab (/= (getvar "CTAB") current_tab)) (setvar "CTAB" current_tab))
  (setq d_m (GM:AutoNewEntities model_before model_after)
        d_l (GM:AutoNewEntities layout_before layout_after))
  (setq delta_m (+ (length (GM:AutoNewEntities d_m *GM_AUTO_CREATED_MODEL*))
                   (length (GM:AutoNewEntities *GM_AUTO_CREATED_MODEL* d_m)))
        delta_l (+ (length (GM:AutoNewEntities d_l *GM_AUTO_CREATED_LAYOUT*))
                   (length (GM:AutoNewEntities *GM_AUTO_CREATED_LAYOUT* d_l))))
  (GM:AutoTrace *GM_AUTO_PERF_CODE* "SNAPSHOT_MARKER" "INFO"
    (strcat "markM=" (itoa (length *GM_AUTO_CREATED_MODEL*))
            " markL=" (itoa (length *GM_AUTO_CREATED_LAYOUT*))
            " diffM=" (itoa (length d_m)) " diffL=" (itoa (length d_l))
            " deltaM=" (itoa delta_m) " deltaL=" (itoa delta_l)))
  nil
)

(defun GM:AutoCleanRun (/ cre_m cre_l res_m res_l scale_ok)
  ;; OPT1/BZ-OPT1 (Fase 2): la leyenda se restaura directo (idempotente); el guard
  ;; solo protege conteo/borrado contra reentrada recursiva del *error*.
  (GM:LayoutRestoreLegend)
  (if *GM_CLEANUP_IN_PROGRESS*
    (list 0 0 0 0 0 0 nil nil)
    (progn
  (setq *GM_CLEANUP_IN_PROGRESS* T)
  ;; OPT1/BZ-OPT1: el fallo de escala base ya no se ignora; queda en el flag global.
  (setq scale_ok (GM:LayoutRestoreScaleBase))
  (setq *GM_AUTO_SCALE_RESTORE_OK* scale_ok)
  (if (not scale_ok) (setq *GM_AUTO_CLEAN_FAILED* T))
  (setq cre_m (length *GM_AUTO_CREATED_MODEL*) cre_l (length *GM_AUTO_CREATED_LAYOUT*))
  ;; OPT2 2026-09-26 (Fase 3): conservar las entidades creadas para que
  ;; GM:AutoVerifyClean compruebe su desaparicion directo (O(creadas)) sin
  ;; volver a fotografiar el Model completo.
  (setq *GM_AUTO_LAST_CREATED_M* *GM_AUTO_CREATED_MODEL*
        *GM_AUTO_LAST_CREATED_L* *GM_AUTO_CREATED_LAYOUT*)
  (setq res_m (GM:AutoDeleteEntitiesEx *GM_AUTO_CREATED_MODEL*)
        res_l (GM:AutoDeleteEntitiesEx *GM_AUTO_CREATED_LAYOUT*))
  ;; OPT1/BZ-OPT1: un borrado con fallos tambien es fallo de limpieza (Fase 2 estricta).
  (if (> (+ (cadr res_m) (cadr res_l)) 0) (setq *GM_AUTO_CLEAN_FAILED* T))
  ;; OPT1/BZ-OPT1 (Fase 3): reponer capas tocadas por el reintento de borrado.
  (vl-catch-all-apply 'GM:RestoreLayerState '())
  (vl-catch-all-apply 'GM:RestoreViewportState '())
  (setq *GM_AUTO_CREATED_MODEL* nil
        *GM_AUTO_CREATED_LAYOUT* nil)
  (setq *GM_CLEANUP_IN_PROGRESS* nil)
  (list cre_m cre_l (car res_m) (car res_l) (cadr res_m) (cadr res_l) (caddr res_m) (caddr res_l))
    )
  )
)

;; Restos tras limpiar. OPT2 (Fase 3): si hay lista de creadas, verifica su
;; desaparicion directo (O(creadas)); si no, cae al diff de handles anterior.
(defun GM:AutoVerifyClean (model_before layout_before / model_after layout_after current_tab rest_m rest_l)
  (if (and (boundp '*GM_AUTO_LAST_CREATED_M*) *GM_AUTO_LAST_CREATED_M*)
    (progn
      (setq rest_m 0 rest_l 0)
      (foreach ent *GM_AUTO_LAST_CREATED_M*
        (if (and ent (entget ent)) (setq rest_m (1+ rest_m))))
      (foreach ent *GM_AUTO_LAST_CREATED_L*
        (if (and ent (entget ent)) (setq rest_l (1+ rest_l))))
      (setq *GM_AUTO_LAST_CREATED_M* nil *GM_AUTO_LAST_CREATED_L* nil)
      (list rest_m rest_l))
    (progn
      (setq current_tab (getvar "CTAB") model_after (GM:AutoEntitySnapshot "Model"))
      (if (and *GM_AUTO_LAYOUT_TAB* (/= (getvar "CTAB") *GM_AUTO_LAYOUT_TAB*))
        (setvar "CTAB" *GM_AUTO_LAYOUT_TAB*)
      )
      (setq layout_after (GM:AutoEntitySnapshot "Layout"))
      (if (and current_tab (/= (getvar "CTAB") current_tab)) (setvar "CTAB" current_tab))
      (list (length (GM:AutoNewEntities model_before model_after))
            (length (GM:AutoNewEntities layout_before layout_after)))
    )
  )
)

;; OPT1/BZ-OPT1 (Fase 1): purga dirigida de definiciones temporales por codigo.
;; Solo toca nombres registrados en *GM_TEMP_BLOCKS* (TEMP_*/ESQ_*); nunca
;; PURGE ALL ni bloques de plantilla. Orden: ESQ_* antes que TEMP_*.
(defun GM:PurgeTempBlocks (/ doc blocks blooms del fail rest nm bobj res _app)
  (setq del 0 fail 0 rest nil)
  (if (and (boundp '*GM_TEMP_BLOCKS*) *GM_TEMP_BLOCKS*)
    (progn
      ;; AUD1: app con catch y liberada en el acto; el doc se libera tambien en
      ;; la rama de fallo de Blocks (antes fugaba en esa rama).
      (setq _app (vl-catch-all-apply 'vlax-get-acad-object '()))
      (if (vl-catch-all-error-p _app) (setq _app nil))
      (setq doc (if _app (vl-catch-all-apply 'vla-get-ActiveDocument (list _app)) nil))
      (GM:ReleaseObjectSafe _app)
      (setq _app nil)
      (if (or (not doc) (vl-catch-all-error-p doc))
        (progn (setq doc nil fail (length *GM_TEMP_BLOCKS*) rest (append *GM_TEMP_BLOCKS* nil)))
        (progn
          (setq blocks (vl-catch-all-apply 'vla-get-Blocks (list doc)))
          (if (vl-catch-all-error-p blocks)
            (progn
              (setq fail (length *GM_TEMP_BLOCKS*) rest (append *GM_TEMP_BLOCKS* nil))
              (GM:ReleaseObjectSafe doc)
              (setq doc nil))
            (progn
              (setq blooms (append
                             (vl-remove-if-not '(lambda (n) (wcmatch (strcase n) "ESQ_*")) *GM_TEMP_BLOCKS*)
                             (vl-remove-if-not '(lambda (n) (wcmatch (strcase n) "TEMP_*")) *GM_TEMP_BLOCKS*)))
              (foreach nm blooms
                (setq bobj (vl-catch-all-apply 'vla-Item (list blocks nm)))
                (if (vl-catch-all-error-p bobj)
                  (setq del (1+ del))
                  (progn
                    (setq res (vl-catch-all-apply 'vla-Delete (list bobj)))
                    (if (vl-catch-all-error-p res)
                      (setq fail (1+ fail) rest (cons nm rest))
                      (setq del (1+ del)))
                    (vl-catch-all-apply 'vlax-release-object (list bobj)))))
              (GM:ReleaseObjectSafe blocks)
              (GM:ReleaseObjectSafe doc)
              (setq blocks nil doc nil)))))
      (setq *GM_TEMP_BLOCKS* (if rest (reverse rest) nil))))
  (list del fail (if rest (reverse rest) nil))
)

;; OPT1/BZ-OPT1 (Fase 2): reseteo centralizado de estado transitorio por codigo.
;; Las preferencias persistentes (CSV, carpeta, MAKE_PDF, MAPA_ANS, SHOW_LOC)
;; quedan fuera a proposito.
(defun GM:ResetPerCodeState (/ )
  (setq *GM_AUTO_MODE* nil
        *GL_DATA_AFECTACIONES* nil
        *UBI_VP_DATA_MEM* nil
        *UBI_TARGET_REFERENCE* ""
        *UBI_TARGET_POLYGON* nil
        *GM_PLACED_BOXES* nil
        *GM_LABEL_EXCLUDE* nil
        *GM_LANDING_GAP* nil
        *GM_LABEL_SAMPLES* nil
        *GM_LABEL_DIST2* nil
        *GM_PROP_USED_GAP* nil
        *GM_ROAD_BOXES_CACHE* nil
        *GM_ROAD_BOXES_CACHE_READY* nil
        ;; Fase 1A (H-15): transitorias por codigo que quedaban con valor de la
        ;; corrida anterior (marco del mapa, predio localizado y listas de
        ;; entidades creadas). Se limpian aqui para no arrastrar estado.
        *GM_AUTO_MAPCONTENT_BOX* nil
        *GM_AUTO_PREDIO_LOC_RECT* nil
        *GM_AUTO_PREDIO_LOC_ENT_HANDLES* nil
        *GM_AUTO_CREATED_MODEL* nil
        *GM_AUTO_CREATED_LAYOUT* nil
        *GM_TEMP_BLOCKS* nil
        *GM_AUTO_CLEAN_FAILED* nil
        *GM_AUTO_SCALE_RESTORE_OK* T
        ;; OPT2 2026-09-26 (Fase 3/4): visibilidad modificada, marcador y lista
        ;; de creadas por codigo; el indice WCS de vias es de lote y NO se limpia.
        *UBI_VIS_MODIFIED* nil
        *GM_AUTO_MARK_LOTE* nil
        *GM_AUTO_LAST_CREATED_M* nil
        *GM_AUTO_LAST_CREATED_L* nil
        ;; F4 2026-10-05: transitorias de escala y del diccionario de viewports.
        ;; Si el Model fallaba antes de fijarlas, el codigo siguiente heredaba
        ;; la escala del anterior (grilla/formato con denominador cruzado).
        *GM_AUTO_SCALE_IDX* nil
        *GM_AUTO_SCALE_STR* nil
        *GM_AUTO_SCALE_VAL* nil
        *UBI_VP_DICT* nil)
  (if (boundp 'GM:ResetRoadCache) (GM:ResetRoadCache))
  (if (boundp 'GM:ResetTempBlocks) (GM:ResetTempBlocks))
  (gc)
  T
)

;; OPT1/BZ-OPT1 (Fase 3): nucleo de foto de capas (ON/OFF, congelada, bloqueada).
(defun GM:SnapshotLayerList (layers / entry result)
  (setq result nil)
  (foreach entry layers
    (if (setq entry (tblsearch "LAYER" entry))
      (setq result (cons (list (cdr (assoc 2 entry))
                               (cdr (assoc 62 entry))
                               (cdr (assoc 70 entry))) result))
    )
  )
  result
)
;; OPT1/BZ-OPT1 (Fase 3): foto por codigo de las capas que toca la limpieza.
;; F4 2026-10-05: se suman GRILLA-VPn y VISTA-n (mismas de la foto de lote);
;; sin ellas, un OFF/FREEZE/LOCK que dejaba la grilla por viewport fugaba al
;; codigo siguiente.
(defun GM:SnapshotLayerState (/ )
  (setq *GM_AUTO_LAYER_SNAPSHOT*
    (GM:SnapshotLayerList (list "0" "Membrete" "GRILLA"
      "GRILLA-VP1" "GRILLA-VP2" "GRILLA-VP3" "GRILLA-VP4" "GRILLA-VP5"
      "VISTA" "VISTA-1" "VISTA-2" "VISTA-3" "VISTA-4" "VISTA-5"
      "SUNARP" "COLINDANTE" "GORE")))
  *GM_AUTO_LAYER_SNAPSHOT*
)
;; OPT1/BZ-OPT1 (Fase 3): restaura ON/OFF y bloqueo desde una foto de capas.
(defun GM:RestoreLayerList (snap / item nm c62 flg cur frozen locked cmd_ok)
  (foreach item snap
    (setq nm (car item) c62 (cadr item) flg (caddr item))
    (if (and nm (tblsearch "LAYER" nm))
      (progn
        (setq cur (tblsearch "LAYER" nm))
        ;; c62 negativo = apagada; bit 1 de 70 = congelada; bit 4 = bloqueada.
        (setq frozen (if (and (numberp flg) (= (logand flg 1) 1)) T nil))
        (setq locked (if (and (numberp flg) (= (logand flg 4) 4)) T nil))
        (setq cmd_ok (vl-catch-all-apply 'vl-cmdf
                       (list "_.LAYER"
                             (if (and (numberp c62) (< c62 0)) "_OFF" "_ON") nm
                             (if frozen "_FREEZE" "_THAW") nm
                             (if locked "_LOCK" "_UNLOCK") nm "")))
        (if (vl-catch-all-error-p cmd_ok)
          (setq *GM_AUTO_CLEAN_FAILED* T)
        )
      )
    )
  )
  T
)
;; OPT1/BZ-OPT1 (Fase 3): restaura las capas de la foto por codigo.
(defun GM:RestoreLayerState (/ )
  (GM:RestoreLayerList *GM_AUTO_LAYER_SNAPSHOT*)
)
;; OPT1/BZ-OPT1 (Fase 3 completa): foto exacta del viewport plantilla.
;; Captura handle + CTAB + ActiveX (CustomScale, Center, Target, Direction,
;; Twist, DisplayLocked normalizado a T/nil) + DXF 12 (centro de vista) + LData DYN_VP_CENTER y
;; DYN_VP_SCALE_IDX. DYN_VP_RECT se excluye a proposito: es el rectangulo del
;; codigo en curso y su ciclo de vida lo gestiona GM:AutoCleanRun por fotos.
(defun GM:SnapshotViewportState (/ vp vobj snap g ed g12)
  (setq *GM_AUTO_VP_SNAPSHOT* nil)
  (setq vp (vl-catch-all-apply 'GM:FindLayoutViewport '()))
  (if (or (vl-catch-all-error-p vp) (not vp))
    nil
    (progn
      (setq ed (entget vp))
      (if (not ed)
        nil
        (progn
          (setq snap (list (cons "HANDLE" (cdr (assoc 5 ed)))
                           (cons "CTAB" (getvar "CTAB"))))
          (setq vobj (vl-catch-all-apply 'vlax-ename->vla-object (list vp)))
          (if (not (vl-catch-all-error-p vobj))
            (progn
              (setq g (vl-catch-all-apply 'vla-get-CustomScale (list vobj)))
              (if (not (vl-catch-all-error-p g)) (setq snap (append snap (list (cons "CUSTOMSCALE" g)))))
              (setq g (vl-catch-all-apply 'vla-get-Center (list vobj)))
              (if (not (vl-catch-all-error-p g)) (setq snap (append snap (list (cons "CENTER" g)))))
              (setq g (vl-catch-all-apply 'vla-get-Target (list vobj)))
              (if (not (vl-catch-all-error-p g)) (setq snap (append snap (list (cons "TARGET" g)))))
              (setq g (vl-catch-all-apply 'vla-get-Direction (list vobj)))
              (if (not (vl-catch-all-error-p g)) (setq snap (append snap (list (cons "DIRECTION" g)))))
              (setq g (vl-catch-all-apply 'vla-get-Twist (list vobj)))
              (if (not (vl-catch-all-error-p g)) (setq snap (append snap (list (cons "TWIST" g)))))
              ;; S: DisplayLocked es la propiedad real del PViewport (:vlax-false
              ;; vale 0 y en LISP es verdadero, por eso se normaliza a T/nil).
              (setq g (vl-catch-all-apply 'vla-get-DisplayLocked (list vobj)))
              (if (not (vl-catch-all-error-p g)) (setq snap (append snap (list (cons "LOCKED" (if (= g :vlax-true) T nil))))))
              (GM:ReleaseObjectSafe vobj)
            )
          )
          (setq g12 (assoc 12 ed))
          (if g12 (setq snap (append snap (list (cons "DXF12" (cdr g12))))))
          (setq g (vl-catch-all-apply 'vlax-ldata-get (list vp "DYN_VP_CENTER")))
          (if (not (vl-catch-all-error-p g)) (setq snap (append snap (list (cons "DYN_VP_CENTER" g)))))
          (setq g (vl-catch-all-apply 'vlax-ldata-get (list vp "DYN_VP_SCALE_IDX")))
          (if (not (vl-catch-all-error-p g)) (setq snap (append snap (list (cons "DYN_VP_SCALE_IDX" g)))))
          (setq *GM_AUTO_VP_SNAPSHOT* snap)
        )
      )
    )
  )
  *GM_AUTO_VP_SNAPSHOT*
)
;; OPT1/BZ-OPT1 (Fase 3 completa): repone CTAB, escala, centro de vista,
;; giro, bloqueo y LData del viewport. Cada propiedad fallida marca
;; *GM_AUTO_CLEAN_FAILED* sin abortar las restantes. Sin foto no hace nada.
(defun GM:RestoreViewportState (/ snap vp vobj ok val ed old12 new12 p)
  (setq snap *GM_AUTO_VP_SNAPSHOT* ok T)
  (if snap
    (progn
      (if (cdr (assoc "CTAB" snap))
        (vl-catch-all-apply 'setvar (list "CTAB" (cdr (assoc "CTAB" snap))))
      )
      (setq vp (vl-catch-all-apply 'GM:FindLayoutViewport '()))
      (if (or (vl-catch-all-error-p vp) (not vp))
        (setq ok nil)
        (progn
          (setq vobj (vl-catch-all-apply 'vlax-ename->vla-object (list vp)))
          (if (vl-catch-all-error-p vobj)
            (setq ok nil)
            (progn
              ;; Desbloquear para escribir; el bloqueo original se repone al final.
              (vl-catch-all-apply 'vla-put-DisplayLocked (list vobj :vlax-false))
              (setq val (assoc "CUSTOMSCALE" snap))
              (if val (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-CustomScale (list vobj (cdr val)))) (setq ok nil)))
              (setq val (assoc "TARGET" snap))
              (if val (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-Target (list vobj (cdr val)))) (setq ok nil)))
              (setq val (assoc "DIRECTION" snap))
              (if val (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-Direction (list vobj (cdr val)))) (setq ok nil)))
              (setq val (assoc "TWIST" snap))
              (if val (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-Twist (list vobj (cdr val)))) (setq ok nil)))
              (setq val (assoc "CENTER" snap))
              (if val (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-Center (list vobj (cdr val)))) (setq ok nil)))
              (vl-catch-all-apply 'vla-put-DisplayLocked (list vobj (if (cdr (assoc "LOCKED" snap)) :vlax-true :vlax-false)))
              (GM:ReleaseObjectSafe vobj)
            )
          )
          (setq ed (entget vp))
          (if (and ed (assoc "DXF12" snap) (assoc 12 ed))
            (progn
              (setq old12 (cdr (assoc "DXF12" snap)) new12 nil)
              (foreach p ed
                (setq new12 (append new12 (list (if (= (car p) 12) (cons 12 old12) p))))
              )
              (if (vl-catch-all-error-p (vl-catch-all-apply 'entmod (list new12))) (setq ok nil))
            )
          )
          (setq val (assoc "DYN_VP_CENTER" snap))
          (if val (if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-ldata-put (list vp "DYN_VP_CENTER" (cdr val)))) (setq ok nil)))
          (setq val (assoc "DYN_VP_SCALE_IDX" snap))
          (if val (if (vl-catch-all-error-p (vl-catch-all-apply 'vlax-ldata-put (list vp "DYN_VP_SCALE_IDX" (cdr val)))) (setq ok nil)))
        )
      )
    )
  )
  ;; P0-4 2026-09-29: vl-cmdf (capturable) en vez de command dentro de catch-all.
  (if (/= (getvar "CVPORT") 1) (vl-catch-all-apply 'vl-cmdf (list "_.pspace")))
  (if (and snap (not ok)) (setq *GM_AUTO_CLEAN_FAILED* T))
  T
)
;; OPT1/BZ-OPT1 (Fase 3 completa): foto de lote ANTES de purgar la grilla
;; estatica. Incluye las capas que la purga toca (GRILLA-VPn) y el viewport
;; exacto; al cierre del lote se restaura para que otro lote arranque intacto.
(defun GM:SnapshotBatchState (/ )
  (setq *GM_AUTO_BATCH_LAYER_SNAPSHOT*
    (GM:SnapshotLayerList (list "0" "Membrete" "GRILLA"
      "GRILLA-VP1" "GRILLA-VP2" "GRILLA-VP3" "GRILLA-VP4" "GRILLA-VP5"
      "VISTA" "VISTA-1" "VISTA-2" "VISTA-3" "VISTA-4" "VISTA-5"
      "SUNARP" "COLINDANTE" "GORE")))
  (GM:SnapshotViewportState)
  (setq *GM_AUTO_BATCH_VP_SNAPSHOT* *GM_AUTO_VP_SNAPSHOT*)
  T
)
;; OPT1/BZ-OPT1 (Fase 3 completa): devuelve la plantilla al estado previo al
;; lote (capas + viewport exacto), tambien con lote cancelado.
(defun GM:RestoreBatchState (/ )
  (GM:RestoreLayerList *GM_AUTO_BATCH_LAYER_SNAPSHOT*)
  (if *GM_AUTO_BATCH_VP_SNAPSHOT*
    (progn
      (setq *GM_AUTO_VP_SNAPSHOT* *GM_AUTO_BATCH_VP_SNAPSHOT*)
      (GM:RestoreViewportState)
    )
  )
  T
)

(defun GM:AutoCleanName (value / result pos ch allowed)
  (setq result "" allowed "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-"
        pos 1)
  (if (= (type value) 'STR)
    (while (<= pos (strlen value))
      (setq ch (substr value pos 1))
      (if (vl-string-search ch allowed) (setq result (strcat result ch)) (setq result (strcat result "_")))
      (setq pos (1+ pos))
    )
  )
  (if (= result "") "SIN_CODIGO" result)
)

(defun GM:AutoOutputPath (folder code)
  (strcat (vl-string-right-trim "\\/" folder) "\\" (GM:AutoCleanName code) ".dwg")
)

;; F3 2026-10-05: el veredicto del WBLOCK ya no es solo existencia. Un DWG de
;; 0 bytes o truncado por un AV pasaba como OK y entraba a PROGRESO. Se exige
;; tamano minimo (1 KB) y cabecera binaria de AutoCAD ("AC1").
(defun GM:AutoDwgValid (path / f h i c size)
  (setq size (vl-file-size path))
  (if (and size (>= size 1024) (setq f (open path "r")))
    (progn
      (setq h "" i 0)
      (while (and (< i 4) (setq c (read-char f)))
        (setq h (strcat h (chr c)) i (1+ i))
      )
      (close f)
      (= (substr h 1 3) "AC1")
    )
    nil
  )
)

(defun GM:AutoPdfOutputPath (folder code)
  (strcat (vl-string-right-trim "\\/" folder) "\\" (GM:AutoCleanName code) " plano preliminar.pdf")
)

;; FASE1 2026-10-21: el veredicto del PDF deja de ser "existe". Un PDF de 0
;; bytes o truncado por un plot fallido pasaba como OK y el cliente recibia un
;; archivo roto (0 bytes o truncado) como si fuera el plano definitivo. Se
;; exige cabecera "%PDF" y tamano minimo (1 KB), igual que hace el DWG con
;; "AC1". Referencia: los PDF reales del lote (AutoCAD 2021) salen como
;; "%PDF-1.7" con 0,9 MB o mas.
(defun GM:AutoPdfValid (path / f h i c size)
  (setq size (vl-file-size path))
  (if (and size (>= size 1024) (setq f (open path "r")))
    (progn
      (setq h "" i 0)
      (while (and (< i 4) (setq c (read-char f)))
        (setq h (strcat h (chr c)) i (1+ i))
      )
      (close f)
      (= (substr h 1 4) "%PDF")
    )
    nil
  )
)