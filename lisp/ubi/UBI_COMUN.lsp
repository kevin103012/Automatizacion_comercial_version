;; ============================================================================
;; UBI_COMUN.lsp - Sistema GM Ingenieros (modulo ubi)
;; Movido SIN CAMBIOS desde PLANO_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================
;;; ==============================================================================
;;;  SUITE MAESTRA: PLANO DE UBICACI\\U+00D3N (V9.8 - AUTO-UPDATE CUADRO ESCALA + FIX TEXTOS)
;;;  (Integraci\\U+00F3n VP Din\\U+00E1mico + Grilla Avanzada + Motor de Textos + Params Completos)
;;; ==============================================================================

(vl-load-com)

;; El sello se publica al terminar la carga, no antes de definir las funciones.
(setq *GM_VER_PLANO* nil)

;; --- VARIABLES GLOBALES DE MEMORIA ---
(setq *UBI_LAYER_LIST* '("AREA AFECTADA (PERIMETRO)" "LIMITE DE AFECTACI\\U+00D3N" "PREDIO MATRIZ" "AREA AFECTADA (AREA)" "EJE DE VIA" "RIOS O QUEBRADAS" "PREDIOS COLINDANTES" "VIA EXISTENTE" "VIVIENDA"))

;; *UBI_FACTOR* es diagnostico de sesion (factor 1/CustomScale); hoy ningun flujo
;; automatico lo lee, pero se conserva para inspeccion manual en consola.
(if (not *UBI_FACTOR*) (setq *UBI_FACTOR* "1.0"))
(if (not *UBI_VP_DATA_MEM*) (setq *UBI_VP_DATA_MEM* '()))
(if (not *UBI_SAFE_LAYERS*) (setq *UBI_SAFE_LAYERS* '()))
(if (not *UBI_VP_TYPE*) (setq *UBI_VP_TYPE* "1")) 
;; BR 2026-09-21: selector de tabla normativa por tipo de plano (variable global).
;; Valores: "UBICACION" (default/lote), "DIAGNOSTICO", "PERIMETRICO", "MATRIZ".
;; Uso: (setq *GM_PLANO_TIPO* "DIAGNOSTICO") antes del lote. Fuente: ESCALAS.xlsx (solo referencia).
(if (not *GM_PLANO_TIPO*) (setq *GM_PLANO_TIPO* "UBICACION")) 
(if (not *UBI_ACTIVE_VP*) (setq *UBI_ACTIVE_VP* 1)) 
(if (not *UBI_VP_DICT*) (setq *UBI_VP_DICT* '())) 
(if (not *UBI_TARGET_CODE*) (setq *UBI_TARGET_CODE* "")) 
(if (not *UBI_TARGET_REFERENCE*) (setq *UBI_TARGET_REFERENCE* ""))
(if (not *UBI_TARGET_POLYGON*) (setq *UBI_TARGET_POLYGON* nil))
(if (not *GM_AUTO_MODE*) (setq *GM_AUTO_MODE* nil))

;; Fase B 2026-10-12: GM:ReleaseObjectSafe vive en LAYOUT/AUTO; si PLANO se
;; cargara sin esos modulos, este fallback evita "no function definition" al
;; liberar wrappers COM (misma liberacion; LAYOUT/AUTO redefinen al cargar).
(if (not (boundp 'GM:ReleaseObjectSafe))
  (defun GM:ReleaseObjectSafe (obj)
    (if (and obj (not (vl-catch-all-error-p obj)))
      (vl-catch-all-apply 'vlax-release-object (list obj)))
    nil
  )
)

;; --- NUEVOS PARAMETROS GLOBALES (ESCALADO DE TEXTOS Y ESTACAS) ---
(if (not *UBI_PROG_OFFSET_MULT*) (setq *UBI_PROG_OFFSET_MULT* "1.5")) 
(if (not *UBI_STAKE_SCALE_FACTOR*) (setq *UBI_STAKE_SCALE_FACTOR* "1.0")) 
(if (not *UBI_TEXT_SCALE_MULT*) (setq *UBI_TEXT_SCALE_MULT* "1.0")) 

;; OPT5 2026-09-27 (F0): instrumentacion fina del formato del Model. Solo mide:
;; sub-tiempos por tramo (MAPA/TEXTOS/ORDEN/CIERRE) y conteos (escaneadas, conHL,
;; aplicadas, yaColor, hlRead). No cambia ninguna propiedad ni el orden de
;; escritura; el resumen lo publica AUTOMATIZACION con GM:FmtStatsReport.
(if (not (boundp '*GM_FMT_STATS*)) (setq *GM_FMT_STATS* nil))
(if (not (boundp '*GM_FMT_HL_MS*)) (setq *GM_FMT_HL_MS* 0))
(if (not (boundp '*GM_FMT_HL_N*)) (setq *GM_FMT_HL_N* 0))
(defun GM:FmtStat (key value)
  (setq *GM_FMT_STATS* (cons (cons key value) (vl-remove (assoc key *GM_FMT_STATS*) *GM_FMT_STATS*)))
  value
)

;; P0-6 2026-09-29: medicion de texto memoizada (estilo|altura|texto) para el
;; membrete/pie y las etiquetas. textbox era una de las llamadas mas repetidas
;; por codigo (labels y "--" se repiten); la cache es pura (misma entrada,
;; misma salida) y se acota al lote. Kill switch no necesario: no dibuja ni
;; modifica nada; si textbox falla, devuelve nil y el llamador conserva su
;; comportamiento previo (fallback/medicion directa).
(if (not (boundp '*GM_TEXT_MEASURE_CACHE*)) (setq *GM_TEXT_MEASURE_CACHE* nil))
(defun GM:TextMeasure (text height style / key hit box result)
  (setq key (strcat (if style style "ROMANS") "|" (rtos (if (numberp height) height 1.5) 2 4) "|"
                    (if (= (type text) 'STR) text ""))
        hit (assoc key *GM_TEXT_MEASURE_CACHE*))
  (if hit
    (cdr hit)
    (progn
      (setq box (vl-catch-all-apply 'textbox
                  (list (list '(0 . "TEXT") (cons 1 (if (= (type text) 'STR) text ""))
                              (cons 40 (if (numberp height) height 1.5))
                              (cons 7 (if style style "ROMANS"))))))
      (setq result (if (and (not (vl-catch-all-error-p box)) box
                            (numberp (car (car box))) (numberp (car (cadr box))))
                     (- (car (cadr box)) (car (car box)))
                     nil))
      (setq *GM_TEXT_MEASURE_CACHE* (cons (cons key result) *GM_TEXT_MEASURE_CACHE*))
      ;; F15-R2 2026-09-29: cota de memoria (la cache vivia todo el lote sin
      ;; limite; se descarta la entrada mas vieja al pasar 2000 claves).
      (if (> (length *GM_TEXT_MEASURE_CACHE*) 2000)
        (setq *GM_TEXT_MEASURE_CACHE* (vl-remove (last *GM_TEXT_MEASURE_CACHE*) *GM_TEXT_MEASURE_CACHE*)))
      result)))


;; OPT7 2026-09-27 (F3): escritura diferencial (leer antes de escribir; saltar lo
;; que ya esta igual). Kill switch *GM_FMT_DIFF_WRITE* (default T; nil = escribir
;; siempre, prueba A/B) y contadores de propiedades de texto/estaca saltadas.
(if (not (boundp '*GM_FMT_DIFF_WRITE*)) (setq *GM_FMT_DIFF_WRITE* T))
(if (not (boundp '*GM_FMT_TX_SKIP*)) (setq *GM_FMT_TX_SKIP* 0))
(if (not (boundp '*GM_FMT_TX_WRITES*)) (setq *GM_FMT_TX_WRITES* 0))
(if (not (boundp '*GM_FMT_STK_SKIP*)) (setq *GM_FMT_STK_SKIP* 0))
;; T2.1 2026-09-28: lecturas comparativas por DXF en UBI_FmtApplyEnt (una sola
;; lectura barata por entidad en vez de ~7 lecturas COM); kill switch para A/B.
;; Ante cualquier duda (entget falla, grupo ausente) se escribe, nunca se salta.
(if (not (boundp '*GM_FMT_DXF_READS*)) (setq *GM_FMT_DXF_READS* T))

;; OPT9 2026-09-27 (correccion): sello por escala APAGADO POR DEFECTO.
;; Por que: la clave del sello (escala+tipo+multiplicadores) no incluye el
;; codigo objetivo, y la clasificacion del formato SI depende de el (una misma
;; polilinea -AF- es "Predios" para otro codigo y "Afectada" para el propio).
;; Selladas las originales en el codigo N, los codigos posteriores con la misma
;; clave heredaban clasificacion/colores y apilado del codigo N: desfase y areas
;; con color cruzado (reporte OPT8, codigos 3-5 con aplicadas=0/escritas=0).
;; El mecanismo queda como opt-in NO SOPORTADO hasta redisenarlo con particion
;; por rol (solo entidades independientes del codigo) y validacion visual por
;; codigo. Para reactivarlo en pruebas: (setq *GM_FMT_SEAL* T) antes del lote.
(if (not (boundp '*GM_FMT_SEAL*)) (setq *GM_FMT_SEAL* nil))
(if (not (boundp '*UBI_INDEX_SEAL_KEY*)) (setq *UBI_INDEX_SEAL_KEY* nil))
;; T2.3 2026-09-28: sello por elemento (lista alineada al indice del lote,
;; (ename . clave); se publica al final de cada UBI_ApplyAutoFormat con indice
;; listo y se limpia con UBI_ResetEntIndex al cerrar el lote).
(if (not (boundp '*GM_FMT_SEALS*)) (setq *GM_FMT_SEALS* nil))
;; OPT8: sub-indices por tipo para UBI_ProcessHyperlinks (solo entidades con
;; hipervinculo): textos (ename hl) y lineas (ename hl tipo).
(if (not (boundp '*UBI_HLTEXT_IDX*)) (setq *UBI_HLTEXT_IDX* nil))
(if (not (boundp '*UBI_HLLINE_IDX*)) (setq *UBI_HLLINE_IDX* nil))

;; OPT6 2026-09-27 (F2): indice estatico del Model por lote. Hipervinculo, capa
;; y tipo de las entidades originales no cambian durante el lote; se capturan una
;; vez (tras la purga de grilla) y los consumidores (visibilidad, formato,
;; textos, busqueda de poligono) evitan el barrido COM por codigo. Las entidades
;; creadas por codigo no entran al indice. Sin READY, cada consumidor cae a su
;; barrido original (flujo manual intacto).
(if (not (boundp '*UBI_ENT_INDEX*)) (setq *UBI_ENT_INDEX* nil))
(if (not (boundp '*UBI_ENT_INDEX_READY*)) (setq *UBI_ENT_INDEX_READY* nil))
(if (not (boundp '*UBI_POLY_INDEX*)) (setq *UBI_POLY_INDEX* nil))
(defun UBI_ResetEntIndex ()
  (setq *UBI_ENT_INDEX* nil *UBI_POLY_INDEX* nil *UBI_ENT_INDEX_READY* nil
        *UBI_HLTEXT_IDX* nil *UBI_HLLINE_IDX* nil *UBI_INDEX_SEAL_KEY* nil
        ;; T2.2-FIX1 2026-09-28: mascaras de visibilidad alineadas al indice.
        *UBI_VIS_BASE* nil *UBI_VIS_PREV* nil *UBI_VIS_PREV_READY* nil
        ;; T2.3 2026-09-28: sello por elemento alineado al indice del lote.
        *GM_FMT_SEALS* nil)
  T
)
;; T2.2-FIX1 2026-09-28: visibilidad diferencial por mascaras alineadas al
;; indice del lote. *UBI_VIS_BASE* guarda la visibilidad original (T=visible)
;; capturada al construir el indice; *UBI_VIS_PREV* la que dejo aplicada el
;; codigo anterior y *UBI_VIS_PREV_READY* su validez. El delta compara
;; posiciones en Lisp puro (nunca ordena ni compara ENAME: el intento T2.2 con
;; vl-sort y < producia "bad argument type for compare" en Civil 3D) y la
;; restauracion final es una sola pasada contra la base al cerrar el lote.
(if (not (boundp '*UBI_VIS_BASE*)) (setq *UBI_VIS_BASE* nil))
(if (not (boundp '*UBI_VIS_PREV*)) (setq *UBI_VIS_PREV* nil))
(if (not (boundp '*UBI_VIS_PREV_READY*)) (setq *UBI_VIS_PREV_READY* nil))
;; Visibilidad real por DXF: 60=1 oculto; sin 60 (o 60=0) visible.
(defun UBI_EntityHiddenP (ent / ed d60)
  (setq ed (entget ent))
  (if (and ed (setq d60 (assoc 60 ed)) (= (cdr d60) 1)) T nil)
)
;; Reconstruye la mascara previa desde el estado real del dibujo (fallo a mitad
;; de un codigo o reanudacion): una lectura DXF por entidad del indice.
(defun UBI_VisResyncPrev (/ prev item)
  (setq prev nil)
  (foreach item *UBI_ENT_INDEX*
    (setq prev (cons (if (UBI_EntityHiddenP (car item)) nil T) prev))
  )
  (setq *UBI_VIS_PREV* (reverse prev) *UBI_VIS_PREV_READY* T)
  T
)
;; Devuelve la visibilidad original capturada al indexar (una pasada al cierre
;; del lote o al cancelar). Sin indice/base cae al restaurador legado.
(defun UBI_RestoreVisibilityBase (/ ix bx item ent ed d60 want cur cnt)
  (if (and *UBI_ENT_INDEX* *UBI_VIS_BASE* (= (length *UBI_ENT_INDEX*) (length *UBI_VIS_BASE*)))
    (progn
      (setq cnt 0 ix *UBI_ENT_INDEX* bx *UBI_VIS_BASE*)
      (while (and ix bx)
        (setq item (car ix) ent (car item) want (if (car bx) T nil) ed (if ent (entget ent) nil))
        (if ed
          (progn
            (setq d60 (assoc 60 ed) cur (if (and d60 (= (cdr d60) 1)) nil T))
            (if (not (equal (if want T nil) (if cur T nil)))
              (progn
                (UBI_SetEntityVisible ent want)
                (if want
                  (if (= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1) (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 0))
                  (if (/= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1) (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 1)))
                (setq cnt (1+ cnt))
              )
            )
          )
        )
        (setq ix (cdr ix) bx (cdr bx))
      )
      (setq *UBI_VIS_PREV* *UBI_VIS_BASE* *UBI_VIS_PREV_READY* T *UBI_VIS_MODIFIED* nil)
      cnt
    )
    (UBI_RestoreHiddenByGM)
  )
)
;; Entrada del indice: (ename hyperlink capa tipo vis_original). El 5to campo
;; (T2.2-FIX1) es la visibilidad original por DXF (T=visible) y alimenta la
;; mascara base de restauracion; los consumidores previos usan car..cadddr.
;; *UBI_POLY_INDEX* guarda (hyperlink ename) de LWPOLYLINE/POLYLINE con
;; hipervinculo, en orden de base.
;; OPT8: *UBI_HLTEXT_IDX* = (ename hl) de TEXT/MTEXT con hl y
;; *UBI_HLLINE_IDX* = (ename hl tipo) de LINE/LWPOLYLINE con hl.
(defun UBI_BatchIndexEnts (/ doc ms vla_obj ent hl lay typ poly htext hline n ent_vis vis_base _app)
  (UBI_ResetEntIndex)
  ;; AUD1: la app COM se libera en el acto (antes anonima: fugaba 1 por lote).
  (setq n 0 poly nil htext nil hline nil vis_base nil
        _app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (vl-catch-all-error-p _app) (setq _app nil))
  (setq doc (if _app (vl-catch-all-apply 'vla-get-ActiveDocument (list _app)) nil))
  (GM:ReleaseObjectSafe _app)
  (setq _app nil ms nil)
  (if (and doc (not (vl-catch-all-error-p doc)))
    (setq ms (vl-catch-all-apply 'vla-get-ModelSpace (list doc)))
    (setq doc nil)
  )
  (if (and ms (not (vl-catch-all-error-p ms)))
    (progn
      (vlax-for vla_obj ms
        (setq ent (vlax-vla-object->ename vla_obj)
              hl (UBI_GetHyperlinkString vla_obj)
              lay (vla-get-Layer vla_obj)
              typ (vla-get-ObjectName vla_obj))
        (if (not hl) (setq hl ""))
        ;; T2.2-FIX1: la visibilidad original entra al indice (mascara base).
        (setq ent_vis (if (UBI_EntityHiddenP ent) nil T))
        (setq *UBI_ENT_INDEX* (cons (list ent hl lay typ ent_vis) *UBI_ENT_INDEX*)
              vis_base (cons ent_vis vis_base) n (1+ n))
        (if (/= hl "")
          (progn
            (if (or (= typ "AcDbPolyline") (= typ "AcDb2dPolyline") (= typ "AcDb3dPolyline"))
              (setq poly (cons (list hl ent) poly)))
            (if (or (= typ "AcDbText") (= typ "AcDbMText"))
              (setq htext (cons (list ent hl) htext)))
            (if (or (= typ "AcDbLine") (= typ "AcDbPolyline"))
              (setq hline (cons (list ent hl typ) hline)))
          )
        )
      )
      (setq vis_base (reverse vis_base)
            *UBI_ENT_INDEX* (reverse *UBI_ENT_INDEX*) *UBI_POLY_INDEX* (reverse poly)
            *UBI_HLTEXT_IDX* (reverse htext) *UBI_HLLINE_IDX* (reverse hline)
            *UBI_VIS_BASE* vis_base *UBI_VIS_PREV* vis_base *UBI_VIS_PREV_READY* T
            *UBI_ENT_INDEX_READY* T)
      (vl-catch-all-apply 'vlax-release-object (list ms))
    )
    (setq *UBI_ENT_INDEX_READY* nil)
  )
  (if doc (vl-catch-all-apply 'vlax-release-object (list doc)))
  n
)
(defun UBI_IndexFindPolyByCode (code_up / hit item)
  (setq hit nil)
  (foreach item *UBI_POLY_INDEX*
    (if (not hit)
      (if (= (vl-string-trim " \t\n\r" (strcase (car item))) code_up) (setq hit (cadr item)))
    )
  )
  hit
)
(defun UBI_IndexFindPolyByRef (target / tkey hit item)
  (setq tkey (UBI_NormalizeSearchKey target) hit nil)
  (foreach item *UBI_POLY_INDEX*
    (if (not hit)
      (if (= tkey (UBI_NormalizeSearchKey (car item))) (setq hit (cadr item)))
    )
  )
  hit
)

;; GLOBALES DE LA GRILLA NATIVA
(if (not *UBI_sg_intervalo*) (setq *UBI_sg_intervalo* "50.0"))
(if (not *UBI_sg_altura*) (setq *UBI_sg_altura* "2.5"))
(if (not *UBI_sg_gap*) (setq *UBI_sg_gap* "1.0"))
(if (not *UBI_sg_chk_izq*) (setq *UBI_sg_chk_izq* "1"))
(if (not *UBI_sg_chk_der*) (setq *UBI_sg_chk_der* "0"))
(if (not *UBI_sg_chk_sup*) (setq *UBI_sg_chk_sup* "0"))
(if (not *UBI_sg_chk_inf*) (setq *UBI_sg_chk_inf* "1"))
(if (not *UBI_sg_col_txt*) (setq *UBI_sg_col_txt* 7)) 
(if (not *UBI_sg_col_grid*) (setq *UBI_sg_col_grid* 8))
(if (not *UBI_sg_idx_style*) (setq *UBI_sg_idx_style* "0"))


;; --- LIMPIEZA DE MEMORIA DE SESION ---
;; POR QUE no toca ss/tmp_ss/ss_vp: eran nombres genericos que podian pisar
;; globales del llamador por alcance dinamico. Solo limpia listas propias.
(defun UBI_Nuke-Memory (/ lst_prog lst_estacas lst_peuc lst_afectada lst_matriz
                          lst_derecho lst_eje lst_achurado lst_rios lst_via
                          lst_predios lst_vivienda lst_grid_txt lst_grid_lin)
  (setq lst_prog nil lst_estacas nil lst_peuc nil
        lst_afectada nil lst_matriz nil lst_derecho nil lst_eje nil
        lst_achurado nil lst_rios nil lst_via nil lst_predios nil
        lst_vivienda nil lst_grid_txt nil lst_grid_lin nil
        *UBI_DRAWORDER_SS* nil)
  (gc) (gc) (gc)
)

;; --- FUNCION BYPASS DRAWORDER ---
(defun UBI_ApplyDrawOrder (lst order / tmp_ss e)
  (if (and lst (> (length lst) 0))
    (progn
      (setq tmp_ss (ssadd))
      (foreach e lst (ssadd e tmp_ss))
      (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" tmp_ss "" order))
      (setq tmp_ss nil) (gc)
    )
  )
)

;; --- MOTOR DE VISTA PREVIA (CUADRO DE ESQUEMAS) ---
;; POR QUE duplicado: LAYOUT trae otro PREVIEW_SCHEMA que en modo AUTO registra
;; omision en vez de nil. Con el orden oficial gana LAYOUT; se conservan ambos
;; para flujo manual por modulo. Solo se usa en interactivo, nunca en el lote.
(defun PREVIEW_SCHEMA (w h h_caj msg / gr code pt_curr loop p1 p2 p3 p4 p5 p6 res)
  (if *GM_AUTO_MODE* nil
    (progn
      (princ (strcat "\n" msg)) (setq loop T res nil)
  (while loop
    (setq gr (grread t 15 0) code (car gr) pt_curr (cadr gr))
    (cond
      ((= code 5)
        (redraw)
        (setq p1 (list (- (car pt_curr) (/ w 2.0)) (- (cadr pt_curr) (/ h 2.0)))
              p2 (list (+ (car pt_curr) (/ w 2.0)) (- (cadr pt_curr) (/ h 2.0)))
              p3 (list (+ (car pt_curr) (/ w 2.0)) (+ (cadr pt_curr) (/ h 2.0)))
              p4 (list (- (car pt_curr) (/ w 2.0)) (+ (cadr pt_curr) (/ h 2.0)))
              p5 (list (+ (car pt_curr) (/ w 2.0)) (- (- (cadr pt_curr) (/ h 2.0)) h_caj))
              p6 (list (- (car pt_curr) (/ w 2.0)) (- (- (cadr pt_curr) (/ h 2.0)) h_caj)))
        (grdraw p1 p2 6) (grdraw p2 p3 6) (grdraw p3 p4 6) (grdraw p4 p1 6)
        (grdraw p1 p6 6) (grdraw p6 p5 6) (grdraw p5 p2 6)
        (grdraw p1 p3 6) (grdraw p2 p4 6)
      )
      ((= code 3) (redraw) (setq res pt_curr loop nil))
      ((or (= code 2) (= code 25)) (redraw) (setq res nil loop nil))
    )
  )
      res
    )
  )
)

;; --- MOTOR DE VISTA PREVIA (CUADRO ESCALA) ---
(defun UBI_PreviewCuadro (w h msg / pt loop gr pt1 pt2 pt3 pt4 ret old_cmd)
  (setq old_cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ msg)
  (setq loop T)
  (while loop
    (setq gr (grread t 13 0))
    (cond
      ((= (car gr) 5) 
        (redraw) 
        (setq pt (cadr gr))
        (setq pt1 pt
              pt2 (list (+ (car pt) w) (cadr pt) 0.0)
              pt3 (list (+ (car pt) w) (+ (cadr pt) h) 0.0)
              pt4 (list (car pt) (+ (cadr pt) h) 0.0))
        (grvecs (list 3 pt1 pt2 3 pt2 pt3 3 pt3 pt4 3 pt4 pt1))
      )
      ((= (car gr) 3) (redraw) (setq ret (cadr gr) loop nil))
      ((or (= (car gr) 2) (= (car gr) 11) (= (car gr) 25)) (redraw) (setq ret nil loop nil))
    )
  )
  (setvar "CMDECHO" old_cmd)
  ret
)

(defun UBI_InsertarCuadroEscala (vp_ent scale_str / p1 p2 p3 p4 pCentro rec_ent hat_ent vla_hat colObj ptTitul ptEscal titulo text_esc_ent)
  (setq p1 (UBI_PreviewCuadro 31.26 6.31 (strcat "\nMueva cursor para ubicar Cuadro de Escala 1/" scale_str " y haga CLIC: ")))
  (if p1
    (progn
      (if (not (tblsearch "LAYER" "Membrete")) (command "_.-layer" "_new" "Membrete" "_color" "7" "Membrete" ""))
      (if (not (tblsearch "STYLE" "WMF-Arial0")) (command "_-STYLE" "WMF-Arial0" "Arial" "0" "1" "0" "N" "N"))
      (if (not (tblsearch "STYLE" "Memoir")) (command "_-STYLE" "Memoir" "Arial" "0" "1" "0" "N" "N"))
      
      (setq p2 (list (+ (car p1) 31.26) (cadr p1) 0.0) 
            p3 (list (+ (car p1) 31.26) (+ (cadr p1) 6.31) 0.0) 
            p4 (list (car p1) (+ (cadr p1) 6.31) 0.0) 
            pCentro (list (+ (car p1) 15.63) (+ (cadr p1) 3.155) 0.0))
      
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") 
                     (cons 8 "Membrete") '(90 . 4) '(70 . 1) (cons 43 0.2) (cons 62 7) 
                     (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p2)) 
                     (list 10 (car p3) (cadr p3)) (list 10 (car p4) (cadr p4))))
      (setq rec_ent (entlast)) 
      
      (command "_.-HATCH" "_P" "SOLID" "_S" rec_ent "" "")
      (setq hat_ent (entlast))
      (if hat_ent
        (progn
          ;; F15-R1: vla_hat/colObj con catch y liberados (flujo manual).
          (setq vla_hat (vl-catch-all-apply 'vlax-ename->vla-object (list hat_ent)))
          (if (not (vl-catch-all-error-p vla_hat))
            (progn
              (setq colObj (vl-catch-all-apply 'vla-get-TrueColor (list vla_hat)))
              (if (not (vl-catch-all-error-p colObj))
                (progn
                  (vl-catch-all-apply 'vla-SetRGB (list colObj 255 255 255))
                  (vl-catch-all-apply 'vla-put-TrueColor (list vla_hat colObj))))
              (vl-catch-all-apply 'vla-put-Layer (list vla_hat "Membrete"))
              (GM:ReleaseObjectSafe colObj)
              (GM:ReleaseObjectSafe vla_hat)))
        )
      )
      
      ;; P0-4 2026-09-29: vl-cmdf (capturable) en vez de command para el apilado.
      (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" rec_ent "" "_F"))
      
      (setq titulo (if (= *UBI_VP_TYPE* "1") "PLANO UBICACI\\U+00D3N" "PLANO DE UBICACI\\U+00D3N"))
      (setq ptTitul (list (car pCentro) (+ (cadr pCentro) 1.25) 0.0))
      (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") (cons 8 "Membrete") (cons 62 1) 
                     '(100 . "AcDbText") (cons 10 ptTitul) (cons 40 2.0) 
                     (cons 1 titulo) (cons 7 "WMF-Arial0") 
                     '(72 . 1) (cons 11 ptTitul) '(73 . 2)))
      
      (setq ptEscal (list (car pCentro) (- (cadr pCentro) 1.45) 0.0))
      (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") (cons 8 "Membrete") (cons 62 7) 
                     '(100 . "AcDbText") (cons 10 ptEscal) (cons 40 1.25) 
                     (cons 1 (strcat "ESCALA: 1/" scale_str)) (cons 7 "Memoir") 
                     '(72 . 1) (cons 11 ptEscal) '(73 . 2)))
      
      ;; --- GUARDAMOS EL HANDLE DEL TEXTO DE LA ESCALA EN EL VIEWPORT ---
      (setq text_esc_ent (entlast))
      ;; P0-2 2026-09-29: entget nil-safe antes de guardar el handle del cuadro.
      (if (and vp_ent text_esc_ent (entget text_esc_ent))
        (vlax-ldata-put vp_ent "DYN_VP_CUADRO_TXT" (cdr (assoc 5 (entget text_esc_ent)))))
      
      (princ "\n\\U+00BFCuadro generado y enlazado exitosamente!")
    )
    (princ "\nSe cancel\\U+00F3 la inserci\\U+00F3n del cuadro.")
  )
)