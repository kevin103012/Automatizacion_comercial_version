;; ============================================================================
;; UBI_FORMATO.lsp - Sistema GM Ingenieros (modulo ubi)
;; Movido SIN CAMBIOS desde PLANO_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================


;; ==============================================================================
;; MOTOR FORMATO MAGICO Y DRAWORDER
;; ==============================================================================
;; OPT7 2026-09-27 (F3): comparacion con epsilon relativo para no reescribir
;; floats que ya estan en el valor destino (0.2 vs 0.2, 0.125 vs 0.125...).
(defun UBI_FmtNear (a b)
  (if (and (numberp a) (numberp b))
    (or (= a b) (<= (abs (- a b)) (* 0.000001 (max 1.0 (abs a) (abs b)))))
    nil
  )
)
;; OPT10 2026-09-27 (F4): normaliza booleanos COM a 1/0. :vlax-false vale 0 y en
;; AutoLISP 0 no es nil, asi que (if cur T nil) devolvia T para falso y forzaba
;; reescritura + vla-Update de miles de polilineas en cada codigo. Se compara el
;; valor normalizado de ambos lados; cualquier lectura rara se trata como
;; "distinto" (rama de escritura, nunca salto a ciegas).
(defun UBI_FmtBoolNorm (val)
  (cond
    ((not val) 0)
    ((numberp val) (if (= val 0) 0 1))
    ((eq val T) 1)
    ((equal val :vlax-false) 0)
    ((equal val :vlax-true) 1)
    (T 1)
  )
)
;; OPT8: sello activo cuando la clave del lote coincide con la pedida (misma
;; escala+tipo+multiplicadores); el formato de las originales ya esta aplicado.
(defun UBI_FmtSealActive (seal_key)
  (if (and *GM_FMT_SEAL* *UBI_INDEX_SEAL_KEY* (equal *UBI_INDEX_SEAL_KEY* seal_key)) T nil)
)
;; OPT7 2026-09-27 (F3): aplica el estilo de una entidad clasificada leyendo cada
;; propiedad antes de escribir; escribe solo lo que cambio y omite vla-Update si
;; no hubo escrituras. Toda lectura fallida se trata como "distinto" (nunca se
;; salta a ciegas). Con *GM_FMT_DIFF_WRITE* nil escribe siempre (A/B).
;; Devuelve (color linetype lts lineweight width hatch truecolor update writes
;; linetypegen-skip linetypegen-writes): OPT10 separa los contadores de
;; LinetypeGeneration de los de ConstantWidth para medir el arreglo booleano.
(defun UBI_FmtApplyEnt (vla_obj objType elem scl_str scl_val / act_color act_ltype act_lts act_lweight gw_mult lt_gen true_col true_col2 act_rgb h_val h_pat fmt cur cur_tc cur_r cur_g cur_b s_col s_lt s_lts s_lw s_w s_h s_tc s_u nw w_val s_ltgen w_ltgen ename ed)
  (setq s_col 0 s_lt 0 s_lts 0 s_lw 0 s_w 0 s_h 0 s_tc 0 s_u 0 nw 0 s_ltgen 0 w_ltgen 0)
  (setq act_color 256 act_ltype "Continuous" act_lts 1.0 act_lweight -3 gw_mult 0.0 lt_gen :vlax-false fmt (UBI_GetUbicacionStyle scl_str elem))
  ;; T2.1 2026-09-28: estado actual por DXF en UNA lectura barata por entidad;
  ;; si la conversion/entget falla se cae a la lectura COM de siempre. Las
  ;; comparaciones conservan la regla "distinto => escribir" (nunca se salta a
  ;; ciegas) y el kill switch *GM_FMT_DXF_READS* permite volver al camino COM.
  (setq ed nil)
  (if (and *GM_FMT_DXF_READS* vla_obj)
    (progn
      (setq ename (vl-catch-all-apply 'vlax-vla-object->ename (list vla_obj)))
      (if (and ename (not (vl-catch-all-error-p ename)) (setq ed (entget ename)))
        nil
        (setq ed nil))))
  (cond
    ((= elem "Derecho") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 230) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "Continuous") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 0.2) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw 40) gw_mult (/ (UBI_GetUbicacionStyleVal fmt 'gw (* scl_val 0.0005)) scl_val) lt_gen (if (UBI_GetUbicacionStyleVal fmt 'ltgen nil) :vlax-true :vlax-false)))
    ((= elem "Eje") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 24) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "DASHDOT") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 0.2) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw 40) gw_mult (/ (UBI_GetUbicacionStyleVal fmt 'gw (* scl_val 0.00025)) scl_val) lt_gen (if (UBI_GetUbicacionStyleVal fmt 'ltgen T) :vlax-true :vlax-false)))
    ((= elem "Predios") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 64) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "Continuous") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 1.0) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw -3) gw_mult (/ (UBI_GetUbicacionStyleVal fmt 'gw (* scl_val 0.0002)) scl_val) lt_gen (if (UBI_GetUbicacionStyleVal fmt 'ltgen nil) :vlax-true :vlax-false))) ;; BP: color estricto ACI 64, sin TrueColor oliva
    ((= elem "Afectada") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 1) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "DASHED2") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 5.0) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw 50) gw_mult (/ (UBI_GetUbicacionStyleVal fmt 'gw (* scl_val 0.0002)) scl_val) lt_gen (if (UBI_GetUbicacionStyleVal fmt 'ltgen T) :vlax-true :vlax-false)))
    ((= elem "Achurado") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 71) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "Continuous") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 1.0) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw 50)))
    ((= elem "Matriz") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 5) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "Continuous") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 1.0) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw 50) gw_mult (/ (UBI_GetUbicacionStyleVal fmt 'gw (* scl_val 0.00032)) scl_val) lt_gen (if (UBI_GetUbicacionStyleVal fmt 'ltgen nil) :vlax-true :vlax-false)))
    ((= elem "Via") (setq act_color 0 act_ltype "Continuous" act_lts 0.2 act_lweight 40)) ;; BP: color estricto ByBlock (ACI 0), sin TrueColor RGB
    ((= elem "Rios") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 4) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "Continuous") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 0.2) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw 40) gw_mult (/ (UBI_GetUbicacionStyleVal fmt 'gw (* scl_val 0.0002)) scl_val) lt_gen (if (UBI_GetUbicacionStyleVal fmt 'ltgen nil) :vlax-true :vlax-false))) ;; BQ: Rios GW por tabla UBICACION
    ((= elem "Indirecta") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 1) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "Continuous") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 1.0) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw -3) gw_mult (/ (UBI_GetUbicacionStyleVal fmt 'gw (* scl_val 0.0002)) scl_val) lt_gen (if (UBI_GetUbicacionStyleVal fmt 'ltgen nil) :vlax-true :vlax-false))) ;; BR: AF Indirecta (solo DIAGNOSTICO; clasificador pendiente)
    ((= elem "Remanente") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 3) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "Continuous") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 1.0) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw -3) gw_mult (/ (UBI_GetUbicacionStyleVal fmt 'gw (* scl_val 0.0002)) scl_val) lt_gen (if (UBI_GetUbicacionStyleVal fmt 'ltgen nil) :vlax-true :vlax-false))) ;; BR: Remanente (solo DIAGNOSTICO; clasificador pendiente)
    ((= elem "Obra") (setq act_color (UBI_GetUbicacionStyleVal fmt 'color 0) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "Continuous") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 1.0) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw -3) gw_mult (/ (UBI_GetUbicacionStyleVal fmt 'gw 0.0) scl_val) lt_gen :vlax-false)) ;; BR: Obra complementaria gris (TrueColor 163,163,163 via rgb; clasificador pendiente)
    ((= elem "Vivienda") (setq act_color 5 act_ltype "Continuous" act_lts 1.0 act_lweight -3))
  )
  (if (and (= elem "Afectada") (= objType "AcDbHatch")) (progn (setq elem "Achurado" fmt (UBI_GetUbicacionStyle scl_str elem) act_color (UBI_GetUbicacionStyleVal fmt 'color 71) act_ltype (UBI_GetUbicacionStyleVal fmt 'ltype "Continuous") act_lts (UBI_GetUbicacionStyleVal fmt 'lts 1.0) act_lweight (UBI_GetUbicacionStyleVal fmt 'lw 50))))
  ;; COLOR ACI: se salta si el indice actual ya es el destino (DXF 62; ausente
  ;; = ByLayer = 256, misma lectura que vla-get-color).
  (setq cur (if ed (if (assoc 62 ed) (cdr (assoc 62 ed)) 256) (vl-catch-all-apply 'vla-get-color (list vla_obj))))
  (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (= cur act_color))
    (setq s_col (1+ s_col))
    (progn (vl-catch-all-apply 'vla-put-color (list vla_obj act_color)) (setq nw (1+ nw))))
  ;; NORMALIZACION TRUECOLOR->ACI: se lee el TrueColor siempre; el bloque solo
  ;; se salta si su ColorIndex ya es el destino (preserva el borde ACI igual con
  ;; TrueColor distinto). T2.1: sin grupo 420 no hay TrueColor propio y el
  ;; bloque COM era un no-op (el indice interno sigue al ACI recien fijado).
  (if (and ed (not (assoc 420 ed)))
    (setq s_tc (1+ s_tc))
    (progn
  (setq true_col (vl-catch-all-apply 'vla-get-TrueColor (list vla_obj)))
  (if (not (vl-catch-all-error-p true_col))
    (progn
      (setq cur_tc (vl-catch-all-apply 'vla-get-ColorIndex (list true_col)))
      (if (and *GM_FMT_DIFF_WRITE* (not (vl-catch-all-error-p cur_tc)) (= cur_tc act_color))
        (setq s_tc (1+ s_tc))
        (progn
          (vl-catch-all-apply 'vla-put-ColorIndex (list true_col act_color))
          (vl-catch-all-apply 'vla-put-TrueColor (list vla_obj true_col))
          (setq nw (1+ nw))))
      (vl-catch-all-apply 'vlax-release-object (list true_col))
      (setq true_col nil))
  )
    )
  )
  ;; POR QUE color estricto ACI: la normalizacion TrueColor->ColorIndex de
  ;; arriba ya fija el color; no se re-aplica RGB (antes Predios
  ;; 119,139,80 y Via 132,164).
  ;; BR: TrueColor solo si la tabla trae (rgb R G B) (Obra 163,163,163 en DIAGNOSTICO). Resto sigue ACI estricto.
  (setq act_rgb (UBI_GetUbicacionStyleVal fmt 'rgb nil))
  (if (and act_rgb (listp act_rgb) (= (length act_rgb) 3))
    (progn
      (setq true_col2 (vl-catch-all-apply 'vla-get-TrueColor (list vla_obj)))
      (if (not (vl-catch-all-error-p true_col2))
        (progn
          (setq cur_r (vl-catch-all-apply 'vla-get-Red (list true_col2))
                cur_g (vl-catch-all-apply 'vla-get-Green (list true_col2))
                cur_b (vl-catch-all-apply 'vla-get-Blue (list true_col2)))
          (if (and *GM_FMT_DIFF_WRITE* (not (vl-catch-all-error-p cur_r)) (not (vl-catch-all-error-p cur_g)) (not (vl-catch-all-error-p cur_b))
                   (= cur_r (car act_rgb)) (= cur_g (cadr act_rgb)) (= cur_b (nth 2 act_rgb)))
            (setq s_col (1+ s_col))
            (progn
              (vl-catch-all-apply 'vla-SetRGB (cons true_col2 act_rgb))
              (vl-catch-all-apply 'vla-put-TrueColor (list vla_obj true_col2))
              (setq nw (1+ nw))))
          (vl-catch-all-apply 'vlax-release-object (list true_col2))
          (setq true_col2 nil))))
  )
  ;; LINETYPE, LTS y LINEWEIGHT con lectura previa (T2.1: DXF 6/48/370 cuando
  ;; hay entget; ausente = ByLayer/1.0/-1, igual semantica que las propiedades
  ;; COM. Ante duda se escribe, nunca se salta a ciegas).
  (setq cur (if ed (if (assoc 6 ed) (cdr (assoc 6 ed)) "ByLayer") (vl-catch-all-apply 'vla-get-Linetype (list vla_obj))))
  (if (and *GM_FMT_DIFF_WRITE* (= (type cur) 'STR) (= (strcase cur) (strcase act_ltype)))
    (setq s_lt (1+ s_lt))
    (progn (vl-catch-all-apply 'vla-put-linetype (list vla_obj act_ltype)) (setq nw (1+ nw))))
  (setq cur (if ed (if (assoc 48 ed) (cdr (assoc 48 ed)) 1.0) (vl-catch-all-apply 'vla-get-LinetypeScale (list vla_obj))))
  (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (UBI_FmtNear cur act_lts))
    (setq s_lts (1+ s_lts))
    (progn (vl-catch-all-apply 'vla-put-LinetypeScale (list vla_obj act_lts)) (setq nw (1+ nw))))
  (setq cur (if ed (if (assoc 370 ed) (cdr (assoc 370 ed)) -1) (vl-catch-all-apply 'vla-get-Lineweight (list vla_obj))))
  (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (= cur act_lweight))
    (setq s_lw (1+ s_lw))
    (progn (vl-catch-all-apply 'vla-put-Lineweight (list vla_obj act_lweight)) (setq nw (1+ nw))))
  ;; HATCH (patron+escala) o grosor/generacion de polilineas.
  (if (= objType "AcDbHatch")
    (progn
      (setq h_val (UBI_GetUbicacionStyleVal fmt 'hscale (* scl_val 0.0001))
            h_pat (UBI_GetUbicacionStyleVal fmt 'pattern "SOLID"))
      (setq cur (vl-catch-all-apply 'vla-get-PatternScale (list vla_obj))
            w_val (vl-catch-all-apply 'vla-get-PatternName (list vla_obj)))
      (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (UBI_FmtNear cur h_val) (= (type w_val) 'STR) (= (strcase w_val) (strcase h_pat)))
        (setq s_h (1+ s_h))
        (progn
          (vl-catch-all-apply 'vla-SetPattern (list vla_obj 1 h_pat))
          (vl-catch-all-apply 'vla-put-PatternScale (list vla_obj h_val))
          (vl-catch-all-apply 'vla-Evaluate (list vla_obj))
          (setq nw (1+ nw)))))
    (if (/= elem "Achurado")
      (progn
        (if (vlax-property-available-p vla_obj 'ConstantWidth)
          (progn
            (setq cur (if ed (if (assoc 43 ed) (cdr (assoc 43 ed)) 0.0) (vl-catch-all-apply 'vla-get-ConstantWidth (list vla_obj))))
            (if (and *GM_FMT_DIFF_WRITE* (numberp cur) (UBI_FmtNear cur (* scl_val gw_mult)))
              (setq s_w (1+ s_w))
              (progn (vl-catch-all-apply 'vla-put-ConstantWidth (list vla_obj (* scl_val gw_mult))) (setq nw (1+ nw))))))
        (if (vlax-property-available-p vla_obj 'LinetypeGeneration)
          (progn
            ;; T2.1: DXF 70 bit 128 (generacion de linetype) cuando hay entget;
            ;; ausente = apagado, igual que el booleano COM normalizado.
            (setq cur (if ed (not (null (if (assoc 70 ed) (logand 128 (cdr (assoc 70 ed))) 0))) (vl-catch-all-apply 'vla-get-LinetypeGeneration (list vla_obj))))
            ;; OPT10 (F4): comparar booleanos normalizados; antes :vlax-false se
            ;; volvia T y casi toda polilinea se reescribia en cada codigo.
            (if (and *GM_FMT_DIFF_WRITE* (or ed (not (vl-catch-all-error-p cur))) (= (UBI_FmtBoolNorm cur) (UBI_FmtBoolNorm lt_gen)))
              (setq s_ltgen (1+ s_ltgen))
              (progn (vl-catch-all-apply 'vla-put-LinetypeGeneration (list vla_obj lt_gen)) (setq nw (1+ nw) w_ltgen (1+ w_ltgen))))))
      )
    )
  )
  ;; Update solo si hubo al menos una escritura.
  (if (> nw 0)
    (vl-catch-all-apply 'vla-Update (list vla_obj))
    (setq s_u (1+ s_u)))
  (list s_col s_lt s_lts s_lw s_w s_h s_tc s_u nw s_ltgen w_ltgen)
)
(defun UBI_ApplyAutoFormat (target_code scl_str scl_val / acad_doc ms linetypes obj vla_obj objType hl_str hl_up code_up prefix elem apply_count lay_name lay_up old_echo padded_hl lst_prog lst_estacas lst_peuc lst_afectada lst_matriz lst_derecho lst_eje lst_achurado lst_rios lst_via lst_predios lst_vivienda lst_grid_txt lst_grid_lin base_normal base_sel txt_peuc txt_dist txt_prog val_estaca dia_vert lay ltype layers_col t_seg n_scan n_hl n_skipcol n_skiplt n_skiplts n_skiplw n_skipw n_skiph n_skiptc n_skipu n_sealskip n_writes item idx_rest skips seal_key seal_on n_skipltgen n_writeltgen seal_rest seals_new seal_entry ekey _app norm_lays norm_e norm_ed norm_ln norm_on)
  ;; AUD1: app/doc/linetypes con catch y release deterministico (antes la app era
  ;; anonima y fugaba 1 por codigo incluso en exito).
  (setq _app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (vl-catch-all-error-p _app) (setq _app nil))
  (setq acad_doc (if _app (vl-catch-all-apply 'vla-get-activedocument (list _app)) nil))
  (if (vl-catch-all-error-p acad_doc) (setq acad_doc nil))
  (GM:ReleaseObjectSafe _app)
  (setq _app nil)
  (setq linetypes (if acad_doc (vl-catch-all-apply 'vla-get-linetypes (list acad_doc)) nil))
  (if (vl-catch-all-error-p linetypes) (setq linetypes nil))
  ;; OPT5 2026-09-27 (F0): reinicio de la instrumentacion fina del formato.
  ;; OPT7 2026-09-27 (F3): contadores de escritura diferencial por codigo.
  (setq *GM_FMT_STATS* nil *GM_FMT_HL_MS* 0 *GM_FMT_HL_N* 0)
  (setq *GM_FMT_TX_SKIP* 0 *GM_FMT_TX_WRITES* 0 *GM_FMT_STK_SKIP* 0)
  (setq t_seg (getvar "MILLISECS") n_scan 0 n_hl 0 n_skipcol 0
        n_skiplt 0 n_skiplts 0 n_skiplw 0 n_skipw 0 n_skiph 0 n_skiptc 0 n_skipu 0 n_writes 0
        n_skipltgen 0 n_writeltgen 0)
  (foreach ltype '("Continuous" "DASHDOT" "DASHED2") (if (= (tblobjname "ltype" ltype) nil) (vl-catch-all-apply 'vla-load (list linetypes ltype "acadiso.lin"))))
  (setq code_up (strcase target_code) prefix (UBI_GetCodePrefix code_up))
  ;; OPT8: clave del sello por escala (escala+tipo+multiplicadores de sesion).
  (setq seal_key (strcat scl_str "|" *GM_PLANO_TIPO* "|" *UBI_TEXT_SCALE_MULT* "|" *UBI_STAKE_SCALE_FACTOR* "|" *UBI_PROG_OFFSET_MULT*)
        seal_on (UBI_FmtSealActive seal_key) n_sealskip 0)

  (setq lst_prog nil lst_estacas nil lst_peuc nil lst_afectada nil lst_matriz nil lst_derecho nil lst_eje nil lst_achurado nil lst_rios nil lst_via nil lst_predios nil lst_vivienda nil lst_grid_txt nil lst_grid_lin nil)
  
  (setq apply_count 0 ms (if acad_doc (vl-catch-all-apply 'vla-get-ModelSpace (list acad_doc)) nil))
  (if (vl-catch-all-error-p ms) (setq ms nil))

  ;; OPT6 (F2): con indice de lote el hyperlink/capa/tipo se leen en memoria y
  ;; el puntero avanza solo al coincidir el ename (orden de base estable durante
  ;; el lote); sin indice, o ante una entidad nueva, se conserva la lectura COM.
  (setq idx_rest (if *UBI_ENT_INDEX_READY* *UBI_ENT_INDEX* nil))
  ;; T2.3 2026-09-28: sello por elemento alineado al orden del indice (solo
  ;; lote). Reemplaza la idea del sello global OPT8 (incompatible por diseno).
  (setq seal_rest *GM_FMT_SEALS* seals_new nil)
  ;; AUD1: con ModelSpace no disponible el barrido se omite; el resto degrada
  ;; con N_SCAN=0 (la regla de revision de OPT9 lo marca si aplicadas=0).
  (if ms
  (vlax-for vla_obj ms
    (setq n_scan (1+ n_scan) obj (vlax-vla-object->ename vla_obj) item (car idx_rest))
    (if (and item (eq obj (car item)))
      (setq idx_rest (cdr idx_rest) objType (cadddr item) lay_name (caddr item) hl_str (cadr item))
      (setq item nil objType (vla-get-ObjectName vla_obj) lay_name (vla-get-Layer vla_obj) hl_str (UBI_GetHyperlinkString vla_obj))
    )
    ;; T2.3: consumir la entrada de sello en el MISMO orden del indice.
    (if item
      (setq seal_entry (car seal_rest) seal_rest (cdr seal_rest))
      (setq seal_entry nil))
    (setq ekey nil)
    ;; T1.6: un solo strcase por entidad (antes se repetia 3-4 veces por fila).
    (setq lay_up (strcase lay_name))
    (if (vl-string-search "GRILLA" lay_up) (if (= objType "AcDbText") (setq lst_grid_txt (cons obj lst_grid_txt)) (setq lst_grid_lin (cons obj lst_grid_lin))))
    ;; OPT8: sello por escala; las originales ya formateadas en esta clave saltan
    ;; clasificacion y formato (los hipervinculos siguen contando).
    (if (and item seal_on)
      (progn (setq n_sealskip (1+ n_sealskip))
             (if (/= hl_str "") (setq n_hl (1+ n_hl))))
      (if (and hl_str (/= hl_str ""))
      (progn
        (setq n_hl (1+ n_hl) hl_up (strcase hl_str) padded_hl (strcat " " hl_up " ") elem nil)
        (cond
          ((vl-string-search "PROGRESIVA" hl_up) (setq lst_prog (cons obj lst_prog))) ((vl-string-search "ESTACA" hl_up) (setq lst_estacas (cons obj lst_estacas))) ((vl-string-search "ETIQUETAS PE UC" hl_up) (setq lst_peuc (cons obj lst_peuc))) ((vl-string-search "DERECHO DE VIA" hl_up) (setq elem "Derecho" lst_derecho (cons obj lst_derecho))) ((vl-string-search "EJE DE VIA" hl_up) (setq elem "Eje" lst_eje (cons obj lst_eje))) ((vl-string-search "VIA EXISTENTE" hl_up) (setq elem "Via" lst_via (cons obj lst_via))) ((vl-string-search "VIVIENDA" hl_up) (setq elem "Vivienda" lst_vivienda (cons obj lst_vivienda)))
          
          ;; Exactamente el Target Code (Matriz)
          ((= hl_up code_up) (setq elem "Matriz" lst_matriz (cons obj lst_matriz))) 
          
          ;; Areas de Afectacion que pertenecen al Target Code
          ((and (vl-string-search code_up hl_up) (vl-string-search "-AF-" hl_up)) (setq elem "Achurado" lst_achurado (cons obj lst_achurado))) 
          ((and (vl-string-search code_up hl_up) (vl-string-search "-PF-" hl_up)) (setq elem "Afectada" lst_afectada (cons obj lst_afectada)))
          
          ;; PREDIOS y Colindantes (Blindado con WCMATCH y Capas de GORE)
          ((or (vl-string-search "GORE" hl_up) (vl-string-search "SUNARP" hl_up) (vl-string-search "PC-" hl_up) 
               (vl-string-search "SUNARP" lay_up) (vl-string-search "COLINDANTE" lay_up) (vl-string-search "GORE" lay_up)
               (and (vl-string-search "-PF-" hl_up) (not (vl-string-search code_up hl_up))) 
               (and (vl-string-search "-AF-" hl_up) (not (vl-string-search code_up hl_up))) 
               (vl-string-search "PREDIO" hl_up) (vl-string-search "COLINDANTE" hl_up) 
               (vl-string-search "PROPIETARIO" hl_up) (vl-string-search "TERCERO" hl_up)
               (and (/= hl_up code_up) (or (and (/= prefix "") (vl-string-search prefix hl_up)) (wcmatch hl_up "*-*-*#*")))) ; <-- CORRECCI\\U+00D3N: ESTRUCTURA CATASTRAL VECINA
           (setq elem "Predios" lst_predios (cons obj lst_predios)))
          
          ((or (vl-string-search " RIO " padded_hl) (vl-string-search "QUEBRADA" hl_up)) (setq elem "Rios" lst_rios (cons obj lst_rios)))
        )
        (if elem
          (progn
            ;; T2.3: sello por elemento; clases estables sin codigo (Predios,
            ;; Via, Derecho, Eje, Rios, Vivienda) y clases del target con codigo
            ;; (Matriz, Achurado, Afectada) para no heredar estilo cruzado.
            (setq ekey (strcat seal_key "|" elem (if (member elem '("Achurado" "Afectada" "Matriz")) (strcat "|" code_up) "")))
            (if (and seal_entry (eq obj (car seal_entry)) (equal (cdr seal_entry) ekey))
              (setq n_sealskip (1+ n_sealskip))
              (progn
                ;; OPT7 2026-09-27 (F3): estilo con escritura diferencial (helper
                ;; lee-antes-de-escribir; devuelve saltos por propiedad + escrituras).
                (setq skips (UBI_FmtApplyEnt vla_obj objType elem scl_str scl_val))
                (setq n_skipcol (+ n_skipcol (nth 0 skips))
                      n_skiplt (+ n_skiplt (nth 1 skips))
                      n_skiplts (+ n_skiplts (nth 2 skips))
                      n_skiplw (+ n_skiplw (nth 3 skips))
                      n_skipw (+ n_skipw (nth 4 skips))
                      n_skiph (+ n_skiph (nth 5 skips))
                      n_skiptc (+ n_skiptc (nth 6 skips))
                      n_skipu (+ n_skipu (nth 7 skips))
                      n_writes (+ n_writes (nth 8 skips))
                      ;; OPT10 (F4): saltos/escrituras de LinetypeGeneration.
                      n_skipltgen (+ n_skipltgen (nth 9 skips))
                      n_writeltgen (+ n_writeltgen (nth 10 skips)))
                (setq apply_count (1+ apply_count))))
          )
        )
      )
    )
    )
    (if item (setq seals_new (cons (cons obj ekey) seals_new)))
  )
  )
  ;; T2.3: publicar el sello del lote (solo con indice; el sello OPT8 opt-in
  ;; queda incompatible por diseno y no debe activarse junto a este).
  (setq *GM_FMT_SEALS* (if (and *UBI_ENT_INDEX_READY* (not seal_on)) (reverse seals_new) nil))

  ;; CHG3 2026-10-10: las capas del predio (matriz/AF/PF) pueden llegar
  ;; congeladas o apagadas en la plantilla: el relleno AF quedaba aplicado
  ;; pero invisible en el DWG y en el PDF. Se reactiva SOLO la capa de esas
  ;; entidades (las de contexto quedan como las dejo el ingeniero) antes del
  ;; DRAWORDER, de los esquemas y de la exportacion.
  (setq norm_lays nil norm_on 0)
  (foreach norm_e (append lst_matriz lst_afectada lst_achurado)
    (setq norm_ed (entget norm_e)
          norm_ln (if norm_ed (cdr (assoc 8 norm_ed)) nil))
    (if (and norm_ln (not (member norm_ln norm_lays)))
      (setq norm_lays (cons norm_ln norm_lays))))
  (if (and *UBI_TARGET_POLYGON* (entget *UBI_TARGET_POLYGON*))
    (progn
      (setq norm_ed (entget *UBI_TARGET_POLYGON*)
            norm_ln (if norm_ed (cdr (assoc 8 norm_ed)) nil))
      (if (and norm_ln (not (member norm_ln norm_lays)))
        (setq norm_lays (cons norm_ln norm_lays)))))
  (foreach norm_ln norm_lays
    (if (UBI_EnsureLayerOn norm_ln) (setq norm_on (1+ norm_on))))
  (if (and (> norm_on 0) *GM_AUTO_MODE* (boundp 'GM:AutoTrace))
    (GM:AutoTrace *ML_TARGET_CODE* "CAPAS_PREDIO" "OK"
      (strcat "reactivadas=" (itoa norm_on))))
  
  ;; AQU\\U+00CD SE LLAMA A LA FUNCI\\U+00D3N ENVIANDO LAS MEDIDAS MATEM\\U+00C1TICAMENTE EXACTAS
  ;; BQ: tabla estricta por VP_TYPE (NORMAL=1, UBICACION=2, default NORMAL). Sin x0.5: UBICACION se lee literal.
  ;; OPT5 2026-09-27 (F0): fin del tramo MAPA (barrido+clasificacion+estilo);
  ;; abre el tramo TEXTOS (mapas por escala + UBI_ProcessHyperlinks).
  (GM:FmtStat "MAPA" (max 0 (- (getvar "MILLISECS") t_seg)))
  (setq t_seg (getvar "MILLISECS"))
  (setq base_sel (if (= *UBI_VP_TYPE* "2") *UBI_DATA_MAP_UBICACION* *UBI_DATA_MAP_NORMAL*))
  (setq base_normal (assoc scl_str base_sel))
  (if (not base_normal) (setq base_normal (assoc scl_str *UBI_DATA_MAP_NORMAL*))) ;; BQ fallback: NORMAL si falta escala en UBICACION
  (if base_normal
    (progn
      (setq txt_peuc (nth 1 base_normal)
            txt_dist (nth 2 base_normal)
            val_estaca (nth 3 base_normal)
            txt_prog (nth 7 base_normal)
            dia_vert (nth 8 base_normal))
      (if (not (numberp txt_dist)) (setq txt_dist txt_peuc)) ;; BQ fallback: dist ausente usa texto general
      (if (not (numberp val_estaca)) (setq val_estaca (* txt_peuc 2.0))) ;; BQ: tabla manda; x2 solo fallback (mock 8-elem)
      (if (not (numberp txt_prog)) (setq txt_prog txt_peuc))
      (if (not (numberp dia_vert)) (setq dia_vert (* txt_peuc 0.5))) ;; BQ: diametro inferido mitad texto
      (setq *UBI_VERT_DIA_LAST* dia_vert) ;; BQ trazabilidad en sesion
      (UBI_ProcessHyperlinks val_estaca txt_prog txt_peuc txt_dist)
    )
  )
  ;; OPT5 2026-09-27 (F0): fin del tramo TEXTOS; abre el tramo ORDEN.
  (GM:FmtStat "TEXTOS" (max 0 (- (getvar "MILLISECS") t_seg)))
  (setq t_seg (getvar "MILLISECS"))
  
  (setq old_echo (getvar "CMDECHO")) (setvar "CMDECHO" 0)
  ;; T1.1 REVERTIDO (revision 28-09-2026): el DRAWORDER agrupado preserva el
  ;; orden actual de la base, no la cascada por grupos; con 14 listas el
  ;; apilado exacto exige los comandos individuales (validar visualmente antes
  ;; de agrupar). Se conserva la cascada original completa.
  (UBI_ApplyDrawOrder lst_grid_lin "_Back") (UBI_ApplyDrawOrder lst_predios "_Front") (UBI_ApplyDrawOrder lst_vivienda "_Front") (UBI_ApplyDrawOrder lst_via "_Front") (UBI_ApplyDrawOrder lst_rios "_Front") (UBI_ApplyDrawOrder lst_achurado "_Front") (UBI_ApplyDrawOrder lst_eje "_Front") (UBI_ApplyDrawOrder lst_derecho "_Front") (UBI_ApplyDrawOrder lst_matriz "_Front") (UBI_ApplyDrawOrder lst_afectada "_Front") (UBI_ApplyDrawOrder lst_peuc "_Front") (UBI_ApplyDrawOrder lst_estacas "_Front") (UBI_ApplyDrawOrder lst_prog "_Front") (UBI_ApplyDrawOrder lst_grid_txt "_Front")
  (setvar "CMDECHO" old_echo) (UBI_Nuke-Memory)
  ;; OPT5 2026-09-27 (F0): fin del tramo ORDEN; abre el tramo CIERRE.
  (GM:FmtStat "ORDEN" (max 0 (- (getvar "MILLISECS") t_seg)))
  (setq t_seg (getvar "MILLISECS"))
  
  ;; APAGAR CAPA OBRAS COMPLEMENTARIAS (AUD1: creacion con catch; el barrido
  ;; se omite si la coleccion no esta disponible en vez de abortar el cierre).
  (setq layers_col (if acad_doc (vl-catch-all-apply 'vla-get-Layers (list acad_doc)) nil))
  (if (vl-catch-all-error-p layers_col) (setq layers_col nil))
  (if layers_col
    (vlax-for lay layers_col
      (setq lay_name (vl-catch-all-apply 'vla-get-Name (list lay)))
      (if (vl-catch-all-error-p lay_name) (setq lay_name nil))
      (if (and (= (type lay_name) 'STR)
               (or (vl-string-search "OBRA COMPLEMENTARIA" (strcase lay_name))
                   (vl-string-search "OBRAS COMPLEMENTARIA" (strcase lay_name))))
        (vl-catch-all-apply 'vla-put-LayerOn (list lay :vlax-false))
      )
    )
  )
  ;; Fase 1B (H-5): liberar coleccion de capas, linetypes, ModelSpace y documento.
  (vl-catch-all-apply 'vlax-release-object (list layers_col))
  (vl-catch-all-apply 'vlax-release-object (list linetypes))
  (vl-catch-all-apply 'vlax-release-object (list ms))
  (vl-catch-all-apply 'vlax-release-object (list acad_doc))
  (setq layers_col nil linetypes nil ms nil acad_doc nil)
  ;; OPT5 2026-09-27 (F0): cierre del tramo CIERRE y publicacion de conteos del
  ;; formato (los lee GM:FmtStatsReport desde AUTOMATIZACION).
  (GM:FmtStat "CIERRE" (max 0 (- (getvar "MILLISECS") t_seg)))
  (GM:FmtStat "N_SCAN" n_scan)
  (GM:FmtStat "N_HL" n_hl)
  (GM:FmtStat "N_APPLY" apply_count)
  (GM:FmtStat "N_SKIPCOL" n_skipcol)
  ;; OPT7 2026-09-27 (F3): saltos por propiedad del formato + textos/estacas.
  (GM:FmtStat "N_SKIPLT" n_skiplt)
  (GM:FmtStat "N_SKIPLTS" n_skiplts)
  (GM:FmtStat "N_SKIPLW" n_skiplw)
  (GM:FmtStat "N_SKIPW" n_skipw)
  (GM:FmtStat "N_SKIPH" n_skiph)
  (GM:FmtStat "N_SKIPTC" n_skiptc)
  (GM:FmtStat "N_SKIPU" n_skipu)
  (GM:FmtStat "N_WRITES" n_writes)
  ;; OPT10 (F4): contadores separados de LinetypeGeneration (arreglo booleano).
  (GM:FmtStat "N_SKIPLTGEN" n_skipltgen)
  (GM:FmtStat "N_WRITELTGEN" n_writeltgen)
  ;; OPT8: entidades saltadas por sello de escala.
  (GM:FmtStat "N_SEALSKIP" n_sealskip)
  (GM:FmtStat "N_TXSKIP" *GM_FMT_TX_SKIP*)
  (GM:FmtStat "N_TXWR" *GM_FMT_TX_WRITES*)
  (GM:FmtStat "N_STKSKIP" *GM_FMT_STK_SKIP*)
  (GM:FmtStat "HL_N" *GM_FMT_HL_N*)
  (GM:FmtStat "HL_MS" *GM_FMT_HL_MS*)
  ;; OPT8: sello de la escala del lote (solo con indice y diferencial activos);
  ;; el siguiente codigo con la misma clave salta el formato de las originales.
  (if (and *UBI_ENT_INDEX_READY* *GM_FMT_SEAL* *GM_FMT_DIFF_WRITE*)
    (setq *UBI_INDEX_SEAL_KEY* seal_key))
  
  (princ (strcat "\n[\\U+00C9XITO] Formato Visual aplicado a " (itoa apply_count) " objetos."))
)