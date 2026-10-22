;; ============================================================================
;; UBI_BASE.lsp - Sistema GM Ingenieros (modulo ubi)
;; Movido SIN CAMBIOS desde PLANO_UBICACION.lsp (division PRY2 2026-10-13).
;; El orden de carga vive en lisp/MANIFIESTO_MODULOS.txt.
;; ============================================================================

(defun UBI_ContainsString (lst str / res s)
  ;; s localizada POR QUE: foreach sin declarar iterador lo filtraba al entorno
  ;; dinamico/global y podia pisar una variable s del llamador.
  (setq res nil) (foreach s lst (if (= (strcase s) (strcase str)) (setq res T))) res
)

(defun UBI_SafeLDataGet (owner key default / res)
  (setq res (vl-catch-all-apply 'vlax-ldata-get (list owner key)))
  (if (vl-catch-all-error-p res) default res)
)
(defun UBI_SafeLDataPut (owner key val) (vl-catch-all-apply 'vlax-ldata-put (list owner key val)) val)

;; OPT2 2026-09-26 (Fase 3): lista de entidades cuya visibilidad cambio en el
;; codigo. Restaurar solo esa lista evita recorrer todo el Model (H-2) y los
;; entmod/entupd innecesarios que llenaban el historial UNDO.
(if (not (boundp '*UBI_VIS_MODIFIED*)) (setq *UBI_VIS_MODIFIED* nil))

;; CHG3 2026-10-10: una capa congelada (bit 1 de DXF 70) o apagada (62
;; negativo) no se muestra, no se puede seleccionar y no sale en el PDF,
;; aunque el estilo de la entidad este bien aplicado. El predio de cada
;; codigo (matriz/AF/PF) debe verse siempre: reactiva su capa con COM.
;; Devuelve T solo si cambio algo. No toca capas de contexto.
(defun UBI_EnsureLayerOn (lname / rec obj off froz changed)
  (setq rec (tblsearch "LAYER" lname) changed nil)
  (if rec
    (progn
      (setq off (< (if (numberp (cdr (assoc 62 rec))) (cdr (assoc 62 rec)) 7) 0)
            froz (= 1 (logand 1 (if (numberp (cdr (assoc 70 rec))) (cdr (assoc 70 rec)) 0))))
      (if (or off froz)
        (progn
          (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list (tblobjname "LAYER" lname))))
          (if (not (vl-catch-all-error-p obj))
            (progn
              (if froz
                (progn
                  (vl-catch-all-apply 'vla-put-Freeze (list obj :vlax-false))
                  (setq changed T)))
              (if off
                (progn
                  (vl-catch-all-apply 'vla-put-LayerOn (list obj :vlax-true))
                  (setq changed T)))
              (vl-catch-all-apply 'vlax-release-object (list obj))
              (setq obj nil)))))))
  changed
)

;; Devuelve T solo si la visibilidad cambio realmente; evita reescribir DXF 60
;; cuando la entidad ya esta en el estado pedido. Ausencia de grupo 60 = visible.
(defun UBI_SetEntityVisible (ent isVisible / ed d60 new60 cur changed)
  ;; T2.2-FIX1: una sola lectura DXF por entidad (antes se leia dos veces).
  (if (and ent (setq ed (entget ent)))
    (progn
      (setq d60 (assoc 60 ed))
      ;; cur = T si visible (sin 60 o 60=0), nil si oculto (60=1).
      (setq cur (if (and d60 (= (cdr d60) 1)) nil T))
      (if (/= cur (if isVisible T nil))
        (progn
          (setq new60 (cons 60 (if isVisible 0 1)))
          (cond (d60 (entmod (subst new60 d60 ed))) ((not isVisible) (entmod (append ed (list new60)))))
          (entupd ent)
          (setq changed T))
        (setq changed nil))
    )
  )
  changed
)

(defun UBI_RestoreHiddenByGM (/ app doc ms vla_obj ent cnt lay lay_name lays)
  ;; F15-R1: doc/ms/Layers/app con catch y liberados al salir; esta rutina corre
  ;; por codigo (limpieza) y dejaba 2-4 wrappers vivos en cada transicion.
  (setq cnt 0
        app (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (vl-catch-all-error-p app) (setq app nil))
  (setq doc (if app (vl-catch-all-apply 'vla-get-ActiveDocument (list app)) nil))
  (if (vl-catch-all-error-p doc) (setq doc nil))
  ;; OPT2 (Fase 3): restaurar solo la lista de modificadas; sin lista se
  ;; conserva el barrido completo por LData. T2.2-FIX1: en lote con delta la
  ;; visibilidad la gobierna la mascara base (UBI_RestoreVisibilityBase); aqui
  ;; solo se invalida el estado previo para forzar la resincronizacion y no se
  ;; pelea con el delta mostrando entidades a mitad de lote.
  (if (and *GM_AUTO_MODE* *UBI_ENT_INDEX_READY* *UBI_ENT_INDEX*)
    (setq *UBI_VIS_PREV_READY* nil cnt 0)
    (progn
      (if *UBI_VIS_MODIFIED*
        (progn
          (foreach ent (reverse *UBI_VIS_MODIFIED*)
            (if (and ent (entget ent))
              (progn
                (UBI_SetEntityVisible ent T)
                (if (= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1) (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 0))
                (setq cnt (1+ cnt)))))
          (setq *UBI_VIS_MODIFIED* nil))
        (progn
          (if doc
            (progn
              (setq ms (vl-catch-all-apply 'vla-get-ModelSpace (list doc)))
              (if (not (vl-catch-all-error-p ms))
                (vlax-for vla_obj ms
                  (setq ent (vlax-vla-object->ename vla_obj))
                  (if (= (UBI_SafeLDataGet ent "GM_VIS_HIDDEN" 0) 1)
                    (progn
                      (UBI_SetEntityVisible ent T)
                      (UBI_SafeLDataPut ent "GM_VIS_HIDDEN" 0)
                      (setq cnt (1+ cnt))
                    )
                  )
                ))
              (GM:ReleaseObjectSafe ms)
              (setq ms nil)))))
      
    )
  )
  ;; T2.2-FIX1 (revision de cierre 28-09-2026): el apagado de capas OBRAS
  ;; COMPLEMENTARIAS lo hace el formato; el reencendido corre en AMBOS modos.
  ;; Antes quedo dentro de la rama legado y el lote terminaba con las capas
  ;; apagadas (la restauracion de entidades la gobierna la mascara base).
  (if doc
    (progn
      (setq lays (vl-catch-all-apply 'vla-get-Layers (list doc)))
      (if (not (vl-catch-all-error-p lays))
        (vlax-for lay lays
          (setq lay_name (strcase (vla-get-Name lay)))
          (if (or (vl-string-search "OBRA COMPLEMENTARIA" lay_name) (vl-string-search "OBRAS COMPLEMENTARIA" lay_name))
            (vl-catch-all-apply 'vla-put-LayerOn (list lay :vlax-true))
          )
        ))
      (GM:ReleaseObjectSafe lays)))
  (GM:ReleaseObjectSafe doc)
  (GM:ReleaseObjectSafe app)
  cnt
)