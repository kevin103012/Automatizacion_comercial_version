;; ============================================================================
;; LAYOUT_UBICACION.lsp - ANCLA DEL MODULO LAY (caratula de sello)
;; El codigo real vive en lisp/ y se ordena en lisp/MANIFIESTO_MODULOS.txt.
;; Manual (APPLOAD): cargar este archivo carga sus modulos en orden.
;; Lote: tools/GM_LANZADOR_LOTE.ps1 carga los modulos directo por manifiesto.
;; Sello: 2026-10-10-CHG3
;; ============================================================================
(setq *GM_VER_LAYOUT* "2026-10-10-CHG3")
(defun GM:AnclaBaseLayout ( / dir up found)
  (setq dir (getvar "DWGPREFIX") up 0 found nil)
  (while (and (not found) dir (< up 6))
    (if (findfile (strcat dir "lisp/MANIFIESTO_MODULOS.txt"))
      (setq found dir)
      (progn
        (setq dir (vl-filename-directory (vl-string-right-trim "\\/" dir)))
        (if (or (not dir) (= dir "") (= dir (substr dir 1 2))) (setq dir nil))
        (setq up (1+ up)))))
  found
)
(defun GM:AnclaCargarLayout (rels / base rel f old)
  (setq base (GM:AnclaBaseLayout))
  (if base
    (progn
      (setq old (getvar "TRUSTEDPATHS"))
      (if (not (vl-string-search (strcase base) (strcase old)))
        (vl-catch-all-apply 'setvar (list "TRUSTEDPATHS" (strcat old ";" base))))
      (foreach rel rels
        (setq f (findfile (strcat base rel)))
        (if f (load f) (princ (strcat "\n[GM] AVISO: falta " rel)))))
    (princ "\n[GM] AVISO: no se hallo lisp/MANIFIESTO_MODULOS.txt; cargue los modulos a mano."))
  (princ)
)
(GM:AnclaCargarLayout '("lisp/lay/LAY_COMUN.lsp" "lisp/lay/LAY_LANDING.lsp" "lisp/lay/LAY_ETIQUETAS.lsp" "lisp/lay/LAY_ESQUEMAS.lsp" "lisp/lay/LAY_COMANDOS.lsp"))
(princ "\nLAYOUT_UBICACION.lsp (ancla): modulos cargados.")
(princ)
