;; ============================================================================
;; AUTOMATIZACION.lsp - ANCLA DEL MODULO GM (caratula de sello)
;; El codigo real vive en lisp/ y se ordena en lisp/MANIFIESTO_MODULOS.txt.
;; Manual (APPLOAD): cargar este archivo carga sus modulos en orden.
;; Lote: tools/GM_LANZADOR_LOTE.ps1 carga los modulos directo por manifiesto.
;; Sello: 2026-10-21-FIX1
;; ============================================================================
(setq *GM_VER_AUTO* "2026-10-21-FIX1")
(defun GM:AnclaBaseAuto ( / dir up found)
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
(defun GM:AnclaCargarAuto (rels / base rel f old)
  (setq base (GM:AnclaBaseAuto))
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
(GM:AnclaCargarAuto '("lisp/gm/GM_NUCLEO.lsp" "lisp/gm/GM_REPORTE.lsp" "lisp/gm/GM_GEO.lsp" "lisp/gm/GM_LOTE.lsp" "lisp/gm/GM_PLANTILLA.lsp" "lisp/gm/GM_MIEMBRO.lsp" "lisp/gm/GM_MODELO.lsp" "lisp/gm/GM_ROTULO.lsp" "lisp/gm/GM_TAREA.lsp" "lisp/gm/GM_COMANDOS.lsp"))
(princ "\nAUTOMATIZACION.lsp (ancla): modulos cargados.")
(princ)                             

