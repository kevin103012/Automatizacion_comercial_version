@echo off
rem GM INGENIEROS - lote automatico de planos (archivo generado; no editar)
rem 1) Quita la marca de descarga de Windows (Zone.Identifier): un ZIP
rem    descargado marca todo lo que se extrae y RemoteSigned bloquea los
rem    .ps1 con el error "no esta firmado digitalmente".
rem 2) Lanza el lanzador con ExecutionPolicy Bypass (solo este proceso).
powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "Get-ChildItem -LiteralPath '%~dp0tools' -File -ErrorAction SilentlyContinue | Unblock-File -ErrorAction SilentlyContinue; Get-ChildItem -LiteralPath '%~dp0lisp' -Recurse -File -ErrorAction SilentlyContinue | Unblock-File -ErrorAction SilentlyContinue; Get-ChildItem -LiteralPath '%~dp0docs' -Recurse -File -ErrorAction SilentlyContinue | Unblock-File -ErrorAction SilentlyContinue; Get-ChildItem -LiteralPath '%~dp0' -File -ErrorAction SilentlyContinue | Where-Object { $_.Extension -in '.ps1','.psm1','.psd1','.cmd','.bat' } | Unblock-File -ErrorAction SilentlyContinue; Unblock-File -LiteralPath '%~dp0' -ErrorAction SilentlyContinue"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0tools\GM_LANZADOR_LOTE.ps1" %*
if errorlevel 1 pause
