@echo off
rem GM INGENIEROS - abrir AutoCAD con el sistema cargado para marcar roles (archivo generado; no editar)
rem Quita la marca de descarga de Windows (Zone.Identifier) antes de lanzar.
powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "Get-ChildItem -LiteralPath '%~dp0tools' -File -ErrorAction SilentlyContinue | Unblock-File -ErrorAction SilentlyContinue; Get-ChildItem -LiteralPath '%~dp0lisp' -Recurse -File -ErrorAction SilentlyContinue | Unblock-File -ErrorAction SilentlyContinue; Get-ChildItem -LiteralPath '%~dp0docs' -Recurse -File -ErrorAction SilentlyContinue | Unblock-File -ErrorAction SilentlyContinue; Get-ChildItem -LiteralPath '%~dp0' -File -ErrorAction SilentlyContinue | Where-Object { $_.Extension -in '.ps1','.psm1','.psd1','.cmd','.bat' } | Unblock-File -ErrorAction SilentlyContinue; Unblock-File -LiteralPath '%~dp0' -ErrorAction SilentlyContinue"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0tools\GM_ABRIR_PLANTILLA.ps1" %*
if errorlevel 1 pause
