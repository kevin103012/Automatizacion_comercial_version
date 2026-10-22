@echo off
rem GM INGENIEROS - quitar la marca de descarga de Windows (archivo generado; no editar)
rem Respaldo manual: util si Windows bloqueo los scripts con el mensaje
rem "no esta firmado digitalmente" (Zone.Identifier).
powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "$files=@(Get-ChildItem -LiteralPath '%~dp0tools' -File -EA SilentlyContinue)+@(Get-ChildItem -LiteralPath '%~dp0lisp' -Recurse -File -EA SilentlyContinue)+@(Get-ChildItem -LiteralPath '%~dp0docs' -Recurse -File -EA SilentlyContinue)+@(Get-ChildItem -LiteralPath '%~dp0' -File -EA SilentlyContinue); $e=0; foreach ($f in $files) { try { Unblock-File -LiteralPath $f.FullName -EA Stop } catch { $e++ } }; $s=0; $u=0; foreach ($f in $files) { try { $h=[System.IO.File]::Open($f.FullName,'Open','Read','None'); $h.Dispose(); if (Get-Item -LiteralPath $f.FullName -Stream Zone.Identifier -EA SilentlyContinue) { $s++ } } catch { $u++ } }; if ($e -gt 0 -or $s -gt 0 -or $u -gt 0) { Write-Host ('ERROR: ' + $s + ' marcado(s), ' + $u + ' en uso (no comprobables) y ' + $e + ' fallo(s).'); exit 1 } else { Write-Host ('OK: ' + $files.Count + ' archivo(s) revisados, ninguno marcado.'); exit 0 }"
rem Etiquetas y no bloques if/else: dentro de un parentesis, cmd no tolera
rem parentesis sin cerrar en los textos de echo y aborta con "No se esperaba .".
if errorlevel 1 goto :gm_error
echo.
echo Listo: la marca de descarga fue quitada de este paquete.
echo Ahora ejecute INICIAR_LOTE.cmd - doble clic en la raiz del paquete.
set "GM_ERR=0"
goto :gm_fin
:gm_error
echo.
echo ERROR: no se pudo quitar la marca de descarga de todos los archivos.
echo Cierre AutoCAD y cualquier editor abierto sobre este paquete y repita.
set "GM_ERR=1"
:gm_fin
pause
if "%GM_ERR%"=="1" exit /b 1
exit /b 0
