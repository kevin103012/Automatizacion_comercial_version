param(
  [string]$Dwg = '',
  [switch]$SinDibujo,
  [string]$Smoke = ''
)

# GM_ABRIR_PLANTILLA.ps1 - abre AutoCAD con los modulos LISP del sistema ya
# cargados, listo para marcar roles (GM_VALIDAR_PLANTILLA / GM_MARCAR_PLANTILLA
# / GM_MARCAR_ROL) en una plantilla propia.
#
# EMP2 2026-10-20: las anclas raiz (PLANO/LAYOUT/AUTOMATIZACION.lsp) solo
# autocargan si el DWG vive dentro de la carpeta del sistema (buscan
# lisp/MANIFIESTO_MODULOS.txt subiendo desde el DWG). Para marcar roles en un
# DWG de otra carpeta esto falla; esta tool carga los modulos por rutas
# absolutas del manifiesto y deja el prompt listo SIN cerrar AutoCAD.
#
# Uso:
#   powershell -ExecutionPolicy Bypass -File tools\GM_ABRIR_PLANTILLA.ps1            (selector de DWG)
#   powershell -ExecutionPolicy Bypass -File tools\GM_ABRIR_PLANTILLA.ps1 -Dwg "C:\ruta\mi_plantilla.dwg"
#   powershell -ExecutionPolicy Bypass -File tools\GM_ABRIR_PLANTILLA.ps1 -SinDibujo (CAD vacio; abrir el DWG a mano)
#   -Smoke "<log>"  (uso interno de verificacion: carga, escribe el log y cierra con QUIT)
#
# No modifica el proyecto ni la plantilla maestra: si se elige el master
# oficial (hash dorado) se rechaza y se pide abrir una copia.

$ErrorActionPreference = 'Stop'
$script:ProjectRoot = Split-Path -Parent $PSScriptRoot
$script:GoldenTemplateHash = '2875910693DB5313986459820A74026115945AFF7ED3A7968707B5B076A5423D'
$script:ManifestPath = Join-Path $script:ProjectRoot 'lisp\MANIFIESTO_MODULOS.txt'

function ConvertTo-GmLispString([string]$Value) {
  $escaped = $Value.Replace('\', '/').Replace('"', '\"')
  return ('"' + $escaped + '"')
}

# Mismo criterio que el lanzador: AutoCAD* mas reciente bajo Program Files.
function Find-GmAcad {
  $candidates = New-Object 'System.Collections.Generic.List[string]'
  foreach ($base in @($env:ProgramFiles, ${env:ProgramFiles(x86)})) {
    if (-not $base) { continue }
    $autodesk = Join-Path $base 'Autodesk'
    if (Test-Path -LiteralPath $autodesk -PathType Container) {
      foreach ($dir in [System.IO.Directory]::GetDirectories($autodesk, 'AutoCAD*')) {
        $candidate = Join-Path $dir 'acad.exe'
        if (Test-Path -LiteralPath $candidate -PathType Leaf) { $candidates.Add($candidate) }
      }
    }
  }
  if ($candidates.Count -gt 0) {
    $ordered = @($candidates | Sort-Object -Descending)
    return $ordered[0]
  }
  return ''
}

# EMP2: el orden de carga vive en el manifiesto; la tool no duplica la lista.
function Get-GmModuleLoadList {
  if (-not (Test-Path -LiteralPath $script:ManifestPath -PathType Leaf)) {
    throw 'Falta lisp\MANIFIESTO_MODULOS.txt (orden de carga de modulos LISP).'
  }
  $list = New-Object 'System.Collections.Generic.List[string]'
  foreach ($raw in [System.IO.File]::ReadAllLines($script:ManifestPath)) {
    $line = $raw.Trim()
    if ($line -eq '' -or $line.StartsWith('#')) { continue }
    $parts = $line -split '\|'
    if ($parts.Count -ne 2) { throw "Linea invalida en MANIFIESTO_MODULOS.txt: $line" }
    $rel = $parts[1].Trim().Replace('/', '\')
    $full = Join-Path $script:ProjectRoot $rel
    if (-not (Test-Path -LiteralPath $full -PathType Leaf)) { throw "Modulo del manifiesto ausente: $rel" }
    $list.Add($rel)
  }
  if ($list.Count -eq 0) { throw 'MANIFIESTO_MODULOS.txt no lista modulos.' }
  return $list
}

if ($Dwg -and $SinDibujo) { throw 'Use -Dwg o -SinDibujo, no ambos.' }

$acadPath = Find-GmAcad
if (-not $acadPath) { throw 'No se encontro acad.exe bajo Program Files\Autodesk. Instale AutoCAD o ajuste la instalacion.' }

$dwgPath = ''
if (-not $SinDibujo) {
  if ($Dwg) {
    # EMP2-FIX1: -Dwg puede venir absoluto (caso normal) o relativo a la raiz.
    $dwgPath = if ([System.IO.Path]::IsPathRooted($Dwg)) {
      [System.IO.Path]::GetFullPath($Dwg)
    } else {
      [System.IO.Path]::GetFullPath((Join-Path $script:ProjectRoot $Dwg))
    }
    if (-not (Test-Path -LiteralPath $dwgPath -PathType Leaf)) { throw "No existe el DWG: $dwgPath" }
  } else {
    Add-Type -AssemblyName System.Windows.Forms
    $dialog = New-Object System.Windows.Forms.OpenFileDialog
    $dialog.Title = 'GM - Elija el DWG base para marcar roles'
    $dialog.Filter = 'DWG (*.dwg)|*.dwg|Todos los archivos (*.*)|*.*'
    $dialog.InitialDirectory = $script:ProjectRoot
    if ($dialog.ShowDialog() -ne [System.Windows.Forms.DialogResult]::OK) {
      Write-Output 'ABRIR_PLANTILLA;CANCELADO (sin DWG no se abre AutoCAD).'
      exit 0
    }
    $dwgPath = [System.IO.Path]::GetFullPath($dialog.FileName)
  }
  # Fail-closed: el master oficial (ruta exacta) no se abre para marcar roles;
  # el lote lo copia a temporal por corrida. Una COPIA en otra ruta se permite
  # (marcar roles en una copia fresca es el flujo de recepcion); si es
  # byte-identica al master se avisa, sin bloquear.
  $masterPath = [System.IO.Path]::GetFullPath((Join-Path $script:ProjectRoot 'PLANTILLA_ORIGINAL_HUANCASANCOS.dwg'))
  if ([string]::Compare($dwgPath, $masterPath, $true) -eq 0) {
    throw 'Ese DWG es la plantilla maestra oficial del sistema. No la marque directamente: copiela con otro nombre y abra la copia.'
  }
  $dwgHash = (Get-FileHash -LiteralPath $dwgPath -Algorithm SHA256).Hash
  if ($dwgHash -eq $script:GoldenTemplateHash) {
    Write-Warning 'El DWG elegido es una copia exacta del master (sin cambios). Marcar roles aqui es valido; recuerde guardar con otro nombre si quiere conservar el master intacto.'
  }
}

$modules = Get-GmModuleLoadList
$lines = New-Object 'System.Collections.Generic.List[string]'
$lines.Add('(setq *GM_ABRIR_TRUSTED_OLD* (getvar "TRUSTEDPATHS"))')
$trustedAdd = @(
  $script:ProjectRoot,
  (Join-Path $script:ProjectRoot 'lisp'),
  (Join-Path $script:ProjectRoot 'lisp\ubi'),
  (Join-Path $script:ProjectRoot 'lisp\lay'),
  (Join-Path $script:ProjectRoot 'lisp\gm')
) | ForEach-Object { ConvertTo-GmLispString $_ }
$lines.Add('(setvar "TRUSTEDPATHS" (strcat *GM_ABRIR_TRUSTED_OLD* ";" ' + (($trustedAdd) -join ' ";" ') + '))')
if ($Smoke) {
  $smokeLisp = ConvertTo-GmLispString $Smoke
  $lines.Add('(setq *GM_SMOKE_F* (open ' + $smokeLisp + ' "a"))')
  $lines.Add('(write-line "SMOKE;BEGIN" *GM_SMOKE_F*)')
  $lines.Add('(close *GM_SMOKE_F*)')
}
foreach ($rel in $modules) {
  $path = ConvertTo-GmLispString (Join-Path $script:ProjectRoot $rel)
  $lines.Add('(load ' + $path + ')')
}
$lines.Add('(setvar "TRUSTEDPATHS" *GM_ABRIR_TRUSTED_OLD*)')
if ($Smoke) {
  # Sonda de verificacion: cuenta de modulos, comandos de rol disponibles,
  # sello AUTO cargado y gate de versiones presente. Here-string para no romper
  # el balance de comillas LISP (una comilla perdida se traga las lineas
  # siguientes del SCR, incluido el QUIT). Cierra con QUIT/No doble (mismo par
  # del lanzador) para que la corrida termine sola.
  $probeTemplate = @'
(progn (setq *GM_SMOKE_F* (open "@LOG@" "a")) (write-line (strcat "SMOKE;LOADS=@N@;MENU=" (if (boundp 'c:MENU_PLANOS) "1" "0") ";VALIDAR=" (if (boundp 'c:GM_VALIDAR_PLANTILLA) "1" "0") ";MARCAR=" (if (boundp 'c:GM_MARCAR_PLANTILLA) "1" "0") ";ROL=" (if (boundp 'c:GM_MARCAR_ROL) "1" "0") ";VERAUTO=" (vl-princ-to-string (if (boundp '*GM_VER_AUTO*) *GM_VER_AUTO* "?")) ";CHECKVER=" (if (boundp 'GM:CheckVersions) "1" "0")) *GM_SMOKE_F*) (close *GM_SMOKE_F*))
'@
  $probe = $probeTemplate.Replace('@LOG@', $Smoke.Replace('\', '/')).Replace('@N@', [string]$modules.Count)
  $lines.Add($probe.TrimEnd())
  $lines.Add('_.QUIT')
  $lines.Add('_No')
  $lines.Add('_.QUIT')
  $lines.Add('_No')
} else {
  $lines.Add('(princ "\nGM LISTO: GM_VALIDAR_PLANTILLA / GM_MARCAR_PLANTILLA / GM_MARCAR_ROL")')
  $lines.Add('(princ "\nGuarde su DWG cuando termine de marcar los roles.")')
  $lines.Add('(princ)')
}
# Sin QUIT en modo normal: el script termina y AutoCAD queda abierto.

$scrPath = Join-Path $env:TEMP ('GM_ABRIR_PLANTILLA_' + [System.Guid]::NewGuid().ToString('N').Substring(0, 8) + '.scr')
[System.IO.File]::WriteAllText($scrPath, ($lines -join "`r`n") + "`r`n", [System.Text.Encoding]::Default)

$arguments = ''
if ($dwgPath) { $arguments = '"' + $dwgPath + '" ' }
$arguments = $arguments + '/b "' + $scrPath + '"'
$workingDir = if ($dwgPath) { Split-Path -Parent $dwgPath } else { $script:ProjectRoot }
$process = Start-Process -FilePath $acadPath -ArgumentList $arguments -WorkingDirectory $workingDir -PassThru

if ($Smoke) {
  Write-Output ('SMOKE;PID=' + $process.Id + ';MODULOS=' + $modules.Count + ';SCR=' + $scrPath)
} else {
  $target = if ($dwgPath) { $dwgPath } else { '(sin dibujo; abralo desde AutoCAD)' }
  Write-Output ('ABRIR_PLANTILLA;OK;PID=' + $process.Id + ';MODULOS=' + $modules.Count)
  Write-Output ('DWG=' + $target)
  Write-Output 'AutoCAD queda abierto: ejecute GM_VALIDAR_PLANTILLA / GM_MARCAR_PLANTILLA / GM_MARCAR_ROL.'
}
