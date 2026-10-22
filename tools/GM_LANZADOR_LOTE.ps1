param(
  [switch]$SelfTest,
  [switch]$EnableLocalDumps,
  [switch]$UiSmokeTest,
  [switch]$PilotTest,
  [switch]$StartupSmoke
)

$ErrorActionPreference = 'Stop'
$script:ProjectRoot = Split-Path -Parent $PSScriptRoot
$script:StateName = 'GM_PLANOS_LOTE_LANZADOR.json'
$script:ReportName = 'GM_PLANOS_LOTE_REPORTE.csv'
$script:ProgressName = 'GM_PLANOS_LOTE_PROGRESO.csv'
$script:ForensicName = 'GM_PLANOS_LOTE_FORENSE.csv'
$script:PartialName = 'GM_PLANOS_LOTE_PENDIENTES_PARCIAL.csv'
$script:PendingName = 'GM_PLANOS_LOTE_PENDIENTES.csv'
$script:ConfigName = 'GM_PLANOS_LOTE_CONFIG.csv'
$script:MonitorName = 'GM_PLANOS_LOTE_MONITOR.csv'
$script:LogName = 'GM_PLANOS_LOTE_LANZADOR.log'
$script:StateDirName = '_sistema'
$script:ExpectedStamps = @{
  'MENU_PLANOS.lsp' = '2026-09-30-FIX3'
  'PLANO_UBICACION.lsp' = '2026-10-10-CHG3'
  'LAYOUT_UBICACION.lsp' = '2026-10-10-CHG3'
  'AUTOMATIZACION.lsp' = '2026-10-21-FIX1'
}
# AUD3 2026-10-06: el master oficial del proyecto se compara por tamano y hash
# exactos. Otra plantilla (2a zona, otro nombre) se acepta por roles: solo se
# exige el hash dorado cuando el archivo se llama igual que el master oficial.
$script:GoldenTemplateName = 'PLANTILLA_ORIGINAL_HUANCASANCOS.dwg'
$script:GoldenTemplateSize = 6875601
$script:GoldenTemplateHash = '2875910693DB5313986459820A74026115945AFF7ED3A7968707B5B076A5423D'

# SYS1 2026-10-05: los archivos de trabajo del lote (progreso, sidecar,
# manifiesto, parcial, forense, monitor y log) viven en la subcarpeta
# _sistema de la salida; arriba quedan planos + REPORTE + PENDIENTES.
function Get-GmStateDir([string]$OutputFolder) {
  return (Join-Path $OutputFolder $script:StateDirName)
}

function Ensure-GmStateDir([string]$OutputFolder) {
  $dir = Get-GmStateDir $OutputFolder
  if (-not (Test-Path -LiteralPath $dir -PathType Container)) {
    New-Item -ItemType Directory -Path $dir | Out-Null
  }
  return $dir
}

function Get-GmStatePath([string]$OutputFolder, [string]$Name) {
  return (Join-Path (Ensure-GmStateDir $OutputFolder) $Name)
}

Add-Type -AssemblyName Microsoft.VisualBasic

function Get-GmTextEncoding([string]$Path) {
  $bytes = [System.IO.File]::ReadAllBytes($Path)
  if ($bytes.Length -ge 3 -and $bytes[0] -eq 0xEF -and $bytes[1] -eq 0xBB -and $bytes[2] -eq 0xBF) {
    return [System.Text.Encoding]::UTF8
  }
  # FASE1 2026-10-21: Excel "Texto Unicode" (CSV en UTF-16 LE con BOM) es un
  # caso real: asi exporta el padron el cliente. Antes se caia en ANSI y el
  # archivo se leia con un caracter nulo entre cada letra; el codigo del
  # predio quedaba corrupto y el lote no encontraba nada. Ahora se detecta
  # el BOM (LE y BE) y se usa la codificacion correcta.
  if ($bytes.Length -ge 2 -and $bytes[0] -eq 0xFF -and $bytes[1] -eq 0xFE) {
    return [System.Text.Encoding]::Unicode
  }
  if ($bytes.Length -ge 2 -and $bytes[0] -eq 0xFE -and $bytes[1] -eq 0xFF) {
    return [System.Text.Encoding]::BigEndianUnicode
  }
  try {
    $strictUtf8 = New-Object System.Text.UTF8Encoding($false, $true)
    [void]$strictUtf8.GetString($bytes)
    return $strictUtf8
  } catch {
    return [System.Text.Encoding]::Default
  }
}

function Get-GmFieldCount([string]$Path, [System.Text.Encoding]$Encoding, [string]$Delimiter) {
  $parser = [Microsoft.VisualBasic.FileIO.TextFieldParser]::new($Path, $Encoding, $true)
  try {
    $parser.TextFieldType = [Microsoft.VisualBasic.FileIO.FieldType]::Delimited
    $parser.SetDelimiters($Delimiter)
    $parser.HasFieldsEnclosedInQuotes = $true
    $parser.TrimWhiteSpace = $false
    $score = 0
    $records = 0
    while (-not $parser.EndOfData -and $records -lt 5) {
      $fields = $parser.ReadFields()
      if ($null -ne $fields -and @($fields | Where-Object { $_ -ne '' }).Count -gt 0) {
        $score += $fields.Length
        $records++
      }
    }
    return $score
  } catch {
    return 0
  } finally {
    $parser.Close()
  }
}

function Read-GmCsv([string]$Path) {
  if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { throw "No existe el CSV: $Path" }
  $encoding = Get-GmTextEncoding $Path
  $semiScore = Get-GmFieldCount $Path $encoding ';'
  $commaScore = Get-GmFieldCount $Path $encoding ','
  $delimiter = if ($semiScore -ge $commaScore) { ';' } else { ',' }
  $parser = [Microsoft.VisualBasic.FileIO.TextFieldParser]::new($Path, $encoding, $true)
  $rows = New-Object 'System.Collections.Generic.List[object]'
  $codes = @{}
  try {
    $parser.TextFieldType = [Microsoft.VisualBasic.FileIO.FieldType]::Delimited
    $parser.SetDelimiters($delimiter)
    $parser.HasFieldsEnclosedInQuotes = $true
    $parser.TrimWhiteSpace = $false
    while (-not $parser.EndOfData) {
      $fields = $parser.ReadFields()
      if ($null -eq $fields -or @($fields | Where-Object { $_ -ne '' }).Count -eq 0) { continue }
      if ($fields.Length -lt 19) {
        throw "CSV invalido: registro con $($fields.Length) columnas; se requieren 19."
      }
      $code = ([string]$fields[14]).Trim()
      if ($code -eq '' -or $code -ieq 'CODIGO') { continue }
      if ($code -notmatch '^[A-Za-z0-9_-]+$') {
        throw "Codigo no compatible con el contrato del lote: '$code'."
      }
      if ($codes.ContainsKey($code)) {
        # F2: el duplicado se ignora como siempre, pero queda en el log (antes
        # era silencioso y un CSV con codigo repetido parecia mas corto).
        Add-GmLog ("CSV: codigo duplicado ignorado: " + $code)
        continue
      }
      $codes[$code] = $true
      $rows.Add([pscustomobject]@{ Code = $code; Fields = $fields })
    }
  } finally {
    $parser.Close()
  }
  if ($rows.Count -eq 0) { throw 'El CSV no contiene codigos validos en la columna 15.' }
  return [pscustomobject]@{ Rows = @($rows.ToArray()); Codes = @($rows | ForEach-Object { $_.Code }); Delimiter = $delimiter }
}

function Get-GmStamp([string]$FileName) {
  $path = Join-Path $script:ProjectRoot $FileName
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { throw "Falta el modulo $FileName" }
  $text = [System.IO.File]::ReadAllText($path)
  $symbol = switch ($FileName) {
    'MENU_PLANOS.lsp' { 'MENU' }
    'PLANO_UBICACION.lsp' { 'PLANO' }
    'LAYOUT_UBICACION.lsp' { 'LAYOUT' }
    'AUTOMATIZACION.lsp' { 'AUTO' }
  }
  $pattern = '\(setq\s+\*GM_VER_' + $symbol + '\*\s+"([^"]+)"\)'
  $match = [regex]::Match($text, $pattern)
  if (-not $match.Success) { throw "No se pudo leer el sello de $FileName" }
  return $match.Groups[1].Value
}

# PRY2 2026-10-13: fuente logica de una familia LISP (ancla + modulos del
# manifiesto). La usa Test-GmStamps para leer GM_VER_REQUIRED del AUTO real.
function Get-GmLogicalSource([string]$HomeName) {
  $rootFile = Join-Path $script:ProjectRoot $HomeName
  $manifest = Join-Path $script:ProjectRoot 'lisp\MANIFIESTO_MODULOS.txt'
  if (-not (Test-Path -LiteralPath $manifest -PathType Leaf)) { return [System.IO.File]::ReadAllText($rootFile) }
  $parts = New-Object 'System.Collections.Generic.List[string]'
  foreach ($raw in [System.IO.File]::ReadAllLines($manifest)) {
    $line = $raw.Trim()
    if ($line -eq '' -or $line.StartsWith('#')) { continue }
    $cols = $line -split '\|'
    if ($cols.Count -ne 2) { continue }
    if ($cols[0].Trim() -ne $HomeName) { continue }
    $rel = $cols[1].Trim().Replace('/', '\')
    $parts.Add([System.IO.File]::ReadAllText((Join-Path $script:ProjectRoot $rel)))
  }
  if ($parts.Count -eq 0) { return [System.IO.File]::ReadAllText($rootFile) }
  return ($parts -join "`r`n")
}

function Test-GmStamps {
  $actual = @{}
  foreach ($name in $script:ExpectedStamps.Keys) {
    $actual[$name] = Get-GmStamp $name
    if ($actual[$name] -ne $script:ExpectedStamps[$name]) {
      throw "Version mezclada en ${name}: esperado $($script:ExpectedStamps[$name]), encontrado $($actual[$name])."
    }
  }
  # PRY2: la tabla GM_VER_REQUIRED vive en los modulos del AUTO; se lee la
  # fuente logica (ancla + modulos del manifiesto), no solo el ancla raiz.
  $auto = Get-GmLogicalSource 'AUTOMATIZACION.lsp'
  foreach ($pair in @(
    '"*GM_VER_MENU*" . "2026-09-30-FIX3"',
    '"*GM_VER_PLANO*" . "2026-10-10-CHG3"',
    '"*GM_VER_LAYOUT*" . "2026-10-10-CHG3"',
    '"*GM_VER_AUTO*" . "2026-10-21-FIX1"'
  )) {
    if (-not $auto.Contains($pair)) { throw "GM_VER_REQUIRED no corresponde a los cuatro sellos: $pair" }
  }
  return $actual
}

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

function Get-GmCodesSet($Codes) {
  $set = @{}
  foreach ($code in @($Codes)) {
    if ($null -ne $code -and ([string]$code).Trim() -ne '') { $set[([string]$code).Trim()] = $true }
  }
  return $set
}

function Test-GmSet([hashtable]$Set, [string]$Code) {
  return ($null -ne $Code -and $Set.ContainsKey($Code.Trim()))
}

function Add-GmSet([hashtable]$Set, [string]$Code) {
  if ($null -ne $Code -and $Code.Trim() -ne '') { $Set[$Code.Trim()] = $true }
}

function Get-GmProgressCodes([string]$OutputFolder) {
  $path = Get-GmStatePath $OutputFolder $script:ProgressName
  $set = @{}
  if (Test-Path -LiteralPath $path -PathType Leaf) {
    foreach ($line in [System.IO.File]::ReadAllLines($path, [System.Text.Encoding]::Default)) {
      $code = $line.Trim()
      if ($code -ne '') { Add-GmSet $set $code }
    }
  }
  return $set
}

function Add-GmProgressCode([string]$OutputFolder, [string]$Code) {
  $path = Get-GmStatePath $OutputFolder $script:ProgressName
  [System.IO.File]::AppendAllText($path, $Code + "`r`n", [System.Text.Encoding]::ASCII)
}

function Remove-GmProgressCode([string]$OutputFolder, [string]$Code) {
  $path = Get-GmStatePath $OutputFolder $script:ProgressName
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { return }
  $kept = @([System.IO.File]::ReadAllLines($path, [System.Text.Encoding]::Default) | Where-Object { $_.Trim() -ine $Code.Trim() -and $_.Trim() -ne '' })
  $tmp = $path + '.tmp'
  [System.IO.File]::WriteAllLines($tmp, $kept, [System.Text.Encoding]::ASCII)
  [System.IO.File]::Replace($tmp, $path, $null)
}

function Get-GmOutcomes([string]$OutputFolder) {
  $path = Join-Path $OutputFolder $script:ReportName
  # F7: REPORTE/PENDIENTES los escribe el LISP con open/write-line (ANSI local),
  # por eso se leen con Encoding Default (ANSI) y PROGRESO con ASCII. Forzar
  # UTF8 aqui corromperia tildes en locales espanoles.
  $latest = @{}
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { return $latest }
  foreach ($line in [System.IO.File]::ReadAllLines($path, [System.Text.Encoding]::Default)) {
    if (-not $line.StartsWith('RESULTADO;')) { continue }
    $parts = $line.Split(';')
    if ($parts.Length -lt 5 -or $parts[1].Trim() -eq '') { continue }
    $code = $parts[1].Trim()
    $count = 1
    if ($latest.ContainsKey($code)) { $count = [int]$latest[$code].Count + 1 }
    # F2: MENSAJE puede contener ';'; ARCHIVO es el ultimo campo (las rutas
    # validadas por el preflight no llevan ';'). Se une 4..ultimo-1.
    $last = $parts.Length - 1
    $file = if ($last -ge 5) { $parts[$last].Trim() } else { '' }
    $message = if ($last -ge 5) { ($parts[4..($last - 1)] -join ';').Trim() } else { $parts[4].Trim() }
    $latest[$code] = [pscustomobject]@{
      Code = $code
      Status = $parts[3].Trim().ToUpperInvariant()
      Count = $count
      Message = $message
      File = $file
    }
  }
  return $latest
}

function Test-GmReportAllOk($Outcomes, [string]$OutputFolder) {
  $report = Join-Path $OutputFolder $script:ReportName
  if (-not (Test-Path -LiteralPath $report -PathType Leaf)) { return $false }
  $hasSummary = $false
  foreach ($line in [System.IO.File]::ReadAllLines($report, [System.Text.Encoding]::Default)) {
    if ($line.StartsWith('ETAPA;LOTE;RESUMEN_LOTE;')) { $hasSummary = $true; break }
  }
  if (-not $hasSummary -or $script:Codes.Count -eq 0) { return $false }
  foreach ($code in $script:Codes) {
    if (-not $Outcomes.ContainsKey($code) -or $Outcomes[$code].Status -ne 'OK') { return $false }
    if (-not (Test-Path -LiteralPath (Get-GmOutputDwg $OutputFolder $code) -PathType Leaf)) { return $false }
  }
  return $true
}

function Get-GmStageSummary([string]$OutputFolder) {
  $path = Join-Path $OutputFolder $script:ReportName
  $summary = @{}
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { return $summary }
  foreach ($line in [System.IO.File]::ReadAllLines($path, [System.Text.Encoding]::Default)) {
    if (-not $line.StartsWith('ETAPA;')) { continue }
    $parts = $line.Split(';')
    if ($parts.Length -lt 5 -or $parts[1].Trim() -eq 'LOTE' -or $parts[1].Trim() -eq '') { continue }
    $code = $parts[1].Trim()
    if (-not $summary.ContainsKey($code)) { $summary[$code] = @{ FirstError = $null; Predio = $null } }
    $stage = $parts[2].Trim()
    $status = $parts[3].Trim().ToUpperInvariant()
    $lastMessageField = $parts.Length - 1
    while ($lastMessageField -gt 4 -and $parts[$lastMessageField] -eq '') { $lastMessageField-- }
    $message = ($parts[4..$lastMessageField] -join ';').Trim()
    if ($status -eq 'ERROR' -and $null -eq $summary[$code].FirstError) {
      $summary[$code].FirstError = [pscustomobject]@{ Stage = $stage; Message = $message }
    }
    if ($stage -eq 'PREDIO_LOCALIZACION') {
      $summary[$code].Predio = [pscustomobject]@{ Status = $status; Message = $message }
    }
  }
  return $summary
}

function ConvertTo-GmPendingField([string]$Value) {
  if ($null -eq $Value) { return '' }
  return ([string]$Value).Replace(';', ',').Replace("`r", ' ').Replace("`n", ' ').Trim()
}

function Write-GmPendings([string]$OutputFolder) {
  if (-not $script:Codes -or $script:Codes.Count -eq 0) { return }
  $progress = Get-GmProgressCodes $OutputFolder
  $outcomes = Get-GmOutcomes $OutputFolder
  $stages = Get-GmStageSummary $OutputFolder
  # AUD4: las filas que el LISP ya registro (ERROR de un codigo que murio sin
  # RESULTADO, o el AVISO del predio) se conservan tal cual; antes el lanzador
  # las reemplazaba por un NO_INTENTADO generico y se perdia el motivo real.
  $existing = @{}
  $pendingPath = Join-Path $OutputFolder $script:PendingName
  if (Test-Path -LiteralPath $pendingPath -PathType Leaf) {
    foreach ($line in [System.IO.File]::ReadAllLines($pendingPath, [System.Text.Encoding]::Default)) {
      $parts = $line.Split(';')
      if ($parts.Length -lt 2) { continue }
      $existingCode = $parts[0].Trim()
      if ($existingCode -eq '' -or $existingCode -ieq 'CODIGO') { continue }
      if (-not $existing.ContainsKey($existingCode)) { $existing[$existingCode] = $line.Trim() }
    }
  }
  $lines = New-Object 'System.Collections.Generic.List[string]'
  $lines.Add('CODIGO;ETAPA;ESTADO;MOTIVO;ARCHIVO')
  foreach ($code in $script:Codes) {
    $outcome = if ($outcomes.ContainsKey($code)) { $outcomes[$code] } else { $null }
    $stageInfo = if ($stages.ContainsKey($code)) { $stages[$code] } else { $null }
    $arch = if (Test-Path -LiteralPath (Get-GmOutputDwg $OutputFolder $code) -PathType Leaf) { Get-GmOutputDwg $OutputFolder $code } else { '' }
    # CHG2-FIX 2026-10-07: la llamada al comando debe ir entre parentesis; sin
    # ellos PowerShell ligaba el -or como argumento de Test-GmSet y completeOk
    # quedaba False aunque el RESULTADO fuera OK (PENDIENTES conservaba filas
    # NO_INTENTADO de una interrupcion previa).
    $completeOk = (Test-GmSet $progress $code) -or ($outcome -and $outcome.Status -eq 'OK' -and $arch)
    if ($completeOk) {
      if ($stageInfo -and $stageInfo.Predio -and $stageInfo.Predio.Status -eq 'AVISO') {
        $lines.Add((ConvertTo-GmPendingField $code) + ';PREDIO_LOCALIZACION;AVISO;' +
          (ConvertTo-GmPendingField $stageInfo.Predio.Message) + ';' + (ConvertTo-GmPendingField $arch))
      }
      continue
    }
    if ($outcome -and $outcome.Status -eq 'ERROR') {
      $stage = if ($stageInfo -and $stageInfo.FirstError) { $stageInfo.FirstError.Stage } else { 'FIN' }
      $reason = if ($outcome.Message) { $outcome.Message } elseif ($stageInfo -and $stageInfo.FirstError) { $stageInfo.FirstError.Message } else { 'El codigo termino en ERROR.' }
      $lines.Add((ConvertTo-GmPendingField $code) + ';' + (ConvertTo-GmPendingField $stage) + ';ERROR;' +
        (ConvertTo-GmPendingField $reason) + ';' + (ConvertTo-GmPendingField $arch))
      continue
    }
    if ($outcome -and $outcome.Status -eq 'OK' -and -not $arch) {
      $lines.Add((ConvertTo-GmPendingField $code) + ';FIN;ERROR;RESULTADO OK sin DWG verificable;')
      continue
    }
    if ($stageInfo -and $stageInfo.Predio -and $stageInfo.Predio.Status -eq 'AVISO') {
      $lines.Add((ConvertTo-GmPendingField $code) + ';PREDIO_LOCALIZACION;AVISO;' +
        (ConvertTo-GmPendingField $stageInfo.Predio.Message) + ';' + (ConvertTo-GmPendingField $arch))
      continue
    }
    if ($existing.ContainsKey($code)) { $lines.Add($existing[$code]); continue }
    $lines.Add((ConvertTo-GmPendingField $code) + ';NO_INTENTADO;AVISO;corrida interrumpida, codigo no intentado;')
  }
  $path = Join-Path $OutputFolder $script:PendingName
  $tmp = $path + '.tmp'
  [System.IO.File]::WriteAllLines($tmp, $lines.ToArray(), [System.Text.Encoding]::Default)
  if (Test-Path -LiteralPath $path -PathType Leaf) {
    # AUD3: el reemplazo deja respaldo .bak dentro de _sistema (fuera de la
    # entrega); antes se reemplazaba sin red y un corte dejaba el PENDIENTES
    # anterior irrecuperable.
    $backup = Join-Path (Ensure-GmStateDir $OutputFolder) ($script:PendingName + '.bak')
    try { [System.IO.File]::Replace($tmp, $path, $backup) }
    catch { Move-Item -LiteralPath $tmp -Destination $path -Force }
  } else {
    Move-Item -LiteralPath $tmp -Destination $path
  }
}

function Get-GmOutputDwg([string]$OutputFolder, [string]$Code) {
  $safe = [regex]::Replace($Code, '[^A-Za-z0-9_-]', '_')
  if ($safe -eq '') { $safe = 'SIN_CODIGO' }
  return (Join-Path $OutputFolder ($safe + '.dwg'))
}

# F3: un DWG de 0 bytes/truncado no cuenta como generado. Exige >=1 KB y la
# cabecera binaria de AutoCAD ("AC1"); espejo de GM:AutoDwgValid del LISP.
function Test-GmDwgFile([string]$Path) {
  if ([string]::IsNullOrWhiteSpace($Path) -or -not (Test-Path -LiteralPath $Path -PathType Leaf)) { return $false }
  try {
    $fi = Get-Item -LiteralPath $Path
    if ($fi.Length -lt 1024) { return $false }
    $fs = [System.IO.File]::OpenRead($Path)
    try {
      $buf = New-Object byte[] 4
      [void]$fs.Read($buf, 0, 4)
      return ($buf[0] -eq 0x41 -and $buf[1] -eq 0x43 -and $buf[2] -eq 0x31)
    } finally { $fs.Close() }
  } catch { return $false }
}

function Save-GmState {
  if (-not $script:State -or -not $script:State.OutputFolder) { return }
  $path = Get-GmStatePath $script:State.OutputFolder $script:StateName
  $tmp = $path + '.tmp'
  $json = $script:State | ConvertTo-Json -Depth 10
  [System.IO.File]::WriteAllText($tmp, $json, (New-Object System.Text.UTF8Encoding($false)))
  if (Test-Path -LiteralPath $path -PathType Leaf) {
    # F5: reemplazo atomico con respaldo .bak; un corte a mitad de escritura ya
    # no deja el manifiesto ausente/corrupto (antes Move -Force sin red).
    try {
      [System.IO.File]::Replace($tmp, $path, ($path + '.bak'))
    } catch {
      Move-Item -LiteralPath $tmp -Destination $path -Force
    }
  } else {
    Move-Item -LiteralPath $tmp -Destination $path
  }
}

function Read-GmState([string]$OutputFolder) {
  $path = Get-GmStatePath $OutputFolder $script:StateName
  $backup = $path + '.bak'
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) {
    if (Test-Path -LiteralPath $backup -PathType Leaf) {
      Add-GmLog 'Manifiesto del lanzador ausente; se recupera desde el respaldo .bak.'
      $path = $backup
    } else {
      throw 'No existe una corrida del lanzador para reanudar.'
    }
  }
  $obj = $null
  try {
    $obj = [System.IO.File]::ReadAllText($path, [System.Text.Encoding]::UTF8) | ConvertFrom-Json
  } catch {
    if ($path -eq $backup -or -not (Test-Path -LiteralPath $backup -PathType Leaf)) {
      throw "El manifiesto del lanzador no se puede leer: $($_.Exception.Message)"
    }
    # AUD3: F5 dejo el respaldo .bak; si el manifiesto principal quedo truncado
    # por un corte, la reanudacion ya no pierde la corrida.
    Add-GmLog ('Manifiesto corrupto; se recupera desde el respaldo .bak. Detalle: ' + $_.Exception.Message)
    $obj = [System.IO.File]::ReadAllText($backup, [System.Text.Encoding]::UTF8) | ConvertFrom-Json
  }
  $state = @{}
  foreach ($property in $obj.PSObject.Properties) { $state[$property.Name] = $property.Value }
  if ([int]$state.Schema -ne 1) { throw 'El archivo de estado pertenece a otra version del lanzador.' }
  # AUD3: la bandera muerta Initialized se poda y el testigo real
  # InitializedBeforeFirstBlock migra a $true en manifiestos anteriores: un
  # manifiesto existente implica que ya hubo un primer bloque y el REPORTE no
  # debe truncarse al reanudar.
  if ($state.ContainsKey('Initialized')) { $state.Remove('Initialized') }
  if ($null -eq $state['InitializedBeforeFirstBlock']) { $state['InitializedBeforeFirstBlock'] = $true }
  # SYS1: los manifiestos previos traen Monitor; el modo diagnostico hereda ese valor.
  if ($null -eq $state['Diag']) { $state['Diag'] = [bool]$state['Monitor'] }
  # CHG2: manifiestos anteriores al rotulo DEP/PROV/DIST heredan el default activo.
  if ($null -eq $state['ShowGeoTxt']) { $state['ShowGeoTxt'] = $true }
  # PRY1: manifiestos anteriores al tipo de plano heredan los defaults
  # historicos (UBICACION / NORMAL), que es como corrieron.
  if ($null -eq $state['PlanType']) { $state['PlanType'] = 'UBICACION' }
  if ($null -eq $state['VpType']) { $state['VpType'] = '1' }
  $state['PlanType'] = Get-GmValidPlanType $state['PlanType']
  $state['VpType'] = Get-GmValidVpType $state['VpType']
  if ($null -eq $state.CrashCounts) { $state.CrashCounts = @{} }
  elseif ($state.CrashCounts -isnot [hashtable]) {
    $crashes = @{}
    foreach ($property in $state.CrashCounts.PSObject.Properties) { $crashes[$property.Name] = [int]$property.Value }
    $state.CrashCounts = $crashes
  }
  foreach ($name in @('Attempted', 'Failed', 'Retried', 'CurrentBatch', 'BatchBaseline')) {
    if ($null -eq $state[$name]) { $state[$name] = @() }
  }
  return $state
}

function Archive-GmRunState([string]$OutputFolder) {
  # SYS1: archiva la subcarpeta _sistema completa y, por compatibilidad, los
  # archivos planos de corridas anteriores a este sello.
  $stateDir = Get-GmStateDir $OutputFolder
  $legacyNames = @(
    $script:ReportName, $script:ProgressName, $script:ForensicName,
    $script:PartialName, $script:PendingName, $script:ConfigName, $script:MonitorName,
    $script:LogName, $script:StateName, ($script:StateName + '.tmp'), ($script:StateName + '.bak')
  )
  # AUD3: el preflight crea un _sistema vacio (prueba de escritura); solo cuenta
  # como estado previo si tiene contenido real, para no generar historiales
  # vacios al pulsar "Empezar desde cero".
  $hasStateDir = (Test-Path -LiteralPath $stateDir -PathType Container) -and
    (@(Get-ChildItem -LiteralPath $stateDir -Force -ErrorAction SilentlyContinue).Count -gt 0)
  $legacy = @($legacyNames | Where-Object { Test-Path -LiteralPath (Join-Path $OutputFolder $_) -PathType Leaf })
  if (-not $hasStateDir -and $legacy.Count -eq 0) { return 'sin archivos previos' }
  # F7: milisegundos en el nombre; dos "Empezar desde cero" en el mismo segundo
  # ya no colisionan con New-Item.
  $archiveName = '_historial_lote_' + (Get-Date).ToString('yyyyMMdd_HHmmss_fff')
  $archive = Join-Path $OutputFolder $archiveName
  New-Item -ItemType Directory -Path $archive | Out-Null
  if ($hasStateDir) { Move-Item -LiteralPath $stateDir -Destination (Join-Path $archive $script:StateDirName) }
  foreach ($name in $legacyNames) {
    $path = Join-Path $OutputFolder $name
    if (Test-Path -LiteralPath $path -PathType Leaf) { Move-Item -LiteralPath $path -Destination $archive }
  }
  # F7: poda de historiales (conserva los 10 mas recientes) para que la carpeta
  # de salida no crezca sin limite entre corridas.
  $hist = @(Get-ChildItem -LiteralPath $OutputFolder -Directory -Filter '_historial_lote_*' -ErrorAction SilentlyContinue |
    Sort-Object Name -Descending)
  if ($hist.Count -gt 10) {
    foreach ($old in $hist[10..($hist.Count - 1)]) {
      if (Remove-GmBlockDirectory $old.FullName) { Add-GmLog ('Historial antiguo podado: ' + $old.Name) }
    }
  }
  return $archive
}

function Write-GmMonitorRow([string]$Event, $Process, [string]$OutputFolder) {
  if (-not $script:State.Diag) { return }
  $path = Get-GmStatePath $OutputFolder $script:MonitorName
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) {
    [System.IO.File]::WriteAllText($path, "HORA;PID;PRIV_MB;WS_MB;HANDLES;THREADS;CPU_S;CPU_DELTA_S;EVENTO`r`n", [System.Text.Encoding]::ASCII)
  }
  $now = (Get-Date).ToString('yyyy-MM-dd HH:mm:ss')
  if ($null -eq $Process) {
    $line = "$now;0;0;0;0;0;0;0;$Event`r`n"
  } else {
    $Process.Refresh()
    $pidValue = $Process.Id
    $priv = [math]::Round($Process.PrivateMemorySize64 / 1MB, 1)
    $ws = [math]::Round($Process.WorkingSet64 / 1MB, 1)
    $handles = $Process.HandleCount
    $threads = $Process.Threads.Count
    $cpu = [math]::Round($Process.TotalProcessorTime.TotalSeconds, 1)
    $delta = 0
    if ($script:CpuPrevious.ContainsKey([string]$pidValue)) { $delta = [math]::Round($cpu - $script:CpuPrevious[[string]$pidValue], 1) }
    $script:CpuPrevious[[string]$pidValue] = $cpu
    $line = "$now;$pidValue;$priv;$ws;$handles;$threads;$cpu;$delta;$Event`r`n"
  }
  [System.IO.File]::AppendAllText($path, $line, [System.Text.Encoding]::ASCII)
}

function Add-GmLog([string]$Message) {
  $line = (Get-Date).ToString('HH:mm:ss') + '  ' + $Message
  if ($script:logList) {
    [void]$script:logList.Items.Add($line)
    while ($script:logList.Items.Count -gt 250) { $script:logList.Items.RemoveAt(0) }
    $script:logList.TopIndex = [math]::Max(0, $script:logList.Items.Count - 1)
  }
  if ($script:State -and $script:State.OutputFolder -and $script:State.Diag -and (Test-Path -LiteralPath $script:State.OutputFolder -PathType Container)) {
    try { [System.IO.File]::AppendAllText((Get-GmStatePath $script:State.OutputFolder $script:LogName), $line + "`r`n", [System.Text.Encoding]::UTF8) } catch { }
  }
}

function Get-GmConfigFromUi {
  return @{
    CsvPath = $script:txtCsv.Text.Trim()
    TemplatePath = $script:txtTemplate.Text.Trim()
    OutputFolder = $script:txtOutput.Text.Trim()
    AcadPath = $script:txtAcad.Text.Trim()
    BlockSize = [int]$script:numBlock.Value
    InnerChunk = [int]$script:numChunk.Value
    StallMinutes = [int]$script:numStall.Value
    MakePdf = [bool]$script:radioPdf.Checked
    ShowPredio = [bool]$script:checkPredio.Checked
    ShowGeoTxt = [bool]$script:checkGeo.Checked
    Diag = [bool]$script:checkDiag.Checked
    RetryFailures = [bool]$script:checkRetry.Checked
    StallKill = [bool]$script:checkStallKill.Checked
    # PRY1/F2: tipo de plano y datos de VP de la grilla.
    PlanType = Get-GmValidPlanType $(if ($script:comboPlanType) { $script:comboPlanType.SelectedItem } else { '' })
    VpType = $(if ($script:comboVpType -and [string]$script:comboVpType.SelectedItem -eq 'UBICACION') { '2' } else { '1' })
  }
}

function Test-GmConfig($Config) {
  foreach ($entry in @(
    @($Config.CsvPath, 'CSV'), @($Config.TemplatePath, 'DWG plantilla'),
    @($Config.OutputFolder, 'carpeta de salida'), @($Config.AcadPath, 'acad.exe')
  )) {
    if ([string]$entry[0] -match '[;"\r\n]') { throw "La ruta de $($entry[1]) contiene un caracter no admitido (; o comillas)." }
  }
  if (-not (Test-Path -LiteralPath $Config.CsvPath -PathType Leaf)) { throw 'Selecciona un CSV existente.' }
  if (-not (Test-Path -LiteralPath $Config.TemplatePath -PathType Leaf)) { throw 'Selecciona una plantilla DWG existente.' }
  if (-not (Test-Path -LiteralPath $Config.OutputFolder -PathType Container)) { throw 'Selecciona una carpeta de salida existente.' }
  if (-not (Test-Path -LiteralPath $Config.AcadPath -PathType Leaf) -or [IO.Path]::GetFileName($Config.AcadPath) -ine 'acad.exe') {
    throw 'Selecciona el ejecutable acad.exe de Civil 3D/AutoCAD.'
  }
  # PRY1: normaliza tipo de plano y VP al catalogo valido (defaults historicos).
  $Config.PlanType = Get-GmValidPlanType $Config.PlanType
  $Config.VpType = Get-GmValidVpType $Config.VpType
  $running = @(Get-Process -Name acad -ErrorAction SilentlyContinue)
  if ($running.Count -gt 0) { throw 'Cierra todas las ventanas de AutoCAD/Civil 3D antes de iniciar. El lanzador no usara ni cerrara sesiones ajenas.' }
  foreach ($path in @($Config.CsvPath, $Config.TemplatePath, $Config.OutputFolder, $Config.AcadPath)) {
    if ($path -match '[;"\r\n]') { throw 'Las rutas no pueden contener punto y coma o comillas.' }
  }
  $stamps = Test-GmStamps
  $csv = Read-GmCsv $Config.CsvPath
  # AUD3: la prueba de escritura se hace dentro de _sistema (la subcarpeta real
  # del estado); antes se probaba la raiz y un _sistema no-creable recien
  # fallaba al arrancar el lote.
  $stateProbeDir = Ensure-GmStateDir $Config.OutputFolder
  $tempProbe = Join-Path $stateProbeDir ('.gm_write_test_' + [guid]::NewGuid().ToString('N'))
  [System.IO.File]::WriteAllText($tempProbe, 'ok', [System.Text.Encoding]::ASCII)
  Remove-Item -LiteralPath $tempProbe -Force
  $templateHash = (Get-FileHash -LiteralPath $Config.TemplatePath -Algorithm SHA256).Hash
  # AUD3: hash dorado solo para el master oficial por nombre exacto; una 2a
  # plantilla (otra zona) sigue admitida por roles, como define SPEC_PLANTILLA.
  $templateName = [IO.Path]::GetFileName($Config.TemplatePath)
  if ($templateName -ieq $script:GoldenTemplateName) {
    $templateSize = (Get-Item -LiteralPath $Config.TemplatePath).Length
    if ($templateSize -ne $script:GoldenTemplateSize -or $templateHash -ne $script:GoldenTemplateHash) {
      throw "La plantilla $($script:GoldenTemplateName) no coincide con el master verificado (tamano/hash). Restaure la copia buena antes de lotear."
    }
  } else {
    Add-GmLog ("Plantilla no oficial '$templateName': se acepta por roles; hash de la corrida $($templateHash.Substring(0, 12))...")
  }
  return [pscustomobject]@{
    Csv = $csv
    Stamps = $stamps
    CsvHash = (Get-FileHash -LiteralPath $Config.CsvPath -Algorithm SHA256).Hash
    TemplateHash = $templateHash
  }
}

function Set-GmUiOptions($State) {
  $script:numBlock.Value = [decimal][math]::Max(1, [math]::Min(100, [int]$State.BlockSize))
  $script:numChunk.Value = [decimal][math]::Max(0, [math]::Min(100, [int]$State.InnerChunk))
  $script:numStall.Value = [decimal][math]::Max(2, [math]::Min(120, [int]$State.StallMinutes))
  $script:radioPdf.Checked = [bool]$State.MakePdf
  $script:radioDwg.Checked = -not [bool]$State.MakePdf
  $script:checkPredio.Checked = [bool]$State.ShowPredio
  $script:checkGeo.Checked = if ($null -eq $State.ShowGeoTxt) { $true } else { [bool]$State.ShowGeoTxt }
  $script:checkDiag.Checked = [bool]$State.Diag
  $script:checkRetry.Checked = [bool]$State.RetryFailures
  $script:checkStallKill.Checked = [bool]$State.StallKill
  # PRY1: tipo de plano y datos de VP del estado guardado (manda al reanudar).
  if ($script:comboPlanType) { $script:comboPlanType.SelectedItem = Get-GmValidPlanType $State.PlanType }
  if ($script:comboVpType) { $script:comboVpType.SelectedItem = $(if ((Get-GmValidVpType $State.VpType) -eq '2') { 'UBICACION' } else { 'NORMAL' }) }
}

function New-GmState($Config, $Preflight) {
  return @{
    Schema = 1
    RunId = [guid]::NewGuid().ToString('N')
    Created = (Get-Date).ToString('o')
    CsvPath = [IO.Path]::GetFullPath($Config.CsvPath)
    CsvHash = $Preflight.CsvHash
    TemplatePath = [IO.Path]::GetFullPath($Config.TemplatePath)
    TemplateHash = $Preflight.TemplateHash
    OutputFolder = [IO.Path]::GetFullPath($Config.OutputFolder)
    AcadPath = [IO.Path]::GetFullPath($Config.AcadPath)
    Total = $Preflight.Csv.Rows.Count
    BlockSize = $Config.BlockSize
    InnerChunk = $Config.InnerChunk
    StallMinutes = $Config.StallMinutes
    MakePdf = $Config.MakePdf
    ShowPredio = $Config.ShowPredio
    ShowGeoTxt = $Config.ShowGeoTxt
    # PRY1: tipo de plano efectivo de la corrida (el boot SCR lo fija en el LISP).
    PlanType = Get-GmValidPlanType $Config.PlanType
    VpType = Get-GmValidVpType $Config.VpType
    Diag = $Config.Diag
    RetryFailures = $Config.RetryFailures
    StallKill = $Config.StallKill
    Stamps = $Preflight.Stamps
    Attempted = @()
    Failed = @()
    Retried = @()
    CurrentBatch = @()
    BatchBaseline = @{}
    CrashCounts = @{}
    BatchNumber = 0
    Complete = $false
    Paused = $false
    FreshWarningLogged = $false
    Phase = 'INITIAL'
  }
}

function Assert-GmResumeMatches($Config, $Preflight, $State) {
  $same = @(
    @([IO.Path]::GetFullPath($Config.CsvPath), [string]$State.CsvPath, 'CSV'),
    @($Preflight.CsvHash, [string]$State.CsvHash, 'contenido del CSV'),
    @([IO.Path]::GetFullPath($Config.TemplatePath), [string]$State.TemplatePath, 'DWG plantilla'),
    @($Preflight.TemplateHash, [string]$State.TemplateHash, 'contenido de la plantilla'),
    @([IO.Path]::GetFullPath($Config.OutputFolder), [string]$State.OutputFolder, 'carpeta de salida'),
    @([IO.Path]::GetFullPath($Config.AcadPath), [string]$State.AcadPath, 'acad.exe')
  )
  foreach ($item in $same) {
    if (-not [string]::Equals([string]$item[0], [string]$item[1], [StringComparison]::OrdinalIgnoreCase)) {
      throw "No coincide $($item[2]) con la corrida guardada. Reanudar requiere los mismos archivos y carpeta."
    }
  }
  if ($State.Total -ne $Preflight.Csv.Rows.Count) { throw 'El numero de codigos del CSV difiere de la corrida guardada.' }
  if ($Config.BlockSize -ne $State.BlockSize -or $Config.InnerChunk -ne $State.InnerChunk -or
      $Config.StallMinutes -ne $State.StallMinutes -or $Config.Diag -ne $State.Diag -or
      $Config.MakePdf -ne $State.MakePdf -or $Config.ShowPredio -ne $State.ShowPredio -or
      $Config.ShowGeoTxt -ne $State.ShowGeoTxt -or $Config.PlanType -ne $State.PlanType -or
      $Config.VpType -ne $State.VpType -or
      $Config.RetryFailures -ne $State.RetryFailures -or $Config.StallKill -ne $State.StallKill) {
    Add-GmLog 'Se usaran las opciones guardadas en la corrida anterior.'
    Set-GmUiOptions $State
  }
}

# AUD3: el manifiesto JSON manda en el lanzador; el sidecar
# `_sistema/GM_PLANOS_LOTE_CONFIG.csv` es la fuente de GM_CONTINUAR_LOTE dentro
# de AutoCAD. Si el sidecar apunta a otro CSV que la corrida guardada, algo se
# desincronizo y la reanudacion se bloquea (fail-closed) con mensaje claro.
function Test-GmSidecarMatches($State) {
  $path = Get-GmStatePath $State.OutputFolder $script:ConfigName
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) {
    Add-GmLog 'Reanudacion: sin sidecar de configuracion; manda el manifiesto del lanzador.'
    return
  }
  $lines = [System.IO.File]::ReadAllLines($path, [System.Text.Encoding]::Default)
  if ($lines.Count -lt 2) {
    Add-GmLog 'Reanudacion: sidecar vacio; manda el manifiesto del lanzador.'
    return
  }
  $parts = $lines[1].Split(';')
  if ($parts.Count -lt 1 -or $parts[0].Trim() -eq '') { return }
  $sidecarCsv = $parts[0].Trim().Replace('\', '/')
  $stateCsv = ([string]$State.CsvPath).Replace('\', '/')
  if (-not [string]::Equals($sidecarCsv, $stateCsv, [StringComparison]::OrdinalIgnoreCase)) {
    throw "El sidecar _sistema/$($script:ConfigName) apunta a otro CSV ($($parts[0].Trim())) que la corrida guardada. Restaure el CSV original o empiece desde cero."
  }
  # PRY1: si el sidecar trae TIPO_PLANO (formato nuevo de 8 campos), debe
  # coincidir con la corrida guardada; un desajuste bloquea (fail-closed) antes
  # de mezclar estilos en la misma salida.
  if ($parts.Count -ge 7) {
    $sidecarTipoRaw = [string]$parts[6].Trim()
    if ($sidecarTipoRaw -ne '') {
      $sidecarTipo = Get-GmValidPlanType $sidecarTipoRaw
      $stateTipo = Get-GmValidPlanType $State.PlanType
      if ($sidecarTipo -ne $stateTipo) {
        throw "El sidecar _sistema/$($script:ConfigName) apunta al tipo de plano '$sidecarTipo' y la corrida guardada es '$stateTipo'. Empiece desde cero o restaure el sidecar original."
      }
    }
  }
  Add-GmLog 'Reanudacion: sidecar de configuracion coincide con el CSV de la corrida.'
}

function Update-GmStateFromFiles {
  $progress = Get-GmProgressCodes $script:State.OutputFolder
  $codeSet = Get-GmCodesSet $script:Codes
  $attempted = Get-GmCodesSet $script:State.Attempted
  $failed = Get-GmCodesSet $script:State.Failed
  foreach ($code in @($progress.Keys)) {
    if (-not (Test-GmSet $codeSet $code)) { throw "Progreso incompatible: '$code' no existe en el CSV seleccionado. No se iniciara el lote." }
    # F5: PROGRESO solo vale con un DWG real (cabecera/tamano), no con un
    # archivo de 0 bytes o truncado por un AV.
    if (-not (Test-GmDwgFile (Get-GmOutputDwg $script:State.OutputFolder $code))) {
      Remove-GmProgressCode $script:State.OutputFolder $code
      $progress.Remove($code)
      $attempted.Remove($code)
      Add-GmSet $failed $code
      Add-GmLog "DWG ausente o invalido para $code; se quito de PROGRESO para regenerarlo."
    }
  }
  $outcomes = Get-GmOutcomes $script:State.OutputFolder
  $reportAllOk = Test-GmReportAllOk $outcomes $script:State.OutputFolder
  foreach ($code in $outcomes.Keys) {
    if (-not (Test-GmSet $codeSet $code)) { continue }
    $outcome = $outcomes[$code]
    if ($outcome.Status -eq 'OK') {
      $dwg = Get-GmOutputDwg $script:State.OutputFolder $code
      if ((Test-GmDwgFile $dwg) -and -not (Test-GmSet $progress $code) -and -not $reportAllOk) {
        Add-GmProgressCode $script:State.OutputFolder $code
        Add-GmSet $progress $code
        Add-GmLog "Recuperado en PROGRESO desde RESULTADO OK + DWG valido: $code"
      }
    }
  }
  foreach ($code in $progress.Keys) {
    Add-GmSet $attempted $code
    $failed.Remove($code)
  }
  foreach ($code in $outcomes.Keys) {
    if (-not (Test-GmSet $codeSet $code)) { continue }
    # FIX4: PROGRESO + DWG presente es la fuente autoritativa de "hecho"; un
    # RESULTADO;ERROR posterior (p.ej. repetido por un reintento) NO re-encola
    # el codigo, para no crear re-trabajo invisible ni pausas seguras falsas.
    if (Test-GmSet $progress $code) { continue }
    $dwgExists = Test-GmDwgFile (Get-GmOutputDwg $script:State.OutputFolder $code)
    if ($outcomes[$code].Status -eq 'OK' -and -not $dwgExists) {
      $attempted.Remove($code)
      Add-GmSet $failed $code
    } else {
      Add-GmSet $attempted $code
      if ($outcomes[$code].Status -eq 'ERROR') { Add-GmSet $failed $code }
      elseif ($outcomes[$code].Status -eq 'OK') { $failed.Remove($code) }
    }
  }
  $script:State.Attempted = @($script:Codes | Where-Object { Test-GmSet $attempted $_ })
  $script:State.Failed = @($script:Codes | Where-Object { Test-GmSet $failed $_ })
  # FIX4: el cortacircuitos por codigo cuenta solo procesos SIN RESULTADO; si el
  # codigo ya dejo RESULTADO (OK o ERROR), el contador se reinicia.
  if ($script:State.CrashCounts -is [hashtable]) {
    foreach ($key in @($script:State.CrashCounts.Keys)) {
      if ($outcomes.ContainsKey($key)) { $script:State.CrashCounts.Remove($key) }
    }
  }
  Write-GmPendings $script:State.OutputFolder
  return [pscustomobject]@{ Progress = $progress; Outcomes = $outcomes }
}

function Get-GmOutcomeCounts($Outcomes) {
  $counts = @{}
  foreach ($code in $Outcomes.Keys) { $counts[$code] = [int]$Outcomes[$code].Count }
  return $counts
}

function Get-GmPendingCurrentBatch($Outcomes, $Progress) {
  $pending = New-Object 'System.Collections.Generic.List[string]'
  $baseline = $script:State.BatchBaseline
  if ($baseline -isnot [hashtable]) {
    $converted = @{}
    if ($null -ne $baseline) { foreach ($p in $baseline.PSObject.Properties) { $converted[$p.Name] = [int]$p.Value } }
    $baseline = $converted
  }
  foreach ($code in @($script:State.CurrentBatch)) {
    if (Test-GmSet $Progress $code) { continue }
    $before = 0
    if ($baseline.ContainsKey($code)) { $before = [int]$baseline[$code] }
    $after = 0
    if ($Outcomes.ContainsKey($code)) { $after = [int]$Outcomes[$code].Count }
    if ($after -le $before) { $pending.Add($code) }
  }
  return @($pending.ToArray())
}

function Remove-GmBlockDirectory([string]$Directory) {
  if (-not $Directory -or -not (Test-Path -LiteralPath $Directory)) { return $true }
  for ($attempt = 0; $attempt -lt 10; $attempt++) {
    try {
      Remove-Item -LiteralPath $Directory -Recurse -Force
      if (-not (Test-Path -LiteralPath $Directory)) { return $true }
    } catch {
      Start-Sleep -Milliseconds 500
    }
  }
  return (-not (Test-Path -LiteralPath $Directory))
}

# AUD3: los %TEMP%\GM_LOTE_<RunId> que quedan tras un cierre abrupto solo se
# borraban al completar Schedule; un crash/kill dejaba huerfanos que nunca se
# podaban. Se limpian los de mas de 7 dias al abrir el lanzador.
function Clear-GmOrphanTemp {
  $removed = 0
  Get-ChildItem -LiteralPath $env:TEMP -Directory -Filter 'GM_LOTE_*' -ErrorAction SilentlyContinue |
    Where-Object { $_.LastWriteTime -lt (Get-Date).AddDays(-7) } |
    ForEach-Object { if (Remove-GmBlockDirectory $_.FullName) { $removed++ } }
  if ($removed -gt 0) { Add-GmLog "Temporales huerfanos podados: $removed carpeta(s) GM_LOTE_* de mas de 7 dias." }
}

# ============================================================================
# PRY1 2026-10-12: lanzador multi-proyecto. Cada proyecto vive en
# proyectos\<Nombre>\proyecto.json (CSV, plantilla, salida, acad y opciones).
# La biblioteca cruza los archivos de la salida con el REPORTE y el CSV del
# proyecto para listar los planos generados (tipo, formato, estado, U.C. y
# distrito). Migracion unica: los proyectos historicos de la raiz se
# registran apuntando a sus rutas existentes, sin mover archivos.
# ============================================================================
$script:ProjectsDirName = 'proyectos'
$script:ProjectFileName = 'proyecto.json'
$script:ProjectsBackupName = '_borrados'
$script:PlanTypeList = @('UBICACION', 'MATRIZ', 'DIAGNOSTICO', 'PERIMETRICO')
$script:ProjectsDirOverride = $null

function Get-GmProjectsDir {
  if ($script:ProjectsDirOverride) { return $script:ProjectsDirOverride }
  return (Join-Path $script:ProjectRoot $script:ProjectsDirName)
}

function Ensure-GmProjectsDir {
  $dir = Get-GmProjectsDir
  if (-not (Test-Path -LiteralPath $dir -PathType Container)) { New-Item -ItemType Directory -Path $dir | Out-Null }
  return $dir
}

function Get-GmProjectDir([string]$Name) {
  return (Join-Path (Get-GmProjectsDir) $Name)
}

# SAL2: la salida vive DENTRO del proyecto con su nombre: proyectos\<nombre>\
# salida_<nombre>. El campo sigue siendo editable; esto es solo el default.
function Get-GmProjectOutputPath([string]$Name) {
  return (Join-Path (Get-GmProjectDir $Name) ('salida_' + $Name))
}

# FASE1 2026-10-21: nombres reservados de Windows. Un proyecto llamado CON o
# NUL rompe la exploracion de archivos (la carpeta existe pero no se ve ni se
# puede borrar) y _borrados es el nombre del respaldo de proyectos: un
# proyecto con ese nombre se pisaria con el store de respaldos.
$script:ReservedProjectNames = @(
  'CON', 'PRN', 'AUX', 'NUL',
  'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
  'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9',
  '_borrados'
)

function Test-GmProjectName([string]$Name) {
  if ($Name -notmatch '^[A-Za-z0-9_-]+$') { return $false }
  if ($script:ReservedProjectNames -contains $Name.ToUpperInvariant()) { return $false }
  return $true
}

# PRY1: normaliza el tipo de plano (estilo) y los datos de VP del lote a los
# catalogos validos; valores vacios o fuera de catalogo caen a los defaults
# historicos (UBICACION / "1" NORMAL = comportamiento previo identico).
function Get-GmValidPlanType($Value) {
  $v = if ($null -ne $Value) { ([string]$Value).Trim().ToUpperInvariant() } else { '' }
  if ($script:PlanTypeList -contains $v) { return $v }
  return 'UBICACION'
}

function Get-GmValidVpType($Value) {
  return $(if ([string]$Value -eq '2') { '2' } else { '1' })
}

function New-GmProjectRecord([string]$Name) {
  return [ordered]@{
    Schema = 2
    Nombre = $Name
    Creado = (Get-Date).ToString('o')
    Actualizado = (Get-Date).ToString('o')
    CsvPath = ''
    TemplatePath = ''
    OutputFolder = ''
    AcadPath = ''
    PlanType = 'UBICACION'
    VpType = '1'
    MakePdf = $true
    ShowPredio = $true
    ShowGeoTxt = $true
    BlockSize = 25
    InnerChunk = 0
    StallMinutes = 15
    Diag = $false
    RetryFailures = $true
    StallKill = $false
  }
}

function Read-GmProject([string]$Name) {
  if (-not (Test-GmProjectName $Name)) { return $null }
  $path = Join-Path (Get-GmProjectDir $Name) $script:ProjectFileName
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { return $null }
  $obj = $null
  try {
    $obj = [System.IO.File]::ReadAllText($path, [System.Text.Encoding]::UTF8) | ConvertFrom-Json
  } catch {
    Add-GmLog "Proyecto '$Name': proyecto.json ilegible; se omite de la lista."
    return $null
  }
  $record = New-GmProjectRecord $Name
  foreach ($property in $obj.PSObject.Properties) {
    if ($record.Contains($property.Name) -and $null -ne $property.Value) { $record[$property.Name] = $property.Value }
  }
  # defaults de campos faltantes o corruptos (proyectos de la primera version).
  $record.Nombre = $Name
  if ($script:PlanTypeList -notcontains [string]$record.PlanType) { $record.PlanType = 'UBICACION' }
  if ([string]$record.VpType -ne '2') { $record.VpType = '1' }
  if ([int]$record.BlockSize -lt 1 -or [int]$record.BlockSize -gt 100) { $record.BlockSize = 25 }
  if ([int]$record.InnerChunk -lt 0 -or [int]$record.InnerChunk -gt 100) { $record.InnerChunk = 0 }
  if ([int]$record.StallMinutes -lt 2 -or [int]$record.StallMinutes -gt 120) { $record.StallMinutes = 15 }
  return $record
}

function Save-GmProjectRecord($Project) {
  $name = [string]$Project.Nombre
  if (-not (Test-GmProjectName $name)) { throw "Nombre de proyecto invalido: '$name'." }
  $dir = Get-GmProjectDir $name
  if (-not (Test-Path -LiteralPath $dir -PathType Container)) { New-Item -ItemType Directory -Path $dir | Out-Null }
  $Project.Actualizado = (Get-Date).ToString('o')
  $json = $Project | ConvertTo-Json -Depth 6
  $path = Join-Path $dir $script:ProjectFileName
  $tmp = $path + '.tmp'
  [System.IO.File]::WriteAllText($tmp, $json, (New-Object System.Text.UTF8Encoding($false)))
  if (Test-Path -LiteralPath $path -PathType Leaf) {
    try { [System.IO.File]::Replace($tmp, $path, $null) } catch { Move-Item -LiteralPath $tmp -Destination $path -Force }
  } else {
    Move-Item -LiteralPath $tmp -Destination $path
  }
}

function Get-GmProjectNames {
  $names = New-Object 'System.Collections.Generic.List[string]'
  $dir = Get-GmProjectsDir
  if (Test-Path -LiteralPath $dir -PathType Container) {
    foreach ($sub in [System.IO.Directory]::GetDirectories($dir)) {
      $name = [IO.Path]::GetFileName($sub)
      if ($name -ieq $script:ProjectsBackupName) { continue }
      if (Test-Path -LiteralPath (Join-Path $sub $script:ProjectFileName) -PathType Leaf) { $names.Add($name) }
    }
  }
  return @($names.ToArray() | Sort-Object)
}

# SAL2: rutas de salida viejas que SIEMPRE se migran una vez. La salida real
# no se mueve ni se borra: queda intacta y el proyecto apunta a la nueva.
function Test-GmLegacyProjectOutput([string]$Name, [string]$Path) {
  if ([string]::IsNullOrWhiteSpace($Path)) { return $true }
  try { $full = [System.IO.Path]::GetFullPath($Path).TrimEnd('\', '/') } catch { return $false }
  $legacy = @((Join-Path (Get-GmProjectDir $Name) 'salida'))
  if ($Name -ieq 'huancasancos') { $legacy += (Join-Path $script:ProjectRoot 'salida') }
  if ($Name -ieq 'chagual') { $legacy += (Join-Path $script:ProjectRoot '2do modelo base\salida nueva') }
  foreach ($candidate in @($legacy)) {
    try { $candidateFull = [System.IO.Path]::GetFullPath($candidate).TrimEnd('\', '/') } catch { continue }
    if ([string]::Equals($full, $candidateFull, [System.StringComparison]::OrdinalIgnoreCase)) { return $true }
  }
  return $false
}

function Sync-GmProjectOutput($Project) {
  $name = [string]$Project.Nombre
  if (-not (Test-GmProjectName $name)) { throw "Nombre de proyecto invalido: '$name'." }
  $default = Get-GmProjectOutputPath $name
  $output = [string]$Project.OutputFolder
  $schema = 0
  try { $schema = [int]$Project.Schema } catch { $schema = 0 }
  $changed = $false
  if ($schema -lt 2) {
    if (Test-GmLegacyProjectOutput $name $output) {
      $Project.OutputFolder = $default
      Add-GmLog ("Proyecto '$name': salida por proyecto '$default' (la anterior '$output' se conserva intacta).")
    } else {
      Add-GmLog ("Proyecto '$name': salida personalizada conservada ('$output').")
    }
    $Project.Schema = 2
    $changed = $true
  } elseif ([string]::IsNullOrWhiteSpace($output)) {
    $Project.OutputFolder = $default
    $changed = $true
  }
  # FASE1 2026-10-21: $output era el valor ANTIGUO (leido antes de las ramas
  # de arriba). Con schema 2 y salida vacia se ponia el default, pero
  # $isDefault se calculaba con el valor viejo (vacio) => la carpeta de salida
  # NO se creaba y el lote fallaba al escribir en una carpeta inexistente.
  # Se relee el valor ya corregido.
  $output = [string]$Project.OutputFolder
  $isDefault = -not [string]::IsNullOrWhiteSpace($output)
  if ($isDefault) {
    try {
      $defaultFull = [System.IO.Path]::GetFullPath($default).TrimEnd('\', '/')
      $outputFull = [System.IO.Path]::GetFullPath([string]$Project.OutputFolder).TrimEnd('\', '/')
      $isDefault = [string]::Equals($defaultFull, $outputFull, [System.StringComparison]::OrdinalIgnoreCase)
    } catch { $isDefault = [string]::Equals([string]$Project.OutputFolder, $default, [System.StringComparison]::OrdinalIgnoreCase) }
  }
  if ($isDefault) {
    try {
      if (-not (Test-Path -LiteralPath $default -PathType Container)) { New-Item -ItemType Directory -Path $default -Force | Out-Null }
    } catch {
      throw ("No se pudo crear la carpeta de salida del proyecto '$name' ($default): " + $_.Exception.Message)
    }
  }
  if ($changed) { Save-GmProjectRecord $Project }
  return $Project
}

# PRY1: crea los registros de los proyectos historicos si no hay ninguno y
# sincroniza TODOS los proyectos a la salida por proyecto (sin mover archivos).
function Initialize-GmProjects {
  Ensure-GmProjectsDir | Out-Null
  $existing = @(Get-GmProjectNames)
  if ($existing.Count -eq 0) {
    $legacy = @(
      @{ Name = 'huancasancos'; Csv = (Join-Path $script:ProjectRoot 'CAJETIN HUANCA.csv'); Tpl = (Join-Path $script:ProjectRoot 'PLANTILLA_ORIGINAL_HUANCASANCOS.dwg') },
      @{ Name = 'chagual'; Csv = (Join-Path $script:ProjectRoot '2do modelo base\CAJETIN CHAGUAL.csv'); Tpl = (Join-Path $script:ProjectRoot '2do modelo base\PLANTILLA_CHAGUAL_PU_ROLES.dwg') }
    )
    $created = New-Object 'System.Collections.Generic.List[string]'
    foreach ($item in $legacy) {
      if (-not (Test-Path -LiteralPath $item.Csv -PathType Leaf)) { continue }
      if (-not (Test-Path -LiteralPath $item.Tpl -PathType Leaf)) { continue }
      # FASE1 2026-10-21: un proyecto que falla no puede impedir que el
      # lanzador arranque. Antes una migracion a medias tumbaba todo.
      try {
        $record = New-GmProjectRecord $item.Name
        $record.CsvPath = $item.Csv
        $record.TemplatePath = $item.Tpl
        Save-GmProjectRecord $record
        Sync-GmProjectOutput $record | Out-Null
        $created.Add($item.Name)
      } catch {
        Add-GmLog ("No se pudo crear el proyecto '" + $item.Name + "': " + $_.Exception.Message)
      }
    }
    if ($created.Count -gt 0) {
      Add-GmLog ('Proyectos creados con salida por proyecto: ' + ($created.ToArray() -join ', ') + ' (sin mover archivos viejos).')
    }
  }
  foreach ($name in @(Get-GmProjectNames)) {
    # FASE1 2026-10-21: cada proyecto se sincroniza por separado: un registro
    # corrupto o una salida que no se puede crear no impiden abrir el resto.
    try {
      $project = Read-GmProject $name
      if ($null -eq $project) { continue }
      Sync-GmProjectOutput $project | Out-Null
    } catch {
      Add-GmLog ("Proyecto '" + $name + "' con problema (se sigue sin sincronizar): " + $_.Exception.Message)
    }
  }
  return @(Get-GmProjectNames)
}

# PRY1: texto de busqueda normalizado (mayusculas, sin tildes) para la caja
# unica de la biblioteca: '24623' encuentra 'U.C. 24623' y 'chugay' a 'CHUGAY'.
function ConvertTo-GmSearchText([string]$Text) {
  if ([string]::IsNullOrEmpty($Text)) { return '' }
  $formD = $Text.Normalize([System.Text.NormalizationForm]::FormD)
  $sb = New-Object System.Text.StringBuilder
  foreach ($ch in $formD.ToCharArray()) {
    if ([System.Globalization.CharUnicodeInfo]::GetUnicodeCategory($ch) -ne [System.Globalization.UnicodeCategory]::NonSpacingMark) {
      [void]$sb.Append($ch)
    }
  }
  return $sb.ToString().Normalize([System.Text.NormalizationForm]::FormC).ToUpperInvariant()
}

# PRY1: codigo -> {Uc, Distrito} desde el CSV del proyecto (col 14 y col 12).
function Get-GmCsvInfoMap([string]$CsvPath) {
  $map = @{}
  if (-not $CsvPath) { return $map }
  if (-not (Test-Path -LiteralPath $CsvPath -PathType Leaf)) { return $map }
  try { $csv = Read-GmCsv $CsvPath } catch { return $map }
  foreach ($row in $csv.Rows) {
    $fields = $row.Fields
    $uc = if ($fields.Length -gt 13) { ([string]$fields[13]).Trim() } else { '' }
    $distrito = if ($fields.Length -gt 11) { ([string]$fields[11]).Trim() } else { '' }
    $map[$row.Code] = [pscustomobject]@{ Uc = $uc; Distrito = $distrito }
  }
  return $map
}

# PRY1: tipo de plano por codigo, leido del REPORTE en orden de escritura: cada
# traza ETAPA;LOTE;TIPO_PLANO fija el tipo de la corrida y los RESULTADO que la
# siguen heredan. Corridas viejas sin traza no aportan tipo (cae al default).
function Get-GmLibraryTypeMap([string]$OutputFolder) {
  $map = @{}
  $path = Join-Path $OutputFolder $script:ReportName
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { return $map }
  $current = ''
  try {
    foreach ($line in [System.IO.File]::ReadAllLines($path, [System.Text.Encoding]::Default)) {
      if ($line.StartsWith('ETAPA;LOTE;TIPO_PLANO;')) {
        $parts = $line.Split(';')
        if ($parts.Length -ge 5) { $current = $parts[4].Trim().ToUpperInvariant() }
        continue
      }
      if ($line.StartsWith('RESULTADO;')) {
        $parts = $line.Split(';')
        if ($parts.Length -lt 3) { continue }
        $code = $parts[1].Trim()
        if ($code -ne '' -and $current -ne '') { $map[$code] = $current }
      }
    }
  } catch { }
  return $map
}

# PRY1: filas de la biblioteca = union de archivos (*.dwg / *.pdf), RESULTADO
# del REPORTE y codigos del CSV; con U.C./distrito del CSV y tipo del reporte.
function Get-GmLibraryRows($Project) {
  $rows = New-Object 'System.Collections.Generic.List[object]'
  if ($null -eq $Project) { return @($rows.ToArray()) }
  $folder = [string]$Project.OutputFolder
  if (-not $folder -or -not (Test-Path -LiteralPath $folder -PathType Container)) { return @($rows.ToArray()) }
  $outcomes = Get-GmOutcomes $folder
  $types = Get-GmLibraryTypeMap $folder
  $info = Get-GmCsvInfoMap ([string]$Project.CsvPath)
  $defaultType = if ([string]$Project.PlanType) { [string]$Project.PlanType } else { 'UBICACION' }
  $codes = @{}
  foreach ($code in @($outcomes.Keys)) { if ($code -ne '') { $codes[$code] = $true } }
  foreach ($code in @($info.Keys)) { if ($code -ne '') { $codes[$code] = $true } }
  foreach ($file in @(Get-ChildItem -LiteralPath $folder -File -ErrorAction SilentlyContinue)) {
    $name = $file.Name
    $code = $null
    if ($name -like '*.dwg') {
      $code = $file.BaseName
    } elseif ($name -like '* plano preliminar.pdf') {
      $code = $name.Substring(0, $name.Length - ' plano preliminar.pdf'.Length)
    }
    if ($code -and $code -ne '') { $codes[$code] = $true }
  }
  foreach ($code in @($codes.Keys | Sort-Object)) {
    $dwg = Get-GmOutputDwg $folder $code
    $pdf = Join-Path $folder ($code + ' plano preliminar.pdf')
    $hasDwg = Test-GmDwgFile $dwg
    $hasPdf = Test-Path -LiteralPath $pdf -PathType Leaf
    $outcome = if ($outcomes.ContainsKey($code)) { $outcomes[$code] } else { $null }
    if (-not $hasDwg -and -not $hasPdf -and $null -eq $outcome) { continue }
    $formato = if ($hasDwg -and $hasPdf) { 'DWG+PDF' } elseif ($hasDwg) { 'DWG' } elseif ($hasPdf) { 'PDF' } else { '' }
    $estado = if ($outcome) { [string]$outcome.Status } else { 'SIN RESULTADO' }
    $tipo = if ($types.ContainsKey($code)) { [string]$types[$code] } else { $defaultType }
    $uc = ''; $distrito = ''
    if ($info.ContainsKey($code)) { $uc = [string]$info[$code].Uc; $distrito = [string]$info[$code].Distrito }
    $archivo = if ($hasDwg) { $dwg } elseif ($hasPdf) { $pdf } else { '' }
    $searchText = ConvertTo-GmSearchText (($code + ' ' + $uc + ' ' + $distrito))
    $ucDigits = [regex]::Replace($uc, '[^0-9]', '')
    $rows.Add([pscustomobject]@{
      Code = $code; Tipo = $tipo; Formato = $formato; Estado = $estado
      Uc = $uc; Distrito = $distrito; Archivo = $archivo
      SearchText = $searchText; UcDigits = $ucDigits
    })
  }
  return @($rows.ToArray())
}

# PRY1: filtros en AND (tipo, formato y caja unica). Sin query = pasa todo.
function Select-GmLibraryRows($Rows, [string]$Tipo, [string]$Formato, [string]$Query) {
  $query = ConvertTo-GmSearchText $Query
  $queryDigits = [regex]::Replace([string]$Query, '[^0-9]', '')
  $result = New-Object 'System.Collections.Generic.List[object]'
  foreach ($row in @($Rows)) {
    if ($Tipo -and $Tipo -ne 'Todos' -and $row.Tipo -ne $Tipo) { continue }
    if ($Formato -and $Formato -ne 'Todos' -and $row.Formato -ne $Formato) { continue }
    if ($query -ne '') {
      $hit = $row.SearchText.Contains($query)
      if (-not $hit -and $queryDigits.Length -ge 3) {
        $hit = ($row.UcDigits -ne '' -and $row.UcDigits.Contains($queryDigits))
      }
      if (-not $hit) { continue }
    }
    $result.Add($row)
  }
  return @($result.ToArray())
}

# PRY1 (borrado, fase F3): una salida solo se puede mover si no es la raiz de
# una unidad, no es la raiz del sistema y no contiene al sistema.
function Test-GmPathDeletable([string]$Path) {
  if ([string]::IsNullOrWhiteSpace($Path)) { return $false }
  try { $full = [System.IO.Path]::GetFullPath($Path).TrimEnd('\') } catch { return $false }
  if ($full.Length -lt 4) { return $false }
  if ($full -ieq [System.IO.Path]::GetPathRoot($full).TrimEnd('\')) { return $false }
  $rootFull = [System.IO.Path]::GetFullPath($script:ProjectRoot).TrimEnd('\')
  if ($full -ieq $rootFull) { return $false }
  if ($rootFull.StartsWith($full + '\', [System.StringComparison]::OrdinalIgnoreCase)) { return $false }
  return $true
}

# FASE1 2026-10-21: una salida de proyecto SOLO puede estar dentro de la
# carpeta del propio proyecto o en un lugar libre elegido por el usuario. Se
# niegan (fail-closed): la carpeta de proyectos, el respaldo _borrados, la
# carpeta de otro proyecto, un ancestro de la carpeta de proyectos y cualquier
# enlace (junction/symlink, que puede apuntar fuera). Moverlas arrastraria el
# store completo, el trabajo de otro proyecto o el contenido de un enlace.
function Test-GmProjectOutputMovable([string]$Name, [string]$OutputPath) {
  if ([string]::IsNullOrWhiteSpace($OutputPath)) { return $true }
  if (-not (Test-GmPathDeletable $OutputPath)) { return $false }
  try {
    $full = [System.IO.Path]::GetFullPath($OutputPath).TrimEnd('\')
    $projectsDir = [System.IO.Path]::GetFullPath((Get-GmProjectsDir)).TrimEnd('\')
    $ownDir = [System.IO.Path]::GetFullPath((Get-GmProjectDir $Name)).TrimEnd('\')
    if ($full -ieq $projectsDir) { return $false }
    if ($projectsDir.StartsWith($full + '\', [System.StringComparison]::OrdinalIgnoreCase)) { return $false }
    if ($full.StartsWith($projectsDir + '\', [System.StringComparison]::OrdinalIgnoreCase)) {
      # dentro del store: solo la carpeta del propio proyecto
      if (-not ($full -ieq $ownDir -or $full.StartsWith($ownDir + '\', [System.StringComparison]::OrdinalIgnoreCase))) { return $false }
    }
    if (Test-Path -LiteralPath $OutputPath) {
      $item = Get-Item -LiteralPath $OutputPath -Force -ErrorAction SilentlyContinue
      if ($item -and (($item.Attributes -band [System.IO.FileAttributes]::ReparsePoint) -ne 0)) { return $false }
    }
    return $true
  } catch {
    return $false
  }
}

# PRY1 (borrado, fase F3): mueve el registro del proyecto y su salida a
# proyectos\_borrados\<nombre>_<fecha>\ y lo quita de la lista. Sin dialogos
# (la confirmacion es de la UI); el llamador debe verificar que no hay bloque.
function Remove-GmProjectFiles([string]$Name) {
  $project = Read-GmProject $Name
  if ($null -eq $project) { throw "El proyecto '$Name' no existe o su configuracion no se puede leer." }
  # FASE1 2026-10-21: el borrado solo toca el registro y la salida del
  # proyecto nombrado. Nombre invalido o salida protegida: no se toca nada.
  if (-not (Test-GmProjectName $Name)) { throw "Nombre de proyecto invalido: '$Name'." }
  $backupRoot = Join-Path (Get-GmProjectsDir) $script:ProjectsBackupName
  if (-not (Test-Path -LiteralPath $backupRoot -PathType Container)) { New-Item -ItemType Directory -Path $backupRoot | Out-Null }
  # F7: milisegundos; dos eliminaciones del mismo proyecto en el mismo segundo
  # no colisionan al crear el respaldo.
  $dest = Join-Path $backupRoot ($Name + '_' + (Get-Date).ToString('yyyyMMdd_HHmmss_fff'))
  New-Item -ItemType Directory -Path $dest | Out-Null
  $projectDir = Get-GmProjectDir $Name
  $output = [string]$project.OutputFolder
  $movedOutput = $false
  $outputBackup = $null
  $hadProjectDir = Test-Path -LiteralPath $projectDir -PathType Container
  try {
    if ($output -and (Test-Path -LiteralPath $output -PathType Container)) {
      $outputFull = [System.IO.Path]::GetFullPath($output).TrimEnd('\')
      $projectFull = [System.IO.Path]::GetFullPath($projectDir).TrimEnd('\')
      if (-not [string]::Equals($outputFull, $projectFull, [System.StringComparison]::OrdinalIgnoreCase)) {
        if (-not (Test-GmProjectOutputMovable $Name $output)) {
          throw "La carpeta de salida '$output' esta protegida (carpeta de proyectos, respaldo _borrados, proyecto ajeno o enlace); muevela a mano antes de eliminar el proyecto."
        }
        $outputBackup = Join-Path $dest 'salida'
        Move-Item -LiteralPath $output -Destination $outputBackup
        $movedOutput = $true
      }
    }
    if (Test-Path -LiteralPath $projectDir -PathType Container) {
      Move-Item -LiteralPath $projectDir -Destination (Join-Path $dest 'proyecto')
    }
  } catch {
    # FASE1: si algo fallo, se deshace lo movido y no queda un respaldo a medias
    # (el proyecto sigue vivo pero habria una carpeta en _borrados).
    if ($movedOutput -and $outputBackup -and (Test-Path -LiteralPath $outputBackup)) {
      try { Move-Item -LiteralPath $outputBackup -Destination $output -ErrorAction SilentlyContinue } catch { }
    }
    try { if (Test-Path -LiteralPath $dest) { Remove-Item -LiteralPath $dest -Recurse -Force -ErrorAction SilentlyContinue } } catch { }
    throw
  }
  # FASE1: el respaldo debe contener el registro antes de decir "eliminado".
  if ($hadProjectDir -and -not (Test-Path -LiteralPath (Join-Path $dest 'proyecto'))) {
    throw "El proyecto '$Name' no se pudo respaldar en '$dest'; no se borro nada."
  }
  # El descubrimiento es por escaneo de carpetas: sin registro, el proyecto
  # desaparece de la lista sin archivo de indice que desincronizar.
  Add-GmLog ("Proyecto '$Name' eliminado. Respaldo en: " + $dest)
  return [pscustomobject]@{ Backup = $dest; OutputMoved = $movedOutput }
}

function Record-GmTemplateWarning {
  if ($script:State.FreshWarningLogged) { return }
  $path = Join-Path $script:State.OutputFolder $script:ReportName
  if (-not (Test-Path -LiteralPath $path -PathType Leaf)) { return }
  foreach ($line in [System.IO.File]::ReadAllLines($path, [System.Text.Encoding]::Default)) {
    if ($line.StartsWith('ETAPA;LOTE;FRESH_CHECK;AVISO;')) {
      $detail = $line.Substring('ETAPA;LOTE;FRESH_CHECK;AVISO;'.Length).TrimEnd(';')
      Add-GmLog "Aviso de plantilla: $detail. El lanzador desactiva la alerta modal, usa la configuracion elegida y nunca guarda la copia."
      $script:State.FreshWarningLogged = $true
      Save-GmState
      return
    }
  }
}

function Set-GmBatch([string[]]$Codes, $Outcomes) {
  $script:State.CurrentBatch = @($Codes)
  $script:State.BatchBaseline = Get-GmOutcomeCounts $Outcomes
  $script:State.Paused = $false
  Save-GmState
}

function Get-GmNextBatch($Outcomes, $Progress) {
  $currentPending = @(Get-GmPendingCurrentBatch $Outcomes $Progress)
  if ($currentPending.Count -gt 0) { return $currentPending }
  if (@($script:State.CurrentBatch).Count -gt 0) {
    $script:State.CurrentBatch = @()
    $script:State.BatchBaseline = @{}
  }
  $attempted = Get-GmCodesSet $script:State.Attempted
  $remaining = @($script:Codes | Where-Object { -not (Test-GmSet $attempted $_) })
  if ($remaining.Count -gt 0) {
    $batch = @($remaining | Select-Object -First ([int]$script:State.BlockSize))
    $script:State.Phase = 'INITIAL'
    Set-GmBatch $batch $Outcomes
    return $batch
  }
  if ($script:State.RetryFailures -and $script:State.Phase -ne 'RETRY_COMPLETE') {
    $retried = Get-GmCodesSet $script:State.Retried
    $retry = @($script:State.Failed | Where-Object { -not (Test-GmSet $retried $_) })
    if ($retry.Count -gt 0) {
      $batch = @($retry | Select-Object -First ([int]$script:State.BlockSize))
      # FIX4: el reintento se marca en Start-GmBlock AL LANZAR el proceso, no al
      # programar el bloque: un fallo previo a Start-Process no consume el intento.
      $script:State.Phase = 'RETRY'
      Set-GmBatch $batch $Outcomes
      return $batch
    }
  }
  $script:State.Phase = 'RETRY_COMPLETE'
  $script:State.CurrentBatch = @()
  $script:State.BatchBaseline = @{}
  $script:State.Complete = $true
  $script:State.Paused = $false
  Save-GmState
  return @()
}

function ConvertTo-GmLispString([string]$Value) {
  $escaped = $Value.Replace('\', '/').Replace('"', '\"')
  return ('"' + $escaped + '"')
}

# PRY2 2026-10-13: el orden de carga de los LISP vive en un solo manifiesto
# (lisp\MANIFIESTO_MODULOS.txt). El lanzador carga esas rutas tal cual; los
# archivos raiz quedan como anclas de sello (APPLOAD manual).
function Get-GmModuleLoadList {
  $manifest = Join-Path $script:ProjectRoot 'lisp\MANIFIESTO_MODULOS.txt'
  if (-not (Test-Path -LiteralPath $manifest -PathType Leaf)) {
    throw 'Falta lisp\MANIFIESTO_MODULOS.txt (orden de carga de modulos LISP).'
  }
  $list = New-Object 'System.Collections.Generic.List[string]'
  foreach ($raw in [System.IO.File]::ReadAllLines($manifest)) {
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

function Get-GmBootScript([string[]]$Batch, [bool]$FirstLaunch, [string]$CsvPath, [string]$OutputFolder) {
  $skip = @($script:Codes | Where-Object { $_ -notin $Batch })
  $lines = New-Object 'System.Collections.Generic.List[string]'
  $csv = ConvertTo-GmLispString $CsvPath
  $output = ConvertTo-GmLispString $OutputFolder
  $lines.Add('(setq *GM_LAUNCHER_TRUSTED_OLD* (getvar "TRUSTEDPATHS"))')
  # PRY2: las rutas confiables cubren la raiz y las carpetas de modulos.
  $trustedAdd = @(
    $script:ProjectRoot,
    (Join-Path $script:ProjectRoot 'lisp'),
    (Join-Path $script:ProjectRoot 'lisp\ubi'),
    (Join-Path $script:ProjectRoot 'lisp\lay'),
    (Join-Path $script:ProjectRoot 'lisp\gm')
  ) | ForEach-Object { ConvertTo-GmLispString $_ }
  $lines.Add('(setvar "TRUSTEDPATHS" (strcat *GM_LAUNCHER_TRUSTED_OLD* ";" ' + (($trustedAdd) -join ' ";" ') + '))')
  foreach ($rel in @(Get-GmModuleLoadList)) {
    $path = ConvertTo-GmLispString (Join-Path $script:ProjectRoot $rel)
    $lines.Add('(load ' + $path + ')')
  }
  $lines.Add('(setvar "TRUSTEDPATHS" *GM_LAUNCHER_TRUSTED_OLD*)')
  $lines.Add('(setq *GM_AUTO_CSV_PATH* ' + $csv + ')')
  $lines.Add('(setq *GM_AUTO_OUTPUT_FOLDER* ' + $output + ')')
  $lines.Add('(setq *GM_AUTO_FRESH_CHECK* nil)')
  $lines.Add('(setq *GM_AUTO_QUIET* T)')
  $lines.Add('(setq *GM_AUTO_FORENSIC* ' + $(if ($script:State.Diag) { 'T' } else { 'nil' }) + ')')
  $lines.Add('(setq *GM_AUTO_MAPA_ANS* "_Yes")')
  $lines.Add('(setq *GM_AUTO_MAKE_PDF* ' + $(if ($script:State.MakePdf) { 'T' } else { 'nil' }) + ')')
  $lines.Add('(setq *GM_AUTO_SHOW_PREDIO_LOC* ' + (ConvertTo-GmLispString $(if ($script:State.ShowPredio) { '1' } else { '0' })) + ')')
  $lines.Add('(setq *GM_AUTO_SHOW_GEO_TXT* ' + (ConvertTo-GmLispString $(if ($script:State.ShowGeoTxt) { '1' } else { '0' })) + ')')
  # PRY1: el tipo de plano (estilo) y los datos de VP viajan al LISP antes del
  # lote; el sidecar los persiste para la continuacion manual.
  $lines.Add('(setq *GM_PLANO_TIPO* ' + (ConvertTo-GmLispString (Get-GmValidPlanType $script:State.PlanType)) + ')')
  $lines.Add('(setq *UBI_VP_TYPE* ' + (ConvertTo-GmLispString (Get-GmValidVpType $script:State.VpType)) + ')')
  $lines.Add('(setq *GM_AUTO_CHUNK* ' + [string][int]$script:State.InnerChunk + ')')
  # AUD3: 0 = sin tope interno de codigos; el reciclaje por bloques del
  # lanzador reemplaza la pausa manual (el tope solo aplica al lote dentro de
  # AutoCAD, documentado en MANUAL_USO).
  $lines.Add('(setq *GM_AUTO_MAX_CODES* 0)')
  $lines.Add('(setq *GM_AUTO_RESUME* ' + $(if ($FirstLaunch) { 'nil' } else { 'T' }) + ')')
  $lines.Add('(setq *GM_AUTO_SKIP_PANEL* T)')
  $lines.Add('(setq *GM_AUTO_PANEL_STATUS* ' + $(if ($script:State.MakePdf) { '2' } else { '1' }) + ')')
  $lines.Add('(setq *GM_AUTO_REPORT_APPEND* ' + $(if ($FirstLaunch) { 'nil' } else { 'T' }) + ')')
  $lines.Add('(setq *GM_AUTO_LAUNCHER_SKIP* nil)')
  for ($offset = 0; $offset -lt $skip.Count; $offset += 32) {
    $end = [math]::Min($offset + 31, $skip.Count - 1)
    $chunk = @($skip[$offset..$end] | ForEach-Object { ConvertTo-GmLispString $_ }) -join ' '
    $lines.Add('(setq *GM_AUTO_LAUNCHER_SKIP* (append *GM_AUTO_LAUNCHER_SKIP* ''(' + $chunk + ')))')
  }
  $lines.Add('(if (and (equal *GM_VER_AUTO* "2026-10-21-FIX1") (not (GM:CheckVersions))) (c:GM_PLANOS_LOTE) (alert "Versiones LISP incompatibles. Revise los cuatro sellos y vuelva a iniciar."))')
  # AUD3: el par QUIT/No se repite a proposito; si el primer QUIT lo consume un
  # prompt pendiente, el segundo garantiza el cierre respondiendo "no guardar".
  $lines.Add('_.QUIT')
  $lines.Add('_No')
  $lines.Add('_.QUIT')
  $lines.Add('_No')
  return ($lines -join "`r`n") + "`r`n"
}

function Start-GmBlock([string[]]$Batch) {
  if ($Batch.Count -eq 0) { return }
  # F5: el CSV no puede cambiar a mitad de una corrida larga (TOCTOU). Se
  # re-hashea antes de cada bloque; si cambio, throw y Schedule pausa seguro
  # sin abrir AutoCAD ni consumir reintentos.
  if ($script:State.CsvPath -and $script:State.CsvHash) {
    if (-not (Test-Path -LiteralPath $script:State.CsvPath -PathType Leaf)) {
      throw 'El CSV de la corrida desaparecio; restaure el archivo original y reanude.'
    }
    $currentCsvHash = (Get-FileHash -LiteralPath $script:State.CsvPath -Algorithm SHA256).Hash
    if ($currentCsvHash -ne $script:State.CsvHash) {
      throw 'El CSV cambio durante la corrida; pausa segura. Restaure el CSV original y reanude.'
    }
  }
  # AUD3: la plantilla maestra se re-hashea por bloque (mismo TOCTOU que el CSV);
  # un master alterado a mitad de corrida pausa seguro antes de copiar nada.
  if ($script:State.TemplatePath -and $script:State.TemplateHash) {
    if (-not (Test-Path -LiteralPath $script:State.TemplatePath -PathType Leaf)) {
      throw 'La plantilla maestra desaparecio; restaure el DWG original y reanude.'
    }
    $currentTemplateHash = (Get-FileHash -LiteralPath $script:State.TemplatePath -Algorithm SHA256).Hash
    if ($currentTemplateHash -ne $script:State.TemplateHash) {
      throw 'La plantilla maestra cambio durante la corrida; pausa segura. Restaure el DWG original y reanude.'
    }
  }
  $script:State.Complete = $false
  $script:State.Paused = $false
  $baselineOutcomes = Get-GmOutcomes $script:State.OutputFolder
  # AUD3: CurrentBatch ya lo fijan Get-GmNextBatch/Complete-GmBlock; aqui solo se
  # refresca la linea base de RESULTADO con la que se mide este proceso.
  $script:State.BatchBaseline = Get-GmOutcomeCounts $baselineOutcomes
  $script:State.BatchNumber = [int]$script:State.BatchNumber + 1
  Save-GmState
  Ensure-GmStateDir $script:State.OutputFolder | Out-Null

  $tempRoot = Join-Path $env:TEMP ('GM_LOTE_' + $script:State.RunId)
  if (-not (Test-Path -LiteralPath $env:TEMP -PathType Container)) { throw 'No existe la carpeta temporal de Windows.' }
  if (-not (Test-Path -LiteralPath $tempRoot -PathType Container)) { New-Item -ItemType Directory -Path $tempRoot | Out-Null }
  $blockDir = Join-Path $tempRoot ('bloque_' + $script:State.BatchNumber.ToString('000'))
  if (Test-Path -LiteralPath $blockDir) { Remove-Item -LiteralPath $blockDir -Recurse -Force }
  New-Item -ItemType Directory -Path $blockDir | Out-Null
  $workingDwg = Join-Path $blockDir 'GM_TRABAJO_FRESCO.dwg'
  Copy-Item -LiteralPath $script:State.TemplatePath -Destination $workingDwg
  $copyHash = (Get-FileHash -LiteralPath $workingDwg -Algorithm SHA256).Hash
  if ($copyHash -ne $script:State.TemplateHash) { throw 'La copia temporal no coincide con el hash de la plantilla maestra.' }
  $scrPath = Join-Path $blockDir 'GM_INICIAR_LOTE.scr'
  # AUD3: un unico testigo decide si el REPORTE se reinicia (w) o se anexa (a):
  # InitializedBeforeFirstBlock. Read-GmState migra manifiestos sin el campo a
  # $true para nunca truncar el reporte de una corrida en curso.
  $firstLaunch = -not [bool]$script:State.InitializedBeforeFirstBlock
  $scr = Get-GmBootScript $Batch $firstLaunch $script:State.CsvPath $script:State.OutputFolder
  [System.IO.File]::WriteAllText($scrPath, $scr, [System.Text.Encoding]::Default)
  $script:State.InitializedBeforeFirstBlock = $true
  Save-GmState

  $arguments = '"' + $workingDwg + '" /b "' + $scrPath + '"'
  $process = Start-Process -FilePath $script:State.AcadPath -ArgumentList $arguments -WorkingDirectory $blockDir -PassThru
  # FIX4: el reintento se marca aqui (al lanzar el proceso), no al programar el
  # bloque: si algo falla antes de Start-Process, el intento no se consume.
  if ($script:State.Phase -eq 'RETRY') {
    $alreadyRetried = Get-GmCodesSet $script:State.Retried
    foreach ($code in @($Batch)) {
      if (-not (Test-GmSet $alreadyRetried $code)) { $script:State.Retried += $code }
    }
    Save-GmState
  }
  $script:activeProc = $process
  $script:activeBlockDir = $blockDir
  $script:currentBatch = @($Batch)
  $script:lastActivityUtc = [DateTime]::UtcNow
  $script:StopRequested = $false
  $script:stallWarned = $false
  $script:stallKilled = $false
  Write-GmMonitorRow 'PROCESO_INICIO' $process $script:State.OutputFolder
  Add-GmLog ("Bloque {0}: PID {1}, codigos {2}, DWG temporal fresco, reporte {3}." -f $script:State.BatchNumber, $process.Id, $Batch.Count, $(if ($firstLaunch) { 'nuevo (w)' } else { 'anexado (a)' }))
  $script:timer.Start()
  Update-GmUiState
}

function Update-GmCurrentState {
  $pre = Update-GmStateFromFiles
  $progressCount = $pre.Progress.Count
  if ($script:progressBar) {
    $script:progressBar.Maximum = [math]::Max(1, [int]$script:State.Total)
    $script:progressBar.Value = [math]::Min([int]$script:State.Total, [int]$script:State.Attempted.Count)
  }
  if ($script:statusLabel) {
    $phaseText = if ($script:State.Phase -eq 'RETRY') { 'reintento de errores' } else { 'primera pasada' }
    $script:statusLabel.Text = "Intentados $($script:State.Attempted.Count)/$($script:State.Total) | OK $progressCount | Pendientes $($script:State.Failed.Count) | $phaseText"
  }
  Update-GmPendingView
  # PRY1: al cerrar cada bloque la biblioteca refleja los planos nuevos (AutoCAD
  # ya salio; el REPORTE no esta bloqueado).
  if ($null -eq $script:activeProc -and $script:panelDetail -and $script:panelDetail.Visible -and $script:ActiveProject) {
    try { Refresh-GmLibrary } catch { Add-GmLog ('Biblioteca: ' + $_.Exception.Message) }
  }
}

# UI1 2026-10-06: vista de pendientes del lanzador. Lee PENDIENTES.csv completo
# (CODIGO;ETAPA;ESTADO;MOTIVO;ARCHIVO), preserva motivos con ';' interno y se
# refresca al cerrar cada bloque. El doble clic abre el PDF (o DWG) del codigo.
function Update-GmPendingView([string]$OutputFolder) {
  if (-not $script:pendingList) { return }
  if (-not $OutputFolder) { $OutputFolder = $script:txtOutput.Text.Trim() }
  $script:pendingList.BeginUpdate()
  try {
    $script:pendingList.Items.Clear()
    $path = Join-Path $OutputFolder $script:PendingName
    if (Test-Path -LiteralPath $path -PathType Leaf) {
      foreach ($line in [System.IO.File]::ReadAllLines($path, [System.Text.Encoding]::Default)) {
        $parts = $line.Split(';')
        if ($parts.Length -lt 2) { continue }
        $code = $parts[0].Trim()
        if ($code -eq '' -or $code -ieq 'CODIGO') { continue }
        $etapa = $parts[1].Trim()
        $estado = if ($parts.Length -gt 2) { $parts[2].Trim() } else { '' }
        $motivo = if ($parts.Length -gt 4) { ($parts[3..($parts.Length - 2)] -join ';').Trim() } elseif ($parts.Length -gt 3) { $parts[3].Trim() } else { '' }
        $item = New-Object System.Windows.Forms.ListViewItem($code)
        [void]$item.SubItems.Add($etapa)
        [void]$item.SubItems.Add($estado)
        [void]$item.SubItems.Add($motivo)
        [void]$script:pendingList.Items.Add($item)
      }
    }
    if ($script:pendingList.Items.Count -eq 0) {
      $item = New-Object System.Windows.Forms.ListViewItem('Sin pendientes')
      [void]$item.SubItems.Add('-')
      [void]$item.SubItems.Add('-')
      [void]$item.SubItems.Add('-')
      [void]$script:pendingList.Items.Add($item)
    }
  } finally {
    $script:pendingList.EndUpdate()
  }
}

function Complete-GmBlock($Process) {
  $Process.Refresh()
  $exitCode = $null
  try { $exitCode = $Process.ExitCode } catch { }
  # AUD3: la clave de CPU del proceso que acaba de salir no se reutiliza.
  $script:CpuPrevious.Remove([string]$Process.Id)
  Write-GmMonitorRow 'PROCESO_SALIO' $null $script:State.OutputFolder
  # FIX4: una reconciliacion inconsistente (p.ej. PROGRESO con codigo ajeno al
  # CSV) pausa la corrida en vez de colgar la cadena automatica con un throw.
  try {
    $pre = Update-GmStateFromFiles
  } catch {
    $script:activeProc = $null
    if ($script:activeBlockDir -and -not (Remove-GmBlockDirectory $script:activeBlockDir)) {
      Add-GmLog "El directorio temporal sigue bloqueado y queda para borrado posterior: $($script:activeBlockDir)"
    }
    $script:activeBlockDir = $null
    $script:timer.Stop()
    $script:runActive = $false
    $script:State.Paused = $true
    Save-GmState
    Add-GmLog ('Reconciliacion inconsistente: ' + $_.Exception.Message + '; corrida en pausa segura. Revise CSV/PROGRESO antes de reanudar.')
    Update-GmUiState
    return
  }
  Record-GmTemplateWarning
  $pending = @(Get-GmPendingCurrentBatch $pre.Outcomes $pre.Progress)
  $script:State.CurrentBatch = @($pending)
  if ($pending.Count -gt 0) {
    $first = $pending[0]
    $counts = $script:State.CrashCounts
    if (-not $counts.ContainsKey($first)) { $counts[$first] = 0 }
    $counts[$first] = [int]$counts[$first] + 1
    $script:State.CrashCounts = $counts
  }
  $script:activeProc = $null
  if ($script:activeBlockDir -and -not (Remove-GmBlockDirectory $script:activeBlockDir)) {
    Add-GmLog "El directorio temporal sigue bloqueado y queda para borrado posterior: $($script:activeBlockDir)"
  }
  $script:activeBlockDir = $null
  $script:timer.Stop()
  Save-GmState
  Update-GmCurrentState
  Add-GmLog ("Fin bloque {0}; exit={1}; resultados guardados={2}; por repetir={3}." -f $script:State.BatchNumber, $exitCode, $script:State.Attempted.Count, $pending.Count)
  if ($null -ne $exitCode -and $exitCode -ne 0 -and $pending.Count -eq 0) {
    Add-GmLog "AutoCAD termino con codigo $exitCode, pero todos los codigos del bloque tienen RESULTADO. Revisar si el codigo se repite en futuros bloques."
  }
  if ($script:StopRequested) {
    $script:State.Paused = $true
    Save-GmState
    $script:runActive = $false
    Add-GmLog 'Lote detenido por el usuario. Se puede reanudar desde la interfaz.'
    Update-GmUiState
    return
  }
  if ($pending.Count -gt 0) {
    $code = $pending[0]
    $crashCount = [int]$script:State.CrashCounts[$code]
    if ($crashCount -gt 2) {
      $script:State.Paused = $true
      Save-GmState
      $script:runActive = $false
      Add-GmLog "Pausa segura: '$code' no llego a RESULTADO tras $crashCount procesos. Revise FORENSE/MONITOR antes de reintentar."
      Update-GmUiState
      if ($script:PilotTestMode) {
        $script:PilotExitCode = 1
        $form.BeginInvoke([Action]{ $form.Close() }) | Out-Null
      }
      return
    }
    Add-GmLog "AutoCAD salio antes de completar '$code'; se reabrira una copia fresca para reintentar."
  }
  Schedule-GmNextBlock
}

function Schedule-GmNextBlock {
  if ($script:StopRequested) { return }
  # FIX4: la reconciliacion no puede colgar la cadena automatica; si falla,
  # pausa segura con aviso (el operador revisa CSV/PROGRESO y reanuda).
  try {
    $pre = Update-GmStateFromFiles
  } catch {
    $script:runActive = $false
    $script:State.Paused = $true
    Save-GmState
    Add-GmLog ('Reconciliacion inconsistente: ' + $_.Exception.Message + '; corrida en pausa segura. Revise CSV/PROGRESO antes de reanudar.')
    if ($script:statusLabel) { $script:statusLabel.Text = 'Pausa segura: revise CSV/PROGRESO antes de reanudar.' }
    Update-GmUiState
    return
  }
  $batch = @(Get-GmNextBatch $pre.Outcomes $pre.Progress)
  if ($batch.Count -eq 0) {
    $script:runActive = $false
    Save-GmState
    Update-GmCurrentState
    Add-GmLog ("Corrida completa. OK en progreso: {0}; pendientes finales: {1}." -f $pre.Progress.Count, $script:State.Failed.Count)
    $tempRoot = Join-Path $env:TEMP ('GM_LOTE_' + $script:State.RunId)
    if ((Test-Path -LiteralPath $tempRoot -PathType Container) -and -not (Remove-GmBlockDirectory $tempRoot)) {
      Add-GmLog "Algunos archivos temporales quedan bloqueados y no se borraron: $tempRoot"
    }
    $script:statusLabel.Text = 'Lote completo. El DWG maestro no fue abierto ni guardado.'
    Update-GmUiState
    if ($script:PilotTestMode) {
      $script:PilotExitCode = 0
      $form.BeginInvoke([Action]{ $form.Close() }) | Out-Null
    }
    return
  }
  try {
    Start-GmBlock $batch
  } catch {
    $script:runActive = $false
    $script:State.Paused = $true
    Save-GmState
    Add-GmLog "No se pudo iniciar el siguiente bloque: $($_.Exception.Message)"
    Update-GmUiState
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'GM - Error al iniciar', 'OK', 'Error') | Out-Null
  }
}

function Update-GmUiState {
  $active = ($null -ne $script:activeProc)
  if ($script:startButton) { $script:startButton.Enabled = -not $active }
  if ($script:resumeButton) { $script:resumeButton.Enabled = -not $active }
  if ($script:stopButton) { $script:stopButton.Enabled = $active }
  if ($script:checkButton) { $script:checkButton.Enabled = -not $active }
  if ($script:openButton) { $script:openButton.Enabled = Test-Path -LiteralPath $script:txtOutput.Text -PathType Container }
  # PRY1: mientras corre un bloque no se cambia de proyecto ni se gestiona la lista.
  if ($script:btnBackToProjects) { $script:btnBackToProjects.Enabled = -not $active }
  if ($script:btnProjectOpen) { $script:btnProjectOpen.Enabled = -not $active }
  if ($script:btnProjectNew) { $script:btnProjectNew.Enabled = -not $active }
  if ($script:btnProjectRefresh) { $script:btnProjectRefresh.Enabled = -not $active }
  if ($script:btnProjectDelete) { $script:btnProjectDelete.Enabled = -not $active }
}

function Start-GmFreshRun {
  $config = Get-GmConfigFromUi
  $pre = Test-GmConfig $config
  # PRY1: la configuracion vigente se persiste en proyecto.json antes de correr.
  Save-GmActiveProject
  if (-not $script:PilotTestMode) {
    $message = "Se archivaran los reportes/progreso anteriores. Los DWG/PDF existentes se conservaran, pero se reemplazaran si comparten codigo.`n`nCada bloque usara una copia temporal de la plantilla y AutoCAD se cerrara sin guardar.`n`nCSV: $($pre.Csv.Rows.Count) codigos`nBloque: $($config.BlockSize)`nSalida: $($config.OutputFolder)`n`nIniciar desde cero?"
    $answer = [System.Windows.Forms.MessageBox]::Show($message, 'GM - Confirmar lote nuevo', 'YesNo', 'Question')
    if ($answer -ne [System.Windows.Forms.DialogResult]::Yes) { return }
  }
  $archive = Archive-GmRunState $config.OutputFolder
  $script:State = New-GmState $config $pre
  $script:State.InitializedBeforeFirstBlock = $false
  $script:Rows = $pre.Csv.Rows
  $script:Codes = $pre.Csv.Codes
  $script:runActive = $true
  $script:StopRequested = $false
  if ($archive -eq 'sin archivos previos') {
    Add-GmLog 'Plantilla maestra verificada por SHA256; no habia archivos de estado previos que archivar.'
  } else {
    Add-GmLog "Plantilla maestra verificada por SHA256; archivos de estado anteriores archivados en $archive"
  }
  Add-GmLog ("CSV validado: {0} codigos unicos, separador '{1}'. Sellos: {2}." -f $script:Codes.Count, $pre.Csv.Delimiter, ($pre.Stamps.Values -join ', '))
  Update-GmPendingView
  Schedule-GmNextBlock
}

function Start-GmResumeRun {
  $config = Get-GmConfigFromUi
  $pre = Test-GmConfig $config
  # PRY1: persiste la configuracion vigente (el reanudado corrige campos con las
  # opciones guardadas; el proyecto queda espejo de lo que corre).
  Save-GmActiveProject
  $state = Read-GmState $config.OutputFolder
  Assert-GmResumeMatches $config $pre $state
  Test-GmSidecarMatches $state
  $script:State = $state
  $script:Rows = $pre.Csv.Rows
  $script:Codes = $pre.Csv.Codes
  $script:runActive = $true
  $script:StopRequested = $false
  $script:State.Complete = $false
  $script:State.Paused = $false
  # FIX4: CrashCounts se conserva al reanudar (es el cortacircuitos anti-loop);
  # solo se poda cuando el codigo alcanza RESULTADO (Update-GmStateFromFiles).
  Add-GmLog 'Reanudacion validada: CSV, hash de plantilla y progreso compatibles.'
  $preFiles = Update-GmStateFromFiles
  Update-GmPendingView
  $pending = @(Get-GmPendingCurrentBatch $preFiles.Outcomes $preFiles.Progress)
  if ($pending.Count -gt 0) {
    $script:State.CurrentBatch = $pending
    $script:State.BatchBaseline = Get-GmOutcomeCounts $preFiles.Outcomes
  }
  Save-GmState
  Schedule-GmNextBlock
}

# AUD3: registro LocalDumps en un solo lugar; la funcion de UI y el switch
# -EnableLocalDumps (instancia elevada) comparten este helper.
function Set-GmLocalDumpsRegistry {
  $key = 'HKLM:\SOFTWARE\Microsoft\Windows\Windows Error Reporting\LocalDumps\acad.exe'
  $dumpFolder = Join-Path $env:LOCALAPPDATA 'CrashDumps'
  if (-not (Test-Path -LiteralPath $dumpFolder -PathType Container)) { New-Item -ItemType Directory -Path $dumpFolder | Out-Null }
  New-Item -Path $key -Force | Out-Null
  New-ItemProperty -Path $key -Name DumpFolder -Value '%LOCALAPPDATA%\CrashDumps' -PropertyType ExpandString -Force | Out-Null
  New-ItemProperty -Path $key -Name DumpType -Value 2 -PropertyType DWord -Force | Out-Null
  New-ItemProperty -Path $key -Name DumpCount -Value 3 -PropertyType DWord -Force | Out-Null
  return $dumpFolder
}

function Enable-GmLocalDumps {
  $admin = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
  if (-not $admin) {
    $argLine = '-NoProfile -ExecutionPolicy Bypass -File "' + $PSCommandPath + '" -EnableLocalDumps'
    Start-Process -FilePath (Join-Path $PSHOME 'powershell.exe') -Verb RunAs -ArgumentList $argLine
    Add-GmLog 'Se solicito permiso de administrador para habilitar LocalDumps.'
    return
  }
  $dumpFolder = Set-GmLocalDumpsRegistry
  Add-GmLog "LocalDumps habilitado para acad.exe en $dumpFolder (volcado completo, maximo 3)."
  [System.Windows.Forms.MessageBox]::Show("LocalDumps quedo habilitado para acad.exe.`nDestino: $dumpFolder", 'GM - Diagnostico', 'OK', 'Information') | Out-Null
}

if ($EnableLocalDumps) {
  Set-GmLocalDumpsRegistry | Out-Null
  exit 0
}

# SAL2: primer arranque / copia temporal del paquete: crea el store y la salida
# de cada proyecto y verifica el default oficial SIN abrir la ventana.
if ($StartupSmoke) {
  $names = @(Initialize-GmProjects)
  if (-not ($names -contains 'huancasancos')) { throw ('STARTUP: no se creo huancasancos. Proyectos: ' + ($names -join ',')) }
  $project = Read-GmProject 'huancasancos'
  if ($null -eq $project) { throw 'STARTUP: el proyecto huancasancos no se puede leer.' }
  $expectedOut = Get-GmProjectOutputPath 'huancasancos'
  if ([string]$project.OutputFolder -ne $expectedOut -or -not (Test-Path -LiteralPath $expectedOut -PathType Container)) {
    throw ("STARTUP: salida incompleta. Esperada: $expectedOut")
  }
  if (-not (Test-Path -LiteralPath (Join-Path (Get-GmProjectDir 'huancasancos') $script:ProjectFileName) -PathType Leaf)) {
    throw 'STARTUP: el proyecto.json del proyecto no existe.'
  }
  Write-Output ("STARTUP OK; PROYECTO=huancasancos; SALIDA={0}" -f $expectedOut)
  exit 0
}

if ($SelfTest) {
  $tempOutput = Join-Path $env:TEMP ('GM_LANZADOR_SELFTEST_' + [guid]::NewGuid().ToString('N'))
  if (-not (Test-Path -LiteralPath $env:TEMP -PathType Container)) { throw 'No existe TEMP para el self-test.' }
  New-Item -ItemType Directory -Path $tempOutput | Out-Null
  try {
    $csv = Read-GmCsv (Join-Path $script:ProjectRoot 'CAJETIN HUANCA.csv')
    if ($csv.Rows.Count -ne 234) { throw "CSV oficial: se esperaban 234 codigos y se encontraron $($csv.Rows.Count)." }
    $pilot = Read-GmCsv (Join-Path $script:ProjectRoot 'PRUEBA_5_PREDIOS.csv')
    if ($pilot.Rows.Count -ne 5) { throw "CSV piloto: se esperaban 5 codigos y se encontraron $($pilot.Rows.Count)." }
    $stamps = Test-GmStamps
    $config = @{
      CsvPath = Join-Path $script:ProjectRoot 'CAJETIN HUANCA.csv'
      TemplatePath = Join-Path $script:ProjectRoot 'PLANTILLA_ORIGINAL_HUANCASANCOS.dwg'
      OutputFolder = $tempOutput
      AcadPath = Find-GmAcad
    }
  $preflight = Test-GmConfig $config
  $script:State = New-GmState (@{
    CsvPath = $config.CsvPath; TemplatePath = $config.TemplatePath; OutputFolder = $config.OutputFolder
    AcadPath = $config.AcadPath; BlockSize = 25; InnerChunk = 0; StallMinutes = 15; StallKill = $false
    MakePdf = $true; ShowPredio = $true; ShowGeoTxt = $true; Diag = $true; RetryFailures = $true
  }) $preflight
  $script:Codes = $csv.Codes
  $boot = Get-GmBootScript @('HS-YA-001') $true $config.CsvPath $config.OutputFolder
  if (-not $boot.Contains('(c:GM_PLANOS_LOTE)') -or -not $boot.Contains('*GM_AUTO_LAUNCHER_SKIP*') -or
      -not $boot.Contains('*GM_AUTO_FRESH_CHECK* nil') -or -not $boot.Contains('*GM_AUTO_QUIET* T') -or
      -not $boot.Contains('*GM_AUTO_FORENSIC*') -or
      -not $boot.Contains('*GM_AUTO_MAPA_ANS* "_Yes"') -or
      -not $boot.Contains('(setq *GM_PLANO_TIPO* "UBICACION")') -or
      -not $boot.Contains('(setq *UBI_VP_TYPE* "1")') -or
      -not $boot.Contains('(load ') -or
      -not $boot.Contains('_.QUIT') -or $boot.Contains('QSAVE') -or $boot.Contains('_.SAVE')) {
    throw 'El script de arranque no paso la prueba de ejecucion/cierre sin guardar.'
  }
  # PRY2: el arranque carga TODOS los modulos del manifiesto (nada se pierde).
  $manifestCount = @(Get-GmModuleLoadList).Count
  if ($manifestCount -lt 4 -or ([regex]::Matches($boot, [regex]::Escape('(load '))).Count -lt $manifestCount) {
    throw "El script de arranque no carga los $manifestCount modulos del manifiesto."
  }
  # PRY1: el tipo de plano guardado en el estado viaja al script de arranque.
  $script:State.PlanType = 'MATRIZ'
  $bootTipo = Get-GmBootScript @('HS-YA-001') $true $config.CsvPath $config.OutputFolder
  if (-not $bootTipo.Contains('(setq *GM_PLANO_TIPO* "MATRIZ")')) { throw 'PRY1: el tipo de plano guardado no llega al script de arranque.' }
  $script:State.PlanType = 'UBICACION'
  if (-not $boot.Contains("append *GM_AUTO_LAUNCHER_SKIP* '(")) { throw 'La lista de codigos diferidos no se serializa como lista AutoLISP.' }
  try {
    $testConfig = @{
      CsvPath = $config.CsvPath; TemplatePath = $config.TemplatePath; OutputFolder = $tempOutput
      AcadPath = $config.AcadPath; BlockSize = 1; InnerChunk = 0; StallMinutes = 15; StallKill = $false
      MakePdf = $true; ShowPredio = $true; ShowGeoTxt = $true; Diag = $false; RetryFailures = $true
    }
    $script:State = New-GmState $testConfig $preflight
    $script:Codes = @('HS-YA-001', 'HS-MC-002', 'HS-MC-003', 'HS-MC-004')
    $script:State.Total = $script:Codes.Count
    $script:State.Attempted = @()
    $script:State.Failed = @()
    [System.IO.File]::WriteAllText((Get-GmStatePath $tempOutput $script:ProgressName), "HS-YA-001`r`n", [System.Text.Encoding]::ASCII)
    if (-not (Test-Path -LiteralPath (Get-GmStateDir $tempOutput) -PathType Container)) {
      throw 'El estado del lote no usa la subcarpeta _sistema.'
    }
    # F5: los fixtures simulan DWGs reales (cabecera AC1032 + relleno > 1 KB);
    # con Test-GmDwgFile un archivo de 3 bytes ya no cuenta como generado.
    $fakeDwg = 'AC1032' + ('0' * 2048)
    [System.IO.File]::WriteAllText((Get-GmOutputDwg $tempOutput 'HS-YA-001'), $fakeDwg, [System.Text.Encoding]::ASCII)
    [System.IO.File]::WriteAllText((Get-GmOutputDwg $tempOutput 'HS-MC-003'), $fakeDwg, [System.Text.Encoding]::ASCII)
    $reportLines = @(
      'ETAPA;HS-YA-001;PREDIO_LOCALIZACION;AVISO;predio sin localizar;'
      'RESULTADO;HS-YA-001;FIN;OK;Plano guardado;'
      'ETAPA;HS-MC-002;ETIQUETA_PROPIETARIO;ERROR;Etiqueta sin posicion;'
      'RESULTADO;HS-MC-002;FIN;ERROR;Error de etiqueta;'
      'RESULTADO;HS-MC-003;FIN;OK;Plano guardado;'
    )
    [System.IO.File]::WriteAllLines((Join-Path $tempOutput $script:ReportName), $reportLines, [System.Text.Encoding]::ASCII)
    $scan = Update-GmStateFromFiles
    $pendingLines = [System.IO.File]::ReadAllLines((Join-Path $tempOutput $script:PendingName), [System.Text.Encoding]::Default)
    $expectedPredio = 'HS-YA-001;PREDIO_LOCALIZACION;AVISO;predio sin localizar;' + (Get-GmOutputDwg $tempOutput 'HS-YA-001')
    if ($pendingLines.Count -ne 4 -or -not ($pendingLines -contains $expectedPredio) -or
        -not ($pendingLines | Where-Object { $_ -like 'HS-MC-002;ETIQUETA_PROPIETARIO;ERROR;*' }) -or
        -not ($pendingLines | Where-Object { $_ -like 'HS-MC-004;NO_INTENTADO;AVISO;*' })) {
      throw ('La reconciliacion del CSV de pendientes no conserva ERROR/AVISO/NO_INTENTADO: ' + ($pendingLines -join ' | '))
    }
    # AUD4: una fila previa del LISP (codigo sin RESULTADO) se conserva en vez
    # de reemplazarse por el NO_INTENTADO generico del lanzador.
    $preservedRow = 'HS-MC-004;FIN;ERROR;No procesado;'
    [System.IO.File]::WriteAllLines((Join-Path $tempOutput $script:PendingName), @('CODIGO;ETAPA;ESTADO;MOTIVO;ARCHIVO', $preservedRow), [System.Text.Encoding]::Default)
    Write-GmPendings $tempOutput
    $afterPreserve = [System.IO.File]::ReadAllLines((Join-Path $tempOutput $script:PendingName), [System.Text.Encoding]::Default)
    if (-not ($afterPreserve -contains $preservedRow) -or ($afterPreserve | Where-Object { $_ -like 'HS-MC-004;NO_INTENTADO;*' })) {
      throw ('AUD4: no se conservo la fila del LISP en PENDIENTES: ' + ($afterPreserve -join ' | '))
    }
    # UI1: la vista de pendientes del lanzador puebla la tabla desde el CSV y
    # preserva motivos con ';' interno (columna MOTIVO completa).
    Add-Type -AssemblyName System.Windows.Forms
    $script:pendingList = New-Object System.Windows.Forms.ListView
    $viewFixture = @(
      'CODIGO;ETAPA;ESTADO;MOTIVO;ARCHIVO',
      'HS-AA-001;FIN;ERROR;motivo con ; interno;C:/out/HS-AA-001.dwg',
      'HS-AA-002;PREDIO_LOCALIZACION;AVISO;predio sin localizar;',
      'HS-AA-003;NO_INTENTADO;AVISO;corrida interrumpida, codigo no intentado;'
    )
    [System.IO.File]::WriteAllLines((Join-Path $tempOutput $script:PendingName), $viewFixture, [System.Text.Encoding]::Default)
    Update-GmPendingView $tempOutput
    if ($script:pendingList.Items.Count -ne 3) { throw ('UI1: la vista de pendientes esperaba 3 filas y hay ' + $script:pendingList.Items.Count + '.') }
    if ($script:pendingList.Items[0].SubItems[3].Text -ne 'motivo con ; interno') { throw 'UI1: el motivo con ; no se preserva en la vista.' }
    if ($script:pendingList.Items[1].SubItems[1].Text -ne 'PREDIO_LOCALIZACION') { throw 'UI1: la etapa no se muestra en la vista.' }
    $script:pendingList = $null
    $script:State.Attempted = @($script:Codes)
    $script:State.Failed = @('HS-MC-002')
    $script:State.Retried = @()
    $script:State.CurrentBatch = @()
    $script:State.Phase = 'INITIAL'
    $retry = Get-GmNextBatch $scan.Outcomes $scan.Progress
    $retryArray = @($retry)
    if ($retryArray.Count -ne 1 -or $retryArray[0] -ne 'HS-MC-002' -or $script:State.Phase -ne 'RETRY') {
      throw 'La cola de reintento no selecciono solo el error pendiente.'
    }
    $storedState = Read-GmState $tempOutput
    if (@($storedState.CurrentBatch).Count -ne 1 -or @($storedState.CurrentBatch)[0] -ne 'HS-MC-002' -or $storedState.Phase -ne 'RETRY') {
      throw 'El manifiesto no preserva la cola de reanudacion/reintento.'
    }
    # AUD3: el campo muerto Initialized se poda y la migracion de
    # InitializedBeforeFirstBlock garantiza que un manifiesto existente no
    # truncara el REPORTE al reanudar.
    $statePath = Get-GmStatePath $tempOutput $script:StateName
    if ($storedState.ContainsKey('Initialized')) { throw 'AUD3: el campo legado Initialized no se poda.' }
    if ($storedState['InitializedBeforeFirstBlock'] -ne $true) { throw 'AUD3: la migracion de InitializedBeforeFirstBlock no aplica $true.' }
    $rawState = [System.IO.File]::ReadAllText($statePath, [System.Text.Encoding]::UTF8)
    [System.IO.File]::WriteAllText($statePath, ($rawState -replace '^\{', '{ "Initialized": true,'), (New-Object System.Text.UTF8Encoding($false)))
    $legacyState = Read-GmState $tempOutput
    if ($legacyState.ContainsKey('Initialized')) { throw 'AUD3: un manifiesto con Initialized no se poda al leer.' }
    # AUD3: el sidecar de configuracion debe apuntar al mismo CSV; si no, la
    # reanudacion se bloquea (misma politica fail-closed que GM_CONTINUAR_LOTE).
    $sidecarPath = Get-GmStatePath $tempOutput $script:ConfigName
    $sidecarOk = @('CSV_PATH;CSV_FOLDER;MAKE_PDF;SHOW_PREDIO_LOC;SELLO;SHOW_GEO_TXT', (($config.CsvPath -replace '\\', '/') + ';x;2;1;x;1'))
    [System.IO.File]::WriteAllLines($sidecarPath, $sidecarOk, [System.Text.Encoding]::Default)
    $sidecarMatched = $false
    try { Test-GmSidecarMatches $storedState; $sidecarMatched = $true } catch { }
    if (-not $sidecarMatched) { throw 'AUD3: el sidecar coincidente no valida la reanudacion.' }
    $sidecarBad = @('CSV_PATH;CSV_FOLDER;MAKE_PDF;SHOW_PREDIO_LOC;SELLO;SHOW_GEO_TXT', 'C:/otra/ruta/OTRO.csv;x;2;1;x;1')
    [System.IO.File]::WriteAllLines($sidecarPath, $sidecarBad, [System.Text.Encoding]::Default)
    $sidecarThrew = $false
    try { Test-GmSidecarMatches $storedState } catch { $sidecarThrew = $true }
    if (-not $sidecarThrew) { throw 'AUD3: un sidecar con otro CSV no bloquea la reanudacion.' }
    # PRY1: sidecar de 8 campos: el mismo tipo valida; otro tipo bloquea.
    $sidecarTipoOk = @('CSV_PATH;CSV_FOLDER;MAKE_PDF;SHOW_PREDIO_LOC;SELLO;SHOW_GEO_TXT;TIPO_PLANO;VP_TYPE', (($config.CsvPath -replace '\\', '/') + ';x;2;1;x;1;UBICACION;1'))
    [System.IO.File]::WriteAllLines($sidecarPath, $sidecarTipoOk, [System.Text.Encoding]::Default)
    try { Test-GmSidecarMatches $storedState } catch { throw ('PRY1: un sidecar con el mismo tipo de plano no debe bloquear: ' + $_.Exception.Message) }
    $sidecarTipoBad = @('CSV_PATH;CSV_FOLDER;MAKE_PDF;SHOW_PREDIO_LOC;SELLO;SHOW_GEO_TXT;TIPO_PLANO;VP_TYPE', (($config.CsvPath -replace '\\', '/') + ';x;2;1;x;1;MATRIZ;1'))
    [System.IO.File]::WriteAllLines($sidecarPath, $sidecarTipoBad, [System.Text.Encoding]::Default)
    $sidecarTipoThrew = $false
    try { Test-GmSidecarMatches $storedState } catch { $sidecarTipoThrew = $true }
    if (-not $sidecarTipoThrew) { throw 'PRY1: un sidecar con otro tipo de plano no bloquea la reanudacion.' }
    # AUD3: si el manifiesto principal desaparece, la reanudacion usa el .bak.
    $stateBak = $statePath + '.bak'
    Copy-Item -LiteralPath $statePath -Destination $stateBak -Force
    Remove-Item -LiteralPath $statePath -Force
    $recoveredState = Read-GmState $tempOutput
    if (@($recoveredState.CurrentBatch).Count -ne 1 -or @($recoveredState.CurrentBatch)[0] -ne 'HS-MC-002') {
      throw 'AUD3: el respaldo .bak no recupera el manifiesto.'
    }
    # FIX4: un fallo de arranque (acad.exe inexistente) no debe consumir el
    # reintento; Retried se marca recien al lanzar el proceso.
    $script:State.Phase = 'RETRY'
    $script:State.CurrentBatch = @('HS-MC-002')
    $script:State.BatchBaseline = @{}
    $script:State.Retried = @()
    $realAcad = $script:State.AcadPath
    $script:State.AcadPath = Join-Path $tempOutput 'acad_inexistente.exe'
    $launchThrew = $false
    try { Start-GmBlock @('HS-MC-002') } catch { $launchThrew = $true }
    $script:State.AcadPath = $realAcad
    if (-not $launchThrew) { throw 'FIX4: se esperaba fallo al lanzar con un acad.exe inexistente.' }
    if (@($script:State.Retried).Count -ne 0) { throw 'FIX4: el reintento se marco antes de ejecutar el bloque.' }
    # F5: el gate de DWG distingue valido de truncado y el respaldo .bak existe.
    $badDwg = Join-Path $tempOutput 'HS-MC-009.dwg'
    [System.IO.File]::WriteAllText($badDwg, 'x', [System.Text.Encoding]::ASCII)
    if (Test-GmDwgFile $badDwg) { throw 'F5: un DWG de 1 byte no debe validar.' }
    if (-not (Test-GmDwgFile (Get-GmOutputDwg $tempOutput 'HS-YA-001'))) { throw 'F5: el fixture DWG valido no valida.' }
    # PRY1 2026-10-12: store de proyectos, biblioteca y borrado con respaldo,
    # probados en TEMP (la raiz del proyecto se sustituye por un sandbox).
    $pryTemp = Join-Path $env:TEMP ('GM_PRY1_' + [guid]::NewGuid().ToString('N'))
    New-Item -ItemType Directory -Path $pryTemp | Out-Null
    $savedRoot = $script:ProjectRoot
    try {
      $script:ProjectRoot = $pryTemp
      $fixtureRows = @(
        'AYALA BELLIDO, ANTONIO;HUICHO CUCHO;POSESIONARIO MAYOR A 10 ANOS;-;-;DERECHO;MAMAYLLA CCOCHA;AYACUCHO;RUSTICO;CANGALLO;0+000;LOS MOROCHUCOS;0+061;U.C. 24623;HS-YA-001;INDICADA;AGOSTO 2026;18 SUR;WGS 84',
        'PEREZ;LA CURVA;PROPIETARIO;-;-;IZQUIERDO;SHALAR;LA LIBERTAD;RUSTICO;SANCHEZ CARRION;1+150;CHUGAY;1+193;U.C. 09091;PC-T1-SH-001;INDICADA;MAYO 2026;18 SUR;WGS 84'
      )
      [System.IO.File]::WriteAllLines((Join-Path $pryTemp 'CAJETIN HUANCA.csv'), $fixtureRows, [System.Text.Encoding]::Default)
      [System.IO.File]::WriteAllText((Join-Path $pryTemp 'PLANTILLA_ORIGINAL_HUANCASANCOS.dwg'), $fakeDwg, [System.Text.Encoding]::Default)
      New-Item -ItemType Directory -Path (Join-Path $pryTemp 'salida') | Out-Null
      $names = @(Initialize-GmProjects)
      if ($names.Count -lt 1 -or $names[0] -ne 'huancasancos') { throw ('PRY1: la migracion esperaba [huancasancos] y dio [' + ($names -join ',') + '].') }
      $record = Read-GmProject 'huancasancos'
      if ($record.Nombre -ne 'huancasancos' -or $record.PlanType -ne 'UBICACION' -or $record.VpType -ne '1' -or $record.CsvPath -ne (Join-Path $pryTemp 'CAJETIN HUANCA.csv')) { throw 'PRY1: el proyecto migrado no conserva CSV ni defaults.' }
      $outDir = Get-GmProjectOutputPath 'huancasancos'
      if ([string]$record.OutputFolder -ne $outDir -or $record.Schema -ne 2 -or -not (Test-Path -LiteralPath $outDir -PathType Container)) {
        throw ('PRY1: la salida por proyecto no se creo/actualizo: ' + $record.OutputFolder)
      }
      if (-not (Test-GmLegacyProjectOutput 'huancasancos' (Join-Path $pryTemp 'salida'))) { throw 'PRY1: la salida legada de huancasancos no se detecta.' }
      if (-not (Test-GmLegacyProjectOutput 'chagual' (Join-Path $script:ProjectRoot '2do modelo base\salida nueva'))) { throw 'PRY1: la salida legada de chagual no se detecta.' }
      # Roundtrip: los campos sobreviven a guardar/leer.
      $record.PlanType = 'MATRIZ'; $record.VpType = '2'; $record.BlockSize = 7
      Save-GmProjectRecord $record
      $record = Read-GmProject 'huancasancos'
      if ($record.PlanType -ne 'MATRIZ' -or $record.VpType -ne '2' -or $record.BlockSize -ne 7) { throw 'PRY1: el roundtrip del proyecto no persistio los campos.' }
      # Biblioteca: archivos + REPORTE + CSV cruzados por codigo; la traza
      # TIPO_PLANO fija el tipo de los RESULTADO que le siguen y el resto cae al
      # default del proyecto (PERIMETRICO aqui, para distinguirlo del trazado).
      $outDir = Get-GmProjectOutputPath 'huancasancos'
      $record.OutputFolder = $outDir
      [System.IO.File]::WriteAllText((Join-Path $outDir 'HS-YA-001.dwg'), $fakeDwg, [System.Text.Encoding]::ASCII)
      [System.IO.File]::WriteAllText((Join-Path $outDir 'HS-YA-001 plano preliminar.pdf'), '%PDF', [System.Text.Encoding]::ASCII)
      [System.IO.File]::WriteAllText((Join-Path $outDir 'PC-T1-SH-001 plano preliminar.pdf'), '%PDF', [System.Text.Encoding]::ASCII)
      $reportRows = @(
        'TIPO;CODIGO;ETAPA;ESTADO;MENSAJE;ARCHIVO',
        'RESULTADO;PC-T1-SH-001;FIN;ERROR;Error de etiqueta;',
        'ETAPA;LOTE;TIPO_PLANO;INFO;MATRIZ;',
        'RESULTADO;HS-YA-001;FIN;OK;Plano guardado;'
      )
      [System.IO.File]::WriteAllLines((Join-Path $outDir $script:ReportName), $reportRows, [System.Text.Encoding]::Default)
      $record.PlanType = 'PERIMETRICO'
      $libRows = @(Get-GmLibraryRows $record)
      if ($libRows.Count -ne 2) { throw ('PRY1: la biblioteca esperaba 2 filas y hay ' + $libRows.Count + '.') }
      $row1 = @($libRows | Where-Object { $_.Code -eq 'HS-YA-001' })[0]
      if ($row1.Tipo -ne 'MATRIZ' -or $row1.Formato -ne 'DWG+PDF' -or $row1.Estado -ne 'OK' -or $row1.Distrito -ne 'LOS MOROCHUCOS') { throw ('PRY1: fila indexada mal construida: ' + $row1.Tipo + '/' + $row1.Formato + '/' + $row1.Estado + '/' + $row1.Distrito) }
      $row2 = @($libRows | Where-Object { $_.Code -eq 'PC-T1-SH-001' })[0]
      if ($row2.Tipo -ne 'PERIMETRICO' -or $row2.Formato -ne 'PDF' -or $row2.Estado -ne 'ERROR') { throw ('PRY1: fila sin indice mal construida: ' + $row2.Tipo + '/' + $row2.Formato + '/' + $row2.Estado) }
      if (@(Select-GmLibraryRows $libRows 'Todos' 'Todos' '').Count -ne 2) { throw 'PRY1: sin filtros debe listar las 2 filas.' }
      if (@(Select-GmLibraryRows $libRows 'MATRIZ' 'Todos' '').Count -ne 1) { throw 'PRY1: el filtro por tipo fallo.' }
      if (@(Select-GmLibraryRows $libRows 'Todos' 'DWG+PDF' '').Count -ne 1) { throw 'PRY1: el filtro por formato fallo.' }
      if (@(Select-GmLibraryRows $libRows 'Todos' 'Todos' 'chugay').Count -ne 1) { throw 'PRY1: la busqueda por distrito fallo.' }
      if (@(Select-GmLibraryRows $libRows 'Todos' 'Todos' '24623').Count -ne 1) { throw 'PRY1: la busqueda por U.C. fallo.' }
      if (@(Select-GmLibraryRows $libRows 'Todos' 'Todos' 'ya-001').Count -ne 1) { throw 'PRY1: la busqueda por codigo fallo.' }
      if ((ConvertTo-GmSearchText ('JOS' + [char]0x00C9)) -ne 'JOSE') { throw 'PRY1: la normalizacion no quita tildes.' }
      # SAL2: la salida personalizada NO se toca y la legada no se mueve.
      $customOut = Join-Path $pryTemp 'salida_custom'
      New-Item -ItemType Directory -Path $customOut -Force | Out-Null
      $customProject = New-GmProjectRecord 'custom'
      $customProject.OutputFolder = $customOut
      $customProject.Schema = 1
      Save-GmProjectRecord $customProject
      @(Initialize-GmProjects) | Out-Null
      $customAfter = Read-GmProject 'custom'
      if ([string]$customAfter.OutputFolder -ne $customOut -or $customAfter.Schema -ne 2) { throw ('PRY1: la salida personalizada se altero: ' + $customAfter.OutputFolder + ' / schema=' + $customAfter.Schema) }
      # FASE1 2026-10-21: nombres reservados, guardas de la salida y sync con
      # salida vacia. El borrado no puede arrastrar el store ni el trabajo de
      # otro proyecto (y un proyecto llamado CON no existe en Windows).
      if (Test-GmProjectName 'CON') { throw 'FASE1: CON es un nombre reservado de Windows.' }
      if (Test-GmProjectName 'nul') { throw 'FASE1: NUL es un nombre reservado de Windows.' }
      if (Test-GmProjectName '_borrados') { throw 'FASE1: _borrados esta reservado (respaldo de proyectos).' }
      if (-not (Test-GmProjectName 'proyecto-1')) { throw 'FASE1: un nombre normal debe aceptarse.' }
      if (Test-GmProjectOutputMovable 'huancasancos' (Get-GmProjectsDir)) { throw 'FASE1: la carpeta de proyectos no debe ser movible.' }
      if (Test-GmProjectOutputMovable 'huancasancos' (Join-Path (Get-GmProjectsDir) $script:ProjectsBackupName)) { throw 'FASE1: la carpeta _borrados no debe ser movible.' }
      if (Test-GmProjectOutputMovable 'huancasancos' (Get-GmProjectDir 'chagual')) { throw 'FASE1: la carpeta de otro proyecto no debe ser movible.' }
      if (-not (Test-GmProjectOutputMovable 'huancasancos' (Join-Path $pryTemp 'salida'))) { throw 'FASE1: una salida normal si debe ser movible.' }
      # Sync: schema 2 con salida vacia debe fijar el default Y crearlo.
      $syncRec = New-GmProjectRecord 'sincroniza'
      $syncRec.Schema = 2
      $syncRec.OutputFolder = ''
      $syncRec = Sync-GmProjectOutput $syncRec
      $syncExpected = Get-GmProjectOutputPath 'sincroniza'
      if ([string]$syncRec.OutputFolder -ne $syncExpected) { throw ('FASE1: el sync no fijo la salida por proyecto: ' + $syncRec.OutputFolder) }
      if (-not (Test-Path -LiteralPath $syncExpected -PathType Container)) { throw 'FASE1: el sync no creo la carpeta de salida por proyecto.' }
      # CSV en UTF-16 (Excel "Texto Unicode"): debe leerse igual que en ANSI.
      $utf16Csv = Join-Path $pryTemp 'utf16.csv'
      [System.IO.File]::WriteAllLines($utf16Csv, $fixtureRows, [System.Text.Encoding]::Unicode)
      $utf16 = Read-GmCsv $utf16Csv
      if ($utf16.Rows.Count -ne 2) { throw ('FASE1: el CSV en UTF-16 no se leio: ' + $utf16.Rows.Count + ' filas.') }
      if ($utf16.Codes[0] -ne 'HS-YA-001' -or $utf16.Codes[1] -ne 'PC-T1-SH-001') { throw 'FASE1: el CSV en UTF-16 devolvio codigos incorrectos.' }
      # F3: guardas del borrado antes de tocar nada.
      if (Test-GmPathDeletable ([System.IO.Path]::GetPathRoot((Get-GmProjectsDir)))) { throw 'PRY1: la raiz de una unidad no debe ser movible/borrable.' }
      if (Test-GmPathDeletable $script:ProjectRoot) { throw 'PRY1: la raiz del sistema no debe ser movible/borrable.' }
      if (-not (Test-GmPathDeletable (Join-Path $pryTemp 'salida'))) { throw 'PRY1: una carpeta de salida normal debe ser movible/borrable.' }
      if (-not (Test-GmPathDeletable $outDir)) { throw 'PRY1: la nueva salida del proyecto debe ser movible/borrable.' }
      # Borrado con respaldo: el registro y la salida quedan en _borrados.
      $removed = Remove-GmProjectFiles 'huancasancos'
      $dest = $removed.Backup
      if (Test-Path -LiteralPath (Get-GmProjectDir 'huancasancos')) { throw 'PRY1: el proyecto debia desaparecer del store.' }
      if (Test-Path -LiteralPath $outDir) { throw 'PRY1: la salida por proyecto debia moverse al respaldo.' }
      if (-not (Test-Path -LiteralPath (Join-Path $dest 'salida'))) { throw 'PRY1: el respaldo no contiene la carpeta salida.' }
      if (-not (Test-Path -LiteralPath (Join-Path $dest 'proyecto'))) { throw 'PRY1: el respaldo no contiene el registro del proyecto.' }
      if (-not (Test-Path -LiteralPath (Join-Path $pryTemp 'salida'))) { throw 'PRY1: la salida legada debia conservarse.' }
      if (@(Get-GmProjectNames).Count -lt 1) { throw 'PRY1: el borrado debia dejar el resto de proyectos.' }
    } finally {
      $script:ProjectRoot = $savedRoot
      Remove-Item -LiteralPath $pryTemp -Recurse -Force -ErrorAction SilentlyContinue
    }
    $tempRootSelf = Join-Path $env:TEMP ('GM_LOTE_' + $script:State.RunId)
    Remove-Item -LiteralPath $tempRootSelf -Recurse -Force -ErrorAction SilentlyContinue
  } finally {
    Remove-Item -LiteralPath $tempOutput -Recurse -Force -ErrorAction SilentlyContinue
  }
  } finally {
    Remove-Item -LiteralPath $tempOutput -Recurse -Force -ErrorAction SilentlyContinue
  }
  Write-Output ("SELFTEST OK; CAJETIN={0}; PILOTO={1}; DELIM={2}; SELLO_AUTO={3}; PREFLIGHT=OK; BOOT=OK; PENDIENTES=OK; RETRY=OK; FIX4=OK; F5=OK; AUD3=OK; AUD4=OK; UI1=OK; PRY1=OK" -f $csv.Rows.Count, $pilot.Rows.Count, $csv.Delimiter, $stamps['AUTOMATIZACION.lsp'])
  exit 0
}

Add-Type -AssemblyName System.Windows.Forms
Add-Type -AssemblyName System.Drawing
[System.Windows.Forms.Application]::EnableVisualStyles()

$script:State = $null
$script:Rows = @()
$script:Codes = @()
$script:activeProc = $null
$script:activeBlockDir = $null
$script:currentBatch = @()
$script:runActive = $false
$script:StopRequested = $false
$script:CpuPrevious = @{}
$script:stallWarned = $false
$script:stallKilled = $false
$script:lastActivityUtc = [DateTime]::UtcNow
$script:PilotTestMode = [bool]$PilotTest
$script:PilotExitCode = 0
$script:PilotOutput = ''
$script:pendingList = $null
# PRY1: proyecto activo (hashtable ordenada leida de proyecto.json) y filas
# cacheadas de la biblioteca para filtrar sin releer el disco en cada tecla.
$script:ActiveProject = $null
$script:libraryRows = @()

$form = New-Object System.Windows.Forms.Form
$form.Text = 'GM Ingenieros - Lanzador de lotes'
$form.StartPosition = 'CenterScreen'
# PRY1-UI (2026-10-13): en pantallas angostas o con escala DPI alta la ventana
# de 900x940 podia exceder el area de trabajo y los botones "Examinar..." de la
# derecha quedaban fuera de pantalla. La ventana se limita al area disponible.
$script:workArea = [System.Windows.Forms.Screen]::PrimaryScreen.WorkingArea
$form.Size = [System.Drawing.Size]::new([math]::Min(900, $script:workArea.Width), [math]::Min(940, $script:workArea.Height))
$form.MinimumSize = [System.Drawing.Size]::new([math]::Min(880, $form.Width), [math]::Min(900, $form.Height))
$form.AutoScaleMode = 'Font'
$form.Font = [System.Drawing.Font]::new('Segoe UI', 9)

# PRY1: dos paneles que se alternan (solo uno visible). El detalle conserva
# TODA la interfaz previa; el panel de proyectos lista, crea y abre proyectos.
$script:panelDetail = New-Object System.Windows.Forms.Panel
$script:panelDetail.Dock = 'Fill'
$script:panelDetail.Visible = $false
# PRY1-UI: el panel nace con 200px de ancho por defecto; sin fijar el tamano
# real del cliente, las anclas de los hijos se capturaban contra 200px y los
# controles de la derecha quedaban dibujados fuera del panel (los botones
# "Examinar..." de las rutas eran invisibles). Es la causa raiz del corte.
$script:panelDetail.Size = $form.ClientSize
$form.Controls.Add($script:panelDetail)
$script:panelProjects = New-Object System.Windows.Forms.Panel
$script:panelProjects.Dock = 'Fill'
$script:panelProjects.Visible = $true
$script:panelProjects.Size = $form.ClientSize
$form.Controls.Add($script:panelProjects)

# PRY1-UI (2026-10-13): cada fila de ruta es un TableLayoutPanel (etiqueta fija
# + caja elastica + boton "Examinar..." fijo). Antes el boton vivia suelto en
# x=765 y en pantallas angostas o con escala DPI quedaba fuera del area visible.
$script:pathRows = @()
$script:pathBrowseButtons = @()
function Add-GmPathRow([string]$Caption, [int]$Top, [string]$Default, [string]$ButtonText, [scriptblock]$OnClick) {
  $row = New-Object System.Windows.Forms.TableLayoutPanel
  $row.Location = [System.Drawing.Point]::new(16, $Top)
  $row.Size = [System.Drawing.Size]::new(854, 29)
  $row.Anchor = 'Top,Left,Right'
  $row.ColumnCount = 3
  $row.RowCount = 1
  [void]$row.ColumnStyles.Add((New-Object System.Windows.Forms.ColumnStyle([System.Windows.Forms.SizeType]::Absolute, 112)))
  [void]$row.ColumnStyles.Add((New-Object System.Windows.Forms.ColumnStyle([System.Windows.Forms.SizeType]::Percent, 100)))
  [void]$row.ColumnStyles.Add((New-Object System.Windows.Forms.ColumnStyle([System.Windows.Forms.SizeType]::Absolute, 115)))
  [void]$row.RowStyles.Add((New-Object System.Windows.Forms.RowStyle([System.Windows.Forms.SizeType]::Absolute, 29)))
  $label = New-Object System.Windows.Forms.Label
  $label.Text = $Caption
  $label.Dock = 'Fill'
  $label.TextAlign = 'MiddleLeft'
  $row.Controls.Add($label, 0, 0)
  $box = New-Object System.Windows.Forms.TextBox
  $box.Anchor = 'Left,Right'
  $box.Margin = New-Object System.Windows.Forms.Padding(0, 2, 10, 0)
  $box.Text = $Default
  $row.Controls.Add($box, 1, 0)
  $button = New-Object System.Windows.Forms.Button
  $button.Text = $ButtonText
  $button.Dock = 'Fill'
  $button.Margin = New-Object System.Windows.Forms.Padding(0, 1, 0, 1)
  $button.Add_Click($OnClick)
  $row.Controls.Add($button, 2, 0)
  $script:panelDetail.Controls.Add($row)
  $script:pathRows += @($row)
  $script:pathBrowseButtons += @($button)
  return $box
}

$defaultCsv = Join-Path $script:ProjectRoot 'CAJETIN HUANCA.csv'
$defaultTemplate = Join-Path $script:ProjectRoot 'PLANTILLA_ORIGINAL_HUANCASANCOS.dwg'
$defaultOutput = Get-GmProjectOutputPath 'huancasancos'

# PRY1: encabezado del detalle (volver + nombre del proyecto).
$script:btnBackToProjects = New-Object System.Windows.Forms.Button
$script:btnBackToProjects.Text = '< Proyectos'
$script:btnBackToProjects.Location = [System.Drawing.Point]::new(16, 10)
$script:btnBackToProjects.Size = [System.Drawing.Size]::new(110, 30)
# PRY1-FIX: el clic vuelve al panel de proyectos (guarda el proyecto actual y
# bloquea si hay un bloque activo; vive en Show-GmProjectsPanel).
$script:btnBackToProjects.Add_Click({ Show-GmProjectsPanel })
$script:panelDetail.Controls.Add($script:btnBackToProjects)
$script:lblProjectName = New-Object System.Windows.Forms.Label
$script:lblProjectName.Text = 'Proyecto: (ninguno)'
$script:lblProjectName.Location = [System.Drawing.Point]::new(136, 16)
$script:lblProjectName.Size = [System.Drawing.Size]::new(500, 22)
$script:lblProjectName.Font = [System.Drawing.Font]::new('Segoe UI', 10, [System.Drawing.FontStyle]::Bold)
$script:panelDetail.Controls.Add($script:lblProjectName)

# PRY1-UI: al elegir una ruta se persiste el proyecto abierto en proyecto.json
# (antes solo se guardaba al volver a Proyectos, iniciar o cerrar la ventana).
$script:txtCsv = Add-GmPathRow 'CSV de entrada' 52 $defaultCsv 'Examinar...' {
  $d = New-Object System.Windows.Forms.OpenFileDialog
  $d.Filter = 'CSV (*.csv)|*.csv|Todos los archivos (*.*)|*.*'
  $d.CheckFileExists = $true
  $d.RestoreDirectory = $true
  if ($d.ShowDialog($form) -eq [System.Windows.Forms.DialogResult]::OK) { $script:txtCsv.Text = $d.FileName; Save-GmActiveProject }
}
$script:txtTemplate = Add-GmPathRow 'DWG plantilla' 88 $defaultTemplate 'Examinar...' {
  $d = New-Object System.Windows.Forms.OpenFileDialog
  $d.Filter = 'DWG (*.dwg)|*.dwg|Todos los archivos (*.*)|*.*'
  $d.CheckFileExists = $true
  $d.RestoreDirectory = $true
  if ($d.ShowDialog($form) -eq [System.Windows.Forms.DialogResult]::OK) { $script:txtTemplate.Text = $d.FileName; Save-GmActiveProject }
}
$script:txtOutput = Add-GmPathRow 'Carpeta salida' 124 $defaultOutput 'Examinar...' {
  $d = New-Object System.Windows.Forms.FolderBrowserDialog
  $d.Description = 'Selecciona la carpeta que recibira DWG, PDF y reportes.'
  $d.ShowNewFolderButton = $true
  if ($d.ShowDialog($form) -eq [System.Windows.Forms.DialogResult]::OK) { $script:txtOutput.Text = $d.SelectedPath; Update-GmUiState; Save-GmActiveProject }
}
$script:txtAcad = Add-GmPathRow 'acad.exe' 160 (Find-GmAcad) 'Examinar...' {
  $d = New-Object System.Windows.Forms.OpenFileDialog
  $d.Filter = 'AutoCAD acad.exe (acad.exe)|acad.exe|Ejecutables (*.exe)|*.exe'
  $d.CheckFileExists = $true
  $d.RestoreDirectory = $true
  if ($d.ShowDialog($form) -eq [System.Windows.Forms.DialogResult]::OK) { $script:txtAcad.Text = $d.FileName; Save-GmActiveProject }
}

$group = New-Object System.Windows.Forms.GroupBox
$group.Text = 'Opciones de generacion y reciclaje'
$group.Location = [System.Drawing.Point]::new(16, 200)
$group.Size = [System.Drawing.Size]::new(854, 212)
$group.Anchor = 'Top,Left,Right'
$script:panelDetail.Controls.Add($group)

$script:radioPdf = New-Object System.Windows.Forms.RadioButton
$script:radioPdf.Text = 'DWG + PDF'
$script:radioPdf.Location = [System.Drawing.Point]::new(16, 25)
$script:radioPdf.Size = [System.Drawing.Size]::new(120, 23)
$script:radioPdf.Checked = $true
$group.Controls.Add($script:radioPdf)
$script:radioDwg = New-Object System.Windows.Forms.RadioButton
$script:radioDwg.Text = 'Solo DWG'
$script:radioDwg.Location = [System.Drawing.Point]::new(150, 25)
$script:radioDwg.Size = [System.Drawing.Size]::new(110, 23)
$group.Controls.Add($script:radioDwg)

$script:checkPredio = New-Object System.Windows.Forms.CheckBox
$script:checkPredio.Text = 'Senalizar predio en el mapa regional (circulos verdes)'
$script:checkPredio.Location = [System.Drawing.Point]::new(278, 25)
$script:checkPredio.Size = [System.Drawing.Size]::new(440, 23)
$script:checkPredio.Checked = $true
$group.Controls.Add($script:checkPredio)

# CHG2: rotulo DEP/PROV/DIST del mapa (activo por defecto; apagarlo lo omite).
$script:checkGeo = New-Object System.Windows.Forms.CheckBox
$script:checkGeo.Text = 'Dibujar rotulo DEPARTAMENTO/PROVINCIA/DISTRITO'
$script:checkGeo.Location = [System.Drawing.Point]::new(460, 135)
$script:checkGeo.Size = [System.Drawing.Size]::new(384, 23)
$script:checkGeo.Checked = $true
$group.Controls.Add($script:checkGeo)

$labelBlock = New-Object System.Windows.Forms.Label
$labelBlock.Text = 'Codigos por proceso:'
$labelBlock.Location = [System.Drawing.Point]::new(18, 61)
$labelBlock.Size = [System.Drawing.Size]::new(130, 22)
$group.Controls.Add($labelBlock)
$script:numBlock = New-Object System.Windows.Forms.NumericUpDown
$script:numBlock.Location = [System.Drawing.Point]::new(148, 58)
$script:numBlock.Size = [System.Drawing.Size]::new(66, 25)
$script:numBlock.Minimum = 1
$script:numBlock.Maximum = 100
$script:numBlock.Value = 25
$group.Controls.Add($script:numBlock)

$labelChunk = New-Object System.Windows.Forms.Label
$labelChunk.Text = 'Compactacion interna (0 = apagada):'
$labelChunk.Location = [System.Drawing.Point]::new(234, 61)
$labelChunk.Size = [System.Drawing.Size]::new(215, 22)
$group.Controls.Add($labelChunk)
$script:numChunk = New-Object System.Windows.Forms.NumericUpDown
$script:numChunk.Location = [System.Drawing.Point]::new(450, 58)
$script:numChunk.Size = [System.Drawing.Size]::new(66, 25)
$script:numChunk.Minimum = 0
$script:numChunk.Maximum = 100
$script:numChunk.Value = 0
$group.Controls.Add($script:numChunk)

$labelStall = New-Object System.Windows.Forms.Label
$labelStall.Text = 'Avisar sin actividad (min):'
$labelStall.Location = [System.Drawing.Point]::new(536, 61)
$labelStall.Size = [System.Drawing.Size]::new(160, 22)
$group.Controls.Add($labelStall)
$script:numStall = New-Object System.Windows.Forms.NumericUpDown
$script:numStall.Location = [System.Drawing.Point]::new(696, 58)
$script:numStall.Size = [System.Drawing.Size]::new(66, 25)
$script:numStall.Minimum = 2
$script:numStall.Maximum = 120
$script:numStall.Value = 15
$group.Controls.Add($script:numStall)

$script:checkDiag = New-Object System.Windows.Forms.CheckBox
$script:checkDiag.Text = 'Modo diagnostico (forense + monitor + log)'
$script:checkDiag.Location = [System.Drawing.Point]::new(18, 98)
$script:checkDiag.Size = [System.Drawing.Size]::new(320, 23)
$script:checkDiag.Checked = $false
$group.Controls.Add($script:checkDiag)
# F7: opt-in; solo cierra el AutoCAD del lanzador si duplica el umbral de aviso.
$script:checkStallKill = New-Object System.Windows.Forms.CheckBox
$script:checkStallKill.Text = 'Matar si se cuelga (2x)'
$script:checkStallKill.Location = [System.Drawing.Point]::new(340, 98)
$script:checkStallKill.Size = [System.Drawing.Size]::new(148, 23)
$script:checkStallKill.Checked = $false
$group.Controls.Add($script:checkStallKill)
$script:checkRetry = New-Object System.Windows.Forms.CheckBox
$script:checkRetry.Text = 'Reintentar errores una vez al final'
$script:checkRetry.Location = [System.Drawing.Point]::new(485, 98)
$script:checkRetry.Size = [System.Drawing.Size]::new(300, 23)
$script:checkRetry.Checked = $true
$group.Controls.Add($script:checkRetry)

$labelCopy = New-Object System.Windows.Forms.Label
$labelCopy.Text = 'Cada bloque abre una copia temporal nueva de la plantilla.'
$labelCopy.Location = [System.Drawing.Point]::new(18, 135)
$labelCopy.Size = [System.Drawing.Size]::new(430, 24)
$group.Controls.Add($labelCopy)

# PRY1/F2: tipo de plano (estilo del formato) y datos de VP de la grilla.
# Los controles se crean en F1; su cableado a estado/arranque llega en F2.
$lblPlanTypeCap = New-Object System.Windows.Forms.Label
$lblPlanTypeCap.Text = 'Tipo de plano:'
$lblPlanTypeCap.Location = [System.Drawing.Point]::new(18, 172)
$lblPlanTypeCap.Size = [System.Drawing.Size]::new(90, 22)
$group.Controls.Add($lblPlanTypeCap)
$script:comboPlanType = New-Object System.Windows.Forms.ComboBox
$script:comboPlanType.DropDownStyle = 'DropDownList'
$script:comboPlanType.Location = [System.Drawing.Point]::new(110, 168)
$script:comboPlanType.Size = [System.Drawing.Size]::new(150, 25)
foreach ($t in $script:PlanTypeList) { [void]$script:comboPlanType.Items.Add($t) }
$script:comboPlanType.SelectedIndex = 0
$group.Controls.Add($script:comboPlanType)
$lblVpCap = New-Object System.Windows.Forms.Label
$lblVpCap.Text = 'Datos de VP (grilla):'
$lblVpCap.Location = [System.Drawing.Point]::new(290, 172)
$lblVpCap.Size = [System.Drawing.Size]::new(130, 22)
$group.Controls.Add($lblVpCap)
$script:comboVpType = New-Object System.Windows.Forms.ComboBox
$script:comboVpType.DropDownStyle = 'DropDownList'
$script:comboVpType.Location = [System.Drawing.Point]::new(424, 168)
$script:comboVpType.Size = [System.Drawing.Size]::new(130, 25)
[void]$script:comboVpType.Items.Add('NORMAL')
[void]$script:comboVpType.Items.Add('UBICACION')
$script:comboVpType.SelectedIndex = 0
$group.Controls.Add($script:comboVpType)

$script:checkButton = New-Object System.Windows.Forms.Button
$script:checkButton.Text = 'Revisar configuracion'
$script:checkButton.Location = [System.Drawing.Point]::new(16, 420)
$script:checkButton.Size = [System.Drawing.Size]::new(160, 32)
$script:panelDetail.Controls.Add($script:checkButton)
$script:startButton = New-Object System.Windows.Forms.Button
$script:startButton.Text = 'Empezar desde cero'
$script:startButton.Location = [System.Drawing.Point]::new(184, 420)
$script:startButton.Size = [System.Drawing.Size]::new(150, 32)
$script:panelDetail.Controls.Add($script:startButton)
$script:resumeButton = New-Object System.Windows.Forms.Button
$script:resumeButton.Text = 'Reanudar corrida'
$script:resumeButton.Location = [System.Drawing.Point]::new(342, 420)
$script:resumeButton.Size = [System.Drawing.Size]::new(140, 32)
$script:panelDetail.Controls.Add($script:resumeButton)
$script:stopButton = New-Object System.Windows.Forms.Button
$script:stopButton.Text = 'Detener bloque'
$script:stopButton.Location = [System.Drawing.Point]::new(490, 420)
$script:stopButton.Size = [System.Drawing.Size]::new(125, 32)
$script:stopButton.Enabled = $false
$script:panelDetail.Controls.Add($script:stopButton)
$script:openButton = New-Object System.Windows.Forms.Button
$script:openButton.Text = 'Abrir salida'
$script:openButton.Location = [System.Drawing.Point]::new(623, 420)
$script:openButton.Size = [System.Drawing.Size]::new(105, 32)
$script:openButton.Anchor = 'Top,Right'
$script:panelDetail.Controls.Add($script:openButton)
$dumpButton = New-Object System.Windows.Forms.Button
$dumpButton.Text = 'Activar LocalDumps (admin)'
$dumpButton.Location = [System.Drawing.Point]::new(736, 420)
$dumpButton.Size = [System.Drawing.Size]::new(134, 32)
$dumpButton.Anchor = 'Top,Right'
$dumpButton.Font = [System.Drawing.Font]::new('Segoe UI', 8)
$script:panelDetail.Controls.Add($dumpButton)

$script:progressBar = New-Object System.Windows.Forms.ProgressBar
$script:progressBar.Location = [System.Drawing.Point]::new(16, 460)
$script:progressBar.Size = [System.Drawing.Size]::new(854, 22)
$script:progressBar.Anchor = 'Top,Left,Right'
$script:panelDetail.Controls.Add($script:progressBar)
$script:statusLabel = New-Object System.Windows.Forms.Label
$script:statusLabel.Text = 'Listo. Revisar configuracion antes de iniciar.'
$script:statusLabel.Location = [System.Drawing.Point]::new(16, 488)
$script:statusLabel.Size = [System.Drawing.Size]::new(854, 24)
$script:statusLabel.Anchor = 'Top,Left,Right'
$script:panelDetail.Controls.Add($script:statusLabel)
$script:logList = New-Object System.Windows.Forms.ListBox
$script:logList.Location = [System.Drawing.Point]::new(16, 518)
$script:logList.Size = [System.Drawing.Size]::new(854, 96)
$script:logList.Anchor = 'Top,Left,Right'
$script:panelDetail.Controls.Add($script:logList)

# UI1 + PRY1: el area inferior tiene dos pestanas. 'Pendientes' conserva la
# tabla previa (doble clic abre el PDF/DWG del codigo); 'Planos del proyecto'
# es la biblioteca con filtros por tipo, formato y texto unico.
$script:bottomTabs = New-Object System.Windows.Forms.TabControl
$script:bottomTabs.Location = [System.Drawing.Point]::new(16, 621)
$script:bottomTabs.Size = [System.Drawing.Size]::new(854, 264)
$script:bottomTabs.Anchor = 'Top,Bottom,Left,Right'
$script:panelDetail.Controls.Add($script:bottomTabs)
$tabPending = New-Object System.Windows.Forms.TabPage
$tabPending.Text = 'Pendientes (doble clic abre el plano)'
$tabPending.Padding = [System.Windows.Forms.Padding]::new(8)
$script:bottomTabs.Controls.Add($tabPending)
$tabLibrary = New-Object System.Windows.Forms.TabPage
$tabLibrary.Text = 'Planos del proyecto'
$tabLibrary.Padding = [System.Windows.Forms.Padding]::new(8)
$script:bottomTabs.Controls.Add($tabLibrary)

$script:pendingList = New-Object System.Windows.Forms.ListView
$script:pendingList.Dock = 'Fill'
$script:pendingList.View = 'Details'
$script:pendingList.FullRowSelect = $true
$script:pendingList.GridLines = $true
$script:pendingList.HideSelection = $false
[void]$script:pendingList.Columns.Add('CODIGO', 110)
[void]$script:pendingList.Columns.Add('ETAPA', 190)
[void]$script:pendingList.Columns.Add('ESTADO', 80)
[void]$script:pendingList.Columns.Add('MOTIVO', 460)
$tabPending.Controls.Add($script:pendingList)
$script:pendingList.Add_DoubleClick({
  if ($script:pendingList.SelectedItems.Count -eq 0) { return }
  $code = $script:pendingList.SelectedItems[0].Text.Trim()
  if ($code -eq '' -or $code -eq 'Sin pendientes') { return }
  $folder = $script:txtOutput.Text.Trim()
  $safe = [regex]::Replace($code, '[^A-Za-z0-9_-]', '_')
  if ($safe -eq '') { $safe = 'SIN_CODIGO' }
  $pdf = Join-Path $folder ($safe + ' plano preliminar.pdf')
  $dwg = Join-Path $folder ($safe + '.dwg')
  if (Test-Path -LiteralPath $pdf -PathType Leaf) { Start-Process -FilePath $pdf; return }
  if (Test-Path -LiteralPath $dwg -PathType Leaf) { Start-Process -FilePath $dwg; return }
  Add-GmLog "Pendiente $code sin DWG/PDF en la carpeta de salida."
})

# PRY1: biblioteca de planos del proyecto (filtros + tabla). Los datos se
# reconstruyen al abrir el proyecto, al refrescar y al cerrar cada bloque.
$lblLibTipo = New-Object System.Windows.Forms.Label
$lblLibTipo.Text = 'Tipo:'
$lblLibTipo.Location = [System.Drawing.Point]::new(8, 12)
$lblLibTipo.Size = [System.Drawing.Size]::new(36, 22)
$tabLibrary.Controls.Add($lblLibTipo)
$script:cmbLibTipo = New-Object System.Windows.Forms.ComboBox
$script:cmbLibTipo.DropDownStyle = 'DropDownList'
$script:cmbLibTipo.Location = [System.Drawing.Point]::new(46, 8)
$script:cmbLibTipo.Size = [System.Drawing.Size]::new(122, 25)
foreach ($item in @('Todos') + $script:PlanTypeList) { [void]$script:cmbLibTipo.Items.Add($item) }
$script:cmbLibTipo.SelectedIndex = 0
$tabLibrary.Controls.Add($script:cmbLibTipo)
$lblLibFormato = New-Object System.Windows.Forms.Label
$lblLibFormato.Text = 'Formato:'
$lblLibFormato.Location = [System.Drawing.Point]::new(184, 12)
$lblLibFormato.Size = [System.Drawing.Size]::new(56, 22)
$tabLibrary.Controls.Add($lblLibFormato)
$script:cmbLibFormato = New-Object System.Windows.Forms.ComboBox
$script:cmbLibFormato.DropDownStyle = 'DropDownList'
$script:cmbLibFormato.Location = [System.Drawing.Point]::new(244, 8)
$script:cmbLibFormato.Size = [System.Drawing.Size]::new(100, 25)
foreach ($item in @('Todos', 'DWG', 'PDF', 'DWG+PDF')) { [void]$script:cmbLibFormato.Items.Add($item) }
$script:cmbLibFormato.SelectedIndex = 0
$tabLibrary.Controls.Add($script:cmbLibFormato)
$lblLibBuscar = New-Object System.Windows.Forms.Label
$lblLibBuscar.Text = 'Buscar (codigo / U.C. / distrito):'
$lblLibBuscar.Location = [System.Drawing.Point]::new(356, 12)
$lblLibBuscar.Size = [System.Drawing.Size]::new(180, 22)
$tabLibrary.Controls.Add($lblLibBuscar)
$script:txtLibSearch = New-Object System.Windows.Forms.TextBox
$script:txtLibSearch.Location = [System.Drawing.Point]::new(540, 8)
$script:txtLibSearch.Size = [System.Drawing.Size]::new(190, 25)
$tabLibrary.Controls.Add($script:txtLibSearch)
$script:btnLibRefresh = New-Object System.Windows.Forms.Button
$script:btnLibRefresh.Text = 'Refrescar'
$script:btnLibRefresh.Location = [System.Drawing.Point]::new(740, 7)
$script:btnLibRefresh.Size = [System.Drawing.Size]::new(88, 26)
$tabLibrary.Controls.Add($script:btnLibRefresh)
$script:gridLibrary = New-Object System.Windows.Forms.DataGridView
$script:gridLibrary.Location = [System.Drawing.Point]::new(8, 38)
$script:gridLibrary.Size = [System.Drawing.Size]::new(830, 180)
$script:gridLibrary.Anchor = 'Top,Bottom,Left,Right'
$script:gridLibrary.ReadOnly = $true
$script:gridLibrary.AllowUserToAddRows = $false
$script:gridLibrary.AllowUserToDeleteRows = $false
$script:gridLibrary.AllowUserToResizeRows = $false
$script:gridLibrary.RowHeadersVisible = $false
$script:gridLibrary.SelectionMode = 'FullRowSelect'
$script:gridLibrary.MultiSelect = $false
foreach ($col in @(
  @('CODIGO', 'CODIGO', 100), @('TIPO', 'TIPO', 95), @('FORMATO', 'FORMATO', 85), @('ESTADO', 'ESTADO', 95),
  @('UC', 'U.C.', 90), @('DISTRITO', 'DISTRITO', 145), @('ARCHIVO', 'ARCHIVO', 220))) {
  $column = New-Object System.Windows.Forms.DataGridViewTextBoxColumn
  $column.Name = $col[0]
  $column.HeaderText = $col[1]
  $column.ReadOnly = $true
  if ($col[0] -eq 'ARCHIVO') { $column.AutoSizeMode = 'Fill' } else { $column.Width = $col[2] }
  [void]$script:gridLibrary.Columns.Add($column)
}
$tabLibrary.Controls.Add($script:gridLibrary)
$script:lblLibCount = New-Object System.Windows.Forms.Label
$script:lblLibCount.Text = 'Mostrando 0 planos'
$script:lblLibCount.Location = [System.Drawing.Point]::new(8, 226)
$script:lblLibCount.Size = [System.Drawing.Size]::new(830, 20)
$script:lblLibCount.Anchor = 'Bottom,Left,Right'
$tabLibrary.Controls.Add($script:lblLibCount)
$libFilterChanged = { Update-GmLibraryView }
$script:cmbLibTipo.Add_SelectedIndexChanged($libFilterChanged)
$script:cmbLibFormato.Add_SelectedIndexChanged($libFilterChanged)
$script:txtLibSearch.Add_TextChanged($libFilterChanged)
$script:btnLibRefresh.Add_Click({ Refresh-GmLibrary })
$script:gridLibrary.Add_CellDoubleClick({
  param($sender, $e)
  if ($e.RowIndex -lt 0) { return }
  $file = [string]$script:gridLibrary.Rows[$e.RowIndex].Cells['ARCHIVO'].Value
  if ($file -and (Test-Path -LiteralPath $file -PathType Leaf)) {
    Start-Process -FilePath $file
  } else {
    Add-GmLog 'Ese codigo no tiene DWG/PDF en la carpeta de salida.'
  }
})

# ============================================================================
# PRY1: funciones UI de proyectos y biblioteca (se invocan solo desde la UI,
# despues de crear los controles).
# ============================================================================
function Refresh-GmProjectList {
  if (-not $script:projectList) { return }
  $script:projectList.BeginUpdate()
  try {
    $script:projectList.Items.Clear()
    foreach ($name in @(Get-GmProjectNames)) {
      $project = $null
      try { $project = Read-GmProject $name } catch { continue }
      if ($null -eq $project) { continue }
      $item = New-Object System.Windows.Forms.ListViewItem($name)
      [void]$item.SubItems.Add([string]$project.PlanType)
      [void]$item.SubItems.Add([string]$project.CsvPath)
      [void]$item.SubItems.Add([string]$project.OutputFolder)
      $updated = [string]$project.Actualizado
      if ($updated.Length -ge 16) { $updated = $updated.Substring(0, 16).Replace('T', ' ') }
      [void]$item.SubItems.Add($updated)
      [void]$script:projectList.Items.Add($item)
    }
    if ($script:ActiveProject) {
      foreach ($item in $script:projectList.Items) {
        if ([string]::Equals($item.Text, [string]$script:ActiveProject.Nombre, [System.StringComparison]::OrdinalIgnoreCase)) { $item.Selected = $true; break }
      }
    }
  } finally {
    $script:projectList.EndUpdate()
  }
}

function Update-GmLibraryView {
  if (-not $script:gridLibrary) { return }
  $tipo = if ($script:cmbLibTipo.SelectedItem) { [string]$script:cmbLibTipo.SelectedItem } else { 'Todos' }
  $formato = if ($script:cmbLibFormato.SelectedItem) { [string]$script:cmbLibFormato.SelectedItem } else { 'Todos' }
  $query = [string]$script:txtLibSearch.Text
  $rows = @(Select-GmLibraryRows $script:libraryRows $tipo $formato $query)
  $script:gridLibrary.SuspendLayout()
  try {
    $script:gridLibrary.Rows.Clear()
    foreach ($row in $rows) {
      [void]$script:gridLibrary.Rows.Add($row.Code, $row.Tipo, $row.Formato, $row.Estado, $row.Uc, $row.Distrito, $row.Archivo)
    }
  } finally {
    $script:gridLibrary.ResumeLayout()
  }
  $script:lblLibCount.Text = ('Mostrando {0} de {1} planos' -f $rows.Count, @($script:libraryRows).Count)
}

function Refresh-GmLibrary {
  if (-not $script:gridLibrary) { return }
  if ($null -eq $script:ActiveProject) {
    $script:libraryRows = @()
    Update-GmLibraryView
    return
  }
  try {
    $script:libraryRows = @(Get-GmLibraryRows $script:ActiveProject)
  } catch {
    $script:libraryRows = @()
    Add-GmLog ('Biblioteca: no se pudo construir la lista de planos: ' + $_.Exception.Message)
  }
  Update-GmLibraryView
}

function Save-GmActiveProject {
  if ($null -eq $script:ActiveProject) { return }
  $project = $script:ActiveProject
  $config = Get-GmConfigFromUi
  $project.CsvPath = $config.CsvPath
  $project.TemplatePath = $config.TemplatePath
  $project.OutputFolder = $config.OutputFolder
  $project.AcadPath = $config.AcadPath
  $project.MakePdf = $config.MakePdf
  $project.ShowPredio = $config.ShowPredio
  $project.ShowGeoTxt = $config.ShowGeoTxt
  $project.PlanType = $config.PlanType
  $project.VpType = $config.VpType
  $project.BlockSize = $config.BlockSize
  $project.InnerChunk = $config.InnerChunk
  $project.StallMinutes = $config.StallMinutes
  $project.Diag = $config.Diag
  $project.RetryFailures = $config.RetryFailures
  $project.StallKill = $config.StallKill
  try {
    Save-GmProjectRecord $project
  } catch {
    Add-GmLog ('Proyecto: no se pudo guardar la configuracion: ' + $_.Exception.Message)
  }
}

function Set-GmUiFromProject($Project) {
  $script:txtCsv.Text = [string]$Project.CsvPath
  $script:txtTemplate.Text = [string]$Project.TemplatePath
  $script:txtOutput.Text = [string]$Project.OutputFolder
  if ([string]$Project.AcadPath) { $script:txtAcad.Text = [string]$Project.AcadPath }
  $script:numBlock.Value = [decimal][math]::Max(1, [math]::Min(100, [int]$Project.BlockSize))
  $script:numChunk.Value = [decimal][math]::Max(0, [math]::Min(100, [int]$Project.InnerChunk))
  $script:numStall.Value = [decimal][math]::Max(2, [math]::Min(120, [int]$Project.StallMinutes))
  $script:radioPdf.Checked = [bool]$Project.MakePdf
  $script:radioDwg.Checked = -not [bool]$Project.MakePdf
  $script:checkPredio.Checked = [bool]$Project.ShowPredio
  $script:checkGeo.Checked = [bool]$Project.ShowGeoTxt
  $script:checkDiag.Checked = [bool]$Project.Diag
  $script:checkRetry.Checked = [bool]$Project.RetryFailures
  $script:checkStallKill.Checked = [bool]$Project.StallKill
  # PRY1/F2: tipo de plano y datos de VP guardados en el proyecto.
  $script:comboPlanType.SelectedItem = Get-GmValidPlanType $Project.PlanType
  $script:comboVpType.SelectedItem = $(if ((Get-GmValidVpType $Project.VpType) -eq '2') { 'UBICACION' } else { 'NORMAL' })
}

function Show-GmProjectsPanel {
  if ($script:activeProc) {
    [System.Windows.Forms.MessageBox]::Show('Hay un bloque AutoCAD activo. Deten el bloque antes de volver a proyectos.', 'GM - Proyecto', 'OK', 'Warning') | Out-Null
    return
  }
  Save-GmActiveProject
  Refresh-GmProjectList
  $script:panelDetail.Visible = $false
  $script:panelProjects.Visible = $true
}

function Show-GmDetailPanel {
  $name = if ($script:ActiveProject) { [string]$script:ActiveProject.Nombre } else { '(sin proyecto)' }
  $script:lblProjectName.Text = 'Proyecto: ' + $name
  $script:panelProjects.Visible = $false
  $script:panelDetail.Visible = $true
}

function Open-GmProject([string]$Name) {
  if ($script:activeProc) {
    [System.Windows.Forms.MessageBox]::Show('Hay un bloque AutoCAD activo; no se puede cambiar de proyecto ahora.', 'GM - Proyecto', 'OK', 'Warning') | Out-Null
    return
  }
  Save-GmActiveProject
  $project = Read-GmProject $Name
  if ($null -eq $project) {
    [System.Windows.Forms.MessageBox]::Show("No se pudo leer el proyecto '$Name'.", 'GM - Proyecto', 'OK', 'Error') | Out-Null
    return
  }
  $script:ActiveProject = $project
  # FASE1 2026-10-21: al abrir un proyecto se sincroniza su salida. El LEEME
  # promete que la carpeta de salida del proyecto se crea al abrirlo; antes
  # solo se leia el registro y, si la carpeta no existia, el lote fallaba al
  # escribir en un sitio inexistente.
  try {
    $project = Sync-GmProjectOutput $project
    $script:ActiveProject = $project
  } catch {
    [System.Windows.Forms.MessageBox]::Show("No se pudo preparar la salida del proyecto '$Name': " + $_.Exception.Message, 'GM - Proyecto', 'OK', 'Error') | Out-Null
    return
  }
  Set-GmUiFromProject $project
  Show-GmDetailPanel
  Refresh-GmLibrary
  Add-GmLog "Proyecto '$Name' abierto."
}

function New-GmProjectInteractive {
  if ($script:activeProc) {
    [System.Windows.Forms.MessageBox]::Show('Hay un bloque AutoCAD activo; no se puede crear un proyecto ahora.', 'GM - Proyecto', 'OK', 'Warning') | Out-Null
    return
  }
  $name = [string]([Microsoft.VisualBasic.Interaction]::InputBox('Nombre del proyecto (letras, numeros, guion y guion bajo):', 'GM - Crear proyecto', ''))
  $name = $name.Trim()
  if ($name -eq '') { return }
  if (-not (Test-GmProjectName $name)) {
    [System.Windows.Forms.MessageBox]::Show('Nombre invalido: usa solo letras, numeros, guion y guion bajo.', 'GM - Crear proyecto', 'OK', 'Warning') | Out-Null
    return
  }
  if (Test-Path -LiteralPath (Get-GmProjectDir $name)) {
    [System.Windows.Forms.MessageBox]::Show("Ya existe el proyecto '$name'.", 'GM - Crear proyecto', 'OK', 'Warning') | Out-Null
    return
  }
  New-Item -ItemType Directory -Path (Get-GmProjectDir $name) -Force | Out-Null
  $project = New-GmProjectRecord $name
  $project.OutputFolder = Get-GmProjectOutputPath $name
  New-Item -ItemType Directory -Path $project.OutputFolder -Force | Out-Null
  $defaultTemplate = Join-Path $script:ProjectRoot 'PLANTILLA_ORIGINAL_HUANCASANCOS.dwg'
  if (Test-Path -LiteralPath $defaultTemplate -PathType Leaf) { $project.TemplatePath = $defaultTemplate }
  $project.AcadPath = $script:txtAcad.Text.Trim()
  Save-GmProjectRecord $project
  Refresh-GmProjectList
  Open-GmProject $name
  Add-GmLog "Proyecto '$name' creado; configure CSV, plantilla y salida."
}

# PRY1 (F3): elimina el proyecto seleccionado con confirmacion explicita. El
# motor Remove-GmProjectFiles (probado en -SelfTest) mueve registro + salida a
# _borrados; los CSV y la plantilla de entrada nunca se tocan.
function Remove-GmProjectInteractive {
  if ($script:activeProc) {
    [System.Windows.Forms.MessageBox]::Show('Hay un bloque AutoCAD activo; no se puede eliminar un proyecto ahora.', 'GM - Eliminar proyecto', 'OK', 'Warning') | Out-Null
    return
  }
  if (-not $script:projectList -or $script:projectList.SelectedItems.Count -eq 0) {
    [System.Windows.Forms.MessageBox]::Show('Selecciona un proyecto de la lista.', 'GM - Eliminar proyecto', 'OK', 'Warning') | Out-Null
    return
  }
  $name = $script:projectList.SelectedItems[0].Text.Trim()
  if ($name -eq '') { return }
  $project = Read-GmProject $name
  if ($null -eq $project) {
    [System.Windows.Forms.MessageBox]::Show("No se pudo leer el proyecto '$name'.", 'GM - Eliminar proyecto', 'OK', 'Error') | Out-Null
    return
  }
  $output = [string]$project.OutputFolder
  $outText = if ($output) { $output } else { '(no definida)' }
  $counts = ''
  if ($output -and (Test-Path -LiteralPath $output -PathType Container)) {
    $nDwg = @(Get-ChildItem -LiteralPath $output -File -Filter '*.dwg' -ErrorAction SilentlyContinue).Count
    $nPdf = @(Get-ChildItem -LiteralPath $output -File -Filter '* plano preliminar.pdf' -ErrorAction SilentlyContinue).Count
    $counts = "`nContenido de la salida: $nDwg DWG y $nPdf PDF.`n"
  }
  $backupDir = Join-Path (Get-GmProjectsDir) $script:ProjectsBackupName
  $message = "Se ELIMINARA el proyecto '$name':`n`n" +
    "- Registro: " + (Get-GmProjectDir $name) + "`n" +
    "- Carpeta de salida: $outText`n" +
    $counts +
    "`nSe guardara un respaldo completo (registro + salida) en:`n$backupDir`n`n" +
    "Los CSV y la plantilla NO se tocan.`n`nEstas seguro?"
  $answer = [System.Windows.Forms.MessageBox]::Show($message, 'GM - Eliminar proyecto', 'YesNo', 'Warning')
  if ($answer -ne [System.Windows.Forms.DialogResult]::Yes) {
    Add-GmLog "Eliminacion del proyecto '$name' cancelada por el usuario."
    return
  }
  try {
    $result = Remove-GmProjectFiles $name
    $deletedActive = ($null -ne $script:ActiveProject) -and
      [string]::Equals([string]$script:ActiveProject.Nombre, $name, [System.StringComparison]::OrdinalIgnoreCase)
    if ($deletedActive) {
      $script:ActiveProject = $null
      $script:libraryRows = @()
      Update-GmLibraryView
      $script:lblProjectName.Text = 'Proyecto: (sin proyecto)'
    }
    Refresh-GmProjectList
    [System.Windows.Forms.MessageBox]::Show("Proyecto '$name' eliminado.`nRespaldo: $($result.Backup)", 'GM - Eliminar proyecto', 'OK', 'Information') | Out-Null
  } catch {
    Add-GmLog ('No se pudo eliminar el proyecto: ' + $_.Exception.Message)
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'GM - Eliminar proyecto', 'OK', 'Error') | Out-Null
  }
}

# PRY1: panel de proyectos. Lista con nombre/tipo/rutas + abrir (doble clic),
# crear, eliminar (con confirmacion y respaldo en _borrados) y refrescar.
$lblProjTitle = New-Object System.Windows.Forms.Label
$lblProjTitle.Text = 'Proyectos GM'
$lblProjTitle.Location = [System.Drawing.Point]::new(16, 14)
$lblProjTitle.Size = [System.Drawing.Size]::new(400, 32)
$lblProjTitle.Font = [System.Drawing.Font]::new('Segoe UI', 13, [System.Drawing.FontStyle]::Bold)
$script:panelProjects.Controls.Add($lblProjTitle)
$lblProjHint = New-Object System.Windows.Forms.Label
$lblProjHint.Text = 'Doble clic para abrir. Cada proyecto guarda su CSV, plantilla, carpeta de salida, opciones y tipo de plano.'
$lblProjHint.Location = [System.Drawing.Point]::new(16, 50)
$lblProjHint.Size = [System.Drawing.Size]::new(854, 22)
$lblProjHint.Anchor = 'Top,Left,Right'
$script:panelProjects.Controls.Add($lblProjHint)
$script:projectList = New-Object System.Windows.Forms.ListView
$script:projectList.Location = [System.Drawing.Point]::new(16, 78)
$script:projectList.Size = [System.Drawing.Size]::new(854, 730)
$script:projectList.Anchor = 'Top,Bottom,Left,Right'
$script:projectList.View = 'Details'
$script:projectList.FullRowSelect = $true
$script:projectList.GridLines = $true
$script:projectList.HideSelection = $false
[void]$script:projectList.Columns.Add('PROYECTO', 150)
[void]$script:projectList.Columns.Add('TIPO', 100)
[void]$script:projectList.Columns.Add('CSV', 270)
[void]$script:projectList.Columns.Add('SALIDA', 230)
[void]$script:projectList.Columns.Add('ACTUALIZADO', 130)
$script:panelProjects.Controls.Add($script:projectList)
$script:btnProjectOpen = New-Object System.Windows.Forms.Button
$script:btnProjectOpen.Text = 'Abrir proyecto'
$script:btnProjectOpen.Location = [System.Drawing.Point]::new(16, 818)
$script:btnProjectOpen.Size = [System.Drawing.Size]::new(130, 32)
$script:btnProjectOpen.Anchor = 'Bottom,Left'
$script:panelProjects.Controls.Add($script:btnProjectOpen)
$script:btnProjectNew = New-Object System.Windows.Forms.Button
$script:btnProjectNew.Text = 'Crear proyecto...'
$script:btnProjectNew.Location = [System.Drawing.Point]::new(154, 818)
$script:btnProjectNew.Size = [System.Drawing.Size]::new(130, 32)
$script:btnProjectNew.Anchor = 'Bottom,Left'
$script:panelProjects.Controls.Add($script:btnProjectNew)
$script:btnProjectRefresh = New-Object System.Windows.Forms.Button
$script:btnProjectRefresh.Text = 'Refrescar lista'
$script:btnProjectRefresh.Location = [System.Drawing.Point]::new(292, 818)
$script:btnProjectRefresh.Size = [System.Drawing.Size]::new(110, 32)
$script:btnProjectRefresh.Anchor = 'Bottom,Left'
$script:panelProjects.Controls.Add($script:btnProjectRefresh)
$script:btnProjectDelete = New-Object System.Windows.Forms.Button
$script:btnProjectDelete.Text = 'Eliminar proyecto'
$script:btnProjectDelete.Location = [System.Drawing.Point]::new(412, 818)
$script:btnProjectDelete.Size = [System.Drawing.Size]::new(130, 32)
$script:btnProjectDelete.Anchor = 'Bottom,Left'
$script:panelProjects.Controls.Add($script:btnProjectDelete)
$projectOpenError = {
  try {
    if ($script:projectList.SelectedItems.Count -gt 0) {
      Open-GmProject $script:projectList.SelectedItems[0].Text
    } else {
      [System.Windows.Forms.MessageBox]::Show('Selecciona un proyecto de la lista.', 'GM - Proyecto', 'OK', 'Warning') | Out-Null
    }
  } catch {
    Add-GmLog ('No se pudo abrir el proyecto: ' + $_.Exception.Message)
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'GM - Proyecto', 'OK', 'Error') | Out-Null
  }
}
$script:btnProjectOpen.Add_Click($projectOpenError)
$script:projectList.Add_DoubleClick($projectOpenError)
$script:btnProjectNew.Add_Click({
  try { New-GmProjectInteractive } catch {
    Add-GmLog ('No se pudo crear el proyecto: ' + $_.Exception.Message)
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'GM - Crear proyecto', 'OK', 'Error') | Out-Null
  }
})
$script:btnProjectRefresh.Add_Click({ Refresh-GmProjectList })
$script:btnProjectDelete.Add_Click({
  try { Remove-GmProjectInteractive } catch {
    Add-GmLog ('No se pudo eliminar el proyecto: ' + $_.Exception.Message)
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'GM - Eliminar proyecto', 'OK', 'Error') | Out-Null
  }
})

$script:timer = New-Object System.Windows.Forms.Timer
$script:timer.Interval = 5000

$script:checkButton.Add_Click({
  try {
    $config = Get-GmConfigFromUi
    $pre = Test-GmConfig $config
    $script:statusLabel.Text = "Preflight OK: $($pre.Csv.Rows.Count) codigos | AutoCAD $($pre.Stamps['AUTOMATIZACION.lsp']) | plantilla SHA256 verificada"
    Add-GmLog "Preflight OK; codigos=$($pre.Csv.Rows.Count); CSV delimitador=$($pre.Csv.Delimiter); no se abrio AutoCAD."
    [System.Windows.Forms.MessageBox]::Show("Configuracion valida.`nCodigos: $($pre.Csv.Rows.Count)`nSellos LISP coherentes.`nLa plantilla maestra coincide con el hash comprobado.", 'GM - Preflight', 'OK', 'Information') | Out-Null
  } catch {
    $script:statusLabel.Text = 'Preflight fallo: ' + $_.Exception.Message
    Add-GmLog ('Preflight fallo: ' + $_.Exception.Message)
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'GM - Preflight', 'OK', 'Error') | Out-Null
  }
})

$script:startButton.Add_Click({
  try { Start-GmFreshRun } catch {
    $script:runActive = $false
    Add-GmLog ('No se inicio: ' + $_.Exception.Message)
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'GM - No se inicio', 'OK', 'Error') | Out-Null
    Update-GmUiState
  }
})

$script:resumeButton.Add_Click({
  try { Start-GmResumeRun } catch {
    $script:runActive = $false
    Add-GmLog ('No se pudo reanudar: ' + $_.Exception.Message)
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'GM - Reanudacion', 'OK', 'Error') | Out-Null
    Update-GmUiState
  }
})

$script:stopButton.Add_Click({
  if ($null -eq $script:activeProc) { return }
  $answer = [System.Windows.Forms.MessageBox]::Show(
    'Se cerrara solo el proceso AutoCAD abierto por este lanzador. El codigo actual puede quedar incompleto y se reintentara al reanudar. Continuar?',
    'GM - Detener bloque', 'YesNo', 'Warning')
  if ($answer -ne [System.Windows.Forms.DialogResult]::Yes) { return }
  $script:StopRequested = $true
  Add-GmLog 'Cierre forzado solicitado por el usuario; el archivo temporal se descartara.'
  try { $script:activeProc.Kill() } catch { Add-GmLog ('No se pudo detener AutoCAD: ' + $_.Exception.Message) }
})

$script:openButton.Add_Click({
  if (Test-Path -LiteralPath $script:txtOutput.Text -PathType Container) { Start-Process explorer.exe -ArgumentList ('"' + $script:txtOutput.Text + '"') }
})

$dumpButton.Add_Click({
  try { Enable-GmLocalDumps } catch {
    Add-GmLog ('No se activo LocalDumps: ' + $_.Exception.Message)
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'GM - LocalDumps', 'OK', 'Error') | Out-Null
  }
})

$script:timer.Add_Tick({
  if ($null -eq $script:activeProc) { $script:timer.Stop(); return }
  try {
    $script:activeProc.Refresh()
    if ($script:activeProc.HasExited) {
      Complete-GmBlock $script:activeProc
      return
    }
    $process = $script:activeProc
    Write-GmMonitorRow 'MUESTRA' $process $script:State.OutputFolder
    $watched = @(
      (Join-Path $script:State.OutputFolder $script:ReportName),
      (Get-GmStatePath $script:State.OutputFolder $script:ForensicName),
      (Get-GmStatePath $script:State.OutputFolder $script:ProgressName)
    )
    foreach ($path in $watched) {
      if (Test-Path -LiteralPath $path -PathType Leaf) {
        $last = (Get-Item -LiteralPath $path).LastWriteTimeUtc
        if ($last -gt $script:lastActivityUtc) { $script:lastActivityUtc = $last; $script:stallWarned = $false }
      }
    }
    $current = @($script:currentBatch) -join ', '
    $elapsed = [DateTime]::UtcNow - $script:lastActivityUtc
    $mem = [math]::Round($process.PrivateMemorySize64 / 1MB, 0)
    $script:statusLabel.Text = "Bloque $($script:State.BatchNumber) | PID $($process.Id) | PRIV $mem MB | cola: $(if ($current) { $current } else { 'iniciando' }) | sin escritura: $([int]$elapsed.TotalMinutes) min"
    if ($elapsed.TotalMinutes -ge [int]$script:State.StallMinutes -and -not $script:stallWarned) {
      $script:stallWarned = $true
      Add-GmLog "AVISO: sin escritura nueva en reportes/forense por $([int]$elapsed.TotalMinutes) minutos. AutoCAD sigue vivo; revise antes de detenerlo."
    }
    # F7: con "Matar si se cuelga", al duplicar el umbral se cierra solo el
    # AutoCAD del lanzador; el tick siguiente detecta HasExited y reconcilia.
    if ($script:State.StallKill -and -not $script:stallKilled -and
        $elapsed.TotalMinutes -ge (2 * [int]$script:State.StallMinutes)) {
      $script:stallKilled = $true
      Add-GmLog "Cuelgue: sin escritura por $([int]$elapsed.TotalMinutes) min (2x el umbral). Se cierra el AutoCAD del lanzador; el codigo en curso se reintenta al reanudar."
      try { $process.Kill() } catch { Add-GmLog ('No se pudo cerrar el proceso: ' + $_.Exception.Message) }
      $script:lastActivityUtc = [DateTime]::UtcNow
    }
    Update-GmUiState
  } catch {
    Add-GmLog ('Error al leer metricas del proceso: ' + $_.Exception.Message)
  }
})

$form.Add_FormClosing({
  if ($null -ne $script:activeProc) {
    [System.Windows.Forms.MessageBox]::Show('Hay un bloque AutoCAD activo. Use Detener bloque antes de cerrar el lanzador.', 'GM - Bloque activo', 'OK', 'Warning') | Out-Null
    $_.Cancel = $true
    return
  }
  # PRY1: al cerrar se persiste la configuracion del proyecto abierto.
  try { Save-GmActiveProject } catch { }
})

# PRY1: inicializar el store de proyectos (migracion unica) y arrancar en el
# panel de proyectos; el pilotaje y el smoke test abren directo el detalle.
$script:UiSmokeError = $null
if (-not $UiSmokeTest -and -not $PilotTest) { Initialize-GmProjects | Out-Null }
Refresh-GmProjectList
$script:statusLabel.Text = 'Selecciona un proyecto (doble clic) o crea uno nuevo en el panel de proyectos.'
Update-GmUiState
$pilotCsv = Join-Path $script:ProjectRoot 'PRUEBA_5_PREDIOS.csv'
if ($PilotTest) {
  if (-not (Test-Path -LiteralPath $env:TEMP -PathType Container)) { throw 'No existe TEMP para el piloto AutoCAD.' }
  # F7: poda de pilotos viejos (> 7 dias) para que %TEMP% no crezca sin limite.
  Get-ChildItem -LiteralPath $env:TEMP -Directory -Filter 'GM_LANZADOR_PILOTO_*' -ErrorAction SilentlyContinue |
    Where-Object { $_.LastWriteTime -lt (Get-Date).AddDays(-7) } |
    ForEach-Object { Remove-Item -LiteralPath $_.FullName -Recurse -Force -ErrorAction SilentlyContinue }
  $script:PilotOutput = Join-Path $env:TEMP ('GM_LANZADOR_PILOTO_' + [guid]::NewGuid().ToString('N'))
  New-Item -ItemType Directory -Path $script:PilotOutput | Out-Null
  $script:txtCsv.Text = $pilotCsv
  $script:txtOutput.Text = $script:PilotOutput
  $script:numBlock.Value = 5
  $script:numChunk.Value = 0
  $script:numStall.Value = 15
  $script:radioPdf.Checked = $true
  $script:radioDwg.Checked = $false
$script:checkPredio.Checked = $true
$script:checkGeo.Checked = $true
$script:checkDiag.Checked = $true
$script:checkRetry.Checked = $true
Show-GmDetailPanel
}
if ($UiSmokeTest) {
  $form.Add_Shown({
    try {
      Show-GmDetailPanel
      # PRY1-UI (2026-10-13): los 4 mini-selectores de ruta deben quedar dentro
      # del panel aun con la ventana en su tamano minimo (regresion del corte
      # a la derecha que sufrian los botones "Examinar...").
      $savedSize = $form.Size
      try {
        $form.Size = $form.MinimumSize
        $script:panelDetail.PerformLayout()
        if (@($script:pathRows).Count -ne 4) { throw 'se esperaban 4 filas de ruta en el detalle.' }
        $panelWidth = $script:panelDetail.ClientSize.Width
        foreach ($row in @($script:pathRows)) {
          if ($row.Right -gt $panelWidth) { throw "una fila de ruta excede el ancho visible del detalle (row=$($row.Left)+$($row.Width) panel=$panelWidth)." }
          if ($row.Bottom -gt $group.Top) { throw 'una fila de ruta se solapa con el grupo de opciones.' }
        }
        foreach ($browse in @($script:pathBrowseButtons)) {
          if ($browse.Text -ne 'Examinar...') { throw 'el selector de ruta debe rotularse Examinar....' }
          if ($browse.Left -lt 0 -or $browse.Right -gt $browse.Parent.ClientSize.Width) {
            throw "un boton Examinar... quedo fuera de su fila (brow=$($browse.Left)+$($browse.Width) fila=$($browse.Parent.ClientSize.Width))."
          }
        }
      } finally {
        $form.Size = $savedSize
        $script:panelDetail.PerformLayout()
      }
      # PRY1-FIX: el atajo "< Proyectos" se prueba por CLIC real; antes el
      # smoke llamaba la funcion directo y el Add_Click faltante paso oculto.
      $script:btnBackToProjects.PerformClick()
      if (-not $script:panelProjects.Visible) { throw 'el clic en "< Proyectos" no volvio al panel de proyectos.' }
      # PRY1-FIX: guardas de layout del detalle (el grupo crecio en F2 y los
      # botones quedaban debajo de su borde).
      if ($group.Bottom -gt $script:checkButton.Top) { throw 'los botones de accion se solapan con el grupo de opciones.' }
      if ($script:progressBar.Bottom -gt $script:statusLabel.Top) { throw 'la barra de progreso se solapa con la etiqueta de estado.' }
      if ($script:statusLabel.Bottom -gt $script:logList.Top) { throw 'la etiqueta de estado se solapa con el log.' }
      if ($script:logList.Bottom -gt $script:bottomTabs.Top) { throw 'el log se solapa con las pestanas.' }
      Refresh-GmLibrary
      Update-GmLibraryView
    } catch {
      $script:UiSmokeError = $_.Exception.Message
    }
    $form.Close()
  })
}
if ($PilotTest) {
  $form.Add_Shown({
    try { Start-GmFreshRun } catch {
      $script:PilotExitCode = 1
      Add-GmLog ('Piloto no iniciado: ' + $_.Exception.Message)
      $form.BeginInvoke([Action]{ $form.Close() }) | Out-Null
    }
  })
}
Clear-GmOrphanTemp
[void]$form.ShowDialog()
if ($UiSmokeTest) {
  if ($script:UiSmokeError) { throw ('UI SMOKE FALLO: ' + $script:UiSmokeError) }
  Write-Output 'UI SELFTEST OK'
}
if ($PilotTest) {
  $pilotLog = Get-GmStatePath $script:PilotOutput $script:LogName
  if ($script:PilotExitCode -eq 0) {
    Write-Output ("PILOT AUTOCAD OK; OUTPUT={0}; LOG={1}" -f $script:PilotOutput, $pilotLog)
  } else {
    Write-Output ("PILOT AUTOCAD PAUSADO :v ; OUTPUT={0}; LOG={1}" -f $script:PilotOutput, $pilotLog)
  }
  exit $script:PilotExitCode
}



