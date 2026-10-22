# Contrato de plantilla GM v1 — el modelo se adapta al script

Objetivo: que cualquier DWG nuevo (Huancayo u otra zona) funcione con el mismo
lote sin reprogramar. El script ya no busca handles fijos; busca ROLES.

## 1. Roles (obligatorios)

Marcar una vez con `GM_MARCAR_PLANTILLA` (VP+LEYENDA automaticos) o con
`GM_MARCAR_ROL` (seleccion explicita de cualquier rol). Se guardan con
`ldata GM_ROL`:

| Rol | Que es | Tipos admitidos | Como se detecta si no esta marcado |
|---|---|---|---|
| `VP_PRINCIPAL` | Viewport principal del plano | VIEWPORT | Legacy `D5CF2`, si no el viewport real mas grande del layout |
| `LEYENDA` | Leyenda base del cajetin | INSERT o ACAD_TABLE | Legacy `166D8E`, si no INSERT con `LEYENDA` en el nombre |
| `MAPA_GEO` | Marco/franja del mapa geografico | LWPOLYLINE, POLYLINE, INSERT, IMAGE | Legacy `167F4D`, si no LWPOLYLINE ancha y baja (la mas alta) |
| `PROYECTO` | Franja de proyecto/logos | LWPOLYLINE, POLYLINE, INSERT, IMAGE | Legacy `15BBFB`, si no rectangulo ancho 8-25 alto abajo (Y<=60) |

Sin rol y sin fallback se registra `FALTA_ROL:<rol>` y el codigo no exporta.
Un rol marcado que no valide (tipo o tamano) es fail-closed: no cae al
legacy/barrido. `MAPA_GEO`/`PROYECTO` se resuelven por legacy/geometria y el
resolutor tambien respeta un `GM_ROL` puesto a mano.

### 1.1 Leyenda tipo tabla (`ACAD_TABLE`)

Desde CHG1 (07-10-2026) la leyenda puede ser una tabla de AutoCAD (caso
Chagual: `CUADRO LEYENDA`, 60.94 x 46.75). Requisitos: marcar el rol con
`GM_MARCAR_ROL` y que la caja medida no exceda 65 x 48. El ciclo completo
(copia, ubicacion, ocultado temporal y restauracion) usa visibilidad COM para
la tabla; la original se restaura siempre (fail-closed si no puede).
Desde CHG3-FIX (10-10-2026) la tabla viaja como **conjunto**: su fondo
(WIPEOUT en la capa de la tabla) y las muestras de simbologia independientes
(capas `LEYENDA*`) dentro de la caja medida +margen 1 se copian, mueven y
ordenan con ella (wipeout al fondo, simbolos al frente), las copias se fuerzan
visibles y los originales se ocultan y restauran con re-verificacion del DXF
60; traza `LEYENDA_ESTADO ... conjunto=N`. La leyenda INSERT de Huancasancos
no cambia.

### 1.2 Mapa combinado (`MAPA_GEO` con la franja incluida)

Si el rol `MAPA_GEO` se marca en un bloque o marco cuyo alto supera 20
unidades (p.ej. `ESQUEMA DE LOCALIZACION`, que contiene el mapa y su franja de
titulo), el sistema deriva el contenido del propio marco medido (margen
interior 2) y las tablas se apilan 4 unidades debajo de su borde inferior,
igual que con la franja separada de Huancasancos. Limitacion honesta: si las
etiquetas/ticks/ruta del mapa viven dentro de un bloque o viewport anidado,
el sistema no puede leerlos; el predio no se senaliza con circulos verdes y el
codigo queda con `PREDIO_LOCALIZACION;AVISO` en PENDIENTES (no es fallo).

### 1.3 Capas del predio congeladas o apagadas (CHG3, 10-10-2026)

Las capas de la matriz, el area afectada (AF) y el perimetro (PF) pueden
venir congeladas o apagadas en la plantilla: el lote las **reactiva
automaticamente** (ON + Thaw) al formatear el Model, traza
`CAPAS_PREDIO;OK;reactivadas=N` y el DWG/PDF muestran el relleno AF. Las
capas de contexto (vias, colindantes, etc.) conservan el estado que dejo el
ingeniero. No hace falta descongelar nada a mano en la plantilla, pero se
recomienda no dejarlas congeladas para que el Model se revise comodo.

### 1.4 Localizacion del predio en el mapa regional (CHG3-FIX2, 10-10-2026)

Para que el circulo del predio y el zoom del mapa regional se generen, el
mapa debe exponer una **fuente de localizacion legible desde el propio DWG**:
la via principal son los ticks UTM del marco (metodo `TICKS`); alternativamente
un VP anidado con el mapa dibujado en el Modelo (metodo `ESQUEMA_VP`: ruta
roja/magenta + fraccion del eje del proyecto) o textos de U.C./distrito en el
layout. Los rotulos de la capa `0. DEP. PROV. DIST.` **no** cuentan como
fuente (son del sistema). Si el mapa es cartografia importada (XREF, raster o
vista guardada no legible), el codigo no inventa posiciones: queda
`PREDIO_LOCALIZACION;AVISO` en PENDIENTES, sin circulos ni zoom, y el plano
exporta igual. Para habilitar circulos en ese caso: bindear/incorporar la
cartografia al DWG o dibujar la ruta del proyecto como entidad interna.

### 1.5 Limpieza del rotulo GEO viejo (CHG4, 12-10-2026)

Una plantilla puede traer **guardado** el rotulo `DEP/PROV/DIST` de una corrida
anterior (textos en la capa `0. DEP. PROV. DIST.`); ese dato es congelado y
falso para el resto del lote. Al iniciar cada lote (y cada continuacion) el
sistema borra esos `TEXT/MTEXT/ATTRIB` UNA vez, antes de medir obstaculos
(traza `GEO_BASE_LIMPIA;OK;N=n`) y en AMBOS modos del toggle CHG2: encendido
-> el plano lleva solo el rotulo fresco del codigo; apagado -> ninguno. Solo
se toca esa capa (exclusiva del sistema); la limpieza corre sobre la copia de
trabajo, nunca sobre el master. No es obligatorio limpiar la plantilla a
mano, pero se recomienda para revisar el dibujo comodo.

## 2. Layout y viewport

- Cualquier nombre de layout (ya no solo `PM-YYY`). El lote usa el layout activo.
- Un viewport real por layout (se excluye el papel `69=1`).
- Unidades: Model en metros, Paper en mm. `CustomScale` libre.
- Escala AUTO por viewport: `need=(maxDim*1000)/(vpH*0.55)`, se elige el primer
  denominador `>=need` en `*UBI_SCALE_LIST*` (20...25000), acotado por abajo a
  `*GM_AUTO_SCALE_MIN*` (`1/1000` en lote; BW 2026-09-22). El selector manual
  conserva 1/20...1/750. Ya no fija 1/1250. Predio chico -> 1/1000;
  grande -> 1/2000+. Traza `AUTOESCALA`.

## 3. Zonas relativas (no coordenadas fijas)

`GM:LayoutCalibrate` calcula desde `x,y,w,h` reales del viewport + `cx` de
leyenda + `tabletop`. Los offsets historicos de Huancasancos se escalan por
`kw=w/278.95` y `kh=h/257.70` para no romper la plantilla actual:

- `ESQUEMA_AF`, `ESQUEMA_LOC`, `TABLAS`, `LEYENDA`, `MEMBRETE 117x65`,
  `ESCALA_VP`, `PIE_MEMBRETE` (a la derecha de `PROYECTO`).
- Si `w,h < 50` o no hay hueco para leyenda+pie -> `FAIL` honesto.

## 4. Datos (sin cambios, formato Huanca)

- CSV igual 19 columnas 0-18, codigo en 14, `U.C.` en 13.
- Hipervinculo matriz `=codigo`; AF/PF `<codigo>-AF-<suf>` / `<codigo>-PF-<suf>`.
- Capas de `*UBI_LAYER_LIST*`, estilos `ROMANS/ST-COORP-GM` (se autocrean),
  plot `DWG To PDF.pc3` heredado de la plantilla.

## 5. Flujo OK

```text
DWG nuevo -> GM_VALIDAR_PLANTILLA (solo lee)
  -> OK -> GM_PLANOS_LOTE normal
  -> FALTA_ROL -> GM_MARCAR_PLANTILLA (una vez) -> re-validar -> lote
Legacy Huancasancos -> se detecta D5CF2/166D8E -> traza LEGACY-MIGRADO -> mismo lote
```

Salida del lote (rev. BB): un DWG principal editable por codigo (`SaveCopyAs`
antes de la limpieza) + PDF auxiliar opcional. El contrato de roles no cambia.

## 6. Comandos

- `GM_VALIDAR_PLANTILLA`: diagnostico `DWG/CTAB/roles/dimensiones/bloques`;
  desde CHG1 agrega el detalle por rol (tipo/handle/caja) y las cajas
  resueltas de mapa/proyecto.
- `GM_MARCAR_PLANTILLA`: asigna `GM_ROL` a `VP_PRINCIPAL` (legacy `D5CF2` o
  el viewport mas grande) y `LEYENDA` (nombre con `LEYENDA`); `MAPA_GEO` y
  `PROYECTO` quedan al respaldo legacy/geometrico y `GM_VALIDAR` los exige
  igual.
- `GM_MARCAR_ROL`: marcado explicito por seleccion; primero se elige el rol
  por numero (`1`=VP_PRINCIPAL, `2`=LEYENDA, `3`=MAPA_GEO, `4`=PROYECTO) y
  despues se hace clic en la entidad. Valida tipo y pertenencia al layout; es
  la via para la leyenda-tabla y el mapa-bloque de plantillas nuevas. (CHG1:
  la seleccion numerica reemplaza al getkword con guiones bajos, rechazado por
  AutoCAD.)
- `GM_DIAGNOSTICO`: ahora vuelca tambien `ROL_*`.

## 7. Contrato de errores (PA-3, 28-09-2026)

- `GM:LayoutFail` y los `exit` internos (`EXEC_ESQUEMAS_PU_FULL`,
  `UBI_CleanByHyperlinks`, `UBI_RunVP*`) abortan con una excepcion no local.
  TODO llamador debe envolver la invocacion en `vl-catch-all-apply` y
  verificar `vl-catch-all-error-p`/retorno. El lote (E9) ya lo hace por cada
  etapa; el modo manual aborta el comando, que es su comportamiento esperado.
- `GM:AutoExportDwg` valida `(getcname "_-WBLOCK")` ANTES de desactivar
  FILEDIA/CMDECHO/CMDDIA; si el CAD no expone el comando, el codigo registra
  el mensaje y no exporta (fail-closed, sin `.tmp` huerfano).
- `GM_PLANOS_LOTE` es fail-closed por limpieza: cualquier resto de entidades o
  bloque temporal detiene el lote y se reanuda con `GM_PLANOS_LOTE_PROGRESO.csv`
  (politica aprobada 28-09-2026 para la tanda de 234).
- Visibilidad (T2.2-FIX1, 28-09-2026): el lote restaura la visibilidad de las
  entidades indexadas contra su mascara base en el cierre y en la cancelacion,
  siempre ANTES de liberar `*UBI_ENT_INDEX*`; la traza
  `VISIBILIDAD_FINAL;OK|ERROR;restauradas=N` lo evidencia y el restaurador
  legado (`UBI_RestoreHiddenByGM`) es el fallback sin indice.
