# Requisitos de plantilla GM - apartados obligatorios del modelo base

Documento de recepcion para un DWG plantilla nuevo (Huancayo u otra zona).
Complementa el contrato tecnico `SPEC_PLANTILLA_GM.md` (raiz): aqui cada
apartado obligatorio se explica con sus medidas, contenidos y criterios de
aceptacion, para un usuario que recien conoce el sistema.

## Resumen de requerimientos

**1. Software.** AutoCAD o Civil 3D (con `DWG To PDF.pc3` y fuentes
`romans.shx`, `Arial`, `Trebuchet MS`) + PowerShell 5.1 para el lanzador.

**2. Plantilla DWG con 4 roles** (`VP_PRINCIPAL`/`LEYENDA` se marcan con
`GM_MARCAR_PLANTILLA`; cualquier rol a mano con `GM_MARCAR_ROL`;
`MAPA_GEO`/`PROYECTO` por respaldo geometrico o `GM_ROL`;
verificar con `GM_VALIDAR_PLANTILLA`): `VP_PRINCIPAL` (un
viewport real por layout, >= 50x50, Model en metros / Paper en milimetros),
`LEYENDA`
(bloque con `LEYENDA` en el nombre, o tabla `ACAD_TABLE` marcada con
`GM_MARCAR_ROL`; caja <= 65x48 + hueco para su copia), `MAPA_GEO`
(marco con la cartografia de la zona + 4 unidades libres debajo; puede ser un
bloque combinado mapa+titulo, ver CHG1), `PROYECTO` (franja abajo de 8-25 de
alto + hueco libre a su derecha).

**3. CSV de 19 columnas intacto.** Codigo valido en col 15 (`HS-YA-001`),
U.C. en col 14, resto para membrete y pie; no re-guardar desde Excel.

**4. Hipervinculos en el Model.** Matriz `=codigo`; afectaciones
`<codigo>-AF-<suf>` / `<codigo>-PF-<suf>`.

**5. Regla de operacion.** Sin rol no exporta; sin hueco o escala compone un
error honesto a `PENDIENTES.csv`; el lote nunca entrega un plano incorrecto.

Regla general: el script NO dibuja el cajetin desde cero. Toma tu DWG, ubica
el predio por su hipervinculo, calcula escala y zonas, y rellena membrete,
tablas, etiquetas, esquemas y pie. Por eso la plantilla debe traer ciertas
piezas en su lugar; esas piezas son los CUATRO ROLES:

| Rol | Pieza | Obligatorio |
|---|---|---|
| `VP_PRINCIPAL` | Viewport principal del layout | Si (exporta o falla) |
| `LEYENDA` | Bloque de leyenda base | Si (exporta o falla) |
| `MAPA_GEO` | Marco/franja del mapa regional | Si (exporta o falla) |
| `PROYECTO` | Franja de proyecto/logos | Si (exporta o falla) |

Sin rol y sin deteccion de respaldo el codigo registra `FALTA_ROL:<rol>` y
NO se exporta (fail-closed). `GM_MARCAR_PLANTILLA` guarda `GM_ROL` de
`VP_PRINCIPAL` y `LEYENDA` en el DWG; `MAPA_GEO`/`PROYECTO` se detectan por
legacy/geometria (o se marcan a mano) y `GM_VALIDAR_PLANTILLA` los exige
igual.

---

## 1. `VP_PRINCIPAL` - viewport principal (la ventana del plano)

**Que es.** La ventana rectangular del layout por la que se ve el terreno del
Model. Es la pieza mas importante: escala, grilla y centrado se calculan desde
su tamano real.

**Requisitos:**

- **Uno solo por layout.** Debe haber exactamente un viewport real; se excluye
  el viewport del papel (aquel con `69=1`, que es el fondo de la hoja).
- **Cualquier nombre de layout vale** (ya no solo `PM-YYY`): el lote usa el
  layout activo.
- **Tamano minimo: ancho y alto >= 50** unidades de papel. Por debajo de eso
  el lote no adivina: registra `FAIL` honesto y el codigo no se exporta.
  (Referencia Huanca: 278.95 x 257.71; otros tamanos se aceptan porque todo
  se escala proporcionalmente, pero el piso es 50x50.)
- **Unidades obligatorias: Model en metros, Paper en milimetros.** La formula
  de escala depende de esto: `need = (maxDim * 1000) / (vpH * 0.55)`.
- **`CustomScale` libre**: el sistema lo lee y lo reescribe por codigo.

**Escalas que maneja.** Denominadores oficiales (20 valores): 20, 25, 50, 75,
100, 125, 200, 250, 500, 750, 1000, 1250, 2000, 2500, 5000, 7500, 10000,
12500, 20000, 25000. El lote elige el primer denominador >= `need`, nunca por
debajo de **1/1000** (minimo automatico). Predio chico -> 1/1000; grande ->
1/2000+. La escala declarada del CSV (`INDICADA`) es solo referencia: manda
la escala real y el pie rotula la real si difieren (traza `ESCALA_Y_PIE`).

**Como cumplirlo.** Deja tu viewport con tamano >= 50x50 y ejecuta
`GM_MARCAR_PLANTILLA` (marca el viewport mas grande si no hay handle legacy).
Verifica con `GM_VALIDAR_PLANTILLA`.

**Si falta.** `FALTA_ROL:VP_PRINCIPAL`; ningun codigo se exporta.

---

## 2. `LEYENDA` - bloque de leyenda base

**Que es.** El bloque del cajetin que explica los simbolos (predio, eje, via,
rios, etc.). El sistema no lo redibuja: trabaja con una copia.

**Requisitos:**

- Debe ser un `INSERT` (bloque) **cuyo nombre contenga `LEYENDA`**
  (ejemplo Huanca: `LEYENDA DE UBICACION`). Asi lo encuentra el marcado
  automatico; sin la palabra, marcar a mano.
- Alternativa desde CHG1 (07-10-2026): una tabla `ACAD_TABLE` con el rol
  `LEYENDA` marcado con `GM_MARCAR_ROL` (caso Chagual: `CUADRO LEYENDA`,
  60.94 x 46.75). La caja medida no debe exceder 65 x 48. La restauracion de
  la original es fail-closed igual que con el bloque.
- Comportamiento: durante el lote el original se oculta y se coloca una copia
  visible en su zona; al terminar (OK, error o cancelacion) el original se
  restaura intacto desde su snapshot DXF. Tu leyenda no se pierde.
- **Hueco requerido** (referencia Huanca, recalculada por `kw/kh`): zona de
  aprox. 65 x 48 (de 298.44,101.29 a 363.44,149.29). Lo obligatorio no son
  esas coordenadas sino que exista hueco libre donde reubicar la copia sin
  tapar el viewport ni el pie.

**Si falta.** `FALTA_ROL:LEYENDA`; ningun codigo se exporta.

---

## 3. `MAPA_GEO` - marco del mapa geografico

**Que es.** La franja o marco donde vive el mapita regional (el que recibe
los circulos verdes del predio afectado y el rotulo DEP/PROV/DIST).

**Requisitos:**

- Debe existir como marco identificable. Deteccion de respaldo: la
  LWPOLYLINE ancha y baja mas alta del layout (referencia Huanca: franja de
  270.84,207.29 a 384.53,213.01).
- **Contenido que pones tu:** cartografia base de la zona (departamentos,
  provincias, red vial de fondo), nortes y titulo de la franja. El sistema
  ubica el predio sobre ese fondo y dibuja circulos, pero NO crea el mapa.
- Deja **4 unidades libres debajo** de la franja: ahi arrancan las tablas.
- Alternativa desde CHG1 (07-10-2026): un **mapa combinado** (bloque o marco
  con el mapa y su franja de titulo, alto > 20; caso Chagual: `ESQUEMA DE
  LOCALIZACION` de 107.25 x 81.11). Se marca con `GM_MARCAR_ROL`; el sistema
  usa el propio marco como contenido (margen 2) y apila tablas 4 unidades
  debajo del borde inferior. Si los ticks/ruta viven dentro del bloque o de
  un viewport anidado, no hay circulos verdes: queda
  `PREDIO_LOCALIZACION;AVISO` (limitacion honesta, no fallo).
- Desde CHG3-FIX2 (10-10-2026): si el mapa expone contenido legible (ruta del
  proyecto dibujada en el Modelo dentro del bloque/viewport anidado, o ticks
  UTM en el layout) el sistema ubica el predio por la fraccion del eje y
  recrea el zoom del circulo. Los rotulos de la capa `0. DEP. PROV. DIST.`
  **no cuentan como fuente** (son del propio sistema). Desde CHG4
  (12-10-2026) las copias viejas de ese rotulo que la plantilla haya guardado
  se **borran solas** al iniciar el lote (`GEO_BASE_LIMPIA;N=n`), en ambos
  modos del toggle; no hace falta limpiarlas a mano (se recomienda igual para
  revisar el dibujo comodo). Si la
  cartografia es importada (XREF/raster/vista guardada), bindearla al DWG o
  dibujar la ruta interna para habilitar los circulos verdes.

**Si falta.** `FALTA_ROL:MAPA_GEO`; el codigo no exporta.

---

## 4. `PROYECTO` - franja de proyecto y logos

**Que es.** La franja institucional con nombre de proyecto y logos. Ademas
es la referencia de posicion del pie.

**Requisitos:**

- Rectangulo identificable: el detector busca un rectangulo ancho de 8-25 de
  alto ubicado abajo (Y <= 60). Referencia Huanca: de 127.78,16.93 a
  283.10,33.86.
- **Deja libre el espacio a su derecha**: ahi va el pie (`PIE_MEMBRETE`,
  aprox. de 284.10,17.93 a 385.62,33.86, con margen 1). Si la franja ocupa
  todo el ancho inferior, la calibracion falla.
- Contenido (lo pones tu): nombre del proyecto, logos y franja institucional.
  El sistema no los mueve.

**Si falta.** `FALTA_ROL:PROYECTO`; el codigo no exporta.

---

## 5. `ESQUEMA_AF` y `ESQUEMA_LOC` - los dos esquemas

**Que son.** Los recuadros de analisis que el sistema genera por codigo: area
afectada (AF) y localizacion (LOC).

**Requisitos:**

- **Tamanos: AF 70 x 60, LOC 80 x 60** (referencia Huanca: AF de
  175.67,62.26 a 245.67,129.76; LOC de 4.67,182.26 a 84.67,249.76). Se
  escalan con `kw = w/278.95` y `kh = h/257.70` segun tu viewport.
- Cada esquema incluye **franja inferior de titulo de 7.5** unidades.
- Deben existir como **hueco libre**: el ajuste centro+escala del predio los
  trata como obstaculos duros. Si el predio no compone ni a 1/25000, el
  codigo falla honesto (`AUTOESCALA`) en vez de encimarlos.

---

## 6. `TABLAS` - zona de tablas de afectacion

**Que es.** El area donde se apilan las tablas de area y perimetro por
afectacion.

**Capacidades exactas:**

- Cuadros de **50 x 24 con separacion 3**, apilados desde arriba: caben 2.
- Con **3 afectaciones** se compactan a **50 x 15 con separacion 2** (la
  geometria interna escala por `h/24`).
- Con **4 o mas** el codigo falla honesto (`TABLAS`) y no exporta: el
  contrato no dibuja mas de 3.
- Encabezados con los **sufijos reales** (`AREA AFECTADA 01`, o `05` si ese
  sufijo existe); cada registro conserva area y perimetro.
- Referencia Huanca: de 305.94,152.29 a 355.94,203.29.

---

## 7. `MEMBRETE` - cajetin de datos del predio

**Que es.** El bloque de datos del titular que el sistema genera por codigo.

**Requisitos:**

- **Tamano 117 x 65** (referencia Huanca: de 268.62,35.26 a 385.62,100.26).
- **Contenido:** 7 filas del CSV + referencia catastral (U.C.) +
  departamento + provincia + distrito + lado + `V-B-`. Codigo y escala NO van
  aqui: pasan al pie.
- **Altura minima de texto 0.8** (medida con `textbox`): si los datos no
  caben a esa altura, el codigo falla en vez de truncar en silencio.
- El membrete manual de `MENU_PLANOS` se conserva para remates.

---

## 8. `ESCALA_VP` - cuadro de escala grafica

**Que es.** El cuadrito con la escala grafica dentro del viewport.

**Requisitos:** cuadro de **31.26 x 6.31** (referencia: de 233.36,39.26 a
264.62,45.57), fondo blanco, textos `PLANO DE UBICACION` y escala real. Lo
dibuja `GM:GenerateLayoutFooter`; la plantilla solo debe dejar el hueco.

---

## 9. `PIE_MEMBRETE` - pie inferior

**Que es.** La linea de datos al pie, a la derecha de `PROYECTO`.

**Requisitos:**

- **Posicion:** solo el hueco a la derecha de `PROYECTO`, margen 1.
- **Contenido (todo del CSV, nada se inventa):** `UBICACION`; codigo (col 15);
  escala real aplicada; fecha (col 17, sin apostrofo inicial); zona (col 18);
  datum (col 19); `HOJA 01` (siempre 01: unica hoja del PDF). No se inventan
  responsables, firmas ni fechas.

---

## 10. CSV de 19 columnas (una fila por predio, sin encabezado)

Mismo orden siempre; separador `;` o `,`; no re-guardar desde Excel (rompe
tildes y estructura). Ejemplo real `HS-YA-001`:

| # | Contenido | Ejemplo | Uso |
|---|---|---|---|
| 1 | Titular | AYALA BELLIDO, ANTONIO | Membrete + etiqueta propietario |
| 2 | Denominacion | HUICHO CUCHO | Membrete |
| 3 | Condicion juridica | POSESIONARIO MAYOR A 10 ANOS | Membrete |
| 4 | Tramo | - | Membrete / tramo AF |
| 5 | Partida | - | Membrete |
| 6 | Lado | DERECHO | Membrete |
| 7 | Sector | MAMAYLLA CCOCHA | Membrete |
| 8 | Departamento | AYACUCHO | Mapa regional + membrete (si falta, respaldo `AYACUCHO`) |
| 9 | Tipo | RUSTICO | Membrete |
| 10 | Provincia | CANGALLO | Mapa + membrete |
| 11 | Progresiva inicio | 0+000 | Contexto/tablas |
| 12 | Distrito | LOS MOROCHUCOS | Mapa + membrete |
| 13 | Progresiva fin | 0+061 | Contexto/tablas |
| 14 | U.C. | U.C. 24623 | Membrete + busqueda alternativa del poligono + `METRICA` |
| 15 | **Codigo (clave)** | HS-YA-001 | Hipervinculo, membrete, pie, archivos. Formato `^[A-Za-z0-9_-]+$` |
| 16 | Escala declarada | INDICADA | Solo referencia; manda la escala real |
| 17 | Fecha | 'AGOSTO 2026 | Pie (se quita el apostrofo inicial) |
| 18 | Zona UTM | 18 SUR | Pie + `METRICA` |
| 19 | Datum | WGS 84 | Pie + `METRICA` |

Filas con codigo vacio se ignoran; codigos con caracteres fuera del contrato
se descartan y quedan trazados (`CSV_CONTRATO`).

---

## 11. Hipervinculos en el Model (pegamento codigo-dibujo)

- **Poligono matriz:** hipervinculo EXACTAMENTE igual al codigo (`=codigo`,
  ej. `HS-YA-001`). Alternativa: la U.C. (col 14). Sin ninguno ->
  `POLIGONO` honesto (revisar DWG/CSV).
- **Afectaciones:** `<codigo>-AF-<suf>` (hatch de area) y
  `<codigo>-PF-<suf>` (polilinea de contorno); ej. `HS-YA-001-AF-01` +
  `HS-YA-001-PF-01` (area 130.06 m2, perimetro 128.89 m en la evidencia).
  AF y PF con igual sufijo son UNA afectacion (area + contorno), no dos.
- El escaneo no mira dentro de bloques ni referencias externas: los
  hipervinculos deben estar en entidades directas del Model.

---

## 12. Capas, estilos y plot

**9 capas** (el formato pinta por capa; entidades en otras capas salen sin
color):

1. `PREDIO MATRIZ` (poligono matriz)
2. `AREA AFECTADA (AREA)` (relleno AF)
3. `AREA AFECTADA (PERIMETRO)` (contorno AF)
4. `LIMITE DE AFECTACION`
5. `EJE DE VIA`
6. `VIA EXISTENTE`
7. `RIOS O QUEBRADAS`
8. `PREDIOS COLINDANTES`
9. `VIVIENDA`

Las capas 1-4 (matriz/AF/PF del codigo) pueden venir congeladas o apagadas:
el lote las **reactiva solo** (ON+Thaw) al formatear el Model (traza
`CAPAS_PREDIO;OK;reactivadas=N`, CHG3 10-10-2026). No es necesario
descongelarlas a mano; se recomienda dejarlas normales para revisar el Model
comodo.

**5 estilos** (se autocrean si faltan; las fuentes deben existir en el CAD):
`ROMANS` (romans.shx), `ST-COORP-GM` (romans.shx), `WMF-Arial0` (Arial),
`ROMANT` (romant.shx), `TREBUCHET` (Trebuchet MS).

**Plot:** la plantilla debe traer configurado `DWG To PDF.pc3`: el PDF
auxiliar se exporta con el. Sin ese plotter el DWG sale, el PDF no.

---

## 13. Porton de validacion de una plantilla nueva (en orden)

```text
DWG nuevo -> GM_VALIDAR_PLANTILLA (solo lee: DWG/CTAB/roles/dimensiones/bloques
             + tipo/handle/caja por rol desde CHG1)
  -> PLANTILLA OK -> GM_PLANOS_LOTE normal
  -> FALTA_ROL:* -> GM_MARCAR_PLANTILLA (VP+LEYENDA) y/o GM_MARCAR_ROL
                    (leyenda-tabla, mapa-bloque) -> re-validar -> lote
  -> GM_DIAGNOSTICO (volcado de soporte con ROL_*)
Piloto: 5 codigos observado -> subset 25 -> lote completo
```

Errores honestos esperados (no detienen el lote, van a `PENDIENTES.csv`):
`POLIGONO` (sin poligono ni U.C.), `AUTOESCALA` (no compone ni a 1/25000),
`TABLAS` (mas de 3 afectaciones), `ETIQUETA_*` (sin posicion limpia).

---

## 14. Checklist rapido de recepcion

- [ ] Model en metros; Paper en mm.
- [ ] Un solo viewport real, >= 50x50, con hueco para esquemas/tablas/pie.
- [ ] `VP_PRINCIPAL` marcado (o detectable).
- [ ] Bloque con `LEYENDA` en el nombre y hueco para su copia.
- [ ] Marco `MAPA_GEO` con cartografia de la zona y 4 unidades libres debajo.
- [ ] Franja `PROYECTO` con hueco libre a la derecha (pie).
- [ ] `GM_VALIDAR_PLANTILLA` => PLANTILLA OK (incluye `MAPA_KIND`).
- [ ] CSV de 19 columnas, sin re-guardar desde Excel, codigos unicos.
- [ ] Poligono matriz con hipervinculo `=codigo` en entidades del Model.
- [ ] `DWG To PDF.pc3` configurado.
- [ ] Piloto de 5 codigos revisado visualmente (MANUAL_USO.md seccion 12.1):
  codigo/membrete, viewport centrado, escala coherente, datos del CSV,
  circulos verdes, sin tablas del codigo anterior, logos intactos.

## 15. Clasificacion del mapa regional (MAPA_KIND, PRY1-MULTI 14-10-2026)

`GM_VALIDAR_PLANTILLA` (y el prime del lote, traza `MAPA_KIND`) ahora clasifica
el modelo de mapa de la plantilla para saber DE ANTEMANO que fuente de
localizacion del predio esperar (ver `GUIA_CIRCULOS_MAPA_REGIONAL.md`):

| MAPA_KIND | Que significa | Localizacion esperada |
|---|---|---|
| `LAYOUT-TICKS` | Ticks UTM legibles en el layout del mapa | Proyeccion exacta del AF (metodo `TICKS`) |
| `BLOQUE-VP` | Mapa en un bloque/vista con VIEWPORT anidado | Ruta/fraccion o textos del bloque (`ESQUEMA_VP`/`DISTRITO`) |
| `BLOQUE` | Mapa dentro de un bloque (INSERT) | Textos del bloque (`DISTRITO`, GEO6) |
| `XREF` | Cartografia importada por referencia externa | Sin fuente legible: `PREDIO_LOCALIZACION;AVISO` honesto |
| `LAYOUT` / `SIN_MARCO` | Sin marco MAPA_GEO o sin fuente | Sin circulos; revisar el marco |

Regla practica para una plantilla nueva (10 minutos): `GM_VALIDAR_PLANTILLA`
debe decir `PLANTILLA OK` + `MAPA_KIND=<tipo>`; correr un piloto de 1 codigo
con el lanzador; si el PDF no trae los circulos verdes, revisar el
`MAPA_KIND` en el reporte antes de escalar a 5/25/234.

Referencias: `SPEC_PLANTILLA_GM.md` (contrato tecnico, raiz),
`ZONAS_LAYOUT_GM.md` (zonas calibradas), `MANUAL_USO.md` (operacion),
`SPEC_METRICAS_GM.md` (estilos y metricas por escala).
