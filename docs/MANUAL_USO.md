# Manual de uso - Sistema GM Ingenieros

Version del manual: 21-10-2026 (sistema PRY1 + HOTFIX 20-10-2026 + entrega
21-10-2026). Destinatarios: operadores y terceros que generan los planos
preliminares de ubicacion a partir de un CSV de predios. No requiere
conocimientos de programacion, solo seguir el flujo.

Indice:

1. [Que es el sistema](#1-que-es-el-sistema)
2. [Requisitos](#2-requisitos)
3. [Carga de los LISP (modo manual)](#3-carga-de-los-lisp-modo-manual)
4. [Flujo manual por codigo](#4-flujo-manual-por-codigo)
5. [Lote corto dentro de AutoCAD](#5-lote-corto-dentro-de-autocad)
6. [Lote largo con el lanzador (recomendado)](#6-lote-largo-con-el-lanzador-recomendado)
7. [Reanudar, detener y continuar](#7-reanudar-detener-y-continuar)
8. [La carpeta de salida](#8-la-carpeta-de-salida)
9. [Como leer los reportes](#9-como-leer-los-reportes)
10. [Errores honestos esperados](#10-errores-honestos-esperados)
11. [Problemas frecuentes](#11-problemas-frecuentes)
12. [Verificacion de calidad](#12-verificacion-de-calidad)
13. [Referencia rapida](#13-referencia-rapida)
14. [Glosario y reglas de oro](#14-glosario-y-reglas-de-oro)

---

## 1. Que es el sistema

Genera, para cada predio de un CSV, un plano preliminar de ubicacion:

- `<CODIGO>.dwg` - plano completo y editable (curvas, grilla, esquemas,
  tablas, membrete, leyenda).
- `<CODIGO> plano preliminar.pdf` - PDF auxiliar (opcional, para revision).
- `GM_PLANOS_LOTE_REPORTE.csv` - bitacora completa de la corrida.
- `GM_PLANOS_LOTE_PENDIENTES.csv` - lista de codigos que no salieron OK.

Componentes:

| Archivo | Funcion |
|---|---|
| `MENU_PLANOS.lsp` | Menu manual: elegir CSV/codigo, generar membrete. |
| `PLANO_UBICACION.lsp` | Model: ubica el poligono, escala, grilla y formato. |
| `LAYOUT_UBICACION.lsp` | Layout: esquemas, tablas, etiquetas y leyenda. |
| `AUTOMATIZACION.lsp` | Lote completo (`GM_PLANOS_LOTE`) y utilidades. |
| `tools/GM_LANZADOR_LOTE.ps1` | Lanzador externo del lote largo (recomendado). |
| `verify-*.cjs` | Verificadores tecnicos (solo desarrollo/QA). |

Sellos de esta version (el sistema se niega a correr si no coinciden):

| Modulo | Sello |
|---|---|
| `MENU_PLANOS.lsp` | `2026-09-30-FIX3` |
| `PLANO_UBICACION.lsp` / `LAYOUT_UBICACION.lsp` | `2026-10-10-CHG3` |
| `AUTOMATIZACION.lsp` | `2026-10-21-FIX1` |

---

## 2. Requisitos

- AutoCAD o Civil 3D instalado; el lanzador pide la ruta de `acad.exe`.
- PowerShell 5.1 (viene con Windows) para el lanzador.
- Node.js 18+ solo si se ejecutan los verificadores (no es necesario para
  generar planos).
- Plantilla maestra `PLANTILLA_ORIGINAL_HUANCASANCOS.dwg` (6,875,601 bytes,
  SHA256 `2875910693DB5313986459820A74026115945AFF7ED3A7968707B5B076A5423D`).
  Si el archivo se llama exactamente asi, el lanzador exige ese tamano y hash
  (preflight y antes de cada bloque). Una segunda plantilla con otro nombre se
  admite por roles (ver `SPEC_PLANTILLA_GM.md`) y queda registrada en el log
  con su hash.
- CSV de predios con **19 columnas** y separador `;` o `,`:
  - columna 15 = **codigo** (ej. `HS-YA-001`), solo letras, numeros, guion y
    guion bajo (`^[A-Za-z0-9_-]+$`);
  - columna 14 = **U.C.** (unidad catastral);
  - las demas columnas las usa el membrete (titular, condicion, fecha, zona,
    datum, etc.). No cambiar la estructura ni el orden.
- Cada poligono matriz del DWG debe tener como hipervinculo **exactamente**
  el codigo del CSV.
- Una copia fresca del master por corrida. No reutilizar un DWG de trabajo
  ya usado por un lote (el sistema lo avisa con `FRESH_CHECK;AVISO`).

---

## 3. Carga de los LISP (modo manual)

### 3.1 Via principal (EMP2): `ABRIR_PLANTILLA.cmd` / `tools\GM_ABRIR_PLANTILLA.ps1`

1. Doble clic en `ABRIR_PLANTILLA.cmd` (en el paquete de entrega) o ejecutar
   `powershell -ExecutionPolicy Bypass -File tools\GM_ABRIR_PLANTILLA.ps1`.
2. Elegir el DWG base en el selector (o pasar `-Dwg "ruta\mi_plantilla.dwg"`;
   con `-SinDibujo` abre AutoCAD vacio y se abre el DWG a mano).
3. La tool agrega las rutas confiables, carga los modulos del manifiesto
   (25 archivos, incluido `MENU_PLANOS.lsp`) con rutas absolutas y deja el
   prompt listo: funciona aunque el DWG viva FUERA de la carpeta del sistema.
4. Ejecutar `GM_VALIDAR_PLANTILLA` / `GM_MARCAR_PLANTILLA` / `GM_MARCAR_ROL`
   y guardar el DWG.
5. La tool rechaza la plantilla maestra oficial (ruta de la raiz del sistema):
   marcar roles siempre sobre una copia.

### 3.2 Respaldo: `APPLOAD` de las 4 raices

1. Abrir el DWG de trabajo en AutoCAD.
2. Escribir `APPLOAD` y cargar **en este orden**:
   1. `MENU_PLANOS.lsp` (codigo real del menu/membrete)
   2. `PLANO_UBICACION.lsp` (ancla UBI)
   3. `LAYOUT_UBICACION.lsp` (ancla LAY)
   4. `AUTOMATIZACION.lsp` (ancla GM)
3. Los tres ultimos archivos son **anclas** de 35 lineas: al cargarlos, cada
   una carga sus modulos desde la carpeta `lisp/` (el orden real esta en
   `lisp/MANIFIESTO_MODULOS.txt`). Mantener siempre la carpeta `lisp/` junto a
   los anclas; el ancla busca `lisp/MANIFIESTO_MODULOS.txt` desde la carpeta
   del dibujo hacia arriba: solo funciona si el DWG vive dentro de la carpeta
   del sistema (o subcarpeta hasta 6 niveles). Si vive fuera, usar la
   herramienta de 3.1.
4. Si el orden o los sellos estan mal, el lote avisa
   "Modulos desactualizados o sin cargar" y no inicia. Recargar en el orden
   indicado.
5. Los 4 archivos raiz son necesarios y se entregan: `MENU_PLANOS.lsp` es
   codigo real; las otras 3 son anclas que portan el sello de familia
   (`*GM_VER_PLANO/LAYOUT/AUTO`), verificado por el lanzador antes de cada
   corrida y por el empaquetador.

Atajos utiles antes de operar:

- `GM_VALIDAR_PLANTILLA` - diagnostico de la plantilla (roles, viewports,
  dimensiones, tipo/handle/caja por rol). No modifica nada.
- `GM_MARCAR_PLANTILLA` - si falta un rol (`FALTA_ROL:*`), lo asigna una vez
  y se guarda en el DWG (viewport y leyenda-bloque).
- `GM_MARCAR_ROL` - marca un rol a mano: elegir `1` viewport, `2` leyenda,
  `3` mapa o `4` proyecto, y despues hacer clic en la entidad (util cuando la
  leyenda es una tabla o el mapa es un bloque sin deteccion automatica).
- `GM_DIAGNOSTICO` - escribe `GM_DIAGNOSTICO.txt` junto al DWG para soporte.

---

## 4. Flujo manual por codigo

Para corregir un plano puntual o aprender el sistema.

### 4.1 Seleccionar datos

1. Ejecutar `MENU_PLANOS`.
2. Elegir el CSV y escribir el codigo (o usar `Seleccionar` para capturarlo
   desde un texto del Model).
3. Tipo de plano `1. UBICACION`.

### 4.2 Membrete

1. Boton `1. GENERAR MEMBRETE`.
2. Presionar `ENTER` para la posicion automatica (margen 15), o hacer clic
   para fijar la esquina inferior derecha.

### 4.3 Model y viewport

1. Ejecutar `PLANO_UBICACION`.
2. Indicar el viewport de trabajo (si hay uno solo se detecta solo).
3. Configurar VP dinamico; el poligono se busca por hipervinculo exacto.
4. Aplicar escala sugerida (automatica, minimo `1/1000`) y `Actualizar VP`.
5. `Formato Magico` (visibilidad y colores) y `Grilla` si corresponde.

### 4.4 Layout

1. Ejecutar `LAYOUT_UBICACION`.
2. Generar los esquemas PU.
3. Ubicar flechas y etiquetas que requieran criterio visual.
4. Revisar solapes y textos antes de aprobar.

---

## 5. Lote corto dentro de AutoCAD

Para unos pocos codigos sin salir del CAD. Para lotes en serie la via
recomendada es el lanzador (seccion 6); este modo queda como apoyo y
continuacion (`GM_CONTINUAR_LOTE`), y sigue siendo la funcion que el
lanzador invoca internamente.

1. Activar el **Layout plantilla** (no ejecutar desde Model).
2. Ejecutar `GM_PLANOS_LOTE`.
3. En el panel: elegir CSV y carpeta de salida; decidir si senalar el predio
   en el mapa regional.
4. Elegir salida con clic explicito:
   - `GENERAR SOLO DWG`;
   - `GENERAR DWG + PDF`.
   (ENTER no inicia el lote.)
5. El sistema procesa los codigos del CSV, exporta y limpia antes del
   siguiente. Al final deja el REPORTE y los PENDIENTES en la carpeta.
6. Si el lote se detiene (cierre, ESC, corte), reabrir el DWG y ejecutar
   `GM_CONTINUAR_LOTE` (retoma sin volver a pedir datos).

Notas:

- La compactacion en caliente (`CHUNK`, cada 12 codigos) viene activa para
  sesiones largas dentro de un mismo AutoCAD; el lanzador externo la fija
  en `0` porque el reciclaje del proceso ya hace esa funcion.
- El tope legado `*GM_AUTO_MAX_CODES*` pausa el lote tras N codigos; para
  continuar, cerrar/reabrir el DWG y ejecutar `GM_CONTINUAR_LOTE`.

---

## 6. Lote largo con el lanzador (recomendado)

Para 20-250 planos. Cada bloque corre en un **AutoCAD nuevo** sobre una copia
temporal, sin tocar la plantilla original. Es la forma estable de generar el
lote completo.

### 6.1 Preparacion

1. Cerrar **todas** las ventanas de AutoCAD/Civil 3D (el lanzador no cierra
   sesiones ajenas).
2. Copiar la plantilla maestra al proyecto si no esta (nombre exacto
   `PLANTILLA_ORIGINAL_HUANCASANCOS.dwg`).
3. Tener el CSV definitivo (no editarlo durante la corrida).

### 6.2 Ejecucion

**Para el operador: doble clic en `INICIAR_LOTE.cmd`** (en la raiz del
paquete de entrega). El `.cmd` quita la marca de descarga de Windows
(`Zone.Identifier`) y lanza el lanzador con `-ExecutionPolicy Bypass`, sin
tocar la politica del equipo. **No ejecute `tools\GM_LANZADOR_LOTE.ps1`
directamente**: con la politica `RemoteSigned` Windows bloquea los scripts
descargados con el mensaje "no esta firmado digitalmente" (ver seccion 11).

Desde la carpeta del proyecto (usuarios tecnicos):

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File ".\tools\GM_LANZADOR_LOTE.ps1"
```

La ventana abre en el **panel de Proyectos**: doble clic (o `Abrir proyecto`)
carga el proyecto en el panel de detalle; `Crear proyecto...` pide un nombre
(letras, numeros, guion y guion bajo) y arma su carpeta `proyectos\<nombre>\`
con salida propia `proyectos\<nombre>\salida_<nombre>`; `Eliminar proyecto`
pide confirmacion explicita (con inventario de DWG/PDF) y mueve registro +
salida a `proyectos\_borrados\` con fecha; los CSV y la plantilla no se tocan.
Cada proyecto guarda su CSV, plantilla, salida, opciones y tipo de plano; al
elegir otro proyecto se guarda el anterior solo. En el panel de detalle, la
pestana **Planos del proyecto** lista lo generado (filtros por tipo, formato y
una caja unica para codigo / U.C. / distrito).

Campos de la ventana:

| Campo | Valor tipico | Que hace |
|---|---|---|
| CSV de entrada | `CAJETIN HUANCA.csv` | Codigos a procesar (19 columnas). |
| DWG plantilla | `PLANTILLA_ORIGINAL_HUANCASANCOS.dwg` | Master read-only. |
| Carpeta salida | `proyectos\huancasancos\salida_huancasancos` | Destino de planos y reportes (por proyecto; editable). |
| `acad.exe` | ruta de AutoCAD 2021+ | Ejecutable a lanzar. |
| Tipo de plano | `UBICACION` | Estilo del formato (`*GM_PLANO_TIPO*`): UBICACION/MATRIZ/DIAGNOSTICO/PERIMETRICO (se guarda por proyecto). |
| Datos de VP (grilla) | `NORMAL` | `*UBI_VP_TYPE*`: NORMAL o UBICACION. |
| DWG + PDF / Solo DWG | DWG + PDF | PDF auxiliar ademas del DWG. |
| Senalizar predio... | marcado | Circulos verdes del predio en el mapa regional. |
| Dibujar rotulo DEPARTAMENTO/PROVINCIA/DISTRITO | marcado | Bloque de texto geografico sobre el mapa regional (CHG2); desmarcarlo lo omite con traza `GEO_FIJO;OMITIDO`. CHG4: al iniciar el lote se borran los rotulos viejos que la plantilla haya guardado (`GEO_BASE_LIMPIA;N=n`); con la opcion apagada el plano queda sin ningun rotulo. PRY1-GEO2: el rotulo sale siempre en su tamano original; si el mapa no tiene hueco libre se coloca igual sobre el mapa (traza `Texto AMPLIO-SOLAPADO`, sin fondo: puede cruzar letras del mapa) y si la fuente original no esta instalada se usa Arial (`GEO_FUENTE`). PRY1-GEO3: estilo de la imagen de referencia (DEPARTAMENTO rojo con valor mas grande, PROVINCIA negro, DISTRITO magenta, todo en negrita). PRY1-GEO5: pegado a la esquina superior derecha y sin fondo blanco. PRY1-GEO4: la columna 12 del CSV admite **dos distritos** separados por `-`, `/` o `|` (ej. `CHUGAY - COCHORCO`) y el rotulo lista ambos; mas de dos partes se avisa en `verify-csv`. |
| Codigos por proceso | `25` | Tamano de bloque (procesos nuevos). |
| Compactacion interna | `0` | 0 = la hace el reciclaje externo. |
| Avisar sin actividad | `15` min | Aviso si el CAD no escribe en 15 min. |
| Modo diagnostico | desmarcado | Genera forense + monitor + log en `_sistema/`. |
| Reintentar errores | marcado | Un reintento extra de los ERROR al final. |

Cada una de las cuatro rutas (CSV, plantilla, carpeta de salida y `acad.exe`)
tiene su boton **Examinar...**, siempre visible: la ventana se ajusta al area
de trabajo de la pantalla. Al elegir una ruta, la seleccion queda guardada al
instante en `proyectos\<Nombre>\proyecto.json`.

1. Boton `Revisar configuracion` (preflight): valida CSV (19 columnas,
   codigos unicos), `acad.exe`, plantilla por SHA256, sellos LISP y carpeta.
2. Boton `Empezar desde cero` (confirmar el aviso de archivado), o
   `Reanudar corrida` si ya existe una.
3. Durante la corrida: no abrir AutoCAD a mano, no cambiar archivos, no
   apagar la maquina. La barra muestra bloque, PID, memoria y cola.
4. `Detener bloque` cierra **solo** el AutoCAD del lanzador; el codigo actual
   se reintenta al reanudar.
5. `Abrir salida` abre la carpeta de salida del proyecto
   (`proyectos\<nombre>\salida_<nombre>` por defecto); `Activar LocalDumps
   (admin)` es solo para diagnostico de cuelgues.
6. Al terminar: `Corrida completa. OK en progreso: N; pendientes finales: M`.

Detalles de comportamiento:

- Cada bloque copia el master a
  `%TEMP%\GM_LOTE_<RunId>\bloque_NNN\GM_TRABAJO_FRESCO.dwg` y arranca
  `acad.exe /b` con un script que carga los modulos del manifiesto y cierra
  con `QUIT/No`. El master nunca se abre ni se guarda.
- El lanzador arranca AutoCAD en **modo desatendido**: sin modales de
  confirmacion (modo silencioso QUIET1) y con el diagnostico apagado salvo
  que se marque.
- Un cierre de AutoCAD con codigo no nulo (`-1073610751`) es normal si los
  codigos del bloque ya tienen `RESULTADO`: queda registrado como aviso.
- Si un codigo no llega a `RESULTADO` en 3 procesos, el lanzador pausa solo
  (pausa segura) para revisar antes de insistir.
- Al cerrar cada bloque, el panel **Pendientes** del lanzador lista las filas
  de `GM_PLANOS_LOTE_PENDIENTES.csv` (`CODIGO`/`ETAPA`/`ESTADO`/`MOTIVO`);
  doble clic sobre una fila abre el PDF (o el DWG) del codigo si existe.

---

## 7. Reanudar, detener y continuar

- **Reanudar corrida**: requiere el **mismo** CSV, plantilla, carpeta y
  `acad.exe` (se validan por hash). Si algo cambio, el lanzador no inicia.
  Retoma los codigos sin `RESULTADO` y los ERROR reintentables. En el lanzador
  manda el manifiesto `_sistema/GM_PLANOS_LOTE_LANZADOR.json`; ademas se
  comprueba que el sidecar `_sistema/GM_PLANOS_LOTE_CONFIG.csv` apunte al mismo
  CSV (si no coincide, no inicia). El sidecar es la fuente cuando se reanuda
  con `GM_CONTINUAR_LOTE` dentro de AutoCAD.
- **Detener bloque**: pausa ordenada; al reanudar se repite el codigo que
  quedo a medias y se conservan los resultados anteriores.
- **Cerrar la ventana del lanzador** no es posible con un bloque activo:
  detenerlo primero.
- **Reanudar dentro de AutoCAD** (sin lanzador): reabrir el DWG y ejecutar
  `GM_CONTINUAR_LOTE`. Usa el sidecar (`_sistema/GM_PLANOS_LOTE_CONFIG.csv`)
  como fuente de verdad; si el CSV no corresponde al progreso, no inicia y
  avisa. No usar `GM_PLANOS_LOTE` para continuar: trunca el reporte.

---

## 8. La carpeta de salida

Cada proyecto escribe en su propia salida, `proyectos\<nombre>\
salida_<nombre>` (default del lanzador, editable en el campo "Carpeta
salida"). Si editas la ruta, asegurate de que exista y no sea blanco ni herraje
la raiz. La salida antigua (`salida/` o `2do modelo base\salida nueva`) queda
intacta; el lanzador solo "gira" el proyecto a la ruta nueva.

Desde SYS1 la entrega queda limpia; el estado interno vive en `_sistema/`:

```text
proyectos/<proyecto>/salida_<proyecto>/
    HS-YA-001.dwg
    HS-YA-001 plano preliminar.pdf
    HS-CH-016.dwg
    HS-CH-016 plano preliminar.pdf
    GM_PLANOS_LOTE_REPORTE.csv        <- bitacora (humano)
    GM_PLANOS_LOTE_PENDIENTES.csv     <- lista de trabajo (humano)
    _sistema/                         <- NO entregar; estado interno
        GM_PLANOS_LOTE_PROGRESO.csv
        GM_PLANOS_LOTE_CONFIG.csv
        GM_PLANOS_LOTE_LANZADOR.json
        GM_PLANOS_LOTE_PENDIENTES_PARCIAL.csv
        GM_PLANOS_LOTE_FORENSE.csv    (solo Modo diagnostico)
        GM_PLANOS_LOTE_MONITOR.csv    (solo Modo diagnostico)
        GM_PLANOS_LOTE_LANZADOR.log   (solo Modo diagnostico)
        *.bak                         (respaldo del manifiesto y de PENDIENTES)
    _historial_lote_<fecha>/          <- corridas archivadas (incluye su _sistema)
```

- `Empezar desde cero` archiva `_sistema/` (y restos de corridas planas
  antiguas) en `_historial_lote_<fecha>`; los DWG/PDF previos se conservan.
- **Entrega al cliente/revisor**: solo los DWG/PDF + `REPORTE` +
  `PENDIENTES`. `_sistema/` es soporte interno.
- El paquete de usuario final no trae carpeta `salida/`: cada proyecto crea su
  carpeta `salida_<nombre>` al arrancar el lote.
- **Al abrir un proyecto** el lanzador sincroniza su salida: crea
  `salida_<nombre>` si no existe y respeta la ruta personalizada si la hay. Si
  no puede prepararla, avisa y no abre el proyecto.
- **Al eliminar un proyecto** el lanzador mueve su registro y su salida a
  `proyectos\_borrados\<nombre>_<fecha>\` (no borra nada: el respaldo queda).
  Se niega a mover la carpeta `proyectos\`, la de `_borrados`, la de otro
  proyecto y cualquier enlace (junction/symlink). Los nombres `CON`, `PRN`,
  `AUX`, `NUL`, `COM1-9`, `LPT1-9` y `_borrados` estan reservados: no se
  pueden usar como nombre de proyecto.

---

## 9. Como leer los reportes

### 9.1 REPORTE (`TIPO;CODIGO;ETAPA;ESTADO;MENSAJE;ARCHIVO`)

- `RESULTADO;HS-YA-001;FIN;OK;...` - el plano se genero.
- `RESULTADO;HS-MC-015;FIN;ERROR;...` - el codigo fallo (esta en PENDIENTES).
- `ETAPA;LOTE;MODO_SALIDA;INFO;DWG+PDF` - configuracion de la corrida.
- `ETAPA;LOTE;PREDIO_LOC_CONFIG;INFO;circulos... ACTIVADOS` - senalizacion.
- `ETAPA;LOTE;GEO_TXT_CONFIG;INFO;rotulo... ACTIVADO` - rotulo DEP/PROV/DIST
  (CHG2); su omision por codigo se traza como `GEO_FIJO;OMITIDO`.
- `ETAPA;LOTE;FRESH_CHECK;AVISO;...` - el DWG traia huellas; informativo.
- `ETAPA;LOTE;CORRIDA;INFO;id=... sello=2026-10-21-FIX1 ...` - quien corrio.
- `ETAPA;LOTE;GEO_BASE_LIMPIA;OK;N=n` - el lote borro `n` rotulos
  DEP/PROV/DIST viejos del modelo base (contaminacion de una corrida previa);
  con el rotulo ON el plano lleva solo el fresco, con OFF queda sin ninguno.
- `ETAPA;LOTE;UNDO_LOTE;OMITIDO;...` - grupo UNDO activo; informativo.
- `ETAPA;LOTE;VISIBILIDAD_FINAL;OK;restauradas=N` - cierre limpio.
- `ETAPA;<CODIGO>;CAPAS_PREDIO;OK;reactivadas=N` - la plantilla traia capas
  del predio (matriz/AF/PF) congeladas o apagadas y el lote las reactivo; el
  relleno del area afectada se ve en el DWG y en el PDF.
- `RESUMEN_LOTE` - resumen final de la corrida.

Regla practica: `RESULTADO` manda. Las filas `ETAPA` explican.

### 9.2 PENDIENTES (`CODIGO;ETAPA;ESTADO;MOTIVO;ARCHIVO`)

- `ERROR` - el codigo no salio OK; el motivo indica la etapa.
- `AVISO` - salio OK pero con observacion (ej. `PREDIO_LOCALIZACION`:
  plano sin circulos verdes).
- `NO_INTENTADO` - la corrida se interrumpio antes de procesarlo.

Al re-correr un pendiente que queda OK, su fila desaparece. Si el lote
termina sin pendientes, el archivo queda solo con la cabecera. El lanzador
refleja este archivo en su panel de pendientes al terminar cada bloque (doble
clic abre el plano del codigo).

### 9.3 PROGRESO (`_sistema/`)

Una linea por codigo OK. Es el estado de reanudacion; `PROGRESO` = codigos
con DWG verificable. No editar a mano salvo soporte tecnico.

---

## 10. Errores honestos esperados

Son fallos cerrados y limpios: el sistema prefiere no exportar antes que
entregar un plano incorrecto. **No detienen el lote** y se reintentan una
vez si el reintento esta activo.

| Etapa | Significado | Re-trabajable |
|---|---|---|
| `POLIGONO` | No hay poligono ni referencia U.C. para el codigo. | Revisar DWG/CSV; puede ser dato faltante. |
| `AUTOESCALA` | El predio no compone ni a 1/25000 (invade zonas). | Revisar geometria del predio. |
| `TABLAS` | Mas de 3 afectaciones; el contrato dibuja 2 (50x24) o 3 (50x15). | No re-trabajable. |
| `ETIQUETA_AFECTACION_*` / `ETIQUETA_PROPIETARIO` | Sin posicion limpia para la etiqueta. | Reintento; si repite, revisar el DWG. |
| `AF_PF_TRAMO` / `ESQUEMAS` | Falla interna al componer esquemas. | Reintento en sesion limpia. |
| `PREDIO_LOCALIZACION;AVISO` | El mapa regional no expone fuente legible (cartografia importada/XREF o sin ticks U.C./distrito). El plano exporta sin circulos verdes ni zoom; la traza `MAPA_XREF` indica la causa si aplica. | Incorporar la cartografia al DWG (bind) o dibujar la ruta/ticks como entidad interna; re-correr el codigo. |
| `EXPORTACION_PDF` | El PDF salio vacio, truncado o sin cabecera `%PDF`. El archivo invalido se borra (no queda un PDF roto que parezca recien generado) y el plano DWG se conserva. | Reintento; si repite, revisar el plotter `DWG To PDF.pc3` de AutoCAD. |

Ejemplos reales del lote de Huancasancos (234): `HS-HU-176`, `HS-SJ-195`
(POLIGONO); `HS-CA-189`, `HS-SR-190`, `HS-SJ-191`, `HS-SR-193`, `HS-SR-199`,
`HS-SH-218`, `HS-SH-220`, `HS-SA-222` (AUTOESCALA); `HS-EP-231` (TABLAS, 5
afectaciones). Estos errores quedan en PENDIENTES y no invalidan el resto.

---

## 11. Problemas frecuentes

| Sintoma / mensaje | Causa | Accion |
|---|---|---|
| "Modulos desactualizados o sin cargar" | LISP viejos o mal orden. | Reanudar con el lanzador (carga por manifiesto) o `ABRIR_PLANTILLA.cmd`; a mano, `APPLOAD` en orden MENU, PLANO, LAYOUT, AUTOMATIZACION (secc. 3.2). |
| "GM_PLANOS_LOTE debe iniciarse desde el Layout plantilla activo" | Se ejecuto desde Model. | Activar el Layout y repetir. |
| "Progreso incompatible: 'X' no existe en el CSV" | Se cambio el CSV de una corrida con progreso. | Usar el mismo CSV o `Empezar desde cero`. |
| "No coincide ... con la corrida guardada" (reanudar) | CSV/plantilla/salida/`acad.exe` distintos. | Restaurar los originales o iniciar de cero. |
| AutoCAD cerro con codigo no nulo, pero todo tiene RESULTADO | Cierre no-cero benigno. | Solo observar; si se repite, revisar MONITOR/FORENSE (diagnostico). |
| Bloque sin escribir reporte y "Avisar sin actividad" | Modal/ventana inesperada en AutoCAD. | Fotografiar la pantalla, `Detener bloque`, revisar log y reanudar. |
| "Pausa segura: 'X' no llego a RESULTADO tras 3 procesos" | Codigo problematico. | Revisar PENDIENTES/FORENSE y reintentar; puede quedar como honesto. |
| `FRESH_CHECK;AVISO` | DWG con huellas de corrida previa. | Informativo; para maxima estabilidad usar copia fresca. |
| `UNDO_LOTE;OMITIDO` | Grupo UNDO activo. | Informativo, no requiere accion. |
| "RESULTADO OK sin DWG verificable" | DWG borrado o movido. | El lanzador lo devuelve a la cola y lo regenera. |
| "Selecciona un CSV y una carpeta de salida valida" | Panel sin datos. | Cargar CSV/carpeta existentes. |
| Caracteres raros en el CSV (ej. `R?STICO`) | Archivo re-guardado en otra codificacion. | Usar el CSV original (ANSI/Latin1); no re-guardar desde Excel. |
| Falta el PDF pero el DWG esta | `EXPORTACION_PDF;ERROR`. | El DWG es valido; re-generar el PDF del codigo puntual. |
| Labels/etiquetas superpuestas | Caso visual puntual. | Completar manualmente con `LAYOUT_UBICACION`. |
| "El archivo ... no esta firmado digitalmente" (o `Zone.Identifier`) al ejecutar un `.ps1` | El paquete venia de un ZIP descargado: Windows marca todo lo que se extrae y la politica `RemoteSigned` bloquea los scripts marcados. | Usar `INICIAR_LOTE.cmd` (quita la marca solo), o `DESBLOQUEAR.cmd` en la raiz, o `Get-ChildItem -Recurse -Filter *.ps1 | Unblock-File` desde la raiz. En el futuro: desbloquear el ZIP antes de extraer (Propiedades > Desbloquear). |
| "Windows protegio tu PC" (aviso azul de SmartScreen) al abrir un `.cmd` | Descarga desde internet sin confirmar el editor. | "Mas informacion" > "Ejecutar de todas formas" (una vez). No hace falta cambiar la politica de ejecucion. |
| El lanzador no aparece / se cierra la ventana al instante | Se ejecuto el `.ps1` con doble clic (no es un ejecutable). | Doble clic en `INICIAR_LOTE.cmd` (raiz del paquete). |

---

## 12. Verificacion de calidad

### 12.1 Revision visual (siempre)

Abrir 2-3 PDF de la corrida y comprobar:

1. El codigo del membrete coincide con el nombre del archivo.
2. El viewport esta centrado en el predio correcto.
3. La escala es coherente (AUTO; minimo `1/1000`).
4. Propietario, condicion y U.C. coinciden con el CSV.
5. Los circulos verdes del mapa regional aparecen (si se activo).
6. No hay tablas ni datos del codigo anterior.
7. Logos y mapa general intactos.

### 12.2 Verificacion tecnica (desarrollo/QA)

Desde la carpeta del proyecto:

```powershell
node verify-csv.cjs
node verify-autolisp.cjs
node verify-diagnostico.cjs
node verify-layout.cjs
node verify-lote.cjs
node verify-plantilla.cjs
node verify-symbols.cjs
```

Todos deben terminar con EXIT 0. `verify-autolisp.cjs` es la compuerta que
impide llamar a funciones que AutoLISP no tiene (por ejemplo `unwind-protect`,
que es de Common Lisp y en AutoCAD 2021 abortaba la etapa de Layout). Su
referencia es la tabla de simbolos real de AutoCAD 2021
(`tools/funciones_autocad_2021.txt`), que se regenera con
`tools\GM_SONDA_FUNCIONES.ps1` (abre AutoCAD). Para el lanzador:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File ".\tools\GM_LANZADOR_LOTE.ps1" -SelfTest
powershell.exe -NoProfile -ExecutionPolicy Bypass -File ".\tools\GM_LANZADOR_LOTE.ps1" -UiSmokeTest
powershell.exe -NoProfile -ExecutionPolicy Bypass -File ".\tools\GM_LANZADOR_LOTE.ps1" -StartupSmoke
```

`-SelfTest` valida CSV (234 oficial + 5 piloto), sellos, preflight (incluido el
hash dorado de la plantilla oficial), la reconciliacion de pendientes y la
reanudacion (respaldo `.bak` del manifiesto, poda de campos legados y sidecar
de configuracion). Exige `acad.exe` localizado y los archivos oficiales
presentes; no abre AutoCAD ni modifica el master.

`-StartupSmoke`: crea el store y la salida de huancasancos (`STARTUP OK` en
consola). Sirve para probar el paquete o una carpeta sin abrir AutoCAD ni la
ventana; si la carpeta ya existe, es sincronizacion idempotente (SAL2).

**Regla de QA para cambios de LISP (20-10-2026):** los verificadores NO
alcanzan. Ademas de los 7 en verde, cualquier correccion de LISP exige un
**E2E real en AutoCAD**, porque un verificador estatico no puede saber si un
simbolo existe en el interprete:

```powershell
powershell -ExecutionPolicy Bypass -File "salida_diag\HOTFIX_2026-10-20\harness\gm-hotfix-e2e.ps1" -Csv "1 Codigo.csv" -Tag HUANCA1B
powershell -ExecutionPolicy Bypass -File "salida_diag\HOTFIX_2026-10-20\harness\gm-hotfix-e2e.ps1" -Csv "PRUEBA_5_PREDIOS.csv" -Tag HUANCA5
```

Referencia: 1 codigo debe dar un PDF de 1.172.152 bytes (identico al golden)
y 5 codigos **4 OK + el `HS-CA-189` en AUTOESCALA**.

Nota para la version de entrega: los `verify-*.cjs` y `-UiSmokeTest` son
herramientas internas de desarrollo (requieren Node.js y el codigo fuente
completo) y NO viajan en el paquete. En el paquete de entrega la validacion
de fabrica ya corrio: el empaquetador ejecuta los 7 verificadores, el
`-SelfTest` y el `-StartupSmoke` **sobre copias temporales** (la entrega
queda intacta) y despues exige que el paquete este limpio: sin `proyectos\`,
sin `.git`, sin basura de editor, con el conjunto exacto de 43 archivos (37 de
fuente + 5 generados + el manifiesto) y sin ninguna ruta absoluta del equipo
que lo armo. En campo se valida con `Revisar configuracion` (preflight) y
`-SelfTest` del lanzador, que no requieren Node.js. Para marcar roles en una
plantilla propia usar `ABRIR_PLANTILLA.cmd` (secc. 3.1).

El `MANIFIESTO_ENTREGA.txt` del paquete lista los 42 archivos con su tamano y
su SHA256 (37 de fuente y 5 generados; el manifiesto no se hashea a si mismo).
Para comprobar un paquete recibido:

```powershell
# en la raiz del paquete descomprimido
$man = Get-Content .\MANIFIESTO_ENTREGA.txt
$man | Where-Object { $_ -match '^[A-Za-z].*;[0-9]+;[0-9A-F]{64}$' } | ForEach-Object {
  $p = $_.Split(';')
  $f = Join-Path (Get-Location) $p[0]
  if ((Get-FileHash -LiteralPath $f -Algorithm SHA256).Hash -ne $p[2]) { "DIFIERE: " + $p[0] }
}
```

El desempacquetado marca los archivos con `Zone.Identifier` y con eso PowerShell
rechaza los `.ps1` ("no esta firmado digitalmente"). Use siempre
`INICIAR_LOTE.cmd` (quitan la marca al arrancar) o `DESBLOQUEAR.cmd` (que ahora
informa la verdad: si no pudo quitar la marca de todos los archivos, lo dice).

### 12.3 Antes de un lote largo

1. Piloto de 5 codigos (`PRUEBA_5_PREDIOS.csv`) observado de punta a punta.
   El switch `-PilotTest` del lanzador automatiza ese piloto: usa
   `PRUEBA_5_PREDIOS.csv`, bloque 5, diagnostico activo y salida en `%TEMP%`.
2. Subset de 25.
3. Lote completo.

No promover a lote grande si un bloque quedo sin reporte o si aparecio un
modal inesperado.

---

## 13. Referencia rapida

### Comandos AutoCAD

| Comando | Uso |
|---|---|
| `MENU_PLANOS` | Menu manual (CSV, codigo, membrete). |
| `PLANO_UBICACION` | Viewport, escala, grilla, formato (interactivo). |
| `PLANO_UBICACION_AUTO` | Version encadenada para el lote. |
| `LAYOUT_UBICACION` | Esquemas, tablas, etiquetas, leyenda. |
| `GM_PLANOS_LOTE` | Lote con panel (corto). |
| `GM_CONTINUAR_LOTE` | Reanudar en 1 clic (sin panel). |
| `GM_VALIDAR_PLANTILLA` | Diagnostico de roles de plantilla. |
| `GM_MARCAR_PLANTILLA` | Marcar roles faltantes (`GM_ROL`). |
| `GM_MARCAR_ROL` | Marcar un rol por seleccion explicita (leyenda tabla, mapa). |
| `GM_DIAGNOSTICO` | Volcar `GM_DIAGNOSTICO.txt` para soporte. |

### Variables utiles (consola LISP)

| Variable | Default | Efecto |
|---|---|---|
| `*GM_AUTO_QUIET*` | nil | T = sin modales informativos (lo fija el lanzador). |
| `*GM_AUTO_CHUNK*` | 12 | Compactacion en caliente cada N codigos; 0 = off. |
| `*GM_AUTO_FORENSIC*` | T | Forense on/off (el lanzador lo apaga salvo diagnostico). |
| `*GM_AUTO_SHOW_PREDIO_LOC*` | "1" | Circulos verdes del predio. |
| `*GM_AUTO_SHOW_GEO_TXT*` | "1" | Rotulo DEP/PROV/DIST del mapa (CHG2); "0" lo omite. CHG4: los rotulos viejos de la plantilla se limpian al iniciar el lote en ambos modos. |
| `*GM_PLANO_TIPO*` | "UBICACION" | Estilo del formato del lote (PRY1): UBICACION/MATRIZ/DIAGNOSTICO/PERIMETRICO. El lanzador lo fija por proyecto y el sidecar lo conserva al continuar. |
| `*UBI_VP_TYPE*` | "1" | Datos de grilla del VP (PRY1): "1" NORMAL / "2" UBICACION. |
| `*GM_AUTO_MAKE_PDF*` | T | PDF auxiliar ademas del DWG. |
| `*GM_AUTO_MAX_CODES*` | 0 | Tope por corrida manual (0 = sin tope). El lanzador lo fija siempre en 0: la unica pausa es el reciclaje por bloques. |

Nota de defaults por via de ejecucion (intencional): el lanzador externo usa
`*GM_AUTO_CHUNK*` 0 (sin compactacion en caliente; el reciclaje por bloques hace
esa funcion), `*GM_AUTO_FORENSIC*` nil salvo Modo diagnostico, `*GM_AUTO_QUIET*`
T y `FRESH_CHECK` silenciado (re-hashea por bloque). El flujo manual conserva
los defaults de la tabla (CHUNK 12, FORENSIC T, QUIET nil).

### Rutas clave

| Ruta | Contenido |
|---|---|
| `proyectos\<proyecto>\salida_<proyecto>/*.dwg` / `*.pdf` | Planos por codigo. |
| `proyectos\<proyecto>\salida_<proyecto>/GM_PLANOS_LOTE_REPORTE.csv` | Bitacora de la corrida. |
| `proyectos\<proyecto>\salida_<proyecto>/GM_PLANOS_LOTE_PENDIENTES.csv` | Lista de trabajo. |
| `proyectos\<proyecto>\salida_<proyecto>/_sistema/` | Estado interno (no entregar). |
| `proyectos\_borrados\` | Respaldos de proyectos eliminados (SAL2). |
| `%TEMP%\GM_LOTE_<RunId>/` | Copias temporales por bloque. |

---

## 14. Glosario y reglas de oro

### Glosario

- **Lote**: procesar el CSV completo codigo por codigo.
- **Bloque**: grupo de N codigos (25 por defecto) en un AutoCAD nuevo.
- **Copia fresca**: copia temporal del master, sin huellas de corridas.
- **Sidecar**: `_sistema/GM_PLANOS_LOTE_CONFIG.csv`; config en disco que
  manda al reanudar.
- **WBLOCK**: exportacion del DWG final del codigo (`-WBLOCK *`).
- **Error honesto**: fallo cerrado y limpio (no exporta, no contamina);
  queda en PENDIENTES.
- **Fail-closed**: ante duda el sistema no inicia/exporta en vez de arriesgar.
- **Roles**: marcas persistentes de la plantilla (`VP_PRINCIPAL`, `LEYENDA`,
  `MAPA_GEO`, `PROYECTO`) que el script detecta.
- **AUTOESCALA**: escala automatica por viewport; minimo `1/1000`.

### Reglas de oro

1. La **plantilla maestra nunca se abre ni se guarda** durante el lote; el
   lanzador trabaja sobre copias temporales.
2. **Cerrar todo AutoCAD** antes de abrir el lanzador.
3. No tocar la PC ni el CAD mientras corre un bloque.
4. No cambiar CSV/plantilla/carpeta a mitad de una corrida; para reanudar
   deben ser los mismos (hash).
5. No re-guardar el CSV con otra codificacion ni editar su estructura de 19
   columnas.
6. Revisar `PENDIENTES.csv` al terminar: los errores honestos son esperados
   y van a la lista de trabajo, no son fallos del sistema.
7. No restaurar los LISP desde zips/copias viejas sin re-sincronizar sellos;
   el preflight y `-SelfTest` lo detectan.
8. Entregar solo planos + REPORTE + PENDIENTES; `_sistema/` es interno.
9. Ante cuelgue o modal, `Detener bloque`, guardar evidencia (captura/log) y
   reanudar; el progreso no se pierde.
10. **Arrancar siempre por `INICIAR_LOTE.cmd` / `ABRIR_PLANTILLA.cmd`** (raiz
    del paquete), nunca por el `.ps1` suelto: los `.cmd` resuelven la marca de
    descarga de Windows y la politica de ejecucion solo para su proceso.
