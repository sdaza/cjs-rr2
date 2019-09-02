##########################
# experimental setup
# author: sebastian daza
##########################

# libraries
library(sdazar)
library(randomizr)
library(readxl)
library(tableone)
library(lubridate)

table = function (...) base::table(..., useNA = 'ifany')

# read excel file
dt = data.table(read_excel("data/BASE DE DATOS OFICIAL COHORTE 2.xlsx"))
setnames(dt, names(dt), tolower(names(dt)))

# muestra
setnames(dt, 'parte de la muestra', 'muestra')
dt = dt[muestra == "SÍ"]
dt[, muestra := "si"]
table(dt$muestra)

# tratamiento
setnames(dt, "grupo (por definir)...5", 'tratamiento')
table(dt$tratamiento)

# unidad
table(dt$unidad)
dt = dt[unidad ==  "C.D.P SANTIAGO SUR"]

# escala pre
setnames(dt, "fecha escalas pre", "escala_pre")
table(dt$escala_pre)

dt = dt[escala_pre != "NO APLICA" & escala_pre != ""]
table(dt$escala_pre)
table(dt$muestra) # 136 para C.D.P SANTIAGO SUR

setnames(dt, "promedio final", "escala")
dt[, escala := as.numeric(escala)]
summary(dt$escala)

# fecha de inicio
setnames(dt, 'fecha inicio', 'fecha_inicio')
dt[, tiempo_carcel := interval(fecha_inicio, "2019-09-02") / years(1)]
dt[, .(fecha_inicio, tiempo_carcel)]

# delito
table(dt$delito)

# compromiso delictual
setnames(dt, "compromiso delictual", "compromiso_delictual")
table(dt$compromiso_delictual)

table(dt$conducta)
dt[is.na(conducta), conducta := 'SIN CLASIFICACION']

# delito
dt[, delito := tolower(delito)]
dt[, droga := ifelse(delito %like% "droga|trafico|tráfico", 1, 0)]
dt[, robo := ifelse(delito %like% "robo|hurto|receptacion", 1, 0)]
dt[, personas := ifelse(delito %like% "lesiones|amenaza", 1, 0)]

table(dt$droga)
table(dt$robo)
table(dt$personas)

dt[droga == 0 & robo == 0 & personas == 0, delito]
dt[, otros := ifelse(droga == 0 & robo == 0 & personas == 0, 1, 0)]
table(dt$otros)

# categorización de indicadores
# escala
dt[, escala_grupos := cut(escala,
                             breaks = quantile(escala, probs = seq(0, 1, by = 1/4)),
                             labels = 1:4, include.lowest = TRUE, right = FALSE)]

table(dt$escala_grupos)

# tiempo en carcel
dt[, tiempo_grupos := cut(log(tiempo_carcel),
                           breaks = quantile(log(tiempo_carcel), probs = seq(0, 1, by = 1/3)),
                           labels = 1:3, include.lowest = TRUE, right = FALSE)]

table(dt$tiempo_grupos)

# edad
dt[, edad_grupos := cut(log(edad),
                         breaks = quantile(log(edad), probs = seq(0, 1, by = 1/4)),
                         labels = 1:4, include.lowest = TRUE, right = FALSE)]

table(dt$edad_grupos)


# blocking
dt[, block_id := paste0(edad_grupos, " ::: ", escala_grupos, " ::: ", tiempo_grupos)]
table(table(dt$block_id) > 1)

# set a seed for reproducability
set.seed(231)
# set.seed(234)

Z = block_ra(blocks = dt$block_id,
             conditions = c("control", "tratamiento"))

dt[, tratamiento := Z]
dt[, tratamiento_complete_ra := complete_ra(N = nrow(dt))]

# evaluar balance
tab1 = CreateTableOne(vars = c("edad", "escala",
                              "tiempo_carcel",
                              "droga", "robo", "personas", "otros",
                              "conducta", "compromiso_delictual"),
                     strata = "tratamiento_complete_ra",
                     data = dt,
                     test=FALSE)
print(tab1, smd = TRUE)

tab2 = CreateTableOne(vars = c("edad", "escala",
                              "tiempo_carcel",
                              "droga", "robo", "personas", "otros",
                              "conducta", "compromiso_delictual"),
                     strata = "tratamiento",
                     data = dt,
                     test=FALSE)
print(tab2, smd = TRUE)

print(addmargins(table(ExtractSmd(tab1) > 0.1)))
print(addmargins(table(ExtractSmd(tab2) > 0.1)))

# exportar datos
seleccion_dt = dt[, .(folio, muestra, unidad, edad, escala, tiempo_carcel, droga, robo, personas, otros,
                      tratamiento, conducta, compromiso_delictual)]

fwrite(seleccion_dt, "output/diseno_experimental_penitenciaria.csv")


