########################
# read data
# RR2 project
# author: sebastian daza
########################

library(haven)
library(readxl)
library(lubridate)
library(sdazar)
library(texreg)
library(brms)

# functions
reverse = function(x) (max(x, na.rm = TRUE) + 1) - x

# read cohort info
df = data.table(read_excel('data/3. BBDD Oficial Cohorte 1 v2.xlsx'))
setnames(df, 'FOLIO', 'id')

# define sample
setnames(df, 'ACCEDE A PARTICIPAR', 'sample')
setnames(df, '¿Caso válido en Cohorte 1?', 'valid')
df[, sample := ifelse(sample == 'SI', 1, 0)]
df[, valid := ifelse(valid == 'EXCLUIDO DE LA MUESTRA', 0, 1)]

table(df$sample, useNA='ifany')
table(df$valid, useNA='ifany')
df = df[valid == 1]

# define treatme
df[, treatment := GRUPO]
df[, treatment := ifelse(treatment ==  'CONTROL', 0, 1)]
table(df$treatment, df$GRUPO)

# duration treatment (weeks)
setnames(df, 'Duración tratamiento (semanas)', 'treatment_weeks')
df[, treatment_days := treatment_weeks * 7]

# survey
setnames(df, 'ENCUESTA DE CARACTERIZACIÓN', 'survey')
df[, survey := ifelse(survey == 'SI', 1, 0)]
prop.table(table(df$treatment, df$survey, useNA='ifany'), 1)

# define place
df[, place := .GRP, by=UNIDAD]
df[, room := .GRP, by=DEPENDENCIA]
table(df$place, useNA='ifany')
table(df$room, useNA='ifany')

# define pretest / posttest
# date pre post test
setnames(df, c("FECHA ESCALAS PRE", "FECHA ESCALA POST"),
         c('date_pretest', 'date_posttest'))

df[, date_pretest := as.Date(date_pretest)]
df[, date_posttest := as.Date(date_posttest)]

df[, days_pre_post := interval(date_pretest, date_posttest) %/% days(1)]

setnames(df, 'ESCALA DE HABILIDADES COGNITIVAS', 'pretest')
df[, pretest := ifelse(pretest == 'SI', 1, 0)]
table(df$pretest)

prop.table(table(df$treatment, df$pretest), 1)

setnames(df, 'HABILIDADES COGNITIVAS POST', 'posttest')
df[, posttest := ifelse(posttest == 'SI', 1, 0)]
table(df$posttest)

prop.table(table(df$treatment, df$posttest), 1)

table(df$pretest, df$posttest)
table(df$pretest == 0 & df$posttest == 0 & df$survey == 0)

setnames(df, 'INICIO SESIONES', 'sessions')
df[, sessions := ifelse(sessions == 'SI', 1, 0)]
table(df$sessions, useNA='ifany')

table(df[, .(treatment, sessions)], useNA='ifany')

df[, nsessions :=
apply(
    df[, lapply(.SD, function(x) ifelse(grepl('SI', x), 1, 0)), .SDcols=names(df) %like% 'Sesión [0-9]+'],
    1,
    sum)]

max(df$nsessions)
min(df$nsessions)

table(df[, .(treatment, nsessions)], useNA='ifany')
table(df[, .(sessions, nsessions)], useNA='ifany')

summary(df[treatment==1, nsessions])

# demographics
df[, age := EDAD]
table(df$age)

setnames(df, 'FECHA INICIO', 'start_sentence')
# df[, start_sentence := as.Date(start_sentence)]
# df[, year_sentence := year(start_sentence)]

setnames(df, 'COMPROMISO DELICTUAL', 'severity')
setnames(df, 'CONDUCTA', 'behavior')

df[, severity := tolower(severity)]
df[grepl('alto', severity), rseverity := 1]
df[grepl('medi', severity), rseverity := 2]
df[grepl('bajo', severity), rseverity := 3]
table(df$rseverity, useNA='ifany')


df[, behavior := tolower(behavior)]
table(df$behavior, useNA='ifany')
df[grepl('mala|pesima', behavior), rbehavior := 1]
df[grepl('regular|no registra', behavior), rbehavior := 2]
df[grepl('buena', behavior), rbehavior := 3]
table(df$rbehavior, useNA='ifany')

df[, crime := tolower(DELITO)]
df[grepl('tráfico|trafico|droga', crime), rcrime := 1]
df[grepl('hurto|robo|receptaci.n', crime), rcrime := 2]
df[is.na(rcrime), rcrime := 3]
table(df$rcrime, useNA='ifany')

df = df[, .(id, treatment, pretest, posttest, survey, rcrime, rbehavior,
       rseverity, age, nsessions, days_pre_post, place, room,
       treatment_days, sample, valid)]

# read scale definion
scales = fread('data/Excel constructos escalas.csv')
nvars = c('dimension', 'comparable', 'pretest', 'posttest', 'question',
          'direction')
setnames(scales, names(scales), nvars)
scales = scales[grepl('SI', comparable)]

scales[, pretest := paste0('p', pretest)]
scales[, posttest := paste0('p', posttest)]

scales[, recode_pretest := ifelse(grepl('Bueno a malo', direction), paste0(pretest, '_rec'), pretest)]
scales[, recode_posttest := ifelse(grepl('Bueno a malo', direction), paste0(posttest, '_rec'), posttest)]

reverse_pretest = scales[grepl('Bueno a malo', direction)]$pretest
reverse_posttest = scales[grepl('Bueno a malo', direction)]$posttest

# create scales by dimension
scales = scales[dimension != '']

pretest_dimensions = list()
for (i in unique(scales$dimension)) {
    pretest_dimensions[[i]] = scales[dimension == i, recode_pretest]
}

posttest_dimensions = list()
for (i in unique(scales$dimension)) {
    posttest_dimensions[[i]] = scales[dimension == i, recode_posttest]
}

# load outcome database
pretest = data.table(read_spss('data/190128 - Base EHC Anterior.sav'))
posttest = data.table(read_spss('data/190129 - Base EHC Posterior.sav'))

setnames(pretest, names(pretest), tolower(names(pretest)))
setnames(posttest, names(posttest), tolower(names(posttest)))

setnames(pretest, c('folio', 'fecha_aplicacion', 'nacionalidad', 'estado_civil',
                    'fecha_nacimiento', 'sexo', 'escolaridad', 'lee', 'escribe'),
                  c('id', 'pre_date', 'nationality', 'married', 'dob', 'gender',
                    'education', 'read', 'write'))
setnames(posttest, c('folio', 'fecha_aplicacion'), c('id', 'post_date'))

pretest = assmis(pretest, list(names(pretest)[names(pretest) %like% 'p[0-9]_[0-9]_[0-9]+']), list(9))
pretest[, paste0(reverse_pretest, '_rec') := lapply(.SD, reverse), .SDcols = reverse_pretest]

posttest = assmis(posttest, list(names(posttest)[names(posttest) %like% 'p[0-9]_[0-9]_[0-9]+']), list(9))
posttest[, paste0(reverse_posttest, '_rec') := lapply(.SD, reverse), .SDcols = reverse_posttest]

for (i in seq_along(pretest_dimensions)) {
    pretest[, paste0('pre_dim', i) := apply(.SD, 1, mean, na.rm = TRUE), .SDcols = pretest_dimensions[[i]]]
}

for (i in seq_along(posttest_dimensions)) {
    posttest[, paste0('post_dim', i) := apply(.SD, 1, mean, na.rm = TRUE), .SDcols = posttest_dimensions[[i]]]
}

pretest[, pre_total := apply(.SD, 1, mean, na.rm = TRUE), .SDcols =  paste0('pre_dim', 1:8)]
posttest[, post_total := apply(.SD, 1, mean, na.rm = TRUE), .SDcols =  paste0('post_dim', 1:8)]

pretest = pretest[, .(id, pre_date, nationality, married, dob, gender, education,
                      read, write, pre_dim1, pre_dim2, pre_dim3, pre_dim4, pre_dim5,
                      pre_dim6, pre_dim7, pre_dim8, pre_total)]

posttest = posttest[, .(id, post_date,post_dim1, post_dim2, post_dim3, post_dim4,
                        post_dim5, post_dim6, post_dim7,
                        post_dim8, post_total)]

setkey(pretest, id)
setkey(posttest, id)

# merge datasets
setkey(df, id)

df = pretest[df]
df = posttest[df]

df[, any_session := ifelse(nsessions > 0, 1, 0)]
df[, sessions_11 := ifelse(nsessions > 10, 1, 0)]

# random checks
ids = unique(df$id)
df[id == sample(ids, 1), .(id, pre_total, post_total)]

# save files
saveRDS(df, 'output/data_cohort_1.rds')
