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

# load outcome database

cov = data.table(read_excel('data/BBDD habilidades cognitivas y caracterización.xlsx'))
setnames(cov, 'FOLIO', 'id')
setnames(cov, 'PROMEDIO_ESCALAS_PRE', 'pre_score_excel')
setnames(cov, 'PROMEDIO_ESCALAS_POST', 'post_score_excel')


# compute scales
cov = assmis(cov, list(names(cov)[names(cov) %like% 'PRE[0-9]_|POST[0-9]_']), list(0))

cov[, missing_pre := apply(.SD, 1, function(x) sum(is.na(x))), .SDcols = names(cov) %like% 'PRE[0-9]_']
cov[, missing_post := apply(.SD, 1, function(x) sum(is.na(x))), .SDcols = names(cov) %like% 'POST[0-9]_']

table(cov[, .(missing_pre, missing_post)])
# cov = cov[missing_pre < 5 &  missing_post < 5]

length(unique(cov$id))

# variables of scales

pre_items = list()

pre_items[['pre_autocontrol']] = c("PRE4_1_1_rec","PRE4_1_2_rec","PRE4_1_3_rec",
                                   "PRE4_1_4_rec","PRE4_1_5_rec", "PRE4_1_6_rec","PRE4_1_7_rec")

pre_items[['pre_aggression']] = c("PRE4_1_9_rec", "PRE4_1_10_rec", "PRE4_1_11_rec",
                    "PRE4_1_12_rec", "PRE4_1_13_rec", "PRE4_1_14_rec")

pre_items[['pre_conflict']] = c("PRE4_2_1", "PRE4_2_2", "PRE4_2_3", "PRE4_2_4",
                 "PRE4_2_5", "PRE4_2_6", "PRE4_2_7", "PRE4_2_8",
                 "PRE4_2_9", "PRE4_2_10", "PRE4_2_11", "PRE4_2_12",
                 "PRE4_2_13", "PRE4_2_14")

pre_items[['pre_empathy']] = c("PRE4_3_1", "PRE4_3_2", "PRE4_3_3", "PRE4_3_4",
                "PRE4_3_5", "PRE4_3_6", "PRE4_3_7", "PRE4_3_8",
                "PRE4_3_9")

pre_items[['pre_kindness']] = c("PRE4_3_10", "PRE4_3_11", "PRE4_3_12", "PRE4_3_13",
                 "PRE4_3_14", "PRE4_3_15")

pre_items[['pre_law']] = c("PRE4_4_1_rec", "PRE4_4_2_rec", "PRE4_4_3_rec",
            "PRE4_4_4_rec", "PRE4_4_5_rec", "PRE4_4_6",
            "PRE4_4_7_rec", "PRE4_4_8_rec", "PRE4_4_9_rec",
            "PRE4_4_10_rec")

pre_items[['pre_antisocial_1']] = c("PRE4_4_11_rec", "PRE4_4_12_rec", "PRE4_4_13_rec",
                     "PRE4_4_14_rec", "PRE4_4_16_rec", "PRE4_4_17_rec",
                     "PRE4_4_18_rec", "PRE4_4_19_rec", "PRE4_4_20_rec",
                     "PRE4_4_21_rec")

pre_items[['pre_antisocial_2']] = c("PRE4_4_22_rec", "PRE4_4_23_rec", "PRE4_4_24_rec",
                     "PRE4_4_25_rec", "PRE4_4_26_rec", "PRE4_4_27_rec",
                     "PRE4_4_28_rec", "PRE4_4_29_rec")


post_items = list()

post_items[['post_autocontrol']] = c("POST4_1_1_rec","POST4_1_2_rec","POST4_1_3_rec",
                                   "POST4_1_4_rec","POST4_1_5_rec", "POST4_1_6_rec","POST4_1_7_rec")


post_items[['post_aggression']] = c("POST4_1_9_rec", "POST4_1_10_rec", "POST4_1_11_rec",
                    "POST4_1_12_rec", "POST4_1_13_rec", "POST4_1_14_rec")

post_items[['post_conflict']] = c("POST4_2_1", "POST4_2_2", "POST4_2_3", "POST4_2_4",
                 "POST4_2_5", "POST4_2_6", "POST4_2_7", "POST4_2_8",
                 "POST4_2_9", "POST4_2_10", "POST4_2_11", "POST4_2_12",
                 "POST4_2_13", "POST4_2_14")

post_items[['post_empathy']] = c("POST4_3_1", "POST4_3_2", "POST4_3_3", "POST4_3_4",
                "POST4_3_5", "POST4_3_6", "POST4_3_7", "POST4_3_8",
                "POST4_3_9")

post_items[['post_kindness']] = c("POST4_3_10", "POST4_3_11", "POST4_3_12", "POST4_3_13",
                 "POST4_3_14", "POST4_3_15")

post_items[['post_law']] = c("POST4_4_1_rec", "POST4_4_2_rec", "POST4_4_3_rec",
            "POST4_4_4_rec", "POST4_4_5_rec", "POST4_4_6",
            "POST4_4_7_rec", "POST4_4_8_rec", "POST4_4_9_rec",
            "POST4_4_10_rec")

post_items[['post_antisocial_1']] = c("POST4_4_11_rec", "POST4_4_12_rec", "POST4_4_13_rec",
                     "POST4_4_14_rec", "POST4_4_16_rec", "POST4_4_17_rec",
                     "POST4_4_18_rec", "POST4_4_19_rec", "POST4_4_20_rec",
                     "POST4_4_21_rec")

post_items[['post_antisocial_2']] = c("POST4_4_22_rec", "POST4_4_23_rec", "POST4_4_24_rec",
                     "POST4_4_25_rec", "POST4_4_26_rec", "POST4_4_27_rec",
                     "POST4_4_28_rec", "POST4_4_29_rec")

for (i in 1:length(pre_items)) {
cov[, names(pre_items)[i] := apply(.SD, 1, mean, na.rm = TRUE), .SDcols = pre_items[[i]]]
cov[, names(post_items)[i] := apply(.SD, 1, mean, na.rm = TRUE), .SDcols = post_items[[i]]]
}

cov[, pre_score := apply(.SD, 1, mean, na.rm = TRUE), .SDcols = names(pre_items)]
cov[, post_score := apply(.SD, 1, mean, na.rm = TRUE), .SDcols = names(post_items)]


prepost = cov[!is.na(id), .(id, pre_score, pre_score_excel,  post_score, post_score_excel)]
prepost[, pre_diff := pre_score - pre_score_excel]
prepost[, post_diff := post_score - post_score_excel]


table(prepost$pre_diff)
table(prepost$post_diff)
df = merge(df, prepost, on = 'id')
df[, pre_post := ifelse(!is.na(post_score) & !is.na(pre_score), 1, 0)]

df[, any_session := ifelse(nsessions > 0, 1, 0)]
df[, sessions_11 := ifelse(nsessions > 10, 1, 0)]

# random checks
ids = unique(df$id)

df[id == sample(ids, 1), .(id, pre_score, post_score)]

# save files
saveRDS(df, 'output/data_cohort_1.rds')
