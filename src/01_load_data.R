########################
# read data
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

dim(df)

# define treatment
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
df[, start_sentence := as.Date(start_sentence)]
df[, year_sentence := year(start_sentence)]

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

# selection model
m1 = glm(pretest ~ treatment + age + as.factor(rseverity) +
    as.factor(rcrime) + as.factor(rbehavior), data = df, family=binomial())

m1 = glm(pretest ~ treatment, data = df, family=binomial())
summary(m1)

summary(m1)

m2 = glm(posttest ~ treatment + age + as.factor(rseverity) +
    as.factor(rcrime) + as.factor(rbehavior), data = df, family=binomial())


m2 = glm(posttest ~ treatment, data = df, family=binomial())


summary(m2)

# load outcome database
cov = data.table(read_excel('data/BBDD habilidades cognitivas y caracterización.xlsx'))
setnames(cov, 'FOLIO', 'id')
setnames(cov, 'PROMEDIO_ESCALAS_PRE', 'pre_score')
setnames(cov, 'PROMEDIO_ESCALAS_POST', 'post_score')

# compute scales
# cov = assmis(cov, list(names(cov)[names(cov) %like% 'PRE[0-9]_|POST[0-9]_']), list(0))

# remove_from_post = c('POST4_2_10_2_rec', 'POST4_4_15', 'POST4_4_30')\
# names(cov)[names(cov) %like% 'PRE[0-9]_|POST[0-9]_']
#

# cov[, pre_autocontrol := apply(.SD, 1, sum, na.rm=TRUE), .SDcols = names(cov) %like% 'PRE4_1_[1-7]_rec']
# cov[, pre_autocontrol := apply(.SD, 1, sum, na.rm=TRUE), .SDcols = names(cov) %like% 'PRE4_1_[9]_rec']
# cov[, post_score := apply(.SD, 1, sum, na.rm=TRUE), .SDcols = names(cov) %like% 'POST[0-9]_']


prepost = cov[, .(id, pre_score, post_score)]
df = merge(df, prepost, on = 'id')
df[, pre_post := ifelse(!is.na(post_score) & !is.na(pre_score), 1, 0)]

df[, any_session := ifelse(nsessions > 0, 1, 0)]
df[, sessions_11 := ifelse(nsessions > 10, 1, 0)]

m1 = lm(post_score ~ treatment + pre_score, data = df)
m2 = lm(post_score ~ any_session + pre_score, data = df)
m3 = lm(post_score ~ sessions_11 + pre_score, data = df)
m4 = lm(post_score ~ treatment * nsessions + pre_score, data = df)

screenreg(list(m1, m2, m3, m4))

o1 = lm(post_score ~ nsessions + pre_score, data = df)
screenreg(o1)

df[, diff := post_score - pre_score]
df[, mean(diff, na.rm=TRUE), treatment]

df[pre_post == 1, mean(pre_score, na.rm = TRUE), treatment]
df[pre_post == 1, mean(post_score, na.rm = TRUE), treatment]

df[, .(sum(is.na(pre_score)), .N), pretest]
sum(!is.na(df$post_score))

# instrumenta

