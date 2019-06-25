# read sample frame

# library(haven)
library(readxl)
library(data.table)
library(lubridate)
table = function (...) base::table(..., useNA = 'ifany')
library(ggplot2)

# end of treatment
end_treatment = ymd('2019-10-15')

# colina
colina = data.table(read_xlsx("data/marco_cohort_2.xlsx", .name_repair = tolower,
                    sheet = 1))

setnames(colina, "fecha de egreso potencial (sino tiene multa)", "exp_release_date")

colina[, exp_release_date := ymd(exp_release_date)]
colina[, diff_months := interval(end_treatment, exp_release_date) / months(1)]

table(colina$exp_release_date)
colina[, .(exp_release_date, diff_months)]

colina[, group := 'colina']

# valparaíso
valpo = data.table(read_xlsx("data/marco_cohort_2.xlsx", .name_repair = tolower,
                   sheet = 2))
setnames(valpo, "fecha de egreso potencial (sino tiene multa)", "exp_release_date")

nrow(valpo)

valpo[, exp_release_date := ymd(exp_release_date)]
valpo[, diff_months := interval(end_treatment, exp_release_date) / months(1)]

table(valpo$exp_release_date)
valpo[, .(exp_release_date, diff_months)]

valpo[, group := 'valparaiso']

# penitenciaría
pen = data.table(read_xlsx("data/marco_cohort_2.xlsx", .name_repair = tolower,
                 sheet = 3))

setnames(pen, "fecha de egreso potencial (sino tiene multa)", "exp_release_date")

nrow(pen)

pen[, exp_release_date := ymd(exp_release_date)]
pen[, diff_months := interval(end_treatment, exp_release_date) / months(1)]

table(pen$exp_release_date)
pen[, .(exp_release_date, diff_months)]

summary(pen$diff_months)

pen[, group := 'penitenciaria']

# rename unidad column
setnames(pen, c('destino_unidad', 'edad'), c('unit', 'age'))
setnames(valpo, c('destino_unidad', 'edad'), c('unit', 'age'))
setnames(colina, c('destino_unidad', 'edad'), c('unit', 'age'))

# create dataset with diff months
vars = c('group', 'diff_months', 'age')
dat = rbindlist(list(pen[, ..vars],
               valpo[, ..vars],
               colina[, ..vars]))

ggplot(dat, aes(x=diff_months, group=as.factor(group),
                color=as.factor(group))) +
    geom_density() +
    labs(x = 'months', y='density', title='months after intervention population')

ggplot(dat, aes(x=age, group=as.factor(group),
                color=as.factor(group))) +
    geom_density() +
    labs(x = 'age', y='density', title='age population')


# sample penitenciaria
pen[, N := .N, unit]
pen[, sample_size := round(N * .70)]

pen[, sample(.N, sample_size), unit]

set.seed(06242019)
ss = pen[,.SD[sample(.N, sample_size)], unit]

vars = c('rut', 'age', 'group', 'unit', 'diff_months', 'exp_release_date')

fss = rbindlist(list(valpo[, ..vars],
               colina[, ..vars],
               ss[, ..vars]), use.names = TRUE, fill = TRUE)

ggplot(fss, aes(x=diff_months, group=as.factor(group),
                color=as.factor(group))) +
    geom_density() +
    labs(x = 'months', y='density', title='months after intervention sample')

ggplot(fss, aes(x=age, group=as.factor(group),
                color=as.factor(group))) +
    geom_density() +
    labs(x = 'age', y='density', title='age sample')


# some checks
table(fss[, .N, rut]$N)

fss[, .N, .(group, unit)]
pen[, .N, unit]

# export sample
fwrite(fss, row.names = FALSE, file = 'output/sample_cohort_2.csv')

