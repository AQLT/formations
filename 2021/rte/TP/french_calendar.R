devtools::install_github("https://github.com/palatej/rjd3toolkit")
devtools::install_github("https://github.com/palatej/rjd3modelling")
library(rjd3modelling)

frenchCalendar <- calendar.new()
calendar.holiday(frenchCalendar, "NEWYEAR")
calendar.holiday(frenchCalendar, "EASTERMONDAY")
calendar.holiday(frenchCalendar, "MAYDAY") # 1er mai
calendar.fixedday(frenchCalendar, 5, 8)
calendar.holiday(frenchCalendar, "WHITMONDAY") # Lundi de Pentecôte
calendar.fixedday(frenchCalendar, 7, 14)
calendar.holiday(frenchCalendar, "ASSUMPTION")
calendar.holiday(frenchCalendar, "ALLSAINTDAY") # toussaint
calendar.holiday(frenchCalendar, "ARMISTICE")
# calendar.holiday(frenchCalendar, "THANKSGIVING")
holidays(frenchCalendar, start = "1990-01-01", length = 12)
leap_year <- function(start = 1990, end = 2030, frequency = 12){
  ly <- ts(0, start = start, end = end, frequency = 12)
  mois_feb <- cycle(ly) == 2
  annees <- trunc(round(time(ly), 3)) # arrondi car parfois des pbs avec fonction time
  # On utilise la définition exacte
  is_ly <- (annees %% 400 == 0) |
    ((annees %% 4 == 0) & (annees %% 100 != 0))
  ly[mois_feb] <- 28 - 28.2425
  ly[mois_feb & is_ly] <- 29 - 28.2425
  # on change si besoin la fréquence
  aggregate(ly, nfrequency = frequency)
}

library(RJDemetra)
groups <- c(1, 2, 3, 4, 5, 6, 0)
frequency <- 12
start <- c(1990, 1)
ly <- leap_year(frequency = 12)
td_reg <- htd(frenchCalendar, frequency = frequency, start = start, length = 12*40,
              groups = groups)
td_reg <- ts(td_reg, start = start, frequency = frequency)
wkd <- ts.union(td_reg, leap_year(frequency = 12))
colnames(wkd) <- c("lundi","mardi","mercredi","jeudi","vendredi",
                   "samedi","leap_year")
ipi_fr <- ipi_c_eu[, "FR"]

myspec1_sa <- x13_spec(spec = "RSA5c",
                       usrdef.varEnabled = TRUE,
                       usrdef.var = wkd,
                       usrdef.varType = "Calendar",
                       easter.enabled = FALSE)
mysa <- x13(ipi_fr, myspec1_sa)
# On retrouve d'ailleurs la partie regarima
# summary(mysa$regarima)
mysa$diagnostics

myspec1 <- regarima_spec_x13(spec = "RG5c",
                             usrdef.varEnabled = TRUE,
                             usrdef.var = wkd,
                             usrdef.varType = "Calendar",
                             easter.enabled = FALSE)
myreg1 <- regarima(ipi_fr, myspec1)
summary(myreg1)

td_reg_post_2003 <- td_reg_pre_2003  <- td_reg
window(td_reg_pre_2003, end = c(2002, 12)) <- 0
window(td_reg_post_2003, start = c(2003, 1)) <- 0
wkd2 <- ts.union(td_reg_pre_2003, td_reg_post_2003,
                 leap_year(frequency = 12))
colnames(wkd2) <- c(paste0(c("lundi","mardi","mercredi","jeudi","vendredi",
                            "samedi"),"_av2003"),
                    paste0(c("lundi","mardi","mercredi","jeudi","vendredi",
                            "samedi"),"_ap2003"),"leap_year")

myspec2_sa <- x13_spec(spec = "RSA5c",
                       usrdef.varEnabled = TRUE,
                       usrdef.var = wkd2,
                       usrdef.varType = "Calendar",
                       easter.enabled = FALSE)
mysa2 <- x13(ipi_fr, myspec2_sa)
mysa3 <- x13(ipi_fr)
coef(mysa2$regarima)
jxsa <- jx13(get_ts(mysa), x13_spec(mysa))
sa <- mysa$final$series[,"sa"]
i <- mysa$final$series[,"i"]
tail(mysa$diagnostics$residuals_test[,1:2],2)

get_indicators(jxsa,"diagnostics.td-sa-all")
get_indicators(jxsa,"diagnostics.td-sa-last")
get_indicators(jxsa,"diagnostics.td-i-last")
rjd3sa::td.f(log(sa), nyears = 8)$pv
rjd3sa::td.f(log(i), nyears = 8)$pv
rjd3sa::td.f(sa2, nyears = 8)
rjd3sa::td.f(i2, nyears = 8)

y_p_c <- data_rte[,"6_Papier Carton"]

myspec1_sa <- x13_spec(spec = "RSA5c",
                       usrdef.varEnabled = TRUE,
                       usrdef.var = wkd,
                       usrdef.varType = "Calendar",
                       easter.enabled = FALSE)
mysa <- x13(y_p_c,myspec1_sa)
summary(mysa$regarima)
mysa <- x13(y_p_c, x13_spec(mysa,
                            tradingdays.test = "None"))
summary(mysa$regarima)

td_reg <- htd(frenchCalendar, frequency = frequency, start = start, length = 12*35,
              groups = c(1,1,1,1,1,2,0))
td_reg <- ts(td_reg, start = start, frequency = frequency)
wkd2 <- ts.union(td_reg, leap_year(frequency = 12))
colnames(wkd2) <- c("lundi_a_vendredi", "samedi","leap_year")
td_reg <- htd(frenchCalendar, frequency = frequency, start = start, length = 12*35,
              groups = c(0,0,0,0,0,1,0))
td_reg <- ts(td_reg, start = start, frequency = frequency)
wkd3 <- ts.union(td_reg, leap_year(frequency = 12))
colnames(wkd3) <- c("samedi","leap_year")
car::linearHypothesis(mysa,
                      c("lundi=mercredi","mardi=mercredi","mercredi=jeudi","jeudi=vendredi",
                        "vendredi=0"), test = "F")

mysa <- x13(y_p_c, x13_spec(mysa,
                            usrdef.var = wkd2,
                            usrdef.varType = "Calendar"))
mysa$diagnostics
summary(mysa$regarima)
mysa <- x13(y_p_c, myspec1_sa)
mysa <- x13(y_p_c, x13_spec(mysa, tradingdays.test = "None"))
summary(mysa$regarima)
myspec1_sa$regarima$regression$trading.days$test
mysa$diagnostics

mod <- jx13(y_p_c, x13_spec(myspec1_sa,tradingdays.test = "None"))
mod$regarima
get_dictionary(mod)
get_indicators(mod,"preprocessing.arima.parameters")[[1]]
get_indicators(mod,"preprocessing.model.pcovar")[[1]]
arima.se  <- sqrt(diag(result(jrobj,"model.pcovar")))
# ARIMA coefficients

car::linearHypothesis(mysa,
                      c("lundi=mercredi","mardi=mercredi","mercredi=jeudi","jeudi=vendredi"), test = "F")
car::linearHypothesis(mysa,
                      c("lundi=mardi","mardi=mercredi","mercredi=jeudi","jeudi=vendredi"), test = "F")

car::linearHypothesis(mysa2,
                 paste0(c("lundi","mardi","mercredi","jeudi","vendredi",
                          "samedi"),"_av2003"),
                 coef(mysa2$regarima)[paste0(c("lundi","mardi","mercredi","jeudi","vendredi",
                                               "samedi"),"_ap2003")], test = "F")
linearHypothesis(myreg1,
                 c("lundi","mardi","mercredi","jeudi","vendredi"),
                 c(0, 0, 0, 0, 0), test = "F")

lg_mean <- longTermMean(frenchCalendar, frequency,
                        groups = groups)
lg_mean
lg_mean/31*7
5-4.838710
lg_mean
6/31
M<-td(12, c(1980,1), 12*30, c(1,1,1,1,2,3,0), contrasts = FALSE)

groups <- c(1,1,1,1,1,0,0)
groups <- c(1,2,3,4,5,6,0)
frequency <- 12
H <- htd(frenchCalendar, frequency, c(1980,1), 12*30, groups,
         contrasts = TRUE)
H <- ts(H, start = 1980, frequency = frequency)
round(apply(H,2,mean),4)
plot(H)
seas_mean = sapply(1:12,function(s){
  apply(H[cycle(H) == s,],2, mean)
})
seas_mean <- apply(seas_mean,1, sum)
lg_mean <- longTermMean(frenchCalendar, frequency,
                        groups = groups)
spectrum(H[,1],method = "ar")
# Première colonne groupe 0, deuxième colonne groupe 1, etc.
lg_mean
remove_lgmean <- function(calendar, frequency)
for (mois in seq_len(frequency)){
  if(is.mts(H)){
    for(groupe in seq_len(ncol(H))){
      H[cycle(H)==mois,groupe] <- H[cycle(H)==mois,groupe] -
        (lg_mean[mois,groupe+1] - lg_mean[mois,1])
    }
  }else{
    H[cycle(H)==mois] <- H[cycle(H)==mois] -
      (lg_mean[mois,2] - lg_mean[mois,1])
  }
}
for (mois in seq_len(frequency)){
  for(groupe in seq_len(ncol(H))){
    H[cycle(H)==mois,groupe] <- H[cycle(H)==mois,groupe] - lg_mean[mois,groupe] +
      mean(lg_mean[mois,])
  }
}
H

MC<-td(4, c(1980,1), 120, c(1,1,1,1,1,2,0), contrasts = T)
HC<-htd(frenchCalendar, 12, c(1980,1), 140, c(1,1,1,1,1,0,0), contrasts = F)
head(HC)[,2]-5/2*head(HC)[,1]
C4<-longTermMean(frenchCalendar, 4)

C12bis<-longTermMean(frenchCalendar, 12, c(1,1,1,1,1,2,0))
C4bis<-longTermMean(frenchCalendar, 4, c(1,1,1,1,1,2,0))

print(C12)
print( C12bis)
pd = jd3.PrespecifiedHoliday$new()
dput(names(jd3.CalendarEvent))

AQLTools::ctrl_c(matrix(c("UNSPECIFIED", "NEWYEAR", "SHROVEMONDAY",
  "SHROVETUESDAY", "ASHWEDNESDAY", "EASTER",
  "JULIANEASTER", "MAUNDYTHURSDAY", "GOODFRIDAY",
  "EASTERMONDAY", "ASCENSION", "PENTECOST",
  "CORPUSCHRISTI", "WHITMONDAY", "MAYDAY",
  "ASSUMPTION", "LABORDAY", "HALLOWEEN",
  "ALLSAINTDAY", "ARMISTICE", "THANKSGIVING",
  "CHRISTMAS"),ncol = 1))

pd <- jd3.PrespecifiedHoliday$new()
t <- rjd3modelling:::enum_of(jd3.CalendarEvent,"THANKSGIVING" , "HOLIDAY")

for( f in c("UNSPECIFIED", "NEWYEAR", "SHROVEMONDAY",
            "SHROVETUESDAY", "ASHWEDNESDAY", "EASTER",
            "JULIANEASTER", "MAUNDYTHURSDAY", "GOODFRIDAY",
            "EASTERMONDAY", "ASCENSION", "PENTECOST",
            "CORPUSCHRISTI", "WHITMONDAY", "MAYDAY",
            "ASSUMPTION", "LABORDAY", "HALLOWEEN",
            "ALLSAINTDAY", "ARMISTICE", "THANKSGIVING",
            "CHRISTMAS")){
  frenchCalendar<-calendar.new()
  calendar.holiday(frenchCalendar, f)
  tryCatch({
    longTermMean(frenchCalendar, frequency,
                 groups = groups)
  },
  error = function(e) print(f))

}

calendar.holiday(frenchCalendar, "EASTERMONDAY")
calendar.holiday(frenchCalendar, "MAYDAY") # 1er mai
calendar.fixedday(frenchCalendar, 5, 8)
calendar.holiday(frenchCalendar, "WHITMONDAY") # Lundi de Pentecôte
calendar.fixedday(frenchCalendar, 7, 14)
calendar.holiday(frenchCalendar, "ASSUMPTION")

calendar.holiday(frenchCalendar, "ALLSAINTDAY") # toussaint
calendar.holiday(frenchCalendar, "ARMISTICE")
# calendar.holiday(frenchCalendar, "THANKSGIVING")


lg_mean <- longTermMean(frenchCalendar, frequency,
                        groups = groups)

frenchCalendar <- calendar.new()
calendar.holiday(frenchCalendar, "NEWYEAR")
calendar.holiday(frenchCalendar, "EASTERMONDAY") # Lundi de Pâques
calendar.holiday(frenchCalendar, "MAYDAY") # 1er mai
calendar.fixedday(frenchCalendar, 5, 8)
calendar.holiday(frenchCalendar, "WHITMONDAY") # Lundi de Pentecôte
calendar.fixedday(frenchCalendar, 7, 14)
calendar.holiday(frenchCalendar, "ASSUMPTION") # Assomption
calendar.holiday(frenchCalendar, "ALLSAINTDAY") # Toussaint
calendar.holiday(frenchCalendar, "ARMISTICE")
groups <- c(1, 1, 1, 1, 1, 0, 0)
frequency <- 12
start <- c(2000,1)
wkd <- htd(frenchCalendar, frequency = frequency, start = start, length = 12*2,
           groups = groups,contrasts = T)
wkd <- ts(wkd, start = start, frequency = frequency)
wkd_def <- td(frequency = frequency, start = start, length = 12*2,
              groups = groups)
wkd_def <- ts(wkd_def, start = start, frequency = frequency)
data <- ts.union(wkd, wkd_def)
data
plot(data, col = c("orange","black"),plot.type = "single")

groups <- c(1, 1, 1, 1, 1, 0, 0)
frequency <- 12
start <- c(2013,1)
wkd <- htd(frenchCalendar, frequency = frequency, start = c(2013,1), length = 12,
           groups = groups, contrasts = T)
wkd <- ts(wkd, start = start, frequency = frequency)
wkd2 <- htd(frenchCalendar, frequency = frequency, start = c(2013,1), length = 12,
           groups = groups, contrasts = F)
wkd2 <- ts(wkd2, start = start, frequency = frequency)
wkd3 <- td(frequency = frequency, start = c(2013,1), length = 12,
           groups = groups, contrasts = T)
wkd3 <- ts(wkd3, start = start, frequency = frequency)
wkd4 <- td(frequency = frequency, start = c(2013,1), length = 12,
           groups = groups, contrasts = F)
wkd4 <- ts(wkd4, start = start, frequency = frequency)
wkd2[,2] - 5/2 * wkd2[,1]
wkd
wkd3
wkd4[,2] - 5/2 * wkd4[,1]

lg_mean <- longTermMean(frenchCalendar, frequency,
                        groups = groups)
lg_mean
nb_jours <- apply(lg_mean,1, sum)
mean_effect = lg_mean / nb_jours * 365.25 / frequency
seas_effect = lg_mean - mean_effect
for (mois in seq_len(nrow(lg_mean))){
  wkd[cycle(wkd)==mois] <- wkd[cycle(wkd)==mois]  -
    (lg_mean[mois,2] - 5/2 * lg_mean[mois,1])
}
lg_mean[,2]- 5/2 * lg_mean[,1]
wkd <- htd(frenchCalendar, frequency = frequency, start = c(2013,1), length = 12*10,
           groups = groups, contrasts = T)
wkd <- ts(wkd, start = start, frequency = frequency)
for (mois in seq_len(nrow(lg_mean))){
  for(groupe in 1:ncol(wkd)){
    wkd[cycle(wkd)==mois,groupe] <- wkd[cycle(wkd)==mois,groupe]  -
      (lg_mean[mois,2] - 5/2*lg_mean[mois,1])
  }

}
(wkd[,2]-wkd[,1]*2)/2
wkd2
wkd2 <- wkd
spectrum(wkd2)
for (mois in seq_len(nrow(lg_mean))){
  wkd[cycle(wkd)==mois] <- wkd[cycle(wkd)==mois]  -
    (lg_mean[mois,2] - lg_mean[mois,1])
}
seas_effect
frenchCalendar$prespecified_holidays[[1]]$validity
.jcall("demetra/calendar/r/Calendars", "Ldemetra/math/matrices/MatrixType;",
       "longTermMean", jcal, as.integer(frequency), as.integer(groups))

library(XLConnect)
library(kableExtra)
holidays <- readWorksheetFromFile("TP/holidaysJD.xlsx", sheet = 1)
holidays <- holidays[!holidays[,1]%in%c("THANKSGIVING", "LABORDAY", "JULIANEASTER"),]
kbl(holidays,booktabs = TRUE,caption = "Jours pré-spécifiés",
    row.names = FALSE)%>%
  kable_paper("hover", full_width = F)

groups = c(1, 2, 3, 4, 5, 6, 0)
con <- td(4,c(2000,1),12,groups = groups,contrast = TRUE)
len <- td(4,c(2000,1),12,groups = groups,contrast = FALSE)
con - apply(len[,-1],2, function(x) x-len[,1])

groups = c(1, 1, 1, 1, 1, 0, 0)
con <- td(4,c(2000,1),12,groups = groups,contrast = TRUE)
len <- td(4,c(2000,1),12,groups = groups,contrast = FALSE)
con - (len[,2] - 5/2*len[,1])

groups = c(1, 1, 1, 1, 2, 0, 0)
con <- td(4,c(2000,1),12,groups = groups,contrast = TRUE)
len <- td(4,c(2000,1),12,groups = groups,contrast = FALSE)
con[,1] - (len[,2] - 4/2*len[,1])

groups = c(1, 1, 1, 1, 2, 0, 0)
con2 <- htd(frenchCalendar,4,c(2000,1),12,groups = groups,contrast = TRUE)
len2 <- htd(frenchCalendar, 4,c(2000,1),12,groups = groups,contrast = FALSE)
con[,1] - (len[,2] - 4/2*len[,1])

holidays(frenchCalendar,"2000-01-01",4,12)
