library(rjd3modelling)
library(RJDemetra)
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
frequency <- 12
start <- c(1990,1)
end = c(2030, 1)
length = (end[1] - start[1]) * 12 + end[2] - start[2]

ly <- leap_year(frequency = frequency, start = start,
                end = end)
reg6 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
           groups = c(1, 2, 3, 4, 5, 6, 0))
reg5 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 2, 3, 4, 5, 0, 0))
reg3 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 2, 2, 2, 2, 0, 0))
reg2 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 1, 1, 1, 1, 2, 0))
reg1 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 1, 1, 1, 1, 0, 0))


regresseurs_JO <- ts(cbind(reg1, reg2, reg3, reg5, reg6),
                              start = start, frequency = frequency)
regresseurs_JO <- ts.union(regresseurs_JO,
                           ly)
colnames(regresseurs_JO) <- c("REG1_semaine",
                              sprintf("REG2_%s", c("lundi_a_vendredi", "samedi")),
                              sprintf("REG3_%s", c("lundi", "mardi_a_vendredi")),
                              sprintf("REG5_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi")),
                              sprintf("REG6_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi")),
                              "leap_year")


complete_variables <- function(liste_var, workspace){
  if(!is.mts(liste_var))
    stop("liste_var doit être de type mts")
  context_dictionary <- .jcall(workspace,"Lec/tstoolkit/algorithm/ProcessingContext;", "getContext")
  ts_variable_managers <- context_dictionary$getTsVariableManagers()
  ts_variables <- .jnew("ec/tstoolkit/timeseries/regression/TsVariables")
  jd_r_variables <- ts_variable_managers$get("r")
  if (is.null(jd_r_variables)) {
    ts_variable_managers$set("r",
                             .jnew("ec/tstoolkit/timeseries/regression/TsVariables"))
    jd_r_variables <- ts_variable_managers$get("r")
  }
  jd_var_names <- jd_r_variables$getNames()

  model_var_names <- colnames(liste_var)

  for (i in seq_along(model_var_names)) {
    name <- model_var_names[i]
    dictionary_var <- jd_r_variables$get(name)
    tsvar <- .jnew("ec/tstoolkit/timeseries/regression/TsVariable",
                   name, RJDemetra:::ts_r2jd(liste_var[, i]))
    if (is.null(dictionary_var)) {
      jd_r_variables$set(name, tsvar)
    } else {
      warning(sprintf("La variable %s existe déjà", name))
    }
  }
}
wk <- load_workspace("rte.xml")
complete_variables(regresseurs_JO, wk)
save_workspace(wk,"rte.xml")


leap_year_preajust <- function(start = 1990, end = 2030, frequency = 12){
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
# Remarque : dans cette spécification on n'ajoute pas d'effet graduel de Pâques
# Est-ce que cela aurait un sens de le faire ? À vous de me le dire !
y_p_c <- data_rte[,"6_Papier Carton"]
mysa <- x13(y_p_c, myspec1_sa)
# Il y a un effet JO résiduel !
mysa$diagnostics
# Mais lorsque l'on  regarde le modèle regarima, il n'y a pas de régresseur JO !
summary(mysa$regarima)

# Première étape : ne pas faire de test pour supprimer les effets J0
mysa <- x13(y_p_c, x13_spec(myspec1_sa, tradingdays.test = "None"),
            userdefined = c("diagnostics.td-sa-last", "diagnostics.td-i-last"))
# C'est corrigé ! ouf !
mysa$diagnostics
mysa$user_defined$`diagnostics.td-sa-last`
mysa$user_defined$`diagnostics.td-i-last`
# Mais on peut affiner le modèle en le simplifiant un peu :
# Tout d'abord on va rassembler samedi et dimanche
summary(mysa$regarima)

# A priori on peut rassembler les jours de la semaine
car::linearHypothesis(mysa,
                      c("lundi=mercredi","mardi=mercredi","mercredi=jeudi","jeudi=vendredi"), test = "F")
wkd2 <- regresseurs_JO[,c("REG2_lundi_a_vendredi", "REG2_samedi",
                          "leap_year")]
# Pour simplifier l'output, on enlève le "REG2_"
colnames(wkd2) <- gsub("REG2_", "", colnames(wkd2))

mysa <- x13(y_p_c, x13_spec(mysa,
                            usrdef.var = wkd2,
                            usrdef.varType = "Calendar"))
# Mais on ne corrige pas bien de l'effet JO dans ce cas ! il vaut mieux garder l'ancien modèle
mysa$diagnostics
library(rjd3modelling)
library(RJDemetra)
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
frequency <- 12
start <- c(1990,1)
end = c(2030, 1)
length = (end[1] - start[1]) * 12 + end[2] - start[2]

ly <- leap_year(frequency = frequency, start = start,
                end = end)
reg6 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
           groups = c(1, 2, 3, 4, 5, 6, 0))
reg5 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 2, 3, 4, 5, 0, 0))
reg3 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 2, 2, 2, 2, 0, 0))
reg2 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 1, 1, 1, 1, 2, 0))
reg1 <- htd(frenchCalendar, frequency = frequency, start = start, length = length,
            groups = c(1, 1, 1, 1, 1, 0, 0))


regresseurs_JO <- ts(cbind(reg1, reg2, reg3, reg5, reg6),
                              start = start, frequency = frequency)
regresseurs_JO <- ts.union(regresseurs_JO,
                           ly)
colnames(regresseurs_JO) <- c("REG1_semaine",
                              sprintf("REG2_%s", c("lundi_a_vendredi", "samedi")),
                              sprintf("REG3_%s", c("lundi", "mardi_a_vendredi")),
                              sprintf("REG5_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi")),
                              sprintf("REG6_%s", c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi")),
                              "leap_year")


complete_variables <- function(liste_var, workspace){
  if(!is.mts(liste_var))
    stop("liste_var doit être de type mts")
  context_dictionary <- .jcall(workspace,"Lec/tstoolkit/algorithm/ProcessingContext;", "getContext")
  ts_variable_managers <- context_dictionary$getTsVariableManagers()
  ts_variables <- .jnew("ec/tstoolkit/timeseries/regression/TsVariables")
  jd_r_variables <- ts_variable_managers$get("r")
  if (is.null(jd_r_variables)) {
    ts_variable_managers$set("r",
                             .jnew("ec/tstoolkit/timeseries/regression/TsVariables"))
    jd_r_variables <- ts_variable_managers$get("r")
  }
  jd_var_names <- jd_r_variables$getNames()

  model_var_names <- colnames(liste_var)

  for (i in seq_along(model_var_names)) {
    name <- model_var_names[i]
    dictionary_var <- jd_r_variables$get(name)
    tsvar <- .jnew("ec/tstoolkit/timeseries/regression/TsVariable",
                   name, RJDemetra:::ts_r2jd(liste_var[, i]))
    if (is.null(dictionary_var)) {
      jd_r_variables$set(name, tsvar)
    } else {
      warning(sprintf("La variable %s existe déjà", name))
    }
  }
}
wk <- load_workspace("rte.xml")
complete_variables(regresseurs_JO, wk)
save_workspace(wk,"rte.xml")


leap_year_preajust <- function(start = 1990, end = 2030, frequency = 12){
  ly <- ts(1, start = start, end = end, frequency = 12)
  mois_feb <- cycle(ly) == 2
  annees <- trunc(round(time(ly), 3)) # arrondi car parfois des pbs avec fonction time
  # On utilise la définition exacte
  is_ly <- (annees %% 400 == 0) |
    ((annees %% 4 == 0) & (annees %% 100 != 0))
  ly[mois_feb] <- 28.25/28
  ly[mois_feb & is_ly] <- 28.25/29
  # on change si besoin la fréquence
  aggregate(ly, nfrequency = frequency)
}
# Remarque : dans cette spécification on n'ajoute pas d'effet graduel de Pâques
# Est-ce que cela aurait un sens de le faire ? À vous de me le dire !
y_p_c <- data_rte[,"6_Papier Carton"]
y_p_c2 <- y_p_c *leap_year_preajust()
mysa <- x13(y_p_c, myspec1_sa)
# Il y a un effet JO résiduel !
mysa$diagnostics
# Mais lorsque l'on  regarde le modèle regarima, il n'y a pas de régresseur JO !
summary(mysa$regarima)

# Première étape : ne pas faire de test pour supprimer les effets J0
mysa <- x13(y_p_c, x13_spec(myspec1_sa, tradingdays.test = "None"),
            userdefined = c("diagnostics.td-sa-last", "diagnostics.td-i-last"))
# L'effet JO est corrigé au niveau global mais pas sur les 8 dernières années
mysa$diagnostics
mysa$user_defined$`diagnostics.td-sa-last` # à 5%
mysa$user_defined$`diagnostics.td-i-last` # ) 1 %
# Une proposition de nouveau modèle :
# On va diviser les régresseurs avant et après 2012 pour prendre en compte
# un changement de coefficients

td_reg_post_2012 <- td_reg_pre_2012  <-
  regresseurs_JO[,grep("REG6", colnames(regresseurs_JO))]
window(td_reg_pre_2012, end = c(2011, 12)) <- 0
window(td_reg_post_2012, start = c(2012, 1)) <- 0
wkd2 <- ts.union(td_reg_pre_2012, td_reg_post_2012,
                 leap_year(frequency = 12))
colnames(wkd2) <- c(paste0(c("lundi","mardi","mercredi","jeudi","vendredi",
                             "samedi"),"_av2003"),
                    paste0(c("lundi","mardi","mercredi","jeudi","vendredi",
                             "samedi"),"_ap2003"),"leap_year")

mysa <- x13(y_p_c, x13_spec(mysa,
                            usrdef.var = wkd2,
                            usrdef.varType = "Calendar"),
            userdefined = c("diagnostics.td-sa-last", "diagnostics.td-i-last",
                            "diagnostics.fcast-outsample-mean",
                            "diagnostics.fcast-outsample-variance"))
# plus de JO résiduel à 5 %
mysa$user_defined$`diagnostics.td-sa-last`
mysa$user_defined$`diagnostics.td-i-last`

# Il y a beaucoup de paramètres à estimer
# mais pas de gros problème visible dans la qualité du modèle
mysa$regarima$residuals.stat
# Ni dans la qualité des prévisions
mysa$user_defined$`diagnostics.fcast-outsample-mean`
mysa$user_defined$`diagnostics.fcast-outsample-variance`

# On pourrait également continuer à faire des tests pour simplifier le modèle

summary(mysa$regarima)
for(jour in c("lundi","mardi","mercredi","jeudi","vendredi",
           "samedi")){
  print(car::linearHypothesis(mysa,
                        sprintf("%s_av2003=%s_ap2003",jour,jour), test = "F"))
}

wkd2 <- ts.union(wkd2[,c("lundi_av2003","mardi_av2003","jeudi_av2003",
                         "lundi_ap2003","mardi_ap2003","jeudi_ap2003")],
                 regresseurs_JO[,c("REG6_mercredi","REG6_vendredi","REG6_samedi")],
                 leap_year(frequency = 12))
colnames(wkd2) <- c("lundi_av2003","mardi_av2003","jeudi_av2003",
                    "lundi_ap2003","mardi_ap2003","jeudi_ap2003",
                    "mercredi","vendredi","samedi","leap_year")

mysa <- x13(y_p_c, x13_spec(mysa,
                            usrdef.var = wkd2,
                            usrdef.varType = "Calendar"),
            userdefined = c("diagnostics.td-sa-last", "diagnostics.td-i-last",
                            "diagnostics.fcast-outsample-mean",
                            "diagnostics.fcast-outsample-variance"))
mysa$user_defined$`diagnostics.td-sa-last`
mysa$user_defined$`diagnostics.td-i-last`
mysa$regarima$residuals.stat
mysa$user_defined$`diagnostics.fcast-outsample-mean`
mysa$user_defined$`diagnostics.fcast-outsample-variance`

# Mais on ne corrige pas bien de l'effet JO dans ce cas ! il vaut mieux garder l'ancien modèle
mysa$diagnostics
mysa$user_defined$`diagnostics.td-sa-last`
mysa$user_defined$`diagnostics.td-i-last`
summary(mysa$regarima)


remotes::install_github("palatej/rjd3sa",
                        INSTALL_opts = "--no-multiarch")