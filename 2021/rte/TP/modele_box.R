i = -1
knitr::opts_template$set(exercice = list(box.title = paste("Exercice "),
                                         box.icon = "fa-question",
                                         box.collapse = NULL))
knitr::opts_template$set(solution = list(box.title = "Solution",
                                         box.body = list(fill = "#e6f6e7", colour = "black"),
                                         box.header = list(fill = "#ace1af", colour = "black"),
                                         box.icon = "fa-check-square",
                                         box.collapse = TRUE))
knitr::opts_template$set(alert = list(box.title = "Attention !",
                                      box.body = list(fill = "#fa5b42", colour = "#fdf6d4"),
                                      box.collapse = NULL,
                                      box.icon = "fa-exclamation-triangle"))
knitr::opts_template$set(indice = list(box.title = "Indice",
                                       box.body = list(fill = "#fff9dc", colour = "black"),
                                       box.header = list(fill = "#ffec8b", colour = "black"),
                                       box.icon = "fa-search",
                                       box.collapse = TRUE))
knitr::opts_template$set(pb = list(box.title = "Quel est le problème ?",
                                       box.body = list(fill = "#fff9dc", colour = "black"),
                                       box.header = list(fill = "#ffec8b", colour = "black"),
                                       box.icon = "fa-search",
                                       box.collapse = TRUE))
knitr::opts_template$set(remarque = list(box.title = "Remarque",
                                         box.body = list(fill = "#fff9dc", colour = "black"),
                                         box.header = list(fill = "#ffec8b", colour = "black"),
                                         box.icon = "fa-search",
                                         box.collapse = NULL))
knitr::opts_template$set(info = list(box.title = "Pour information",
                                       box.body = list(fill = "#fff9dc", colour = "black"),
                                       box.header = list(fill = "#ffec8b", colour = "black"),
                                       box.icon = "fa-search",
                                       box.collapse = NULL))
options(enable_print_style = FALSE)
