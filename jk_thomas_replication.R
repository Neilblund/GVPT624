library(tidyverse)
library(labelled)
library(modelsummary)
library(ggeffects)
library(marginaleffects)
library(fixest)
# Replication Data for: Sisters Are Doing It for Themselves: How Female Combatants
# Help Generate Gender-Inclusive Peace Agreements in Civil Wars
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/Q7UV5W&version=1.0

# Downloading and recoding data
download.file(url = 'https://dataverse.harvard.edu/api/access/datafile/6988475', destfile='thomas_apsr.tsv', mode='wb')

data<-read_tsv(file='thomas_apsr.tsv')|>
  filter(interstateonly==0 & inter_intrastate==0)

data$cowcode<-factor(data$cowcode)

labs<-list(
  cat4_prevalence_best = "Female Combat Prevalence",
  exc_eq = "Women's Equality",
  Allddr = "Gender-Inclusive DDR/Rehab",
  WINGOImputed_fill = "Women's INGOs",
  v2x_gencs = "Women's Civil Society Participation",
  v2x_gencl = "Womens Civil Liberties",
  nonvprotgovt = "Number of Anti-state Women's Protests",
  lgt = "Agreement Length",
  substantive = "Substantive Agreement",
  ideolleft = "Leftist Rebel Ideology",
  religious = "Religious Rebel Ideology",
  UNintervention = "UN Involvement",
  majpowintervention = "Major Power Involvement",
  UNwomen = "UN Women Office",
  eqgen_binary = "Equality Provision",
  ssrddr_binary = "DDR provision",
  Krause_wpm_neg = "Women Signatories",
  cowcode = 'country'
)

# labelling variables
var_label(data) <-labs




# a long list of model formulas
forms<-list(
  `Model 1 (equality)` = formula(exc_eq  ~ cat4_prevalence_best),
  `Model 2 (equality)` = formula(
    exc_eq ~ cat4_prevalence_best +  nonvprotgovt + v2x_gencs + WINGOImputed_fill +  UNintervention + majpowintervention + UNwomen  + religious + ideolleft  + lgt + substantive +  v2x_gencl + Krause_wpm_neg +  eqgen_binary
  ),
  `Model 3 (DDR/Rehab)` = formula(Allddr ~ cat4_prevalence_best),
  `Model 4 (DDR/Rehab)` = formula(
    Allddr ~ cat4_prevalence_best + nonvprotgovt + v2x_gencs + WINGOImputed_fill + UNintervention + majpowintervention + UNwomen + religious + ideolleft + lgt + substantive + v2x_gencl + Krause_wpm_neg + ssrddr_binary
  )
)

# estimating effects for each: 
models<-forms|>
  map(\(x) .f=feglm(fml=x, data = data, family='binomial', cluster=~country))


modelsummary(models, 
             coef_map = labs,
             statistic = c("std.error"),
             stars=TRUE,
             coef_omit = '(Intercept)',
             )

# effects of main IV

predict_response(models[[1]], margin='empirical', 
                 terms='cat4_prevalence_best'
                     )|>


  plot()

predict_response(models[[3]], margin='empirical', terms='cat4_prevalence_best')|>
  plot()




# effects 1
avg_comparisons(models[[1]], variables='cat4_prevalence_best')

# effects 2
avg_comparisons(models[[2]], variables='cat4_prevalence_best')

# effects 3
avg_comparisons(models[[3]], variables='cat4_prevalence_best')

# effects 4
avg_comparisons(models[[4]], variables='cat4_prevalence_best')



