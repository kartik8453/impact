rm(list = ls())

library(dplyr)
library(data.table)
library(ggplot2)

MH_SURVEY <- readRDS("Data/Intermediate/mhSurvey.rds")

# .phq-9 ----
# .........................................................................

for (phqName in names(MH_SURVEY)[grepl("^phq9_", names(MH_SURVEY))]) {
  MH_SURVEY[, eval(paste0(phqName, "_num")) := fcase(
    get(phqName)=="Not at all", 0,
    get(phqName)=="Several days", 1,
    get(phqName)=="More than half of the days", 2,
    get(phqName)=="Nearly every day", 3)]
}

MH_SURVEY[, phq9Total := phq9_interest_num + phq9_depressed_num + phq9_sleep_num + phq9_tired_num + phq9_appetite_num + 
            phq9_failure_num + phq9_concentrating_num + phq9_speed_num + phq9_selfharm_num]

MH_SURVEY[, phq9Class := fcase(
  20<=phq9Total, "Severe depression",
  15<=phq9Total, "Moderately severe depression",
  10<=phq9Total, "Moderate depression",
  5<=phq9Total, "Mild depression",
  0<=phq9Total, "Minimal depression")]

MH_SURVEY[, phq9Class := factor(phq9Class, levels = c("Minmal depression", "Mild depression", "Moderate depression", 
                                                      "Moderately severe depression", "Severe depression"))]

MH_SURVEY[, phq9Binary := 10<=phq9Total]

# .gad ----
# .........................................................................

for (gadName in names(MH_SURVEY)[grepl("^gad7_", names(MH_SURVEY))]) {
  MH_SURVEY[, eval(paste0(gadName, "_num")) := fcase(
    get(gadName)=="Not at all", 0,
    get(gadName)=="Several days", 1,
    get(gadName)=="More than half of the days", 2,
    get(gadName)=="Nearly every day", 3)]
}

MH_SURVEY[, gad7Total := gad7_afraid_num + gad7_annoyed_num + gad7_anxious_num + gad7_control_num + 
            gad7_relaxing_num + gad7_restless_num + gad7_worrying_num ]

MH_SURVEY[, gad7Class := fcase(
  15<=gad7Total, "Severe anxiety",
  10<=gad7Total, "Moderate anxiety",
  5<=gad7Total, "Mild anxiety",
  0<=gad7Total, "Minimal anxiety")]

MH_SURVEY[, gad7Class := factor(gad7Class, levels = c("Minimal anxiety", "Mild anxiety", "Moderate anxiety", "Severe anxiety"))]
MH_SURVEY[, gad7Binary := 10<=gad7Total]

# .ACHA ----
# .........................................................................

for (achaName in names(MH_SURVEY)[grepl("^acha_12months_times_", names(MH_SURVEY))]) {
  MH_SURVEY[, eval(paste0(gsub("12months_times", "times", achaName), "_num")) := fcase(
    get(achaName)=="Never", 1,
    get(achaName)=="1-2 times", 2,
    get(achaName)=="3-4 times", 3,
    get(achaName)=="5-6 times", 4,
    get(achaName)=="7-8 times", 5,
    get(achaName)=="9-10 times", 6,
    get(achaName)=="11 or more times", 7)]
}

MH_SURVEY[, achaTotal := acha_times_attemptSuicide_num + acha_times_overwhelmed_num + acha_times_hopeless_num + acha_times_exhausted_num + 
            acha_times_considerSuicide_num + acha_times_depressed_num + acha_times_sad_num]

# Services and previous year
achaYesNo = c(names(MH_SURVEY)[grepl("^acha_12months_any", names(MH_SURVEY))],
              names(MH_SURVEY)[grepl("^acha_services_", names(MH_SURVEY))], "acha_depression")
for (achaName in achaYesNo) {
  MH_SURVEY[, eval(paste0(achaName, "_bin")) := fcase(
    get(achaName)=="No", 0,
    get(achaName)=="Yes", 1)]
}

MH_SURVEY[acha_depression_bin==0, acha_services_dianosed_bin := 0]
MH_SURVEY[acha_depression_bin==0, acha_services_therapy_bin := 0]
MH_SURVEY[acha_depression_bin==0, acha_services_medication_bin := 0]

# Create index
numericComp = names(MH_SURVEY)[grepl("acha_times.*num$", names(MH_SURVEY))]
binComp = c("acha_12months_any_anorexia_bin", "acha_12months_any_anxiety_bin", "acha_12months_any_depression_bin", "acha_12months_any_bulimia_bin",
            "acha_12months_any_seasonal_bin")
binServiceComp = c("acha_services_dianosed_bin", "acha_services_therapy_bin", "acha_services_medication_bin")
achaVars = c(numericComp, binComp, binServiceComp)
MH_SURVEY[, eval(paste0(achaVars, "S")) := lapply(.SD, scale), .SDcols = achaVars]
MH_SURVEY[, acha_times_depressed_numS - scale(acha_times_depressed_num)]

for (i in achaVars) {
  MH_SURVEY[is.na(get(i)), stopifnot(.N==0)]
}

MH_SURVEY[, mhNumericMean := rowMeans(.SD), .SDcols = numericComp]
MH_SURVEY[, mhBinMean := rowMeans(.SD), .SDcols = binComp]
MH_SURVEY[, mhBinServiceMean := rowMeans(.SD), .SDcols = binServiceComp]

MH_SURVEY[, indexMean := rowMeans(.SD), .SDcols = paste0(achaVars, "S")]
MH_SURVEY[, mentalHealthIndex := scale(indexMean)]

corAchaPhq = round(MH_SURVEY[, cor(mentalHealthIndex, phq9Total, method="pearson")], 2)

# First creating list of users and then defining quantiles, otherwsie some users randomly split
# to two quantiles at cutoff
MH_SURVEY[, quantileMH := ntile(mentalHealthIndex, 20)]
sumForGraph = MH_SURVEY[, lapply(.SD, mean), .SDcols = c("gad7Total", "phq9Total", "mentalHealthIndex"), by="quantileMH"]

ggplot(sumForGraph, aes(x = mentalHealthIndex, y=phq9Total)) +   
  geom_point() +
  geom_text(aes(x=-0.9, y=18, label = paste0("Correlation coef. ", corAchaPhq)), size=3, check_overlap=TRUE) + 
  theme_classic(base_size = 9) +
  labs(x = "NCHA - Poor Mental Health Index, Std. Dev.", y = "PHQ-9 Total") + 
  geom_smooth(method=lm, se=FALSE, color="gray", formula = y~x)
ggsave("Exhibits/Figure A14.pdf", width = 15, height = 7.5, units = "cm")
