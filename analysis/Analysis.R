#======================================================================================================
# Draft of Analysis
#--------------------------------------------------
library(data.table)
library(ggplot2)
library(lavaan)
library(semPlot)
#--------------------------------------------------
semdata <- fread("data/aggregated_data/AnalysisData-Deletion.csv")

names(semdata)

analysis_data <- semdata[, .(subjectNo,wmc,gf, GENERAL, ACTIVE, PERCEPTUAL, MUSICAL, SINGING, EMOTIONS,
                             ravenSex, BeatPerception, MelodicMemory, AdjustedNumberSeries, RavensAvg,
                             MeanOspanPartialScore, MeanSspanPartialScore, TonePartial)]

analysis_data

# Do it like paper OR do it with individual items? 
# GENERAL  =~ X2 + X6 + X7 + X9 + X11 + X18 +  X19 + X20 + X21 +  X22 + X25 + X26 + X27 + X28 + X29 + X30 + X32 + X33
2 + 38
adderVector <- c(2,6,7,9,11,18,19,20,21,25,26,27,28,29,30,32,33)
forMeasurementModel <- adderVector + 38
forMeasurementModel

#--------------------------------------------------
# Create Measurment Model 
str(semdata)
## Define Model 
measurement.model <- '
gf =~ AdjustedNumberSeries + RavensAvg
wmc =~ MeanOspanPartialScore + MeanSspanPartialScore + TonePartial
gen =~ V40 + V44 + V45 + V47 + V49 + V56 + V57 + V58 + V59 + V63 + V64 + V65 + V66 + V67 + V68 + V70 + V71
'

## lavaan
measurment.model.fit <- sem(measurement.model, data = semdata, sample.nobs = 239, std.lv = FALSE)
semPaths(measurement.model.fit, whatLabels = "std", edge.label.cex = 2)
summary(measurment.model.fit)
fitMeasures(measurment.model.fit)

semPaths(measurment.model.fit,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

#--------------------------------------------------
# Double Models - One with General Predict Beat and Melodic
#               - Two with Cognitive Predict Beat and Melodic 
#--------------------------------------------------

# GENERAL PREDICTS BEAT AND MELODIC 
model.1 <- '
MelodicMemory ~ V40 + V44 + V45 + V47 + V49 + V56 + V57 + V58 + V59 + V63 + V64 + V65 + V66 + V67 + V68 + V70 + V71
BeatPerception ~ V40 + V44 + V45 + V47 + V49 + V56 + V57 + V58 + V59 + V63 + V64 + V65 + V66 + V67 + V68 + V70 + V71
## Covariances
MelodicMemory ~~ BeatPerception
'


## Fits
model.fit.1 <- sem(model.1, data = semdata, sample.nobs = 239, std.lv = TRUE)
semPaths(model.fit.1, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.1)
fitMeasures(model.fit.1)

semPaths(model.fit.1,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

