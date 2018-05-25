#======================================================================================================
# Lab-Wide Descriptives
#--------------------------------------------------
library(data.table)
library(psych)
#--------------------------------------------------
icmpc <- fread("data/aggregated_data/AnalysisData-Deletion.csv")
dropped <- fread("data/aggregated_data/DropList.csv")
#--------------------------------------------------
# Total 
nrow(icmpc) + nrow(dropped)
# Dropped 
dropped[Outlier == "YES", .(Outlier, OutlierReason)]
dropped[ExcludeReason == "Self Reported Hearing Loss"]
# Sample
nrow(icmpc)
describe(icmpc$goldage)
table(icmpc$ravenSex)
#--------------------------------------------------
# Two hundred fifty-four students enrolled at Louisiana State University completed the study. 
# We recruited students, mainly in the Department of Psychology and the School of Music. 
# The criteria inclusion in the analysis included reporting no hearing loss, not actively taking medication that would alter cognitive performance, and individuals whose performance on any task performed greater than 3 standard deviations from the mean score of that task.
# Using these criteria, eight participants were not eligible due to self reporting hearing loss, and six participants were eliminated as univariate outliers due to performance on one or more of the tasks of working memory capaicity.
# Thus, 240 participants met the criteria for inclusion. 
# The eligible participants were between the ages of 17 and 50 (M = 19.85, SD = 3.36; 149 females). 
# Participants volunteered, received course credit, or were paid $15.
#--------------------------------------------------
# DESCRIPTIVES
#--------------------------------------------------
# Check for Proper Correlations (Unsworth, 2009)
# Need sig positive correaltion between SymSpan, Ospan, TSpan -- All measuring WMC 

# All variables used for modeling approximated nomral distributions. 
# Processing errors for each task were positively skewed for the complex span tasks simlar to Unsworth, Redick, Heitz, Broadway, and Engle (2009). 
# Positive and significant correlations were found between the three tasks measuring working memory capacity (WMC) and the two measuring general fluid intelligence (Gf).
# The recall scores negatively correlated with the reported number of errors in each task, suggesting that rehearsal processes were effectively limited by the processing tasks (Unsworth et al., 2009). 
# Given the relationships between the WMC and Gf tasks, a composite measure of both were created by averaging the z-scores of the respective tasks. 


#======================================================================================================
# All WMC Correlate Positive and Significant with each other 
wmc <- icmpc[, .(TonePartial, 
                  MeanOspanPartialScore, 
                  MeanSspanPartialScore)]
pairs.panels(wmc, lm = TRUE, stars = TRUE)

# And each task should negatively correlate with own processing task 
negatives <- icmpc[, .(TonePartial, toneMathError,
                        MeanOspanPartialScore, OspanMathError,
                        MeanSspanPartialScore, MeanSymmErrorTotal )]

pairs.panels(negatives, lm = TRUE, stars = TRUE)
#--------------------------------------------------
# Above Correlations Suggest Creating new WMC composite

icmpc[, wmc := ((scale(TonePartial)+scale(MeanOspanPartialScore)+scale(MeanSspanPartialScore))/3)]
#--------------------------------------------------
# Check that Gf are both measuring the same 
# Create New Gf Variables

icmpc[, AdjustedNumberSeries := NumberTotalScore/NumberAttempted]
icmpc[, RavensAvg := RavensTotaljv/3]
gfscores <- icmpc[, .(RavensBlock1, RavensBlock2, RavensBlock3,RavensAvg, NumberAttempted, NumberTotalScore, NumberTotalTime, AdjustedNumberSeries)]

pairs.panels(gfscores, lm = TRUE, stars = TRUE)
#--------------------------------------------------
# Above Correlations Suggest Creating new GF composite

icmpc[, gf := (scale(AdjustedNumberSeries) + scale(RavensAvg)/2)]

gfscores <- icmpc[, .(gf,RavensBlock1, RavensBlock2, RavensBlock3,RavensAvg, NumberAttempted, NumberTotalScore, NumberTotalTime, AdjustedNumberSeries)]

pairs.panels(gfscores, lm = TRUE, stars = TRUE)
#--------------------------------------------------
# Look at composites and their original scores

compositeComparer <- icmpc[, .(gf, wmc, RavensAvg, AdjustedNumberSeries, TonePartial, MeanOspanPartialScore, MeanSspanPartialScore)]

pairs.panels(compositeComparer, lm = TRUE, stars = TRUE)
#======================================================================================================