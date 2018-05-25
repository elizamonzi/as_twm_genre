#======================================================================================================
# Data Screening
# David John Baker
#--------------------------------------------------
library(data.table)
library(ggplot2)
library(psych)
library(stringr)
#--------------------------------------------------
# Import Data

master <- fread("data/aggregated_data/Master-Data_ICMPC18_May14.csv")
# First row is not real, drop it for now
master <- master[-1,]

#--------------------------------------------------
# Get Demographics
str(master)
names(master)

master[, ravenage := as.numeric(ravenAge)]

master$age

# Fix free responders 
master$age[170] <- 19
# Fix these below
master[age=="",.(subjectNo, ravenAge)]

master[, goldage := as.numeric(age)]

# Younger people 
master[ goldage < 18, .(subjectNo,goldage)]

# Update Gold Age with Real Age from Ravens 
master[subjectNo == 541]$goldage <- master[subjectNo == 541]$ravenage
master[subjectNo == 947]$goldage <- master[subjectNo == 947]$ravenage
master[subjectNo == 950]$goldage <- master[subjectNo == 950]$ravenage

master[subjectNo == 545]$goldage <- master[subjectNo == 545]$ravenage
master[subjectNo == 719]$goldage <- master[subjectNo == 719]$ravenage
master[subjectNo == 959]$goldage <- master[subjectNo == 959]$ravenage
master[subjectNo == 965]$goldage <- master[subjectNo == 965]$ravenage

ggplot(master, aes(goldage)) + geom_density() + labs(title = "Age Density WMMII Sample", x = "Age", y = "Density")

# Older People Screening
master[, .(subjectNo, goldage, zAge = scale(goldage))][zAge > 3]

# EXPLORE WHAT DIFFERENT STANDARD AGE DROPS LOOK LIKE
# Drops 10 for 3SD Outlier 
master[subjectNo != 953 & subjectNo != 734 & subjectNo != 742 &
         subjectNo != 1301 & subjectNo != 988 & subjectNo != 728 & subjectNo != 726 &
         subjectNo != 986 & subjectNo != 906 & subjectNo != 548, .(subjectNo, goldage, zAge = scale(goldage))][zAge > 3]

# Drops 6 for 5SD outlier 
master[, .(subjectNo, goldage, zAge = scale(goldage))][zAge > 5]

master[subjectNo != 953 & subjectNo != 734 & subjectNo != 742 &
         subjectNo != 1301 & subjectNo != 988 & subjectNo != 728, .(subjectNo, goldage, zAge = scale(goldage))][zAge > 5]

# AGE INFORMATION
describe(master[,.(goldage)])

#--------------------------------------------------
# Clean gender

master[ravenSex == "", .(subjectNo, ravenSex, gender)]

master[subjectNo == 507]$ravenSex <- master[subjectNo == 507]$gender 
master[subjectNo == 556]$ravenSex <- master[subjectNo == 556]$gender
master[subjectNo == 935]$ravenSex <- master[subjectNo == 935]$gender
master[subjectNo == 944]$ravenSex <- master[subjectNo == 944]$gender
master[subjectNo == 976]$ravenSex <- master[subjectNo == 976]$gender
master[subjectNo == 1026]$ravenSex <- master[subjectNo == 1026]$gender
master[subjectNo == 1027]$ravenSex <- master[subjectNo == 1027]$gender

master$ravenSex <- str_to_lower(master$ravenSex)
table(master$ravenSex)

#======================================================================================================
# Begin Screening 
## First drop prolematic cases 
master[!complete.cases(master)][,.(subjectNo)]

# 545, 719 , missing Goldsmiths Self Report  
#--------------------------------------------------
# Column for tracking outlier and reason 

master[, Exclude := "NO"]
master[, ExcludeReason := "NONE"]

# NOTE BOTH OF THESE HAVE SELF REPORTED HEARING LOSS!
master[subjectNo == 545]$Exclude <- "YES"
master[subjectNo == 545]$ExcludeReason <- "No Gold-MSI"

master[subjectNo == 719]$Exclude <- "YES"
master[subjectNo == 719]$ExcludeReason <- "No Gold-MSI"

# DROP 50 YEAR OLD NON MUSICIAN

master[subjectNo == 953]$Exclude <- "YES"
master[subjectNo == 953]$ExcludeReason <- "Older SONA Participant"

# Remove Doublers (This code is redundant, people should never have been in dataset)

master[Doubler == "DOUBLER"]$Exclude <- "YES"
master[Doubler == "DOUBLER"]$ExcludeReason <- "Took Experiment Twice"


master <- master[Doubler == "SINGLER"]


#--------------------------------------------------
# Get Variables of Interest
names(master)

# Demographic, Gold-MSI, SES, Gf, WMC 

gmsiwmcgf <- master[, .(subjectNo, ravenSex, goldage, 
                        BeatPerception, MelodicMemory, GENERAL, ACTIVE,PERCEPTUAL,MUSICAL, SINGING, EMOTIONS,
                         familyIncome, highestFather, highestMother,
                        RavensBlock1, RavensBlock2, RavensBlock3,
                        NumberAttempted, NumberTotalScore, NumberTotalTime,
                        toneaccError,ToneSpeedErr,TonePartial, TonePartialB1, TonePartialB2, TonePartialB3,
                        OspanAccError, OspanSpeedError,MeanOspanPartialScore, MeanOspanPartialScoreBlock1, MeanOspanPartialScoreBlock2, MeanOspanPartialScoreBlock3,
                        symmAccError, SymmSpeedError,MeanSspanPartialScore, MeanSspanPartialScoreBlock1, MeanOspanPartialScoreBlock2, MeanOspanPartialScoreBlock3)]

#======================================================================================================
# Outlier Detection
#--------------------------------------------------
# Column for tracking outlier and reason 

master[, Outlier := "NO"]
master[, OutlierReason := "NONE"]

#--------------------------------------------------
# Univariate 
zMaster <- apply(master[, .(RavensTotaljv, MeanOspanPartialScore, TonePartial,
                             MeanSspanPartialScore, NumberTotalScore)], 2, scale)

outlier <- data.table(cbind(master$subjectNo, zMaster))

# Ravens 
outlier[order(RavensTotaljv)]  # 970, 519, 532, 961
outlier[order(-RavensTotaljv)]

master[subjectNo == 970]$Outlier <- "YES"
master[subjectNo == 970]$OutlierReason <- "LOW RAVENS"

master[subjectNo == 519]$Outlier <- "YES"
master[subjectNo == 519]$OutlierReason <- "LOW RAVENS"

master[subjectNo == 532]$Outlier <- "YES"
master[subjectNo == 532]$OutlierReason <- "LOW RAVENS"

master[subjectNo == 961]$Outlier <- "YES"
master[subjectNo == 961]$OutlierReason <- "LOW RAVENS"


# O Span
outlier[order(MeanOspanPartialScore)]   # (961) already removed
outlier[order(-MeanOspanPartialScore)]

master[subjectNo == 961]$Outlier <- "YES"
master[subjectNo == 961]$OutlierReason <- "LOW RAVENS, LOW OSPAN"

# Tone Span
outlier[order(TonePartial)]   # 980, 1007
outlier[order(-TonePartial)]

master[subjectNo == 980]$Outlier <- "YES"
master[subjectNo == 980]$OutlierReason <- "LOW TSPAN"

master[subjectNo == 1007]$Outlier <- "YES"
master[subjectNo == 1007]$OutlierReason <- "LOW TSPAN"


# S Span 
outlier[order(MeanSspanPartialScore)]
outlier[order(-MeanSspanPartialScore)]

# Number Series
outlier[order(NumberTotalScore)]
outlier[order(-NumberTotalScore)]
#--------------------------------------------------
# Medication/Hearing Exculusion
# OK for all Medication, were excluded already 

master[meds == 0 | ravenHearing == "Yes" , .(subjectNo, meds, listMeds, ravenHearing, Outlier)]


# 1014 , check means related to sample for performance 
# 1311 , check for means 
# 518, 532, check means 
# 723, 924, 926, 920, 946, 



# REMOVE ALL SELF REPORTED HEARING LOSS 

master$Exclude <- ifelse(test = master$ravenHearing == "Yes", yes = "YES", no = master$Exclude)


master$ExcludeReason <- ifelse(test = master$ravenHearing == "Yes", yes = "Self Reported Hearing Loss", no = master$Exclude)

master[ravenHearing == "Yes", .(ravenHearing, Exclude, ExcludeReason)]

#--------------------------------------------------
# Create List of All Exclusions 

master[Exclude == "YES" | Outlier == "YES", .(subjectNo,Exclude, ExcludeReason, OutlierReason)]

master[, DROP := "NO"]

master[Exclude == "YES" | Outlier == "YES"]$DROP <- "YES"

droplist <- master[DROP == "YES", .(subjectNo, Exclude, ExcludeReason, Outlier, OutlierReason, DROP)]

fwrite(droplist, "data/aggregated_data/DropList.csv")
#--------------------------------------------------
# Plot Questions about Multivariate exclusion
names(master)
#--------------------------------------------------
# Drop Outliers, Missing Data, Self Select Out Before Composite Creatation
master <- master[DROP == "NO"]

#--------------------------------------------------
# Check for Proper Correlations (Unsworth, 2009)
# Need sig positive correaltion between SymSpan, Ospan, TSpan -- All measuring WMC 

wmc <- master[, .(TonePartial, 
                  MeanOspanPartialScore, 
                  MeanSspanPartialScore)]
pairs.panels(wmc, lm = TRUE, stars = TRUE)

# And each task should negatively correlate with own processing task 
negatives <- master[, .(TonePartial, toneMathError,
                        MeanOspanPartialScore, OspanMathError,
                        MeanSspanPartialScore, MeanSymmErrorTotal )]

pairs.panels(negatives, lm = TRUE, stars = TRUE)
#--------------------------------------------------
# Above Correlations Suggest Creating new WMC composite

master[, wmc := ((scale(TonePartial)+scale(MeanOspanPartialScore)+scale(MeanSspanPartialScore))/3)]
#--------------------------------------------------
# Check that Gf are both measuring the same 
# Create New Gf Variables

master[, AdjustedNumberSeries := NumberTotalScore/NumberAttempted]
master[, RavensAvg := RavensTotaljv/3]
gfscores <- master[, .(RavensBlock1, RavensBlock2, RavensBlock3,RavensAvg, NumberAttempted, NumberTotalScore, NumberTotalTime, AdjustedNumberSeries)]

pairs.panels(gfscores, lm = TRUE, stars = TRUE)

#--------------------------------------------------
# Above Correlations Suggest Creating new GF composite

master[, gf := (scale(AdjustedNumberSeries) + scale(RavensAvg)/2)]

gfscores <- master[, .(gf,RavensBlock1, RavensBlock2, RavensBlock3,RavensAvg, NumberAttempted, NumberTotalScore, NumberTotalTime, AdjustedNumberSeries)]

pairs.panels(gfscores, lm = TRUE, stars = TRUE)
#--------------------------------------------------
# Look at composites and their original scores

compositeComparer <- master[, .(gf, wmc, RavensAvg, AdjustedNumberSeries, TonePartial, MeanOspanPartialScore, MeanSspanPartialScore)]

pairs.panels(compositeComparer, lm = TRUE, stars = TRUE)
#======================================================================================================
# Answer Drop Questions
names(master)
master[goldage > 45, .(subjectNo, gf, wmc, familyIncome, highestFather, highestMother)]

agethreshold <- 32

ggplot(master, aes(x = goldage, y = GENERAL, size = wmc)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Age Predicts General", x = "Age", y = "General Score on GMSI, All Ages")

ggplot(master[goldage < agethreshold], aes(x = goldage, y = GENERAL, size = wmc)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = paste("Age Predicts General, No one above",agethreshold), x = "Age", y = "General Score on GMSI")

ggplot(master, aes(x = goldage, y = MUSICAL, size = wmc)) + 
  geom_point() + 
  geom_smooth(method = "lm")+
  labs(title = "Age Predicts Musical", x = "Age", y = "Musical Score on GMSI, All Ages")

ggplot(master[goldage < agethreshold], aes(x = goldage, y = MUSICAL, size = wmc)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = paste("Age Predicts Musical, No one above",agethreshold), x = "Age", y = "Musical Score")

plot(lm(MUSICAL ~ goldage, data = master))

#======================================================================================================
# Create Descriptives and Reliability Table Here

# Coming to Repo near you!!!!

#======================================================================================================
# Write out Dataset for Analysis 

AnalysisData <- master

fwrite(AnalysisData, "data/aggregated_data/AnalysisData-Deletion.csv")
