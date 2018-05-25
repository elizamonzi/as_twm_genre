#======================================================================================================
# Big Kahuna Alligator
#--------------------------------------------------
library(data.table)
source("scripts/scoreGMSI.R")
source("scripts/create.gmsi.data.R")
source("scripts/eprimeFunctions.R")
source("scripts/scoreospan.R")
source("scripts/createOspanDataset.R")
source("scripts/scoreRavens.R")
source("scripts/createRavens.R")
source("scripts/scoreToneSpan.R")
source("scripts/createToneSpan.R")
#--------------------------------------------------
# Create GMSI Extra Scores
setwd("data/goldmsi/combined/")
score.gmsi.extra()
create.gmsi.dataset()
# Remove Junk Files
junk <- dir(pattern="_data") 
file.remove(junk)
setwd("../../..")
#--------------------------------------------------
# Create Number Series Data
#--------------------------------------------------
setwd("data/numberseries/combined/")
# Create New Rotation Individual Files 
score.number.series()
# Update New Dataset 
create.num.dataset()
# Do we get warnings?!?!?
# Remove Junk Files
junk <- dir(pattern="num_data") 
file.remove(junk)
setwd("../../..")
#--------------------------------------------------
# Score Ospan
#--------------------------------------------------
setwd("data/ospan/combined/")
# Create New Rotation Individual Files 
score.o.span()
# Update New Dataset 
create.ospan.dataset()
# FIX INCORRECT LOG PROBLEM, SORT OF WORKS

# Remove Junk Files
junk <- dir(pattern="ospan_data") 
file.remove(junk)
setwd("../../..")
#--------------------------------------------------
# Score Ravens 
#--------------------------------------------------
setwd("data/ravens/combined/")
# Create New Rotation Individual Files 
score.ravens()
# Update New Dataset 
create.ravens.dataset()
# Do we get warnings?!?!?
# Remove Junk Files
junk <- dir(pattern="ravens_data") 
file.remove(junk)
setwd("../../..")
#--------------------------------------------------
# Create Sym Span Data
#--------------------------------------------------
# ADD IN NEW FEATURE FOR SYM SPAN
setwd("data/symspan/combined/")
# Create New Rotation Individual Files 
score.sym.span()
# Update New Dataset 
create.sym.dataset()
# Remove Junk Files
junk <- dir(pattern="sym_data") 
file.remove(junk)
setwd("../../..")
#--------------------------------------------------
# Create Tone Span
#--------------------------------------------------
setwd("data/tonespan/combined/")
# Create New Rotation Individual Files 
score.tone.span()
# Update New Dataset 
create.tone.dataset()
# Do we get warnings?!?!?

# Remove Junk Files
junk <- dir(pattern="tone_data") 
file.remove(junk)
setwd("../../..")
#--------------------------------------------------
# Get Qualtrics Data
#--------------------------------------------------
setwd("data/qualtrics/combined")
# NOTE CURRENTLY THE SPECIFIC FILE NAME NEEDS
# TO BE WRITTEN IN THIS FUNCTION !!!!!!
create.qualtrics()
setwd("../../..")
#--------------------------------------------------
# Create Master Analysis Spreadheet
#--------------------------------------------------
# Create Master Spreadsheet for Analysis 
setwd("data/aggregated_data/")

#--------------------------------------------------
# From Pipeline 
beatData <- fread("BeatData_icmpc18.csv", colClasses = "character")
melodicData <- fread("MelodicData_icmpc18.csv", colClasses = "character")
goldExtraData <- fread("Gold_MSI_icmpc18.csv", colClasses = "character")
ravensData <- fread("RavensData_icmpc18.csv", colClasses = "character")
numberseriesData <- fread("NumerSeriesData.csv", colClasses = "character")
symmetryData <- fread("SymmetrySpanData_icmpc18.csv", colClasses = "character")
oSpanData <- fread("OSPAN_data_icmpc18.csv", colClasses = "character")
toneSpanData <- fread("ToneSpanData_icmpc18.csv", colClasses = "character")

#--------------------------------------------------
# From JV May 14 Drop 

beatDataMay14 <- fread("beat_May14.csv", colClasses = "character")
melodicDatayMay14 <- fread("melodic_May14.csv", colClasses = "character")
ravensDataMay14 <- fread("ravens_May14.csv", colClasses = "character")
numberSeriesDataMay14 <- fread("ns_May14.csv", colClasses = "character")
wmcDataMay14 <- fread("wmc_May14.csv", colClasses = "character")
setnames(wmcDataMay14,"subject","subjectNo")
subjectLog <- fread("WMmusic2TsubjectLog.csv", colClasses = "character")
setnames(subjectLog,"Participant Number","subjectNo")

masterMay14 <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                       list(goldExtraData,beatDataMay14,melodicDatayMay14,
                            ravensDataMay14,numberSeriesDataMay14,
                            wmcDataMay14,ravensData,subjectLog))

fwrite(masterMay14,"Master-Data_ICMPC18_May14.csv")
# Drop V gold, ravens, op, sym, tone

goldExtraData[, V1 := NULL]
ravensData[, V1 := NULL]
symmetryData[, V1 := NULL]
oSpanData[, V1 := NULL]
toneSpanData[, V1 := NULL]

master <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                 list(goldExtraData,beatData,melodicData,ravensData,numberseriesData,symmetryData,oSpanData,toneSpanData))


master <- master[order(as.numeric(subjectNo))]

fwrite(master,"Master-Data_ICMPC18.csv")
setwd("../../")
#======================================================================================================