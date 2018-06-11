#======================================================================================================
# Draft of Analysis
#--------------------------------------------------
library(data.table)
library(psych)
library(stringr)
library(ggplot2)
library(apaTables)
install.packages("apaTables")
#--------------------------------------------------
asData <- fread("data/aggregated_data/Master-Data_ICMPC18_May26-withstrats.csv")

# Divide Musicians and Non-Musicans 
names(asData)

asData[,. (Counts = .N), by ="Source.x"]

# Defined musician status based on subject pool recruitment 

# Take String and split it on the comma, as many as there are
# This creates something call a List 
asData$stratz <- str_split(string = asData$summary,pattern = ",",n = Inf)


musicalpeople <- asData[Source.x == "Musician"]$stratz
nonmusicalpeople <- asData[Source.x == "Sona"]$stratz

musicalvector <- unlist(musicalpeople)
unmusicalvector <- unlist(nonmusicalpeople)

musicalvector <- str_trim(musicalvector, side ="both")
unmusicalvector <- str_trim(unmusicalvector, side ="both")

musicalvector <- data.table(musicalvector)
musicalvector[musicalvector == "rehearsal"] <- "Rehearsal"
musicalcounts <- musicalvector[, (Counts = .N), by="musicalvector"]
musicalcounts

unmusicalvector <- data.table(unmusicalvector)
unmusicalcounts <- unmusicalvector[, (Counts = .N), by="unmusicalvector"]
unmusicalcounts

setnames(musicalcounts,"V1","Counts")
setnames(unmusicalcounts,"V1","Counts")

musicalcounts$Source <- "Musical"
unmusicalcounts$Source <- "Non-Musical"

setnames(musicalcounts,"musicalvector","Strategy")
setnames(unmusicalcounts,"unmusicalvector","Strategy")
musicalcounts
unmusicalcounts

setwd("figures/")

fwrite(musicalcounts, "MusicalCounts.csv")
fwrite(unmusicalcounts, "UnMusicalCounts.csv")

stratPlotData <- fread("../data/figureData/StrategyPlotData.csv")

nonMusicalResponseAval <- 114 - 22 
MusicalResponseAval <- 100 - 66 

stratPlotData$TotalResponseAval <- 999
stratPlotData
stratPlotData[Source =="Non-Musical"]$TotalResponseAval <- nonMusicalResponseAval
stratPlotData[Source =="Musical"]$TotalResponseAval <- MusicalResponseAval
stratPlotData$PercentResponse <- stratPlotData$Counts / stratPlotData$TotalResponseAval * 100
stratPlotData

table(stratPlotData$Strategy)

str(stratPlotData)

stratPlotData$Strategy <- factor(stratPlotData$Strategy, levels = c(
  "Rehearsal",
  "Ordinal", 
  "No Strategy Indicated", 
  "Sensory-Motor",
  "Explicit Musical Word", 
  "Metaphor", 
  "Auditory Image",
  "No Response Available"
)
)

setnames(stratPlotData,"Source","Group")

stratPlotData[Group == "Non-Musical"]$Group <- "Non Musician"
stratPlotData[Group == "Musical"]$Group <- "Musician"

tiff('test.tiff', units="in", width = 5, height = 5, res = 300)
ggplot(stratPlotData[Strategy != "No Response Available"], 
       aes(x = Strategy, y = PercentResponse)) +
  #      scale_x_discrete(limit = positions) +
  geom_bar(aes(fill=Group), position = "dodge",stat = "identity") + 
  theme_bw() + scale_fill_grey(start = .1, end = .7) + 
  theme(legend.position = c(.8,.85), panel.grid.major = element_blank(),
        legend.text = element_text(size = 20), legend.title = element_text(size = 20),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 20)) +
  labs(title = "Strategies Employed by Musicians and Non-Musicians", 
       x = "Strategy", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5, size = 20))
ggsave("plotplot.jpg")
dev.off()
#----------------------------------------------
# 


#----------------------------------------------
# Predict Tone Score from Number Strats used
# number of strats used
names(asData)

asData$Response <- str_detect(asData$summary, pattern = "[A-Z]")

asData$CommaCount <- 0

asData$CommaCountX <- str_count(asData$summary, ",")

asData$StratCount <- ifelse(test = asData$Response == TRUE, yes = asData$CommaCountX + 1, no = 0)

regressionModel <- asData[, .(TonePartial, StratCount, ASI, ASII, ASIII, ASIV, FMT, MTI, MTII, MTIII, MTIV,
                              melodicTraning, melodicCourses, melodicPrivate, 
                              harmonicTraning, harmonicCourses, harmonicPrivate,
                              sightsingTraning, sightsingCourses, sightsingPrivate,
                              classical, relgious, pop, GENERAL, MUSICAL, BeatPerception, MelodicMemory)]


# Replace all Ps in AS and MT 

asData[ASI=="P"]$ASI <- 4
asData[,ASI := as.numeric(ASI)]

asData$MTI
asData[ASII=="P"]$ASII <- 4
asData[,ASII := as.numeric(ASII)]

asData[ASIII=="P"]$ASIII <- 4
asData[,ASIII := as.numeric(ASIII)]

asData[ASIV=="P"]$ASIV <- 4
asData[,ASIV := as.numeric(ASIV)]

asData[FMT=="P"]$FMT <- 4
asData[,FMT := as.numeric(FMT)]

asData[MTI=="P"]$MTI <- 4
asData[,MTI := as.numeric(MTI)]

asData[MTII=="P"]$MTII <- 4
asData[,MTII := as.numeric(MTII)]

asData[MTIII=="P"]$MTIII <- 4
asData[,MTIII := as.numeric(MTIII)]

asData[MTIV=="P"]$MTIV <- 4
asData[,MTIV := as.numeric(MTIV)]

asData[, .(ASI, ASII, ASIII, ASIV, MTI, MTII, MTIII, MTIV, FMT)]

# Based on scoring algorithm, add to create new composite scores
asData$CompositeAural <- apply(cbind(asData$ASI , asData$ASII , asData$ASIII , asData$ASIV),1,sum,na.rm=TRUE)

asData$CompositeGrade <- apply(X = cbind(asData$ASI , asData$ASII , asData$ASIII , asData$ASIV ,
                asData$MTI , asData$MTII , asData$MTIII , asData$MTIV , asData$FMT),1,sum,na.rm=TRUE)


regressionModel$TonePartial
regressionModel$MTI
names(asData)
pairs.panels(asData[Source.x == "Musician",.(TonePartial, StratCount, CompositeGrade)], lm = TRUE, stars = TRUE)

pairs.panels(asData[, .(TonePartial, MeanOspanPartialScore, MeanSspanPartialScore)], lm = TRUE, stars = TRUE)

model.1 <- lm(TonePartial ~ GENERAL, data = asData)
model.2 <- lm(TonePartial ~ GENERAL + CompositeAural, data = asData)
model.3 <- lm(TonePartial ~ GENERAL + CompositeAural + MeanSspanPartialScore, data = asData)

apa.reg.table(model.1, filename = "TableModel1.doc", table.number = 2)
apa.reg.table(model.2, filename = "TableModel2.doc", table.number = 3)
apa.reg.table(model.3, filename = "TableModel3.doc", table.number = 4)

apa.reg.table(model.1, model.2, filename = "TableModels1_2.doc", table.number = 2)

anova(model.1, model.2)
anova(model.2, model.3)

summary(model.1 )
summary(model.2)
summary(model.3)

apaTables::apa.reg.table(model.1)

plot(model.1)


#----------------------------------
# Predict AS Grades from ToneSpan Score

pairs.panels(asData[Source.x == "Musician", .(TonePartial, CompositeAural)], lm = TRUE, stars = TRUE)

pairs.panels(asData[, .(TonePartial, CompositeAural)], lm = TRUE, stars = TRUE)

#-----------------------------------
# Exploratory Part 
names(asData)

asData$refComp <- apply(cbind(asData$classical, asData$blues, asData$folk, asData$jazz),1,sum)
asData$intReb <- apply(cbind(asData$alternative, asData$rock, asData$heavyMetal),1,sum)
asData$upConv <- apply(cbind(asData$country, asData$relgious, asData$pop, asData$SoundtracksThemeSongs),1,sum)
asData$energRhyth <- apply(cbind(asData$dancElectronica, asData$rapHipHop, asData$soulFunk),1,sum)

asData$hypTonal <- apply(cbind(asData$classical, asData$soulFunk, asData$relgious, asData$pop),1,sum)
asData$hypAtonal <- apply(cbind(asData$rapHipHop, asData$heavyMetal, asData$dancElectronica),1,sum)

asData$instrument


pairs.panels(asData[Source.x == "Musician", .(hypTonal, hypAtonal, CompositeAural)], lm = TRUE, stars = TRUE)

pairs.panels(asData[Source.x == "Musician", .(refComp, intReb, upConv, energRhyth, CompositeAural)], lm = TRUE, stars = TRUE)

pairs.panels(asData[Source.x == "Musician", .(classical, 
                        blues, 
                        country, 
                        dancElectronica, 
                        folk,
                        rapHipHop, 
                        soulFunk, 
                        relgious, 
                        alternative, 
                        jazz, 
                        rock, 
                        pop, 
                        heavyMetal, 
                        SoundtracksThemeSongs, 
                        CompositeAural)], lm = TRUE, stars = TRUE)

pairs.panels(asData[, .(classical, 
                                              blues, 
                                              country, 
                                              dancElectronica, 
                                              folk,
                                              rapHipHop, 
                                              soulFunk, 
                       
                                              TonePartial)], lm = TRUE, stars = TRUE)

pairs.panels(asData[, .( 
                        soulFunk, 
                        relgious, 
                        alternative, 
                        jazz, 
                        rock, 
                        pop, 
                        heavyMetal, 
                        SoundtracksThemeSongs, 
                        TonePartial)], lm = TRUE, stars = TRUE)

apa.cor.table(asData[, .(classical,
                         blues,
                         country, 
                         dancElectronica, 
                         folk,
                         rapHipHop, 
                         soulFunk, 
                         relgious, 
                         alternative, 
                         jazz, 
                         rock, 
                         pop, 
                         heavyMetal, 
                         SoundtracksThemeSongs, 
                         TonePartial)], filename = "CorrTable.doc", table.number = 3)

ggplot(asData[Source.x == "Musician"], aes(x = CompositeAural, y = heavyMetal, size = GENERAL)) + 
  geom_point() + geom_jitter() + geom_smooth(method = "lm")
