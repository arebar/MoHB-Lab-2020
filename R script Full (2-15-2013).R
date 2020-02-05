options(scipen=999)

par(mai=c(.5,.5,.5,.5))

##TESTING HELOOOOO

###Package installs###
install.packages('multilevel')
install.packages('sem')
install.packages('MASS')
install.packages('sm')
install.packages('mlmmm')
install.packages('ggplot2')
install.packages('psychometric')
install.packages('MBESS')
install.packages('lme4')
install.packages('gdata')
install.packages('nFactors')
install.packages('snippets',,'http://www.rforge.net/')  
install.packages('tm')
install.packages('gplots') 
install.packages('stats')
install.packages('gsubfn')
install.packages('gdata')
install.packages('lme4')
install.packages('sem')
install.packages('hexbin')
install.packages('mvnormtest')
install.packages('FactoMineR')
install.packages('multilevel')
install.packages('ltm')
install.packages('foreign')
source('http://openmx.psyc.virginia.edu/getOpenMx.R')
install.packages('QuantPsyc')
install.packages('ggm')
install.packages('psych')
install.packages('arm')
install.packages('pbkrtest')
install.packages('car')
install.packages('forecast')





##################Start Individualized FB here ##################################################



###Reading in datasets### ###Put ID in as 'Cha' ###
Cha <-2009

Int1All <-read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\int1.csv", na.strings=".", stringsAsFactors=FALSE)
Int2All <-read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\int2.csv", na.strings=".", stringsAsFactors=FALSE)
Int3All <-read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\int3.csv", na.strings=".", stringsAsFactors=FALSE)
Int123All  <-read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\intAll.csv", na.strings=".", stringsAsFactors=FALSE)
BB1All <- read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\bb1.csv", na.strings=".", stringsAsFactors=FALSE)
BB2All <- read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\bb2.csv", na.strings=".", stringsAsFactors=FALSE)
BB3All <- read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\bb3.csv", na.strings=".", stringsAsFactors=FALSE)
BB123All <- read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\bbAll.csv", na.strings=".", stringsAsFactors=FALSE)
Survey <-read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\Burst_scales.csv", stringsAsFactors=FALSE)
Exclus <- Survey[ which(Survey$id==Cha), ]
Int1 <- Int1All[ which(Int1All$id==Cha), ]
Int2 <- Int2All[ which(Int2All$id==Cha), ]
Int3 <- Int3All[ which(Int3All$id==Cha), ]
Int123 <- Int123All[ which(Int123All$id==Cha), ]
BB1 <- BB1All[ which(BB1All$id==Cha), ]
BB2 <- BB2All[ which(BB2All$id==Cha), ]
BB3 <- BB3All[ which(BB3All$id==Cha), ]
BB123 <- BB123All[ which(BB123All$id==Cha), ]

BB2 <- read.csv("U:\\iSAHIB Data\\Final Feedbacks 08-24-2011\\bb1107.csv", na.strings=".", stringsAsFactors=FALSE)


###affect and behavior contour plots###

#fancy<-Int123$IntAGA_int+(Int123$IntAGV/1000)
#FancyShmancy <- table(fancy)
#write.table(FancyShmancy, file = "I:/End of Study Feedback/Finalized Feedbacks/Contour plots/Felt/2009_unscored.csv", sep = ",", col.names = NA,
#            qmethod = "double")



#fancy2<-Int123$IntIGselfA_int+(Int123$IntIGselfC_int/1000)
#FancyShmancy2 <- table(fancy2)
#write.table(FancyShmancy2, file = "I:/End of Study Feedback/Finalized Feedbacks/Contour plots/Behaved/2009_unscored.csv", sep = ",", col.names = NA,
#            qmethod = "double")


# High Density Scatterplot with Binning
library(hexbin)

bin<-hexbin(Int123$IntIGselfC_int,Int123$IntIGselfA_int , xbins=50)
bin2<-hexbin(Int123$IntAGV_int, Int123$IntAGA_int, xbins=45)
bmp("I:/End of Study Feedback/Finalized Feedbacks/Behave.bmp", width=4, height=4, units="in", res=450)
plot(bin, xlab = " ",colramp=function(n) BTC(n,beg = 90,end = 15), ylab=" ", legend=FALSE)
dev.off()

bmp("I:/End of Study Feedback/Finalized Feedbacks/Felt.bmp", width=4, height=4, units="in", res=450)
plot(bin2, xlab = " ",colramp=function(n) BTC(n,beg = 90,end = 15), ylab=" ", legend = FALSE)
dev.off()


###Age Control Plot###



attach(Survey)
Survey$control_Mean <-((controltotal1_1 + controltotal1_2 + controltotal2_1 + controltotal2_2 + controltotal3_1 + controltotal3_2)/6)
detach(Survey)
bmp("I:/End of Study Feedback/Finalized Feedbacks/ConAge1.bmp", width=18, height=4, units="in", res=150)
scatter.smooth(x=Survey$age_b1_1, y=Survey$control_Mean, ylim=range(1:7), pch=19,col="blue",
,ylab = "Perceived Control", xlab = " ")
dev.off()

mean(Survey$age_b1_1, na.rm=TRUE)

attach(Survey)
Survey$control_Mean <-((controltotal1_1 + controltotal1_2 + controltotal2_1 + controltotal2_2 + controltotal3_1 + controltotal3_2)/6)
detach(Survey)
control_Mean <-((Exclus$controltotal1_1 + Exclus$controltotal1_2 + Exclus$controltotal2_1 + Exclus$controltotal3_1 + Exclus$controltotal3_2 )/5)
bmp("I:/End of Study Feedback/Finalized Feedbacks/ConAge.bmp", width=18, height=4, units="in", res=150)
scatter.smooth(x=Survey$age_b1_1, y=Survey$control_Mean, ylim=range(1:7), pch=19,col="blue",
,ylab = "Perceived Control", xlab = " ")
points(Exclus$age_b1_1, control_Mean, type="b", col="red",pch=21, cex = 1.5)
dev.off()


###Health Control Plots###
Avg1_1a<-mean(Survey$sf36Gen1_1, na.rm = TRUE)
Avg1_1<-(Avg1_1a/6)*1.4

Avg1_2a<-mean(Survey$sf36Gen1_2, na.rm = TRUE)
Avg1_2<-(Avg1_2a/6)*1.4

Avg2_1a<-mean(Survey$sf36Gen2_1, na.rm = TRUE)
Avg2_1<-(Avg2_1a/6)*1.4

Avg2_2a<-mean(Survey$sf36Gen2_2, na.rm = TRUE)
Avg2_2<-(Avg2_2a/6)*1.4

Avg3_1a<-mean(Survey$sf36Gen3_1, na.rm = TRUE)
Avg3_1<-(Avg3_1a/6)*1.4

Avg3_2a<-mean(Survey$sf36Gen3_2, na.rm = TRUE)
Avg3_2<-(Avg3_2a/6)*1.4

Avg1_1c<-mean(Survey$controltotal1_1, na.rm = TRUE)

Avg1_2c<-mean(Survey$controltotal1_2, na.rm = TRUE)

Avg2_1c<-mean(Survey$controltotal2_1, na.rm = TRUE)

Avg2_2c<-mean(Survey$controltotal2_2, na.rm = TRUE)

Avg3_1c<-mean(Survey$controltotal3_1, na.rm = TRUE)

Avg3_2c<-mean(Survey$controltotal3_2, na.rm = TRUE)

ConHealth1 <-c(Avg1_1, Avg1_2, Avg2_1, Avg2_2, Avg3_1, Avg3_2)
ConHealth2 <-c(Avg1_1c, Avg1_2c, Avg2_1c, Avg2_2c, Avg3_1c, Avg3_2c)
bmp("I:/End of Study Feedback/Finalized Feedbacks/ConHealth.bmp", width=8, height=3.5, units="in", res=300)
plot(ConHealth1, pch=22, type="o", col="red", ylim=range(1:7), axes=FALSE, xlab=" ", ylab=" ")
lines(ConHealth2, pch=19, type="o", lty=2, col="blue")
axis(1, at=1:6, lab=c("Pre Burst 1","Post Burst 1","Pre Burst 2","Porst Burst 2",
"Pre Burst 3", "Post Burst 3"))
axis(2, at=1:7, lab=c(1,2,3,4,5,6,7))
box()
dev.off()

ConHealth1 <-c((((Exclus$sf36Gen1_1)/6)*1.4),(((Exclus$sf36Gen1_2)/6)*1.4),(((Exclus$sf36Gen2_1)/6)*1.4),
(((Exclus$sf36Gen2_2)/6)*1.4),(((Exclus$sf36Gen3_1)/6)*1.4), (((Exclus$sf36Gen3_2)/6)*1.4))
ConHealth2 <-c(Exclus$controltotal1_1, Exclus$controltotal1_2, Exclus$controltotal2_1, Exclus$controltotal2_2,
Exclus$controltotal3_1, Exclus$controltotal3_2)
bmp("I:/End of Study Feedback/Finalized Feedbacks/ConHealth.bmp", width=8, height=3.5, units="in", res=300)
plot(ConHealth1, pch=22, type="o", col="red", ylim=range(1:7), axes=FALSE, xlab=" ", ylab=" ")
lines(ConHealth2, pch=19, type="o", lty=2, col="blue")
axis(1, at=1:6, lab=c("Pre Burst 1","Post Burst 1","Pre Burst 2","Porst Burst 2",
"Pre Burst 3", "Post Burst 3"))
axis(2, at=1:7, lab=c(1,2,3,4,5,6,7))
box()
dev.off()


###Control over Days Plots###
Control_Su<-BB123[ which(BB123$Record_dayofweek==1), ]
Control_Mo<-BB123[ which(BB123$Record_dayofweek==2), ]
Control_Tu<-BB123[ which(BB123$Record_dayofweek==3), ]
Control_We<-BB123[ which(BB123$Record_dayofweek==4), ]
Control_Th<-BB123[ which(BB123$Record_dayofweek==5), ]
Control_Fr<-BB123[ which(BB123$Record_dayofweek==6), ]
Control_Sa<-BB123[ which(BB123$Record_dayofweek==7), ]
ConMean_Su <- mean(Control_Su$CONTROL_d, na.rm=TRUE)
ConMean_Mo <- mean(Control_Mo$CONTROL_d, na.rm=TRUE)
ConMean_Tu <- mean(Control_Tu$CONTROL_d, na.rm=TRUE)
ConMean_We <- mean(Control_We$CONTROL_d, na.rm=TRUE)
ConMean_Th <- mean(Control_Th$CONTROL_d, na.rm=TRUE)
ConMean_Fr <- mean(Control_Fr$CONTROL_d, na.rm=TRUE)
ConMean_Sa <- mean(Control_Sa$CONTROL_d, na.rm=TRUE)
ConMeans <- c(ConMean_Su, ConMean_Mo, ConMean_Tu, ConMean_We, ConMean_Th, ConMean_Fr, ConMean_Sa) 
bmp("I:/End of Study Feedback/Finalized Feedbacks/ConDays.bmp", width=9, height=3.5, units="in", res=250)
barplot(ConMeans, xlab=" ", ylab="Daily Perceived Control", ylim=range(0:100), border="black", col="magenta")
box()
dev.off()


###Control by Interactions###
AllInts <- rbind(Int1, Int2, Int3)  
Control_St<-AllInts[ which(AllInts$IntTYPE_int==0), ]
Control_Fr<-AllInts[ which(AllInts$IntTYPE_int==1), ]
Control_Cw<-AllInts[ which(AllInts$IntTYPE_int==2), ]
Control_Rp<-AllInts[ which(AllInts$IntTYPE_int==3|AllInts$IntTYPEfam_int==1), ]
Control_Fa<-AllInts[ which(AllInts$IntTYPE_int==4), ]
Control_Se<-AllInts[ which(AllInts$IntTYPE_int==5), ]
ConMean_St <- mean(Control_St$IntCONTROL_int, na.rm=TRUE)
ConMean_Cw <- mean(Control_Cw$IntCONTROL_int, na.rm=TRUE)
ConMean_Fr <- mean(Control_Fr$IntCONTROL_int, na.rm=TRUE)
ConMean_Rp <- mean(Control_Rp$IntCONTROL_int, na.rm=TRUE)
ConMean_Fa <- mean(Control_Fa$IntCONTROL_int, na.rm=TRUE)
ConMean_Se <- mean(Control_Se$IntCONTROL_int, na.rm=TRUE)
x<-c(0, 10, 20, 30, 40, 50)
y <-c(1,1,1,1,1,1)
control<-c(ConMean_St, ConMean_Fr, ConMean_Cw, ConMean_Rp, ConMean_Fa, ConMean_Se)
Almost<-data.frame(control=control, x=x, y=y)


bmp("I:/End of Study Feedback/Finalized Feedbacks/ConInts.bmp", width=26, height=4, units="in", res=250)
radius <- sqrt((Almost$control*Almost$control*Almost$control*Almost$control)/ pi )
symbols(Almost$x, Almost$y, circles=radius, inches=.5, fg="black", bg=rainbow(6), ann=FALSE, axes=FALSE)
axis(1, at=0, label="Strangers")
axis(1, at=10, label="Friends")
axis(1, at=20, label="Co-Workers")
axis(1, at=30, label="Romantic Partner(s)")
axis(1, at=40, label="Family")
axis(1, at=50, label="Service Professionals/Other")
dev.off()


###iMEAN Word Cloud###

library(snippets)
AllDays <- rbind(BB1, BB2, BB3)
EnthM <- mean(AllDays$PAENTH_d, na.rm=TRUE)
HapM <- mean(AllDays$PAHAP_d, na.rm=TRUE)
AlertM <- mean(AllDays$PAALERT_d, na.rm=TRUE)
ProudM <- mean(AllDays$PAPROUD_d, na.rm=TRUE)
ExcM <- mean(AllDays$PAEXCITED_d, na.rm=TRUE)
CalmM <- mean(AllDays$PDCALM_d, na.rm=TRUE)
PeacM <- mean(AllDays$PDPEAC_d, na.rm=TRUE)
SatM <- mean(AllDays$PDSAT_d, na.rm=TRUE)
ContM <- mean(AllDays$PDCONTENT_d, na.rm=TRUE)
RelaxM <- mean(AllDays$PDRELAX_d, na.rm=TRUE)
NervM <- mean(AllDays$UANERV_d, na.rm=TRUE)
EmbarM <- mean(AllDays$UAEMBR_d, na.rm=TRUE)
UpsetM <- mean(AllDays$UAUPSET_d, na.rm=TRUE)
TenseM <- mean(AllDays$UATENSE_d, na.rm=TRUE)
SlugM <- mean(AllDays$UDSLUG_d, na.rm=TRUE)
SadM <- mean(AllDays$UDSAD_d, na.rm=TRUE)
BorM <- mean(AllDays$UDBORED_d, na.rm=TRUE)
DepM <- mean(AllDays$UDDEPRSD_d, na.rm=TRUE)
RelM <- mean(AllDays$RELIEF_d, na.rm=TRUE)
DisM <- mean(AllDays$UDDISAP_d, na.rm=TRUE)
AngM <- mean(AllDays$ANGER_d, na.rm=TRUE)
GratM <- mean(AllDays$GRATEFUL_d, na.rm=TRUE)
SnobM <- mean(AllDays$HUBRIS_d, na.rm=TRUE)
FatM <- mean(AllDays$FATIGUE_d, na.rm=TRUE)

colors <-c("red","dark red", "indianred2", "red4" ,"orangered1",
           "red","dark red", "indianred2", "red4" ,"orangered1",
           "red","dark red", "indianred2", "red4" ,"orangered1",
           "red","dark red", "indianred2", "red4" ,"orangered1",
           "red","dark red", "indianred2", "red4" ,"orangered1")

words <-c(alert=AlertM, excited=ExcM, enthusiastic=EnthM, happy=HapM,
     satisfied=SatM, proud=ProudM, relieved=RelM, grateful=GratM,  
          peaceful=PeacM, relaxed=RelaxM, calm=CalmM, content=ContM,
          sad=SadM, embarrassed=EmbarM, snobbish=SnobM, upset=UpsetM,
  sluggish=SlugM, bored=BorM, depressed=DepM, disappointed=DisM,
          nervous=NervM,  tense=TenseM, angry=AngM, fatigued=FatM)

bmp("I:/End of Study Feedback/Finalized Feedbacks/iMeanCloud.bmp", width=8, height=5, units="in", res=100)
cloud(words,yspace=1.3, espace=0.01, minh=0, col=colors)
dev.off()

###iSD Word Cloud###

EnthSD <- sd(AllDays$PAENTH_d, na.rm=TRUE)
HapSD <- sd(AllDays$PAHAP_d, na.rm=TRUE)
AlertSD <- sd(AllDays$PAALERT_d, na.rm=TRUE)
ProudSD <- sd(AllDays$PAPROUD_d, na.rm=TRUE)
ExcSD <- sd(AllDays$PAEXCITED_d, na.rm=TRUE)
CalmSD <- sd(AllDays$PDCALM_d, na.rm=TRUE)
PeacSD <- sd(AllDays$PDPEAC_d, na.rm=TRUE)
SatSD <- sd(AllDays$PDSAT_d, na.rm=TRUE)
ContSD <- sd(AllDays$PDCONTENT_d, na.rm=TRUE)
RelaxSD <- sd(AllDays$PDRELAX_d, na.rm=TRUE)
NervSD <- sd(AllDays$UANERV_d, na.rm=TRUE)
EmbarSD <- sd(AllDays$UAEMBR_d, na.rm=TRUE)
UpsetSD <- sd(AllDays$UAUPSET_d, na.rm=TRUE)
TenseSD <- sd(AllDays$UATENSE_d, na.rm=TRUE)
SlugSD <- sd(AllDays$UDSLUG_d, na.rm=TRUE)
SadSD <- sd(AllDays$UDSAD_d, na.rm=TRUE)
BorSD <- sd(AllDays$UDBORED_d, na.rm=TRUE)
DepSD <- sd(AllDays$UDDEPRSD_d, na.rm=TRUE)
RelSD <- sd(AllDays$RELIEF_d, na.rm=TRUE)
DisSD <- sd(AllDays$UDDISAP_d, na.rm=TRUE)
AngSD <- sd(AllDays$ANGER_d, na.rm=TRUE)
GratSD <- sd(AllDays$GRATEFUL_d, na.rm=TRUE)
SnobSD <- sd(AllDays$HUBRIS_d, na.rm=TRUE)
FatSD <- sd(AllDays$FATIGUE_d, na.rm=TRUE)

wordsSD <-c(alert=AlertSD, excited=ExcSD, enthusiastic=EnthSD, happy=HapSD,
     satisfied=SatSD, proud=ProudSD, relieved=RelSD, grateful=GratSD,  
          peaceful=PeacSD, relaxed=RelaxSD, calm=CalmSD, content=ContSD,
          sad=SadSD, embarrassed=EmbarSD, upset=UpsetSD,
  sluggish=SlugSD, bored=BorSD, depressed=DepSD, disappointed=DisSD,
          nervous=NervSD,  tense=TenseSD, angry=AngSD, fatigued=FatSD)
colors2 <-c("deepskyblue", "blue", "skyblue", "dark blue", "light blue",
            "darkblue", "light blue", "blue", "skyblue", "dark blue",
            "deepskyblue", "dark blue", "skyblue", "dark blue", "light blue",
            "deepskyblue", "skyblue", "blue", "dark blue", "light blue",
           "deepskyblue", "blue", "skyblue", "dark blue", "light blue")
bmp("I:/End of Study Feedback/Finalized Feedbacks/iSDCloud.bmp", width=8, height=5, units="in", res=100)
cloud(wordsSD, yspace=1.3, espace=0.01, col=colors2, minh=0)
dev.off()



###Time Allocation Pie Charts###
WSB1 <- mean(BB1$TUWS_d, na.rm=TRUE)
LB1 <- mean(BB1$TUL_d, na.rm=TRUE)
OOB1 <- mean(BB1$TUOO_d, na.rm=TRUE)
WSB2 <- mean(BB2$TUWS_d, na.rm=TRUE)
LB2 <- mean(BB2$TUL_d, na.rm=TRUE)
OOB2 <- mean(BB2$TUOO_d, na.rm=TRUE)
WSB3 <- mean(BB3$TUWS_d, na.rm=TRUE)
LB3 <- mean(BB3$TUL_d, na.rm=TRUE)
OOB3 <- mean(BB3$TUOO_d, na.rm=TRUE)

bmp("I:/End of Study Feedback/Finalized Feedbacks/B1Time.bmp", pointsize=10, width=3, height=3, units="in", res=150)
Burst1 <-c(WSB1, LB1, OOB1)
lbls <-c(" ", " ", " ")
pct <- round(Burst1/sum(Burst1)*100)
lbls <-paste(lbls, pct)
lbls <-paste(lbls,"%",sep="")
pie(Burst1,labels=lbls, col=c("blue", "forest green", "yellow"), las=.8)
dev.off()

bmp("I:/End of Study Feedback/Finalized Feedbacks/B2Time.bmp", pointsize=10, width=3, height=3, units="in", res=150)
Burst2 <-c(WSB2, LB2, OOB2)
lbls <-c(" ", " ", " ")
pct <- round(Burst2/sum(Burst2)*100)
lbls <-paste(lbls, pct)
lbls <-paste(lbls,"%",sep="")
pie(Burst2,labels=lbls, col=c("blue", "forest green", "yellow"), las=.8)
dev.off()

bmp("I:/End of Study Feedback/Finalized Feedbacks/B3Time.bmp", pointsize=10, width=3, height=3, units="in", res=150)
Burst3 <-c(WSB3, LB3, OOB3)
lbls <-c(" ", " ", " ")
pct <- round(Burst3/sum(Burst3)*100)
lbls <-paste(lbls, pct)
lbls <-paste(lbls,"%",sep="")
pie(Burst2,labels=lbls, col=c("blue", "forest green", "yellow"), las=.8)
dev.off()


###personality scatter plot###

Op <-mean(Exclus$bfiO1_1, Exclus$bfiO1_2, Exclus$bfiO2_1, Exclus$bfiO2_2,
Exclus$bfiO3_1, Exclus$bfiO3_2)
Co <-mean(Exclus$bfiC1_1, Exclus$bfiC1_2, Exclus$bfiC2_1, Exclus$bfiC2_2,
Exclus$bfiC3_1, Exclus$bfiC3_2)
Ex <-mean(Exclus$bfiE1_1, Exclus$bfiE1_2, Exclus$bfiE2_1, Exclus$bfiE2_2,
Exclus$bfiE3_1, Exclus$bfiE3_2)
Ag <-mean(Exclus$bfiA1_1, Exclus$bfiA1_2, Exclus$bfiA2_1, Exclus$bfiA2_2,
Exclus$bfiA3_1, Exclus$bfiA3_2)
Ne <-mean(Exclus$bfiN1_1, Exclus$bfiN1_2, Exclus$bfiN2_1, Exclus$bfiN2_2,
Exclus$bfiN3_1, Exclus$bfiN3_2)

O1<-c(Op, 3)
C1<-c(Co, 3)
E1<-c(Ex, 3)
A1<-c(Ag, 3)
N1<-c(Ne, 3)
Both<-data.frame(O=O1, C=C1, E=E1, A=A1, N=N1)
bmp("I:/End of Study Feedback/Finalized Feedbacks/Person.bmp", width=8, height=2.5, units="in", res=100, pointsize=8)
barplot(as.matrix(Both), xlab=" ", ylab=" ", ylim=range(0:5), beside=TRUE, col=topo.colors(2),
names.arg=c(" ", " ",
" ", " ", " "), border="black")
box()
dev.off()



####END OF IND FB##############












####Intensity of Physical Activity HLM manuscript###

Intense1 <-read.csv("R:\\Data analysis\\Intensity of PA\\intenseHLM.csv", stringsAsFactors=FALSE)
library(gdata)
intense2<- rename.vars(Intense1, c("ID", "Intn_MILD","Intn_MOD",
                        "Intn_VIG", "sex", "LPAeff", "inten_Mild_c",
                        "inten_Mod_c", "inten_Vig_c",
                        "MildPA", "ModPA", "VigPA", "PC_MILD2", "PC_MOD2",
                        "PC_VIG2", "RhodesHabit", "RHabit_c",
                        "RHabitXint_MILD", "RHabitXint_MOD",
                        "RHabitXint_VIG"),
            c("id", "intn.3","intn.2", "intn.1", "sex", "PAeff", "intnC.3",
                        "intnC.2", "intnC.1", "PA.3", "PA.2", "PA.1", "PC.3", "PC.2",
                        "PC.1", "auto", "autoC",
                        "autoXint.3", "autoXint.2", "autoXint.1"),)

#converting wide data to long data (with between-person vars)#
                        
intense3 = reshape(intense2, direction="long",varying=2:16)
                        
#renaming variables#
                        
intense4<- rename.vars(intense3, c("time"), c("intensity"))

#exporting to SAS#

library(foreign)
write.foreign(intense4, "R:/Data analysis/Intensity of PA/intense_HLM.txt",
             "R:/Data analysis/Intensity of PA/intense_HLM.sas", package="SAS" )

library(lme4)
intPA <- lmer(PA ~ intn+PC+auto*intn+auto +
  (intensity| id), data=intense4)




############## All in one Scoring the SC-IATs with only correct response times##########
####Computer 3 and 4 diffusion model (with Interquartile cut-offs)###############################################

library(psych)

##making list of all files to run##
setwd("R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/INS1/simple") 
File <- list.files("R:\\Fall 2011 KINES 321 Eval Prim II\\Data\\SCIAT data\\INS1\\simple")

# import csv-files
library(plyr)
SCIAT_PAa <- mdply(File, read.csv, as.is=T)


##organizing new data files into separated based on correct, block, etc.##
SCIAT_PA <-SCIAT_PAa[which(SCIAT_PAa$Block==2|SCIAT_PAa$Block==4), ]
SCIAT_PA$Block4 <-ifelse(SCIAT_PA$Block ==4, 1,0)
SCIAT_PA$Block2 <-ifelse(SCIAT_PA$Block ==2, 1,0)
Subj<-tapply(SCIAT_PA$Subj, SCIAT_PA$Subj,mean)
Subja<-data.frame(Subj)
BW<-data.frame(Subj=Subja$Subj)  

SCIAT_PA$rtlow <-ifelse (SCIAT_PA$RT > 299, c(SCIAT_PA$RT), c(NA))
SCIAT_PA$rtnohi <-ifelse (SCIAT_PA$rtlow < 10000, c(SCIAT_PA$rtlow), c(NA))
library(VIM)
missing_PA <-tapply(SCIAT_PA$rtlow, SCIAT_PA$Subj, countNA)
tenK_PA<-tapply(SCIAT_PA$rtnohi, SCIAT_PA$Subj, countNA)
missing_PA<-data.frame(missing_PA)
missing_PA$percLow <-missing_PA$missing_PA/144
missing_PA$Subj<-BW$Subj

da2 <- merge(SCIAT_PA,missing_PA,by="Subj")

da2$rtokay <-ifelse(da2$percLow<0.10, c(da2$rtnohi), c(NA))
da2$RT1<-ifelse(da2$Block==2, c(da2$RT), c(NA))
da2$RT2<-ifelse(da2$Block==4, c(da2$RT), c(NA))
Mean_RT1<-tapply(da2$RT1,da2$Subj,mean, na.rm=TRUE)
Mean_RT2<-tapply(da2$RT2, da2$Subj,mean,na.rm = TRUE)
SD<-tapply(da2$RT,da2$Subj,sd, na.rm = TRUE)

Mean_RT1<-data.frame(Mean_RT1)
Mean_RT1$Subj<-BW$Subj
Mean_RT2<-data.frame(Mean_RT2)
Mean_RT2$Subj<-BW$Subj
SD<-data.frame(SD)
SD$Subj<-BW$Subj

BWda<-merge(Mean_RT1, Mean_RT2, by="Subj")
BWd<-merge(BWda, SD, by="Subj")
BWd$Diff<-BWd$Mean_RT2-BWd$Mean_RT1
BWd$d<-BWd$Diff/BWd$SD

#reliability#
da2$RT11<-ifelse(da2$Trial < 36, c(da2$RT2), c(NA)) 
da2$RT12<-ifelse(da2$Trial > 36, c(da2$RT2), c(NA))
da2$RT21<-ifelse(da2$Trial < 36, c(da2$RT1), c(NA))
da2$RT22<-ifelse(da2$Trial > 36, c(da2$RT1), c(NA))

Mean_RT11<-tapply(da2$RT11, da2$Subj, mean, na.rm = TRUE)
Mean_RT12<-tapply(da2$RT12, da2$Subj, mean, na.rm = TRUE)
Mean_RT21<-tapply(da2$RT21, da2$Subj, mean, na.rm = TRUE)
Mean_RT22<-tapply(da2$RT22, da2$Subj, mean, na.rm = TRUE)

SD_RT11<-tapply(da2$RT11, da2$Subj, sd, na.rm = TRUE)
SD_RT12<-tapply(da2$RT12, da2$Subj, sd, na.rm = TRUE)
SD_RT21<-tapply(da2$RT21, da2$Subj, sd, na.rm = TRUE)
SD_RT22<-tapply(da2$RT22, da2$Subj, sd, na.rm = TRUE)



BWreld<-data.frame(BWd$Subj)
BWreld$Mean_RT11<-Mean_RT11
BWreld$Mean_RT12<-Mean_RT12
BWreld$Mean_RT21<-Mean_RT21
BWreld$Mean_RT22<-Mean_RT22
BWreld$SD_RT11<-SD_RT11
BWreld$SD_RT12<-SD_RT12
BWreld$SD_RT21<-SD_RT21
BWreld$SD_RT22<-SD_RT22
BWreld$SD_rel1<-(BWreld$SD_RT11+BWreld$SD_RT12)/2
BWreld$SD_rel2<-(BWreld$SD_RT21+BWreld$SD_RT22)/2
BWreld$Diff1<-BWreld$Mean_RT12-BWreld$Mean_RT11
BWreld$Diff2<-BWreld$Mean_RT22-BWreld$Mean_RT21
BWreld$relD1<-BWreld$Diff1/BWreld$SD_rel1
BWreld$relD2<-BWreld$Diff2/BWreld$SD_rel2


BWreldx<-data.frame(BWd$Subj)
BWreldx$relD1<-BWreld$relD1
BWreldx$relD2<-BWreld$relD2
BWreldx$Subj<-BWd$Subj

BW<-merge(BWd, BWreldx, by="Subj")
library(gdata)
BW<-remove.vars(BW, names="BWd.Subj")

#making values for EZ2 model#
#using same outlier cuts as in Klauer et al (2007)#
da2$rtlow2<-ifelse (da2$RT > 100, c(da2$RT), c(NA))
da2$RT1_EZ<-ifelse(da2$Block==2, c(da2$rtlow2), c(NA))
da2$RT2_EZ<-ifelse(da2$Block==4, c(da2$rtlow2), c(NA))

da2$prop<-ifelse(da2$Correct =="False", c(1), c(0))
da2$prop1<-ifelse(da2$Block==2, c(da2$prop), c(NA))
da2$prop2<-ifelse(da2$Block==4, c(da2$prop), c(NA))

da2$realSmall<-ifelse(da2$RT < 100, c(1), c(0))
tooTiny<-tapply(da2$realSmall,da2$Subj, sum, na.rm=TRUE)
tooTiny<-data.frame(tooTiny)
tooTiny$Subj<-BW$Subj
BWx<-merge(BW,tooTiny,by="Subj")
BW<-BWx

IQR1<-tapply(da2$RT1_EZ, da2$Subj, IQR, na.rm = TRUE)
quartiles1<-tapply(da2$RT1_EZ, da2$Subj, quantile, na.rm= TRUE)
quartiles1x<-as.character(quartiles1)
quart1xx<-unlist(strsplit(quartiles1x, ","))
quartiles1x <- matrix(quart1xx, nrow = 10, ncol=5, byrow=T)
zoo<-data.frame(quartiles1x)
zoo$Subj<-BW$Subj
names(zoo)[2] <- "Q25"
names(zoo)[4] <- "Q75"
zoo$Q25<-as.character(zoo$Q25)
zoo$Q75<-as.character(zoo$Q75)
zoo$Q25<-as.numeric(zoo$Q25)
zoo$Q75<-as.numeric(zoo$Q75)
zoo$IQR1<-IQR1
zoo$Q3and1<-(zoo$Q75+(1.5*zoo$IQR1))
zoo$Q1and1<-(zoo$Q25-(1.5*zoo$IQR1))
zoo2<-merge(zoo,da2, by="Subj")
zoo2$tooBig1<-ifelse(zoo2$RT1_EZ < zoo2$Q3and1, c(0), c(1))
zoo2$tooLit1<-ifelse(zoo2$RT1_EZ > zoo2$Q1and1, c(0), c(1))
tooLittle1<-tapply(zoo2$tooLit1,zoo2$Subj,sum, na.rm=TRUE)
tooBig1<-tapply(zoo2$tooBig1,zoo2$Subj,sum, na.rm=TRUE)
tooLittle1<-data.frame(tooLittle1)
tooLittle1$tooBig1<-tooBig1
tooLittle1$Subj<-BW$Subj
BWx<-merge(BW, tooLittle1, by="Subj")
BW<-BWx

zoo2$rtQ31<-ifelse (zoo2$RT1_EZ < zoo2$Q3and1, c(zoo2$RT1_EZ), c(NA))
zoo2$RTz1<-ifelse (zoo2$rtQ31 > zoo2$Q1and1, c(zoo2$rtQ31), c(NA))
zoo2$RTCor1<-ifelse (zoo2$Correct =="True", c(zoo2$RTz1), c(NA))
varCor1<-tapply(zoo2$RTCor1,zoo2$Subj,var, na.rm=TRUE)
prop1a<-tapply(zoo2$prop1, zoo2$Subj, sum,na.rm = TRUE)
MCor1<-tapply(zoo2$RTCor1,zoo2$Subj,mean, na.rm=TRUE)
prop1a<-data.frame(prop1a)
prop1a$Subj<-BW$Subj
prop1a$prop1b<-prop1a$prop1a/72
prop1a$prop1<-ifelse(prop1a$prop1b>.01,prop1a$prop1b,(.5/72))
prop1a$MCor1<-MCor1
prop1a$varCor1<-varCor1
BWx<-merge(BW,prop1a,by="Subj")
BW<-BWx
BW<-remove.vars(BW,names=c("prop1b","prop1a" ))

IQR2<-tapply(da2$RT2_EZ, da2$Subj, IQR, na.rm = TRUE)
quartiles2<-tapply(da2$RT2_EZ, da2$Subj, quantile, na.rm= TRUE)
quartiles2x<-as.character(quartiles2)
quart2xx<-unlist(strsplit(quartiles2x, ","))
quartiles2x <- matrix(quart2xx, nrow = 10, ncol=5, byrow=T)
zoox2<-data.frame(quartiles2x)
zoox2$Subj<-BW$Subj
names(zoox2)[2] <- "Q25"
names(zoox2)[4] <- "Q75"
zoox2$Q25<-as.character(zoox2$Q25)
zoox2$Q75<-as.character(zoox2$Q75)
zoox2$Q25<-as.numeric(zoox2$Q25)
zoox2$Q75<-as.numeric(zoox2$Q75)
zoox2$IQR2<-IQR2
zoox2$Q3and1<-(zoox2$Q75+(1.5*zoox2$IQR2))
zoox2$Q1and1<-(zoox2$Q25-(1.5*zoox2$IQR2))
zoox222<-merge(zoox2,da2, by="Subj")
zoox222$tooBig2<-ifelse(zoox222$RT2_EZ < zoox222$Q3and1, c(0), c(1))
zoox222$tooLit2<-ifelse(zoox222$RT2_EZ > zoox222$Q1and1, c(0), c(1))
tooLittle2<-tapply(zoox222$tooLit2,zoox222$Subj,sum, na.rm=TRUE)
tooBig2<-tapply(zoox222$tooBig2,zoox222$Subj,sum, na.rm=TRUE)
tooLittle2<-data.frame(tooLittle2)
tooLittle2$tooBig2<-tooBig2
tooLittle2$Subj<-BW$Subj
BWx<-merge(BW, tooLittle2, by="Subj")
BW<-BWx

zoox222$rtQ31<-ifelse (zoox222$RT2_EZ < zoox222$Q3and1, c(zoox222$RT2_EZ), c(NA))
zoox222$RTz2<-ifelse (zoox222$rtQ31 > zoox222$Q1and1, c(zoox222$rtQ31), c(NA))
zoox222$RTCor2<-ifelse (zoox222$Correct =="True", c(zoox222$RTz2), c(NA))
varCor2<-tapply(zoox222$RTCor2,zoox222$Subj,var, na.rm=TRUE)
prop2a<-tapply(zoox222$prop2, zoox222$Subj, sum,na.rm = TRUE)
MCor2<-tapply(zoox222$RTCor2,zoox222$Subj,mean, na.rm=TRUE)
prop2a<-data.frame(prop2a)
prop2a$Subj<-BW$Subj
prop2a$prop2b<-prop2a$prop2a/72
prop2a$prop2<-ifelse(prop2a$prop2b>.01,prop2a$prop2b,(.5/72))
prop2a$MCor2<-MCor2
prop2a$varCor2<-varCor2
BWx<-merge(BW,prop2a,by="Subj")
BW<-BWx
BW<-remove.vars(BW,names=c("prop2b","prop2a" ))


##d-score with Klauer et al outlier cuts##
SDz1K<-tapply(zoo2$RTz1,zoo2$Subj,sd, na.rm=T)
SDz2K<-tapply(zoox222$RTz2, zoox222$Subj,sd, na.rm=T)
SD<-data.frame(SDz1K)
SD$SDz2K<-SDz2K
SD$Subj<-BW$Subj
SD$SDK<-(SD$SDz1K + SD$SDz2K)/2

Mz1K<-tapply(zoo2$RTz1, zoo2$Subj,mean, na.rm=T)
Mz2K<-tapply(zoox222$RTz2,zoox222$Subj,mean, na.rm=T) 
Me<-data.frame(Mz1K)
Me$Mz2K<-Mz2K
Me$Subj<-BW$Subj
Me$MeanK<-(Me$Mz1K + Me$Mz2K)/2
SDMe<-merge(SD,Me,by="Subj")
SDMe$dK<-(SDMe$Mz2K-SDMe$Mz1K)/SDMe$SDK
BWx<-merge(SDMe,BW,by="Subj")
BW<-remove.vars(BWx, names=c("SDz1K", "SDz2K", "SDK", "Mz1K", "Mz2K", "MeanK"))


#reliability#
zoo2$RT11<-ifelse(zoo2$Trial < 36, c(zoo2$RT2_EZ), c(NA)) 
zoo2$RT12<-ifelse(zoo2$Trial > 36, c(zoo2$RT2_EZ), c(NA))
zoo2$RT21<-ifelse(zoo2$Trial < 36, c(zoo2$RT1_EZ), c(NA))
zoo2$RT22<-ifelse(zoo2$Trial > 36, c(zoo2$RT1_EZ), c(NA))

Mean_RT11<-tapply(zoo2$RT11, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT12<-tapply(zoo2$RT12, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT21<-tapply(zoo2$RT21, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT22<-tapply(zoo2$RT22, zoo2$Subj, mean, na.rm = TRUE)

SD_RT11<-tapply(zoo2$RT11, zoo2$Subj, sd, na.rm = TRUE)
SD_RT12<-tapply(zoo2$RT12, zoo2$Subj, sd, na.rm = TRUE)
SD_RT21<-tapply(zoo2$RT21, zoo2$Subj, sd, na.rm = TRUE)
SD_RT22<-tapply(zoo2$RT22, zoo2$Subj, sd, na.rm = TRUE)



BWreld<-data.frame(BWd$Subj)
BWreld$Mean_RT11<-Mean_RT11
BWreld$Mean_RT12<-Mean_RT12
BWreld$Mean_RT21<-Mean_RT21
BWreld$Mean_RT22<-Mean_RT22
BWreld$SD_RT11<-SD_RT11
BWreld$SD_RT12<-SD_RT12
BWreld$SD_RT21<-SD_RT21
BWreld$SD_RT22<-SD_RT22
BWreld$SD_rel1<-(BWreld$SD_RT11+BWreld$SD_RT12)/2
BWreld$SD_rel2<-(BWreld$SD_RT21+BWreld$SD_RT22)/2
BWreld$Diff1z<-BWreld$Mean_RT12-BWreld$Mean_RT11
BWreld$Diff2z<-BWreld$Mean_RT22-BWreld$Mean_RT21
BWreld$relD1z<-BWreld$Diff1z/BWreld$SD_rel1
BWreld$relD2z<-BWreld$Diff2z/BWreld$SD_rel2


BWreldx<-data.frame(BWd$Subj)
BWreldx$relD1z<-BWreld$relD1z
BWreldx$relD2z<-BWreld$relD2z
BWreldx$Subj<-BWd$Subj

BW<-merge(BW, BWreldx, by="Subj")
library(gdata)
BW<-remove.vars(BW, names="BWd.Subj")



#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

BW$s2 = .01
# The default value for the scaling parameter s equals .1

# If Pc equals 0, .5, or 1, the method will not work, and
# an edge-correction is required.

BW$Pc1 = 1-BW$prop1
BW$Pc2 = 1-BW$prop2

BW$VRT1 =varCor1/1000000
BW$VRT2 =varCor2/1000000

BW$L1 = qlogis(BW$Pc1)
BW$L2 = qlogis(BW$Pc2)
# The function "qlogis" calculates the logit.

BW$x1 = BW$L1*(BW$L1*BW$Pc1^2 - BW$L1*BW$Pc1 + BW$Pc1 - 0.5)/BW$VRT1
BW$v1 = sign(BW$Pc1-0.5)*.1*BW$x1^(1/4)
BW$x2 = BW$L2*(BW$L2*BW$Pc2^2 - BW$L2*BW$Pc2 + BW$Pc2 - 0.5)/BW$VRT2
BW$v2 = sign(BW$Pc2-0.5)*.1*BW$x2^(1/4)
# This gives drift rate.

BW$a1 = BW$s2*qlogis(BW$Pc1)/BW$v1
BW$a2 = BW$s2*qlogis(BW$Pc2)/BW$v2
# This gives boundary separation.

BW$y1 = -1*BW$v1*BW$a1/BW$s2
BW$y2 = -1*BW$v2*BW$a2/BW$s2

BW$MRT1 = MCor1/1000
BW$MRT2 = MCor2/1000


BW$MDT1 = (BW$a1/(2*BW$v1))*(1-exp(BW$y1))/(1+exp(BW$y1))
BW$Ter1 = BW$MRT1-BW$MDT1
BW$MDT2 = (BW$a2/(2*BW$v2))*(1-exp(BW$y2))/(1+exp(BW$y2))
BW$Ter2 = BW$MRT2-BW$MDT2
# This gives nondecision time.

#printing values we need#

write.table(BW, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/INSsimple.csv", sep = ",", col.names = NA,
            qmethod = "double")







########################### Exploring EZ data ##########################

EZ_IAT <-read.csv("R:\\Data analysis\\EZ2 imp atts\\Scored data\\PA SCIAT data Fall Project 2011.csv", na.strings="NA", stringsAsFactors=FALSE)
library(psych)
EZ_IAT$v_diff <-EZ_IAT$v2 - EZ_IAT$v1
EZ_IAT$a_diff <-EZ_IAT$a2 - EZ_IAT$a1
EZ_IAT$Ter_diff <-EZ_IAT$Ter2 - EZ_IAT$Ter1
describe(EZ_IAT)
pairs.panels(EZ_IAT[3:12])
pairs.panels(EZ_IAT[3 and 10:15])

hist(EZ_IAT$d)
mean(EZ_IAT$d, na.rm = TRUE)
sd(EZ_IAT$d, na.rm = TRUE)


mean(EZ_IAT$v_diff, na.rm = TRUE)
sd(EZ_IAT$v_diff, na.rm = TRUE)
hist(EZ_IAT$v_diff)

mean(EZ_IAT$a_diff, na.rm = TRUE)
sd(EZ_IAT$a_diff, na.rm = TRUE)
hist(EZ_IAT$a_diff)

mean(EZ_IAT$Ter_diff, na.rm = TRUE)
sd(EZ_IAT$Ter_diff, na.rm = TRUE)
hist(EZ_IAT$Ter_diff)


PhysAct <-read.csv("R:\\Fall 2011 KINES 422-428 Class Projects\\422\\Data\\EZ test.csv", na.strings="NA", stringsAsFactors=FALSE)

PhysAct$ID2<-PhysAct$ID + (1000*PhysAct$visit)
EZ_IAT$ID2<-EZ_IAT$ID + (1000*EZ_IAT$visit)

EZPA <-merge(EZ_IAT, PhysAct, BY="ID2")

EZPA$viga<-(EZPA$IPAQ_02_1*60)
EZPA$vigb<-EZPA$viga+EZPA$IPAQ_02_2
EZPA$vig<-(EZPA$IPAQ_01*EZPA$vigb)

EZPA$moda<-(EZPA$IPAQ_04_1*60)
EZPA$modb<-EZPA$moda+EZPA$IPAQ_04_2
EZPA$mod<-(EZPA$IPAQ_03*EZPA$modb)

EZPA$walka<-(EZPA$IPAQ_06_1*60)
EZPA$walkb<-EZPA$walka+EZPA$IPAQ_06_2
EZPA$walk<-(EZPA$IPAQ_05*EZPA$walkb)

EZPA$metmin2<-(3.3*EZPA$walk + 4*EZPA$mod + 8*EZPA$vig)

EZPA$metmin<-ifelse(EZPA$metmin2 <15000, c(EZPA$metmin2), c(NA))


cor(v2, EZPA$vig, use="pairwise.complete.obs")
cor(v2, EZPA$mod, use="pairwise.complete.obs")
cor(v2, EZPA$walk, use="pairwise.complete.obs")



attach(EZPA)
plot(MCor1, metmin, main="Scatterplot Example",
   xlab="drift rate difference ", ylab="PA", pch=19) 

###################Focusing by item and by catgory#################################

items <-read.csv("R:\\Data analysis\\EZ2 imp atts\\all_PA1.csv", na.strings="NA", stringsAsFactors=FALSE)

betray<- items[ which(items$Stim=="betray"), ]
bike<- items[ which(items$Stim=="bike"), ]
bless<- items[ which(items$Stim=="bless"), ]
brave<- items[ which(items$Stim=="brave"), ]
carry<- items[ which(items$Stim=="carry"), ]
cozy<- items[ which(items$Stim=="cozy"), ]
danger<- items[ which(items$Stim=="danger"), ]
deformed<- items[ which(items$Stim=="deformed"), ]
desire<- items[ which(items$Stim=="desire"), ]
disgusting<- items[ which(items$Stim=="disgusting"), ]
dreary<- items[ which(items$Stim=="dreary"), ]
excitement<- items[ which(items$Stim=="excitement"), ]
fireworks<- items[ which(items$Stim=="fireworks"), ]
friendly<- items[ which(items$Stim=="friendly"), ]
glee<- items[ which(items$Stim=="glee"), ]
gloom<- items[ which(items$Stim=="gloom"), ]
inferior<- items[ which(items$Stim=="inferior"), ]
jog<- items[ which(items$Stim=="jog"), ]
jump<- items[ which(items$Stim=="jump"), ]
lift<- items[ which(items$Stim=="lift"), ]
loneliness<- items[ which(items$Stim=="loneliness"), ]
miracle<- items[ which(items$Stim=="miracle"), ]
move<- items[ which(items$Stim=="move"), ]
nightmare<- items[ which(items$Stim=="nightmare"), ]
passion<- items[ which(items$Stim=="passion"), ]
play<- items[ which(items$Stim=="play"), ]
protected<- items[ which(items$Stim=="protected"), ]
pull<- items[ which(items$Stim=="pull"), ]
stand<- items[ which(items$Stim=="stand"), ]
stretch<- items[ which(items$Stim=="stretch"), ]
sunset<- items[ which(items$Stim=="sunset"), ]
thief<- items[ which(items$Stim=="thief"), ]
throw<- items[ which(items$Stim=="throw"), ]
tragic<- items[ which(items$Stim=="tragic"), ]
walk<- items[ which(items$Stim=="walk"), ]
waste<- items[ which(items$Stim=="waste"), ]

GOOD<-rbind(cozy, brave, fireworks, friendly, passion, desire, miracle, sunset, glee, protected, bless, excitement)
BAD<-rbind(inferior, waste, dreary, deformed, tragic, danger, thief, nightmare, betray, loneliness, gloom, disgusting)
PHYSACT<-rbind(walk, jog, bike, move, stand, lift, carry, pull, stretch, play, jump, throw)


LEFT<- items[ which(items$Resp==1), ]
RIGHT<- items[ which(items$Resp==9), ]


##Scoring them by categories (I haven't done the by item yet)###

BAD$rtlow <-ifelse (BAD$RT > 299, c(BAD$RT), c(NA))
BAD$rtnohi <-ifelse (BAD$rtlow < 10000, c(BAD$rtlow), c(NA))
missing_PA <-sum(is.na(BAD$rtlow))
BAD$percLow <-missing_PA/192
perclow<-missing_PA/192
BAD$rtokay <-ifelse(BAD$percLow<0.10, c(BAD$rtnohi), c(NA))
BAD$RT1<-ifelse(BAD$Block==2, c(BAD$rtokay), c(NA))
BAD$RT2<-ifelse(BAD$Block==4, c(BAD$rtokay), c(NA))
Mean_RT1<-mean(BAD$RT1, na.rm = TRUE)
Mean_RT2<-mean(BAD$RT2, na.rm = TRUE)
SD<-sd(BAD$rtokay, na.rm = TRUE)
Diff<-Mean_RT2-Mean_RT1
d<-Diff/SD

#making values for EZ2 model#
#using same outlier cuts as in Klauer et al (2007)#

BAD$rtlow2<-ifelse (BAD$RT > 100, c(BAD$RT), c(NA))
BAD$RT1_EZ<-ifelse(BAD$Block<3, c(BAD$rtlow2), c(NA))
BAD$RT2_EZ<-ifelse(BAD$Block>2, c(BAD$rtlow2), c(NA))
BAD$prop<-ifelse(BAD$Correct =="FALSE", c(1), c(0))
BAD$prop1<-ifelse(BAD$Block<3, c(BAD$prop), c(NA))
BAD$prop2<-ifelse(BAD$Block>2, c(BAD$prop), c(NA))
BAD$realSmall<-ifelse(BAD$RT < 100, c(1), c(0))
tooTiny<-sum(BAD$realSmall, na.rm=TRUE)

IQR1<-IQR(BAD$RT1_EZ, na.rm = TRUE)
quartiles1<-quantile(BAD$RT1_EZ, na.rm= TRUE)
quart751<-quartiles1[4]
Q3and1<-quart751+(1.5*IQR1)
quart251<-quartiles1[2]
Q1and1<-quart251-(1.5*IQR1)
BAD$tooBig1<-ifelse(BAD$RT1_EZ < Q3and1, c(0), c(1))
BAD$tooLit1<-ifelse(BAD$RT1_EZ > Q1and1, c(0), c(1))
tooLittle1<-sum(BAD$tooLit1, na.rm=TRUE)
tooBig1<-sum(BAD$tooBig1, na.rm=TRUE)
BAD$rtQ31<-ifelse (BAD$RT1_EZ < Q3and1, c(BAD$RT1_EZ), c(NA))
BAD$RTz1<-ifelse (BAD$rtQ31 > Q1and1, c(BAD$rtQ31), c(NA))
BAD$RTCor1<-ifelse (BAD$Correct =="TRUE", c(BAD$RTz1), c(NA))
varCor1<-var(BAD$RTCor1, na.rm=TRUE)
prop1a<-sum(BAD$prop1, na.rm = TRUE)
prop1<-prop1a/96
MCor1<-mean(BAD$RTCor1, na.rm=TRUE)

IQR2<-IQR(BAD$RT2_EZ, na.rm = TRUE)
quartiles2<-quantile(BAD$RT2_EZ, na.rm= TRUE)
quart752<-quartiles2[4]
Q3and2<-quart752+(1.5*IQR2)
quart252<-quartiles2[2]
Q1and2<-quart252-(1.5*IQR2)
BAD$tooBig2<-ifelse(BAD$RT2_EZ < Q3and2, c(0), c(1))
BAD$tooLit2<-ifelse(BAD$RT2_EZ > Q1and2, c(0), c(1))
tooLittle2<-sum(BAD$tooLit2, na.rm=TRUE)
tooBig2<-sum(BAD$tooBig2, na.rm=TRUE)
BAD$rtQ32<-ifelse (BAD$RT2_EZ < Q3and2, c(BAD$RT2_EZ), c(NA))
BAD$RTz2<-ifelse (BAD$rtQ32 > Q1and2, c(BAD$rtQ32), c(NA))
BAD$RTCor2<-ifelse (BAD$Correct =="TRUE", c(BAD$RTz2), c(NA))
varCor2<-var(BAD$RTCor2, na.rm=TRUE)
prop2a<-sum(BAD$prop2, na.rm = TRUE)
prop2<-prop2a/96
MCor2<-mean(BAD$RTCor2, na.rm=TRUE)


#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

get.vaTer = function(Pc, VRT, MRT, s=.1)
{
    s2 = s^2
    # The default value for the scaling parameter s equals .1

    if (Pc == 0)
        cat("Oops, Pc == 0!\n")
    if (Pc == 0.5)
        cat("Oops, Pc == .5!\n")
    if (Pc == 1)
        cat("Oops, Pc == 1!\n")
    # If Pc equals 0, .5, or 1, the method will not work, and
    # an edge-correction is required.

    L = qlogis(Pc)
    # The function "qlogis" calculates the logit.
    x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
    v = sign(Pc-0.5)*s*x^(1/4)
    # This gives drift rate.

    a = s2*qlogis(Pc)/v
    # This gives boundary separation.

    y   = -v*a/s2
    MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
    Ter = MRT-MDT
    # This gives nondecision time.

    return(list(v, a, Ter))
}

EZa1<-get.vaTer(prop1,(varCor1/1000000),(MCor1/1000), s = 0.1)
EZ1 <- data.frame(matrix(unlist(EZa1), nrow=1, byrow=3))
v1<-(EZ1$X1)
a1<-(EZ1$X2)
Ter1<-(EZ1$X3)

EZa2<-get.vaTer(prop2,(varCor2/1000000),(MCor2/1000), s = 0.1)
EZ2 <- data.frame(matrix(unlist(EZa2), nrow=1, byrow=3))
v2<-(EZ2$X1)
a2<-(EZ2$X2)
Ter2<-(EZ2$X3)

#printing values we need#
victory1<-c("BAD",999,d,perclow,MCor1,MCor2,varCor1, varCor2,
           prop1, prop2, tooBig1, tooBig2,
           tooLittle1, tooLittle2,
           tooTiny, v1,
            v2,a1,a2,Ter1,Ter2)
victory <- data.frame(matrix(unlist(victory1), nrow=1, byrow=55))

write.table(victory, file = "R:/Data Analysis/EZ2 imp atts/Scored data/Scored PA Data/BAD.csv", sep = ",", col.names = NA,
           qmethod = "double")




GOOD$rtlow <-ifelse (GOOD$RT > 299, c(GOOD$RT), c(NA))
GOOD$rtnohi <-ifelse (GOOD$rtlow < 10000, c(GOOD$rtlow), c(NA))
missing_PA <-sum(is.na(GOOD$rtlow))
GOOD$percLow <-missing_PA/192
perclow<-missing_PA/192
GOOD$rtokay <-ifelse(GOOD$percLow<0.10, c(GOOD$rtnohi), c(NA))
GOOD$RT1<-ifelse(GOOD$Block==2, c(GOOD$rtokay), c(NA))
GOOD$RT2<-ifelse(GOOD$Block==4, c(GOOD$rtokay), c(NA))
Mean_RT1<-mean(GOOD$RT1, na.rm = TRUE)
Mean_RT2<-mean(GOOD$RT2, na.rm = TRUE)
SD<-sd(GOOD$rtokay, na.rm = TRUE)
Diff<-Mean_RT2-Mean_RT1
d<-Diff/SD

#making values for EZ2 model#
#using same outlier cuts as in Klauer et al (2007)#

GOOD$rtlow2<-ifelse (GOOD$RT > 100, c(GOOD$RT), c(NA))
GOOD$RT1_EZ<-ifelse(GOOD$Block<3, c(GOOD$rtlow2), c(NA))
GOOD$RT2_EZ<-ifelse(GOOD$Block>2, c(GOOD$rtlow2), c(NA))
GOOD$prop<-ifelse(GOOD$Correct =="FALSE", c(1), c(0))
GOOD$prop1<-ifelse(GOOD$Block<3, c(GOOD$prop), c(NA))
GOOD$prop2<-ifelse(GOOD$Block>2, c(GOOD$prop), c(NA))
GOOD$realSmall<-ifelse(GOOD$RT < 100, c(1), c(0))
tooTiny<-sum(GOOD$realSmall, na.rm=TRUE)

IQR1<-IQR(GOOD$RT1_EZ, na.rm = TRUE)
quartiles1<-quantile(GOOD$RT1_EZ, na.rm= TRUE)
quart751<-quartiles1[4]
Q3and1<-quart751+(1.5*IQR1)
quart251<-quartiles1[2]
Q1and1<-quart251-(1.5*IQR1)
GOOD$tooBig1<-ifelse(GOOD$RT1_EZ < Q3and1, c(0), c(1))
GOOD$tooLit1<-ifelse(GOOD$RT1_EZ > Q1and1, c(0), c(1))
tooLittle1<-sum(GOOD$tooLit1, na.rm=TRUE)
tooBig1<-sum(GOOD$tooBig1, na.rm=TRUE)
GOOD$rtQ31<-ifelse (GOOD$RT1_EZ < Q3and1, c(GOOD$RT1_EZ), c(NA))
GOOD$RTz1<-ifelse (GOOD$rtQ31 > Q1and1, c(GOOD$rtQ31), c(NA))
GOOD$RTCor1<-ifelse (GOOD$Correct =="TRUE", c(GOOD$RTz1), c(NA))
varCor1<-var(GOOD$RTCor1, na.rm=TRUE)
prop1a<-sum(GOOD$prop1, na.rm = TRUE)
prop1<-prop1a/96
MCor1<-mean(GOOD$RTCor1, na.rm=TRUE)

IQR2<-IQR(GOOD$RT2_EZ, na.rm = TRUE)
quartiles2<-quantile(GOOD$RT2_EZ, na.rm= TRUE)
quart752<-quartiles2[4]
Q3and2<-quart752+(1.5*IQR2)
quart252<-quartiles2[2]
Q1and2<-quart252-(1.5*IQR2)
GOOD$tooBig2<-ifelse(GOOD$RT2_EZ < Q3and2, c(0), c(1))
GOOD$tooLit2<-ifelse(GOOD$RT2_EZ > Q1and2, c(0), c(1))
tooLittle2<-sum(GOOD$tooLit2, na.rm=TRUE)
tooBig2<-sum(GOOD$tooBig2, na.rm=TRUE)
GOOD$rtQ32<-ifelse (GOOD$RT2_EZ < Q3and2, c(GOOD$RT2_EZ), c(NA))
GOOD$RTz2<-ifelse (GOOD$rtQ32 > Q1and2, c(GOOD$rtQ32), c(NA))
GOOD$RTCor2<-ifelse (GOOD$Correct =="TRUE", c(GOOD$RTz2), c(NA))
varCor2<-var(GOOD$RTCor2, na.rm=TRUE)
prop2a<-sum(GOOD$prop2, na.rm = TRUE)
prop2<-prop2a/96
MCor2<-mean(GOOD$RTCor2, na.rm=TRUE)


#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

get.vaTer = function(Pc, VRT, MRT, s=.1)
{
    s2 = s^2
    # The default value for the scaling parameter s equals .1

    if (Pc == 0)
        cat("Oops, Pc == 0!\n")
    if (Pc == 0.5)
        cat("Oops, Pc == .5!\n")
    if (Pc == 1)
        cat("Oops, Pc == 1!\n")
    # If Pc equals 0, .5, or 1, the method will not work, and
    # an edge-correction is required.

    L = qlogis(Pc)
    # The function "qlogis" calculates the logit.
    x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
    v = sign(Pc-0.5)*s*x^(1/4)
    # This gives drift rate.

    a = s2*qlogis(Pc)/v
    # This gives boundary separation.

    y   = -v*a/s2
    MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
    Ter = MRT-MDT
    # This gives nondecision time.

    return(list(v, a, Ter))
}

EZa1<-get.vaTer(prop1,(varCor1/1000000),(MCor1/1000), s = 0.1)
EZ1 <- data.frame(matrix(unlist(EZa1), nrow=1, byrow=3))
v1<-(EZ1$X1)
a1<-(EZ1$X2)
Ter1<-(EZ1$X3)

EZa2<-get.vaTer(prop2,(varCor2/1000000),(MCor2/1000), s = 0.1)
EZ2 <- data.frame(matrix(unlist(EZa2), nrow=1, byrow=3))
v2<-(EZ2$X1)
a2<-(EZ2$X2)
Ter2<-(EZ2$X3)

#printing values we need#
victory1<-c("GOOD",999,d,perclow,MCor1,MCor2,varCor1, varCor2,
           prop1, prop2, tooBig1, tooBig2,
           tooLittle1, tooLittle2,
           tooTiny, v1,
            v2,a1,a2,Ter1,Ter2)
victory <- data.frame(matrix(unlist(victory1), nrow=1, byrow=55))

write.table(victory, file = "R:/Data Analysis/EZ2 imp atts/Scored data/Scored PA Data/GOOD.csv", sep = ",", col.names = NA,
           qmethod = "double")



PHYSACT$rtlow <-ifelse (PHYSACT$RT > 299, c(PHYSACT$RT), c(NA))
PHYSACT$rtnohi <-ifelse (PHYSACT$rtlow < 10000, c(PHYSACT$rtlow), c(NA))
missing_PA <-sum(is.na(PHYSACT$rtlow))
PHYSACT$percLow <-missing_PA/192
perclow<-missing_PA/192
PHYSACT$rtokay <-ifelse(PHYSACT$percLow<0.10, c(PHYSACT$rtnohi), c(NA))
PHYSACT$RT1<-ifelse(PHYSACT$Block==2, c(PHYSACT$rtokay), c(NA))
PHYSACT$RT2<-ifelse(PHYSACT$Block==4, c(PHYSACT$rtokay), c(NA))
Mean_RT1<-mean(PHYSACT$RT1, na.rm = TRUE)
Mean_RT2<-mean(PHYSACT$RT2, na.rm = TRUE)
SD<-sd(PHYSACT$rtokay, na.rm = TRUE)
Diff<-Mean_RT2-Mean_RT1
d<-Diff/SD

#making values for EZ2 model#
#using same outlier cuts as in Klauer et al (2007)#

PHYSACT$rtlow2<-ifelse (PHYSACT$RT > 100, c(PHYSACT$RT), c(NA))
PHYSACT$RT1_EZ<-ifelse(PHYSACT$Block<3, c(PHYSACT$rtlow2), c(NA))
PHYSACT$RT2_EZ<-ifelse(PHYSACT$Block>2, c(PHYSACT$rtlow2), c(NA))
PHYSACT$prop<-ifelse(PHYSACT$Correct =="FALSE", c(1), c(0))
PHYSACT$prop1<-ifelse(PHYSACT$Block<3, c(PHYSACT$prop), c(NA))
PHYSACT$prop2<-ifelse(PHYSACT$Block>2, c(PHYSACT$prop), c(NA))
PHYSACT$realSmall<-ifelse(PHYSACT$RT < 100, c(1), c(0))
tooTiny<-sum(PHYSACT$realSmall, na.rm=TRUE)

IQR1<-IQR(PHYSACT$RT1_EZ, na.rm = TRUE)
quartiles1<-quantile(PHYSACT$RT1_EZ, na.rm= TRUE)
quart751<-quartiles1[4]
Q3and1<-quart751+(1.5*IQR1)
quart251<-quartiles1[2]
Q1and1<-quart251-(1.5*IQR1)
PHYSACT$tooBig1<-ifelse(PHYSACT$RT1_EZ < Q3and1, c(0), c(1))
PHYSACT$tooLit1<-ifelse(PHYSACT$RT1_EZ > Q1and1, c(0), c(1))
tooLittle1<-sum(PHYSACT$tooLit1, na.rm=TRUE)
tooBig1<-sum(PHYSACT$tooBig1, na.rm=TRUE)
PHYSACT$rtQ31<-ifelse (PHYSACT$RT1_EZ < Q3and1, c(PHYSACT$RT1_EZ), c(NA))
PHYSACT$RTz1<-ifelse (PHYSACT$rtQ31 > Q1and1, c(PHYSACT$rtQ31), c(NA))
PHYSACT$RTCor1<-ifelse (PHYSACT$Correct =="TRUE", c(PHYSACT$RTz1), c(NA))
varCor1<-var(PHYSACT$RTCor1, na.rm=TRUE)
prop1a<-sum(PHYSACT$prop1, na.rm = TRUE)
prop1<-prop1a/96
MCor1<-mean(PHYSACT$RTCor1, na.rm=TRUE)

IQR2<-IQR(PHYSACT$RT2_EZ, na.rm = TRUE)
quartiles2<-quantile(PHYSACT$RT2_EZ, na.rm= TRUE)
quart752<-quartiles2[4]
Q3and2<-quart752+(1.5*IQR2)
quart252<-quartiles2[2]
Q1and2<-quart252-(1.5*IQR2)
PHYSACT$tooBig2<-ifelse(PHYSACT$RT2_EZ < Q3and2, c(0), c(1))
PHYSACT$tooLit2<-ifelse(PHYSACT$RT2_EZ > Q1and2, c(0), c(1))
tooLittle2<-sum(PHYSACT$tooLit2, na.rm=TRUE)
tooBig2<-sum(PHYSACT$tooBig2, na.rm=TRUE)
PHYSACT$rtQ32<-ifelse (PHYSACT$RT2_EZ < Q3and2, c(PHYSACT$RT2_EZ), c(NA))
PHYSACT$RTz2<-ifelse (PHYSACT$rtQ32 > Q1and2, c(PHYSACT$rtQ32), c(NA))
PHYSACT$RTCor2<-ifelse (PHYSACT$Correct =="TRUE", c(PHYSACT$RTz2), c(NA))
varCor2<-var(PHYSACT$RTCor2, na.rm=TRUE)
prop2a<-sum(PHYSACT$prop2, na.rm = TRUE)
prop2<-prop2a/96
MCor2<-mean(PHYSACT$RTCor2, na.rm=TRUE)


#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

get.vaTer = function(Pc, VRT, MRT, s=.1)
{
    s2 = s^2
    # The default value for the scaling parameter s equals .1

    if (Pc == 0)
        cat("Oops, Pc == 0!\n")
    if (Pc == 0.5)
        cat("Oops, Pc == .5!\n")
    if (Pc == 1)
        cat("Oops, Pc == 1!\n")
    # If Pc equals 0, .5, or 1, the method will not work, and
    # an edge-correction is required.

    L = qlogis(Pc)
    # The function "qlogis" calculates the logit.
    x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
    v = sign(Pc-0.5)*s*x^(1/4)
    # This gives drift rate.

    a = s2*qlogis(Pc)/v
    # This gives boundary separation.

    y   = -v*a/s2
    MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
    Ter = MRT-MDT
    # This gives nondecision time.

    return(list(v, a, Ter))
}

EZa1<-get.vaTer(prop1,(varCor1/1000000),(MCor1/1000), s = 0.1)
EZ1 <- data.frame(matrix(unlist(EZa1), nrow=1, byrow=3))
v1<-(EZ1$X1)
a1<-(EZ1$X2)
Ter1<-(EZ1$X3)

EZa2<-get.vaTer(prop2,(varCor2/1000000),(MCor2/1000), s = 0.1)
EZ2 <- data.frame(matrix(unlist(EZa2), nrow=1, byrow=3))
v2<-(EZ2$X1)
a2<-(EZ2$X2)
Ter2<-(EZ2$X3)

#printing values we need#
victory1<-c("PHYSACT",999,d,perclow,MCor1,MCor2,varCor1, varCor2,
           prop1, prop2, tooBig1, tooBig2,
           tooLittle1, tooLittle2,
           tooTiny, v1,
            v2,a1,a2,Ter1,Ter2)
victory <- data.frame(matrix(unlist(victory1), nrow=1, byrow=55))

write.table(victory, file = "R:/Data Analysis/EZ2 imp atts/Scored data/Scored PA Data/PHYSACT.csv", sep = ",", col.names = NA,
           qmethod = "double")


RIGHT$rtlow <-ifelse (RIGHT$RT > 299, c(RIGHT$RT), c(NA))
RIGHT$rtnohi <-ifelse (RIGHT$rtlow < 10000, c(RIGHT$rtlow), c(NA))
missing_PA <-sum(is.na(RIGHT$rtlow))
RIGHT$percLow <-missing_PA/192
perclow<-missing_PA/192
RIGHT$rtokay <-ifelse(RIGHT$percLow<0.10, c(RIGHT$rtnohi), c(NA))
RIGHT$RT1<-ifelse(RIGHT$Block==2, c(RIGHT$rtokay), c(NA))
RIGHT$RT2<-ifelse(RIGHT$Block==4, c(RIGHT$rtokay), c(NA))
Mean_RT1<-mean(RIGHT$RT1, na.rm = TRUE)
Mean_RT2<-mean(RIGHT$RT2, na.rm = TRUE)
SD<-sd(RIGHT$rtokay, na.rm = TRUE)
Diff<-Mean_RT2-Mean_RT1
d<-Diff/SD

#making values for EZ2 model#
#using same outlier cuts as in Klauer et al (2007)#

RIGHT$rtlow2<-ifelse (RIGHT$RT > 100, c(RIGHT$RT), c(NA))
RIGHT$RT1_EZ<-ifelse(RIGHT$Block<3, c(RIGHT$rtlow2), c(NA))
RIGHT$RT2_EZ<-ifelse(RIGHT$Block>2, c(RIGHT$rtlow2), c(NA))
RIGHT$prop<-ifelse(RIGHT$Correct =="FALSE", c(1), c(0))
RIGHT$prop1<-ifelse(RIGHT$Block<3, c(RIGHT$prop), c(NA))
RIGHT$prop2<-ifelse(RIGHT$Block>2, c(RIGHT$prop), c(NA))
RIGHT$realSmall<-ifelse(RIGHT$RT < 100, c(1), c(0))
tooTiny<-sum(RIGHT$realSmall, na.rm=TRUE)

IQR1<-IQR(RIGHT$RT1_EZ, na.rm = TRUE)
quartiles1<-quantile(RIGHT$RT1_EZ, na.rm= TRUE)
quart751<-quartiles1[4]
Q3and1<-quart751+(1.5*IQR1)
quart251<-quartiles1[2]
Q1and1<-quart251-(1.5*IQR1)
RIGHT$tooBig1<-ifelse(RIGHT$RT1_EZ < Q3and1, c(0), c(1))
RIGHT$tooLit1<-ifelse(RIGHT$RT1_EZ > Q1and1, c(0), c(1))
tooLittle1<-sum(RIGHT$tooLit1, na.rm=TRUE)
tooBig1<-sum(RIGHT$tooBig1, na.rm=TRUE)
RIGHT$rtQ31<-ifelse (RIGHT$RT1_EZ < Q3and1, c(RIGHT$RT1_EZ), c(NA))
RIGHT$RTz1<-ifelse (RIGHT$rtQ31 > Q1and1, c(RIGHT$rtQ31), c(NA))
RIGHT$RTCor1<-ifelse (RIGHT$Correct =="TRUE", c(RIGHT$RTz1), c(NA))
varCor1<-var(RIGHT$RTCor1, na.rm=TRUE)
prop1a<-sum(RIGHT$prop1, na.rm = TRUE)
prop1<-prop1a/96
MCor1<-mean(RIGHT$RTCor1, na.rm=TRUE)

IQR2<-IQR(RIGHT$RT2_EZ, na.rm = TRUE)
quartiles2<-quantile(RIGHT$RT2_EZ, na.rm= TRUE)
quart752<-quartiles2[4]
Q3and2<-quart752+(1.5*IQR2)
quart252<-quartiles2[2]
Q1and2<-quart252-(1.5*IQR2)
RIGHT$tooBig2<-ifelse(RIGHT$RT2_EZ < Q3and2, c(0), c(1))
RIGHT$tooLit2<-ifelse(RIGHT$RT2_EZ > Q1and2, c(0), c(1))
tooLittle2<-sum(RIGHT$tooLit2, na.rm=TRUE)
tooBig2<-sum(RIGHT$tooBig2, na.rm=TRUE)
RIGHT$rtQ32<-ifelse (RIGHT$RT2_EZ < Q3and2, c(RIGHT$RT2_EZ), c(NA))
RIGHT$RTz2<-ifelse (RIGHT$rtQ32 > Q1and2, c(RIGHT$rtQ32), c(NA))
RIGHT$RTCor2<-ifelse (RIGHT$Correct =="TRUE", c(RIGHT$RTz2), c(NA))
varCor2<-var(RIGHT$RTCor2, na.rm=TRUE)
prop2a<-sum(RIGHT$prop2, na.rm = TRUE)
prop2<-prop2a/96
MCor2<-mean(RIGHT$RTCor2, na.rm=TRUE)


#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

get.vaTer = function(Pc, VRT, MRT, s=.1)
{
    s2 = s^2
    # The default value for the scaling parameter s equals .1

    if (Pc == 0)
        cat("Oops, Pc == 0!\n")
    if (Pc == 0.5)
        cat("Oops, Pc == .5!\n")
    if (Pc == 1)
        cat("Oops, Pc == 1!\n")
    # If Pc equals 0, .5, or 1, the method will not work, and
    # an edge-correction is required.

    L = qlogis(Pc)
    # The function "qlogis" calculates the logit.
    x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
    v = sign(Pc-0.5)*s*x^(1/4)
    # This gives drift rate.

    a = s2*qlogis(Pc)/v
    # This gives boundary separation.

    y   = -v*a/s2
    MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
    Ter = MRT-MDT
    # This gives nondecision time.

    return(list(v, a, Ter))
}

EZa1<-get.vaTer(prop1,(varCor1/1000000),(MCor1/1000), s = 0.1)
EZ1 <- data.frame(matrix(unlist(EZa1), nrow=1, byrow=3))
v1<-(EZ1$X1)
a1<-(EZ1$X2)
Ter1<-(EZ1$X3)

EZa2<-get.vaTer(prop2,(varCor2/1000000),(MCor2/1000), s = 0.1)
EZ2 <- data.frame(matrix(unlist(EZa2), nrow=1, byrow=3))
v2<-(EZ2$X1)
a2<-(EZ2$X2)
Ter2<-(EZ2$X3)

#printing values we need#
victory1<-c("RIGHT",999,d,perclow,MCor1,MCor2,varCor1, varCor2,
           prop1, prop2, tooBig1, tooBig2,
           tooLittle1, tooLittle2,
           tooTiny, v1,
            v2,a1,a2,Ter1,Ter2)
victory <- data.frame(matrix(unlist(victory1), nrow=1, byrow=55))

write.table(victory, file = "R:/Data Analysis/EZ2 imp atts/Scored data/Scored PA Data/RIGHT.csv", sep = ",", col.names = NA,
           qmethod = "double")


LEFT$rtlow <-ifelse (LEFT$RT > 299, c(LEFT$RT), c(NA))
LEFT$rtnohi <-ifelse (LEFT$rtlow < 10000, c(LEFT$rtlow), c(NA))
missing_PA <-sum(is.na(LEFT$rtlow))
LEFT$percLow <-missing_PA/192
perclow<-missing_PA/192
LEFT$rtokay <-ifelse(LEFT$percLow<0.10, c(LEFT$rtnohi), c(NA))
LEFT$RT1<-ifelse(LEFT$Block==2, c(LEFT$rtokay), c(NA))
LEFT$RT2<-ifelse(LEFT$Block==4, c(LEFT$rtokay), c(NA))
Mean_RT1<-mean(LEFT$RT1, na.rm = TRUE)
Mean_RT2<-mean(LEFT$RT2, na.rm = TRUE)
SD<-sd(LEFT$rtokay, na.rm = TRUE)
Diff<-Mean_RT2-Mean_RT1
d<-Diff/SD

#making values for EZ2 model#
#using same outlier cuts as in Klauer et al (2007)#

LEFT$rtlow2<-ifelse (LEFT$RT > 100, c(LEFT$RT), c(NA))
LEFT$RT1_EZ<-ifelse(LEFT$Block<3, c(LEFT$rtlow2), c(NA))
LEFT$RT2_EZ<-ifelse(LEFT$Block>2, c(LEFT$rtlow2), c(NA))
LEFT$prop<-ifelse(LEFT$Correct =="FALSE", c(1), c(0))
LEFT$prop1<-ifelse(LEFT$Block<3, c(LEFT$prop), c(NA))
LEFT$prop2<-ifelse(LEFT$Block>2, c(LEFT$prop), c(NA))
LEFT$realSmall<-ifelse(LEFT$RT < 100, c(1), c(0))
tooTiny<-sum(LEFT$realSmall, na.rm=TRUE)

IQR1<-IQR(LEFT$RT1_EZ, na.rm = TRUE)
quartiles1<-quantile(LEFT$RT1_EZ, na.rm= TRUE)
quart751<-quartiles1[4]
Q3and1<-quart751+(1.5*IQR1)
quart251<-quartiles1[2]
Q1and1<-quart251-(1.5*IQR1)
LEFT$tooBig1<-ifelse(LEFT$RT1_EZ < Q3and1, c(0), c(1))
LEFT$tooLit1<-ifelse(LEFT$RT1_EZ > Q1and1, c(0), c(1))
tooLittle1<-sum(LEFT$tooLit1, na.rm=TRUE)
tooBig1<-sum(LEFT$tooBig1, na.rm=TRUE)
LEFT$rtQ31<-ifelse (LEFT$RT1_EZ < Q3and1, c(LEFT$RT1_EZ), c(NA))
LEFT$RTz1<-ifelse (LEFT$rtQ31 > Q1and1, c(LEFT$rtQ31), c(NA))
LEFT$RTCor1<-ifelse (LEFT$Correct =="TRUE", c(LEFT$RTz1), c(NA))
varCor1<-var(LEFT$RTCor1, na.rm=TRUE)
prop1a<-sum(LEFT$prop1, na.rm = TRUE)
prop1<-prop1a/96
MCor1<-mean(LEFT$RTCor1, na.rm=TRUE)

IQR2<-IQR(LEFT$RT2_EZ, na.rm = TRUE)
quartiles2<-quantile(LEFT$RT2_EZ, na.rm= TRUE)
quart752<-quartiles2[4]
Q3and2<-quart752+(1.5*IQR2)
quart252<-quartiles2[2]
Q1and2<-quart252-(1.5*IQR2)
LEFT$tooBig2<-ifelse(LEFT$RT2_EZ < Q3and2, c(0), c(1))
LEFT$tooLit2<-ifelse(LEFT$RT2_EZ > Q1and2, c(0), c(1))
tooLittle2<-sum(LEFT$tooLit2, na.rm=TRUE)
tooBig2<-sum(LEFT$tooBig2, na.rm=TRUE)
LEFT$rtQ32<-ifelse (LEFT$RT2_EZ < Q3and2, c(LEFT$RT2_EZ), c(NA))
LEFT$RTz2<-ifelse (LEFT$rtQ32 > Q1and2, c(LEFT$rtQ32), c(NA))
LEFT$RTCor2<-ifelse (LEFT$Correct =="TRUE", c(LEFT$RTz2), c(NA))
varCor2<-var(LEFT$RTCor2, na.rm=TRUE)
prop2a<-sum(LEFT$prop2, na.rm = TRUE)
prop2<-prop2a/96
MCor2<-mean(LEFT$RTCor2, na.rm=TRUE)


#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

get.vaTer = function(Pc, VRT, MRT, s=.1)
{
    s2 = s^2
    # The default value for the scaling parameter s equals .1

    if (Pc == 0)
        cat("Oops, Pc == 0!\n")
    if (Pc == 0.5)
        cat("Oops, Pc == .5!\n")
    if (Pc == 1)
        cat("Oops, Pc == 1!\n")
    # If Pc equals 0, .5, or 1, the method will not work, and
    # an edge-correction is required.

    L = qlogis(Pc)
    # The function "qlogis" calculates the logit.
    x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
    v = sign(Pc-0.5)*s*x^(1/4)
    # This gives drift rate.

    a = s2*qlogis(Pc)/v
    # This gives boundary separation.

    y   = -v*a/s2
    MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
    Ter = MRT-MDT
    # This gives nondecision time.

    return(list(v, a, Ter))
}

EZa1<-get.vaTer(prop1,(varCor1/1000000),(MCor1/1000), s = 0.1)
EZ1 <- data.frame(matrix(unlist(EZa1), nrow=1, byrow=3))
v1<-(EZ1$X1)
a1<-(EZ1$X2)
Ter1<-(EZ1$X3)

EZa2<-get.vaTer(prop2,(varCor2/1000000),(MCor2/1000), s = 0.1)
EZ2 <- data.frame(matrix(unlist(EZa2), nrow=1, byrow=3))
v2<-(EZ2$X1)
a2<-(EZ2$X2)
Ter2<-(EZ2$X3)

#printing values we need#
victory1<-c("LEFT",999,d,perclow,MCor1,MCor2,varCor1, varCor2,
           prop1, prop2, tooBig1, tooBig2,
           tooLittle1, tooLittle2,
           tooTiny, v1,
            v2,a1,a2,Ter1,Ter2)
victory <- data.frame(matrix(unlist(victory1), nrow=1, byrow=55))

write.table(victory, file = "R:/Data Analysis/EZ2 imp atts/Scored data/Scored PA Data/LEFT.csv", sep = ",", col.names = NA,
           qmethod = "double")














######IAT version of EZ diffusion model################



Cha <-326
IATPA <-read.csv("R:\\Data analysis\\EZ2 imp atts\\Spring 2009 IAT\\326.csv", na.strings=".", stringsAsFactors=FALSE)

IATPA$rtlow <-ifelse (IATPA$RT > 299, c(IATPA$RT), c(NA))
IATPA$rtnohi <-ifelse (IATPA$RT < 10000, c(IATPA$RT), c(NA))
missing_PA <-sum(is.na(IATPA$rtlow))
IATPA$percLow <-missing_PA/200
perclow<-missing_PA/200
IATPA$rtokay <-ifelse(IATPA$percLow<0.10, c(IATPA$rtnohi), c(NA))
IATPA$RT36<-ifelse(IATPA$Block==3| IATPA$Block==6, c(IATPA$rtokay), c(NA))
IATPA$RT47<-ifelse(IATPA$Block==4| IATPA$Block==7, c(IATPA$rtokay), c(NA))
IATPA$RT3<-ifelse(IATPA$Block==3, c(IATPA$rtokay), c(NA))
IATPA$RT4<-ifelse(IATPA$Block==4, c(IATPA$rtokay), c(NA))
IATPA$RT6<-ifelse(IATPA$Block==6, c(IATPA$rtokay), c(NA))
IATPA$RT7<-ifelse(IATPA$Block==7, c(IATPA$rtokay), c(NA))
SD36 <-sd(IATPA$RT36, na.rm=TRUE)
SD47 <-sd(IATPA$RT47, na.rm=TRUE)
M3 <-mean(IATPA$RT3, na.rm=TRUE)
M6 <-mean(IATPA$RT6, na.rm=TRUE)
M4 <-mean(IATPA$RT4, na.rm=TRUE)
M7 <-mean(IATPA$RT7, na.rm=TRUE)
Diff63<-M6-M3
Diff74<-M7-M4
d63<-Diff63/SD36
d74<-Diff74/SD47
d<-(d63+d74)/2


#making values for EZ2 model#
#using same outlier cuts as in Klauer et al (2007)#
IATPA$rtlow2<-ifelse (IATPA$RT > 100, c(IATPA$RT), c(NA))
IATPA$RT01EZ<-ifelse(IATPA$Block==2, c(IATPA$rtlow2), c(NA))
IATPA$RT02EZ<-ifelse(IATPA$Block==5, c(IATPA$rtlow2), c(NA))
IATPA$RT3EZ<-ifelse(IATPA$Block==3, c(IATPA$rtlow2), c(NA))
IATPA$RT6EZ<-ifelse(IATPA$Block==6, c(IATPA$rtlow2), c(NA))
IATPA$RT4EZ<-ifelse(IATPA$Block==4, c(IATPA$rtlow2), c(NA))
IATPA$RT7EZ<-ifelse(IATPA$Block==7, c(IATPA$rtlow2), c(NA))
IATPA$prop<-ifelse(IATPA$Correct =="FALSE", c(1), c(0))
IATPA$prop01<-ifelse(IATPA$Block==2, c(IATPA$prop), c(NA))
IATPA$prop02<-ifelse(IATPA$Block==5, c(IATPA$prop), c(NA))
IATPA$prop3<-ifelse(IATPA$Block==3, c(IATPA$prop), c(NA))
IATPA$prop6<-ifelse(IATPA$Block==6, c(IATPA$prop), c(NA))
IATPA$prop4<-ifelse(IATPA$Block==4, c(IATPA$prop), c(NA))
IATPA$prop7<-ifelse(IATPA$Block==7, c(IATPA$prop), c(NA))
IATPA$realSmall<-ifelse(IATPA$RT < 100, c(1), c(0))
tooTiny<-sum(IATPA$realSmall, na.rm=TRUE)

IQR01<-IQR(IATPA$RT01EZ, na.rm = TRUE)
quartiles01<-quantile(IATPA$RT01EZ, na.rm= TRUE)
quart7501<-quartiles01[4]
Q3and01<-quart7501+(1.5*IQR01)
quart2501<-quartiles01[2]
Q1and01<-quart2501-(1.5*IQR01)
IATPA$tooBig01<-ifelse(IATPA$RT01EZ < Q3and01, c(0), c(1))
IATPA$tooLit01<-ifelse(IATPA$RT01EZ > Q1and01, c(0), c(1))
tooLittle01<-sum(IATPA$tooLit01, na.rm=TRUE)
tooBig01<-sum(IATPA$tooBig01, na.rm=TRUE)
IATPA$rtQ301<-ifelse (IATPA$RT01EZ < Q3and01, c(IATPA$RT01EZ), c(NA))
IATPA$RTz01<-ifelse (IATPA$rtQ301 > Q1and01, c(IATPA$rtQ301), c(NA))
IATPA$RTCor01<-ifelse (IATPA$Correct =="TRUE", c(IATPA$RTz01), c(NA))
varCor01<-var(IATPA$RTCor01, na.rm=TRUE)
prop01a<-sum(IATPA$prop01, na.rm = TRUE)
prop01b<-prop01a/20
prop01<-ifelse(prop01b>.01,prop01b,(.5/20))
MCor01<-mean(IATPA$RTCor01, na.rm=TRUE)

IQR02<-IQR(IATPA$RT02EZ, na.rm = TRUE)
quartiles02<-quantile(IATPA$RT02EZ, na.rm= TRUE)
quart7502<-quartiles02[4]
Q3and02<-quart7502+(1.5*IQR02)
quart2502<-quartiles02[2]
Q1and02<-quart2502-(1.5*IQR02)
IATPA$tooBig02<-ifelse(IATPA$RT02EZ < Q3and02, c(0), c(1))
IATPA$tooLit02<-ifelse(IATPA$RT02EZ > Q1and02, c(0), c(1))
tooLittle02<-sum(IATPA$tooLit02, na.rm=TRUE)
tooBig02<-sum(IATPA$tooBig02, na.rm=TRUE)
IATPA$rtQ302<-ifelse (IATPA$RT02EZ < Q3and02, c(IATPA$RT02EZ), c(NA))
IATPA$RTz02<-ifelse (IATPA$rtQ302 > Q1and02, c(IATPA$rtQ302), c(NA))
IATPA$RTCor02<-ifelse (IATPA$Correct =="TRUE", c(IATPA$RTz02), c(NA))
varCor02<-var(IATPA$RTCor02, na.rm=TRUE)
prop02a<-sum(IATPA$prop02, na.rm = TRUE)
prop02b<-prop02a/20
prop02<-ifelse(prop02b>.01,prop02b,(.5/20))
MCor02<-mean(IATPA$RTCor02, na.rm=TRUE)

IQR3<-IQR(IATPA$RT3EZ, na.rm = TRUE)
quartiles3<-quantile(IATPA$RT3EZ, na.rm= TRUE)
quart753<-quartiles3[4]
Q3and3<-quart753+(1.5*IQR3)
quart253<-quartiles3[2]
Q1and3<-quart253-(1.5*IQR3)
IATPA$tooBig3<-ifelse(IATPA$RT3EZ < Q3and3, c(0), c(1))
IATPA$tooLit3<-ifelse(IATPA$RT3EZ > Q1and3, c(0), c(1))
tooLittle3<-sum(IATPA$tooLit3, na.rm=TRUE)
tooBig3<-sum(IATPA$tooBig3, na.rm=TRUE)
IATPA$rtQ33<-ifelse (IATPA$RT3EZ < Q3and3, c(IATPA$RT3EZ), c(NA))
IATPA$RTz3<-ifelse (IATPA$rtQ33 > Q1and3, c(IATPA$rtQ33), c(NA))
IATPA$RTCor3<-ifelse (IATPA$Correct =="TRUE", c(IATPA$RTz3), c(NA))
varCor3<-var(IATPA$RTCor3, na.rm=TRUE)
prop3a<-sum(IATPA$prop3, na.rm = TRUE)
prop3b<-prop3a/20
prop3<-ifelse(prop3b>.01,prop3b,(.5/20))
MCor3<-mean(IATPA$RTCor3, na.rm=TRUE)

IQR6<-IQR(IATPA$RT6EZ, na.rm = TRUE)
quartiles6<-quantile(IATPA$RT6EZ, na.rm= TRUE)
quart756<-quartiles6[4]
Q3and6<-quart756+(1.5*IQR6)
quart256<-quartiles6[2]
Q1and6<-quart256-(1.5*IQR6)
IATPA$tooBig6<-ifelse(IATPA$RT6EZ < Q3and6, c(0), c(1))
IATPA$tooLit6<-ifelse(IATPA$RT6EZ > Q1and6, c(0), c(1))
tooLittle6<-sum(IATPA$tooLit6, na.rm=TRUE)
tooBig6<-sum(IATPA$tooBig6, na.rm=TRUE)
IATPA$rtQ36<-ifelse (IATPA$RT6EZ < Q3and6, c(IATPA$RT6EZ), c(NA))
IATPA$RTz6<-ifelse (IATPA$rtQ36 > Q1and6, c(IATPA$rtQ36), c(NA))
IATPA$RTCor6<-ifelse (IATPA$Correct =="TRUE", c(IATPA$RTz6), c(NA))
varCor6<-var(IATPA$RTCor6, na.rm=TRUE)
prop6a<-sum(IATPA$prop6, na.rm = TRUE)
prop6b<-prop6a/20
prop6<-ifelse(prop6b>.01,prop6b,(.5/20))
MCor6<-mean(IATPA$RTCor6, na.rm=TRUE)

IQR4<-IQR(IATPA$RT4EZ, na.rm = TRUE)
quartiles4<-quantile(IATPA$RT4EZ, na.rm= TRUE)
quart754<-quartiles4[4]
Q3and4<-quart754+(1.5*IQR4)
quart254<-quartiles4[2]
Q1and4<-quart254-(1.5*IQR4)
IATPA$tooBig4<-ifelse(IATPA$RT4EZ < Q3and4, c(0), c(1))
IATPA$tooLit4<-ifelse(IATPA$RT4EZ > Q1and4, c(0), c(1))
tooLittle4<-sum(IATPA$tooLit4, na.rm=TRUE)
tooBig4<-sum(IATPA$tooBig4, na.rm=TRUE)
IATPA$rtQ34<-ifelse (IATPA$RT4EZ < Q3and4, c(IATPA$RT4EZ), c(NA))
IATPA$RTz4<-ifelse (IATPA$rtQ34 > Q1and4, c(IATPA$rtQ34), c(NA))
IATPA$RTCor4<-ifelse (IATPA$Correct =="TRUE", c(IATPA$RTz4), c(NA))
varCor4<-var(IATPA$RTCor4, na.rm=TRUE)
prop4a<-sum(IATPA$prop4, na.rm = TRUE)
prop4b<-prop4a/40
prop4<-ifelse(prop4b>.01,prop4b,(.5/40))
MCor4<-mean(IATPA$RTCor4, na.rm=TRUE)

IQR7<-IQR(IATPA$RT7EZ, na.rm = TRUE)
quartiles7<-quantile(IATPA$RT7EZ, na.rm= TRUE)
quart757<-quartiles7[4]
Q3and7<-quart757+(1.5*IQR7)
quart257<-quartiles7[2]
Q1and7<-quart257-(1.5*IQR7)
IATPA$tooBig7<-ifelse(IATPA$RT7EZ < Q3and7, c(0), c(1))
IATPA$tooLit7<-ifelse(IATPA$RT7EZ > Q1and7, c(0), c(1))
tooLittle7<-sum(IATPA$tooLit7, na.rm=TRUE)
tooBig7<-sum(IATPA$tooBig7, na.rm=TRUE)
IATPA$rtQ37<-ifelse (IATPA$RT7EZ < Q3and7, c(IATPA$RT7EZ), c(NA))
IATPA$RTz7<-ifelse (IATPA$rtQ37 > Q1and7, c(IATPA$rtQ37), c(NA))
IATPA$RTCor7<-ifelse (IATPA$Correct =="TRUE", c(IATPA$RTz7), c(NA))
varCor7<-var(IATPA$RTCor7, na.rm=TRUE)
prop7a<-sum(IATPA$prop7, na.rm = TRUE)
prop7b<-prop7a/40
prop7<-ifelse(prop7b>.01,prop7b,(.5/40))
MCor7<-mean(IATPA$RTCor7, na.rm=TRUE)


#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

get.vaTer = function(Pc, VRT, MRT, s=.1)
{
    s2 = s^2
    # The default value for the scaling parameter s equals .1

    if (Pc == 0)
        cat("Oops, Pc == 0!\n")
    if (Pc == 0.5)
        cat("Oops, Pc == .5!\n")
    if (Pc == 1)
        cat("Oops, Pc == 1!\n")
    # If Pc equals 0, .5, or 1, the method will not work, and
    # an edge-correction is required.

    L = qlogis(Pc)
    # The function "qlogis" calculates the logit.
    x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
    v = sign(Pc-0.5)*s*x^(1/4)
    # This gives drift rate.

    a = s2*qlogis(Pc)/v
    # This gives boundary separation.

    y   = -v*a/s2
    MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
    Ter = MRT-MDT
    # This gives nondecision time.

    return(list(v, a, Ter))
}

EZa01<-get.vaTer((1-prop01),(varCor01/1000000),(MCor01/1000), s = 0.1)
EZ01 <- data.frame(matrix(unlist(EZa01), nrow=1, byrow=3))
v01<-(EZ01$X1)
a01<-(EZ01$X2)
Ter01<-(EZ01$X3)

EZa02<-get.vaTer((1-prop02),(varCor02/1000000),(MCor02/1000), s = 0.1)
EZ02 <- data.frame(matrix(unlist(EZa02), nrow=1, byrow=3))
v02<-(EZ02$X1)
a02<-(EZ02$X2)
Ter02<-(EZ02$X3)

EZa3<-get.vaTer((1-prop3),(varCor3/1000000),(MCor3/1000), s = 0.1)
EZ3 <- data.frame(matrix(unlist(EZa3), nrow=1, byrow=3))
v3<-(EZ3$X1)
a3<-(EZ3$X2)
Ter3<-(EZ3$X3)

EZa6<-get.vaTer((1-prop6),(varCor6/1000000),(MCor6/1000), s = 0.1)
EZ6 <- data.frame(matrix(unlist(EZa6), nrow=1, byrow=3))
v6<-(EZ6$X1)
a6<-(EZ6$X2)
Ter6<-(EZ6$X3)

EZa4<-get.vaTer((1-prop4),(varCor4/1000000),(MCor4/1000), s = 0.1)
EZ4 <- data.frame(matrix(unlist(EZa4), nrow=1, byrow=3))
v4<-(EZ4$X1)
a4<-(EZ4$X2)
Ter4<-(EZ4$X3)

EZa7<-get.vaTer((1-prop7),(varCor7/1000000),(MCor7/1000), s = 0.1)
EZ7 <- data.frame(matrix(unlist(EZa7), nrow=1, byrow=3))
v7<-(EZ7$X1)
a7<-(EZ7$X2)
Ter7<-(EZ7$X3)

#printing values we need#
victory1<-c(Cha,d,perclow,d63,d74,MCor01,MCor02,MCor3,MCor6,MCor4,
           MCor7, varCor01, varCor02, varCor3, varCor6, varCor4, varCor7,
         prop01, prop02, prop3, prop6, prop4, prop7, prop01b, prop02b,
            prop3b, prop6b, prop4b, prop7b, tooBig01, tooBig02,
           tooBig3, tooBig6, tooBig4, tooBig7, tooLittle01, tooLittle02,
           tooLittle3, tooLittle6, tooLittle4, tooLittle7, tooTiny, v01,
            v02,v3,v6,v4,v7,a01,a02,a3,a6,a4,a7,Ter01,Ter02,Ter3,Ter6,
             Ter4,Ter7)

victory <- data.frame(matrix(unlist(victory1), nrow=1, byrow=55))

write.table(victory, file = "R:/Data Analysis/EZ2 imp atts/Measurement Models/IAT/326_S09.csv", sep = ",", col.names = NA,
         qmethod = "double")


######Factor Analysis of IAT data####################
library(sem)

IATa <-read.csv("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\IAT\\IAT.csv", na.strings="NA", stringsAsFactors=FALSE)

IAT=data.frame(Ter3=IATa$Ter3,Ter4=IATa$Ter4,Ter6=IATa$Ter6,
                 Ter7=IATa$Ter7, a3=IATa$a3, a4=IATa$a4, a6=IATa$a6,
                 a7=IATa$a7, v3=IATa$v3, v4=IATa$v4, v6=IATa$v6, v7=IATa$v7)

IATcorr<-cov(IAT, use = "pairwise.complete.obs")

library(mvnormtest)
x <- as.matrix(IAT) # n x p numeric matrix
z<-na.omit(x)
y<-t(z)
mshapiro.test(y)
center <- colMeans(z) # centroid
n <- nrow(z); p <- ncol(z); cov <- cov(z);
d <- mahalanobis(z,center,cov) # distances
qqplot(qchisq(ppoints(n),df=p),d,
  main="QQ Plot Assessing Multivariate Normality",
  ylab="Mahalanobis D2")
abline(a=0,b=1) 

b<-na.omit(IAT)
library(mvoutlier)
outliers <-
aq.plot(b)
outliers # show list of outliers 




cfa1.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model1.txt")
model1.sem <- sem(cfa1.model, IATcorr, nrow(IAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model1.sem)
# print standardized coefficients (loadings)
library(QuantPsyc)
lm.beta(pleas.fit)


cfa2.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model2.txt")
model2.sem <- sem(cfa2.model, IATcorr, nrow(IAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model2.sem)
# print standardized coefficients (loadings)
std.coef(model2.sem)


cfa3.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model3.txt")
model3.sem <- sem(cfa3.model, IATcorr, nrow(IAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model3.sem)
# print standardized coefficients (loadings)
std.coef(model3.sem)


cfa4.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model4.txt")
model4.sem <- sem(cfa4.model, IATcorr, nrow(IAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model4.sem)
# print standardized coefficients (loadings)
std.coef(model4.sem)
mod.indices(model4.sem)

cfa5.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model5.txt")
model5.sem <- sem(cfa5.model, IATcorr, nrow(IAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model5.sem)
# print standardized coefficients (loadings)
std.coef(model5.sem)


cfa6.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model6.txt")
model6.sem <- sem(cfa6.model, IATcorr, nrow(IAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model6.sem)
# print standardized coefficients (loadings)
std.coef(model6.sem)


cfa7.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model7.txt")
model7.sem <- sem(cfa7.model, IATcorr, nrow(IAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model7.sem)
# print standardized coefficients (loadings)
std.coef(model7.sem)

cfa8.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model8.txt")
model8.sem <- sem(cfa8.model, IATcorr, nrow(IAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model8.sem)
# print standardized coefficients (loadings)
std.coef(model8.sem)



######Factor Analysis of SCIAT data####################
library(sem)

SCIATa <-read.csv("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\SCIAT\\SCIAT.csv", na.strings="NA", stringsAsFactors=FALSE)

SCIAT=data.frame(Ter1=SCIATa$Ter1,Ter2=SCIATa$Ter2, a1=SCIATa$a1, a2=SCIATa$a2,
                 v1=SCIATa$v1, v2=SCIATa$v2)

SCIATcorr<-cor(SCIAT, use = "pairwise.complete.obs")


                 
library(mvnormtest)
x <- as.matrix(SCIAT) # n x p numeric matrix
z<-na.omit(x)
y<-t(z)
mshapiro.test(y)
center <- colMeans(z) # centroid
n <- nrow(z); p <- ncol(z); cov <- cov(z);
d <- mahalanobis(z,center,cov) # distances
qqplot(qchisq(ppoints(n),df=p),d,
  main="QQ Plot Assessing Multivariate Normality",
  ylab="Mahalanobis D2")
abline(a=0,b=1) 

b<-na.omit(IAT)
library(mvoutlier)
outliers <-
aq.plot(b)
outliers # show list of outliers 


                 
                 
                 
cfa1.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model1s.txt")
model1.sem <- sem(cfa1.model, SCIATcorr, nrow(SCIAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model1.sem)
# print standardized coefficients (loadings)
std.coef(model1.sem) 
mod.indices(model1.sem)


cfa2.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model2s.txt")
model2.sem <- sem(cfa2.model, SCIATcorr, nrow(SCIAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model2.sem)
# print standardized coefficients (loadings)
std.coef(model2.sem)
mod.indices(model2.sem)

cfa3.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model3s.txt")
model3.sem <- sem(cfa3.model, SCIATcorr, nrow(SCIAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model3.sem)
# print standardized coefficients (loadings)
std.coef(model3.sem)
mod.indices(model3.sem)

cfa4.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model4s.txt")
model4.sem <- sem(cfa4.model, SCIATcorr, nrow(SCIAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model4.sem)
# print standardized coefficients (loadings)
std.coef(model4.sem)
mod.indices(model4.sem)

cfa5.model <- specify.model("R:\\Data analysis\\EZ2 imp atts\\Measurement Models\\model5s.txt")
model5.sem <- sem(cfa5.model, SCIATcorr, nrow(SCIAT))
# print results (fit indices, paramters, hypothesis tests)
summary(model5.sem)
# print standardized coefficients (loadings)
std.coef(model5.sem)
mod.indices(model5.sem)



############## All in one Scoring the SC-IATs with correct and incorrect response times##########
####Computer 1 and 2 diffusion model (with Interquartile cut-offs)###############################################

library(psych)

##making list of all files to run##
setwd("R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/PA1/fancy") 
File <- list.files("R:\\Fall 2011 KINES 321 Eval Prim II\\Data\\SCIAT data\\PA1\\fancy")

# import csv-files
library(plyr)
SCIAT_PAa <- mdply(File, read.csv)


##organizing new data files into separated based on correct, block, etc.##
SCIAT_PA <-SCIAT_PAa[which(SCIAT_PAa$Block==2|SCIAT_PAa$Block==4), ]
SCIAT_PA$Odd <-ifelse(SCIAT_PA$Trial %% 2 == 0, 0,1)
SCIAT_PA$Even <-ifelse(SCIAT_PA$Trial %% 2 == 0, 1,0)
SCIAT_PA$False <-ifelse(SCIAT_PA$Correct == "FALSE", 1,0)
SCIAT_PA$True <-ifelse(SCIAT_PA$Correct == "FALSE", 0,1)
SCIAT_PA$Block4 <-ifelse(SCIAT_PA$Block ==4, 1,0)
SCIAT_PA$Block2 <-ifelse(SCIAT_PA$Block ==2, 1,0)
SCIAT_PA$firstTruea <-SCIAT_PA$Odd+SCIAT_PA$True + SCIAT_PA$Block4
SCIAT_PA$firstTrueb <-SCIAT_PA$Even+SCIAT_PA$True + SCIAT_PA$Block2
SCIAT_PA$firstTrue<-ifelse(SCIAT_PA$firstTruea>2 | SCIAT_PA$firstTrueb>2,1,0)

SCIAT_PA$firstTruea2 <-SCIAT_PA$Even+SCIAT_PA$True + SCIAT_PA$Block4
SCIAT_PA$firstTrueb2 <-SCIAT_PA$Odd+SCIAT_PA$True + SCIAT_PA$Block2
SCIAT_PA$firstTrue2<-ifelse(SCIAT_PA$firstTruea2>2 | SCIAT_PA$firstTrueb2>2,1,0)
SCIAT_PA$firstTrue22<-ifelse(SCIAT_PA$firstTruea2>2 | SCIAT_PA$firstTrueb2>2,0,1)

SCIAT_PA$firstTrueCorr<-ifelse(SCIAT_PA$firstTruea>2 | SCIAT_PA$firstTrueb>2,0,1)

SCIAT_PA$ToDiffx<-ifelse(SCIAT_PA$Order==31,SCIAT_PA$Block,0)
ToDiff<-tapply(SCIAT_PA$ToDiffx, SCIAT_PA$Subj, sum)
Subj<-tapply(SCIAT_PA$Subj, SCIAT_PA$Subj,mean)
Subja<-data.frame(Subj)
BWa<- data.frame(ToDiff)
BW<-data.frame(Subj=Subja$Subj,ToDiff=BWa$ToDiff)  

SCIAT1 <- merge(SCIAT_PA,BW,by="Subj")

SCIAT1$ToDiff2<-ifelse(SCIAT1$ToDiff==2, SCIAT1$firstTrue, SCIAT1$firstTrue2)
SCIAT1$Correct2<-ifelse(SCIAT1$ToDiff==2, SCIAT1$firstTrue22, SCIAT1$firstTrueCorr)


Diffusion <-SCIAT1[which(SCIAT1$ToDiff2==0), ]
Incor <-SCIAT1[which(SCIAT1$Correct=="FALSE"), ]
Cora <-SCIAT1[which(SCIAT1$Correct2 == 0), ]
Cor<-Cora[which(Cora$Correct == "TRUE"), ]
SCIAT1$RTand1 <- as.numeric(c( "NA", SCIAT1$RT[ - length(SCIAT1$RT) ] ))
SCIAT1$Correctand1 <- c( "NA", SCIAT1$Correct[ - length(SCIAT1$Correct) ] )
da <-SCIAT1[which(SCIAT1$Correct=="TRUE"), ]
as.numeric(da$RTand1)
da$RTa<-da$RT + da$RTand1
da$RTb<-ifelse(da$Correctand1=="FALSE", da$RTa, da$RT)

da$rtlow <-ifelse (da$RTb > 299, c(da$RTb), c(NA))
da$rtnohi <-ifelse (da$rtlow < 10000, c(da$rtlow), c(NA))
library(VIM)
missing_PA <-tapply(da$rtlow, da$Subj, countNA)
tenK_PA<-tapply(da$rtnohi, da$Subj, countNA)
missing_PA<-data.frame(missing_PA)
missing_PA$percLow <-missing_PA$missing_PA/144
missing_PA$Subj<-BW$Subj

da2 <- merge(da,missing_PA,by="Subj")

da2$rtokay <-ifelse(da2$percLow<0.10, c(da2$rtnohi), c(NA))
da2$RT1<-ifelse(da2$Block==2, c(da2$RTb), c(NA))
da2$RT2<-ifelse(da2$Block==4, c(da2$RTb), c(NA))
Mean_RT1<-tapply(da2$RT1,da2$Subj,mean, na.rm=TRUE)
Mean_RT2<-tapply(da2$RT2, da2$Subj,mean,na.rm = TRUE)
SD<-tapply(da2$RTb,da2$Subj,sd, na.rm = TRUE)

Mean_RT1<-data.frame(Mean_RT1)
Mean_RT1$Subj<-BW$Subj
Mean_RT2<-data.frame(Mean_RT2)
Mean_RT2$Subj<-BW$Subj
SD<-data.frame(SD)
SD$Subj<-BW$Subj

BWda<-merge(Mean_RT1, Mean_RT2, by="Subj")
BWd<-merge(BWda, SD, by="Subj")
BWd$Diff<-BWd$Mean_RT2-BWd$Mean_RT1
BWd$d<-BWd$Diff/BWd$SD

#reliability#
da2$RT11F<-ifelse(da2$Trial < 120, c(da2$RT2), c(NA)) 
da2$RT11T<-ifelse(da2$Trial < 120, c(da2$RT1), c(NA))
da2$RT12F<-ifelse(da2$Trial > 119, c(da2$RT2), c(NA))
da2$RT12T<-ifelse(da2$Trial > 119, c(da2$RT1), c(NA))
da2$RT21F<-ifelse(da2$Trial < 313, c(da2$RT1), c(NA))
da2$RT21T<-ifelse(da2$Trial < 313, c(da2$RT2), c(NA))
da2$RT22F<-ifelse(da2$Trial > 312, c(da2$RT1), c(NA))
da2$RT22T<-ifelse(da2$Trial > 312, c(da2$RT2), c(NA))

Mean_RT11F<-tapply(da2$RT11F, da2$Subj, mean, na.rm = TRUE)
Mean_RT11T<-tapply(da2$RT11T, da2$Subj, mean, na.rm = TRUE)
Mean_RT12F<-tapply(da2$RT12F, da2$Subj, mean, na.rm = TRUE)
Mean_RT12T<-tapply(da2$RT12T, da2$Subj, mean, na.rm = TRUE)
Mean_RT21F<-tapply(da2$RT21F, da2$Subj, mean, na.rm = TRUE)
Mean_RT21T<-tapply(da2$RT21T, da2$Subj, mean, na.rm = TRUE)
Mean_RT22F<-tapply(da2$RT22F, da2$Subj, mean, na.rm = TRUE)
Mean_RT22T<-tapply(da2$RT22T, da2$Subj, mean, na.rm = TRUE)


SD_RT11F<-tapply(da2$RT11F, da2$Subj, sd, na.rm = TRUE)
SD_RT11T<-tapply(da2$RT11T, da2$Subj, sd, na.rm = TRUE)
SD_RT12F<-tapply(da2$RT12F, da2$Subj, sd, na.rm = TRUE)
SD_RT12T<-tapply(da2$RT12T, da2$Subj, sd, na.rm = TRUE)
SD_RT21F<-tapply(da2$RT21F, da2$Subj, sd, na.rm = TRUE)
SD_RT21T<-tapply(da2$RT21T, da2$Subj, sd, na.rm = TRUE)
SD_RT22F<-tapply(da2$RT22F, da2$Subj, sd, na.rm = TRUE)
SD_RT22T<-tapply(da2$RT22T, da2$Subj, sd, na.rm = TRUE)



BWreld<-data.frame(BWd$Subj)
BWreld$Mean_RT11F<-Mean_RT11F
BWreld$Mean_RT11T<-Mean_RT11T
BWreld$Mean_RT12F<-Mean_RT12F
BWreld$Mean_RT12T<-Mean_RT12T
BWreld$Mean_RT21F<-Mean_RT21F
BWreld$Mean_RT21T<-Mean_RT21T
BWreld$Mean_RT22F<-Mean_RT22F
BWreld$Mean_RT22T<-Mean_RT22T
BWreld$SD_RT11F<-SD_RT11F
BWreld$SD_RT11T<-SD_RT11T
BWreld$SD_RT12F<-SD_RT12F
BWreld$SD_RT12T<-SD_RT12T
BWreld$SD_RT21F<-SD_RT21F
BWreld$SD_RT21T<-SD_RT21T
BWreld$SD_RT22F<-SD_RT22F
BWreld$SD_RT22T<-SD_RT22T
BWreld$ToDiff<-BW$ToDiff
BWreld$SD_rel1<-ifelse(BWreld$ToDiff==2, ((BWreld$SD_RT11T+BWreld$SD_RT12T)/2),
                       ((BWreld$SD_RT11F+BWreld$SD_RT12F)/2))
BWreld$SD_rel2<-ifelse(BWreld$ToDiff==2, ((BWreld$SD_RT21T+BWreld$SD_RT22T)/2),
                       ((BWreld$SD_RT21F+BWreld$SD_RT22F)/2))

BWreld$Diff1<-ifelse(BWreld$ToDiff==2, (BWreld$Mean_RT12T-BWreld$Mean_RT11T),(BWreld$Mean_RT11F-BWreld$Mean_RT12F))
BWreld$Diff2<-ifelse(BWreld$ToDiff==2, (BWreld$Mean_RT22T-BWreld$Mean_RT21T),(BWreld$Mean_RT21F-BWreld$Mean_RT22F))
BWreld$relD1<-BWreld$Diff1/BWreld$SD_rel1
BWreld$relD2<-BWreld$Diff2/BWreld$SD_rel2


BWreldx<-data.frame(BWd$Subj)
BWreldx$relD1<-BWreld$relD1
BWreldx$relD2<-BWreld$relD2
BWreldx$Subj<-BWd$Subj

BW<-merge(BWd, BWreldx, by="Subj")
library(gdata)
BW<-remove.vars(BW, names="BWd.Subj")

#making values for EZ2 model#
#using same outlier cuts as in Klauer et al (2007)#
da2$rtlow2<-ifelse (da2$RTb > 100, c(da2$RTb), c(NA))
da2$RT1_EZ<-ifelse(da2$Block==2, c(da2$rtlow2), c(NA))
da2$RT2_EZ<-ifelse(da2$Block==4, c(da2$rtlow2), c(NA))

da2$prop<-ifelse(da2$Correctand1 =="FALSE", c(1), c(0))
da2$prop1<-ifelse(da2$Block==2, c(da2$prop), c(NA))
da2$prop2<-ifelse(da2$Block==4, c(da2$prop), c(NA))

da2$realSmall<-ifelse(da2$RTb < 100, c(1), c(0))
tooTiny<-tapply(da2$realSmall,da2$Subj, sum, na.rm=TRUE)
tooTiny<-data.frame(tooTiny)
tooTiny$Subj<-BW$Subj
BWx<-merge(BW,tooTiny,by="Subj")
BW<-BWx

IQR1<-tapply(da2$RT1_EZ, da2$Subj, IQR, na.rm = TRUE)
quartiles1<-tapply(da2$RT1_EZ, da2$Subj, quantile, na.rm= TRUE)
quartiles1x<-as.character(quartiles1)
quart1xx<-unlist(strsplit(quartiles1x, ","))
quartiles1x <- matrix(quart1xx, nrow = 65, ncol=5, byrow=T)
zoo<-data.frame(quartiles1x)
zoo$Subj<-BW$Subj
names(zoo)[2] <- "Q25"
names(zoo)[4] <- "Q75"
zoo$Q25<-as.character(zoo$Q25)
zoo$Q75<-as.character(zoo$Q75)
zoo$Q25<-as.numeric(zoo$Q25)
zoo$Q75<-as.numeric(zoo$Q75)
zoo$IQR1<-IQR1
zoo$Q3and1<-(zoo$Q75+(1.5*zoo$IQR1))
zoo$Q1and1<-(zoo$Q25-(1.5*zoo$IQR1))
zoo2<-merge(zoo,da2, by="Subj")
zoo2$tooBig1<-ifelse(zoo2$RT1_EZ < zoo2$Q3and1, c(0), c(1))
zoo2$tooLit1<-ifelse(zoo2$RT1_EZ > zoo2$Q1and1, c(0), c(1))
tooLittle1<-tapply(zoo2$tooLit1,zoo2$Subj,sum, na.rm=TRUE)
tooBig1<-tapply(zoo2$tooBig1,zoo2$Subj,sum, na.rm=TRUE)
tooLittle1<-data.frame(tooLittle1)
tooLittle1$tooBig1<-tooBig1
tooLittle1$Subj<-BW$Subj
BWx<-merge(BW, tooLittle1, by="Subj")
BW<-BWx

zoo2$rtQ31<-ifelse (zoo2$RT1_EZ < zoo2$Q3and1, c(zoo2$RT1_EZ), c(NA))
zoo2$RTz1<-ifelse (zoo2$rtQ31 > zoo2$Q1and1, c(zoo2$rtQ31), c(NA))
zoo2$RTCor1<-ifelse (zoo2$Correct =="TRUE", c(zoo2$RTz1), c(NA))
varCor1<-tapply(zoo2$RTCor1,zoo2$Subj,var, na.rm=TRUE)
prop1a<-tapply(zoo2$prop1, zoo2$Subj, sum,na.rm = TRUE)
MCor1<-tapply(zoo2$RTCor1,zoo2$Subj,mean, na.rm=TRUE)
prop1a<-data.frame(prop1a)
prop1a$Subj<-BW$Subj
prop1a$prop1b<-prop1a$prop1a/72
prop1a$prop1<-ifelse(prop1a$prop1b>.01,prop1a$prop1b,(.5/72))
prop1a$MCor1<-MCor1
prop1a$varCor1<-varCor1
BWx<-merge(BW,prop1a,by="Subj")
BW<-BWx
BW<-remove.vars(BW,names=c("prop1b","prop1a" ))

IQR2<-tapply(da2$RT2_EZ, da2$Subj, IQR, na.rm = TRUE)
quartiles2<-tapply(da2$RT2_EZ, da2$Subj, quantile, na.rm= TRUE)
quartiles2x<-as.character(quartiles2)
quart2xx<-unlist(strsplit(quartiles2x, ","))
quartiles2x <- matrix(quart2xx, nrow = 65, ncol=5, byrow=T)
zoox2<-data.frame(quartiles2x)
zoox2$Subj<-BW$Subj
names(zoox2)[2] <- "Q25"
names(zoox2)[4] <- "Q75"
zoox2$Q25<-as.character(zoox2$Q25)
zoox2$Q75<-as.character(zoox2$Q75)
zoox2$Q25<-as.numeric(zoox2$Q25)
zoox2$Q75<-as.numeric(zoox2$Q75)
zoox2$IQR2<-IQR2
zoox2$Q3and1<-(zoox2$Q75+(1.5*zoox2$IQR2))
zoox2$Q1and1<-(zoox2$Q25-(1.5*zoox2$IQR2))
zoox222<-merge(zoox2,da2, by="Subj")
zoox222$tooBig2<-ifelse(zoox222$RT2_EZ < zoox222$Q3and1, c(0), c(1))
zoox222$tooLit2<-ifelse(zoox222$RT2_EZ > zoox222$Q1and1, c(0), c(1))
tooLittle2<-tapply(zoox222$tooLit2,zoox222$Subj,sum, na.rm=TRUE)
tooBig2<-tapply(zoox222$tooBig2,zoox222$Subj,sum, na.rm=TRUE)
tooLittle2<-data.frame(tooLittle2)
tooLittle2$tooBig2<-tooBig2
tooLittle2$Subj<-BW$Subj
BWx<-merge(BW, tooLittle2, by="Subj")
BW<-BWx

zoox222$rtQ31<-ifelse (zoox222$RT2_EZ < zoox222$Q3and1, c(zoox222$RT2_EZ), c(NA))
zoox222$RTz2<-ifelse (zoox222$rtQ31 > zoox222$Q1and1, c(zoox222$rtQ31), c(NA))
zoox222$RTCor2<-ifelse (zoox222$Correct =="TRUE", c(zoox222$RTz2), c(NA))
varCor2<-tapply(zoox222$RTCor2,zoox222$Subj,var, na.rm=TRUE)
prop2a<-tapply(zoox222$prop2, zoox222$Subj, sum,na.rm = TRUE)
MCor2<-tapply(zoox222$RTCor2,zoox222$Subj,mean, na.rm=TRUE)
prop2a<-data.frame(prop2a)
prop2a$Subj<-BW$Subj
prop2a$prop2b<-prop2a$prop2a/72
prop2a$prop2<-ifelse(prop2a$prop2b>.01,prop2a$prop2b,(.5/72))
prop2a$MCor2<-MCor2
prop2a$varCor2<-varCor2
BWx<-merge(BW,prop2a,by="Subj")
BW<-BWx
BW<-remove.vars(BW,names=c("prop2b","prop2a" ))


##d-score with Klauer et al outlier cuts##
SDz1K<-tapply(zoo2$RTz1,zoo2$Subj,sd, na.rm=T)
SDz2K<-tapply(zoox222$RTz2, zoox222$Subj,sd, na.rm=T)
SD<-data.frame(SDz1K)
SD$SDz2K<-SDz2K
SD$Subj<-BW$Subj
SD$SDK<-(SD$SDz1K + SD$SDz2K)/2

Mz1K<-tapply(zoo2$RTz1, zoo2$Subj,mean, na.rm=T)
Mz2K<-tapply(zoox222$RTz2,zoox222$Subj,mean, na.rm=T) 
Me<-data.frame(Mz1K)
Me$Mz2K<-Mz2K
Me$Subj<-BW$Subj
Me$MeanK<-(Me$Mz1K + Me$Mz2K)/2
SDMe<-merge(SD,Me,by="Subj")
SDMe$dK<-(SDMe$Mz2K-SDMe$Mz1K)/SDMe$SDK
BWx<-merge(SDMe,BW,by="Subj")
BW<-remove.vars(BWx, names=c("SDz1K", "SDz2K", "SDK", "Mz1K", "Mz2K", "MeanK"))





#reliability#
zoo2$RT11F<-ifelse(zoo2$Trial < 120, c(zoo2$RT2_EZ), c(NA)) 
zoo2$RT11T<-ifelse(zoo2$Trial < 120, c(zoo2$RT1_EZ), c(NA))
zoo2$RT12F<-ifelse(zoo2$Trial > 119, c(zoo2$RT2_EZ), c(NA))
zoo2$RT12T<-ifelse(zoo2$Trial > 119, c(zoo2$RT1_EZ), c(NA))
zoo2$RT21F<-ifelse(zoo2$Trial < 313, c(zoo2$RT1_EZ), c(NA))
zoo2$RT21T<-ifelse(zoo2$Trial < 313, c(zoo2$RT2_EZ), c(NA))
zoo2$RT22F<-ifelse(zoo2$Trial > 312, c(zoo2$RT1_EZ), c(NA))
zoo2$RT22T<-ifelse(zoo2$Trial > 312, c(zoo2$RT2_EZ), c(NA))

Mean_RT11F<-tapply(zoo2$RT11F, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT11T<-tapply(zoo2$RT11T, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT12F<-tapply(zoo2$RT12F, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT12T<-tapply(zoo2$RT12T, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT21F<-tapply(zoo2$RT21F, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT21T<-tapply(zoo2$RT21T, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT22F<-tapply(zoo2$RT22F, zoo2$Subj, mean, na.rm = TRUE)
Mean_RT22T<-tapply(zoo2$RT22T, zoo2$Subj, mean, na.rm = TRUE)


SD_RT11F<-tapply(zoo2$RT11F, zoo2$Subj, sd, na.rm = TRUE)
SD_RT11T<-tapply(zoo2$RT11T, zoo2$Subj, sd, na.rm = TRUE)
SD_RT12F<-tapply(zoo2$RT12F, zoo2$Subj, sd, na.rm = TRUE)
SD_RT12T<-tapply(zoo2$RT12T, zoo2$Subj, sd, na.rm = TRUE)
SD_RT21F<-tapply(zoo2$RT21F, zoo2$Subj, sd, na.rm = TRUE)
SD_RT21T<-tapply(zoo2$RT21T, zoo2$Subj, sd, na.rm = TRUE)
SD_RT22F<-tapply(zoo2$RT22F, zoo2$Subj, sd, na.rm = TRUE)
SD_RT22T<-tapply(zoo2$RT22T, zoo2$Subj, sd, na.rm = TRUE)



BWreldz<-data.frame(BWd$Subj)
BWreldz$Mean_RT11F<-Mean_RT11F
BWreldz$Mean_RT11T<-Mean_RT11T
BWreldz$Mean_RT12F<-Mean_RT12F
BWreldz$Mean_RT12T<-Mean_RT12T
BWreldz$Mean_RT21F<-Mean_RT21F
BWreldz$Mean_RT21T<-Mean_RT21T
BWreldz$Mean_RT22F<-Mean_RT22F
BWreldz$Mean_RT22T<-Mean_RT22T
BWreldz$SD_RT11F<-SD_RT11F
BWreldz$SD_RT11T<-SD_RT11T
BWreldz$SD_RT12F<-SD_RT12F
BWreldz$SD_RT12T<-SD_RT12T
BWreldz$SD_RT21F<-SD_RT21F
BWreldz$SD_RT21T<-SD_RT21T
BWreldz$SD_RT22F<-SD_RT22F
BWreldz$SD_RT22T<-SD_RT22T
BWreldz$ToDiff<-BWreld$ToDiff
BWreldz$SD_rel1<-ifelse(BWreldz$ToDiff==2, ((BWreldz$SD_RT11T+BWreldz$SD_RT12T)/2),
                        ((BWreldz$SD_RT11F+BWreldz$SD_RT12F)/2))
BWreldz$SD_rel2<-ifelse(BWreldz$ToDiff==2, ((BWreldz$SD_RT21T+BWreldz$SD_RT22T)/2),
                        ((BWreldz$SD_RT21F+BWreldz$SD_RT22F)/2))

BWreldz$Diff1z<-ifelse(BWreldz$ToDiff==2, (BWreldz$Mean_RT12T-BWreldz$Mean_RT11T),(BWreldz$Mean_RT11F-BWreldz$Mean_RT12F))
BWreldz$Diff2z<-ifelse(BWreldz$ToDiff==2, (BWreldz$Mean_RT22T-BWreldz$Mean_RT21T),(BWreldz$Mean_RT21F-BWreldz$Mean_RT22F))
BWreldz$relD1z<-BWreldz$Diff1z/BWreldz$SD_rel1
BWreldz$relD2z<-BWreldz$Diff2z/BWreldz$SD_rel2


BWreldx<-data.frame(BWd$Subj)
BWreldx$relD1z<-BWreldz$relD1z
BWreldx$relD2z<-BWreldz$relD2z
BWreldx$Subj<-BWd$Subj

BWx<-merge(BW, BWreldx, by="Subj")
BW<-remove.vars(BWx, names = "BWd.Subj")


#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

BW$s2 = .01
# The default value for the scaling parameter s equals .1

# If Pc equals 0, .5, or 1, the method will not work, and
# an edge-correction is required.

BW$Pc1 = 1-BW$prop1
BW$Pc2 = 1-BW$prop2

BW$VRT1 =varCor1/1000000
BW$VRT2 =varCor2/1000000

BW$L1 = qlogis(BW$Pc1)
BW$L2 = qlogis(BW$Pc2)
# The function "qlogis" calculates the logit.

BW$x1 = BW$L1*(BW$L1*BW$Pc1^2 - BW$L1*BW$Pc1 + BW$Pc1 - 0.5)/BW$VRT1
BW$v1 = sign(BW$Pc1-0.5)*.1*BW$x1^(1/4)
BW$x2 = BW$L2*(BW$L2*BW$Pc2^2 - BW$L2*BW$Pc2 + BW$Pc2 - 0.5)/BW$VRT2
BW$v2 = sign(BW$Pc2-0.5)*.1*BW$x2^(1/4)
# This gives drift rate.

BW$a1 = BW$s2*qlogis(BW$Pc1)/BW$v1
BW$a2 = BW$s2*qlogis(BW$Pc2)/BW$v2
# This gives boundary separation.

BW$y1 = -1*BW$v1*BW$a1/BW$s2
BW$y2 = -1*BW$v2*BW$a2/BW$s2

BW$MRT1 = MCor1/1000
BW$MRT2 = MCor2/1000


BW$MDT1 = (BW$a1/(2*BW$v1))*(1-exp(BW$y1))/(1+exp(BW$y1))
BW$Ter1 = BW$MRT1-BW$MDT1
BW$MDT2 = (BW$a2/(2*BW$v2))*(1-exp(BW$y2))/(1+exp(BW$y2))
BW$Ter2 = BW$MRT2-BW$MDT2
# This gives nondecision time.

#3 Assumption checks for EZ diffusion modle#

#1- Shape of RT distributions#


skew1<-tapply(zoo2$RTCor1,zoo2$Subj,skew)
skew2<-tapply(zoox222$RTCor2, zoox222$Subj, skew)
skew<-data.frame(skew1)
skew$Subj<-BW$Subj
skew$skew2<-skew2
BWx<-merge(BW, skew, by="Subj")
BW<-BWx

#2- Bias of response selection

LEFT<- zoo2[ which(zoo2$Resp==1), ]
LEFTx<-zoox222[ which(zoox222$Resp==1), ]
LEFT1c<- LEFT[ which(LEFT$Correctand1=="TRUE"), ]
LEFT1i<- LEFT[ which(LEFT$Correctand1=="FALSE"), ]
LEFT2c<- LEFTx[ which(LEFTx$Correctand1=="TRUE"), ]
LEFT2i<- LEFTx[ which(LEFTx$Correctand1=="FALSE"), ]
RIGHT<- zoo2[ which(zoo2$Resp==9), ]
RIGHT1x<- zoox222[ which(zoox222$Resp==9), ]
RIGHT1<- RIGHT[ which(RIGHT$Block==2), ]
RIGHT2<- RIGHT1x[ which(RIGHT$Block==4), ]
RIGHT1c<- RIGHT1[ which(RIGHT1$Correctand1=="TRUE"), ]
RIGHT1i<- RIGHT1[ which(RIGHT1$Correctand1=="FALSE"), ]
RIGHT2c<- RIGHT2[ which(RIGHT2$Correctand1=="TRUE"), ]
RIGHT2i<- RIGHT2[ which(RIGHT2$Correctand1=="FALSE"), ]

MeanL1c<-tapply(LEFT1c$RTCor1,LEFT1c$Subj, mean, na.rm = TRUE)
MeanL1i<-tapply(LEFT1i$RTCor1, LEFT1i$Subj, mean, na.rm = TRUE)
MeanL2c<-tapply(LEFT2c$RTCor2,LEFT2c$Subj,mean, na.rm = TRUE)
MeanL2i<-tapply(LEFT2i$RTCor2, LEFT2i$Subj, mean,na.rm = TRUE)
MeanR1c<-tapply(RIGHT1c$RTCor1,RIGHT1c$Subj, mean, na.rm = TRUE)
MeanR1i<-tapply(RIGHT1i$RTCor1,RIGHT1i$Subj,mean, na.rm = TRUE)
MeanR2c<-tapply(RIGHT2c$RTCor2,RIGHT2c$Subj,mean, na.rm = TRUE)
MeanR2i<-tapply(RIGHT2i$RTCor2,RIGHT2i$Subj,mean, na.rm = TRUE)




#3 - Relative speed of error responses

MeanInc<-tapply(Incor$RT,Incor$Subj,mean, na.rm=TRUE)
MeanCor<-tapply(Cor$RT, Cor$Subj, mean,na.rm=TRUE)
SDInc<-tapply(Incor$RT,Incor$Subj,sd, na.rm=TRUE)
SDCor<-tapply(Cor$RT,Cor$Subj,sd, na.rm=TRUE)

BW$Ter<-(BW$Ter1+BW$Ter2)/2
BW$a<-(BW$a1+BW$a2)/2
BW$vdiff<-BW$v1-BW$v2

#printing values we need#

write.table(BW, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy.csv", sep = ",", col.names = NA,
            qmethod = "double")

write.table(MeanL1c, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanL1c.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(MeanL1i, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanL1i.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(MeanL2c, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanL2c.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(MeanL2i, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanL2i.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(MeanR1c, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanR1c.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(MeanR1i, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanR1i.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(MeanR2c, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanR2c.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(MeanR2i, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanR2i.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(MeanInc, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanInc.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(MeanCor, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_MeanCor.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(SDInc, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_SDInc.csv", sep = ",", col.names = NA,
            qmethod = "double")
write.table(SDCor, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_SDCor.csv", sep = ",", col.names = NA,
            qmethod = "double")


Diffusion$cond<-ifelse(Diffusion$Block==2,1,2)
Diffusion$RTsec<-Diffusion$RT/1000
Diff1=data.frame(Subj=Diffusion$Subj, cond=Diffusion$cond, correct=Diffusion$True, RTsec=Diffusion$RTsec)
write.table(Diff1, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/SCIAT data/Scored SCIATs/PAfancy_diff.csv", sep = ",", col.names = NA,
            qmethod = "double")





###Andre's paper Factor Analysis'###

Andre1 <- read.csv("R:\\Data analysis\\Andre paper\\dailyamanda_0718.csv",
                   na.strings="NA")
Andre<-Andre1[ which(Andre1$Day<8), ]

all=data.frame(enthusiastic=Andre$enthusiastic, happy=Andre$happy, alert=Andre$alert,
               proud=Andre$proud, excited=Andre$excited, calm=Andre$calm,
               peaceful=Andre$peaceful, satisfied=Andre$satisfied,
               relaxed=Andre$relaxed, content=Andre$content, nervous=Andre$nervous,
               embarrassed=Andre$embarrassed, upset=Andre$upset,
               stress=Andre$stress, tense=Andre$tense, sluggish=Andre$sluggish,
               sad=Andre$sad, bored=Andre$bored,
               depressed=Andre$depressed, disappointed=Andre$disappointed)

all1<-as.matrix(all)

library(psych)

# Pricipal Components Analysis
# entering raw data and extracting PCs
# from the correlation matrix
fit <- princomp(na.omit(all), cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit) 

FAkup<-factanal(na.omit(all1), rotation="promax", factors=2, scores)
print(FAkup, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2
load <- FAkup$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(all),cex=.7) # add variable names 

##parall1l analysis of kuppens items##
allc<-cor(all1)

write.table(all, file = "C:/Documents and Settings/alh379/My Documents/Dropbox/Lab/Manuscripts/In Process/Andre's paper/Data analysis/parAll.csv", sep = ",", col.names = NA,
            qmethod = "double")


library(nFactors)
ev <- eigenComputes(allc, use="everything") # get eigenvalues
ap <- parallel(subject=nrow(all),var=ncol(all),
               rep=100,cent=.05)
nS <- nScree(ev$values, ap$eigen$qevpea)
plotnScree(nS) 


###New composites:
Andre$embarrassed2<-sqrt(Andre$embarrassed)
Andre$upset2<-sqrt(Andre$upset)
Andre$tense2<-sqrt(Andre$tense)
Andre$sad2<-sqrt(Andre$sad)
Andre$depressed2<-sqrt(Andre$depressed)
Andre$disappointed2<-sqrt(Andre$disappointed)
Andre$nervous2<-sqrt(Andre$nervous)
Andre$stress2<-sqrt(Andre$stress)
hist(Andre$sad2)

Andre$pleas<-(Andre$excited+Andre$enthusiastic+Andre$happy+Andre$calm+Andre$peaceful+Andre$satisfied+Andre$relaxed+Andre$content)/8
Andre$unpleas<-(Andre$embarrassed+Andre$upset+Andre$tense+Andre$sad+Andre$disappointed+Andre$depressed+Andre$nervous+Andre$stress)/8
Andre$Both<-(Andre$enthusiastic + Andre$happy + Andre$alert + Andre$proud + Andre$excited + Andre$calm + Andre$peaceful + Andre$satisfied + Andre$relaxed + Andre$content + Andre$nervous + Andre$embarrassed + Andre$upset + Andre$stress + Andre$tense + Andre$sluggish + Andre$sad + Andre$bored + Andre$depressed + Andre$disappointed)/20

Andre$unpleas2<-(Andre$embarrassed2+Andre$upset2+Andre$tense2+Andre$sad2+Andre$disappointed2+Andre$depressed2+Andre$nervous2+Andre$stress2)/8

Andre$PAA1<-(Andre$excited+Andre$enthusiastic+Andre$happy)/3
Andre$UDA1<-(Andre$upset+Andre$sad+Andre$depressed)/3
Andre$UAA1<-(Andre$nervous+Andre$stress+Andre$tense)/3
Andre$PDA1<-(Andre$peaceful+Andre$calm+Andre$relaxed)/3

library(ltm)

pleas=data.frame(Andre$excited,Andre$enthusiastic,Andre$happy,Andre$calm,Andre$peaceful,Andre$satisfied,Andre$relaxed,Andre$content)
unpleas=data.frame(Andre$embarrassed,Andre$upset,Andre$tense,Andre$sad,Andre$disappointed,Andre$depressed,Andre$nervous,Andre$stress)

cronbach.alpha(pleas, na.rm=TRUE)
cronbach.alpha(unpleas, na.rm=TRUE)

PAA1m=data.frame(Andre$excited,Andre$enthusiastic,Andre$happy)
UDA1m=data.frame(Andre$upset,Andre$sad,Andre$depressed)
UAA1m=data.frame(Andre$nervous,Andre$stress,Andre$tense)
PDA1m=data.frame(Andre$peaceful,Andre$calm,Andre$relaxed)

cronbach.alpha(PAA1m, na.rm=TRUE)
cronbach.alpha(UDA1m, na.rm=TRUE)
cronbach.alpha(UAA1m, na.rm=TRUE)
cronbach.alpha(PDA1m, na.rm=TRUE)

##iMean and iSDs##

f <- function(x) c( iM=mean(x, na.rm=TRUE), iSD=sd(x, na.rm=TRUE) )

iAndre3=do.call( "rbind", tapply( Andre$pleas, Andre$id, f ))
iAndre4=do.call( "rbind", tapply( Andre$unpleas, Andre$id, f ))
iAndre2=do.call( "rbind", tapply( Andre$unpleas2, Andre$id, f ))
iAndre5<-cbind(iAndre3, iAndre4, iAndre2)
iAndreboth=do.call( "rbind", tapply( Andre$Both, Andre$id, f ))


iAndre1=do.call( "rbind", tapply( Andre$PAA1, Andre$id, f ))
iAndre2=do.call( "rbind", tapply( Andre$PDA1, Andre$id, f ))
iAndre3=do.call( "rbind", tapply( Andre$UAA1, Andre$id, f ))
iAndre4=do.call( "rbind", tapply( Andre$UDA1, Andre$id, f ))
iAndre5<-cbind(iAndre1, iAndre2, iAndre3, iAndre4)
iAndreboth=do.call( "rbind", tapply( Andre$Both, Andre$id, f ))

iAndre=data.frame(iAndre5)

write.table(iAndreboth, file = "R:/Data Analysis/Andre paper/BothimiSD.csv", sep = ",", col.names = NA,
            qmethod = "double")



##redo analysis with new composites##

Andre <- read.csv("R:\\Data analysis\\Andre paper\\EMOREGv2.csv",
                  na.strings="NA")
Andre$sup_c <- (Andre$erq_supp - 3.50851254480108)
Andre$reap_c <- (Andre$erq_reap - 4.83261648743548)
Andre$supXreap <-Andre$sup_c*Andre$reap_c

##descriptives and correlations##

library(psych)
Andre.descrip<-describe(Andre)
write.table(Andre.descrip, file = "R:/Data Analysis/Andre paper/newDescrips.csv", sep = ",", col.names = NA,
            qmethod = "double")
Andre.cor<-cor(Andre, use="pairwise.complete.obs")
write.table(Andre.cor, file = "R:/Data Analysis/Andre paper/newCors.csv", sep = ",", col.names = NA,
            qmethod = "double")

##regression models##
# Multiple Linear Regression Example
Andre$sex1 = Andre$sex-1
sum(Andre$sex1==1)


Andre$pleas.iM_c=Andre$pleas.iM-(mean(Andre$pleas.iM, na.rm=T))
Andre$unpleas.iM_c=Andre$unpleas.iM-(mean(Andre$unpleas.iM, na.rm=T))

pleas.fit <- lm(pleas.iSD ~ pleas.iM_c + sup_c + reap_c+ sup_c*reap_c, data=Andre)
summary(pleas.fit) # show results

unpleas.fit <- lm(unpleas.iSD ~ unpleas.iM_c + sup_c + reap_c+ sup_c*reap_c, data=Andre)
summary(unpleas.fit) # show results

##standardized Betas##
library(QuantPsyc)

St <- data.frame (Make.Z (Andre))
lm1.z <- lm (pleas.iSD ~ pleas.iM + erq_supp + erq_reap+ supXreap, data=StAndre)
summary(lm1.z)

##OR##
library(QuantPsyc)

lm.beta(pleas.fit)

unpleas.fit <- lm(unpleas.iSD ~ unpleas.iM + sup_c + reap_c + supXreap, data=Andre)
summary(unpleas.fit) # show results
lm.beta(unpleas.fit)




paa.fit <- lm(paa.iSD ~ paa.iM + sup_c + reap_c + supXreap, data=Andre)
summary(paa.fit) # show results

pda.fit <- lm(pda.iSD ~ pda.iM + sup_c + reap_c + supXreap, data=Andre)
summary(pda.fit) # show results

uaa.fit <- lm(uaa.iSD ~ uaa.iM + sup_c + reap_c + supXreap, data=Andre)
summary(uaa.fit) # show results

uda.fit <- lm(uda.iSD ~ uda.iM + sup_c + reap_c + supXreap, data=Andre)
summary(uda.fit) # show results




##testing reliabiltiy of iSDs##

rel <- read.csv("R:\\Data analysis\\Andre paper\\rel21.csv",
                na.strings="NA")


rel$pleas1<-(rel$excited+rel$enthusiastic+rel$happy+rel$calm+rel$peaceful+rel$satisfied+rel$relaxed+rel$content)/8
rel$unpleas1<-(rel$embarrassed+rel$upset+rel$tense+rel$sad+rel$disappointed+rel$depressed+rel$nervous+rel$stress)/8


rel1<-rel[ which(rel$Day<8), ]
rel2a<-rel[ which(rel$Day<9), ]
rel2<-rel2a[ which(rel2a$Day>0), ]
rel3a<-rel[ which(rel$Day<10), ]
rel3<-rel3a[ which(rel3a$Day>1), ]
rel4a<-rel[ which(rel$Day<11), ]
rel4<-rel4a[ which(rel4a$Day>2), ]
rel5a<-rel[ which(rel$Day<12), ]
rel5<-rel5a[ which(rel5a$Day>3), ]
rel6a<-rel[ which(rel$Day<13), ]
rel6<-rel6a[ which(rel6a$Day>4), ]
rel7a<-rel[ which(rel$Day<14), ]
rel7<-rel7a[ which(rel7a$Day>5), ]
rel8a<-rel[ which(rel$Day<15), ]
rel8<-rel8a[ which(rel8a$Day>6), ]
rel9a<-rel[ which(rel$Day<16), ]
rel9<-rel9a[ which(rel9a$Day>7), ]
rel10a<-rel[ which(rel$Day<17), ]
rel10<-rel10a[ which(rel10a$Day>8), ]
rel11a<-rel[ which(rel$Day<18), ]
rel11<-rel11a[ which(rel11a$Day>9), ]
rel12a<-rel[ which(rel$Day<19), ]
rel12<-rel12a[ which(rel12a$Day>10), ]
rel13a<-rel[ which(rel$Day<20), ]
rel13<-rel13a[ which(rel13a$Day>11), ]
rel14a<-rel[ which(rel$Day<21), ]
rel14<-rel14a[ which(rel14a$Day>12), ]
rel15a<-rel[ which(rel$Day<22), ]
rel15<-rel[ which(rel15a$Day>13), ]




f <- function(x) c( iM=mean(x, na.rm=TRUE), iSD=sd(x, na.rm=TRUE) )
irel1p=do.call( "rbind", tapply( rel1$pleas1, rel1$id, f ))
irel1u=do.call( "rbind", tapply( rel1$unpleas1, rel1$id, f ))

irel2p=do.call( "rbind", tapply( rel2$pleas1, rel2$id, f ))
irel2u=do.call( "rbind", tapply( rel2$unpleas1, rel2$id, f ))

irel3p=do.call( "rbind", tapply( rel3$pleas1, rel3$id, f ))
irel3u=do.call( "rbind", tapply( rel3$unpleas1, rel3$id, f ))

irel4p=do.call( "rbind", tapply( rel4$pleas1, rel4$id, f ))
irel4u=do.call( "rbind", tapply( rel4$unpleas1, rel4$id, f ))

irel5p=do.call( "rbind", tapply( rel5$pleas1, rel5$id, f ))
irel5u=do.call( "rbind", tapply( rel5$unpleas1, rel5$id, f ))

irel6p=do.call( "rbind", tapply( rel6$pleas, rel6$id, f ))
irel6u=do.call( "rbind", tapply( rel6$unpleas1, rel6$id, f ))

irel7p=do.call( "rbind", tapply( rel7$unpleas, rel7$id, f ))
irel7u=do.call( "rbind", tapply( rel7$pleas, rel7$id, f ))

irel8p=do.call( "rbind", tapply( rel8$pleas1, rel8$id, f ))
irel8u=do.call( "rbind", tapply( rel8$unpleas1, rel8$id, f ))

irel9p=do.call( "rbind", tapply( rel9$pleas1, rel9$id, f ))
irel9u=do.call( "rbind", tapply( rel9$unpleas1, rel9$id, f ))

irel10p=do.call( "rbind", tapply( rel10$pleas1, rel10$id, f ))
irel10u=do.call( "rbind", tapply( rel10$unpleas1, rel10$id, f ))

irel11p=do.call( "rbind", tapply( rel11$pleas1, rel11$id, f ))
irel11u=do.call( "rbind", tapply( rel11$unpleas1, rel11$id, f ))

irel12p=do.call( "rbind", tapply( rel12$pleas1, rel12$id, f ))
irel12u=do.call( "rbind", tapply( rel12$unpleas1, rel12$id, f ))

irel13p=do.call( "rbind", tapply( rel13$pleas1, rel13$id, f ))
irel13u=do.call( "rbind", tapply( rel13$unpleas1, rel13$id, f ))

irel14p=do.call( "rbind", tapply( rel14$pleas1, rel14$id, f ))
irel14u=do.call( "rbind", tapply( rel14$unpleas1, rel14$id, f ))

irel15p=do.call( "rbind", tapply( rel15$pleas1, rel15$id, f ))
irel15u=do.call( "rbind", tapply( rel15$unpleas1, rel15$id, f ))

icombo.p<-cbind(irel1p, irel2p, irel3p, irel4p, irel5p, irel6p,
                irel7p, irel8p, irel9p, irel10p, irel11p, irel12p, irel13p, irel14p)

icombo.u<-cbind(irel1u, irel2u, irel3u, irel4u, irel5u, irel6u,
                irel7u, irel8u, irel9u, irel10u, irel11u, irel12u, irel13u, irel14u)

write.table(icombo.u, file = "R:/Data Analysis/Andre paper/iSDrelUnpleas.csv", sep = ",", col.names = NA,
            qmethod = "double")

write.table(irel15u, file = "R:/Data Analysis/Andre paper/iSDrel2.csv", sep = ",", col.names = NA, qmethod = "double")



u<- read.csv("R:\\Data analysis\\Andre paper\\iSDrelUnpleas.csv",
             na.strings="NA")

p<- read.csv("R:\\Data analysis\\Andre paper\\iSDrelPleas.csv",
             na.strings="NA")

pleas.iM=data.frame(p$iM1, p$iM2,p$iM3,p$iM4,p$iM5,p$iM6,p$iM7,p$iM8,p$iM9,p$iM10,p$iM11,p$iM12,p$iM13,p$iM14,p$iM15)
unpleas.iM=data.frame(u$iM1, u$iM2,u$iM3,u$iM4,u$iM5,u$iM6,u$iM7,u$iM8,u$iM9,u$iM10,u$iM11,u$iM12,u$iM13,u$iM14,u$iM15)
pleas.iSD=data.frame(p$iSD1, p$iSD2,p$iSD3,p$iSD4,p$iSD5,p$iSD6,p$iSD7,p$iSD8,p$iSD9,p$iSD10,p$iSD11,p$iSD12,p$iSD13,p$iSD14,p$iSD15)
unpleas.iSD=data.frame(u$iSD1, u$iSD2,u$iSD3,u$iSD4,u$iSD5,u$iSD6,u$iSD7,u$iSD8,u$iSD9,u$iSD10,u$iSD11,u$iSD12,u$iSD13,u$iSD14,u$iSD15)

pleas.iSD2=data.frame(p$iSD1,p$iSD15)
unpleas.iSD2=data.frame(u$iSD1,u$iSD15)

cor(pleas.iSD2, use="pairwise.complete.obs")
cor(unpleas.iSD2, use="pairwise.complete.obs")


library(ltm)

cronbach.alpha(pleas.iM, na.rm=TRUE)
cronbach.alpha(unpleas.iM, na.rm=TRUE)
cronbach.alpha(pleas.iSD2, na.rm=TRUE)
cronbach.alpha(unpleas.iSD2, na.rm=TRUE)


##enter data##

stab <-read.csv("C:\\Documents and Settings\\alh379\\My Documents\\Dropbox\\Manuscripts\\Under Review\\PA Implicit Attitudes (In)Stability\\Data Analysis\\321 Stability of Imp_new.csv", na.strings=".", stringsAsFactors=FALSE)

##modeling stability of Imp Atts while testing measurement invariance across time##

require(OpenMx)

stab1=data.frame(d11=stab$D1,d12=stab$D2,
                 d13=stab$D3, d21=stab$D1P, d22=stab$D2P, d23=stab$D3P)
stabcov<-cov(stab1, use = "pairwise.complete.obs")
stabMeans<-c((mean(stab1$d11, na.rm=TRUE)), (mean(stab1$d12, na.rm=TRUE)), (mean(stab1$d13, na.rm=TRUE)),
             (mean(stab1$d21, na.rm=TRUE)), (mean(stab1$d22, na.rm=TRUE)), (mean(stab1$d23, na.rm=TRUE)))
names(stabMeans)<-c("d11", "d12", "d13", "d21", "d22", "d23")

manifest=c("d11","d12","d13","d21","d22", "d23")
latent=c("AA1","AA2")



configural <- mxModel("Configural", 
                      type="RAM",
                      manifestVars=manifest,
                      latentVars=latent,
                      mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                      # manifest variances    
                      mxPath(
                        from=manifest,
                        arrows=2, free=TRUE, values = .8,
                        labels=c("sigma11", "sigma12", "sigma13", "sigma21", "sigma22", "sigma23")
                        ),
                      # latent variances
                      mxPath(
                        from=latent,
                        arrows=2,
                        free=c(FALSE,FALSE),
                        values=c(1,1),
                        labels=c("sigmaAE1", "sigmaAE2")
                        ),
                      mxPath(
                        from=c("AA1"),
                        to=c("d11","d12", "d13"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=paste("lamdba",c("11","12", "13"),sep="")
                        ),
                      mxPath(
                        from=c("AA2"),
                        to=c("d21","d22", "d23"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=paste("lambda",c("21","22", "23"),sep="")
                        ),
                      mxPath(
                        from=c("AA1"),
                        to=c("AA2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("sigmaAE1AE2")
                        ),
                      # manifest means
                      mxPath(
                        from="one",
                        to=manifest,
                        arrows=1,
                        free=TRUE,
                        values=c(.2,.2,.2,.2,.2,.2),
                        labels=c("tau11", "tau12", "tau13", "tau21", "tau22", "tau23")
                        ),
                      # latent means
                      mxPath(
                        from="one",
                        to=latent,
                        arrows=1,
                        free=FALSE,
                        values=0,
                        labels=c("AE1man", "AE2mean")
                        ))  


# ----------------------------------------
# run model and examine output

config <- mxRun(configural)
summary(config)




weak <- mxModel("Weak Factorial Invariance", 
                type="RAM",
                manifestVars=manifest,
                latentVars=latent,
                mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                # manifest variances    
                mxPath(
                  from=manifest,
                  arrows=2, free=TRUE, values = .8,
                  labels=c("sigma11", "sigma12", "sigma13", "sigma21", "sigma22", "sigma23")
                  ),
                # latent variances
                mxPath(
                  from=latent,
                  arrows=2,
                  free=c(FALSE,FALSE),
                  values=c(1,1),
                  labels=c("sigmaAE1", "sigmaAE2")
                  ),
                mxPath(
                  from=c("AA1"),
                  to=c("d11","d12", "d13"),
                  arrows=1,
                  free=TRUE,
                  values=c(.2, .2, .2),
                  labels=paste("lamdba",c("1","2", "3"),sep="")
                  ),
                mxPath(
                  from=c("AA2"),
                  to=c("d21","d22", "d23"),
                  arrows=1,
                  free=TRUE,
                  values=c(.2, .2, .2),
                  labels=paste("lambda",c("1","2", "3"),sep="")
                  ),
                mxPath(
                  from=c("AA1"),
                  to=c("AA2"),
                  arrows=1,
                  free=TRUE,
                  values=.05,
                  labels=c("sigmaAE1AE2")
                  ),
                # manifest means
                mxPath(
                  from="one",
                  to=manifest,
                  arrows=1,
                  free=TRUE,
                  values=c(.2,.2,.2,.2,.2,.2),
                  labels=c("tau11", "tau12", "tau13", "tau21", "tau22", "tau23")
                  ),
                # latent means
                mxPath(
                  from="one",
                  to=latent,
                  arrows=1,
                  free=TRUE,
                  values=.2,
                  labels=c("AE1man", "AE2mean")
                  ),
                mxFIMLObjective(
                  covariance="weak_expCov",
                  means="weak_expMean")
                )  


# ----------------------------------------
# run model and examine output

we <- mxRun(weak)
summary(we)




strong <- mxModel("Strong Factorial Invariance", 
                  type="RAM",
                  manifestVars=manifest,
                  latentVars=latent,
                  mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                  # manifest variances    
                  mxPath(
                    from=manifest,
                    arrows=2, free=TRUE, values = .8,
                    labels=c("sigma11", "sigma12", "sigma13", "sigma21", "sigma22", "sigma23")
                    ),
                  # latent variances
                  mxPath(
                    from=latent,
                    arrows=2,
                    free=c(FALSE,FALSE),
                    values=c(1,1),
                    labels=c("sigmaAE1", "sigmaAE2")
                    ),
                  mxPath(
                    from=c("AA1"),
                    to=c("d11","d12", "d13"),
                    arrows=1,
                    free=TRUE,
                    values=c(.2, .2, .2),
                    labels=paste("lamdba",c("1","2", "3"),sep="")
                    ),
                  mxPath(
                    from=c("AA2"),
                    to=c("d21","d22", "d23"),
                    arrows=1,
                    free=TRUE,
                    values=c(.2, .2, .2),
                    labels=paste("lambda",c("1","2", "3"),sep="")
                    ),
                  mxPath(
                    from=c("AA1"),
                    to=c("AA2"),
                    arrows=1,
                    free=TRUE,
                    values=.05,
                    labels=c("sigmaAE1AE2")
                    ),
                  # manifest means
                  mxPath(
                    from="one",
                    to=manifest,
                    arrows=1,
                    free=TRUE,
                    values=c(.2,.2,.2,.2,.2,.2),
                    labels=c("tau1", "tau2", "tau3", "tau1", "tau2", "tau3")
                    ),
                  # latent means
                  mxPath(
                    from="one",
                    to=latent,
                    arrows=1,
                    free=TRUE,
                    values=.2,
                    labels=c("AE1man", "AE2mean")
                    ))  


# ----------------------------------------
# run model and examine output

st <- mxRun(strong)
summary(st)





strict <- mxModel("Strict Factorial Invariance", 
                  type="RAM",
                  manifestVars=manifest,
                  latentVars=latent,
                  mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                  # manifest variances    
                  mxPath(
                    from=manifest,
                    arrows=2, free=TRUE, values = .8,
                    labels=c("sigma1", "sigma1", "sigma1", "sigma2", "sigma2", "sigma2")
                    ),
                  # latent variances
                  mxPath(
                    from=latent,
                    arrows=2,
                    free=c(FALSE,FALSE),
                    values=c(1,1),
                    labels=c("sigmaAE1", "sigmaAE2")
                    ),
                  mxPath(
                    from=c("AA1"),
                    to=c("d11","d12", "d13"),
                    arrows=1,
                    free=TRUE,
                    values=c(.2, .2, .2),
                    labels=paste("lamdba",c("1","2", "3"),sep="")
                    ),
                  mxPath(
                    from=c("AA2"),
                    to=c("d21","d22", "d23"),
                    arrows=1,
                    free=TRUE,
                    values=c(.2, .2, .2),
                    labels=paste("lambda",c("1","2", "3"),sep="")
                    ),
                  mxPath(
                    from=c("AA1"),
                    to=c("AA2"),
                    arrows=1,
                    free=TRUE,
                    values=.05,
                    labels=c("sigmaAE1AE2")
                    ),
                  # manifest means
                  mxPath(
                    from="one",
                    to=manifest,
                    arrows=1,
                    free=TRUE,
                    values=c(.2,.2,.2,.2,.2,.2),
                    labels=c("tau1", "tau2", "tau3", "tau1", "tau2", "tau3")
                    ),
                  # latent means
                  mxPath(
                    from="one",
                    to=latent,
                    arrows=1,
                    free=TRUE,
                    values=.2,
                    labels=c("AE1man", "AE2mean")
                    ))  


# ----------------------------------------
# run model and examine output

stri <- mxRun(strict)
summary(stri)



###########Compare model fit###############


mxCompare(config, we)

mxCompare(we, st)

mxCompare(st, stri)





stab <-read.csv("C:\\Documents and Settings\\alh379\\My Documents\\Dropbox\\Manuscripts\\Under Review\\PA Implicit Attitudes (In)Stability\\Data Analysis\\321 Stability of Imp_new.csv", na.strings=".", stringsAsFactors=FALSE)

##modeling stability of Self-reported PA while testing measurement invariance across time##

require(OpenMx)

stab$vig1sd<-(stab$vig1)/sd(stab$vig1)
stab$vig2sd<-(stab$vig2)/sd(stab$vig2)
stab$mild1sd<-(stab$mild1)/sd(stab$mild1)
stab$mod1sd<-(stab$mod1)/sd(stab$mod1)
stab$mild2sd<-(stab$mild2)/sd(stab$mild2)
stab$mod2sd<-(stab$mod2)/sd(stab$mod2)

stab1=data.frame(d11=stab$vig1sd,d12=stab$mod1sd,
                 d13=stab$mild1sd, d21=stab$vig2sd, d22=stab$mod2sd, d23=stab$mild2sd)
stabcov<-cov(stab1, use = "pairwise.complete.obs")
stabMeans<-c((mean(stab1$d11, na.rm=TRUE)), (mean(stab1$d12, na.rm=TRUE)), (mean(stab1$d13, na.rm=TRUE)),
             (mean(stab1$d21, na.rm=TRUE)), (mean(stab1$d22, na.rm=TRUE)), (mean(stab1$d23, na.rm=TRUE)))
names(stabMeans)<-c("d11", "d12", "d13", "d21", "d22", "d23")

manifest=c("d11","d12","d13","d21","d22", "d23")
latent=c("AA1","AA2")



configural <- mxModel("Configural", 
                      type="RAM",
                      manifestVars=manifest,
                      latentVars=latent,
                      mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                      # manifest variances    
                      mxPath(
                        from=manifest,
                        arrows=2, free=TRUE, values = .8,
                        labels=c("sigma11", "sigma12", "sigma13", "sigma21", "sigma22", "sigma23")
                        ),
                      # latent variances
                      mxPath(
                        from=latent,
                        arrows=2,
                        free=c(FALSE,FALSE),
                        values=c(1,1),
                        labels=c("sigmaAE1", "sigmaAE2")
                        ),
                      mxPath(
                        from=c("AA1"),
                        to=c("d11","d12", "d13"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=paste("lamdba",c("11","12", "13"),sep="")
                        ),
                      mxPath(
                        from=c("AA2"),
                        to=c("d21","d22", "d23"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=paste("lambda",c("21","22", "23"),sep="")
                        ),
                      mxPath(
                        from=c("AA1"),
                        to=c("AA2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("sigmaAE1AE2")
                        ),
                      # manifest means
                      mxPath(
                        from="one",
                        to=manifest,
                        arrows=1,
                        free=TRUE,
                        values=c(.2,.2,.2,.2,.2,.2),
                        labels=c("tau11", "tau12", "tau13", "tau21", "tau22", "tau23")
                        ),
                      # latent means
                      mxPath(
                        from="one",
                        to=latent,
                        arrows=1,
                        free=FALSE,
                        values=0,
                        labels=c("AE1man", "AE2mean")
                        ))  


# ----------------------------------------
# run model and examine output

config <- mxRun(configural)
summary(config)




weak <- mxModel("Weak Factorial Invariance", 
                type="RAM",
                manifestVars=manifest,
                latentVars=latent,
                mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                # manifest variances    
                mxPath(
                  from=manifest,
                  arrows=2, free=TRUE, values = .8,
                  labels=c("sigma11", "sigma12", "sigma13", "sigma21", "sigma22", "sigma23")
                  ),
                # latent variances
                mxPath(
                  from=latent,
                  arrows=2,
                  free=c(FALSE,FALSE),
                  values=c(1,1),
                  labels=c("sigmaAE1", "sigmaAE2")
                  ),
                mxPath(
                  from=c("AA1"),
                  to=c("d11","d12", "d13"),
                  arrows=1,
                  free=TRUE,
                  values=c(.2, .2, .2),
                  labels=paste("lamdba",c("1","2", "3"),sep="")
                  ),
                mxPath(
                  from=c("AA2"),
                  to=c("d21","d22", "d23"),
                  arrows=1,
                  free=TRUE,
                  values=c(.2, .2, .2),
                  labels=paste("lambda",c("1","2", "3"),sep="")
                  ),
                mxPath(
                  from=c("AA1"),
                  to=c("AA2"),
                  arrows=1,
                  free=TRUE,
                  values=.05,
                  labels=c("sigmaAE1AE2")
                  ),
                # manifest means
                mxPath(
                  from="one",
                  to=manifest,
                  arrows=1,
                  free=TRUE,
                  values=c(.2,.2,.2,.2,.2,.2),
                  labels=c("tau11", "tau12", "tau13", "tau21", "tau22", "tau23")
                  ),
                # latent means
                mxPath(
                  from="one",
                  to=latent,
                  arrows=1,
                  free=TRUE,
                  values=.2,
                  labels=c("AE1man", "AE2mean")
                  ),
                mxFIMLObjective(
                  covariance="weak_expCov",
                  means="weak_expMean")
                )  


# ----------------------------------------
# run model and examine output

we <- mxRun(weak)
summary(we)




strong <- mxModel("Strong Factorial Invariance", 
                  type="RAM",
                  manifestVars=manifest,
                  latentVars=latent,
                  mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                  # manifest variances    
                  mxPath(
                    from=manifest,
                    arrows=2, free=TRUE, values = .8,
                    labels=c("sigma11", "sigma12", "sigma13", "sigma21", "sigma22", "sigma23")
                    ),
                  # latent variances
                  mxPath(
                    from=latent,
                    arrows=2,
                    free=c(FALSE,FALSE),
                    values=c(1,1),
                    labels=c("sigmaAE1", "sigmaAE2")
                    ),
                  mxPath(
                    from=c("AA1"),
                    to=c("d11","d12", "d13"),
                    arrows=1,
                    free=TRUE,
                    values=c(.2, .2, .2),
                    labels=paste("lamdba",c("1","2", "3"),sep="")
                    ),
                  mxPath(
                    from=c("AA2"),
                    to=c("d21","d22", "d23"),
                    arrows=1,
                    free=TRUE,
                    values=c(.2, .2, .2),
                    labels=paste("lambda",c("1","2", "3"),sep="")
                    ),
                  mxPath(
                    from=c("AA1"),
                    to=c("AA2"),
                    arrows=1,
                    free=TRUE,
                    values=.05,
                    labels=c("sigmaAE1AE2")
                    ),
                  # manifest means
                  mxPath(
                    from="one",
                    to=manifest,
                    arrows=1,
                    free=TRUE,
                    values=c(.2,.2,.2,.2,.2,.2),
                    labels=c("tau1", "tau2", "tau3", "tau1", "tau2", "tau3")
                    ),
                  # latent means
                  mxPath(
                    from="one",
                    to=latent,
                    arrows=1,
                    free=TRUE,
                    values=.2,
                    labels=c("AE1man", "AE2mean")
                    ))  


# ----------------------------------------
# run model and examine output

st <- mxRun(strong)
summary(st)





strict <- mxModel("Strict Factorial Invariance", 
                  type="RAM",
                  manifestVars=manifest,
                  latentVars=latent,
                  mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                  # manifest variances    
                  mxPath(
                    from=manifest,
                    arrows=2, free=TRUE, values = .8,
                    labels=c("sigma1", "sigma1", "sigma1", "sigma2", "sigma2", "sigma2")
                    ),
                  # latent variances
                  mxPath(
                    from=latent,
                    arrows=2,
                    free=c(FALSE,FALSE),
                    values=c(1,1),
                    labels=c("sigmaAE1", "sigmaAE2")
                    ),
                  mxPath(
                    from=c("AA1"),
                    to=c("d11","d12", "d13"),
                    arrows=1,
                    free=TRUE,
                    values=c(.2, .2, .2),
                    labels=paste("lamdba",c("1","2", "3"),sep="")
                    ),
                  mxPath(
                    from=c("AA2"),
                    to=c("d21","d22", "d23"),
                    arrows=1,
                    free=TRUE,
                    values=c(.2, .2, .2),
                    labels=paste("lambda",c("1","2", "3"),sep="")
                    ),
                  mxPath(
                    from=c("AA1"),
                    to=c("AA2"),
                    arrows=1,
                    free=TRUE,
                    values=.05,
                    labels=c("sigmaAE1AE2")
                    ),
                  # manifest means
                  mxPath(
                    from="one",
                    to=manifest,
                    arrows=1,
                    free=TRUE,
                    values=c(.2,.2,.2,.2,.2,.2),
                    labels=c("tau1", "tau2", "tau3", "tau1", "tau2", "tau3")
                    ),
                  # latent means
                  mxPath(
                    from="one",
                    to=latent,
                    arrows=1,
                    free=TRUE,
                    values=.2,
                    labels=c("AE1man", "AE2mean")
                    ))  


# ----------------------------------------
# run model and examine output

stri <- mxRun(strict)
summary(stri)



###########Compare model fit###############


mxCompare(config, weak)

mxCompare(we, st)

mxCompare(st, stri)





##Auto Attitudes and Evaluative Primes

##entering data##
Bears <-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\Auto Atts IAT Scoring\\KINES 321 Compiled Qual and IAT.csv", na.strings=".", stringsAsFactors=FALSE)

##making dummy variables for each condition##
Bears$control<-ifelse(Bears$Condition == 1, c(1), c(0))
Bears$PAf<-ifelse(Bears$Condition == 2, c(1), c(0))
Bears$PAm<-ifelse(Bears$Condition == 3, c(1), c(0))
Bears$Sed<-ifelse(Bears$Condition == 4, c(1), c(0))
Bears$Bothf<-ifelse(Bears$Condition == 5, c(1), c(0))
Bears$Bothm<-ifelse(Bears$Condition == 6, c(1), c(0))
Bears$PA<-ifelse(Bears$Condition ==2 | Bears$Condition ==3, c(1), c(0))
Bears$PAall<-ifelse(Bears$Condition ==2 | Bears$Condition ==3 | Bears$Condition ==5 | Bears$Condition ==6, c(1), c(0))
Bears$Sedall<-ifelse(Bears$Condition ==4 | Bears$Condition ==5 | Bears$Condition ==6, c(1), c(0))

##making new intentions variable##
Bears$L1PAint2<-(Bears$L1IntPA_1+Bears$L1IntPA_2+Bears$L1IntPA_3)/3
Bears$L2PAint2<-(Bears$L2IntPA_1+Bears$L2IntPA_2+Bears$L2IntPA_3)/3

##making new SC-IAT difference scores##
Bears$adiffPA1<-(Bears$a2PA1-Bears$a1PA1)
Bears$adiffSED1<-(Bears$a2SED1-Bears$a1SED1)
Bears$adiffFLOW1<-(Bears$a2FLOW1-Bears$a1FLOW1)
Bears$adiffINS1<-(Bears$a2INS1-Bears$a1INS1)

Bears$TerdiffPA1<-(Bears$Ter2PA1-Bears$Ter1PA1)
Bears$TerdiffSED1<-(Bears$Ter2SED1-Bears$Ter1SED1)
Bears$TerdiffFLOW1<-(Bears$Ter2FLOW1-Bears$Ter1FLOW1)
Bears$TerdiffINS1<-(Bears$Ter2INS1-Bears$Ter1INS1)

Bears$vdiffPA1<-Bears$v1PA1-Bears$v2PA1
Bears$vdiffSED1<-Bears$v1SED1-Bears$v2SED1
Bears$vdiffFLOW1<-Bears$v1FLOW1-Bears$v2FLOW1
Bears$vdiffINS1<-Bears$v1INS1-Bears$v2INS1


##centered scores##

mean(Bears$vdiffPA1, na.rm=TRUE)
mean(Bears$adiffPA1, na.rm=TRUE)
Bears$vdiffPA1_c <-Bears$vdiffPA1 - .05161329
Bears$adiffPA1_c<-Bears$aPA1 - .006020432

mean(Bears$L1PAint2, na.rm=TRUE)
Bears$L1PAint2_c <-Bears$L1PAint2 - 76.21296

mean(Bears$TerdiffPA1, na.rm=TRUE)
Bears$TerdiffPA1_c <-Bears$TerdiffPA1 - 0.04951393

mean(Bears$dKPA1, na.rm=TRUE)
Bears$dKPA1_c <-Bears$dKPA1 - 0.5285155

mean(Bears$dPA1, na.rm=TRUE)
Bears$dPA1_c <-Bears$dPA1 - 0.336116


##testing eval priming on automatic attitudes##

d.fit <- lm(dPA1 ~ PAall + Sedall, data=Bears)
summary(d.fit) # show results
d.fit <- lm(dPA1 ~ control + Sedall, data=Bears)
summary(d.fit) # show results

d.fit <- lm(dKPA1 ~ PAall + Sedall, data=Bears)
summary(d.fit) # show results
d.fit <- lm(dKPA1 ~ control + Sedall, data=Bears)
summary(d.fit) # show results

d.fit <- lm(TerdiffPA1 ~ PAall + Sedall, data=Bears)
summary(d.fit) # show results
d.fit <- lm(TerdiffPA1 ~ Sedall + control, data=Bears)
summary(d.fit) # show results

d.fit <- lm(vdiffPA1 ~ PAall + Sedall, data=Bears)
summary(d.fit) # show results
d.fit <- lm(vdiffPA1 ~ Sedall + control, data=Bears)
summary(d.fit) # show results

d.fit <- lm(adiffPA1 ~ PAall + Sedall, data=Bears)
summary(d.fit) # show results
d.fit <- lm(adiffPA1 ~ PAall + control, data=Bears)
summary(d.fit) # show results

##testing if EZ diffusion parameters predict unintentional PA above and beyond Reflective Att##

Bears$v1_c <-Bears$v1PA1 - (mean(Bears$v1PA1, na.rm=T))
Bears$v2_c <-Bears$v2PA1 - (mean(Bears$v2PA1, na.rm=T))

d.fit <- lm(AvgSteps2b ~ v1_c + v2_c + v1_c*v2_c, data=Bears)
summary(d.fit) # show results

d.fit <- lm(L2metmin ~ v1_c + v2_c + v1_c*v2_c, data=Bears)
summary(d.fit) # show results

d.fit <- lm(AvgSteps2b ~ dKPA1 + TerdiffPA1, data=Bears)
summary(d.fit) # show results

d.fit <- lm(AvgSteps2b ~ L1PAint2 + adiffPA1, data=Bears)
summary(d.fit) # show results

d.fit <- lm(AvgSteps2b ~ L1PAint2 + vdiffPA1, data=Bears)
summary(d.fit) # show results

d.fit <- lm(AvgSteps2b ~ L1PAint2 + vdiffPA1 + adiffPA1 + TerdiffPA1 +dKPA1, data=Bears)
summary(d.fit) # show results

d.fit <- lm(StepsD1 ~ L1PAint2 + vdiffPA1 + Att, data=Bears)
summary(d.fit) # show results

d.fit <- lm(StepsD1 ~ L1PAint2 + adiffPA1 + Att, data=Bears)
summary(d.fit) # show results



PSIF<-data.frame(v1_c=Bears$v1_c, v2_c=Bears$v2_c,
                 vdiffPA1=Bears$vdiffPA1, vdiffSED1=Bears$vdiffSED1,
                 vdiffINS1=Bears$vdiffINS1, vdiffFLOW1=Bears$vdiffFLOW1,
                 TerdiffPA1=Bears$TerPA1, TerdiffSED1=Bears$TerSED1,
                 TerdiffIns1=Bears$TerINS1, TerdiffFLOW1=Bears$TerFLOW1,
                 adiffPA1=Bears$aPA1, adiffSED1=Bears$aSED1,
                 adiffINS1=Bears$aINS1, adiffFLOW1=Bears$aFLOW1,
                 AvgSteps2b=Bears$AvgSteps2b, metmin1=Bears$L1metmin,
                 metmin2=Bears$L2metmin, int=Bears$L1PAint2)


PSIF<-data.frame(v1PA1=Bears$v1PA1, v2PA1=Bears$v2PA1, v1SED1=Bears$v1SED1, v2SED1=Bears$v2SED1,
                 v1INS1=Bears$v1INS1, v2INS1=Bears$v2INS1, v1FLOW1=Bears$v1FLOW1, v2FLOW1=Bears$v2FLOW1,
                 Ter1PA1=Bears$Ter1PA1, Ter2PA1=Bears$Ter2PA1, Ter1SED1=Bears$Ter1SED1, Ter2SED1=Bears$Ter2SED1,
                 Ter1FLOW1=Bears$Ter1FLOW1, Ter2FLOW1=Bears$Ter2FLOW1, Ter1INS1=Bears$Ter1INS1, Ter2INS1=Bears$Ter2INS1,
                 a1PA1=Bears$a1PA1, a2PA1=Bears$a2PA1, a1SED1=Bears$a1SED1, a2SED1=Bears$a2SED1,
                 a1FLOW1=Bears$a1FLOW1, a2FLOW1=Bears$a2FLOW1,a1INS1=Bears$a1INS1, a2INS1=Bears$a2INS1,
                 AvgSteps2b=Bears$AvgSteps2b, Att=Bears$L1PAAttAffective, metmin1=Bears$L1metmin,
                 metmin2=Bears$L2metmin, int=Bears$L1PAint2, vdiffPA=Bears$vdiffPA1,
                 adiffPA=Bears$adiffPA1, TerdiffPA=Bears$TerdiffPA1, dKPA=Bears$dKPA1)

PSIF.cor<-cor(PSIF, use="pairwise.complete.obs")
write.table(PSIF.cor, file = "C:/Documents and Settings/Amanda Hyde/My Documents/Dropbox/Auto Atts IAT Scoring/Parameter Cors.csv", sep = ",", col.names = NA,
            qmethod = "double")

##probe interaction (in SAS)##

JN<-data.frame(L2metmin=Bears$L2metmin, L1metmin_c=Bears$L1metmin_c, L1PAint2_c=Bears$L1PAint2_c,
               vdiffPA_c=Bears$vdiffPA_c, aaddPA_c=Bears$aaddPA_c)

#exporting to SAS#

library(foreign)
write.foreign(JN, "C:/Documents and Settings/alh379/My Documents/Dropbox/Auto Atts IAT Scoring/JN.txt",
              "C:/Documents and Settings/alh379/My Documents/Dropbox/Auto Atts IAT Scoring/JN.sas", package="SAS" )



PSIF<-data.frame(vdiffPA=Bears$vdiffPA, vdiffPA2=Bears$vdiffPA2, vdiffSED=Bears$vdiffSED, vdiffSED2=Bears$vdiffSED2,
                 vdiffIns=Bears$vdiffIns, vdiffFlow=Bears$vdiffFlow, TerPA=Bears$TerPA, TerPA2=Bears$TerPA2,
                 TerSED=Bears$TerSED, TerSED2=Bears$TerSED2, TerIns=Bears$TerIns,
                 TerFlow=Bears$TerFlow, aaddPA=Bears$aaddPA, aaddPA2=Bears$aaddPA2,
                 aaddSED=Bears$aaddSED, aaddSED2=Bears$aaddSED2, aaddIns=Bears$aaddIns, aaddFlow=Bears$aaddFlow)

PSIF.cor<-cor(PSIF, use="pairwise.complete.obs")
write.table(PSIF.cor, file = "C:/Documents and Settings/Amanda Hyde/My Documents/Dropbox/Auto Atts IAT Scoring/Parameter Cors.csv", sep = ",", col.names = NA,
            qmethod = "double")

write.table(Bears, file = "C:/Documents and Settings/alh379/My Documents/Dropbox/Auto Atts IAT Scoring/Big data.csv", sep = ",", col.names = NA,
            qmethod = "double")


##Unused Scripts##

##Bears$adiffPA<-Bears$L1a2PA - Bears$L1a1PA
##Bears$TerdiffPA<-Bears$L1Ter2PA - Bears$L1Ter1PA
##Bears$adiffPA2<-Bears$L2a2PA - Bears$L2a1PA
##Bears$TerdiffPA2<-Bears$L2Ter2PA - Bears$L2Ter1PA
##Bears$adiffSED<-Bears$L1a2SED - Bears$L1a1SED
##Bears$TerdiffSED<-Bears$L1Ter2SED - Bears$L1Ter1SED


##descriptives and correlations##

##library(psych)
##Bears.descrip<-describe(Bears)
##write.table(Bears.descrip, file = "C:/Documents and Settings/Amanda Hyde/My Documents/Dropbox/Auto Atts IAT Scoring/Descrips.csv", sep = ",", col.names = NA,
##          qmethod = "double")
##Bears.cor<-cor(Bears, use="pairwise.complete.obs")
##write.table(Bears.cor, file = "C:/Documents and Settings/Amanda Hyde/My Documents/Dropbox/Auto Atts IAT Scoring/Cors.csv", sep = ",", col.names = NA,
##          qmethod = "double")

##d.fit <- lm( ~ PAf + PAm + Sed + Bothf + Bothm, data=Bears)
##summary(d.fit) # show results


##d.fit <- lm(TerSED ~ PAall + Sedall, data=Bears)
##summary(d.fit) # show results

##d.fit <- lm(TerSED ~ Sedall + control, data=Bears)
##summary(d.fit) # show results

##d.fit <- lm(vdiffSED ~ PAall + Sedall, data=Bears)
##summary(d.fit) # show results

##d.fit <- lm(vdiffSED ~ Sedall + control, data=Bears)
##summary(d.fit) # show results

##Bears$L1AttPA<-(Bears$L1PAAttAffective + Bears$L1PAAttInstr)/2
##Bears$L2AttPA<-(Bears$L2PAAttAffective + Bears$L2PAAttInstr)/2

##d.fit <- lm(L2AttPA ~ L1AttPA + aaddPA + vdiffPA + TerPA, data=Bears)
##summary(d.fit) # show results






############## (USE THIS ONE) Scoring the SC-IATs with correct and incorrect response times########
############## Computers 1 & 2 Diffusion model#########################

###########Physical Activity#####################################


library(psych)
Cha <-210
SCIAT_PAa <-read.csv("R:\\Fall 2011 KINES 321 Eval Prim II\\Data\\Lab Visit 1\\Comp 2\\210\\pa sc-iat ii\\2101.csv", na.strings=".", stringsAsFactors=FALSE)
SCIAT_PA <-SCIAT_PAa[which(SCIAT_PAa$Block==2|SCIAT_PAa$Block==4), ]
SCIAT_PA$Odd <-ifelse(SCIAT_PA$Trial %% 2 == 0, 0,1)
SCIAT_PA$Even <-ifelse(SCIAT_PA$Trial %% 2 == 0, 1,0)
SCIAT_PA$False <-ifelse(SCIAT_PA$Correct == "FALSE", 1,0)
SCIAT_PA$True <-ifelse(SCIAT_PA$Correct == "FALSE", 0,1)
SCIAT_PA$Block4 <-ifelse(SCIAT_PA$Block ==4, 1,0)
SCIAT_PA$Block2 <-ifelse(SCIAT_PA$Block ==2, 1,0)
SCIAT_PA$firstTruea <-SCIAT_PA$Odd+SCIAT_PA$True + SCIAT_PA$Block4
SCIAT_PA$firstTrueb <-SCIAT_PA$Even+SCIAT_PA$True + SCIAT_PA$Block2
SCIAT_PA$firstTrue<-ifelse(SCIAT_PA$firstTruea>2 | SCIAT_PA$firstTrueb>2,1,0)

SCIAT_PA$firstTruea2 <-SCIAT_PA$Even+SCIAT_PA$True + SCIAT_PA$Block4
SCIAT_PA$firstTrueb2 <-SCIAT_PA$Odd+SCIAT_PA$True + SCIAT_PA$Block2
SCIAT_PA$firstTrue2<-ifelse(SCIAT_PA$firstTruea2>2 | SCIAT_PA$firstTrueb2>2,1,0)
SCIAT_PA$firstTrue22<-ifelse(SCIAT_PA$firstTruea2>2 | SCIAT_PA$firstTrueb2>2,0,1)

SCIAT_PA$firstTrueCorr<-ifelse(SCIAT_PA$firstTruea>2 | SCIAT_PA$firstTrueb>2,0,1)

SCIAT_PA$ToDiff<-ifelse(SCIAT_PA$Order==30,SCIAT_PA$Block,0)
ToDiff<-sum(SCIAT_PA$ToDiff)
SCIAT_PA$Todiffa=ToDiff
SCIAT_PA$ToDiff2<-ifelse(SCIAT_PA$Todiffa==2, SCIAT_PA$firstTrue, SCIAT_PA$firstTrue2)
SCIAT_PA$Correct2<-ifelse(SCIAT_PA$Todiffa==2, SCIAT_PA$firstTrue22, SCIAT_PA$firstTrueCorr)


Diffusion <-SCIAT_PA[which(SCIAT_PA$ToDiff2==0), ]
Incor <-SCIAT_PA[which(SCIAT_PA$Correct=="FALSE"), ]
Cora <-SCIAT_PA[which(SCIAT_PA$Correct2 == 0), ]
Cor<-Cora[which(Cora$Correct == "TRUE"), ]
SCIAT_PA$RTand1 <- as.numeric(c( "NA", SCIAT_PA$RT[ - length(SCIAT_PA$RT) ] ))
SCIAT_PA$Correctand1 <- c( "NA", SCIAT_PA$Correct[ - length(SCIAT_PA$Correct) ] )
da <-SCIAT_PA[which(SCIAT_PA$Correct=="TRUE"), ]
as.numeric(da$RTand1)
da$RTa<-da$RT + da$RTand1
da$RTb<-ifelse(da$Correctand1=="FALSE", da$RTa, da$RT)

da$rtlow <-ifelse (da$RTb > 299, c(da$RTb), c(NA))
da$rtnohi <-ifelse (da$rtlow < 10000, c(da$rtlow), c(NA))
missing_PA <-sum(is.na(da$rtlow))
da$percLow <-missing_PA/192
perclow<-missing_PA/192
da$rtokay <-ifelse(da$percLow<0.10, c(da$rtnohi), c(NA))
da$RT1<-ifelse(da$Block==2, c(da$RTb), c(NA))
da$RT2<-ifelse(da$Block==4, c(da$RTb), c(NA))
Mean_RT1<-mean(da$RT1, na.rm = TRUE)
Mean_RT2<-mean(da$RT2, na.rm = TRUE)
SD<-sd(da$RTb, na.rm = TRUE)
Diff<-Mean_RT2-Mean_RT1
d<-Diff/SD

#reliability#

da$RT11<-ifelse(da$Trial < 120, c(da$RTb), c(NA))
da$RT12a<-ifelse(da$Trial > 119, c(da$RTb), c(NA))
da$RT12<-ifelse(da$Trial < 313, c(da$RTb), c(NA))
da$RT21a<-ifelse(da$Trial < 313, c(da$RTb), c(NA))
da$RT21<-ifelse(da$Trial > 119, c(da$RTb), c(NA))
da$RT22<-ifelse(da$Trial > 312, c(da$RTb), c(NA))
Mean_RT11<-mean(da$RT11, na.rm = TRUE)
Mean_RT12<-mean(da$RT12, na.rm = TRUE)
Mean_RT21<-mean(da$RT21, na.rm = TRUE)
Mean_RT22<-mean(da$RT22, na.rm = TRUE)
SD_rel1a<-sd(da$RT11, na.rm = TRUE)
SD_rel1b<-sd(da$RT21, na.rm = TRUE)
SD_rel1<-(SD_rel1a+SD_rel1b)/2
SD_rel2a<-sd(da$RT12, na.rm = TRUE)
SD_rel2b<-sd(da$RT22, na.rm = TRUE)
SD_rel2<-(SD_rel2a+SD_rel2b)/2
Diff1<-Mean_RT21-Mean_RT11
Diff2<-Mean_RT22-Mean_RT12
relD1<-Diff1/SD_rel1
relD2<-Diff2/SD_rel2

#making values for EZ2 model#
#using same outlier cuts as in Klauer et al (2007)#

da$prop<-ifelse(da$Correctand1 =="FALSE", c(1), c(0))
da$prop1<-ifelse(da$Block==2, c(da$prop), c(NA))
da$prop2<-ifelse(da$Block==4, c(da$prop), c(NA))

prop1a<-sum(da$prop1, na.rm = TRUE)
prop1b<-prop1a/72
prop1<-ifelse(prop1b>.01,prop1b,(.5/72))

prop2a<-sum(da$prop2, na.rm = TRUE)
prop2b<-prop2a/72
prop2<-ifelse(prop2b>.01,prop2b,(.5/72))

MCor1<-mean(da$RT1, na.rm=TRUE)
MCor2<-mean(da$RT2, na.rm=TRUE)
varCor1<-var(da$RT1, na.rm=TRUE)
varCor2<-var(da$RT2, na.rm=TRUE)


#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

get.vaTer = function(Pc, VRT, MRT, s=.1)
{
  s2 = s^2
  # The default value for the scaling parameter s equals .1
  
  if (Pc == 0)
    cat("Oops, Pc == 0!\n")
  if (Pc == 0.5)
    cat("Oops, Pc == .5!\n")
  if (Pc == 1)
    cat("Oops, Pc == 1!\n")
  # If Pc equals 0, .5, or 1, the method will not work, and
  # an edge-correction is required.
  
  L = qlogis(Pc)
  # The function "qlogis" calculates the logit.
  x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
  v = sign(Pc-0.5)*s*x^(1/4)
  # This gives drift rate.
  
  a = s2*qlogis(Pc)/v
  # This gives boundary separation.
  
  y   = -v*a/s2
  MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
  Ter = MRT-MDT
  # This gives nondecision time.
  
  return(list(v, a, Ter))
}

EZa1<-get.vaTer((1-prop1),(varCor1/1000000),(MCor1/1000), s = 0.1)
EZ1 <- data.frame(matrix(unlist(EZa1), nrow=1, byrow=3))
v1<-(EZ1$X1)
a1<-(EZ1$X2)
Ter1<-(EZ1$X3)

EZa2<-get.vaTer((1-prop2),(varCor2/1000000),(MCor2/1000), s = 0.1)
EZ2 <- data.frame(matrix(unlist(EZa2), nrow=1, byrow=3))
v2<-(EZ2$X1)
a2<-(EZ2$X2)
Ter2<-(EZ2$X3)

a<-mean(a1,a2, na.rm=T)
Ter<-mean(Ter1,Ter2, na.rm=T)
vdiff<-(v1-v2)

#3 Assumption checks for EZ diffusion modle#

#1- Shape of RT distributions#

library(psych)

skew1<-skew(da$RT1)
skew2<-skew(da$RT2)

#2- Bias of response selection

LEFT<- da[ which(da$Resp==1), ]
LEFT1<- LEFT[ which(LEFT$Block==2), ]
LEFT2<- LEFT[ which(LEFT$Block==4), ]
LEFT1c<- LEFT1[ which(LEFT1$Correctand1=="TRUE"), ]
LEFT1i<- LEFT1[ which(LEFT1$Correctand1=="FALSE"), ]
LEFT2c<- LEFT2[ which(LEFT2$Correctand1=="TRUE"), ]
LEFT2i<- LEFT2[ which(LEFT2$Correctand1=="FALSE"), ]
RIGHT<- da[ which(da$Resp==9), ]
RIGHT1<- RIGHT[ which(RIGHT$Block<3), ]
RIGHT2<- RIGHT[ which(RIGHT$Block>2), ]
RIGHT1c<- RIGHT1[ which(RIGHT1$Correctand1=="TRUE"), ]
RIGHT1i<- RIGHT1[ which(RIGHT1$Correctand1=="FALSE"), ]
RIGHT2c<- RIGHT2[ which(RIGHT2$Correctand1=="TRUE"), ]
RIGHT2i<- RIGHT2[ which(RIGHT2$Correctand1=="FALSE"), ]

MeanL1c<-mean(LEFT1c$RT1, na.rm = TRUE)
MeanL1i<-mean(LEFT1i$RT1, na.rm = TRUE)
MeanL2c<-mean(LEFT2c$RT2, na.rm = TRUE)
MeanL2i<-mean(LEFT2i$RT2, na.rm = TRUE)
MeanR1c<-mean(RIGHT1c$RT1, na.rm = TRUE)
MeanR1i<-mean(RIGHT1i$RT1, na.rm = TRUE)
MeanR2c<-mean(RIGHT2c$RT2, na.rm = TRUE)
MeanR2i<-mean(RIGHT2i$RT2, na.rm = TRUE)

#3 - Relative speed of error responses

MeanInc<-mean(Incor$RT, na.rm=TRUE)
MeanCor<-mean(Cor$RT, na.rm=TRUE)
SDInc<-sd(Incor$RT, na.rm=TRUE)
SDCor<-sd(Cor$RT, na.rm=TRUE)

#printing values we need#
victory1<-c(Cha, d,Ter,a,vdiff, perclow,relD1,relD2,MCor1,MCor2,varCor1, varCor2, prop1, prop2,
            v1,v2,a1,a2,Ter1,Ter2, skew1, skew2, MeanL1c, MeanL1i, MeanL2c, MeanL2i,
            MeanR1c, MeanR1i, MeanR2c, MeanR2i, MeanCor, MeanInc, SDCor, SDInc)
victory <- data.frame(matrix(unlist(victory1), nrow=1, byrow=34))

write.table(victory, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/Scored SCIATS - revised/2101PA.csv", sep = ",", col.names = NA,
            qmethod = "double")

Diffusion$cond<-ifelse(Diffusion$Block==2,1,2)
Diffusion$RTsec<-Diffusion$RT/1000
Diff1=data.frame(cond=Diffusion$cond, correct=Diffusion$True, RTsec=Diffusion$RTsec)
write.table(Diff1, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/Scored SCIATS - revised/Diffusion Data/2101PAdiff.csv", sep = ",", col.names = NA,
            qmethod = "double")






############## USE THIS ONE: Scoring the SC-IATs###############################################
######## Computer 2 and 3 ##################



library(psych)
Cha <-195
SCIAT_PAa <-read.csv("R:\\Fall 2011 KINES 321 Eval Prim II\\Data\\Lab Visit 1\\Comp 4\\195\\pa sc-iat\\1951.csv", na.strings=".", stringsAsFactors=FALSE)
SCIAT_PA <-SCIAT_PAa[which(SCIAT_PAa$Block==2|SCIAT_PAa$Block==4), ]

##R:\Data analysis\EZ2 imp atts\321 FA 2010 SCIAT##

SCIAT_PA$rtlow <-ifelse (SCIAT_PA$RT > 299, c(SCIAT_PA$RT), c(NA))
SCIAT_PA$rtnohi <-ifelse (SCIAT_PA$rtlow < 10000, c(SCIAT_PA$rtlow), c(NA))
missing_PA <-sum(is.na(SCIAT_PA$rtlow))
SCIAT_PA$percLow <-missing_PA/192
perclow<-missing_PA/192
SCIAT_PA$rtokay <-ifelse(SCIAT_PA$percLow<0.10, c(SCIAT_PA$rtnohi), c(NA))
SCIAT_PA$RT1<-ifelse(SCIAT_PA$Block==2, c(SCIAT_PA$rtokay), c(NA))
SCIAT_PA$RT2<-ifelse(SCIAT_PA$Block==4, c(SCIAT_PA$rtokay), c(NA))
Mean_RT1<-mean(SCIAT_PA$RT1, na.rm = TRUE)
Mean_RT2<-mean(SCIAT_PA$RT2, na.rm = TRUE)
SD<-sd(SCIAT_PA$rtokay, na.rm = TRUE)
Diff<-Mean_RT2-Mean_RT1
d<-Diff/SD
#reliability#
SCIAT_PA$RT11<-ifelse(SCIAT_PA$Trial < 37, c(SCIAT_PA$RT1), c(NA))
SCIAT_PA$RT12<-ifelse(SCIAT_PA$Trial > 36, c(SCIAT_PA$RT1), c(NA))
SCIAT_PA$RT21<-ifelse(SCIAT_PA$Trial < 37, c(SCIAT_PA$RT2), c(NA))
SCIAT_PA$RT22<-ifelse(SCIAT_PA$Trial > 36, c(SCIAT_PA$RT2), c(NA))
Mean_RT11<-mean(SCIAT_PA$RT11, na.rm = TRUE)
Mean_RT12<-mean(SCIAT_PA$RT12, na.rm = TRUE)
Mean_RT21<-mean(SCIAT_PA$RT21, na.rm = TRUE)
Mean_RT22<-mean(SCIAT_PA$RT22, na.rm = TRUE)
SD_rel1a<-sd(SCIAT_PA$RT11, na.rm = TRUE)
SD_rel1b<-sd(SCIAT_PA$RT21, na.rm = TRUE)
SD_rel1<-(SD_rel1a+SD_rel1b)/2
SD_rel2a<-sd(SCIAT_PA$RT12, na.rm = TRUE)
SD_rel2b<-sd(SCIAT_PA$RT22, na.rm = TRUE)
SD_rel2<-(SD_rel2a+SD_rel2b)/2
Diff1<-Mean_RT21-Mean_RT11
Diff2<-Mean_RT22-Mean_RT12
relD1<-Diff1/SD_rel1
relD2<-Diff2/SD_rel2

#making values for EZ2 model#
da<-SCIAT_PA
da$prop<-ifelse(da$Correct =="FALSE", c(1), c(0))
da$prop1<-ifelse(da$Block==2, c(da$prop), c(NA))
da$prop2<-ifelse(da$Block==4, c(da$prop), c(NA))

prop1a<-sum(da$prop1, na.rm = TRUE)
prop1b<-prop1a/72
prop1<-ifelse(prop1b>.01,prop1b,(.5/72))

prop2a<-sum(da$prop2, na.rm = TRUE)
prop2b<-prop2a/72
prop2<-ifelse(prop2b>.01,prop2b,(.5/72))

MCor1<-mean(da$RT1, na.rm=TRUE)
MCor2<-mean(da$RT2, na.rm=TRUE)
varCor1<-var(da$RT1, na.rm=TRUE)
varCor2<-var(da$RT2, na.rm=TRUE)


#EZ diffusion model parameters (Wagenmakers, van der Maas, & Grasman, 2007)#

get.vaTer = function(Pc, VRT, MRT, s=.1)
{
  s2 = s^2
  # The default value for the scaling parameter s equals .1
  
  if (Pc == 0)
    cat("Oops, Pc == 0!\n")
  if (Pc == 0.5)
    cat("Oops, Pc == .5!\n")
  if (Pc == 1)
    cat("Oops, Pc == 1!\n")
  # If Pc equals 0, .5, or 1, the method will not work, and
  # an edge-correction is required.
  
  L = qlogis(Pc)
  # The function "qlogis" calculates the logit.
  x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
  v = sign(Pc-0.5)*s*x^(1/4)
  # This gives drift rate.
  
  a = s2*qlogis(Pc)/v
  # This gives boundary separation.
  
  y   = -v*a/s2
  MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
  Ter = MRT-MDT
  # This gives nondecision time.
  
  return(list(v, a, Ter))
}

EZa1<-get.vaTer((1-prop1),(varCor1/1000000),(MCor1/1000), s = 0.1)
EZ1 <- data.frame(matrix(unlist(EZa1), nrow=1, byrow=3))
v1<-(EZ1$X1)
a1<-(EZ1$X2)
Ter1<-(EZ1$X3)

EZa2<-get.vaTer((1-prop2),(varCor2/1000000),(MCor2/1000), s = 0.1)
EZ2 <- data.frame(matrix(unlist(EZa2), nrow=1, byrow=3))
v2<-(EZ2$X1)
a2<-(EZ2$X2)
Ter2<-(EZ2$X3)

a<-mean(a1,a2, na.rm=T)
Ter<-mean(Ter1,Ter2, na.rm=T)
vdiff<-(v1-v2)


#printing values we need#
victory1<-c(Cha, d,Ter,a,vdiff, perclow,relD1,relD2,MCor1,MCor2,varCor1, varCor2, prop1, prop2,
            v1,v2,a1,a2,Ter1,Ter2)
victory <- data.frame(matrix(unlist(victory1), nrow=1, byrow=20))

write.table(victory, file = "R:/Fall 2011 KINES 321 Eval Prim II/Data/Scored SCIATS - revised/1951PA.csv", sep = ",", col.names = NA,
            qmethod = "double")

##Auto Stability Resubmission analyses##

##enter data##

stab <-read.csv("C:\\Documents and Settings\\Amanda Hyde\\My Documents\\Dropbox\\Manuscripts\\Under Review\\PA Implicit Attitudes (In)Stability\\Data Analysis\\321 Stability of Imp_new.csv", na.strings=".", stringsAsFactors=FALSE)

stab1 <- data.frame(d11=stab$D1,d12=stab$D2,
                 d13=stab$D3, d21=stab$D1P, d22=stab$D2P, d23=stab$D3P,
                 vig1=stab$vig1,mod1=stab$mod1,mild1=stab$mild1,
                 vig2=stab$vig2,mod2=stab$mod2,mild2=stab$mild2,
                  sex=stab$sex, steps=stab$AdjStepsB)


stab1$vig1   <- stab$vig1 /sd(stab$vig1, na.rm=T)
stab1$vig2   <- stab$vig2 /sd(stab$vig2, na.rm=T )
stab1$mod1   <- stab$mod1 /sd(stab$mod1, na.rm=T )
stab1$mod2   <- stab$mod2 /sd(stab$mod2, na.rm=T )
stab1$mild1  <- stab$mild1/sd(stab$mild1, na.rm=T)
stab1$mild2  <- stab$mild2/sd(stab$mild2, na.rm=T)
stab1$steps <- stab1$steps/sd(stab1$steps,na.rm=T)

# ------------------------------
# A.S. added following to read in data - 2012-01-28
# ran model with scaled variables

######Latent Growth in Structural Equation Modeling of Automatic Attitudes and PA data####################
require(OpenMx)

stabcov2 <- cov(stab1[,-c(15)], use = "pairwise.complete.obs") 

stabScaledMeans <- colMeans(stab1[,-c(15)],na.rm=TRUE)

manifest <- colnames(stabcov2)
latent <- c("AA1","AA2","AAdiff","SR1","SR2", "SRdiff")


AAPAscaled <- mxModel("AAPAscaled", 
          type="RAM",
          manifestVars=manifest,
          latentVars=latent,
          mxData(stabcov2,type="cov", numObs=164, means=stabScaledMeans),
    # manifest variances    
    mxPath(
        from=manifest,
        arrows=2, free=TRUE, values = .8,
        lbound=.000001,
        labels=c(rep("resd1",3),rep("resd2",3),rep("resSR1man",3),rep("resSR2man",3), "ressex", "ressteps")
    ),
    # manifest residual covariance within time
   # mxPath(
    #    from=manifest[1:6],
     #   to=manifest[7:12,
      #  arrows=2,
      #  free=TRUE,
      #  values=.2,
      #  labels=c(rep("omegaAARSres1",3),rep("omegaAARSres2",3))
    #),
    # latent variances
    mxPath(
        from=latent,
        arrows=2,
        free=c(F,F,T,F,F,T),
        values=c(1,1,.8,1,1,.8),
        lbound=.0000001,
        labels=paste("res",latent,sep="")
    ),
    mxPath(
        from=c("AA1"),
        to=c("d11","d12", "d13"),
        arrows=1,
        free=TRUE,
        values=.2,
        labels=paste("b_",c("d11","d12", "d13"),sep="")
    ),
    mxPath(
        from=c("AA2"),
        to=c("d21","d22", "d23"),
        arrows=1,
        free=TRUE,
        values=.2,
        labels=paste("b_",c("d21","d22", "d23"),sep="")
    ),
    mxPath(
        from=c("AA1","AAdiff"),
        to=c("AA2","AA2"),
        arrows=1,
        free=FALSE,
        values=1,
    ),
    mxPath(
        from=c("AAdiff"),
        to=c("AA1"),
        arrows=2,
        free=TRUE,
        values=.05,
        labels=c("AAdiffAA1")
    ),
    # ------------
    mxPath(
        from=c("SR1"),
        to=c("vig1","mod1", "mild1"),
        arrows=1,
        free=TRUE,
        values=.2,
        labels=paste("b_",c("vig1","mod1", "mild1"),sep="")
    ),
    mxPath(
        from=c("SR2"),
        to=c("vig2","mod2", "mild2"),
        arrows=1,
        free=TRUE,
        values=.2,
        labels=paste("b_",c("vig2","mod2", "mild2"),sep="")
    ),
    mxPath(
        from=c("SR1","SRdiff"),
        to=c("SR2","SR2"),
        arrows=1,
        free=FALSE,
        values=1,
    ),
    mxPath(
        from=c("SRdiff"),
        to=c("SR1"),
        arrows=2,
        free=TRUE,
        values=.05,
        labels=c("SRdiffSR1")
    ),

    # manifest means
   mxPath(
       from="one",
       to=manifest,
       arrows=1,
       free=c(rep(F,12),T,T),
       values=c(rep(0,12),.2,.2)
   ),
    # latent means
    mxPath(
        from="one",
        to=c("AA1","AAdiff","SR1","SRdiff"),
        arrows=1,
        free=TRUE,
        values=.05,
        labels=paste("mean",c("AA1","AAdiff","SR1","SRdiff"),sep="")
    ),
    # latent change as predictors of steps
    mxPath(
        from=c("AAdiff","SRdiff"),
        to=c("steps","steps"),
        arrows=1,
        free=TRUE,
        values=c(.05),
        labels=c("bAAdiffsteps","bSRdiffsteps")
    ),
    # covariance between latent change
    mxPath(
        from="AAdiff",
        to="SRdiff",
        arrows=2,
        free=TRUE,
        values=.2,
        labels="AASRdiffs"
    ),
    mxPath(
        from=c("sex"),
        to=c("steps", "AAdiff", "SRdiff"),
        arrows=1,
        free=TRUE,
        values=.8,
        labels=paste("b_sex",c("steps", "AAdiff", "SRdiff"),sep="")
    )
)   
 

# ----------------------------------------
# run model and examine output

AAPAfit <- mxRun(AAPAscaled)
summary(AAPAfit)





##Habit and consistency of PA##

##entering data##
Habitdaily <-read.csv("C:\\Documents and Settings\\Amanda Hyde\\My Documents\\Dropbox\\Habit and consistency of PA\\422-428 Daily Data (F10).csv", na.strings=".", stringsAsFactors=FALSE)
Hdaily<-data.frame(ID=Habitdaily$ClassID, day=Habitdaily$Day,
                   sQ=Habitdaily$Slpqual_01, pss=Habitdaily$Pss_04,
                   fatigue=Habitdaily$Tpbad_10, vigPA=Habitdaily$Ipaq_01,
                   modPA=Habitdaily$Ipaq_02, walkPA=Habitdaily$Ipaq_03,
                   fatigue2=Habitdaily$Hrqol_05, phys=Habitdaily$Hrqol_07,
                   mental=Habitdaily$Hrqol_08, PA=Habitdaily$metmin)

Hdaily$VM=Hdaily$modPA + Hdaily$vigPA

id<-tapply(Hdaily$ID, Hdaily$ID,mean)
PAm<-tapply(Hdaily$PA, Hdaily$ID,mean, na.rm=T)
PAsd<-tapply(Hdaily$PA, Hdaily$ID,sd, na.rm=T)
walkm<-tapply(Hdaily$walkPA, Hdaily$ID,mean, na.rm=T)
walksd<-tapply(Hdaily$walkPA, Hdaily$ID,sd, na.rm=T)
modm<-tapply(Hdaily$modPA, Hdaily$ID,mean, na.rm=T)
modsd<-tapply(Hdaily$modPA, Hdaily$ID,sd, na.rm=T)
vigm<-tapply(Hdaily$vigPA, Hdaily$ID,mean, na.rm=T)
vigsd<-tapply(Hdaily$vigPA, Hdaily$ID,sd, na.rm=T)
VMm<-tapply(Hdaily$VM, Hdaily$ID,mean, na.rm=T)
VMsd<-tapply(Hdaily$VM, Hdaily$ID,sd, na.rm=T)
sQm<-tapply(Hdaily$sQ, Hdaily$ID,mean, na.rm=T)
sQsd<-tapply(Hdaily$sQ, Hdaily$ID,sd, na.rm=T)
pssm<-tapply(Hdaily$pss, Hdaily$ID,mean, na.rm=T)
psssd<-tapply(Hdaily$pss, Hdaily$ID,sd, na.rm=T)
fatiguem<-tapply(Hdaily$fatigue, Hdaily$ID,mean, na.rm=T)
fatiguesd<-tapply(Hdaily$fatigue, Hdaily$ID,sd, na.rm=T)
fatigue2m<-tapply(Hdaily$fatigue2, Hdaily$ID,mean, na.rm=T)
fatigue2sd<-tapply(Hdaily$fatigue2, Hdaily$ID,sd, na.rm=T)
physm<-tapply(Hdaily$phys, Hdaily$ID,mean, na.rm=T)
physsd<-tapply(Hdaily$phys, Hdaily$ID,sd, na.rm=T)
mentalm<-tapply(Hdaily$mental, Hdaily$ID,mean, na.rm=T)
mentalsd<-tapply(Hdaily$mental, Hdaily$ID,sd, na.rm=T)

Hbw<-data.frame(id)
Hbw$PAiM<-PAm
Hbw$PAisd<-PAsd
Hbw$walkiM<-walkm
Hbw$walkisd<-walksd
Hbw$modiM<-modm
Hbw$modisd<-modsd
Hbw$vigiM<-vigm
Hbw$vigisd<-vigsd
Hbw$VMiM<-vigm
Hbw$VMisd<-vigsd
Hbw$sQiM<-sQm
Hbw$sQisd<-sQsd
Hbw$pssiM<-pssm
Hbw$pssisd<-psssd
Hbw$fatigueiM<-fatiguem
Hbw$fatigueisd<-fatiguesd
Hbw$fatigue2iM<-fatigue2m
Hbw$fatigue2isd<-fatigue2sd
Hbw$physiM<-physm
Hbw$physisd<-physsd
Hbw$mentaliM<-mentalm
Hbw$mentalisd<-mentalsd

Habitbase <-read.csv("C:\\Documents and Settings\\Amanda Hyde\\My Documents\\Dropbox\\Habit and consistency of PA\\422-428 Pre BMI Post IAT iMSteps (F10).csv", na.strings=".", stringsAsFactors=FALSE)
Hbase<-data.frame(id=Habitbase$ClassID, sex=Habitbase$sex,
                  year=Habitbase$year, ethnic=Habitbase$ethnic,
                  race=Habitbase$race, norm_pa=Habitbase$norm_pa,
                  habit01=Habitbase$Habit_01, habit02=Habitbase$Habit_02,
                  habit03=Habitbase$Habit_03, habit04=Habitbase$Habit_04,
                  habit05=Habitbase$Habit_05, habit06=Habitbase$Habit_06,
                  habit07=Habitbase$Habit_07, habit08=Habitbase$Habit_08,
                  habit09=Habitbase$Habit_09, habit10=Habitbase$Habit_10,
                  habit11=Habitbase$Habit_11, habit12=Habitbase$Habit_12,
                  BMI=Habitbase$bmi1, stepsiM=Habitbase$AdjStepsB)
Hbase$habit=(Hbase$habit01+Hbase$habit02+Hbase$habit03+Hbase$habit04+Hbase$habit05+Hbase$habit06+Hbase$habit07+Hbase$habit08+Hbase$habit09+Hbase$habit10+Hbase$habit11+Hbase$habit12)/12


Hbw1<-merge(Hbase, Hbw, by="id")
Hbw<-Hbw1

Hbw$PAicv=(Hbw$PAisd/Hbw$PAiM)
Hbw$walkicv=(Hbw$walkisd/Hbw$walkiM)
Hbw$modicv=(Hbw$modisd/Hbw$modiM)
Hbw$vigicv=(Hbw$vigisd/Hbw$vigiM)
Hbw$VMicv=(Hbw$VMisd/Hbw$VMiM)

Habitsteps <-read.csv("C:\\Documents and Settings\\Amanda Hyde\\My Documents\\Dropbox\\Habit and consistency of PA\\422-428 Daily Steps (multilevel format F10).csv", na.strings=".", stringsAsFactors=FALSE)
Hsteps<-data.frame(id=Habitsteps$ClassID, day=Habitsteps$Day., DoW=Habitsteps$DayofWk,
                  weekend=Habitsteps$Weekend, Totsteps=Habitsteps$TotSteps,
                  AdjSteps=Habitsteps$AdjSteps)
Hsteps$AdjStepsb=(ifelse(Hsteps$AdjSteps<500,NA,Hsteps$AdjSteps))

stepsm<-tapply(Hsteps$AdjStepsb, Hsteps$id,mean, na.rm=T)
stepssd<-tapply(Hsteps$AdjStepsb, Hsteps$id,sd, na.rm=T)
Hbw$stepsiM<-stepsm
Hbw$stepsisd<-stepssd

Hbw.cor<-cor(Hbw, use="pairwise.complete.obs")
write.table(Hbw.cor, file = "C:/Documents and Settings/Amanda Hyde/My Documents/Dropbox/Habit and consistency of PA/Cors.csv", sep = ",", col.names = NA,
            qmethod = "double")

Hdaily$id=Hdaily$ID

d.fit <- lm(habit ~ walkiM + walkisd, data=Hbw)
summary(d.fit) # show results

library(ggm)

pcor(c(Hbw$walkiSD,Hbw$habit,Hbw$walkiM))



####EZ Diffusion model analysis for Study 1####

EZ <-read.csv("C:\\Users\\hydea\\Dropbox\\Data analysis\\EZ diff RR\\All Data_SCIAT.csv", na.strings=".", stringsAsFactors=FALSE)
EZ$v1PA=EZ$v1PA1*10
EZ$v2PA=EZ$v2PA1*10
EZ$a1PA=EZ$a1PA1*10
EZ$a2PA=EZ$a2PA1*10
EZ$v1SED=EZ$v1SED1*10
EZ$v2SED=EZ$v2SED1*10
EZ$a1SED=EZ$a1SED1*10
EZ$a2SED=EZ$a2SED1*10
EZ$v1FLOW=EZ$v1FLOW1*10
EZ$v2FLOW=EZ$v2FLOW1*10
EZ$a1FLOW=EZ$a1FLOW1*10
EZ$a2FLOW=EZ$a2FLOW1*10
EZ$PAadiff=EZ$a2PA-EZ$a1PA
EZ$PAvdiff=EZ$v1PA-EZ$v2PA
EZ$PATerdiff=EZ$Ter1PA1-EZ$Ter2PA1
EZ$SEDadiff=EZ$a2SED-EZ$a1SED
EZ$SEDvdiff=EZ$v1SED-EZ$v2SED
EZ$SEDTerdiff=EZ$Ter1SED1-EZ$Ter2SED1
EZ$FLOWadiff=EZ$a2FLOW-EZ$a1FLOW
EZ$FLOWvdiff=EZ$v1FLOW-EZ$v2FLOW
EZ$FLOWTerdiff=EZ$Ter1FLOW1-EZ$Ter2FLOW1


library(psych)
EZ.desc<-describe(AV)
write.table(EZ.desc, file = "C:/Users/hydea/Dropbox/Data analysis/EZ diff RR/Descriptives.csv", sep = ",", col.names = NA,
            qmethod = "double")


EZ.cor<-cor(EZ, use="pairwise.complete.obs")
write.table(EZ.cor, file = "C:/Documents and Settings/Amanda Hyde/My Documents/Dropbox/Auto Atts IAT Scoring/Cors.csv", sep = ",", col.names = NA,
            qmethod = "double")





# paired t-test
t.test(EZ$v1PA,EZ$v2PA,paired=TRUE)
t.test(EZ$v1SED,EZ$v2SED,paired=TRUE)
t.test(EZ$v1FLOW,EZ$v2FLOW,paired=TRUE)

t.test(EZ$a2PA,EZ$a1PA,paired=TRUE)
t.test(EZ$a2SED,EZ$a1SED,paired=TRUE)
t.test(EZ$a2FLOW,EZ$a1FLOW,paired=TRUE)

t.test(EZ$Ter2PA1,EZ$Ter1PA1,paired=TRUE)
t.test(EZ$Ter2SED1,EZ$Ter1SED1,paired=TRUE)
t.test(EZ$Ter2FLOW1,EZ$Ter1FLOW1,paired=TRUE)


EZ$PAaAV=(EZ$a2PA+EZ$a1PA)/2
EZ$PAvAV=(EZ$v1PA+EZ$v2PA)/2
EZ$PATerAV=(EZ$Ter2PA1+EZ$Ter1PA1)/2
EZ$SEDaAV=(EZ$a2SED+EZ$a1SED)/2
EZ$SEDvAV=(EZ$v1SED+EZ$v2SED)/2
EZ$SEDTerAV=(EZ$Ter2SED1+EZ$Ter1SED1)/2
EZ$FLOWaAV=(EZ$a2FLOW+EZ$a1FLOW)/2
EZ$FLOWvAV=(EZ$v1FLOW+EZ$v2FLOW)/2
EZ$FLOWTerAV=(EZ$Ter2FLOW1+EZ$Ter1FLOW1)/2

AV<-data.frame(id=EZ$id, aAV.1=EZ$PAaAV, aAV.2=EZ$SEDaAV, aAV.3=EZ$FLOWaAV,
               vAV.1=EZ$PAvAV, vAV.2=EZ$SEDvAV,vAV.3=EZ$FLOWvAV,
               TerAV.1=EZ$PATerAV, TerAV.2=EZ$SEDTerAV, TerAV.3=EZ$FLOWTerAV)


library(MASS)
library(forecast)

boxcox(aAV.3 = aAV.2 + aAV.1 + TerAV.1 + TerAV.2 + TerAV.3 + vAV.1 + vAV.2 + vAV.3, data = AV,
       lambda = seq(-1, 1, length = 10))

four$vStepsAVG_bc <- BoxCox(four$vStepsAVG,.19)

##testing multivariate normality##
nomiss <-read.csv("C:\\Users\\hydea\\Dropbox\\Data analysis\\EZ diff RR\\nomiss.csv", na.strings=".", stringsAsFactors=FALSE)

library(mvsf)
C = t(nomiss[3:11])
mvsf(C)

nomiss2<-data.frame(nomiss[3:11])

library(MVN)
hzTest(nomiss2, cov = T, qqplot = T)
mardiaTest(nomiss2, cov = T, qqplot = T)
roystonTest(nomiss2, qqplot = T)



library(mvoutlier)
outliers <-aq.plot(EZ[c("aAV.1", "aAV.2", "aAV.3",
                     "vAV.1", "vAV.2", "vAV.3",
                     "TerAV.1", "TerAV.2", "TerAV.3")])
outliers # show list of outliers
EZo<-cbind(EZ, outliers)


##cutting those with multivariate outliers##
bob2<-bob1[which(bob1$outlier=="FALSE"), ]





EZav.cor<-cor(AV, use="pairwise.complete.obs")
write.table(AV, file = "C:/Users/hydea/Dropbox/Data analysis/EZ diff RR/nomiss.csv", sep = ",", col.names = NA,
            qmethod = "double")


##unused analyses##


##EZbw<-data.frame(id=EZ$id, aAV.1=EZ$PAaAV, aAV.2=EZ$SEDaAV, aAV.3=EZ$FLOWaAV,
##                 vAV.1=EZ$PAvAV, vAV.2=EZ$SEDvAV,vAV.3=EZ$FLOWvAV,
##                 TerAV.1=EZ$PATerAV, TerAV.2=EZ$SEDTerAV, TerAV.3=EZ$FLOWTerAV)

#converting wide data to long data#

##EZB = reshape(EZbw, direction="long",varying=2:10)

# One Within Factor
##aov.ex1 = aov(aAV~time+Error(id/time),data=EZB)  #do the analysis of variance
##summary(aov.ex1)                                    #show the summary table
##print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell

##aov.ex2 = aov(vAV~time+Error(id/time),data=EZB)  #do the analysis of variance
##summary(aov.ex2)                                    #show the summary table
##print(model.tables(aov.ex2,"means"),digits=3)       #report the means and the number of subjects/cell


##EZ diffusion model study 2 analyses##

S <-read.csv("C:\\Users\\hydea\\Dropbox\\Data analysis\\EZ diff RR\\AllData_study2.csv", na.strings=".", stringsAsFactors=FALSE)

S$v1PA=S$v1PA1*10
S$v2PA=S$v2PA1*10
S$a1PA=S$a1PA1*1
S$a2PA=S$a2PA1*1
S$v1_c=S$v1PA - (mean(S$v1PA, na.rm=T))
S$v2_c=S$v2PA - (mean(S$v2PA, na.rm=T))


S$PAadiff=S$a2PA-S$a1PA
S$PAvdiff=S$v1PA-S$v2PA
S$PATerdiff=S$Ter2PA1-S$Ter1PA1

library(forecast)

S$lamPA <- BoxCox(S$count_hr,-.4)
S$lamAtt<-BoxCox(S$L1AttAffective, 6)
hist(S$lamPA)
skew(S$count_hr)
S$lamPA2<-BoxCox(S$count_hr, 0)

library(psych)
descrip2<-describe(S)
write.table(descrip2, file = "C:/Users/hydea/Dropbox/Data analysis/EZ diff RR/Descrips2.csv", sep = ",", col.names = NA,
            qmethod = "double")

cor2<-cor(S, use="pairwise.complete.obs")
write.table(cor2, file = "C:/Users/hydea/Dropbox/Data analysis/EZ diff RR/Cors2.csv", sep = ",", col.names = NA,
            qmethod = "double")

# paired t-test
t.test(S$v1PA,S$v2PA,paired=TRUE)
t.test(S$a2PA,S$a1PA,paired=TRUE)
t.test(S$Ter2PA1,S$Ter1PA1,paired=TRUE)



##making interaction variable##

mean(S$PAadiff, na.rm=T)
S$PAadiff_c<-S$PAadiff-2.553523
mean(S$PAvdiff, na.rm=T)
S$PAvdiff_c<-S$PAvdiff-0.8639116


#test of multicollinearity##
library(car)
vif(d.fit)

d.fit <- lm(L1AttAffective ~ PATerdiff + PAvdiff + PAadiff, data=S)
summary(d.fit) # show results

d.fit <- lm(L1AttInstr ~ PATerdiff + PAvdiff + PAadiff, data=S)
summary(d.fit) # show results

d.fit <- lm(count_hr ~ L1IntPA + v1_c + v2_c + v1_c*v2_c, data=S)
summary(d.fit) # show results

d.fit <- lm(lamPA2 ~ L1IntPA + PATerdiff + PAvdiff + PAadiff, data=S)
summary(d.fit) # show results

d.fit <- lm(lamPA2 ~ L1IntPA + PATerdiff + PAvdiff + PAadiff + dKPA1, data=S)
summary(d.fit) # show results

d.fit <- lm(count_hr ~ L1IntPA + PATerdiff + PAvdiff + PAadiff + dKPA1, data=S)
summary(d.fit) # show results

d.fit <- lm(count_hr ~ L1IntPA + dKPA1 + PAvdiff, data=S)
summary(d.fit) # show results


##standardized Betas##
library(QuantPsyc)
StCoeff <- data.frame (Make.Z (S))

lm1.z <- lm (vStepsAVG ~ RhodesHabPAb_c + PA_v_c, data=StCoeff)
summary(lm1.z)

d.fit <- lm(lamAtt ~  v1_c + v2_c + v1_c*v2_c , data=S)
summary(d.fit) # show results


d.fit <- lm(lamAtt ~  PATerdiff + PAvdiff + PAadiff, data=S)
summary(d.fit) # show results


d.fit <- lm(L1IntPA_1 ~ PAadiff, data=S)
summary(d.fit) # show results













##iMeans of Accelerameter data##


study2 <-read.csv("C:\\Documents and Settings\\alh379\\My Documents\\Dropbox\\Auto Atts IAT Scoring\\study2acc.csv", na.strings=".", stringsAsFactors=FALSE)

study2$validDays<-ifelse(study2$StudyDay ==1|study2$StudyDay==14, 0,1)
study2a<-study2[ which(study2$validDays==1), ]
study2BWa<-tapply(study2a$id, study2a$id,mean)
study2BW<-data.frame(id=study2BWa)
count<-tapply(study2a$count_hr, study2a$id,mean, na.rm=T)
light_f<-tapply(study2a$light_f, study2a$id,mean, na.rm=T)
life_f<-tapply(study2a$lfstyl_f, study2a$id,mean, na.rm=T)
mod_f<-tapply(study2a$mod_f, study2a$id,mean, na.rm=T)
vig_f<-tapply(study2a$vig_f, study2a$id,mean, na.rm=T)
light_m<-tapply(study2a$light_m, study2a$id,mean, na.rm=T)
mod_m<-tapply(study2a$mod_m, study2a$id,mean, na.rm=T)
vig_m<-tapply(study2a$vig_m, study2a$id,mean, na.rm=T)

study2BW$count_hr<-count
study2BW$light_f<-light_f
study2BW$life_f<-life_f
study2BW$mod_f<-mod_f
study2BW$vig_f<-vig_f
study2BW$light_m<-light_m
study2BW$mod_m<-mod_m
study2BW$vig_m<-vig_m

write.table(study2BW, file = "C:/Documents and Settings/alh379/My Documents/Dropbox/Auto Atts IAT Scoring/PAfancy.csv", sep = ",", col.names = NA,
            qmethod = "double")


scatter.smooth(x=S$PAvdiff, y=S$lamPA2, pch=19,col="blue",
               ,ylab = "Physical Activity", xlab = " ")




###Automaticity and Daily Intentions####

#LAPTOP#
hab <-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\Habit and PA\\simp.base_study1.csv", na.strings=".", stringsAsFactors=FALSE)
day <-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\Habit and PA\\simp.daily_study1.csv", na.strings=".", stringsAsFactors=FALSE)

#WORK#
##hab <-read.csv("C:\\Documents and Settings\\alh379\\My Documents\\Dropbox\\Data Analyses\\Habit and PA\\simp.base_study1.csv", na.strings=".", stringsAsFactors=FALSE)
##day <-read.csv("C:\\Documents and Settings\\alh379\\My Documents\\Dropbox\\Data Analyses\\Habit and PA\\simp.daily_study1.csv", na.strings=".", stringsAsFactors=FALSE)

h <- merge(hab,day,by="id")

h$sleep.hrs=h$sleep.hrs/60
h$StudyDay=h$StudyDay-1
h$habit_c=h$L1PAHab-(mean(h$L1PAHab, na.rm=T))

bwa<-tapply(h$id, h$id,mean)
stress.iM<-tapply(h$stress, h$id,mean, na.rm=T)
sleep.qual.iM<-tapply(h$sleep.qual, h$id,mean,na.rm=T)
sleep.hrs.iM<-tapply(h$sleep.hrs, h$id,mean,na.rm=T)
int.iM<-tapply(h$DIntPA, h$id,mean,na.rm=T)
bw<-data.frame(id=bwa, stress.iM=stress.iM, sleep.qual.iM=sleep.qual.iM,
               sleep.hrs.iM=sleep.hrs.iM, int.iM=int.iM)

h<-merge(h, bw, by="id")

h$auto=(h$L1PAHab_2 + h$L1PAHab_3 + h$L1PAHab_5 + h$L1PAHab_8 + h$L1PAHab_10)/5
h$auto_c=h$auto-(mean(h$auto, na.rm=T))
h$habit_c=h$L1PAHab-(mean(h$L1PAHab, na.rm=T))
h$stress.iM_c=h$stress.iM-(mean(h$stress.iM, na.rm=T))
h$sleep.qual.iM_c=h$sleep.qual.iM-(mean(h$sleep.qual.iM, na.rm=T))
h$sleep.hrs.iM_c=h$sleep.hrs.iM-(mean(h$sleep.hrs.iM, na.rm=T))
h$int.iM_c=h$int.iM-(mean(h$int.iM, na.rm=T))


h$stress.r=h$stress-h$stress.iM
h$sleep.qual.r=h$sleep.qual-h$sleep.qual.iM
h$sleep.hrs.r=h$sleep.hrs-h$sleep.hrs.iM
h$int.r=h$DIntPA-h$int.iM

###Multilevel/MixedFX model - requires "nlme" package - no download needed
#Note: this is different from the lme4 package used before
#We will use this package to get individual level predictions
library(lme4)
library(arm)


#Linear time model (MLM)
haha <- lmer(count_hr ~ Sex + wkend + auto_c + int.iM_c + int.r + auto_c*int.r + auto_c*int.iM_c + (wkend|id) + (int.r|id), data=h)
summary(haha)


haha <- lmer(light_f ~ wkend + auto_c + int.iM_c + int.r + auto_c*int.r + auto_c*int.iM_c + auto_c*wkend + (wkend|id) + (int.r|id), data=h)
summary(haha)

haha <- lmer(mod_f ~ wkend + auto_c + int.iM_c + int.r + auto_c*int.r + auto_c*int.iM_c + auto_c*wkend + (wkend|id) + (int.r|id), data=h)
summary(haha)

haha <- lmer(vig_f ~ wkend + auto_c + int.iM_c + int.r + auto_c*int.r + auto_c*int.iM_c + auto_c*wkend + (wkend|id) + (int.r|id), data=h)
summary(haha)




##plotting interactions##
library(MBESS)

intr.plot.2d(b.0=29334.25, b.x=-17.87, b.z=675.89, b.xz=-22.36, x.min=0, x.max=100, mean.z=0, sd.z=1.243, legend.pos="topright")

##signficance of MLM coefficients based on simulations##
E1.sim <- sim (haha)
coef.E1.sim <- coef(E1.sim)
fixef.E1.sim <- fixef(E1.sim)
ranef.E1.sim <- ranef(E1.sim)
sigma.E1.sim <- sigma.hat(E1.sim)
p<-data.frame(fixef.E1.sim)
haha.sd<-sapply(p,sd)
haha.mean<-sapply(p,mean)
haha.ci<-data.frame(lo=(haha.mean-haha.sd*1.96), hi=(haha.mean+haha.sd*1.96) )
haha.ci

# write out text datafile and
# an SAS program to read it
library(foreign)
write.foreign(h, "C:/Users/alh379/Dropbox/Data Analyses/Habit and PA/h.txt", "C:/Users/alh379/Dropbox/Data Analyses/Habit and PA/h.sas",   package="SAS") 


cors<-cor(h, use="pairwise.complete.obs")
write.table(cors, file = "C:/Users/alh379/Dropbox/Data Analyses/Habit and PA/Cors.csv", sep = ",", col.names = NA,
            qmethod = "double")

##within-person correlations##
library(plyr)

withinCors<-ddply(h, .(id), summarise, cor=cor(count_hr, int.r, use="pairwise.complete.obs"))

##Intraclass correlations ICCs##
library(nlme)
library(multilevel)

ICC1.lme(count_hr, id, h)

ICC1.lme(DIntPA, id, h)

##plotting by id##

library(ggplot2)

qplot(StudyDay, count_hr, group=id, data=h, geom="line", color=id)





###Habit and Physical Activity and Stress####

#LAPTOP#
hab <-read.csv("C:\\Users\\alh379\\Dropbox\\Data Analyses\\Habit and PA\\simp.base_study1.csv", na.strings=".", stringsAsFactors=FALSE)
day <-read.csv("C:\\Users\\alh379\\Dropbox\\Data Analyses\\Habit and PA\\simp.daily_study1.csv", na.strings=".", stringsAsFactors=FALSE)

#WORK#
##hab <-read.csv("C:\\Documents and Settings\\alh379\\My Documents\\Dropbox\\Data Analyses\\Habit and PA\\simp.base_study1.csv", na.strings=".", stringsAsFactors=FALSE)
##day <-read.csv("C:\\Documents and Settings\\alh379\\My Documents\\Dropbox\\Data Analyses\\Habit and PA\\simp.daily_study1.csv", na.strings=".", stringsAsFactors=FALSE)

h <- merge(hab,day,by="id")

h$sleep.hrs=h$sleep.hrs/60
h$StudyDay=h$StudyDay-1
h$habit_c=h$L1PAHab-(mean(h$L1PAHab, na.rm=T))

bwa<-tapply(h$id, h$id,mean)
stress.iM<-tapply(h$stress, h$id,mean, na.rm=T)
sleep.qual.iM<-tapply(h$sleep.qual, h$id,mean,na.rm=T)
sleep.hrs.iM<-tapply(h$sleep.hrs, h$id,mean,na.rm=T)
int.iM<-tapply(h$DIntPA, h$id,mean,na.rm=T)
bw<-data.frame(id=bwa, stress.iM=stress.iM, sleep.qual.iM=sleep.qual.iM,
               sleep.hrs.iM=sleep.hrs.iM, int.iM=int.iM)

h<-merge(h, bw, by="id")

h$auto=(h$L1PAHab_2 + h$L1PAHab_3 + h$L1PAHab_5 + h$L1PAHab_8 + h$L1PAHab_10)/5
h$auto_c=h$auto-(mean(h$auto, na.rm=T))
h$habit_c=h$L1PAHab-(mean(h$L1PAHab, na.rm=T))
h$stress.iM_c=h$stress.iM-(mean(h$stress.iM, na.rm=T))
h$sleep.qual.iM_c=h$sleep.qual.iM-(mean(h$sleep.qual.iM, na.rm=T))
h$sleep.hrs.iM_c=h$sleep.hrs.iM-(mean(h$sleep.hrs.iM, na.rm=T))
h$int.iM_c=h$int.iM-(mean(h$int.iM, na.rm=T))


h$stress.r=h$stress-h$stress.iM
h$sleep.qual.r=h$sleep.qual-h$sleep.qual.iM
h$sleep.hrs.r=h$sleep.hrs-h$sleep.hrs.iM
h$int.r=h$DIntPA-h$int.iM

###Multilevel/MixedFX model - requires "nlme" package - no download needed
#Note: this is different from the lme4 package used before
#We will use this package to get individual level predictions
library(lme4)
library(arm)


#Linear time model (MLM)
haha <- lmer(count_hr ~ wkend + auto_c + int.iM_c + int.r + auto_c*int.r + auto_c*int.iM_c + (wkend|iid) + (int.r|id), data=h)
summary(haha)

#Formula: count_hr ~ wkend + auto_c + int.iM_c + int.r + auto_c * int.r +      auto_c * int.iM_c + (int.r | id) 
#Data: h 
#AIC   BIC logLik deviance REMLdev
#30789 30846 -15383    30842   30767
#Random effects:
#  Groups   Name        Variance  Std.Dev. Corr  
#id       (Intercept)  48603205  6971.60       
#int.r            3915    62.57 0.078 
#Residual             134403189 11593.24       
#Number of obs: 1421, groups: id, 129

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)     29334.25     752.22   39.00
#wkend           -2137.54     744.18   -2.87
#auto_c            675.89     584.45    1.16
#int.iM_c          117.50      31.17    3.77
#int.r             -17.87      16.18   -1.10
#auto_c:int.r      -22.36      11.81   -1.89
#auto_c:int.iM_c   -23.63      25.06   -0.94

#lo           hi
#X.Intercept.    27810.81510 31022.517554
#wkend           -3722.58563  -650.755206
#auto_c           -359.03951  1823.465685
#int.iM_c           57.29971   175.043389
#int.r             -46.68033    12.215440
#auto_c.int.r      -43.00727    -3.598225
#auto_c.int.iM_c   -69.43932    19.411970


haha <- lmer(light_f ~ wkend + auto_c + int.iM_c + int.r + auto_c*int.r + auto_c*int.iM_c + auto_c*wkend + (wkend|id) + (int.r|id), data=h)
summary(haha)

haha <- lmer(mod_f ~ wkend + auto_c + int.iM_c + int.r + auto_c*int.r + auto_c*int.iM_c + auto_c*wkend + (wkend|id) + (int.r|id), data=h)
summary(haha)

haha <- lmer(vig_f ~ wkend + auto_c + int.iM_c + int.r + auto_c*int.r + auto_c*int.iM_c + auto_c*wkend + (wkend|id) + (int.r|id), data=h)
summary(haha)




##plotting interactions##
library(MBESS)

intr.plot.2d(b.0=29334.25, b.x=-17.87, b.z=675.89, b.xz=-22.36, x.min=0, x.max=100, mean.z=0, sd.z=1.243, legend.pos="topright")

##signficance of MLM coefficients based on simulations##
E1.sim <- sim (haha)
coef.E1.sim <- coef(E1.sim)
fixef.E1.sim <- fixef(E1.sim)
ranef.E1.sim <- ranef(E1.sim)
sigma.E1.sim <- sigma.hat(E1.sim)
p<-data.frame(fixef.E1.sim)
haha.sd<-sapply(p,sd)
haha.mean<-sapply(p,mean)
haha.ci<-data.frame(lo=(haha.mean-haha.sd*1.96), hi=(haha.mean+haha.sd*1.96) )
haha.ci

# write out text datafile and
# an SAS program to read it
library(foreign)
write.foreign(h, "C:/Users/alh379/Dropbox/Data Analyses/Habit and PA/h.txt", "C:/Users/alh379/Dropbox/Data Analyses/Habit and PA/h.sas",   package="SAS") 


cors<-cor(h, use="pairwise.complete.obs")
write.table(cors, file = "C:/Users/alh379/Dropbox/Data Analyses/Habit and PA/Cors.csv", sep = ",", col.names = NA,
            qmethod = "double")

##within-person correlations##
library(plyr)

withinCors<-ddply(h, .(id), summarise, cor=cor(count_hr, int.r, use="pairwise.complete.obs"))

##Intraclass correlations ICCs##
library(nlme)
library(multilevel)

ICC1.lme(count_hr, id, h)

ICC1.lme(DIntPA, id, h)

##plotting by id##

library(ggplot2)

qplot(StudyDay, count_hr, group=id, data=h, geom="line", color=id)

###fancy OPenMx latent interactions model for stability of automatic evaluations paper##
##modeled using Little, Bovaird, & Widaman, 2006's orthogonal power and product terms##



##enter data##

stab <-read.csv("C:\\Users\\alh379\\SkyDrive\\Manuscripts\\Under Review\\PA Implicit Attitudes (In)Stability\\Data Analysis\\321 Stability of Imp_new.csv", na.strings=".", stringsAsFactors=FALSE)

##modeling stability of Imp Atts while testing measurement invariance across time##

require(OpenMx)

stab1=data.frame(id=stab$id, d11=stab$D1,d12=stab$D2,
                 d13=stab$D3, d21=stab$D1P, d22=stab$D2P, d23=stab$D3P,
                 metmin1=stab$metmin1/sd(stab$metmin1, na.rm=T),
                 metmin2=stab$metmin2/sd(stab$metmin2, na.rm=T), sex=stab$sex,
                 d11d21=stab$D1*stab$D1P, d11d22=stab$D1*stab$D2P,
                 d11d23=stab$D1*stab$D3P, d12d21=stab$D2*stab$D1P,
                 d12d22=stab$D2*stab$D2P, d12d23=stab$D2*stab$D3P,
                 d13d21=stab$D3*stab$D1P, d13d22=stab$D3*stab$D2P,
                 d13d23=stab$D3*stab$D3P)

library(gdata)

d11d21.fit<-lm(d11d21 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d11d21data<-data.frame(od11d21=d11d21.fit$residuals)
d11d21data$ida<-row.names(d11d21data)
d11d21data$ida<-as.numeric(d11d21data$ida)
d11d21data$id<-d11d21data$ida+100
d11d21data<-remove.vars(d11d21data, names="ida")
stab2<-merge(stab1, d11d21data, by="id")

d11d22.fit<-lm(d11d22 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d11d22data<-data.frame(od11d22=d11d22.fit$residuals)
d11d22data$ida<-row.names(d11d22data)
d11d22data$ida<-as.numeric(d11d22data$ida)
d11d22data$id<-d11d22data$ida+100
d11d22data<-remove.vars(d11d22data, names="ida")
stab3<-merge(stab2, d11d22data, by="id")

d11d23.fit<-lm(d11d23 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d11d23data<-data.frame(od11d23=d11d23.fit$residuals)
d11d23data$ida<-row.names(d11d23data)
d11d23data$ida<-as.numeric(d11d23data$ida)
d11d23data$id<-d11d23data$ida+100
d11d23data<-remove.vars(d11d23data, names="ida")
stab4<-merge(stab3, d11d23data, by="id")

d12d21.fit<-lm(d12d21 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d12d21data<-data.frame(od12d21=d12d21.fit$residuals)
d12d21data$ida<-row.names(d12d21data)
d12d21data$ida<-as.numeric(d12d21data$ida)
d12d21data$id<-d12d21data$ida+100
d12d21data<-remove.vars(d12d21data, names="ida")
stab5<-merge(stab4, d12d21data, by="id")

d12d22.fit<-lm(d12d22 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d12d22data<-data.frame(od12d22=d12d22.fit$residuals)
d12d22data$ida<-row.names(d12d22data)
d12d22data$ida<-as.numeric(d12d22data$ida)
d12d22data$id<-d12d22data$ida+100
d12d22data<-remove.vars(d12d22data, names="ida")
stab6<-merge(stab5, d12d22data, by="id")

d12d23.fit<-lm(d12d23 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d12d23data<-data.frame(od12d23=d12d23.fit$residuals)
d12d23data$ida<-row.names(d12d23data)
d12d23data$ida<-as.numeric(d12d23data$ida)
d12d23data$id<-d12d23data$ida+100
d12d23data<-remove.vars(d12d23data, names="ida")
stab7<-merge(stab6, d12d23data, by="id")

d13d21.fit<-lm(d13d21 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d13d21data<-data.frame(od13d21=d13d21.fit$residuals)
d13d21data$ida<-row.names(d13d21data)
d13d21data$ida<-as.numeric(d13d21data$ida)
d13d21data$id<-d13d21data$ida+100
d13d21data<-remove.vars(d13d21data, names="ida")
stab8<-merge(stab7, d13d21data, by="id")

d13d22.fit<-lm(d13d22 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d13d22data<-data.frame(od13d22=d13d22.fit$residuals)
d13d22data$ida<-row.names(d13d22data)
d13d22data$ida<-as.numeric(d13d22data$ida)
d13d22data$id<-d13d22data$ida+100
d13d22data<-remove.vars(d13d22data, names="ida")
stab9<-merge(stab8, d13d22data, by="id")

d13d23.fit<-lm(d13d23 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d13d23data<-data.frame(od13d23=d13d23.fit$residuals)
d13d23data$ida<-row.names(d13d23data)
d13d23data$ida<-as.numeric(d13d23data$ida)
d13d23data$id<-d13d23data$ida+100
d13d23data<-remove.vars(d13d23data, names="ida")
stab10<-merge(stab9, d13d23data, by="id")
stab10<-remove.vars(stab10, names=c("d11", "d12", "d13", "d21", "d22", "d23",
                                    "metmin1", "metmin2", "sex", "d11d21",
                                    "d11d22", "d11d23", "d12d21", "d12d22",
                                    "d12d23", "d13d21", "d13d22", "d13d23"))
stability<-merge(stab10, stab1, by="id", all=T)
stability<-remove.vars(stability, names=c("d11d21",
                                          "d11d22", "d11d23", "d12d21", "d12d22",
                                          "d12d23", "d13d21", "d13d22", "d13d23", "id"))


stabcov<-cov(stability, use = "pairwise.complete.obs")
stabMeans<-c((mean(stability$od11d21, na.rm=T)), (mean(stability$od11d22, na.rm=T)),
             (mean(stability$od11d23, na.rm=T)), (mean(stability$od12d21, na.rm=T)),
             (mean(stability$od12d22, na.rm=T)), (mean(stability$od12d23, na.rm=T)),
             (mean(stability$od13d21, na.rm=T)), (mean(stability$od13d22, na.rm=T)),
             (mean(stability$od13d23, na.rm=T)),
             (mean(stability$d11, na.rm=TRUE)), (mean(stability$d12, na.rm=TRUE)), (mean(stability$d13, na.rm=TRUE)),
             (mean(stability$d21, na.rm=TRUE)), (mean(stability$d22, na.rm=TRUE)), (mean(stability$d23, na.rm=TRUE)),
             (mean(stability$metmin1, na.rm=TRUE)), (mean(stability$metmin2, na.rm=TRUE)), mean(stability$sex, na.rm=T))

names(stabMeans)<-c("od11d21", "od11d22", "od11d23", "od12d21", "od12d22",
                    "od12d23", "od13d21", "od13d22", "od13d23",
                    "d11", "d12", "d13", "d21", "d22", "d23",
                    "metmin1", "metmin2", "sex")

manifest=c("od11d21", "od11d22", "od11d23", "od12d21", "od12d22",
           "od12d23", "od13d21", "od13d22", "od13d23",
           "d11", "d12", "d13", "d21", "d22", "d23",
           "metmin1", "metmin2", "sex")
latent=c("AE1","AE2", "AE1xAE2")



configural <- mxModel("Configural", 
                      type="RAM",
                      manifestVars=manifest,
                      latentVars=latent,
                      mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                      # manifest variances    
                      mxPath(
                        from=manifest,
                        arrows=2, free=TRUE, values = .8,
                        labels=c("rod11d21", "rod11d22", "rod11d23", "rod12d21", "rod12d22",
                                 "rod12d23", "rod13d21", "rod13d22", "rod13d23",
                                 "rd11", "rd12", "rd13", "rd21", "rd22", "rd23",
                                 "rmetmin1", "rmetmin2", "rsex")
                        ),
                      # latent variances
                      mxPath(
                        from=latent,
                        arrows=2,
                        free=c(FALSE,FALSE,FALSE),
                        values=c(1,1,1),
                        labels=c("rAE1", "rAE2", "rAE1xAE2")
                        ),
                      mxPath(
                        from=c("metmin1"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bmetmin1metmin2")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("d11","d12", "d13"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=c("AEd1", "AEd2", "AEd3")
                        ),
                      mxPath(
                        from=c("AE2"),
                        to=c("d21","d22", "d23"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=c("AEd1", "AEd2", "AEd3")
                        ),
                      mxPath(
                        from=c("AE1xAE2"),
                        to=c("od11d21", "od11d22", "od11d23",
                             "od12d21", "od12d22", "od12d23",
                             "od13d21", "od13d22", "od13d23"),
                        arrows=1,
                        free=TRUE,
                        values=.2,
                        labels=c("xod11d21", "xod11d22", "xod11d23",
                                 "xod12d21", "xod12d22", "xod12d23",
                                 "xod13d21", "xod13d22", "xod13d23")
                        ),
                      mxPath(
                        from=c("od11d21"),
                        to=c("od11d22", "od11d23", "od12d21", "od13d21"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od11d21od11d22", "od11d21od11d23", "od11d21od12d21", "od11d21od13d21")
                        ),
                      mxPath(
                        from=c("od11d22"),
                        to=c("od11d23", "od12d22", "od13d22"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od11d22od11d23", "od11d22od12d22", "od11d22od13d22")
                        ),
                      mxPath(
                        from=c("od12d21"),
                        to=c("od12d22", "od12d23", "od13d21"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od12d21od12d22", "od12d21od12d23", "od12d21od13d21")
                        ),                      
                      mxPath(
                        from=c("od12d22"),
                        to=c("od12d23", "od13d22"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od12d22od12d23", "od12d22od13d22")
                        ),                      
                      mxPath(
                        from=c("od13d21"),
                        to=c("od13d22", "od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od13d21od13d22", "od13d21od13d23")
                        ),                      
                      mxPath(
                        from=c("od13d22"),
                        to=c("od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od13d22od13d23")
                        ),                      
                      mxPath(
                        from=c("od11d23"),
                        to=c("od12d23", "od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od11d23od12d23", "od11d23od13d23")
                        ),                      
                      mxPath(
                        from=c("od12d23"),
                        to=c("od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od13d23od13d23")
                        ),
                      mxPath(
                        from=c("sex"),
                        to=c("metmin1"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bsexmetmin1")
                        ),
                      mxPath(
                        from=c("sex"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("sexmetmin2")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("AE2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1AE2")
                        ),
                      mxPath(
                        from=c("AE2"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE2metmin2")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE2metmin2")
                        ),
                      mxPath(
                        from=c("AE1xAE2"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1xAE2metmin2")
                        ),
                      # manifest means
                      mxPath(
                        from="one",
                        to=manifest,
                        arrows=1,
                        free=TRUE,
                        values=.2,
                        labels=c("mod11d21", "mod11d22", "mod11d23", "mod12d21", "mod12d22",
                                 "mod12d23", "mod13d21", "mod13d22", "mod13d23",
                                 "md11", "md12", "md13", "md21", "md22", "md23",
                                 "mmetmin1", "mmetmin2", "msex")
                        ),
                      # latent means
                      mxPath(
                        from="one",
                        to=latent,
                        arrows=1,
                        free=FALSE,
                        values=0,
                        labels=c("mAE1", "mAE2", "mAE1xAE2")
                        ))  


# ----------------------------------------
# run model and examine output

config <- mxRun(configural)
summary(config)






####same model but with steps instead of self-reported PA#####


##enter data##

stab <-read.csv("C:\\Users\\alh379\\SkyDrive\\Manuscripts\\Under Review\\PA Implicit Attitudes (In)Stability\\Data Analysis\\321 Stability of Imp_new.csv", na.strings=".", stringsAsFactors=FALSE)

##modeling stability of Imp Atts while testing measurement invariance across time##

require(OpenMx)

stab1=data.frame(id=stab$id, d11=stab$D1,d12=stab$D2,
                 d13=stab$D3, d21=stab$D1P, d22=stab$D2P, d23=stab$D3P,
                 steps=stab$AdjStepsB/sd(stab$AdjStepsB, na.rm=T), sex=stab$sex,
                 d11d21=stab$D1*stab$D1P, d11d22=stab$D1*stab$D2P,
                 d11d23=stab$D1*stab$D3P, d12d21=stab$D2*stab$D1P,
                 d12d22=stab$D2*stab$D2P, d12d23=stab$D2*stab$D3P,
                 d13d21=stab$D3*stab$D1P, d13d22=stab$D3*stab$D2P,
                 d13d23=stab$D3*stab$D3P)

library(gdata)

d11d21.fit<-lm(d11d21 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d11d21data<-data.frame(od11d21=d11d21.fit$residuals)
d11d21data$ida<-row.names(d11d21data)
d11d21data$ida<-as.numeric(d11d21data$ida)
d11d21data$id<-d11d21data$ida+100
d11d21data<-remove.vars(d11d21data, names="ida")
stab2<-merge(stab1, d11d21data, by="id")

d11d22.fit<-lm(d11d22 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d11d22data<-data.frame(od11d22=d11d22.fit$residuals)
d11d22data$ida<-row.names(d11d22data)
d11d22data$ida<-as.numeric(d11d22data$ida)
d11d22data$id<-d11d22data$ida+100
d11d22data<-remove.vars(d11d22data, names="ida")
stab3<-merge(stab2, d11d22data, by="id")

d11d23.fit<-lm(d11d23 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d11d23data<-data.frame(od11d23=d11d23.fit$residuals)
d11d23data$ida<-row.names(d11d23data)
d11d23data$ida<-as.numeric(d11d23data$ida)
d11d23data$id<-d11d23data$ida+100
d11d23data<-remove.vars(d11d23data, names="ida")
stab4<-merge(stab3, d11d23data, by="id")

d12d21.fit<-lm(d12d21 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d12d21data<-data.frame(od12d21=d12d21.fit$residuals)
d12d21data$ida<-row.names(d12d21data)
d12d21data$ida<-as.numeric(d12d21data$ida)
d12d21data$id<-d12d21data$ida+100
d12d21data<-remove.vars(d12d21data, names="ida")
stab5<-merge(stab4, d12d21data, by="id")

d12d22.fit<-lm(d12d22 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d12d22data<-data.frame(od12d22=d12d22.fit$residuals)
d12d22data$ida<-row.names(d12d22data)
d12d22data$ida<-as.numeric(d12d22data$ida)
d12d22data$id<-d12d22data$ida+100
d12d22data<-remove.vars(d12d22data, names="ida")
stab6<-merge(stab5, d12d22data, by="id")

d12d23.fit<-lm(d12d23 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d12d23data<-data.frame(od12d23=d12d23.fit$residuals)
d12d23data$ida<-row.names(d12d23data)
d12d23data$ida<-as.numeric(d12d23data$ida)
d12d23data$id<-d12d23data$ida+100
d12d23data<-remove.vars(d12d23data, names="ida")
stab7<-merge(stab6, d12d23data, by="id")

d13d21.fit<-lm(d13d21 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d13d21data<-data.frame(od13d21=d13d21.fit$residuals)
d13d21data$ida<-row.names(d13d21data)
d13d21data$ida<-as.numeric(d13d21data$ida)
d13d21data$id<-d13d21data$ida+100
d13d21data<-remove.vars(d13d21data, names="ida")
stab8<-merge(stab7, d13d21data, by="id")

d13d22.fit<-lm(d13d22 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d13d22data<-data.frame(od13d22=d13d22.fit$residuals)
d13d22data$ida<-row.names(d13d22data)
d13d22data$ida<-as.numeric(d13d22data$ida)
d13d22data$id<-d13d22data$ida+100
d13d22data<-remove.vars(d13d22data, names="ida")
stab9<-merge(stab8, d13d22data, by="id")

d13d23.fit<-lm(d13d23 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d13d23data<-data.frame(od13d23=d13d23.fit$residuals)
d13d23data$ida<-row.names(d13d23data)
d13d23data$ida<-as.numeric(d13d23data$ida)
d13d23data$id<-d13d23data$ida+100
d13d23data<-remove.vars(d13d23data, names="ida")
stab10<-merge(stab9, d13d23data, by="id")
stab10<-remove.vars(stab10, names=c("d11", "d12", "d13", "d21", "d22", "d23",
                                    "steps", "sex", "d11d21",
                                    "d11d22", "d11d23", "d12d21", "d12d22",
                                    "d12d23", "d13d21", "d13d22", "d13d23"))
stability<-merge(stab10, stab1, by="id", all=T)
stability<-remove.vars(stability, names=c("d11d21",
                                          "d11d22", "d11d23", "d12d21", "d12d22",
                                          "d12d23", "d13d21", "d13d22", "d13d23", "id"))


stabcov<-cov(stability, use = "pairwise.complete.obs")
stabMeans<-c((mean(stability$od11d21, na.rm=T)), (mean(stability$od11d22, na.rm=T)),
             (mean(stability$od11d23, na.rm=T)), (mean(stability$od12d21, na.rm=T)),
             (mean(stability$od12d22, na.rm=T)), (mean(stability$od12d23, na.rm=T)),
             (mean(stability$od13d21, na.rm=T)), (mean(stability$od13d22, na.rm=T)),
             (mean(stability$od13d23, na.rm=T)),
             (mean(stability$d11, na.rm=TRUE)), (mean(stability$d12, na.rm=TRUE)), (mean(stability$d13, na.rm=TRUE)),
             (mean(stability$d21, na.rm=TRUE)), (mean(stability$d22, na.rm=TRUE)), (mean(stability$d23, na.rm=TRUE)),
             (mean(stability$steps, na.rm=TRUE)), mean(stability$sex, na.rm=T))

names(stabMeans)<-c("od11d21", "od11d22", "od11d23", "od12d21", "od12d22",
                    "od12d23", "od13d21", "od13d22", "od13d23",
                    "d11", "d12", "d13", "d21", "d22", "d23",
                    "steps", "sex")

manifest=c("od11d21", "od11d22", "od11d23", "od12d21", "od12d22",
           "od12d23", "od13d21", "od13d22", "od13d23",
           "d11", "d12", "d13", "d21", "d22", "d23",
           "steps", "sex")
latent=c("AE1","AE2", "AE1xAE2")



configural <- mxModel("Configural", 
                      type="RAM",
                      manifestVars=manifest,
                      latentVars=latent,
                      mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                      # manifest variances    
                      mxPath(
                        from=manifest,
                        arrows=2, free=TRUE, values = .8,
                        labels=c("rod11d21", "rod11d22", "rod11d23", "rod12d21", "rod12d22",
                                 "rod12d23", "rod13d21", "rod13d22", "rod13d23",
                                 "rd11", "rd12", "rd13", "rd21", "rd22", "rd23",
                                 "rsteps", "rsex")
                        ),
                      # latent variances
                      mxPath(
                        from=latent,
                        arrows=2,
                        free=c(FALSE,FALSE,FALSE),
                        values=c(1,1,1),
                        labels=c("rAE1", "rAE2", "rAE1xAE2")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("d11","d12", "d13"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=c("AEd1", "AEd2", "AEd3")
                        ),
                      mxPath(
                        from=c("AE2"),
                        to=c("d21","d22", "d23"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=c("AEd1", "AEd2", "AEd3")
                        ),
                      mxPath(
                        from=c("AE1xAE2"),
                        to=c("od11d21", "od11d22", "od11d23",
                             "od12d21", "od12d22", "od12d23",
                             "od13d21", "od13d22", "od13d23"),
                        arrows=1,
                        free=TRUE,
                        values=.2,
                        labels=c("xod11d21", "xod11d22", "xod11d23",
                                 "xod12d21", "xod12d22", "xod12d23",
                                 "xod13d21", "xod13d22", "xod13d23")
                        ),
                      mxPath(
                        from=c("od11d21"),
                        to=c("od11d22", "od11d23", "od12d21", "od13d21"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od11d21od11d22", "od11d21od11d23", "od11d21od12d21", "od11d21od13d21")
                        ),
                      mxPath(
                        from=c("od11d22"),
                        to=c("od11d23", "od12d22", "od13d22"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od11d22od11d23", "od11d22od12d22", "od11d22od13d22")
                        ),
                      mxPath(
                        from=c("od12d21"),
                        to=c("od12d22", "od12d23", "od13d21"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od12d21od12d22", "od12d21od12d23", "od12d21od13d21")
                        ),                      
                      mxPath(
                        from=c("od12d22"),
                        to=c("od12d23", "od13d22"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od12d22od12d23", "od12d22od13d22")
                        ),                      
                      mxPath(
                        from=c("od13d21"),
                        to=c("od13d22", "od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od13d21od13d22", "od13d21od13d23")
                        ),                      
                      mxPath(
                        from=c("od13d22"),
                        to=c("od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od13d22od13d23")
                        ),                      
                      mxPath(
                        from=c("od11d23"),
                        to=c("od12d23", "od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od11d23od12d23", "od11d23od13d23")
                        ),                      
                      mxPath(
                        from=c("od12d23"),
                        to=c("od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od13d23od13d23")
                        ),
                      mxPath(
                        from=c("sex"),
                        to=c("steps"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("sexsteps")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("AE2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1AE2")
                        ),
                      mxPath(
                        from=c("AE2"),
                        to=c("steps"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE2steps")
                        ),
                      mxPath(
                        from=c("AE1xAE2"),
                        to=c("steps"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1xAE2steps")
                        ),
                      # manifest means
                      mxPath(
                        from="one",
                        to=manifest,
                        arrows=1,
                        free=TRUE,
                        values=.2,
                        labels=c("mod11d21", "mod11d22", "mod11d23", "mod12d21", "mod12d22",
                                 "mod12d23", "mod13d21", "mod13d22", "mod13d23",
                                 "md11", "md12", "md13", "md21", "md22", "md23",
                                 "steps", "msex")
                        ),
                      # latent means
                      mxPath(
                        from="one",
                        to=latent,
                        arrows=1,
                        free=FALSE,
                        values=0,
                        labels=c("mAE1", "mAE2", "mAE1xAE2")
                        ))  


# ----------------------------------------
# run model and examine output

config <- mxRun(configural)
summary(config)




###same model but with self-reported and steps combined in one model (not used in MS) ##


##enter data##

stab <-read.csv("C:\\Users\\alh379\\SkyDrive\\Manuscripts\\Under Review\\PA Implicit Attitudes (In)Stability\\Data Analysis\\321 Stability of Imp_new.csv", na.strings=".", stringsAsFactors=FALSE)

##modeling stability of Imp Atts while testing measurement invariance across time##

require(OpenMx)

stab1=data.frame(id=stab$id, d11=stab$D1,d12=stab$D2,
                 d13=stab$D3, d21=stab$D1P, d22=stab$D2P, d23=stab$D3P,
                 metmin1=stab$metmin1/sd(stab$metmin1, na.rm=T),
                 metmin2=stab$metmin2/sd(stab$metmin2, na.rm=T),
                 steps=stab$AdjStepsB/sd(stab$AdjStepsB, na.rm=T), sex=stab$sex,
                 d11d21=stab$D1*stab$D1P, d11d22=stab$D1*stab$D2P,
                 d11d23=stab$D1*stab$D3P, d12d21=stab$D2*stab$D1P,
                 d12d22=stab$D2*stab$D2P, d12d23=stab$D2*stab$D3P,
                 d13d21=stab$D3*stab$D1P, d13d22=stab$D3*stab$D2P,
                 d13d23=stab$D3*stab$D3P)

library(gdata)

d11d21.fit<-lm(d11d21 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d11d21data<-data.frame(od11d21=d11d21.fit$residuals)
d11d21data$ida<-row.names(d11d21data)
d11d21data$ida<-as.numeric(d11d21data$ida)
d11d21data$id<-d11d21data$ida+100
d11d21data<-remove.vars(d11d21data, names="ida")
stab2<-merge(stab1, d11d21data, by="id")

d11d22.fit<-lm(d11d22 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d11d22data<-data.frame(od11d22=d11d22.fit$residuals)
d11d22data$ida<-row.names(d11d22data)
d11d22data$ida<-as.numeric(d11d22data$ida)
d11d22data$id<-d11d22data$ida+100
d11d22data<-remove.vars(d11d22data, names="ida")
stab3<-merge(stab2, d11d22data, by="id")

d11d23.fit<-lm(d11d23 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d11d23data<-data.frame(od11d23=d11d23.fit$residuals)
d11d23data$ida<-row.names(d11d23data)
d11d23data$ida<-as.numeric(d11d23data$ida)
d11d23data$id<-d11d23data$ida+100
d11d23data<-remove.vars(d11d23data, names="ida")
stab4<-merge(stab3, d11d23data, by="id")

d12d21.fit<-lm(d12d21 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d12d21data<-data.frame(od12d21=d12d21.fit$residuals)
d12d21data$ida<-row.names(d12d21data)
d12d21data$ida<-as.numeric(d12d21data$ida)
d12d21data$id<-d12d21data$ida+100
d12d21data<-remove.vars(d12d21data, names="ida")
stab5<-merge(stab4, d12d21data, by="id")

d12d22.fit<-lm(d12d22 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d12d22data<-data.frame(od12d22=d12d22.fit$residuals)
d12d22data$ida<-row.names(d12d22data)
d12d22data$ida<-as.numeric(d12d22data$ida)
d12d22data$id<-d12d22data$ida+100
d12d22data<-remove.vars(d12d22data, names="ida")
stab6<-merge(stab5, d12d22data, by="id")

d12d23.fit<-lm(d12d23 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d12d23data<-data.frame(od12d23=d12d23.fit$residuals)
d12d23data$ida<-row.names(d12d23data)
d12d23data$ida<-as.numeric(d12d23data$ida)
d12d23data$id<-d12d23data$ida+100
d12d23data<-remove.vars(d12d23data, names="ida")
stab7<-merge(stab6, d12d23data, by="id")

d13d21.fit<-lm(d13d21 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d13d21data<-data.frame(od13d21=d13d21.fit$residuals)
d13d21data$ida<-row.names(d13d21data)
d13d21data$ida<-as.numeric(d13d21data$ida)
d13d21data$id<-d13d21data$ida+100
d13d21data<-remove.vars(d13d21data, names="ida")
stab8<-merge(stab7, d13d21data, by="id")

d13d22.fit<-lm(d13d22 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d13d22data<-data.frame(od13d22=d13d22.fit$residuals)
d13d22data$ida<-row.names(d13d22data)
d13d22data$ida<-as.numeric(d13d22data$ida)
d13d22data$id<-d13d22data$ida+100
d13d22data<-remove.vars(d13d22data, names="ida")
stab9<-merge(stab8, d13d22data, by="id")

d13d23.fit<-lm(d13d23 ~ d11 + d12 + d13 + d21 + d22 + d23, data=stab1)
d13d23data<-data.frame(od13d23=d13d23.fit$residuals)
d13d23data$ida<-row.names(d13d23data)
d13d23data$ida<-as.numeric(d13d23data$ida)
d13d23data$id<-d13d23data$ida+100
d13d23data<-remove.vars(d13d23data, names="ida")
stab10<-merge(stab9, d13d23data, by="id")
stab10<-remove.vars(stab10, names=c("d11", "d12", "d13", "d21", "d22", "d23",
                                    "metmin1", "metmin2", "steps", "sex", "d11d21",
                                    "d11d22", "d11d23", "d12d21", "d12d22",
                                    "d12d23", "d13d21", "d13d22", "d13d23"))
stability<-merge(stab10, stab1, by="id", all=T)
stability<-remove.vars(stability, names=c("d11d21",
                                          "d11d22", "d11d23", "d12d21", "d12d22",
                                          "d12d23", "d13d21", "d13d22", "d13d23", "id"))


stabcov<-cov(stability, use = "pairwise.complete.obs")
stabMeans<-c((mean(stability$od11d21, na.rm=T)), (mean(stability$od11d22, na.rm=T)),
             (mean(stability$od11d23, na.rm=T)), (mean(stability$od12d21, na.rm=T)),
             (mean(stability$od12d22, na.rm=T)), (mean(stability$od12d23, na.rm=T)),
             (mean(stability$od13d21, na.rm=T)), (mean(stability$od13d22, na.rm=T)),
             (mean(stability$od13d23, na.rm=T)),
             (mean(stability$d11, na.rm=TRUE)), (mean(stability$d12, na.rm=TRUE)), (mean(stability$d13, na.rm=TRUE)),
             (mean(stability$d21, na.rm=TRUE)), (mean(stability$d22, na.rm=TRUE)), (mean(stability$d23, na.rm=TRUE)),
             (mean(stability$metmin1, na.rm=TRUE)),
             (mean(stability$metmin2, na.rm=TRUE)),
             (mean(stability$steps, na.rm=TRUE)),
             mean(stability$sex, na.rm=T))

names(stabMeans)<-c("od11d21", "od11d22", "od11d23", "od12d21", "od12d22",
                    "od12d23", "od13d21", "od13d22", "od13d23",
                    "d11", "d12", "d13", "d21", "d22", "d23",
                    "metmin1", "metmin2", "steps", "sex")

manifest=c("od11d21", "od11d22", "od11d23", "od12d21", "od12d22",
           "od12d23", "od13d21", "od13d22", "od13d23",
           "d11", "d12", "d13", "d21", "d22", "d23",
           "metmin1", "metmin2", "steps", "sex")
latent=c("AE1","AE2", "AE1xAE2")



configural <- mxModel("Configural", 
                      type="RAM",
                      manifestVars=manifest,
                      latentVars=latent,
                      mxData(stabcov,type="cov", numObs=164, means=stabMeans),
                      # manifest variances    
                      mxPath(
                        from=manifest,
                        arrows=2, free=TRUE, values = .8,
                        labels=c("rod11d21", "rod11d22", "rod11d23", "rod12d21", "rod12d22",
                                 "rod12d23", "rod13d21", "rod13d22", "rod13d23",
                                 "rd11", "rd12", "rd13", "rd21", "rd22", "rd23",
                                 "rmetmin1", "rmetmin2", "rsteps", "rsex")
                        ),
                      # latent variances
                      mxPath(
                        from=latent,
                        arrows=2,
                        free=c(FALSE,FALSE,FALSE),
                        values=c(1,1,1),
                        labels=c("rAE1", "rAE2", "rAE1xAE2")
                        ),
                      mxPath(
                        from=c("metmin1"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bmetmin1metmin2")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("d11","d12", "d13"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=c("AEd1", "AEd2", "AEd3")
                        ),
                      mxPath(
                        from=c("AE2"),
                        to=c("d21","d22", "d23"),
                        arrows=1,
                        free=TRUE,
                        values=c(.2, .2, .2),
                        labels=c("AEd1", "AEd2", "AEd3")
                        ),
                      mxPath(
                        from=c("AE1xAE2"),
                        to=c("od11d21", "od11d22", "od11d23",
                             "od12d21", "od12d22", "od12d23",
                             "od13d21", "od13d22", "od13d23"),
                        arrows=1,
                        free=TRUE,
                        values=.2,
                        labels=c("xod11d21", "xod11d22", "xod11d23",
                                 "xod12d21", "xod12d22", "xod12d23",
                                 "xod13d21", "xod13d22", "xod13d23")
                        ),
                      mxPath(
                        from=c("od11d21"),
                        to=c("od11d22", "od11d23", "od12d21", "od13d21"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od11d21od11d22", "od11d21od11d23", "od11d21od12d21", "od11d21od13d21")
                        ),
                      mxPath(
                        from=c("od11d22"),
                        to=c("od11d23", "od12d22", "od13d22"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od11d22od11d23", "od11d22od12d22", "od11d22od13d22")
                        ),
                      mxPath(
                        from=c("od12d21"),
                        to=c("od12d22", "od12d23", "od13d21"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od12d21od12d22", "od12d21od12d23", "od12d21od13d21")
                        ),                      
                      mxPath(
                        from=c("od12d22"),
                        to=c("od12d23", "od13d22"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od12d22od12d23", "od12d22od13d22")
                        ),                      
                      mxPath(
                        from=c("od13d21"),
                        to=c("od13d22", "od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od13d21od13d22", "od13d21od13d23")
                        ),                      
                      mxPath(
                        from=c("od13d22"),
                        to=c("od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od13d22od13d23")
                        ),                      
                      mxPath(
                        from=c("od11d23"),
                        to=c("od12d23", "od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od11d23od12d23", "od11d23od13d23")
                        ),                      
                      mxPath(
                        from=c("od12d23"),
                        to=c("od13d23"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("od13d23od13d23")
                        ),
                      mxPath(
                        from=c("sex"),
                        to=c("metmin1"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bsexmetmin1")
                        ),
                      mxPath(
                        from=c("sex"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("sexmetmin2")
                        ),
                      mxPath(
                        from=c("sex"),
                        to=c("steps"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("sexsteps")
                        ),
                      mxPath(
                        from=c("metmin1"),
                        to=c("steps"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("stepsmetmin1")
                        ),
                      mxPath(
                        from=c("steps"),
                        to=c("metmin2"),
                        arrows=2,
                        free=TRUE,
                        values=.05,
                        labels=c("stepsmetmin2")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("AE2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1AE2")
                        ),
                      
                      mxPath(
                        from=c("AE1"),
                        to=c("metmin1"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1metmin1")
                        ),
                      mxPath(
                        from=c("AE2"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE2metmin2")
                        ),
                      mxPath(
                        from=c("AE2"),
                        to=c("steps"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE2steps")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("steps"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1steps")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1steps")
                        ),
                      mxPath(
                        from=c("AE1xAE2"),
                        to=c("steps"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1xAE2steps")
                        ),
                      mxPath(
                        from=c("AE1"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE2metmin2")
                        ),
                      mxPath(
                        from=c("AE1xAE2"),
                        to=c("metmin2"),
                        arrows=1,
                        free=TRUE,
                        values=.05,
                        labels=c("bAE1xAE2metmin2")
                        ),
                      # manifest means
                      mxPath(
                        from="one",
                        to=manifest,
                        arrows=1,
                        free=TRUE,
                        values=.2,
                        labels=c("mod11d21", "mod11d22", "mod11d23", "mod12d21", "mod12d22",
                                 "mod12d23", "mod13d21", "mod13d22", "mod13d23",
                                 "md11", "md12", "md13", "md21", "md22", "md23",
                                 "mmetmin1", "mmetmin2", "msex")
                        ),
                      # latent means
                      mxPath(
                        from="one",
                        to=latent,
                        arrows=1,
                        free=FALSE,
                        values=0,
                        labels=c("mAE1", "mAE2", "mAE1xAE2")
                        ))  


# ----------------------------------------
# run model and examine output

config <- mxRun(configural)
summary(config)





#########Operation CF #######################


##entering data##

djh <-read.csv("C:\\Users\\alh379\\SkyDrive\\Data Analyses\\Operation CF\\djh.csv", na.strings=".", stringsAsFactors=FALSE)


##scoring scales##
djh$age<-((djh$agey*12)+djh$agem)/12
djh$lteq<-(djh$Pastren*9+djh$Pamod*5)
djh$PAmin<-(djh$Pastren+djh$Pamod)*djh$Padur
djh$PAintent<-(djh$Paint*djh$Paintdur)
djh$PAintent2<-(djh$PAintint1+djh$PAintint2)/2
djh$stress<-(djh$stress01 + djh$stress02 + djh$stress03)/3
djh$ACintent<-(djh$Acint*djh$Acintdur)
djh$ACintent2<-(djh$Acintint1+djh$Acintint2)/2
djh$AcSRE<-(djh$AcSE01 + djh$AcSE02 + djh$AcSE03 + djh$AcSE04 + 
  djh$AcSE05 + djh$AcSE06 + djh$AcSE07 + djh$AcSE08)/8
djh$ccSRE<-(djh$AcSE09 +
  djh$AcSE10 + djh$AcSE11 + djh$AcSE12 + djh$AcSE13)/5
djh$PASRE<-(djh$PASE1 + djh$PASE2 + djh$PASE3 + djh$PASE4 + djh$PASE5
            + djh$PASE6 + djh$PASE7 + djh$PASE8)/8
djh$impPA<-djh$imp5
djh$impAc<-djh$imp3
djh$impNoPA<-(djh$imp1 + djh$imp2 + djh$imp3 + djh$imp4 + djh$imp6 + djh$imp7)/6
djh$impNoAc<-(djh$imp1 + djh$imp2 + djh$imp4 + djh$imp5 + djh$imp6 + djh$imp7)/6
djh$impPAw<-djh$impPA/djh$impNoPA
djh$impAcw<-djh$impAc/djh$impNoAc
djh$impPAAcdiff<-djh$impPA-djh$impAc


##interitem reliability of SRE scales - ignoring nesting (all time points) - ##

library(ltm)

AcSRE=data.frame(djh$AcSE01, djh$AcSE02, djh$AcSE03, djh$AcSE04,
                 djh$AcSE05, djh$AcSE06, djh$AcSE07, djh$AcSE08, djh$AcSE09,
                 djh$AcSE10, djh$AcSE11, djh$AcSE12, djh$AcSE13)
PASRE=data.frame(djh$PASE1, djh$PASE2, djh$PASE3, djh$PASE4, djh$PASE5,
                 djh$PASE6, djh$PASE7, djh$PASE8)

cronbach.alpha(AcSRE, na.rm=TRUE) ##.913##
cronbach.alpha(PASRE, na.rm=TRUE) ##.916##






##cutting those without 4 time points##
djh2<-djh[which(djh$rep==4), ]






##descriptives and correlations##

library(psych)
descrips<-describe(djh)
write.table(descrips, file = "C:/Users/alh379/SkyDrive/Data Analyses/DJH/Descrips.csv", sep = ",", col.names = NA,
            qmethod = "double")

cor<-cor(djh, use="pairwise.complete.obs")
write.table(cor, file = "C:/Users/alh379/SkyDrive/Data Analyses/DJH/Cors.csv", sep = ",", col.names = NA,
            qmethod = "double")

descrips2<-describe(djh2)
write.table(descrips2, file = "C:/Users/alh379/SkyDrive/Data Analyses/DJH/Descrips2.csv", sep = ",", col.names = NA,
            qmethod = "double")

cor2<-cor(djh2, use="pairwise.complete.obs")
write.table(cor2, file = "C:/Users/alh379/SkyDrive/Data Analyses/DJH/Cors2.csv", sep = ",", col.names = NA,
            qmethod = "double")


##reshaping the long data into wide data##

djh2_wide<-reshape(djh2, idvar = "id", timevar = "time", direction = "wide")

write.table(djh2_wide, file = "C:/Users/alh379/SkyDrive/Data Analyses/DJH/djh_wide.csv", sep = ",", col.names = NA,
            qmethod = "double")

##interitem reliability of each time point##

AcSRE.0=data.frame(djh2_wide$AcSE01.0, djh2_wide$AcSE02.0, djh2_wide$AcSE03.0, djh2_wide$AcSE04.0,
                   djh2_wide$AcSE05.0, djh2_wide$AcSE06.0, djh2_wide$AcSE07.0, djh2_wide$AcSE08.0, djh2_wide$AcSE09.0,
                   djh2_wide$AcSE10.0, djh2_wide$AcSE11.0, djh2_wide$AcSE12.0, djh2_wide$AcSE13.0)
PASRE.0=data.frame(djh2_wide$PASE1.0, djh2_wide$PASE2.0, djh2_wide$PASE3.0, djh2_wide$PASE4.0, djh2_wide$PASE5.0,
                   djh2_wide$PASE6.0, djh2_wide$PASE7.0, djh2_wide$PASE8.0)

cronbach.alpha(AcSRE.0, na.rm=TRUE) ##.925##
cronbach.alpha(PASRE.0, na.rm=TRUE) ##.864##

AcSRE.4=data.frame(djh2_wide$AcSE01.4, djh2_wide$AcSE02.4, djh2_wide$AcSE03.4, djh2_wide$AcSE04.4,
                   djh2_wide$AcSE05.4, djh2_wide$AcSE06.4, djh2_wide$AcSE07.4, djh2_wide$AcSE08.4, djh2_wide$AcSE09.4,
                   djh2_wide$AcSE10.4, djh2_wide$AcSE11.4, djh2_wide$AcSE12.4, djh2_wide$AcSE13.4)
PASRE.4=data.frame(djh2_wide$PASE1.4, djh2_wide$PASE2.4, djh2_wide$PASE3.4, djh2_wide$PASE4.4, djh2_wide$PASE5.4,
                   djh2_wide$PASE6.4, djh2_wide$PASE7.4, djh2_wide$PASE8.4)

cronbach.alpha(AcSRE.4, na.rm=TRUE) ##.702##
cronbach.alpha(PASRE.4, na.rm=TRUE) ##.987##

AcSRE.8=data.frame(djh2_wide$AcSE01.8, djh2_wide$AcSE02.8, djh2_wide$AcSE03.8, djh2_wide$AcSE04.8,
                   djh2_wide$AcSE05.8, djh2_wide$AcSE06.8, djh2_wide$AcSE07.8, djh2_wide$AcSE08.8, djh2_wide$AcSE09.8,
                   djh2_wide$AcSE10.8, djh2_wide$AcSE11.8, djh2_wide$AcSE12.8, djh2_wide$AcSE13.8)
PASRE.8=data.frame(djh2_wide$PASE1.8, djh2_wide$PASE2.8, djh2_wide$PASE3.8, djh2_wide$PASE4.8, djh2_wide$PASE5.8,
                   djh2_wide$PASE6.8, djh2_wide$PASE7.8, djh2_wide$PASE8.8)

cronbach.alpha(AcSRE.8, na.rm=TRUE) ##.916##
cronbach.alpha(PASRE.8, na.rm=TRUE) ##.854##


AcSRE.12=data.frame(djh2_wide$AcSE01.12, djh2_wide$AcSE02.12, djh2_wide$AcSE03.12, djh2_wide$AcSE04.12,
                    djh2_wide$AcSE05.12, djh2_wide$AcSE06.12, djh2_wide$AcSE07.12, djh2_wide$AcSE08.12, djh2_wide$AcSE09.12,
                    djh2_wide$AcSE10.12, djh2_wide$AcSE11.12, djh2_wide$AcSE12.12, djh2_wide$AcSE13.12)
PASRE.12=data.frame(djh2_wide$PASE1.12, djh2_wide$PASE2.12, djh2_wide$PASE3.12, djh2_wide$PASE4.12, djh2_wide$PASE5.12,
                    djh2_wide$PASE6.12, djh2_wide$PASE7.12, djh2_wide$PASE8.12)

cronbach.alpha(AcSRE.12, na.rm=TRUE) ##.925##
cronbach.alpha(PASRE.12, na.rm=TRUE) ##.902##

##histogram##
time12<-icf[which(icf$time==12), ]
time8<-icf[which(icf$time==8), ]
time4<-icf[which(icf$time==4), ]
time0<-icf[which(icf$time==0), ]
hist(time0$Acintbeh)


cf<-djh2

##windzoring DVs##
cf$PA <-ifelse(cf$PAmin<(mean(cf$PAmin, na.rm=T)+3*sd(cf$PAmin, na.rm=T)), cf$PAmin,cf$PAmin<(mean(cf$PAmin, na.rm=T)+3*sd(cf$PAmin, na.rm=T)))
cf$PA2 <-ifelse(cf$lteq<(mean(cf$lteq, na.rm=T)+3*sd(cf$lteq, na.rm=T)), cf$lteq,cf$lteq<(mean(cf$lteq, na.rm=T)+3*sd(cf$lteq, na.rm=T)))
cf$Ac1 <-ifelse(cf$Achours<(mean(cf$Achours, na.rm=T)+3*sd(cf$Achours, na.rm=T)), cf$Achours,cf$Achours<(mean(cf$Achours, na.rm=T)+3*sd(cf$Achours, na.rm=T)))
cf$Ac<-cf$Ac1*60

length(which(cf$PAmin>(mean(cf$PAmin, na.rm=T)+3*sd(cf$PAmin, na.rm=T))))
length(which(cf$lteq>(mean(cf$lteq, na.rm=T)+3*sd(cf$lteq, na.rm=T))))
length(which(cf$Achours>(mean(cf$Achours, na.rm=T)+3*sd(cf$Achours, na.rm=T))))




####making iMeans and residuals##

f <- function(x) c( iM=mean(x, na.rm=TRUE), iSD=sd(x, na.rm=TRUE) )

icfID=do.call( "rbind", tapply( cf$id, cf$id, f ))
icf1=do.call( "rbind", tapply( cf$PA, cf$id, f ))
icf2=do.call( "rbind", tapply( cf$Ac, cf$id, f ))
icf3=do.call( "rbind", tapply( cf$PASRE, cf$id, f ))
icf4=do.call( "rbind", tapply( cf$AcSRE, cf$id, f ))
icf5=do.call( "rbind", tapply( cf$impPA, cf$id, f ))
icf6=do.call( "rbind", tapply( cf$impAc, cf$id, f ))
icf7=do.call( "rbind", tapply( cf$impPAw, cf$id, f ))
icf8=do.call( "rbind", tapply( cf$impAcw, cf$id, f ))
icf9=do.call( "rbind", tapply( cf$impPAAcdiff, cf$id, f ))
icf10=do.call( "rbind", tapply( cf$ccSRE, cf$id, f ))

icf11<-cbind(icfID, icf1, icf2, icf3, icf4, icf5, icf6, icf7, icf8, icf9, icf10)
icf11<-data.frame(icf11)
icf11$id<-icf11$iM
icf<-merge(cf, icf11, by="id")

icf$iMPA<-icf$iM.1
icf$iMAc<-icf$iM.2
icf$iMPASRE<-icf$iM.3
icf$iMAcSRE<-icf$iM.4
icf$iMimpPA<-icf$iM.5
icf$iMimpAc<-icf$iM.6
icf$iMimpPAw<-icf$iM.7
icf$iMimpAcw<-icf$iM.8
icf$iMimpPAAcdiff<-icf$iM.9
icf$iMccSRE<-icf$iM.10

icf$rPA<-icf$PA-icf$iMPA
icf$rAc<-icf$Ac-icf$iMAc
icf$rPASRE<-icf$PASRE-icf$iMPASRE
icf$rAcSRE<-icf$AcSRE-icf$iMAcSRE
icf$rimpPA<-icf$impPA-icf$iMimpPA
icf$rimpAc<-icf$impAc-icf$iMimpAc
icf$rimpPAw<-icf$impPAw-icf$iMimpPAw
icf$rimpAcw<-icf$impAcw-icf$iMimpAcw
icf$rimpPAAcdiff<-icf$impPAAcdiff-icf$iMimpPAAcdiff
icf$rccSRE<-icf$ccSRE-icf$iMccSRE


##sample center iM variables##
icf$iMPA_c<-icf$iMPA - mean(icf$iMPA, na.rm=T)
icf$iMAc_c<-icf$iMAc - mean(icf$iMAc, na.rm=T)
icf$iMPASRE_c<-icf$iMPASRE - mean(icf$iMPASRE, na.rm=T)
icf$iMAcSRE_c<-icf$iMAcSRE - mean(icf$iMAcSRE, na.rm=T)
icf$iMimpPA_c<-icf$iMimpPA - mean(icf$iMimpPA, na.rm=T)
icf$iMimpAc_c<-icf$iMimpAc - mean(icf$iMimpAc, na.rm=T)
icf$iMimpPAw_c<-icf$iMimpPAw - mean(icf$iMimpPAw, na.rm=T)
icf$iMimpAcw_c<-icf$iMimpAcw - mean(icf$iMimpAcw, na.rm=T)
icf$iMimpPAAcdiff_c<-icf$iMimpPAAcdiff - mean(icf$iMimpPAAcdiff, na.rm=T)
icf$iMccSRE_c<-icf$iMccSRE - mean(icf$iMccSRE, na.rm=T)


###multivariate hierarchical model###

write.table(icf, file = "C:/Users/alh379/SkyDrive/Data Analyses/Operation CF/CF2.csv", sep = ",", col.names = NA,
            qmethod = "double")


icf <-read.csv("C:\\Users\\alh379\\SkyDrive\\Data Analyses\\Operation CF\\CF2.csv", na.strings=".", stringsAsFactors=FALSE)

#Linear time model (MLM)

library(lme4)

haha <- lmer(dv ~ -1 + dummyPA + dummyAc + iMPASRE_c + iMAcSRE_c
             + iMimpPA_c + iMimpAc_c + rPASRE + rAcSRE + rimpPA + rimpAc
             + dummyPA*iMPASRE_c + dummyPA*iMAcSRE_c + dummyPA*iMimpPA_c
             + dummyPA*iMimpAc_c
             + dummyPA*rPASRE + dummyPA*rAcSRE + dummyPA*rimpPA + dummyPA*rimpAc
             + iMPASRE_c*iMimpPA_c
             + rPASRE*rimpPA
             + dummyPA*iMPASRE_c*iMimpPA_c
             + dummyPA*rPASRE*rimpPA
             + (rAcSRE|id) + (rPASRE|id) + (rimpPA|id) + (rimpAc|id), data=icf)


summary(haha)

haha2 <- lmer(dv ~ -1 + dummyPA + dummyAc + iMPASRE_c + iMAcSRE_c
              + iMimpPA_c + iMimpAc_c + rPASRE + rAcSRE + rimpPA + rimpAc
              + dummyAc*iMPASRE_c + dummyAc*iMAcSRE_c + dummyAc*iMimpPA_c + dummyAc*iMimpAc_c
              + dummyAc*rPASRE + dummyAc*rAcSRE + dummyAc*rimpPA + dummyAc*rimpAc
              + iMAcSRE_c*iMimpAc_c
              + rAcSRE*rimpAc
              + dummyAc*iMAcSRE_c*iMimpAc_c
              + dummyAc*rAcSRE*rimpAc
              + (rAcSRE|id) + (rPASRE|id) + (rimpPA|id) + (rimpAc|id), data=icf)


summary(haha2)


library(arm)
##signficance of MLM coefficients based on simulations##
E1.sim <- sim (haha)
coef.E1.sim <- coef(E1.sim)
fixef.E1.sim <- fixef(E1.sim)
ranef.E1.sim <- ranef(E1.sim)
sigma.E1.sim <- sigma.hat(E1.sim)
p<-data.frame(fixef.E1.sim)
haha.sd<-sapply(p,sd)
haha.mean<-sapply(p,mean)
haha.ci<-data.frame(lo=(haha.mean-haha.sd*1.96), hi=(haha.mean+haha.sd*1.96) )
haha.ci



haha <- lmer(PA ~ iMPASRE_c
             + iMimpPAw_c + rPASRE + rimpPAw + iMPASRE_c*iMimpPAw_c
             + rPASRE*rimpPAw
             + (rPASRE|id) + (rimpPAw|id), data=icf)

summary(haha)




cfmodel <- lmer(dv ~ -1 + dummyPA + dummyAc + iMPASRE_c + rPASRE
                + iMccSRE_c + rccSRE + dummyPA*iMPASRE_c
                + dummyPA*rPASRE
                + (rPASRE|id) + (rccSRE|id), data=icf)

summary(cfmodel)



cfmodel <- lmer(dv ~ -1 + dummyPA + dummyAc + iMPASRE_c + rPASRE
                + iMccSRE_c + rccSRE + dummyAc*iMPASRE_c
                + dummyAc*rPASRE + iMPASRE_c*iMccSRE_c
                + iMccSRE_c*dummyAc + dummyAc*iMPASRE_c*iMccSRE_c
                + rPASRE*rccSRE
                + rccSRE*dummyAc + dummyAc*rPASRE*rccSRE
                + (rPASRE|id) + (rccSRE|id), data=icf)

summary(cfmodel)


###Bob Habits###

###Bob Habits###

bob <-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\Bob's Habit\\bob2.csv", na.strings=".", stringsAsFactors=FALSE)

bob <-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\Bob's Habit\\bob2.csv", na.strings=".", stringsAsFactors=FALSE)


###multivariate outlier test - need no NA###

obob<-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\Bob's Habit\\bob.csv", na.strings=".", stringsAsFactors=FALSE)

library(mvoutlier)
outliers <-aq.plot(obob[c("habit_PAT1", "habit_AUT1", "habit_NEG1", "habit_SRB1",
                          "habit_AUT2", "habit_NEG2", "habit_SRB2", "habit_AUT3",
                          "habit_NEG3", "habit_PAT2", "habit_PAT3", "habit_AUT4",
                          "habit_NEG4", "habit_SRB3", "habit_PAT4", "habit_PAT5", "habit_SRB4")])
outliers # show list of outliers
bob1<-cbind(bob, outliers)

##cutting those with multivariate outliers##
bob2<-bob1[which(bob1$outlier=="FALSE"), ]

##CFA##

require(OpenMx)
bob3=data.frame(pat1=bob2$habit_PAT1, aut1=bob2$habit_AUT1,
                neg1=bob2$habit_NEG1, srb1=bob2$habit_SRB1,
                aut2=bob2$habit_AUT2, neg2=bob2$habit_NEG2,
                srb2=bob2$habit_SRB2, aut3=bob2$habit_AUT3,
                neg3=bob2$habit_NEG3, pat2=bob2$habit_PAT2,
                pat3=bob2$habit_PAT3, aut4=bob2$habit_AUT4,
                neg4=bob2$habit_NEG4, srb3=bob2$habit_SRB3,
                pat4=bob2$habit_PAT4, pat5=bob2$habit_PAT5,
                srb4=bob2$habit_SRB4)

cov<-cov(bob3, use = "pairwise.complete.obs")

means<-c((mean(bob3$pat1, na.rm=TRUE)), (mean(bob3$aut1, na.rm=TRUE)),
         (mean(bob3$neg1, na.rm=TRUE)), (mean(bob3$srb1, na.rm=TRUE)),
         (mean(bob3$aut2, na.rm=TRUE)), (mean(bob3$neg2, na.rm=TRUE)),
         (mean(bob3$srb2, na.rm=TRUE)), (mean(bob3$aut3, na.rm=TRUE)),
         (mean(bob3$neg3, na.rm=TRUE)), (mean(bob3$pat2, na.rm=TRUE)),
         (mean(bob3$pat3, na.rm=TRUE)), (mean(bob3$aut4, na.rm=TRUE)),
         (mean(bob3$neg4, na.rm=TRUE)), (mean(bob3$srb3, na.rm=TRUE)),
         (mean(bob3$pat4, na.rm=TRUE)), (mean(bob3$pat5, na.rm=TRUE)),
         (mean(bob3$srb4, na.rm=TRUE)))

names(means)<-c("pat1", "aut1", "neg1", "srb1",
                "aut2", "neg2", "srb2", "aut3",
                "neg3", "pat2", "pat3", "aut4",
                "neg4", "srb3", "pat4", "pat5", "srb4")

manifest=c("pat1", "aut1", "neg1", "srb1",
           "aut2", "neg2", "srb2", "aut3",
           "neg3", "pat2", "pat3", "aut4",
           "neg4", "srb3", "pat4", "pat5", "srb4")




latent<-c("HAB")

uni <- mxModel("uni", 
               type="RAM",
               manifestVars=manifest,
               latentVars=latent,
               mxData(cov,type="cov", numObs=497, means=means),
               # manifest variances    
               mxPath(
                 from=manifest,
                 arrows=2, free=TRUE,
                 labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                          "vaut2", "vneg2", "vsrb2", "vaut3",
                          "vneg3", "vpat2", "vpat3", "vaut4",
                          "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
               ),
               # latent variances and covariance
               mxPath(
                 from="HAB",
                 arrows=2, values=1,
                 free=F
               ),
               mxPath(
                 from=c("HAB"),
                 to=c("neg1"),
                 arrows=1,
                 free=F,
                 values=1,
                 labels=c("NEGneg1")
               ),
               mxPath(
                 from=c("HAB"),
                 to=c("neg2", "neg3", "neg4"),
                 arrows=1,
                 free=TRUE,
                 labels=c("NEGneg2", "NEGneg3", "NEGneg4")
               ),
               mxPath(
                 from=c("HAB"),
                 to=c("pat1"),
                 arrows=1,
                 free=T,
                 values=1,
                 labels=c("PATpat1")
               ),
               mxPath(
                 from=c("HAB"),
                 to=c("pat2", "pat3", "pat4", "pat5"),
                 arrows=1,
                 free=TRUE,
                 labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
               ),
               mxPath(
                 from=c("HAB"),
                 to=c("srb1"),
                 arrows=1,
                 free=T,
                 values=1,
                 labels=c("SRBsrb1")
               ),
               mxPath(
                 from=c("HAB"),
                 to=c("srb2", "srb3", "srb4"),
                 arrows=1,
                 free=TRUE,
                 labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
               ),
               mxPath(
                 from=c("HAB"),
                 to=c("aut1"),
                 arrows=1,
                 free=T,
                 values=1,
                 labels=c("AUTaut1")
               ),
               mxPath(
                 from=c("HAB"),
                 to=c("aut2", "aut3", "aut4"),
                 arrows=1,
                 free=TRUE,
                 labels=c("AUTaut2", "AUTaut3", "AUTaut4")
               ),                
               # manifest means
               mxPath(
                 from="one",
                 to=manifest,
                 arrows=1,
                 free=FALSE,
                 values=0,
                 labels=c("mpat1", "maut1", "mneg1", "msrb1",
                          "maut2", "mneg2", "msrb2", "maut3",
                          "mneg3", "mpat2", "mpat3", "maut4",
                          "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
               ),
               # latent means
               mxPath(
                 from="one",
                 to=c("HAB"),
                 arrows=1, values=1,
                 free=T,
                 labels=c("mNEGSRB")),
               # latent means
               mxPath(
                 from="one",
                 to="HAB",
                 arrows=1, values=0,
                 free=F)
)


uni <- mxRun(uni)
summary(uni, intervals=T)


latentH<-c("AUT", "NEGSRB", "PAT", "HAB")

threeHNEGSRB <- mxModel("threeHNEGSRB", 
                        type="RAM",
                        manifestVars=manifest,
                        latentVars=latentH,
                        mxData(cov,type="cov", numObs=497, means=means),
                        # manifest variances    
                        mxPath(
                          from=manifest,
                          arrows=2, free=TRUE,
                          labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                                   "vaut2", "vneg2", "vsrb2", "vaut3",
                                   "vneg3", "vpat2", "vpat3", "vaut4",
                                   "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                        ),
                        # latent variances and covariance
                        mxPath(
                          from=c("AUT", "NEGSRB", "PAT"),
                          arrows=2, values=2,
                          free=T
                        ),
                        mxPath(
                          from="HAB",
                          arrows=2, values=1,
                          free=F
                        ),
                        mxPath(
                          from=c("HAB"),
                          to=c("NEGSRB", "AUT", "PAT"),
                          arrows=1,
                          free=T,
                          labels=c("HABNEGSRB", "HABAUT", "HABPAT")
                        ),
                        mxPath(
                          from=c("NEGSRB"),
                          to=c("neg1"),
                          arrows=1,
                          free=F,
                          values=1,
                          labels=c("NEGneg1")
                        ),
                        mxPath(
                          from=c("NEGSRB"),
                          to=c("neg2", "neg3", "neg4"),
                          arrows=1,
                          free=TRUE,
                          labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                        ),
                        mxPath(
                          from=c("PAT"),
                          to=c("pat1"),
                          arrows=1,
                          free=F,
                          values=1,
                          labels=c("PATpat1")
                        ),
                        mxPath(
                          from=c("PAT"),
                          to=c("pat2", "pat3", "pat4", "pat5"),
                          arrows=1,
                          free=TRUE,
                          labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                        ),
                        mxPath(
                          from=c("NEGSRB"),
                          to=c("srb1"),
                          arrows=1,
                          free=T,
                          values=1,
                          labels=c("SRBsrb1")
                        ),
                        mxPath(
                          from=c("NEGSRB"),
                          to=c("srb2", "srb3", "srb4"),
                          arrows=1,
                          free=TRUE,
                          labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                        ),
                        mxPath(
                          from=c("AUT"),
                          to=c("aut1"),
                          arrows=1,
                          free=F,
                          values=1,
                          labels=c("AUTaut1")
                        ),
                        mxPath(
                          from=c("AUT"),
                          to=c("aut2", "aut3", "aut4"),
                          arrows=1,
                          free=TRUE,
                          labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                        ),                
                        # manifest means
                        mxPath(
                          from="one",
                          to=manifest,
                          arrows=1,
                          free=FALSE,
                          values=0,
                          labels=c("mpat1", "maut1", "mneg1", "msrb1",
                                   "maut2", "mneg2", "msrb2", "maut3",
                                   "mneg3", "mpat2", "mpat3", "maut4",
                                   "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                        ),
                        # latent means
                        mxPath(
                          from="one",
                          to=c("NEGSRB","PAT", "AUT"),
                          arrows=1, values=1,
                          free=T,
                          labels=c("mNEGSRB","mPAT", "mAUT")),
                        # latent means
                        mxPath(
                          from="one",
                          to="HAB",
                          arrows=1, values=0,
                          free=F)
)


threeHNEGSRB <- mxRun(threeHNEGSRB)
summary(threeHNEGSRB, intervals=T)






latent=c("NEGSRB","PAT", "AUT")

threeorthNEGSRB <- mxModel("threeorthNEGSRB", 
                           type="RAM",
                           manifestVars=manifest,
                           latentVars=latent,
                           mxData(cov,type="cov", numObs=497, means=means),
                           # manifest variances    
                           mxPath(
                             from=manifest,
                             arrows=2, free=TRUE,
                             labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                                      "vaut2", "vneg2", "vsrb2", "vaut3",
                                      "vneg3", "vpat2", "vpat3", "vaut4",
                                      "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                           ),
                           # latent variances and covariance
                           mxPath(
                             from=c("AUT", "NEGSRB", "PAT"),
                             arrows=2, values=2,
                             free=T
                           ),
                           mxPath(
                             from=c("NEGSRB"),
                             to=c("neg1"),
                             arrows=1,
                             free=F,
                             values=1,
                             labels=c("NEGneg1")
                           ),
                           mxPath(
                             from=c("NEGSRB"),
                             to=c("neg2", "neg3", "neg4"),
                             arrows=1,
                             free=TRUE,
                             labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                           ),
                           mxPath(
                             from=c("PAT"),
                             to=c("pat1"),
                             arrows=1,
                             free=F,
                             values=1,
                             labels=c("PATpat1")
                           ),
                           mxPath(
                             from=c("PAT"),
                             to=c("pat2", "pat3", "pat4", "pat5"),
                             arrows=1,
                             free=TRUE,
                             labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                           ),
                           mxPath(
                             from=c("NEGSRB"),
                             to=c("srb1"),
                             arrows=1,
                             free=T,
                             values=1,
                             labels=c("SRBsrb1")
                           ),
                           mxPath(
                             from=c("NEGSRB"),
                             to=c("srb2", "srb3", "srb4"),
                             arrows=1,
                             free=TRUE,
                             labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                           ),
                           mxPath(
                             from=c("AUT"),
                             to=c("aut1"),
                             arrows=1,
                             free=F,
                             values=1,
                             labels=c("AUTaut1")
                           ),
                           mxPath(
                             from=c("AUT"),
                             to=c("aut2", "aut3", "aut4"),
                             arrows=1,
                             free=TRUE,
                             labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                           ),                
                           # manifest means
                           mxPath(
                             from="one",
                             to=manifest,
                             arrows=1,
                             free=FALSE,
                             values=0,
                             labels=c("mpat1", "maut1", "mneg1", "msrb1",
                                      "maut2", "mneg2", "msrb2", "maut3",
                                      "mneg3", "mpat2", "mpat3", "maut4",
                                      "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                           ),
                           # latent means
                           mxPath(
                             from="one",
                             to=c("NEGSRB","PAT", "AUT"),
                             arrows=1, values=1,
                             free=T,
                             labels=c("mNEGSRB","mPAT", "mAUT"))
)


threeorthNEGSRB <- mxRun(threeorthNEGSRB)
summary(threeorthNEGSRB, intervals=T)




latent=c("NEGSRB","PAT", "AUT")

threeobNEGSRB <- mxModel("threeobNEGSRB", 
                         type="RAM",
                         manifestVars=manifest,
                         latentVars=latent,
                         mxData(cov,type="cov", numObs=497, means=means),
                         # manifest variances    
                         mxPath(
                           from=manifest,
                           arrows=2, free=TRUE,
                           labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                                    "vaut2", "vneg2", "vsrb2", "vaut3",
                                    "vneg3", "vpat2", "vpat3", "vaut4",
                                    "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                         ),
                         # latent variances and covariance
                         mxPath(
                           from=c("AUT", "NEGSRB", "PAT"), 
                           arrows=2,
                           free=F, values=1
                         ),
                         mxPath(
                           from=c("AUT"), 
                           to=c("NEGSRB", "PAT"), arrows=2,
                           free=T
                         ),
                         mxPath(
                           from=c("NEGSRB"), 
                           to=c("PAT"), arrows=2,
                           free=T
                         ),
                         mxPath(
                           from=c("NEGSRB"),
                           to=c("neg1"),
                           arrows=1,
                           free=F,
                           values=1,
                           labels=c("NEGneg1")
                         ),
                         mxPath(
                           from=c("NEGSRB"),
                           to=c("neg2", "neg3", "neg4"),
                           arrows=1,
                           free=TRUE,
                           labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                         ),
                         mxPath(
                           from=c("PAT"),
                           to=c("pat1"),
                           arrows=1,
                           free=F,
                           values=1,
                           labels=c("PATpat1")
                         ),
                         mxPath(
                           from=c("PAT"),
                           to=c("pat2", "pat3", "pat4", "pat5"),
                           arrows=1,
                           free=TRUE,
                           labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                         ),
                         mxPath(
                           from=c("NEGSRB"),
                           to=c("srb1"),
                           arrows=1,
                           free=T,
                           values=1,
                           labels=c("SRBsrb1")
                         ),
                         mxPath(
                           from=c("NEGSRB"),
                           to=c("srb2", "srb3", "srb4"),
                           arrows=1,
                           free=TRUE,
                           labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                         ),
                         mxPath(
                           from=c("AUT"),
                           to=c("aut1"),
                           arrows=1,
                           free=F,
                           values=1,
                           labels=c("AUTaut1")
                         ),
                         mxPath(
                           from=c("AUT"),
                           to=c("aut2", "aut3", "aut4"),
                           arrows=1,
                           free=TRUE,
                           labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                         ),                
                         # manifest means
                         mxPath(
                           from="one",
                           to=manifest,
                           arrows=1,
                           free=FALSE,
                           values=0,
                           labels=c("mpat1", "maut1", "mneg1", "msrb1",
                                    "maut2", "mneg2", "msrb2", "maut3",
                                    "mneg3", "mpat2", "mpat3", "maut4",
                                    "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                         ),
                         # latent means
                         mxPath(
                           from="one",
                           to=c("NEGSRB","PAT", "AUT"),
                           arrows=1, values=1,
                           free=T,
                           labels=c("mNEGSRB","mPAT", "mAUT"))
)


threeobNEGSRB <- mxRun(threeobNEGSRB)
summary(threeobNEGSRB, intervals=T)






latentH<-c("AUT", "NEG", "SRB", "HAB")

threeHnoPAT <- mxModel("threeHnoPAT", 
                       type="RAM",
                       manifestVars=manifest,
                       latentVars=latentH,
                       mxData(cov,type="cov", numObs=497, means=means),
                       # manifest variances    
                       mxPath(
                         from=manifest,
                         arrows=2, free=TRUE,
                         labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                                  "vaut2", "vneg2", "vsrb2", "vaut3",
                                  "vneg3", "vpat2", "vpat3", "vaut4",
                                  "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                       ),
                       # latent variances and covariance
                       mxPath(
                         from=c("AUT", "NEG", "SRB"),
                         arrows=2, values=2,
                         free=T
                       ),
                       mxPath(
                         from="HAB",
                         arrows=2, values=1,
                         free=F
                       ),
                       mxPath(
                         from=c("HAB"),
                         to=c("NEG", "SRB", "AUT"),
                         arrows=1,
                         free=T,
                         labels=c("HABNEGSRB", "HABAUT", "HABPAT")
                       ),
                       mxPath(
                         from=c("NEG"),
                         to=c("neg1"),
                         arrows=1,
                         free=F,
                         values=1,
                         labels=c("NEGneg1")
                       ),
                       mxPath(
                         from=c("NEG"),
                         to=c("neg2", "neg3", "neg4"),
                         arrows=1,
                         free=TRUE,
                         labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                       ),
                       mxPath(
                         from=c("SRB"),
                         to=c("srb1"),
                         arrows=1,
                         free=F,
                         values=1,
                         labels=c("SRBsrb1")
                       ),
                       mxPath(
                         from=c("SRB"),
                         to=c("srb2", "srb3", "srb4"),
                         arrows=1,
                         free=TRUE,
                         labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                       ),
                       mxPath(
                         from=c("AUT"),
                         to=c("aut1"),
                         arrows=1,
                         free=F,
                         values=1,
                         labels=c("AUTaut1")
                       ),
                       mxPath(
                         from=c("AUT"),
                         to=c("aut2", "aut3", "aut4"),
                         arrows=1,
                         free=TRUE,
                         labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                       ),                
                       # manifest means
                       mxPath(
                         from="one",
                         to=manifest,
                         arrows=1,
                         free=FALSE,
                         values=0,
                         labels=c("mpat1", "maut1", "mneg1", "msrb1",
                                  "maut2", "mneg2", "msrb2", "maut3",
                                  "mneg3", "mpat2", "mpat3", "maut4",
                                  "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                       ),
                       # latent means
                       mxPath(
                         from="one",
                         to=c("NEG","SRB", "AUT"),
                         arrows=1, values=1,
                         free=T,
                         labels=c("mNEGSRB","mPAT", "mAUT")),
                       # latent means
                       mxPath(
                         from="one",
                         to="HAB",
                         arrows=1, values=0,
                         free=F)
)


threeHnoPAT <- mxRun(threeHnoPAT)
summary(threeHnoPAT, intervals=T)






latent=c("NEG", "SRB", "AUT")

threeorthnoPAT <- mxModel("threeorthnoPAT", 
                          type="RAM",
                          manifestVars=manifest,
                          latentVars=latent,
                          mxData(cov,type="cov", numObs=497, means=means),
                          # manifest variances    
                          mxPath(
                            from=manifest,
                            arrows=2, free=TRUE,
                            labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                                     "vaut2", "vneg2", "vsrb2", "vaut3",
                                     "vneg3", "vpat2", "vpat3", "vaut4",
                                     "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                          ),
                          # latent variances and covariance
                          mxPath(
                            from=c("AUT", "NEG", "SRB"),
                            arrows=2, values=2,
                            free=T
                          ),
                          mxPath(
                            from=c("NEG"),
                            to=c("neg1"),
                            arrows=1,
                            free=F,
                            values=1,
                            labels=c("NEGneg1")
                          ),
                          mxPath(
                            from=c("NEG"),
                            to=c("neg2", "neg3", "neg4"),
                            arrows=1,
                            free=TRUE,
                            labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                          ),
                          mxPath(
                            from=c("SRB"),
                            to=c("srb1"),
                            arrows=1,
                            free=F,
                            values=1,
                            labels=c("SRBsrb1")
                          ),
                          mxPath(
                            from=c("SRB"),
                            to=c("srb2", "srb3", "srb4"),
                            arrows=1,
                            free=TRUE,
                            labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                          ),
                          mxPath(
                            from=c("AUT"),
                            to=c("aut1"),
                            arrows=1,
                            free=F,
                            values=1,
                            labels=c("AUTaut1")
                          ),
                          mxPath(
                            from=c("AUT"),
                            to=c("aut2", "aut3", "aut4"),
                            arrows=1,
                            free=TRUE,
                            labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                          ),                
                          # manifest means
                          mxPath(
                            from="one",
                            to=manifest,
                            arrows=1,
                            free=FALSE,
                            values=0,
                            labels=c("mpat1", "maut1", "mneg1", "msrb1",
                                     "maut2", "mneg2", "msrb2", "maut3",
                                     "mneg3", "mpat2", "mpat3", "maut4",
                                     "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                          ),
                          # latent means
                          mxPath(
                            from="one",
                            to=c("NEG","SRB", "AUT"),
                            arrows=1, values=1,
                            free=T,
                            labels=c("mNEGSRB","mPAT", "mAUT"))
)


threeorthnoPAT <- mxRun(threeorthnoPAT)
summary(threeorthnoPAT, intervals=T)




latent=c("NEG", "SRB", "AUT")

threeobnoPAT <- mxModel("threeobnoPAT", 
                        type="RAM",
                        manifestVars=manifest,
                        latentVars=latent,
                        mxData(cov,type="cov", numObs=497, means=means),
                        # manifest variances    
                        mxPath(
                          from=manifest,
                          arrows=2, free=TRUE,
                          labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                                   "vaut2", "vneg2", "vsrb2", "vaut3",
                                   "vneg3", "vpat2", "vpat3", "vaut4",
                                   "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                        ),
                        # latent variances and covariance
                        mxPath(
                          from=c("AUT", "NEG", "SRB"), 
                          arrows=2,
                          free=F, values=1
                        ),
                        mxPath(
                          from=c("AUT"), 
                          to=c("NEG", "SRB"), arrows=2,
                          free=T
                        ),
                        mxPath(
                          from=c("NEG"), 
                          to=c("SRB"), arrows=2,
                          free=T
                        ),
                        mxPath(
                          from=c("NEG"),
                          to=c("neg1"),
                          arrows=1,
                          free=F,
                          values=1,
                          labels=c("NEGneg1")
                        ),
                        mxPath(
                          from=c("NEG"),
                          to=c("neg2", "neg3", "neg4"),
                          arrows=1,
                          free=TRUE,
                          labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                        ),
                        mxPath(
                          from=c("SRB"),
                          to=c("srb1"),
                          arrows=1,
                          free=F,
                          values=1,
                          labels=c("SRBsrb1")
                        ),
                        mxPath(
                          from=c("SRB"),
                          to=c("srb2", "srb3", "srb4"),
                          arrows=1,
                          free=TRUE,
                          labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                        ),
                        mxPath(
                          from=c("AUT"),
                          to=c("aut1"),
                          arrows=1,
                          free=F,
                          values=1,
                          labels=c("AUTaut1")
                        ),
                        mxPath(
                          from=c("AUT"),
                          to=c("aut2", "aut3", "aut4"),
                          arrows=1,
                          free=TRUE,
                          labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                        ),                
                        # manifest means
                        mxPath(
                          from="one",
                          to=manifest,
                          arrows=1,
                          free=FALSE,
                          values=0,
                          labels=c("mpat1", "maut1", "mneg1", "msrb1",
                                   "maut2", "mneg2", "msrb2", "maut3",
                                   "mneg3", "mpat2", "mpat3", "maut4",
                                   "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                        ),
                        # latent means
                        mxPath(
                          from="one",
                          to=c("NEG", "SRB","AUT"),
                          arrows=1, values=1,
                          free=T,
                          labels=c("mNEGSRB","mPAT", "mAUT"))
)


threeobnoPAT <- mxRun(threeobnoPAT)
summary(threeobnoPAT, intervals=T)


latentH<-c("NEG", "PAT", "SRB", "AUT", "HAB")
fourH <- mxModel("fourH", 
                 type="RAM",
                 manifestVars=manifest,
                 latentVars=latentH,
                 mxData(cov,type="cov", numObs=497, means=means),
                 # manifest variances    
                 mxPath(
                   from=manifest,
                   arrows=2, free=TRUE,
                   labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                            "vaut2", "vneg2", "vsrb2", "vaut3",
                            "vneg3", "vpat2", "vpat3", "vaut4",
                            "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                 ),
                 # latent variances and covariance
                 mxPath(
                   from=c("AUT", "NEG", "SRB", "PAT"),
                   arrows=2, values=2,
                   free=T
                 ),
                 mxPath(
                   from="HAB",
                   arrows=2, values=1,
                   free=F
                 ),
                 mxPath(
                   from=c("HAB"),
                   to=c("SRB", "AUT"),
                   arrows=1,
                   free=T,
                   labels=c("HABSRB", "HABAUT")
                 ),
                 mxPath(
                   from=c("HAB"),
                   to=c("NEG", "PAT"),
                   arrows=1,
                   free=T,
                   labels=c("HABNEG", "HABPAT")
                 ),
                 mxPath(
                   from=c("NEG"),
                   to=c("neg1"),
                   arrows=1,
                   free=F,
                   values=1,
                   labels=c("NEGneg1")
                 ),
                 mxPath(
                   from=c("NEG"),
                   to=c("neg2", "neg3", "neg4"),
                   arrows=1,
                   free=TRUE,
                   labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                 ),
                 mxPath(
                   from=c("PAT"),
                   to=c("pat1"),
                   arrows=1,
                   free=F,
                   values=1,
                   labels=c("PATpat1")
                 ),
                 mxPath(
                   from=c("PAT"),
                   to=c("pat2", "pat3", "pat4", "pat5"),
                   arrows=1,
                   free=TRUE,
                   labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                 ),
                 mxPath(
                   from=c("SRB"),
                   to=c("srb1"),
                   arrows=1,
                   free=F,
                   values=1,
                   labels=c("SRBsrb1")
                 ),
                 mxPath(
                   from=c("SRB"),
                   to=c("srb2", "srb3", "srb4"),
                   arrows=1,
                   free=TRUE,
                   labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                 ),
                 mxPath(
                   from=c("AUT"),
                   to=c("aut1"),
                   arrows=1,
                   free=F,
                   values=1,
                   labels=c("AUTaut1")
                 ),
                 mxPath(
                   from=c("AUT"),
                   to=c("aut2", "aut3", "aut4"),
                   arrows=1,
                   free=TRUE,
                   labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                 ),                
                 # manifest means
                 mxPath(
                   from="one",
                   to=manifest,
                   arrows=1,
                   free=FALSE,
                   values=0,
                   labels=c("mpat1", "maut1", "mneg1", "msrb1",
                            "maut2", "mneg2", "msrb2", "maut3",
                            "mneg3", "mpat2", "mpat3", "maut4",
                            "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                 ),
                 # latent means
                 mxPath(
                   from="one",
                   to=c("NEG","PAT", "SRB", "AUT"),
                   arrows=1, values=1,
                   free=T,
                   labels=c("mNEG","mPAT", "mSRB", "mAUT")),
                 # latent means
                 mxPath(
                   from="one",
                   to="HAB",
                   arrows=1, values=0,
                   free=F)
)


# ----------------------------------------
# run model and examine output

fourH <- mxRun(fourH)
summary(fourH, intervals=T)



latentH<-c("NEG", "AUT", "SRB", "PAT")
fourob <- mxModel("fourob", 
                  type="RAM",
                  manifestVars=manifest,
                  latentVars=latentH,
                  mxData(cov,type="cov", numObs=497, means=means),
                  # manifest variances    
                  mxPath(
                    from=manifest,
                    arrows=2, free=TRUE,
                    labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                             "vaut2", "vneg2", "vsrb2", "vaut3",
                             "vneg3", "vpat2", "vpat3", "vaut4",
                             "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                  ),
                  # latent variances and covariance
                  mxPath(
                    from=c("AUT", "NEG", "SRB", "PAT"),
                    arrows=2, values=1,
                    free=F
                  ),
                  mxPath(
                    from=c("AUT"),
                    to=c("NEG", "SRB", "PAT"),
                    arrows=1,
                    free=T,
                  ),
                  mxPath(
                    from=c("NEG"),
                    to=c("SRB", "PAT"),
                    arrows=1,
                    free=T,
                  ),
                  mxPath(
                    from=c("SRB"),
                    to=c("PAT"),
                    arrows=1,
                    free=T,
                  ),
                  mxPath(
                    from=c("NEG"),
                    to=c("neg1"),
                    arrows=1,
                    free=F,
                    values=1,
                    labels=c("NEGneg1")
                  ),
                  mxPath(
                    from=c("NEG"),
                    to=c("neg2", "neg3", "neg4"),
                    arrows=1,
                    free=TRUE,
                    labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                  ),
                  mxPath(
                    from=c("PAT"),
                    to=c("pat1"),
                    arrows=1,
                    free=F,
                    values=1,
                    labels=c("PATpat1")
                  ),
                  mxPath(
                    from=c("PAT"),
                    to=c("pat2", "pat3", "pat4", "pat5"),
                    arrows=1,
                    free=TRUE,
                    labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                  ),
                  mxPath(
                    from=c("SRB"),
                    to=c("srb1"),
                    arrows=1,
                    free=F,
                    values=1,
                    labels=c("SRBsrb1")
                  ),
                  mxPath(
                    from=c("SRB"),
                    to=c("srb2", "srb3", "srb4"),
                    arrows=1,
                    free=TRUE,
                    labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                  ),
                  mxPath(
                    from=c("AUT"),
                    to=c("aut1"),
                    arrows=1,
                    free=F,
                    values=1,
                    labels=c("AUTaut1")
                  ),
                  mxPath(
                    from=c("AUT"),
                    to=c("aut2", "aut3", "aut4"),
                    arrows=1,
                    free=TRUE,
                    labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                  ),                
                  # manifest means
                  mxPath(
                    from="one",
                    to=manifest,
                    arrows=1,
                    free=FALSE,
                    values=0,
                    labels=c("mpat1", "maut1", "mneg1", "msrb1",
                             "maut2", "mneg2", "msrb2", "maut3",
                             "mneg3", "mpat2", "mpat3", "maut4",
                             "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                  ),
                  # latent means
                  mxPath(
                    from="one",
                    to=c("NEG","PAT", "SRB", "AUT"),
                    arrows=1, values=1,
                    free=T,
                    labels=c("mNEG","mPAT", "mSRB", "mAUT"))
)


# ----------------------------------------
# run model and examine output

fourob <- mxRun(fourob)
summary(fourob, intervals=T)



latentH<-c("NEG", "AUT", "SRB", "PAT")
fourorth <- mxModel("fourorth", 
                    type="RAM",
                    manifestVars=manifest,
                    latentVars=latentH,
                    mxData(cov,type="cov", numObs=497, means=means),
                    # manifest variances    
                    mxPath(
                      from=manifest,
                      arrows=2, free=TRUE,
                      labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                               "vaut2", "vneg2", "vsrb2", "vaut3",
                               "vneg3", "vpat2", "vpat3", "vaut4",
                               "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                    ),
                    # latent variances and covariance
                    mxPath(
                      from=c("AUT", "NEG", "SRB", "PAT"),
                      arrows=2, values=c(5,3,.5,.5),
                      free=T
                    ),
                    mxPath(
                      from=c("NEG"),
                      to=c("neg1"),
                      arrows=1,
                      free=F,
                      values=1,
                      labels=c("NEGneg1")
                    ),
                    mxPath(
                      from=c("NEG"),
                      to=c("neg2", "neg3", "neg4"),
                      arrows=1,
                      free=TRUE,
                      labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                    ),
                    mxPath(
                      from=c("PAT"),
                      to=c("pat1"),
                      arrows=1,
                      free=F,
                      values=1,
                      labels=c("PATpat1")
                    ),
                    mxPath(
                      from=c("PAT"),
                      to=c("pat2", "pat3", "pat4", "pat5"),
                      arrows=1,
                      free=TRUE,
                      labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                    ),
                    mxPath(
                      from=c("SRB"),
                      to=c("srb1"),
                      arrows=1,
                      free=F,
                      values=1,
                      labels=c("SRBsrb1")
                    ),
                    mxPath(
                      from=c("SRB"),
                      to=c("srb2", "srb3", "srb4"),
                      arrows=1,
                      free=TRUE,
                      labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                    ),
                    mxPath(
                      from=c("AUT"),
                      to=c("aut1"),
                  \\    arrows=1,
                      free=F,
                      values=1,
                      labels=c("AUTaut1")
                    ),
                    mxPath(
                      from=c("AUT"),
                      to=c("aut2", "aut3", "aut4"),
                      arrows=1,
                      free=TRUE,
                      labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                    ),                
                    # manifest means
                    mxPath(
                      from="one",
                      to=manifest,
                      arrows=1,
                      free=FALSE,
                      values=0,
                      labels=c("mpat1", "maut1", "mneg1", "msrb1",
                               "maut2", "mneg2", "msrb2", "maut3",
                               "mneg3", "mpat2", "mpat3", "maut4",
                               "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                    ),
                    # latent means
                    mxPath(
                      from="one",
                      to=c("NEG","PAT", "SRB", "AUT"),
                      arrows=1, values=1,
                      free=T,
                      labels=c("mNEG","mPAT", "mSRB", "mAUT"))
)


# ----------------------------------------
# run model and examine output

fourorth <- mxRun(fourorth)
summary(fourorth, intervals=T)





##compare models###

mxCompare(fourH, uni)

mxCompare(fourH, fourorth)

mxCompare(fourH, fourob)

mxCompare(fourH, threeorthNEGSRB)

mxCompare(fourH, threeobNEGSRB)

mxCompare(fourH, threeHNEGSRB)

mxCompare(fourH, threeorthnoPAT)

mxCompare(fourH, threeobnoPAT)

mxCompare(fourH, threeHnoPAT)



### KINES 321 Fall 2012 Implementation Intentions Group Differences ###


##making between dataset into within (repeat rows)##
#rep <- bmi[rep(seq_len(nrow(bmi)), each=7),]

cf <-read.csv("C:\\Users\\alh379\\SkyDrive\\K321 analyses\\bwrepeat_schort.csv", na.strings=".", stringsAsFactors=FALSE)

#names(cf)

####making iMeans and residuals##

f <- function(x) c( iM=mean(x, na.rm=TRUE), iSD=sd(x, na.rm=TRUE) )

icfID=do.call( "rbind", tapply( cf$id, cf$id, f ))
icf1=do.call( "rbind", tapply( cf$intPA_mv_lag, cf$id, f ))
icf2=do.call( "rbind", tapply( cf$intPA_vmw_lag, cf$id, f ))
icf3=do.call( "rbind", tapply( cf$intSED, cf$id, f ))


icf11<-cbind(icfID, icf1, icf2, icf3)
icf11<-data.frame(icf11)
icf11$id<-icf11$iM
icf<-merge(cf, icf11, by="id")

icf$iMintPA_mv<-icf$iM.1
icf$iMintPA_vmw<-icf$iM.2
icf$iMintSED<-icf$iM.3

icf$rintPA_mv<-icf$intPA_mv-icf$iMintPA_mv
icf$rintPA_vmw<-icf$intPA_vmw-icf$iMintPA_vmw
icf$rintSED<-icf$intSED-icf$iMintSED


icf$metminb_c<-icf$metminb-(mean(icf$metminb, na.rm=T))
icf$RhodesHabPAb_c<-icf$RhodesHabPAb-(mean(icf$RhodesHabPAb, na.rm=T))
icf$iMintPA_vmw_c<-icf$iMintPA_vmw-(mean(icf$iMintPA_vmw, na.rm=T))
icf$PA_v_c<-icf$PA_v-(mean(icf$PA_v, na.rm=T))


#Linear time model (MLM)
hah1a <- lmer(metmin ~ metminb + sexb + BMI + studyday + RhodesHabPAb_c + rintPA_vmw
              + iMintPA_vmw_c + PA_v_c
              +PA_v_c*iMintPA_vmw_c + PA_v_c*rintPA_vmw
              + RhodesHabPAb_c*iMintPA_vmw_c + RhodesHabPAb_c*rintPA_vmw
              + (rintPA_vmw|id) + (studyday|id), data=icf)
summary(hah1a)

library(lme4)
#Linear time model (MLM)
haha <- lmer(sedentary ~ BMI + sexb + studyday + sedentaryb + RhodesHabSEDb
             + groupPA + groupSED + groupPA*groupSED + daywkend
             + RhodesHabSEDb*groupSED
             + daywkend*groupSED 
             + (daywkend|id) + (studyday|id), data=icf)
summary(haha)

##signficance of MLM coefficients based on simulations##
library(pbkrtest)
beet_no.harv <- update(hah1a, .~.-iMintPA_vmw_c*PA_v_c)
PBmodcomp(hah1a, beet_no.harv, nsim=20)



write.table(icf, file = "C:/Users/alh379/SkyDrive/K321 analyses/iMr.csv", sep = ",", col.names = NA,
            qmethod = "double")

##making between dataset into within (repeat rows) short into long##
#rep <- bmi[rep(seq_len(nrow(bmi)), each=7),]






### Dissertation Study 4 ###
habitrel<-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\K321 analyses\\Alldata.csv", na.strings=".", stringsAsFactors=FALSE)

##scoring a scale##
library(psych)
items<-c("habPA02b", "habPA03b", "habPA05b", "habPA08b", "habPA10b")
scaleKey <-c(1,1,1,1,1)
results<-score.items(keys=scaleKey, items = habitrel[items], totals = F, missing = T, min = 1, max=7)




four <-read.csv("C:\\Users\\alh379\\SkyDrive\\Study 4\\newFour.csv", na.strings=".", stringsAsFactors=FALSE)
four$HrAVG<-(four$Hr1 + four$Hr2 + four$Hr3)/3
four$vAstepsAVG<-(four$vAsteps_d2 + four$vAsteps_d3 + four$vAsteps_d4
                  + four$vAsteps_d5 + four$vAsteps_d6 + four$vAsteps_d7)
four$intPAb_c <- four$intPAb - mean(four$intPAb, na.rm=T)
four$PA_v_c <- four$PA_v - mean(four$PA_v, na.rm=T)
four$RhodesHabPAb_c <- four$RhodesHabPAb - mean(four$RhodesHabPAb, na.rm=T)
four$vAstepsAVG_c <- four$vAstepsAVG - mean(four$vAstepsAVG, na.rm=T)
four$HabPAb_c <- four$HabPAb - mean(four$HabPAb, na.rm=T)
four$bmi_c <- four$bmi - mean(four$bmi, na.rm=T)
four$vStepsOTHER<-(four$vSteps_d3 + four$vSteps_d4
                  + four$vSteps_d5 + four$vSteps_d6 + four$vSteps_d7)/5

library(psych)
descrip<-describe(four)
write.table(descrip, file = "C:/Users/alh379/SkyDrive/Study 4/Study2Descrips.csv", sep = ",", col.names = NA,
            qmethod = "double")
cor<-cor(four, use="pairwise.complete.obs")
write.table(cor, file = "C:/Users/alh379/SkyDrive/Study 4/Study2Cors.csv", sep = ",", col.names = NA,
            qmethod = "double")

four.fit <- lm(vStepsAVG ~ RhodesHabPAb_c + PA_v_c, data=four)
summary(four.fit) # show results
turk1<-summary(four.fit)

four.fit <- lm(vSteps_d2 ~ RhodesHabPAb_c + PA_v_c, data=four)
summary(four.fit)

turk2<-summary(four.fit) # show results



##standardized Betas##
library(QuantPsyc)
StCoeff <- data.frame (Make.Z (four))

lm1.z <- lm (vStepsAVG ~ RhodesHabPAb_c + PA_v_c, data=StCoeff)
summary(lm1.z)

lm1.z <- lm (vSteps_d2 ~ RhodesHabPAb_c + PA_v_c, data=StCoeff)
summary(lm1.z)




##Box Cox Transformations##


library(MASS)
library(forecast)

boxcox(vStepsAVG ~ RhodesHabPAb_c + PA_v_c, data = four,
       lambda = seq(0.19, .2, length = 10))

four$vStepsAVG_bc <- BoxCox(four$vStepsAVG,.19)

boxcox(vSteps_d2 ~ RhodesHabPAb_c + PA_v_c, data = four,
       lambda = seq(.4, .5, length = 10))

four$vSteps_d2_bc <- BoxCox(four$vStepsAVG,.48)







####Pat's Thesis Stuff - Executive Functioning, Physical Activity, and Sedentary Behavior#####

##entering dataset##
Promise <-read.csv("X:\\Data analysis\\Pat's Thesis 2012\\Daily use this one.csv", na.strings=".", stringsAsFactors=FALSE)
CEF <-read.csv("X:\\Data analysis\\Pat's Thesis 2012\\CEF.csv", na.strings=".", stringsAsFactors=FALSE)



##making density plots########
#Promiseplot<-density(Promise$Cog_1)
#Promiseplot2<-density(Promise$Cog_2)
#Promiseplot3<-density(Promise$Cog_3)
#Promiseplot4<-density(Promise$Cog_4)
#PromiseplotT<-density(Promise$DCogTot)
#PromiseplotPA<-density(Promise$metminD)
#PromiseplotSB<-density(Promise$Dsedentary)


#plot(Promiseplot, type="o", pch=30,lwd=0, lty=2, col="red",
#     main="Daily Cognitive Function", xlab="Cognitive Measure", ylab="Density")
#lines(Promiseplot2, type="o", pch=30,lwd=1, lty=2, col="orange")
#lines(Promiseplot3, type="o", pch=30,lwd=1, lty=2, col="green")
#lines(Promiseplot4, type="o", pch=30,lwd=1, lty=2, col="blue")



#plot(PromiseplotT, type="o", pch=22, lwd=2, lty=2, col="black",
#     main="Daily Cognitive Total", xlab="Cognitive Total", ylab="Density")

#title(main="Daily Cognitive Total", xlab="Cognitive Total", ylab="Density")

#plot(PromiseplotPA,type="o",pch=20, lwd=1, lty=2, col="darkgreen",
#     main="Physical Activity",xlab="Physical Activity", ylab="Density")

#plot(PromiseplotSB, type="o", pch=20, lwd=1, lty=2, col="darkred",
#     main="Sedentary Behavior",xlab="Sedentary Behavior", ylab="Density")


write.table(ef, file = "X:/Data analysis/Pat's Thesis 2012/EF.csv", sep = ",", col.names = NA,
            qmethod = "double")

##descriptives and correlations##

library(psych)
Pat.descrip<-describe(CEF)
write.table(Pat.descrip, file = "X:/Data analysis/Pat's Thesis 2012/Descrips.csv", sep = ",", col.names = NA,
            qmethod = "double")
Pat.cor<-cor(CEF, use="pairwise.complete.obs")
write.table(Pat.cor, file = "X:/Data analysis/Pat's Thesis 2012/newCors.csv", sep = ",", col.names = NA,
            qmethod = "double")



##Box Cox Transformations##


#library(MASS)
#library(forecast)

#boxcox(DCogTot ~ Dsedentary + metminD, data = Promise,
#       lambda = seq(.5, 1, length = 10))

#Promise$DCogTot_bc <- BoxCox(Promise$DCogTot,.97)

#boxcox(metminD ~ Dsedentary + DCogTot, data = Promise,
#       lambda = seq(0.3,.4, length = 10))

#Promise$metminD_bc <- BoxCox(Promise$metminD,.38)

#hist(Promise$metminD_bc)
#hist(Promise$metminD)

####making iMeans and residuals##

f <- function(x) c( iM=mean(x, na.rm=TRUE), iSD=sd(x, na.rm=TRUE) )

icfID=do.call( "rbind", tapply( Promise$id, Promise$id, f ))
icf1=do.call( "rbind", tapply( Promise$metmin, Promise$id, f ))
icf2=do.call( "rbind", tapply( Promise$Dsedentary, Promise$id, f ))


icf11<-cbind(icfID, icf1, icf2)
icf11<-data.frame(icf11)
icf11$id<-icf11$iM
ef<-merge(Promise, icf11, by="id")

ef$metminD_iM<-ef$iM.1
ef$Dsedentary_iM<-ef$iM.2

ef$rmetminD<-ef$metmin-ef$metminD_iM
ef$rDsedentary<-ef$Dsedentary-ef$Dsedentary_iM


##centeringvariables
ef$metminD_iMc<-ef$metminD_iM-(mean(ef$metminD_iM, na.rm=T))
ef$Dsedentary_iMc<-ef$Dsedentary_iM-(mean(ef$Dsedentary_iM, na.rm=T))




###hierarchical linear modeling
library(lme4)
haha <- lmer(DCogTot ~ Dsedentary_iMc + metminD_iMc + metminD_iMc*Dsedentary_iMc +
  rDsedentary + rmetminD + rmetminD*rDsedentary +
  (rmetminD|id)+(rDsedentary| id)+(rmetminD*rDsedentary| id), data=ef)
summary(haha)

install.packages('pbkrtest')
library(MBESS)

##signficance of MLM coefficients based on simulations##

library(pbkrtest)
beet_no.harv <- update(haha, .~.-rDsedentary)
PBmodcomp(haha, beet_no.harv, nsim=20)

beet_no.harv <- update(haha, .~.-rmetminD)
PBmodcomp(haha, beet_no.harv, nsim=20)

beet_no.harv <- update(haha, .~.-Dsedentary_iMc)
PBmodcomp(haha, beet_no.harv, nsim=20)

beet_no.harv <- update(haha, .~.-metminD_iMc)
PBmodcomp(haha, beet_no.harv, nsim=20)

beet_no.harv <- update(haha, .~.-rmetminD*rDsedentary)
PBmodcomp(haha, beet_no.harv, nsim=20)

beet_no.harv <- update(haha, .~.-metminD_iMc*Dsedentary_iMc)
PBmodcomp(haha, beet_no.harv, nsim=20)



##within-person correlations##
library(plyr)

withinCors<-ddply(CEF, .(id), summarise, cor=cor(rmetminD, rDsedentary, use="pairwise.complete.obs"))
mean(withinCors, na.rm=T)
sd(withinCors, na.rm=T)

##Intraclass correlations ICCs##
library(nlme)
library(psychometric)

ICC1.lme(metmin, id, ef)

ICC1.lme(DCogTot, id, ef)

##plotting by id##

library(ggplot2)

qplot(DCogTot,metmin, group=id, data=h, geom="line", color=id)



###Dissertation Study 4 Preliminary Analyses: Automatic Evaluations, Habits, and Physical Activity####

##reading in data##
base <-read.csv("C:\\Users\\alh379\\SkyDrive\\Study 4\\prelim4.csv", na.strings=".", stringsAsFactors=FALSE)
day <-read.csv("C:\\Users\\alh379\\SkyDrive\\Study 4\\prelimDLa.csv", na.strings=".", stringsAsFactors=FALSE)
wkday <-read.csv("C:\\Users\\alh379\\SkyDrive\\Study 4\\prelimDLb.csv", na.strings=".", stringsAsFactors=FALSE)
sciat <-read.csv("C:\\Users\\alh379\\SkyDrive\\Study 4\\prelimSCIAT4.csv", na.strings=".", stringsAsFactors=FALSE)

#between person corrs and descriptives##
dutchBW <-merge(base,sciat, by="id", all.x=F)

library(psych)
descrip<-describe(dutchBW)
write.table(descrip, file = "C:/Users/alh379/SkyDrive/Study 4/Study1Descrip.csv", sep = ",", col.names = NA,
            qmethod = "double")
Andre.cor<-cor(Andre, use="pairwise.complete.obs")
write.table(Andre.cor, file = "R:/Data Analysis/Andre paper/newCors.csv", sep = ",", col.names = NA,
            qmethod = "double")



#converting wide data to long data (with between-person vars)#
dla = reshape(day, direction="long",varying=list(names(day)[2:15]),
              v.names="count", idvar=c("id"), timevar="day")

dlb = reshape(wkday, direction="long",varying=list(names(wkday)[2:15]),
              v.names="wkday", idvar=c("id"), timevar="day")


##merging data##
dl<-merge(dla,dlb, by=c("day", "id"), all.x=F)
base2 <- merge(base,sciat,by="id", all.x=F)
dutch <-merge(base2,dl, all.x=F)


##scoring a scale##
library(psych)
items<-c("L1PAHab_2", "L1PAHab_3", "L1PAHab_5", "L1PAHab_8", "L1PAHab_10")
scaleKey <-c(1,1,1,1,1)
results<-score.items(keys=scaleKey, items = dutch[items], totals = F, missing = T, min = 1, max=7)
dutch$Rhabit<-results$score

dutch$PAv_c<-dutch$vdiffPA1-(mean(dutch$vdiffPA1, na.rm=T))
dutch$Rhabit_c<-dutch$Rhabit-(mean(dutch$Rhabit, na.rm=T))

##Box Cox Transformations##


library(MASS)
library(forecast)

boxcox(count ~ Rhabit_c + PAv_c, data = dutch,
       lambda = seq(0.2, .3, length = 10))
dutch$count_bc <- BoxCox(dutch$count,.29)

##making dummy variables##
dutch$dummy <- factor( with (dutch, ifelse((day==2),1,0)))
dutch$wkend <- factor( with (dutch, ifelse((wkday==6|wkday==0),1,0)))

library(lme4)

dutch.fit <- lmer(count_bc ~ PAv_c + wkend + Sex + BMI +
  Rhabit_c + dummy + dummy*Rhabit_c + dummy*PAv_c +
  (dummy| id) + (wkend| id), data=dutch)
summary(dutch.fit)

dutch.fit0 <- lmer(count_bc ~ PAv_c + wkend + Sex + BMI +
  Rhabit_c + day + day*Rhabit_c + day*PAv_c +
  (day| id) + (wkend| id), data=dutch)
summary(dutch.fit0)

##testing p value of itneraction##
##signficance of MLM coefficients based on simulations##

library(pbkrtest)
beet_no.harv <- update(dutch.fit, .~.-dummy*PAv_c)
PBmodcomp(dutch.fit, beet_no.harv, nsim=20)


write.table(dutch, file = "C:/Users/alh379/SkyDrive/Study 4/dutchHLM.csv", sep = ",", col.names = NA,
            qmethod = "double")




### KINES 321 Fall 2012 Implementation Intentions Group Differences - NEW and IMPROVED###


cf <-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\K321 analyses\\bwrepeat.csv", na.strings=".", stringsAsFactors=FALSE)

daysNA <-tapply(cf$metmin, cf$id, countNA)
daysNA<-data.frame(daysNA)
table(daysNA$daysNA)
hist(daysNA$daysNA, col="lightblue", main = " ",
     xlab="Number of Missed Daily Logs")

hist(cf$actPA, col="lightblue", main = " ",
     xlab="Clarity of Physical Activity Plans")
hist(cf$actSED, col="lightblue", main = " ",
     xlab="Clarity of Limiting Sedentary Behavior Plans")


####making iMeans and residuals##

f <- function(x) c( iM=mean(x, na.rm=TRUE), iSD=sd(x, na.rm=TRUE) )

icfID=do.call( "rbind", tapply( cf$id, cf$id, f ))
icf1=do.call( "rbind", tapply( cf$intPA_mv_lag, cf$id, f ))
icf2=do.call( "rbind", tapply( cf$intPA_vmw_lag, cf$id, f ))
icf3=do.call( "rbind", tapply( cf$intSED, cf$id, f ))


icf11<-cbind(icfID, icf1, icf2, icf3)
icf11<-data.frame(icf11)
icf11$id<-icf11$iM
icf<-merge(cf, icf11, by="id")

icf$iMintPA_mv<-icf$iM.1
icf$iMintPA_vmw<-icf$iM.2
icf$iMintSED<-icf$iM.3

icf$rintPA_mv<-icf$intPA_mv-icf$iMintPA_mv
icf$rintPA_vmw<-icf$intPA_vmw-icf$iMintPA_vmw
icf$rintSED<-icf$intSED-icf$iMintSED


icf$metminb_c<-icf$metminb-(mean(icf$metminb, na.rm=T))
icf$RhodesHabPAb_c<-icf$RhodesHabPAb-(mean(icf$RhodesHabPAb, na.rm=T))
icf$iMintPA_vmw_c<-icf$iMintPA_vmw-(mean(icf$iMintPA_vmw, na.rm=T))
icf$PA_v_c<-icf$PA_v-(mean(icf$PA_v, na.rm=T))
icf$BMI_c<-icf$BMI-(mean(icf$BMI, na.rm=T))

##descriptives and correlations##

library(psych)
descrips<-describe(icf)
write.table(descrips, file = "C:/Users/alh379/SkyDrive/DataAnalyses/K321 analyses/Descrips.csv", sep = ",", col.names = NA,
            qmethod = "double")

cor<-cor(check, use="pairwise.complete.obs")
write.table(cor, file = "C:/Users/alh379/SkyDrive/DataAnalyses/K321 analyses/CorsAlldata.csv", sep = ",", col.names = NA,
            qmethod = "double")



#Linear time model (MLM)
library(lme4)
hah1a <- lmer(metmin ~ metminb + sexb + BMI_c + studyday + daywkend +
  groupSED + groupPA + groupPA*groupSED +
  Rhabit*groupPA + daywkend*groupSED + daywkend*groupPA*groupSED +
  studyday*groupPA + studyday*groupSED + studyday*groupPA*groupSED
              + (daywkend|id) + (studyday|id), data=icf)
summary(hah1a)



##signficance of MLM coefficients based on simulations##
library(pbkrtest)
beet_no.harv <- update(hah1a, .~.-daywkend*groupPA)
PBmodcomp(hah1a, beet_no.harv, nsim=20)



write.table(icf, file = "C:/Users/alh379/SkyDrive/K321 analyses/iMr.csv", sep = ",", col.names = NA,
            qmethod = "double")

##making between dataset into within (repeat rows) short into long##
#rep <- bmi[rep(seq_len(nrow(bmi)), each=7),]



###Manipulation check of Implementation Planning Conditions##

check <-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\K321 analyses\\ManCheck2.csv", na.strings=".", stringsAsFactors=FALSE)

##ANOVA of two way factorial design##
fit <- aov(actSED1 ~ groupPA*groupSED, data=check)
summary(fit)


# Plot Means with Error Bars
library(gplots)
attach(check)
factor <- factor(groupSED)
plotmeans(actSED_mean~factor,xlab="Limiting Sedentary Planning",
          ylab="Clarity of Plans", main="Mean Plot\nwith 95% CI") 

##EZ Diffusion Model Discriminant Validity Tests##

##entering data##
Bears <-read.csv("C:\\Users\\hydea\\Dropbox\\Data analysis\\EZ diff RR\\KINES 321 Compiled Qual and IAT.csv", na.strings=".", stringsAsFactors=FALSE)
yup <-read.csv("C:\\Users\\alh379\\SkyDrive\\DataAnalyses\\Auto Atts IAT Scoring\\process.csv", na.strings=".", stringsAsFactors=FALSE)

library(psych)

i<-data.frame(Bears$L1PAAtt_4, Bears$L1PAAtt_5, Bears$L1PAAtt_6)
alpha(i)

#process<-data.frame(id=Bears$id, PA.v1=Bears$v1PA1, PA.v2=Bears$v2PA1,
#                 PA.a1=Bears$a1PA1, PA.a2=Bears$a2PA1,
#                 PA.n1=Bears$Ter1PA1, PA.n2=Bears$Ter2PA1,
#                 SED.v1=Bears$v1SED1, SED.v2=Bears$v2SED1,
#                 SED.a1=Bears$a1SED1, SED.a2=Bears$a2SED1,
#                 SED.n1=Bears$Ter1SED1, SED.n2=Bears$Ter2SED1,
#                 FLOW.v1=Bears$v1FLOW1, FLOW.v2=Bears$v2FLOW1,
#                 FLOW.a1=Bears$a1FLOW1, FLOW.a2=Bears$a2FLOW1,
#                 FLOW.n1=Bears$Ter1FLOW1, FLOW.n2=Bears$Ter2FLOW1)

##scatter plots##
attach(process)
plot(PA.v1, PA.v2, main="Information Processing Efficiency Scores",
     xlab="Block PA/Good vs. Bad", ylab="Block Good vs. PA/Bad", pch=19)

plot((PA.v1^2), (PA.v2^2), main="Information Processing Efficiency Scores",
     xlab="Block PA/Good vs. Bad", ylab="Block Good vs. PA/Bad", pch=19)

plot((PA.v1^(1/2)), (PA.v2^(1/2)), main="Information Processing Efficiency Scores",
     xlab="Block PA/Good vs. Bad", ylab="Block Good vs. PA/Bad", pch=19)

plot((1/PA.v1), (1/PA.v2), main="Information Processing Efficiency Scores",
     xlab="Block PA/Good vs. Bad", ylab="Block Good vs. PA/Bad", pch=19)

plot((log(PA.v1)), (log(PA.v2)), main="Information Processing Efficiency Scores",
     xlab="Block PA/Good vs. Bad", ylab="Block Good vs. PA/Bad", pch=19)

##testing multivariate normality##
library(mvsf)
C = t(yup[3:4])
mvsf(C)

C = t(yup[5:6])
mvsf(C)

C = t(yup[7:8])
mvsf(C)

qqplot(yup$PA.v1, yup$PA.v2, xlab="v1", ylab="v2")

yup$PA.n1.log<-log(PA.n1)
yup$PA.n2.log<-log(PA.n2)

C.log = t(yup[30:31])
mvsf(C.log)

qqplot(yup$PA.v1.log, yup$PA.v2.log, xlab="v1 log", ylab="v2 log")

yup$PA.v1.sqrt<-sqrt(PA.v1)
yup$PA.v2.sqrt<-sqrt(PA.v2)

C.sqrt = t(yup[32:33])
mvsf(C.sqrt)

qqplot(yup$PA.v1.sqrt, yup$PA.v2.sqrt, xlab="v1 sqrt", ylab="v2 sqrt")


#plot((PA.v1), (PA.v2), main="Information Processing Efficiency Scores",
#     xlab="Block PA/Good vs. Bad", ylab="Block Good vs. PA/Bad", pch=19)

#attach(process)
#plot(PA.a1, PA.a2, main="Response Conservativeness Scores",
#     xlab="Block PA/Good vs. Bad", ylab="Block Good vs. PA/Bad", pch=19)

#attach(process)
#plot(PA.n1, PA.n2, main="Nondecision Processing Time Scores",
#     xlab="Block PA/Good vs. Bad", ylab="Block Good vs. PA/Bad", pch=19)

#PAv.fit = lm(PA.v1 ~ PA.v2, data=process)
#process$PAv.res = resid(PAv.fit)

#PAa.fit = lm(PA.a1 ~ PA.a2, data=process)
#process$PAa.res = resid(PAa.fit)

#PAn.fit = lm(PA.n1 ~ PA.n2, data=process)
#process$PAn.res = resid(PAn.fit)


#SEDv.fit = lm(SED.v1 ~ SED.v2, data=process)
#process$SEDv.res = resid(SEDv.fit)

#SEDa.fit = lm(SED.a1 ~ SED.a2, data=process)
#process$SEDa.res = resid(SEDa.fit)

#SEDn.fit = lm(SED.n1 ~ SED.n2, data=process)
#process$SEDn.res = resid(SEDn.fit)


#FLOWv.fit = lm(FLOW.v1 ~ FLOW.v2, data=process)
#FLOWv.res = resid(FLOWv.fit)

#FLOWa.fit = lm(FLOW.a1 ~ FLOW.a2, data=process)
#FLOWa.res = resid(FLOWa.fit)

#FLOWn.fit = lm(FLOW.n1 ~ FLOW.n2, data=process)
#FLOWn.res = resid(FLOWn.fit)


#write.table(FLOWv.res, file = "C:/Users/alh379/SkyDrive/DataAnalyses/Auto Atts IAT Scoring/FLowv.csv", sep = ",", col.names = NA,
#            qmethod = "double")

#write.table(FLOWa.res, file = "C:/Users/alh379/SkyDrive/DataAnalyses/Auto Atts IAT Scoring/FLowa.csv", sep = ",", col.names = NA,
#            qmethod = "double")

#write.table(FLOWn.res, file = "C:/Users/alh379/SkyDrive/DataAnalyses/Auto Atts IAT Scoring/FLown.csv", sep = ",", col.names = NA,
#            qmethod = "double")

#write.table(process, file = "C:/Users/alh379/SkyDrive/DataAnalyses/Auto Atts IAT Scoring/process.csv", sep = ",", col.names = NA,
#            qmethod = "double")

library(psych)
descrips<-describe(yup)
write.table(descrips, file = "C:/Users/alh379/SkyDrive/DataAnalyses/Auto Atts IAT Scoring/DescripsYUP.csv", sep = ",", col.names = NA,
            qmethod = "double")

cor<-cor(Bears, use="pairwise.complete.obs")
write.table(cor, file = "C:/Users/alh379/SkyDrive/DataAnalyses/Auto Atts IAT Scoring/CorsYUP.csv", sep = ",", col.names = NA,
            qmethod = "double")

yup$PAv = yup$PA.v1-yup$PA.v2
yup$PAa = yup$PA.a2-yup$PA.a1
yup$PAn = yup$PA.n1-yup$PA.n2
yup$SEDv = yup$SED.v1-yup$SED.v2
yup$SEDa = yup$SED.a2-yup$SED.a1
yup$SEDn = yup$SED.n1-yup$SED.n2
yup$FLOWv = yup$FLOW.v1-yup$FLOW.v2
yup$FLOWa = yup$FLOW.a2-yup$FLOW.a1
yup$FLOWn = yup$FLOW.n1-yup$FLOW.n2

yups<-data.frame(PAv=yup$PAv, PAa=yup$PAa, PAn=yup$PAn,
                 SEDv=yup$SEDv, SEDa=yup$SEDa, SEDn=yup$SEDn,
                 FLOWv=yup$FLOWv, FLOWa=yup$FLOWa, FLOWn=yup$FLOWn)

means<-c((mean(yups$PAv, na.rm=T)), (mean(yups$PAa, na.rm=T)), (mean(yups$PAn, na.rm=T)),
         (mean(yups$SEDv, na.rm=T)), (mean(yups$SEDa, na.rm=T)), (mean(yups$SEDn, na.rm=T)),
         (mean(yups$FLOWv, na.rm=T)), (mean(yups$FLOWa, na.rm=T)), (mean(yups$FLOWn, na.rm=T)))

names(means)<-c("PAv", "PAa", "PAn", "SEDv", "SEDa", "SEDn", "FLOWv", "FLOWa", "FLOWn")

##Measurement Model##

require(OpenMx)
manifests<-c("PAv", "PAa", "PAn", "SEDv", "SEDa", "SEDn",
             "FLOWv", "FLOWa", "FLOWn")
latents<-c("IAT")
latents2<-c("PA", "SED", "FLOW")
latents3<-c("IATn", "IATa")
latentG<-c("G", "IAT")

none <-mxModel("none", type = "RAM", manifestVars=manifests, latentVars=latents2,
            mxPath(from="PA", to=c("PAn","PAv", "PAa"),free=T, arrows=1, value=.1),
            mxPath(from="SED", to=c("SEDn","SEDv", "SEDa"),free=T, arrows=1, value=.08),
            mxPath(from="FLOW", to=c("FLOWn","FLOWv", "FLOWa"),free=T, arrows=1, value=.08),
            mxPath(from=manifests, arrows=2, values=.8),
            mxPath(from=latents2, arrows=2, free=FALSE, values=1.0),
            mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

none <- mxRun(none)
summary(none)



uni <-mxModel("uni", type = "RAM", manifestVars=manifests, latentVars=latents,
                mxPath(from="IAT", to=c("PAn","SEDn", "FLOWn", "PAv", "SEDv", "FLOWv",
                                        "PAa", "SEDa", "FLOWa"),value=.08),
                mxPath(from=manifests, arrows=2),
                mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
                mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

uni <- mxRun(uni)
summary(uni)



IATna <-mxModel("IATna", type = "RAM", manifestVars=manifests, latentVars=latents3,
               mxPath(from="IATn", to=c("PAn","SEDn", "FLOWn"),value=.08),
               mxPath(from="IATa", to=c("PAa","SEDa", "FLOWa"),value=.08),
               mxPath(from=manifests, arrows=2),
               mxPath(from=latents3, arrows=2, free=FALSE, values=1.0),
               mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

IATna <- mxRun(IATna)
summary(IATna)



IATnv <-mxModel("IATna", type = "RAM", manifestVars=manifests, latentVars=latents3,
                mxPath(from="IATn", to=c("PAn","SEDn", "FLOWn"),value=.08),
                mxPath(from="IATa", to=c("PAv","SEDv", "FLOWv"),value=.08),
                mxPath(from=manifests, arrows=2),
                mxPath(from=latents3, arrows=2, free=FALSE, values=1.0),
                mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

IATnv <- mxRun(IATnv)
summary(IATnv)



IATav <-mxModel("IATna", type = "RAM", manifestVars=manifests, latentVars=latents3,
                mxPath(from="IATn", to=c("PAa","SEDa", "FLOWa"),value=.08),
                mxPath(from="IATa", to=c("PAv","SEDv", "FLOWv"),value=.08),
                mxPath(from=manifests, arrows=2),
                mxPath(from=latents3, arrows=2, free=FALSE, values=1.0),
                mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

IATav <- mxRun(IATav)
summary(IATav)


n <-mxModel("n", type = "RAM", manifestVars=manifests, latentVars=latents,
            mxPath(from=latents, to=c("PAn","SEDn", "FLOWn"),value=.08),
            mxPath(from=manifests, arrows=2),
            mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
            mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

n <- mxRun(n)
summary(n)


a <-mxModel("a", type = "RAM", manifestVars=manifests, latentVars=latents,
            mxPath(from=latents, to=c("PAa","SEDa", "FLOWa")),
            mxPath(from=manifests, arrows=2),
            mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
            mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

a <- mxRun(a)
summary(a)


v <-mxModel("v", type = "RAM", manifestVars=manifests, latentVars=latents,
            mxPath(from=latents, to=c("PAv","SEDv", "FLOWv")),
            mxPath(from=manifests, arrows=2),
            mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
            mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

v <- mxRun(v)
summary(v)


hyp <-mxModel("hyp", type = "RAM", manifestVars=manifests, latentVars=latents,
              mxPath(from=manifests, arrows=2),
              mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

hyp <- mxRun(hyp)
summary(hyp)



na <-mxModel("na", type = "RAM", manifestVars=manifests, latentVars=latents,
              mxPath(from=latents, to=c("PAn","SEDn", "FLOWn", "PAa", "SEDa", "FLOWa")),
              mxPath(from=manifests, arrows=2),
              mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
              mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

na <- mxRun(na)
summary(na)




nv <-mxModel("nv", type = "RAM", manifestVars=manifests, latentVars=latents,
             mxPath(from=latents, to=c("PAn","SEDn", "FLOWn", "PAv", "SEDv", "FLOWv")),
             mxPath(from=manifests, arrows=2),
             mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
             mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

nv <- mxRun(nv)
summary(nv)


av <-mxModel("av", type = "RAM", manifestVars=manifests, latentVars=latents,
             mxPath(from=latents, to=c("PAa","SEDa", "FLOWa", "PAv", "SEDv", "FLOWv")),
             mxPath(from=manifests, arrows=2),
             mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
             mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

av <- mxRun(av)
summary(av)


nav <-mxModel("nav", type = "RAM", manifestVars=manifests, latentVars=latents,
             mxPath(from=latents, to=c("PAa","SEDa", "FLOWa", "PAv", "SEDv", "FLOWv",
                                       "PAn", "SEDn", "FLOWn")),
             mxPath(from=manifests, arrows=2),
             mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
             mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

nav <- mxRun(nav)


noneOB <-mxModel("noneOB", type = "RAM", manifestVars=manifests, latentVars=latents2,
               mxPath(from="PA", to=c("PAn","PAv", "PAa"),free=T, arrows=1, value=.1),
               mxPath(from="SED", to=c("SEDn","SEDv", "SEDa"),free=T, arrows=1, value=.08),
               mxPath(from="FLOW", to=c("FLOWn","FLOWv", "FLOWa"),free=T, arrows=1, value=.08),
               mxPath(from=c("PA", "SED"), to="FLOW", free=T, arrows=2),
               mxPath(from="PA", to="SED", free=T, arrows=2),
               mxPath(from=manifests, arrows=2, values=.8),
               mxPath(from=latents2, arrows=2, free=FALSE, values=1.0),
               mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))


# run model and examine output

noneOB <- mxRun(noneOB)
summary(noneOB)



latentTHAT<-c("PA", "SED", "FLOW", "IAT")

nonev <-mxModel("nonev", type = "RAM", manifestVars=manifests, latentVars=latentTHAT,
                 mxPath(from="PA", to=c("PAn","PAv", "PAa"),free=T, arrows=1, value=.1),
                 mxPath(from="SED", to=c("SEDn","SEDv", "SEDa"),free=T, arrows=1, value=.08),
                 mxPath(from="FLOW", to=c("FLOWn","FLOWv", "FLOWa"),free=T, arrows=1, value=.08),
                 mxPath(from="IAT", to=c("PAv", "SEDv", "FLOWv"), free=T, arrows=1),
                 mxPath(from=manifests, arrows=2, values=.8),
                 mxPath(from=latentTHAT, arrows=2, free=FALSE, values=1.0),
                 mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))


# run model and examine output

nonev <- mxRun(nonev)
summary(nonev)

nonea <-mxModel("nonev", type = "RAM", manifestVars=manifests, latentVars=latentTHAT,
                mxPath(from="PA", to=c("PAn","PAv", "PAa"),free=T, arrows=1, value=.1),
                mxPath(from="SED", to=c("SEDn","SEDv", "SEDa"),free=T, arrows=1, value=.08),
                mxPath(from="FLOW", to=c("FLOWn","FLOWv", "FLOWa"),free=T, arrows=1, value=.08),
                mxPath(from="IAT", to=c("PAa", "SEDa", "FLOWa"), free=T, arrows=1),
                mxPath(from=manifests, arrows=2, values=.8, free=T),
                mxPath(from=latentTHAT, arrows=2, free=FALSE, values=1.0),
                #manifest means
                mxPath(
                  from="one",
                  to=manifests,
                  arrows=1, values=0,
                  free=F),
                # latent means
                mxPath(
                  from="one",
                  to=latentTHAT,
                  arrows=1, values=0,
                  free=F),    
                mxData(cov(yups, use="pairwise.complete.obs"), type="cov", means=means, numObs=105))


# run model and examine output

nonea <- mxRun(nonea)
summary(nonea)


nonen <-mxModel("nonev", type = "RAM", manifestVars=manifests, latentVars=latentTHAT,
                mxPath(from="PA", to=c("PAn","PAv", "PAa"),free=T, arrows=1, value=.1),
                mxPath(from="SED", to=c("SEDn","SEDv", "SEDa"),free=T, arrows=1, value=.08),
                mxPath(from="FLOW", to=c("FLOWn","FLOWv", "FLOWa"),free=T, arrows=1, value=.08),
                mxPath(from="IAT", to=c("PAn", "SEDn", "FLOWn"), free=T, arrows=1),
                mxPath(from=manifests, arrows=2, values=.8),
                mxPath(from=latentTHAT, arrows=2, free=FALSE, values=1.0),
                mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))


# run model and examine output

nonen <- mxRun(nonen)
summary(nonen)



Gv <-mxModel("Gv", type = "RAM", manifestVars=manifests, latentVars=latentG,
              mxPath(from="G", to=c("PAa","SEDa", "FLOWa", "PAv", "SEDv", "FLOWv",
                                        "PAn", "SEDn", "FLOWn")),
              mxPath(from="IAT", to=c("PAv", "SEDv", "FLOWv")),
              mxPath(from=manifests, arrows=2),
              mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
              mxData(cov(yups, use="pairwise.complete.obs"), type="cov", numObs=105))

# run model and examine output

Gv <- mxRun(Gv)
summary(Gv)




##comparing model fit##

mxCompare(none, hyp)
mxCompare(nav, hyp)
mxCompare(nv, hyp)
mxCompare(av, hyp)
mxCompare(na, hyp)
mxCompare(uni, hyp)
mxCompare(noneOB, hyp)
mxCompare(noneOB, none)
mxCompare(nonev, none)
mxCompare(nonea, none)
mxCompare(nonen, none)

mxCompare(a, hyp)
mxCompare(v, hyp)
mxCompare(n, hyp)
mxCompare(nav, c(n, a, v, na, av, nv, IATna, IATnv, IATav, hyp))




New bob 

latent<-c("HABIT")

uni <- mxModel("uni", 
               type="RAM",
               manifestVars=manifest,
               latentVars=latent,
               mxData(cov,type="cov", numObs=497, means=means),
               # manifest variances    
               mxPath(
                 from=manifest,
                 arrows=2, free=TRUE,
                 labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                          "vaut2", "vneg2", "vsrb2", "vaut3",
                          "vneg3", "vpat2", "vpat3", "vaut4",
                          "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
               ),
               # latent variances and covariance
               mxPath(
                 from=c("HABIT"),
                 arrows=2, values=2,
                 free=T
               ),
               mxPath(
                 from="HAB",
                 arrows=2, values=1,
                 free=F
               ),
               mxPath(
                 from=c("HAB"),
                 to=c("NEGSRB", "AUT", "PAT"),
                 arrows=1,
                 free=T,
                 labels=c("HABNEGSRB", "HABAUT", "HABPAT")
               ),
               mxPath(
                 from=c("NEGSRB"),
                 to=c("neg1"),
                 arrows=1,
                 free=F,
                 values=1,
                 labels=c("NEGneg1")
               ),
               mxPath(
                 from=c("NEGSRB"),
                 to=c("neg2", "neg3", "neg4"),
                 arrows=1,
                 free=TRUE,
                 labels=c("NEGneg2", "NEGneg3", "NEGneg4")
               ),
               mxPath(
                 from=c("PAT"),
                 to=c("pat1"),
                 arrows=1,
                 free=F,
                 values=1,
                 labels=c("PATpat1")
               ),
               mxPath(
                 from=c("PAT"),
                 to=c("pat2", "pat3", "pat4", "pat5"),
                 arrows=1,
                 free=TRUE,
                 labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
               ),
               mxPath(
                 from=c("NEGSRB"),
                 to=c("srb1"),
                 arrows=1,
                 free=T,
                 values=1,
                 labels=c("SRBsrb1")
               ),
               mxPath(
                 from=c("NEGSRB"),
                 to=c("srb2", "srb3", "srb4"),
                 arrows=1,
                 free=TRUE,
                 labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
               ),
               mxPath(
                 from=c("AUT"),
                 to=c("aut1"),
                 arrows=1,
                 free=F,
                 values=1,
                 labels=c("AUTaut1")
               ),
               mxPath(
                 from=c("AUT"),
                 to=c("aut2", "aut3", "aut4"),
                 arrows=1,
                 free=TRUE,
                 labels=c("AUTaut2", "AUTaut3", "AUTaut4")
               ),                
               # manifest means
               mxPath(
                 from="one",
                 to=manifest,
                 arrows=1,
                 free=FALSE,
                 values=0,
                 labels=c("mpat1", "maut1", "mneg1", "msrb1",
                          "maut2", "mneg2", "msrb2", "maut3",
                          "mneg3", "mpat2", "mpat3", "maut4",
                          "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
               ),
               # latent means
               mxPath(
                 from="one",
                 to=c("NEGSRB","PAT", "AUT"),
                 arrows=1, values=1,
                 free=T,
                 labels=c("mNEGSRB","mPAT", "mAUT")),
               # latent means
               mxPath(
                 from="one",
                 to="HAB",
                 arrows=1, values=0,
                 free=F)
)


threeobH <- mxRun(threeobH)
summary(threeobH, intervals=T)






threeobH <- mxModel("threeobH", 
                    type="RAM",
                    manifestVars=manifest,
                    latentVars=latentH,
                    mxData(cov,type="cov", numObs=497, means=means),
                    # manifest variances    
                    mxPath(
                      from=manifest,
                      arrows=2, free=TRUE,
                      labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                               "vaut2", "vneg2", "vsrb2", "vaut3",
                               "vneg3", "vpat2", "vpat3", "vaut4",
                               "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                    ),
                    # latent variances and covariance
                    mxPath(
                      from=c("AUT", "NEGSRB", "PAT"),
                      arrows=2, values=2,
                      free=T
                    ),
                    mxPath(
                      from="HAB",
                      arrows=2, values=1,
                      free=F
                    ),
                    mxPath(
                      from=c("HAB"),
                      to=c("NEGSRB", "AUT", "PAT"),
                      arrows=1,
                      free=T,
                      labels=c("HABNEGSRB", "HABAUT", "HABPAT")
                    ),
                    mxPath(
                      from=c("NEGSRB"),
                      to=c("neg1"),
                      arrows=1,
                      free=F,
                      values=1,
                      labels=c("NEGneg1")
                    ),
                    mxPath(
                      from=c("NEGSRB"),
                      to=c("neg2", "neg3", "neg4"),
                      arrows=1,
                      free=TRUE,
                      labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                    ),
                    mxPath(
                      from=c("PAT"),
                      to=c("pat1"),
                      arrows=1,
                      free=F,
                      values=1,
                      labels=c("PATpat1")
                    ),
                    mxPath(
                      from=c("PAT"),
                      to=c("pat2", "pat3", "pat4", "pat5"),
                      arrows=1,
                      free=TRUE,
                      labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                    ),
                    mxPath(
                      from=c("NEGSRB"),
                      to=c("srb1"),
                      arrows=1,
                      free=T,
                      values=1,
                      labels=c("SRBsrb1")
                    ),
                    mxPath(
                      from=c("NEGSRB"),
                      to=c("srb2", "srb3", "srb4"),
                      arrows=1,
                      free=TRUE,
                      labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                    ),
                    mxPath(
                      from=c("AUT"),
                      to=c("aut1"),
                      arrows=1,
                      free=F,
                      values=1,
                      labels=c("AUTaut1")
                    ),
                    mxPath(
                      from=c("AUT"),
                      to=c("aut2", "aut3", "aut4"),
                      arrows=1,
                      free=TRUE,
                      labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                    ),                
                    # manifest means
                    mxPath(
                      from="one",
                      to=manifest,
                      arrows=1,
                      free=FALSE,
                      values=0,
                      labels=c("mpat1", "maut1", "mneg1", "msrb1",
                               "maut2", "mneg2", "msrb2", "maut3",
                               "mneg3", "mpat2", "mpat3", "maut4",
                               "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                    ),
                    # latent means
                    mxPath(
                      from="one",
                      to=c("NEGSRB","PAT", "AUT"),
                      arrows=1, values=1,
                      free=T,
                      labels=c("mNEGSRB","mPAT", "mAUT")),
                    # latent means
                    mxPath(
                      from="one",
                      to="HAB",
                      arrows=1, values=0,
                      free=F)
)


threeobH <- mxRun(threeobH)
summary(threeobH, intervals=T)






latent=c("NEGSRB","PAT", "AUT")

threeob <- mxModel("threeob", 
                   type="RAM",
                   manifestVars=manifest,
                   latentVars=latent,
                   mxData(cov,type="cov", numObs=497, means=means),
                   # manifest variances    
                   mxPath(
                     from=manifest,
                     arrows=2, free=TRUE,
                     labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                              "vaut2", "vneg2", "vsrb2", "vaut3",
                              "vneg3", "vpat2", "vpat3", "vaut4",
                              "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                   ),
                   # latent variances and covariance
                   mxPath(
                     from=c("AUT", "NEGSRB", "PAT"),
                     arrows=2, values=2,
                     free=T
                   ),
                   mxPath(
                     from=c("NEGSRB"),
                     to=c("neg1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("NEGneg1")
                   ),
                   mxPath(
                     from=c("NEGSRB"),
                     to=c("neg2", "neg3", "neg4"),
                     arrows=1,
                     free=TRUE,
                     labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                   ),
                   mxPath(
                     from=c("PAT"),
                     to=c("pat1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("PATpat1")
                   ),
                   mxPath(
                     from=c("PAT"),
                     to=c("pat2", "pat3", "pat4", "pat5"),
                     arrows=1,
                     free=TRUE,
                     labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                   ),
                   mxPath(
                     from=c("NEGSRB"),
                     to=c("srb1"),
                     arrows=1,
                     free=T,
                     values=1,
                     labels=c("SRBsrb1")
                   ),
                   mxPath(
                     from=c("NEGSRB"),
                     to=c("srb2", "srb3", "srb4"),
                     arrows=1,
                     free=TRUE,
                     labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                   ),
                   mxPath(
                     from=c("AUT"),
                     to=c("aut1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("AUTaut1")
                   ),
                   mxPath(
                     from=c("AUT"),
                     to=c("aut2", "aut3", "aut4"),
                     arrows=1,
                     free=TRUE,
                     labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                   ),                
                   # manifest means
                   mxPath(
                     from="one",
                     to=manifest,
                     arrows=1,
                     free=FALSE,
                     values=0,
                     labels=c("mpat1", "maut1", "mneg1", "msrb1",
                              "maut2", "mneg2", "msrb2", "maut3",
                              "mneg3", "mpat2", "mpat3", "maut4",
                              "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                   ),
                   # latent means
                   mxPath(
                     from="one",
                     to=c("NEGSRB","PAT", "AUT"),
                     arrows=1, values=1,
                     free=T,
                     labels=c("mNEGSRB","mPAT", "mAUT"))
)


threeob <- mxRun(threeob)
summary(threeob, intervals=T)



fourobH <- mxModel("fourobH", 
                   type="RAM",
                   manifestVars=manifest,
                   latentVars=latentH,
                   mxData(cov,type="cov", numObs=497, means=means),
                   # manifest variances    
                   mxPath(
                     from=manifest,
                     arrows=2, free=TRUE,
                     labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                              "vaut2", "vneg2", "vsrb2", "vaut3",
                              "vneg3", "vpat2", "vpat3", "vaut4",
                              "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                   ),
                   # latent variances and covariance
                   mxPath(
                     from=c("AUT", "NEG", "SRB", "PAT"),
                     arrows=2, values=2,
                     free=T
                   ),
                   mxPath(
                     from="HAB",
                     arrows=2, values=1,
                     free=F
                   ),
                   mxPath(
                     from=c("HAB"),
                     to=c("SRB", "AUT"),
                     arrows=1,
                     free=T,
                     labels=c("HABSRB", "HABAUT")
                   ),
                   mxPath(
                     from=c("HAB"),
                     to=c("NEG", "PAT"),
                     arrows=1,
                     free=T,
                     labels=c("HABNEG", "HABPAT")
                   ),
                   mxPath(
                     from=c("NEG"),
                     to=c("neg1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("NEGneg1")
                   ),
                   mxPath(
                     from=c("NEG"),
                     to=c("neg2", "neg3", "neg4"),
                     arrows=1,
                     free=TRUE,
                     labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                   ),
                   mxPath(
                     from=c("PAT"),
                     to=c("pat1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("PATpat1")
                   ),
                   mxPath(
                     from=c("PAT"),
                     to=c("pat2", "pat3", "pat4", "pat5"),
                     arrows=1,
                     free=TRUE,
                     labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                   ),
                   mxPath(
                     from=c("SRB"),
                     to=c("srb1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("SRBsrb1")
                   ),
                   mxPath(
                     from=c("SRB"),
                     to=c("srb2", "srb3", "srb4"),
                     arrows=1,
                     free=TRUE,
                     labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                   ),
                   mxPath(
                     from=c("AUT"),
                     to=c("aut1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("AUTaut1")
                   ),
                   mxPath(
                     from=c("AUT"),
                     to=c("aut2", "aut3", "aut4"),
                     arrows=1,
                     free=TRUE,
                     labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                   ),                
                   # manifest means
                   mxPath(
                     from="one",
                     to=manifest,
                     arrows=1,
                     free=FALSE,
                     values=0,
                     labels=c("mpat1", "maut1", "mneg1", "msrb1",
                              "maut2", "mneg2", "msrb2", "maut3",
                              "mneg3", "mpat2", "mpat3", "maut4",
                              "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                   ),
                   # latent means
                   mxPath(
                     from="one",
                     to=c("NEG","PAT", "SRB", "AUT"),
                     arrows=1, values=1,
                     free=T,
                     labels=c("mNEG","mPAT", "mSRB", "mAUT")),
                   # latent means
                   mxPath(
                     from="one",
                     to="HAB",
                     arrows=1, values=0,
                     free=F)
)


# ----------------------------------------
# run model and examine output

fourobH <- mxRun(fourobH)
summary(fourobH, intervals=T)




fourobH <- mxModel("fourobH", 
                   type="RAM",
                   manifestVars=manifest,
                   latentVars=latentH,
                   mxData(cov,type="cov", numObs=497, means=means),
                   # manifest variances    
                   mxPath(
                     from=manifest,
                     arrows=2, free=TRUE,
                     labels=c("vpat1", "vaut1", "vneg1", "vsrb1",
                              "vaut2", "vneg2", "vsrb2", "vaut3",
                              "vneg3", "vpat2", "vpat3", "vaut4",
                              "vneg4", "vsrb3", "vpat4", "vpat5", "vsrb4")
                   ),
                   # latent variances and covariance
                   mxPath(
                     from=c("AUT", "NEG", "SRB", "PAT"),
                     arrows=2, values=2,
                     free=T
                   ),
                   mxPath(
                     from="HAB",
                     arrows=2, values=1,
                     free=F
                   ),
                   mxPath(
                     from=c("HAB"),
                     to=c("SRB", "AUT"),
                     arrows=1,
                     free=T,
                     labels=c("HABSRB", "HABAUT")
                   ),
                   mxPath(
                     from=c("HAB"),
                     to=c("NEG", "PAT"),
                     arrows=1,
                     free=T,
                     labels=c("HABNEG", "HABPAT")
                   ),
                   mxPath(
                     from=c("NEG"),
                     to=c("neg1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("NEGneg1")
                   ),
                   mxPath(
                     from=c("NEG"),
                     to=c("neg2", "neg3", "neg4"),
                     arrows=1,
                     free=TRUE,
                     labels=c("NEGneg2", "NEGneg3", "NEGneg4")
                   ),
                   mxPath(
                     from=c("PAT"),
                     to=c("pat1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("PATpat1")
                   ),
                   mxPath(
                     from=c("PAT"),
                     to=c("pat2", "pat3", "pat4", "pat5"),
                     arrows=1,
                     free=TRUE,
                     labels=c("PATpat2", "PATpat3", "PATpat4", "PATpat5")
                   ),
                   mxPath(
                     from=c("SRB"),
                     to=c("srb1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("SRBsrb1")
                   ),
                   mxPath(
                     from=c("SRB"),
                     to=c("srb2", "srb3", "srb4"),
                     arrows=1,
                     free=TRUE,
                     labels=c("SRBsrb2", "SRBsrb3", "SRBsrb4")
                   ),
                   mxPath(
                     from=c("AUT"),
                     to=c("aut1"),
                     arrows=1,
                     free=F,
                     values=1,
                     labels=c("AUTaut1")
                   ),
                   mxPath(
                     from=c("AUT"),
                     to=c("aut2", "aut3", "aut4"),
                     arrows=1,
                     free=TRUE,
                     labels=c("AUTaut2", "AUTaut3", "AUTaut4")
                   ),                
                   # manifest means
                   mxPath(
                     from="one",
                     to=manifest,
                     arrows=1,
                     free=FALSE,
                     values=0,
                     labels=c("mpat1", "maut1", "mneg1", "msrb1",
                              "maut2", "mneg2", "msrb2", "maut3",
                              "mneg3", "mpat2", "mpat3", "maut4",
                              "mneg4", "msrb3", "mpat4", "mpat5", "msrb4")
                   ),
                   # latent means
                   mxPath(
                     from="one",
                     to=c("NEG","PAT", "SRB", "AUT"),
                     arrows=1, values=1,
                     free=T,
                     labels=c("mNEG","mPAT", "mSRB", "mAUT")),
                   # latent means
                   mxPath(
                     from="one",
                     to="HAB",
                     arrows=1, values=0,
                     free=F)
)


# ----------------------------------------
# run model and examine output

fouro <- mxRun(fourobH)
summary(fourobH, intervals=T)
