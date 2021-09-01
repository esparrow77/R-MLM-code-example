##Load packages
library(Matrix)
library(lme4)
library(lmerTest)
library(car)
library(readr)
library(jtools)
library(reghelper)
library(ggeffects)
library(ggplot2)
library(psych)
library(emmeans)
library(nlme)
library(devtools)
library(emmeans)
library(plotly)

##Read in datasets.Edit to your path 
CCT2B <- read_csv("/Users/delluser/Dropbox/My PC (DESKTOP-DUOC1HI)/Desktop/RPractice/CCT2B.csv")
View(CCT2B)

CCT2BRoster <- read_csv("/Users/delluser/Dropbox/My PC (DESKTOP-DUOC1HI)/Desktop/RPractice/CCT2BRoster.csv")
View(CCT2BRoster)

##t-tests for participant characteristics
#OA higher income, vocab, PANAS_positive
#OA lower perceptual speed, DASS21 stress & anxiety dimensions

describe(CCT2BRoster~Age_Group)

t.test(Education~ Age_Group, data = CCT2BRoster)
t.test(Income~ Age_Group, data = CCT2BRoster)
t.test(Mill_hill~ Age_Group, data = CCT2BRoster)
t.test(PANAS_p~ Age_Group, data = CCT2BRoster)
t.test(PANAS_n~ Age_Group, data = CCT2BRoster)
t.test(DASS_D~ Age_Group, data = CCT2BRoster)
t.test(DASS_A~ Age_Group, data = CCT2BRoster)
t.test(DASS_S~ Age_Group, data = CCT2BRoster)
t.test(VASSelfRelevance~ Age_Group, data = CCT2BRoster)

#######################Start LMM

##Create a second dummy-coded variable for Context
CCT2B$Context_CS <- recode(CCT2B$Context_SC,"'0'='1'; else='0'")

##Create a second dummy-coded variable for age (O=0, Y=1)
CCT2B$Age_OY <- recode(CCT2B$Age_YO,"'0'='1'; else='0'")

#Label for Age
CCT2B$Age <- recode(CCT2B$Age_YO,"'0'='Younger'; else='Older'")

##Center continuous IV (Self Relevance VAS)
CCT2B$SelfRelevance_c <- scale(CCT2B$SelfRelevance)

#Effects-coded age
CCT2B$Age_e <- recode(CCT2B$Age_YO,"'0'='-1'; else='1'")

#Effects-coded Context
CCT2B$Context_e <- recode(CCT2B$Context_SC,"'0'='-1'; else='1'")



#Effects-coded gain value
CCT2B$gain_e <- recode(CCT2B$gain_LH,"'0'='-1'; else='1'")

#Effects-coded loss value
CCT2B$loss_e <- recode(CCT2B$loss_LH,"'0'='-1'; else='1'")

#Effects-coded loss cards
CCT2B$losscards_e <- recode(CCT2B$losscards_LH,"'0'='-1'; else='1'")


##Run a completely null model, no effects or random effects (intercept-only)
#tests for significant clustering i.e., MLM appropriate
#Average cards flipped is 11.34
M0 <- gls(Cards_flipped~1, data=CCT2B, method = "ML")
summary(M0)

##Run unconditional model, add random effects (no fixed effects yet)
M1 <- lme(Cards_flipped~1,data = CCT2B, method = "ML", random = ~1|ID)
summary(M1)
anova(M0,M1)##Compare intercept-only to unconditional random-intercept model
#using AIC, model fit improved in M1, MLM appropriate

##Run model with fixed effects & random effects (SC)
M2 <- lme(Cards_flipped~ Age_YO + Context_SC + losscards_LH + gain_LH + loss_LH,
          data=CCT2B, method = "ML",random = ~1|ID)
summary(M2)
anova(M0,M1,M2) #therefore adding predictors is a better fit


##Run model with fixed effects & random effects and interactions (SC)
M3a <- lme(Cards_flipped ~  Age_YO + Context_SC + gain_LH + loss_LH + 
             losscards_LH + (Age_YO*Context_SC) + (Age_YO*gain_LH)+ 
             (Age_YO*loss_LH) + (Age_YO*losscards_LH),
           data=CCT2B, method = "ML", random= ~1|ID)
summary(M3a)

##Run model with fixed effects & random effects and interactions (CS)
M3b <- lme(Cards_flipped ~  Age_YO + Context_CS + gain_LH + loss_LH + 
             losscards_LH + (Age_YO*Context_CS) + (Age_YO*gain_LH)+ 
             (Age_YO*loss_LH) + (Age_YO*losscards_LH),
           data=CCT2B, method = "ML", random= ~1|ID)
summary(M3b)

anova(M0,M1,M2,M3b) 

########Run model with effects coded variables and their interactions



##Running lme with cards flipped 
##Run model with fixed effects & random effects and interactions USING CARDS FLIPPED
M4e <- lme(Cards_flipped ~  Age_e + Context_e + gain_e + loss_e + 
             losscards_e + (Age_e*Context_e) + (Age_e*gain_e)+ 
             (Age_e*loss_e) + (Age_e*losscards_e),
           data=CCT2B, method = "ML", random= ~1|ID)
summary(M4e)
Anova(M4e)
AIC(M4e)

M4f <- lme(Cards_flipped ~  Age_e + Context_e + gain_e + loss_e + 
             losscards_e + (Age_e*Context_e) + (Age_e*gain_e)+ 
             (Age_e*loss_e) + (Age_e*losscards_e)+
             (Age_e*gain_e*Context_e)+(Age_e*loss_e*Context_e)+(Age_e*losscards_e*Context_e),
           data=CCT2B, method = "ML", random= ~1|ID)
summary(M4f)
Anova(M4f)
AIC(M4f)## 3-way interactions increase model AIC, use M4f for analysis
anova(M0,M1,M2,M3b,M4e,M4f)


#break up interactions for lme analysis 
#younger reference
M4y <- lme(Cards_flipped ~  Age_YO + Context_e + gain_e + loss_e + 
             losscards_e + (Age_YO*Context_e) + (Age_YO*gain_e)+ 
             (Age_YO*loss_e) + (Age_YO*losscards_e)+
             (Age_YO*gain_e*Context_e)+(Age_YO*loss_e*Context_e)+(Age_YO*losscards_e*Context_e),
           data=CCT2B, method = "ML", random= ~1|ID)
summary(M4y)
Anova(M4y)




#older reference
M4old <- lme(Cards_flipped ~  Age_OY + Context_e + gain_e + loss_e + 
             losscards_e + (Age_OY*Context_e) + (Age_OY*gain_e)+ 
             (Age_OY*loss_e) + (Age_OY*losscards_e)+
             (Age_OY*gain_e*Context_e)+(Age_OY*loss_e*Context_e)+(Age_OY*losscards_e*Context_e),
           data=CCT2B, method = "ML", random= ~1|ID)
summary(M4old)
Anova(M4old)



######################

#Means for ease of interpreting Main effects of lme analysis
tapply(CCT2B$Cards_flipped, CCT2B$Context,mean)
tapply(CCT2B$Cards_flipped, CCT2B$gain_code,mean)
tapply(CCT2B$Cards_flipped, CCT2B$loss_code,mean)
tapply(CCT2B$Cards_flipped, CCT2B$losscards_code,mean)


#Let's get means and CI's for plotting lme analysis 
emAxC<-emmeans(M4f, ~ Age_e*Context_e)
summary(emAxC)
contrast(emAxC, "revpairwise", by= "Age_e", adjust= "none")
#Younger adults flipped more in SGCL than CGSL 
#Older adults flipped more in SGCL than CGSL 

emAxG<-emmeans(M4f, ~ Age_e*gain_e)
summary(emAxG)
contrast(emAxG, "revpairwise", by= "Age_e", adjust= "none")
#Younger adults flipped more cards in high gain than high loss 
#Older adults flipped more more cards in high gain than high loss (p=0.04)

emAxL<-emmeans(M4f, ~ Age_e*loss_e)
summary(emAxL)
contrast(emAxL, "revpairwise", by= "Age_e", adjust= "none")
#Younger adults non sensitive to loss value 
#Older adults flipped more cards for low loss than high loss

emAxLC<-emmeans(M4f, ~ Age_e*losscards_e)
summary(emAxLC)
contrast(emAxLC, "revpairwise", by= "Age_e", adjust= "none")
#Younger adults flipped more for 1 loss card than 2 loss cards 
#Older adults flipped more for 1 loss card than 2 loss cards



#Set up formatting theme for the plot
plot_theme <- theme(axis.line=element_line( size = 0.5), axis.ticks =element_line(size = 0.5),
                    axis.text=element_text(color = "black", size= 12,face="plain"), axis.title=element_text(color = "black", face="plain", size = 12), 
                    strip.text = element_text(size = 12, face = "plain"),  panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                    panel.background = element_blank(), strip.background = element_blank(),
                    legend.key = element_blank(), legend.title = element_blank(), legend.text = element_text(face = "plain", size = 12),
                    panel.spacing = unit(1, "lines"))


##########Plot for Age x Context using Cards flipped lme analysis
##Read in dataset.Edit to your path 
Hot2B_ContextEMM <- read_csv("/Users/delluser/Dropbox/My PC (DESKTOP-DUOC1HI)/Desktop/RPractice/Hot2B_ContextEMM.csv")
View(Hot2B_ContextEMM)


##Cards flipped as a function of age and context
tiff('Hot2B_AxC.tiff', units = "in", width = 5, height = 3, res = 250)
h1<-ggplot(Hot2B_ContextEMM, aes(x = Context, color = Age, group = Age, y= emmean)) + 
  stat_summary(fun= mean, geom = "point")+
  stat_summary(fun= mean, geom = "line")+
  geom_errorbar(aes(ymin=lowerCL, ymax=higherCL), width=.1,alpha=0.2,
                position=position_dodge(0.05))+
  scale_fill_manual(values=c( "palevioletred1", "firebrick")) +
  scale_color_manual(values=c( "palevioletred1", "firebrick")) +
  labs(x = "\nContext",
       y = "Cards Flipped (EMM)")+
  ylim(2,3.5)+
  plot_theme
dev.off()



#Gain value Plots########################## lme 

##Read in dataset.Edit to your path 
Hot2B_GainEMM <- read_csv("/Users/delluser/Dropbox/My PC (DESKTOP-DUOC1HI)/Desktop/RPractice/Hot2B_GainEMM.csv")

##Cards Flipped as a function of age and Gain value

tiff('Hot2B_AxG.tiff', units = "in", width = 5, height = 3, res = 250)
h2<-ggplot(Hot2B_GainEMM, aes(x = Gain, color = Age, group = Age, y= emmeans)) + 
  stat_summary(fun= mean, geom = "point")+
  stat_summary(fun= mean, geom = "line")+
  geom_errorbar(aes(ymin=lowerCL, ymax=higherCL), width=.1,alpha=0.2,
                position=position_dodge(0.05))+
  scale_fill_manual(values=c( "palevioletred1", "firebrick")) +
  scale_color_manual(values=c( "palevioletred1", "firebrick")) +
  labs(x = "\nGain Value",
       y = "Cards Flipped (EMM)")+
  ylim(2,3.5)+
  plot_theme
dev.off()


#Loss Value Plots############################################### lme 

##Read in dataset.Edit to your path 
Hot2B_LossEMM <- read_csv("/Users/delluser/Dropbox/My PC (DESKTOP-DUOC1HI)/Desktop/RPractice/Hot2B_LossEMM.csv")

#Cards flipped as a function of age and Loss Value
tiff('Hot2B_AxL.tiff', units = "in", width = 5, height = 3, res = 250)
h3<-ggplot(Hot2B_LossEMM, aes(x = Loss, color = Age, group = Age, y= emmeans)) + 
  stat_summary(fun= mean, geom = "point")+
  stat_summary(fun= mean, geom = "line")+
  geom_errorbar(aes(ymin=lowerCL, ymax=higherCL), width=.1,alpha=0.2,
                position=position_dodge(0.05))+
  scale_fill_manual(values=c( "palevioletred1", "firebrick")) +
  scale_color_manual(values=c( "palevioletred1", "firebrick")) +
  labs(x = "\nLoss Value",
       y = "Cards Flipped (EMM)")+
  ylim(2,3.5)+
  plot_theme
dev.off()

#Loss Cards############################################################## lme 

Hot2B_CardsEMM <- read_csv("/Users/delluser/Dropbox/My PC (DESKTOP-DUOC1HI)/Desktop/RPractice/Hot2B_CardsEMM.csv")

##cards flipped as a function of age and loss probability
tiff('Hot2B_AxLC.tiff', units = "in", width = 5, height = 3, res = 250)
h4<-ggplot(Hot2B_CardsEMM, aes(x = LossCards, color = Age, group = Age, y= emmeans)) + 
  stat_summary(fun= mean, geom = "point")+
  stat_summary(fun= mean, geom = "line")+
  geom_errorbar(aes(ymin=lowerCL, ymax=higherCL), width=.1,alpha=0.2,
                position=position_dodge(0.05))+
  scale_fill_manual(values=c( "palevioletred1", "firebrick")) +
  scale_color_manual(values=c( "palevioletred1", "firebrick")) +
  labs(x = "\nLoss Cards",
       y = "Cards Flipped (EMM)")+
  ylim(2,3.5)+
  plot_theme
dev.off()


##combine 4 plots
plot1<-h1 + labs(title = NULL, x= NULL, y=NULL)+ theme(legend.position = "none")
plot2<-h2+ labs(title = NULL, x= NULL, y= NULL)+theme(legend.position = "none")
plot3<-h3 + labs(title = NULL, x= NULL, y=NULL)+ theme(legend.position = "none")
plot4<-h4+labs(title = NULL, x= NULL, y= NULL)+ theme(legend.position = "none")


prow<-plot_grid(plot1, plot2,plot3,plot4)
prow



tiff('CCT2Bhot.tiff', units = "in", width = 6, height = 4, res = 250)
plot_grid(prow)
dev.off()


#split by younger and older to figure out 3 way agexcontextxlosscard interaction

Younger <- subset(CCT2B, Age_OY=='1',
                  select = (1:29)) 
Older <- subset(CCT2B, Age_OY=='0',
                select = (1:29)) 


Mlossy <- lme(Cards_flipped ~ Context_e + losscards_e +
               (Context_e*losscards_e),
             data=Younger, method = "ML", random= ~1|ID)
summary(Mlossy)
Anova(Mlossy)


###breaking apart younger context x loss cards interaction, using M4y
#Let's get means and CI's for plotting lme analysis 
emYLxYLC<-emmeans(Mlossy, ~ losscards_e*Context_e)
summary(emYLxYLC)
contrast(emYLxYLC, "revpairwise", by= "Context_e", adjust= "none")
#more cards flipped for SGCL low loss than high loss SGCL
#no loss sensitivity for CGSL (keeping low regardless to avoid self loss)
##Read in dataset.Edit to your path 
Hot2B_younger3EMM <- read_csv("/Users/delluser/Dropbox/My PC (DESKTOP-DUOC1HI)/Desktop/RPractice/Hot2B_younger3EMM.csv")
View(Hot2B_younger3EMM)


tiff('Hot2B_younger3.tiff', units = "in", width = 5, height = 3, res = 250)
plot1<-ggplot(Hot2B_younger3EMM, aes(x = Losscard, color = Context, group = Context, y= emmean)) + 
  stat_summary(fun= mean, geom = "point")+
  stat_summary(fun= mean, geom = "line")+
  geom_errorbar(aes(ymin=lowerCL, ymax=higherCL), width=.1,alpha=0.2,
                position=position_dodge(0.05))+
  scale_fill_manual(values=c( "gold", "red")) +
  scale_color_manual(values=c( "gold", "red")) +
  labs(x = "\nLoss Cards",
       y = "Cards Flipped (EMM)")+
  ylim(1.5,4)+
  plot_theme
dev.off()

plot1




###breaking apart older context x loss cards interaction, using Mlosso
Mlosso <- lme(Cards_flipped ~ + Context_e + losscards_e +  
                (Context_e*losscards_e),
              data=Older, method = "ML", random= ~1|ID)
summary(Mlosso)
Anova(Mlosso)



#Let's get means and CI's for plotting lme analysis 
emOLxOLC<-emmeans(Mlosso, ~ losscards_e*Context_e)
summary(emOLxOLC)
contrast(emOLxOLC, "revpairwise", by= "Context_e", adjust= "none")
#more cards flipped for SGCL low loss than high loss SGCL
#no loss sensitivity for CGSL (keeping low regardless to avoid self loss)
##Read in dataset.Edit to your path 
Hot2B_older3EMM <- read_csv("/Users/delluser/Dropbox/My PC (DESKTOP-DUOC1HI)/Desktop/RPractice/Hot2B_older3EMM.csv")
View(Hot2B_older3EMM)

tiff('Hot2B_older3.tiff', units = "in", width = 5, height = 3, res = 250)
plot2<-ggplot(Hot2B_older3EMM, aes(x = Losscard, color = Context, group = Context, y= emmean)) + 
  stat_summary(fun= mean, geom = "point")+
  stat_summary(fun= mean, geom = "line")+
  geom_errorbar(aes(ymin=lowerCL, ymax=higherCL), width=.1,alpha=0.2,
                position=position_dodge(0.05))+
  scale_fill_manual(values=c( "gold", "red")) +
  scale_color_manual(values=c( "gold", "red")) +
  labs(x = "\nLoss Cards",
       y = "Cards Flipped (EMM)")+
  ylim(1.5,4)+
  plot_theme
dev.off()

plot2
plot3<-ggplot(Hot2B_older3EMM, aes(x = c(Losscard), color = Context, group = Context, y= emmean)) + 
  stat_summary(fun= mean, geom = "point")+
  stat_summary(fun= mean, geom = "line")+
  geom_errorbar(aes(ymin=lowerCL, ymax=higherCL), width=.1,alpha=0.2,
                position=position_dodge(0.05))+
  scale_fill_manual(values=c( "gold", "red")) +
  scale_color_manual(values=c( "gold", "red")) +
  labs(x = "\nLoss Cards",
       y = "Cards Flipped (EMM)")+
  ylim(1.5,4)+
  plot_theme
plot3




library(cowplot)

plot1<-plot1 + labs(title = NULL, x= NULL)+ theme(legend.position = "none")
plot3<-plot3+ labs(title = NULL, x= NULL, y= NULL)+scale_color_brewer(plabels=c("SGCL", "CGSL"))
plot2<-plot2 + labs(title = NULL, x= NULL, y=NULL)+ theme(legend.position = "none")
plot1
plot2

prow<-plot_grid(plot1, plot2)
prow



tiff('CCT2B_Losscardscowplotleg.tiff', units = "in", width = 5, height = 3, res = 250)
plot_grid(plot2)
dev.off()





















############add self relevance as a predictor


M4sr <- lme(Cards_flipped ~  Age_e + Context_e + gain_e + loss_e + SelfRelevance_c+
             losscards_e + (Age_e*Context_e) + (Age_e*gain_e)+ 
             (Age_e*loss_e) + (Age_e*losscards_e)+(Age_e*SelfRelevance_c)+
             (Age_e*gain_e*Context_e)+(Age_e*loss_e*Context_e)+(Age_e*losscards_e*Context_e)+(Age_e*SelfRelevance_c*Context_e),
           data=CCT2B, method = "ML", random= ~1|ID)
summary(M4sr)
Anova(M4sr)
AIC(M4sr)## 3-way interactions increase model AIC, use M4f for analysis
anova(M4f, M4sr)# AIC slightly improved, but sig 

emsr<-emmeans(M4sr, ~ SelfRelevance_c*Context_e)
summary(emsr)


#each group with its own panel
qplot(x = Context, y = emmean, data = Hot2BC_ContextEMM, facets = ~Age) +
  geom_errorbar(aes(ymin=lowerCL, ymax=higherCL), width=.1,alpha=0.2,
                position=position_dodge(0.05))+
  plot_theme

##try boxplot


CCT2BC<- read_csv("/Users/delluser/Dropbox/My PC (DESKTOP-DUOC1HI)/Desktop/RPractice/CCT2BC.csv")
View(CCT2BC)
summary(CCT2BC) #Context means collapsed across ID

tiff('CCT2BC.tiff', units = "in", width = 5, height = 3, res = 250)
ggplot(CCT2BC, aes(x = factor(Context), y = Flip)) + 
  geom_boxplot(outlier.shape = NA,position=position_dodge(1), aes(color = Age_Group)) +
  geom_dotplot(position = position_dodge(1),binaxis='y', stackdir='center',stackratio = 0.6, binwidth = .3, alpha = 0.2,  dotsize = .25, aes(fill =Age_Group)) +
  stat_summary(fun=mean, geom="point", shape=18,
               size=3, color="black", aes(fill = Age_Group), position = position_dodge(1))+
  scale_fill_manual(values=c( "snow4","red3")) +
  scale_color_manual(values=c( "snow4","red3")) +
  labs(x = "\nContext",
       y = "Cards Flipped")+
  plot_theme +
  theme(axis.ticks.x = element_blank() ) 
dev.off()




library(cowplot)

plot1<-hotB + theme(legend.position = "none") + xlab(NULL)
plot2<-hotC + ylab(NULL) + xlab(NULL) + theme(legend.position = "none")

legend<- get_legend(hotB)
prow<-plot_grid(plot1, plot2, labels = "AUTO")


tiff('CCT2BC.tiff', units = "in", width = 5, height = 3, res = 250)
plot_grid(plot1, plot2)
dev.off()



YA <- subset(CCT2BRoster, Age_Group=='1',
             select = (1:20)) 
OA <- subset(CCT2BRoster, Age_Group=='2',
             select = (1:20)) 

#Pearson correlations separately for age group for SGCL

cor.test(YA$SGCL, YA$Age, 
         method= "pearson")

cor.test(OA$SGCL, OA$Age, 
         method= "pearson")

cor.test(YA$SGCL, YA$Education, 
         method= "pearson")

cor.test(OA$SGCL, OA$Education, 
         method= "pearson")


cor.test(YA$SGCL, YA$Income, 
         method= "pearson")

cor.test(OA$SGCL, OA$Income, 
         method= "pearson")


cor.test(YA$SGCL, YA$Mill_hill, 
         method= "pearson")

cor.test(OA$SGCL, OA$Mill_hill, 
         method= "pearson")



cor.test(YA$SGCL, YA$PANAS_p, 
         method= "pearson")

cor.test(OA$SGCL, OA$PANAS_p, 
         method= "pearson")


cor.test(YA$SGCL, YA$PANAS_n, 
         method= "pearson")

cor.test(OA$SGCL, OA$PANAS_n, 
         method= "pearson")


cor.test(YA$SGCL, YA$DASS_D, 
         method= "pearson")

cor.test(OA$SGCL, OA$DASS_D, 
         method= "pearson")


cor.test(YA$SGCL, YA$DASS_A, 
         method= "pearson")

cor.test(OA$SGCL, OA$DASS_A, 
         method= "pearson")


cor.test(YA$SGCL, YA$DASS_S, 
         method= "pearson")

cor.test(OA$SGCL, OA$DASS_S, 
         method= "pearson")



cor.test(YA$SGCL, YA$VASSelfRelevance, 
         method= "pearson")

cor.test(OA$SGCL, OA$VASSelfRelevance, 
         method= "pearson")

###correlations for CGSL

cor.test(YA$CGSL, YA$Age, 
         method= "pearson")

cor.test(OA$CGSL, OA$Age, 
         method= "pearson")

cor.test(YA$CGSL, YA$Education, 
         method= "pearson")

cor.test(OA$CGSL, OA$Education, 
         method= "pearson")


cor.test(YA$CGSL, YA$Income, 
         method= "pearson")

cor.test(OA$CGSL, OA$Income, 
         method= "pearson")


cor.test(YA$CGSL, YA$Mill_hill, 
         method= "pearson")

cor.test(OA$CGSL, OA$Mill_hill, 
         method= "pearson")



cor.test(YA$CGSL, YA$PANAS_p, 
         method= "pearson")

cor.test(OA$CGSL, OA$PANAS_p, 
         method= "pearson")


cor.test(YA$CGSL, YA$PANAS_n, 
         method= "pearson")

cor.test(OA$CGSL, OA$PANAS_n, 
         method= "pearson")


cor.test(YA$CGSL, YA$DASS_D, 
         method= "pearson")

cor.test(OA$CGSL, OA$DASS_D, 
         method= "pearson")


cor.test(YA$CGSL, YA$DASS_A, 
         method= "pearson")

cor.test(OA$CGSL, OA$DASS_A, 
         method= "pearson")


cor.test(YA$CGSL, YA$DASS_S, 
         method= "pearson")

cor.test(OA$CGSL, OA$DASS_S, 
         method= "pearson")



cor.test(YA$CGSL, YA$VASSelfRelevance, 
         method= "pearson")

cor.test(OA$CGSL, OA$VASSelfRelevance, 
         method= "pearson")


##Post hoc analysis for defense

#Looking at Gender Differences in Social Risk taking
t.test(Education~ Gender_MF, data = CCT2BRoster)
t.test(Income~ Gender_MF, data = CCT2BRoster)
t.test(CGSL~ Gender_MF, data = CCT2BRoster)
t.test(VASSelfRelevance~ Gender_MF, data = CCT2BRoster)

##checking order effects
test<-aov(SGCL~ Version_CGSLSGCL*Age_Group, data= CCT2BRoster)
summary (test)

test<-aov(CGSL~ Version_CGSLSGCL*Age_Group, data= CCT2BRoster)
summary (test)



