library(mltools)
library(tidyverse)
library(caret)
library(MASS)
library(emmeans)
library(lme4)
library(pbkrtest)
library(ecospat)
library(data.table)
diversity <- read.csv("CaptiveAlphaDiet_reduced.csv")
str(diversity)
diversitytrial<- read.csv("CaptiveAlphaDiet_trialonly.csv")
str(diversitytrial)
diversity$location=as.factor(diversity$location)
diversity$Diet_Type=as.factor(diversity$Diet_Type)
diversity$Pen_Name=as.factor(diversity$Pen_Name)
diversity$DeerSpecies=as.factor(diversity$DeerSpecies)
diversity$Deer_Name=as.factor(diversity$Deer_Name)

##location-----------------------------------------------------------------------------
Shannondiv <- lmer(Shannon ~ DeerSpecies + location + DeerSpecies*location + (1|Deer_Name), 
                 data = diversity)
summary(Shannondiv)
Simpsondiv <- lmer(Simpson ~ DeerSpecies + location + DeerSpecies*location + (1|Deer_Name), 
                   data = diversity)
summary(Simpsondiv)
Richnessdiv <- lmer(Observed ~ DeerSpecies + location + DeerSpecies*location + (1|Deer_Name), 
                    data = diversity)
summary(Richnessdiv)

Shannondiv <- aov(Shannon ~ DeerSpecies + location + DeerSpecies*location , 
                   data = diversity)
summary(Shannondiv)
Simpsondiv <- aov(Simpson ~ DeerSpecies + location + DeerSpecies*location, 
                   data = diversity)
summary(Simpsondiv)
Richnessdiv <- aov(Observed ~ DeerSpecies + location + DeerSpecies*location , 
                    data = diversity)
summary(Richnessdiv)
#pen name trial only-------------------------------------------------------

Shannondiv <- aov(Shannon ~ DeerSpecies + Pen_Name + DeerSpecies*Pen_Name , 
                  data = diversitytrial)
summary(Shannondiv)
Simpsondiv <- aov(Simpson ~ DeerSpecies + Pen_Name + DeerSpecies*Pen_Name, 
                  data = diversitytrial)
summary(Simpsondiv)
Richnessdiv <- aov(Observed ~ DeerSpecies + Pen_Name + DeerSpecies*Pen_Name , 
                   data = diversitytrial)
summary(Richnessdiv)

#subsetting by species and location

diversity_MD <- diversity[diversity$DeerSpecies == "Mule Deer", ]
diversity_WT <- diversity[diversity$DeerSpecies == "White-tailed", ]
diversity_SP <- diversity[diversity$location == "Start Pen", ]
diversity_TP <- diversity[diversity$location == "Trial Pens", ]
diversity_EP <- diversity[diversity$location == "End Pen", ]
diversity_Post <- diversity[diversity$location == "Post Transition", ]
diversity_Pre <- diversity[diversity$location == "Pre Transition", ]

diversityT_MD <- diversitytrial[diversitytrial$DeerSpecies == "Mule Deer", ]
diversityT_WT <- diversitytrial[diversitytrial$DeerSpecies == "White-tailed", ]
str(diversityT_MD)

str(diversity_Pre)

#location by species and location separately----------------------

ShannondivMD <- lm(Shannon ~ location, data = diversity_MD)
summary(ShannondivMD)
SimpsondivMD <- lm(Simpson ~  location, data = diversity_MD)
summary(SimpsondivMD)
RichnessdivMD <- lm(Observed ~ location, data = diversity_MD)
summary(RichnessdivMD)

ShannondivMD <- glm(Shannon ~ location, data = diversity_MD)
summary(ShannondivMD)
SimpsondivMD <- glm(Simpson ~  location, data = diversity_MD)
summary(SimpsondivMD)
RichnessdivMD <- glm(Observed ~ location, data = diversity_MD)
summary(RichnessdivMD)

ShannondivMD <- aov(Shannon ~ location, data = diversity_MD)
summary(ShannondivMD)
SimpsondivMD <- aov(Simpson ~  location, data = diversity_MD)
summary(SimpsondivMD)
RichnessdivMD <- aov(Observed ~ location, data = diversity_MD)
summary(RichnessdivMD)

ShannondivWT <- lm(Shannon ~ location, data = diversity_WT)
summary(ShannondivWT)
SimpsondivWT <- lm(Simpson ~  location, data = diversity_WT)
summary(SimpsondivWT)
RichnessdivWT <- lm(Observed ~ location, data = diversity_WT)
summary(RichnessdivWT)

ShannondivWT <- glm(Shannon ~ location, data = diversity_WT)
summary(ShannondivWT)
SimpsondivWT <- glm(Simpson ~  location, data = diversity_WT)
summary(SimpsondivWT)
RichnessdivWT <- glm(Observed ~ location, data = diversity_WT)
summary(RichnessdivWT)

ShannondivWT <- aov(Shannon ~ location, data = diversity_WT)
summary(ShannondivWT)
SimpsondivWT <- aov(Simpson ~  location, data = diversity_WT)
summary(SimpsondivWT)
RichnessdivWT <- aov(Observed ~ location, data = diversity_WT)
summary(RichnessdivWT)

ShannondivSP <- aov(Shannon ~ DeerSpecies, data = diversity_SP)
summary(ShannondivSP)
SimpsondivSP <- aov(Simpson ~  DeerSpecies, data = diversity_SP)
summary(SimpsondivSP)
RichnessdivSP <- aov(Observed ~ DeerSpecies, data = diversity_SP)
summary(RichnessdivSP)

ShannondivTP <- aov(Shannon ~ DeerSpecies, data = diversity_TP)
summary(ShannondivTP)
SimpsondivTP <- aov(Simpson ~  DeerSpecies, data = diversity_TP)
summary(SimpsondivSP)
RichnessdivTP <- aov(Observed ~ DeerSpecies, data = diversity_TP)
summary(RichnessdivTP)

ShannondivEP <- aov(Shannon ~ DeerSpecies, data = diversity_EP)
summary(ShannondivEP)
SimpsondivEP <- aov(Simpson ~  DeerSpecies, data = diversity_EP)
summary(SimpsondivEP)
RichnessdivEP <- aov(Observed ~ DeerSpecies, data = diversity_EP)
summary(RichnessdivEP)

ShannondivPost <- aov(Shannon ~ DeerSpecies, data = diversity_Post)
summary(ShannondivPost)
SimpsondivPost <- aov(Simpson ~  DeerSpecies, data = diversity_Post)
summary(SimpsondivPost)
RichnessdivPost <- aov(Observed ~ DeerSpecies, data = diversity_Post)
summary(RichnessdivPost)

ShannondivPre <- aov(Shannon ~ DeerSpecies, data = diversity_Pre)
summary(ShannondivPre)
SimpsondivPre <- aov(Simpson ~  DeerSpecies, data = diversity_Pre)
summary(SimpsondivPre)
RichnessdivPre <- aov(Observed ~ DeerSpecies, data = diversity_Pre)
summary(RichnessdivPre)

#diet comp----------------------------------------------------------------------------------------

Shannondiv <- lm(Shannon ~ DeerSpecies + grass + DeerSpecies*grass , 
                   data = diversity)
summary(Shannondiv)
Simpsondiv <- lm(Simpson ~ DeerSpecies + grass + DeerSpecies*grass , 
                   data = diversity)
summary(Simpsondiv)
Richnessdiv <- lm(Observed ~ DeerSpecies + grass + DeerSpecies*grass , 
                    data = diversity)
summary(Richnessdiv)

#diet comp for trial only

Shannondiv <- lm(Shannon ~ DeerSpecies + grass + DeerSpecies*grass , 
                  data = diversitytrial)
summary(Shannondiv)
Simpsondiv <- aov(Simpson ~ DeerSpecies + grass + DeerSpecies*grass, 
                  data = diversitytrial)
summary(Simpsondiv)
Richnessdiv <- aov(Observed ~ DeerSpecies + conifer + DeerSpecies*conifer , 
                   data = diversitytrial)
summary(Richnessdiv)

#diet comp by species----------------------------------------------------
ShannondivWT <- lm(Shannon ~  conifer , 
                   data = diversity_WT)
summary(ShannondivWT)
SimpsondivWT <- lm(Simpson ~ conifer , 
                   data = diversity_WT)
summary(SimpsondivWT)
RichnessdivWT <- lm(Observed ~ conifer  , 
                    data = diversity_WT)
summary(RichnessdivWT)

ShannondivMD <- lm(Shannon ~  conifer , 
                   data = diversity_MD)
summary(ShannondivMD)
SimpsondivMD <- lm(Simpson ~  conifer , 
                   data = diversity_MD)
summary(SimpsondivMD)
RichnessdivMD <- lm(Observed ~ conifer  , 
                    data = diversity_MD)
summary(RichnessdivMD)

#diet comp trial only by species-----------------------------------------
ShannondivWT <- lm(Shannon ~  conifer , 
                   data = diversityT_WT)
summary(ShannondivWT)
SimpsondivWT <- lm(Simpson ~  conifer , 
                   data = diversityT_WT)
summary(SimpsondivWT)
RichnessdivWT <- lm(Observed ~ conifer  , 
                    data = diversityT_WT)
summary(RichnessdivWT)

ShannondivMD <- lm(Shannon ~  conifer , 
                   data = diversityT_MD)
summary(ShannondivMD)
SimpsondivMD <- lm(Simpson ~  conifer , 
                   data = diversityT_MD)
summary(SimpsondivMD)
RichnessdivMD <- lm(Observed ~ conifer  , 
                    data = diversityT_MD)
summary(RichnessdivMD)

#nutrition----------------------------------------------------------------

Shannondiv <- lm(Shannon ~ DeerSpecies + NDF + DeerSpecies*NDF , 
                 data = diversity)
summary(Shannondiv)
Simpsondiv <- lm(Simpson ~ DeerSpecies + NDF + DeerSpecies*NDF , 
                 data = diversity)
summary(Simpsondiv)
Richnessdiv <- lm(Observed ~ DeerSpecies + NDF + DeerSpecies*NDF , 
                  data = diversity)
summary(Richnessdiv)
#nutrition by species-----------------------------------------------------
ShannondivWT <- lm(Shannon ~  Tannins , 
                 data = diversity_WT)
summary(ShannondivWT)
SimpsondivWT <- lm(Simpson ~  Tannins , 
                 data = diversity_WT)
summary(SimpsondivWT)
RichnessdivWT <- lm(Observed ~ Tannins  , 
                  data = diversity_WT)
summary(RichnessdivWT)

ShannondivMD <- lm(Shannon ~  Tannins , 
                   data = diversity_MD)
summary(ShannondivMD)
SimpsondivMD <- lm(Simpson ~  Tannins , 
                   data = diversity_MD)
summary(SimpsondivMD)
RichnessdivMD <- lm(Observed ~ Tannins  , 
                    data = diversity_MD)
summary(RichnessdivMD)
#nutrition by trial only-------------------------------------------------------
Shannondiv <- lm(Shannon ~ DeerSpecies + NDF + DeerSpecies*NDF , 
                 data = diversitytrial)
summary(Shannondiv)
Simpsondiv <- lm(Simpson ~ DeerSpecies + NDF + DeerSpecies*NDF , 
                 data = diversitytrial)
summary(Simpsondiv)
Richnessdiv <- lm(Observed ~ DeerSpecies + NDF + DeerSpecies*NDF , 
                  data = diversitytrial)
summary(Richnessdiv)


