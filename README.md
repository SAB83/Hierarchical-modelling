# Hierarchical-modelling
Analyzing fledge date and colony departure date of migratory birds

library(tidyverse)
library(car)
library(lme4)         
library(plyr)
library(MuMIn)
library(lmerTest)
library(ggpubr)
library(ggplot2)
library(hrbrthemes)

#Normality test..................................................................

d <- data.frame(residuals = residuals(m),    # Residuals
                std_residuals = rstudent(m), # Studentized Residuals
                fitted = fitted(m),          # Fitted values
                cooks = cooks.distance(m))   # Cook's D
d <- mutate(d, observation = 1:nrow(d))          # Observation number

#Histogram of residuals
ggplot(data = d, aes(x = std_residuals)) +
  geom_histogram(bins = 30)

# Check for normality

ggplot(data = d, aes(sample = std_residuals)) +
  stat_qq() +
  stat_qq_line()

# Check for constant variance (no heteroscedasticity)

ggplot(data = d, aes(x = fitted, y = std_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0)

# Cook's D

ggplot(d, aes(x = observation, y = cooks)) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_hline(yintercept = 4/nrow(d), linetype = "dashed")

#-------------------------------------------------------------------
#CI estimation:

(coeftbl <- as.data.frame(coef(summary(m))))
with(coeftbl,
     Estimate + outer(`Std. Error`, c(lower=-1, upper=1)) * sqrt(qchisq(0.95, 1)))


#-------------------------------------------------------------
#read data

data <- read.csv("/home/name/262-2/data.csv")

##factoring-----------##factoring-----------

data$cavity<-as.factor(data$cavity)
levels(data$cavity)

data$t<-as.factor(data$t)
levels(data$t)

data$treatment<-as.factor(data$treatment)
levels(data$treatment)
data$treatment<-revalue(data$treatment, c("C"="Control", "LE"="Light Extension"))
# Models ----------------------------------------------------------------------------------------

#days spent in the nest:

data <- filter(data, !is.na(nd)) 

m <- lmer( nd ~ t+fed+ w4+  fat+ nestlings+  (1|cavity), data = data,REML=FALSE, na.action ="na.fail")

m1 <- lm( nd ~ t+ w4+ fed+ fat+ nestlings, data = data, na.action ="na.fail")

anova(m,m1)
coef
F<- dredge(m)
F
vif(m)

summary(m)
AIC(m)

#----------------------------------------------------------------------------------
# duration in the colony:

data2 <- filter(data, !is.na(dcp)) 

m <- lmer( dcp ~ t+fed+ nestlings+ (1|cavity), data = data2, REML=FALSE, na.action ="na.fail")

coef
F<- dredge(m)
F
summary(m)
vif(m)


#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#fledge date######fledge datedate######fledge datedate######fledge datedate######fledge date

data2 <- filter(data, !is.na(fld)) 

data2 <- data2[-c(41,67), ]

m <- lmer( fld ~ t+fed+ w4+ fat+ nestlings+ (1|cavity), data = data2,REML=FALSE, na.action ="na.fail")

F<- dredge(m)
F
summary(m)
vif(m)


#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

#colony departure date######colony departure date######colony departure date

data3 <- filter(data, !is.na(cd))

data3 <- data3[-c(78,66,5), ]

m <- lmer( cd ~ t+fed+ nestlings+ (1|cavity), data = data3,REML=FALSE, na.action ="na.fail")
F<- dredge(m)
F
summary(m)


####################################################################################
#plot ######plot######plot#####plot######plot#####plot######plot#####plot###########
#plot ######plot######plot#####plot######plot#####plot######plot#####plot###########
#plot ######plot######plot#####plot######plot#####plot######plot#####plot###########

library(hrbrthemes)
library(ggplot2)

#cd####cd####cd####cd####

ggplot(data = data, aes(x= fed,y= cd, col = treatment))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size = 3,alpha= .8)+
  geom_smooth(method = lm, se = FALSE, size= 1,alpha = .8)+
  theme_bw()+
  ylab("Colony departure date (Jan 1=1)")+
  xlab("First egg date (Jan 1=1)")+ scale_color_manual(values=c("purple", "black"))+
  theme(text = element_text(size=rel(5)),
        strip.text.x = element_text(size=rel(4)),
        strip.text.y = element_text(size=rel(4)))


#agedc & fed####agedc & fed####agedc & fed###

ggplot(data = data, aes(x= fed,y= agedc, col = treatment))+
  viridis::scale_color_viridis(discrete = TRUE)+
  geom_point(size = 3,alpha= .8)+
  geom_smooth(method = lm, se = FALSE, size= 1,alpha = .8)+
  theme_bw()+
  ylab("Colony departure date (Jan 1=1)")+
  xlab("First egg date (Jan 1=1)")+ scale_color_manual(values=c("purple", "black"))+
  theme(text = element_text(size=rel(5)),
        strip.text.x = element_text(size=rel(4)),
        strip.text.y = element_text(size=rel(4)))


#correlation####correlation####correlation####correlation###

ggscatter(data, x =  "w4", y = "fld", cor.coef = TRUE,
          xlab = "First egg date (Jan 1=1)", ylab = "Fledge date (Jan 1=1)",
          cor.method = "pearson")+
  geom_smooth(method=lm , color="red", fill="grey", se=TRUE)+
  theme_bw()

#fld####fld####fld####fld####fld####fld####fld####fld####

ggplot(data = data2, aes(x= fed,y= fld, col = treatment))+
  viridis::scale_color_viridis(discrete = TRUE)+ 
  geom_point(size = 3,alpha= .8)+
  geom_smooth(method = lm, se = FALSE, size= 1,alpha = .8)+
  theme_bw()+
  ylab("Fledge date (Jan 1=1)")+
  xlab("First egg date (Jan 1=1)")+ scale_color_manual(values=c("purple", "black"))+
  theme(text = element_text(size=rel(5)),
        strip.text.x = element_text(size=rel(4)),
        strip.text.y = element_text(size=rel(4))) 

#box plot####box plot####box plot####box plot

library(viridis)

ggplot(data, aes (x=treatment,y=nd, fill= treatment),  na.rm = TRUE)+
  geom_boxplot()+ ylab("Duration in the nest (days)")+
  xlab("Treatment")+
  scale_fill_manual(values=c("steelblue2", "orchid2"))+
  stat_summary(fun.y = mean, geom = "point", size = 2, color = "black")+theme_bw()+
  #scale_color_manual(values=c("#8F4407", "#873F07"))+  
  theme(text = element_text(size=rel(5)),
        strip.text.x = element_text(size=rel(4)),
        strip.text.y = element_text(size=rel(4)))


ggplot(data2, aes (x=treatment,y=dcp, fill = treatment),  na.rm = TRUE)+
  geom_boxplot()+ ylab("Period of being at the colony")+
  xlab("Treatment")+
  scale_fill_manual(values=c("steelblue2", "orchid2"))+
  stat_summary(fun.y = mean, geom = "point", size = 2, color = "black")+theme_bw()+
  #scale_color_manual(values=c("black", "purple"))+
  theme(text = element_text(size=rel(5)),
        strip.text.x = element_text(size=rel(4)),
        strip.text.y = element_text(size=rel(4)))

ggplot(data, aes (x=treatment,y=fld, color = treatment),  na.rm = TRUE)+
  geom_boxplot()+ ylab("Fledge date")+
  xlab("Treatment")+
  scale_fill_brewer(palette ="chartreuse",direction = -1)+
  stat_summary(fun.y = mean, geom = "point", size = 2, color = "black")+theme_bw()


