library(dplyr)
library(lme4)
library(car)
library(glmmTMB)
library(emmeans)
library(ggthemes)
library(DHARMa)
library(ggplot2)
library(tidyverse)
library(nadiv)
library(broom.mixed)
library(AICcmodavg)
library(formattable)
library(gridExtra)
library(sjPlot)
library(rstatix)

### BLUP ###
# # Boldness represented by cross
# setwd("C:/Users/Alais/Desktop/Personality/Statistics")
# pers <- read.csv2("short_2.csv", sep = ";")
# 
# blup <- glmmTMB(cross ~ scale(openfield_no, scale=FALSE) +
#                   scale(mass) + sex +
#                   (1|eartag),
#                 data = pers,
#                 ziformula = ~1,
#                 family = nbinom1)
# 
# plot(blup)
# summary(blup)
# 
# # Wyci?gniecie jednego blupa dla jednego eartaga
# blup_2 <- ranef(blup)
# blup_2$cond$eartag
# 
# df_BLUPS_B <- data_frame(eartag = row.names(blup_2$cond$eartag),
#                          BLUP_B = blup_2$cond$eartag[,"(Intercept)"])
# df_BLUPS_B


# Data for BLUP in Biomark file
# vv <- merge(dane, df_BLUPS_B, by.eartag = by)
# head(vv)
# 
# write.csv2(vv,file="BIOMARK-BLUP_2.csv")


setwd("C:/Users/Użytkownik/Desktop/Personality/Statistics")
dane <- read.csv2("BIOMARK-BLUP_2.csv", sep = ";")
head(dane)


dane <- dane[order(dane$year_time),] 
dane$Year <- as.character(dane$year_time) # R produkuje same warto?ci NaN przy normalnych warto?ciach year. 
# Poniewa? rok jest tylko poziomem i nie s? wa?ne dok?adne warto?ci, 
# rok zosta? zamieniony na warto?? tekstow?


###   Data exploration   ###

# Number of observations per year
table(dane$year_time)

#Number of observations per plot
table(dane$plot)

#number of individuals
a<-unique(dane$eartag)
length(a)
#196

#outliers?
dotchart(dane$Exploration_rate) #yes
dotchart(dane$Body_mass) #no
dotchart(dane$distance) #yes


#shape of distance
hist(dane$distance)
hist(log(dane$distance))
hist(sqrt(dane$distance))


# RELATIONSHIPS?
plot(x = dane$Exploration_rate, 
     y = dane$dispersal_distance,
     pch = 16, cex = 0.5, 
     ylab = "Seed dispersal distance (cm)",
     xlab = "Crossings")

plot(x = dane$cross, 
     y = dane$dispersal_distance,
     pch = 16, cex = 0.5, 
     ylab = "Seed dispersal distance (cm)",
     xlab = "Crossings")

plot(x = dane$Body_mass, 
     y = dane$dispersal_distance,
     pch = 16, cex = 0.5, 
     ylab = "Seed dispersal distance (cm)",
     xlab = "Body mass")


#sex effect
boxplot(dane$Exploration_rate ~ Sex + Year,
        col=c("coral1","cadetblue2"),
        varwidth = TRUE, 
        data = dane,
        frame = FALSE,
        xlab     = "Sex",
        ylab     = "Number of crosses")


boxplot(dane$Exploration_rate ~ Sex + Year,
        col=c("coral1","cadetblue2"),
        varwidth = TRUE, 
        data = dane,
        frame = FALSE,
        xlab     = "Sex",
        ylab     = "Cross BLUP")


#sex x year effect
boxplot(dane$dispersal_distance ~ Sex*Year,
        varwidth = TRUE, 
        data = dane,
        xlab     = "sex",
        ylab     = "Seed dispersal distance")
abline(h = mean(dane$dispersal_distance, na.rm = TRUE), 
       lty = 2)


#Plot effect
boxplot(dane$Exploration_rate ~ plot,
        varwidth = TRUE, 
        data = dane,
        xlab     = "Plot",
        ylab     = "Crossings")
abline(h = mean(dane$Exploration_rate, na.rm = TRUE), 
       lty = 2)

boxplot(dane$Exploration_rate ~ plot,
        varwidth = TRUE, 
        data = dane,
        xlab     = "Plot",
        ylab     = "Crossings")
abline(h = mean(dane$Exploration_rate, na.rm = TRUE), 
       lty = 2)

boxplot(dane$dispersal_distance ~ plot,
        varwidth = TRUE, 
        data = dane,
        xlab     = "Plot",
        ylab     = "Seed dispersal distance")
abline(h = mean(dane$dispersal_distance, na.rm = TRUE), 
       lty = 2)

boxplot(dane$Body_mass ~ plot,
        varwidth = TRUE, 
        data = dane,
        xlab     = "Plot",
        ylab     = "Body mass (g)")
abline(h = mean(dane$Body_mass, na.rm = TRUE), 
       lty = 2)

# Is it possible that the distance - crossings relationship
# changes by year?
p <- ggplot()
p <- p + geom_point(data = dane, 
                    aes(y = distance, x = Exploration_rate),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Cross BLUP") + 
  ylab("Seed dispersal distance (cm)")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(data = dane, 
                     aes(x = Exploration_rate, y = dispersal_distance), method = "lm")
p <- p + facet_wrap( ~ Year, scales = "fixed")
p


# Is it possible that the distance - body mass relationship
# changes by year?
p <- ggplot()
p <- p + geom_point(data = dane, 
                    aes(y = dispersal_distance, x = Body_mass),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Body mass (g)") + 
  ylab("Seed dispersal distance (cm)")
p <- p + theme(text = element_text(size=15))
p <- p + geom_smooth(data = dane, 
                     aes(x = Body_mass, y = dispersal_distance), method = "lm")
p <- p + facet_wrap( ~ Year, scales = "fixed")
p




### Individual variation ###

summary(dane)
head(dane)

### DYSTANS wszytskie lata###

hist(dane$dispersal_distance)
hist(sqrt(dane$dispersal_distance))

distance1<-glmmTMB(sqrt(dispersal_distance)~Exploration_rate*Year+Sex*Year+Body_mass*Year
                   +(1|eartag)+ (1|plot) + (1|station),data=dane)
summary(distance1)
Anova(distance1) 

distance2<-glmmTMB(sqrt(dispersal_distance)~Exploration_rate+Body_mass*Year+Sex*Year+
                     (1|eartag)+ (1|plot)+(1|station),data=dane)
summary(distance2)
Anova(distance2)


dane$Year<-relevel(factor(dane$Year), ref="2020")

distance3<-glmmTMB(sqrt(dispersal_distance)~Body_mass*Year+Exploration_rate+Sex2+
                     (1|eartag)+ (1|plot) + (1|station),data=dane)
summary(distance3)
Anova(distance3)

tab_model(distance3,  auto.label = T, show.re.var=F, show.ngroups = F,
          pred.labels = c("Intercept", "Mass:year", "Cross (BLUP)", "Sex"),
          dv.labels = c("Distance of dispersal from the station"))

tab_model(distance3)


# WYKRES wp?yw masy cia?a na odleg?o?? wynoszenia od szalki #

lista <- list(Body_mass= seq(min(10), max(51), by = 1))


d2 <- glmmTMB(sqrt(dispersal_distance)~Body_mass*Year+Exploration_rate+Sex2+
                (1|eartag)+ (1|plot) + (1|station),data=dane)

Anova(d2)

mm2 <- emmip(
  d2,
  ~Body_mass * Year,
  at = lista,
  type="response",
  CIs=TRUE,
  plotit = FALSE
)


plot_distance2 <- ggplot() + 
  geom_line(data=mm2, aes(y=yvar, x=Body_mass, color=Year, linetype=Year), alpha=0.7, linewidth=2) + 
  scale_linetype_manual(values=c("dashed", "dashed", "solid")) +
  theme_classic() + 
  geom_ribbon(data=mm2, aes(ymin = LCL, ymax = UCL, x=Body_mass, fill=Year), alpha=0.1) + 
  ylab("Seed dispercal distance [m]") +xlab("Body mass [g]") + 
  theme(text=element_text(size=14))+
  scale_color_manual(values = c("mediumpurple3","orange2","grey30"))+
  scale_fill_manual(values = c("mediumpurple3","orange2","grey30"))


plot_distance2
plot_distance2<- plot_distance2+ theme(legend.position = "none")



### CACHING ###

dane$Year<-relevel(factor(dane$Year), ref="2020")

caching1<-glmmTMB(cached~Exploration_rate*Year+Sex*Year+scale(Body_mass)*Year
                  +(1|eartag)+ (1|plot) + (1|station),family=binomial,data=dane)
summary(caching1)
Anova(caching1) 


tab_model(caching1,  auto.label = T, show.re.var=F, show.ngroups = F,
          pred.labels = c("Intercept", "Mass:year", "BLUP", "Sex"),
          dv.labels = c("Probability of caching"))


### WYKRES interakcja exploration rate*year ###


c1 <- glmmTMB(cached~Exploration_rate*Year+Sex*Year+Body_mass*Year
              +(1|eartag)+ (1|plot) + (1|station),family=binomial,data=dane)

lista <- list(Exploration_rate= seq(min(dane$Exploration_rate), max(dane$Exploration_rate), by = 0.1))

mm <- emmip(
  c1,
  ~Exploration_rate * Year,
  at = lista,
  type="response",
  CIs=TRUE,
  plotit = FALSE
)

mm$cached <- mm$yvar

plot_cached <- ggplot() + 
  geom_line(data=mm, aes(y=cached, x=Exploration_rate, color=Year, linetype=Year), alpha=0.7, size=2) + 
  scale_linetype_manual(values=c("dashed", "solid", "dashed")) +
  theme_classic() + 
  geom_ribbon(data=mm, aes(ymin = LCL, ymax = UCL, x=Exploration_rate, fill=Year), alpha=0.1) + 
  ylab("Probability of cachineg") +xlab("Exploration rate") + 
  theme(text=element_text(size=14))+
  scale_color_manual(values = c("mediumpurple3","orange2","grey30"))+
  scale_fill_manual(values = c("mediumpurple3","orange2","grey30"))
plot_cached

plot_cached <- plot_cached+ theme(legend.position = "none")



### WYKRES wp?yw masy cia?a na caching ###


c2 <- glmmTMB(cached~Exploration_rate*Year+Sex*Year+Body_mass*Year
              +(1|eartag)+ (1|plot) + (1|station),family=binomial,data=dane)


lista <- list(Body_mass= seq(min(10), max(51), by = 1))

mm2 <- emmip(
  c2,
  ~Body_mass * Year,
  at = lista,
  type="response",
  CIs=TRUE,
  plotit = FALSE
)

mm2$cached <- mm2$yvar


plot_cached2 <- ggplot() + 
  geom_line(data=mm2, aes(y=cached, x=Body_mass, color=Year,linetype=Year), alpha=0.7, size=2) + 
  theme_classic() + 
  scale_linetype_manual(values=c("solid", "solid", "solid")) +
  geom_ribbon(data=mm2, aes(ymin = LCL, ymax = UCL, x=Body_mass, fill=Year), alpha=0.1) + 
  ylab("Probability of cachineg") +xlab("Body mass [g]") + 
  theme(text=element_text(size=14))+
  scale_color_manual(values = c("mediumpurple3","orange2","grey30"))+
  scale_fill_manual(values = c("mediumpurple3","orange2","grey30"))
plot_cached2
plot_cached2<- plot_cached2+ theme(legend.position = "none")


### plot sex and caching

c3<- glmmTMB(cached~Exploration_rate*Year+Sex*Year+Body_mass*Year
             +(1|eartag)+ (1|plot) + (1|station),family=binomial,data=dane)

mm<-emmip(c3,~Sex*Year,type="response", CIs=T, plotit=F)

logit2prob<-function(logit){
  odds<-exp(logit)
  prob<-odds/(1+odds)
  return(prob)
}

mm$cached <- mm$yvar
mm$LCLt <- mm$LCL
mm$UCLt <- mm$UCL

plot_c3 <- ggplot() + 
  theme_classic() + 
  geom_col(data=mm, aes(y=cached, x=Year, fill=Sex), position="dodge", linewidth=2)


plot_c3<-plot_c3+geom_errorbar(data=mm, aes(ymin = LCLt, ymax = UCLt, x=Year, group=Sex), position=position_dodge(0.9), width=0.2) + 
  ylab("Probability of caching") +xlab("Year") +
  scale_fill_manual(values = c("lightpink3","lightskyblue2")) +
  theme(text=element_text(size=15))


plot_c3

### CONSUMPTION ###

dane$Year<-relevel(factor(dane$Year), ref="2020")

eating1<-glmmTMB(eaten~Exploration_rate*Year+Sex*Year+Body_mass*Year
                  +(1|eartag)+ (1|plot)+(1|station), family=binomial,data=dane)
summary(eating1)
Anova(eating1)



tab_model(eating1,  auto.label = T, show.re.var=F, show.ngroups = F,
          pred.labels = c("Intercept", "Cross (BLUP):year", "Mass", "Sex"),
          dv.labels = c("Probability of consumption"))



### wykres consumption i exploration rate ###


e1 <- glmmTMB(eaten~Exploration_rate*Year+Sex*Year+Body_mass*Year
              +(1|eartag)+ (1|plot)+(1|station),data=dane)


lista <- list(Exploration_rate= seq(min(dane$Exploration_rate), max(dane$Exploration_rate), by = 0.1))

mm <- emmip(
  e1,
  ~Exploration_rate * Year,
  at = lista,
  type="response",
  CIs=TRUE,
  plotit = FALSE
)

mm$eaten <- mm$yvar

plot_eaten <- ggplot() + 
  geom_line(data=mm, aes(y=eaten, x=Exploration_rate, color=Year,linetype=Year), alpha=0.7, size=2) + 
  scale_linetype_manual(values=c("dashed", "solid", "dashed"))+
  theme_classic() + 
  geom_ribbon(data=mm, aes(ymin = LCL, ymax = UCL, x=Exploration_rate, fill=Year), alpha=0.1) + 
  ylab("Probability of consumption") +xlab("Exploration rate") + 
  theme(text=element_text(size=14))+
  scale_color_manual(values = c("mediumpurple3","orange2","grey30"))+
  scale_fill_manual(values = c("mediumpurple3","orange2","grey30"))
plot_eaten

plot_eaten <- plot_eaten + theme(legend.position = "none")



# Wyres consumption and sex


e2<- glmmTMB(eaten~Exploration_rate*Year+Sex*Year+Body_mass*Year
             +(1|eartag)+ (1|plot)+(1|station),data=dane)

mm<-emmip(e2,~Sex*Year,type="response", CIs=T, plotit=F)

logit2prob<-function(logit){
  odds<-exp(logit)
  prob<-odds/(1+odds)
  return(prob)
}

mm$eaten <- mm$yvar
mm$LCLt <- mm$LCL
mm$UCLt <- mm$UCL

plot_e2 <- ggplot() + 
  theme_classic() + 
  geom_col(data=mm, aes(y=eaten, x=Year, fill=Sex), position="dodge", size=2)


plot_e2<-plot_e2+geom_errorbar(data=mm, aes(ymin = LCLt, ymax = UCLt, x=Year, group=Sex), position=position_dodge(0.9), width=0.2) + 
  ylab("Probability of consumption") +xlab("Year") +
  scale_fill_manual(values = c("lightpink3","lightskyblue2")) +
  theme(text=element_text(size=15))


plot_e2



### DISTANCE FROM THE TREE ###

hist(dane$dispersal_tree)
hist(log(dane$dispersal_tree))
hist(sqrt(dane$dispersal_tree))

dane$scaled.mass<-scale(dane$Body_mass)

tree1<-glmmTMB(sqrt(dispersal_tree)~Exploration_rate*Year+Sex*Year+scaled.mass*Year
                 +(1|eartag)+(1|plot)+ (1|station),data=dane)
summary(tree1)
Anova(tree1)


tree2<-glmmTMB(sqrt(dispersal_tree)~Sex*Year+scaled.mass*Year + Exploration_rate
               +(1|eartag)+ (1|plot)+(1|station),data=dane)
summary(tree2)
Anova(tree2)

dane$Year<-relevel(factor(dane$Year), ref="2020")

tree3<-glmmTMB(sqrt(dispersal_tree)~scale(Body_mass)*Year +Sex+ Exploration_rate
               +(1|eartag)+ (1|station),data=dane)
summary(tree3)
Anova(tree3)

 
tab_model(tree3,  auto.label = T, show.re.var=F, show.ngroups = F,
          pred.labels = c("Intercept", "Sex:year", "Mass:year", "BLUP"),
          dv.labels = c("Distance of dispersal from the nearest tree"))


# Wykres interakcja Body mass*Year

t2 <- glmmTMB(sqrt(dispersal_tree)~scale(Body_mass)*Year +Sex+ Exploration_rate
              +(1|eartag)+ (1|station),data=dane)


lista2 <- list(Body_mass= seq(min(10), max(51), by = 1))

mm2 <- emmip(
  t2,
  ~Body_mass * Year,
  at = lista2,
  type="response",
  CIs=TRUE,
  plotit = FALSE
)


plot_tree2 <- ggplot() + 
  geom_line(data=mm2, aes(y=yvar, x=Body_mass, color=Year,linetype=Year), alpha=0.7, size=2) + 
  theme_classic() + 
  scale_linetype_manual(values=c("dashed", "dashed", "solid"))+
  geom_ribbon(data=mm2, aes(ymin = LCL, ymax = UCL, x=Body_mass, fill=Year), alpha=0.1) + 
  ylab("Distance from nearest tree [m]") +xlab("Body mass [g]") + 
  theme(text=element_text(size=14))+
  scale_color_manual(values = c("mediumpurple3","orange2","grey30"))+
  scale_fill_manual(values = c("mediumpurple3","orange2","grey30"))

plot_tree2

plot_tree2 <- plot_tree2 + theme(legend.position = "none")
# plot_tree<-plot_tree + scale_fill_colorblind(name = "year", labels = c("2020", "2021", "2022"))
# scale_color_colorblind(name = "Year", labels = c("2020", "2021", "2022"))




# Plots all

grid.arrange(plot_eaten, plot_cached2,plot_cached, plot_c3,plot_distance2,plot_tree2,
             nrow=2, ncol=3)

grid.arrange(plot_tree, plot_tree2, nrow=1, ncol=2 )



### Odleg?o?? od szalki a odleg?o?c od drzewa

DistanceFromStation<-dane$distance
DistanceFromTree<-dane$distance_tree

cor(DistanceFromStation,DistanceFromTree,  method = "pearson", use = "complete.obs")

plot(DistanceFromStation,DistanceFromTree, pch = 19, col = "lightblue")
abline(lm(DistanceFromTree ~ DistanceFromStation), col = "red", lwd = 3)

CachingAll <- dane$cached
cor(DistanceFromTree,CachingAll,  method = "pearson", use = "complete.obs")


### Zwi?zek mi?dzy crosss a sex i body mass ###

# 
# setwd("C:/Users/Alais/Desktop/Personality/Statistics")
# trapping <- read.csv2("TRAPPING-sorted3.csv", sep = ";")
# head(trapping)
# 
# blup <- glmmTMB(cross ~ scale(openfield_no, scale=FALSE) +
#                   scale(mass) + sex +
#                   (1|eartag),
#                 data = trapping,
#                 ziformula = ~1,
#                 family = nbinom1)
# 
# blup_2 <- ranef(blup)
# blup_2$cond$eartag
# 
# df_BLUPS_B <- tibble(eartag = row.names(blup_2$cond$eartag),
#                          BLUP_B = blup_2$cond$eartag[,"(Intercept)"])
# df_BLUPS_B
# 
# 
# vv <- merge(trapping, df_BLUPS_B, by.eartag = by)
# head(vv)
# 
# write.csv2(vv,file="Trapping_BLUP.csv")

setwd("C:/Users/Użytkownik/Desktop/Personality/Statistics")
trapping <- read.csv2("Trapping_BLUP.csv", sep = ";")
head(trapping)
summary (trapping)

trapping <- trapping[order(trapping$year_time),] 
trapping$Year <- as.character(trapping$year_time)

t.test(Body_mass~Sex, trapping) # tak

t.test(Cross~Sex, trapping) # nie


# Body mass and sex visual #

library("plyr")
mass_sex <- ddply(trapping, "sex", summarise, grp.mean=mean(Body_mass))


ggplot(trapping,aes(x=Sex, y=Body_mass,color=Sex))+geom_boxplot()+
  facet_wrap(~Year)+theme_classic()+
  geom_jitter(trapping, mapping=aes(x=Sex, y=Body_mass, color=Sex), size=1)+
  labs(color="Sex")

# cross and mass and sex visual #

ggplot(trapping, aes(fill=Sex, y=cross, x=Body_mass)) + 
  geom_bar(position="dodge", stat="identity")


cross_sex_mass<-glmmTMB(Cross~scale(Body_mass) + Sex + Openfield_number*Year + (1|plot)+(1|eartag),
                        ziformula=~1, data=trapping, family = nbinom2)

summary(cross_sex_mass)
Anova(cross_sex_mass)

lista <- list(Body_mass= seq(min(trapping$Body_mass), max(trapping$Body_mass), by = 1))
mm <- emmip(
  cross_sex_mass,
  ~Body_mass + Sex,
  at = lista,
  type="response",
  CIs=TRUE,
  plotit = FALSE
)

plot_cross_mass_sex <- ggplot() + 
  geom_line(data=mm, aes(y=yvar, x=Body_mass, color=Sex), alpha=0.7, size=2) + 
  theme_classic() + 
  geom_ribbon(data=mm, aes(ymin = LCL, ymax = UCL, x=Body_mass, fill=Sex), alpha=0.1) + 
  labs(fill="Sex", colour="Sex") +
  ylim(0,NA) +
  ylab("Crossings") +xlab("Body mass [g]") + 
  scale_linetype_manual(values=c("solid","dashed")) +
  theme(text=element_text(size=14))

plot_cross_mass_sex



### DISTANCE AND REPRODUCTIVE STATUS ###

males<-glmmTMB(sqrt(dispersal_distance)~sex2 + Body_mass + Exploration_rate + Year
               +(1|eartag)+(1|plot)+ (1|station),data=dane)
summary(males)
Anova(males)



# ABUNDANCE #

# Distance #



distance_abu3<-glmmTMB(sqrt(dispersal_distance)~Body_mass*Abundance+Exploration_rate+Sex2+
                         +(1|eartag)+ (1|plot) + (1|station),data=dane)
summary(distance_abu3)
Anova(distance_abu3)


tab_model(distance_abu3,  auto.label = T, show.re.var=F, show.ngroups = F,
          pred.labels = c("Intercept", "Exploration rate: Abundance", "Mass", "Sex"),
          dv.labels = c("Probability of consumption"))


# caching #


caching_abu1<-glmmTMB(cached~Exploration_rate*Abundance+Sex*Abundance+scale(Body_mass)*Abundance
                      +(1|eartag)+ (1|plot) + (1|station),family=binomial,data=dane)
summary(caching_abu1)
Anova(caching_abu1) 



tab_model(caching_abu3,  auto.label = T, show.re.var=F, show.ngroups = F,
          pred.labels = c("Intercept", "Exploration rate: Abundance", "Mass", "Sex"),
          dv.labels = c("Probability of consumption"))


# Consumption #


eating_abu1<-glmmTMB(eaten~Exploration_rate*Abundance+Sex*Abundance+Body_mass*Abundance
                 +(1|eartag)+ (1|plot)+(1|station), family = binomial, data=dane)
summary(eating_abu1)
Anova(eating_abu1)


eating_abu2<-glmmTMB(eaten~Exploration_rate*Abundance+Sex+Body_mass*Abundance
                     +(1|eartag)+ (1|plot)+(1|station), family = binomial, data=dane)
summary(eating_abu2)
Anova(eating_abu2)


eating_abu3<-glmmTMB(eaten~Exploration_rate*Abundance+Body_mass+Sex
                 +(1|eartag)+ (1|plot)+ (1|station), family = binomial,data=dane)
summary(eating_abu3)
Anova(eating_abu3)

tab_model(eating_abu3,  auto.label = T, show.re.var=F, show.ngroups = F,
          pred.labels = c("Intercept", "Exploration rate: Abundance", "Mass", "Sex"),
          dv.labels = c("Probability of consumption"))



# Distance tree #


tree_abu1<-glmmTMB(sqrt(dispersal_tree)~Exploration_rate*Abundance+Sex*Abundance+scale(Body_mass)*Abundance
               +(1|eartag)+(1|plot)+ (1|station),data=dane)
summary(tree_abu1)
Anova(tree_abu1)


tree_abu2<-glmmTMB(sqrt(dispersal_tree)~Sex*Abundance+scale(Body_mass)*Abundance + Exploration_rate
               +(1|eartag)+(1|station),data=dane)
summary(tree_abu2)
Anova(tree_abu2)


tree_abu3<-glmmTMB(sqrt(dispersal_tree)~scale(Body_mass)*Abundance +Sex+ Exploration_rate
               +(1|eartag)+ (1|station),data=dane)
summary(tree_abu3)
Anova(tree_abu3)


tab_model(tree_abu2,  auto.label = T, show.re.var=F, show.ngroups = F,
          pred.labels = c("Intercept", "Sex:Abundance", "Body mass:Abundance", "Exploration rate"),
          dv.labels = c("Distance of dispersal from the nearest tree"))

