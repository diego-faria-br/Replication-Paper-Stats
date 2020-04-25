library(robustbase)
library(foreign)
library(tidyr)
library(car)
library(ggplot2)
library(rdd)
library(dplyr)
library(stargazer)
library(rdrobust)
library(jtools)
library(kableExtra)
library(MASS)
library(sandwich)
library(lmtest)
library(estimatr)

rm(list = ls())
dat<-read.dta("./Data/main data.dta")
dat <- dat %>% rename(nextyear=femaleonballotnextyear,
                      margin=femalecand_margin_of_victory,
                      turnout=femaleturnout_nextcycle, electorate=female_percentageofelectorate_th)
bw<-.15

dat_filtered <- dat %>% filter(margin<=bw & margin>=-bw) 
# dat_filtered <- dat_filtered %>% select(nextyear,margin,womanwon,turnout,electorate,winXfem_v_c_p_4)%>% select(nextyear,womanwon,electorate) %>% na.omit()

datm <- dat_filtered %>% select(nextyear,margin,womanwon)
datt <- dat_filtered %>% select(margin,womanwon,turnout)
date <- dat_filtered %>%select(margin,womanwon,electorate)


# ================ Column 1 Table 1====================================================

reg1<-lm(nextyear~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon+womanwon,dat_filtered)

#robust https://stats.stackexchange.com/questions/117052/replicating-statas-robust-option-in-r

# c1 <- coeftest(regm,vcov=vcovHC(regm,"HC1"))
# stargazer(c1,type = "text")

#https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf robust estimation of std. errors

cov1 <- vcovHC(regm, type = "HC1")
robust.se1 <- sqrt(diag(cov1))

# regm<-lm(nextyear~womanwon+margin+fem_v_c_p_2+
#            fem_v_c_p_3+fem_v_c_p_4+
#            winXfem_v_c_p+winXfem_v_c_p_2+
#            winXfem_v_c_p_3+winXfem_v_c_p_4,
#          dat_filtered)


# ================ Column 2 Table 1====================================================


reg2<-lm(turnout~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon+womanwon,dat_filtered)



cov2 <- vcovHC(reg2, type = "HC1")
robust.se2 <- sqrt(diag(cov2))

# c2 <- coeftest(reg2,vcov=vcovHC(reg2,"HC1"))
# sjstats::robust(reg2)[6,]





# ================ Column 3 Table 1====================================================



reg3<-lm(electorate~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon+womanwon,dat_filtered)

cov3 <- vcovHC(reg3, type = "HC1")
robust.se3 <- sqrt(diag(cov3))

# c3 <- coeftest(reg3,vcov=vcovHC(reg3,"HC1"))

# ===========================Table 1====================================================

stargazer(reg1,reg2,reg3,type = "text",
          keep.stat=c("n","rsq"), 
          se=list(robust.se1,robust.se2,robust.se3),
          covariate.labels = c("Woman Won","Constant"),
          # keep=c("womanwon","Constant"),
          omit = c(":","I(margin2)","I(margin3)","I(margin4)","margin"),  
          column.labels=c("Woman on Ballot","Women's Voter Turnout","Female Share of Electorate"), 
          align=TRUE,dep.var.labels=c("","",""))


# dat_filtered <- na.omit(dat_filtered)

# dat_filtered$reg1<- predict(reg1)

g1<-ggplot(dat_filtered, aes(x=margin,y=nextyear))+
  #   geom_line(aes(y=reg1),data = dat_filtered[dat_filtered$margin >= 0,], 
  #           color = "#00BFC4", 
  #           size = 1) +
  # geom_line(aes(y=reg1),data = dat_filtered[dat_filtered$margin <= 0,], 
  #           color = "#F8766D", 
  #           size = 1)+
  geom_smooth(method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
                         color = "#F8766D", 
                         size = 1, se=TRUE) +
  geom_smooth(method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              color = "#00BFC4", 
              size = 1, se=TRUE)+
  geom_vline(xintercept = 0)+
  # geom_line(aes(y=reg1),data = dat_filtered[dat_filtered$margin <= 0,], 
  #           color = "#F8766D", 
  #           size = 1)+
    stat_summary_bin(binwidth = 0.005)
g1

g2<-ggplot(dat_filtered, aes(x=margin,y=turnout))+
  geom_smooth(method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
              color = "#F8766D", 
              size = 1, se=TRUE) +
  geom_smooth(method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              color = "#00BFC4", 
              size = 1, se=TRUE)+
  geom_vline(xintercept = 0)+
  stat_summary_bin(binwidth = 0.005, size=0.2)
g2

g3<-ggplot(dat_filtered, aes(x=margin,y=electorate))+
  geom_smooth(method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
              color = "#F8766D", 
              size = 1, se=TRUE) +
  geom_smooth(method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              color = "#00BFC4", 
              size = 1, se=TRUE)+
  geom_vline(xintercept = 0)+
  stat_summary_bin(binwidth = 0.005, size=0.2)
g3




# stargazer(reg1,type = "text")
# 
# datt<-dat %>% select(Year,womanwon,margin,nextyear,turnout,electorate)
# # tab1<-Restimate(femaleonballotnextyear~ femalecand_margin_of_victory+I(femalecand_margin_of_victory^2)+I(femalecand_margin_of_victory^3)+I(femalecand_margin_of_victory^4)+womanwon,)
# # tab1<-RDestimate(femaleonballotnextyear~ femalecand_margin_of_victory,data=datt,cutpoint=0,bw=.15)
# rd<-rdrobust(datt$nextyear,datt$margin,p=4,h=0.15)
# g<-rdplot(datt$femaleonballotnextyea,datt$femalecand_margin_of_victory,p=4,
#        h=0.15,nbins=c(50,50))
# rd2<-rdrobust(datt$femaleturnout_nextcycle,datt$femalecand_margin_of_victory,p=4,h=0.15)
# rd3<-rdrobust(datt$female_percentageofelectorate_ne,datt$femalecand_margin_of_victory,p=4,h=0.15)
# kable(summary(rd))
# summary(rd2)
# summary(rd3)
# 
# plot(rd)
