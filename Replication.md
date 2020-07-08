---
title: "Statistics II: Statistical Modeling & Causal Inference"
subtitle: "Do female politicians empower women to vote or run for ofﬁce? A regression discontinuity approach"
author: "Diego Oliveira Faria"
date: "25/05/2020"
bibliography: bibliography.bib
output:
  html_document: 
    code_folding: hide
    css: mycss.css
    fig_caption: yes
    fig_width: 6
    highlight: pygments
    number_sections: yes
    theme: journal
    toc: yes
    toc_float:
      collapsed: no
---

```{r setup}
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE)

```

# Summary

<center>
<img src= "https://media.giphy.com/media/RkDLG7P2g11WeoVYHN/giphy.gif" alt="soda_size" width="450"/></center>
<br>
<br>
<br>

## Women in Politics

This work presents the replication of the results from the paper **Do female politicians empower women to vote or run for ofﬁce? A regression discontinuity approach** by *David E. Broockman* [@pap] from the Department of Political Science, University of California, Berkeley. The paper's goal was to test the hypothesis that an increase in women's representation as elected officials has a positive effect on female participation in politics both in mass and elite levels.

The gender gap in politics is a pressing issue in many countries, both in terms of mass and elite participation. Participation in the mass level means casting; participation in the elite level means occupying positions as elected officials. Such a phenomenon has called the attention of many scholars. And some relevant results confirmed the hypothesis for specific realities, such as in India [@dein]

The question to be answered was whether the presence of women as a candidate or as officeholders do reduce the gender gap of political participation in the government by making more women race for elected posts or vote?
Previous works suggest a positive impact of women's elections on both women's political participation and presence in the office.



## Data

The data is available for replication at [@rep].

* **Female Candidates** <br>
  This dataset has information about all elections that women ran for state legislature since 1999. It was made available by the Center on American Women and Politics (CAWP). The author treated it, and only elections with a woman opposing a man remained. That resulted in a dataset with 3,813 state legislative elections where a woman opposed a man in 2002, 2004, 2006, and 2008.

* **Election returns** <br>
  The author gathered State legislative returns from state legislative websites. Then, he matched this information with the information of women opposing men.


* **Dependent variable I: women’s candidacies in nearby** <br>
  The information about districts was obtained from the US Census and then matched to the election return and CAWP. The US Census yielded the geographic location and boundaries of state legislative districts. These geographic boundary ﬁles allowed the author to identify other districts' nearby' each of the 3813 main districts and thus where elites and potential female candidates might learn of women's electoral victories and ultimately witness women serving in ofﬁce.

  
* **Dependent variable II: women's voter turnout** <br>
  Information purchased from a well-known voter data clearinghouse. The dataset yield the number of votes cast in the general election and the number of votes cast by women in every even-numbered election year between 2000 and 2010 from their comprehensive US voter ﬁles. This dataset yielded two variables of interest in each election:  the share of the electorate which women comprised, and the women's voter turnout.


## Empirical Strategy

The strategy used by the author was a sharp regression discontinuity design (sharp RDD). The forcing variable was the female margin of votes, rescaled around 50% of votes received. The rescaled variable's positive values mean that the woman won the election; negative values of margin mean that the man won the election.

The main results were obtained by specifying a 4 th order polynomial on each side of the decontinuity in the form:

**$\small Dependent \: Variable= \alpha+\beta_1V_i+\beta_2V_i^2 +\beta_3V_i^3 +\beta_4V_i^4 +\gamma F_i +\beta_5V_i+\beta_6V_i^2 +\beta_7V_i^3 +\beta_8V_i^4 +\epsilon_i$**



Where $V$ stands for the female margin of votes, and $F$ is the dummy variable that shows whether the woman won or not.


The equation applies to each one of the dependent variables. A local linear regression model was also employed, but the results appear only in the appendix of the paper, so we do not replicate it here.


The design seems to fit well with this type of problem. It is plausible the assumption that the average potential outcomes are equal around the discontinuity; it means that other covariates that could affect the outcomes don't change abruptly in the threshold. Consequently, the change in the dependent variable's outcomes would be a result only of the change in the state of the forcing variable. Although, further analysis must and made in this replication to check the robustness of this assumption.


## Findings

Statistically, there was no discernible effect. The election of an additional woman did not affect women's political participation—results in contrast with findings from India. Although electing the first woman may have an impact, other barriers may be too strong to erode by merely electing more women.




# Analysis

```{r Libraries and Data, collapse=FALSE}

library(foreign)
library(tidyr)
library(car)
library(ggplot2)
library(dplyr)
library(stargazer)
library(rdrobust)
library(sandwich)
library(lmtest)
library(gtable)

rm(list = ls())
dat<-read.dta("./Data/main data.dta")
dat <- dat %>% rename(nextyear=femaleonballotnextyear,
                      margin=femalecand_margin_of_victory,
                      turnout=femaleturnout_nextcycle, electorate=female_percentageofelectorate_ne,cand10=femcands_over_contests_next_10r,cand75=femcands_over_contests_next_75m,win10=femwins_over_contests_next_10r,win75=femwins_over_contests_next_75m)

```

## Does electing women empower other women to vote?

The first result to be replicated are the effect of  women on ballot, women's turnout in the next election, and women's turnout elative to total electorate.



### Replication - Table 1


```{r Table1,warning=FALSE,echo=TRUE,warning=FALSE,results='asis'}

bw<-.15

dat_filtered <- dat %>% filter(margin<=bw & margin>=-bw) 

datm <- dat_filtered %>% dplyr:: select(nextyear,margin,womanwon)
datt <- dat_filtered %>% dplyr:: select(margin,womanwon,turnout)
date <- dat_filtered %>% dplyr:: select(margin,womanwon,electorate)


# ================ Column 1 Table 1====================================================

reg1<-lm(nextyear~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+womanwon+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon,dat_filtered)

# Robust standard error estimate 

cov1 <- vcovHC(reg1, type = "HC1") 
robust.se1 <- sqrt(diag(cov1))

# ================ Column 2 Table 1====================================================

# Linear regression for the 2nd column
reg2<-lm(turnout~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon+womanwon,dat_filtered)

# Robust standard error estimate
cov2 <- vcovHC(reg2, type = "HC1")
robust.se2 <- sqrt(diag(cov2))

# ================ Column 3 Table 1====================================================

# Linear regression for the 3nd column

reg3<-lm(electorate~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon+womanwon,dat_filtered)

# Robust standard error estimate

cov3 <- vcovHC(reg3, type = "HC1")
robust.se3 <- sqrt(diag(cov3))

# ===========================Table 1-Replication========================================

stargazer(reg1,reg2,reg3,type = "html",title="Table 1 - Replication",
          keep.stat=c("n","rsq","bic"), 
          se=list(robust.se1,robust.se2,robust.se3),
          covariate.labels = c("Woman Won","Constant"),
          omit = c(":","I(margin2)","I(margin3)","I(margin4)","margin"),  
          column.labels=c("Woman on Ballot 	&nbsp;&nbsp;","Women's Voter Turnout 	&nbsp;&nbsp;","Fem. Share of Elect."), 
          align=TRUE,dep.var.labels=c("","",""),notes.append = FALSE,notes = c("<sup>&sstarf;</sup>p<0.1;<sup>&sstarf;&sstarf;</sup>p<0.05;<sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01"))

```

<br>

**The results are exactly the same obtained by the author. Except for the robust standard error in the second column, possibly an small miscorrection in the paper.**


### Replication - Figure 3

Some visualization improvements were made in the graphic if compared to the paper. First, I associated different colors for information bellow, and above the cutoff, I also binned data is by its mean,with size proportional to the number of observations in each bin.

```{r Figure 3, error=FALSE, message=FALSE, warning=FALSE, hold=TRUE}
# 
dat_filtered$bin<- cut(dat_filtered$margin,breaks = seq(from = -0.15,to = 0.15,by=0.005),ordered_result = TRUE)
 
dat_g <- dat_filtered %>% group_by(bin) %>%summarise(mean_n = mean(nextyear,na.rm = T),mean_t = mean(turnout,na.rm = T),mean_e = mean(electorate,na.rm = T),c10 =mean(cand10,na.rm = T),c75 =mean(cand75,na.rm = T),w10 =mean(win10,na.rm = T),w75 =mean(win75,na.rm = T), n=n()) %>% mutate(x=seq(from=-0.1475,to=0.1475,by=0.005),won=ifelse(x>=0,1,0))

# summary(dat_filtered$nextyear)

#============================== Graph 1 =========================================

g1<-ggplot()+geom_point(data=dat_g,aes(x=x,y=mean_n,colour=factor(won),size=n), shape=20)+
  geom_text(show.legend = FALSE)+
   geom_smooth(aes(x=margin,y=nextyear,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
              # color = "#F8766D", 
              size = 1, se=TRUE) +
  geom_smooth(aes(x=margin,y=nextyear,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              # color = "#00BFC4", 
              size = 1, se=TRUE) + 
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_x_continuous("Female Margin of Victory - Election 1", breaks = c(-.15,-.1,-.05,0,.05,.1,.15),limits=c(-.15,.15),expand = c(0, 0))+
  ggtitle("(c) Women on Ballot Next Year in the Same District - Election 2") +
  theme(legend.position = "none",
        axis.title.y=element_blank())




#============================== Graph 2 =========================================


g2 <- ggplot()+geom_point(data=dat_g,aes(x=x,y=mean_t,colour=factor(won),size=n), shape=20)+
   geom_smooth(aes(x=margin,y=turnout,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
              # color = "#F8766D", 
              size = 1, se=TRUE) +
  geom_smooth(aes(x=margin,y=turnout,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              # color = "#00BFC4", 
              size = 1, se=TRUE) + 
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_x_continuous("Female Margin of Victory - Election 1", breaks = c(-.15,-.1,-.05,0,.05,.1,.15),limits=c(-.15,.15),expand = c(0, 0))+
  ggtitle("(a) Female Voter Turnout - Election 2") +
  theme(legend.position = "none",
        axis.title.y=element_blank())


#============================== Graph 3 =========================================  
  


g3 <- ggplot()+geom_point(data=dat_g,aes(x=x,y=mean_e,colour=factor(won),size=n), shape=20)+
   geom_smooth(aes(x=margin,y=electorate,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
              # color = "#F8766D", 
              size = 1, se=TRUE) +
  geom_smooth(aes(x=margin,y=electorate,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              # color = "#00BFC4", 
              size = 1, se=TRUE) + 
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_x_continuous("Female Margin of Victory - Election 1", breaks = c(-.15,-.1,-.05,0,.05,.1,.15),limits=c(-.15,.15),expand = c(0, 0))+
  ggtitle("(b) Female Percentage of the Electorate - Election 2") +
  theme(legend.position = "none",         axis.title.y=element_blank())

  

gridExtra::  grid.arrange(g2,g3,g1,nrow=3)


```

The graphs present the same results as the paper. Visually it is possible to see that only one result presents statistically significant local average treatment effect in the around the threshold.



## Do female politicians break glass ceilings for other women to run?

In this part of the paper, the authors investigate possible spillover effects in districts close to where the elections occurred. 

They examined two effects: women's candidacies and women's turnout. The results show the estimated impact for districts picked within 75 miles from the district where a female won or lost, and for the 10 closest districts criteria.

### Replication - Table 2

```{r Table2,warning=FALSE,echo=TRUE,warning=FALSE,results='asis'}

datcand10 <- dat_filtered %>% dplyr:: select(nextyear,margin,cand10)
datcand75 <- dat_filtered %>% dplyr:: select(margin,womanwon,cand75)
datwin10 <- dat_filtered %>% dplyr:: select(margin,womanwon,win10)
datwin75 <- dat_filtered %>% dplyr:: select(margin,womanwon,win75)

# ========================= Table 2- Column 1===========================================

regcand75 <-lm(cand75~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon+womanwon,dat_filtered)

# Robust standard error estimate 

covc75 <- vcovHC(regcand75, type = "HC1") 
robust.cand75 <- sqrt(diag(covc75))

# ================ Table 2- Column 2====================================================

regcand10 <-lm(cand10~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon+womanwon,dat_filtered)

# Robust standard error estimate 

covc10 <- vcovHC(regcand10, type = "HC1") 
robust.cand10 <- sqrt(diag(covc10))

# ================ Table 2- Column 3====================================================

regwin75 <-lm(win75~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon+womanwon,dat_filtered)

# Robust standard error estimate 

covw75 <- vcovHC(regwin75, type = "HC1") 
robust.win75 <- sqrt(diag(covw75))


# ================ Table 2- Column 4====================================================

regwin10 <-lm(win10~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*womanwon+I(margin^2)*womanwon+I(margin^3)*womanwon+
           I(margin^4)*womanwon+womanwon,dat_filtered)

# Robust standard error estimate 

covw10 <- vcovHC(regwin10, type = "HC1") 
robust.win10 <- sqrt(diag(covw10))

# ================ Table 2 - Replication ==========================================================

stargazer(regcand75,regcand10,regwin75,regwin10,type = "html",title="Table 2 - Replication",
          keep.stat=c("n","rsq","bic"), 
          se=list(robust.cand75,robust.cand10,robust.win75,robust.win10),
          covariate.labels = c("Woman Won","Constant"),
          omit = c(":","I(margin2)","I(margin3)","I(margin4)","margin"),  
          column.labels=c("<b> Fem. Cand. per Contest </b>","<b>Fem. Vict. per Contest<b>"),
          column.separate = c(2,2),
          model.numbers = FALSE,
          multicolumn = TRUE,
          align=TRUE,dep.var.labels=c("75 mi.","10 closest","75 mi.","10 closest")
          ,notes.append = FALSE,notes = c("<sup>&sstarf;</sup>p<0.1;<sup>&sstarf;&sstarf;</sup>p<0.05;<sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01"))


```



**The results are exactly the same obtained by the author. **


### Replication - Figure 4


```{r Figure 4, error=FALSE, fig.height=5, fig.width=10, message=FALSE, warning=FALSE, hold=TRUE}

#============================== Graph 4 =========================================

g4<-ggplot()+geom_point(data=dat_g,aes(x=x,y=c75,colour=factor(won),size=n), shape=20)+
  geom_text(show.legend = FALSE)+
   geom_smooth(aes(x=margin,y=cand75,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
              size = 1, se=TRUE) +
  geom_smooth(aes(x=margin,y=cand75,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              size = 1, se=TRUE) + 
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_x_continuous("Female Margin of Victory - Election 1", breaks = c(-.15,-.1,-.05,0,.05,.1,.15),limits=c(-.15,.15),expand = c(0, 0))+
  ggtitle("(a) Female Candidates 75 miles")+
  theme(legend.position = "none",         axis.title.y=element_blank())




#============================== Graph 5 =========================================


g5 <- ggplot()+geom_point(data=dat_g,aes(x=x,y=c10,colour=factor(won),size=n), shape=20)+
   geom_smooth(aes(x=margin,y=cand10,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
              # color = "#F8766D", 
              size = 1, se=TRUE) +
  geom_smooth(aes(x=margin,y=cand10,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              # color = "#00BFC4", 
              size = 1, se=TRUE) + 
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_x_continuous("Female Margin of Victory - Election 1", breaks = c(-.15,-.1,-.05,0,.05,.1,.15),limits=c(-.15,.15),expand = c(0, 0))+
  ggtitle("(c) Female Candidates 10 closest")+
  theme(legend.position = "none",         axis.title.y=element_blank())


#============================== Graph 6 =========================================  
  


g6 <- ggplot()+geom_point(data=dat_g,aes(x=x,y=w75,colour=factor(won),size=n), shape=20)+
   geom_smooth(aes(x=margin,y=win75,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
              # color = "#F8766D", 
              size = 1, se=TRUE) +
  geom_smooth(aes(x=margin,y=win75,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              # color = "#00BFC4", 
              size = 1, se=TRUE) + 
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_x_continuous("Female Margin of Victory - Election 1", breaks = c(-.15,-.1,-.05,0,.05,.1,.15),limits=c(-.15,.15),expand = c(0, 0))+
  ggtitle("(b) Female Win 75 Miles")+
  theme(legend.position = "none",         axis.title.y=element_blank())


#============================== Graph 7 =========================================  
  


g7 <- ggplot()+geom_point(data=dat_g,aes(x=x,y=w10,colour=factor(won),size=n), shape=20)+
   geom_smooth(aes(x=margin,y=win10,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin <= 0,], 
              size = 1, se=TRUE) +
  geom_smooth(aes(x=margin,y=win10,colour=factor(womanwon)),method = "lm",formula = y~poly(x,4),data = dat_filtered[dat_filtered$margin >= 0,], 
              size = 1, se=TRUE) + 
  geom_vline(xintercept = 0,linetype="dotted")+
  scale_x_continuous("Female Margin of Victory - Election 1", breaks = c(-.15,-.1,-.05,0,.05,.1,.15),limits=c(-.15,.15))+
  ggtitle("(d) Female Win 10 Closest")+
  theme(legend.position = "none",         axis.title.y=element_blank())


gridExtra::  grid.arrange(g4,g6,g5,g7,nrow=2)

```


The graphs present the same results as the paper. Visually it is possible to see that none of the graphs present significant local average treatment effect.


# Extension

## Falsification Checks

To test the robustness of the results, we do some falsification checks that are no present in the paper. The first one is a placebo cutoff at 0.05 female margin of victory, the second is robustness to specification, testing a linear model with different slopes, and the third test check for any covariate sorting around the threshold, for party affiliation, which could indicate some discontinuity of covariates.

### Placebo Cutoff

The results show that the placebo cutoff at the point of a 0.05 positive female margin of victory doesn't produce any statistically significant result, as expected.

```{r Table3,warning=FALSE,echo=TRUE,warning=FALSE,results='asis'}
#===================== Placebo Cutoff-  Table 1 ==========================

dat_filtered<-dat_filtered %>% mutate(plac1=ifelse(margin>=0.05,1,0))

regp1<-lm(nextyear~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*plac1+I(margin^2)*plac1+I(margin^3)*plac1+
           I(margin^4)*plac1+plac1,dat_filtered)

regp2<-lm(turnout~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*plac1+I(margin^2)*plac1+I(margin^3)*plac1+
           I(margin^4)*plac1+plac1,dat_filtered)

regp3<-lm(electorate~margin+I(margin^2)+
           I(margin^3)+I(margin^4)+
           margin*plac1+I(margin^2)*plac1+I(margin^3)*plac1+
           I(margin^4)*plac1+plac1,dat_filtered)

stargazer(regp1, regp2,regp3,type = "html",title= "Table 3 - Placebo Cutoff",
          keep.stat=c("n","rsq","bic"), 
          covariate.labels = c("Woman Won","Constant"),
          omit = c(":","I(margin2)","I(margin3)","I(margin4)","margin"),  
          column.labels=c("Woman on Ballot","Women's Turnout","Fem. Share of the Electorate"),
          model.numbers = FALSE,
          align=TRUE,dep.var.labels=c("WB","FT","PE","WB","FT","PE"),notes.append = FALSE,notes = c("<sup>&sstarf;</sup>p<0.1;<sup>&sstarf;&sstarf;</sup>p<0.05;<sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01"))



```

### Sensitivity to Specification

The results show that the model, specifically for the results from Table 1 changes are not signification for a linear specification with different slopes.

```{r Table4,warning=FALSE,echo=TRUE,warning=FALSE,results='asis'}


# ================ Column 1 Table 4====================================================

regl1 <- lm(nextyear~womanwon+margin+womanwon*margin,data = dat_filtered)

# Robust standard error estimate 

cov1 <- vcovHC(regl1, type = "HC1") 
robust.sel1 <- sqrt(diag(cov1))

# ================ Column 2 Table 4====================================================

# Linear regression for the 2nd column
regl2 <- lm(turnout~womanwon+margin+womanwon*margin,data = dat_filtered)

# Robust standard error estimate
covl2 <- vcovHC(regl2, type = "HC1")
robust.sel2 <- sqrt(diag(covl2))

# ================ Column 3 Table 4====================================================

# Linear regression for the 3rd column

regl3 <- lm(electorate~womanwon+margin+womanwon*margin,data = dat_filtered)

# Robust standard error estimate

covl3 <- vcovHC(regl3, type = "HC1")
robust.sel3 <- sqrt(diag(covl3))

# ===========================Table 4================================

stargazer(reg1, reg2,reg3,regl1,regl2,regl3,type = "html",title= "Table 4 - Sensitivity",
          keep.stat=c("n","rsq","bic"), 
          se=list(robust.se1,robust.se2,robust.se3,robust.sel1,robust.sel2,robust.sel3),
          covariate.labels = c("Woman Won","Constant"),
          omit = c(":","I(margin2)","I(margin3)","I(margin4)","margin"),  
          column.labels=c("Polynomial Model","Linear Model"),
          column.separate = c(3,3),
          model.numbers = FALSE,
          align=TRUE,dep.var.labels=c("WB","FT","PE","WB","FT","PE"),notes.append = FALSE,notes = c("<sup>&sstarf;</sup>p<0.1;<sup>&sstarf;&sstarf;</sup>p<0.05;<sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01"))




```

<br>

Where WB, FT and PE, are the dependent variables corresponding to the first, second and third column of the Table 1, respectively.

The conclusion is that the model is robust to a linear specification with different slopes.


### Sorting Around the Threshold

The density probablity function shows a possible sorting just above the threshold for candidates without party affiliation. Also, it also shows a higher frequency of democrats bellow the cutoff if compared to Republicans.


```{r Graphics 3, hold=TRUE,warning=FALSE,error=FALSE,message=FALSE,fig.width=6,fig.height=3}
ggplot(dat_filtered,aes(margin,fill=Pty,colour=Pty))+
         geom_density(alpha=.1)+
        geom_vline(xintercept=0,linetype="dotted" )+
  scale_x_continuous("Female Margin of Victory", breaks = c(-.15,-.1,-.05,0,.05,.1,.15),limits=c(-.15,.15))+
  theme_minimal()
       

```

These results may indicate some discontinuity of unobserved covariates around the threshold. Some hyphotesis to explain this phenomenon could be that non-affiliated candidates has a previous expected advatage in the race that could make them no unwilling to be attached to any party and to the consequent obligations. The difference among Democrats and Republicans around the treshold, is less straightforward to explain, and should require further investigation.




## Partisanship 

Another important analysis is to check possible differences of effects accordig to partisanship of the voters, for the results showed in the Table 1.

According to a research carried out by [@pew], from Washington DC. Democrats and Republicans have different views about gender gaps. Some findings are that: 

- Democrats are largely dissatisfied with the nation's progress on this issue if compared to Republicans

- Democrats are also much more likely than Republicans to say that men have easier lives than women these days


So, the hypothesis to be tested here is whether partisanship would affect the results. To examine this hypothesis,  I subset the data by party affiliation. The reason is that electing a woman would have a higher impact on Republican electorate since after a woman's election, this group would have a higher potential for learning and changing attitudes about woman's role in the political arena.

```{r Table5,warning=FALSE,echo=TRUE,warning=FALSE,results='asis'}

dem<- dat_filtered %>% subset(Pty=="D")

repl <- dat_filtered %>% subset(Pty=="R")

# ================ Column 1 Table 4====================================================

reg1d <- lm(nextyear~womanwon+margin+womanwon*margin,data = dem)

# Robust standard error estimate 

cov1d <- vcovHC(reg1d, type = "HC1") 
robust.se1d <- sqrt(diag(cov1d))


reg1p <- lm(nextyear~womanwon+margin+womanwon*margin,data = repl)

# Robust standard error estimate 

cov1p <- vcovHC(reg1p, type = "HC1") 
robust.se1p <- sqrt(diag(cov1p))

# ================ Column 2 Table 4====================================================

reg2d <- lm(turnout~womanwon+margin+womanwon*margin,data = dem)

# Robust standard error estimate 

cov2d <- vcovHC(reg2d, type = "HC1") 
robust.se2d <- sqrt(diag(cov2d))


reg2p <- lm(turnout~womanwon+margin+womanwon*margin,data = repl)

# Robust standard error estimate 

cov2p <- vcovHC(reg2p, type = "HC1") 
robust.se2p <- sqrt(diag(cov2p))

# ================ Column 3 Table 4====================================================

reg3d <- lm(electorate~womanwon+margin+womanwon*margin,data = dem)

# Robust standard error estimate 

cov3d <- vcovHC(reg3d, type = "HC1") 
robust.se3d <- sqrt(diag(cov3d))


reg3p <- lm(electorate~womanwon+margin+womanwon*margin,data = repl)

# Robust standard error estimate 

cov3p <- vcovHC(reg3p, type = "HC1") 
robust.se3p <- sqrt(diag(cov3p))

# ===========================Table 4========================================

stargazer(reg1d,reg2d,reg3d,reg1p,reg2p,reg3p,type = "html",title= "Table 5 - Partisanship",
          keep.stat=c("n","rsq","bic"), 
          se=list(robust.se1d,robust.se2d,robust.se1p,robust.se2p,robust.se3p),
          covariate.labels = c("Woman Won","Constant"),
          omit = c(":","I(margin2)","I(margin3)","I(margin4)","margin"),  
          # column.labels=c("Democrats"," Republicans"),
          # column.separate = c(3,3),
          model.numbers = FALSE,
          # table.layout = "=l-d-c-!-t-m-s=n",
          align=TRUE,dep.var.labels=c("WB - Dem","FT-Dem","PE-Dem","WB-Rep","FT-Rep","PE-Rep"),notes.append = FALSE,notes = c("<sup>&sstarf;</sup>p<0.1;<sup>&sstarf;&sstarf;</sup>p<0.05;<sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01"))



```

Where WB, FT, and PE, are the dependent variables corresponding to the first, second and third columns of Table 1, respectively. And the suffixes -Dem and -Rep, stands, respectively, for democrats and republicans.

Regarding the female's turnout, and the proportion of females in the electorate, the results are consistent with the hypothesis presented; in both situations, the effect seems to be higher among republicans. For the effect of *woman on ballot*, the results do not differ among democrats and republicans.


# References



