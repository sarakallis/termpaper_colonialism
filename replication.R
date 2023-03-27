###Seminar: Colonialism and its Consequences in Asia and Africa, University of Zurich, Spring 2021
##Paper Title: Colonial legacies on LGBT+ rights and attitudes
##Author: Sara Kallis
##Date: 28 June 2021

###Contents:
##Part 1: Operationalisation
##Part 2: Data Analysis
##Part 3: Data Visualisation

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)
library(sjPlot) 
library(haven)
library(foreign)
library(tidyverse)
library(sf)
library(stargazer)
library(stringr)
library(lmtest)

##Part 1: Operationalisation####

##run data wrangling script for 'data1'

##country sample n = 49
  #data1_noNA <- na.omit(data1) #N = 30
  #data1$LO <- as.integer(data1$LO)
  
###DV2: Norms, Attitudes#####
  ####prep Afrobarometer data####
  afrobar <- read.spss("afrobarometer_round7_2019_merged.sav", 
                       use.value.labels = TRUE, to.data.frame	= TRUE)
  afrobar <- afrobar[, c("COUNTRY", "REGION", "Q1", "Q87C", "Q97", "Q98", "Q101" ,"Q115", "Q118", "Q98")]    
  afrobar <-afrobar[afrobar$Q87C !="Missing", ]
  afrobar <-afrobar[afrobar$Q98 !="Missing", ]
  afrobar$Q87C <- gsub("Strongly dislike", "1", afrobar$Q87C)
  afrobar$Q87C <- gsub("Somewhat dislike", "2", afrobar$Q87C)
  afrobar$Q87C <- gsub("Would not care", "3", afrobar$Q87C)
  afrobar$Q87C <- gsub("Somewhat like", "4", afrobar$Q87C)
  afrobar$Q87C <- gsub("Strongly like", "5", afrobar$Q87C)
  afrobar$Q87C <- gsub("Refused to answer", "8", afrobar$Q87C)
  afrobar$Q87C <- gsub("Don’t know", "9", afrobar$Q87C)
  afrobar <-afrobar[afrobar$Q87C !="8", ]
  afrobar <-afrobar[afrobar$Q87C !="9", ]
  afrobar$Q97 <- gsub("No formal schooling", "0", afrobar$Q97)
  afrobar$Q97 <- gsub("Informal schooling only", "1", afrobar$Q97)
  afrobar$Q97 <- gsub("Some primary schooling", "2", afrobar$Q97)
  afrobar$Q97 <- gsub("Primary school completed", "3", afrobar$Q97)
  afrobar$Q97 <- gsub("Some secondary school / high school", "4", afrobar$Q97) #Intermediate school or Some secondary school
  afrobar$Q97 <- gsub("Secondary school / high school completed", "5", afrobar$Q97)
  afrobar$Q97 <- gsub("Post-secondary qualifications, other than university", "6", afrobar$Q97)
  afrobar$Q97 <- gsub("Some university", "7", afrobar$Q97)
  afrobar$Q97 <- gsub("University completed", "8", afrobar$Q97)
  afrobar$Q97 <- gsub("Post-graduate", "9", afrobar$Q97)
  afrobar <-afrobar[afrobar$Q97 !="Don't know", ]
  afrobar <-afrobar[afrobar$Q97 !="Refused", ]
  afrobar <-afrobar[afrobar$Q97 !="Missing", ]
  afrobar$Q115 <- gsub("Rural", "1", afrobar$Q115)
  afrobar$Q115 <- gsub("Semi-Urban", "2", afrobar$Q115)
  afrobar$Q115 <- gsub("Urban", "3", afrobar$Q115) ##coded 2 and 3 opposite to afrobarometer codebook
  afrobar$Q101 <- gsub("Female", "0", afrobar$Q101)
  afrobar$Q101 <- gsub("Male", "1", afrobar$Q101)
  afrobar$Q1 <- as.integer(afrobar$Q1)
  afrobar$Q87C <- as.integer(afrobar$Q87C)
  afrobar$Q97 <- as.integer(afrobar$Q97)
  afrobar$Q115 <- as.integer(afrobar$Q115)
  afrobar$Q118 <- as.integer(afrobar$Q118)
  afrobar$Q101 <- as.integer(afrobar$Q101)
  afrobar$Q98.1 <- NULL
  afrobar$Q118 <- NULL
  afrobar <- afrobar %>%
    rename(
      lgbt = Q87C,age = Q1, education = Q97, male = Q101, urban = Q115, religion = Q98)
  
  ####merge####
  data2 <- merge(data1, afrobar, by.x = "country", by.y = "COUNTRY" ) ##31 countries
  data2_noNA <- na.omit(data2) #20 countries
  rm(afrobar, iv_z_legal_hiv_pop_polity)
  #data2$LO <- as.integer(data2$LO)
  
##Part 2: Data Analysis####
  
  ###Hypothesis 1####
  model1 <- lm(legal_protection ~ LO, data = data1)
  summary(model1)
  plot(model1)
  
  model2 <- lm(legal_protection ~ LO + colpower, data = data1)
  summary(model2)
  
  model3 <- lm(legal_protection ~ LO + aids_d_1990 + hiv_inf_1990 + polity + pop_2019, data = data1)
  summary(model3)
  
  model4 <- lm(legal_protection ~ LO + (aids_d_1990) + polity + log(pop_2019), data = data1)
  summary(model4)
  plot(model4)
    hist(data1$pop_2019)
    hist(log(data1$pop_2019))
    ##don't show this
    
    model4b <- lm(legal_protection ~ LO_British + (aids_d_1990) + polity + log(pop_2019), data = data1)
    model4f <- lm(legal_protection ~ LO_French + (aids_d_1990) + polity + log(pop_2019), data = data1)
    model4m <- lm(legal_protection ~ LO_Mixed + (aids_d_1990) + polity + log(pop_2019), data = data1)
    model4bf <- lm(legal_protection ~ LO_French + LO_British + (aids_d_1990) + polity + log(pop_2019), data = data1)
  
    #Count Dependent Variable, so Poisson:
    model5 <- glm(legal_protection ~ LO + (aids_d_1990) + polity + log(pop_2019), data = data1, family = "poisson")
    summary(model5) ##LO1 significantly negatively correlated, aids deaths sign. positive, polity sign. positive, pop. positive
    plot(model5) 
    
    model5bf <- glm(legal_protection ~ LO_British + LO_French + (aids_d_1990) + polity + log(pop_2019), data = data1, family = "poisson")
    model5m <- glm(legal_protection ~ LO_Mixed + (aids_d_1990) + polity + log(pop_2019), data = data1, family = "poisson")
    
    
    data1$loglegal <- log(data1$legal_protection)
    
    summary(lm(loglegal ~ LO + colpower, data = data1))
    
    ##negative binomial
    library(MASS)
    model12 <- glm.nb(legal_protection ~ LO, data = data1)
    model13 <- glm.nb(legal_protection ~ LO + aids_d_1990 + hiv_inf_1990 + polity + pop_2019, data = data1)
    model13bf <- glm.nb(legal_protection ~ LO_British + LO_French + aids_d_1990 + polity + pop_2019, data = data1)
    model13m <- glm.nb(legal_protection ~ LO_Mixed + aids_d_1990 + polity + pop_2019, data = data1)
    
    
    summary(model12)
    summary(model13)
    
    #Protestant: UK and US only
    data1_Prot <- data1 
    data1_Prot <- data1_Prot[data1_Prot$colpower != "F",] 
    data1_Prot <- data1_Prot[data1_Prot$colpower != "P",]
    data1_Prot <- data1_Prot[data1_Prot$colpower != "I",]
    data1_Prot <- data1_Prot[data1_Prot$colpower != "SP",]
    data1_Prot <- data1_Prot[data1_Prot$colpower != "B",]
    data1_Prot <- na.omit(data1_Prot)
    
    
    unique(data1_Prot$country) 
    #Model 4: 
    
    #Catholic: F and Portugal only
    data1_Cath <- data1
    data1_Cath <- data1_Cath[data1_Cath$colpower != "UK",] 
    data1_Cath <- data1_Cath[data1_Cath$colpower != "US",] 
    data1_Cath <- na.omit(data1_Cath)
    unique(data1_Cath$country) #16

    
  ###Hypothesis 2####
    
    model6 <- lm(lgbt ~ mis, data = data2)
    summary(model6)
    
    model7 <- lm(lgbt ~ mis + aids_d_1990 + hiv_inf_1990 + age + education + male + urban, data = data2)
    summary(model7)
  
    model8 <- lm(lgbt ~ mis + aids_d_1990 + hiv_inf_1990 + age + education + male + urban + religion, 
                 data = data2)
    summary(model8)
    
    model9 <- lm(lgbt ~ mis + aids_d_1990 + hiv_inf_1990 + age + education + male + urban + country, 
                 data = data2)
    summary(model9)
    
    #cross check: 
      #lgbt ~ LO with controls
    model10 <- lm(lgbt ~ mis + aids_d_1990 + hiv_inf_1990 + age + education + male + urban + LO, data = data2)
    summary(model10)
    
      #legal_protection ~ mis
    model11 <- lm(legal_protection ~ mis + LO + (aids_d_1990) + polity + log(pop_2019), data = data1)
    summary(model11)
    unique(data2$country)
    
    #No South Africa#
    data1_noSA <- data1
    data1_noSA <- data1_noSA[data1_noSA$country != "South Africa",]
    data2_noSA <- data2
    data2_noSA <- data2_noSA[data2_noSA$country != "South Africa",]
    #re do: model 2, 3, 5 & 7
    #Model 2: nothing significant
    #Model 3: only aids deaths + and *
    #Model 5: LO French + and **, aids deaths + and ***
    #Model 7: +mis***, -aids deaths***, -hiv infections*, -age***, +education***, -male, +urban***
    
    plot_model(title = )
    plot_model(lm(lgbt ~ mis, data = data2), type = "slope") 
    plot_model(lm(lgbt ~ mis, data = data2_noSA), type = "slope") 
    plot_model(lm(lgbt ~ mis, data = data2_lowmis), type = "slope") ## now it looks like lgbt acceptance is high only in countries with mis < 2
    unique(data2_lowmis$country)
    plot_model(lm(lgbt ~ mis, data = data2_highmis), type = "slope") #
    
    
    #Low Mis Countries Sample
    data2_lowmis <- data2
    data2_lowmis <- data2_lowmis[data2_lowmis$mis != "4",]
    data2_lowmis <- data2_lowmis[data2_lowmis$mis != "3",]
    summary(lm(lgbt ~ mis, data = data2_lowmis)) #mis is negative! and *** !
    
    #High Mis Countries
    data2_highmis <- data2
    data2_highmis <- data2_highmis[data2_highmis$mis != "1",]
    data2_highmis <- data2_highmis[data2_highmis$mis != "2",]
    summary(lm(lgbt ~ mis, data = data2_highmis)) #mis is positive and ***
    
    ##Re-calc model 7:
    model14 <- lm(lgbt ~ mis + aids_d_1990 + hiv_inf_1990 + age + education + male + urban, 
                  data = data2_lowmis)
    model15 <- lm(lgbt ~ mis + aids_d_1990 + hiv_inf_1990 + age + education + male + urban, 
                  data = data2_highmis)
    
    ##Religion
    summary(lm(lgbt ~ religion, data = data2_lowmis)) ## negative: Christian**, Lutheran*, Baptist., Pentecostal., Ismaeli*, Mouridiya***, Tijaniya***, Traditional**
                                                      ##positive: Evangelical.
  
    
    summary(lm(lgbt ~ religion, data = data2_highmis)) ##most are negative and significant. exceptions:+Coptic***, +Ismaeli**, +Hindu***, +Zionist Christian***, +Assembly of God***, +Apostolic**
    
    ##Limit sample to only Catholic, only Protestant mis? separate UK and F?
    unique(data2$colpower) #"F"  "UK" "US" "P" 
   
    #Protestant: UK and US only
    data2_Prot <- data2 
    data2_Prot <- data2_Prot[data2_Prot$colpower != "F",] 
    data2_Prot <- data2_Prot[data2_Prot$colpower != "P",]
    unique(data2_Prot$country) 
    model16 <- lm(lgbt ~ mis + aids_d_1990 + hiv_inf_1990 + polity + age + education + male + urban, 
                  data = data2_Prot)
      #Model 7: same findings
    
    #Catholic: F and Portugal only
    data2_Cath <- data2
    data2_Cath <- data2_Cath[data2_Cath$colpower != "UK",] 
    data2_Cath <- data2_Cath[data2_Cath$colpower != "US",] 
    unique(data2_Cath$country) #16
    model17 <- lm(lgbt ~ mis + aids_d_1990 + hiv_inf_1990 + polity + age + education + male 
                  + urban, data = data2_Cath)
    
    ##Present Findings####
    
      #Hypothesis 1: Effect on Laws
    stargazer(model1, model4, model5, model12, model13, type = "html", 
              out = "table1.doc")
    plot_model( type = "slope")
    
    stargazer(model4bf, model4m, type = "html", 
              out = "table1_new.doc")
    ##Appendix
    stargazer(model5bf, model5m, model13bf, model13m, type = "html", 
              out = "table1_appendix.doc")
    
    
      #Hypothesis 2: Effect on Norms
    stargazer(model6, model7, type = "html", 
              out = "table2.doc")
    plot_model(model6, type = "slope")
    plot_model(model7, type = "pred")
    plot_model(model7, type = "std")
    
    stargazer(model14, model15, type = "html", 
              out = "table3.doc")
    
    stargazer(model16, model17, type = "html", 
              out = "table4.doc")
    
    

    plot_model(model1, type = "slope") #LO = 0 = 
    
  
