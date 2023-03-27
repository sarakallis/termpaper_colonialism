setwd("~/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper")
library(readxl)
library(sjPlot) 
library(haven)
library(foreign)
library(tidyverse)
library(sf)
library(stargazer)
library(stringr)
library(lmtest)

###variables:
iv_zilt_legal_hiv ##contains: colonial history variables, colonial legal origins, HIV/AIDS infections and deaths 1990. Unit: Country
dv_lgbtlaws2019 ##contains: 2 indices on legal protection of sexual orientation and gender identity, 1 composite index. Unit: Country
data1 ##Research design operationalisation 1: iv_zilt_legal_hiv merged with dv_lgbtlaws2019

##country sample n = 49
countrysample <- read.csv("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/countrysample.csv")

###IV: ZILTENER DATA####
colonialdata <- read_excel("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Independent Variables/Ziltener_Copy of Colonial_transformation_data.xls")
        colonialdata$country <- gsub("Unit. Arab. Emir", "United Arab Emirates", colonialdata$country)
        colonialdata$country <- gsub("Central African", "Central African Republic", colonialdata$country)
        colonialdata$country <- gsub("Ivory coast", "Côte d'Ivoire", colonialdata$country)
        colonialdata$country <- gsub("Gambia, The", "Gambia", colonialdata$country)
        colonialdata$country <- gsub("Egypt, Arab Rep", "Egypt", colonialdata$country)
        colonialdata$country <- gsub("Iran, Islamic R", "Iran", colonialdata$country)
        colonialdata$country <- gsub("Gambia, The", "Gambia", colonialdata$country)
        colonialdata$country <- gsub("Equatorial Guin", "Equatorial Guinea", colonialdata$country)
        colonialdata$country <- gsub("Korea, Rep.", "South Korea", colonialdata$country)
        colonialdata$country <- gsub("Syrian Arab Rep", "Syrian Arab Republic", colonialdata$country)
        colonialdata$country <- gsub("Zaire", "Democratic Republic of the Congo", colonialdata$country)
        colonialdata$violcol <- NULL #included in violtot
        colonialdata$violres <- NULL #included in violtot
        colonialdata$violind <- NULL #included in violtot
        colonialdata$powertransf <- NULL #not in codebook

#unique(colonialdata$country)
#merge1 <- merge(colonialdata, lgbtlaws2019, by = "country", all.x= F) #N = 51

###IV: Klerman et al. 2011 replication, https://academic.oup.com/jla/article/3/2/379/899816#supplementary-data 
#klerman <- read.dta("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Independent Variables/lar002_supplementary_data/Klerman_etal_LO_v_CO.dta")
#write.csv(klerman, "/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Independent Variables/lar002_supplementary_data/Klerman_etal_LO_v_CO.csv")
legalorigin <- read.csv("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Independent Variables/lar002_supplementary_data/Klerman_etal_LO_v_CO.csv")
        legalorigin <- legalorigin[, c("country", "countrycode", "LO", "CO", "legor_EC")]
        legalorigin$country <- gsub("Congo", "Congo, Rep.", legalorigin$country)
        legalorigin$country <- gsub("Cote D'Ivoire", "Côte d'Ivoire", legalorigin$country)
        legalorigin$country <- gsub("Zaire", "Democratic Republic of the Congo", legalorigin$country)
        
    unique(legalorigin$CO)
    
legalorigin <- merge(legalorigin, countrysample, all.x = F)

iv_zilt_legalorigin <- merge(legalorigin, colonialdata, all.x = T)
iv_zilt_legalorigin <- merge(countrysample, iv_zilt_legalorigin) #N = 44
iv_zilt_legalorigin$countrycode1 <- NULL
rm(colonialdata, legalorigin)

###CONTROL: HIV/AIDS####
    
    ##New HIV Infections, 1990
inf <- read.csv("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Independent Variables/New HIV infections_1990_2019.csv")
inf <- inf[, c("country", "hiv_inf_1990")]
inf <- merge(countrysample, inf, all.x = T) #N = 45. Missing: Côte d'Ivoire, Mali, Mauritania, Seychelles

    ##AIDS Deaths, 1990
d <- read_excel("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Independent Variables/AIDS-related deaths - All ages - 1990_2019.xlsx")
d <- d[, c("country", "aids_d_1990")]
d$aids_d_1990 <- as.integer(d$aids_d_1990)
d <- merge(countrysample, d, all.x = T) #N = 42. Missing: Benin, Botswana, Ethiopia, Mali, Mauritius, Seychelles, Togo

iv_hivaids <- merge(d, inf, all.x = T)

ggplot(iv_hivaids) + geom_point(aes(x = aids_d_1990, y = hiv_inf_1990)) #linear


iv_zilt_legal_hiv <- merge(iv_hivaids, iv_zilt_legalorigin, all.x = T)

###DV 1: Laws######
    ##Sexual Orientation: ILGA
sexual_prot <- read_excel("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Dependent Variable(s)/Laws/ilga_legal.xlsx")
sexual_prot <- sexual_prot[, c("continent", "country", "ilga_index")]  #formerly: ilga2019
    ##Gender diversity: UNAIDS
gender_prot <- read.csv("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Dependent Variable(s)/Laws/UNAIDS_legalprot_trans.csv")
gender_prot$country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", gender_prot$country)

dv_lgbtlaws2019 <- merge(sexual_prot, gender_prot, all.x = F) #N = 60
dv_lgbtlaws2019$gender_sexuality <- dv_lgbtlaws2019$ilga_index + dv_lgbtlaws2019$state_gender_index
rm(sexual_prot, gender_prot)
#write.csv(lgbtlaws2019, file = "/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Dependent Variable(s)/Laws/lgbtlaws2019.csv")

    #plot 2 variables
#ggplot(dv_lgbtlaws2019) + geom_point(aes(x = ilga_index, y = state_gender_index)) #not very linear...

#####RD 1: EFFECT ON LAWS#####
data1 <- merge(iv_zilt_legal_hiv, dv_lgbtlaws2019)
data1 <- na.omit(data1) #N = 31
data1$LO <-  data1$LO %>% str_replace_all("French", "0") %>% str_replace_all("Mixed", "2") %>% str_replace_all("Common", "1")
data1$legor_EC <-  NULL #NA when included in regression
data1$CO <-  NULL #NA when included in regression
rm(countrysample, d, dv_lgbtlaws2019, inf, iv_hivaids, iv_zilt_legal_hiv, iv_zilt_legalorigin)

    ##DV: gender_sexuality

    ##Effect of colonial experiences, Ziltener Data
ols1 <- lm(gender_sexuality ~ colpower_num + colonset + colend + colyears,
           data = data1) ## -colonset*, +colend*, R2=0.41, ADJ=0.20
ols2 <- lm(gender_sexuality ~ violtot + domform + firstcen
           + ethnfunc + tradepol + invest + plantat + gold + mining + forpre + workimm + mis + borders,
           data = data1) ## nothing is significant. R2=0.43, ADJ = -0.00
ols2b <- lm(gender_sexuality ~ domform + firstcen
            + ethnfunc + tradepol + invest + forpre + workimm + mis + borders,
            data = data1)

ols2c <- lm(gender_sexuality ~ domform + firstcen
            + ethnfunc + tradepol + invest + mis,
            data = data1)

summary(ols2d)

ols2d <- lm(gender_sexuality ~ domform
            + ethnfunc + tradepol + invest + mis,
            data = data1)
plot_model(ols2d, type = "slope")
plot_model(lm(gender_sexuality ~ domform + invest, data = data1), type = "slope")
plot_model(lm(gender_sexuality ~ domform + invest + mis, data = data1_noSA), type = "slope")


    ##Effect of colonial origin of legal systems
ols3 <- lm(gender_sexuality ~ LO,
           data = data1) ##+Mixed*, ADJ=0.12

    ##Effect of 1990 HIV/AIDS
ols4 <- lm(gender_sexuality ~ aids_d_1990 + hiv_inf_1990,
           data = data1) #nothing..

    ##All significant ones
ols5 <- lm(gender_sexuality ~ colonset + LO,
        data = data1) ##-colonset*, +LOMixed., R2=0.47, ADJ=0.41, F-Stat 8.026

##present findings of effect on laws
stargazer(ols1, type = "html", out = "/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Tables and Plots/table1.doc")
stargazer(ols2, type = "html", out = "/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Tables and Plots/table2.doc")
stargazer(ols2, ols2b, ols2c, ols2d, type = "html", out = "/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Tables and Plots/tables2_2d.doc")


stargazer(ols3, ols4, ols5, type = "html", out = "/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Tables and Plots/table3.doc")

plot_model(ols5, type = "slope") ##exported as: ols5_slope.
plot_model(ols1, type = "slope") ##exported as: ols1_slope. 1=UK, 2=FR, 3=SP, 4=BE, 5=P, 7=IT, 10=US

####Sensitivity Analysis 1####
#1. Limit sample to countries colonised by FR and UK:
data1_UKFR <- data1
data1_UKFR <- data1_UKFR[data1_UKFR$colpower_num != 3,]
data1_UKFR <- data1_UKFR[data1_UKFR$colpower_num != 4,]
data1_UKFR <- data1_UKFR[data1_UKFR$colpower_num != 5,]
data1_UKFR <- data1_UKFR[data1_UKFR$colpower_num != 7,]
data1_UKFR <- data1_UKFR[data1_UKFR$colpower_num != 10,]
unique(data1_UKFR$country) #N=20

ols6 <- lm(gender_sexuality ~ colpower + colonset + colend,
           data = data1_UKFR) ## +colend*, -colonset., R2adj = 0.36
ols7 <- lm(gender_sexuality ~ violtot + domform + firstcen
           + ethnfunc + tradepol + invest + plantat + gold + mining + forpre + workimm + mis + borders,
           data = data1_UKFR) #nothing
ols8 <- lm(gender_sexuality ~ aids_d_1990 + hiv_inf_1990,
           data = data1_UKFR) #nothing
ols9 <- lm(gender_sexuality ~ colonset + LO,
        data = data1_UKFR) ##+LO2***, R2adj = 0.91
plot_model(ols9, type = "slope") 

    ##Same results, only LO:Mixed has stronger effect

#2. Remove South Africa#
data1_noSA <- data1
data1_noSA <- data1_noSA[data1_noSA$country != "South Africa",]
summary(lm(gender_sexuality ~ colpower + colonset + colend, data = data1_noSA)) ##NOTHING is significant!

#3. Sample UK, FR, without S.A.
data1_UF_noSA <- data1_UKFR
data1_UF_noSA <- data1_UF_noSA[data1_UF_noSA$country != "South Africa",]


###Instrumental Variable: AHIV/IDS infections and Deaths#####
###Three Conditions:
#(i) Z has a casual effect on X, 
#(ii) Z affects the outcome variable Y only through X (Z does not have a direct influence on Y which is referred to as the exclusion restriction), and 
#(iii) There is no confounding for the effect of Z on Y.

summary(lm(gender_sexuality ~ hiv_inf_1990 + aids_d_1990, data = data1))


summary(lm(aids_d_1990 ~ colpower_num + colonset + colend + colyears + violtot + domform + firstcen + ethnfunc + tradepol + invest + plantat + gold + mining + forpre + workimm + mis + borders, data = data1))
summary(lm(aids_d_1990 ~ colpower_num + colonset + colend + violtot + firstcen + invest + plantat + mining + forpre + mis + borders, data = data1))
summary(lm(aids_d_1990 ~ colpower_num + colonset + colend + violtot + firstcen + invest + mining + forpre + mis + borders, data = data1))

summary(lm(hiv_inf_1990 ~ colpower_num + colonset + colend + violtot + domform + firstcen + ethnfunc + tradepol + invest + plantat + gold + mining + forpre + workimm + mis + borders, data = data1))
summary(lm(hiv_inf_1990 ~ colpower_num + colonset + colend + violtot + domform + firstcen + ethnfunc + tradepol + plantat + gold + mining + mis + borders, data = data1))

##2SLS estimation
install.packages("AER")
install.packages("systemfit")
library(AER)
library(systemfit)


summary(ivreg(gender_sexuality ~ colpower | aids_d_1990 + hiv_inf_1990))

###old test regressions####
    summary(lm(ilga_index ~ violcol + violres + violind + violtot, data = merge1))
    summary(lm(state_gender_index ~ violcol + violres + violind + violtot, data = merge1))
    summary(lm(gender_sexuality ~ violcol + violres + violind + violtot, data = merge1))
    
    
    summary(lm(ilga_index ~ domform + firstcen + ethnfunc + powertransf, data = merge1))
    summary(lm(gender_sexuality ~ domform + firstcen + ethnfunc + powertransf, data = merge1)) #*domform, .firstcen
    summary(lm(ilga_index ~ tradepol + invest + plantat + gold, data = merge1))
    summary(lm(gender_sexuality ~ tradepol + invest + plantat + gold, data = merge1))
    summary(lm(ilga_index ~ mining + forpre + workimm + mis + borders, data = merge1)) ##forpre**
    summary(lm(gender_sexuality ~ mining + forpre + workimm + mis + borders, data = merge1))
    summary(lm(ilga_index ~ econ_transformation + soc_transformation + col_transformation, data = merge1))
    summary(lm(gender_sexuality ~ econ_transformation + soc_transformation + col_transformation, data = merge1))
    summary(lm(gender_sexuality ~ mis, data = merge1)) #*mis with POSITIVE coefficient
    
    
     lm1 <- lm(ilga_index ~ forpre, data = merge1)
    plot_model(lm1, type = "pred") ##Höherer Bevölkerungsanteil der Kolonialmacht signifikant positiv linear korreliert mit höheren ILGA Wert (gut für LGBT rights)
    
    
    summary(lm(ilga_index ~ colpower + colonset + colend + colyears, data = merge1)) ##colend***, colonset**
    summary(lm(gender_sexuality ~ colpower + colonset + colend + colyears, data = merge1)) ##colend**, colonset*
    lm2 <- lm(gender_sexuality ~ colpower + colonset + colend + colyears, data = merge1)
    plot_model(lm2, type = "pred") ##perfectly horizontal...
    plot_model(lm2, type = "slope") ##1.the later the end of colonialism (post-1975), 2. the earlier the onset, 
                                    ##3. (specific colpower), 4. the longer the colyears, the HIGHER the index.
    lm3 <- lm(gender_sexuality ~ colpower_num + colonset + colend + colyears, data = merge1) ##colend***, colonset**
    summary(lm3) #*colend, *colonset
    plot_model(lm3, type = "slope") ##colpower_num: only P very positive >> remove from sample and see changes

##remove countries colonised by Portugal###
    merge2 <- merge1[merge1$colpower_num != 5,]
    plot_model(lm(gender_sexuality ~ colpower + colonset + colend + colyears, data = merge2), type = "slope") ##colpower_num: only P very positive >> remove from sample and see changes
    summary(lm(ilga_index ~ colpower_num + colonset + colend + colyears, data = merge2)) #colonset*, colend**
    summary(lm(ilga_index ~ mis + forpre, data = merge2)) #forpre**, mis.
    summary(lm(ilga_index ~ mis + forpre + colonset + colend, data = merge2)) #forpre., colonset., colend*

##remove South Africa
    merge3 <- merge2[-(41),] ##44-3
    plot_model(lm(gender_sexuality ~ colpower + colonset + colend + colyears, data = merge3), type = "slope") ##colpower_num: only P very positive >> remove from sample and see changes
    summary(lm(gender_sexuality ~  mis + forpre + violind + domform + ethnfunc + 
                   invest + gold + mining, data = merge3)) ## **forpre, *invest, .ethnfunc
    plot(lm(gender_sexuality ~  mis + forpre + violind + domform + ethnfunc + 
                invest + gold + mining, data = merge3))
    ##outliers: 4, 11, 45, 48
    merge4 <- merge3[-c(4, 11, 45, 48),] 
    plot(lm(gender_sexuality ~  mis + forpre + violind + domform + ethnfunc + 
                invest + gold + mining, data = merge4))
    plot
    

    ###plot geographic distribution##
    map <- st_read("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/africa shapefile/afr_g2014_2013_0.shp")
    unique(map$ADM0_NAME)
    map$ADM0_NAME <- gsub("C�te d'Ivoire", "Côte d'Ivoire", map$ADM0_NAME)#C�te d'Ivoire
    names(map)[names(map) == "ADM0_NAME"] <- "country"
    map_lgbt <- full_join(map, lgbtlaws2019)
    head(map_lgbt)
    unique(map_lgbt$country)
    
    
     ggplot(map_lgbt) + 
        geom_sf(aes(fill = gender_sexuality)) +
        scale_fill_gradient(low = "honeydew3", high = "brown4", na.value = "gray100") ##too much time, doesn't load
    
    
    ###DV2: Attitudes (AFROBAROMETER) ####
afrobar <- read.spss("/Users/sara/Documents/FS21/Kolonialismus in Afrika und Asien/Seminar Paper/Data/Dependent Variable(s)/Afrobarometer/Round 7 2019 merged.sav", 
                     use.value.labels = TRUE, to.data.frame	= TRUE)
     
     plot(afrobar$Q86A)
     unique(afrobar$COUNTRY)
     
     ##1: Age, 998=Refused, 999=Don’t Know , -1=Missing
     ##Q86A: In the past year, how often, if at all, have you personally been discriminated against based on any of the following: Your gender?
     ##87C:Homosexuals as neighbours
     ##97: Education of respondent.  0=No formal schooling, 1=Informal schooling only (including Koranic schooling), 2=Some primary schooling, 3=Primary school completed, 4=Intermediate school or Some secondary school / high school, 5=Secondary school / high school completed, 6=Post-secondary qualifications, other than university e.g. a diploma or degree from a polytechnic or college, 7=Some university, 8=University completed, 9=Post- graduate, 99=Don’t know [Do not read], 98=Refused to answer, -1=Missing
     ##98: Religion of respondent?
     ##101: Gender of respondent. 1=Male, 2=Female, -1=Missing
     ##115: Rural or urban
     ##118: 
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
    
    
   # unique(afrobar$Q98) #71 religions....
    
    
    afrobar$Q115 <- gsub("Rural", "1", afrobar$Q115)
    afrobar$Q115 <- gsub("Semi-Urban", "2", afrobar$Q115)
    afrobar$Q115 <- gsub("Urban", "3", afrobar$Q115) ##coded 2 and 3 opposite to afrobarometer codebook
   
    afrobar$Q118 <- gsub("Primary school completed", "3", afrobar$Q118)
    afrobar$Q118 <- gsub("Some secondary school / high school", "4", afrobar$Q118)
    afrobar$Q118 <- gsub("High school completed", "5", afrobar$Q118)
    afrobar$Q118 <- gsub("Post-secondary qualifications, other than university", "6", afrobar$Q118)
    afrobar$Q118 <- gsub("Some university", "7", afrobar$Q118)
    afrobar$Q118 <- gsub("University completed", "8", afrobar$Q118)
    afrobar$Q118 <- gsub("Post graduate", "9", afrobar$Q118)
    
    afrobar$Q101 <- gsub("Female", "0", afrobar$Q101)
    afrobar$Q101 <- gsub("Male", "1", afrobar$Q101)
    
    afrobar$Q1 <- as.integer(afrobar$Q1)
    afrobar$Q87C <- as.integer(afrobar$Q87C)
    afrobar$Q97 <- as.integer(afrobar$Q97)
    afrobar$Q115 <- as.integer(afrobar$Q115)
    afrobar$Q118 <- as.integer(afrobar$Q118)
    afrobar$Q101 <- as.integer(afrobar$Q101)
    afrobar$Q98.1 <- NULL

    #unique(afrobar$COUNTRY)


    data2 <- merge(data1, afrobar, by.x = "country", by.y = "COUNTRY" ) ##29k observations
    unique(data2$Q98) #57 religions...
    unique(data2$country) #N = 21
    data2$Q118 <- NULL
    data2 <- data2 %>%
        rename(
            lgbt = Q87C,age = Q1, education = Q97, male = Q101, urban = Q115, religion = Q98)
    
    model1 <- lm(lgbt ~ age + education + male + urban, data = data2)
    
    summary(model1)
    plot_model(model1, type = "slope")

    
    summary(lm(Q87C ~ Q1 + Q97 + Q101 + Q115 + Q118, data = data2), robust =T)
    
    plot(lm(Q87C ~ Q1 + Q97 + Q101 + Q115 + Q118,
               data = data2)) ##might be homoscedasticity, carry out bp/ncv test
    
            bptest(lm(Q87C ~ Q1 + Q97 + Q101 + Q115 + Q118, data = data2)) #H0 (Homoskedasticity) rejected for p-value < 0.05. Homosk. exists.
            ncvTest(lm(Q87C ~ Q1 + Q97 + Q101 + Q115 + Q118, data = data2)) #HOMOSKEDASTICITY
            
            #Solution: robust standard errors##
            coeftest(lm(Q87C ~ Q1 + Q97 + Q101 + Q115 + Q118, data = data2), vcovHC(lm(Q87C ~ Q1 + Q97 + Q101 + Q115 + Q118, data = data2), type = "HC0"))
            vcovHC()    
            
    summary(lm(lgbt ~ religion, data = data2)) 
            #.+Apostolic/ New Apostolic/ Old Apostolic
            #*+Assembly of God
            #**+Bahai 
            #*+Don't know 
            #*+Hindu
            #***+Hisbulah Mission
            #*+Izala
            #*+Refused
            #. Zionist Christian Church                   
            
    summary(lm(Q87C ~ Q1 + Q97 + Q101 + Q115 + Q118 + colonset + colend,
               data = data2))
    
    summary(lm(Q87C ~ Q1 + Q97 + Q101 + Q115 + Q118 + colonset + colend + domform + firstcen + ethnfunc
               + tradepol + invest + plantat + gold + mining + forpre + workimm + mis + borders + colpower,
               data = data2))
    plot(lm(Q87C ~ Q1 + Q97 + Q101 + Q115 + Q118 + colonset + colend + domform + firstcen + ethnfunc
             + tradepol + invest + plantat + gold + mining + forpre + workimm + mis + borders + colpower,
             data = data2))
    
    ### Causal chain: colonialism >> variation in health infrastructure >> variation in HIV/AIDS intensity >> variation in anti-LGBT attitudes
    summary(lm(lgbt ~ aids_d_1990 + hiv_inf_1990, data = data2))
    summary(lm(lgbt ~ aids_d_1990 + hiv_inf_1990 + invest, data = data2))
    summary(lm(lgbt ~ aids_d_1990 + hiv_inf_1990 + invest + age + education + male + urban, data = data2))
    summary(lm(lgbt ~ aids_d_1990 + hiv_inf_1990 + invest + mis + pol_transformation + econ_transformation 
               + soc_transformation + col_transformation + age + education + male + urban, data = data2))
    summary(lm(lgbt ~ aids_d_1990 + hiv_inf_1990 + pol_transformation + econ_transformation 
               + soc_transformation + col_transformation + age + education + male + urban, data = data2))
    
