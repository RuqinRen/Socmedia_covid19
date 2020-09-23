library(dplyr)
library(openxlsx)
install.packages("hablar")
library(hablar)
install.packages('stringr')
library(stringr)
getwd()
wave1 <- read.csv("wave1.csv")
wave2 <- read.csv("wave2.csv")
names(wave2)
dim(wave2)
names(wave1)

#combine respondents who participated in both waves
data150 <- wave2 %>% inner_join(wave1, by=c("RecipientEmail" = "Q60.1"))
data150 <- unique(data150)
names(data150)
data_twowave <- data150[,c(12,18:50,68:162)]
twowave <- subset(data_twowave, select = -c(Q60.x))
twowave <- rename(twowave,
                  #newname = oldname
                wave2_area_origin = Q62.x,
                wave2_f2fnumber = Q33.x,
                wave2_importance_socmedia = Q20.x,
                wave2_hour_socmedia = Q21_1.x,
                wave2_hour_socmedia_news =Q27_1.x,
                wave2_multiple_media = Q23.x,
                wave2_covid_us = Q74.x,
                wave2_covid_home = Q75.x,
                wave2_covidnews_en =Q76.x,
                wave2_covidnew_nonen = Q77.x,
                
                wave2_gss_1 =Q30_1.x,
                wave2_gss_2 =Q30_2.x,
                wave2_gss_3 =Q30_3.x,
                wave2_gss_4 =Q30_4.x,
                wave2_gss_5 =Q30_5.x,
                wave2_gss_6 =Q30_6.x,
                
                wave2_dep_1 =Q57_1.x,
                wave2_dep_2 =Q57_2.x,
                wave2_dep_3 =Q57_3.x,
                wave2_dep_4 =Q57_4.x,
                wave2_dep_5 =Q57_5.x,
                wave2_dep_6 =Q57_6.x,
                wave2_dep_7 =Q57_7.x,
                wave2_dep_8 =Q57_8.x,
                
                wave2_anx_1 =Q58_1.x,
                wave2_anx_2 =Q58_2.x,
                wave2_anx_3 =Q58_3.x,
                wave2_anx_4 =Q58_4.x,
                wave2_anx_5 =Q58_5.x,
                wave2_anx_6 =Q58_6.x,
                wave2_anx_7 =Q58_7.x,
                
                RecipientEmail_duplicate = Q60.1,
                consent_duplicate = Q60.y,
                area_duplicate = Q62.y,
                wave1_f2fnumber = Q33.y,
                wave1_importance_socmedia = Q20.y,
                wave1_hour_socmedia =Q21_1.y,
                wave1_hour_socmedia_news =Q27_1.y,
                wave1_multiple_media = Q23.y,
                wave1_covid_us = Q74.y,
                wave1_covid_home = Q75.y,
                wave1_covidnews_en =Q76.y,
                wave1_covidnew_nonen = Q77.y,
                wave1_gss_1 =Q30_1.y,
                wave1_gss_2 =Q30_2.y,
                wave1_gss_3 =Q30_3.y,
                wave1_gss_4 =Q30_4.y,
                wave1_gss_5 =Q30_5.y,
                wave1_gss_6 =Q30_6.y,
                
                person1 = Q29_1,
                person2 = Q29_2,
                person3 = Q29_3,
                person4 = Q29_4,
                person5 = Q29_5,
                person6 = Q29_6,
                person1_nature = Q31,
                person1_rank_f2f=Q68_1,
                person1_rank_onlinecall=Q68_2,
                person1_rank_onlinevideo=Q68_3,
                person1_rank_text=Q68_4,
                person1_frequency = Q34,
                person1_like = Q35,
                person1_close = Q36,
                
                person2_nature = Q37,
                person2_rank_f2f=Q69_1,
                person2_rank_onlinecall=Q69_2,
                person2_rank_onlinevideo=Q69_3,
                person2_rank_text=Q69_4,
                person2_frequency = Q39,
                person2_like = Q40,
                person2_close = Q41,
                
                person3_nature = Q42,
                person3_rank_f2f=Q70_1,
                person3_rank_onlinecall=Q70_2,
                person3_rank_onlinevideo=Q70_3,
                person3_rank_text=Q70_4,
                person3_frequency = Q44,
                person3_like = Q45,
                person3_close = Q46,
                
                person4_nature = Q47,
                person4_rank_f2f=Q71_1,
                person4_rank_onlinecall=Q71_2,
                person4_rank_onlinevideo=Q71_3,
                person4_rank_text=Q71_4,
                person4_frequency = Q49,
                person4_like = Q50,
                person4_close = Q51,
                
                person5_nature = Q52,
                person5_rank_f2f=Q72_1,
                person5_rank_onlinecall=Q72_2,
                person5_rank_onlinevideo=Q72_3,
                person5_rank_text=Q72_4,
                person5_frequency = Q54,
                person5_like = Q55,
                person5_close = Q56,
                
                person6_nature = Q62.1,
                person6_rank_f2f=Q73_1,
                person6_rank_onlinecall=Q73_2,
                person6_rank_onlinevideo=Q73_3,
                person6_rank_text=Q73_4,
                person6_frequency = Q64,
                person6_like = Q65,
                person6_close = Q66,
                
                wave1_dep_1 =Q57_1.y,
                wave1_dep_2 =Q57_2.y,
                wave1_dep_3 =Q57_3.y,
                wave1_dep_4 =Q57_4.y,
                wave1_dep_5 =Q57_5.y,
                wave1_dep_6 =Q57_6.y,
                wave1_dep_7 =Q57_7.y,
                wave1_dep_8 =Q57_8.y,
                
                wave1_anx_1 =Q58_1.y,
                wave1_anx_2 =Q58_2.y,
                wave1_anx_3 =Q58_3.y,
                wave1_anx_4 =Q58_4.y,
                wave1_anx_5 =Q58_5.y,
                wave1_anx_6 =Q58_6.y,
                wave1_anx_7 =Q58_7.y,
                
                newssource_lang = Q67,
                year = Q6,
                sex = Q8,
                degree = Q9,
                year_US = Q10,
                english = Q15,
                freq_home = Q19,
                firstgen = Q12,
                family_US = Q13,
                )

twowave <- twowave[-c(33,34)]

#remove those who chose "other" in areas of origin
twowave <- twowave[-which(twowave$wave2_area_origin == "Other"),]

#convert survey scales to ordered numbers
a <- twowave %>% 
   mutate_at(vars(ends_with("importance_socmedia")), 
             funs("rc" = recode(.,
                                "None at all" = 1,
                                "A little important"  = 2,
                                "Moderately importantly"  = 3,
                                "Very important"= 4,
                                "Extremely important" = 5))) %>%
   mutate_at(vars(contains("_covid_")), 
             funs("rc" = recode(.,
                                "Not at all severe" = 1,
                                "Not so severe"  = 2,
                                "Somewhat severe"  = 3,
                                "Very severe"= 4,
                                "Extremely severe" = 5))) %>%
   mutate_at(vars(contains("_covidnews_")), 
             funs("rc" = recode(.,
                                "Very rare" = 1,
                                "Once a week"  = 2,
                                "Several times a week"  = 3,
                                "Once a day"= 4,
                                "Several times a day" = 5))) %>%
   mutate_at(vars(ends_with("_frequency")), 
             funs("rc" = recode(.,
                                "Never" = 1,
                                "Rarely"  = 2,
                                "Sometimes"  = 3,
                                "Frequently"= 4,
                                "Always" = 5))) %>%
   mutate_at(vars(ends_with("_like")), 
             funs("rc" = recode(.,
                                "Do not like at all" = 1,
                                "Like a little"  = 2,
                                "Like somewhat"  = 3,
                                "Like a great deal"= 4,
                                "Like a lot" = 5))) %>%
   mutate_at(vars(ends_with("_close")), 
             funs("rc" = recode(.,
                                "Not at all close" = 1,
                                "Slightly close"  = 2,
                                "Moderately close"  = 3,
                                "Very close"= 4,
                                "Extremely close" = 5))) 


a <- a%>%
   mutate_at(vars(contains("_anx_")), 
             funs("rc" = recode(.,
                                "Not at all" = 1,
                                "Several days"  = 2,
                                "More than half the days"  = 3,
                                "Nearly every day"= 4))) %>%
   mutate_at(vars(contains("_dep_")), 
             funs("rc" = recode(.,
                                "Not at all" = 1,
                                "Several days"  = 2,
                                "More than half the days"  = 3,
                                "Nearly every day"= 4)))  %>% 
   mutate_at(vars(contains("year_US")), 
                   funs("year_US_rc" = recode(.,
                                      "Less than 1 year" = 1,
                                      "1-2 years"  = 2,
                                      "2-3 years"  = 3,
                                      "3-4 years"= 4,
                                      "More than 4 years" = 5))) 


write.csv(a, "twowave_renamed.csv")
twowave <- read.csv("twowave_renamed.csv")

#f2fnumber, convert to numeric type
#manually correct numbers

twowave[5,"wave2_f2fnumber"] <- 20
twowave[29,"wave2_f2fnumber"] <- 4
twowave[47,"wave2_f2fnumber"] <- 9
twowave[58,"wave2_f2fnumber"] <- 0

twowave[,"wave2_f2fnumber"] <- as.numeric(twowave[,"wave2_f2fnumber"] )
twowave[147,"wave2_f2fnumber"] <- 52

twowave[21,"wave1_f2fnumber"] <- 8
twowave[42,"wave1_f2fnumber"] <- 6
twowave[50,"wave1_f2fnumber"] <- 8
twowave[72,"wave1_f2fnumber"] <- 60
twowave[81,"wave1_f2fnumber"] <- 8
twowave[127,"wave1_f2fnumber"] <- 100
twowave[131,"wave1_f2fnumber"] <- 25
twowave[140,"wave1_f2fnumber"] <- 20
twowave[149,"wave1_f2fnumber"] <- 10


#hablar package
#convert data type
twowave <- twowave %>% 
   convert(num(starts_with("wave2_gss"), 
               starts_with("wave2_dep"),
               starts_with("wave2_anx"),
               starts_with("wave1_gss"), 
               starts_with("wave1_dep"),
               starts_with("wave1_anx"),
               wave1_f2fnumber,
               wave1_hour_socmedia,
               wave1_hour_socmedia_news,
               wave2_f2fnumber,
               wave2_hour_socmedia,
               wave2_hour_socmedia_news,
               contains("_rank_"),
               year,
               year_US
   )
   ) %>%
   as.data.frame()

#adjust year
twowave[107,"year"] <- 1985
twowave[7,"year"] <-1995
twowave[114,"year"] <-1997
twowave[140,"year"] <-1995
twowave[43,"year"] <-1996
twowave[106,"year"] <-2000

twowave[,"year"] <- 2020 -twowave[,"year"]
mean(twowave[,"year"], na.rm = TRUE)
#fill in NAs with mean value
twowave[is.na(twowave[,"year"]), "year"] <- c(25.5, 25.5, 25.5)

twowave <- twowave[-c(19:33, 104:118)]
#aggregate multi-item questions
 wave2_gss_avg <- twowave %>% select(starts_with('wave2_gss')) %>%
   transmute(wave2_gss_avg = rowMeans(., na.rm = TRUE))%>% 
   as.data.frame()
 wave2_dep_avg <- twowave %>% select(starts_with('wave2_dep')) %>%
   transmute(wave2_dep_avg = rowMeans(., na.rm = TRUE))%>% 
   as.data.frame()
 wave2_anx_avg <- twowave %>% select(starts_with('wave2_anx')) %>%
   transmute(wave2_anx_avg = rowMeans(., na.rm = TRUE))%>% 
   as.data.frame()
 
 wave1_gss_avg <- twowave %>% select(starts_with('wave1_gss')) %>%
   transmute(wave1_gss_avg = rowMeans(., na.rm = TRUE))%>% 
   as.data.frame()
 wave1_dep_avg <- twowave %>% select(starts_with('wave1_dep')) %>%
   transmute(wave1_dep_avg = rowMeans(., na.rm = TRUE))%>% 
   as.data.frame()
 wave1_anx_avg <- twowave %>% select(starts_with('wave1_anx')) %>%
   transmute(wave1_anx_avg = rowMeans(., na.rm = TRUE))%>% 
   as.data.frame()
 
twowave <- cbind(twowave, wave2_gss_avg,wave2_dep_avg,wave2_anx_avg, wave1_gss_avg, wave1_dep_avg, wave1_anx_avg )
twowave <- twowave[-c(1,2,13:18,29:34, 124:153)] #remove multi-item questions

#fill in NAs to empty cells
twowave[twowave==""]<-NA
netsize <- twowave %>% select(person1, person2, person3,person4,person5,person6) %>% transmute(netsize = rowSums(!is.na(.))) %>% as.data.frame()
twowave <- cbind(twowave, netsize)
twowave <- twowave[-c(21:26)]

#english
twowave <- twowave %>% 
   mutate_at(vars(contains("english")), 
                  funs("english_rc" = recode(.,
                                     "Not well at all" = 1,
                                     "Slightly well"  = 2,
                                     "Moderately well"  = 3,
                                     "Very well"= 4,
                                     "Extremely well" = 5))) %>%
   mutate_at(vars(contains("freq_home")), 
             funs("english_rc" = recode(.,
                                        "Fewer than once a year" = 1,
                                        "1-2 times a year"  = 2,
                                        "3-4 times a year"  = 3,
                                        "5-6 times a year"= 4,
                                        "More than 6 times a year" = 5))) 

twowave <- twowave %>% rename(year_US_rc = rc)

write.csv(twowave, "twowave_renamed2.csv")
twowave <- read.csv("twowave_renamed2.csv")


#count mainstream American social media
#count multiple platform
twowave[,"wave2_multiple_media"] <- as.character(twowave[,"wave2_multiple_media"] )
twowave[,"wave2_multiple_media_US"] <- str_count(twowave[,"wave2_multiple_media"], "Facebook|Instagram|Twitter|Snapchat")
twowave[,"wave2_multiple_media"]  <- str_count(twowave[,"wave2_multiple_media"], ',') + 1
twowave[,"wave2_multiple_media_nonUS"] <- twowave[,"wave2_multiple_media"] - twowave[,"wave2_multiple_media_US"]

twowave[,"wave1_multiple_media"] <- as.character(twowave[,"wave1_multiple_media"] )
twowave[,"wave1_multiple_media_US"] <- str_count(twowave[,"wave1_multiple_media"], "Facebook|Instagram|Twitter|Snapchat")
twowave[,"wave1_multiple_media"]  <- str_count(twowave[,"wave1_multiple_media"], ',') + 1
twowave[,"wave1_multiple_media_nonUS"] <- twowave[,"wave1_multiple_media"] - twowave[,"wave1_multiple_media_US"]

#adjust network related variables

a <- 
 twowave %>% 
   select(ends_with("rank_f2f")) %>%
    transmute(f2f_connection_top1 = rowSums(. == 1, na.rm = T)) 
 
b <- 
   twowave %>% 
   select(ends_with("rank_f2f")) %>%
   transmute(f2f_connection_top2 = rowSums(. == 1 | .==2, na.rm = T) ) 

twowave$f2f_connection_top1 <- a$f2f_connection_top1
twowave$f2f_connection_top2 <- b$f2f_connection_top2

twowave$online_netsize  <- twowave_rm$netsize - twowave_rm$f2f_connection_top1

#weighted richness for each individual
f2f_richness <- 4
onlinevideo_richness <- 3
onlinecall_richness <- 2
text_richness <- 1

twowave$person1_richness_weighted <- 
   (5-twowave$person1_rank_f2f) * f2f_richness + 
   (5-twowave$person1_rank_onlinecall) * onlinecall_richness +
   (5-twowave$person1_rank_onlinevideo) * onlinevideo_richness +
   (5-twowave$person1_rank_text) * text_richness 


twowave$person2_richness_weighted <- 
   (5-twowave$person2_rank_f2f) * f2f_richness + 
   (5-twowave$person2_rank_onlinecall) * onlinecall_richness +
   (5-twowave$person2_rank_onlinevideo) * onlinevideo_richness +
   (5-twowave$person2_rank_text) * text_richness 


twowave$person3_richness_weighted <- 
   (5-twowave$person3_rank_f2f) * f2f_richness + 
   (5-twowave$person3_rank_onlinecall) * onlinecall_richness +
   (5-twowave$person3_rank_onlinevideo) * onlinevideo_richness +
   (5-twowave$person3_rank_text) * text_richness 


twowave$person4_richness_weighted <- 
   (5-twowave$person4_rank_f2f) * f2f_richness + 
   (5-twowave$person4_rank_onlinecall) * onlinecall_richness +
   (5-twowave$person4_rank_onlinevideo) * onlinevideo_richness +
   (5-twowave$person4_rank_text) * text_richness 


twowave$person5_richness_weighted <- 
   (5-twowave$person5_rank_f2f) * f2f_richness + 
   (5-twowave$person5_rank_onlinecall) * onlinecall_richness +
   (5-twowave$person5_rank_onlinevideo) * onlinevideo_richness +
   (5-twowave$person5_rank_text) * text_richness 


twowave$person6_richness_weighted <- 
   (5-twowave$person6_rank_f2f) * f2f_richness + 
   (5-twowave$person6_rank_onlinecall) * onlinecall_richness +
   (5-twowave$person6_rank_onlinevideo) * onlinevideo_richness +
   (5-twowave$person6_rank_text) * text_richness 

   
#overall closeness
a <- twowave %>% select(ends_with('_close_rc')) %>% mutate(avg_close = rowMeans(., na.rm = T))

#overall liking
b <-  twowave %>% select(ends_with('_like_rc')) %>% mutate(avg_like = rowMeans(., na.rm = T))

twowave$avg_close <- a$avg_close
twowave$avg_like <- b$avg_like

write.csv(twowave, "twowave0922.csv")
twowave <- read.csv("twowave0922.csv")


