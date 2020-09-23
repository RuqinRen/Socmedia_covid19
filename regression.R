hist(twowave$wave2_anx_avg)
hist(twowave$wave2_dep_avg)
hist(twowave$wave2_gss_avg)

hist(twowave$wave1_anx_avg)
hist(twowave$wave1_dep_avg)
hist(twowave$wave1_gss_avg)


twowave$hour_socmedia_diff = twowave$wave2_hour_socmedia - twowave$wave1_hour_socmedia
twowave$wave1_hour_socmedia_nonnews = twowave$wave1_hour_socmedia - twowave$wave1_hour_socmedia_news
twowave$wave2_hour_socmedia_nonnews = twowave$wave2_hour_socmedia - twowave$wave2_hour_socmedia_news
twowave$diff_gss = twowave$wave2_gss_avg - twowave$wave1_gss_avg

a <-glm(data = twowave, wave2_dep_avg  ~  wave1_dep_avg+ year  + year_US_rc + english_rc + avg_close + avg_like + netsize*wave2_hour_socmedia + wave2_multiple_media_nonUS) 
a <-glm(data = twowave, wave2_dep_avg  ~  wave1_dep_avg + year + wave1_hour_socmedia*wave1_f2fnumber  )
a <-glm(data = twowave, netsize ~  wave1_dep_avg + year + wave1_hour_socmedia )
a <-glm(data = twowave, online_netsize ~  wave1_dep_avg + year + wave1_hour_socmedia )
a <-glm(data = twowave, wave2_dep_avg  ~  wave1_dep_avg +  wave1_hour_socmedia_news  + wave1_covid_us   )

summary(glm(data= twowave, wave2_dep_avg ~ wave1_dep_avg + wave1_hour_socmedia_news ))

summary(glm(data = twowave, wave2_dep_avg ~ wave1_dep_avg + wave2_gss_avg ))

mediation1 <- ' 
            # direct effect
             wave2_dep_avg ~ wave1_dep_avg+ year  + year_US_rc + english_rc + avg_close + avg_like + wave1_f2fnumber + c*wave1_gss_avg 
           
           # mediator
             wave2_hour_socmedia ~ a*wave1_gss_avg
             wave2_dep_avg ~ b*wave2_hour_socmedia
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '
fit <- sem(mediation1, data = twowave)
summary(fit)

mediation2 <- ' 
            # direct effect
             wave2_dep_avg ~ wave1_dep_avg+ year  + year_US_rc + english_rc + avg_close + avg_like +wave1_f2fnumber + c*netsize 
           
           # mediator
             wave2_hour_socmedia ~ a*netsize
             wave2_dep_avg ~ b*wave2_hour_socmedia
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '
fit <- sem(mediation2, data = twowave)
summary(fit)

mediation3 <- ' 
            # direct effect
             wave2_dep_avg ~ wave1_dep_avg+ year  + year_US_rc + english_rc + avg_close + avg_like +wave2_f2fnumber + c*netsize 
           
           # mediator
             wave2_hour_socmedia_nonnews ~ a*netsize
             wave2_dep_avg ~ b*wave2_hour_socmedia_nonnews
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '

fit <- sem(mediation3, data = twowave)
summary(fit)

mediation4 <- ' 
            # direct effect
                wave2_dep_avg ~ wave1_dep_avg+ year  + year_US_rc + english_rc + avg_close + avg_like + wave1_f2fnumber + c*netsize 
           
           # mediator
             wave2_hour_socmedia_news ~ a*netsize
             wave2_dep_avg ~ b*wave2_hour_socmedia_news
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '

fit <- sem(mediation4, data = twowave)
summary(fit)

mediation5 <- ' 
            # direct effect
             wave2_anx_avg ~ wave1_anx_avg+ year  + year_US_rc + english_rc + avg_close + avg_like + wave1_f2fnumber + c*wave1_gss_avg 
           
           # mediator
             wave2_hour_socmedia ~ a*wave1_gss_avg
             wave2_anx_avg ~ b*wave2_hour_socmedia
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '
fit <- sem(mediation5, data = twowave)
summary(fit)

mediation6 <- ' 
             # direct effect
             wave2_anx_avg ~ wave1_anx_avg+ year  + year_US_rc + english_rc + avg_close + avg_like + wave1_f2fnumber + c*wave1_gss_avg 
           
           # mediator
             wave2_hour_socmedia_nonnews ~ a*wave1_gss_avg
             wave2_anx_avg ~ b*wave2_hour_socmedia_nonnews
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '
fit <- sem(mediation6, data = twowave)
summary(fit)

mediation7 <- ' 
             # direct effect
             wave2_anx_avg ~ wave1_anx_avg+ year  + year_US_rc + english_rc + avg_close + avg_like + wave1_f2fnumber + c*netsize 
           
           # mediator
             wave2_hour_socmedia ~ a*netsize
             wave2_anx_avg ~ b*wave2_hour_socmedia
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '
fit <- sem(mediation7, data = twowave)
summary(fit)

mediation8 <- ' 
             # direct effect
             wave2_anx_avg ~ wave1_anx_avg+ year  + year_US_rc + english_rc + avg_close + avg_like + wave1_f2fnumber + c*netsize 
           
           # mediator
             wave2_hour_socmedia_news ~ a*netsize
             wave2_anx_avg ~ b*wave2_hour_socmedia_news
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '
fit <- sem(mediation8, data = twowave)
summary(fit)

mediation9 <- ' 
             # direct effect
             wave2_anx_avg ~ wave1_anx_avg+ year  + year_US_rc + english_rc + avg_close + avg_like + wave1_f2fnumber + c*netsize 
           
           # mediator
             wave2_hour_socmedia_nonnews ~ a*netsize
             wave2_anx_avg ~ b*wave2_hour_socmedia_nonnews
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '
fit <- sem(mediation9, data = twowave)
summary(fit)
