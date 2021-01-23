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

summary(glm(data=twowave, wave2_gss_avg ~ wave1_dep_avg + wave1_hour_socmedia + wave1_f2fnumber + netsize ))
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

#use this
mediation2 <- ' 
            # direct effect
             wave2_dep_avg ~ wave1_dep_avg+ year  + year_US_rc + english_rc +wave1_f2fnumber + c*netsize 
           
           # mediator
             wave2_hour_socmedia ~ a*netsize
             wave2_dep_avg ~ b*wave2_hour_socmedia
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '
fit <- sem(mediation2, data = twowave, se= "bootstrap")
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

#use this
mediation7 <- ' 
            # direct effect
             wave2_anx_avg ~ wave1_anx_avg+ year  + year_US_rc + english_rc + +wave1_f2fnumber + c*netsize 
           
           # mediator
             wave2_hour_socmedia ~ a*netsize
             wave2_anx_avg ~ b*wave2_hour_socmedia
             
           # indirect effect (a*b)
             ab := a*b
             
           # total effect
             total := c + (a*b)
         '
fit <- sem(mediation7, data = twowave, se="bootstrap")
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

mediation10 <- ' 
             # direct effect
             wave2_dep_avg ~ wave1_dep_avg+ year  + year_US_rc + english_rc + wave1_f2fnumber + c*netsize 
             
           # mediator
             wave2_hour_socmedia_nonnews ~ a1*netsize
             wave2_hour_socmedia_news ~ a2*netsize
             wave2_dep_avg ~ b1*wave2_hour_socmedia_nonnews
             wave2_dep_avg ~ b2*wave2_hour_socmedia_news
             
           #covariances
             wave2_hour_socmedia_nonnews ~~ wave2_hour_socmedia_news
             
           # indirect effect (a*b)
             indirect1 := a1*b1
             indirect2 := a2*b2
             
           # total effect
             total := c + (a1*b1) + c(a2*b2)
         '
fit <- sem(mediation10, data = twowave, se = "bootstrap")
summary(fit)

mediation11 <- ' 
             # direct effect
             wave2_anx_avg ~ wave1_anx_avg+ year  + year_US_rc + english_rc + wave1_f2fnumber + c*netsize 
             
           # mediator
             wave2_hour_socmedia_nonnews ~ a1*netsize
             wave2_hour_socmedia_news ~ a2*netsize
             wave2_anx_avg ~ b1*wave2_hour_socmedia_nonnews
             wave2_anx_avg ~ b2*wave2_hour_socmedia_news
             
           #covariances
             wave2_hour_socmedia_nonnews ~~ wave2_hour_socmedia_news
             
           # indirect effect (a*b)
             indirect1 := a1*b1
             indirect2 := a2*b2
             
           # total effect
             total := c + (a1*b1) + c(a2*b2)
         '
fit <- sem(mediation11, data = dat,se = "bootstrap")
summary(fit)


mediation11 <- ' 
             # direct effect
             wave2_anx_avg ~ wave1_anx_avg+ year  + year_US_rc + english_rc + wave1_f2fnumber + c*wave1_f2fnumber 
             
           # mediator
             wave2_hour_socmedia_nonnews ~ a1*wave1_f2fnumber
             wave2_hour_socmedia_news ~ a2*wave1_f2fnumber
             wave2_anx_avg ~ b1*wave2_hour_socmedia_nonnews
             wave2_anx_avg ~ b2*wave2_hour_socmedia_news
             
           #covariances
             wave2_hour_socmedia_nonnews ~~ wave2_hour_socmedia_news
             
           # indirect effect (a*b)
             indirect1 := a1*b1
             indirect2 := a2*b2
             
           # total effect
             total := c + (a1*b1) + c(a2*b2)
         '
fit <- sem(mediation11, data = twowave,se = "bootstrap")
summary(fit)

# mediation19 <- ' 
#              # direct effect
#              wave2_anx_avg ~ year  + year_US_rc + english_rc + avg_close + avg_like + wave1_f2fnumber + c*wave1_anx_avg 
#            
#            # mediator
#              wave2_hour_socmedia_news ~ a*wave1_anx_avg
#              wave2_anx_avg ~ b*wave2_hour_socmedia_news
#              
#            # indirect effect (a*b)
#              ab := a*b
#              
#            # total effect
#              total := c + (a*b)
#          '
# fit <- sem(mediation19, data = twowave, se = "bootstrap")
# summary(fit)


library(xtable)
library(Hmisc)


### print correlation
corstars <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}


dat <- twowave[c("wave2_dep_avg","wave1_dep_avg", "wave2_anx_avg","wave1_anx_avg","netsize","wave2_hour_socmedia" ,"wave2_hour_socmedia_nonnews", "wave2_hour_socmedia_news",
                 "year", "year_US_rc","english_rc","wave1_f2fnumber", "wave1_covid_us_rc" )]

#the f2fnumber column is problematic -- delete rows with value 100 or more
dat <- dat %>% select(wave1_f2fnumber <100)


# to correct for selection bias, use raking method
library(anesrake)



correlationMatrix<- corstars(dat)
xtable <- xtable(correlationMatrix)
print (xtable, type ="html",digits = 2, include.colnames = FALSE, column.labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13))

### print reg tables
library(stargazer)
stargazer(main0,main1,main2,main3,main4,main5,
          title="Linear Regression of Different types of Self-disclosure's Effects on Three Outcome Variables (Main Effects)", 
          align=TRUE,
          type = "html",
          column.sep.width = "0pt",
          no.space = TRUE,
          font.size = "large",
          df = FALSE)


