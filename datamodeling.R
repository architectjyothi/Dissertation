rm(list = ls(all=TRUE))
library(fBasics)
library(lmtest)
library(jtools)
library(dplyr)
library(rpart)
library(rpart.plot)
library(car)
library(DMwR)
library(stargazer)
library(caret)
library(ROCR)

getwd()
data<-  read.csv('/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/CompaniesHouse/MyDataFinal.csv',sep=",", 
                            comment.char = "",check.names = FALSE, quote="\"",
                            na.strings=c("NA","NaN", " "))
sum(is.na(data()))
str(data)
i<-sapply(data, is.factor)
data[i]<-lapply(df[i],as.numeric)

datanum <- data %>% mutate_if(is.factor,as.numeric)
data <-data[-c(1)]

#install.packages("rpart.plot")

lm <-lm(datanum$AllAgepop ~ . , data=datanum)
summary(lm)
#logistic regression
log_reg <- glm(datanum$AllAgepop~., data = datanum)
summary(log_reg)

pred<-predict(lm,newdata = datanum)
nmdata<-mean((pred))
summary(pred)
plot(lm$residuals)

vif(lm)

regr.eval(datanum$AllAgepop, pred)
#install.packages("stargazer")

stargazer(lm, type="text", out="models.txt")

#datanum <- datanum[!is.na(datanum)]
#confusionMatrix( table(prob_train,datanum$AllAgepop) )
str(prob_train)
str(datanum$AllAgepop)
levels(prob_train)
levels(datanum$AllAgepop)

prob_train <- predict(log_reg, type = "response")
predic <- prediction(prob_train, datanum$AllAgepop)
perf <- performance(predic, measure="tpr", x.measure="fpr")
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
##############################################################################################
#FFor the KBI firms, look at the  ONS Science and technology classification 2015.

#http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/rel/regional-trends/london-analysis/identifying-science-and-technology-businesses-in-official-statistics/index.html
# Digital technologies
sic_digital = c(26110,26120,26200,26400,26511,26512,26800,33130,
                58210,58290,62011,62012,62020,62030,62090,63110,
                63120,95110)
# Life Sciences and healthcare
sic_science = c(21100,21200,26600,26701,32500,72110,75000,86101,
                86102,86210,86220,86230,86900)
# Publishing and broadcasting
sic_publishing = c(26301,26309,26702,58110,58120,58130,58141,58142,
                   58190,59111,59112,59113,59120,59131,59132,59133,
                   59140,59200,60100,60200,61100,61200, 61300,61900,
                   63910,63990,73110,73120,73200,74100,74201,74202,
                   74203,74209,95120)
# Other scientific activities of manufacturing
sic_otherScManuf = c(19201,19209,20110,20120,20130,95210,95220,95250,
                     20140,20150,20160,20170,20200,20301,20302,20411,
                     20412,20420,20510,20520,20530,20590,20600,25210,
                     25300,25400,26513,26514,26520,27110,27120,27200,
                     27310,27320,27330,27400,27510,27520,27900,28110,
                     28120,28131,28132,28140,28150,28210,28220,28230,
                     28240,28250,28290,28301,28302,28410,28490,28910,
                     28921,28922,28923,28930,28940,28950,28960,28990,
                     29100,29201,29202,29203,29310,29320,30110,30120,
                     30200,30300,30400,30910,30920,30990,32120,32401,
                     33120,33140,33150,33160,33170)
# Other scientific activity services
sic_otherScServices = c(51101,51102,51210,51220,71111,71112,71121,71122,
                        71129,71200,72190,72200,74901,74902,85410,85421,
                        85422)


#####   From OliverWyman classification of the leisure industry
### http://www.oliverwyman.com/content/dam/oliver-wyman/global/en/files/archive/2012/20120612_BISL_OW_State_of_the_UK_Leisure_Industry_Final_Report.pdf
# Food and accomodation
sic_food = c(55100,55201,55202,55209,55300, 56101,56102,56103,
             56210,56290,56301,56302)
# Entertainment
sic_entertainment = c(59140, 82301,82302, 90010,91011,91012,91020,91030,
                      91040,92000,93110, 93120,93130,93191,93199, 93020)
# Retail except retail trade of motor vehicles
sic_retail = c(47190,47990,47290,47789,47782,47810,47890,47820,
               47781,47110,47791,47430,47300,47250,47610,47240,
               47530,47710,47410,47750,47540,47230,47760,47721,
               47210,47599,47650,47520,47741,47722,47220,47749,
               47421,47630,47591,47620,47799,47640,47429,47510,
               47260,47770,47910)
##############################################################################################
##############################################################################################
#data_digital- First KBI
data_digital <- data[data$SIC_2013  %in% sic_digital, ]
data_digital18 <- data[data$SIC2018  %in% sic_digital, ]
data_digital <- datanum[datanum$SIC_2013  %in% sic_digital, ]
data_digital18 <- datanum[datanum$SIC2018  %in% sic_digital, ]

log_regdd <- glm(data_digital$AllAgepop~., data = data_digital)
#summary(log_regdd)
pred_dd<-predict(log_regdd,newdata = datanum)
nmdata_dd<-mean((pred_dd))
#print(nmdata_dd)
#summary(pred_dd)
plot(log_regdd$residuals)
vif(log_regdd)
lm_dd <-lm(data_digital$AllAgepop ~ . , data=data_digital)
summary(lm_dd)
regr.eval(data_digital$AllAgepop, pred)
vif(lm_dd)
plot(data_digital$AllAgepop,data_digital$SIC_2013,main='Scatterplot')
abline(lm_dd,col=2,lwd=3)
coef(lm_dd)
confint(lm_dd)
anova(lm_dd)
plot(lm_dd$residuals)
par(mfrow=c(2,2))
plot(lm_dd)
jarqueberaTest(lm_dd$residuals)
dwtest(lm_dd)
summ(lm_dd)
stargazer(lm_dd, type="text", out="models_dd_lm.txt")

#lets do for 2018 SIC for data digital
lm_dd18 <-lm(data_digital18$AllAgepop ~ . , data=data_digital18)
summary(lm_dd18)
stargazer(lm_dd, type="text", out="models_dd_lm18.txt")
par(mfrow=c(2,2))
plot(lm_dd18)
###############################################

#data_Science - Second KBI
data_science <- data[data$SIC_2013  %in% sic_science, ]
data_science18 <- data[data$SIC2018  %in% sic_science, ]
data_science <- datanum[datanum$SIC_2013  %in% sic_science, ]
data_science18 <- datanum[datanum$SIC2018  %in% sic_science, ]

#lets do for 2013/2018 SIC for data science
lm_ds <-lm(data_science$AllAgepop ~ . , data=data_science)
lm_ds18 <-lm(data_science18$AllAgepop ~ . , data=data_science18)
stargazer(lm_ds, type="text", out="models_ds_lm.txt")
stargazer(lm_ds18, type="text", out="models_ds_lm18.txt")
par(mfrow=c(2,2))
plot(lm_ds)
plot(lm_ds18)
###############################################
#data_publishing- third KBI
data_publishing <- data[data$SIC_2013  %in% sic_publishing, ]
data_publishing18 <- data[data$SIC2018  %in% sic_publishing, ]
data_publishing <- datanum[datanum$SIC_2013  %in% sic_publishing, ]
data_publishing18 <- datanum[datanum$SIC2018  %in% sic_publishing, ]

#lets do for 2013/2018 SIC for data publishing
lm_dp <-lm(data_publishing$AllAgepop ~ . , data=data_publishing)
lm_dp18 <-lm(data_publishing18$AllAgepop ~ . , data=data_publishing18)
stargazer(lm_dp, type="text", out="models_dp_lm.txt")
stargazer(lm_dp18, type="text", out="models_dp_lm18.txt")
par(mfrow=c(2,2))
plot(lm_dp)
plot(lm_dp18)
###############################################
#data_other manufacturing- fourth KBI
data_othermanu <- data[data$SIC_2013  %in% sic_otherScManuf, ]
data_othermanu18 <- data[data$SIC2018  %in% sic_otherScManuf, ]

data_othermanu <- datanum[datanum$SIC_2013  %in% sic_otherScManuf, ]
data_othermanu18 <- datanum[datanum$SIC2018  %in% sic_otherScManuf, ]

#lets do for 2013/2018 SIC for data othermanufacturing
lm_do <-lm(data_othermanu$AllAgepop ~ . , data=data_othermanu)
lm_do18 <-lm(data_othermanu18$AllAgepop ~ . , data=data_othermanu18)
stargazer(lm_do, type="text", out="models_do_lm.txt")
stargazer(lm_do18, type="text", out="models_do_lm18.txt")
par(mfrow=c(2,2))
plot(lm_do)
plot(lm_do18)
###############################################
#data_otherscience services- fifth KBI
data_otherScSer <- data[data$SIC_2013  %in% sic_otherScServices, ]
data_otherScSer18 <- data[data$SIC2018  %in% sic_otherScServices, ]
data_otherScSer <- datanum[datanum$SIC_2013  %in% sic_otherScServices, ]
data_otherScSer18 <- datanum[datanum$SIC2018  %in% sic_otherScServices, ]
#lets do for 2013/2018 SIC for data otherscience
lm_dosc <-lm(data_otherScSer$AllAgepop ~ . , data=data_otherScSer)
lm_dosc18 <-lm(data_otherScSer18$AllAgepop ~ . , data=data_otherScSer18)
stargazer(lm_dosc, type="text", out="models_dosc_lm.txt")
stargazer(lm_dosc18, type="text", out="models_dosc_lm18.txt")
par(mfrow=c(2,2))
plot(lm_dosc)
plot(lm_dosc18)
###############################################
#data_fifth- sixth KBI
data_food <- data[data$SIC_2013  %in% sic_food, ]
data_food18 <- data[data$SIC2018  %in% sic_food, ]
data_food <- datanum[datanum$SIC_2013  %in% sic_food, ]
data_food18 <- datanum[datanum$SIC2018  %in% sic_food, ]
#lets do for 2013/2018 SIC for data food
lm_df <-lm(data_food$AllAgepop ~ . , data=data_food)
lm_df18 <-lm(data_food18$AllAgepop ~ . , data=data_food18)
stargazer(lm_df, type="text", out="models_df_lm.txt")
stargazer(lm_df18, type="text", out="models_df_lm18.txt")
par(mfrow=c(2,2))
plot(lm_df)
plot(lm_df18)
###############################################

#data_entertainment- seventh KBI
data_enter <- data[data$SIC_2013  %in% sic_entertainment, ]
data_enter18 <- data[data$SIC2018  %in% sic_entertainment, ]
data_enter <- datanum[datanum$SIC_2013  %in% sic_entertainment, ]
data_enter18 <- datanum[datanum$SIC2018  %in% sic_entertainment, ]
#lets do for 2013/2018 SIC for data entertainment
lm_de <-lm(data_enter$AllAgepop ~ . , data=data_enter)
lm_de18 <-lm(data_enter18$AllAgepop ~ . , data=data_enter18)
stargazer(lm_de, type="text", out="models_de_lm.txt")
stargazer(lm_de18, type="text", out="models_de_lm18.txt")
par(mfrow=c(2,2))
plot(lm_de)
plot(lm_de18)
###############################################
#data_retail- eighth KBI
data_retail <- data[data$SIC_2013  %in% sic_retail, ]
data_retail18 <- data[data$SIC2018  %in% sic_retail, ]
data_retail <- datanum[datanum$SIC_2013  %in% sic_retail, ]
data_retail18 <- datanum[datanum$SIC2018  %in% sic_retail, ]
lm_dr <-lm(data_retail$AllAgepop ~ . , data=data_retail)
lm_dr18 <-lm(data_retail18$AllAgepop ~ . , data=data_retail18)
stargazer(lm_dr, type="text", out="models_dr_lm.txt")
stargazer(lm_dr18, type="text", out="models_dr_lm18.txt")
par(mfrow=c(2,2))
plot(lm_dr)
plot(lm_dr18)
###############################################
stargazer(data)
stargazer(data,
          datanum,
          lm,
          lm_dd,
          lm_dd18,
          lm_de,
          lm_de18,
          lm_df,
          lm_df18,
          lm_do,
          lm_do18,
          lm_dosc,
          lm_dosc18,
          lm_dp,
          lm_dp18,
          lm_dr,
          lm_dr18,
          lm_ds,
          lm_ds18, dep.var.labels=c("MyData","myDataNumeric",'modelcomplete',
                                    'Data Digital 2013',
                                    'Data Digital 2018',
                                    'Data entertainment 2013',
                                    'Data entertainment 2018',
                                    'Data food 2013',
                                    'Data food 2018',
                                    'Data othermanufacturing 2013',
                                    'Data othermanufacturing 2018',
                                    'Data otherScience 2013',
                                    'Data otherScience 2018',
                                    'Data publishing 2013',
                                    'Data publishing 2018',
                                    'Data retail 2013',
                                    'Data retail 2018',
                                    'Data science 2013',
                                    'Data science 2018' ),type="text", out="models_all.txt")
write.csv(datanum,file='/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/CompaniesHouse/MyDataFinalnumeric.csv')
correlation.matrix <- cor(datanum)
stargazer(correlation.matrix, title="Correlation Matrix",type="text", out="Correlation Matrix.txt")
library(corrplot)
corrplot(correlation.matrix)
corrplot(correlation.matrix,method = 'pie')
corrplot(correlation.matrix,method = 'number')
corrplot(correlation.matrix, type="upper", order="hclust", tl.col="black", tl.srt=45)
hist(correlation.matrix)
hist(lm$residuals)
hist(lm_dd$residuals)
hist(lm_dd18$residuals)
hist(lm_de$residuals)
hist(lm_de18$residuals)
hist(lm_df$residuals)
hist(lm_df18$residuals)
hist(lm_do$residuals)
hist(lm_do18$residuals)
hist(lm_dosc$residuals)
hist(lm_dosc18$residuals)
hist(lm_dp$residuals)
hist(lm_dp18$residuals)
hist(lm_dr$residuals)
hist(lm_dr18$residuals)
hist(lm_ds$residuals)
hist(lm_ds18$residuals)
hist(lm_ds$coefficients)
hist(lm_ds18$coefficients)

plot(data$AllAgepop,data$SIC_2013,main='scatterplot')
cor(data$AllAgepop,data$SIC_2013) # 0.001649589
plot(log(data$AllAgepop),log(data$SIC_2013),main='scatterplot')
cor(log(data$AllAgepop),log(data$SIC_2013))
str(data)
str(datanum)
datanum$AllAgepop[which(is.nan(datanum$AllAgepop))] = NA
datanum$AllAgepop[which(datanum$AllAgepop==Inf)] = NA
mod <- lm((log(abs(datanum$AllAgepop)) + 
             (log(abs(datanum$SIC_2013)))))
model <- lm(AllAgepop~SIC_2013,data)
model_1 <- lm(AllAgepop~SIC_2013+SIC2018,data)
install.packages("jtools")
library(jtools)
summ(model)
summ(model_1)
log_pop<-log(data$AllAgepop)
sum(is.na(log_pop))
log_sic <- log(data$SIC_2013)
plot(log_sic~log_pop)
fit<-lm(log_sic~log_pop)
mm<-nls(data$AllAgepop ~ data$SIC_2013)
hist(log_pop)
exp(data$AllAgepop)


summary(model)
attributes(model)
model$residuals
plot(data$AllAgepop,data$SIC_2013,main='Scatterplot for data')
abline(model,col=2,lwd=3)

plot(log(data_digital$AllAgepop),log(data_digital$SIC_2013),main='Scatterplot for data_digital')
abline(lm_dd,col=2,lwd=3)
plot(log(data_enter$AllAgepop),log(data_enter$SIC_2013),main='Scatterplot for data_enter')
abline(lm_de,col=2,lwd=3)
plot(log(data_food$AllAgepop),log(data_food$SIC_2013),main='Scatterplot for data_food')
abline(lm_df,col=2,lwd=3)
plot(log(data_retail$AllAgepop),log(data_retail$SIC_2013),main='Scatterplot for data_retail')
abline(lm_dr,col=2,lwd=3)

plot(log(data_othermanu$AllAgepop),log(data_othermanu$SIC_2013),main='Scatterplot for data_othermanu')
abline(lm_do,col=2,lwd=3)
plot(log(data_otherScSer$AllAgepop),log(data_otherScSer$SIC_2013),main='Scatterplot for data_otherScSer')
abline(lm_dosc,col=2,lwd=3)
plot(log(data_publishing$AllAgepop),log(data_publishing$SIC_2013),main='Scatterplot for data_publishing ')
abline(lm_dp,col=2,lwd=3)
plot(log(data_science$AllAgepop),log(data_science$SIC_2013),main='Scatterplot for data_science')
abline(lm_ds,col=2,lwd=3)

coef(model)
confint(model)
anova(model)

par(mfrow=c(2,2))
plot(model)
hist(model$residuals)
hist(model$coefficients)
hist(model$fitted.values)
hist(model$effects)

hist(lm$residuals, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(lm$residuals), col="blue", lwd=2) # add a density estimate with defaults
lines(density(lm$residuals, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

hist(lm_dd$residuals, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(lm_dd$residuals), col="blue", lwd=2) # add a density estimate with defaults
lines(density(lm_dd$residuals, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

hist(lm_de$residuals, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(lm_de$residuals), col="blue", lwd=2) # add a density estimate with defaults
lines(density(lm_de$residuals, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

hist(lm_df$residuals, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(lm_df$residuals), col="blue", lwd=2) # add a density estimate with defaults
lines(density(lm_df$residuals, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

hist(lm_do$residuals, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(lm_do$coefficients), col="blue", lwd=2) # add a density estimate with defaults
lines(density(lm_do$rank, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

hist(lm_dosc$residuals, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(lm_dosc$coefficients), col="blue", lwd=2) # add a density estimate with defaults
lines(density(lm_dosc$rank, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

hist(lm_dp$residuals, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(lm_dp$coefficients), col="blue", lwd=2) # add a density estimate with defaults
lines(density(lm_dp$rank, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

hist(lm_ds$residuals, prob=TRUE, col="grey")# prob=TRUE for probabilities not counts
lines(density(lm_ds$coefficients), col="blue", lwd=2) # add a density estimate with defaults
lines(density(lm_ds$rank, adjust=2), lty="dotted", col="darkgreen", lwd=2) 

#install.packages("fBasics")
#Residuals are Normally Distributed

jarqueberaTest(model$residuals)
#Residuals are independent
#install.packages('lmtest')

dwtest(model)
#Simple Regression Residual Plots
layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot
plot(model$resid~data$AllAgepop[order(data$AllAgepop)],
     main="Pop x Residuals\nfor Simple Regression",
     xlab="Population", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(model$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(model$resid)
qqline(model$resid)

#///////////Multiple Regression Example///////////

multi.fit = lm(AllAgepop~SIC_2013+SIC2018, data=data)
summary(multi.fit)

#Residual Analysis for Multiple Regression
dwtest(multi.fit) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated
jarqueberaTest(multi.fit$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero

#Multiple Regression Residual Plots
layout(matrix(c(1,2,3,4),2,2,byrow=T))
plot(multi.fit$fitted, rstudent(multi.fit),
     main="Multi Fit Studentized Residuals",
     xlab="Predictions",ylab="Studentized Resid",
     ylim=c(-2.5,2.5))
abline(h=0, lty=2)
plot(data$AllAgepop, multi.fit$resid,
     main="Residuals by Population",
     xlab="Population",ylab="Residuals")
abline(h=0,lty=2)
hist(multi.fit$resid,main="Histogram of Residuals")
qqnorm(multi.fit$resid)
qqline(multi.fit$resid)
anova(multi.fit)

lmodel <- lm(log(AllAgepop)~log(SIC_2013),data)
summary(log(datanum$AllAgepop))
all(is.na(log(datanum$AllAgepop)))
which(is.na(datanum))          
