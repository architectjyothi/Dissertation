rm(list = ls(all=TRUE))

library(plyr)
library(stats)
require(rgdal)
require(sf)
library(ggplot2)
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(geojsonio)
library(tmaptools)
library(reshape2)
library(dplyr)
library(corrplot)
library(lattice)
#?count
getwd()
setwd('/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data')
data<-  read.csv('/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/CompaniesHouse/MyDataFinal.csv',sep=",", 
                 comment.char = "",check.names = FALSE, quote="\"",
                 na.strings=c("NA","NaN", " "))
#count(data,vars =c('SIC_2013'))
#aggregate(data$SIC_2013,data,sum)
#count(rownames(data$SIC_2013))
#freq = as.data.frame(table(data$SIC_2013))
#data_digital <- data[freq$Var1  %in% sic_digital, ]
#plot(as.data.frame(table(data$SIC_2013)))
#count(data_digital,vars = 'SIC_2013')
#count(data_digital,vars = 'SIC2018')

finalshape <-readOGR(dsn = '/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/pointpolygonjoint/',layer='Join_Output')
summary(finalshape)
class(finalshape)
class(data)
#install.packages('spdplyr')
#library(spdplyr)
#ukdata<-finalshape %>% inner_join(data,by=c('postcode'='Postcode'))
head(finalshape@data)
names(finalshape)
#shape <- readOGR(dsn = 'MergedUK/',layer='Expot_clip')

BNG = '+init=epsg:27700'
shapeBNG <-spTransform(finalshape,BNG)
class(shapeBNG)
head(shapeBNG@data)
shapeBNG_SF <- st_as_sf(shapeBNG)
summary(shapeBNG_SF)
#datashape <- fortify(finalshape)
class(shapeBNG_SF)
class(data)
Mydatajoin <- merge(shapeBNG_SF,data,by.x='postcode',by.y = 'Postcode')
class(Mydatajoin)
summary(Mydatajoin)

#count the number of firms
?quadratcount
firm_count <- count(Mydatajoin,Mydatajoin$SIC_2013) %>%
  print()
firm_count <- count(Mydatajoin,vars =c('SIC2018','SUM_AllPeo'))
count(Mydatajoin,vars =c('SIC_2013'))

Mydatajoin %>% count(Mydatajoin$SIC_2013)
plot(tally(group_by(Mydatajoin, SIC_2013)))
write_sf(Mydatajoin, "/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/pointpolygonjoint/Mydatajoin.shp")
pdf <- data.frame(data_digital$SIC_2013,data_digital$SUM_AllPeo)
chisq<-chisq.test(Mydata$SUM_AllPeo,Mydata$SIC_2013)
#observed counts
chisq$observed
chisq$expected
chisq$statistic
chisq$p.value
####################################################################
str(data)
Mydata <- data
Mydata<- read.csv('Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/pointpolygonjoint/Mydatajoin.csv')
head(Mydata)
row.names(Mydata) = Mydata$SIC_2013
table(Mydata$SIC_2013)
  
####################################################################
#######################Data digital for KBI###########################
####################################################################
data_digital <- filter(Mydata,SIC_2013 %in% sic_digital)
df <- data.frame(table(data_digital$SIC_2013))
table(data_digital$SUM_AllPeo)
qplot(df$Var1,log(df$Freq))
pop <- data.frame(tapply(pdf$data_digital.SUM_AllPeo, pdf$data_digital.SIC_2013, sum)) ### contingency table from data.frame : array with named dimnames
total <- cbind(pop,df)
#######################PLots###########################
plot(tapply(pdf$data_digital.SUM_AllPeo, pdf$data_digital.SIC_2013, sum))
plot(log(pdf))
plot(log(pop))
plot(log(data_digital$SUM_AllPeo))
plot(total$Freq,total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..)
plot(log(total$Freq),log(total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..))
abline(h=0,lty=2)
abline(lm(log(total$Freq)~log(total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..), data = total), col = "blue")
#######################model###########################
model = lm(log(total$Freq)~log(total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..), data = total)
summary(model)
plot(model$residuals)
hist(model$residuals)
plot(model)
m=cor((log(total$Freq)),log(total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..))
m=cor(total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..,total$Freq)
str(total)
total$Var1 <- as.numeric(total$Var1)
write.csv('total.csv')
m=cor(total)
corrplot((log(total$Freq)),log(total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..))
corrplot ( m, method = 'number')
corrplot.mixed(m)
####################################################################
#######################Data digital for KBI#########################
######################Data digital 2018 for KBI###########################
####################################################################
data_digital18 <- filter(Mydata,IncorporationDate %in% sic_digital)
df18 <- data.frame(table(data_digital18$IncorporationDate))
pop18 <- data.frame(tapply(data_digital18$SUM_AllPeo, data_digital18$IncorporationDate, sum)) ### contingency table from data.frame : array with named dimnames
total18 <- cbind(pop18,df18)
#######################model###########################
model18 = lm(log(total18$Freq)~log(total18$tapply.data_digital18.SUM_AllPeo..data_digital18.IncorporationDate..), data = total18)
summary(model18)
confint(model18, level=0.95) # CIs for model parameters 
plot(model$residuals)
hist(model$residuals)
plot(model)
m=cor((log(total$Freq)),log(total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..))
m=cor(total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..,total$Freq)
str(total)
total$Var1 <- as.numeric(total$Var1)
write.csv('total.csv')
m=cor(total)
corrplot((log(total$Freq)),log(total$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..))
corrplot ( m, method = 'number')
corrplot.mixed(m)
####################################################################
#######################Data digital for KBI#########################
####################################################################
####################################################################
#######################Science for KBI###########################
####################################################################
data_science <- filter(Mydata,SIC_2013 %in% sic_science)
df_s<- data.frame(table(data_science$SIC_2013))
table(data_science$SUM_AllPeo)
qplot(df_s$Var1,log(df_s$Freq))
pop_s <- data.frame(tapply(data_science$SUM_AllPeo, data_science$SIC_2013, sum)) ### contingency table from data.frame : array with named dimnames
total_s <- cbind(pop_s,df_s)
summary(total_s)
str(total_s)
total_s$pop <- total_s$tapply.data_science.SUM_AllPeo..data_science.SIC_2013..sum.
total_s$tapply.data_science.SUM_AllPeo..data_science.SIC_2013..sum. <- NULL
total_s$Var1 <- as.numeric(total_s$Var1)
#######################PLots###########################
######################Science for KBI###########################
####################################################################
data_science18 <- filter(Mydata,IncorporationDate %in% sic_science)
df_s18<- data.frame(table(data_science18$IncorporationDate))
pop_s18 <- data.frame(tapply(data_science18$SUM_AllPeo, data_science18$IncorporationDate, sum)) ### contingency table from data.frame : array with named dimnames
total_s18 <- cbind(pop_s18,df_s18)
summary(total_s18)
str(total_s)
total_s18$pop <- total_s18$tapply.data_science18.SUM_AllPeo..data_science18.IncorporationDate..
total_s18$tapply.data_science18.SUM_AllPeo..data_science18.IncorporationDate.. <- NULL
total_s18$Var1 <- as.numeric(total_s18$Var1)
model_s18 = lm(log(total_s18$Freq)~log(total_s18$pop), data = total_s18)
summary(model_s18)
confint(model_s18, level=0.95) # CIs for model parameters 
#######################PLots###########################


plot(log(pop))
plot(log(data_science$SUM_AllPeo))
plot(total_s$Freq,total_s$tapply.data_science.SUM_AllPeo..data_science.SIC_2013..sum.)
plot(log(total_s$Freq),log(total_s$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..))
abline(h=3,lty=2)
abline(lm(log(total_s$Freq)~log(total_s$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..), data = total_s), col = "blue")
#######################model###########################
model_s = lm(log(total_s$Freq)~log(total_s$pop), data = total_s)
fit <- glm(log(total_s$pop)+log(total_s$Freq),data=total_s,family=binomial())
coefficients(model_s)
confint(model_s, level=0.95) # CIs for model parameters 
fitted(model_s) # predicted values
residuals(model_s) # residuals
anova(model_s) # anova table 
vcov(model_s) # covariance matrix for model parameters 
influence(model_s) # regression diagnostics
qqnorm(log(total_s$Freq))
qqline(log(total_s$Freq),col='red')
qqnorm(log(total_s$pop))
qqline(log(total_s$pop),col='red')
  summary(model_s$residuals)
plot(model_s)
##########
ggplot(total_s, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq)) +geom_text(label=rownames(total_s))
# Add the regression line
ggplot(total_s, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
    geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_s)$adj.r.squared, 5),"Intercept =",signif(model_s$coefficients[[1]],5 )," Slope =",signif(model_s$coefficients[[2]], 5)," P =",signif(summary(model_s)$coefficients[2,4], 5)))
library(ggplot2)
ggplotRegression(lm(log(pop) ~ log(Freq), data = total_s))

# Remove the confidence interval
ggplot(total_s, aes(y=log(Freq), x=log(pop))) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)
# Loess method
ggplot(total_s, aes(x=log(Freq), y=log(pop))) + 
  geom_point()+
  geom_smooth()
###############
# scatter plot of x and y variables
# color by groups
scatterPlot <- ggplot(total_s,aes(x=log(pop), y=log(Freq), color=total_s$Var1)) + 
  geom_point() + 
  scale_color_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(total_s, aes(log(Freq), fill=total_s$Var1)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(total_s, aes(log(Freq), fill=total_s$pop)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
ydensity
#####

########
summary(model_s)
plot(model$residuals)
hist(model$residuals)
plot(model_s)
cor(total_s)
####################################################################
#######################Data science for KBI###########################
####################################################################
####################################################################
#######################Publishing and others for KBI###########################
####################################################################
data_pub <- filter(Mydata,SIC_2013 %in% sic_publishing)
data_man <- filter(Mydata,SIC_2013 %in% sic_otherScManuf)
data_ser <- filter(Mydata,SIC_2013 %in% sic_otherScServices)
df_p<- data.frame(table(data_pub$SIC_2013))
df_m<- data.frame(table(data_man$SIC_2013))
df_ser<- data.frame(table(data_ser$SIC_2013))

pop_p <- data.frame(tapply(data_pub$SUM_AllPeo, data_pub$SIC_2013, sum))
### contingency table from data.frame : array with named dimnames
pop_m <- data.frame(tapply(data_man$SUM_AllPeo, data_man$SIC_2013, sum))
pop_ser <- data.frame(tapply(data_ser$SUM_AllPeo, data_ser$SIC_2013, sum))

total_p <- cbind(pop_p,df_p)
total_m <- cbind(pop_m,df_m)
total_ser <- cbind(pop_ser,df_ser)

summary(total_s)
str(total_s)
total_p$pop <- total_p$tapply.data_pub.SUM_AllPeo..data_pub.SIC_2013..sum.
total_p$tapply.data_pub.SUM_AllPeo..data_pub.SIC_2013..sum. <- NULL
total_p$Var1 <- as.numeric(total_p$Var1)

total_m$pop <- total_m$tapply.data_man.SUM_AllPeo..data_man.SIC_2013..sum.
total_m$tapply.data_man.SUM_AllPeo..data_man.SIC_2013..sum. <- NULL
total_m$Var1 <- as.numeric(total_m$Var1)

total_ser$pop <- total_ser$tapply.data_ser.SUM_AllPeo..data_ser.SIC_2013..sum.
total_ser$tapply.data_ser.SUM_AllPeo..data_ser.SIC_2013..sum. <- NULL
total_ser$Var1 <- as.numeric(total_ser$Var1)

####################################################################
#######################Publishing and others for KBI 2018###########################
####################################################################
data_pub18 <- filter(Mydata,IncorporationDate %in% sic_publishing)
data_man18 <- filter(Mydata,IncorporationDate %in% sic_otherScManuf)
data_ser18 <- filter(Mydata,IncorporationDate %in% sic_otherScServices)
df_p18<- data.frame(table(data_pub18$IncorporationDate))
df_m18<- data.frame(table(data_man18$IncorporationDate))
df_ser18<- data.frame(table(data_ser18$IncorporationDate))

pop_p18 <- data.frame(tapply(data_pub18$SUM_AllPeo, data_pub18$IncorporationDate, sum))
### contingency table from data.frame : array with named dimnames
pop_m18 <- data.frame(tapply(data_man18$SUM_AllPeo, data_man18$IncorporationDate, sum))
pop_ser18 <- data.frame(tapply(data_ser18$SUM_AllPeo, data_ser18$IncorporationDate, sum))

total_p18 <- cbind(pop_p18,df_p18)
total_m18 <- cbind(pop_m18,df_m18)
total_ser18 <- cbind(pop_ser18,df_ser18)

total_p18$pop <- total_p18$tapply.data_pub18.SUM_AllPeo..data_pub18.IncorporationDate..sum.
total_p18$tapply.data_pub18.SUM_AllPeo..data_pub18.IncorporationDate..sum. <- NULL
total_p18$Var1 <- as.numeric(total_p18$Var1)

total_m18$pop <- total_m18$tapply.data_man18.SUM_AllPeo..data_man18.IncorporationDate..sum.
total_m18$tapply.data_man18.SUM_AllPeo..data_man18.IncorporationDate..sum.<- NULL
total_m18$Var1 <- as.numeric(total_m18$Var1)

total_ser18$pop <- total_ser18$tapply.data_ser18.SUM_AllPeo..data_ser18.IncorporationDate..sum.
total_ser18$tapply.data_ser18.SUM_AllPeo..data_ser18.IncorporationDate..sum. <- NULL
total_ser18$Var1 <- as.numeric(total_ser18$Var1)

model_p18 = lm(log(total_p18$Freq)~log(total_p18$pop), data = total_p18)
summary(model_p18)
confint(model_p18, level=0.95) # CIs for model parameters 
model_m18 = lm(log(total_m18$Freq)~log(total_m18$pop), data = total_m18)
summary(model_m18)
confint(model_m18, level=0.95) # CIs for model parameters 
model_ser18 = lm(log(total_ser18$Freq)~log(total_ser18$pop), data = total_ser18)
summary(model_ser18)
confint(model_ser18, level=0.95) # CIs for model parameters 
#######################PLots###########################
#######################PLots##################################################PLots##################################################PLots###########################
str(data_KBI)
data_KBI$companycategory <- as.numeric(as.character(data_KBI$companycategory))   
str(data_KBI$companycategory)
plot(data_KBI$companycategory)
is.na(data_KBI$companycategory)
KBItest <- na.omit(data_KBI$companycategory)
#######################PLots##################################################PLots###########################
#######################PLots###########################
#######################PLots###########################
#######################PLots###########################

plot(log(pop))
plot(log(data_science$SUM_AllPeo))
plot(total_s$Freq,total_s$tapply.data_science.SUM_AllPeo..data_science.SIC_2013..sum.)
plot(log(total_s$Freq),log(total_s$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..))
abline(h=3,lty=2)
abline(lm(log(total_s$Freq)~log(total_s$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013..), data = total_s), col = "blue")
#######################model###########################
model_p = lm(log(total_p$Freq)~log(total_p$pop), data = total_p)
summary(model_p)
coefficients(model_p)
confint(model_p, level=0.95) # CIs for model parameters 
fitted(model_p) # predicted values
residuals(model_p) # residuals
anova(model_p) # anova table 
vcov(model_p) # covariance matrix for model parameters 
influence(model_p) # regression diagnostics
summary(model_p$residuals)
plot(model_p)
##########
ggplot(total_p, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_p, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_p)$adj.r.squared, 5),"Intercept =",signif(model_p$coefficients[[1]],5 )," Slope =",signif(model_p$coefficients[[2]], 5)," P =",signif(summary(model_p)$coefficients[2,4], 5)))
#############################################

model_m = lm(log(total_m$Freq)~log(total_m$pop), data = total_m)
summary(model_m)
coefficients(model_m)
confint(model_m, level=0.95) # CIs for model parameters 
fitted(model_m) # predicted values
residuals(model_m) # residuals
anova(model_m) # anova table 
vcov(model_m) # covariance matrix for model parameters 
influence(model_m) # regression diagnostics
summary(model_m$residuals)
plot(model_m)
##########
ggplot(total_m, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_m, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_m)$adj.r.squared, 5),"Intercept =",signif(model_m$coefficients[[1]],5 )," Slope =",signif(model_m$coefficients[[2]], 5)," P =",signif(summary(model_m)$coefficients[2,4], 5)))
#############################################
########
model_ser = lm(log(total_ser$Freq)~log(total_ser$pop), data = total_ser)
summary(model_ser)
coefficients(model_ser)
confint(model_ser, level=0.95) # CIs for model parameters 
fitted(model_ser) # predicted values
residuals(model_ser) # residuals
anova(model_ser) # anova table 
vcov(model_ser) # covariance matrix for model parameters 
influence(model_ser) # regression diagnostics
summary(model_ser$residuals)
plot(model_ser)
##########
ggplot(total_ser, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_ser, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_ser)$adj.r.squared, 5),"Intercept =",signif(model_ser$coefficients[[1]],5 )," Slope =",signif(model_ser$coefficients[[2]], 5)," P =",signif(summary(model_ser)$coefficients[2,4], 5)))
#############################################
####################################################################
#######################KBI closed###########################
####################################################################
KBI <- c(26110,26120,26200,26400,26511,26512,26800,33130,
                       58210,58290,62011,62012,62020,62030,62090,63110,
                       63120,95110,21100,21200,26600,26701,32500,72110,75000,86101,
                86102,86210,86220,86230,86900,26301,26309,26702,58110,58120,58130,58141,58142,
                   58190,59111,59112,59113,59120,59131,59132,59133,
                   59140,59200,60100,60200,61100,61200, 61300,61900,
                   63910,63990,73110,73120,73200,74100,74201,74202,
                   74203,74209,95120,19201,19209,20110,20120,20130,95210,95220,95250,
                     20140,20150,20160,20170,20200,20301,20302,20411,
                     20412,20420,20510,20520,20530,20590,20600,25210,
                     25300,25400,26513,26514,26520,27110,27120,27200,
                     27310,27320,27330,27400,27510,27520,27900,28110,
                     28120,28131,28132,28140,28150,28210,28220,28230,
                     28240,28250,28290,28301,28302,28410,28490,28910,
                     28921,28922,28923,28930,28940,28950,28960,28990,
                     29100,29201,29202,29203,29310,29320,30110,30120,
                     30200,30300,30400,30910,30920,30990,32120,32401,
                     33120,33140,33150,33160,33170,51101,51102,51210,
                      51220,71111,71112,71121,71122,71129,71200,72190,72200,
                      74901,74902,85410,85421, 85422)

data_KBI <- filter(Mydata,SIC_2013 %in% KBI)

df_KBI<- data.frame(table(data_KBI$SIC_2013))
pop_KBI <- data.frame(tapply(data_KBI$SUM_AllPeo, data_KBI$SIC_2013, sum))
total_KBI <- cbind(df_KBI,pop_KBI)
total_KBI$pop <- total_KBI$tapply.data_KBI.SUM_AllPeo..data_KBI.SIC_2013..sum.
total_KBI$tapply.data_KBI.SUM_AllPeo..data_KBI.SIC_2013..sum. <- NULL
total_KBI$Var1 <- as.numeric(total_KBI$Var1)
str(total_KBI)
model_KBI = lm(log(total_KBI$Freq)~log(total_KBI$pop), data = total_KBI)
summary(model_KBI)
coefficients(model_KBI)
confint(model_KBI, level=0.95) # CIs for model parameters 
fitted(model_KBI) # predicted values
residuals(model_KBI) # residuals
anova(model_KBI) # anova table 
vcov(model_KBI) # covariance matrix for model parameters 
influence(model_KBI) # regression diagnostics
summary(model_KBI$residuals)
plot(model_KBI)
##########
ggplot(total_KBI, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_KBI, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_KBI)$adj.r.squared, 5),"Intercept =",signif(model_KBI$coefficients[[1]],5 )," Slope =",signif(model_KBI$coefficients[[2]], 5)," P =",signif(summary(model_KBI)$coefficients[2,4], 5)))
#############################################
#############################################
data_KBI18 <- filter(Mydata,IncorporationDate %in% KBI)
df_KBI18<- data.frame(table(data_KBI18$IncorporationDate))
pop_KBI18 <- data.frame(tapply(data_KBI18$SUM_AllPeo, data_KBI18$IncorporationDate, sum))
total_KBI18 <- cbind(df_KBI18,pop_KBI18)
total_KBI18$pop <- total_KBI18$tapply.data_KBI18.SUM_AllPeo..data_KBI18.IncorporationDate..sum.
total_KBI18$tapply.data_KBI18.SUM_AllPeo..data_KBI18.IncorporationDate..sum. <- NULL
total_KBI18$Var1 <- as.numeric(total_KBI18$Var1)
str(total_KBI18)
model_KBI18 = lm(log(total_KBI18$Freq)~log(total_KBI18$pop), data = total_KBI18)
summary(model_KBI18)
coefficients(model_KBI18)
confint(model_KBI18, level=0.95) # CIs for model parameters 
fitted(model_KBI) # predicted values
residuals(model_KBI) # residuals
anova(model_KBI) # anova table 
vcov(model_KBI) # covariance matrix for model parameters 
influence(model_KBI18) # regression diagnostics
summary(model_KBI$residuals)
plot(model_KBI)
##########
####################################################################
#######################KBI closed###########################
####################################################################
####################################################################
#######################Retail open###########################
####################################################################

    
Retail = c(55100,55201,55202,55209,55300, 56101,56102,56103,
             56210,56290,56301,56302,59140, 82301,82302, 90010,91011,91012,91020,91030,
                      91040,92000,93110, 93120,93130,93191,93199, 93020,47190,47990,47290,47789,47782,47810,47890,47820,
               47781,47110,47791,47430,47300,47250,47610,47240,
               47530,47710,47410,47750,47540,47230,47760,47721,
               47210,47599,47650,47520,47741,47722,47220,47749,
               47421,47630,47591,47620,47799,47640,47429,47510,
               47260,47770,47910)

data_Retail <- filter(Mydata,SIC_2013 %in% Retail)
df_Retail<- data.frame(table(data_Retail$SIC_2013))
pop_Retail <- data.frame(tapply(data_Retail$SUM_AllPeo, data_Retail$SIC_2013, sum))
total_Retail <- cbind(df_Retail,pop_Retail)
total_Retail$pop <- total_Retail$tapply.data_Retail.SUM_AllPeo..data_Retail.SIC_2013..sum.
total_Retail$tapply.data_Retail.SUM_AllPeo..data_Retail.SIC_2013..sum. <- NULL
total_Retail$Var1 <- as.numeric(total_Retail$Var1)
str(total_Retail)
model_Retail = lm(log(total_Retail$Freq)~log(total_Retail$pop), data = total_Retail)
summary(model_Retail)
coefficients(model_Retail)
confint(model_Retail, level=0.95) # CIs for model parameters 
fitted(model_Retail) # predicted values
residuals(model_Retail) # residuals
anova(model_Retail) # anova table 
vcov(model_Retail) # covariance matrix for model parameters 
influence(model_Retail) # regression diagnostics
summary(model_Retail$residuals)
plot(model_Retail)
##########
ggplot(total_Retail, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_Retail, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_Retail)$adj.r.squared, 5),"Intercept =",signif(model_Retail$coefficients[[1]],5 )," Slope =",signif(model_Retail$coefficients[[2]], 5)," P =",signif(summary(model_Retail)$coefficients[2,4], 5)))
#############################################
#############################################
data_Retail18 <- filter(Mydata,IncorporationDate %in% Retail)
df_Retail18<- data.frame(table(data_Retail18$IncorporationDate))
pop_Retail18 <- data.frame(tapply(data_Retail18$SUM_AllPeo, data_Retail18$IncorporationDate, sum))
total_Retail18 <- cbind(df_Retail18,pop_Retail18)
total_Retail18$pop <- total_Retail18$tapply.data_Retail18.SUM_AllPeo..data_Retail18.IncorporationDate..
total_Retail18$tapply.data_Retail18.SUM_AllPeo..data_Retail18.IncorporationDate.. <- NULL
total_Retail18$Var1 <- as.numeric(total_Retail18$Var1)
str(total_Retail18)
model_Retail18 = lm(log(total_Retail18$Freq)~log(total_Retail18$pop), data = total_Retail18)
summary(model_Retail18)
coefficients(model_Retail)
confint(model_Retail18, level=0.95) # CIs for model parameters 
fitted(model_Retail) # predicted values
residuals(model_Retail) # residuals
anova(model_Retail) # anova table 
vcov(model_Retail) # covariance matrix for model parameters 
influence(model_Retail) # regression diagnostics
summary(model_Retail$residuals)
plot(model_Retail)
####################################################################
#######################Retail closed###########################
####################################################################
data_food <- filter(Mydata,SIC_2013 %in% sic_food)
df_food<- data.frame(table(data_food$SIC_2013))
pop_food <- data.frame(tapply(data_food$SUM_AllPeo, data_food$SIC_2013, sum))
total_food <- cbind(df_food,pop_food)
total_food$pop <- total_food$tapply.data_food.SUM_AllPeo..data_food.SIC_2013..sum.
total_food$tapply.data_food.SUM_AllPeo..data_food.SIC_2013..sum. <- NULL
total_food$Var1 <- as.numeric(total_food$Var1)
str(total_food)
model_food = lm(log(total_food$Freq)~log(total_food$pop), data = total_food)
summary(model_food)
coefficients(model_Retail)
confint(model_food, level=0.95) # CIs for model parameters 
plot(model_food)
##########
ggplot(total_food, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_food, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_food)$adj.r.squared, 5),"Intercept =",signif(model_food$coefficients[[1]],5 )," Slope =",signif(model_food$coefficients[[2]], 5)," P =",signif(summary(model_food)$coefficients[2,4], 5)))
#############################################
####################################################################
data_food18 <- filter(Mydata,IncorporationDate %in% sic_food)
df_food18<- data.frame(table(data_food18$IncorporationDate))
pop_food18 <- data.frame(tapply(data_food18$SUM_AllPeo, data_food18$IncorporationDate, sum))
total_food18 <- cbind(df_food18,pop_food18)
total_food18$pop <- total_food18$tapply.data_food18.SUM_AllPeo..data_food18.IncorporationDate..
total_food18$tapply.data_food18.SUM_AllPeo..data_food18.IncorporationDate.. <- NULL
total_food18$Var1 <- as.numeric(total_food18$Var1)
str(total_food18)
model_food18 = lm(log(total_food18$Freq)~log(total_food18$pop), data = total_food18)
summary(model_food18)
confint(model_food18, level=0.95) # CIs for model parameters 
plot(model_food)
##########
#############################################
######################Retail closed###########################
####################################################################
data_rr <- filter(Mydata,SIC_2013 %in% sic_retail)
df_rr<- data.frame(table(data_rr$SIC_2013))
pop_rr <- data.frame(tapply(data_rr$SUM_AllPeo, data_rr$SIC_2013, sum))
total_rr <- cbind(df_rr,pop_rr)
total_rr$pop <- total_rr$tapply.data_rr.SUM_AllPeo..data_rr.SIC_2013..sum.
total_rr$tapply.data_rr.SUM_AllPeo..data_rr.SIC_2013..sum. <- NULL
total_rr$Var1 <- as.numeric(total_rr$Var1)
str(total_rr)
model_rr = lm(log(total_rr$Freq)~log(total_rr$pop), data = total_rr)
summary(model_rr)
coefficients(model_rr)
confint(model_rr, level=0.95) # CIs for model parameters 
plot(model_rr)
##########
ggplot(total_rr, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_rr, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_rr)$adj.r.squared, 5),"Intercept =",signif(model_rr$coefficients[[1]],5 )," Slope =",signif(model_rr$coefficients[[2]], 5)," P =",signif(summary(model_rr)$coefficients[2,4], 5)))
#############################################
###########################this is for 2013 as incorporatio date#########################################
data_rr18 <- filter(Mydata,IncorporationDate %in% sic_retail)
df_rr18<- data.frame(table(data_rr18$IncorporationDate))
pop_rr18 <- data.frame(tapply(data_rr18$SUM_AllPeo, data_rr18$IncorporationDate, sum))
total_rr18 <- cbind(df_rr18,pop_rr18)
total_rr18$pop <- total_rr18$tapply.data_rr18.SUM_AllPeo..data_rr18.IncorporationDate..sum.
total_rr18$tapply.data_rr18.SUM_AllPeo..data_rr18.IncorporationDate..sum. <- NULL
total_rr18$Var1 <- as.numeric(total_rr18$Var1)
str(total_rr18)
model_rr18 = lm(log(total_rr18$Freq)~log(total_rr18$pop), data = total_rr18)
summary(model_rr18)
confint(model_rr18, level=0.95) # CIs for model parameters 
##########
ggplot(total_rr, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_rr, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_rr)$adj.r.squared, 5),"Intercept =",signif(model_rr$coefficients[[1]],5 )," Slope =",signif(model_rr$coefficients[[2]], 5)," P =",signif(summary(model_rr)$coefficients[2,4], 5)))
#############################################
####################################################################
######################Retail closed###########################
####################################################################
data_enter <- filter(Mydata,SIC_2013 %in% sic_entertainment)
df_enter<- data.frame(table(data_enter$SIC_2013))
pop_enter <- data.frame(tapply(data_enter$SUM_AllPeo, data_enter$SIC_2013, sum))
total_enter <- cbind(df_enter,pop_enter)
total_enter$pop <- total_enter$tapply.data_enter.SUM_AllPeo..data_enter.SIC_2013..sum.
total_enter$tapply.data_enter.SUM_AllPeo..data_enter.SIC_2013..sum.<- NULL
total_enter$Var1 <- as.numeric(total_enter$Var1)
str(total_enter)
model_enter = lm(log(total_enter$Freq)~log(total_enter$pop), data = total_enter)
summary(model_enter)
coefficients(model_enter)
confint(model_enter, level=0.95) # CIs for model parameters 
plot(model_enter)
##########
ggplot(total_enter, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_enter, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_enter)$adj.r.squared, 5),"Intercept =",signif(model_enter$coefficients[[1]],5 )," Slope =",signif(model_enter$coefficients[[2]], 5)," P =",signif(summary(model_enter)$coefficients[2,4], 5)))
#############################################
#######################Retail closed###########################
####################################################################
str(Mydata)
data_enter18 <- filter(Mydata,IncorporationDate %in% sic_entertainment)
df_enter18<- data.frame(table(data_enter18$IncorporationDate))
pop_enter18 <- data.frame(tapply(data_enter18$SUM_AllPeo, data_enter18$IncorporationDate, sum))
total_enter18 <- cbind(df_enter18,pop_enter18)
total_enter18$pop <- total_enter18$tapply.data_enter18.SUM_AllPeo..data_enter18.IncorporationDate..
total_enter18$tapply.data_enter18.SUM_AllPeo..data_enter18.IncorporationDate..<- NULL
total_enter18$Var1 <- as.numeric(total_enter18$Var1)
str(total_enter18)
model_enter18 = lm(log(total_enter18$Freq)~log(total_enter18$pop), data = total_enter18)
summary(model_enter18)
coefficients(model_enter)
confint(model_enter18, level=0.95) # CIs for model parameters 
plot(model_enter)
##########
ggplot(total_enter18, aes(y=log(Freq), x=log(pop))) + geom_point(aes(size=Freq))
# Add the regression line
ggplot(total_enter18, aes(y=log(Freq), x=log(pop))) + 
  geom_point(aes(size=Freq))+
  geom_smooth(method=lm) + labs(title = paste("Adj R2 = " , signif(summary(model_enter18)$adj.r.squared, 5),"Intercept =",signif(model_enter18$coefficients[[1]],5 )," Slope =",signif(model_enter18$coefficients[[2]], 5)," P =",signif(summary(model_enter18)$coefficients[2,4], 5)))
#############################################
####################################################################

set.seed(1234)
p= qplot(pop$tapply.pdf.data_digital.SUM_AllPeo..pdf.data_digital.SIC_2013.., geom ='dotplot')
p
p + coord_flip()

hist(pdf)
hist(log(data_digital$SUM_AllPeo))
qplot(log(data_digital$SUM_AllPeo))
x=log(data_digital$SUM_AllPeo)
y=df$Freq
t<-xy.coords(x,y)
complete.cases(x,y)
plot(y~x)
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = data_digital), col = "blue")
pdf$data_digital.SUM_AllPeo = cut(pdf$data_digital.SIC_2013)
names(dimnames(pdf <- c('SIC','Pop')))
dim(df)
hist(log(df$Freq))
data_digital$newcolumn <- table(data_digital$SIC_2013)
list1<-data.frame(cbind(lapply(data_digital,class)))
list1$SIC_2013 <- seq(1,nrow(list1))

df <- aggregate(data_digital,by=list(data_digital$SUM_AllPeo),FUN = mean)
hist(log(df$SUM_AllPeo))
qplot((table(data_digital$SIC_2013)))
plot((table(data_digital$SIC_2013)),log(df$SUM_AllPeo),type='scattered')
aggregate(data_digital,by=list(data_digital$SUM_AllPeo),FUN = length)
df2 <- aggregate((table(data_digital$SIC_2013)), by=list(data_digital$SUM_AllPeo), FUN=sum, na.rm=TRUE)
?aggregate
data_digital %>% count(SUM_AllPeo)
data_digital %>% add_tally()
data_digital %>% group_by(SUM_AllPeo) %>% count(SIC_2013)
datacount <- data_digital %>% add_count(SIC_2013)
data_digital %>% count(data_digital$SIC_2013,sort=TRUE)
hist(datacount$n)
table(data_digital$SIC_2013)
unique(data_digital$SIC_2013)
rev(data_digital$SIC_2013)
sort()
countsnew <- data.frame(aggregate(data_digital,by=SIC_2013,FUN=count))
data_digital$count <- data_digital %>% count(SIC_2013)
varlist <-data.frame(cbind(lapply(data_digital, class)))
Mydatajoin$count <- data_digital %>% count(SIC_2013)
countsSIC <-data.frame((table(data_digital$SIC_2013)))
countspop <-data.frame((table(data_digital$SUM_AllPeo)))
plot(x,data_digital$SUM_AllPeo)
freq = as.data.frame(table(Mydatajoin$SIC_2013,Mydatajoin$SUM_AllPeo))
qplot(SUM_AllPeo, data = data_digital, geom = "histogram")
qplot(n, data = x, geom = "histogram")
qplot(data_digital$SUM_AllPeo, data_digital %>% count(SIC_2013), data = data_digital, geom = "point") + stat_smooth(method="lm", se=FALSE, size=1)
qplot(countspop$Freq, countspop$Var1, data = countspop, geom = "point") + stat_smooth(method="lm", se=FALSE, size=1)

is.na(Mydatajoin)
newdata <- na.omit(Mydatajoin)
class(countsSIC)
pop <- data_digital$SUM_AllPeo[countsSIC$Var1]
summary(pop)
data_digital %>% filter(data_digital$SUM_AllPeo, SIC_2013== 26110)
data.frame(data_digital$SUM_AllPeo, data_digital$SIC_2013)
write.csv(countspop, "/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/pointpolygonjoint/countpop.csv")
write.csv(countsSIC, "/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/pointpolygonjoint/countSIC.csv")

#%n% sic_digital)

write.csv(Mydatajoin, "/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/pointpolygonjoint/Mydatajoin.csv")
###########################################################
list1<-as.data.frame(cbind(lapply(mergingdata,class)))
list1<-cbind(list1,seq.int(nrow(list1)))
UKsub <- mergingdata[,c(1:4,7:8,42:48,51:56,58)]
UKsub1 <- st_set_geometry(mergingdata[,,c(1:4,7:8,42:48,51:56,58)],NULL)
UKmelt <- melt(UKsub1,id.vars = 1:3)
attach(UKmelt)
hist2 <- ggplot(UKmelt, aes(x=value)) + geom_histogram(aes(y = ..density..)) + geom_density(colour="red", size=1, adjust=1)
hist2 + facet_wrap(~ value, scales="free")


ggplot()+
  geom_sf(data=shape,size = 3,color='black',fill='cyan1') +
  ggtitle('data_plot')+
  coord_sf()
shape

#cluster <- readOGR(dsn = '/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/Boundaries/Boundaries',layer='OutputCluster14')
#cluster
ggplot()+
  geom_sf(data=cluster,size = 3,color='black',fill='cyan1') +
  ggtitle('data_plot')+
  coord_sf()
plot(cluster)
qtm(cluster)
qtm(shape)
st_bbox(cluster)
summary(cluster)
summary(shape)
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

BNG = '+init=epsg:27700'

clusterBNG <-spTransform(cluster,BNG)
shapeBNG<-spTransform(shape,BNG)
summary(clusterBNG)
summary(shapeBNG)
tmap_mode('view')
test <- fortify

tm_shape(clusterBNG) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(shapeBNG) +
  tm_dots(col = "blue")
