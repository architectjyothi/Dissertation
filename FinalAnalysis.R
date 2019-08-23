rm(list = ls(all=TRUE))
getwd()
setwd('Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/pointpolygonjoint/')
library(maptools)
library(RColorBrewer)
library(classInt) 
#library(OpenStreetMap) 
library(sp) 
library(rgeos) 
library(tmap) 
library(tmaptools) 
library(sf) 
library(rgdal) 
library(geojsonio)
library(Rcpp)
library(methods)
library(ggplot2)

#reade the shape file
#cluster <- read_shape('Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/')
data<- read_shape('pointpolygonjoint/Mydatajoin.shp',as.sf = TRUE)
d <- read.csv('/Users/jyothigupta/Documents/UCL/CASA_Dissertations/UrbanScalinglaw/Data/pointpolygonjoint/Mydatajoin.csv')
library("PerformanceAnalytics")
dd_data <- d[, c('SUM_AllPeo','ladnm','SIC_2013','IncorporationDate','SUM_Basic')]
chart.Correlation(dd_data, histogram=TRUE, pch=19)

str(d)

drop <- c("FID_1","OBJECTID",'id','FID_2','FID_Cluste','SUM_Join_C','SUM_d_Work','SUM_Agricu','SUM_Manufa','SUM_Constr',
          'SUM_HotelR','SUM_Financ','SUM_RealEs','SUM_Admin','SUM_Educat','SUM_Income','SUM_NetInc','SUM_NIncBH',
          'SUM_NIncAH','SUM_Househ','SUM_CarsVa','SUM_Dwelli','SUM_OccuDw','SUM_UnDwel','SUM_UnSecD','SUM_Employ'
          ,'SUM_Manage','SUM_Profes','SUM_techni','SUM_Admin2','SUM_SkillT','SUM_Servic','SUM_Sales','SUM_plant',
          'SUM_Basic','Var.42')
didi = d[,!(names(d) %in% drop)]
sa<- d %>% select(11,50,51,52)
corrplot(tesy,method = 'number')
str(d)
tesy<-cor(sa)
ggplot(d, aes(x = log(SUM_AllPeo) , fill = companycategory)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))
ggplot(d, aes(x = log(SUM_AllPeo))) +
  geom_density(aes(color = companycategory), alpha = 0.5) +
  theme_classic()

ggplot(d, aes(x = log(SUM_AllPeo))) +
  geom_density(aes(color = companystatus), alpha = 0.5) +
  theme_classic()
ggplot(d, aes(x = log(SUM_AllPeo))) +
  geom_density(aes(color = incorporationdate), alpha = 0.5) +
  theme_classic()

ggplot(d, aes(x = log(SUM_AllPeo), y = log(SIC_2013))) +
  geom_point(aes(color = companycategory),
             size = 0.5) +
  stat_smooth(method = 'lm',
              formula = y~poly(x, 2),
              se = TRUE,
              aes(color = companycategory)) +
  theme_classic()
#install.packages("GGally")
library(GGally)
# Convert data to numeric
corr <- data.frame(lapply(didi, as.integer))

# Plot the graph
ggcorr(corr,
method = c("pairwise", "spearman"),
nbreaks = 6,
hjust = 0.8,
label = TRUE,
label_size = 3,
color = "grey50")

logit <- glm(SUM_AllPeo~SIC_2013+companycategory, data = didi)
summary(logit)

countsSIC <-data.frame((table(df$SIC_2013)))
countpop <- data.frame((table(df$SUM_AllPeo)))
plot(log(countpop$Var1),log(countsSIC$Freq))
plot(tapply(log(df$SUM_AllPeo), log(df$SIC_2013), sum))
pop <- data.frame(tapply(df$SUM_AllPeo, log(df$SIC_2013), sum)) ### contingency table from data.frame : array with named dimnames

library(hexbin)
x <- rnorm(1000)
y <- rnorm(1000)
bin<-hexbin(x, y, xbins=50) 
plot(hexbin(log(df$SUM_AllPeo), df$SIC2018, xbins=50) , main="Hexagonal Binning")

plot(pop)
hist(countpop$Freq, 
     main="Count of the number of firms in 2013", 
     xlab="Firms", 
     border="blue", 
     col="green",
     xlim=c(10000,7000000),
     las=1, 
     breaks=5)


hist(log(countpop$Var1), prob=TRUE, col="grey",freq = FALSE)# prob=TRUE for probabilities not counts
lines(density(log(countpop$Var1)), col="blue", lwd=2) # add a density estimate with defaults
lines(density(log(countpop$Var1), adjust=2), lty="dotted", col="darkgreen", lwd=2) 


hist(log(countsSIC$Freq), prob=TRUE, col="grey",freq = FALSE)# prob=TRUE for probabilities not counts
lines(density(log(countsSIC$Freq)), col="blue", lwd=2) # add a density estimate with defaults
lines(density(log(countsSIC$Freq), adjust=2), lty="dotted", col="darkgreen", lwd=2) 

plot(df$companycategory, df$SIC_2013, main="Scatterplot Company category and count of SIC", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
ggplot(countpop, aes(y=log(countpop$Var1),x=log(countpop$Freq), colour=countpop$Var1))   +  geom_point()+ geom_line() 
str(countpop)
countpop$Var1 <- as.numeric(countpop$Var1)
qtm(data)
class(data)
qtm(data,fill='AllAgpp')
summary(data$AllAgpp)
#basemap
UK_osm <- read_osm(data,type='ersi',zoom = NULL)
autoplot(data)
qtm(UK_osm) +
  tm_shape(data) +
  tm_polygons("Rate.of.JobSeekers.Allowance..JSA..Claimants...2015",
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) +
  tm_compass(position = c("left", "bottom"),type = "arrow") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c
            ("right", "bottom"))
ggplot(data)+geom_sf(aes(fill=SUM_AlP))
tmap_mode('view')
tm_shape(data) +
  tm_polygons(col = NA, alpha = 0.5)
BNG = "+init=epsg:27700"
dataBNG <- spTransform(data,BNG)
#create a ppp object
data.ppp <- ppp(x=data@coords[,1],y=BluePlaquesSub@coords[,2],
                          window=window)
