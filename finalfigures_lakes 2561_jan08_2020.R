
#find # of unique countries in dataset
data<-read.csv("~/MSc_Chl_Clm project/jan 08 2020 work/alldata_withSRAD_2561lakes_jan082020.csv")
library(dplyr)
names(data)
x<-data %>% select(27)
unique(x)
library(tidyverse)





final.data1<-read.csv("~/MSc_Chl_Clm project/jan 08 2020 work/alldata_withSRAD_2561lakes_jan082020.csv")




#REAL WORK START
#XY Scatterplots 2561 lakes with srad

#TP-CHL scatterplot  ##agreed to keep this relationship linear - consisten with Alex and roberto paper

#springSRAD-CHL
ChlvSpringSRAD_gam1<-gam(log(Chla)~s(MAMSrad), method = "REML", data=final.data1)
summary(ChlvSpringSRAD_gam1)
AIC(ChlvSpringSRAD_gam1)

par(mfrow = c(2,2))
gam.check(ChlvSpringSRAD_gam1)

#trying with setting k=4
ChlvSpringSRAD_gam1<-gam(log(Chla)~s(MAMSrad, k=4), method = "REML", data=final.data1)
summary(ChlvSpringSRAD_gam1)
AIC(ChlvSpringSRAD_gam1)

par(mfrow = c(2,2))
gam.check(ChlvSpringSRAD_gam1)

Chl.SprSrad_gam1<-ggplot(final.data1, aes((MAMSrad), log(Chla))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = y~s(x))
Chl.SprSrad_gam1+labs(x='SpringSrad', y='logChl-a')


#with k=4
Chl.SprSrad_gam1<-ggplot(final.data1, aes((MAMSrad), log(Chla))) + geom_point(shape='.', colour = "red") + geom_smooth(method = "gam", formula = y~s(x, k=4))
Chl.SprSrad_gam1+labs(x='SpringSrad', y='logChl-a')

Chl.SprSrad_gam1<-ggplot(final.data1, aes((MAMSrad), log(Chla))) + geom_point(size = 0.75, alpha = 1/4, colour = "red") + geom_smooth(method = "gam", formula = y~s(x, k=4))
Chl.SprSrad_gam1+labs(x='SpringSrad', y='logChl-a')


#trying with srad linear model

sradlinear<-lm(log(Chl.a)~SpringSrad, data=final.data1)
summary(sradlinear)
AIC(sradlinear)#8367.039

sradlinear_fig<-ggplot(final.data1, aes(x=SpringSrad, y=log(Chl.a))) + geom_jitter(alpha = 1/4, size = 0.75) +geom_smooth(method = lm)
sradlinear_fig + ylab("log Chl-a")
names(final.data1)



#CHLvTP
ChlvTP<-lm(formula = log(Chl.a)~log(TP), data=final.data1)
ChlvTP
summary(ChlvTP)

AIC(ChlvTP)#6581.412

CHL.TP<-ggplot(data=final.data1, mapping = aes(x=log(TP), y=log(Chla)))+geom_point(shape='.')+geom_smooth(method = lm) 
CHL.TP+labs(x="logTP", y="logChl-a")

CHL.TP<-ggplot(data=final.data1, mapping = aes(x=log(TP), y=log(Chl.a)))+geom_point(size = 0.75, alpha = 1/4)+geom_smooth(method = lm) 
CHL.TP+labs(x="logTP", y="logChl-a")



#ChlvTP_test_gam<-gam(log(ChlValues)~s(log(TP.value)), method = "REML", data=final.data1)
#summary(ChlvTP_test_gam)
#AIC(ChlvTP_test_gam) #11159.92 #lower AIC#better fit



#summertemp-CHL
##gam_y <- gam(y ~ s(x), method = "REML")
#GAM model
Chlvsummertemp_gam<-gam(log(Chla)~s(JJAtemp), method = "REML", data=final.data1)
summary(Chlvsummertemp_gam)
AIC(Chlvsummertemp_gam)
par(mfrow = c(2,2))
gam.check(Chlvsummertemp_gam)

#with k=4
Chlvsummertemp_gam<-gam(log(Chla)~s(JJAtemp, k=4), method = "REML", data=final.data1)
summary(Chlvsummertemp_gam)
AIC(Chlvsummertemp_gam)
par(mfrow = c(2,2))
gam.check(Chlvsummertemp_gam)

#linear models
Chl.Stemp.linear<-lm(formula = log(Chl.a) ~ SummerTemp, final.data1)
summary(Chl.Stemp.linear)
AIC(Chl.Stemp.linear)

Chl.Stemp.fig<-ggplot(final.data1, aes(SummerTemp, log(Chl.a)))+geom_jitter(alpha = 1/4, size = 0.75) +geom_smooth(method = lm)
Chl.Stemp.fig + ylab("log Chl-a")




#ggplot(Sample_data, aes(x, y)) + geom_point() + geom_smooth(method = "gam", formula = y ~s(x))
#GAM figures
## does not work, can't find formula at the end# Chl.STemp<-ggplot(filtereddata, aes(log(summerMeanTemp), log(ChlValues))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = (log(ChlValues)~s(log(summerMeanTemp))))
Chl.summertemp_gam<-ggplot(final.data1, aes(JJAtemp, log(Chla))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = y~s(x))
Chl.summertemp_gam+labs(x='SummerTemp', y='logChl-a')

#with k=4
Chl.summertemp_gam<-ggplot(final.data1, aes(JJAtemp, log(Chla))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = y~s(x, k=4))
Chl.summertemp_gam+labs(x='SummerTemp', y='logChl-a')


Chl.summertemp_gam<-ggplot(final.data1, aes(JJAtemp, log(Chla))) + geom_point(size = 0.75, alpha = 1/4, colour = "red") + geom_smooth(method = "gam", formula = y~s(x, k=4))
Chl.summertemp_gam+labs(x='SummerTemp', y='logChl-a')



#spring and summer ppt-CHL #linear models

springppt.chl<-lm(formula = log(Chl.a)~SpringPpt, final.data1)
summary(springppt.chl)

summerppt.chl<-lm(formula = log(Chl.a)~SummerPpt, final.data1)
summary(summerppt.chl)
AIC(summerppt.chl)


#Chlvspringppt_gam <-gam(log(Chla)~s((MAMppt)), method = "REML", data=final.data1)
#summary(Chlvspringppt_gam)
#has even lower R^2 #0.013

#summerprecip-CHL
Chlvsummerpre_gam<-gam(log(Chla)~s((JJAppt)), method = "REML", data=final.data1)
summary(Chlvsummerpre_gam)
AIC(Chlvsummerpre_gam)

par(mfrow = c(2,2))
gam.check(Chlvsummerpre_gam)

#with k=4
Chlvsummerpre_gam<-gam(log(Chla)~s(JJAppt, k=4), method = "REML", data=final.data1)
summary(Chlvsummerpre_gam)
AIC(Chlvsummerpre_gam)

par(mfrow = c(2,2))
gam.check(Chlvsummerpre_gam)



Chl.SPre_gam<-ggplot(final.data1, aes(JJAppt, log(Chla))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = y~s(x)) 
Chl.SPre_gam+labs(x='SummerPpt', y='logChl-a')

#with k=4
Chl.SPre_gam<-ggplot(final.data1, aes(JJAppt, log(Chla))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = y~s(x, k=4)) 
Chl.SPre_gam+labs(x='SummerPpt', y='logChl-a')

#trying linear model for ppt cuz edf = 1.34

#Chlvppt<-lm(formula = log(Chla)~(JJAppt), data=final.data1)
#Chlvppt
#summary(Chlvppt)
#also has low R^2 = 0.018

#summercldcvr-CHL
Chlvsummercldcvr_gam<-gam(log(Chla)~s((JJAcloudcover)), method = "REML", data=final.data1)
summary(Chlvsummercldcvr_gam)
AIC(Chlvsummercldcvr_gam)

par(mfrow = c(2,2))
gam.check(Chlvsummercldcvr_gam)

#k=4
Chlvsummercldcvr_gam<-gam(log(Chla)~s(JJAcloudcover, k=4), method = "REML", data=final.data1)
summary(Chlvsummercldcvr_gam)
AIC(Chlvsummercldcvr_gam)

par(mfrow = c(2,2))
gam.check(Chlvsummercldcvr_gam)



Chl.Sumrcldcvr_gam<-ggplot(final.data1, aes(JJAcloudcover, log(Chla))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = y~s(x))
Chl.Sumrcldcvr_gam +labs(x='SummerCldcvr', y='logChl-a')

#k=4
Chl.Sumrcldcvr_gam<-ggplot(final.data1, aes(JJAcloudcover, log(Chla))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = y~s(x, k=4))
Chl.Sumrcldcvr_gam +labs(x='SummerCldcvr', y='logChl-a') + expand_limits(x=c(35,80), y=c(-7.5, 0))


Chl.Sumrcldcvr_gam<-ggplot(final.data1, aes(JJAcloudcover, log(Chla))) + geom_point(size = 0.75, alpha = 1/4, colour = "red") + geom_smooth(method = "gam", formula = y~s(x, k=4))
Chl.Sumrcldcvr_gam +labs(x='SummerCldcvr', y='logChl-a') + expand_limits(x=c(35,80), y=c(-7.5, 0))



#linearmodel for summer cloud cover
names(final.data1)

Chl.cld.linear<-lm(formula = log(Chl.a)~SummerCldcvr, final.data1)
summary(Chl.cld.linear)
AIC(Chl.cld.linear)

Chl.cld.fig<-ggplot(final.data1, aes(SummerCldcvr, log(Chl.a))) + geom_jitter(size = 0.75, alpha = 1/4) + geom_smooth(method = lm)
Chl.cld.fig + ylab("log Chl-a")



#springcldcvr-CHL
#Chlvspringcldcvr_gam<-gam(log(ChlValues)~s(log(springMeancldcvr)), method = "REML", data=final.data1)
#summary(Chlvspringcldcvr_gam)
#par(mfrow = c(2,2))
#gam.check(Chlvspringcldcvr_gam)

#Chl.Sprcldcvr_gam<-ggplot(final.data1, aes(log(springMeancldcvr), log(ChlValues))) + geom_point(shape='.') + geom_smooth(method = "gam") +theme_classic()
#Chl.Sprcldcvr_gam +labs(x='logSprCldcvr', y='logChl')



#averagedepth-CHL #works but really weird figure #will not use
#ChlvAvgdepth_gam<-gam(log(Chla)~s(log(Meandepth)), method = "REML", data=final.data1)
#summary(ChlvAvgdepth_gam)
#AIC(ChlvAvgdepth_gam)

#par(mfrow = c(2,2))
#gam.check(ChlvAvgdepth_gam)



#don't need to do k=4 because already low edf - edf= 1.76 - near linear but low r^2 - 0.013 = <0.05
Chl.wshed1_gam<-gam(log(Chla)~s(log(Watershedarea)), method = "REML", data=final.data1)
summary(Chl.wshed1_gam)
AIC(Chl.wshed1_gam)


Chl.wshed1<-ggplot(final.data1, aes(log(Watershedarea), log(Chla))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = y~s(x))
Chl.wshed1+labs(x='WatershedArea', y='logChl-a')




install.packages("cowplot")
library(cowplot)
#withunits
#plot_grid(Chl.SprSrad_gam1+labs(x='logMAMSrad(KJ.m^2.day^-1)', y='logChla(mg/L)')
#          ,CHL.TP+labs(x="logTP(mg/L)", y="logChla(mg/L)")
#         ,Chl.STemp+labs(x='logJJAtemp(oC)', y='logChla(mg/L)')
#          ,Chl.SPre_gam+labs(x='logJJAppt(mm)', y='logChla(mg/L)')
#          ,Chl.Sumrcldcvr_gam +labs(x='logJJAcloudcover(%)', y='logChla(mg/L)')
#          ,Chl.avgdepth_gam+labs(x='logMeandepth(m)', y='logChla(mg/L)')
#          ,labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 12)

#withOUT units
plot_grid(Chl.SprSrad_gam1+labs(x='SpringSrad', y='logChl-a')
          ,CHL.TP+labs(x='logTP', y='logChl-a')
          ,Chl.summertemp_gam+labs(x='SummerTemp', y='logChl-a')
          ,Chl.SPre_gam+labs(x='SummerPpt', y='logChl-a')
          ,Chl.Sumrcldcvr_gam+labs(x='SummerCldcvr', y='logChl-a')
          ,Chl.wshed1+labs(x='logWatershedArea', y='logChl-a')
          ,labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 12)


#only 4 figures
plot_grid(Chl.SprSrad_gam1+labs(x='SpringSrad', y='logChl-a')
          ,CHL.TP+labs(x='logTP', y='logChl-a')
          ,Chl.summertemp_gam+labs(x='SummerTemp', y='logChl-a')
          ,Chl.Sumrcldcvr_gam+labs(x='SummerCloudcover', y='logChl-a')
          ,labels = c('A', 'B', 'C', 'D'), label_size = 12)



#only 4 figures - linear models
plot_grid(sradlinear_fig+labs(x='SpringSrad', y='logChl-a')
          ,CHL.TP+labs(x='logTP', y='logChl-a')
          ,Chl.Stemp.fig+labs(x='SummerTemp', y='logChl-a')
          ,Chl.cld.fig+labs(x='SummerCloudcover', y='logChl-a')
          ,labels = c('A', 'B', 'C', 'D'), label_size = 12)

#Random forest model

logchl2<-log10(final.data1$Chla)
logtp2<-log10(final.data1$TP)



library(randomForest)
#with Srad and 2561 lakes
#new tests
data.imputed1<-rfImpute(logchl2 ~ logtp2 
                        + MAMtemp+ JJAtemp 
                        + MAMppt + JJAppt 
                        + MAMcloudcover + JJAcloudcover
                        + MAMSrad + JJASrad
                        + Lakearea + Volume  
                        + Meandepth + Residencetime 
                        + Elevation + Watershedarea
                        , data = final.data1, iter=6)


rfmodel_srad<-randomForest(logchl2 ~ logtp2 
                           + MAMtemp+ JJAtemp 
                           + MAMppt + JJAppt 
                           + MAMcloudcover + JJAcloudcover
                           + MAMSrad + JJASrad
                           + Lakearea + Volume  
                           + Meandepth + Residencetime 
                           + Elevation + Watershedarea
                           , data = data.imputed1, proximity=TRUE, ntree=500, mtry= 5)

rfmodel_srad #explains 59.77% = 60%

order(rfmodel_srad$importance)#read in lowest to highest #most impt clim var is summertemp - spring temp then springSRAD - summer srad#least impt is volume

rfmodel_srad$importance






write.table(rfmodel_srad$importance, "modelimportance_2561lakesFeb172020.csv", sep = (","))


modelimportance_2561lakes<-read.csv("~/MSc_Chl_Clm project/modelimportance_2561lakesFeb172020.csv")


orderedforest20<-modelimportance_2561lakes[with(modelimportance_2561lakes,order(Importance, Variable)),] #orders based on value #lowest to highest ###USE THIS
orderedforest20
write.table(orderedforest20, "orderedforest20_2561lakes_Feb172020.csv", sep = (","))

##trying to make stacked barplot


barplotdata20<-read.csv("~/MSc_Chl_Clm project/barplotdata_lakes2561_feb172020.csv")


install.packages("RColorBrewer")
library(RColorBrewer)

#extrapolating color pallettes
nb.cols<- 15



mycolors3<-colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)

p<-ggplot(data=barplotdata20, aes(x=Type, y=Importance, fill=Variable))

p+geom_bar(stat="identity", width = 1, color="black", size = 1) + theme_classic() + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 0.8)) + scale_fill_manual(values = mycolors3) 

#reorder bars from tallest to shortest
p<-ggplot(data=barplotdata20, aes(x=reorder(Type, -Importance), y=Importance, fill=Variable))

p+geom_bar(stat="identity", width = 1, color="black", size = 1) + theme_classic() + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 0.9, size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) + scale_fill_manual(values = mycolors3)





#corrmatrix
#build correlation heatmap

library(ggcorrplot)

names(final.data1)

#generate object for variables to be tested
corr_variables1<-final.data1[,c(11, 12,32,35, 38,40,41,43,169:170,182:183,194,196, 232, 233)]
write.table(corr_variables1, file = "correlation_vars1.csv", sep = ",")
filtered_vars1 <- read.csv("~/MSc_Chl_Clm project/correlation_vars1.csv")


#generate corrmatrix
corr1 <- round(cor(filtered_vars1), 1)
head(corr1[, 1:6])


#generate p-value matrix
p.mat1 <- cor_pmat(filtered_vars1)
head(p.mat1[, 1:4])

#basic viz for corrplot
ggcorrplot(corr, method = "square") #square method 
ggcorrplot(corr, method = "circle") #circle method

#using this
ggcorrplot(corr1, hc.order = TRUE, outline.col = "white")

#this gives numbers but doesn't blank out insignif relations
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

#using this - better version of above
ggcorrplot(corr1, p.mat = p.mat1, hc.order = TRUE,
           type = "lower", insig = "blank", lab = TRUE)

##create correlation table


filtered_vars1

cormat1<-rcorr(as.matrix(filtered_vars1), type = "pearson")
cormat1

cormat1$r
cormat1$n
cormat1$P


#format all into 1 table


flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

finalcorrdata<-flattenCorrMatrix(cormat1$r, cormat1$P)
finalcorrdata
write.table(finalcorrdata, "filteredvars1_2561lakes_withsrad_corrdata_jan102020.csv", sep = (","))


#trying new correlation matrix #kendall # non parametric version of pearsons



library(ggcorrplot)

computecor<-(cor(corr_variables1, method = "kendall", use = "complete.obs")) #function round just rounds the numbers
computecor

p.mat_computecor <- cor_pmat(corr_variables1)
p.mat_computecor


ggcorrplot(computecor, p.mat = p.mat_computecor, hc.order = TRUE,
           type = "lower", insig = "blank", lab = TRUE)



##PCA

names(final.data1)

PCA_model<-prcomp(final.data1[,c(11, 12, 169:170,182:183,194,196, 232, 233)], scale. = TRUE) ##WORKS #USING THIS
PCA_model
biplot(PCA_model)


#PCAVIZ
install.packages("factoextra")
library(factoextra)

#simplified version #use this
fviz_pca_var(PCA_model,repel = TRUE, addEllipses=FALSE) + theme_classic()

fviz_pca_var(PCA_model,repel = TRUE,addEllipses = FALSE) + theme_classic()

autoplot(PCA_model)


###supplementary table 1
summary(final.data1$Year)
summary(final.data1$Lat)
summary(final.data1$Lon)
summary(final.data1$HydroLAKES.Elevation)
summary(final.data1$HydroLAKES.Lake_area)
summary(final.data1$LakeVolume)
summary(final.data1$Depth.mean)
summary(final.data1$HydroLAKES.Res_time)
summary(final.data1$HydroLAKES.Wshd_area)
summary(final.data1$HydroLAKES.Shore_len)
summary(final.data1$HydroLAKES.Shore_dev)
summary(final.data1$ChlValues)
summary(final.data1$TP.value)
summary(final.data1$springMeanTemp)
summary(final.data1$summerMeanTemp)
summary(final.data1$springMeanPre)
summary(final.data1$summerMeanPre)
summary(final.data1$springMeancldcvr)
summary(final.data1$summerMeancldcvr)
summary(final.data1$springMeanSrad)
summary(final.data1$summerMeanSrad)
