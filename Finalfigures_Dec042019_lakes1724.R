lakes1724<-read.csv("~/MSc_Chl_Clm project/ChlLakes_AllClim_Nov072019_1724lakes_filteredforTP and SRAD Na's.csv")

#create scatter plots

library(ggplot2)

names(lakes1724)



#REAL WORK START
#XY Scatterplots

ChlvTP1<-lm(formula = log(ChlValues)~log(TP.value), data=lakes1724)
ChlvTP1
CHL.TP1<-ggplot(data=lakes1724, mapping = aes(x=log(TP.value), y=log(ChlValues)))+geom_point(shape='.')+geom_smooth(method = lm)+theme_classic() 
CHL.TP1+labs(x="logTP", y="logChl")


#summertemp-CHL
##gam_y <- gam(y ~ s(x), method = "REML")
#GAM model
Chlvsummertemp_gam1<-gam(log(ChlValues)~s(log(summerMeanTemp)), method = "REML", data=lakes1724)
summary(Chlvsummertemp_gam1)
par(mfrow = c(2,2))
gam.check(Chlvsummertemp_gam1)

#ggplot(Sample_data, aes(x, y)) + geom_point() + geom_smooth(method = "gam", formula = y ~s(x))
#GAM figures
## does not work, can't find formula at the end# Chl.STemp<-ggplot(filtereddata, aes(log(summerMeanTemp), log(ChlValues))) + geom_point(shape='.') + geom_smooth(method = "gam", formula = (log(ChlValues)~s(log(summerMeanTemp))))
Chl.STemp1<-ggplot(lakes1724, aes(log(summerMeanTemp), log(ChlValues))) + geom_point(shape='.') + geom_smooth(method = "gam")+theme_classic() #works
Chl.STemp1+labs(x='logSumrTemp', y='logChl')



#summerprecip-CHL
Chlvsummerpre_gam1<-gam(log(ChlValues)~s(log(summerMeanPre)), method = "REML", data=lakes1724)
summary(Chlvsummerpre_gam1)
par(mfrow = c(2,2))
gam.check(Chlvsummerpre_gam1)

Chl.SPre_gam1<-ggplot(lakes1724, aes(log(summerMeanPre), log(ChlValues))) + geom_point(shape='.') + geom_smooth(method = "gam") +theme_classic()
Chl.SPre_gam1+labs(x='logSumrPrecip', y='logChl')


#springSRAD-CHL
ChlvSpringSRAD_gam1<-gam(log(ChlValues)~s(log(springMeanSrad)), method = "REML", data=lakes1724)
summary(ChlvSpringSRAD_gam1)
par(mfrow = c(2,2))
gam.check(ChlvSpringSRAD_gam1)

Chl.SprSrad_gam1<-ggplot(lakes1724, aes(log(springMeanSrad), log(ChlValues))) + geom_point(shape='.') + geom_smooth(method = "gam") +theme_classic()
Chl.SprSrad_gam1+labs(x='logSprSrad', y='logChl')



#springcldcvr-CHL
Chlvspringcldcvr_gam1<-gam(log(ChlValues)~s(log(springMeancldcvr)), method = "REML", data=lakes1724)
summary(Chlvspringcldcvr_gam1)
par(mfrow = c(2,2))
gam.check(Chlvspringcldcvr_gam1)

Chl.Sprcldcvr_gam1<-ggplot(lakes1724, aes(log(springMeancldcvr), log(ChlValues))) + geom_point(shape='.') + geom_smooth(method = "gam") +theme_classic()
Chl.Sprcldcvr_gam1+labs(x='logSprCldcvr', y='logChl')



#averagedepth-CHL
ChlvAvgdepth_gam<-gam1(log(ChlValues)~s(log(HydroLAKES.Depth_avg)), method = "REML", data=lakes1724)
summary(ChlvAvgdepth_gam1)
par(mfrow = c(2,2))
gam.check(ChlvAvgdepth_gam1)

Chl.avgdepth_gam1<-ggplot(lakes1724, aes(log(HydroLAKES.Depth_avg), log(ChlValues))) + geom_point(shape='.') + geom_smooth(method = "gam") +theme_classic()
Chl.avgdepth_gam1+labs(x='logAvgDepth', y='logChl')



install.packages("cowplot")
library(cowplot)

plot_grid(CHL.TP1+labs(x="logTP", y="logChl")
          ,Chl.STemp1+labs(x='logSumrTmp', y='logChl')
          ,Chl.SPre_gam1+labs(x='logSumrPre', y='logChl')
          ,Chl.SprSrad_gam1+labs(x='logSprSrad', y='logChl')
          ,Chl.Sprcldcvr_gam1 +labs(x='logSprCld', y='logChl')
          ,Chl.avgdepth_gam1+labs(x='logAvgDepth', y='logChl')
          ,labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size = 12)

#Random forest model

library(randomForest)

logchl<-log10(lakes1724$ChlValues)
logtp<-log10(lakes1724$TP.value)

logchl3<-log(lakes1724$ChlValues)
logtp3<-log(lakes1724$TP.value)


#without PET and Winter clim vars and 1724 lakes
model1<-randomForest(logchl ~ logtp + springMeanTemp+ summerMeanTemp+ 
                       + springMeanPre + summerMeanPre
                     + springMeancldcvr + summerMeancldcvr
                     + springMeanSrad+ summerMeanSrad + HydroLAKES.Lake_area  
                     + HydroLAKES.Vol_total + HydroLAKES.Depth_avg + HydroLAKES.Res_time + HydroLAKES.Elevation 
                     + HydroLAKES.Wshd_area, data = lakes1724, iter=5, na.action = na.exclude)

model1 #60.60% explained
model1$importance 


order(model1$importance)#read in lowest to highest #most impt clim var is summer cld cvr #least impt is lake area

write.table(model1$importance, "modelimportance_1724lakesDec022019.csv", sep = (","))


modelimportance_1724lakes<-read.csv("~/MSc_Chl_Clm project/modelimportance_1724lakesDec022019.csv")


orderedforest<-modelimportance_1724lakes[with(modelimportance_1724lakes,order(Importance, Variable)),] #orders based on value #lowest to highest ###USE THIS
orderedforest
write.table(orderedforest, "orderedforest_1724lakes_Nov202019.csv", sep = (","))

#stackedbarplot
barplotdata1<-read.csv("~/MSc_Chl_Clm project/barplotdata1.csv")

#this version has numbers - using this #not using this anymore
p<-ggplot(data=barplotdata1, aes(x=Type, y=Importance, fill=Variable))
p+geom_bar(stat="identity", width = 1, color="black", size = 0.3, alpha = 0.5)+geom_text(aes(label = Importance), position = position_stack(vjust = 0.5), size = 3) + theme_classic() +theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 0.8)) 

#updated final version #using this
nb.cols1<- 15

mycolors3_1724<-colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols1)
p<-ggplot(data=barplotdata1, aes(x=Type, y=Importance, fill=Variable))
p+geom_bar(stat="identity", width = 1, color="black", size = 1) + theme_classic() + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 0.7)) + scale_fill_manual(values = mycolors3_1724)



#corrmatrix

#this method produces corr matrix with p and n values

install.packages("Hmisc")
library(Hmisc)
names(lakes1724)
climvars_chl_TP1<-lakes1724[,c(15, 17,45, 48, 51, 53, 54, 56,161:162,174:175,273:274, 286:287)]

cormat1<-rcorr(as.matrix(climvars_chl_TP1), type = "pearson")
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
write.table(finalcorrdata, "clim_chl_TP_corrdata_1724.csv", sep = (","))


##PCA
names(lakes1724)

princomp(lakes1724[,c(15, 17, 161:162,174:175,273:274, 286:287)])  ##ALSO WORKS  ##NOT USING THIS

PCA_model1<-prcomp(lakes1724[,c(15, 17, 161:162,174:175,273:274, 286:287)], scale. = TRUE) ##WORKS #USING THIS



#PCAVIZ
install.packages("factoextra")
library(factoextra)


fviz_pca_var(PCA_model, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)+theme_classic()
fviz_pca_biplot(PCA_model, repel = TRUE)  ##does not work


#simplified version #use this for updated figs
fviz_pca_var(PCA_model1,repel = TRUE, addEllipses=FALSE) + theme_classic()


#can use get_pca to find out these info about the PCA
vars<-get_pca(PCA_model)
head(vars$cor)
head(vars$coord)
head(vars$cos2)
head(vars$contrib)



###supplementary table 1
summary(lakes1724$Year)
summary(lakes1724$Lat)
summary(lakes1724$Lon)
summary(lakes1724$HydroLAKES.Elevation)
summary(lakes1724$HydroLAKES.Lake_area)
summary(lakes1724$LakeVolume)
summary(lakes1724$Depth.mean)
summary(lakes1724$HydroLAKES.Res_time)
summary(lakes1724$HydroLAKES.Wshd_area)
summary(lakes1724$HydroLAKES.Shore_len)
summary(lakes1724$HydroLAKES.Shore_dev)
summary(lakes1724$ChlValues)
summary(lakes1724$TP.value)
summary(lakes1724$springMeanTemp)
summary(lakes1724$summerMeanTemp)
summary(lakes1724$springMeanPre)
summary(lakes1724$summerMeanPre)
summary(lakes1724$springMeancldcvr)
summary(lakes1724$summerMeancldcvr)
summary(lakes1724$springMeanSrad)
summary(lakes1724$summerMeanSrad)


##building residual plot of CHL-TP and then testing SRAD and Cldcvr against it

ChlvTP1<-lm(formula = log(ChlValues)~log(TP.value), data=lakes1724)
ChlvTP1
#compute residual
ChlvTP1.resid<-resid(ChlvTP1)
ChlvTP1.resid
plot(ChlvTP1.resid)
#plot residuals on y-axis against spring SRAD on x-axis




##USING THIS ONE
CHL.TP1resid_SRAD<-ggplot(data=lakes1724, mapping = aes(alpha= 0.5, x=(springMeanSrad), y=(ChlvTP1.resid)))+geom_point(shape = ".")+geom_abline(intercept = 0, slope= 0)+theme_classic() 
CHL.TP1resid_SRAD+labs(x="SprSrad", y="TP.CHL.resid")





