Usa_data<-read.csv("~/USA SRAD project/all data _landuse_climate_seasonal_population_may_16_2020/USA_alldata_climate_landuse_no.na's_may162020.csv")

usadata1<-read.csv("~/USA SRAD project/all data _landuse_climate_seasonal_population_may_16_2020/USA_alldata_climate_landuse_no.na's_may162020_noTPNa's.csv")

library(tidyverse)
#REAL WORK START

#Random forest model with only landuse

logchl5<-log10(usadata1$ChlValues)
logtp5<-log10(usadata1$TP.value)


library(randomForest)



is.infinite(Usa_data$ChlValues)
is.infinite(Usa_data$TP.value)


data.imputed7<-rfImpute(logchl5 ~ logtp5 
                        + Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13+Pro14+Pro15+Pro16+Pro17+Population
                        + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                        + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                        + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                        +springMeantmp + summerMeantmp
                        +springMeanSrad + summerMeanSrad
                        +springMeanppt + summerMeanppt
                        +springMeancldcvr + summerMeancldcvr ,data = usadata1, iter=1)



rfmodel_landuse<-randomForest(logchl5 ~ logtp5 
                              + Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13+Pro14+Pro15+Pro16+Pro17+Population
                              + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                              + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                              + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                              +springMeantmp + summerMeantmp
                              +springMeanSrad + summerMeanSrad
                              +springMeanppt + summerMeanppt
                              +springMeancldcvr + summerMeancldcvr ,data = data.imputed7, proximity=TRUE, ntree=500, mtry= 5)

rfmodel_landuse$importance

write.table(rfmodel_landuse$importance, "barplotdata_landuse.csv", sep = (","))

barplotdata_landuse<-read.csv("~/MSc_Chl_Clm project/barplotdata_landuse.csv")
##trying to make stacked barplot




install.packages("RColorBrewer")
library(RColorBrewer)

#extrapolating color pallettes
nb.cols<- 33

mycolors3<-colorRampPalette(brewer.pal(33, "RdYlBu"))(nb.cols)
p<-ggplot(data=barplotdata_landuse, aes(x=reorder(Type,-Value), y=Value, fill=Variable))
p+geom_bar(stat="identity", width = 1, color="black", size = 1) + theme_classic() + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 0.9, size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) + scale_fill_manual(values = mycolors3)

##with full dataset

logchl7<-log10(Usa_data$ChlValues)
logtp7<-log10(Usa_data$TP.value)



data.imputed9<-rfImpute(logchl7 ~ logtp7 
                        + Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13+Pro14+Pro15+Pro16+Pro17+Population
                        + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                        + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                        + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                        +springMeantmp + summerMeantmp
                        +springMeanSrad + summerMeanSrad
                        +springMeanppt + summerMeanppt
                        +springMeancldcvr + summerMeancldcvr ,data = Usa_data, iter=1)



rfmodel_landuse_full<-randomForest(logchl7 ~ logtp7 
                              + Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13+Pro14+Pro15+Pro16+Pro17+Population
                              + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                              + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                              + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                              +springMeantmp + summerMeantmp
                              +springMeanSrad + summerMeanSrad
                              +springMeanppt + summerMeanppt
                              +springMeancldcvr + summerMeancldcvr ,data = data.imputed9, proximity=TRUE, ntree=500, mtry= 5)

rfmodel_landuse_full

write.table(rfmodel_landuse_full$importance, "barplotdata_landuse_full.csv", sep = (","))

barplotdata_landuse_full<-read.csv("~/MSc_Chl_Clm project/barplotdata_landuse_full.csv")
##trying to make stacked barplot




install.packages("RColorBrewer")
library(RColorBrewer)

#extrapolating color pallettes
nb.cols<- 33

mycolors3<-colorRampPalette(brewer.pal(33, "RdYlBu"))(nb.cols)
p<-ggplot(data=barplotdata_landuse_full, aes(x=reorder(Type,-Value), y=Value, fill=Variable))
p+geom_bar(stat="identity", width = 1, color="black", size = 1) + theme_classic() + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 0.9, size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) + scale_fill_manual(values = mycolors3)

#full dataset without tp

logchl7<-log10(Usa_data$ChlValues)
logtp7<-log10(Usa_data$TP.value)



data.imputed10<-rfImpute(logchl7 ~ Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13
                        +Pro14+Pro15+Pro16+Pro17+Population
                        + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                        + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                        + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                        +springMeantmp + summerMeantmp
                        +springMeanSrad + summerMeanSrad
                        +springMeanppt + summerMeanppt
                        +springMeancldcvr + summerMeancldcvr ,data = Usa_data, iter=1)



rfmodel_landuse_withoutTP<-randomForest(logchl7 ~ Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13
                                   +Pro14+Pro15+Pro16+Pro17+Population
                                   + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                                   + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                                   + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                                   +springMeantmp + summerMeantmp
                                   +springMeanSrad + summerMeanSrad
                                   +springMeanppt + summerMeanppt
                                   +springMeancldcvr + summerMeancldcvr ,data = data.imputed10, proximity=TRUE, ntree=500, mtry= 5)

rfmodel_landuse_withoutTP
rfmodel_landuse_withoutTP$importance

write.table(rfmodel_landuse_withoutTP$importance, "barplotdata_landuse_withoutTP.csv", sep = (","))

barplotdata_landuse_withoutTP<-read.csv("~/MSc_Chl_Clm project/barplotdata_landuse_withoutTP.csv")
##trying to make stacked barplot




install.packages("RColorBrewer")
library(RColorBrewer)

#extrapolating color pallettes
nb.cols<- 33

mycolors3<-colorRampPalette(brewer.pal(33, "RdYlBu"))(nb.cols)
p<-ggplot(data=barplotdata_landuse_withoutTP, aes(x=reorder(Type,-Value), y=Value, fill=Variable))
p+geom_bar(stat="identity", width = 1, color="black", size = 1) + theme_classic() + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 0.9, size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) + scale_fill_manual(values = mycolors3)



#edit #2, no tp


data.imputed7<-rfImpute(logchl5 ~  
                        + Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13+Pro14+Pro15+Pro16+Pro17
                        + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                        + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                        + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                        +springMeantmp + summerMeantmp
                        +springMeanSrad + summerMeanSrad
                        +springMeanppt + summerMeanppt
                        +springMeancldcvr + summerMeancldcvr ,data = Usa_data, iter=1)



rfmodel_landuse<-randomForest(logchl5 ~  
                              + Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13+Pro14+Pro15+Pro16+Pro17
                              + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                              + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                              + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                              +springMeantmp + summerMeantmp
                              +springMeanSrad + summerMeanSrad
                              +springMeanppt + summerMeanppt
                              +springMeancldcvr + summerMeancldcvr ,data = data.imputed7, proximity=TRUE, ntree=500, mtry= 5)

rfmodel_landuse$importance




tpchlmodel<-lm(ChlValues ~ TP.value, data=Usa_data)

summary(tpchlmodel)


rfmodel_landuse$importance


summary(Usa_data)



data.imputed9<-rfImpute(logchl5 ~ logtp5 
                        + Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13+Pro14+Pro15+Pro16+Pro17
                        , data = landuse, iter=1)



rfmodel_landuse_only<-randomForest(logchl5 ~ logtp5
                              + Pro1+Pro2+Pro3+Pro4+Pro5+Pro6+Pro7+Pro8+Pro9+Pro10+Pro11+Pro12+Pro13+Pro14+Pro15+Pro16+Pro17   
                              , data = data.imputed9, proximity=TRUE, ntree=500, mtry= 5)

rfmodel_landuse_only
rfmodel_landuse_only$importance
rfmodel_landuse$importance



data.imputed9<-rfImpute(logchl5 ~ logtp5, data = landuse, iter=1)



rfmodel_landuse9<-randomForest(logchl5 ~ logtp5, data = landuse, proximity=TRUE, ntree=500, mtry= 5)



summary(rfmodel_landuse)
rfmodel_landuse #explains 40.71% of variation

rfmodel_landuse$importance #srad second most impt





#Random forest model

logchl3<-log10(Usa_data$ChlValues)
logtp3<-log10(Usa_data$TP.value)


library(randomForest)



is.infinite(Usa_data$ChlValues)
is.infinite(Usa_data$TP.value)


data.imputed5<-rfImpute(logchl3 ~ logtp3 
                        + springMeantmp+ summerMeantmp 
                        + springMeanppt + summerMeanppt 
                        + springMeancldcvr + summerMeancldcvr
                        + springMeanSrad + summerMeanSrad
                        + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                        + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                        + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                        , data = Usa_data, iter=1)



rfmodel_usa<-randomForest(logchl3 ~ logtp3 
                          + springMeantmp+ summerMeantmp 
                          + springMeanppt + summerMeanppt 
                          + springMeancldcvr + summerMeancldcvr
                          + springMeanSrad + summerMeanSrad
                          + HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                          + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                          + HydroLAKES.Elevation + HydroLAKES.Wshd_area
                          , data = data.imputed5, proximity=TRUE, ntree=500, mtry= 5)


summary(rfmodel_usa)
rfmodel_usa #explains 46.4% of variation

rfmodel_usa$importance #srad second most impt

write.csv(rfmodel_usa$importance, "usabarplotdata.csv", sep = ("," ))



##trying to make stacked barplot


barplotdataUS<-read.csv("~/MSc_Chl_Clm project/usabarplotdata1.csv")

library(ggplot2)
install.packages("RColorBrewer")
library(RColorBrewer)

#extrapolating color pallettes
nb.cols<- 15


mycolors3<-colorRampPalette(brewer.pal(11, "RdYlBu"))(nb.cols)


p<-ggplot(data=barplotdataUS, aes(x=Type, y=Importance, fill=Variable))

p+geom_bar(stat="identity", width = 1, color="black", size = 1) + theme_classic() + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 0.9, size = 20, color = "black"), axis.text.y = element_text(size = 20, color = "black"), axis.title.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) + scale_fill_manual(values = mycolors3)




#corrmatrix
#build correlation heatmap

library(ggcorrplot)

names(Usa_data)

#generate object for variables to be tested
corr_variables5<-Usa_data[,c(11,12,34,37,40,42,43,45,147:148,160:161,173,174,186:187)]

computecor<-(cor(corr_variables5, method = "kendall", use = "complete.obs")) #function round just rounds the numbers
computecor

p.mat_computecor <- cor_pmat(corr_variables1)
p.mat_computecor


ggcorrplot(computecor, p.mat = p.mat_computecor, hc.order = TRUE,
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


names(Usa_data)

library(lme4)
mixedmodel<-lmer(ChlValues~TP.value+(1+TP.value|Year.y), data = Usa_data)

mixedmodel<-lmer(ChlValues~TP.value+(1|Year.y), data = Usa_data)
mixedmodel
summary(mixedmodel)


install.packages("lmerTest")
library(lmerTest)
mixedmodel<-lmer(ChlValues~TP.value+(1|Year.y), data = Usa_data)
mixedmodel

car::Anova(mixedmodel)


install.packages("sjp.glmer")
summary(mixedmodel)
plot(mixedmodel)

mm_plot <- ggplot(Usa_data, aes(x = TP.value, y = ChlValues, colour = Year.y)) +
    facet_wrap(~Year.y, nrow=2) +   
    geom_point(alpha = 0.5) +
    theme_classic() 
mm_plot


mixedmodel1<-lmer(ChlValues~HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                 + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                 + HydroLAKES.Elevation + HydroLAKES.Wshd_area +(1+HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                                                                 + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                                                                 + HydroLAKES.Elevation + HydroLAKES.Wshd_area|Year.y), data = Usa_data)
summary(mixedmodel1)



mm_plot <- ggplot(Usa_data, aes(x = HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                                + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                                + HydroLAKES.Elevation + HydroLAKES.Wshd_area , y = ChlValues, colour = HydroLAKES.Lake_area + HydroLAKES.Vol_total  
                                + HydroLAKES.Depth_avg + HydroLAKES.Res_time 
                                + HydroLAKES.Elevation + HydroLAKES.Wshd_area )) +
  facet_wrap(~Year.y, nrow=2) +   
  geom_point(alpha = 0.5) +
  theme_classic() 

mm_plot





#trophic classes

oligolakes<-Usa_data %>% filter(ChlValues < 0.0026)
mesolakes<-Usa_data %>% filter(ChlValues > 0.0026, ChlValues < 0.02)
eutrophiclakes<- Usa_data %>% filter(ChlValues > 0.02, ChlValues < 0.056)
hypereutrophic<- Usa_data %>% filter(ChlValues > 0.056)

Usa_data %>% max(ChlValues)

max
