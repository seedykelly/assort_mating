
rm(list=ls())

## ---- load_libraries ----
library(tidyverse)
library(ggplot2)
library(reshape2)
library(psych)
library(geomorph)
library(Hmisc)
library(ppcor)
library(kableExtra)
#library(scales)
library(knitr)
library(cowplot)
library(broom)
library(magick)
#library(here)
library(grid)
library(png)

## ---- end

#shape analysis
# body
beetle.body<-readland.tps("japanese.bodyshape.tps",specID="ID")#7 pairs of homosexual males reclassified as single males

(dimnames(beetle.body)[[3]])#list of specimens
body.factors<-read.csv("beetlefactorsbody.csv",header=T)
links.beetle.body<-matrix(c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,15,15,14,14,4,12,1,13,14),ncol=2,byrow=T)
beetle<-list("land"=beetle.body,"links"=links.beetle.body, "sex"=body.factors$sex, "pair_id"=body.factors$pair_id,"id"=body.factors$id)
sex<-as.factor(beetle$sex)
pair_id<-as.factor(beetle$pair_id)
beetlefactors<-as.factor(paste(beetle$sex, beetle$pair_id))
slidedir<-matrix(c(1,2,3,2,3,4,3,4,5,4,5,6,5,6,7,6,7,8,7,8,9,8,9,10,9,10,11,10,11,12,11,12,1),ncol=3,byrow=T)
Y.GPA<-gpagen(beetle$land, curves=slidedir)
gdf.body_shape<-geomorph.data.frame(Y.GPA, pair_id=beetle$pair_id, sex=beetle$sex,id=beetle$id)
save(gdf.body_shape, file = "gdf.body_shape") 

#shape analysis
#wing
beetle.wing<-readland.tps("japanese.wingshape.tps",specID="ID")
(dimnames(beetle.wing)[[3]])#list of specimens
wing.factors<-read.csv("beetlefactorswing.csv",header=T)
links.beetle.wing<-matrix(c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,1),ncol=2,byrow=T)
beetle<-list("land"=beetle.wing,"links"=links.beetle.wing, "sex"=wing.factors$sex, "pair_id"=wing.factors$pair_id,"id"=wing.factors$id, "status"=wing.factors$status)
sex<-as.factor(beetle$sex)
pair_id<-as.factor(beetle$pair_id)
beetlefactors<-as.factor(paste(beetle$sex, beetle$pair_id))
slidedir<-matrix(c(3,4,5,4,5,6,5,6,7),ncol=3,byrow=T)
Y.GPA<-gpagen(beetle$land, curves=slidedir)
gdf.wing_shape<-geomorph.data.frame(Y.GPA, pair_id=beetle$pair_id, sex=beetle$sex, id=beetle$id)
save(gdf.wing_shape, file = "gdf.wing_shape")

# wing shape vector diagrams 
#males
beetle.males<-beetle.wing[,,246:361]
wing.factors.males<-wing.factors[c(246:361),]
links.beetle.wing<-matrix(c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,1),ncol=2,byrow=T)
beetle.males<-list("land"=beetle.males,"links"=links.beetle.wing, "status"=wing.factors.males$status,"sex"=wing.factors.males$sex)
slidedir<-matrix(c(3,4,5,4,5,6,5,6,7),ncol=3,byrow=T)
Y.GPA.males<-gpagen(beetle.males$land, curves=slidedir)
gdf.male.wing<-geomorph.data.frame(shape=Y.GPA.males$coords, Csize=Y.GPA.males$Csize)
save(gdf.male.wing, file = "gdf.male.wing") #this file loaded and analyzed uing procD

male_wing<-mshape(gdf.male.wing$shape)
links.beetle.wing<-matrix(c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,1),ncol=2,byrow=T)
PCA_male_wing<-plotTangentSpace(gdf.male.wing$shape)
png("male_wing_min_pc1.png", res=400,width = 6, height = 6, units = 'in')
plotRefToTarget(male_wing,PCA_male_wing$pc.shapes$PC1min,mag=6, links=links.beetle.wing)
dev.off()
png("male_wing_max_pc1.png", res=400,width = 6, height = 6, units = 'in')
plotRefToTarget(male_wing,PCA_male_wing$pc.shapes$PC1max,mag=6, links=links.beetle.wing)
dev.off()

#females
beetle.females<-beetle.wing[,,1:116]
wing.factors.females<-wing.factors[c(1:116),]
links.beetle.wing<-matrix(c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,1),ncol=2,byrow=T)
beetle.females<-list("land"=beetle.females,"links"=links.beetle.wing, "status"=wing.factors.females$status,"sex"=wing.factors.females$sex)
slidedir<-matrix(c(3,4,5,4,5,6,5,6,7),ncol=3,byrow=T)
Y.GPA.females<-gpagen(beetle.females$land, curves=slidedir)
gdf.female.wing<-geomorph.data.frame(shape=Y.GPA.females$coords, Csize=Y.GPA.females$Csize)
save(gdf.female.wing, file = "gdf.female.wing") #this file loaded and analyzed uing procD

female_wing<-mshape(gdf.female.wing$shape)
links.beetle.wing<-matrix(c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,1),ncol=2,byrow=T)
PCA_female_wing<-plotTangentSpace(gdf.female.wing$shape)
png("female_wing_min_pc1.png", res=400,width = 6, height = 6, units = 'in')
plotRefToTarget(female_wing,PCA_female_wing$pc.shapes$PC1min,mag=6, links=links.beetle.wing)
dev.off()
png("female_wing_max_pc1.png", res=400,width = 6, height = 6, units = 'in')
plotRefToTarget(female_wing,PCA_female_wing$pc.shapes$PC1max,mag=6, links=links.beetle.wing)
dev.off()

## ---- shape-analysis ----
load(file = "gdf.body_shape")
body.factors<-read.csv("beetlefactorsbody.csv",header=T)
beetle.body.shape<-gm.prcomp(gdf.body_shape$coords, sex=gdf.body_shape$sex, pair_id=gdf.body_shape$pair_id, status=gdf.body_shape$status)
pc_1.body<-as.data.frame(beetle.body.shape$x[,1])
pc_2.body<-as.data.frame(beetle.body.shape$x[,2])
pc_3.body<-as.data.frame(beetle.body.shape$x[,3])
Csize.body<-as.data.frame(log(gdf.body_shape$Csize))

total.body.shape.data<-cbind(body.factors$sex,body.factors$pair_id,body.factors$id,body.factors$status,pc_1.body,pc_2.body,pc_3.body,Csize.body)
names(total.body.shape.data) <- c("sex", "pair_id", "id","status", "pc1.body", "pc2.body","pc3.body","Csize.body")

load(file = "gdf.wing_shape")
wing.factors<-read.csv("beetlefactorswing.csv",header=T)
beetle.wing.shape<-gm.prcomp(gdf.wing_shape$coords, sex=gdf.wing_shape$sex, pair_id=gdf.wing_shape$pair_id)
pc_1.wing<-as.data.frame(beetle.wing.shape$x[,1])
pc_2.wing<-as.data.frame(beetle.wing.shape$x[,2])
pc_3.wing<-as.data.frame(beetle.wing.shape$x[,3])
Csize.wing<-as.data.frame(log(gdf.wing_shape$Csize))

total.wing.shape.data<-cbind(wing.factors$sex,wing.factors$pair_id, wing.factors$id,wing.factors$status,pc_1.wing,pc_2.wing,pc_3.wing,Csize.wing)
names(total.wing.shape.data) <- c("sex", "pair_id", "id","status","pc1.wing", "pc2.wing","pc3.wing", "Csize.wing")

#put the body and wing data in one table
wing.body<-merge(total.body.shape.data,total.wing.shape.data, by="id")
# body and wing correlation for all beetles
wing.body.female <- wing.body %>%
  filter(sex.x=="female")
female.corr<-rcorr(wing.body.female$Csize.body,wing.body.female$Csize.wing)
wing.body.male <- wing.body %>%
  filter(sex.x=="male")
male.corr<-rcorr(wing.body.male$Csize.body,wing.body.male$Csize.wing)

#pairs
#library(plyr)
wing.body.total.1<-filter(wing.body,status.x=="pair")
wing.body.total.2<-wing.body.total.1 %>%
  dplyr::select(-c(id,sex.y, pair_id.y,status.y)) %>%
  gather(trait, value, pc1.body:Csize.wing) %>% 
  group_by(pair_id.x,trait)
#detach("package:plyr", unload=TRUE)
wing.body.total.3<-unite(wing.body.total.2, "trait.2", "sex.x","trait", sep="_")
wing.body.total.4<-spread(wing.body.total.3, "trait.2", "value")

#body shape partial correlations
male_pc1_body<-pcor.test(wing.body.total.4$female_pc1.body,wing.body.total.4$male_pc1.body,wing.body.total.4[,c("female_Csize.body", "male_Csize.body")])
male_pc2_body<-pcor.test(wing.body.total.4$female_pc2.body,wing.body.total.4$male_pc2.body,wing.body.total.4[,c("female_Csize.body", "male_Csize.body")])
male_pc3_body<-pcor.test(wing.body.total.4$female_pc3.body,wing.body.total.4$male_pc3.body,wing.body.total.4[,c("female_Csize.body", "male_Csize.body")])

body.Csize<-(rcorr(wing.body.total.4$female_Csize.body,wing.body.total.4$male_Csize.body))

## wing shape partial correlations
male_pc1_wing<-pcor.test(wing.body.total.4$female_pc1.wing,wing.body.total.4$male_pc1.wing,wing.body.total.4[,c("female_Csize.wing", "male_Csize.wing")])
male_pc2_wing<-pcor.test(wing.body.total.4$female_pc2.wing,wing.body.total.4$male_pc2.wing,wing.body.total.4[,c("female_Csize.wing", "male_Csize.wing")])
male_pc3_wing<-pcor.test(wing.body.total.4$female_pc3.wing,wing.body.total.4$male_pc3.wing,wing.body.total.4[,c("female_Csize.wing", "male_Csize.wing")])

wing.Csize<-(rcorr(wing.body.total.4$female_Csize.wing,wing.body.total.4$male_Csize.wing))

## ---- end 

## clean data from master file
beetle.data_11<-beetle.data %>%
  select(c(sex, pair_id,Body.length.1, Body.length.2, Elytral.width.1,Elytral.width.2, wing.length.1,wing.length.2))
beetle.data_11$body.length<-(rowMeans(beetle.data_11[,3:4],na.rm=TRUE))###
beetle.data_11$elytra.width<-(rowMeans(beetle.data_11[,5:6],na.rm=TRUE))
beetle.data_11$wing.length<-(rowMeans(beetle.data_11[,7:8],na.rm=TRUE))
beetle.data_12<-beetle.data_11 %>%
  select(c(sex, pair_id,body.length, elytra.width,wing.length))
save(beetle.data_12, file = "all_beetles_size_data.RData") #this file loaded and analyzed uing procD

## size analysis: pairs
beetle.data_1<-beetle.data %>%
  select(c(sex, pair_id,Body.length.1, Body.length.2, Elytral.width.1,Elytral.width.2, wing.length.1,wing.length.2)) %>%
  filter(!pair_id=="single")
beetle.data_1$body.length<-(rowMeans(beetle.data_1[,3:4],na.rm=TRUE))###
beetle.data_1$elytra.width<-(rowMeans(beetle.data_1[,5:6],na.rm=TRUE))
beetle.data_1$wing.length<-(rowMeans(beetle.data_1[,7:8],na.rm=TRUE))
beetle.data_2<-beetle.data_1 %>%
  select(c(sex, pair_id,body.length, elytra.width,wing.length)) %>%
  gather(trait, value, body.length:wing.length) %>% 
  group_by(sex,trait)
beetle.data_3<-tidyr::unite(beetle.data_2, "trait.2", "sex","trait", sep="_")
beetle.data_4<-tidyr::spread(beetle.data_3, "trait.2", "value") %>%
  na.omit()
save(beetle.data_4, file = "paired_beetles_size_data.RData") #this file loaded and analyzed uing procD

## ---- size-analysis ----
load("paired_beetles_size_data.RData")
load("all_beetles_size_data.RData")
source("format_pval.R")
source("corstars.R")

## size analysis: all beetles
female.size <- beetle.data_12 %>%
  filter(sex=="female")
male.size <- beetle.data_12 %>%
  filter(sex=="male")
corr.table<-rbind(corstars(female.size[,3:5]),corstars(male.size[,3:5]))
library(tibble)
corr.table.2<-rownames_to_column(corr.table, var="trait")
corr.table.3<-mutate(corr.table.2,trait = dplyr::recode(trait,"body.length" = "Body length",
                                                       "elytra.width"="Elytra width",
                                                       "wing.length"= "Wing length",
                                                       "body.length1"="Body length",
                                                       "elytra.width1"="Elytra width",
                                                       "wing.length1"="Wing length"))
## size association within pairs
mf.elytra<-beetle.data_4[,c(3,6)]
mf.body<-beetle.data_4[,c(2,5)]
mf.wing<-beetle.data_4[,c(4,7)]

#apparent or true assortative mating
body.assort.1<-lm(male_body.length~female_body.length, data=beetle.data_4)$residuals
spear.corr.body<-rcorr(abs(body.assort.1),beetle.data_4$female_body.length, type = "spearman")
wing.assort.1<-lm(male_wing.length~female_wing.length, data=beetle.data_4)$residuals
spear.corr.wing<-rcorr(abs(wing.assort.1),beetle.data_4$female_wing.length, type = "spearman")
centroid.wing.1<-lm(male_Csize.wing~female_Csize.wing, data=wing.body.total.4)$residuals
spear.corr.centroid<-rcorr(abs(centroid.wing.1),wing.body.total.4$female_Csize.wing, type = "spearman")
wing.shape<-lm(male_pc1.wing~female_pc1.wing, data=wing.body.total.4)$residuals
spear.corr.wingshape<-rcorr(abs(wing.shape),wing.body.total.4$female_pc1.wing, type = "spearman")

#PC1 controlling for wing centroid size
female.PC1.res<-lm(female_pc1.wing~female_Csize.wing, data=wing.body.total.4)$residuals
male.PC1.res<-lm(male_pc1.wing~male_Csize.wing, data=wing.body.total.4)$residuals
wing.residuals<-as.data.frame(cbind(female.PC1.res,male.PC1.res))

body_length.assort<-rcorr(as.matrix(mf.body))$r[1,2]
wing_length.assort<-rcorr(as.matrix(mf.wing))$r[1,2]
corr.test.mf<-r.test(n=116, body_length.assort,wing_length.assort)

## ---- density-plot ----
load(file="beetle_data.RData")
beetle_data$body.length<-(rowMeans(beetle_data[,6:7],na.rm=TRUE))
## ---- end 
