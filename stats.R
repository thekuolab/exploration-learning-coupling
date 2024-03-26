library(tidyverse)
library(nlme)
library(emmeans)
library(effectsize)
library(merDeriv)
library(outliers)
library(DHARMa)
library(r2glmm)
library(mgcv)
library(GGally)
source("HighstatLibV10.R")

# 1. Exploration
# 1.1 Simple plotting
boldness<-read_csv("boldness_raw.csv")
boldness$distance_moved_st<-boldness$distance_moved/boldness$total_time
boldness$time_novel_st<-boldness$time_novel/boldness$total_time

pca<-prcomp(boldness[,-c(1:3,6:8)],center = TRUE,scale. = TRUE)
print(pca)
summary(pca)

boldness$pc1<-pca$x[,1]
boldness$pc2<-pca$x[,2]

write_csv(boldness,"boldness.csv")

ggplot(data = boldness,aes(x = as.factor(exp),y = pc1))+
  theme_bw()+
  geom_point()+
  geom_text(aes(label = id,vjust = 1))+
  geom_line(aes(group = id))

# 1.2 Stats
lm_pc1.1<-lme(pc1~as.factor(exp),random = ~1|id,data = boldness,method = "ML")
lm_pc1.2<-lme(pc1~as.factor(exp)*as.factor(sex),random = ~1|id,data = boldness,method = "ML")
lm_pc1.3<-lme(pc1~as.factor(exp)+as.factor(sex),random = ~1|id,data = boldness,method = "ML")
AIC(lm_pc1.1,lm_pc1.2,lm_pc1.3)  

summary(lm_pc1.2)

boldness_avg<-data.frame(matrix(0,ncol=3,nrow=length(unique(boldness$id))))
colnames(boldness_avg)<-c("id","sex","boldness_avg")

for (i in 1:nrow(boldness_avg)){
  
  boldness_sub<-subset(boldness,subset = id==unique(boldness$id)[i])
  boldness_avg[i,]$id<-boldness_sub$id[1]
  boldness_avg[i,]$sex<-boldness_sub$sex[1]
  boldness_avg[i,]$boldness_avg<-mean(boldness_sub$pc1)
  
}

# 2. Reverse learning
# 2.1 Setting up raw data 
df<-read_csv("reverse learning.csv")
df$'prey1_2'<-ifelse(df$'prey1'=="P",1,0)
df$percent_unpalatable_50<-df$unpalatable_50/df$total_50
df$percent_unpalatable_consumed<-df$unpalatable_consumed/df$total_consumed

df$group2<-ifelse(df$group=="Y","YR","RY")

lizards<-unique(df$id)

for (i in 1:length(lizards)){
  
  df[df$id==lizards[i],]$group2<-df[df$id==lizards[i],]$group2[1]
  
}

write_csv(df,"reverse learning.csv")

dff<-merge(x = df,y = boldness_avg,by = c("id","sex"),no.dups = TRUE)
dff2<-dff[is.na(dff$prey1_2)==FALSE,]

# 2.2 Foraging priority
dff3<-dff[is.na(dff$percent_unpalatable_50)==FALSE,]
df_unp50<-dff3[,c(2,4,8,10,17,18)]
reg_unp50_all<-glm(cbind(unpalatable_50,palatable_50)~.^2,data = df_unp50,family = binomial(link = "cloglog"))

selection<-step(reg_unp50_all,direction = "backward")
summary(selection)

reg_unp50_best<-glm(formula = cbind(unpalatable_50, palatable_50) ~ sex + exp + 
                      group2 + boldness_avg + sex:exp + sex:group2 + exp:group2 + 
                      group2:boldness_avg, family = binomial(link = "cloglog"), 
                    data = df_unp50)

# Testing for overdispersion
overdisp_fun(reg_unp50_best)
summary(reg_unp50_best)
# McFadden's pseudo R^2
with(summary(reg_unp50_best), 1 - deviance/null.deviance)

# Model validation
library(DHARMa)
plot(E<-simulateResiduals(reg_unp50_best))

par(mfrow=c(2,2))
plotResiduals(E,as.factor(df_unp50$boldness_avg))
plotResiduals(E,as.factor(df_unp50$group2))

# Effect size
library(merDeriv)
params_reg_unp50<-parameters::model_parameters(reg_unp50_best) 
d_unp50<-oddsratio_to_d(params_reg_unp50$Coefficient,log = TRUE)
interpret_unp50<-interpret_cohens_d(d_unp50,rules = "gignac2016")
effectsize_unp50<-data.frame(cbind(params_reg_unp50,d_unp50,interpret_unp50))

write_csv(effectsize_unp50,"effectsize_unp50.csv")

# Plotting
df_unp50$boldness_cat<-ifelse(df_unp50$boldness_avg>=median(df_unp50$boldness_avg),"shy","bold")

unp50means_by_sex<-aggregate(unpalatable_50/(unpalatable_50+palatable_50)~sex*exp,data = df_unp50,FUN = mean)
colnames(unp50means_by_sex)[3]<-"mean"

unp50means_by_boldness_group<-aggregate(unpalatable_50/(unpalatable_50+palatable_50)~boldness_cat*group2*exp,data = df_unp50,FUN = mean)
colnames(unp50means_by_boldness_group)[4]<-"mean"

# By sex
unp50_exp_f<-ggplot(data = subset(df_unp50,subset = sex=="f"),aes(x = as.factor(exp),y = unpalatable_50/(unpalatable_50+palatable_50)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unp50means_by_sex,subset = sex == "f"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unp50means_by_sex,subset = sex == "f"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unp50means_by_sex,subset = sex == "f"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("")+
  ylab("% bitter in 1st 50% attacked")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(a)")

unp50_exp_m<-ggplot(data = subset(df_unp50,subset = sex=="m"),aes(x = as.factor(exp),y = unpalatable_50/(unpalatable_50+palatable_50)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unp50means_by_sex,subset = sex == "m"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unp50means_by_sex,subset = sex == "m"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unp50means_by_sex,subset = sex == "m"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("")+
  ylab("")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(b)")

# By treatment x exploration
unp50_RY_shy<-ggplot(data = subset(df_unp50,subset = group2=="RY"&boldness_cat=="shy"),aes(x = as.factor(exp),y = unpalatable_50/(unpalatable_50+palatable_50)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unp50means_by_boldness_group,subset = group2=="RY"&boldness_cat=="shy"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unp50means_by_boldness_group,subset = group2=="RY"&boldness_cat=="shy"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unp50means_by_boldness_group,subset = group2=="RY"&boldness_cat=="shy"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("")+
  ylab("% bitter in 1st 50% attacked")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(a)")

unp50_RY_bold<-ggplot(data = subset(df_unp50,subset = group2=="RY"&boldness_cat=="bold"),aes(x = as.factor(exp),y = unpalatable_50/(unpalatable_50+palatable_50)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unp50means_by_boldness_group,subset = group2=="RY"&boldness_cat=="bold"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unp50means_by_boldness_group,subset = group2=="RY"&boldness_cat=="bold"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unp50means_by_boldness_group,subset = group2=="RY"&boldness_cat=="bold"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("")+
  ylab("")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(b)")

unp50_YR_shy<-ggplot(data = subset(df_unp50,subset = group2=="YR"&boldness_cat=="shy"),aes(x = as.factor(exp),y = unpalatable_50/(unpalatable_50+palatable_50)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unp50means_by_boldness_group,subset = group2=="YR"&boldness_cat=="shy"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unp50means_by_boldness_group,subset = group2=="YR"&boldness_cat=="shy"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unp50means_by_boldness_group,subset = group2=="YR"&boldness_cat=="shy"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("Trial")+
  ylab("% bitter in 1st 50% attacked")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(c)")

unp50_YR_bold<-ggplot(data = subset(df_unp50,subset = group2=="YR"&boldness_cat=="bold"),aes(x = as.factor(exp),y = unpalatable_50/(unpalatable_50+palatable_50)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unp50means_by_boldness_group,subset = group2=="YR"&boldness_cat=="bold"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unp50means_by_boldness_group,subset = group2=="YR"&boldness_cat=="bold"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unp50means_by_boldness_group,subset = group2=="YR"&boldness_cat=="bold"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("Trial")+
  ylab("")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(d)")

# 2.2 Overall avoidance
dff4<-dff[is.na(dff$percent_unpalatable_consumed)==FALSE,]
df_unpcons<-dff4[,c(2,4,11,13,17,18)]

# GLM
unpcons_alltrials_start<-glm(cbind(unpalatable_consumed,palatable_consumed)~.^2,data = df_unpcons,family = binomial(link = "cloglog"))

selection<-step(unpcons_alltrials_start,direction = "backward")
summary(selection)

unpcons_alltrials_best<-glm(formula = cbind(unpalatable_consumed, palatable_consumed) ~ 
                              sex + exp + group2 + boldness_avg + sex:boldness_avg + exp:boldness_avg + 
                              group2:boldness_avg, family = binomial(link = "cloglog"), 
                            data = df_unpcons)

# Model validation - slight underdispersion b/c unequal variances between sexes
plot(E<-simulateResiduals(unpcons_alltrials_best))
plotResiduals(E,as.factor(df_unpcons$exp))
plotResiduals(E,as.factor(df_unpcons$group2))

# Summary
summary(unpcons_alltrials_best)

# Effect size
params_glm_unpcons<-parameters::model_parameters(unpcons_alltrials_best) 
d_unpcons<-oddsratio_to_d(params_glm_unpcons$Coefficient,log = TRUE)
interpret_unpcons<-interpret_cohens_d(d_unpcons,rules = "gignac2016")
effectsize_unpcons<-data.frame(cbind(params_glm_unpcons,d_unpcons,interpret_unpcons))

write_csv(effectsize_unpcons,"effectsize_unpcons.csv")

# Plotting
df_unpcons$boldness_cat<-ifelse(df_unpcons$boldness_avg>=median(df_unpcons$boldness_avg),"shy","bold")

# By sex
unpconsmeans_by_sex<-aggregate(unpalatable_consumed/(unpalatable_consumed+palatable_consumed)~sex*exp,data = df_unpcons,FUN = mean)
colnames(unpconsmeans_by_sex)[3]<-"mean"

unpcons_f<-ggplot(data = subset(df_unpcons,subset = sex=="f"),aes(x = as.factor(exp),y = unpalatable_consumed/(unpalatable_consumed+palatable_consumed)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unpconsmeans_by_sex,subset = sex=="f"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unpconsmeans_by_sex,subset = sex=="f"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unpconsmeans_by_sex,subset = sex=="f"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("Trial")+
  ylab("% consumed that were bitter")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(c)")

unpcons_m<-ggplot(data = subset(df_unpcons,subset = sex=="m"),aes(x = as.factor(exp),y = unpalatable_consumed/(unpalatable_consumed+palatable_consumed)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(means_by_sex,subset = sex=="m"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(means_by_sex,subset = sex=="m"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(means_by_sex,subset = sex=="m"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("Trial")+
  ylab("")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(d)")


# By boldness*group
unpconsmeans_by_boldness_group<-aggregate(unpalatable_consumed/(unpalatable_consumed+palatable_consumed)~boldness_cat*group2*exp,data = df_unpcons,FUN = mean)
colnames(unpconsmeans_by_boldness_group)[4]<-"mean"

unpcons_RY_shy<-ggplot(data = subset(df_unpcons,subset = group2=="RY"&boldness_cat=="shy"),aes(x = as.factor(exp),y = unpalatable_consumed/(unpalatable_consumed+palatable_consumed)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unpconsmeans_by_boldness_group,subset = group2=="RY"&boldness_cat=="shy"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unpconsmeans_by_boldness_group,subset = group2=="RY"&boldness_cat=="shy"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unpconsmeans_by_boldness_group,subset = group2=="RY"&boldness_cat=="shy"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("")+
  ylab("% consumed that were bitter")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(a)")

unpcons_RY_bold<-ggplot(data = subset(df_unpcons,subset = group2=="RY"&boldness_cat=="bold"),aes(x = as.factor(exp),y = unpalatable_consumed/(unpalatable_consumed+palatable_consumed)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unpconsmeans_by_boldness_group,subset = group2=="RY"&boldness_cat=="bold"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unpconsmeans_by_boldness_group,subset = group2=="RY"&boldness_cat=="bold"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unpconsmeans_by_boldness_group,subset = group2=="RY"&boldness_cat=="bold"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("")+
  ylab("")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(b)")

unpcons_YR_shy<-ggplot(data = subset(df_unpcons,subset = group2=="YR"&boldness_cat=="shy"),aes(x = as.factor(exp),y = unpalatable_consumed/(unpalatable_consumed+palatable_consumed)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unpconsmeans_by_boldness_group,subset = group2=="YR"&boldness_cat=="shy"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unpconsmeans_by_boldness_group,subset = group2=="YR"&boldness_cat=="shy"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unpconsmeans_by_boldness_group,subset = group2=="YR"&boldness_cat=="shy"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("Trial")+
  ylab("% consumed that were bitter")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(c)")

unpcons_YR_bold<-ggplot(data = subset(df_unpcons,subset = group2=="YR"&boldness_cat=="bold"),aes(x = as.factor(exp),y = unpalatable_consumed/(unpalatable_consumed+palatable_consumed)))+
  geom_boxplot(aes(alpha = 0.3))+
  geom_point(position = position_jitter(width = 0.1),alpha = 0.3,size = 3)+
  geom_point(data = subset(unpconsmeans_by_boldness_group,subset = group2=="YR"&boldness_cat=="bold"),aes(x = as.factor(exp),y = mean,color = "red",size =3),alpha = 0.5)+
  geom_line(data = subset(unpconsmeans_by_boldness_group,subset = group2=="YR"&boldness_cat=="bold"&exp < 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_line(data = subset(unpconsmeans_by_boldness_group,subset = group2=="YR"&boldness_cat=="bold"&exp >= 6),aes(x = exp,y = mean),stat = "smooth",method = "lm",se = FALSE,color = "red",alpha = 0.8,linetype = 2,linewidth=1)+
  geom_vline(xintercept = 5.5,linetype = 2)+
  xlab("Trial")+
  ylab("")+
  coord_cartesian(ylim=c(0,1))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        plot.title = element_text(size=18),
        legend.position = "")+
  ggtitle("(d)")

# figure 1
gridExtra::grid.arrange(unp50_RY_shy,unp50_RY_bold,unp50_YR_shy,unp50_YR_bold,ncol=2)

# figure 2
gridExtra::grid.arrange(unpcons_RY_shy,unpcons_RY_bold,unpcons_YR_shy,unpcons_YR_bold,ncol=2)

# figure 3
gridExtra::grid.arrange(unp50_exp_f,unp50_exp_m,unpcons_f,unpcons_m,ncol=2)

# ANOVA comparing # of crickets consumed per trial between slow and fast explorers
aggregate((unpalatable_consumed+palatable_consumed)~boldness_cat,data = df_unpcons,FUN = mean)
anova(lm((unpalatable_consumed+palatable_consumed)~boldness_cat,data = df_unpcons))
