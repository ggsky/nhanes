rm(list = ls()) 
setwd("C:/Users/wy195/Desktop/Z241230，150+350") 

library(readxl)
library(pROC)
library(rms)
library(Hmisc)
library(ggplot2)
library(ResourceSelection)
library(rmda)
library(tidyverse)
library(plyr)
library(survey)
library(extrafont)
loadfonts(device="win")
par(family="Times New Roman")
# library(showtext)
# showtext.auto()

#1.import data
charls <- read_excel("./charls.xlsx") 

#2.missing_rate
colSums(is.na(charls))
charls[charls == "NULL"] <- NA
colSums(is.na(charls))

charls<-charls[!is.na(charls$TyG) & !is.na(charls$心脏病),]
colSums(is.na(charls)*100/nrow(charls))

#3.transform type
charls<-charls[,c('心脏病','TyG',setdiff(colnames(charls)[c(1:26)],'心脏病'))]
colnames(charls)[1]<-'grp'
table(charls$grp)
charls$grp<-ifelse(charls$grp==1,1,0)

cnt<-lapply(charls, function(x) {
  length(unique(x))
})
cnt<-do.call(rbind,cnt)
cnt<-data.frame(var=rownames(cnt),cnt=cnt)
fvar<-cnt[cnt$cnt<=7,]$var[-1]
charls[,fvar]<-lapply(charls[,fvar],as.factor)
charls[,setdiff(colnames(charls),fvar)]<-lapply(charls[,setdiff(colnames(charls),fvar)],as.numeric)
str(charls)
summary(charls)
colSums(is.na(charls)*100/nrow(charls))


#4.mi
library(mice)
mi<-mice(charls,seed = 123)
df<-complete(mi)
colSums(is.na(df)*100/nrow(df))
summary(df)

#5.RCS
dd <- datadist(df) 
options(datadist='dd') 
fit<- lrm(grp~rcs(TyG, 4), data =df)
p<-sprintf('%.3f',anova(fit)[2,3])
p_value <- ifelse(as.numeric(p) < 0.001, '<0.001', paste0('=',p))
OR<-Predict(fit,TyG,ref.zero = TRUE)
ggplot()+
  geom_line(data=OR, aes(TyG,yhat),
            linetype="solid",size=1,alpha = 0.7,color='red')+
  geom_ribbon(data=OR, 
              aes(TyG,ymin = lower, ymax = upper),
              alpha = 0.1, fill = "red")+
  geom_hline(yintercept=0, linetype=2,size=1)+
  geom_vline(xintercept=8.53, linetype=2,size=1)+
  labs(x = "TyG level",y = "OR(95%CI)")+
  geom_text(aes(x = 8, y = 1, label = paste0("P for nonlinear ", p_value)),
              family = "Times New Roman", size = 5) +
  theme_classic() 
# theme(text = element_text(family='Times New Roman',size=14))+
#   scale_x_continuous(breaks = c(0, 5, 10, 15))  


#6.baseline table
library(crosstable)
crosstable(df,by='grp',test = T,percent_digits = 2,num_digits =2,percent_pattern = "{n} ({p_col})")%>%as_flextable(compact=T)

#7.OR1
#model1
quartiles <- quantile(df$TyG, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
df$quantile2 <- cut(df$TyG, 
                 breaks = c(-Inf, quartiles[1], quartiles[2], quartiles[3], Inf),
                 labels = c('Q1', 'Q2', 'Q3', 'Q4'),
                 include.lowest = TRUE)
glm<-glm(grp~quantile2,family = binomial(),df)
glm2<-summary(glm)
OR<-round(exp(coef(glm)),3)
SE<-glm2$coefficients[,2]
CI5<-round(exp(coef(glm)-1.96*SE),3)
CI95<-round(exp(coef(glm)+1.96*SE),3)
OR1<-paste0(OR,'(',CI5,'-',CI95,')')
P<-round(glm2$coefficients[,4],3)
multi<-data.frame("variable"=rownames(glm2$coefficients),
                  "OR"=OR1,
                  "P"=P)[-1,]
multi

#model2
form2 <- as.formula(paste('grp~', paste0(colnames(df)[c(3,5:18,28)], collapse = '+')))
glm<-glm(form2,family = binomial(),df)
glm2<-summary(glm)
OR<-round(exp(coef(glm)),3)
SE<-glm2$coefficients[,2]
CI5<-round(exp(coef(glm)-1.96*SE),3)
CI95<-round(exp(coef(glm)+1.96*SE),3)
OR1<-paste0(OR,'(',CI5,'-',CI95,')')
P<-round(glm2$coefficients[,4],3)
multi<-data.frame("variable"=rownames(glm2$coefficients),
                  "OR"=OR1,
                  "P"=P)[-1,]
multi<-multi[grepl('quantile2',multi$variable),]
multi


#8.敏感性分析—非高血压人群

#9.线性相关
lm<-lm(糖化血红蛋白~quantile2,df)
sum<-summary(lm)
ci<-paste0(round(coef(lm),3),'(',round(confint(lm)[,1],3),'-',round(confint(lm)[,2],3),')')  
P<-round(sum$coefficients[,4],3)
multi<-data.frame("variable"=rownames(sum$coefficients),
                  "beta"=ci,
                  "P"=P)[-1,]
multi


form3 <- as.formula(paste('糖化血红蛋白~', paste0(colnames(df)[c(3,5:18,28)], collapse = '+')))
lm<-lm(form3,df)
sum<-summary(lm)
ci<-paste0(round(coef(lm),3),'(',round(confint(lm)[,1],3),'-',round(confint(lm)[,2],3),')')  
P<-round(sum$coefficients[,4],3)
multi<-data.frame("variable"=rownames(sum$coefficients),
                  "beta"=ci,
                  "P"=P)[-1,]
multi

#10.subgroup
library(jstable)
forestfm<-TableSubgroupMultiGLM(grp~TyG,var_subgroups=c("性别","高血压","糖尿病",'哮喘','中风'),
                      data=df,family="binomial")

forestfm2<-forestfm[2:16,]
forestfm2$Count<-ifelse(!is.na(forestfm2$Count),forestfm2$Count,'')
forestfm2$Percent<-ifelse(!is.na(forestfm2$Percent),forestfm2$Percent,'')
forestfm2$OR<-ifelse(!is.na(forestfm2$OR),forestfm2$OR,'')
forestfm2$Lower<-ifelse(!is.na(forestfm2$Lower),forestfm2$Lower,'')
forestfm2$Upper<-ifelse(!is.na(forestfm2$Upper),forestfm2$Upper,'')
forestfm2$`P value`<-gsub('[c(TyG = )]','',forestfm2$`P value`)
forestfm2$`P value`<-ifelse(!is.na(forestfm2$`P value`),forestfm2$`P value`,'')
forestfm2$`P for interaction`<-ifelse(!is.na(forestfm2$`P for interaction`),forestfm2$`P for interaction`,'')
forestfm2[,4:6]<-lapply(forestfm2[,4:6],as.numeric)

library(forestploter)
str(forestfm2)
forestfm2$'OR(95%CI)'<-ifelse(!is.na(forestfm2$OR),paste0(forestfm2$OR,'(',forestfm2$Lower,', ',forestfm2$Upper,')'),'')
forestfm2$` `<-paste(rep(" ",30),collapse=" ")

tm <- forest_theme(base_size = 10,  #字体大小
                   ci_pch = 15,  #控制置信区间点的形状
                   ci_col = "#509579", #CI颜色 
                   ci_alpha = 0.8,  # 置信区间透明度    
                   ci_lty = 1,# 置信区间线型          
                   ci_lwd = 2,      
                   ci_Theight = 0.2, 
                   vertline_lwd = 1,            
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   footnote_gp=gpar(cex = 0.6, fontface="italic", family="serif"),
                   base_family = "serif")

p <- forest(forestfm2[, c(1:3,10,9,7,8)], # 指定要显示的列
            est = forestfm2$OR,   # 估计值
            lower = forestfm2$Lower,#置信区间下限
            upper = forestfm2$Upper,# 置信区间上限
            sizes = 0.8,
            xlim=c(0,6),        
            ci_column =4 ,     # 置信区间所在列
             ref_line = 1,      # 参考线的位置
            theme = tm         # 应用之前定义的主题
)
p


#11.OR1
#model1
quartiles <- quantile(df$TyG, probs =0.5, na.rm = TRUE)
df$quantile2 <- cut(df$TyG, 
                    breaks = c(-Inf, quartiles[1], Inf),
                    labels = c('Q1', 'Q2'),
                    include.lowest = TRUE)
glm<-glm(grp~quantile2,family = binomial(),df)
glm2<-summary(glm)
OR<-round(exp(coef(glm)),3)
SE<-glm2$coefficients[,2]
CI5<-round(exp(coef(glm)-1.96*SE),3)
CI95<-round(exp(coef(glm)+1.96*SE),3)
OR1<-paste0(OR,'(',CI5,'-',CI95,')')
P<-round(glm2$coefficients[,4],3)
multi<-data.frame("variable"=rownames(glm2$coefficients),
                  "OR"=OR1,
                  "P"=P)[-1,]
multi

#modfel2
glm<-glm(form2,family = binomial(),df)
glm2<-summary(glm)
OR<-round(exp(coef(glm)),3)
SE<-glm2$coefficients[,2]
CI5<-round(exp(coef(glm)-1.96*SE),3)
CI95<-round(exp(coef(glm)+1.96*SE),3)
OR1<-paste0(OR,'(',CI5,'-',CI95,')')
P<-round(glm2$coefficients[,4],3)
multi<-data.frame("variable"=rownames(glm2$coefficients),
                  "OR"=OR1,
                  "P"=P)[-1,]
multi<-multi[grepl('quantile2',multi$variable),]
multi

#12.中介分析
set.seed(123)
library(mediation)
Y2<-lm(糖化血红蛋白~TyG,data=df)
Y3<-glm(grp~糖化血红蛋白+TyG,data=df,family=binomial)
med<-mediate(Y2,Y3,treat='TyG',mediator = '糖化血红蛋白',boot =T,sims = 1000)
summary(med)
plot(med)


