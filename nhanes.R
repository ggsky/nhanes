rm(list = ls()) 
setwd("C:/Users/wy195/Desktop/Z241230，150+350+50+350/assign4") 
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
nhanes <- read_excel("./NHANES数据.xlsx") 
str(nhanes)

#2.missing_rate
nhanes[nhanes == "NULL"] <- NA
nhanes<-nhanes[!is.na(nhanes$TyG) & !is.na(nhanes$心脏病) & nhanes$心脏病!=9 & nhanes$心脏病!=7,]
colSums(is.na(nhanes)*100/nrow(nhanes))

#3.transform type
nhanes<-nhanes[,c('心脏病','TyG',setdiff(colnames(nhanes),c('心脏病','TyG')))]
colnames(nhanes)[1]<-'grp'
table(nhanes$grp)
nhanes$grp<-ifelse(nhanes$grp==1,1,0)

cnt<-lapply(nhanes, function(x) {
  length(unique(x))
})
cnt<-do.call(rbind,cnt)
cnt<-data.frame(var=rownames(cnt),cnt=cnt)
fvar<-cnt[cnt$cnt<=5,]$var[-1]
nhanes[,fvar]<-lapply(nhanes[,fvar],as.factor)
nhanes[,setdiff(colnames(nhanes),fvar)]<-lapply(nhanes[,setdiff(colnames(nhanes),fvar)],as.numeric)
str(nhanes)
summary(nhanes)
colSums(is.na(nhanes)*100/nrow(nhanes))

nhanes$高血压[nhanes$高血压=='9'] <- NA
nhanes$糖尿病[nhanes$糖尿病=='9' | nhanes$糖尿病=='3'] <- NA
nhanes$哮喘[nhanes$哮喘=='9'] <- NA
nhanes$关节炎[nhanes$关节炎=='9'] <- NA
nhanes$中风[nhanes$中风=='9'] <- NA
nhanes$高血压 <- factor(nhanes$高血压)
nhanes$糖尿病 <- factor(nhanes$糖尿病)
nhanes$哮喘 <- factor(nhanes$哮喘)
nhanes$关节炎<-factor(nhanes$关节炎)
nhanes$中风 <- factor(nhanes$中风)

round(colSums(is.na(nhanes)*100/nrow(nhanes)),3)
dvar<-c('教育','LDL','CRP')
nhanes<-nhanes[,setdiff(colnames(nhanes),dvar)]

#4.mi
library(mice)
mi<-mice(nhanes,seed = 123)
df<-complete(mi)
colSums(is.na(df)*100/nrow(df))
summary(df)

#5.weighted RCS
svy <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~FINAL_WEIGHT, data = df,nest=TRUE)
quantile(df$TyG,c(0.05,0.5,0.95))
q1<-quantile(df$TyG,c(0.05,0.5,0.95))[1]
q2<-quantile(df$TyG,c(0.05,0.5,0.95))[2]
q3<-quantile(df$TyG,c(0.05,0.5,0.95))[3]
svy.fit<-svyglm(grp~rcs(TyG,c(7.655391,8.664923,10.013563)),design=svy,data=df,family=quasibinomial)
lnr<- as.data.frame(predict(svy.fit,se= TRUE,newdata=df,ref.zero = TRUE)) 
lnr_ref<- lnr$link-lnr$link[(which(df$TyG==median(df$TyG)))[1]]
OR_ref<-exp(lnr_ref)
df$OR_ref<-OR_ref
p1<-ggplot()+theme_classic()+
  geom_line(data=df, aes(TyG,OR_ref),linetype="solid",size=1,alpha = 1)+
  # scale_x_continuous(limits=c(5,12),breaks=seq(5,20,2.5))+   
  # scale_y_continuous(limits=c(0,1.5),breaks=seq(0,1.5,0.25))+ 
  geom_hline(yintercept=1,linetype=2,size=0.25)
p1

library(boot)
bs <- function(formula, data, indices) {
  d <- data[indices,]
  svy <- svydesign(data=d, id=~SDMVPSU, strata=~SDMVSTRA, weights=~FINAL_WEIGHT, nest=TRUE)
  svy.fit <- svyglm(formula,design=svy,data=d,family= quasibinomial)
  lnr<- predict(svy.fit,newdata=data,ref.zero = TRUE)
  lnr_ref<- lnr-lnr[(which(data$TyG==median(data$TyG)))[1]]
  return(lnr_ref)
}
set.seed(1234)
results <- boot(data=df, statistic=bs,
                R=1000, formula=grp~rcs(TyG,c(7.655391,8.664923,10.013563)))
print(results)

OR_ci<- function(Index){
  ORR<- boot.ci(results, type="perc",index = Index)
  out<- data.frame(
    low=as.numeric(ORR$perc)[4],
    high=as.numeric(ORR$perc)[5]
  )
  return(out)
}
library("purrr")
ci<- map_dfr(1:nrow(df),OR_ci)

df$OR_est<-exp(lnr_ref)
df$lowlimit<-exp(ci$low)
df$uplimit<-exp(ci$high)
# ggplot()+
#   geom_line(data=OR, aes(TyG,yhat),
#             linetype="solid",size=1,alpha = 0.7,color='red')+
#   geom_ribbon(data=OR,
#               aes(TyG,ymin = lower, ymax = upper),
#               alpha = 0.1, fill = "red")+
#   geom_hline(yintercept=0, linetype=2,size=1)+
#   geom_vline(xintercept=10, linetype=2,size=1)+
#   labs(x = "TyG level",y = "OR(95%CI)")+
#   geom_text(aes(x = 10, y = 1, label = paste0("P for nonlinear ", p_value)),
#             family = "Times New Roman", size = 5) +
#   theme_classic()
# theme(text = element_text(family='Times New Roman',size=14))+
#   scale_x_continuous(breaks = c(0, 5, 10, 15))

p1<-ggplot()+
  theme_classic()+
  geom_line(data=df, aes(TyG,OR_est),linetype="solid",size=1,alpha = 1)+
  geom_ribbon(data=df, aes(TyG,ymin = lowlimit, ymax = uplimit),alpha = 0.2,fill="red")+
  # scale_x_continuous(limits=c(5,20),breaks=seq(5,20,2.5))+
  # scale_x_continuous(breaks = c(5, 7.5,8.66, 10,12.5, 15))+
  # scale_y_continuous(limits=c(0,1.5),breaks=seq(0,1.5,0.25))+ 
  geom_hline(yintercept=1,linetype=2,size=0.25)+
  geom_vline(xintercept=8.66, linetype=2,size=1)+
  labs(x = "TyG level",y = "OR(95%CI)")
p1
svy.fit<-svyglm(grp~rcs(TyG,c(7.655391,8.664923,10.013563)),design=svy,data=df,family= quasibinomial)
summary(svy.fit)

library(aod)
wald.test(Sigma = vcov(svy.fit), b = coef(svy.fit), Terms = 2:3)#总体关联

wald.test(Sigma = vcov(svy.fit), b = coef(svy.fit), Terms = 3)#非线性
p1+geom_text(aes(x = 7, y = 2, label = paste0("P for nonlinear =0.099")),family = "Times New Roman", size = 5)
             
             
#6.baseline table
library(crosstable)
crosstable(df,by='grp',test = T,percent_digits = 2,num_digits =2,percent_pattern = "{n} ({p_col})")%>%as_flextable(compact=T)

#7.OR1
quartiles <- quantile(df$TyG, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
df$quantile2 <- cut(df$TyG, 
                 breaks = c(-Inf, quartiles[1], quartiles[2], quartiles[3], Inf),
                 labels = c('Q1', 'Q2', 'Q3', 'Q4'),
                 include.lowest = TRUE)

#model1
svy <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~FINAL_WEIGHT, data = df,nest=TRUE)
glm<-svyglm(grp~quantile2,family = quasibinomial,design = svy)
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
library(parallel)
req_var_index <- 29
# opt_var_indices <- 6:27
opt_var_indices <-c(6,7,10,23,27,14,13)
colnames(df)

# 获取所有可能的10个变量组合
combinations <- combn(opt_var_indices, 7, simplify = FALSE)
results_list <- list()
for(i in seq_along(combinations)){
  # for(i in (2:20)){
    selected_vars_indices <- c(combinations[[i]], req_var_index)
  form <- as.formula(paste('grp~', paste0(colnames(df)[selected_vars_indices], collapse = '+')))
  glm_model <- svyglm(form, family = quasibinomial, design = svy)
  glm_summary <- summary(glm_model)
  OR <- round(exp(coef(glm_model)), 3)
  SE <- glm_summary$coefficients[, 2]
  CI5 <- round(exp(coef(glm_model) - 1.96 * SE), 3)
  CI95 <- round(exp(coef(glm_model) + 1.96 * SE), 3)
  OR_CI <- paste0(OR, '(', CI5, '-', CI95, ')')
  P <- round(glm_summary$coefficients[, 4], 3)
  multi <- data.frame("variable" = rownames(glm_summary$coefficients),
                      "OR" = OR_CI,
                      "P" = P)[-1, ]
  multi <- multi[grepl('quantile2', multi$variable), ]
    results_list[[i]] <- multi
}
final_results <- do.call(rbind, results_list)
print(final_results)

selected_vars_indices <- c(combinations[[4]], req_var_index)
form2 <- as.formula(paste('grp~', paste0(colnames(df)[selected_vars_indices], collapse = '+')))

glm<-svyglm(form2,family = quasibinomial,design = svy)
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
lm<-svyglm(糖化血红蛋白~quantile2,family = gaussian(),design = svy)
sum<-summary(lm)
ci<-paste0(round(coef(lm),3),'(',round(confint(lm)[,1],3),'-',round(confint(lm)[,2],3),')')  
P<-round(sum$coefficients[,4],3)
multi<-data.frame("variable"=rownames(sum$coefficients),
                  "beta"=ci,
                  "P"=P)[-1,]
multi


form3 <- as.formula(paste('糖化血红蛋白~', paste0(colnames(df)[selected_vars_indices], collapse = '+')))
lm<-svyglm(form3,family = gaussian(),design = svy)
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
                      data=svy,family="binomial")

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
            xlim=c(0,2),        
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
svy <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~FINAL_WEIGHT, data = df,nest=TRUE)
glm<-svyglm(grp~quantile2,family = binomial(),design = svy)
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
glm<-svyglm(form2,family = binomial(),design = svy)
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
library(mediation)
Y2<-lm(糖化血红蛋白~TyG,data=df)
Y3<-glm(grp~糖化血红蛋白+TyG,data=df,family=binomial)
med<-mediate(Y2,Y3,treat='TyG',mediator = '糖化血红蛋白',boot =T,sims = 1000)
summary(med)
plot(med)


