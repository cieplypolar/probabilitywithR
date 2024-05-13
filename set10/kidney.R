# survival analysis

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)
library("Rcpp")
library(dplyr)

mydata = read.csv("./kidney.csv")

# sad factorizing
mydata$id=as.factor(mydata$id)
mydata$sex=as.factor(mydata$sex)
mydata$status=as.factor(mydata$status)
mydata$frail=as.factor(mydata$frail)

mydata=mydata %>%
  mutate(status = ifelse(status == 0,1,0)) # necessary for survfit

# let's see summary
summary(mydata)

# estymator Kaplana-Mayera

KM <- survfit(Surv(time, status) ~ 1, data=mydata)
summary(KM)
summary(KM, times = c(1,10*(1:15))) # I suppose bins for times

plot(KM, xlab="Days", main = 'Kaplan Meyer') 
autoplot(KM)

KM_02 <- survfit(Surv(time, status) ~ disease, data=mydata) # depends on treatment and type of event
summary(KM_02, times = c(1,10*(1:15)))
print(KM_02)

plot(KM_02, xlab="Days", main = 'Kaplan Meyer') 
autoplot(KM_02)

ggsurvplot(KM_02,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF", "#33ffa5", "#ff3361"))


surv_diff <- survdiff(Surv(time, status) ~ disease, data = mydata) #log rank test
surv_diff

autoplot(survfit(Surv(time, status) ~ disease, data=mydata))
survdiff(Surv(time, status) ~ disease, data = mydata)

# zamiana zmiennej ilosciowej age na jakosciowa

mydata_02 <- mutate(mydata, AGE = ifelse((age < 60), "<60", ">=60"),
                    AGE = factor(AGE))

KM_03 <- survfit(Surv(time, status) ~ AGE, data=mydata_02)

print(KM_03)

ggsurvplot(KM_03,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata")


# model Coxa

cox <- coxph(Surv(time, status) ~ disease + age + sex + frailty(id), data=mydata)
summary(cox)
cox


ggforest(cox, data=mydata)
zp=cox.zph(cox)
zp
plot(zp)

step(cox)


