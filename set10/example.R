# survival analysis

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)
library("Rcpp")


data(veteran)
head(veteran)

summary(veteran)
veteran$trt=as.factor(veteran$trt)
veteran$celltype=as.factor(veteran$celltype)
veteran$prior=as.factor(veteran$prior)
summary(veteran)

km <- with(veteran, Surv(time, status))
head(km,20)

# estymator Kaplana-Mayera

KM <- survfit(Surv(time, status) ~ 1, data=veteran)
summary(KM)
summary(KM, times = c(1,30,60,90*(1:10)))

plot(KM, xlab="Days", main = 'Kaplan Meyer') 
autoplot(KM)

KM_02 <- survfit(Surv(time, status) ~ trt, data=veteran)
summary(KM_02, times = c(1,30,60,90*(1:10)))
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
           palette = c("#E7B800", "#2E9FDF"))


surv_diff <- survdiff(Surv(time, status) ~ trt, data = veteran) #log rank test
surv_diff



autoplot(survfit(Surv(time, status) ~ trt+prior, data=veteran))
survdiff(Surv(time, status) ~ trt+prior, data = veteran)

# zamiana zmiennej ilosciowej age na jakosciowa

veteran_02 <- mutate(veteran, AGE = ifelse((age < 60), "<60", ">=60"),
                     AGE = factor(AGE))

KM_03 <- survfit(Surv(time, status) ~ AGE, data=veteran_02)

print(KM_03)

ggsurvplot(KM_03,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata")


#model Coxa

cox <- coxph(Surv(time, status) ~ trt + celltype + karno+ diagtime + age + prior , data = veteran)
summary(cox)
cox

ggforest(cox, data = veteran)
zp=cox.zph(cox)
zp
plot(zp[1])
plot(zp[3])

step(cox)

cox_02 <- coxph(Surv(time, status) ~ celltype + karno , data = veteran)
summary(cox_02)

