setwd("C:/Users/valeb/OneDrive/Desktop/data mining/ELABORATO DM")
library(readr)
credit_risk_dataset <- read_csv("dati elaborato/credit_risk_dataset.csv")
save(credit_risk_dataset, file = "dati elaborato/credit_risk_dataset.Rdata")
c<-credit_risk_dataset
str(c)

table(c$loan_intent)
table(c$loan_grade)
table(c$cb_person_default_on_file)

#trovare livelli variabili qualitative#
c$person_home_ownership<-factor(c$person_home_ownership)
nlevels(c$person_home_ownership)

c$loan_intent<-factor(c$loan_intent)
nlevels(c$loan_intent)

c$loan_grade<-factor(c$loan_grade)
nlevels(c$loan_grade)

c$cb_person_default_on_file<-factor(c$cb_person_default_on_file)
nlevels(c$cb_person_default_on_file)

####analisi missing data####
sapply(c, function(x)(sum(is.na(x)))) #conta missing data
library(VIM)
missingness<-aggr(c,col=c('blue','red'), numbers=TRUE,sartVars=TRUE, labels=names(c),cex.axis=.7,gap=3)
library(mice)
imputed_data<-mice(c, m=5, method='pmm',maxit=50, seed=500)

imputed_data$imp$person_emp_length #visualizzo i risultati generati nei 5 dataset imputati (maxit), per i Na della colonna person_emp_length
imputed_data$imp$loan_int_rate #idem per loan_int_rate

c_compl<-complete(imputed_data,1) #visualizzo il "nuovo" dataset senza missing data, e scelgo di utilizzare il primo dataset imputato (avremmo potuto scegliere qualsiasi dataset tra i 5 imputati con mice

sapply(c_compl, function(x)(sum(is.na(x)))) #non ci sono più Na
missingness<-aggr(c_compl,col=c('blue','red'), numbers=TRUE,sartVars=TRUE, labels=names(c_compl),cex.axis=.7,gap=3)

#salva il dataset come CSV
write.csv(c_compl, file = "credit_risk_completo.csv", row.names = FALSE)
c_compl <- read.csv("credit_risk_completo.csv")

####collinearità####
fit=lm(loan_amnt ~ ., data=c_compl)
summary(fit)
drop1(fit, test="F")
#cb_person_default_on_file non significativa

par(mfrow=c(2,2)) 
plot(fit)
par(mfrow=c(1,1))

#2 tab una qual una quant

#QUANTITATIVE
fit$terms

cov=attr(terms(fit), "term.labels") 
cov

library(dplyr)
c_numeric <- c_compl[,cov]%>% dplyr::select_if(is.numeric)
colnames(c_numeric)

require(corrgram)
corrgram(c_numeric, lower.panel = panel.cor, cex=1, cex.labels = 1) #cor tra variabili num

y = as.numeric(c_compl$loan_amnt)
X<-c_numeric
X=as.matrix(X)


library(mctest)

fitcoll1=lm(y~X)
imcdiag(fitcoll1) #person_age e cb_person_cred_hist_length collineari

#rimuovo person_age che ha il vif maggiore (e tol minore)
X <- X[, -which(colnames(X) == "person_age")]
fitcoll2=lm(y~X)
imcdiag(fitcoll2) #non c'è più collinearità

c_new<-subset(c_compl,select=-person_age)

#QUALITATIVE
library(dplyr)
c_fac <- c_compl[,cov]%>% dplyr::select_if(is.factor)

#chi quardo e chi quadro norm
library(plyr)
combos <- combn(ncol(c_fac),2)
adply(combos, 2, function(x) {
  test <- chisq.test(c_fac[, x[1]], c_fac[, x[2]])
  tab  <- table(c_fac[, x[1]], c_fac[, x[2]])
  out <- data.frame("Row" = colnames(c_fac)[x[1]]
                    , "Column" = colnames(c_fac[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(c_fac[,x[1]], c_fac[,x[2]]))
                    , "u1" =length(unique(c_fac[,x[1]]))-1
                    , "u2" =length(unique(c_fac[,x[2]]))-1
                    , "nMinu1u2" =sum(table(c_fac[,x[1]], c_fac[,x[2]]))* min(length(unique(c_fac[,x[1]]))-1 , length(unique(c_fac[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(c_fac[,x[1]], c_fac[,x[2]]))* min(length(unique(c_fac[,x[1]]))-1 , length(unique(c_fac[,x[2]]))-1)) 
  )
  
  
  return(out)
  
}) 

#dai chi quadro normalizzato non risultano variabili qualitative collineari (no >0.8), non devo eliminare nulla

#fit finale senza person_age
fit2=lm(loan_amnt~person_income + person_home_ownership + 
          person_emp_length + loan_int_rate + loan_grade +loan_int_rate+ loan_percent_income + loan_grade +
          loan_status+cb_person_default_on_file+cb_person_cred_hist_length, data=c_compl)
summary(fit2)
drop1(fit2, test="F")

####interazioni####
library(MASS)
fit_interazioni=stepAIC(fit2, scope=~(person_income + person_home_ownership + 
          person_emp_length + loan_int_rate + loan_grade +loan_int_rate+ loan_percent_income + loan_grade +
          loan_status+cb_person_default_on_file+cb_person_cred_hist_length)^2,direction="both")
summary(fit_interazioni)
drop1(fit_interazioni,test="F")

fit_interazioni_ridotto <- update(fit_interazioni, 
                                  . ~ . - person_emp_length:loan_status 
                                  - loan_int_rate:cb_person_cred_hist_length)
drop1(fit_interazioni_ridotto, test = "F")
anova(fit2, fit_interazioni_ridotto, test = "LRT")
AIC(fit2)
AIC(fit_interazioni_ridotto)
#l'anova mostra un miglioramento nell'adattamento dei dati nel fit con iterazioni
#il miglioramento non giustifica tuttavia la complessità aggiunta, quindi proseguo con fit2


####linearità####
library(MASS)
boxcoxreg1<-boxcox(fit2)
title("Lambda")
lambda=boxcoxreg1$x[which.max(boxcoxreg1$y)]
lambda #lambda=0.30

new_loan_amnt<-(y^lambda)/lambda 
c_new<-cbind(c_new,new_loan_amnt) #aggiunto variabile al dataset

hist(c_new$loan_amnt)
hist(c_new$new_loan_amnt) #migliora notevolmente

c_newnew<-subset(c_new,select=-loan_amnt)

fitBox=lm(new_loan_amnt~person_income + person_home_ownership + 
            person_emp_length + loan_int_rate + loan_grade +loan_int_rate+ loan_percent_income + loan_grade +
            loan_status+cb_person_default_on_file+cb_person_cred_hist_length, data=c_new)
summary(fitBox)
drop1(fitBox, test="F")

#proseguiamo modificando le variabili qualitative e quantitative

#VARIABILI QUANTITATIVE
#seleziono variabili quantitative
library(ggplot2)
continuous_vars <- c("person_income", "person_emp_length", "loan_int_rate", "loan_percent_income")
for (var in continuous_vars) {
  p <- ggplot(c_newnew, aes(x = .data[[var]], y = new_loan_amnt)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", col = "blue", span = 0.5) +
    ggtitle(paste("Scatter plot with LOESS fit:", var)) +
    theme_minimal()
  
  print(p)
}

#dai grafici vediamo relazioni non lineari nelle variabili person_income, loan_percent_income
#applico le loess sulle variabili non lineari
library(gam)
modello_loess <- gam(new_loan_amnt ~ lo(person_income) + person_home_ownership + person_emp_length + 
                       loan_intent + loan_grade + loan_int_rate + loan_status + 
                       lo(loan_percent_income)+cb_person_default_on_file+cb_person_cred_hist_length, data = c_newnew)
plot(modello_loess)

#i grafici suggeriscono una trasformazione logaritmica per person_income e polinomiale per loan_percent_income

#PERSON_INCOME#
c_newnew$log_income <- log(c_newnew$person_income)

#modelli
mod_inc1 <- lm(new_loan_amnt ~ person_income, data = c_newnew)
mod_inc2 <- lm(new_loan_amnt ~ log_income, data = c_newnew)

#confronto risultati
results_income <- data.frame(
  Transformation = c("None", "Log"),
  R_squared = c(summary(mod_inc1)$r.squared, 
                summary(mod_inc2)$r.squared),
  AIC = c(AIC(mod_inc1), AIC(mod_inc2))
)
print(results_income) #log ha r-quadro più alto e AIC più basso, logaritmico migliore!

#plot per confrontare le trasformazioni
par(mfrow=c(1,2))
plot(c_newnew$person_income, c_newnew$new_loan_amnt, 
     main="Original", xlab="Income", ylab="new_loan_amnt")
plot(c_newnew$log_income, c_newnew$new_loan_amnt, 
     main="Log transform", xlab="Log Income", ylab="new_loan_amnt")


#LOAN_STATUS, rendo factor#
c_newnew$loan_status <- as.factor(c_newnew$loan_status)
nlevels(c_newnew$loan_status)
#variabile dummy, non va trasformata


#LOAN_PERCENT_INCOME#
c_newnew$poly_loan_percent_income<-poly(c_newnew$loan_percent_income, 2, raw = TRUE)
mod_percent1 <- lm(new_loan_amnt ~ loan_percent_income, data = c_newnew)
mod_percent2 <- lm(new_loan_amnt ~ poly_loan_percent_income, data = c_newnew)

# Confronto modelli
results_percent <- data.frame(
  Transformation = c("None", "Poly"),
  R_squared = c(summary(mod_percent1)$r.squared, 
                summary(mod_percent2)$r.squared),
  AIC = c(AIC(mod_percent1), AIC(mod_percent2))
)

print(results_percent)

#utilizzo trasformazione polinomiale
library(gam)
fitin=lm(new_loan_amnt ~ person_income + person_home_ownership + person_emp_length + 
           loan_intent + loan_grade + loan_int_rate + loan_status + 
           loan_percent_income+cb_person_default_on_file+cb_person_cred_hist_length, data = c_newnew)
finfin=gam(new_loan_amnt ~ lo(person_income) + person_home_ownership + person_emp_length + 
            loan_intent + loan_grade + loan_int_rate + loan_status + 
            lo(loan_percent_income)+cb_person_default_on_file+cb_person_cred_hist_length, data = c_newnew)
library(car)
AIC(fitin)
AIC(finfin)

#QUALITATIVE#
par(mfrow=c(2,2)) #Visualizzazione distribuzione delle variabili qualitative

#person_home_ownership 
barplot(table(c_newnew$person_home_ownership),
        main="Distribution of Home Ownership",
        xlab="Home Ownership Status",
        ylab="Frequency")
#Categorie sbilanciate

#loan_intent
barplot(table(c_newnew$loan_intent),
        main="Distribution of Loan Intent",
        xlab="Loan Intent",
        ylab="Frequency")
#distribuzione abbastanza uniforme tra le categorie

#loan_grade
barplot(table(c_newnew$loan_grade),
        main="Distribution of Loan Grade",
        xlab="Loan Grade",
        ylab="Frequency")
#distribuzione decrescente

#default_history
barplot(table(c_newnew$cb_person_default_on_file),
        main="Distribution of Default History",
        xlab="Default History",
        ylab="Frequency")

# Tabelle con frequenze
table(c_newnew$person_home_ownership)
table(c_newnew$loan_intent)
table(c_newnew$loan_grade)
table(c_newnew$cb_person_default_on_file)

#OPTIMAL GROUPING#
#PERSON_HOME_OWNERSHIP#
library(factorMerger)
reduce_levels <- mergeFactors(response = c_newnew$new_loan_amnt, factor = c_newnew$person_home_ownership)
plot(reduce_levels , panel = "GIC",title = "", panelGrid = FALSE )

opt_grup=cutTree(reduce_levels)
length(opt_grup)
table(opt_grup)

c_newnew$optimal_ownership=as.factor(opt_grup)
head(c_newnew)

plot(c_newnew$optimal_ownership, c_newnew$new_loan_amnt)
table(opt_grup, c_newnew$optimal_ownership)

#LOAN_INTENT#
reduce_levels <- mergeFactors(response = c_newnew$new_loan_amnt, factor = c_newnew$loan_intent)
plot(reduce_levels , panel = "GIC",title = "", panelGrid = FALSE )

opt_grup=cutTree(reduce_levels)
length(opt_grup)
table(opt_grup)

c_newnew$optimal_loan_intent=as.factor(opt_grup)

plot(c_newnew$optimal_loan_intent, c_newnew$new_loan_amnt)
table(opt_grup, c_newnew$optimal_loan_intent)

#LOAN_GRADE#
#provato optimal grouping, conviene lasciare i livelli separati tra di loro in quanto ogni livello ha significatività alta ** o ***


par(mfrow=c(2,2)) #Visualizzazione distribuzione delle variabili qualitative

#person_home_ownership 
barplot(table(c_newnew$optimal_ownership),
        main="Distribution of Home Ownership",
        xlab="Home Ownership Status",
        ylab="Frequency")
#Categorie sbilanciate

#loan_intent
barplot(table(c_newnew$optimal_loan_intent),
        main="Distribution of Loan Intent",
        xlab="Loan Intent",
        ylab="Frequency")
#distribuzione abbastanza uniforme tra le categorie

#loan_grade
barplot(table(c_newnew$loan_grade),
        main="Distribution of Loan Grade",
        xlab="Loan Grade",
        ylab="Frequency")
#distribuzione decrescente

#default_history
barplot(table(c_newnew$cb_person_default_on_file),
        main="Distribution of Default History",
        xlab="Default History",
        ylab="Frequency")

#CREAZIONE E MIGLIORAMENTO MODELLO#
#Creo modello completo con tutte le variabili (quantitative già trasformate e qualitative ricodificate)
#tentativo di modello con tutte le varibili

fit3 <- lm(new_loan_amnt ~ person_income + I(log(person_income)) + optimal_ownership + 
                         person_emp_length + loan_int_rate + loan_percent_income + I(loan_percent_income^2) + optimal_loan_intent + loan_grade +
                         loan_status+cb_person_default_on_file+cb_person_cred_hist_length, data=c_newnew)
summary(fit3)
drop1(fit3, test='F')
AIC(fit3)

#reset test
install.packages("lmtest")
library(lmtest)
reset_test<-resettest(fit3, power = 2, type = "fitted",  data = c_newnew)
print(reset_test)
#p-value < 2.2e-16

#scelta covariate
library(MASS)
stepwise_aic<-stepAIC(lm(new_loan_amnt~., data=c_newnew), direction="both")
stepwise_bic<-step(lm(new_loan_amnt~., data=c_newnew), direction="both", k=log(nrow(c_newnew)))

fit4=lm(new_loan_amnt ~ person_income + loan_grade + loan_int_rate + 
  loan_status + log_income + poly_loan_percent_income + optimal_loan_intent + 
  optimal_ownership, data=c_newnew)

AIC(fit4)
summary(fit4)
#la procedura bic rimuove le variabili non modificate, person_emp_length, e le due cb

c_newnew$person_emp_length<-NULL
c_newnew$cb_person_default_on_file<-NULL
c_newnew$cb_person_cred_hist_length<-NULL
fit4=lm(new_loan_amnt ~ person_income + loan_grade + loan_int_rate + 
          loan_status + log_income + poly_loan_percent_income + optimal_loan_intent + 
          optimal_ownership, data=c_newnew)
summary(fit4)
drop1(fit4, test="F") #variabili tutte altamente significative

anova(fitBox, fit4, test="LRT")

#scelgo fit4 -> obbiettivo modello robusto


####OUTLIER####
par(mfrow=c(2,2)) 
plot(fit4)
par(mfrow=c(1,1))

library(car)
influencePlot(fit4,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

cooksd <- cooks.distance(fit4)
cooksda=data.frame(cooksd)

n_used=length(fit4$residuals)
n_used # be careful!!! 

cutoff <- 4/(n_used-length(fit4$coefficients)-2)
cutoff
Noinflu=data.frame(c_newnew[cooksd < cutoff, ])  # influential row numbers

fitnooutlier = lm(new_loan_amnt ~ person_income + loan_grade + loan_int_rate + 
                    loan_status + log_income + poly_loan_percent_income + optimal_loan_intent + 
                    optimal_ownership , data=Noinflu)
summary(fitnooutlier)

par(mfrow=c(2,2)) 
plot(fitnooutlier)
par(mfrow=c(1,1))

library(car)
AIC(fit4)
AIC(fitnooutlier)

#> AIC(fit4) 146213.7
#> #AIC(fitnooutlier) 112330.1

####eteroschedasticità####
library(lmtest)
bptest(fitnooutlier)
#ancora alta hetero perchè p-value molto piccolo

#trasformo y in log e vedo se diminuisce hetero
Noinflu$new_loan_amnt_log = log(Noinflu$new_loan_amnt)
fit5<-lm(new_loan_amnt_log ~ person_income + loan_grade + loan_int_rate + 
           loan_status + log_income + poly_loan_percent_income + optimal_loan_intent + 
           optimal_ownership,data = Noinflu)
bptest(fit5) #log inutile

#non cambia nulla

#proviamo con White
library(car)
ncvTest(fitnooutlier)

#ipotesi white
library(lmtest)
library(sandwich)
summary(fitnooutlier)
vcov_white <- vcovHC(fitnooutlier, type = "HC0")
coeftest(fitnooutlier, vcov = vcov_white)

#wls perchè hetero presente
weights <- 1 / fitted(fitnooutlier)^2  # Pesatura inversa alla varianza stimata
fit_wls <- lm(new_loan_amnt ~ person_income + loan_grade + loan_int_rate + 
                loan_status + log_income + poly_loan_percent_income + optimal_loan_intent + 
                optimal_ownership, 
              data = Noinflu, weights = weights)

plot(fitted(fit_wls), resid(fit_wls))
abline(h = 0, col = "red")

bptest(fit_wls)

#hetero sistemata p-value = 0.9893

par(mfrow=c(2,2))
plot(fit_wls)
par(mfrow=c(1,1))


####modello finale####
fitfinale=lm(new_loan_amnt ~ person_income + loan_grade + loan_int_rate + 
               loan_status + log_income + poly_loan_percent_income + optimal_loan_intent + 
               optimal_ownership, 
             data = Noinflu)
summary(fitfinale) #Multiple R-squared:  0.9757,	Adjusted R-squared:  0.9757
drop1(fitfinale, test="F")

par(mfrow=c(2,2)) 
plot(fitfinale)
par(mfrow=c(1,1))


####10. Bootstrap####
library(car)

BOOT.MOD=Boot(fitfinale, R=1999)# R=replications
summary(BOOT.MOD, high.moments=TRUE)

ci_perc<-Confint(BOOT.MOD, level=c(.95), type="perc")
ci_norm<-Confint(BOOT.MOD, level=c(.95), type="norm")

hist(BOOT.MOD, legend="separate")

print(ci_perc)
print(ci_norm)

library(moments)

# Calcolo skewness e kurtosis per ogni coefficiente bootstrap
skewness_vals <- apply(BOOT.MOD$t, 2, skewness)
kurtosis_vals <- apply(BOOT.MOD$t, 2, kurtosis)

print(skewness_vals)
print(kurtosis_vals)

# AIC per i modelli
aic_fitBox <- AIC(fitBox)
aic_fitfinale <- AIC(fitfinale)

aic_fitBox
aic_fitfinale

anova(fitBox, fitfinale, test = "Chisq")

#ulteriore check
library(lmtest)
bptest(fitBox)
bptest(fitfinale)

#####11. Modello logistico ####
#ricarica dataset senza Na
c_compl<-read.csv("credit_risk_completo.csv")

logistico1<-glm(loan_status~., data=c_compl, family="binomial")
summary(logistico1)
drop1(logistico1, test="LRT")

library(MASS)
logistico2<-stepAIC(logistico1,direction="both")

logistico3<-glm(loan_status ~ person_age + person_income + person_home_ownership + 
                  person_emp_length + loan_intent + loan_grade + loan_amnt + 
                  loan_int_rate + loan_percent_income,data=c_compl,family="binomial")
summary(logistico3)
drop1(logistico3, test="LRT")

par(mfrow=c(2,2)) 
plot(logistico3)
par(mfrow=c(1,1)) 

anova(logistico1,logistico3,test="LRT")

#oddsratio
exp(coef(logistico3))

library(forestmodel)
print(forest_model(logistico3),text_size = 5)

# Visualizzazione coefficienti
library(coefplot)
coefplot(logistico3, intercept=FALSE)

# Calcolo probabilità predette
c_compl$predicted_p <- predict(logistico2, c_compl, type="response") 
tail(c_compl)

# Target predetto con soglia 0.5
c_compl$predicted_y <- ifelse(c_compl$predicted_p > 0.5, 1, 0)

# Matrice di confusione con proporzioni
conf_matrix <- table(observed=c_compl$loan_status, predicted=c_compl$predicted_y)/nrow(c_compl)
print(conf_matrix)
# Calcolo accuratezza
accuracy <- sum(diag(conf_matrix))
print(paste("Accuracy:", round(accuracy, 3)))
