rm(list= ls())

require(ggplot2)
require(gghalves)
require(ggthemes)
require(scales)
require(moments)
require(colorspace)
require(car)


#TODO: CAPIRE COME GESTIRE GLI OUTLIER. BISOGNA FARE PULIZIA DEI DATI E DOPO FARE
#LE CONSIDERAZIONI SUI TEST STATISTICI E IL RESTO.
#QUINDI L'IDEA È: PRIMA FACCIO L'ANALISI DESCRITTIVA SUL DATA SET COME È FORNITO
#E POI RIFACCIO LA DESCRIZIONE SUL DATASET RIPULITO.
#
#DOPO DI CHE FAI I PUNTI SEGUENTI(DAL PUNTO 4 IN POI) CON IL DATASET RIPULITO,
#ALTRIMENTI I RISULTATI SONO FALSATI.

#Avendo già fatto parte del codice per questi punti, alla fine dei conti il lavoro
#non è sprecato: cambiano i risultati e le considerazioni da fare (che vanno più
#nel report che qui), ma la sintassi delle funzioni è la stessa.


setwd("C:/Users/aless/Desktop/MASTER DATA SCIENCE/statistica-inferenziale-per-datascience/progetto finale")

#1
newborn_data = read.csv("neonati.csv",sep = ",", stringsAsFactors = T)
attach(newborn_data)

#2
#Dataset description: see requirements
#Study goal: see requirements

#Variables types:
#Anni.madre: quantitative - discrete (int values)
#N.gravidanze: quantitative - discrete (int values)
#Fumatrici: qualitative - nominal scale (2 levels)
#Gestazione: quantitative - discrete (int values)
#Peso: quantitative - discrete (int values)
#Lunghezza: quantitative - discrete (int values)
#Cranio: quantitative - discrete (int values)
#Tipo.parto: qualitative - nominal scale
#Ospedale: qualitative - nominal scale
#Sesso: qualitative - nominal scale


#3 descriptive analysis 

N = nrow(newborn_data) #sample size

#To describe data with a single chart i can use gghalves:
#For each quantitative variable one can make a violin plot + boxplot;
#by this way one can have an idea of data distribution and possible outliers.
#For qualitative variables one can make a bar plot of relative frequency distribution
#(that is an histogram of values).

##QUALITATIVE VARS (Fumatrici, Tipo.parto, Ospedale, Sesso)

#create a custom function to reduce code
qualitative.summary = function(x) {
    var_name = deparse(substitute(x))
    
    relative = as.data.frame(table(x)/N)
    
    #bar chart
    ggplot(data=relative, aes(x=relative[,1],y=Freq))+
        # geom_col(col="maroon",fill="lightskyblue", size=1)+
        geom_col(col="#DA95CC",fill="#87AEDF", size=0.8)+
        labs(title = paste(var_name,"relative frequency distribution"),
             x = var_name, y = "Frequency")+
        scale_y_continuous(labels = label_percent())+
        geom_text(aes(label=percent(Freq)),vjust=-0.5)+
        theme_hc()
    # ggsave(paste(var_name,"relative frequency distribution.png"))
}

##QUANTITATIVE VARS (Anni.madre, N.gravidanze, Gestazione, Peso, Lunghezza, Cranio)

#use a custom function to compute position indexes (quantitative vars)
position.summary = function(x) {
    quartiles = quantile(x)
    x_min = quartiles[1]
    x_Q1 = quartiles[2]
    x_median = quartiles[3]
    x_Q3 = quartiles[4]
    x_max = quartiles[5]
    
    d = data.frame(min = x_min, Q1 = x_Q1, median = x_median, Q3 = x_Q3, max = x_max)
    return(d)
}

#use a custom function to compute variability indexes (quantitative vars)
variability.summary = function(x) {
    n = length(x)
    x_mu = mean(x)
    x_IQR = IQR(x)
    x_var = var(x)
    x_sigma = sqrt(x_var)
    x_CV = x_sigma/x_mu * 100 #coefficient of variability
    x_IQR=IQR(x)
    
    d = data.frame(n = n, mu=x_mu,variance = x_var,sigma = x_sigma, CV = x_CV, IQR = x_IQR)
    return(d)
}

#use a custom function to compute shape indexes (quantitative vars)
shape.summary = function(x){
    x_skewness = skewness(x)
    x_kurtosis = kurtosis(x)-3
    
    d = data.frame(skewness = x_skewness, kurtosis = x_kurtosis)
    return(d)
}

#custom function to save all statistical indexes for a quantitative variable
quantitative.summary = function(variable) {
    var_name = deparse(substitute(variable))
    
    df_position = position.summary(variable)
    df_variability = variability.summary(variable)
    df_shape = shape.summary(variable)
    total_summary = cbind(data.frame(name = var_name),
                          df_position,df_variability,df_shape)
    write.csv(total_summary,paste0(var_name, " stats.csv"),row.names = F)
    
    #charts
    chart = ggplot(data=newborn_data,aes(y=variable))+
        geom_half_violin(side="r",fill="#87AEDF")+
        geom_half_boxplot(side="l",fill="#DA95CC",center=T,width=0.5)+
        labs(title = paste(var_name,"distribution summary"),
             y = var_name)+      
        coord_flip()+
        theme_hc()
    
    print(chart)
    
    # ggsave(paste(var_name,"distribution summary.png"), chart)
    
    
    return(total_summary) #questo return effettivamente serve?
    
}

qualitative.summary(Fumatrici)
qualitative.summary(Tipo.parto)
qualitative.summary(Ospedale)
qualitative.summary(Sesso)

mother.age.summary = quantitative.summary(Anni.madre)
### WAIT
### There are 2 abnormal values in Anni.madre: there is one record with age = 0
### and one record with age = 1. These two values are veeery weird -> ask what teh hell happened.
### The ohter values starts from 13 and are up to 46. 13 and 14 years is more realistic but for clarity, 
### ask to coach (dai mbare, chi spacchiu è na gravidanza a 13 anni?!).
n.pregnancy.summary = quantitative.summary(N.gravidanze)
### WAIT pt. 2
### The maximum value here is 12 (that is for a 38 yrs woman).
### Well, it's actually possible, but... What the hell?! 
gestational.age.summary = quantitative.summary(Gestazione)
weight.summary = quantitative.summary(Peso)
length.summary = quantitative.summary(Lunghezza)
head.summary = quantitative.summary(Cranio)

## All other quantitative vars show no weird things.

################# DATA CLEANING ####################

#First step: Missing values.
#First missing values must be checked and handled (if there is any).
#To compute the number of missing values for each variable:
colSums(is.na(newborn_data))

#There are no missing values in this data set, so we can move on second step. 

#Second step: Outliers.

#In previous charts one can see that there are a lot of outliers in qualitative
#variables. How many of them are real anomalous? Are they really anomalous or 
#are they however representative of some real situation? If an anomalous value is
#found, how to handle it? 
#To answer to all these questions one must refer to so called "domain knowledge", 
#that is, to check some reference value that can be found in literature for each variable.
#
#In this case study the IQR method is used to discover outliers: 
#If a record, for a variable, has a value outside the range [Q1-1.5*IQR, Q3+1.5*IQR]
#it's considered an outlier. We recollect that IQR = Q3 - Q1.
# These records are displayed in the previous box plots as
#dots outside whiskers.

##Anni.madre
#First anomaly is found in mother's age variable.
#From statistical summary the relative minimum is 14.5 (i.e. Q1 - 1,5*IQR).
#In data set we have two values equal to 14, one equal to 13 and two other
#record with 0 and 1.
#The former values (13 and 14) can't be considered anomalies since it's biologically
#possible to have a pregnancy at that age. 
#As for the latter (0 and 1) they're a clear example of anomalous values, since 
#it's impossible for human to have a pregnancy at that age.   

#As for the other outliers, here we have a relative maximum of 42.5 (i.e. Q3 + 1.5*IQR).
#In this case menopause process must be taken in account:
#From literature (
#https://www.menopause.org/for-women/menopauseflashes/menopause-symptoms-and-treatments/menopause-101-a-primer-for-the-perimenopausal
#https://www.nhs.uk/conditions/menopause/
#https://www.nia.nih.gov/health/menopause/what-menopause
#) the average age when menopause starts is 51. Even if some women experience menopause
#earlier (even between 40 and 58 years), in this case we haven't enough information
#to say that women with age > 42.5 (relative maximum) are in menopause (and so they are outliers),
#so these records are considered valid.

##Gestazione
#Our data is perfectly in line with literature (
#https://www.health.ny.gov/community/pregnancy/why_is_40_weeks_so_important.htm
#https://www.betterhealth.vic.gov.au/health/healthyliving/pregnancy-week-by-week
#):
#Average human gestation period is 40 weeks, but babies are considered 
#'full term' if they are born anywhere between 37-42 weeks.
#In our data we have mean and median values close to these reference 
#(mean is 38.98 weeks, median is 39 weeks), and the quartiles are aligned too: 
#Q1 is 38 weeks, Q3 is 40 weeks, and we have also a little Coefficient of variation CV
#(around 4%) that means that data are quite centered around mean value.
#From violin plot note as 40 weeks is also the mode, so perfectly in line with standards.
#As for outliers here there aren't values above relative maximum, while there are
#several values under relative minimum (here 35 weeks).
N_outliers_gestazione = length(Gestazione[Gestazione < 35])
N_outliers_gestazione #67 records (2.68% of data)

#These are cases of preterm births. According to literature:
#Extremely preterm infants are born 23 through 28 weeks.
#Moderately preterm infants are born between 29 and 33 weeks.
#Late preterm infants are born between 34 and 37 weeks.

#In our data the minimum is 25 weeks, so with available data all records are valid,
#and babies born between 25 and 35 weeks are considered preterm, but this report
#doesn't account for the difference between preterm and full term births.

##N.gravidanze
#Here the only real limit is that this variable must be >=0, and that's our case.
#Even if there isn't a real maximum value, one could do some calculations...
#A woman has a fertile life of about 38 years, 
#i.e. 51 (average menopause start) - 13 (average age for a girl's first period).
#Doing a simply but large (default) approximation one can say that a woman can have
#one pregnancy per year; so, in total a woman can have up to 38 pregnancies:
#even if that value is very huge (and infrequent) it's however far from maximum in
#our data (that is 12 pregnancies before the one that is recorded here), so even 
#there are some values outside whiskers, all data are possible and so valid regarding this variable.

##Peso
#C'È DA DIVENTARE SCEMI QUI. OUTLIER Sì O NO? 
#A SENTIMENTO MI SENTO DI DIRE DI Sì, MA COME OGNI COSA "DIPENDE". 
#E CHE DIAVOLO.
#
#
#Literature (https://www.wikiwand.com/en/Low_birth_weight) says that:
#Birth weight may be classified as:
#
# High birth weight (macrosomia): greater than 4,200 g (9 lb 4 oz)
# Normal weight (term delivery): 2,500–4,200 g (5 lb 8 oz – 9 lb 4 oz)
# Low birth weight: less than 2,500 g (5 lb 8 oz)
# Very low birth weight (VLBW): less than 1,500 g (3 lb 5 oz)
# Extremely low birth weight: less than 1,000 g (2 lb 3 oz)
#
#In our sample there are both full term and preterm births, so there isn't an universal
#reference value, so, in order to determine if a weight value is valid or not,
# the gestational age (that here is the variable "Gestazione") must be 
#taken in account.
#
#In literature (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5261648/#pmed.1002220.s009
#) there is a table where are listed percentiles of estimated fetal weight vs 
#gestational age in weeks.
#According to S1 table of this article we have that weight depends on gestational age,
#i.e. weeks of gestation.
#Without going too much in detail we can visualize the curves of 5th, 50th, and 95th
#percentiles overlaid on a scatterplot of our data, just to get an idea if out sample is
#too far from this reference.

peso_min_rel = quantile(Peso,0.25)-1.5*IQR(Peso)
peso_max_rel = quantile(Peso,0.75)+1.5*IQR(Peso)
N_outliers_weight = length(Peso[Peso < peso_min_rel]) + length(Peso[Peso > peso_max_rel])
N_outliers_weight #69 records (2.76% of data)

#TODO: 
#Fai un grafico con ggplot con più livelli, dove il primo livello è dato dalle 
#curve di riferimento (quindi prese dalla tabella dell'articolo) e il secondo è
#uno scatter plot della tua nuvola di punti. 

#Basta passare data separatamente ai vari strati, e non al ggplot() complessivo.

weights.reference = read.csv("./files/weights vs gestational age.csv")
diameters.reference = read.csv("./files/head diameter vs gestational age.csv")

ggplot()+
    geom_line(data = weights.reference, aes(x=Gestational.Ages, y = X50, color="50th percentile"), linetype = "dotdash")+ #twodash
    geom_line(data = weights.reference, aes(x=Gestational.Ages, y = X2.5, color="2.5th percentile"), linetype = "longdash")+ #twodash
    geom_line(data = weights.reference, aes(x=Gestational.Ages, y = X97.5, color="97.5th percentile"), linetype = "longdash")+ #twodash
    geom_point(data = newborn_data, aes(x=Gestazione, y = Peso, color = "Sample data"), alpha = 0.2)+
    scale_color_manual(values = c("50th percentile" = "green", "Sample data" = "blue", "2.5th percentile" = "red", "97.5th percentile"="black"))+
    scale_x_continuous(breaks = seq(10,45,5))+
    theme_hc()+
    labs(title="Weights - sample data and reference", x = "Gestational age (weeks)", y = "Weight (g)", color = "")

#Forzare i dati fuori dalla striscia a stare dentro? Non so Rick.
#CHIEDI!

#vignette("ggplot2-specs")

##Cranio
#Fai la stessa roba che hai fatto per la parte di Peso. E poi dal garfico che esce
#fuori si vede
#
#No, non va bene, perchè sono due misure diverse... UFFAAAAAAAAA
#MANNAGGINA :((((


ggplot()+
    geom_line(data = diameters.reference, aes(x=Gestational.Ages, y = X50, color="50th percentile"), linetype = "dotdash")+ #twodash
    geom_line(data = diameters.reference, aes(x=Gestational.Ages, y = X2.5, color="2.5th percentile"), linetype = "longdash")+ #twodash
    geom_line(data = diameters.reference, aes(x=Gestational.Ages, y = X97.5, color="97.5th percentile"), linetype = "longdash")+ #twodash
    geom_point(data = newborn_data, aes(x=Gestazione, y = Cranio, color = "Sample data"), alpha = 0.2)+
    scale_color_manual(values = c("50th percentile" = "green", "Sample data" = "blue", "2.5th percentile" = "red", "97.5th percentile"="black")) +
    theme_hc()+
    labs(title="Head diameter", x = "Gestational age (weeks)", y = "Head diameter")


##Lenght 
#E QUI MI ATTACCO AL CAZZO perchè nell'articolo di prima nella tabella non c'è
#questa misura (proprio per niente)... Mi sa che mi sto affossando...



#NO DATA CLEANING REQUIRED (except for anni.madre weird values, but htey're only 2 records)
#See Dejan comment on discord.

#These two records are anomalous only as for the mother's age value, but all other
#variables have realistic values; for this reason the record is kept, but mother's age
#is set to median value of this variable (outlier value is replaced with median and not
#with mean because median is more robust against outliers, although in this case mean 
#and median don't differ too much).

newborn_data$Anni.madre[Anni.madre == 0] = mother.age.summary$median
newborn_data$Anni.madre[Anni.madre == 1] = mother.age.summary$median

attach(newborn_data)





#4 - mean value for weight and length are statistically equal to the
# corresponding values in population? 

# Here I have to do a t-test, where I compare sample mean with a fixed value
# (that is the value taken from literature for these variables).
# OR
# I could do also a Z-test if the population variance is known... 
# Let's see what literature says!

# Reference link: https://www.who.int/tools/child-growth-standards/standards
# WHO standards for these two variables state a normal distribution
# regardless nationality and cultural background. 
# From FAQ: "The standards describe normal child growth from birth
#  to 5 years under optimal environmental conditions and can be applied 
#  to all children everywhere, regardless of ethnicity, 
#  socioeconomic status and type of feeding." 
#  
#  So we can assume that weight and length have a normal distribution in
#  population, with a known mean.
#  The mean value can be taken from tables at column M (mean) or at the
#  value with Z-score = 0 (that corresponds to mean according to z-distribution).

# However, like in most medical studies, the values are separated for males
# and females, so one can't do a single t test with all sample data, but
# first one must divide data in two groups (male and females) and then 
# compute a t test for weight and one for length for each group.

Lunghezza_M = Lunghezza[Sesso == "M"]
Lunghezza_F = Lunghezza[Sesso == "F"]

Peso_M = Peso[Sesso == "M"]
Peso_F = Peso[Sesso == "F"]

#From WHO tables
weight_mean_M = 3.3464*1000 #g
weight_mean_F = 3.2322*1000 #g
length_mean_M = 49.8842*10 #mm
length_mean_F = 49.1477*10 #mm

#Before computing t-test we must check the assumptions: 
# - Independent values: data are from different people, so they don't 
# affect each other.
# - Random sample data: data is collected randomly from newborn 
# babies population (we trust whoi collected data)
# - Normal distribution: we can check it with a shapiro-wilk test. 
# If data are not normally distributed, a non-parametric test must be
# used.

#Shapiro test for weight:
shapiro.test(Peso_M)
shapiro.test(Peso_F)

#Shapiro test for length:
shapiro.test(Lunghezza_M)
shapiro.test(Lunghezza_F)

#For each variable we get p < 5% (here p is around 1e-16), so there is
#a strong evidence that data are not normally distributed -> t-test cannot 
#be used. To test mean values, a Wilcox

#two sided (default) - alpha = 5% (default)
#weight
wilcox.test(Peso_M, mu = weight_mean_M)
wilcox.test(Peso_F, mu = weight_mean_F)
#length
wilcox.test(Lunghezza_M, mu = length_mean_M)
wilcox.test(Lunghezza_F, mu = length_mean_F)


##5 differences between Male and Females for other vars.
#Chi squared test? Or always t-test? 
#Chi squared is more suitable for qualitative variables (compare frequency 
#distribution of a categorical variable between two or more groups).
#t-test (or Wilcoxon test) is for numeric (quantitative) variables.

#Considering the medical contex, the variables where i can have some differences
#between male and females are the medical ones, that is:
# - Head circumference
# - Weight
# - Length
# - Pregnancy weeks.
#For other variables there is no sense to compute differences between male and
#females babies (for example I'm not expecting some relationship between sex and
#the hospital where the baby was born).
#
#Due to these considerations the appropriate test here is the t-test. 
#In this case the two-samples t-test is used (independent groups).

#Separation in two groups (already done for weight and length)

Gestazione_M = Gestazione[Sesso == "M"]
Gestazione_F = Gestazione[Sesso == "F"]
Cranio_M = Cranio[Sesso == "M"]
Cranio_F = Cranio[Sesso == "F"]

#First of all let's test if the variance is similar between groups:
leveneTest(Gestazione~Sesso, data = newborn_data) #Test must be not significant
#p-value = 0.068 -> test not significant (alpha=5%) -> assume equal variance

#Normality test in two groups
shapiro.test(Gestazione_M)
shapiro.test(Gestazione_F)
#Data not normally distributed -> use a Wilcox test.
wilcox.test(Gestazione~Sesso, data = newborn_data)
# wilcox.test(Gestazione_M,Gestazione_F) #equivalent syntax (same p-value)
# p-value < alpha (5%) -> there is a significant mean difference


leveneTest(Cranio~Sesso, data = newborn_data)  #Test must be not significant
#p-value = 0.274 -> test not significant (alpha=5%) -> assume equal variance

#Normality test in two groups
shapiro.test(Cranio_M)
shapiro.test(Cranio_F)
#Data not normally distributed -> use a Wilcox test.
wilcox.test(Cranio~Sesso, data = newborn_data)
# p-value < alpha (5%) -> there is a significant mean difference

leveneTest(Peso~Sesso, data=newborn_data) #Test must be not significant
#p-value = 0.373 -> test not significant (alpha=5%) -> assume equal variance
#shapiro test already done
wilcox.test(Peso~Sesso, data=newborn_data)

leveneTest(Lunghezza~Sesso, data=newborn_data) #Test must be not significant
#p-value = 0.00121 -> test IS significant (alpha=5%) -> variances aren't homogeneous
#shapiro test already done
wilcox.test(Lunghezza~Sesso, data=newborn_data)



##6 in some hospitals there are more caesarean childbirth than natural ones.
##Let's check if it's true.

# In this case we must compare frequency distributions of two qualitative variables.
# Chi squared test is the tool.
test.indipendency=chisq.test(x=Tipo.parto,y=Ospedale)

test.indipendency$p.value
# p-value = 0.578 -> can't reject H0 (alpha = 5%) -> can't determine an association between
# the two variables (i.e. it's not true that some hospitals prefer caesarean birth)

#Visualize the contingency matrix
ggpubr::ggballoonplot(data=as.data.frame(test.indipendency$observed),
                      fill="blue")
#Actually there isn't a pattern in balloon plot


#### MULTIDIMENSIONAL ANALYSIS ####

##1 Check relationships between variable pairs. Pay attention to response variable.

#Compute the Pearson correlation factor (r) between all pairs. Here the snippet
#from official doc is used.

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- (cor(x, y))
    txt <- format(c(r, 1), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 1.5)
}
#correlations
pairs(newborn_data,lower.panel=panel.cor, upper.panel=panel.smooth)

#TODO: commentare quello che esce fuori
#Attenzione alle variabili qualitative: qui sto usando correlazione di Pearson
#che per queste variabili non va bene del tutto

# possible multicollinearity warnings:
# Cranio vs Lunghezza (r = 0.6)
# Anni.madre vs N.gravidanze (r = 0.38)
# Gestazione vs Lunghezza (r = 0.62)
# Gestazione vs Cranio (r = 0.46)
# Lungehzza vs Sesso (r = 0.19) #quite safe
# Gestazione vs Sesso (r = 0.13) #quite safe
# Cranio vs Sesso (r = 0.15) #quite safe

#Other regressor pairs are safe (r is often below 10%).
#However this can be confirmed by computing VIF factors,
#after model build.

##2 Compute a linear regression model with all variables

#first check normality of response variable (Peso)
shapiro.test(Peso) #not necessary!

mod = lm(Peso~., data = newborn_data)
summary(mod)

#Per tutti gli altri punti, prima bisogna fare data cleaning, altrimenti i commenti
#e il resto sono falsati


#MEGA UPDATE: NON FARE DATA CLEANING! 
#Vedi commenti di Dejan su discord.

### Linear coefficient analysis 
# First of all the F-test leads to a p-value < 2.2e-16, and taking an 
# alpha = 5%, we can reject H0 hypothesis (linear model equals to constant model).
# This means that the linear model is better than a constant value in explaining 
# weight variability.
# This model explains about 72.8% of response variability (that is the adjusted r-squared).
# 
# Considering that this kind of model is fine, now one can go further and
# investigate if all regressors are useful for the scope, and eventually remove
# them from the model.
# In other words, here the step wise optimization is applied: one starts from
# the complete model (i.e. a linear model with all possible regressors) and then
# one tries to improve model performances by removing regressors that aren't useful,
# i.e. regressors that aren't statistically significant in explaining response variability.
# 
# We can assess this aspect by computing a two tails t-test on regeressors' coefficients.
# 
# Coefficients that are significant (alpha = 5%):
# - Intercept
# - N.gravidanze
# - Gestazione
# - Lunghezza
# - Cranio
# - Tipo.parto
# - Sesso
# - Ospedale (dummy with 1 = osp3) 

#If one wants more safety, with alpha = 1%, the significant coeff. are:
# - Intercept
# - Gestazione
# - Lunghezza
# - Cranio
# - Sesso

##3 improve the model

#Create a second model with only significant regressors

mod2 = lm(Peso~Gestazione+Lunghezza+Cranio+Sesso, data = newborn_data)
summary(mod2)
#Statistical parameters:
#F-test passed (reject H0 -> better than constant)
#adjusted R-squared = 72.6%
#all regressors have coefficient with p-value < 1%.

#check if two models are significantly different -> anova

anova(mod,mod2)
#p-value < 1% ==> two models are statistically different.

#TODO CHECK THE RESIDUALS! (ma questo è chiesto dopo, quindi per ora ok)
 
#The best model is mod2 because it's simpler, and the performance is similar
#(R-squared doesn't change a lot)

##4 non linear and interaction effects

#Non linear effetcs (no interactions)
mod3 = update(mod2,~.+I(Gestazione^2))
summary(mod3)
#Statistical parameters:
#F-test passed (reject H0 -> better than constant)
#adjusted R-squared = 72.6%
#all regressors have coefficient with p-value < 1%, except Gestazione^2 (0.0241)
#AND Gestazione (0.1047). This model prefers Gestazione^2 more than Gestazione. 

#Let's see what happens if wee keep only Gestazione^2
mod4 = update(mod3,~.-Gestazione)
summary(mod4)
#Statistical parameters:
#F-test passed (reject H0 -> better than constant)
#adjusted R-squared = 72.6%
#all regressors have coefficient with p-value < 1%.


#just for curiosity
mod5= update(mod2,~.+I(log(Gestazione)))
summary(mod5)
#Statistical parameters:
#F-test passed (reject H0 -> better than constant)
#adjusted R-squared = 72.6%
#all regressors have coefficient with p-value < 1%, including log(Gestazione).

mod6 = lm(Peso~Lunghezza+Cranio+Sesso+I(log(Gestazione)), data = newborn_data)
summary(mod6)
#Statistical parameters:
#F-test passed (reject H0 -> better than constant)
#adjusted R-squared = 72.5%
#all regressors have coefficient with p-value < 1%, including log(Gestazione).
#Respect to mod5, here logarithmic term is more significant, but has a lower coefficient (in abs).

#who is better? mod5 or mod6? 
#first of all let's see if they're different with anova:
anova(mod5,mod6)
#p-value < 1% ==> they're different
#we could see AIC, BIC and other metrics.

#lower these metrics, better the model
AIC(mod,mod2,mod3,mod4,mod5,mod6) #best = mod
BIC(mod,mod2,mod3,mod4,mod5,mod6) #best = mod4

#best is mod 4 (occam).

#and what about interaction effects? Bho, idk if include them or not. 
#See lessons to check when it's fine include them. 
#Bunu chiù, va bene così, non complichiamoci di più la vita cercando cose che 
#non esistono.

#SUL MODELLO SELEZIONATO FAI ANALISI DEI RESIDUI.

