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
    
    d = data.frame(n = n, mu=x_mu,variance = x_var,sigma = x_sigma, CV = x_CV)
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

quantitative.summary(Anni.madre)
### WAIT
### There are 2 abnormal values in Anni.madre: there is one record with age = 0
### and one record with age = 1. These two values are veeery weird -> ask what teh hell happened.
### The ohter values starts from 13 and are up to 46. 13 and 14 years is more realistic but for clarity, 
### ask to coach (dai mbare, chi spacchiu è na gravidanza a 13 anni?!).
quantitative.summary(N.gravidanze)
### WAIT pt. 2
### The maximum value here is 12 (that is for a 38 yrs woman).
### Well, it's actually possible, but... What the hell?! 
quantitative.summary(Gestazione)
quantitative.summary(Peso)
quantitative.summary(Lunghezza)
quantitative.summary(Cranio)

## All other quantitative vars show no weird things.


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


#Replace mu = 0 with actual value taken from literature
#t.test(Lunghezza,mu=0) #two sided (default) - alpha = 5% (default)
#t.test(Peso,mu=0)

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
# t.test(Peso_M, mu = weight_mean_M)
# t.test(Peso_F, mu = weight_mean_F)
wilcox.test(Peso_M, mu = weight_mean_M)
wilcox.test(Peso_F, mu = weight_mean_F)



#length
# t.test(Lunghezza_M, mu = length_mean_M)
# t.test(Lunghezza_F, mu=length_mean_F)
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
# Rimuovi le ultime 3 colonne dal dataframe <- se metto stringsAsFactor=T in read.csv non serve
df_senza_ultime_3 = newborn_data[, -c((ncol(newborn_data)-2):ncol(newborn_data))]

# pairs(df_senza_ultime_3,lower.panel=panel.cor, upper.panel=panel.smooth) 
pairs(newborn_data,lower.panel=panel.cor, upper.panel=panel.smooth)

#TODO: commentare quello che esce fuori
#Attenzione alle variabili qualitative: qui sto usando correlazione di Pearson
#che per queste variabili non va bene del tutto

##2 Compute a linear regression model with all variables

#first check normality of response variable (Peso)
shapiro.test(Peso)

mod = lm(Peso~., data = newborn_data)
summary(mod)

#Per tutti gli altri punti, prima bisogna fare data cleaning, altrimenti i commenti
#e il resto sono falsati




