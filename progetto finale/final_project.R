rm(list= ls())

require(ggplot2)
require(gghalves)
require(ggthemes)
require(scales)
require(moments)
require(colorspace)

setwd("C:/Users/aless/Desktop/MASTER DATA SCIENCE/statistica-inferenziale-per-datascience/progetto finale")

#1
newborn_data = read.csv("neonati.csv",sep = ",")
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

#two sided (default) - alpha = 5% (default)
#weight
t.test(Peso_M, mu = weight_mean_M)
t.test(Peso_F, mu = weight_mean_F)

#length
t.test(Lunghezza_M, mu = length_mean_M)
t.test(Lunghezza_F, mu=length_mean_F)

#Only for male length I have p-value > alpha... Curious...


#5 differences between Male and Females for other vars.
#Chi squared test? Or always t-test?









