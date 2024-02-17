rm(list= ls())

require(ggplot2)
require(gghalves)
require(ggthemes)
require(scales)
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

##QUALITATIVE VARS (fumatrici, Tipo.parto, Ospedale, Sesso)

#create a custom function to reduce code
qualitative_summary = function(x) {
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

qualitative_summary(Fumatrici)
qualitative_summary(Tipo.parto)
qualitative_summary(Ospedale)
qualitative_summary(Sesso)


# ggplot(data=newborn_data)+
#     geom_boxplot(aes(y=Anni.madre))+
#     theme_hc()



##FARE QUESTO GRAFICO PURE PER LE ALTRE VARIABILI QUANTITATIVE

ggplot(data=newborn_data)+
    geom_half_violin(mapping = aes(y=Anni.madre),
                     side="l",fill="#87AEDF")+
    geom_half_boxplot(mapping = aes(y=Anni.madre),
                      side="r",fill="#DA95CC",center=T,width=0.5)+
    theme_hc()










