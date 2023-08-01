#######
#############DSAP STATTISTICAL ANALYSIS SESSION#####

############# ANALYSIS OF HIGH IMPACT HYBRIDS 
#IMPORTING DATASET

DSAP <- read.csv("Data2023.csv", header = TRUE) 


#NAMING VARIABLES
DSAP$Year <- as.factor(DSAP$Year)
DSAP$System <- as.factor(DSAP$System)
DSAP$Hybrid <- as.factor(DSAP$Hybrid)
DSAP$Rep <- as.factor(DSAP$Rep)
DSAP$PHT <- as.numeric(DSAP$PHT)
DSAP$EHT <- as.numeric(DSAP$EHT)
DSAP$YIELD <- as.numeric(DSAP$YIELD)
DSAP$PROT <- as.numeric(DSAP$PROT)
DSAP$STA <- as.numeric(DSAP$STA)
DSAP$OIL <- as.numeric(DSAP$OIL)

###HISTOGRAMS FOR THE DATA

hist(DSAP$PHT, freq = FALSE, main = "Plant Height Distribution")
hist(DSAP$EHT, freq = FALSE, main = "Ear Height Distribution")
hist(DSAP$YIELD, freq = FALSE, main = "Yield Distribution")

hist(DSAP$PHT, freq = FALSE,col="Red",xlab = 'Plant Height (cm)', main = "Plant Hieght Distribution")
hist(DSAP$EHT, freq = FALSE,col = "Blue", xlab = 'Ear Height (cm)' , main = "Ear Height Distribution")
hist(DSAP$YIELD, freq = FALSE,col = "Brown",xlab = 'Grain Yield (Bu/Acre)', main = "Grain Yield Distribution")


hist(DSAP$PROT, freq = FALSE,col="Red",xlab = 'Protein Content (%)', main = "Protein Distribution")
hist(DSAP$STA, freq = FALSE,col = "Blue", xlab = 'Starch Content (%)' , main = "Starch Distribution")
hist(DSAP$OIL, freq = FALSE,col = "Brown",xlab = 'Oil Content (%)', main = "Oil Distribution")



library(ggplot2)
library(dplyr)
install.packages("hrbrthemes")
library(hrbrthemes)

ggplot(DSAP, aes(x = YIELD, fill = System)) +
  geom_histogram(position = "identity", alpha = 0.4)+
  labs(y= "Frequency", x = "Grain Yield [Bu/Acre]")
 

ggplot(DSAP, aes(x = PHT, fill = System)) +
  geom_histogram(position = "identity", alpha = 0.4)+
  labs(y= "Frequency", x = "Plant Height [cm]")

ggplot(DSAP, aes(x = EHT, fill = System)) +
  geom_histogram(position = "identity", alpha = 0.4)+
  labs(y= "Frequency", x = "Ear Height [cm]")








####PLOTING BOXPLOTs

#Performance across systems

library(ggplot2)
ggplot(DSAP, aes(x = System, y = YIELD,  fill = System, las =1)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot(outlier.colour="Black", outlier.shape=1,outlier.alpha = 0, #this removes outliers. 
               outlier.size=2, notch = FALSE,varwidth = FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        color = "white"))+
  theme(axis.text= element_text(angle = , hjust = 1, size=13, color="black"))+
  labs(x = "Management System", y = "Grain Yield [Bu/Acre]", cex.lab=5) +
  theme(axis.title = element_text(size = rel(1.5)))


## Performance between hybrids 

library(ggplot2)
ggplot(DSAP, aes(x = Hybrid, y = YIELD,  fill = System, las= 2)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot(outlier.colour="Black", outlier.shape=1,outlier.alpha = 0, #this removes outliers. 
               outlier.size=2, notch = FALSE,varwidth = FALSE) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        color = "white"))+
  theme(axis.text= element_text(angle = 90, hjust = 1, size=13, color="black"))+
  labs(x = "High Impact Hybrids", y = "Grain Yield", cex.lab=5) +
  theme(axis.title = element_text(size = rel(1.5)))



#SIMPLE LINEAR MODEL For Agronomic Traits

model_Yield <- lm(YIELD ~ System + Hybrid + System*Hybrid, data = DSAP)
anova(model_Yield)

model_PHT <- lm(PHT ~ System + Hybrid + System*Hybrid, data = DSAP)
anova(model_PHT)

model_EHT <- lm(EHT ~ System + Hybrid + System*Hybrid, data = DSAP)
anova(model_EHT)


#SIMPLE LINEAR MODEL For Grain Quality Traits

model_PROT <- lm(PROT ~ Hybrid, data = DSAP)
anova(model_PROT)

model_STA <- lm(STA ~ Hybrid, data = DSAP)
anova(model_STA)

model_OIL <- lm(OIL ~ Hybrid, data = DSAP)
anova(model_OIL)


# Multiple mean comparison test                   

install.packages("agricolae") 
library(agricolae)
Yield_System <- HSD.test(model_Yield,alpha = 0.05, "System") #Tuckeys least square differences. 
Yield_System

Yield_Hybrid <- HSD.test(model_Yield,alpha = 0.05, "Hybrid") #Tuckeys least square differences. 
Yield_Hybrid


Prot_Hybrid<- HSD.test(model_PROT,alpha = 0.05, "Hybrid") #Tuckeys least square differences. 
Prot_Hybrid

STA_Hybrid <- HSD.test(model_STA,alpha = 0.05, "Hybrid") #Tuckeys least square differences. 
STA_Hybrid

OIL_Hybrid <- HSD.test(model_OIL,alpha = 0.05, "Hybrid") #Tuckeys least square differences. 
OIL_Hybrid


#Correlations between Traits
install.packages("corrplot")  # Uncomment and run this line if you haven't installed the corrplot package
library(corrplot)
DSAP2 <- na.omit(DSAP)

# Calculate the correlation matrix for all the traits
correlation_matrix <- cor(DSAP2[,9:16])

# Create a heatmap to visualize the correlation matrix
corrplot(correlation_matrix, method = "color", type = "full", 
         tl.cex = 0.7, cl.cex = 0.7, addCoef.col = "black")



