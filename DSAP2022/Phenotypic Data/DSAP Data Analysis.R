#######
#############DSAP STATTISTICAL ANALYSIS SESSION#####

###SIMPLE OPERATIONS IN R###
 ##ADDITION

Y = 5 + 15 

X <- 5 + 15


####### MULTIPLICATIONS 
Z <- Y * X 
T <- 14 * 20 * 50 



####SET OF NUMBERS
M = c(1,1,3,2,5,5,5,7,6,10,10,10,4,4,4,5,5,7,7,7,8,9,8,5, 2, 3, 4, 5,6)
mean(M)
max(M)
min(M)

hist(M)




############# ANALYSIS OF HIGH IMPACT HYBRIDS 
#IMPORTING DATASET





#NAMING VARIABLES
DSAP$Student <- as.factor(DSAP$Student)
DSAP$Code <- as.factor(DSAP$Code)
DSAP$Pedigree <- as.factor(DSAP$Pedigree)
DSAP$REP <- as.factor(DSAP$REP)
DSAP$PHT <- as.numeric(DSAP$PHT)
DSAP$EHT <- as.numeric(DSAP$EHT)


###HISTOGRAMS FOR THE DATA

hist(DSAP$PHT)
hist(DSAP$EHT)


####PLOTING OTHER BoxPlots
par(mfrow=c(1,1))
plot(DSAP$Code, DSAP$PHT, xlab="Hybrid", color= DSAP$Code,fill= DSAP$Code, ylab="Plant Height [cm]",col = c("#2AEBD7", "#732AEB", "#EBE22A"))

plot(DSAP$Code, DSAP$EHT, xlab="Hybrid", color= DSAP$Code,fill= DSAP$Code, ylab="Ear Height [cm]",col = c("#2AEBD7", "#732AEB", "#EBE22A"))








