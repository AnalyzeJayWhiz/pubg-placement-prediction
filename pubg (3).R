pubg=read.csv(file.choose())
pubg
n=floor(0.07*nrow(pubg))
set.seed(123)
data=sample(seq_len(nrow(pubg)),size=n)
sample=pubg[data,]
sample
write.csv(sample,"C:\\Users\\Rupeshkumar\\Desktop\\Clg stuff\\sem3\\R prac\\archive (1)\\sample.csv")
################################################################################################################
sample=read.csv(file.choose())
na.omit(sample)
sum(is.na(sample))
cols_with_na <- colnames(sample)[colSums(is.na(sample)) > 0];cols_with_na

sum(is.na(sample$winPlacePerc))

mean_value <- mean(sample$winPlacePerc, na.rm = TRUE)

# Replace NA values with the calculated mean
sample$winPlacePerc <- ifelse(is.na(sample$winPlacePerc), mean_value, sample$winPlacePerc);

sum(is.na(sample))
################################################################################################################
attach(sample)
hist(killPoints)
y=scale(killPoints);hist(y)
plot(kills,winPlacePerc)
#count unique matches
no_matches=length(unique(matchId))
no_grp=length(unique(groupId)); no_grp
cat("there are",no_matches,"matches in our database")

# Number of players match type wise
m_types <- sample[, "matchType", drop = FALSE]
m_types <- as.data.frame(table(m_types))
colnames(m_types) <- c("Type", "Count")
print(m_types)

#In PUBG there are essentially three main modes of game: Solo, Duo and Squad.
#In a squad mode, you play in a group of 4 players. Here we can see that the match types are further broken down taking into account view modes:
#FPP - First Person Perspective
#TPP - Thirst Peron Perspective
# Normal - you can switch between views during a game However, I am not able to identify what flare- and crash- types of matches are.


library(ggplot2)

# Set the size of the plot
options(repr.plot.width = 15, repr.plot.height = 8)

# Assuming 'm_types' is your data frame
ticks <- m_types$Type
p <- ggplot(data = m_types, aes(x = Type, y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14)) +
  labs(title = "Match types") +
  xlab("Type") +
  ylab("Count")

p


#DATA FRAME OF MATCH TYPE
m_types2 <- as.data.frame(table(sample$matchType));m_types2

# Calculate aggregated counts
aggregated_squads <- sum(m_types2[m_types2$Var1 %in% c("squad-fpp", "squad", "normal-squad-fpp", "normal-squad"), "Freq"])
aggregated_duos <- sum(m_types2[m_types2$Var1 %in% c("duo-fpp", "duo", "normal-duo-fpp", "normal-duo"), "Freq"])
aggregated_solo <- sum(m_types2[m_types2$Var1 %in% c("solo-fpp", "solo", "normal-solo-fpp", "normal-solo"), "Freq"])

# Create the aggregated_mt data frame
aggregated_mt <- data.frame(count = c(aggregated_squads, aggregated_duos, aggregated_solo))
row.names(aggregated_mt) <- c("squad", "duo", "solo");aggregated_mt


library(gridExtra)

# Set the size of the plot
options(repr.plot.width = 5, repr.plot.height = 5)

# Create labels and colors
labels <- c("squad", "duo", "solo")
colors <- c("dodgerblue", "orange", "green")

# Assuming 'aggregated_mt' is your data frame
pie_data <- aggregated_mt$count
names(pie_data) <- labels

# Create the pie chart
p <- ggplot(data = data.frame(labels = names(pie_data), values = pie_data), aes(x = factor(1), y = values, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Types")) +
  labs(title = "Match Types Distribution")

# Convert the ggplot object to a grob
g <- ggplotGrob(p)

# Save the pie chart
ggsave("pie_chart.png", g, width = 5, height = 5)

# Display the pie chart
grid.arrange(g)

#histogram of number of groups
hist(numGroups)

#boxplot kills vs damage delts
ggplot(sample, aes(x = kills, y = damageDealt,group=kills)) +
  geom_boxplot() +
  labs(title = "Damage Dealt vs. Number of Kills")

####################################################################################
#boxplot kills vs damage delts
ggplot(sample, aes(x = DBNOs, y = kills,group=DBNOs)) +
  geom_boxplot() +
  labs(title = "Number of DBNOs vs. Number of Kills")
####################################################################################
ggplot(sample, aes(x = killStreaks, y = kills,group=killStreaks)) +
  geom_boxplot() +
  labs(title = "Number of kill streaks vs. Number of Kills")
###################################################################################
#max distance kills
###################################################################################


# Assuming you have your data frame named "train" loaded with appropriate data
# Calculate the number of players who didn't walk, drive, or swim
walk0 <- sum(walkDistance == 0)
ride0 <- sum(rideDistance == 0)
swim0 <- sum(swimDistance == 0)

# Print the results
cat(paste(walk0, "of players didn't walk at all,", ride0, "players didn't drive, and", swim0, "didn't swim."))

########################################################################################################
# Assuming you have your data frame named "train" loaded with appropriate data
# Subset the data frame to rows where walkDistance is 0
walk0_rows <- sample[walkDistance == 0, ]

# Calculate statistics and print
avg_place <- mean(walk0_rows$winPlacePerc, na.rm = TRUE)
min_place <- min(walk0_rows$winPlacePerc, na.rm = TRUE)
max_place <- max(walk0_rows$winPlacePerc, na.rm = TRUE)
quantile_95 <- quantile(walk0_rows$winPlacePerc, 0.95, na.rm = TRUE)

cat("Average place of non-walking players is", format(avg_place, nsmall = 3),
    ", minimum is", min_place,
    "and the best is", max_place,
    ", 95% of players have a score below", quantile_95, ".\n")
# Create a histogram
hist(walk0_rows$winPlacePerc, breaks = 40, main = "winPlacePerc distribution for non-walking players",
     xlab = "winPlacePerc", ylab = "Frequency", col = "blue")

##############################################################################################
suspects <- sample[winPlacePerc == 1 & walkDistance == 0, ]
na.omit(suspects)
length(suspects)

##############################################################################################
# Assuming you have your data frame named "train" loaded with appropriate data

# Calculate statistics and print
avg_weapons <- mean(weaponsAcquired, na.rm = TRUE)
min_weapons <- min(weaponsAcquired, na.rm = TRUE)
max_weapons <- max(weaponsAcquired, na.rm = TRUE)
quantile_99 <- quantile(weaponsAcquired, 0.99, na.rm = TRUE)

cat("Average number of acquired weapons is", format(avg_weapons, nsmall = 3),
    ", minimum is", min_weapons,
    "and the maximum is", max_weapons,
    ", 99% of players acquired less than", quantile_99, "weapons.\n")

# Create a histogram
hist(weaponsAcquired, breaks = seq(0, 88,by = 11), 
     main = "weaponsAcquired distribution",
     xlab = "weaponsAcquired", ylab = "Frequency", col = "blue")

# Set smaller margins
par(mar = c(4, 4, 2, 2))  # c(bottom, left, top, right)

# Create the histogram
#dev.off()
ggplot(sample, aes(x = weaponsAcquired)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  labs(title = "weaponsAcquired distribution",
       x = "weaponsAcquired", y = "Frequency") +
  theme_minimal()+xlim(0, 15)
##############################################################################################
library(pheatmap)

# Select only numeric columns from the data frame

numeric_data <- sample[, sapply(sample, is.numeric)]
names(numeric_data)
numeric_data =numeric_data[,1:24]
numeric_data[is.na(numeric_data)] <- 0
names(numeric_data)
# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data);correlation_matrix

# Create the cluster map using pheatmap
pheatmap(correlation_matrix, 
         fontsize = 8,
         cellwidth = 15,
         cellheight = 15,
         color = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Correlation Cluster Map",
         fontsize_rowtitle = 12,
         fontsize_coltitle = 12)

numeric_data <- sample[, sapply(sample, is.numeric)]
correlation_matrix <- cor(numeric_data)
correlation_table <- round(correlation_matrix, 2)
print(correlation_table)
################################################################################################################
################
#MMOODDEELLIINNGG######################################################################

sum(is.na(sample))
library(car)
model <- lm(winPlacePerc ~ assists + boosts + damageDealt + heals + killPlace + killPoints +
              longestKill + matchDuration + revives + rideDistance +
              roadKills + swimDistance + teamKills + vehicleDestroys + walkDistance + weaponsAcquired,
            data = sample)

vif_values <- vif(model)
vif_values

n=floor(0.75*nrow(sample))
set.seed(124)
trainingdata=sample(seq_len(nrow(sample)),size=n)
training=sample[trainingdata,]
testing=sample[-trainingdata,]
# t1=table(training$winPlacePerc);t1
# t2=table(testing$winPlacePerc);t2
# #write.csv(training,"Z:\\22pbd029\\pbd-3803(B)\\training.csv")
# prop.table(t1)
# prop.table(t2)

training$squadFlag=ifelse(training$matchType %in% c("squad-fpp","squad","normal-squad-fpp","normal-squad"),1,0)
training$soloFlag=ifelse(training$matchType %in% c("solo-fpp", "solo" , "normal-solo-fpp", "normal-solo" ),1,0)
training$duoFlag=ifelse(training$matchType %in% c("duo-fpp" , "duo", "normal-duo-fpp","normal-duo" ),1,0)
training$crashFlag=ifelse(training$matchType %in% c("crashfpp", "crashtpp"),1,0)
training$flareFlag=ifelse(training$matchType %in% c("flaretpp", "flarefpp"),1,0)

#LINEAR REGREESSION
fit1=lm(winPlacePerc ~ assists + boosts + damageDealt + heals + killPlace + killPoints +
          longestKill + matchDuration + revives + rideDistance +roadKills + swimDistance + teamKills + vehicleDestroys + walkDistance + weaponsAcquired + squadFlag + soloFlag + duoFlag + crashFlag, data = training)

summary(fit1)
#contrasts(as.factor(training$matchType))
# b=summary(fit1)$coefficient["Pr(>|t|)"]
# b[b>=0.60]
fit2=lm(winPlacePerc ~ assists + boosts + damageDealt + heals + killPlace + killPoints +
          longestKill + matchDuration + revives + rideDistance+ swimDistance + teamKills + vehicleDestroys + walkDistance + weaponsAcquired + squadFlag + soloFlag + duoFlag + crashFlag, data = training)

summary(fit2)
############################
fit3=lm(winPlacePerc ~ assists + boosts + damageDealt + heals + killPlace + killPoints +
          longestKill + matchDuration + revives + rideDistance+ swimDistance + teamKills + vehicleDestroys + walkDistance + weaponsAcquired + squadFlag + soloFlag + duoFlag, data = training)

summary(fit3)
#######################
fit4=lm(winPlacePerc ~ assists + boosts + damageDealt + heals + killPlace+
          longestKill + matchDuration + revives + rideDistance+ swimDistance + teamKills + vehicleDestroys + walkDistance + weaponsAcquired + squadFlag + soloFlag + duoFlag, data = training)

summary(fit4)
######################

fit5=lm(winPlacePerc ~ assists + boosts + damageDealt + heals + killPlace+
          longestKill + matchDuration + revives + rideDistance+ swimDistance + teamKills + walkDistance + weaponsAcquired + squadFlag + soloFlag + duoFlag, data = training)

summary(fit5)

#model=6.620e-01+(assists)*2.100e-02+(boosts)*1.490e-02+(damageDealt)*-2.109e-04+(heals)*1.264e-03+(killPlace)*-4.038e-03+(longestKill)*-1.636e-04+(matchDuration)*-1.765e-04+(revives)*7.017e-03+(rideDistance)*2.229e-05+(swimDistance)*1.301e-04+(teamKills)*-1.836e-02+(walkDistance)*1.330e-04+(weaponsAcquired)*1.520e-02+(squadFlag)*3.924e-02+(soloFlag)*1.167e-01+(duoFlag)*8.755e-02  


#model diagonostics: overall model significance
# #H0:beta1=beta2=...=betap=0(insignificant)
# #H1: Atleast one of them differs(significant)
# 
# with(fit5,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=FALSE))
# or
# 1-pchisq(2056.9-1624.0,df=(2498-2490)) #here, null deviance-residual deviance

#pvalue=0<0.05,we reject H0 i.e Atleast one of them is non zero. Hence model building can be done and model is significant

#prediction
p=predict(fit5,type='response')
head(p)

# Calculate Mean Squared Error (MSE)#######################
mse <- mean((training$winPlacePerc - p)^2)
cat("Mean Squared Error:", mse, "\n")

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((training$winPlacePerc - p)^2))
cat("Root Mean Squared Error:", rmse, "\n")

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(training$winPlacePerc - p))
cat("Mean Absolute Error:", mae, "\n")
##########################################################################
####LOGISTIC########################################################

#for winplaceprec >0.9 no. of matches is not equal to winners
training$winPlacePerc_LR=ifelse(training$winPlacePerc>0.5,1,0)
training$winPlacePerc_LR
table(training$winPlacePerc_LR)
length(unique(training$matchId))
names(training)
#############################
fit6=glm(winPlacePerc_LR ~ assists + boosts + damageDealt + heals + killPlace + killPoints +
           longestKill + matchDuration + revives + rideDistance+ swimDistance + teamKills + vehicleDestroys + walkDistance + weaponsAcquired + squadFlag + soloFlag + duoFlag + crashFlag,data = training,family = binomial())
summary(fit6)

fit7=glm(winPlacePerc_LR ~ assists + boosts + damageDealt + heals + killPlace + killPoints +
           longestKill + matchDuration+ rideDistance+ swimDistance + teamKills + vehicleDestroys + walkDistance + weaponsAcquired + squadFlag + soloFlag + duoFlag + crashFlag,data = training,family = binomial())
summary(fit7)


fit8=glm(winPlacePerc_LR ~ assists + boosts + damageDealt + heals + killPlace+
           longestKill + matchDuration+ rideDistance+ swimDistance + teamKills + vehicleDestroys + walkDistance + weaponsAcquired + squadFlag + soloFlag + duoFlag + crashFlag,data = training,family = binomial())
summary(fit8)


fit9=glm(winPlacePerc_LR ~ assists + boosts + damageDealt + heals + killPlace+
           longestKill + matchDuration+ rideDistance+ swimDistance + teamKills + vehicleDestroys + walkDistance + weaponsAcquired + squadFlag + soloFlag + duoFlag,data = training,family = binomial())
summary(fit9)
# 
# with(fit9,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=FALSE))

pLR=predict(fit9,type='response')
head(pLR)

predLR=ifelse(pLR>0.5,'True','False');predLR
tabLR=table(predicted=pred,actual=training$winPlacePerc_LR) ;tabLR
sumtableLR=addmargins(tab,FUN=sum);sumtableLR

TAP=sumtableLR[3,2];TAP #TRUE ACTUAL POSITIVE
TAN=sumtableLR[3,1];TAN #TRUE ACTUAL NEGATIVE
TP=sumtableLR[2,2];TP #TRUE  POSITIVE
TN=sumtableLR[1,1];TN #TRUE NEGATIVE
FP=sumtableLR[2,1];FP #FALSE POSITIVE
FN=sumtableLR[1,2];FN #FALSE NEGATIVE

#sensitivity (RECALL)
TPR=TP/TAP;TPR #TRUE POSITIVE RATE
#since sensitivity is 86% that means our model to predict the win place well.

#specificity
TRN=TN/TAN ;TRN #TRUE NEGATIVE RATE
#since specificity is 93%,our model is good for not win place.

#1-specificity
FPR=FP/TAN;FPR #FALSE POSITIVE RATE


#The proportion of obs. correctly classified is(accuracy):
sum(diag(tab))/sum(tab) 
#or
#acc=((118984+91597)/233465)*100; acc
#since accuracy is 90% but sensitivity is more,we cant rely on for win place

library(ROCR)
ROCR_pred=prediction(p,training$winPlacePerc_LR) ;ROCR_pred
ROCR_pref=performance(ROCR_pred,'tpr','fpr') ;ROCR_pref
plot(ROCR_pref,colorize=T,main="ROC CURVE",ylab="Sensitivity",xlab="1-Specificity")
abline(a=0,b=1)

auc=performance(ROCR_pred,measure="auc")
auc=auc@y.values[[1]] ;auc

# In this case, the AUC is 0.96, which is a very good score. This means that the model is able to correctly identify people with major depressive syndrome with a high degree of accuracy.
# 
# The sensitivity of the model is 0.8, which means that 80% of the people with major depressive syndrome were correctly identified by the model. The specificity of the model is 0.89, which means that 89% of the people without major depressive syndrome were correctly identified by the model.


#-+-+---++-+---+-+-+-+-++--+-+-+-+-+-+-+-++-+-+---+-+-+-+-+-+-+---+-+-+-+-+-+-+-+-+----+-+-+-+-+-+-+-+-+-+-+
#-+-+---++-+---+-+-+-+-++--+-+-+-+-+-+-+-++-+-+---+-+-+-+-+-+-+---+-+-+-+-+-+-+-+-+----+-+-+-+-+-+-+-+-+-+-+
#-+-+---++-+---+-+-+-+-++--+-+-+-+-+-+-+-++-+-+---+-+-+-+-+-+-+---+-+-+-+-+-+-+-+-+----+-+-+-+-+-+-+-+-+-+-+
library(xgboost)
library(ggplot2)
library(lattice)
library(caret)
library(e1071)

summary(sample)
