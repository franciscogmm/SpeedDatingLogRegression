library(data.table)
library(ggplot2)
library(psych)
library(VIM)
library(mice)

#access files
rounds <- fread('rounds.csv', sep=',')
participants <- fread('participants.csv',sep=',')

str(rounds)

#change variable types
rounds$same_country <- as.numeric(as.character(rounds$same_country))
rounds$diff_pref_attractive <- as.numeric(as.character(rounds$diff_pref_attractive))
rounds$diff_pref_sincere <- as.numeric(as.character(rounds$diff_pref_sincere))
rounds$diff_pref_intelligent <- as.numeric(as.character(rounds$diff_pref_intelligent))
rounds$diff_pref_fun <- as.numeric(as.character(rounds$diff_pref_fun))
rounds$diff_pref_ambitious <- as.numeric(as.character(rounds$diff_pref_ambitious))
rounds$diff_pref_sharedhobbies <- as.numeric(as.character(rounds$diff_pref_sharedhobbies))
#count total number of rows
nrow(rounds)

#check how many NAs are there
#summary(rounds)
mice_plot <- aggr(rounds, col = c('navyblue','yellow'), numbers = T, sortVars = T,labels = names(rounds), cex.axis = .7, gap=3, ylab = c("Missing data","Pattern"))

#removal of rows with incomplete cases
rounds.nona <- rounds[complete.cases(rounds),]
nrow(rounds.nona)
names(rounds.nona)

#store indexes in a variable
rounds.nona.char <- rounds.nona[,c(1,3,4,5,7,8)]
#store numerical data
rounds.nona <- rounds.nona[,c(2,6,9:32)]
rounds.nona$rating_by_partner_attractive <- rounds.nona$rating_by_partner_attractive/10
rounds.nona$rating_by_partner_sincere <- rounds.nona$rating_by_partner_sincere/10
rounds.nona$rating_by_partner_intelligent <- rounds.nona$rating_by_partner_intelligent/10
rounds.nona$rating_by_partner_fun <- rounds.nona$rating_by_partner_fun/10
rounds.nona$rating_by_partner_ambitious <- rounds.nona$rating_by_partner_ambitious/10
rounds.nona$rating_by_partner_sharedhobbies <- rounds.nona$rating_by_partner_sharedhobbies/10

rounds.nona$rated_partner_attractive <- rounds.nona$rated_partner_attractive/10
rounds.nona$rated_partner_sincere <- rounds.nona$rated_partner_sincere/10
rounds.nona$rated_partner_intelligent <- rounds.nona$rated_partner_intelligent/10
rounds.nona$rated_partner_fun <- rounds.nona$rated_partner_fun/10
rounds.nona$rated_partner_ambitious <- rounds.nona$rated_partner_ambitious/10
rounds.nona$rated_partner_sharedhobbies <- rounds.nona$rated_partner_sharedhobbies/10
str(rounds.nona)

#get the correlation coefficients...
#looks like there are no glaring correlation coefficients we need to worry about
M <- cor(rounds.nona)
round(M, digits = 3)
corrplot::corrplot(M, type = 'lower', diag = F)
#pairs.panels(rounds.nona)

#check the distribution of all original variables
par(mfrow = c(5,5), mar = c(2,3,1,1))
hist(rounds.nona$match, main = 'match')
hist(rounds.nona$wave_condition, main = 'wave_condition')
hist(rounds.nona$met_so_far, main = 'met_so_far')
hist(rounds.nona$int_corr, main = 'int_corr')
hist(rounds.nona$same_race, main = 'same_race')
hist(rounds.nona$same_country, main = 'same_country')
hist(rounds.nona$age_ratio, main = 'age_ratio')
hist(rounds.nona$met_before, main = 'met_before')
hist(rounds.nona$diff_pref_attractive, main = 'diff_pref_attr')
hist(rounds.nona$diff_pref_sincere, main = 'diff_pref_sinc')
hist(rounds.nona$diff_pref_intelligent, main = 'diff_pref_int')
hist(rounds.nona$diff_pref_fun, main = 'diff_pref_fun')
hist(rounds.nona$diff_pref_ambitious, main = 'diff_pref_amb')
hist(rounds.nona$diff_pref_sharedhobbies, main = 'diff_pref_shared')
hist(rounds.nona$rating_by_partner_attractive, main = 'rating_by_attr')
hist(rounds.nona$rating_by_partner_sincere, main = 'rating_by_sinc')
hist(rounds.nona$rating_by_partner_intelligent, main = 'rating_by_int')
hist(rounds.nona$rating_by_partner_fun, main = 'rating_by_fun')
hist(rounds.nona$rating_by_partner_ambitious, main = 'rating_by_amb')
hist(rounds.nona$rating_by_partner_sharedhobbies, main = 'rating_by_shared')
hist(rounds.nona$rated_partner_attractive, main = 'rated_attr')
hist(rounds.nona$rated_partner_sincere, main = 'rated_sinc')
hist(rounds.nona$rated_partner_intelligent, main = 'rated_fun')
hist(rounds.nona$rated_partner_fun, main = 'rated_int')
hist(rounds.nona$rated_partner_ambitious, main = 'rated_amb')
hist(rounds.nona$rated_partner_sharedhobbies, main = 'rated_shared')

#check distribution for log and sqrt transformations
rounds.log <- rounds.nona
rounds.sqrt <- rounds.nona
names(rounds.log)

#log transformation distributions
par(mfrow = c(5,5), mar = c(2,3,1,1))
rounds.log[,c(3,9:14)] <- log(rounds.log[,c(3,9:14)])
hist(rounds.log$match, main = 'match')
hist(rounds.log$wave_condition, main = 'wave_condition')
hist(rounds.log$met_so_far, main = 'met_so_far')
hist(rounds.log$int_corr, main = 'int_corr')
hist(rounds.log$same_race, main = 'same_race')
hist(rounds.log$same_country, main = 'same_country')
hist(rounds.log$age_ratio, main = 'age_ratio')
hist(rounds.log$met_before, main = 'met_before')
hist(rounds.log$diff_pref_attractive, main = 'diff_pref_attr')
hist(rounds.log$diff_pref_sincere, main = 'diff_pref_sinc')
hist(rounds.log$diff_pref_intelligent, main = 'diff_pref_int')
hist(rounds.log$diff_pref_fun, main = 'diff_pref_fun')
hist(rounds.log$diff_pref_ambitious, main = 'diff_pref_amb')
hist(rounds.log$diff_pref_sharedhobbies, main = 'diff_pref_shared')
hist(rounds.log$rating_by_partner_attractive, main = 'rating_by_attr')
hist(rounds.log$rating_by_partner_sincere, main = 'rating_by_sinc')
hist(rounds.log$rating_by_partner_intelligent, main = 'rating_by_int')
hist(rounds.log$rating_by_partner_fun, main = 'rating_by_fun')
hist(rounds.log$rating_by_partner_ambitious, main = 'rating_by_amb')
hist(rounds.log$rating_by_partner_sharedhobbies, main = 'rating_by_shared')
hist(rounds.log$rated_partner_attractive, main = 'rated_attr')
hist(rounds.log$rated_partner_sincere, main = 'rated_sinc')
hist(rounds.log$rated_partner_intelligent, main = 'rated_int')
hist(rounds.log$rated_partner_fun, main = 'rated_fun')
hist(rounds.log$rated_partner_ambitious, main = 'rated_amb')
hist(rounds.log$rated_partner_sharedhobbies, main = 'rated_shared')

#sqrt transformation distributions
par(mfrow = c(5,5), mar = c(2,3,1,1))
rounds.sqrt[,c(3,9:14)] <- sqrt(rounds.sqrt[,c(3,9:14)])
hist(rounds.sqrt$match, main = 'match')
hist(rounds.sqrt$wave_condition, main = 'wave_condition')
hist(rounds.sqrt$met_so_far, main = 'met_so_far')
hist(rounds.sqrt$int_corr, main = 'int_corr')
hist(rounds.sqrt$same_race, main = 'same_race')
hist(rounds.sqrt$same_country, main = 'same_country')
hist(rounds.sqrt$age_ratio, main = 'age_ratio')
hist(rounds.sqrt$met_before, main = 'met_before')
hist(rounds.sqrt$diff_pref_attractive, main = 'diff_pref_attr')
hist(rounds.sqrt$diff_pref_sincere, main = 'diff_pref_sinc')
hist(rounds.sqrt$diff_pref_intelligent, main = 'diff_pref_int')
hist(rounds.sqrt$diff_pref_fun, main = 'diff_pref_fun')
hist(rounds.sqrt$diff_pref_ambitious, main = 'diff_pref_amb')
hist(rounds.sqrt$diff_pref_sharedhobbies, main = 'diff_pref_shared')
hist(rounds.sqrt$rating_by_partner_attractive, main = 'rating_by_attr')
hist(rounds.sqrt$rating_by_partner_sincere, main = 'rating_by_sinc')
hist(rounds.sqrt$rating_by_partner_intelligent, main = 'rating_by_int')
hist(rounds.sqrt$rating_by_partner_fun, main = 'rating_by_fun')
hist(rounds.sqrt$rating_by_partner_ambitious, main = 'rating_by_amb')
hist(rounds.sqrt$rating_by_partner_sharedhobbies, main = 'rating_by_shared')
hist(rounds.sqrt$rated_partner_attractive, main = 'rated_attr')
hist(rounds.sqrt$rated_partner_sincere, main = 'rated_sinc')
hist(rounds.sqrt$rated_partner_intelligent, main = 'rated_int')
hist(rounds.sqrt$rated_partner_fun, main = 'rated_fun')
hist(rounds.sqrt$rated_partner_ambitious, main = 'rated_amb')
hist(rounds.sqrt$rated_partner_sharedhobbies, main = 'rated_shared')

final <- rounds.sqrt
final$wave_condition <- as.numeric(as.character(final$wave_condition))
final$same_race <- as.numeric(as.character(final$same_race))
final$met_before <- as.numeric(as.character(final$met_before))
final$match <- as.factor(as.character(final$match))
str(final)

summary(final)

boxplot(final)

#split data into train and test set
indexes = sample(1:nrow(final), size=0.2*nrow(final))
train <- final[-indexes,]
test<- final[indexes,]

library(MASS)
#initial model
fit <- glm(match~., family = binomial(logit), data = train)
summary(fit)

#fit.step
library(ROCR)
fitpreds.orig <- predict(fit, test, type = 'response')
fitpred.orig <- prediction(fitpreds.orig, test$match)
fitperf.orig <- performance(fitpred.orig, "tpr","fpr")
plot(fitperf.orig, col = 'green', lwd =2, main = "ROC Curve for Logistic")
abline(a = 0, b = 1, lwd = 2, lty=2, col = 'gray')
auc.orig <- performance(fitpred.orig, measure = 'auc')
auc.orig <- auc.orig@y.values[[1]]

cat('AUC: ',auc.orig)

library(caret)

test$predorig <- ifelse(fitpreds.orig >0.5, 1,0)
confusionMatrix(test$predorig, test$match, positive = "1")

#try to improve the model
fit.step <- step(fit)
summary(fit.step)

#removal of variables with high P-value
fit.step1 <- glm(formula = match ~ diff_pref_sharedhobbies + rating_by_partner_attractive + 
                   rating_by_partner_fun + rating_by_partner_ambitious + rating_by_partner_sharedhobbies + 
                   rated_partner_attractive + rated_partner_sincere + rated_partner_fun + 
                   rated_partner_ambitious + rated_partner_sharedhobbies, family = binomial(logit), 
                 data = train)

summary(fit.step1)

library(car)
vif(fit.step1)

durbinWatsonTest(fit.step1)
crPlots(fit.step)

#fit.step
library(ROCR)
fitpreds <- predict(fit.step1, test, type = 'response')
fitpred <- prediction(fitpreds, test$match)
fitperf <- performance(fitpred, "tpr","fpr")
plot(fitperf, col = 'green', lwd =2, main = "ROC Curve for Logistic")
abline(a = 0, b = 1, lwd = 2, lty=2, col = 'gray')
auc <- performance(fitpred, measure = 'auc')
auc <- auc@y.values[[1]]

cat('AUC: ',auc)

library(caret)

test$pred <- ifelse(fitpreds >0.5, 1,0)
confusionMatrix(test$pred, test$match, positive = "1")


names(rounds)
names(participants)

#more descriptive charts
descriptive <- merge(rounds, participants, by.x = 'iid_id', by.y = 'id', all.x = T)
colnames(descriptive)[33:62] <- paste("iid",colnames(descriptive[,c(33:62)]), sep = "_")
names(descriptive)
descriptive <- merge(descriptive, participants, by.x = "partner_id", by.y = "id", all.x = T)
names(descriptive)
colnames(descriptive)[63:92] <- paste("partner",colnames(descriptive[,c(63:92)]), sep = "_")
names(descriptive)
summary(descriptive)

#subset to match = 1
nomatch<- split(descriptive,descriptive$match)$'0'
yesmatch<- split(descriptive,descriptive$match)$'1'

par(mfrow = c(1,3), mar = c(2.5,2,1,1))
boxplot(yesmatch[,c(34,64)], main = "Age", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(38,68)], main = "Romantic Goal", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(56,86)], main = "Expected Happiness", col = c('lightblue','orange'), names = c('Women','Men'))

par(mfrow = c(3,5), mar = c(2.5,2,1,1))
boxplot(yesmatch[,c(41,71)], main = "Exercise", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(42,72)], main = "Dining", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(43,73)], main = "Museums", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(44,74)], main = "Art", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(45,75)], main = "Hiking", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(46,76)], main = "Gaming", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(47,77)], main = "Clubbing", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(48,78)], main = "Reading", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(49,79)], main = "TV", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(50,80)], main = "Theater", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(51,81)], main = "Movies", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(52,82)], main = "Concerts", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(53,83)], main = "Music", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(54,84)], main = "Shopping", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(55,85)], main = "Yoga", col = c('lightblue','orange'), names = c('Women','Men'))

#title("Comparison of Interests between Women and Men", outer=TRUE)

par(mfrow = c(2,3), mar = c(2.5,2,1,1))
boxplot(yesmatch[,c(57,87)], main = "Attractiveness", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(58,88)], main = "Sincerity", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(59,89)], main = "Intelligence", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(60,90)], main = "Fun", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(61,91)], main = "Ambitiousness", col = c('lightblue','orange'), names = c('Women','Men'))
boxplot(yesmatch[,c(62,92)], main = "Shared Hobbies", col = c('lightblue','orange'), names = c('Women','Men'))

library(fmsb)
library(tidyr)
summary(yesmatch[,c(41:55)])
statswoman <- as.data.frame(0.6264)
colnames(statswoman) <- 'exercise'
statswoman$dining <- mean(yesmatch[,42])
statswoman$museums <- mean(yesmatch[,43])
statswoman$art <- mean(yesmatch[,44])
statswoman$hiking <- mean(yesmatch[,45])
statswoman$gaming <- mean(yesmatch[,46])
statswoman$clubbing <- mean(yesmatch[,47])
statswoman$reading <- mean(yesmatch[,48])
statswoman$tv <- mean(yesmatch[,49])
statswoman$theater <- mean(yesmatch[,50])
statswoman$movies <- mean(yesmatch[,51])
statswoman$concerts <- mean(yesmatch[,52])
statswoman$music <- mean(yesmatch[,53])
statswoman$shopping <- mean(yesmatch[,54])
statswoman$yoga <- mean(yesmatch[,55])
statswoman

summary(yesmatch[,c(71:85)])
statsman <- as.data.frame(mean(yesmatch[,71]))
colnames(statsman) <- 'exercise'
statsman$dining <- mean(yesmatch[,72])
statsman$museums <- mean(yesmatch[,73])
statsman$art <- mean(yesmatch[,74])
statsman$hiking <- mean(yesmatch[,75])
statsman$gaming <- mean(yesmatch[,76])
statsman$clubbing <- mean(yesmatch[,77])
statsman$reading <- mean(yesmatch[,78])
statsman$tv <- mean(yesmatch[,79])
statsman$theater <- mean(yesmatch[,80])
statsman$movies <- mean(yesmatch[,81])
statsman$concerts <- mean(yesmatch[,82])
statsman$music <- mean(yesmatch[,83])
statsman$shopping <- mean(yesmatch[,84])
statsman$yoga <- mean(yesmatch[,85])
statsman

statsman <- rbind(rep(1,15), rep(0,15), statsman)
statsman
statswoman <- rbind(rep(1,15), rep(0,15), statswoman)
statswoman

women <- yesmatch[,c(41:55)]
men <- yesmatch[,c(71:85)]
colors_border=c( rgb(0.2,0.5,0.5,0.9), 
                 rgb(0.8,0.2,0.5,0.9) , 
                 rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.9,0.8,0.9), 
             rgb(0.8,0.2,0.5,0.4) , 
             rgb(0.7,0.5,0.1,0.4) )
colors_inw=c( rgb(0.9,0.5,0.5,0.4), 
              rgb(0.8,0.2,0.5,0.4) , 
              rgb(0.7,0.5,0.1,0.4) )

radarchart(statsman, axistype = 1,
           #custom polygon
           pcol=colors_border, 
           pfcol=colors_in,
           plwd = 4,
           plty = 1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8)
radarchart(statswoman, axistype = 1,
           #custom polygon
           pcol=colors_border, 
           pfcol=colors_inw,
           plwd = 4,
           plty = 1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8)

prefwoman <- as.data.frame(mean(yesmatch[,57]))
colnames(prefwoman) <- 'Attractiveness'
prefwoman$Sincerity <- mean(yesmatch[,58])
prefwoman$Intelligence <- mean(yesmatch[,59])
prefwoman$Fun <- mean(yesmatch[,60])
prefwoman$Ambitious <- mean(yesmatch[,61])
prefwoman$SharedHobbies <- mean(yesmatch[,62])

prefwoman

prefman <- as.data.frame(mean(yesmatch[,87]))
colnames(prefman) <- 'Attractiveness'
prefman$Sincerity <- mean(yesmatch[,88])
prefman$Intelligence <- mean(yesmatch[,89])
prefman$Fun <- mean(yesmatch[,90])
prefman$Ambitious <- mean(yesmatch[,91])
prefman$SharedHobbies <- mean(yesmatch[,92])

prefman

prefman <- rbind(rep(1,15), rep(0,15), prefman)
prefman
prefwoman <- rbind(rep(1,15), rep(0,15), prefwoman)
prefwoman

colors_border=c( rgb(0.2,0.5,0.5,0.9), 
                 rgb(0.8,0.2,0.5,0.9) , 
                 rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.9,0.8,0.9), 
             rgb(0.8,0.2,0.5,0.4) , 
             rgb(0.7,0.5,0.1,0.4) )
colors_inw=c( rgb(0.9,0.5,0.5,0.4), 
              rgb(0.8,0.2,0.5,0.4) , 
              rgb(0.7,0.5,0.1,0.4) )

radarchart(prefman, axistype = 1,
           #custom polygon
           pcol=colors_border, 
           pfcol=colors_in,
           plwd = 4,
           plty = 1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8)
radarchart(prefwoman, axistype = 1,
           #custom polygon
           pcol=colors_border, 
           pfcol=colors_inw,
           plwd = 4,
           plty = 1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           #custom labels
           vlcex=0.8)

