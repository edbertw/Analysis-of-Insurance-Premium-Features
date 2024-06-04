library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(reshape2)
library(randomForest)
library(caret)
 
insurance <- read_csv("insurance.csv")
print(summary(insurance))
null_counts <- sapply(insurance, function(x)
  sum(is.na(x)))
table <- data.frame(NoOfNull = null_counts)
print(table)
insurance <- insurance |>
  mutate(children = factor(children)) |>
  mutate(charges = round(charges,2)) |> #convert to 2 d.p. for precision
  mutate(bmi = round(bmi,1))

premiumvsgender<-insurance |> 
  group_by(sex) |>
  summarize(mean_charges = mean(charges)) |>
  ggplot(aes(sex,mean_charges,fill = sex)) +
  geom_col(show.legend=FALSE) +
  xlab("Gender") +
  ylab("Average Premiums") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0,hjust =1)) 

premiumvssmoker<-insurance |> 
  group_by(smoker) |>
  summarize(mean_charges = mean(charges)) |>
  ggplot(aes(smoker,mean_charges,fill = smoker)) +
  geom_col(show.legend=FALSE) +
  xlab("Smoker") +
  ylab("Average Premiums") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0,hjust =1))

premiumvsregion<-insurance |> 
  group_by(region) |>
  summarize(mean_charges = mean(charges)) |>
  ggplot(aes(region,mean_charges,fill = region)) +
  geom_col(show.legend=FALSE) +
  xlab("Regions") +
  ylab("Average Premiums") +
  theme_stata() +
  theme(axis.text.x = element_text(angle = 45,hjust =1))+
  theme(axis.text.y = element_text(angle = 0,hjust =1))

premiumvschildren<-insurance |> 
  group_by(children) |>
  summarize(mean_charges = mean(charges)) |>
  ggplot(aes(children,mean_charges,fill = children)) +
  geom_col(show.legend=FALSE) +
  xlab("# of children") +
  ylab("Average Premiums") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0,hjust =1))

print(grid.arrange(premiumvsgender,premiumvssmoker,premiumvsregion,premiumvschildren,ncol=2,nrow=2))
#we see that most number of smokers are in southeast in respect to other areas.
smokerbyregions <- insurance |>
  ggplot(aes(region,fill = smoker)) +
  geom_bar(position = "dodge") +
  theme_stata() +
  theme(axis.text.x = element_text(angle = 45,hjust =1)) +
  theme(legend.text = element_text(size = 6),legend.title = element_text(size=10))
  
print(smokerbyregions)
#we see that smoking greatly affects premium value, but just how important of a factor is it?

premiumvsage <- insurance |>
  ggplot(aes(age,charges,color=smoker)) +
  geom_line() +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0,hjust =1)) +
  theme(legend.text = element_text(size = 6),legend.title = element_text(size=10))

premiumvsbmi <- insurance |>
  ggplot(aes(bmi,charges,color=smoker)) +
  geom_line()+
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0,hjust =1)) +
  theme(legend.text = element_text(size = 6),legend.title = element_text(size=10))
#bmi vs charges positive correlation IF the person is a smoker
print(grid.arrange(premiumvsage,premiumvsbmi,nrow=1,ncol=2))

#encode factors to numeric for machine learning evaluation
insurance <- insurance |>
  mutate(sex = ifelse(sex == "male",0,1),
         smoker = ifelse(smoker =="yes",1,0),
         region = case_when(
           region == "northwest" ~ 0,
           region == "northeast" ~ 1,
           region == "southeast" ~ 2,
           TRUE ~ 3
         )) |>
  mutate_at(c("sex","children","smoker","region"),as.numeric)
print(head(insurance))
#building correlation matrix & heatmap
corr_matrix <- round(cor(insurance),2)
print(head(corr_matrix))
melted_corr_matrix <- melt(corr_matrix)
print(head(melted_corr_matrix))
heatmap <- melted_corr_matrix |>
  ggplot(aes(Var1,Var2,fill=value)) +
  geom_tile() +
  geom_text(aes(Var2,Var1,label=value),color = "black", size=2.8) +
  theme(axis.text.x = element_text(angle = 45,hjust =1)) +
  scale_fill_gradient(low = "blue", high = "grey") +
  xlab("") +
  ylab("") +
  ggtitle("Correlation Heatmap")
#from the heatmap we see most correlation with smoker and charges variables
print(heatmap)

#How random forest works ? Multiple decision trees searching for best feature among a random subset of features
#outputs mean of squared residuals which is basically MAE (Error term)
#also outputs %Var explained which is basically the model's ability to detect more complex features
#more number of trees will cause higher accuracy but more fitting runtime.
set.seed(42) 
rf_model <- randomForest(formula = insurance$charges ~ . , data = insurance, ntree=1250,mtry = 3,
                         keep.forest= FALSE, importance = TRUE )
print(rf_model) 
predicted_insurance <- insurance |>
  mutate(prediction = rf_model$predicted)
print(head(predicted_insurance))

#how accurate is this model visually?
predictedvsactual <- predicted_insurance |>
  ggplot(aes(charges,prediction)) +
  geom_point(color = "red") +
  geom_abline(slope=1,intercept=0,lty="solid",color='blue') +
  theme_stata() +
  xlab("Actual Premium") +
  ylab("Prediction Premium") +
  ggtitle("Random Forest Regression") +
  theme(axis.text.y = element_text(angle = 0,hjust =1))
print(predictedvsactual)
model <- lm(predicted_insurance$prediction~predicted_insurance$charges)
print(summary(model)) #85.3% accuracy and 0.853 r-squared value.
#As visualized, many predicted points lie in the y = x line and are approximately similar to the actual value

importance <- as.data.frame(varImp(rf_model))
importance <- importance |>
  mutate(Overall = round(Overall,2)) |>
  arrange(desc(Overall)) |>
  rename(Importance_Score = Overall)
print(importance)
#what is the most important ?
print(importance |> 
  top_n(1, Importance_Score))
#We can see that whether the policyholder is a smoker or not affects premium prices the most (Positively)

#Possible improvements 
# - Implementing more complex machine learning algorithms / testing out several algorithms to determine the most accurate one
# - A dataset that can incorporate more features/variables for a more rounded observation
