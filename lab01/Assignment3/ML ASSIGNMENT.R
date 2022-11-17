
# load the data
library(readr)
PID_data <- read_csv("PID.csv", col_names = FALSE)
PID_data

# View the column name
colnames(PID_data)

# Update/change the column names
colnames(PID_data) <- c("Num_Preg", "Plasma_glucose", "Diastolic_BP", "Triceps_ST",
                        "Serum_insulin", "Body_MI", "Diabetes_PF", "Age", "Diabetes")

# Note that "Diabetes" is categorical of binary, so we make it a factor
PID_data$Diabetes <- as.factor(PID_data$Diabetes)

# summary
summary(PID_data)

#Scatter Plot

library(ggplot2)
scatter <- ggplot(data=PID_data, aes(x=Age, y=Plasma_glucose, color=Diabetes)) +
  geom_point() + labs(x="Age", y="Plasma glucose") +
  ggtitle("Distribution of Plasma glucose concentration on Age by Diabetes level")
scatter

"Yes! Motivation: Looking at the plot, one can see that the plasma glucose concentration
is more higher for the group with Diabetes level 1 compared to those with level 0.
This is more evident when plasma glucose concentration is 150 or higher.
At such one can conclude that it will be easy
to classify Diabetes by a standard logistic regression"


# First we partition our dataset into Training and Testing

library(caTools)

set.seed(101) #  This is needed for reproducibility
sample = sample.split(PID_data$Diabetes, SplitRatio = .70)
Training = subset(PID_data, sample == TRUE)
Testing  = subset(PID_data, sample == FALSE)


## Question 2 model 1: GLM
########################################################
"Logistic regression analysis belongs to the class of generalized linear models.
In R generalized linear models are handled by the glm() function. The function is
written as glm(response ~ predictor, family = binomial(link = "logit"), data).
Please note that logit is the default for binomial; thus, we do not have to type it explicitly.
The glm() function returns a model object, therefore we may apply extractor functions, such as summary(),
fitted() or predict, among others, on it. However, please note that the output numbers are on the logit scale.
To actually predict probabilities we need to provide the predict() function an
additional argument type = "response".      "

# Our model
fglm <- glm(Diabetes ~ Plasma_glucose + Age, family=binomial, data=Training)

#Diabetes[Diabetes=="0"] when you want to force R to observe the "0", use this


# To predict
fitted.glm <- predict(fglm, newdata=Testing, type = "response")



#### for Threshold = 0.5 (=default)
# pos_or_neg <- ifelse(probability_prediction > threshold, positive_class, negative_class)
#p_class <- factor(pos_or_neg, levels = levels(test_values))

pos_or_neg <- ifelse(fitted.glm >= 0.5, 1, 0)
p_class <- factor(pos_or_neg, levels = levels(Testing$Diabetes))



#Report the probabilistic equation of the estimated model (i.e., how the target depends
#on the features and the estimated model parameters probabilistically)

summary(fglm)

"From the glm model, Both Plasma glucose and Age have a significant influence on Diabetes.
More explicitly, For every unit increase in Plasma glucose, with Age fixed,
the Probability of belonging to the Diabetes group1 will increase by 0.036.
Similary, for every unit increase in Age, while keeping Plasma glucose fixed,
the Probability of belonging to the Diabetes group1 will increase by 0.223."


#Compute also the training misclassification error and make a scatter plot

## classification performance
library(caret)

fglm2 <- glm(Diabetes ~ Plasma_glucose + Age, family=binomial, data=Training)


fitted.glm2 <- predict(fglm2, type = "response")

#For Training
pos_or_neg2 <- ifelse(fitted.glm2 >= 0.5, 1, 0)
p_class2 <- factor(pos_or_neg2, levels = levels(Training$Diabetes))


#Training classification

cm = confusionMatrix(p_class2, Training$Diabetes)
print(cm) # 76% accuracy

#Testing classification
cm = confusionMatrix(p_class, Testing$Diabetes)
print(cm) # 74% accuracy


# comment:
" from the results of this classification, we can see that our classification accuracy is 74%,
this implies that our misclassificatioin error is about 26%. This is not too bad I think"

# Scatterplot
pdata <- cbind.data.frame("Predicted"= p_class, Testing)
ggplot(data=pdata, aes(x=Age, y=Plasma_glucose, color= Predicted)) +
  geom_point() + labs(x="Age", y="Plasma glucose") +
  ggtitle("Distribution of Plasma glucose concentration on Age by predicted Diabetes level")



# comment:
"Just as seen with the original plot earlier, the plot with the predicted Diabetes also depits that
level-1 Diabetes have more plasma glucose compare to level-0 group"

