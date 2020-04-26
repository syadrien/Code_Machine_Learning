# Load some libraries and packages
library(foreign) # for loading Stata data
library(MASS) # for LDA
library(class) # for kNN
library(faraway) # for ilogit
library(ggplot2) # for plots
library(arm) # for a few base features
library(OOmisc) # for ePCP
library(pROC) # for ROC curve


# now read in the data: each obs is a case
data <- read.dta(file.choose())
data <- read_dta("C:/Users/elira/Desktop/R_Projects/Introduction to Machine Learning/Data/Court.dta")

# rescale the "liberal" variable
data$liberal2 <- round(data$liberal * 100, 2)

plot(propetit ~ liberal2, data, xlim=c(0,100), ylim = c(0,1), 
     xlab="Liberalism", 
     ylab="Observed Petition Outcome")

# demonstrating OLS is poor
lmod <- lm(propetit ~ liberal2, data)
abline(lmod)


# Logistic regression on the full data
logit <- glm(propetit ~ ineffcou + multpet + usparty + liberal2,
               data = data,
               family = binomial); summary(logit)

logit.probs <- predict(logit, 
                       type = "response") 

head(logit.probs)

logit.pred <- ifelse(logit.probs > 0.5, 1 ,0)

# confusion matrix
table(logit.pred, data$propetit)

mean(logit.pred == data$propetit)


# Make training and test set - more appropriate for inference
set.seed(436)
samples <- sample(1:nrow(data), 
                  nrow(data)*0.8, 
                  replace = FALSE)
train <- data[samples, ]
test <- data[-samples, ]

propetit <- test$propetit

logit <- glm(propetit ~ ineffcou + multpet + usparty + liberal2, 
             data = train, 
             family = binomial)

logit.probs <- predict(logit, 
                       newdata = test, 
                       type="response") 

logit.pred <- ifelse(logit.probs > 0.5, 1, 0)

table(logit.pred, propetit)

mean(logit.pred == propetit)




# Fit restricted model with fewer X for another check
logit1 <- glm(propetit ~ usparty + liberal2,
             data = train, 
             family = binomial)

logit.probs1 = predict(logit1, 
                       newdata = test, 
                       type="response") 

logit.pred1 <- ifelse(logit.probs1 > 0.5, 1, 0)

table(logit.pred1, propetit)

mean(logit.pred1 == propetit)

6 / (0 + 6) # example of accuracy measure: TP/FP+TP == precision/"positive predicted value"


#
# Another approach beyond test/train as well as classification - conditional, out of sample predicted probabilities
logitmod3 <- glm(propetit ~ liberal2 + usparty + ineffcou + multpet + 
                   liberal2*usparty, # INXN term
                   family = binomial(link=logit), 
                 data = data)

# CIs for predicted probabilities
newdata2 <- with(data, data.frame(liberal2 = rep(seq(from = 20, to = 80, length.out = 100),
                                                   2), 
                                  usparty = rep(0:1, each = 100),
                                  ineffcou = mean(ineffcou),
                                  multpet = mean(multpet))) 

newdata3 <- cbind(newdata2, predict(logitmod3, 
                                    newdata = newdata2, 
                                    type = "link",
                                    se = TRUE))

# Add CIs
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Recode usparty as a factor
newdata3$usparty <- factor(newdata3$usparty, labels=c("No", "Yes"))

# Plot predictions with CIs
ggplot(newdata3, aes(x = liberal2, y = PredictedProb, color = usparty)) +
  geom_line() +
  geom_errorbar(aes(ymin = LL, ymax = UL),
                color="gray",
                size=.3,
                width=.2,
                position = position_dodge(.9)) +
  labs(x = "Court Liberalism",
       y = "Probability of Granting Relief",
       color = "U.S. is a Party") +
  scale_fill_hue(breaks = c("No", "Yes"),
                 labels = c("No",  "Yes")) +
  ggtitle("The Conditional Effect of Liberalism on Granting Relief") +
  theme_bw() +
  theme(legend.justification = c(.7,1),
        legend.position = c(.9,.3))



#
# Quick tangent: demonstrating logit and probit are the same
# Logit Model
logitmod <- glm(propetit ~ liberal2, 
                family = binomial(link = logit), 
                data); summary(logitmod)

# Probit Model
probitmod <- glm(propetit ~ liberal2, 
                 family = binomial(link = probit), 
                 data); summary(probitmod)

# Now plot the curves
plot(propetit ~ liberal2, data, xlim=c(0,100), ylim = c(0,1), 
     xlab="Liberalism", 
     ylab="Prob of Granting Relief")
x <- seq(0, 100, 1) # key to produce two curves on a hypothetical scale to allow the two to be mapped together

lines(x, ilogit(-3.2828 + 0.06929 * x), lty = 1, col="red", lwd = 2) # Plug in estimated intercept and slope, logit
lines(x, pnorm(-1.9731 + 0.04176 * x), lty = 2, col="blue", lwd = 2) # Plug in estimated intercept and slope, probit
legend(0, 0.8, # placement of legend
       lty = c(1,2), 
       lwd = c(2,2), 
       col = c("red","blue"), c("Logit","Probit"), cex=1)

## NOTES:
# ilogit = inverse logit distribution: intercept + slope * scale - the logit link function
# pnorm = functional form for probit is normal, thus the command "pnorm"

# Using coefficient estimates, we can calculate in-sample predicted probabilities 
# corresponding to specific values of liberalism

# The following two predictions show, based on probit and logit specifications, 
# the probabilities of making pro-petitioner decision when the variable liberalism is at its minimum, median, and maximum.

# Prediction when liberalism is at its min, median, and max
ilogit(-3.2828 + 0.06929 * 21) #min
pnorm (-1.9731 + 0.04176 * 21) #min
ilogit(-3.2828 + 0.06929 * 41) #median
pnorm (-1.9731 + 0.04176 * 41) #median
ilogit(-3.2828 + 0.06929 * 83.11) #max
pnorm (-1.9731 + 0.04176 * 83.11) #max

# "phat" from glm is another way of calculating the full set of in-sample predictions
logit.phat <- logitmod$fitted.values # vector of fitted values from logit model
probit.phat <- probitmod$fitted.values # same for probit
xb1 <- 0.06929 * data$liberal2; summary(xb1)
xb2 <- 0.04176 * data$liberal2; summary(xb2)

# Plot each curve side by side
par(mfrow=c(1,2))
plot(logit.phat ~ xb1, lty=1, type = "o", xlim=c(1,6), 
     xlab = "xb", 
     ylab = "Predicted Probability")
plot(probit.phat ~ xb2, lty=2, type = "o", col = "blue", xlim = c(0,4), 
     xlab = "xb", 
     ylab = "Predicted Probability")
par(mfrow=c(1,1)) # reset plot pane space

# Now, plot predicted probabilities against one another to see how closely they match
mydata2 <- data.frame(cbind(logit.phat, probit.phat))
qplot(logit.phat, probit.phat, data = mydata2, geom = "point",
      xlab = "Logit", ylab = "Probit") +
  theme_bw()



#
## Linear Discriminant Analysis
lda <- lda(propetit ~ ineffcou + multpet + usparty + liberal2,
               data=train)

# inspect the model (don't use summary here)
lda
# proportions of yes's and no's in the data 
# group means across the features
# coefs like any model

# now, we can check for classification rates and errors
propetit <- test$propetit

lda.pred <- predict(lda, 
                    newdata = test) 

data.frame(lda.pred)[1:5,]

# confusion matrix
table(lda.pred$class, propetit)

# check the classification rate
mean(lda.pred$class == propetit)



#
## K-Nearest Neighbors

# Respecify just to make sure
set.seed(89235)
samples <- sample(1:nrow(data), 
                  nrow(data)*0.8, 
                  replace = FALSE)
train <- data[samples, ]
test <- data[-samples, ]

propetit <- test$propetit

knn <- knn(train, test, 
           train$propetit, 
           k = 1)

table(knn, propetit)

mean(knn == propetit) 



#
# ROC curves
# ROC curves plot correct predictions (sensitivity, true positive rate) against false predictions (specificity, false positive rate)
# A model with excellent fit will produce an AUC that is large (near 1)
# A model with poor fit will produce an AUC that is small (near 0)
# In other words, a poor fit produces a curve close to the 45-degree diagonal, where it's equally likely to see correct and false predictions

# logit on full sample
logit <- glm(propetit ~ ineffcou + multpet + usparty + liberal2,
             data = train,
             family = binomial)

y <- test$propetit
pred1 <- predict(logit, 
                 newdata = test, 
                 type="response")

plot.roc(y, pred1, col = "red") 

