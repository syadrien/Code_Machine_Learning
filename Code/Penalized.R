# penalized regression lab
# pdwaggoner@uchicago.edu


# load libs
library(glmnet)
library(foreach)
library(pROC)
library(ggfortify)
library(ggpubr)
library(gridExtra)
library(tidyverse)


# read in the state medicaid data
dataset <- read_csv(file.choose())

dataset <- dataset %>% 
  select(oppose_expansion, gop_governor, percent_favorable_aca, gop_leg, bal2012, multiplier,
           percent_nonwhite, percent_uninsured, percent_metro, percent_poverty) 

set.seed(1244)

# store some things
d <- dataset
f <- oppose_expansion ~ scale(d[,-1])
y <- d$oppose_expansion
X <- model.matrix(f, d)[,-1]
n <- length(y)

## LASSO WITH ALPHA = 1
# CV to find optimal lambda
cv1 <- cv.glmnet(X, y, 
                 family = "binomial", 
                 nfold = 10, 
                 alpha = 1)
plot(cv1) # viz cv

# Store fitted values across multiple values of lambda
cv1.glmnet.fit <- (cv1$glmnet.fit)
plot(cv1.glmnet.fit, xvar = "lambda")

# Fit the optimal model and inspect
lassomod <- glmnet(X, y, 
                   family = "binomial", 
                   lambda = cv1$lambda.min, 
                   alpha = 1)
(lassomod$beta)


#
## RIDGE WITH ALPHA = 0
# CV to find optimal lambda
cv2 <- cv.glmnet(X, y, 
                 family = "binomial", 
                 nfold = 10, 
                 alpha = 0)

# plot cv error across lambda
plot(cv2)

# Store the fitted values for plotting all smoothed models
cv2.glmnet.fit <- (cv2$glmnet.fit)
plot(cv2.glmnet.fit, xvar = "lambda")

# Fit the optimal model and inspect coefs
ridgemod <- glmnet(X, y, 
                   family = "binomial", 
                   lambda = cv2$lambda.min, 
                   alpha = 0)
(ridgemod$beta)

# 
## ELASTIC NET WITH 0 < ALPHA < 1
# CV for search for alpha values (ranging between 0 and 1, for mixing of L1 and L2)
doParallel::registerDoParallel(cores=2)

a <- seq(0.1, 0.9, 0.05) # create a sequence to constrain the search

search <- foreach(i = a, .combine = rbind) %dopar% {
  # calcluate multiple lambda values at all alpha values to find the optimal mix between L1 and L2
  cv <- cv.glmnet(X, y, 
                  family = "binomial",
                  nfold = 10, 
                  alpha = i)
  # Store in a data frame to search next
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.min], 
             lambda.min = cv$lambda.min, alpha = i)
}

# select and plot the alpha value for the optimal lambda value that minimizes MSE 
(cv3 <- search[search$cvm == min(search$cvm), ])

ggplot(search, aes(alpha, cvm)) +
  geom_line() +
  geom_vline(aes(xintercept = cv3[, 3], 
                 color="red"), 
             show.legend = FALSE) +
  #annotate("text", x = 0.7, y = 0.998, label = "Min. MSE") +
  #geom_curve(aes(x = 0.7, y = 0.995, xend = 0.75, yend = 0.989), 
  #           color = "red", 
  #           size = 0.3, 
  #           arrow = arrow(length = unit(0.03, "npc"))) +
  labs(y = "Mean Squared Error",
       x = "Alpha") +
  theme_minimal()


# store values across multiple models
cv3.1 <- cv.glmnet(X, y, 
                   family = "binomial", 
                   nfold = 10,
                   alpha = cv3$alpha)
plot(cv3.1)

cv3.glmnet.fit <- (cv3.1$glmnet.fit)
plot(cv3.glmnet.fit, xvar = "lambda")

# Fit and inspect the optimal model
elasticnetmod <- glmnet(X, y, 
                        family = "binomial", 
                        lambda = cv3$lambda.min, 
                        alpha = cv3$alpha)

elasticnetmod$beta # coefs (and dropped features)
(elasticnetmod$lambda == cv3$lambda.min) # check lambda values; should evaluate to TRUE


#
# better plot of the output via autoplot (ggplot2)
lassoplot <- autoplot(cv1$glmnet.fit, "lambda", label = TRUE, main = "LASSO (alpha = 1)") + 
  theme(legend.position="right") + 
  scale_colour_discrete(name = "Features", 
                        labels = c("Intercept", "GOP Governor", "% Favorable ACA", "GOP Legislature", "2012 Ballot", "Multiplier", "% Nonwhite", "% Uninsured", "% Metropolitan", "% Poverty")) + 
  theme(legend.title = element_text(size=20)) + 
  theme(legend.text = element_text(size = 18)) + 
  geom_vline(data = NULL, 
             xintercept = log(cv1$lambda.min), 
             na.rm = FALSE, show.legend = TRUE)

ridgeplot <- autoplot(cv2$glmnet.fit, "lambda", label = TRUE, main = "Ridge (alpha = 0)") + 
  geom_vline(data = NULL, 
             xintercept = log(cv2$lambda.min), 
             na.rm = FALSE, 
             show.legend = TRUE)

elasticnetplot <- autoplot(cv3.1$glmnet.fit, "lambda", label = TRUE, main = "Elastic Net") + 
  geom_vline(data = NULL, 
             xintercept = log(cv3$lambda.min), 
             na.rm = FALSE, 
             show.legend = TRUE)

g_legend <- function(plot){
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend <- g_legend(lassoplot)

final_plot <- grid.arrange(arrangeGrob(ridgeplot + 
                                     theme_bw() +
                                     theme(legend.position="none"),
                                   lassoplot + 
                                     theme_bw() +
                                     theme(legend.position="none"), 
                                   elasticnetplot + 
                                     theme_bw() +
                                     theme(legend.position="none"), 
                                   mylegend, nrow = 1))

# 
