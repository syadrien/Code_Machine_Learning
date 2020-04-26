# Clustering

# HAC
# k-means
# GMMs


#
# Hierarchical agglomerative clustering

# Load libraries
library(tidyverse)
library(skimr)
library(dendextend) # for "cutree" function

# Using data from the 1977 US census statistical abstract
# store as a data fr?me (currently a matrix)
s <- as.data.frame(state.x77)

# take a look at the summary stats and distributions for each
skim(s)


# select a few related features, standardize, and calculate euclidean distance matrix
s_sub <- s %>% 
  select(Income, Illiteracy? `Life Exp`, `HS Grad`) %>% 
  scale() %>% 
  dist()


s_sub # inspect to make sure features are on the same scale

# Fit and viz all in a single pane
par(mfrow = c(2,2))
hc_single <- hclust(s_sub, 
                    method = "single"); plot(hc_single, h?ng = -1)

hc_complete <- hclust(s_sub, 
                      method = "complete"); plot(hc_complete, hang = -1)

hc_average <- hclust(s_sub, 
                     method = "average"); plot(hc_average, hang = -1)

hc_centroid <- hclust(s_sub,
             ?        method = "centroid"); plot(hc_centroid, hang = -1)

# reset plot space
par(mfrow = c(1,1))


# And we can cut and compare trees if we aren't sure about 3 or 4 clusters, e.g.
cuts <- cutree(hc_complete, 
               k = c(3,4))

### Inspect assig?ments for each iteration...
cuts

### Or, a matrix of assignments by cut
table(`3 Clusters` = cuts[,1], 
      `4 Clusters` = cuts[,2])



#
## k-means and 2012 pres vote shares

# Load data and update the pres vote data for 2012
pres <- read.csv(file.choo?e()) # use "2012_pres dem vote by state" data in Data folder - 2012 presidential election Dem vote shares by state
colnames(pres)[colnames(pres)=="X"] <- "State" 
colnames(pres)[colnames(pres)=="dem_vs"] <- "DVS"

head(pres)

# fit the algorithm
set.seed(6?4)

kmeans <- kmeans(pres[ ,2], 
                 centers = 2,
                 nstart = 15)

# Inspect the kmeans object
str(kmeans)

pres$Cluster <- as.factor(kmeans$cluster) # save clusters as factor for plotting

# Assess a little more descriptively
t ?- as.table(kmeans$cluster)
(t <- data.frame(t))
rownames(t) <- pres$State
colnames(t)[colnames(t)=="Freq"] <- "Assignment"
t$Var1 <- NULL

head(t, 10)

# evaluate the distribution of states based on their cluster assignment
ggplot(pres, aes(DVS, fill = Clu?ter)) + 
  geom_histogram(binwidth = 3) + 
  theme_bw() +
  scale_fill_manual(values=c("darkblue", "darkred")) +
  labs(x = "Democratic Vote Share",
       y = "Count of States") +
  geom_vline(xintercept = 50, linetype="solid", 
             color = "dark?ray", size=1.2)


# Searching for the likely "misclassified" state
which(pres$DVS < 50 & pres$DVS > 47) 

pres[19,]
