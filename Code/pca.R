# PCA
# Two cases: US arrests and legislative professionalism

# load libraries
library(tidyverse)
library(ggfortify) # for autoplot

# EXAMPLE 1: US ARRESTS
arrest_fit <- prcomp(USArrests, 
                  scale = TRUE)

summary(arrest_fit)

names(arrest_fit) # rotation = loadings; x = scores

# visualize
autoplot(arrest_fit,
         shape = FALSE, # names instead of points
         loadings.label = TRUE) + # show the loading directions
theme_bw()


#
# EXAMPLE 2: STATE LEGISLATOR DATA

# First read in .Rdata
st <- x %>% 
  filter(sessid == "2009/10") %>% 
  select(-c(fips, stateabv, sessid, mds1, mds2, year)) %>%
  na.omit()

# standardize features
st_scale <- data.frame(scale(st[,2:5]))
rownames(st_scale) <- st$state

# fit the model
leg_fit <- prcomp(st_scale); summary(leg_fit) # no need to scale because we standardized earlier

# loadings (feature-level) and scores (observation-level)
loadings <- leg_fit$rotation; head(loadings)

scores <- leg_fit$x; head(scores)


# Plot scree
plot(leg_fit,
     type="l", 
     main = "PCs across PVE")

# Biplot
autoplot(leg_fit,
         shape = FALSE, 
         loadings.label = TRUE) +
  theme_bw()

#


#
# On your own if you're interested (manual calc and plotting)
# munge data
scaled_df <- USArrests %>%
  rownames_to_column(var = "State") %>%
  as_tibble() %>%
  mutate_at(.vars = vars(-State), scale)

# covariance matrix
arrests_cov <- scaled_df %>%
  select(-State) %>%
  cov()

# calculate and store evs
(arrests_eigen <- eigen(arrests_cov))

# extract first two loadings
phi <- arrests_eigen$vectors[, 1:2]
phi <- -phi 
row.names(phi) <- c("Murder", "Assault", "UrbanPop", "Rape")
colnames(phi) <- c("PC1", "PC2")


# Calculate and store PC scores
PC1 <- as.matrix(select_if(scaled_df, is.numeric)) %*% phi[,1]
PC2 <- as.matrix(select_if(scaled_df, is.numeric)) %*% phi[,2]
PC <- tibble(
  State = scaled_df$State,
  PC1 = PC1[,1],
  PC2 = PC2[,1]
)


# plot scores
ggplot(PC, aes(PC1, PC2)) +
  geom_vline(xintercept = 0, size = 1, color = "black", alpha = .5) +
  geom_hline(yintercept = 0, size = 1, color = "black", alpha = .5) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = State)) +
  labs(title = "First two principal components of USArrests",
       x = "First principal component",
       y = "Second principal component") +
  theme_bw()

# biplot
phi_df <- (phi * 1.75) %>%
  as.data.frame %>%
  rownames_to_column(var = "variable")

ggplot(PC, aes(PC1, PC2)) +
  geom_vline(xintercept = 0, size = 1, color = "black", alpha = .5) +
  geom_hline(yintercept = 0, size = 1, color = "black", alpha = .5) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = State)) +
  geom_segment(data = phi_df,
               aes(x = 0, y = 0,
                   xend = PC1,
                   yend = PC2),
               arrow = arrow(length = unit(0.03, "npc")),
               color = "purple") +
  ggrepel::geom_text_repel(data = phi_df,
                           aes(x = PC1, y = PC2, label = variable),
                           color = "purple") +
  labs(title = "First two principal components of USArrests",
       x = "First principal component",
       y = "Second principal component") +
  theme_bw()