set.seed(1680) # for reproducibility
#---------------------------------
# call in R packages for use in this study
library(cluster)  # cluster analysis methods
library(dplyr) # for data cleaning
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

# -----------------------------------------------------
# read bank data into R, creating data frame bank
# -----------------------------------------------------
data_for_clustering <- read.csv("data_for_clustering.csv")

# -----------------------------------------------------
# data cleaning, make sure to remove name before clustering
# -----------------------------------------------------
data_for_clustering <- subset(data_for_clustering,
                              select = c("response","age", "whitecollar", "bluecollar","other_job",
                                         "divorced", "married","single",
                                         "primary", "secondary", "tertiary", "unknown_edu"))

data_for_clustering$whitecollar <- factor (data_for_clustering$whitecollar)
data_for_clustering$bluecollar <- factor (data_for_clustering$bluecollar)
data_for_clustering$other_job <- factor (data_for_clustering$other_job)
data_for_clustering$divorced <- factor (data_for_clustering$divorced)
data_for_clustering$married <- factor (data_for_clustering$married)
data_for_clustering$single <- factor (data_for_clustering$single)
data_for_clustering$primary <- factor (data_for_clustering$primary)
data_for_clustering$secondary <- factor (data_for_clustering$secondary)
data_for_clustering$tertiary <- factor (data_for_clustering$tertiary)
data_for_clustering$unknown_edu <- factor (data_for_clustering$unknown_edu)
 
glimpse(data_for_clustering)

summary(data_for_clustering)

# -----------------------------------------------------
# clustering with PAM solution 
# -----------------------------------------------------

# Calculate Gower distance 
# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, the type remains coded as "I"

gower_dist <- daisy(data_for_clustering[, -1], metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)

# print out the most similar and dissimilar pair 
# in the data to see if it makes sense
gower_mat <- as.matrix(gower_dist)

# Output most similar pair
data_for_clustering[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
data_for_clustering[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#----------------------------------
# Selecting the number of clusters
# Calculate silhouette width for many k using PAM
# This step may take 10 minutes or more to complete 
# (depending on the number of Clusters)

sil_width <- c(NA)

for(i in 2:16){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:16, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:16, sil_width)

#------------------------------------------------
# Cluster Interpretation

pam_fit <- pam(gower_dist, diss = TRUE, k = 15)

pam_results <- data_for_clustering %>%
  dplyr::select(-response) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

# Show the Medoids
# interpretation is that the medoids serve as exemplars of each cluster
data_for_clustering[pam_fit$medoids, ]

# -----------------------------------------------------
# Cluster Interpretation
# Via Visualization
# with t-distributed stochastic neighborhood embedding, or t-SNE
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         response = data_for_clustering$response)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

# ----------------------------------------------------
# Mosaic Plot
# response  Response to term deposit offer (yes, no)
# ----------------------------------------------------
# add the cluster membership information into dataset 
data_for_clustering$cluster <- pam_fit$clustering

with(data_for_clustering, print(table(cluster, response)))

mosaicplot(~cluster + response, data = data_for_clustering,
           main =list("Subcribe the Term of Deposit",col="gray20"),
           xlab="Cluster", ylab="Subcribe",col = c("gray70","mediumseagreen"),
           type = "deviance", border = NA)

#----------------------------------------------------------
# Cluster Interpretation
# Via Descriptive Statistics
#----------------------------------------------------------
# compute percentage of yes responses to term deposit offer
response_table <- table(data_for_clustering$cluster, data_for_clustering$response)
cat("\nPercentage Responses\n")
for (i in 1:15)
  cat("\n", toupper(letters[i]),
      round(100 * response_table[i,2] /
              sum(response_table[i,]), digits = 1))

# compute the percentage of the customers in each cluster
print(round(100 * table(data_for_clustering$cluster) / nrow(data_for_clustering), digits = 1))

