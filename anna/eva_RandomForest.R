library(caret)
library(randomForest)

# Read in data provided by Eva and make a matrix
eva_subset <- read.table("subset_total.txt", header=T, sep="\t")
pca_subset <- as.matrix(eva_subset[,3:22])

# Run HDBSCAN clustering and visualize the results
hdb_pca_subset <- hdbscan(pca_subset, minPts = 12)
eva_subset$hdb_cluster <- hdb_pca_subset$cluster
hdb_pca_subset_3d <- plot_ly(eva_subset, x=~V3, y=~V4, z=~V5, color=~as.factor(eva_subset$hdb_cluster),
                             text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}<br>outlier_score"),
                             marker=list(size=3)) %>% 
  add_markers()%>%
  layout(title="HDBSCAN subset afro minPts=12. PC1-3(3)",
         scene=list(xaxis=list(title="PC1"), yaxis=list(title="PC2"), zaxis=list(title="PC3")))

# Remove "0" cluster (it's a "noise" cluster)
eva_subset_filtered <- eva_subset[eva_subset$hdb_cluster!=0,]
eva_subset_filtered$hdb_cluster <- as.factor(eva_subset_filtered$hdb_cluster)

# Create training and testing subsets
set.seed(123)
set_index <- createDataPartition(eva_subset_filtered$hdb_cluster, p=.8, list=F)
training_set <- eva_subset_filtered[set_index,]
test_set <- eva_subset_filtered[-set_index,]

# Shuffle the cluster labels in test set (control)
n <- length(test_set$hdb_cluster)
test_set$hdb_cluster <- test_set$hdb_cluster[sample(1:n,n)]
head(test_set)
table(training_set$hdb_cluster)

# Run random forest classification
rf_oob_comp <- randomForest(formula = hdb_cluster ~ V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22,
                            data= training_set, xtest = test_set[,3:9], ytest = test_set[,26])
print(rf_oob_comp)
