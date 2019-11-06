library(factoextra)

# Data preparation (PC1-PC20 coords)
pca_matrix <- read.table("pca_labeled_all.txt", header=T, sep="\t")
#rownames(pca_matrix) <- pca_matrix$ID1
pca_matrix <- as.matrix(pca_matrix[,3:22])


# K-MEANS clusterization
# Find the best k -- better not
#fviz_nbclust(pca_matrix, kmeans, method = "gap_stat", k.max=20)

# Compute k-means with k = 4
set.seed(1234)
km.res <- kmeans(pca_matrix, 3, nstart = 25)
head(km.res$cluster)

# Add k-means cluster labels to original dataframe with all PCs 
pca_labeled <- read.table("pca_labeled_all.txt", header=T, sep="\t")
km_pca <- as.data.frame(cbind(pca_labeled, km.res$cluster))
colnames(km_pca)[24] <- "cluster"
km_pca$cluster <- as.factor(km_pca$cluster)
# Plot 3D PCA (PC1-3) with colors corresponding to each new cluster. Save it as html file
km_pca_3d <- plot_ly(km_pca, x=~PC1, y=~PC2, z=~V5, color=~cluster, colors=brewer.pal(n = 3, name = "Set1"), 
                     text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}"),
                     marker=list(size=3)) %>% 
  add_markers()%>%
  layout(title="K-means clusters",
         scene=list(xaxis=list(title="PC1"), yaxis=list(title="PC2"), zaxis=list(title="PC3")))
htmlwidgets::saveWidget(as_widget(km_pca_3d), "km_pca_3d_labeles.html")

# Plot 3D PCA (PC2-4)
km_pca_3d <- plot_ly(km_pca, x=~PC2, y=~V5, z=~V6, color=~cluster, colors=brewer.pal(n = 3, name = "Set1"), 
                     text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}"),
                     marker=list(size=3)) %>% 
  add_markers()%>%
  layout(title="K-means clusters. PC2-4",
         scene=list(xaxis=list(title="PC2"), yaxis=list(title="PC3"), zaxis=list(title="PC4")))
htmlwidgets::saveWidget(as_widget(km_pca_3d), "km_pca_3d_pc2-4_labeles.html")


# HDBSCAN clustering algorithm
install.packages("dbscan")
library(dbscan)

# Run HDBSCAN and plot 3d PCA with new clusters
hdb_pca <- hdbscan(pca_matrix, minPts = 25)
# PCA (PC1-3) plot
hdb_pca_3d <- plot_ly(km_pca, x=~PC1, y=~PC2, z=~V5, color=~as.factor(hdb_pca$cluster), colors=brewer.pal(n = length(unique(hdb_pca$cluster)), name = "Set1"), 
                     text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}"),
                     marker=list(size=3, opacity=0.7)) %>% 
  add_markers()%>%
  layout(title="HDBSCAN minPts=20 PC1-3",
         scene=list(xaxis=list(title="PC1"), yaxis=list(title="PC2"), zaxis=list(title="PC3")))
htmlwidgets::saveWidget(as_widget(hdb_pca_3d), "hdb_pca_3d_pc1-3_20_labeles.html")
# PCA (PC2-4) plot
hdb_pca_3d <- plot_ly(km_pca, x=~PC2, y=~V5, z=~V6, color=~as.factor(hdb_pca$cluster), colors=brewer.pal(n = length(unique(hdb_pca$cluster)), name = "Set1"), 
                      text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}"),
                      marker=list(size=3, opacity=0.7)) %>% 
  add_markers()%>%
  layout(title="HDBSCAN minPts=30. PC2-4",
         scene=list(xaxis=list(title="PC2"), yaxis=list(title="PC3"), zaxis=list(title="PC4")))
htmlwidgets::saveWidget(as_widget(hdb_pca_3d), "hdb_pca_3d_pc2-4_30_labeles.html")


# Subset interesting (africans etc) populations
pca_subset <- read.table("pca_labeled_all.txt", header=T, sep="\t")
pops <- c("African-Americans", "Esan", "Yoruba")
pca_subset <- pca_subset[pca_subset$POP%in%pops,]
pca_subset <- as.matrix(pca_subset[,3:22])
# And run HDBSCAN -- can it fully resolve African and African-American populations? (not really)
hdb_pca_subset <- hdbscan(pca_subset[,1:3], minPts = 10)
hdb_pca_subset_3d <- plot_ly(km_pca[km_pca$POP%in%pops,], x=~PC1, y=~PC2, z=~V5, color=~as.factor(hdb_pca_subset$cluster),
                      opacity=hdb_pca_subset$outlier_scores,
                      text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}<br>outlier_score"),
                      marker=list(size=3)) %>% 
  add_markers()%>%
  layout(title="HDBSCAN subset afro minPts=10. PC1-3(3)",
         scene=list(xaxis=list(title="PC1"), yaxis=list(title="PC2"), zaxis=list(title="PC3")))
htmlwidgets::saveWidget(as_widget(hdb_pca_subset_3d), "hdb_pca_subset_afro_3d_pc1-33_10_labeles.html")
