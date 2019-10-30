library(factoextra)

# Data preparation (PC1-PC20 coords)
pca_matrix <- read.table("pca_labeled_all.txt", header=T, sep="\t")
#rownames(pca_matrix) <- pca_matrix$ID1
pca_matrix <- as.matrix(pca_matrix[,3:22])


# K-MEANS
# Find the best k
fviz_nbclust(pca_matrix, kmeans, method = "gap_stat", k.max=20)
ggsave("kmeans_best_k20.png")

# Compute k-means with k = 4
set.seed(1234)
km.res <- kmeans(pca_matrix, 3, nstart = 25)

fviz_cluster(km.res, data = pca_matrix,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

head(km.res$cluster)

km_pca <- as.data.frame(cbind(pca_labeled, km.res$cluster))
colnames(km_pca)[24] <- "cluster"
km_pca$cluster <- as.factor(km_pca$cluster)
km_pca_3d <- plot_ly(km_pca, x=~PC1, y=~PC2, z=~V5, color=~cluster, colors=brewer.pal(n = 3, name = "Set1"), 
                     text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}"),
                     marker=list(size=3)) %>% 
  add_markers()%>%
  layout(title="K-means clusters",
         scene=list(xaxis=list(title="PC1"), yaxis=list(title="PC2"), zaxis=list(title="PC3")))
htmlwidgets::saveWidget(as_widget(km_pca_3d), "km_pca_3d_labeles.html")
#api_create(km_pca_3d, filename = "km_pca_3d_labeles")

km_pca_3d <- plot_ly(km_pca, x=~PC2, y=~V5, z=~V6, color=~cluster, colors=brewer.pal(n = 3, name = "Set1"), 
                     text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}"),
                     marker=list(size=3)) %>% 
  add_markers()%>%
  layout(title="K-means clusters. PC2-4",
         scene=list(xaxis=list(title="PC2"), yaxis=list(title="PC3"), zaxis=list(title="PC4")))
htmlwidgets::saveWidget(as_widget(km_pca_3d), "km_pca_3d_pc2-4_labeles.html")


# HDBSCAN
install.packages("dbscan")
library(dbscan)

hdb_pca <- hdbscan(pca_matrix, minPts = 30)
hdb_pca_3d <- plot_ly(km_pca, x=~PC1, y=~PC2, z=~V5, color=~as.factor(hdb_pca$cluster), colors=brewer.pal(n = length(unique(hdb_pca$cluster)), name = "Set1"), 
                     text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}"),
                     marker=list(size=3, opacity=0.7)) %>% 
  add_markers()%>%
  layout(title="HDBSCAN minPts=30. PC1-3",
         scene=list(xaxis=list(title="PC1"), yaxis=list(title="PC2"), zaxis=list(title="PC3")))
htmlwidgets::saveWidget(as_widget(hdb_pca_3d), "hdb_pca_3d_pc1-3_30_labeles.html")
hdb_pca_3d <- plot_ly(km_pca, x=~PC2, y=~V5, z=~V6, color=~as.factor(hdb_pca$cluster), colors=brewer.pal(n = length(unique(hdb_pca$cluster)), name = "Set1"), 
                      text=~POP, hovertemplate=paste("<b>%{text}</b><br>x: %{x}<br>y: %{y}<br>z: %{z}"),
                      marker=list(size=3, opacity=0.7)) %>% 
  add_markers()%>%
  layout(title="HDBSCAN minPts=30. PC2-4",
         scene=list(xaxis=list(title="PC2"), yaxis=list(title="PC3"), zaxis=list(title="PC4")))
htmlwidgets::saveWidget(as_widget(hdb_pca_3d), "hdb_pca_3d_pc2-4_30_labeles.html")
