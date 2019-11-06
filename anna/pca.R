library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(RColorBrewer)

# Read in PCA results computed by PLINK
pca <- read.table("plink.eigenvec", sep=" ", nrows = 5444)

# Read in info about individuals and their population indentification
labels <- read.table("samples-pop-2019-10-25.txt", header=T, sep=' ')
labels$ID <- as.character(labels$ID)
# Separate individual's IDs by "_" 
labels <- separate(data = labels, col = ID, into = c("ID1", "ID2"), sep = "_", remove=F)
colnames(labels) <- c("ID_total", "ID1", "ID", "POP")
#write.table(labels, "labels.txt", col.names = T, row.names = F, quote = F, sep="\t")

colnames(pca)[1:4] <- c("ID1", "ID", "PC1", "PC2")
pca$ID <- as.character(pca$ID)

# Merge PCA results and populations info by two columns ID1 and ID
pca_labeled <- merge(pca, labels[,2:4], by=c("ID1", "ID"), all=F)
#sum(duplicated(pca_labeled$ID))
# Write down merged dataframe
write.table(pca_labeled, "pca_labeled_all.txt", col.names = T, row.names = F, sep="\t", quote = F)
write.table(pca_labeled[,c(1:4,23)], "pca_labeled.txt", col.names = T, row.names = F, sep="\t", quote = F)

# Subsample some certain or random populations
pops <- c("Russians", "Finns", "Basques", "Komi", "Jews", "Yoruba", "Yakuts")
rpops <- as.character(sample(pca_labeled$POP, 10))

pca_subset <- pca_labeled[pca_labeled$POP%in%pops,]
pca_rsubset <- pca_labeled[pca_labeled$POP%in%rpops,]

# Plot 2D PCA with whole dataset and subsamples using ggplot package
pic2 <- ggplot(pca_labeled, aes(PC1, PC2, col=POP)) + geom_point()
pic3 <- ggplot(pca_subset, aes(PC1, PC2, col=POP)) + geom_point()
pic_r10 <- ggplot(pca_rsubset, aes(PC1, PC2, col=POP)) + geom_point()
# Make ggplots interactive with plotly package
gg <- ggplotly(pic3)
gg_r10 <- ggplotly(pic_r10)

# Plot interactive 3D PCA using plotly package
pic_3d <- plot_ly(pca_labeled, x=~PC1, y=~PC2, z=~V5, color=~POP) %>% 
  add_trace(marker=list(size=2))


# Publish on Plotly (should specify Sys.setenv("plotly_username"="username") and Sys.setenv("plotly_api_key"="api_key"))
api_create(gg, filename = "pca_some_labeles")
api_create(gg_r10, filename = "pca_r10_labeles")
api_create(gg_pc13, filename = "pca_pc13_labeles")
api_create(pic_3d, filename = "pca_3d_labeles")

# Or save as .html file
htmlwidgets::saveWidget(as_widget(pic_3d), "pic_3d.html")
