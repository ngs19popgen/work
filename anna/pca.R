library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(RColorBrewer)

library(datasets)

data(iris)
head(iris)

pca <- read.table("plink.eigenvec", sep=" ", nrows = 5444)
head(pca)
labels <- read.table("samples-pop-2019-10-25.txt", header=T, sep=' ')
labels$ID <- as.character(labels$ID)
head(labels$ID)
labels <- separate(data = labels, col = ID, into = c("ID1", "ID2"), sep = "_", remove=F)
colnames(labels) <- c("ID_total", "ID1", "ID", "POP")
colnames(labels) <- c("ID", "ID1", "ID2", "POP")
write.table(labels, "labels.txt", col.names = T, row.names = F, quote = F, sep="\t")
colnames(pca)[1:4] <- c("ID1", "ID", "PC1", "PC2")
pca$ID <- as.character(pca$ID)

pca_labeled <- merge(pca, labels[,2:4], by=c("ID1", "ID"), all=F)
sum(duplicated(pca_labeled$ID))

pops <- c("Russians", "Finns", "Basques", "Komi", "Jews", "Yoruba", "Yakuts")
rpops <- as.character(sample(pca_labeled$POP, 10))
pca_subset <- pca_labeled[pca_labeled$POP%in%pops,]
pca_rsubset <- pca_labeled[pca_labeled$POP%in%rpops,]
pic2 <- ggplot(pca_labeled, aes(PC1, PC2, col=POP)) + geom_point()
pic3 <- ggplot(pca_subset, aes(PC1, PC2, col=POP)) + geom_point()
pic_r10 <- ggplot(pca_rsubset, aes(PC1, PC2, col=POP)) + geom_point()
pic_3d <- plot_ly(pca_labeled, x=~PC1, y=~PC2, z=~V5, color=~POP, 
                  colors = brewer.pal(80),"Spectral") %>% add_markers()

pic_pc13 <- ggplot(pca_labeled, aes(PC1, V5, col=POP)) + geom_point()

write.table(pca_labeled[,c(1:4,23)], "pca_labeled.txt", col.names = T, row.names = F, sep="\t", quote = F)
#pca_labeled <- read.table("pca_labeled.txt", header=T, sep="\t")
gg <- ggplotly(pic3)
gg_r10 <- ggplotly(pic_r10)
gg_pc13 <- ggplotly(pic_pc13)

Sys.setenv("plotly_username"="valyaeva.anna")
Sys.setenv("plotly_api_key"="2DiTimC3tsoJXuJjF0u4")
api_create(gg, filename = "pca_some_labeles")
api_create(gg_r10, filename = "pca_r10_labeles")
api_create(gg_pc13, filename = "pca_pc13_labeles")
api_create(pic_3d, filename = "pca_3d_labeles")
