# Read in raw data 
ord <- read.table("/home/danat/data/data_from_article/ord")
ord$idx <- c(1:nrow(ord))
# Create vector with populations' names ordered by V1 and V2 values 
lvls <- unique(as.character(ord[order(-ord$V1, -ord$V2),]$Name))

# Melt the table to plot
new_tbl <- melt(ord, id.vars=c("Name", "idx"))
new_tbl$variable <- factor(new_tbl$variable, levels=c("V3", "V2", "V1"))

# A plot that we want to make nicer
bp = barplot(t(as.matrix(ord)),
             space = c(0.2),
             names.arg = ord$Name,
             axes = T,
             col=rainbow(4),
             xlab="Individual #",
             ylab="Ancestry",
             legend.text = c(V3 = "gathered-hundreing", V2 = "early farmers", V4 = "ld"),
             border=NA)

####################
library(ggplot2)

# A bit nicer plot 
ggplot(new_tbl, aes(fill=variable, y=value, x=factor(new_tbl$Name, levels = lvls))) +
  geom_bar(position="fill", stat="identity", width=0.8)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color="black", size=10), legend.position = "top") +xlab("")+ylab("")+
  scale_x_discrete(labels=lvls) + scale_fill_discrete(name = "", breaks=c("V3", "V2", "V1"), labels = c("Yamnaya steppe", "early farmers", "hunter-gatherer"))
ggsave("danat_plot.png", width=7, height=5, dpi=600)
