weird_table <- read.table('f_try.eigenvec')
plot(weird_table[3:4])
names <- read.table('samples-pop-2019-10-25.txt', header = T)


library("tidyr")
name_separat <- separate(data = names, col = ID, into = c("Name", "Biggest_Name"), sep = "_")


str(weird_table)
str(names)

god_save_us <- merge(weird_table, name_separat, by.x = "V2", by.y = "Biggest_Name")

library("ggplot2")

ggplot(god_save_us, aes(x=V3,y=V4, color = POP))+
  geom_point()+
  theme(legend.position = "none")

###################

write.table(god_save_us, file = "god_save_us.txt")
for_3d <- god_save_us[,c(3,4,5,24)]
write.table(for_3d, file = "for_3d", sep = ",")
