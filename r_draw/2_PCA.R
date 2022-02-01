library(ggplot2)


draw_pca <- function(table_name, name, classes, colors) {
  table <- read.table(paste0("data/", table_name, ".tsv"), header = T, sep = "\t")
  table[name][table[name] == 0] <- classes[1]
  table[name][table[name] == 1] <- classes[2]
  pdf(paste0("images/", table_name, ".pdf"))
  theme <- theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(), axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"), axis.ticks = element_line(colour = "black"), plot.margin = unit(c(1, 1, 1, 1), "line"))
  p <- ggplot(table, aes_string(x = "PC1", y = "PC2", color = name))
  p <- p + geom_point() + theme + scale_color_manual(values = colors)
  print(p)
  dev.off()
}


table_name <- "pca_death"
name <- "death"
classes <- c("alive", "dead")
colors <- c("#8dd3c7", "#fb8072")
draw_pca(table_name, name, classes, colors)

table_name <- "pca_sex"
name <- "sex"
classes <- c("male", "female")
colors <- c("#bc80bd", "#8dd3c7")
draw_pca(table_name, name, classes, colors)
