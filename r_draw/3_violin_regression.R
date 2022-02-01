library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)




files <- list.files(path = "data/regression", pattern = "*.tsv", full.names = TRUE, recursive = FALSE)
get_name <- function(x) strtoi(sapply(strsplit(x, "/."), `[`, 1), base = 0L)
for (file in files) {
  name <- tools::file_path_sans_ext(basename(file))

  pheno_table <- na.omit(read.table(file, header = T, sep = "\t"))
  cf <- coef(lm(pheno_table[, 2] ~ pheno_table[, 1]))
  print(cf)
  pheno_table[1] <- lapply(pheno_table[1], as.character)
  colnames(pheno_table)[1]

  sample_size <- pheno_table %>%
    group_by(!!!rlang::syms(colnames(pheno_table)[1])) %>%
    summarize(num = n())

  myaxiss <- "myaxis"

  pdf(paste0("images/", name, ".pdf"))
  p <- pheno_table %>%
    left_join(sample_size) %>%
    mutate(myaxis = paste0(!!!rlang::syms(colnames(pheno_table)[1]), "\n", "n=", num)) %>%
    ggplot(aes_string(x = myaxiss, y = colnames(pheno_table)[2], fill = colnames(pheno_table)[1])) +
    geom_violin(width = 0.5) +
    geom_boxplot(width = 0.1, color = "white", alpha = 0.2) +
    geom_abline(slope = cf[2], intercept = cf[1], lwd = .8, color = "grey") +
    scale_fill_viridis(discrete = TRUE) +
    # theme_ipsum() +
    # theme(
    #   legend.position="none",
    #   plot.title = element_text(size=11)
    # ) +
    ggtitle(paste0(colnames(pheno_table)[1], " regression on ", colnames(pheno_table)[2])) +
    xlab("")
  # c("#BCE4D8", "#49A4B9", "#2C5985")
  p <- p + scale_fill_manual(values = c("#ccebc5", "#8dd3c7", "#bebada"))
  print(p)
  dev.off()
}




