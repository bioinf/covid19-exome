library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(grid)



draw_boxplot_violin_and_save <- function(all_features_table, by, feature, prefix = "", use_sd = TRUE) {
  # cat(paste0("Drawing for ", by, " and ", feature, "...\n"))
  pheno_table <- all_features_table[c(by, feature)]
  pheno_table <- na.omit(pheno_table)
  pheno_table[1] <- lapply(pheno_table[1], as.character)
  
  
  pval <- FALSE
  if (by == 'death'){
    group_a <- pheno_table[pheno_table[by]=='dead',feature]
    group_b <- pheno_table[pheno_table[by]=='alive',feature]
    pval <- wilcox.test(group_a, group_b, paired=FALSE)$p.value
  }
  # if (pval){
  #   pval = paste0('p-value = ', pval)
  # }else{
  #   pval = ''
  # }
  
  sample_size <- pheno_table %>%
    group_by(!!!rlang::syms(colnames(pheno_table)[1])) %>%
    summarize(num = n())

  series <- as.numeric(unlist(select(pheno_table, feature)))
  iqr <- IQR(series)
  Q <- quantile(series, probs = c(.25, .75), na.rm = FALSE)
  condition <- pheno_table[feature] > (Q[1] - 1.5 * iqr) & pheno_table[feature] < (Q[2] + 1.5 * iqr)

  if (use_sd) {
    mean <- lapply(pheno_table[feature], mean, na.rm = TRUE)
    sd <- lapply(pheno_table[feature], sd, na.rm = TRUE)
    condition <- abs((pheno_table[feature] - mean)/sd) < 3
  }
  
  eliminated <- subset(pheno_table, condition)
  outliers <- subset(pheno_table, !condition)
  min_range <- min(eliminated[feature])
  max_range <- max(eliminated[feature])
  
  more_max <- c()
  less_min <- c()
  for (i in unlist(sample_size[by])){
    more_max <- c(more_max, sum(outliers[outliers[by] == i,feature]>max_range))
    less_min <- c(less_min, sum(outliers[outliers[by] == i,feature]<min_range))
  }
  sample_size$more <- more_max
  sample_size$less <- less_min
  sample_size
  text <- paste0("Included ", 
                 dim(eliminated)[1], 
                 " non-NA values in range [", 
                 min_range, 
                 ", ", 
                 max_range, 
                 "]\n", 
                 sum(outliers[feature] < min_range), 
                 " outliers less than ", 
                 min_range, 
                 "\n", 
                 sum(outliers[feature] > max_range), 
                 " outliers greater than ", 
                 max_range)
  myaxiss <- "myaxis"
  name <- paste0("images/", prefix, "boxplot_violin_", by, "_", feature, ".pdf")
  pdf(name)
  p <- eliminated %>%
    left_join(sample_size) %>%
    mutate(myaxis = paste0(!!!rlang::syms(colnames(eliminated)[1]), "\n", 
                           "n=", num, '\n',
                           less, " outliers less than ", as.character(min_range), '\n',
                           more,  " outliers greater than ", as.character(max_range))) %>%
    ggplot(aes_string(x = myaxiss, y = colnames(eliminated)[2], fill = colnames(eliminated)[1])) +
    geom_violin(width = 0.7, scale = "width") +
    geom_boxplot(width = 0.1, color = "white", alpha = 0.2) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.line = element_line(size = 1, colour = "black"),
      panel.grid.major = element_line(colour = "#e3e7e8"),
      panel.grid.minor = element_line(colour = "#e3e7e8"),
      panel.border = element_blank(), panel.background = element_blank(),
    ) +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(paste0("Distribution of ", colnames(eliminated)[2], " by ", colnames(eliminated)[1], ' ', pval)) +
    xlab(text) +
    scale_fill_manual(values = c("#ccebc5", "#8dd3c7", "#bebada"))
  p
  print(p)
  dev.off()
  # cat(dim(outliers)[1])
  # cat(paste0(" Done! Saved in ", name, ".\n"))
  cat(paste0(feature, "\t", pval ))
  return(p)
}


name <- "data/boxplots_analyses.tsv"
all_features_table <- read.table(name, header = T, sep = "\t")
all_features_table
bys <- c("death", "severity")
features <- c('CT_start', 'news', 'creat', 'ldh', 'crp', 'leuk', 'l', 'd_dym', 'neut', 'fer','glu', 'pcr', 'il6')
all_features_table[all_features_table$death == 1 & !is.na(all_features_table$death), ]$death <- "dead"
all_features_table[all_features_table$death == 0 & !is.na(all_features_table$death), ]$death <- "alive"
all_features_table[all_features_table$severity == 0 & !is.na(all_features_table$severity), ]$severity <- NA
for (by in bys) {
  for (feature in features) {
    draw_boxplot_violin_and_save(all_features_table, by, feature, use_sd=TRUE)
  }
}
head(all_features_table)
