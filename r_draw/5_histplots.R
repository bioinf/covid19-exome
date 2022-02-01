library(plyr)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)



draw_histogram_and_save_old <- function(all_table, score, feature, prefix = "", groups=2) {
  cat(paste0("Drawing for ", score, " and ", feature, "...\n"))
  all_table[feature] <- round(all_table[feature])

  cur_table <- all_table[c(feature, score)]
  cur_table <- na.omit(cur_table)
  # mu <- ddply(cur_table, score, summarise, grp.mean = mean(!!!rlang::syms(feature)))
  colors <- c("#ccebc5", "#8dd3c7", "#bebada")
  name <- paste0("images/", prefix, "histogram_", score, "__", feature, ".pdf")
  pdf(name)
  p <- ggplot(cur_table, aes_string(x = feature, fill = score, color = score)) +
    geom_bar(aes(y = ..count..), alpha = 0.79) +
    # geom_vline(data = mu, aes_string(xintercept = "grp.mean", color = score), linetype = "dashed") +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.line = element_line(size = 1, colour = "black"),
      panel.grid.major = element_line(colour = "#e3e7e8"),
      panel.grid.minor = element_line(colour = "#e3e7e8"),
      panel.border = element_blank(), panel.background = element_blank(),
    ) +
    facet_wrap(as.formula(paste("~", score)), nrow = groups, scales = "free_y", ) +
    theme(strip.background = element_blank(), strip.placement = "outside") +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors)
  print(p)
  dev.off()
  cat(paste0("Done! Saved in ", name, ".\n"))
}



draw_histogram_and_save <- function(all_table, score, feature, prefix = "") {
  cat(paste0("Drawing for ", score, " and ", feature, "...\n"))

  
  cur_table <- all_table[c(feature, score)]
  cur_table <- na.omit(cur_table)
  # mu <- ddply(cur_table, score, summarise, grp.mean = mean(!!!rlang::syms(feature)))
  colors <- c("#ccebc5", "#8dd3c7", "#bebada")
  colors <- colors[1:dim(unique(all_table[score]))[1]]
  cat(colors)
  name <- paste0("images/", prefix, "histogram_", score, "__", feature, ".pdf")
  pdf(name)
  p <- ggplot(cur_table, aes_string(x = feature, y=score, fill = score, color = score)) +
    geom_bar(aes(y = ..count..), alpha = 0.79, position="fill", color="black", size=0.25) +
    # geom_vline(data = mu, aes_string(xintercept = "grp.mean", color = score), linetype = "dashed") +
    # coord_flip() +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.line = element_line(size = 1, colour = "black"),
      panel.grid.major = element_line(colour = "#e3e7e8"),
      panel.grid.minor = element_line(colour = "#e3e7e8"),
      panel.border = element_blank(), panel.background = element_blank(),
    )+
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors)
  print(p)
  dev.off()
  cat(paste0("Done! Saved in ", name, ".\n"))
}





name <- "data/features_for_hist.tsv"
all_table <- read.table(name, header = T, sep = "\t")

all_table[all_table$severity == 0 & !is.na(all_table$severity), ]$severity <- NA
score <- "top_10_score"

pdf("images/score_hist.pdf")
p<-hist(all_table$SCORE, col="#bebada", breaks=20)
p
print(p)
dev.off()



all_table
# all_table[all_table[score] == 1, ][score] <- "top-10"
# all_table[all_table[score] == 0, ][score] <- "bottom-90"

features <- c("death", "severity", "storm")



for (feature in features) {
  all_table[feature] <- round(all_table[feature])
  all_table[feature] <- lapply(all_table[feature], as.character)
  draw_histogram_and_save(all_table, feature, score)
}


name <- "data/boxplots_analyses.tsv"
all_features_table <- read.table(name, header = T, sep = "\t")

bys <- c("death", "severity")
features <- c("CT_start", "news")
all_features_table[all_features_table$death == 1 & !is.na(all_features_table$death), ]$death <- "dead"
all_features_table[all_features_table$death == 0 & !is.na(all_features_table$death), ]$death <- "alive"
all_features_table[all_features_table$severity == 0 & !is.na(all_features_table$severity), ]$severity <- NA
all_features_table$severity <- as.character(all_features_table$severity)

for (by in bys) {
  for (feature in features) {
    groups <- 2
    if (by =='severity'){
      groups <- 3
    }
    draw_histogram_and_save_old(all_features_table, by, feature, prefix = "", groups = groups)
  }
}

for (feature in features) {
  cur_table <- all_features_table[c(feature, 'death')]
  cur_table <- na.omit(cur_table)
  alive = cur_table[cur_table$death=='alive',feature]
  dead = cur_table[cur_table$death=='dead',feature]
  group_a <- as.data.frame(table(alive))$Freq
  group_b <- as.data.frame(table(dead))$Freq
  pval<-chisq.test(rbind(group_a, group_b))$p.value
  cat(paste0(feature, '\t', pval, '\n'))
}

feature <- 'CT_start'

cur_table <- all_features_table[c(feature, 'death')]
cur_table <- na.omit(cur_table)
alive = cur_table[cur_table$death=='alive',feature]
dead = cur_table[cur_table$death=='dead',feature]
group_a <- as.data.frame(table(alive))$Freq
group_b <- as.data.frame(table(dead))$Freq


