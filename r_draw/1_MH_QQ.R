library("ggplot2")
# install.packages("CMplot")
library("CMplot")

get_pos <- function(x) strtoi(sapply(strsplit(x, ":"), `[`, 2), base = 0L)
get_chr <- function(x) sapply(strsplit(as.character(x), ":"), function(elt) substr(elt[[1]], 4, 1000))

snp2gene <- list(
  "chr10:71799129" = "CDH23",
  "chr10:71799195" = "CDH23",
  "chr16:88738516" = "PIEZO1",
  "chr19:50259161" = "MYH14",
  "chr19:50263450" = "MYH14",
  "chr2:219280564" = "DNAJB2",
  "chr22:20992196" = "LZTR1",
  "chr3:38894643" = "SCN11A",
  "chr3:68997990" = "EOGT",
  "chr6:16306520" = "ATXN1",
  "chr6:51830849" = "PKHD1",
  "chr9:132278286" = "SETX",
  "chr9:98299383" = "GABBR2"
)

prepare_snp <- function(table_name) {
  pheno_table <- read.table(paste0(table_name, ".tsv"), header = T, sep = "\t")

  pheno_table$chr <- get_chr(pheno_table$locus)
  pheno_table$chr[pheno_table$chr == "X"] <- "23"
  pheno_table$chr[pheno_table$chr == "Y"] <- "24"
  pheno_table$position <- get_pos(pheno_table$locus)

  pheno_table <- subset(pheno_table, select = -c(rsid))
  pheno_table <- na.omit(pheno_table)
  pheno_table <- pheno_table[order(pheno_table$chr), ]

  snp <- pheno_table$locus
  chr <- pheno_table$chr
  pos <- pheno_table$position
  pval <- pheno_table$p_value
  final_table <- data.frame(snp, chr, pos, pval)
  return(final_table)
}

draw_mhplot_qq <- function(table, name, format, color, SNPs, genes, max_pval = 8) {
  CMplot(table,
    plot.type = "m", col = c("grey40", "grey70"), # chr colors
    highlight = SNPs,
    highlight.text = genes,
    highlight.col = c("#fb8072"),
    highlight.cex = 1, highlight.pch = c(16),
    LOG10 = TRUE, ylim = c(0, max_pval), # limits of log pval
    threshold = c(0.05 / nrow(table), 1e-4), # cut-offs of pval
    threshold.lty = c(1, 2), threshold.lwd = c(1, 1),
    threshold.col = c("black", "grey"), # threshold colors
    amplify = TRUE, chr.den.col = NULL,
    signal.col = c("#fb8072", "#b3de69"), # colors of significant
    signal.cex = c(1.5, 1.5), signal.pch = c(19, 19),
    file = format, # file format
    memo = name, # file postfix
    dpi = 500, file.output = TRUE, verbose = TRUE,
    width = 14, height = 5
  )
  CMplot(table,
    plot.type = "q", col = color, box = FALSE, file = format, memo = name, dpi = 500,
    conf.int = TRUE, conf.int.col = NULL, threshold.col = "red", threshold.lty = 2,
    file.output = TRUE, verbose = TRUE,
    width = 5, height = 3.5
  )
}

phenos <- c("l", "CT_start", "leuk_1", "neut_1", "crp")
# colors <- c('#8dd3c7', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69', '#fccde5', '#d9d9d9', '#bc80bd', '#ccebc5', '#ffed6f')
colors <- c("#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f")


cutoff <- 4
full_snps <- list()

max_pval <- 0
for (i in 1:length(phenos)) {
  pheno_name <- phenos[i]
  table <- paste0("withoutImputing_out_hail_gwas_com/gwas_", pheno_name)
  table <- read.table(paste0(table, ".tsv"), header = T, sep = "\t")
  max_pval <- max(max_pval, max(-log10(table$p_value)))
}
max_pval <- ceiling(max_pval)
for (i in 1:length(phenos)) {
  pheno_name <- phenos[i]
  color <- colors[i]
  cat(paste0('\nDrawing for "', pheno_name, '"...\n\n'))
  table <- paste0("withoutImputing_out_hail_gwas_com/gwas_", pheno_name)
  table <- prepare_snp(table)
  SNPs <- table[-log10(table$pval) > cutoff, 1]
  genes <- list()
  for (snp in SNPs) {
    genes <- append(genes, snp2gene[snp])
  }
  full_snps <- append(full_snps, SNPs)
  draw_mhplot_qq(table, pheno_name, "pdf", color, SNPs, unlist(genes), max_pval)
  cat(paste0('\nDone with "', pheno_name, '"!\n==========================\n'))
}

unique_snps <- sort(unique(unlist(full_snps)))
for (i in unique_snps) {
  cat(paste0('"', i, '" = ,\n'))
}
