library(reshape2)
library(ggplot2)


name <- "data/for_big_plot.tsv"
all_features_table <- read.table(name, header = T, sep = "\t")
all_features_table$feature <- factor(all_features_table$feature, levels = c('creat before IRNT', 'creat after IRNT', 'creat_1 before IRNT', 'creat_1 after IRNT', 'creat_dynam before IRNT', 'creat_dynam after IRNT', 'creat_max_min before IRNT', 'creat_max_min after IRNT', 'crp before IRNT', 'crp after IRNT', 'crp_1 before IRNT', 'crp_1 after IRNT', 'crp_dynam before IRNT', 'crp_dynam after IRNT', 'crp_max_min before IRNT', 'crp_max_min after IRNT', 'days_at_hosp before IRNT', 'days_at_hosp after IRNT', 'days_to_hosp before IRNT', 'days_to_hosp after IRNT', 'l before IRNT', 'l after IRNT', 'l_1 before IRNT', 'l_1 after IRNT', 'l_3 before IRNT', 'l_3 after IRNT', 'l_dynam before IRNT', 'l_dynam after IRNT', 'l_max_min before IRNT', 'l_max_min after IRNT', 'leuk before IRNT', 'leuk after IRNT', 'leuk_1 before IRNT', 'leuk_1 after IRNT', 'leuk_3 before IRNT', 'leuk_3 after IRNT', 'leuk_7 before IRNT', 'leuk_7 after IRNT', 'leuk_dynam before IRNT', 'leuk_dynam after IRNT', 'leuk_max_min before IRNT', 'leuk_max_min after IRNT', 'neut before IRNT', 'neut after IRNT', 'neut_1 before IRNT', 'neut_1 after IRNT', 'neut_3 before IRNT', 'neut_3 after IRNT', 'neut_dynam before IRNT', 'neut_dynam after IRNT', 'neut_max_min before IRNT', 'neut_max_min after IRNT', 'pc1 before IRNT', 'pc1 after IRNT', 'pc2 before IRNT', 'pc2 after IRNT'))
cur_table
pdf("./images/IRNT_comparison.pdf")
p <- ggplot(all_features_table,aes(x = value)) + 
  facet_wrap(~feature,scales = "free") + 
  geom_histogram()+
  theme_classic() + 
  theme( axis.text = element_text( size = 4 ),
         axis.title = element_text( size = 10 ),
         # The new stuff
         strip.text = element_text(size = 3.2))

print(p)
dev.off()
