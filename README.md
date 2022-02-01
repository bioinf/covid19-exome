# COVID-19 exome project

## Directory `feature_processing`

Preparation of datasets, exploratory data analysis, filtering, normalization of data.

- `feature_mapping.py`, `extra_feature_mapping.py` - name mappings of features (from Russian to English short abbreviations).
- `part1_ids_preprocessing.ipynb` - preprocessing of VCF and phenotypes ids.
- `part2_phenotype_preprocessing.ipynb` - cleaning and preparation of phenotypes, renaming of cols, etc.
- `part2.5_new_table_analysis.ipynb` - the same, but with extra-features.
- `part3_march_pheno_eda_and_normalysing.ipynb` - EDA, filtering, normalization of data.

## Directory `cvas_and_rvas`

Common variants association study (CVAS) and Rare variants association study (RVAS).
- `sd3_gwas_com.ipynb` - CVAS.
- `final_rwas_pipeline_p1_hail_prepare.ipynb` - first step of RVAS (preparation of tables). 
- `final_rwas_pipeline_p2_statistics_and_plots.ipynb` - second step of RVAS (tests and plots).
- `out_hail_gwas_com_sd3/` - directpry with p values of cvas.
- `out_hail_rvas/` - directory with p values of rvas.
- `rvas_dataset/` - directory with table for the 2nd part of rvas  


## Directory `risk_score_and_regression`

Validation of found SNPs: its annotation and statistical checks.

- `Variants_annotations_and_score_calculation.ipynb` - the main script with all annotations, statistics counts, etc (`columns_to_check.py` need for this notebook).
- `draw_data/` - directory with datasets for R plots.
- `other_data/` - directory with outputs of this script, 
which are not needed for drawing pictures.



## Directory `r_draw`
R code for drawing figures, and figures itself.
- `1_MH_QQ.R` - draw Manhattan and QQ plots (files: `Rectangular-Manhattan.*t*.pdf` and `QQplot*.pdf` respectively). 
- `2_PCA.R` - draw plots for principal components from EDA (for sex and death). Images: `images/pca_*.pdf`.
- `3_violin_regression.R` - draw violin plots and regression on SNPS and associated features (from `data/regression/regr_rs*_*.tsv`). Images: `images/regr_<rs>_<feature>.pdf`.
- `4_boxplots.R` - draw boxplots for features by death and severity (from `data/boxplots_analyses.tsv`). Images: `images/bozplot_violin_<death/severity>_<feature>.pdf`.
- `5_histplots.R` - draw histplots (from: `data/features_for_hist.tsv` and `data/boxplots_analyses.tsv`). Imaged data:
    - `images/histogram_<feature>___top_10_score.pdf` - histograms for score by severity/death/storm;
    - `images/score_hist.pdf` - histogram pf the snps' score;  
    - `images/histogram_<death/severity>_<feature>.pdf` - histograms of features by death/severity.
- `images/` - drawn figures.
