# Import data
gene_data <- read.csv("test-data-skoog.csv")
head(gene_data)
# Data are representative of taxa as rows and columns showing genes (values equal copies of those genes for each taxa). These 4 genes are required for the full pathway. We will show some examples of how to subset, filter, and perform basic calculations.
# 
#
library(reshape2)
#
# This data is in wide format. Convert to long format
## Compare long vs. wide format
gene_data_long <- melt(gene_data)
head(gene_data_long)
#
#
# Use wide-format data and subset to find taxa with all genes required in pathway
# Subset taxa so they have both Gene C and D, but have either Gene A or B. This demonstrates that the taxa are capable of the pathway.
# Let's try this one at a time:
tmp <- subset(gene_data, (GENE_C > 0 & GENE_D > 0))
tmp2 <- subset(gene_data, !(GENE_A == 0 ))