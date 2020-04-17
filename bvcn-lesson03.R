# Import data
gene_data <- read.table("test-data-skoog.txt")
head(gene_data)
# Data are representative of taxa as rows and columns showing genes (values equal copies of those genes for each taxa). These 4 genes are required for the full pathway. We will show some examples of how to subset, filter, and perform basic calculations.
 

# Data frame versus matrix in R
## Data frames can include different types of data
class(gene_data)
head(gene_data)
str(gene_data) # Factors and numerics included in this data


# So matrices...
# When do you use one over the other? It depends on what kind of data you're working with. If you're working with different types of information in the same table, you likely need a data frame. But if you're working with a single data type, a matrix may be better for you. Additionally, matrices are more efficient with respect to memory. Therefore, a lot of statistical tools/methods require a matrix as input. We will review this.
#
# Colnames and rownames:
colnames(gene_data) # reports all of my column names! 

# Let's change some of these because there is a typo
colnames(gene_data)[5] <- "GENE_D" #Changes the 5th column name to "GENE_D"
colnames(gene_data)

# How about rows - these are just random numbers! R assigns increasing numbers automatically as row names, unless you specify. 
row.names(gene_data) # So when would you need to specify row.names???
# Recall the data I'm looking at are integers and a factor column, so a data frame. BUT if I need it to be in a matrix, I can modify this.
class(gene_data)
str(gene_data)

# Let's work on modifying data frames
library(reshape2)
library(tidyverse)

# Re-import
gene_data <- read.csv("test-data-skoog.csv")
colnames(gene_data)[5] <- "GENE_D" # we did this above as well

# Example of how to change the order
colnames(gene_data)
gene_data_reorder <- gene_data[, c("Taxon", "GENE_A", "GENE_B", "GENE_C", "GENE_D")]

tmp <- gene_data[c(1,2,4,3,5)] #Combine specific columns "c()"

# Use wide-format data and subset to find taxa with all genes required in pathway
# Subset taxa so they have both Gene C and D, but have either Gene A or B. This demonstrates that the taxa are capable of the pathway.

# Let's try this one at a time:
tmp <- subset(gene_data, (GENE_C > 0 & GENE_D > 0))

tmp2 <- subset(gene_data, !(GENE_A == 0 ))

# What about subsetting dataframe so all taxa have all genes in pathway? How would we do this?



# This data is in wide format. Convert to long format - this is more versatile.
## Compare long vs. wide format
gene_data_long <- melt(gene_data_reorder)
head(gene_data_long)
# View(gene_data)
# View(gene_data_long)


# Split Taxon column
gene_data_long <- separate(gene_data_long, Taxon, c("phylum", "class", "order", "family", "genus", "species"), sep = ";", remove=FALSE)
head(gene_data_long)


# Mean for Class
mean_class <- gene_data_long %>%
  group_by(class, variable) %>%
  summarise(MEAN = mean(value)) %>%
  data.frame
head(mean_class)

# # Relative abundance of genes by Taxon
# head(gene_data_long)
# colnames(gene_data_long)
# #
# relabun_all <- gene_data_long %>%
#   group_by(order, variable) %>%
#   summarise(RELABUN = value/sum(value)) %>%
#   data.frame
# head(relabun_all)

# Add column to categorize data
## Let's simulate replicates that we need to average across
head(gene_data_long)
#
# Add in a simulated column that represents replicates
gene_data_long$REPLICATE <- sample(c("rep-1", "rep-2"), replace = TRUE)
head(gene_data_long)
#
# Average across replicates




#
# Review from last week - explaination:
# iris[order(iris$Species, -iris$Petal.Length),]
#
# With "[ ]" we use a comma to designate if rows or columns are being considered within the brackets. Without a comma, it defaults to considering columns.
# By default it is: [rows, columns]
#
# Let's show an example using "head()"
head(iris) #Prints the top section of your data frame (6 lines total)
# Let's play around with adding brackets
head(iris[1,]) # prints the 1st row of the data frame
head(iris[1:3,]) #prints the 1st 3 rows of the data frame
## ^That's what the colon is for! it selects colums 1 through 3 (inclusive)
## If we move the comma
head(iris[,1:3]) #Now we print the 1st 3 columns only
head(iris[1:3]) # This also prints the 1st 3 columns - remember that R defaults to columns when no comma is present.
#
# Print the 1st 3 rows of the first 2 columns
head(iris[1:3,1:2])
#
head(iris)
# Let's break this function down:
# "order(iris$Species, -iris$Petal.Length)" will order the iris data frame by Species first (ascending) and THEN by Petal.length (descending because of the - sign). This is followed by a comma because we are ordering the rows.
iris[order(iris$Species, -iris$Petal.Length), ]
#
# Let's try adding something! 
iris[order(iris$Species, -iris$Petal.Length), 1:3]