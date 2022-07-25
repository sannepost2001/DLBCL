#https://cran.rstudio.com/web/packages/FField/index.html
#Archived on 2022-06-22 as check problems were not corrected despite reminders. 
#Tools ==> install package FField_0.1.0.tar.gz

# Install and load GenVisR
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager",force = TRUE)

library(BiocManager)
library(GenVisR)

library(stringr)
library(dplyr)
# Load relevant data from the manuscript
mutationData <- read.delim("/home/sanne/bioinf/afstuderen/DATA/RUMC/onco_test.csv", sep = ",")
# clinicalData <- read.delim("http://genomedata.org/gen-viz-workshop/GenVisR/BKM120_Clinical.tsv")
# mutationBurden <- read.delim("http://genomedata.org/gen-viz-workshop/GenVisR/BKM120_MutationBurden.tsv")

# Reformat the mutation data for waterfall()
# mutationData <- mutationData[,c("patient", "gene.name", "trv.type", "amino.acid.change")]
colnames(mutationData) <- c("sample", "gene", "variant_class", "amino.acid.change")
mutationDataDiag <-mutationData[grepl('diag', mutationData$sample), ]
mutationDataR1 <-mutationData[grepl('r1', mutationData$sample), ]

# mutationData %>% filter(str_detect(sample, "diag"))
# mutationData <- mutationData[mutationData$sample %like% "diag", ]
# Create a vector to save mutation priority order for plotting
mutation_priorityDiag <- as.character(unique(mutationDataDiag$variant_class))
mutation_priorityR1 <- as.character(unique(mutationDataR1$variant_class))

# Create an initial plot
waterfall(mutationDataDiag, fileType = "Custom", variant_class_order=mutation_priorityDiag)
waterfall(mutationDataR1, fileType = "Custom", variant_class_order=mutation_priorityR1)


