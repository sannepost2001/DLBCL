#https://cran.rstudio.com/web/packages/FField/index.html
#Archived on 2022-06-22 as check problems were not corrected despite reminders. 
#Tools ==> install package FField_0.1.0.tar.gz
library("readxl")
library(tidyr)
library(dplyr)
library(arsenal)
library(knitr)
library(VennDiagram)
library(stringr)
library("writexl")
knitr::opts_chunk$set(echo = TRUE)
library(xlsx)
library(ggplot2)
library(gridExtra)
# Install and load GenVisR
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager",force = TRUE)

library(BiocManager)
library(GenVisR)

library(stringr)
library(dplyr)

## read excel
## reused from: https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames/12945838#12945838
library(readxl)
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <-
    lapply(sheets, function(X)
      readxl::read_excel(filename, sheet = X))
  if (!tibble)
    x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
## reused from: https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames/24352576#24352576

GETDF_FROMLIST <- function(DF_LIST, ITEM_LOC) {
  DF_SELECTED <- DF_LIST[[ITEM_LOC]]
  DF_SELECTED <- tibble::as_tibble(DF_SELECTED)
  return(DF_SELECTED)
}

## locations
## Give the location of the .xlsx file you want to use
EXCEL_location <- "//home//sanne//bioinf//afstuderen//DATA//RUMC//20062022_incll63_allmutdiagr1_together_3germlinefiltersapplied_voorsanne.xlsx"
## Give the output location where the output files need to go. 
## Remember the // at the end
## Use the mysheets function to read the .xlsx file in as a large list
mysheets <- read_excel_allsheets(EXCEL_location)
# get genes for the oncoplot
get_genes <- function(chars){
  chars <- chars
  ALL_genes = data.frame()
  # go over all the tabs in the excel file
  for (i in 1:length(mysheets)) {
    case_genes = data.frame()
    # check the tabs with the chars asked for in the function
    if (str_detect(names(mysheets[i]), chars)) {
      print (paste("Verwerken van Sheet [", names(mysheets[i]) , "]"))
      case_name <- names(mysheets[i])
      # put case/tab data in a dataframe
      case <- GETDF_FROMLIST(mysheets, i)
      for (i in 1:nrow(case)) {
        gene_row = data_frame()
        row <- case[i,]
        # print(row)
        # genes in the oncoplot
        genes <- c("B2M","BTG1", "BTG2", "CD58", "CD79B", 
                    "DDX3X", "DTX1", "H1-4","H1-5", "IRF2BP2", "KMT2D", "LRP1B", 
                    "MPEG1", "MYC", "MYD88", "NFKB2", "PIM1", 
                    "TP53", "ZC3H12A")
        gene_name <- row$`Gene name`
        # get collumns needed for the oncoplot
        if (gene_name %in% genes){
          # print(case_name)
          row_info <- row %>%  select(`Gene name`, `Protein Effect` , Hgvsg)
          # print(row_info)
          gene_row <-  rbind(gene_row, row_info)
          gene_row$Case.Name <- case_name
          # print(genes_row)
        }
        case_genes <-  rbind(case_genes, gene_row)
        # print(case_genes)
      }
    }
    else{
      # add genes that relapsed but fell trough the filter steps
      # Diagnosis tabs with the mutations that have relapsed but are only in the diagnoses tabs and the other way around
      # with the criteria: samples VAF between 5-90% (0.05-0.9 AF_tumor), variant coverage >=20 reads (>=20 DP_tumor) and F1R2_alt_tumor >=4 and F2R1_alt_tumor>=4
      if(chars == "diag"){
        case_name <- names(mysheets[i])
        header <- GETDF_FROMLIST(mysheets, i)
        headernamen <- colnames(header)
        Lastdpheaderzoek="Last"
        Lastafheaderzoek="Last"
        LastF2R1headerzoek="Last"
        
        for (h in length(headernamen):1) {
          if (str_starts(headernamen[h],"DP_")> 20 && Lastheaderzoek=="Last" &&
              str_starts(headernamen[h],"AF_") > 0.90  && str_starts(headernamen[h],"AF_") > 0.05
              && Lastafheaderzoek=="Last" &&
              str_starts(headernamen[h],"F1R2_alt_") >= 4 && LastF2R1headerzoek=="Last"){
            Lastheaderzoek <- headernamen[h]
            genes <- c("B2M","BTG1", "BTG2", "CD58", "CD79B", 
                       "DDX3X", "DTX1", "H1-4","H1-5", "IRF2BP2", "KMT2D", "LRP1B", 
                       "MPEG1", "MYC", "MYD88", "NFKB2", "PIM1", 
                       "TP53", "ZC3H12A")
            gene_name <- row$`Gene name`
            if (gene_name %in% genes){
              # print(case_name)
              row_info <- row %>%  select(`Gene name`, `Protein Effect` , Hgvsg)
              # print(row_info)
              gene_row <-  rbind(gene_row, row_info)
              gene_row$Case.Name <- case_name
              # print(genes_row)
            }
            case_genes <-  rbind(case_genes, gene_row)
          }
          else if(chars == "rel"){
            case_name <- names(mysheets[i])
            header <- GETDF_FROMLIST(mysheets, i)
            headernamen <- colnames(header)
            Firstdpheaderzoek="Start"
            Firstafheaderzoek="Start"
            FirstF2R1headerzoek="Start"
            for (h in 1:length(headernamen)) {
              if (str_starts(headernamen[h],"DP_")> 20 && Firstdpheaderzoek=="Start"  &&
                  str_starts(headernamen[h],"AF_") > 0.90  && str_starts(headernamen[h],"AF_") > 0.05
                  && Firstafheaderzoek=="Start" &&
                  str_starts(headernamen[h],"F1R2_alt_") >= 4 && FirstF2R1headerzoek=="Start" ){
                Firstheaderzoek <- headernamen[h]
                genes <- c("2NF888", "B2M","BTG1", "BTG2", "CD58", "CD79B", "CRYBG2",
                           "DDX3X", "DTX1", "H1-4","H1-5", "IRF2BP2", "KMT2D", "LRP1B",
                           "MPEG1", "MUC12", "MUC4", "MYC", "MYD88", "NFKB2", "PIM1",
                           "PTX1", "SORCS3", "SPEN", "TEXBD", "TP53", "ZC3H12A")
                gene_name <- row$`Gene name`
                if (gene_name %in% genes){
                  # print(case_name)
                  row_info <- row %>%  select(`Gene name`, `Protein Effect` , Hgvsg)
                  # print(row_info)
                  gene_row <-  rbind(gene_row, row_info)
                  gene_row$Case.Name <- case_name
                  # print(genes_row)
                }
                case_genes <-  rbind(case_genes, gene_row)
              }
            }
          }
        }
      }
      ALL_genes <-  rbind(ALL_genes, case_genes)
    }
    colnames(ALL_genes) <- c("gene", "variant_class", "amino.acid.change", "sample")
    return(ALL_genes)
  }
}
# call on the function
mutationDataDiag <- get_genes("diag")
mutationDataR1 <- get_genes("rel")

# Create a vector to save mutation priority order for plotting
mutation_priorityDiag <- as.character(unique(mutationDataDiag$variant_class))
mutation_priorityR1 <- as.character(unique(mutationDataR1$variant_class))

# Create an initial plot
plotDiag <- waterfall(mutationDataDiag, fileType = "Custom", variant_class_order=mutation_priorityDiag, mainXlabel = TRUE )
plotR1 <- waterfall(mutationDataR1, fileType = "Custom", variant_class_order=mutation_priorityR1, mainXlabel = TRUE)


