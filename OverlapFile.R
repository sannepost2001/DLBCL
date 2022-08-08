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

# Install and load GenVisR
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager",force = TRUE)

library(BiocManager)
#library(GenVisR)

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
EXCEL_locationCase <-"//home//sanne/bioinf//afstuderen//ANALYSIS//DymNewData//10perc//cases//Excel//"

## make dataframe
genenamesDF <- data.frame(matrix(ncol = 1, nrow = 0))
# read in all excel files on that location
ExcelBestanden <-list.files(EXCEL_locationCase, pattern = "^case*", ignore.case = TRUE)
# for every file take out genes of the group and make the overview
for (Bestand in 1:length(ExcelBestanden)) {
  mysheets <- read_excel_allsheets(gsub(" ","",paste(EXCEL_locationCase,ExcelBestanden[Bestand])))
  for (i in 1:length(mysheets)) {
    if (str_detect(names(mysheets[i]), "RECIDIEF_specifiek") ||
        str_detect(names(mysheets[i]), "RECIDIEF_verrijkt") ||
        str_detect(names(mysheets[i]), "DIAGNOSE_verrijkt") ) {
        print (paste("Verwerken van bestand [",ExcelBestanden[Bestand],"]Sheet [", names(mysheets[i]) , "]"))
        case <- GETDF_FROMLIST(mysheets, i)
        genenames <- case %>% select(`Gene.name`)
        
        CaseFileName<-gsub(".xlsx","",ExcelBestanden[Bestand])
        genename <- unique(tibble(genenames,CaseFileName))
        genenamesDF <- rbind(genename, genenamesDF)
      #  print (genenamesDF)
    }
  }
}
# take all the unique genes
genenamesDFUnique<-unique(genenamesDF)
library(rpivotTable)
data(genenamesDFUnique)
# make pivot table
rpivotTable(data = genenamesDFUnique, rows = "Gene.name",cols="CaseFileName", vals = "Freq", aggregatorName = "Count",  rendererName = "Table", width="100%", height="400px")
