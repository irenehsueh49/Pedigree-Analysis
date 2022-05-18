---
title: "Irene Hsueh's BS 858 Homework 1"
author: "Irene Hsueh"
date: "9/21/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(naniar)
```

# Analyzing Family Files
```{r}
#Reading in CSV File
genotypes <- read.csv("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 858 - Statistical Genetics I/Class 1 - Pedigree Analysis/Homework 1/homework1_data.csv") %>% 
#Reordering and Renaming Variables
  dplyr::select(family_id = famid, 
                id, sex, 
                father_id = fa, 
                mother_id = mo, 
                disease = aff,
                rs_allele1 = rs12075_Al1,
                rs_allele2 = rs12075_Al2) %>% 
#Converting Missing Data to NA
  naniar::replace_with_na_all(condition= ~.x %in% c(0, ".")) %>%
#Recoding Variables
  dplyr::mutate(disease = recode_factor(disease, "1"="Unaffected", "2"="Affected"),
                sex = recode_factor(sex, "1"="Male", "2"="Female"))
head(genotypes, 20)
```

```{r}
n_individuals                       <- nrow(genotypes)
n_families                          <- length(table(genotypes$family_id))
n_children                          <- sum(!is.na(genotypes$mother_id))
n_parents                           <- sum(is.na(genotypes$mother_id))
n_mothers                           <- sum(is.na(genotypes$mother_id) & genotypes$sex == "Female")

n_phenotypes                        <- sum(!is.na(genotypes$disease))
n_no_phenotypes                     <- sum(is.na(genotypes$disease))
proportion_no_phenotypes            <- n_no_phenotypes / n_individuals * 100

n_genotypes                         <- sum(!is.na(genotypes$rs_allele1) & !is.na(genotypes$rs_allele2))
n_no_genotypes                      <- sum(is.na(genotypes$rs_allele1) & is.na(genotypes$rs_allele2))

proportion_genotypes                <- n_genotypes / n_individuals * 100

n_parents_no_genotypes              <- sum(is.na(genotypes$mother_id) & is.na(genotypes$rs_allele1))
proportion_parents_nogenotypes      <- n_parents_no_genotypes / n_parents * 100

n_mothers_no_genotypes              <- sum(is.na(genotypes$mother_id) & 
                                           genotypes$sex == "Female" & 
                                           is.na(genotypes$rs_allele1))
proportion_mothers_nogenotypes      <- n_mothers_no_genotypes / n_mothers * 100

n_children_no_genotypes             <- sum(!is.na(genotypes$mother_id) & is.na(genotypes$rs_allele1))
proprotion_children_nogenotypes     <- n_children_no_genotypes / n_children * 100

n_phenotypes_genotypes              <- sum(!is.na(genotypes$disease) & !is.na(genotypes$rs_allele1))

mean_children_per_family            <- mean(tapply(!is.na(genotypes$mother_id), 
                                                   genotypes$family_id, sum))

mean_children_phenotypes_genotypes  <- mean(tapply(!is.na(genotypes$mother_id) & 
                                                   !is.na(genotypes$disease) &
                                                   !is.na(genotypes$rs_allele1), 
                                                   genotypes$family_id, sum))

families_children_affected          <- tapply(!is.na(genotypes$mother_id) & 
                                              genotypes$disease == "Affected", 
                                              genotypes$family_id, sum)
table(families_children_affected)
sum(table(families_children_affected))

n_parents_affected                  <- sum(is.na(genotypes$mother_id) & 
                                          genotypes$disease=="Affected", na.rm=TRUE)
n_parents_phenotypes                <- sum(is.na(genotypes$mother_id) & !is.na(genotypes$disease))
proportion_parents_affected         <- n_parents_affected / n_parents_phenotypes * 100
```



```{r}
#Alphabeticaly Ordering Genotypes
rs_genotype <- ifelse(genotypes$rs_allele1 < genotypes$rs_allele2, 
                      paste(genotypes$rs_allele1, genotypes$rs_allele2, sep="/"),
                      paste(genotypes$rs_allele2, genotypes$rs_allele1, sep="/"))
table(rs_genotype)
table(rs_genotype)/sum(table(rs_genotype))*100


#Allele Counts
rs_alleles <- c(genotypes$rs_allele1, genotypes$rs_allele2)
table(rs_alleles)
table(rs_alleles)/sum(table(rs_alleles))*100
```


