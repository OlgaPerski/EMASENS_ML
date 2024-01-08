# load required packages

if (!require("pacman", quietly = TRUE)) install.packages("pacman")

pkgs <- c("caret",
          "conflicted",
          "corrplot",
          "cowplot",
          "dlm",
          "flextable",
          "forcats",
          "fuzzyjoin",
          "ggplot2",
          "ggridges",
          "glmnet",
          "glue",
          "Gmisc",
          "grid",
          "gtsummary",
          "here",
          "htmlTable",
          "imputeTS",
          "janitor",
          "jsonlite",
          "kernlab",
          "lubridate",
          "magrittr",
          "mgcv",
          "modeldata",
          "officer",
          "parsnip",
          "pROC",
          "ranger",
          "readr",
          "readxl",
          "ROCR",
          "rsample",
          "shapr",
          "stringr",
          "themis",
          "tidymodels",
          "tidyverse",
          "vip",
          "xgboost")

pacman::p_load(pkgs, character.only=T)

if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
# if IRanges is not already installed, comment out below
# BiocManager::install("IRanges")

library("IRanges")

conflict_prefer("rename", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("%within%", "lubridate")
conflict_prefer("lag", "dplyr")
conflict_prefer("unname", "base")
conflict_prefer("spec", "yardstick")
conflict_prefer("populate", "rsample")
