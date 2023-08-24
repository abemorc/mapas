
# Road kills
# Instalacion/carga librerias  --------------------------------------------


# Instalacion librerias faltantes -----------------------------------------

oldw <- getOption("warn")
options(warn = -1)

packages <- c("readxl", "tidyverse", "lubridate", "tidytable", "psych",
              "here", "visreg", "Hmisc", "broom", "MuMIn",
              "PerformanceAnalytics", "modeest", "caret")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

options(warn = oldw)


# carga de librerias usadas en proyecto road kills -------------------------

###
library(here)
library(readxl)
library(tidyverse)
library(lubridate)
library(modeest)
library(PerformanceAnalytics)
library(psych)
library(caret)
library(MuMIn)
library(visreg)
library(tidytable)
library(broom)
library(Hmisc)


load(here("24.11.RData"))


