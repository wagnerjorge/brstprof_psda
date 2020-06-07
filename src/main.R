library(psda)
library(KRLS)
library(tidyverse)


source('G:\\Dropbox_Novo\\Dropbox\\2. Doutorado\\4. Artigos\\Software Article\\saeb_2017\\brstprof_psda\\src\\comparisons.R', echo=TRUE)

setwd('G:\\Dropbox_Novo\\Dropbox\\2. Doutorado\\4. Artigos\\Software Article\\saeb_2017\\brstprof_psda\\data')
center <- read.table('saeb2017_center.csv', header = TRUE, sep = ',')
radius <- read.table('saeb2017_radius.csv', header = TRUE, sep = ',')


phi_crm <- monte_carlo(center, radius, size = 100, model = 'interval', model_int = 'crm')
phi_crm <- monte_carlo(center, radius, size = 100, model = 'interval', model_int = 'ietkrr')
phi_plr <- monte_carlo(center, radius, size = 100, model = 'plr')
