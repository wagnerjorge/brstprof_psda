library(psda)
library(ggplot2)
library(data.table)

setwd('G:/Dropbox_Novo/Dropbox/2. Doutorado/4. Artigos/Software Article/saeb_2017')
center <- read.table('saeb2017_center.csv', header = TRUE, sep = ',')
radius <- read.table('saeb2017_radius.csv', header = TRUE, sep = ',')

head(round(center, 1))
head(round(radius, 1))

set.seed(1)
samp <- sample(1 : nrow(center), .7 * nrow(center), replace = F)

center_train <- center[samp, -1]
radius_train <- radius[samp, -1]

center_test <- center[-samp, -1]
radius_test <- radius[-samp, -1]
n_test <- nrow(center_test)
id <- center$county
id_test <- id[-samp]


dta_train <- list()
dta_train[[1]] <- center_train
dta_train[[2]] <- radius_train
names(dta_train) <- c('center', 'radius')
class(dta_train) <- 'paggregated'

dta_test <- list()
dta_test[[1]] <- center_test
dta_test[[2]] <- radius_test
names(dta_test) <- c('center', 'radius')
class(dta_test) <- 'paggregated'

v <- 20 
pol_dta_train <- psymbolic(dta_train, v)
pol_dta_test <- psymbolic(dta_test, v)


head(pol_dta_train$proficiency_mt, 2)

pmean(pol_dta_train$proficiency_mt)
pmean(pol_dta_train$proficiency_lp)
pmean(pol_dta_train$employees)


sqrt(pvar(pol_dta_train$proficiency_mt))
sqrt(pvar(pol_dta_train$proficiency_lp))
sqrt(pvar(pol_dta_train$employees))

pcorr(pol_dta_train$proficiency_mt)
pcorr(pol_dta_train$proficiency_lp)
pcorr(pol_dta_train$employees)


pplot(pol_dta_train$proficiency_mt) + 
labs(x = 'Dimension 1', y = 'Dimension 2') + theme_bw()


fit <- plr(proficiency_mt ~ proficiency_lp+ computers +
	     classroom + employees + classroom_used, 
	     data = pol_dta_train)
fit

s <-summary(fit)
s

plot(fit$residuals, ylab = 'Residuals')
hist(fit$residuals, xlab = 'Residuals', prob = T, main = '')
names(fit)

fitted_polygons <- fitted(fit, polygon = T, vertices = v)
head(fitted_polygons, 1)

pplot(fitted_polygons) + labs(x = 'Dimension 1', y = 'Dimension 2') +
  theme_bw()

rmsea(fitted_polygons, pol_dta_train$proficiency_mt)

#Prediction

zeros <- matrix(0, nrow = nrow(center_test), ncol = ncol(center_test))
X1 <- cbind(1, center_test[, -2], zeros)
X2 <- cbind(zeros, 1, radius_test[, -2])
X <- rbind(as.matrix(X1), as.matrix(X2))
Y <- matrix(c(center_test[, 2], radius_test[, 2]))

beta <- (fit$coefficients)

Y_estimated <- X %*% beta
Y_center_estimated <- Y_estimated[1 : n_test]
Y_radius_estimated <- Y_estimated[(n_test + 1) : (2 * n_test)]

dta_estimated <- list()
dta_estimated[[1]] <- data.frame(y_estimated = Y_center_estimated)
dta_estimated[[2]] <- data.frame(y_estimated = Y_radius_estimated)
names(dta_estimated) <- c('center', 'radius')
class(dta_estimated) <- 'paggregated'

dta_estimated <- psymbolic(dta_estimated, v)
rmsea(dta_estimated$y_estimated, pol_dta_test$proficiency_mt)




