
squared <- function(polygons){
  lower <- t(sapply(polygons, function(y) apply(y, 2, function(x) min(x))))[, 1]
  upper <- t(sapply(polygons, function(y) apply(y, 2, function(x) max(x))))[, 1]
  
  squares <- vector('list', length(polygons))
  for(i in 1 : length(polygons)){
    squares[[i]] <- matrix(c(rep(lower[i], 2), 
                             upper[i], lower[i],
                             rep(upper[i], 2),
                             lower[i], upper[i]), ncol = 2, byrow = TRUE)
  }
  squares
}

all_squared <- function(polygonal_variables){
  var_names <- names(polygonal_variables)
  
  out <- vector('list', length(var_names))
  for(j in 1 : length(var_names)){
    out[[j]] <- squared(polygonal_variables[[var_names[j]]])
  }
  list2env(setNames(out, var_names))
}

interval <- function(polygons){
  var_names <- names(polygons)

  center <- matrix(0, nrow = length(polygons[[var_names[1]]]), ncol = length(polygons))
  range <- matrix(0, nrow = length(polygons[[var_names[1]]]), ncol = length(polygons))
  for(j in 1 : length(var_names)){
    temp <- polygons[[var_names[j]]]
    lower <- t(sapply(temp, function(y) apply(y, 2, function(x) min(x))))[, 1]
    upper <- t(sapply(temp, function(y) apply(y, 2, function(x) max(x))))[, 1]
    
    center[, j] <- (upper + lower) / 2
    range[, j] <- upper - lower
  }  
  
  center <- data.frame(center)
  range <- data.frame(range)
  
  names(center) <- var_names
  names(range) <- var_names
  
  list(center = center, range = range)
}

#interval_prediction <- function(center, range){
#  lower <- range - center / 2
#  upper <- range + center / 2
#  
#  squares <- vector('list', length(lower))
#  for(i in 1 : length(lower)){
#    squares[[i]] <- matrix(c(rep(lower[i], 2), 
#                             upper[i], lower[i],
#                             rep(upper[i], 2),
#                             lower[i], upper[i]), ncol = 2, byrow = TRUE)
#  }
#  squares
#}

# ETKRR_S3

##### Gaussian Kernel
gauss.kern = function(a, b, s){
  as.vector(exp(-(1 / s)*(a - b)^2))
}

sigma2est = function(y1, y2, frac = .5){
  n = length(y1)
  m = floor(n*frac)
  idx1 = sample(1 : n, m, replace = T)
  idx2 = sample(1 : n, m, replace = T)
  tmp = (y1[idx1] - y2[idx2])^2
  mean(quantile(tmp[tmp != 0], probs = c(.9, .1)))
}

kernel.reg3 = function(x, y, tol = 1e-10, maxit = 100)
{
  x = as.matrix(x)
  n = nrow(x)
  # Initialization
  x = cbind(1, x)
  betahat = solve(t(x)%*%x)%*%t(x)%*%y
  yhat = x%*%betahat
  s2 = sum((y-yhat)^2)/(n-ncol(x))
  K = gauss.kern(y, yhat, s2)
  S = sum(2-2*K)
  it = 1
  # Model Step
  repeat {
    it = it+1
    betahat = solve(t(x)%*%diag(K)%*%x)%*%t(x)%*%diag(K)%*%y
    yhat = x%*%betahat
    K = gauss.kern(y, yhat, s2)
    S = c(S, sum(2-2*K))
    if (abs(S[it]-S[(it-1)]) <= tol || it >= maxit) break
  }
  (result = list(coef = as.vector(betahat), fitted = as.vector(yhat), criterion = S, weigth = K, iter = it))
}


monte_carlo <- function(df_center, df_radius, size = 100, model = 'interval', model_int = 'ietkrr'){
  phi <- rep(0, size)
  for(i in 1 : size){
    # Select a sample from the data
    set.seed(i)
    samp <- sample(1 : nrow(df_center), .7 * nrow(df_center), replace = F)
    
    df_center_train <- df_center[samp, -1]
    df_radius_train <- df_radius[samp, -1]
    
    df_center_test <- df_center[-samp, -1]
    df_radius_test <- df_radius[-samp, -1]
    n_test <- nrow(df_center_test)
    id <- df_center$county
    id_test <- id[-samp]
    
    # Building the polygons  
    dta_train <- list()
    dta_train[[1]] <- df_center_train
    dta_train[[2]] <- df_radius_train
    names(dta_train) <- c('center', 'radius')
    class(dta_train) <- 'paggregated'
    
    dta_test <- list()
    dta_test[[1]] <- df_center_test
    dta_test[[2]] <- df_radius_test
    names(dta_test) <- c('center', 'radius')
    class(dta_test) <- 'paggregated'
    
    v <- 20 
    pol_dta_train <- psymbolic(dta_train, v)
    pol_dta_test <- psymbolic(dta_test, v)
    
    if(model == 'plr'){
      # Polygonal Linear Regression (PLR)
      fit <- plr(proficiency_mt ~ proficiency_lp+ computers + classroom + employees + 
                   classroom_used, data = pol_dta_train)
      
      # Prediction PLR
      zeros <- matrix(0, nrow = nrow(center_test), ncol = ncol(center_test))
      X1 <- cbind(1, center_test[, -2], zeros)
      X2 <- cbind(zeros, 1, radius_test[, -2])
      X <- rbind(as.matrix(X1), as.matrix(X2))
      #Y <- matrix(c(center_test[, 2], radius_test[, 2]))
      
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
      phi[i] <- rmsea(dta_estimated$y_estimated, pol_dta_test$proficiency_mt)
    }    
    # Center-Range model
    if(model == 'interval'){
      sqr_train <- all_squared(pol_dta_train)
      sqr_test <- all_squared(pol_dta_test)
      
      interval_train <- interval(sqr_train)
      interval_test <- interval(sqr_test)
      
      y_center_train <- interval_train$center$proficiency_mt
      x_center_train <- interval_train$center
      x_center_train$proficiency_mt <- NULL
      
      y_range_train <- interval_train$range$proficiency_mt
      x_range_train <- interval_train$range
      x_range_train$proficiency_mt <- NULL
      
      y_center_test <- interval_test$center$proficiency_mt
      x_center_test <- interval_test$center
      x_center_test$proficiency_mt <- NULL
      
      y_range_test <- interval_test$range$proficiency_mt
      x_range_test <- interval_test$range
      x_range_test$proficiency_mt <- NULL
      
      if(model_int == 'crm'){
        fit_center <- lm(proficiency_mt ~ proficiency_lp+ computers + classroom + employees +
                           classroom_used, data = interval_train$center)
        fit_range <- lm(proficiency_mt ~ proficiency_lp+ computers + classroom + employees +
                          classroom_used, data = interval_train$range)
        
        # Prediction CRM
        predict_center <- predict(fit_center, x_center_test)
        predict_range <- predict(fit_range, x_range_test)
      }
      else{
        # Prediction IETKRR
        beta_center <- matrix(kernel.reg3(x = x_center_train, y = y_center_train)$coef)
        beta_range <- matrix(kernel.reg3(x = x_range_train, y = y_range_train)$coef)

        predict_center <- as.vector(cbind(1, as.matrix(x_center_test)) %*% beta_center)
        predict_range <- as.vector(cbind(1, as.matrix(x_range_test)) %*% beta_range)
      }
      
      squares_test <- list()
      squares_test[[1]] <- y_center_test
      squares_test[[2]] <- y_range_test
      names(squares_test) <- c('center', 'radius')
      class(squares_test) <- 'paggregated'
      
      predict_squares <- list()
      predict_squares[[1]] <- predict_center
      predict_squares[[2]] <- predict_range
      names(predict_squares) <- c('center', 'radius')
      class(predict_squares) <- 'paggregated'
      
      squares_test <- psymbolic(squares_test, 4)
      predict_squares <- psymbolic(predict_squares, 4)
      
      phi[i] <- rmsea(predict_squares[['X1']], squares_test[['X1']])
    }  

  }
  phi
}