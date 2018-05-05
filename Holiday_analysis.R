library(ggplot2)  # Plotting
library(forecast) # R. Hyndman's forecast: ARIMA, ETS, AR-NN
library(timeDate) # Handles dates and holidays
library(caret)    # General neural nets

# Helper functions
#####################################################################################

compute_error <- function(pred, data, start, end){
  diff <- pred - data[start:end]
  error <- sum(abs(diff/data[start:end]))
  return(error)
}

plot_pred <- function(pred, data, start, end, title, param1, param2){
  n <- end - start + 1
  plot(1:n, data[start:end], type="l", col="red", 
       main=paste(title, param1, param2, sep=" "))
  lines(1:n, pred, type="l", col="blue")
}

rank_reduce <- function(data, num_comp){
  rr <- svd(data, nu=num_comp, nv=num_comp)
  data <- rr$u %*% diag(rr$d[1:num_comp]) %*% t(rr$v)
  return(data.frame(data))
}

holiday_pred <- function(pred, actual, start, end){
  print("Average")
  print(mean(pred))
  print(mean(rev_actual[start:end]))
  
  print("Thanksgiving")
  x <- which(holidays[,1]!=0, arr.ind=TRUE)
  x <- x[length(x)]
  print(pred[x - start + 1])
  print(rev_actual[x])
  
  print("Black Friday")
  x <- which(holidays[,2]!=0, arr.ind=TRUE)
  x <- x[length(x)]
  print(pred[x - start + 1])
  print(rev_actual[x])
  
  print("Black Friday Weekend - Sat")
  x <- which(holidays[,3]!=0, arr.ind=TRUE)
  x <- x[length(x)]
  print(pred[x - start + 1])
  print(rev_actual[x])
  
  print("Black Friday Weekend - Sun")
  x <- which(holidays[,4]!=0, arr.ind=TRUE)
  x <- x[length(x)]
  print(pred[x - start + 1])
  print(rev_actual[x])
  
  print("Cyber Monday")
  x <- which(holidays[,5]!=0, arr.ind=TRUE)
  x <- x[length(x)]
  print(pred[x - start + 1])
  print(rev_actual[x])
  
  print("Christmas")
  x <- which(holidays[,6]!=0, arr.ind=TRUE)
  x <- x[length(x)]
  print(pred[x - start + 1])
  print(rev_actual[x])
}

holiday_ratio <- function(hol_ind, rev_data){
  hr <- rep(0,length(hol_ind))
  for(i in 1:length(hol_ind)){
    if(hol_ind[i] < length(rev_data)){
      min_ind <- max(1, hol_ind[i]-365)
      med <- median(rev_data[min_ind:hol_ind[i]])
      hr[i] <- rev_data[hol_ind[i]]/med
    }
  }
  return(mean(hr[which(hr!=0, arr.ind=TRUE)]))
}

ratio_correction <- function(pred, h_ratios, holidays, start){
  for(i in 1:length(h_ratios)){
    hol_ind <- which(holidays[,i] != 0, arr.ind=TRUE)
    hol_ind <- hol_ind[which(hol_ind > start, arr.ind=TRUE)] - start + 1
    min_ind <- max(1, hol_ind-365)
    med <- median(pred[min_ind:hol_ind])
    pred[hol_ind] <- med*h_ratios[i]
  }
  return(pred)
}

# Main Code
#####################################################################################

data <- read.csv("rev_12_3.csv", header=TRUE)
end <- length(data[,1]) # index of last data point

ar_vals <- seq(1,20,1) # auto-regression values to try in neural net
hl_vals <- seq(1,20,1) # number of hidden layer nodes to try in neural net
tr_vals <- seq(0.4,0.9,0.1) # percent of data to use for training
ann_error <- array(0, dim=c(length(ar_vals), length(hl_vals), length(tr_vals))) # 3-d matrix of errors

kc <- 0
for(k in tr_vals){
  kc <- kc+1 # index for training set size
  
  start <- floor(tr_vals[kc]*end) # Use this fraction of samples for training
  num_pred <- end - start + 1 # Number of elements to be predicted
  
  time <- seq(1,start-1)
  rev <- as.numeric(gsub( "[$,]", "", as.character(data[time,2])))/1e6
  time_actual <- seq(1:end)
  rev_actual <- as.numeric(gsub( "[$,]", "", as.character(data[,2])))/1e6
  plot(time_actual, rev_actual, type="l") # plot actual revenue
  
  thanksgiving <- rep(0,end)
  black_friday <- rep(0,end)
  black_saturday <- rep(0,end)
  black_sunday <- rep(0,end)
  cyber_monday <- rep(0,end)
  christmas <- rep(0,end)
  year <- rep(0,end)'
  
  # assemble dummy variables for holidays to serve as regressors
  for(i in 1:end){
    date <- as.Date(data[i,1],format="%m/%d/%Y")
    year[i] <- as.numeric(format(date, "%Y"))
    thgv <- holiday(as.numeric(format(date, "%Y")), Holiday="USThanksgivingDay")
    xmas <- holiday(as.numeric(format(date, "%Y")), Holiday="USChristmasDay")
    if(as.numeric(date) == as.numeric(thgv)){
      thanksgiving[i] <- 1
      black_friday[i+1] <- 1
      black_saturday[i+2] <- 1
      black_sunday[i+3] <- 1
      cyber_monday[i+4] <- 1
    }
    if(as.numeric(date) == as.numeric(xmas)){
      christmas[i] <- 1
    }
  }
  
  holidays <- cbind(thanksgiving, black_friday, black_saturday, black_sunday,
                    cyber_monday, christmas)
  
  h_ratios <- rep(0,length(holidays[1,]))
  for(i in 1:length(h_ratios)){
    hold_ind <- which(holidays[,i]!=0, arr.ind=TRUE)
    h_ratios[i] <- holiday_ratio(hold_ind, rev)
  }
  
  # forecast tbats: ETS with multi-seasonlity
  #####################################################################################
  
  # x <- msts(rev,seasonal.periods=c(7,365))
  # fit <- tbats(x)
  # pred <- forecast(fit,h=num_pred)
  # plot_pred(pred$mean, rev_actual, start, end, "TBATS: ", "N/A", "N/A")
  # error <- compute_error(pred$mean, rev_actual, start, end)
  # print("TBATS")
  # print(error)
  # holiday_pred(pred$mean, rev_actual, start, end)
  # print("-------------------------")
  
  # forecast auto.arima: ARIMA with multi-seasonality using Fourier components
  #####################################################################################
  
  x <- msts(rev,seasonal.periods=c(7,365))
  z <- fourier(x, K=c(3,15))
  zf <- fourierf(x, K=c(3,15), h=num_pred)
  fit <- auto.arima(x, xreg=cbind(z,holidays[1:(start-1),]), seasonal=FALSE)
  pred <- forecast(fit, xreg=cbind(zf,holidays[start:end,]), h=num_pred)
  plot_pred(pred$mean, rev_actual, start, end, "ARIMA: ", "N/A", "N/A")
  error <- compute_error(pred$mean, rev_actual, start, end)
  print("ARIMA")
  print(error)
  holiday_pred(pred$mean, rev_actual, start, end)
  print("-------------------------")
  
  # caret avvNNet: General neural net with STL decomposition and multi-seasonality
  #####################################################################################
  
  ann_iterative <- function(ann, num_ar, num_min, df, df_new, num_pred){
    fc <- matrix(-1, 1, num_pred+num_min)
    for(i in 1:num_min){
      fc[i] <- df[length(df[,1])-num_min-1+i,1]
    }
    for(i in (num_min+1):(num_pred+num_min)){
      if(i > 365){
        reg_new <- data.frame(cbind(1, matrix(fc[1,(i-num_ar):(i-1)], nrow=1),
                                    fc[i-7], fc[i-365],
                                    matrix(df_new[i-num_min,], nrow=1)))
      }else{
        reg_new <- data.frame(cbind(1, matrix(fc[1,(i-num_ar):(i-1)], nrow=1),
                                    fc[i-7], df[length(df[,1])-365+i-num_min-1,1],
                                    matrix(df_new[i-num_min,], nrow=1)))
      }
      fc[i] <- predict(ann, reg_new, type=c("raw"))
    }
    return(fc[1,-(1:num_min)])
  }
  
  general_ann <- function(num_ar, num_hidden){
    rev_ts <- ts(rev, frequency=7)
    rev_stl <- stl(rev_ts, s.window="period")
    stl_weekly <- rev_stl$time.series[,1]
    rev_ns <- rev_stl$time.series[,2] + rev_stl$time.series[,3]
    if(num_ar > 7){
      num_min <- num_ar
      t <- matrix(0,start-1-num_ar,num_ar)
      for(i in 1:num_ar){
        t[,i] <- rev_ns[(num_ar-i+1):(start-i-1)]
      }
    }else{
      num_min <- 7
      if(num_ar > 0){
        t <- matrix(0,start-1-num_min,num_ar)
        for(i in 1:num_ar){
          t[,i] <- rev_ns[(num_min-i+1):(start-i-1)]
        }
      }else{
        t <- rep(0,length(1:(start-1)))
      }
    }
    y <- rev_ns[(num_min+1):(start-1)]
    weekly <- rev_ns[(num_min-7+1):(start-7-1)]
    annual <- c(rev_ns[(1+num_min):(365+num_min)], rev_ns[(1+num_min):(start-1-365)])
    df <- data.frame(matrix(c(y, t, weekly, annual,
                              holidays[(num_min+1):(start-1),]), nrow=start-1-num_min))
    wts_init <- rowSums(holidays[(num_min+1):(start-1),])*100+1
    # num_comp <- length(df[1,])
    # df <- rank_reduce(df, num_comp) # SVD doesn't help much with pure time series data
    ann <- avNNet(X1 ~ ., data=df, repeats=100, weights=wts_init,
                  size=num_hidden, linout=TRUE, trace=FALSE, maxit=1000, decay=1e-5)
    df_new <- matrix(c(holidays[start:end,]), nrow=num_pred)
    pred <- ann_iterative(ann, num_ar, num_min, df, df_new, num_pred)
    
    # restore weekly seasonality
    stl_weekly_rep <- rep(stl_weekly[(start%%7):((start%%7)+6)],ceiling(num_pred/7))
    pred <- pred + 1.5*stl_weekly_rep[1:num_pred]
    
    # plot result and compute error
    plot_pred(pred, rev_actual, start, end, "General Neural Net: ", num_ar, num_hidden)
    error <- compute_error(pred, rev_actual, start, end)
    print(error)
    holiday_pred(pred, rev_actual, start, end)
    
    #   print("-------------------------")
    #   # apply ratio correction
    #   pred <- ratio_correction(pred, h_ratios, holidays, start)
    #   plot_pred(pred, rev_actual, start, end, "General Neural Net (ratio corrected): ", num_ar, num_hidden)
    #   error <- compute_error(pred, rev_actual, start, end)
    #   print(error)
    #   holiday_pred(pred, rev_actual, start, end)
    
    return(error)
  }
  
  ic <- 0 # index for auto-regression order
  jc <- 0 # index for number of hidden layer nodes
  
  for(i in ar_vals){
    ic <- ic+1
    for(j in hl_vals){
      jc <- jc+1
      print(paste("ANN with AR:", i, "Hidden:", j, sep=" "))
      ann_error[ic, jc, kc] <- general_ann(i,j)
      print("-------------------------")
    }
    jc <- 0
  }


