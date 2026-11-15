get_daily_T <- function(df078, dayj = 1){
  x <- df078[dayj,]
  StID <- substr(x, 1, 7)
  Day <- paste(
    substr(x, 8, 11), "-",
    substr(x, 12, 13), "-",
    substr(x, 14, 15), sep = ""
  )
  
  Date <- seq(
    from = as.POSIXct(
      paste(Day, "00:00"),
      tz="UTC"
    ),
    to = as.POSIXct(
      paste(Day, "23:00"),
      tz="UTC"
    ),
    by = "hour"
  )
  
  Data <- c(
    substr(x, 19, 24) |> as.numeric()/10, ## 00:00:00
    substr(x, 25, 31) |> as.numeric()/10, ## 01:00:00
    substr(x, 32, 38) |> as.numeric()/10, ## 02:00:00
    substr(x, 39, 46) |> as.numeric()/10, ## 03:00:00
    substr(x, 47, 53) |> as.numeric()/10, ## 04:00:00
    substr(x, 54, 60) |> as.numeric()/10, ## 05:00:00
    substr(x, 61, 67) |> as.numeric()/10, ## 06:00:00
    substr(x, 68, 74) |> as.numeric()/10, ## 07:00:00
    substr(x, 75, 80) |> as.numeric()/10, ## 08:00:00
    substr(x, 81, 87) |> as.numeric()/10, ## 09:00:00
    substr(x, 88, 94) |> as.numeric()/10, ## 10:00:00
    substr(x, 95, 101) |> as.numeric()/10, ## 11:00:00
    substr(x, 102, 108) |> as.numeric()/10, ## 12:00:00
    substr(x, 109, 115) |> as.numeric()/10, ## 13:00:00
    substr(x, 116, 122) |> as.numeric()/10, ## 14:00:00
    substr(x, 123, 129) |> as.numeric()/10, ## 15:00:00
    substr(x, 130, 136) |> as.numeric()/10, ## 16:00:00
    substr(x, 137, 143) |> as.numeric()/10, ## 17:00:00
    substr(x, 144, 150) |> as.numeric()/10, ## 18:00:00
    substr(x, 151, 157) |> as.numeric()/10, ## 19:00:00
    substr(x, 158, 164) |> as.numeric()/10, ## 20:00:00
    substr(x, 165, 171) |> as.numeric()/10, ## 21:00:00
    substr(x, 172, 178) |> as.numeric()/10, ## 22:00:00
    substr(x, 179, 185) |> as.numeric()/10 ## 23:00:00
  )
  
  df <- data.frame(Date = Date,
                   Temp = Data)
  print(Day)
  return(df)
}

process_data <- function(St_Id){
  #St_Id <- St_Ids[1]
  file078 <- paste(
    "data/92st/L", St_Id, ".078", sep = ""
  )
  df078 <- read.csv(file = file078, header = F)
  n <- nrow(df078)
  
  ProcessedDf <- get_daily_T(df078, 1)
  for (i in 2:n) {
    df <- get_daily_T(df078, i)
    ProcessedDf <- rbind(
      ProcessedDf,
      df
    )
  }
  return(ProcessedDf)
}

get_as.ts <- function(data){
  #x <- data[, c(6, 8, 10, 12, 14, 16, 18)]
  x <- data$Temp..A.C.
  timeSeries(x, data$Date)
}

ts.to.df <- function(ts.){
  p <- ncol(ts.) 
  n <- nrow(ts.)
  X <- data.frame(matrix(ncol = p, nrow = n))
  for (i in 1:(p)) {
    X[,i] <- as.numeric(ts.[,i])
  }
  colnames(X) <- names(ts.)
  return(X)
}

get_data_byYear <- function(Data, year){
  Data[Data$Year == year,]
}

get_data_byMonth <- function(Data, month){
  Data[Data$Month == month,]
}

get_Seasons <- function(Data, Sep_dates){
  Data <- Data |> na.omit()
  Data <- St.Df
  Sep_dates <- sepd2
  
  Years <- Data$Year |> unique()
  Data$Season <- ""
  #Data$Date <- Data$Date #|> as.timeDate()
  n <- nrow(Sep_dates)
  S <- paste("S", seq(1, n), sep = "")
  for (year in Years) {
    #year <- Years[2]
    df <- Data[Data[,"Year"] == year, ]
    df[,"Season"] <- "S1"
    for (i in 2:n) {
      sepd <- year |> paste(
        "-", Sep_dates[i,], sep = ""
      ) |> as.timeDate()
      
      idx <- df$Date >= sepd[1] & df$Date < sepd[2]
      df[idx, "Season"] <- S[i]
    }
    
    Data[Data[,"Year"] == year, ] <- df
    print(year)
  }
  return(Data)
}

get_parms <- function(Mclust_object, type = "Mu"){
  
  Mu <- Mclust_object$parameters$mean |> as.numeric()
  Pro <- Mclust_object$parameters$pro
  Var <- Mclust_object$parameters$variance$sigmasq
  if(type == "Mu") return(Mu)
  if(type == "Pro") return(Pro)
  if(type == "Var") return(Var)
}

hist_plot <- function(Data, log = FALSE) {
  
  S <- Data$Station[1]
  Y <- Data$Year[1]
  m <- Data$Temp |>
    Mclust(G = 2)
  mp <- get_parms(m)
  
  t <- S
  
  p1 <- 
    Data |> 
    ggplot(aes(x = Temp)) +
    geom_histogram(aes(y = stat(density)),
                   alpha = .5,
                   colour="black", 
                   fill="white",
                   binwidth = 4) +
    labs(
      x = "Temperature", 
      y = "Freq", 
      #colour = "Number of cylinders",
      title = t
    ) +
    mapply(
      function(mean, sd, lambda) {
        stat_function(
          fun = function(x) {
            (dnorm(x, mean = mean, sd = sd)) * lambda
          }
        )
      },
      mean = m$parameters$mean,
      sd = sqrt(m$parameters$variance$sigmasq),
      lambda = m$parameters$pro
    )
 
  ybar <- 
    p1 |>
    layer_data() |> 
    as_tibble() |> 
    dplyr::select(count, x, y) |> 
    dplyr::filter(x == 0 | x == 16)
  p1 <- 
    p1 +
  #  annotate("bar", x = 0, y = ybar$y[1], 
  #           label = "0%C", color = "red", width = 4) +
  #  annotate("bar", x = 16, y = ybar$y[2], 
  #           label = "16%C", color = "blue", width = 4) +
    geom_vline(xintercept = mp[1], color = "red") +
    geom_vline(xintercept = mp[2], color = "blue")
  
  
  
  #return(p1)
  list(p1, summary(m))
}


fun = function(x, Prm) {
  d <- ((dnorm(x, mean = Prm$Mu1, sd = sqrt(Prm$Var1))) * Prm$Pro1) +
    ((dnorm(x, mean = Prm$Mu2, sd = sqrt(Prm$Var2))) * Prm$Pro2)
  return(d)
}