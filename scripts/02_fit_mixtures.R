
##------------------------------------------------------------------------------
## Density Estimation 
##
## In this file, we estimate all the densities based on methodes discused 
## in the report. 
## This file returns at the end the data series (produced with the the 
## estimations of densities) that will be used later for regression modeling 
## -----------------------------------------------------------------------------

library(naniar)
library(mclust)
library(ggplot2)
library(timeSeries)
library(mixtools)



##------------------------------------------------------------------------------
## Thresholds counts
## -----------------------------------------------------------------------------

##----------------------------------------------------
## 0 C threshold  {-2 < T < 2}
##----------------------------------------------------

St <- St_names[1]
df <- DF |> 
  select(Date, all_of(St))
df <- df |>
  add_column(
    Year = format(df$Date, "%Y"),
    Month = format(df$Date, "%m"),
    Day = format(df$Date, "%d"),
    Temp = df[,St]
  )
df <- df[,-2]
df
thresh0_15c <- list()


yrlyHist <- list()
for (St in St_names) {
  print(St)
  St.Df <- DF |> 
    select(Date, St)
  St.Df <- St.Df |>
    add_column(
      Year = format(St.Df$Date, "%Y"),
      Month = format(St.Df$Date, "%m"),
      Day = format(St.Df$Date, "%d")
    ) |> 
    na.omit()
  colnames(St.Df)[2] <- "Temp"
  St.Df <- St.Df[St.Df$Temp > -50,]
  St.Yrs <- St.Df$Year |> unique() |> na.omit()
  
  thC <- 
    matrix(
      nrow = length(St.Yrs),
      ncol = 3
    ) |> 
    as.data.frame()
  colnames(thC) <- c("Year", "0", "16")
  thC[,"Year"] <- St.Yrs
  
  thF <- 
    matrix(
      nrow = length(St.Yrs),
      ncol = 3
    ) |> 
    as.data.frame()
  colnames(thF) <- c("Year", "0", "16")
  thF[,"Year"] <- St.Yrs
  for (year in St.Yrs) {
    #year <- St.Yrs[3]
    print(year)
    
    Df.Yrs <- St.Df[St.Df$Year == year,]
    COND1 <- nrow(Df.Yrs)
    if(COND1 < 7665) next
    
    subt <- "Station :" |>
      paste(St, "                     Year :", year)
    p <- Df.Yrs |>
      ggplot(aes(x = Temp)) +
      geom_histogram(aes(y = stat(density)),
                     alpha = .5,
                     colour="black", 
                     fill="white",
                     binwidth = 4) +
      labs(
        x = "Temperature", 
        y = "Counts", 
        #colour = "Number of cylinders",
        title = "Histogram of hourly temperatures",
        subtitle = subt
      )
    yrlyHist[[St]][[year]] <- p
    co <- p |>
      layer_data() |> 
      as_tibble() |> 
      select(x, count, density) |> 
      dplyr::filter(x == 0 | x == 16)
    thC[thC$Year == year, c("0", "16")] <- co$count
    thF[thF$Year == year, c("0", "16")] <- co$density
  }
  thC <- thC |> 
    as.data.table() |> 
    melt.data.table(id.vars = c("Year"),
                    measure.vars = c("0", "16"),
                    variable.name = "Class",
                    value.name = "Count")
  thF <- thF |> 
    as.data.table() |> 
    melt.data.table(id.vars = c("Year"),
                    measure.vars = c("0", "16"),
                    variable.name = "Class",
                    value.name = "Freq")
  thST <- merge(thC, thF, by = c("Year", "Class"))
  thST <- thST |> add_column(Station = St)
  if(St == St_names[1]){
    thrTC <- thST
  }else{
    thrTC <- thrTC |> rbind(thST)
  }
  #thresh0_15c[[St]]$Year <- Melange_2_gauss[[St]]$Year |>
  #  as.numeric()
  #thresh0_15c[[St]] <- th
}

outputs_path <- "output/Part 1"
save(thrTC, file = paste(
  outputs_path, "/thrTC.RData", sep = ""
))
save(yrlyHist, file = paste(
  outputs_path, "/yrlyHist.RData", sep = ""
))


##------------------------------------------------------------------------------
## Gaussian mixture models
## -----------------------------------------------------------------------------

##----------------------------------------------------
## 2 Two clusters (2 populations)
##----------------------------------------------------

DF <- df
rm(df)


Melange_2_gauss <- list()
for (St in St_names) {
  #print(St)
  #St.Df <- x92st_data[[St]] |>
  #  na.omit()
  #St.Yrs <- St.Df$Year |> unique()
  
  print(St)
  St.Df <- DF |> 
    select(Date, all_of(St))
  St.Df <- St.Df |>
    add_column(
      Year = format(St.Df$Date, "%Y"),
      Month = format(St.Df$Date, "%m"),
      Day = format(St.Df$Date, "%d")
    ) |> 
    na.omit()
  colnames(St.Df)[2] <- "Temp"
  St.Df <- St.Df[St.Df$Temp > -100,]
  St.Yrs <- St.Df$Year |> unique() |> na.omit()
  
  mu_gauss <- matrix(nrow = length(St.Yrs),
                     ncol = 8) |> 
    as.data.frame()
  colnames(mu_gauss) <- c("Year", "n", "Mu1", "Mu2", "Pro1", "Pro2", "Var1", "Var2")
  mu_gauss[,"Year"] <- St.Yrs
  for (year in St.Yrs) {
    print(year)
    
    Df.Yrs <- St.Df[St.Df$Year == year,]
    
    COND1 <- nrow(Df.Yrs)
    #if(COND1 < 7665) next
    
    gauss_model <- Df.Yrs$Temp |> 
      Mclust(G = 2) |> 
      summary()
    
    mu_gauss[mu_gauss$Year == year, c("Mu1", "Mu2")] <- gauss_model$mean
    mu_gauss[mu_gauss$Year == year, c("Pro1", "Pro2")] <- gauss_model$pro
    mu_gauss[mu_gauss$Year == year, c("Var1", "Var2")] <- gauss_model$variance
    mu_gauss[mu_gauss$Year == year, c("n")] <- COND1
    
  }
  Melange_2_gauss[[St]]$Year <- Melange_2_gauss[[St]]$Year |>
    as.numeric()
  Melange_2_gauss[[St]] <- mu_gauss
}
Melange_2_gauss
outputs_path <- "output/Part 1"
save(Melange_2_gauss, file = paste(
  outputs_path, "/melange_2_gauss.RData", sep = ""
))


##----------------------------------------------------
## 3 Three clusters (3 populations)
##----------------------------------------------------

Melange_3_gauss <- list()
for (St in St_names) {
  print(St)
  St.Df <- x92st_data[[St]] |>
    na.omit()
  St.Yrs <- St.Df$Year |> unique()
  mu_gauss <- matrix(nrow = length(St.Yrs),
                     ncol = 8) |> 
    as.data.frame()
  colnames(mu_gauss) <- c("Year", "n", "Mu1", "Mu2", "Mu3",
                          "Pro1", "Pro2", "Pro3")
  mu_gauss[,"Year"] <- St.Yrs
  
  for (year in St.Yrs) {
    print(year)
    Df.Yrs <- St.Df[St.Df$Year == year,]
    
    COND1 <- nrow(Df.Yrs)
    if(COND1 < 7665) next
    
    gauss_model <- Df.Yrs$Temp |>
      Mclust(G = 3)
    
    mu_gauss[mu_gauss$Year == year, c("Mu1", "Mu2", "Mu3")] <- gauss_model |>
      get_parms(type = "Mu")
    mu_gauss[mu_gauss$Year == year, c("Pro1", "Pro2", "Pro3")] <- gauss_model |>
      get_parms(type = "Pro")
    mu_gauss[mu_gauss$Year == year, c("n")] <- Df.Yrs |> 
      nrow()
  }
  Melange_3_gauss[[St]]$Year <- Melange_3_gauss[[St]]$Year |>
    as.numeric()
  Melange_3_gauss[[St]] <- mu_gauss
}

Melange_3_gauss[[92]] |> head()
outputs_path <- "output/Part 1"
save(Melange_3_gauss, file = paste(
  outputs_path, "/melange_3_gauss.RData", sep = ""
))

##----------------------------------------------------
## 4 Foor clusters (4 population)
##----------------------------------------------------

Melange_4_gauss <- list()
for (St in St_names) {
  print(St)
  St.Df <- x92st_data[[St]] |>
    na.omit()
  St.Yrs <- St.Df$Year |> unique()
  mu_gauss <- matrix(nrow = length(St.Yrs),
                     ncol = 10) |> 
    as.data.frame()
  colnames(mu_gauss) <- c("Year", "n", "Mu1", "Mu2", "Mu3", "Mu4",
                          "Pro1", "Pro2", "Pro3", "Pro4")
  mu_gauss[,"Year"] <- St.Yrs
  
  for (year in St.Yrs) {
    print(year)
    Df.Yrs <- St.Df[St.Df$Year == year,]
    
    COND1 <- nrow(Df.Yrs)
    if(COND1 < 7665) next
    
    gauss_model <- Df.Yrs$Temp |>
      Mclust(G = 4)
    
    mu_gauss[mu_gauss$Year == year, c("Mu1", "Mu2", "Mu3", "Mu4")] <- gauss_model |>
      get_parms(type = "Mu")
    mu_gauss[mu_gauss$Year == year, c("Pro1", "Pro2", "Pro3", "Pro4")] <- gauss_model |>
      get_parms(type = "Pro")
    mu_gauss[mu_gauss$Year == year, c("n")] <- Df.Yrs |> 
      nrow()
  }
  Melange_4_gauss[[St]]$Year <- Melange_4_gauss[[St]]$Year |>
    as.numeric()
  Melange_4_gauss[[St]] <- mu_gauss
}

Melange_4_gauss[[1]] |> head()
outputs_path <- "output/Part 1"
save(Melange_4_gauss, file = paste(
  outputs_path, "/melange_4_gauss.RData", sep = ""
))

##------------------------------------------------------------------------------
## Seasons Separation
## -----------------------------------------------------------------------------

##----------------------------------------------------
## 2 Two saisons ({S1: "09-21", "03-20"}, 
##                {S2: "03-21", "09-20"})
##----------------------------------------------------

sepd2 <- matrix(
  c("09-21", "03-20", 
    "03-21", "09-20"),
  ncol = 2, nrow = 2, byrow = T
)
rm(x92st_data)
x <- x92st_data[[90]]

y2 <- x |> 
  get_Seasons(Sep_dates = sepd2)
y2 |> ggplot() +
  geom_histogram(aes(x = Temp))

densW <- y2[y2$Season == "S1",]$Temp |>
  hist()



Mu_sepd2 <- list()
hist_sep2 <- list()
for (St in St_names) {
  print(St)
  
  #St <- St_names[10]
  spSt <- 
    sepdates |> 
    dplyr::filter(St_Name == all_of(St)) |> 
    select(sph1, sph2, spe1, spe2) |> 
    as.data.frame()
  
  sepd2 <- matrix(
    c(spSt[,1], spSt[,2], 
      spSt[,3], spSt[,4]),
    ncol = 2, nrow = 2, byrow = T
  )
  
  St.Df <-
    df |> 
    dplyr::filter(Station == St) |> 
    na.omit() |> 
    dplyr::filter(Temp > -100) |> 
    get_Seasons(Sep_dates = sepd2)
  
  p1 <- St.Df |> 
    ggplot(aes(x = Temp, color = Season)) +
    geom_histogram(aes(y = stat(density)),
                   alpha = .5,
                   colour="black", 
                   fill="white",
                   binwidth = 4) +
    geom_density(adjust = 4) +
    labs(
      x = "Temperature", 
      y = "Frequence" 
      #colour = "Number of cylinders"
    )
  p1
  St.Yrs <- St.Df$Year |> unique() |> as.numeric()

  mu_sep <- matrix(nrow = length(St.Yrs),
                   ncol = 8) |> 
    as.data.frame()
  colnames(mu_sep) <- c("Year", "n", "Mu1", "Mu2", "Pro1", "Pro2", "Var1", "Var2")
  mu_sep[,"Year"] <- St.Yrs
  
  hist_st <- list()
  for (year in St.Yrs) {
    #year <- St.Yrs[1]
    print(year)
    Df.Yrs <- St.Df[St.Df$Year == year,]
    
    p11 <- Df.Yrs |> 
      ggplot(aes(x = Temp, color = Season)) +
      geom_histogram(aes(y = stat(density)),
                     alpha = .5,
                     colour="black", 
                     fill="white",
                     binwidth = 4) +
      geom_density(adjust = 4) +
      labs(
        x = "Temperature", 
        y = "Frequence" 
        #colour = "Number of cylinders"
      )
    hist_st[[year]] <- p11
    
    COND <- Df.Yrs |> 
      nrow()
    COND1 <- Df.Yrs[Df.Yrs$Season == "S1",] |>
      nrow()
    COND2 <- Df.Yrs[Df.Yrs$Season == "S2",] |> 
      nrow()
    if(COND < 7665){
      print("COND")
      next
    }else{
      if(COND1 < 3822){
        print("COND1")
        next
      }
      if(COND2 < 3822){
        print("COND2")
        next
      }
    }
    
    mu1_model <- Df.Yrs[Df.Yrs$Season == "S1",]$Temp |>
      #na.omit() |>
      Mclust(G = 1)
    mu2_model <- Df.Yrs[Df.Yrs$Season == "S2",]$Temp |>
      #na.omit() |>
      Mclust(G = 1)
    
    mu_sep[mu_sep$Year == year, c("Mu1", "Mu2", "Pro1", "Pro2", "Var1", "Var2")] <- 
      c(
        mu1_model |> get_parms(type = "Mu"),
        mu2_model |> get_parms(type = "Mu"),
        0.5, 0.5,
        mu1_model |> get_parms(type = "Var"),
        mu2_model |> get_parms(type = "Var")
      )
    
    mu_sep[mu_sep$Year == year, c("n")] <- Df.Yrs |> 
      nrow()
  }
  Mu_sepd2[[St]]$Year <- Mu_sepd2[[St]]$Year |>
    as.numeric()
  Mu_sepd2[[St]] <- mu_sep
  hist_sep2[[St]] <- hist_st
}

z <- matrix(nrow = 92, ncol = 3) |>
  as.data.frame()
colnames(z) <- c("St_name", "Start_date", "End_date")
i <- 0
for (St in St_names) {
  i <- i + 1
  print(paste(i, St))
  x <- x92st_data[[St]] |> na.omit()
  y <- x$Date 
  y1 <- y |> head(n=1)
  y2 <- y |> tail(n=1)
  z[i,"St_name"] <- St
  z[i,"Start_date"] <- y1
  z[i,"End_date"] <- y2 
}

outputs_path <- "output/Part 1"
save(Mu_sepd2, file = paste(
  outputs_path, "/Mu_sepd2.RData", sep = ""
))

##----------------------------------------------------
## 3 Two saisons ({S1: "10-21", "02-20"}, 
##                {S2: "02-21", "06-20"},
##                {S3: "06-21", "10-20"})
##----------------------------------------------------

sepd3 <- matrix(
  c("10-21", "02-20", 
    "02-21", "06-20",
    "06-21", "10-20"),
  ncol = 2, nrow = 3, byrow = T
)

Mu_sepd3 <- list()
for (St in St_names) {
  print(St)
  St.Df <- x92st_data[[St]] |>
    get_Seasons(Sep_dates = sepd3)# |>
    #na.omit()
  St.Yrs <- St.Df$Year |> unique()
  mu_sep <- matrix(nrow = length(St.Yrs),
                   ncol = 5) |> 
    as.data.frame()
  colnames(mu_sep) <- c("Year", "n", "Mu1", "Mu2", "Mu3")
  mu_sep[,"Year"] <- St.Yrs
  for (year in St.Yrs) {
    print(year)
    Df.Yrs <- St.Df[St.Df$Year == year,]
    
    COND <- Df.Yrs |> 
      nrow()
    COND1 <- Df.Yrs[Df.Yrs$Season == "S1",] |>
      nrow()
    COND2 <- Df.Yrs[Df.Yrs$Season == "S2",] |> 
      nrow()
    COND3 <- Df.Yrs[Df.Yrs$Season == "S3",] |> 
      nrow()
    if(COND < 7665){
      print("COND")
      next
    }else{
      if(COND1 < 2541){
        print("COND1")
        next
      }
      if(COND2 < 2541){
        print("COND2")
        next
      }
      if(COND3 < 2541){
        print("COND3")
        next
      }
    }
    
    mu1_model <- Df.Yrs[Df.Yrs$Season == "S1",]$Temp |>
      #na.omit() |>
      Mclust(G = 1)
    mu2_model <- Df.Yrs[Df.Yrs$Season == "S2",]$Temp |>
      #na.omit() |>
      Mclust(G = 1)
    mu3_model <- Df.Yrs[Df.Yrs$Season == "S3",]$Temp |>
      #na.omit() |>
      Mclust(G = 1)
    
    mu_sep[mu_sep$Year == year, c("Mu1", "Mu2", "Mu3")] <- c(
      mu1_model |> get_parms(),
      mu2_model |> get_parms(),
      mu3_model |> get_parms()
    )
    
    mu_sep[mu_sep$Year == year, c("n")] <- Df.Yrs |> 
      nrow()
  }
  Mu_sepd3[[St]]$Year <- Mu_sepd3[[St]]$Year |>
    as.numeric()
  Mu_sepd3[[St]] <- mu_sep
}

outputs_path <- "output/Part 1"
save(Mu_sepd3, file = paste(
  outputs_path, "/Mu_sepd3.RData", sep = ""
))

##----------------------------------------------------
## 4 Two saisons ({S1: "11-21", "02-20"}, 
##                {S2: "02-21", "05-20"},
##                {S3: "05-21", "08-20"},
##                {S4: "08-21", "11-20"})
##----------------------------------------------------

sepd4 <- matrix(
  c("11-21", "02-20", 
    "02-21", "05-20",
    "05-21", "08-20",
    "08-21", "11-20"),
  ncol = 2, nrow = 4, byrow = T
)

Mu_sepd4 <- list()
for (St in St_names) {
  print(St)
  St.Df <- x92st_data[[St]] |>
    get_Seasons(Sep_dates = sepd4) #|>
    #na.omit()
  St.Yrs <- St.Df$Year |> unique()
  
  mu_sep <- matrix(nrow = length(St.Yrs),
                   ncol = 6) |> 
    as.data.frame()
  colnames(mu_sep) <- c("Year", "n", "Mu1", "Mu2", "Mu3", "Mu4")
  mu_sep[,"Year"] <- St.Yrs
  for (year in St.Yrs) {
    print(year)
    Df.Yrs <- St.Df[St.Df$Year == year,]
    
    COND <- Df.Yrs |> 
      nrow()
    COND1 <- Df.Yrs[Df.Yrs$Season == "S1",] |>
      nrow()
    COND2 <- Df.Yrs[Df.Yrs$Season == "S2",] |> 
      nrow()
    COND3 <- Df.Yrs[Df.Yrs$Season == "S3",] |> 
      nrow()
    COND4 <- Df.Yrs[Df.Yrs$Season == "S4",] |> 
      nrow()
    if(COND < 7665){
      print("COND")
      next
    }else{
      if(COND1 < 1911){
        print("COND1")
        next
      }
      if(COND2 < 1911){
        print("COND2")
        next
      }
      if(COND3 < 1911){
        print("COND3")
        next
      }
      if(COND4 < 1911){
        print("COND4")
        next
      }
    }
    
    mu1_model <- Df.Yrs[Df.Yrs$Season == "S1",]$Temp |>
      #na.omit() |>
      Mclust(G = 1)
    mu2_model <- Df.Yrs[Df.Yrs$Season == "S2",]$Temp |>
      #na.omit() |>
      Mclust(G = 1)
    mu3_model <- Df.Yrs[Df.Yrs$Season == "S3",]$Temp |>
      #na.omit() |>
      Mclust(G = 1)
    mu4_model <- Df.Yrs[Df.Yrs$Season == "S4",]$Temp |>
      #na.omit() |>
      Mclust(G = 1)
    
    mu_sep[mu_sep$Year == year, c("Mu1", "Mu2", "Mu3", "Mu4")] <- c(
      mu1_model |> get_parms(),
      mu2_model |> get_parms(),
      mu3_model |> get_parms(),
      mu4_model |> get_parms()
    )
    mu_sep[mu_sep$Year == year, c("n")] <- Df.Yrs |> 
      nrow()
  }
  Mu_sepd4[[St]]$Year <- Mu_sepd4[[St]]$Year |>
    as.numeric()
  Mu_sepd4[[St]] <- mu_sep
}

outputs_path <- "output/Part 1"
save(Mu_sepd4, file = paste(
  outputs_path, "/Mu_sepd4.RData", sep = ""
))




##### window separation 


St <- St_names[1]
print(St)
St.Df <- DF |> 
  select(Date, all_of(St))
St.Df <- St.Df |>
  add_column(
    Year = format(St.Df$Date, "%Y"),
    Month = format(St.Df$Date, "%m"),
    Day = format(St.Df$Date, "%d")
  ) |> 
  na.omit()
colnames(St.Df)[2] <- "Temp"
St.Df <- St.Df[St.Df$Temp > -100,]
St.Yrs <- St.Df$Year |> unique() |> na.omit()

sepd2 <- matrix(
  c("09-21", "03-20", 
    "03-21", "09-20"),
  ncol = 2, nrow = 2, byrow = T
)

St.Df |> get_Seasons(Sep_dates = sepd2)

bwin <- function(df, )