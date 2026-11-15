library(dplyr)
library(data.table)
library(readxl)
library(tidyverse)
library(plotly)
library(gridExtra)
library(mclust)
library(mixtools)
library(countrycode)
library(patchwork) # to add plots together

source("scripts/functions.R")

load(file = "output/Part 1/melange_2_gauss.RData")
load(file = "output/Part 1/melange_3_gauss.RData")
load(file = "output/Part 1/melange_4_gauss.RData")
load(file = "output/Part 1/Mu_sepd2.RData")
load(file = "output/Part 1/Mu_sepd3.RData")
load(file = "output/Part 1/Mu_sepd4.RData")
load("output/Part 1/thresh0_15c.RData")
load("output/Part 1/yrlyHist.RData")

X92st_list <- read_excel("data/92st_list.xlsx")
#View(X92st_list)
St_Ids <- X92st_list$stnid
X92st_list$St_Name[c(65,66,67)] <- c(
  "QUEBEC_JEAN LESAGE INTL A",
  "MONTREAL_PIERRE ELLIOTT TRUDEAU INTL A",
  "MONTREAL_MIRABEL INT'L A"
)
St_names <- X92st_list$St_Name
lookup <- structure(X92st_list$stnid, 
                    .Names = X92st_list$St_Name)

for (St in St_names) {
  print(St)
  Melange_2_gauss[[St]]$Year <- Melange_2_gauss[[St]]$Year |>
    as.numeric()
  #Melange_3_gauss[[St]]$Year <- Melange_3_gauss[[St]]$Year |>
  #  as.numeric()
  #Melange_4_gauss[[St]]$Year <- Melange_4_gauss[[St]]$Year |>
  #  as.numeric()
  
  Mu_sepd2[[St]]$Year <- Mu_sepd2[[St]]$Year |>
    as.numeric()
  #Mu_sepd3[[St]]$Year <- Mu_sepd3[[St]]$Year |>
  #  as.numeric()
  #Mu_sepd4[[St]]$Year <- Mu_sepd4[[St]]$Year |>
  #  as.numeric()
  
  #thresh0_15c[[St]]$Year <- thresh0_15c[[St]]$Year |> 
  #  as.numeric()
}

df <- 
  "data/92st/df.csv" |>
  fread()
Years <- df$Date |>
  format("%Y")

df <- df |>
  add_column(Year = Years) |>
  melt.data.table(id.vars = c("Date", "Year"),
                  measure.vars = St_names,
                  variable.name = "Station",
                  value.name = "Temp")

x <-
  df |> 
  dplyr::filter(Station == St_names[2]) |> 
  na.omit() |> 
  dplyr::filter(Temp > -100)

get_Seasons(x, sepd2)

get_Seasons <- function(x, Sep_dates){
 
  x <- x |> as.data.frame()
  #Sep_dates <- sepd2
  
  Years <- x$Year |> unique()
  x$Season <- ""
  Data <- x
  Data$Date <- Data$Date |> as.timeDate()
  n <- nrow(Sep_dates)
  S <- paste("S", seq(1, n), sep = "")
  
  for (year in Years) {
    #year <- Years[1]
    df <- Data[Data[,"Year"] == year, ]
    df[,"Season"] <- "S1"
    for (i in 2:n) {
      #i <- 1
      sepd <- year |> paste(
        "-", Sep_dates[i,], sep = ""
      ) |> as.timeDate()
      sepd
      idx <- df$Date >= sepd[1] & df$Date < sepd[2]
      df[idx, "Season"] <- S[i]
    }
    
    Data[Data[,"Year"] == year, ] <- df
    print(year)
    #Data
  }
  return(Data)
}

t <- 
  "Histogramme des temperatures horaires  -  " |> 
  paste0("VICTORIA INT'L A")
p1 <- 
  x |> 
  ggplot(aes(x = Temp)) +
  geom_histogram(aes(y = stat(density)),
                 alpha = .5,
                 colour="black", 
                 fill="white",
                 binwidth = 4) +
  labs(
    x = "Temperature", 
    y = "densit??", 
    #colour = "Number of cylinders"
  )
p1
ybar <- 
  p1 |>
  layer_data() |> 
  as_tibble() |> 
  select(count, x, y) |> 
  dplyr::filter(x == 0 | x == 16)
p1 <- 
  p1 +
  annotate("bar", x = 0, y = ybar$y[1], fill = "blue", 
           label = "0%C", color = "blue", width = 4) +
  annotate("bar", x = 16, y = ybar$y[2], fill = "red", 
           label = "16C", color = "red", width = 4) 


x |> hist_plot()  
p1 + theme_gray(base_size = 14)
p1 + theme_classic() 



m1 <- x$Temp |> 
  Mclust(G = 2)

x |> ggplot(aes(x = Temp)) +
  geom_histogram(aes(y = stat(density)),
                 alpha = .5,
                 colour="black", 
                 fill="white",
                 binwidth = 4) +
  mapply(
    function(mean, sd, lambda) {
      stat_function(
        fun = function(x) {
          (dnorm(x, mean = mean, sd = sd)) * lambda
        }
      )
    },
    mean = m1$parameters$mean,
    sd = sqrt(m1$parameters$variance$sigmasq),
    lambda = m1$parameters$pro
  )



#file_path <- 
#  "output/Part 1/PDF1/" |>
#  paste0(St, ".pdf")
pdf(file = "output/Part 1/PDF1/summary2.pdf", 
    width = 10, height = 7)
for (St in St_names) {
  St <- St_names[66]
  print(St)
  x <-
    df |> 
    dplyr::filter(Station == St) |> 
    na.omit() |> 
    dplyr::filter(Temp > -100)
  #m <- x$Temp |> 
  #  Mclust(G = 3)
  Yrs <- x$Year |> unique()
  y <- hist_plot(x)
  p1 <- y[[1]]
  m <- y[[2]]
  p1 <- p1 + theme_classic() 
  
  p21 <- 
    Melange_2_gauss[[St]] |> 
    as.data.table() |>
    select(Year, Mu1, Mu2) |> 
    melt.data.table(id.vars = c("Year"),
                    measure.vars = c("Mu1", "Mu2"),
                    variable.name = "Class",
                    value.name = "Mu") |>
    dplyr::filter(Year > 1981 & Year < 2013) |> 
    na.omit() |> 
    ggplot(aes(x = Year, y = Mu, color = Class)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm") +
    labs(
      x = "Ann??e", 
      y = "Mu estim??", 
      #colour = "Number of cylinders"
    )
  p21 <- p21 + theme_classic() 
  p21
  p22 <- 
    Melange_2_gauss[[St]] |> 
    as.data.table() |>
    select(Year, Pro1, Pro2) |> 
    melt.data.table(id.vars = c("Year"),
                    measure.vars = c("Pro1", "Pro2"),
                    variable.name = "Class",
                    value.name = "Prob") |> 
    na.omit() |> 
    ggplot(aes(x = Year, y = Prob, color = Class)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm")
  p22
  
  p23 <- 
    Melange_2_gauss[[St]] |> 
    as.data.table() |>
    select(Year, Var1, Var2) |> 
    melt.data.table(id.vars = c("Year"),
                    measure.vars = c("Var1", "Var2"),
                    variable.name = "Class",
                    value.name = "Var") |> 
    na.omit() |> 
    ggplot(aes(x = Year, y = Var, color = Class)) +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "lm")
  p23
  #p31 <- 
  #  thresh0_15c[[St]] |> 
  #  as.data.table() |> 
  #  melt.data.table(id.vars = c("Year"),
  #                  measure.vars = c("0", "16"),
  #                  variable.name = "Threshold",
  #                  value.name = "T_C") |> 
  #  na.omit() |> 
  #  ggplot(aes(x = Year, y = T_C, color = Threshold)) +
  #  geom_point() +
  #  geom_smooth(formula = y ~ x, method = "lm")
  #p31
  #file_path <- 
  #  "output/Part 1/PDF1/" |>
  #  paste0(St, ".pdf")
  #pdf(file = file_path, width = 10, height = 7)
  
  r <- x$Temp |> range()
  d <- data.frame(x = seq(r[1],r[2],.5))
  Prm <- data.frame(Mu1 = m$mean[1], Mu2 = m$mean[2],
                    Var1 = m$variance[1], Var2 = m$variance[2],
                    Pro1 = m$pro[1], Pro2 = m$pro[2])
  d$y <- d$x |> fun(Prm)
  p2 <- d |> ggplot(aes(x = x, y = y)) +
    geom_line() +
    geom_vline(xintercept = Prm$Mu1, color = "red") +
    geom_vline(xintercept = Prm$Mu2, color = "blue") 
  p3 <- x |> ggplot(aes(x = Temp)) +
    geom_histogram(aes(y = stat(density)),
                   alpha = .5,
                   colour="black", 
                   fill="white",
                   binwidth = 4) +
    geom_line(data = d, aes(x = x, y = y)) +
    geom_vline(xintercept = Prm$Mu1, color = "red") +
    geom_vline(xintercept = Prm$Mu2, color = "blue") +
    labs(
      x = "Temperature", 
      y = "Densit??", 
      #colour = "Number of cylinders"
    )
  p3 <- p3 + theme_classic() 
  grid.arrange(p21,p1,p23,p2,p22, p3,ncol = 2)
  #for (year in Yrs) {
  #  year <- 2008
  #  p12 <- 
  #    x |>
  #    dplyr::filter(Year == year) |>
  #    hist_plot(P = TRUE)
    
    #p2anno <-
    #  Melange_2_gauss[[St]] |> 
    #  as.data.table() |> 
    #  dplyr::filter(Year == year)
    
  #  p12
  #  p22 <- 
  #    p22 +
  #    annotate("point", x = as.numeric(year), y = p2anno$Pro1, color = "black")+
  #    annotate("point", x = as.numeric(year), y = p2anno$Pro2, color = "black")
  #  
    
  #  grid.arrange(p1,p21,p12,p22,p23,ncol = 2)
    #P <- subplot(p1, p12,
    #        p21,p22, p31,nrows = 2) |> 
    #  layout(title = St,
    #         plot_bgcolor='#e5ecf6', 
    #         xaxis = list( 
    #           zerolinecolor = '#ffff', 
     #          zerolinewidth = 2, 
    #           gridcolor = 'ffff'), 
    #         yaxis = list( 
    #           zerolinecolor = '#ffff', 
    #           zerolinewidth = 2, 
    #           gridcolor = 'ffff'))
  #}
  #dev.off(dev.cur())
}
dev.off(dev.cur())




corr_2gaus <- X92st_list |> as.data.frame()
corr_2gaus$Corr_Mu1 <- NA
corr_2gaus$Corr_Mu2 <- NA
for (St in St_names) {
  x <- Melange_2_gauss[[St]]
  corr1 <- cor(x$Year, x$Mu1)
  corr2 <- cor(x$Year, x$Mu2)
  corr_2gaus[corr_2gaus$St_Name == St, "Corr_Mu1"] <- corr1
  corr_2gaus[corr_2gaus$St_Name == St, "Corr_Mu2"] <- corr2
}
corr_2gaus
outputs_path <- "output/Part 1"
save(corr_2gaus, file = paste(
  outputs_path, "/corr_2gaus.RData", sep = ""
))

corr_3gaus <- X92st_list |> as.data.frame()
corr_3gaus$Corr_Mu1 <- NA
corr_3gaus$Corr_Mu2 <- NA
corr_3gaus$Corr_Mu3 <- NA
for (i in 24:92) {
  x <- Melange_3_gauss[[i]]
  #x$Year <- as.numeric(x$Year)
  corr1 <- cor(x$Year, x$Mu1)
  corr2 <- cor(x$Year, x$Mu2)
  corr3 <- cor(x$Year, x$Mu3)
  St <- St_names[i]
  corr_3gaus[corr_3gaus$St_Name == St, "Corr_Mu1"] <- corr1
  corr_3gaus[corr_3gaus$St_Name == St, "Corr_Mu2"] <- corr2
  corr_3gaus[corr_3gaus$St_Name == St, "Corr_Mu3"] <- corr3
}
corr_3gaus |> head()
outputs_path <- "output/Part 1"
save(corr_3gaus, file = paste(
  outputs_path, "/corr_3gaus.RData", sep = ""
))


corr_4gaus <- X92st_list |> as.data.frame()
corr_4gaus$Corr_Mu1 <- NA
corr_4gaus$Corr_Mu2 <- NA
corr_4gaus$Corr_Mu3 <- NA
corr_4gaus$Corr_Mu4 <- NA
for (i in 1:92) {
  x <- Melange_3_gauss[[i]]
  corr1 <- cor(x$Year, x$Mu1)
  corr2 <- cor(x$Year, x$Mu2)
  corr3 <- cor(x$Year, x$Mu3)
  corr4 <- cor(x$Year, x$Mu4)
  
  St <- St_names[i]
  corr_4gaus[corr_4gaus$St_Name == St, "Corr_Mu1"] <- corr1
  corr_4gaus[corr_4gaus$St_Name == St, "Corr_Mu2"] <- corr2
  corr_4gaus[corr_4gaus$St_Name == St, "Corr_Mu3"] <- corr3
  corr_4gaus[corr_4gaus$St_Name == St, "Corr_Mu4"] <- corr4
}
corr_4gaus |> head()
outputs_path <- "output/Part 1"
save(corr_4gaus, file = paste(
  outputs_path, "/corr_4gaus.RData", sep = ""
))


# 63toronto  88St johns    45winnipeg    16whithorse   48churchil
# 27cambrig bay     8vancouver   66montreal     33calgary

Sw0c <- X92st_list$St_Name[c(16,45,48,63,88)]
Swt0c <- X92st_list$St_Name[c(8,27,33,66)]

S <- c(Sw0c, Swt0c)

x <- Melange_2_gauss[[Sw0c[1]]]
as.data.table(x)

library("data.table")
library(tidyverse) # it has ggplot2 package
library(cowplot) # it allows you to save figures in .png file
library(smplot2)
df <-
  melt.data.table(as.data.table(x),
                  id.vars = NULL,
                  measure.vars = c("Mu1", "Mu2"),
                  variable.name = "Class",
                  value.name = "Mu")

ggscatterstats(data = df, x = Year, y = Mu)

ggplot(seatbelts_long2, aes(x = Year, y = Mu)) + 
  geom_point(aes(color = Class), 
             na.rm = TRUE) +
  stat_cor
  
geom_smooth(aes(color = Class),
              method = "lm",
              formula = y ~ x,
              na.rm = TRUE) +

  
  geom_cor(
    mapping = aes(color = Class)
  )

plot_2_gauss <- list()
for (s in S) {
  s <- S[1]
  df <- Melange_2_gauss[[s]] |>
    as.data.table() |>
    melt.data.table(id.vars = NULL,
                    measure.vars = c("Mu1", "Mu2"),
                    variable.name = "Class",
                    value.name = "Mu")
  plot_2_gauss[[s]] <- df |>
    ggplot(aes(x = Year, y = Mu)) + 
    geom_point(aes(color = Class), 
               na.rm = TRUE) + 
    geom_smooth(aes(color = Class),
                method = "lm",
                formula = y ~ x,
                na.rm = TRUE) +
    labs(title = s) +
    theme(
      plot.title = element_text(hjust = .5,
                                size = 5),
      text = element_text(size = 5))
}
library(gridExtra)
grid.arrange(plot_2_gauss[[1]],plot_2_gauss[[2]],plot_2_gauss[[3]],
             plot_2_gauss[[4]],plot_2_gauss[[5]],plot_2_gauss[[6]],
             plot_2_gauss[[7]],plot_2_gauss[[8]],plot_2_gauss[[9]],
             ncol = 2, nrow = 5)


plot_2_gauss <- list()
for (St in St_names) {
  Melange_2_gauss[[St]]$Year <- Melange_2_gauss[[St]]$Year |>
    as.numeric()
  plot_2_gauss[[St]] <- Melange_2_gauss[[St]] |>
    ggplot() +
    geom_point(aes(x = Year, y = Mu1, color = "1")) +
    geom_point(aes(x = Year, y = Mu2, color = "2")) +
    labs(x = "Year", y = "Mu") +
    labs(title = "Two gaussian mixture") +
    theme1
}
pdf("output/Part 1/Melange_2_gauss.pdf", width = 30, height = 20)
plot_2_gauss[[1]]
dev.off(dev.cur())

Melange_2_gauss[[1]] |> head()
S <- names(Melange_2_gauss)
y <- Melange_2_gauss[[1]]

L <- reshape(
  y,
  varying = c("Mu1", "Mu2"),
  v.names = "Mu",
  times = seq(1953,2013),
  timevar = "Year",
  direction = "long"
)

get_results1 <- function(data_list){
  data_list <- y
  m <- length(data_list)
  S <- names(data_list)
  d <- 4#ncol(data_list[[1]]) - 2
  n <- nrow(data_list)
  
  for (s in S) {
    df <- matrix(nrow = d*n, ncol = 4) |>
      as.data.frame()
    colnames(df) <- c("Year", "Temperature", "Season", "Station")
    for (i in 1:d) {
      i <- 1
      j <- (i - 1)*n + 1
      df[j:(i*n),1:2] <- y[,c(1,i+2)]
      df[j:(i*n), "Season"] <- colnames(y)[i+2]
      df[j:(i*n), "Station"] <- S[1]
    } 
  }
}

resh_df <- function(df){
  d <- ncol(df) - 2
  n <- nrow(df)
  df <- matrix(nrow = d*n, ncol = 4) |>
    as.data.frame()
  colnames(df) <- c("Year", "Temperature", "Season", "Station")
  for (i in 1:d) {
    j <- (i - 1)*n + 1
    df[j:(i*n),1:2] <- y[,c(1,i+2)]
    df[j:(i*n), "Season"] <- colnames(y)[i+2]
  }
}

get_results1(Melange_2_gauss)

ggplot(y) +
  geom_point(aes(x = Year, y = Mu1, color = Gender), 
             na.rm = TRUE,
             position = "jitter") +
  geom_smooth(aes(x = Systolic, y = Diastolic),
              method = "loess",
              formula = y ~ x,
              na.rm = TRUE)

Melange_2_gauss[[70]] |>
  ggplot() +
  geom_line(aes(x = Year, y = Mu1, color = "Mu1")) +
  geom_line(aes(x = Year, y = Mu2, color = "Mu2")) 
  labs(x = "Year", y = "Mu") +
  labs(title = "Two gaussian mixture") 

Mu_sepd2[[3]] |>
  ggplot() +
  geom_line(aes(x = Year, y = Mu1, color = "Mu1")) +
  geom_line(aes(x = Year, y = Mu2, color = "Mu2"))

demo_bpx <- all.Stations.Data[[1]] |> 
  get_Seasons(sepd2)
ggplot(demo_bpx) +
  geom_bar(aes(x = Temp, fill = Race), 
           position = "dodge")

Melange_3_gauss[[1]] |>
  ggplot() +
  geom_line(aes(x = Year, y = Mu1, color = "Mu1")) +
  geom_line(aes(x = Year, y = Mu2, color = "Mu2")) +
  geom_line(aes(x = Year, y = Mu3, color = "Mu3"))

Mu_sepd3[[2]] |>
  ggplot() +
  geom_line(aes(x = Year, y = Mu1, color = "Mu1")) +
  geom_line(aes(x = Year, y = Mu2, color = "Mu2")) +
  geom_line(aes(x = Year, y = Mu3, color = "Mu3"))


Melange_4_gauss[[10]] |>
  ggplot() +
  geom_line(aes(x = Year, y = Mu1, color = "Mu1")) +
  geom_line(aes(x = Year, y = Mu2, color = "Mu2")) +
  geom_line(aes(x = Year, y = Mu3, color = "Mu3")) +
  geom_line(aes(x = Year, y = Mu4, color = "Mu4"))

Mu_sepd4[[10]] |>
  ggplot() +
  geom_line(aes(x = Year, y = Mu1, color = "Mu1")) +
  geom_line(aes(x = Year, y = Mu2, color = "Mu2")) +
  geom_line(aes(x = Year, y = Mu3, color = "Mu3")) +
  geom_line(aes(x = Year, y = Mu4, color = "Mu4"))




x <- lm_melange_3gaus[[3]]$Mu3 |> summary()
x$coefficients 

lmm <- lm(Mu1 ~ Year, data = Mu_sep[[2]])
lmm |> summary()
lmm$residuals |> plot()
qqplot(lmm$residuals, lmm$df.residual)

plot(Mu_sep[[2]]$Mu1,residuals(lmm))
abline(h=0)

residuals(lmm) |> hist()







##########################


df <- 
  "data/92st/df.csv" |>
  fread()
Years <- df$Date |>
  format("%Y")

df <- df |>
  add_column(Year = Years) |>
  melt.data.table(id.vars = c("Date", "Year"),
                  measure.vars = St_names,
                  variable.name = "Station",
                  value.name = "Temp")

x <-
  df |> 
  dplyr::filter(Station == St_names[2]) |> 
  na.omit() |> 
  dplyr::filter(Temp > -100)

sepd2 <- matrix(
  c("09-21", "03-20", 
    "03-21", "09-20"),
  ncol = 2, nrow = 2, byrow = T
)

get_Seasons(x, sepd2)


n <- 50

win_sep <- list()
for (St in St_names[47:92]) {
  t1 <- Sys.time()
  print(St)
  x <-
    df |> 
    dplyr::filter(Station == St) |> 
    na.omit() |> 
    dplyr::filter(Temp > -100)
  
  sp1 <- "2000" |> paste(
    "-", sepd2[1,], sep = ""
  ) |> as.Date()
  sp2 <- "2000" |> paste(
    "-", sepd2[2,], sep = ""
  ) |> as.Date()
  
  mu <- matrix(nrow = n, ncol = 3) |> 
    as.data.frame()
  colnames(mu) <- c("j", "m1", "m2")
  mu[,"j"] <- seq(1,n)
  for (j in 20:n) {
    print(j)
    #sp1 <- sp1 + 1
    #sp2 <- sp2 + 1
    sp <- matrix(c(
      substr(sp1 + j, 6, 10),
      substr(sp2 + j, 6, 10)
    ),
    ncol = 2, nrow = 2, byrow = T)
    print(sp)
    y <- get_Seasons(x, sp)
    m1 <- y[y$Season == "S1", "Temp"] |> mean()
    m2 <- y[y$Season == "S2", "Temp"] |> mean()
    mu[j,c("m1", "m2")] <- c(m1, m2)
  }
  mu[,"diff"] <- mu[,"m2"] - mu[,"m1"]
  t2 <- Sys.time()
  print(t2 - t1)
  win_sep[[St]] <- mu
}
outputs_path <- "output/Part 1"
save(win_sep, file = paste(
  outputs_path, "/win_sep.RData", sep = ""
))

load("output/Part 1/win_sep.RData")
v <- c()
for (j in 1:92) {
  v <- c(v,
         which.max(win_sep[[j]]$diff))
}

sp1
sp2

sepdates <- X92st_list |> 
  select(
    St_Name, PR, Lat, Long, X, Y
  ) |> 
  add_column(
    sph1 = (sp1[1] + v) |> substr(6, 10),
    sph2 = (sp1[2] + v) |> substr(6, 10),
    spe1 = (sp2[1] + v) |> substr(6, 10),
    spe2 = (sp2[2] + v) |> substr(6, 10)
  )

St <- St_names[1]
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

v |> hist()
vv <- (sp1[1] + v) |> as.Date() |> as_tibble() |> 
  add_column(
    St = St_names,
    X = X92st_list$X,
    Y = X92st_list$Y,
    lat = X92st_list$Lat,
    long = X92st_list$Long
  )
vv$Date <- vv$value |> substr(6,10)
vvv <- vv |> group_by(value) |> add_count(value)
g <- vvv |> ggplot(aes(x = value)) 
g + geom_histogram(aes(y = stat(count)),
                   alpha = .5,
                   colour= "black", 
                   fill= "grey90") +
  theme_classic()

g1 <- sepdates |> ggplot()
g1 + geom_text(aes(x = X, y = Y, label = paste(sph1)),
               size = 3) +
  theme_classic()

g1 + geom_text(aes(x = X, y = Y, label = sph1)) +
  theme_classic()

