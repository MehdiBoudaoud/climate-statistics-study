source("scripts/functions.R")

library(readxl)
X92st_list <- read_excel("data/92st_list.xlsx")
#View(X92st_list)

St_Ids <- X92st_list$stnid
X92st_list$St_Name[c(65,66,67)] <- c(
  "QUEBEC_JEAN LESAGE INTL A",
  "MONTREAL_PIERRE ELLIOTT TRUDEAU INTL A",
  "MONTREAL_MIRABEL INT'L A"
)
St_names <- X92st_list$St_Name
St_names

for (St_name in St_names[64:92]) {
  ID <- X92st_list[X92st_list$St_Name == St_name, "stnid"]
  print(St_name)
  t1 <- Sys.time()
  df <- process_data(ID)
  t2 <- Sys.time()
  print(t2 - t1)
  #eval(parse(text=paste0(St_name,"=df")))
  file_path <- "data/92st/92st_RData/" |>
    paste(St_name, ".csv", sep = "")
  write.csv(df,
            file = file_path)
  #x92st_data[[St_name]] <- df
}


x92st_data <- list()
j <- 0
for (St_name in St_names) {
  j <- j + 1
  print(paste(j, St_name))
  t1 <- Sys.time()
  file_p <- paste("data/92st/92st_RData/", St_name, ".csv", sep = "")
  St.Df <- read.csv(file = file_p, header = T)
  St.Df <- St.Df[,-1]
  addCols <- c("Year", "Month", "Day", "Time")
  St.Df[,addCols] <- cbind(
    substr(St.Df[,1], 1, 4),
    substr(St.Df[,1], 6, 7),
    substr(St.Df[,1], 9, 10),
    substr(St.Df[,1], 12, 16)
  )
  
  x92st_data[[St_name]] <- St.Df[St.Df$Temp > -100,]
  t2 <- Sys.time()
  print(t2 - t1)
}

file_path <- "data/92st/92st_RData/x92_data.RData"
#save(x92st_data, file = file_path)
load(file_path)


subset_Station <- read.csv("data/subset_Station Inventory EN-3.csv",
                           header = T)
subset_Station$Name[c(26, 27)] <- c("QUEBEC_JEAN LESAGE INTL A", 
                                    "MONTREAL_PIERRE ELLIOTT TRUDEAU INTL A")
province <- data.frame(prov1 = unique(subset_Station$Province),
                       prov2 = c("BC", "AB", "SK", "MB", "ON",
                                 "QC", "NB", "NS", "PE", "NL"))

StsNames <- subset_Station$Name
StsNames |> length()

load(file = "output/processed/ProceData1.RData")

all.Stations.Data[[3]]$Temp..A.C. |> which_na() |> length()
l <- all.Stations.Data[[3]]$Temp..A.C. |> which_na()
all.Stations.Data[[4]]$Temp..A.C. |> plot(type = "l")
all.Stations.Data[[1]]$Date.Time..LST.[l] |> range()



## Load all variables
all.Stations.Data.full <- list()
j <- 0
for (St in StsNames) {
  j <- j + 1
  print(
    paste(j, " - Importing station : ", St, sep = "")
  )
  Pr <- subset_Station[subset_Station$Name == St,
  ]$Province
  Pr <- province[province$prov1 == Pr,]$prov2
  path <- "data/Loaded Data"
  path <- path |> 
    paste("/", Pr, "/", St, ".csv", sep = "")
  St.Df <- path |>
    read.csv(header = TRUE)
  #St.Df <- all.Stations.Data.full[[St]]
  St.Df <- St.Df[,c(5, 6, 8, 10, 12, 14, 16, 18, 22, 24)]
  addCols <- c("Year", "Month", "Day", "Time")
  St.Df[,addCols] <- cbind(
    substr(St.Df[,1], 1, 4),
    substr(St.Df[,1], 6, 7),
    substr(St.Df[,1], 9, 10),
    substr(St.Df[,1], 12, 16)
  )
  St.Df[,1] <- St.Df[,1] |>
    as.Date()
  
  all.Stations.Data.full[[St]] <- St.Df
}
St.Df |> rm()
save(all.Stations.Data.full, file = "output/processed/ProceDataFull1.RData")

## Load Temperatures only
all.Stations.Data <- list()
j <- 0
for (St in StsNames) {
  j <- j + 1
  print(
    paste(j, " - Importing station : ", St, sep = "")
  )
  Pr <- subset_Station[subset_Station$Name == St,
                       ]$Province
  Pr <- province[province$prov1 == Pr,]$prov2
  path <- "data/Loaded Data"
  path <- path |> 
    paste("/", Pr, "/", St, ".csv", sep = "")
  St.Df <- path |>
    read.csv(header = TRUE)
  St.Df <- all.Stations.Data[[St]]
  St.Df <- St.Df[,c(5, 6)]
  addCols <- c("Year", "Month", "Day", "Time")
  St.Df[,addCols] <- cbind(
    substr(St.Df[,1], 1, 4),
    substr(St.Df[,1], 6, 7),
    substr(St.Df[,1], 9, 10),
    substr(St.Df[,1], 12, 16)
  )
  St.Df[,1] <- St.Df[,1] |>
    as.Date()
    
  all.Stations.Data[[St]] <- St.Df |> na.omit()
}
St.Df |> rm()
save(all.Stations.Data, file = "output/processed/ProceData1.RData")



library(timeSeries)
library(naniar)
library(mclust)
library(ggplot2)
library(gridExtra) 
library(tidyverse)

TYPES <- c("ORGL", "CTRL", "PGW")

B1 <- list(b1 = c("06-15", "09-30"),
           b2 = c("10-01", "06-14"))

B2 <- list(b1 = c("06-09", "09-06"),
           b2 = c("09-07", "06-10"))

B3 <- list(b1 = c("05-22", "09-18"),
           b2 = c("09-19", "05-23"))




Ss <- list(s2 = c("C	tC)"),
           s4 = c("Hiver"))



Stati <- subset_Station$Name
Stati[26] <- "QUEBEC_JEAN LESAGE INTL A"
Stati[27] <- "MONTREAL_PIERRE ELLIOTT TRUDEAU INTL A"

p <- 2
for (p in 1:10) {
  Stations <- subset_Station[subset_Station$Province == province$prov1[p],1]
  #Stations <- subset_Station$Name
  S <- length(Stations)
  source("scripts/functions.R")
  #S <- seq(1, 39)
  B <- B3
  file <- province$prov2[p] |> 
    paste("-juli_sep.pdf", sep = "")
  
  pdf(file = paste("output/Graph 3/Mean/", file, sep = ""), width = 50, height = 30)
  
  #pdf(file = "3 mois_sep.pdf", width = 50, height = 30)
  #pdf(file = "4 mois_sep.pdf", width = 50, height = 30)
  
  PGW <- matrix(nrow = 0, ncol = 4) |>
    as.data.frame()
  colnames(PGW) <- c('Year', 'Station', 'Max', 'Min')
  
  CTRL <- matrix(nrow = 0, ncol = 4) |>
    as.data.frame()
  colnames(CTRL) <- c('Year', 'Station', 'Max', 'Min')
  
  for (s in 1:S) {
    hist_sep <- list()
    MU <- list()
    mu_plot1 <- list()
    mu_plot2 <- list()
    
    if(Stations[s] == "OTTAWA MACDONALD-CARTIER INT'L A") next
    
    
    
    
    for (TYPE in TYPES) {
      j <- which(TYPES == TYPE)
      
      df.h <- read_data(Stations[s],
                        TYPE = TYPE)
      
      y <- df.h |>
        mulSaisModl2(B, Ss, Stations[s])
      
      df.h <- y[[1]]
      #parm <- c(y[[2]][[1]]$parameters$mean,
      #          y[[2]][[2]]$parameters$mean)
      parm <- c(y[[2]][[1]], y[[2]][[2]])
      hist_sep[[j]] <- ggplot(df.h,
                              aes(x = Temp, color = Saison)) +
        geom_histogram(aes(y = stat(density)), 
                       colour="black", fill="white") +
        geom_density(alpha = .05, fill = "#FF666665")  +
        geom_density(alpha = .01) +
        
        labs(x = "TempC)rature", y = "DensitC)") +
        labs(title = Stations[s],
             subtitle = paste(TYPE)) + #, "   C)tC)",
        #B[[i]][[1]], "-", B[[i]][[2]])) +
        theme(
          plot.title = element_text(#color = "blue", 
            size = 25, 
            face = "bold",
            hjust = .1),
          plot.subtitle = element_text(#color = "darkblue",
            size = 15,
            hjust = .1),
          text = element_text(size = 15)
        ) +
        geom_vline(xintercept = parm,
                   color = c("#F8766D", "#00BFC4")) +
        annotate(geom = "text", x = parm, y = c(.01, .025), 
                 label = round(parm, digits = 1), 
                 hjust = "midle", size = 5,
                 color = c("#F8766D", "#00BFC4"))
      
      mu <- matrix(nrow = 12, ncol = 2)
      for (i in 1:12) {
        st <- paste("200", i, "-01-01", sep = "")
        if(i > 9){
          st <- paste("20", i, "-01-01", sep = "")
        }
        
        en <- paste("200", i, "-12-31", sep = "")
        if(i > 9){
          en <- paste("20", i, "-12-31", sep = "")
        }
        print(c(st, en))
        
        
        #df.h <- df |> window(st, en)
        
        df.h <- read_data(Stations[s],
                          TYPE = TYPE,
                          DATES = c(st, en))
        y <- df.h |>
          mulSaisModl2(B, Ss, Stations[s])
        df.h <- y[[1]]
        #mu[i,] <- c(y[[2]][[1]]$parameters$mean,
        #            y[[2]][[2]]$parameters$mean)
        mu[i,] <- c(y[[2]][[1]], 
                    y[[2]][[2]])
      }
      Parm <- data.frame(ind = seq(2001, 2012),
                         Temp = mu[,1],
                         Saison = rep("Ete", 12))
      Parm <- Parm |> 
        rbind(data.frame(ind = seq(2001, 2012),
                         Temp = mu[,2], 
                         Saison = rep("Hiver", 12))) |>
        rbind(data.frame(ind = seq(2001, 2012),
                         Temp = c(mu[,1] - mu[,2]), 
                         Saison = rep("Diff", 12)))
      
      mu <- Parm[Parm$Saison != "Diff",]
      mu_plot1[[j]] <- ggplot(mu, aes(x = ind, y = Temp,
                                      color = Saison)) + 
        geom_line() +
        geom_point() +
        labs(x = "AnnC)e", y = "TempC)rature") +
        labs(title = paste("Moyennes estimC)es:  ", TYPE)) +
        theme(
          plot.title = element_text(#color = "blue", 
            size = 25, 
            face = "bold",
            hjust = .1),
          plot.subtitle = element_text(#color = "blue",
            size = 15,
            hjust = .1),
          text = element_text(size = 15)
        ) 
      #  scale_color_manual(values = colors)
      
      dif <- Parm[Parm$Saison == "Diff",]
      mu_plot2[[j]] <- ggplot(dif, aes(x = ind, y = Temp)) +
        geom_line() + 
        geom_point() +
        #ylim(5, 15) + 
        labs(x = "AnnC)e", y = "TempC)rature") +
        labs(title = paste("DiffC)rence des moyennes estimC)es:  ", TYPE)) +
        theme(
          plot.title = element_text(#color = "blue", 
            size = 25, 
            face = "bold",
            hjust = .1),
          plot.subtitle = element_text(#color = "darkblue",
            size = 15,
            hjust = .1),
          text = element_text(size = 15)
        )
      
      MU[[j]] <- Parm
    }
    
    Mu <- get_mu2(MU)
    Muu <- get_mu3(Mu) 
    
    pgw <- data.frame(Year = seq(2001, 2012))
    pgw$Station <- Stations[s]
    pgw$Max <- Muu[[1]][Muu[[1]]$Type == "PGW - ORGL",]$Temp
    pgw$Min <- Muu[[2]][Muu[[2]]$Type == "PGW - ORGL",]$Temp
    PGW <- PGW |> rbind(pgw)
    
    ctrl <- data.frame(Year = seq(2001, 2012))
    ctrl$Station <- Stations[s]
    ctrl$Max <- Muu[[1]][Muu[[1]]$Type == "CTRL - ORGL",]$Temp
    ctrl$Min <- Muu[[2]][Muu[[2]]$Type == "CTRL - ORGL",]$Temp
    CTRL <- CTRL |> rbind(ctrl)
    
    
    
    MU_plot <- list()
    Diff_plot <- list()
    TITLES <- c('Tmax (C	tC))', 'Tmin (Hiver)', 'T_diff (C	tC) - Hiver)')
    for (j in 1:3) {
      MU_plot[[j]] <- ggplot(Mu[[j]], aes(x = ind,
                                          y = Temp,
                                          color = Type)) + 
        geom_line() +
        geom_point() +
        #geom_line(aes(y = CTRL), color = "green") +
        #geom_line(aes(y = PGW), color = "red") +
        labs(x = "AnnC)e", y = "TempC)rature") +
        labs(title = paste("Moyennes estimC)es:  ", TITLES[j])) +
        #scale_color_manual(values = colors) +
        theme(
          plot.title = element_text(#color = "blue", 
            size = 25, 
            face = "bold",
            hjust = .1),
          plot.subtitle = element_text(#color = "darkblue",
            size = 15,
            hjust = .1),
          text = element_text(size = 15)
        )
      
      Diff_plot[[j]] <- ggplot(Muu[[j]], aes(x = ind,
                                             y = Temp,
                                             color = Type)) + 
        geom_line() +
        geom_point() +
        #  geom_line(aes(y = CTRL - ORGL), color = "green") +
        #  geom_line(aes(y = PGW - ORGL), color = "red") + 
        labs(title = paste("Moyennes estimC)es:  ", TITLES[j]),
             x = "AnnC)es", y = "T") +
        #  scale_colour_manual("", 
        #                      breaks = c("CTRL - ORGL", "PGW - ORGL"),
        #                      values = c("red", "green")) +
        theme(
          plot.title = element_text(#color = "blue", 
            size = 25, 
            face = "bold",
            hjust = .1),
          plot.subtitle = element_text(#color = "darkblue",
            size = 15,
            hjust = .1),
          text = element_text(size = 15)
        )
    }
    
    p1 <- "output/Graph 3/Mean/"
    p2 <- province$prov2[p]
    p3 <- paste(p1, p2, sep = "")
    logic <- dir.exists(p3)
    
    if(logic == FALSE){
      dir.create(p3)
    }
    
    
    
    #file <- Stati[s] |>
    #  paste(".png", sep = "")
    
    #png(file = file, width = 2000, height = 1400)
    grid.arrange(hist_sep[[1]], mu_plot1[[1]], mu_plot2[[1]],
                 hist_sep[[2]], mu_plot1[[2]], mu_plot2[[2]],
                 hist_sep[[3]], mu_plot1[[3]], mu_plot2[[3]],
                 MU_plot[[1]], MU_plot[[2]], MU_plot[[3]],
                 Diff_plot[[1]], Diff_plot[[2]], Diff_plot[[3]],
                 ncol = 3, nrow = 5)
    #dev.off(dev.cur())
  }
  
  
  file1 <- "PGW" |>
    paste(province$prov2[p], ".csv", sep = "")
  
  file2 <- "CTRL" |>
    paste(province$prov2[p], ".csv", sep = "")
  
  write.csv(PGW, file = paste("output/Graph 3/Mean/", file1, sep = ""))
  write.csv(CTRL, file = paste("output/Graph 3/Mean/", file2, sep = ""))
  
  dev.off(dev.cur())
}


d <- density(df.h[df.h$Saison == "C	tC)", ]$Temp)
d <- data.frame(x = d$x,
                y = d$y)
ggplot(d, aes(x = x, y = y)) +
  geom_line()

stn <- Stations[1]
d1 <- read_data(stn,
                TYPE = TYPES[1])
d2 <- read_data(stn,
                TYPE = TYPES[2])
d3 <- read_data(stn,
                TYPE = TYPES[3])

d1$Type <- TYPES[1]
d2$Type <- TYPES[2]
d3$Type <- TYPES[3]

d <- rbind(d1,d2,d3)
d <- d |> df.Seasonal()

ggplot(d, aes(x = Temp, color = Type)) +
  #geom_histogram(aes(y = stat(density)), 
  #               fill="white") +
  geom_density(alpha = .1, fill = "#FF666665")

geom_histogram(aes(x = Temp),
               bins = 50,
               fill = 'lightgrey')

ind <- which.max(d$y)
d$x[ind]

x1 <- data.frame(x = seq(1, 100),
                 y = rnorm(100, 0, 1))
x1$Group <- "X1"

x2 <- data.frame(x = seq(1, 100),
                 y = rnorm(100, 1, 1))
x2$Group <- "X2"

df <- rbind(x1, x2)
df

library(latex2exp)
library(ggplot2)

ggplot(df, aes(x = x, y = y, color = Group)) +
  geom_line() +
  labs(title = TeX("Graphique $X_j$"))











ggplot(Mu[[3]], aes(x = X4)) + 
  geom_line(aes(y = X1), color = "blue") +
  geom_line(aes(y = X2), color = "green") +
  geom_line(aes(y = X3), color = "red")

get_mu(MU)
df <- merge_data(Stations[1], TYPES, B, Ss)
df

hist(df$ORGL, col='red', xlim=c(-35, 100))
hist(df$CTRL, col='green', add=TRUE)
hist(df$PGW, col='blue', add=TRUE)

library(tidyr)
library(ggplot2)

df %>% 
  gather(value=ORGL) %>% 
  ggplot(aes(x=ORGL, fill=ORGL)) + 
  geom_histogram(position="dodge")

# Simply use fill = a vector of colors
p <- ggplot(df, aes(x = CTRL)) +
  geom_bar() + 
  ylab("Count") + 
  ggtitle("Specify manual colors in a bar chart")

ggplotly(p)
