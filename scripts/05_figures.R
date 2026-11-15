library(gridExtra)
library(data.table)
library(dplyr)
library(tidyverse)
library(readxl)


## Images saving

source("scripts/functions.R")
load("output/Part 1/thrTC.RData")


X92st_list <- read_excel("data/92st_list.xlsx")
#View(X92st_list)
St_Ids <- X92st_list$stnid
X92st_list$St_Name[c(65,66,67)] <- c(
  "QUEBEC_JEAN LESAGE INTL A",
  "MONTREAL_PIERRE ELLIOTT TRUDEAU INTL A",
  "MONTREAL_MIRABEL INT'L A"
)
St_names <- X92st_list$St_Name


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

for (St in St_names) {
  #St <- St_names[1]
  x <- df |> 
    dplyr::filter(Station == St) |> 
    na.omit() |> 
    dplyr::filter(Temp > -100) 
  x$Date |> range() |> print()
}

x <-
  df |> 
  dplyr::filter(Station == St_names[1]) |> 
  na.omit() |> 
  dplyr::filter(Temp > -100)

for (St in St_names) {
  print(St)
  x <-
    df |> 
    dplyr::filter(Station == St_names[1]) |> 
    na.omit() |> 
    dplyr::filter(Temp > -100)
}
#b2182b
#ef8a62
#fddbc7
#d1e5f0
#67a9cf
#2166ac

colors_set <- c(
  "#67001f",
  "#b2182b",
  "#d6604d",
  "#f4a582",
  "#fddbc7",
  "#d1e5f0",
  "#92c5de",
  "#4393c3",
  "#2166ac",
  "#053061"
)

###-----------------------------------------
###  Chapter 1
###-----------------------------------------



## Histogram estimation, width = 1,2,4
bw <- c(1,2,4)
p1 <- 
  x |> 
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

p1 + theme_classic() 

p2 <- 
  x |> 
  ggplot(aes(x = Temp)) +
  geom_histogram(aes(y = stat(density)),
                 alpha = .5,
                 colour="black", 
                 fill="white",
                 binwidth = 1) +
  labs(
    x = "Temperature", 
    y = "Frequence", 
    #colour = "Number of cylinders"
  )
p2 + theme_classic()

gridExtra::grid.arrange(p1,p2)
subplot(p1, p2, shareY = TRUE)


## Kernel estimation, width = 1,2,4
kern <- c("gaussian", "rectangular", "epanechnikov")

d1 <- x |> ggplot(aes(x = Temp)) +
  geom_density(kernel = kern[1],
               adjust = 2)
d1  

d2 <- x |> ggplot(aes(x = Temp)) +
  geom_density(kernel = kern[2],
               adjust = 2)
d2

d3 <- x |> ggplot(aes(x = Temp)) +
  geom_density(kernel = kern[3],
               adjust = 2)
d3
subplot(d1, d2, d3, shareY = TRUE)
gridExtra::grid.arrange(d1,d2,d3)
a <- arrangeGrob(d1,d2)
a |> plot()


library(grid)
grid.arrange(rectGrob(), rectGrob())
## Not run:  
library(ggplot2)
pl <- lapply(1:11, function(.x) qplot(1:10, rnorm(10), main=paste("plot", .x)))
ml <- marrangeGrob(pl, nrow=2, ncol=2)
## non-interactive use, multipage pdf
ggsave("output/multipage.pdf", ml)
## interactive use; open new devices
ml
pl


## Gaussian mixture model
load(file = "output/Part 1/melange_2_gauss.RData")

mu1 <- c()
mu2 <- c()
for (St in St_names) {
  Melange_2_gauss[[St]]$Year <- Melange_2_gauss[[St]]$Year |> as.numeric()
  x <- Melange_2_gauss[[St]] |> 
    dplyr::filter(Year >= 1981 & Year <= 2012)
  mu1 <- c(mu1,
           mean(x$Mu1))
  mu2 <- c(mu2,
           mean(x$Mu2))
}


df <- X92st_list |> 
  add_column(
    Mu1 = mu1,
    Mu2 = mu2,
    Ecart = mu2 - mu1
  )

x <- Melange_2_gauss[[St]]
x |> summary()
Prm <- x[x$Year == 1961,]
f <- x |> as_tibble() |> 
  pivot_longer(cols = Mu1:Var2, names_to = c(".value", "Class"), names_pattern = "(.)(.)")


d <- data.frame(x = seq(-40,40,0.5))
d$y <- d$x |> fun(Prm)
d |> ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = Prm$Mu1) +
  geom_vline(xintercept = Prm$Mu2) +
  


fun(-40:20,Prm)



### Maps 

wmap <- map_data("world")
ca <- wmap |> dplyr::filter(region == "Canada")
worldplot <- ggplot()+
  geom_polygon(data=ca, aes(x=long, y=lat), fill = "white")+
  coord_fixed(1.3) +
  geom_point(aes(x = X92st_list$Long, y = X92st_list$Lat, color = X92st_list$PR))

x <- X92st_list |> 
  add_column()

mp <- ca |> ggplot() +
  geom_path(aes(x = long, y = lat, group = group),
            color = "grey") +
  geom_point(data = df, aes(x = Long, y = Lat, color = Ecart), size = 2) 

ggplotly(mp)
