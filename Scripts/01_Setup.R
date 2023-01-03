##### Load libraries and data -----------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(grid)
library(gridBase)
library(lme4)
library(survival)
library(sjPlot)
library(sjmisc)

# Load data from local copy of CSV; handle NAs
Data <- read.csv("Data/SeedRemovalData.csv")
Data_SM <- read.csv("Data/SeedMassData.csv")
names(Data)[1] <- "Depot"
Data <- Data[-29, ]
Data[25, "t_24"] <- 14
Data[45, "t_24"] <- 15
Data[64, "t_7.5"] <- 23
Data[64, "t_11"] <- 10

# Create copy of data with treatments and only a few key time points
Data2 <- Data[, c(1:5, 18, 30, 31, 33)]

# Sort seed removal data into the eight different treatment groups
Data_CN_YW_YE <- subset(Data, Species == "CN" & Warmed == 1 & Elaiosome == 1)
Data_CN_YW_NE <- subset(Data, Species == "CN" & Warmed == 1 & Elaiosome == 0)
Data_CN_NW_YE <- subset(Data, Species == "CN" & Warmed == 0 & Elaiosome == 1)
Data_CN_NW_NE <- subset(Data, Species == "CN" & Warmed == 0 & Elaiosome == 0)
Data_CA_YW_YE <- subset(Data, Species == "CA" & Warmed == 1 & Elaiosome == 1)
Data_CA_YW_NE <- subset(Data, Species == "CA" & Warmed == 1 & Elaiosome == 0)
Data_CA_NW_YE <- subset(Data, Species == "CA" & Warmed == 0 & Elaiosome == 1)
Data_CA_NW_NE <- subset(Data, Species == "CA" & Warmed == 0 & Elaiosome == 0)

# Sort seed mass data into warmed versus unwarmed groups for each species
Data_SM_CN_W <- subset(Data_SM, Species == "CN" & Warmed == 1)
Data_SM_CN_NW <- subset(Data_SM, Species == "CN" & Warmed == 0)
Data_SM_CA_W <- subset(Data_SM, Species == "CA" & Warmed == 1)
Data_SM_CA_NW <- subset(Data_SM, Species == "CA" & Warmed == 0)





##### Define plotting functions ---------------------------------------------------------------------------

# Functions to take average number (and SD and SEM) of seeds remaining at a given time
na.mean <- function(x){
  mean(x)}
na.sd <- function(x){
  sd(x)}
na.sem <- function(x){
  na.sd(x)/sqrt(length(x))}

# Function to apply above functions to each treatment group
time.means <- function(df){
  df <- select(df, -Block)
  df_means <- apply(df[, 5:ncol(df)], MARGIN = 2, FUN = na.mean)
  df_sd <- apply(df[, 5:ncol(df)], MARGIN = 2, FUN = na.sd)
  df_sem <- apply(df[, 5:ncol(df)], MARGIN = 2, FUN = na.sem)
  df_new <- data.frame(cbind(c(seq(0, 12, by = 0.5), 24, 36, 48), df_means, df_sem))
  names(df_new) <- c("Time", "Mean", "SEM")
  return(df_new)}

# Function to evaluate whether two survival curves are different
# Use 2-sided K-S test, with code adapted from ks.test function
# Set function environment to "stats" for calling internally-compiled code
time.ks <- function(df1, df2){
  df1 <- select(df1, -Block)
  df2 <- select(df2, -Block)
  df_means1 <- apply(df1[, 5:ncol(df1)], MARGIN = 2, FUN = na.mean)
  df_means2 <- apply(df2[, 5:ncol(df2)], MARGIN = 2, FUN = na.mean)
  D <- max(abs((1 - df_means1/25) - (1 - df_means2/25)))
  n <- length(5:ncol(df1))
  pkstwo <- function(x, tol = 1e-06, D = D){
    p <- rep(0, length(x))
    i <- which(x > 0)
    if(length(i)){
      p[i] <- .Call(C_pKS2, p = x[i], tol)}
    return(p)}
  pval <- min(1, max(0, 1 - pkstwo(sqrt(n)*D)))
  return(pval)}
environment(time.ks) <- asNamespace("stats")

# Get vector of plotting colours with transparency
c.alpha <- function(colour){
  colRGB <- col2rgb(colour)
  newCol <- rgb(colRGB[1, ], colRGB[2, ], colRGB[3, ], alpha = 145, maxColorValue = 255)
  return(newCol)}
PlotColours <- sapply(c("darkgreen", "green", "blue", "red", "black", "grey"), c.alpha)
PlotColours_All <- sapply(c("black", "darkblue", "dodgerblue", "darkorchid4",
                            "purple", "red", "orange", "yellow2"), c.alpha)

# Function to plot survival curves
surv.plots <- function(df1, df2, colour1, colour2, bottom, left, atext){
  
  # Get p-value from K-S test
  pval <- time.ks(df1, df2)
  pval <- ifelse(pval < 0.001, "italic(p) < 0.001",
                 paste("paste(italic(p), \" =\")",
                       paste0("\"", sprintf("%0.3f", round(pval, 3)), "\""), sep = "~"))
  
  # Plot survival curves
  ggplot() +
    geom_point(data = time.means(df1), aes(x = Time, y = Mean), colour = colour1, size = 0.01) +
    geom_line(data = time.means(df1), aes(x = Time, y = Mean), colour = colour1, size = 0.25) +
    geom_errorbar(data = time.means(df1), 
                  aes(x = Time, ymin = Mean - SEM, ymax = Mean + SEM), width = 0.3, 
                  colour = colour1, size = 0.2) +
    geom_point(data = time.means(df2), aes(x = Time, y = Mean), colour = colour2, size = 0.01) +
    geom_line(data = time.means(df2), aes(x = Time, y = Mean), colour = colour2, size = 0.25) +
    geom_errorbar(data = time.means(df2), 
                  aes(x = Time, ymin = Mean - SEM, ymax = Mean + SEM), width = 0.3, 
                  colour = colour2, size = 0.2) +
    coord_cartesian(ylim = c(0, 25)) +
    scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 48), 
                       breaks = c(seq(1, 12, by = 0.5), 24, 36, 48),
                       labels = c(0, rep("", 21), 12, 24, 36, 48)) +
    annotate("text", x = 47.7, y = 25, label = atext, hjust = 1, size = 1.3) +
    annotate("text", x = 47.7, y = 23.4, label = pval, hjust = 1, size = 1.3, parse = TRUE) +
    xlab("Time (Hours)") +
    ylab("Seeds Remaining") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "gray90", size = 0.2),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(size = 3.5),
          axis.text.y = element_text(size = 3.5),
          axis.title.x = element_text(size = 4),
          axis.title.y = element_text(size = 4),
          axis.ticks = element_line(colour = "black", size = 0.2),
          axis.ticks.length = unit(0.04, "cm")) -> graph
  
  # Format graphs depending on panel placement
  if(bottom == FALSE & left == TRUE){
    graph + theme(axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  plot.margin = unit(c(0.08, 0.03, 0.43, 0.06), "cm"))} -> graph
  if(bottom == TRUE & left == TRUE){
    graph + theme(plot.margin = unit(c(0.01, 0.03, 0.08, 0.06), "cm"))} -> graph
  if(bottom == FALSE & left == FALSE){
    graph + theme(axis.ticks = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.margin = unit(c(0.08, 0.10, 0.43, 0.42), "cm"))} -> graph
  if(bottom == TRUE & left == FALSE){
    graph + theme(axis.ticks.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title.y = element_blank(),
                  plot.margin = unit(c(0.01, 0.10, 0.08, 0.42), "cm"))} -> graph
  
  # Output graph
  return(graph)}

# Put individual dataframes into a list to plot all survival curves simultaneously
Data_List <- list(Data_CN_YW_YE, Data_CN_YW_NE, Data_CN_NW_YE, Data_CN_NW_NE,
                  Data_CA_YW_YE, Data_CA_YW_NE, Data_CA_NW_YE, Data_CA_NW_NE)
    
# Function to plot all survival curves simultaneously
surv.plotsAll <- function(dfAll, colourAll){
  
  # Plot survival curves
  ggplot() +
    geom_point(data = time.means(dfAll[[1]]), aes(x = Time, y = Mean), colour = colourAll[1], size = 0.3) +
    geom_line(data = time.means(dfAll[[1]]), aes(x = Time, y = Mean), colour = colourAll[1], size = 0.5) +
    coord_cartesian(ylim = c(0, 25)) +
    scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 48), 
                       breaks = c(seq(1, 12, by = 0.5), 24, 36, 48),
                       labels = c(0, rep("", 21), 12, 24, 36, 48)) +
    xlab("Time (Hours)") +
    ylab("Seeds Remaining") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "gray90", size = 0.2),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
          panel.background = element_rect(fill = "white"),
          axis.text.x = element_text(size = 5.5),
          axis.text.y = element_text(size = 5.5),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          axis.ticks = element_line(colour = "black", size = 0.4),
          axis.ticks.length = unit(0.06, "cm")) -> graph
  for(i in 2:8){
    graph +
      geom_point(data = time.means(dfAll[[i]]), aes(x = Time, y = Mean), colour = colourAll[i], size = 0.12) +
      geom_line(data = time.means(dfAll[[i]]), aes(x = Time, y = Mean), colour = colourAll[i], size = 0.5) -> graph}
  
  # Output graph
  return(graph)}  

