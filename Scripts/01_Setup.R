##### Load libraries and data -----------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(grid)
library(gridBase)
library(lme4)
library(survival)
library(sjPlot)
library(sjmisc)

# Load data from local copy of CSV
Data <- read.csv("Data/SeedRemovalData.csv")
Data_SM <- read.csv("Data/SeedMassData.csv")
names(Data)[1] <- "Depot"





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
                            "purple", "red", "orange3", "yellow2"), c.alpha)

# Function to plot 2 survival curves
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
    geom_errorbar(data = time.means(df1), aes(x = Time, ymin = Mean - SEM, ymax = Mean + SEM),
                  width = 0.3, colour = colour1, size = 0.2) +
    geom_point(data = time.means(df2), aes(x = Time, y = Mean), colour = colour2, size = 0.01) +
    geom_line(data = time.means(df2), aes(x = Time, y = Mean), colour = colour2, size = 0.25) +
    geom_errorbar(data = time.means(df2), aes(x = Time, ymin = Mean - SEM, ymax = Mean + SEM),
                  width = 0.3, colour = colour2, size = 0.2) +
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

# Function to plot 4 survival curves
# Note: no K-S test since there's more than 2 curves
surv.plots4 <- function(df1, df2, df3, df4, colour1, colour2, colour3, colour4, bottom){
  
  # Put dataframes and colours into condensed lists
  dfList <- list(df1, df2, df3, df4)
  colList <- c(colour1, colour2, colour3, colour4)
  
  # Plot survival curves
  ggplot() +
    geom_point(data = time.means(df1), aes(x = Time, y = Mean), colour = colList[1], size = 0.15) +
    geom_line(data = time.means(df1), aes(x = Time, y = Mean), colour = colList[1], size = 0.5) +
    geom_errorbar(data = time.means(df1), aes(x = Time, ymin = Mean - SEM, ymax = Mean + SEM),
                  width = 0.3, colour = colList[1], size = 0.25) +
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
          axis.text.x = element_text(size = 4.5),
          axis.text.y = element_text(size = 4.5),
          axis.title.x = element_text(size = 5),
          axis.title.y = element_text(size = 5),
          axis.ticks = element_line(colour = "black", size = 0.2),
          axis.ticks.length = unit(0.04, "cm")) -> graph
  for(i in 2:4){
    graph +
      geom_point(data = time.means(dfList[[i]]), aes(x = Time, y = Mean),
                 colour = colList[i], size = 0.15) +
      geom_line(data = time.means(dfList[[i]]), aes(x = Time, y = Mean),
                colour = colList[i], size = 0.5) +
      geom_errorbar(data = time.means(dfList[[i]]), aes(x = Time, ymin = Mean - SEM, ymax = Mean + SEM),
                    width = 0.3, colour = colList[i], size = 0.25)-> graph}
  
  # Format graphs depending on panel placement
  if(bottom == FALSE){
    graph + theme(axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  plot.margin = unit(c(0.08, 0.03, 0.43, 0.06), "cm"))} -> graph
  if(bottom == TRUE){
    graph + theme(plot.margin = unit(c(0.01, 0.03, 0.08, 0.06), "cm"))} -> graph
  
  # Output graph
  return(graph)}





##### Prep data for plotting and model fits ---------------------------------------------------------------

# Remove depot 29 since it was never used in the experiment (was damaged in transit)
Data <- Data[-29, ]

# Handle NAs in data sheet, caused by camera malfunctions
# Numbers are backup in-situ "midpoint" counts intended to be happen halfway between photographs
# Was usually closer to time of photograph, though, especially for 24, 36, and 48-hr marks
Data[25, "t_24"] <- 14
Data[45, "t_24"] <- 15
Data[64, "t_7.5"] <- 23
Data[64, "t_11"] <- 10

# Create copy of data with treatments and only a few key time points for GLM
Data_GLM <- Data[, c(1:5, 18, 30, 31, 33)]

# Subset data by species for GLMs
Data_GLM_CN <- subset(Data_GLM, Species == "CN")
Data_GLM_CA <- subset(Data_GLM, Species == "CA")

# Sort seed removal data into the eight different treatment groups for plotting
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





##### Convert data to format suitable for survival model functions ----------------------------------------

# Construct concise representation of original dataset
# ToD indicates time of death
# Cens indicates censor status (1 = dead, 0 = censored and survived until end)
for(i in 1:nrow(Data)){
  if(i == 1){
    df_main <- matrix(ncol = 7, nrow = 0)}
  row_sub <- Data[i, ]
  for(j in 7:ncol(Data)){
    if(j == 7){
      prevNum <- 25
      df_sub <- matrix(ncol = 7, nrow = 0)}
    curNum <- as.numeric(row_sub[j])
    if(curNum < prevNum){
      df_sub <- rbind(df_sub, matrix(c(as.character(row_sub[1:5]), str_remove(names(Data)[j], "t_"), 1),
                                     nrow = prevNum - curNum, ncol = 7, byrow = TRUE))}
    if(j == ncol(Data) & curNum > 0){
      df_sub <- rbind(df_sub, matrix(c(as.character(row_sub[1:5]), str_remove(names(Data)[j], "t_"), 0),
                                     nrow = curNum, ncol = 7, byrow = TRUE))}
    prevNum <- curNum}
  df_main <- rbind(df_main, df_sub)}
df_main <- data.frame(df_main, stringsAsFactors = FALSE)
names(df_main) <- c(names(Data)[1:5], "ToD", "Cens")
df_main$Depot <- as.numeric(df_main$Depot)
df_main$Block <- as.numeric(df_main$Block)
df_main$Warmed <- as.numeric(df_main$Warmed)
df_main$Elaiosome <- as.numeric(df_main$Elaiosome)
df_main$ToD <- as.numeric(df_main$ToD)
df_main$Cens <- as.numeric(df_main$Cens)
DataAlt <- df_main

# Remove temporary variables since they will no longer be used
remove(df_main, df_sub, row_sub, curNum, prevNum, i, j)

# Split data into CN and CA for separate models
DataAlt_CA <- subset(DataAlt, Species == "CA")
DataAlt_CN <- subset(DataAlt, Species == "CN")

