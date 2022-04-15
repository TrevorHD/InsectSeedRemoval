##### Load libraries and data -----------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(grid)
library(gridBase)
library(lme4)
library(lmtest)

# Load data from local copy of CSV
Data <- read.csv("Data/SeedRemovalData.csv")
names(Data)[1] <- "Depot"

# Create copy of data with treatments and only a few key time points
Data2 <- Data[, c(1:5, 18, 30, 31, 33)]
Data2 <- na.omit(Data2)





##### Fit GLM to seed removal data ------------------------------------------------------------------------

# Total proportion of seeds removed after a given time
1 - mean(na.omit(Data$t_6))/25  # 6h
1 - mean(na.omit(Data$t_12))/25 # 12h
1 - mean(na.omit(Data$t_24))/25 # 24h
1 - mean(na.omit(Data$t_48))/25 # 48h

# Proportion of seeds removed after a given time for warmed CN E+
1 - mean(na.omit(filter(Data, Species == "CN", Warmed == 1, Elaiosome == 1)$t_6))/25  # 6h
1 - mean(na.omit(filter(Data, Species == "CN", Warmed == 1, Elaiosome == 1)$t_12))/25 # 12h
1 - mean(na.omit(filter(Data, Species == "CN", Warmed == 1, Elaiosome == 1)$t_24))/25 # 24h
1 - mean(na.omit(filter(Data, Species == "CN", Warmed == 1, Elaiosome == 1)$t_48))/25 # 48h

# Proportion of seeds removed after a given time for unwarmed CA E-
1 - mean(na.omit(filter(Data, Species == "CA", Warmed == 0, Elaiosome == 0)$t_6))/25  # 6
1 - mean(na.omit(filter(Data, Species == "CA", Warmed == 0, Elaiosome == 0)$t_12))/25 # 12h
1 - mean(na.omit(filter(Data, Species == "CA", Warmed == 0, Elaiosome == 0)$t_24))/25 # 24h
1 - mean(na.omit(filter(Data, Species == "CA", Warmed == 0, Elaiosome == 0)$t_48))/25 # 48h

# Use binomial error structure with logit link function
# Response is vector of "successes" (seeds removed) and "failures" (seeds not removed)

# Fit GLM for seed removal at 6 hours
Fit6 <- glmer(cbind(25 - Data2$t_6, Data2$t_6) ~ Species + Warmed + Elaiosome + Species:Warmed +
              Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit6)

# Fit GLM for seed removal at 12 hours
Fit12 <- glmer(cbind(25 - Data2$t_12, Data2$t_12) ~ Species + Warmed + Elaiosome + Species:Warmed +
               Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit12)

# Fit GLM for seed removal at 24 hours
Fit24 <- glmer(cbind(25 - Data2$t_24, Data2$t_24) ~ Species + Warmed + Elaiosome + Species:Warmed +
                 Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit24)

# Fit GLM for seed removal at 48 hours; remove all non-significant interaction terms
Fit48 <- glmer(cbind(25 - Data2$t_48, Data2$t_48) ~ Species + Warmed + Elaiosome + Species:Warmed +
                 Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit48)

# Model response for proportion of seeds removed after a given time for warmed CN E+
# Note: use inverse logit exp(x)/(1 + exp(x)) to transform these values to proportions
predict(Fit12.1, newdata = data.frame(Species = "CN", Warmed = 1, Elaiosome = 1), re.form = NA) # 12hr
predict(Fit24, newdata = data.frame(Species = "CN", Warmed = 1, Elaiosome = 1), re.form = NA)   # 24hr
predict(Fit48.2, newdata = data.frame(Species = "CN", Warmed = 1, Elaiosome = 1), re.form = NA) # 48hr

# Model response for proportion of seeds removed after a given time for unwarmed CA E-
# Note: use inverse logit exp(x)/(1 + exp(x)) to transform these values to proportions
predict(Fit12.1, newdata = data.frame(Species = "CA", Warmed = 0, Elaiosome = 0), re.form = NA) # 12hr
predict(Fit24, newdata = data.frame(Species = "CA", Warmed = 0, Elaiosome = 0), re.form = NA)   # 24hr
predict(Fit48.2, newdata = data.frame(Species = "CA", Warmed = 0, Elaiosome = 0), re.form = NA) # 48hr





##### Prepare data for plotting ---------------------------------------------------------------------------

# Sort into the eight different treatment groups
Data.CN_YW_YE <- subset(Data, Species == "CN" & Warmed == 1 & Elaiosome == 1)
Data.CN_YW_NE <- subset(Data, Species == "CN" & Warmed == 1 & Elaiosome == 0)
Data.CN_NW_YE <- subset(Data, Species == "CN" & Warmed == 0 & Elaiosome == 1)
Data.CN_NW_NE <- subset(Data, Species == "CN" & Warmed == 0 & Elaiosome == 0)
Data.CA_YW_YE <- subset(Data, Species == "CA" & Warmed == 1 & Elaiosome == 1)
Data.CA_YW_NE <- subset(Data, Species == "CA" & Warmed == 1 & Elaiosome == 0)
Data.CA_NW_YE <- subset(Data, Species == "CA" & Warmed == 0 & Elaiosome == 1)
Data.CA_NW_NE <- subset(Data, Species == "CA" & Warmed == 0 & Elaiosome == 0)

# Functions to take average number (and SD and SEM) of seeds remaining at a given time
na.mean <- function(x){
  mean(na.omit(x))}
na.sd <- function(x){
  sd(na.omit(x))}
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
    
  
  
  
  
##### Plot E+ vs E- ---------------------------------------------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure1.tif", width = 2800, height = 2000, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1400)
pushViewport(viewport(layout = gly))

# CN Unwarmed: E+ (dark green) v E- (light green)
print(surv.plots(Data.CN_NW_YE, Data.CN_NW_NE, PlotColours[1], PlotColours[2],
                 bottom = FALSE, left = TRUE, atext = "CN Unwarmed"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 25:700))

# CN Warmed: E+ (dark green) v E- (light green)
print(surv.plots(Data.CN_YW_YE, Data.CN_YW_NE, PlotColours[1], PlotColours[2],
                 bottom = TRUE, left = TRUE, atext = "CN Warmed"),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 25:700))

# CA Unwarmed: E+ (dark green) v E- (light green)
print(surv.plots(Data.CA_NW_YE, Data.CA_NW_NE, PlotColours[1], PlotColours[2],
                 bottom = FALSE, left = FALSE, atext = "CA Unwarmed"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 700:1375))

# CA Warmed: E+ (dark green) v E- (light green)
print(surv.plots(Data.CA_YW_YE, Data.CA_YW_NE, PlotColours[1], PlotColours[2],
                 bottom = TRUE, left = FALSE, atext = "CA Warmed"),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 700:1375))

# Create legend
grid.text(label = c("E+", "E-"), x = c(0.934, 0.934), 
          y = c(0.887, 0.864), hjust = c(1, 1), gp = gpar(cex = 0.3))
grid.segments(x0 = c(0.944, 0.944), y0 = c(0.886, 0.863), 
              x1 = c(0.961, 0.961), y1 = c(0.886, 0.863),
              gp = gpar(col = c(PlotColours[1], PlotColours[2]), lty = rep(1, 2), lwd = rep(0.6, 2)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### Plot Warmed vs Unwarmed -----------------------------------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure2.tif", width = 2800, height = 2000, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1400)
pushViewport(viewport(layout = gly))

# CN E+: Warmed (red) v unwarmed (blue)
print(surv.plots(Data.CN_NW_YE, Data.CN_YW_YE, PlotColours[3], PlotColours[4],
                 bottom = FALSE, left = TRUE, atext = "CN E+"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 25:700))

# CN E-: Warmed (red) v unwarmed (blue)
print(surv.plots(Data.CN_NW_NE, Data.CN_YW_NE, PlotColours[3], PlotColours[4],
                 bottom = TRUE, left = TRUE, atext = "CN E-"),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 25:700))

# CA E+: Warmed (red) v unwarmed (blue)
print(surv.plots(Data.CA_NW_YE, Data.CA_YW_YE, PlotColours[3], PlotColours[4],
                 bottom = FALSE, left = FALSE, atext = "CA E+"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 700:1375))

# CA E: Warmed (red) v unwarmed (blue)
print(surv.plots(Data.CA_NW_NE, Data.CA_YW_NE, PlotColours[3], PlotColours[4],
                 bottom = TRUE, left = FALSE, atext = "CA E-"),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 700:1375))

# Create legend
grid.text(label = c("Unwarmed", "Warmed"), x = c(0.934, 0.934), 
          y = c(0.887, 0.864), hjust = c(1, 1), gp = gpar(cex = 0.3))
grid.segments(x0 = c(0.944, 0.944), y0 = c(0.886, 0.863), 
              x1 = c(0.961, 0.961), y1 = c(0.886, 0.863),
              gp = gpar(col = c(PlotColours[3], PlotColours[4]), lty = rep(1, 2), lwd = rep(0.6, 2)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### Plot CN vs CA ---------------------------------------------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure3.tif", width = 2800, height = 2000, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1400)
pushViewport(viewport(layout = gly))

# E+ Unwarmed: CN (black) v CA (grey)
print(surv.plots(Data.CA_NW_YE, Data.CN_NW_YE, PlotColours[5], PlotColours[6],
                 bottom = FALSE, left = TRUE, atext = "E+ Unwarmed"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 25:700))

# E+ Warmed: CN (black) v CA (grey)
print(surv.plots(Data.CA_YW_YE, Data.CN_YW_YE, PlotColours[5], PlotColours[6],
                 bottom = TRUE, left = TRUE, atext = "E+ Warmed"),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 25:700))

# E- Unwarmed: CN (black) v CA (grey)
print(surv.plots(Data.CA_NW_NE, Data.CN_NW_NE, PlotColours[5], PlotColours[6],
                 bottom = FALSE, left = FALSE, atext = "E- Unwarmed"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 700:1375))

# E- Warmed: CN (black) v CA (grey)
print(surv.plots(Data.CA_YW_NE, Data.CN_YW_NE, PlotColours[5], PlotColours[6],
                 bottom = TRUE, left = FALSE, atext = "E- Warmed"),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 700:1375))

# Create legend
grid.text(label = c("CN", "CA"), x = c(0.934, 0.934), 
          y = c(0.887, 0.864), hjust = c(1, 1), gp = gpar(cex = 0.3))
grid.segments(x0 = c(0.944, 0.944), y0 = c(0.886, 0.863), 
              x1 = c(0.961, 0.961), y1 = c(0.886, 0.863),
              gp = gpar(col = c(PlotColours[5], PlotColours[6]), lty = rep(1, 2), lwd = rep(0.6, 2)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

