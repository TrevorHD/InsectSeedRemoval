##### Load libraries and data -----------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(grid)
library(gridBase)
library(httr)
library(lme4)
library(lmtest)

# Load data from private repo
# Username and password obviously not included for security reasons
# Note: access to GitHub API with httr has been deprecated and will need to be fixed
Data <- GET(url = "https://api.github.com/repos/TrevorHD/InsectSeedRemoval/contents/SeedRemovalData.csv",
            authenticate("email", "password"),
            accept("application/vnd.github.v3.raw")) %>%  
  content(as = "parsed", type = "text/csv")

# For now, load data from local copy
Data <- read.csv("SeedRemovalData.csv")
names(Data)[1] <- "Depot"

# Create copy of data with treatments and only a few key time points
Data2 <- Data[, c(1:5, 18, 30, 31, 33)]
Data2 <- na.omit(Data2)





##### Fit GLM to seed removal data ------------------------------------------------------------------------

# Total proportion of seeds removed after a given time
1 - mean(na.omit(Data$t_12))/25 # 12h
1 - mean(na.omit(Data$t_24))/25 # 24h
1 - mean(na.omit(Data$t_48))/25 # 48h

# Proportion of seeds removed after a given time for warmed CN E+
1 - mean(na.omit(filter(Data, Species == "CN", Warmed == 1, Elaiosome == 1)$t_12))/25 # 12h
1 - mean(na.omit(filter(Data, Species == "CN", Warmed == 1, Elaiosome == 1)$t_24))/25 # 24h
1 - mean(na.omit(filter(Data, Species == "CN", Warmed == 1, Elaiosome == 1)$t_48))/25 # 48h

# Use binomial error structure with logit link function
# Response is vector of "successes" (seeds removed) and "failures" (seeds not removed)

# Fit GLM for seed removal at 6 hours
Fit6 <- glmer(cbind(25 - Data2$t_6, Data2$t_6) ~ Species + Warmed + Elaiosome + Species:Warmed +
              Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit6)

# Fit GLM for seed removal at 12 hours; remove all non-significant interaction terms
Fit12 <- glmer(cbind(25 - Data2$t_12, Data2$t_12) ~ Species + Warmed + Elaiosome + Species:Warmed +
               Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit12)
Fit12.1 <- glmer(cbind(25 - Data2$t_12, Data2$t_12) ~ Species + Warmed + Elaiosome + Species:Elaiosome +
                 Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit12.1)
lrtest(Fit12.1, Fit12)

# Fit GLM for seed removal at 24 hours
Fit24 <- glmer(cbind(25 - Data2$t_24, Data2$t_24) ~ Species + Warmed + Elaiosome + Species:Warmed +
                 Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit24)

# Fit GLM for seed removal at 12 hours; remove all non-significant interaction terms
Fit48 <- glmer(cbind(25 - Data2$t_48, Data2$t_48) ~ Species + Warmed + Elaiosome + Species:Warmed +
                 Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit48)
Fit48.1 <- glmer(cbind(25 - Data2$t_48, Data2$t_48) ~ Species + Warmed + Elaiosome + Species:Elaiosome + 
                 Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
summary(Fit48.1)
lrtest(Fit48.1, Fit48)
Fit48.2 <- glmer(cbind(25 - Data2$t_48, Data2$t_48) ~ Species + Warmed + Elaiosome + Species:Elaiosome + 
                 (1 | Block), data = Data2, family = "binomial")
summary(Fit48.2)
lrtest(Fit48.2, Fit48.1)

# Model response for proportion of seeds removed after a given time for warmed CN E+
# Note: use inverse logit exp(x)/(1 + exp(x)) to transform these values to proportions
predict(Fit12.1, newdata = data.frame(Species = "CN", Warmed = 1, Elaiosome = 1), re.form = NA) # 12hr
predict(Fit24, newdata = data.frame(Species = "CN", Warmed = 1, Elaiosome = 1), re.form = NA)   # 24hr
predict(Fit48.2, newdata = data.frame(Species = "CN", Warmed = 1, Elaiosome = 1), re.form = NA) # 48hr





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

# Function to plot survival curves
surv.plots <- function(df1, df2, colour1, colour2, bottom){
  ggplot() +
    geom_point(data = time.means(df1), aes(x = Time, y = Mean), colour = colour1, size = 3) +
    geom_line(data = time.means(df1), aes(x = Time, y = Mean), colour = colour1, size = 1.3) +
    geom_errorbar(data = time.means(df1), 
                  aes(x = Time, ymin = Mean - SEM, ymax = Mean + SEM), width = 0.25, 
                  colour = colour1, size = 1.3) +
    geom_point(data = time.means(df2), aes(x = Time, y = Mean), colour = colour2, size = 3) +
    geom_line(data = time.means(df2), aes(x = Time, y = Mean), colour = colour2, size = 1.3) +
    geom_errorbar(data = time.means(df2), 
                  aes(x = Time, ymin = Mean - SEM, ymax = Mean + SEM), width = 0.25, 
                  colour = colour2, size = 1.3) +
    coord_cartesian(ylim = c(0, 25)) +
    scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 48), 
                       breaks = c(seq(1, 12, by = 0.5), 24, 36, 48),
                       labels = c(0, rep("", 21), 12, 24, 36, 48)) +
    xlab("Time (Hours)") +
    ylab("Seeds Remaining") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "gray90"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.background = element_rect(fill = "white"),
          axis.text.y = element_text(size = 30),
          axis.title.y = element_text(size = 38)) -> graph
  
    # Remove x-axis labels if graph will be stacked above another
    if(bottom == TRUE){
      graph + theme(axis.text.x = element_text(size = 30),
                    axis.title.x = element_text(size = 38))} -> graph
    if(bottom == FALSE){
      graph + theme(axis.ticks.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.title.x = element_blank())} -> graph
  # Output graph
  return(graph)}
    
  
  
  
  
##### Plot E+ vs E- ---------------------------------------------------------------------------------------

# Prepare graphics device
jpeg(filename = "Figure1.jpeg", width = 2800, height = 2000, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1400)
pushViewport(viewport(layout = gly))

# CN Unwarmed: E+ (dark green) v E- (light green)
print(surv.plots(Data.CN_NW_YE, Data.CN_NW_NE, "darkgreen", "green", bottom = FALSE),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 25:675))

# CN Warmed: E+ (dark green) v E- (light green)
print(surv.plots(Data.CN_YW_YE, Data.CN_YW_NE, "darkgreen", "green", bottom = TRUE),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 25:675))

# CA Unwarmed: E+ (dark green) v E- (light green)
print(surv.plots(Data.CA_NW_YE, Data.CA_NW_NE, "darkgreen", "green", bottom = FALSE),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 725:1375))

# CA Warmed: E+ (dark green) v E- (light green)
print(surv.plots(Data.CA_YW_YE, Data.CA_YW_NE, "darkgreen", "green", bottom = TRUE),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 725:1375))

# Create figure labels
grid.text(label = c("CN Unwarmed", "CN Warmed", "CA Unwarmed", "CA Warmed"), 
          x = c(0.385, 0.385, 0.886, 0.886), y = c(0.955, 0.480, 0.955, 0.480), hjust = 0, gp = gpar(cex = 3))

# Create legend
grid.text(label = c("E+", "E-", "E+", "E-"), x = c(0.408, 0.408, 0.909, 0.909), 
          y = c(0.925, 0.910, 0.925, 0.910), hjust = 0, gp = gpar(cex = 2.4))
grid.segments(x0 = c(0.385, 0.385, 0.886, 0.886), y0 = c(0.925, 0.910, 0.925, 0.910), 
              x1 = c(0.400, 0.400, 0.901, 0.901), y1 = c(0.925, 0.910, 0.925, 0.910),
              gp = gpar(col = rep(c("darkgreen", "green"), 2), lty = rep(1, 4), lwd = rep(2, 4)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### Plot Warmed vs Unwarmed -----------------------------------------------------------------------------

# Prepare graphics device
jpeg(filename = "Figure2.jpeg", width = 2800, height = 2000, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1400)
pushViewport(viewport(layout = gly))

# CN E+: Warmed (red) v unwarmed (blue)
print(surv.plots(Data.CN_YW_YE, Data.CN_NW_YE, "red", "blue", bottom = FALSE),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 25:675))

# CN E-: Warmed (red) v unwarmed (blue)
print(surv.plots(Data.CN_YW_NE, Data.CN_NW_NE, "red", "blue", bottom = TRUE),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 25:675))

# CA E+: Warmed (red) v unwarmed (blue)
print(surv.plots(Data.CA_YW_YE, Data.CA_NW_YE, "red", "blue", bottom = FALSE),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 725:1375))

# CA E: Warmed (red) v unwarmed (blue)
print(surv.plots(Data.CA_YW_NE, Data.CA_NW_NE, "red", "blue", bottom = TRUE),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 725:1375))

# Create figure labels
grid.text(label = c("CN E+", "CN E-", "CA E+", "CA E-"), 
          x = c(0.385, 0.385, 0.886, 0.886), y = c(0.955, 0.480, 0.955, 0.480), hjust = 0, gp = gpar(cex = 3))

# Create legend
grid.text(label = c("Warmed", "Unwarmed", "Warmed", "Unwarmed"), x = c(0.408, 0.408, 0.909, 0.909), 
          y = c(0.925, 0.910, 0.925, 0.910), hjust = 0, gp = gpar(cex = 2.4))
grid.segments(x0 = c(0.385, 0.385, 0.886, 0.886), y0 = c(0.925, 0.910, 0.925, 0.910), 
              x1 = c(0.400, 0.400, 0.901, 0.901), y1 = c(0.925, 0.910, 0.925, 0.910),
              gp = gpar(col = rep(c("red", "blue"), 2), lty = rep(1, 4), lwd = rep(2, 4)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### Plot CN vs CA ---------------------------------------------------------------------------------------

# Prepare graphics device
jpeg(filename = "Figure3.jpeg", width = 2800, height = 2000, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1400)
pushViewport(viewport(layout = gly))

# E+ Unwarmed: CN (black) v CA (grey)
print(surv.plots(Data.CA_NW_YE, Data.CN_NW_YE, "grey", "black", bottom = FALSE),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 25:675))

# E+ Warmed: CN (black) v CA (grey)
print(surv.plots(Data.CA_YW_YE, Data.CN_YW_YE, "grey", "black", bottom = TRUE),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 25:675))

# E- Unwarmed: CN (black) v CA (grey)
print(surv.plots(Data.CA_NW_NE, Data.CN_NW_NE, "grey", "black", bottom = FALSE),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 725:1375))

# E- Warmed: CN (black) v CA (grey)
print(surv.plots(Data.CA_YW_NE, Data.CN_YW_NE, "grey", "black", bottom = TRUE),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 725:1375))

# Create figure labels
grid.text(label = c("E+ Unwarmed", "E+ Warmed", "E- Unwarmed", "E- Warmed"), 
          x = c(0.385, 0.385, 0.886, 0.886), y = c(0.955, 0.480, 0.955, 0.480), hjust = 0, gp = gpar(cex = 3))

# Create legend
grid.text(label = c("CN", "CA", "CN", "CA"), x = c(0.408, 0.408, 0.909, 0.909), 
          y = c(0.925, 0.910, 0.925, 0.910), hjust = 0, gp = gpar(cex = 2.4))
grid.segments(x0 = c(0.385, 0.385, 0.886, 0.886), y0 = c(0.925, 0.910, 0.925, 0.910), 
              x1 = c(0.400, 0.400, 0.901, 0.901), y1 = c(0.925, 0.910, 0.925, 0.910),
              gp = gpar(col = rep(c("black", "grey"), 2), lty = rep(1, 4), lwd = rep(2, 4)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

