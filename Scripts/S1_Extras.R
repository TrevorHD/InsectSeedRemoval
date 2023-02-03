##### Fit GLM to seed removal data (species not separate) -------------------------------------------------

# Use binomial error structure with logit link function
# Response is vector of "successes" (seeds removed) and "failures" (seeds not removed)
# Models for 48 hours not fit due to convergence issues

# Fit GLMs for seed removal at 6, 12, and 24 hours
Fit6 <- glmer(cbind(25 - Data2$t_6, Data2$t_6) ~ Species + Warmed + Elaiosome + Species:Warmed +
                Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
Fit12 <- glmer(cbind(Data2$t_6 - Data2$t_12, Data2$t_12) ~ Species + Warmed + Elaiosome + Species:Warmed +
                 Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")
Fit24 <- glmer(cbind(Data2$t_12 - Data2$t_24, Data2$t_24) ~ Species + Warmed + Elaiosome + Species:Warmed +
                 Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")

# Model selection performed on 24 hour models to minimise AIC
#Fit24 <- glmer(cbind(Data2$t_12 - Data2$t_24, Data2$t_24) ~ Species + Warmed + Elaiosome +
#                 Species:Elaiosome + Warmed:Elaiosome + (1 | Block), data = Data2, family = "binomial")

# Check model summaries
summary(Fit6)
summary(Fit12)
summary(Fit24)

# Model response for proportion of seeds removed after a given time for warmed CN E+
predict(Fit12.1, newdata = data.frame(Species = "CN", Warmed = 1, Elaiosome = 1), re.form = NA) # 12hr
predict(Fit24, newdata = data.frame(Species = "CN", Warmed = 1, Elaiosome = 1), re.form = NA)   # 24hr
predict(Fit48.2, newdata = data.frame(Species = "CN", Warmed = 1, Elaiosome = 1), re.form = NA) # 48hr

# Model response for proportion of seeds removed after a given time for unwarmed CN E-
predict(Fit12.1, newdata = data.frame(Species = "CN", Warmed = 0, Elaiosome = 0), re.form = NA) # 12hr
predict(Fit24, newdata = data.frame(Species = "CN", Warmed = 0, Elaiosome = 0), re.form = NA)   # 24hr
predict(Fit48.2, newdata = data.frame(Species = "CN", Warmed = 0, Elaiosome = 0), re.form = NA) # 48hr

# Model response for proportion of seeds removed after a given time for unwarmed CA E-
predict(Fit12.1, newdata = data.frame(Species = "CA", Warmed = 0, Elaiosome = 0), re.form = NA) # 12hr
predict(Fit24, newdata = data.frame(Species = "CA", Warmed = 0, Elaiosome = 0), re.form = NA)   # 24hr
predict(Fit48.2, newdata = data.frame(Species = "CA", Warmed = 0, Elaiosome = 0), re.form = NA) # 48hr

# Note: for the above, use inverse logit exp(x)/(1 + exp(x)) to transform these values to proportions
# Unwarmed E- CN and CA have similarly low rates of removal, hence them both being included above





##### [Unused] Marginal effect plots for interactions (species not separate) ------------------------------

# Prepare graphics device
tiff(filename = "Figure S4.tif", width = 2400, height = 3000, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1500, 1200)
pushViewport(viewport(layout = gly))

# Set theme and axis objects to modify individual plot formatting
ip_t1 <- theme(panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(colour = "gray90", size = 0.2),
               panel.grid.minor.x = element_blank(),
               panel.grid.minor.y = element_blank(),
               panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
               panel.background = element_rect(fill = "white"),
               axis.text.x = element_text(size = 4),
               axis.text.y = element_text(size = 4),
               axis.title.x = element_blank(),
               axis.title.y = element_text(size = 4.5),
               axis.ticks = element_line(colour = "black", size = 0.4),
               axis.ticks.length = unit(0.06, "cm"),
               plot.title = element_blank(),
               legend.position = "none")
ip_t2 <- ip_t1 + theme(axis.text.y = element_blank(),
                       axis.title.y = element_blank(),
                       axis.ticks.y = element_blank())
ip_m1 <- scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 1), 
                            breaks = c(0, 1), labels = c("E-", "E+"))
ip_m2 <- scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 1), 
                            breaks = c(0, 1), labels = c("NW", "W"))
ip_m3 <- scale_y_continuous(name = "Probability of Removal", expand = c(0.01, 0.01),
                            limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

# Marginal plots for Elaiosome:Species interaction  
print(plot_model(Fit6, type = "pred", terms = c("Elaiosome", "Species"), colors = c("grey", "black")) +
        ip_m1 + ip_m3 + ip_t1, vp = viewport(layout.pos.row = 1:500, layout.pos.col = 1:450))
print(plot_model(Fit12, type = "pred", terms = c("Elaiosome", "Species"), colors = c("grey", "black")) +
        ip_m1 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 1:500, layout.pos.col = 450:825))
print(plot_model(Fit24, type = "pred", terms = c("Elaiosome", "Species"), colors = c("grey", "black")) +
        ip_m1 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 1:500, layout.pos.col = 825:1200))
# For CN, removing the elaiosome decreases rate of removal; not as evident for CA

# Marginal plots for Warmed:Species interaction  
print(plot_model(Fit6, type = "pred", terms = c("Warmed", "Species"), colors = c("grey", "black")) +
        ip_m2 + ip_m3 + ip_t1, vp = viewport(layout.pos.row = 500:1000, layout.pos.col = 1:450))
print(plot_model(Fit12, type = "pred", terms = c("Warmed", "Species"), colors = c("grey", "black")) +
        ip_m2 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 500:1000, layout.pos.col = 450:825))
print(plot_model(Fit24, type = "pred", terms = c("Warmed", "Species"), colors = c("grey", "black")) +
        ip_m2 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 500:1000, layout.pos.col = 825:1200))

# Marginal plots for Warmed:Elaiosome interaction  
print(plot_model(Fit6, type = "pred", terms = c("Warmed", "Elaiosome"), colors = c("green", "darkgreen")) +
        ip_m2 + ip_m3 + ip_t1, vp = viewport(layout.pos.row = 1000:1500, layout.pos.col = 1:450))
print(plot_model(Fit12, type = "pred", terms = c("Warmed", "Elaiosome"), colors = c("green", "darkgreen")) +
        ip_m2 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 1000:1500, layout.pos.col = 450:825))
print(plot_model(Fit24, type = "pred", terms = c("Warmed", "Elaiosome"), colors = c("green", "darkgreen")) +
        ip_m2 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 1000:1500, layout.pos.col = 825:1200))
# When there is no warming, seeds with elaiosomes are more likely to be removed
# When warming is added, this difference mostly disappears; there is an obvious interaction
# Warming drastically increases chance of removal E- seeds, but not really for E+

# Create plot labels
grid.text(label = rep(tlist, 3), x = rep(seq(0.34, 0.966, length.out = 3), 2), 
          y = rep(c(0.966, 0.634, 0.300), each = 3), hjust = rep(1, 9), gp = gpar(cex = 0.3))

# Create legend
grid.text(label = c("CN", "CA", "E+", "E-"), x = rep(0.132, 4), 
          y = c(0.966, 0.948, 0.930, 0.912), hjust = rep(0, 4), gp = gpar(cex = 0.3))
grid.segments(x0 = rep(0.109, 4), y0 = c(0.966, 0.948, 0.930, 0.912), 
              x1 = rep(0.125, 4), y1 = c(0.966, 0.948, 0.930, 0.912),
              gp = gpar(col = c(PlotColours[5], PlotColours[6], PlotColours[1], PlotColours[2]),
                        lty = rep(1, 4), lwd = rep(1.1, 4)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [Unused] Plot all 8 survival curves simultaneously --------------------------------------------------

# Put individual dataframes into a list to plot all survival curves simultaneously
Data_List <- list(Data_CN_YW_YE, Data_CN_YW_NE, Data_CN_NW_YE, Data_CN_NW_NE,
                  Data_CA_YW_YE, Data_CA_YW_NE, Data_CA_NW_YE, Data_CA_NW_NE)

# Function to plot all survival curves simultaneously
surv.plotsAll <- function(dfAll, colourAll){
  
  # Plot survival curves
  ggplot() +
    geom_point(data = time.means(dfAll[[1]]), aes(x = Time, y = Mean),
               colour = colourAll[1], size = 0.3) +
    geom_line(data = time.means(dfAll[[1]]), aes(x = Time, y = Mean),
              colour = colourAll[1], size = 0.5) +
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
          axis.ticks.length = unit(0.06, "cm"),
          plot.margin = unit(c(0.01, 0.10, 0.08, 0.10), "cm")) -> graph
  for(i in 2:8){
    graph +
      geom_point(data = time.means(dfAll[[i]]), aes(x = Time, y = Mean),
                 colour = colourAll[i], size = 0.12) +
      geom_line(data = time.means(dfAll[[i]]), aes(x = Time, y = Mean),
                colour = colourAll[i], size = 0.5) -> graph}
  
  # Output graph
  return(graph)}  

# List of formatted timesteps for text
tlist <- c(expression(paste(~italic("t"), " = ", 6)),
           expression(paste(~italic("t"), " = ", 12)),
           expression(paste(~italic("t"), " = ", 24)))
tlistc <- c(expression(paste("CN, ", ~italic("t"), " = ", 6)),
            expression(paste("CN, ", ~italic("t"), " = ", 12)),
            expression(paste("CN, ", ~italic("t"), " = ", 24)),
            expression(paste("CA, ", ~italic("t"), " = ", 6)),
            expression(paste("CA, ", ~italic("t"), " = ", 12)),
            expression(paste("CA, ", ~italic("t"), " = ", 24)))

# Prepare graphics device
tiff(filename = "Figure S3.tif", width = 2800, height = 3200, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1600, 1400)
pushViewport(viewport(layout = gly))

# Plot survival curves
# Error bars excluded for plotting clarity
print(surv.plotsAll(Data_List, PlotColours_All),
      vp = viewport(layout.pos.row = 25:1575, layout.pos.col = 25:1375))

# Create legend
grid.text(label = c("CN W E+ [1]", "CN W E- [2]", "CN NW E+ [3]", "CN NW E- [4]",
                    "CA W E+ [5]", "CA W E- [6]", "CA NW E+ [7]", "CA NW E- [8]"), x = rep(0.905, 8), 
          y =  seq(0.945, 0.761, length.out = 8), hjust = rep(1, 8), gp = gpar(cex = 0.45))
grid.text(label = c("1", "3", "7", "6", "5", "2", "8", "4"),
          x = c(0.216, 0.312, 0.354, 0.395, 0.408, 0.443, 0.495, 0.546),
          y = rep(0.420, 8), gp = gpar(cex = 0.45))
grid.segments(x0 = rep(0.920, 8), y0 = seq(0.945, 0.761, length.out = 8), 
              x1 = rep(0.937, 8), y1 = seq(0.945, 0.761, length.out = 8),
              gp = gpar(col = PlotColours_All, lty = rep(1, 8), lwd = rep(1.1, 8)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

