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
tiff(filename = "FigureS2.tif", width = 2400, height = 3000, units = "px", res = 800, compression = "lzw")

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
ip_m3 <- scale_y_continuous(name = "Probability of Removal", expand = c(0.01, 0.01), limits = c(0, 1), 
                            breaks = seq(0, 1, by = 0.1))

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

