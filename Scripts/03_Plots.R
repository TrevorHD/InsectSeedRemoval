##### [F2] Plot E+ vs E- survival curve -------------------------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure 2.tif", width = 2800, height = 2000, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1400)
pushViewport(viewport(layout = gly))

# CN Unwarmed: E+ (dark green) v E- (light green)
print(surv.plots(Data_CN_NW_YE, Data_CN_NW_NE, PlotColours[1], PlotColours[2],
                 bottom = FALSE, left = TRUE, atext = "CN Unwarmed"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 25:700))

# CN Warmed: E+ (dark green) v E- (light green)
print(surv.plots(Data_CN_YW_YE, Data_CN_YW_NE, PlotColours[1], PlotColours[2],
                 bottom = TRUE, left = TRUE, atext = "CN Warmed"),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 25:700))

# CA Unwarmed: E+ (dark green) v E- (light green)
print(surv.plots(Data_CA_NW_YE, Data_CA_NW_NE, PlotColours[1], PlotColours[2],
                 bottom = FALSE, left = FALSE, atext = "CA Unwarmed"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 700:1375))

# CA Warmed: E+ (dark green) v E- (light green)
print(surv.plots(Data_CA_YW_YE, Data_CA_YW_NE, PlotColours[1], PlotColours[2],
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





##### [F3] Plot Warmed vs Unwarmed survival curve ---------------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure 3.tif", width = 2800, height = 2000, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1400)
pushViewport(viewport(layout = gly))

# CN E+: Warmed (red) v unwarmed (blue)
print(surv.plots(Data_CN_NW_YE, Data_CN_YW_YE, PlotColours[3], PlotColours[4],
                 bottom = FALSE, left = TRUE, atext = "CN E+"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 25:700))

# CN E-: Warmed (red) v unwarmed (blue)
print(surv.plots(Data_CN_NW_NE, Data_CN_YW_NE, PlotColours[3], PlotColours[4],
                 bottom = TRUE, left = TRUE, atext = "CN E-"),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 25:700))

# CA E+: Warmed (red) v unwarmed (blue)
print(surv.plots(Data_CA_NW_YE, Data_CA_YW_YE, PlotColours[3], PlotColours[4],
                 bottom = FALSE, left = FALSE, atext = "CA E+"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 700:1375))

# CA E: Warmed (red) v unwarmed (blue)
print(surv.plots(Data_CA_NW_NE, Data_CA_YW_NE, PlotColours[3], PlotColours[4],
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





##### [FS3] Plot all survival curves simultaneously -------------------------------------------------------

# Error bars excluded for plotting clarity

# Prepare graphics device
tiff(filename = "FigureS1.tif", width = 2800, height = 3200, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1600, 1400)
pushViewport(viewport(layout = gly))

# Plot survival curves
print(surv.plotsAll(Data_List, PlotColours_All), vp = viewport(layout.pos.row = 25:1575, layout.pos.col = 25:1375))

# Create legend
grid.text(label = c("CN W E+ [1]", "CN W E- [2]", "CN NW E+ [3]", "CN NW E- [4]",
                    "CA W E+ [5]", "CA W E- [6]", "CA NW E+ [7]", "CA NW E- [8]"), x = rep(0.905, 8), 
          y =  seq(0.945, 0.761, length.out = 8), hjust = rep(1, 8), gp = gpar(cex = 0.45))
grid.text(label = c("1", "3", "7", "6", "5", "2", "8", "4"),
          x = c(0.216, 0.312, 0.354, 0.395, 0.408, 0.443, 0.495, 0.546), y = rep(0.420, 8), gp = gpar(cex = 0.45))
grid.segments(x0 = rep(0.920, 8), y0 = seq(0.945, 0.761, length.out = 8), 
              x1 = rep(0.937, 8), y1 = seq(0.945, 0.761, length.out = 8),
              gp = gpar(col = PlotColours_All, lty = rep(1, 8), lwd = rep(1.1, 8)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [FS4] Marginal effect plots for interactions (species separate) -------------------------------------

# Prepare graphics device
tiff(filename = "FigureS3.tif", width = 2400, height = 2000, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1200)
pushViewport(viewport(layout = gly))

# Marginal plots for Warmed:Elaiosome interaction (CN)
print(plot_model(Fit6_CN, type = "pred", terms = c("Warmed", "Elaiosome"), colors = c("green", "darkgreen")) +
        ip_m2 + ip_m3 + ip_t1, vp = viewport(layout.pos.row = 1:500, layout.pos.col = 1:450))
print(plot_model(Fit12_CN, type = "pred", terms = c("Warmed", "Elaiosome"), colors = c("green", "darkgreen")) +
        ip_m2 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 1:500, layout.pos.col = 450:825))
print(plot_model(Fit24_CN, type = "pred", terms = c("Warmed", "Elaiosome"), colors = c("green", "darkgreen")) +
        ip_m2 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 1:500, layout.pos.col = 825:1200))
# Elaiosome removal consistently results in lower seed removal rates
# Warming results in significant increase in removal rates at t=6 and t=24; significant interaction at t=12

# Marginal plots for Warmed:Elaiosome interaction (CA)
print(plot_model(Fit6_CA, type = "pred", terms = c("Warmed", "Elaiosome"), colors = c("green", "darkgreen")) +
        ip_m2 + ip_m3 + ip_t1, vp = viewport(layout.pos.row = 500:1000, layout.pos.col = 1:450))
print(plot_model(Fit12_CA, type = "pred", terms = c("Warmed", "Elaiosome"), colors = c("green", "darkgreen")) +
        ip_m2 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 500:1000, layout.pos.col = 450:825))
print(plot_model(Fit24_CA, type = "pred", terms = c("Warmed", "Elaiosome"), colors = c("green", "darkgreen")) +
        ip_m2 + ip_m3 + ip_t2, vp = viewport(layout.pos.row = 500:1000, layout.pos.col = 825:1200))
# When there is no warming, seeds with elaiosomes are more likely to be removed
# When warming is added, this difference mostly disappears; there is an obvious interaction
# Warming drastically increases chance of removal E- seeds, but not really for E+

# Create plot labels
grid.text(label = tlistc, x = rep(seq(0.34, 0.966, length.out = 3), 2), y = c(rep(0.950, 3), rep(0.450, 3)),
          hjust = rep(1, 6), gp = gpar(cex = 0.3))

# Create legend
grid.text(label = c("E+", "E-"), x = rep(0.132, 2),  y = c(0.950, 0.925), hjust = rep(0, 2),
          gp = gpar(cex = 0.3))
grid.segments(x0 = rep(0.109, 2), y0 = c(0.950, 0.925), x1 = rep(0.125, 2), y1 = c(0.950, 0.925),
              gp = gpar(col = c(PlotColours[1], PlotColours[2]), lty = rep(1, 2), lwd = rep(1.1, 2)))

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### [FS5] Plot CN vs CA survival curve ------------------------------------------------------------------

# Prepare graphics device
tiff(filename = "Figure S5.tif", width = 2800, height = 2000, units = "px", res = 800, compression = "lzw")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it
gly <- grid.layout(1000, 1400)
pushViewport(viewport(layout = gly))

# E+ Unwarmed: CN (black) v CA (grey)
print(surv.plots(Data_CA_NW_YE, Data_CN_NW_YE, PlotColours[5], PlotColours[6],
                 bottom = FALSE, left = TRUE, atext = "E+ Unwarmed"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 25:700))

# E+ Warmed: CN (black) v CA (grey)
print(surv.plots(Data_CA_YW_YE, Data_CN_YW_YE, PlotColours[5], PlotColours[6],
                 bottom = TRUE, left = TRUE, atext = "E+ Warmed"),
      vp = viewport(layout.pos.row = 500:975, layout.pos.col = 25:700))

# E- Unwarmed: CN (black) v CA (grey)
print(surv.plots(Data_CA_NW_NE, Data_CN_NW_NE, PlotColours[5], PlotColours[6],
                 bottom = FALSE, left = FALSE, atext = "E- Unwarmed"),
      vp = viewport(layout.pos.row = 25:500, layout.pos.col = 700:1375))

# E- Warmed: CN (black) v CA (grey)
print(surv.plots(Data_CA_YW_NE, Data_CN_YW_NE, PlotColours[5], PlotColours[6],
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

