
# Do all the plots I usually want.
# df: The input data frame

do_standard_plots <- function(df) {
  control_plot(df, 'plots/1h')
  
  AWS_plot(df, ycolumns = c("air_temp_3m_20", "air_temp_3m_30", "air_temp_3m_40", "air_temp_3m_50"), output = "plots/AWS/pot_temp.png")
  AWS_plot(df, ycolumns = c("wdir_20", "wdir_30", "wdir_40", "wdir_50"), output = "plots/AWS/wd.png")
  
  AWS_plot(df, ycolumns = c("cov_uT_20", "cov_vT_20"), output = "plots/AWS/uvT20.png", ylim=c(-0.5, 0.5))
  AWS_plot(df, ycolumns = c("cov_uT_30", "cov_vT_30"), output = "plots/AWS/uvT30.png", ylim=c(-0.5, 0.5))
  AWS_plot(df, ycolumns = c("cov_uT_40", "cov_vT_40"), output = "plots/AWS/uvT40.png", ylim=c(-0.5, 0.5))
  AWS_plot(df, ycolumns = c("cov_uT_50", "cov_vT_50"), output = "plots/AWS/uvT50.png", ylim=c(-0.5, 0.5))
  
  # A scatterplot
  AWS_plot(aws_joined_1h, ycolumns = "air_temp_3m_20", xcolumn = "air_temp_1m_20", linetype = "none", symbol = "o", output="plots/AWS/scatter.png")
 
}

do_corr_plots <- function(df) {

  aws_joined_cor = cor(df[ , !(names(df) %in% c("t"))], use="pairwise.complete.obs")
  png("plots/AWS/corrplot_all.png", width = 3000, height = 3000)  # you can set size/resolution
  corrplot(aws_joined_cor, tl.cex = 1, cl.cex = 3)
  title("Correlations: all stations, all variables", cex.main = 8)
  dev.off()
  
  for (i in c("20", "30", "40", "50")) {
    if (i == "20") cor_matrix = cor(aws20_1h[ , !(names(aws20_1h) %in% c("t"))], use="pairwise.complete.obs")
    if (i == "30") cor_matrix = cor(aws30_1h[ , !(names(aws30_1h) %in% c("t"))], use="pairwise.complete.obs")
    if (i == "40") cor_matrix = cor(aws40_1h[ , !(names(aws40_1h) %in% c("t"))], use="pairwise.complete.obs")
    if (i == "50") cor_matrix = cor(aws50_1h[ , !(names(aws50_1h) %in% c("t"))], use="pairwise.complete.obs")
    
    png(paste0("plots/AWS/corrplot_",i,".png"), width = 3000, height = 3000)  # you can set size/resolution
    corrplot(cor_matrix, tl.cex = 3, cl.cex=6)
    title("Correlations: all stations, all variables", cex.main = 8)
    dev.off()
  }
}