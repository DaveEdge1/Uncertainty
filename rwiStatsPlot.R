rwiStatsPlot <- function(rwi1, winLen = 100){
  df1 <- rwi.stats.running(rwi1, window.length = winLen, window.overlap = (winLen-1))
  df2 <- melt(data.frame("Year" = df1$mid.year, "rbar" = df1$rbar.eff, "eps" = df1$eps), id.vars = "Year")
  p1 <- ggplot(data=df2, mapping = aes(x=Year, y=value, color=variable)) + geom_line()
  return(p1)
}

data("ca533")
rwiStatsPlot(ca533)
