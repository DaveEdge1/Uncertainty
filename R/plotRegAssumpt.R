#Regression Assumptions




plotRegAssumpt <- function(testDat){

  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop(
      "Package 'reshape2' must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' must be installed to use this function.",
      call. = FALSE
    )
  }

  df1 <- data.frame(
    "PIwidth" <- unlist(lapply(testDat, function(x) x$runDetails$`Prediction Interval`)),
    "chronology" <- unlist(lapply(testDat, function(x) x$runDetails$Chronology)),
    "bootMethod" <- unlist(lapply(testDat, function(x) x$runDetails$BootstMethod)),
    "normality" <- unlist(lapply(testDat, function(x) x$regAssumpt$residsNotNorm)),
    "AR1" <- unlist(lapply(testDat, function(x) x$regAssumpt$residAR1)),
    "varTrend" <- unlist(lapply(testDat, function(x) x$regAssumpt$residsHeteroscedastic))
  )
  names(df1) <- c(  "PIwidth",
                    "chronology",
                    "bootMethod",
                    "normality",
                    "AR1",
                    "varTrend")

  plotData1 <- melt(df1, id.vars = c("PIwidth", "chronology","bootMethod"))
  plotData1$significance <- NA
  plotData1$significance[plotData1$variable == "normality"] <- 0.05
  plotData1$significance[plotData1$variable == "AR1"] <- 0.25
  plotData1$significance[plotData1$variable == "varTrend"] <- 0.05
  plotData2 <- plotData1[,c("variable", "significance")]

  p001 <- ggplot(data = plotData1, mapping = aes(y=value, x = variable, shape=chronology, color=PIwidth)) +
    #geom_line(mapping = aes(y=significance), lty=2, color=alpha("red", 0.5), inherit.aes = FALSE)+
    geom_hline(aes(yintercept = significance), lty=2, color=alpha("red", 0.5), data = plotData2)+
    geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chronology, size=chronology, color=PIwidth)) +
    scale_size_manual(values = c(3,1.5))+
    scale_shape_manual(values = c(17,20))+
    #scale_alpha_manual(values = c(1,0.1))+
    theme_classic()+
    facet_grid(variable~bootMethod, scales = "free", space = "fixed")+
    #scale_color_brewer(palette="Dark2")+
    #scale_color_manual(values = c("red", "black"))+
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      text = element_text(size = 10, color = "black"),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
  print(p001)

  df1 <- tibble::as_tibble(df1)
  returns <- list(df1, p001)
  return(returns)
}















