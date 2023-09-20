plotCapture <- function(testDat){
  df0 <- data.frame(matrix(ncol = 5, data=NA))
  names(df0) <- c("Capture", "boot", "chron", "PI", "Err")
  for (i in 1:length(testDat)){
    df1 <- data.frame(
      "Capture" = as.numeric(testDat[[i]]$CaptureSummary),
      "boot" = testDat[[i]]$runDetails$BootstMethod,
      "chron" = testDat[[i]]$runDetails$Chronology,
      "PI" = testDat[[i]]$runDetails$`Prediction Interval`
    )
    df1$Err <- c("T", "E", "T", "E")
    names(df1) <- c("Capture", "boot", "chron", "PI", "Err")

    df0 <- rbind(df0, df1)
  }

  df0 <- df0[complete.cases(df0),]

  plotDat <- melt(df0, id.vars = c("boot", "chron", "PI", "Err"))

  pal1 <- c("#cc8b86","#54494b","#508484","#f3a712","#cfd11a")

  numplots <- unique(plotDat$PI)
  if (numplots == 1){
    p1 <- ggplot(data=plotDat, mapping = aes(x=PI, y=value, fill=boot)) +
      #geom_hline(yintercept = 0.5, lty=2, color=alpha("black", 0.5))+
      geom_violin() +
      geom_quasirandom(alpha = 0.8, width = 0.16, mapping = aes(shape=chron, size=chron)) +
      scale_size_manual(values = c(3,1.5))+
      #scale_color_manual(values = c("darkgreen", "blue"))+
      scale_shape_manual(values = c(17,20))+
      facet_grid(boot~Err, scales = "free", space="free_x") +
      #scale_fill_brewer(palette="BrBG") +
      scale_fill_manual(values = pal1)+
      theme_classic()+
      #scale_y_continuous(limits = c(.45,.70))+
      ylab(" ")+
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(size = 10, color = "black"),
        legend.title = element_blank()
      )
    print(p1)
  }else{
    for (i in numplots){
      p1 <- ggplot(data=plotDat[plotDat$PI == i,], mapping = aes(x=PI, y=value, fill=boot)) +
        #geom_hline(yintercept = 0.5, lty=2, color=alpha("black", 0.5))+
        geom_violin() +
        geom_quasirandom(alpha = 0.8, width = 0.16, mapping = aes(shape=chron, size=chron)) +
        scale_size_manual(values = c(3,1.5))+
        #scale_color_manual(values = c("darkgreen", "blue"))+
        scale_shape_manual(values = c(17,20))+
        facet_grid(boot~Err, scales = "free", space="free_x") +
        #scale_fill_brewer(palette="BrBG") +
        scale_fill_manual(values = pal1)+
        theme_classic()+
        #scale_y_continuous(limits = c(.45,.70))+
        ylab(" ")+
        theme(
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          text = element_text(size = 10, color = "black"),
          legend.title = element_blank()
        )
    }
    print(p1)
  }
}



