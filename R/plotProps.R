

#Plot chronology properties
plotProps <- function(chron, climDat, testDat){
  #Requires rwi, testdata, climate data

  #choose only the sim chrons

  simIndex <- unlist(lapply(testDat, function(x) x$runDetails$Chronology)) == "sim1"

  origIndex <- !simIndex

  sims <- testDat[simIndex]

  orig <- testDat[origIndex]

  simCors <- unlist(lapply(sims, function(x)
    cor(chron(x$synchron$synChron)[as.numeric(rownames(x$synchron$synChron)) %in% climDat[,1],1],
        climDat[climDat[,1] %in% as.numeric(rownames(x$synchron$synChron)),2])))

  chron646mv <- chron(chron)
  origCor <- cor(chron646mv[as.numeric(rownames(chron646mv)) %in% climDat[,1],1], climDat[climDat[,1] %in% as.numeric(rownames(chron646mv)),2])


  simRbar <- unlist(lapply(sims, function(x) rwi.stats(x$synchron$synChron)$rbar.eff))
  origRbar <- rwi.stats(chron)$rbar.eff

  simAR1 <- unlist(lapply(sims, function(x) x$synchron$synChronData$surChronAR1))
  origAR1 <- acf(chron646mv[,1], plot = FALSE)$acf[2]

  simAR1 <- unlist(lapply(sims, function(x) x$synchron$synChronData$surChronAR1))


  sims[[1]]$synchron$synChron

  simSD <- list()
  for (i in 1:length(sims)){
    simSD[[i]] <- mean(apply(sims[[i]]$synchron$synChron[as.numeric(rownames(sims[[i]]$synchron$synChron)) %in% climDat[,1],], 1, function(x) sum(!is.na(x))))
  }
  simSD <- unlist(simSD)
  origSD <- mean(apply(chron[as.numeric(rownames(chron)) %in% climDat[,1],], 1, function(x) sum(!is.na(x))))

  chron1 <- "chron"[simIndex]


  chron1 <- rep("sim", length(simSD))
  chron1 <- c(chron1, rep("chron", length(origSD)))

  allData <- data.frame(
    "AR1" = c(simAR1, origAR1),
    "rbar" = c(simRbar, origRbar),
    "Cor" = c(simCors, origCor),
    "SD" = c(simSD, origSD),
    "chron" = chron1
  )

  plotData1 <- melt(allData, id.vars="chron")
  head(plotData1)
  plotData1$value[plotData1$variable=="SD"] <- plotData1$value[plotData1$variable=="SD"]/100

  p001 <- ggplot(data = plotData1, mapping = aes(y=value, x = variable, shape=chron, color=variable)) +
    geom_quasirandom(alpha = 0.8, width = 0.5, mapping = aes(shape=chron, size=chron, color=variable)) +
    scale_size_manual(values = c(3,1.5))+
    scale_shape_manual(values = c(17,20))+
    scale_y_continuous(sec.axis = sec_axis(~ . * 100, name = "sample depth"))+
    #scale_alpha_manual(values = c(1,0.1))+
    theme_classic()+
    facet_grid(.~variable, scales = "free", space = "free")+
    #scale_color_brewer(palette="Dark2")+
    #scale_color_manual(values = c("red", "black"))+
    theme(
      axis.title.x = element_blank(),
      text = element_text(size = 10, color = "black"),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
  print(p001)

  returns <- list(tibble::as_tibble(allData), p001)

  return(returns)

}

