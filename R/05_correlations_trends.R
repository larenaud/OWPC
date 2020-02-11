

randEffSmplMean <- apply(randEffModel[,,,2], 1:2, mean) 
randEffSmpl.025 <- apply(randEffModel[,,,2], 1:2,
                         quantile, probs = 0.025)
randEffSmpl.975 <- apply(randEffModel[,,,2], 1:2,
                         quantile, probs = 0.975)

SmplDrawCol <- matrix(NA, nrow = nrow(randEffSmplMean),
                      ncol = ncol(randEffSmplMean))

SmplDrawCol[which(randEffSmplMean > 0.0, arr.ind=TRUE)] <- "orangered" # removed threshold of 0.4
SmplDrawCol[which(randEffSmplMean < -0.0, arr.ind=TRUE)] <- "blue"

# Build matrix of "significance" for corrplot
SmplDraw <- SmplDrawCol
SmplDraw[which(!is.na(SmplDraw), arr.ind = TRUE)] <- 0
SmplDraw[which(is.na(SmplDraw), arr.ind = TRUE)] <- 1
SmplDraw <- matrix(as.numeric(SmplDraw),
                   nrow = nrow(randEffSmplMean),
                   ncol = ncol(randEffSmplMean))

# Draw correlation matrix
Colour <- colorRampPalette(c("blue", "white", "orangered"))(200)
par()
CorrRes <- corrplot(randEffSmplMean, order = "original",
                    p.mat = SmplDraw, insig = "n",
                    method = "color", col = Colour,
                    addgrid.col = "darkgray", cl.pos = "r",
                    tl.col = "black", tl.cex = 1, cl.cex = 1,
                    type = "full", tl.pos = "tl", bg="white",
                    diag = TRUE)
