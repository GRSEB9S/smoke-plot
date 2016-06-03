###################################################
##### viz.uncertainty
##### A function for visualizing posterior uncertainty
# Arguments:-
# mat: a matrix, usually three column vectors (1) covariate distribution (2) mean estimates (3) error estimates
# nsim: number of draws from posterior
# conf: confidence level (note that 95% confidence requires conf=0.975)
# palette: a color palette for shading
# shade.alpha: extent of transparency (higher values means more transparency in sparse areas)
# xlab: x-axis label
# ylab: y-axis label
# ybreaks: vector for desired location of y-axis tick marks
# spag: plot spaghetti lines (one line per draw)
# median: plot median line
# medianlwd: median linewidth
# mediancol: color of median line
###################################################

viz.uncertainty <- function (mat, nsim=1000, conf=0.975, palette=magma(40), slices=200, shade.alpha=0.1,
                       xlab='t', ylab='beta(t)', ybreaks=NULL, spag=FALSE, median=FALSE, medianlwd=1, mediancol='orange') {
  
  normv <- function(n, mean, sd){
    if (!is.null(conf)) {
      out <- rnorm(n*length(mean), mean = mean, sd = sd * qnorm(conf))
    } else {
      out <- rnorm(n*length(mean), mean = mean, sd = sd)
    }
    return( matrix(out, ncol = n, byrow = FALSE))
  }
  
  set.seed(10)
  draws <- normv(nsim, mat[,2], mat[,5])
  
  lo <- list()
  for (i in 1:ncol(draws)) {
    lo[[i]] <- loess(draws[,i] ~ mat[,1], span = 0.25,
                     control = loess.control(surface = "i", statistics="a", trace.hat="a"))
  }

  # should I plot the median line?
  med <- smooth.spline(mat[,1], rowMeans(draws), spar=0.25)
  med <- data.frame(x=med$x, y=med$y)
  
  smooth.mat <- matrix(NA, ncol=length(lo), nrow = length(mat[,1]))
  for(i in 1:length(lo)) {
    smooth.mat[,i] <- lo[[i]]$fitted
  }

  smooth.long <- melt(smooth.mat) 
  smooth.long$x <- rep(mat[,1], nsim)
  smooth.long <- smooth.long[with(smooth.long, order(x)), ]

  library(ggplot2)
  library(viridis)

  if (is.null(ybreaks)) {
    ybreaks <- round(seq(min(smooth.long$value), max(smooth.long$value), by = 2), 0)
  }
  
  p0 <- ggplot(smooth.long, aes(x=x, y=value)) + 
    xlab(xlab) + ylab(ylab) + 
    scale_x_continuous(breaks = round(seq(min(smooth.long$x), max(smooth.long$x), by = 180), 0)) +
    scale_y_continuous(breaks = ybreaks) + 
    theme_bw(base_size = 8) + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())

  gg.tiles <- gg.poly <- gg.spag <- gg.median <- gg.CI1 <- gg.CI2 <- gg.lm <- gg.points <- gg.title <- NULL

  print("Computing density estimates for each vertical cut ...")
  flush.console()

  min_value <- min(smooth.long[,3], na.rm=TRUE)
  max_value <- max(smooth.long[,3], na.rm=TRUE)
  ylim <- c(min_value, max_value)

  # vertical cross-sectional density estimate
  d2 <- ddply(smooth.long[, c("x", "value")], .(x), function(df) {
    res <- data.frame(density(df$value, na.rm=TRUE, n=slices, from=ylim[1], to=ylim[2])[c("x", "y")])
    colnames(res) <- c("y", "dens")
    return(res)
  }, .progress="text")

  maxdens <- max(d2$dens)
  mindens <- min(d2$dens)
  d2$dens.scaled <- (d2$dens - mindens)/maxdens  
  d2$alpha.factor <- d2$dens.scaled^shade.alpha
  
  # define gg elements
  gg.tiles <-  list(geom_tile(data=d2, aes(x=x, y=y, fill=dens.scaled, alpha=alpha.factor)), scale_fill_gradientn("dens.scaled", colours=palette), scale_alpha_continuous(range=c(0.001, 1)))
  gg.median <- geom_path(data=med, aes(x=x, y=y), size=medianlwd, linejoin="mitre", color=mediancol)
  gg.spag <-  geom_path(data=smooth.long, aes(x=x, y=value, group=Var2), size=0.7, alpha=10/1000, color=viridis(1))
  gg.hline <- geom_hline(yintercept=0, linetype=2)
  
  print("Build ggplot figure ...")
  flush.console()
  
  if (median==TRUE) {
    gg.elements <- list(gg.tiles, gg.median, gg.hline, theme(legend.position="none"))
  }
  
  if (spag==TRUE) {
    gg.elements <- list(gg.tiles, gg.spag, gg.hline, theme(legend.position="none"))
  }
  
  if (median==FALSE & spag==FALSE) {
    gg.elements <- list(gg.tiles, gg.hline, theme(legend.position="none"))
  }
  
  p0 + gg.elements
}
