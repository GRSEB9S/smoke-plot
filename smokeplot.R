######################################################
##################   smoke.plot  ##################### 
## A function for visualizing posterior uncertainty ##
######################################################
# Arguments:-
# mat: a matrix, usually three column vectors:
#     (1) covariate distribution (2) mean estimates (3) error estimates
# nsim: number of draws from posterior
# palette: a color or color palette for smoke
# slices: slices used for denisty estimation
#         (more = longer run time)
# smoke: plot smoke uncertainty
# smoke.alpha: extent of transparency
#              (higher values = greater transparency in sparse areas)
# spag: plot spaghetti lines (one line per draw)
# ci: plot confidence interval (takes conf level set above)
# conf: confidence level (note that 95% confidence requires conf=0.975)
# cilwd: confidence interval linewidth
# cicol: color of confidence interval lines
# median: plot median line
# medianlwd: median linewidth
# mediancol: color of median line
# hint: horizontal line intercept
# xlab: x-axis label
# ylab: y-axis label
# ybreaks: vector for desired location of y-axis tick marks
# ylim: limits of the y-axis
# title: plot title
# seed: seed for draws from posterior
######################################################

smokeplot <- function (mat, draws=TRUE, smooth=TRUE, nsim=1000, palette=magma(40), slices=200, smoke=TRUE, smoke.alpha=0.1, spag=FALSE,
                       shape=21, ci=FALSE, cilwd=1, cicol='black', conf=0.975,
                       median=FALSE, medianlwd=1, mediancol='black', hline=NULL, 
                       xlab='x', ylab='', title='', theme=NULL, seed=1,
                       ybreaks=NULL, ylim=NULL, yby=2, xbreaks=NULL, xlim=NULL, xby=2) {
  
  library(ggplot2)
  library(reshape2)
  library(plyr)
  library(viridis)
  library(ggthemes)
  
  if (draws==TRUE) {
    normv <- function(n, mean, sd){
      out <- rnorm(n*length(mean), mean = mean, sd = sd)
      return( matrix(out, ncol = n, byrow = FALSE))
    }
  
    set.seed(seed)
    draws <- normv(nsim, mat[,2], mat[,3])
  }  
  
  if (smooth==TRUE) {
    lo <- list()
    for (i in 1:ncol(draws)) {
      lo[[i]] <- loess(draws[,i] ~ mat[,1], span = 0.25,
                       control = loess.control(surface = "i", statistics="a", trace.hat="a"))
    }
    smooth.mat <- matrix(NA, ncol=length(lo), nrow = length(mat[,1]))
    for(i in 1:length(lo)) {
      smooth.mat[,i] <- lo[[i]]$fitted
    }
    smooth.long <- melt(smooth.mat)
    smooth.long$x <- rep(mat[,1], nsim)
    smooth.long <- smooth.long[with(smooth.long, order(x)), ]
  } else {
    smooth.long <- mat[with(mat, order(x)), ]
  }

  if (is.null(ybreaks)) {
    ybreaks <- round(seq(min(smooth.long$value), max(smooth.long$value), by = yby), 0)
  }
  if (is.null(xbreaks)) {
    xbreaks <- round(seq(min(smooth.long$x), max(smooth.long$x), by = xby), 0)
  }
  if (is.null(ylim)) {
    ylim <- c(min(smooth.long$value), max(smooth.long$value))
  }
  if (is.null(xlim)) {
    xlim <- c(min(smooth.long$x), max(smooth.long$x))
  }
  
  if (is.null(theme)) {
    p0 <- ggplot(smooth.long, aes(x=x, y=value)) + 
      xlab(xlab) + ylab(ylab) +
      scale_x_continuous(expand = c(0, 0), breaks = xbreaks, limits = xlim) +
      scale_y_continuous(expand = c(0, 0), breaks = ybreaks, limits = ylim) + theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  } else {
    p0 <- ggplot(smooth.long, aes(x=x, y=value)) + 
      xlab(xlab) + ylab(ylab) +
      scale_x_continuous(expand = c(0, 0), breaks = xbreaks, limits = xlim) + 
      scale_y_continuous(expand = c(0, 0), breaks = ybreaks, limits = ylim) + theme
  }
  
  gg.raster <- gg.hline <- gg.spag <- gg.median <- gg.ci1 <- gg.ci2 <- gg.title <- NULL
  
  # define gg elements
  if (smoke == TRUE) {
    min_value <- min(smooth.long[,3], na.rm=TRUE)
    max_value <- max(smooth.long[,3], na.rm=TRUE)
    if (is.null(ylim)) {
      ylim <- c(min_value, max_value)
    }
    # vertical cross-sectional density estimate
    print("Computing density estimates for each vertical cut ...")
    flush.console()
    d2 <- ddply(smooth.long[, c("x", "value")], .(x), function(df) {
      res <- data.frame(density(df$value, na.rm=TRUE, n=slices, from=ylim[1], to=ylim[2])[c("x", "y")])
      colnames(res) <- c("y", "dens")
      return(res)
    }, .progress="text")
    maxdens <- max(d2$dens)
    mindens <- min(d2$dens)
    d2$dens.scaled <- (d2$dens - mindens)/maxdens  
    d2$alpha.factor <- d2$dens.scaled^smoke.alpha
    gg.raster <-  list(geom_raster(data=d2, aes(x=x, y=y, fill=dens.scaled, alpha=alpha.factor), interpolate=TRUE),
                       scale_fill_gradientn("dens.scaled", colours=palette), scale_alpha_continuous(range=c(0.001, 1)))
  }
  if (median == TRUE) {
    med <- smooth.spline(mat[,1], rowMeans(draws), spar=0.25)
    med <- data.frame(x=med$x, y=med$y)
    gg.median <- geom_path(data=med, aes(x=x, y=y), size=medianlwd, linejoin="mitre", color=mediancol)
  }
  if (spag == TRUE) {
    gg.spag <- geom_path(data=smooth.long, aes(x=x, y=value, group=Var2), size=0.7, alpha=10/1000, color='black')
  }
  if (ci == TRUE) {
    conf.dat <- data.frame(upr = mat[,2] + (mat[,3]*qnorm(conf)),
                           lwr = mat[,2] - (mat[,3]*qnorm(conf)),
                           x = mat[,1])
    gg.ci1 <- geom_path(data=conf.dat, aes(x=x, y=upr), size=cilwd, linetype=2, color=cicol)
    gg.ci2 <- geom_path(data=conf.dat, aes(x=x, y=lwr), size=cilwd, linetype=2, color=cicol)
  }
  # add horizontal line
  if (is.numeric(hline)) {
    gg.hline <- geom_hline(yintercept=hline, linetype=3)
  }
  
  gg.title <- ggtitle(title)
  
  print("Build ggplot figure ...")
  flush.console()
  
  gg.elements <- list(gg.raster, gg.spag, gg.median, gg.ci1, gg.ci2, gg.hline, gg.title, theme(legend.position="none"))
  
  p0 + gg.elements
}
