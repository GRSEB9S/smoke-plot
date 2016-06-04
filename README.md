# SmokePlot

A function (smoke.plot.R) for visualizing results uncertainty. The function draws from a user-supplied posterior distribution p(θ|y), generally in the form of three column vectors: (1) covariate values, (2) mean estimates and (3) error estimates. The draws are then graphed with visual weighting so that areas of the posterior with concentrated uncertainty are emphasized, whereas areas with diffuse uncertainty are less prominent. 

The aim is to avoid emphasizing the limits of the confidence interval at the expense of regions that are more information-rich. The function uses geom_raster and interpolation to produce smaller files with smoother edges. Traditional confidence intervals, a median line, and spaghetti lines can be added as options.

The code is adapted from [Felix Schönbrodt](http://www.nicebread.de/visually-weighted-watercolor-plots-new-variants-please-vote/) and is motivated by Solomon Hsiang's [paper](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2265501).

## Usage
```
  mat <- as.matrix(read.table("https://raw.github.com/george-wood/SmokePlot/master/example_matrix.txt", header=FALSE))
  source("https://raw.github.com/george-wood/SmokePlot/master/smoke.plot.R")
  smoke.plot(mat, palette=magma(10), slices=1000, smoke.alpha=0.5,
             ylim=c(-4, 1), title='color spectrum')
```

## Examples
![Ex1](/img/smoke_plots.png)

## Dependencies
* ```ggplot2```
* ```reshape2```
* ```viridis```
* ```plyr```
