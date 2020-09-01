###
### Vignette
###

# # setwd
# setwd("U:/sandbox/spatialsocialmedia/vignette")
# source("U:/sandbox/spatialsocialmedia/R/get.interactions.R")
# source("U:/sandbox/spatialsocialmedia/R/plot.spatial.network.R")
load("U:/sandbox/spatialsocialmedia/data/covid.ie.g.rda")

# load package
library(SpatialSocialMedia)
data("covid.ie.g")

# inspect graph
igraph::list.vertex.attributes(covid.ie.g)
igraph::list.edge.attributes(covid.ie.g)

# sample for speed
library(igraph)
covid.ie.g <- subgraph.edges(graph=covid.ie.g, eids=which(as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")<="2020-03-31"), delete.vertices=TRUE)

# plot montly snapshots with igraph
par(mfrow=c(2,5), mai = c(1, 0.1, 0.1, 0.1))
# plot with R maps
plot.spatial.network(package = "igraph", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), edge.width.igraph = 0.1)
plot.spatial.network(package = "network", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"))
plot.spatial.network(package = "geosphere", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), database = "world", edge.width.ggplot = 10)
plot.spatial.network(package = "ggplot2", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), database = "world", edge.width.ggplot = 1)
plot.spatial.network(package = "ggraph", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), database = "world")
# plot with GADM
plot.spatial.network(package = "igraph", geo.graph = covid.ie.g, database = "gadm", gadm.level = 1, area=c("Ireland", "United Kingdom:Northern Ireland"), edge.width.igraph = 0.1)
plot.spatial.network(package = "network", geo.graph = covid.ie.g, database = "gadm", gadm.level = 1, area=c("Ireland", "United Kingdom:Northern Ireland"))
plot.spatial.network(package = "geosphere", geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), database = "gadm", gadm.level = 1, edge.width.ggplot = 10)
plot.spatial.network(package = "ggplot2", geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), database = "gadm", gadm.level = 1, edge.width.ggplot = 1)
plot.spatial.network(package = "ggraph", geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), database = "gadm", gadm.level = 1)

# plot with igraph
png(file="plot.test.igraph.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "igraph", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), edge.width.igraph = 0.1)
dev.off()

# plot with igraph and GADM
png(file="plot.test.igraph.gadm.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "igraph", geo.graph = covid.ie.g, database = "gadm", gadm.level = 1, area=c("Ireland", "United Kingdom:Northern Ireland"), edge.width.igraph = 0.1)
dev.off()

# plot with network
png(file="plot.test.network.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "network", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"))
dev.off()

# plot with igraph and GADM
png(file="plot.test.network.gadm.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "network", geo.graph = covid.ie.g, database = "gadm", gadm.level = 1, area=c("Ireland", "United Kingdom:Northern Ireland"))
dev.off()

# plot with geosphere
png(file="plot.test.geosphere.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "geosphere", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), database = "world", edge.width.ggplot = 10)
dev.off()

# plot with geosphere and GADM
png(file="plot.test.geosphere.gadm.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "geosphere", geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), database = "gadm", gadm.level = 1, edge.width.ggplot = 10)
dev.off()

# plot with ggplot2
png(file="plot.test.ggplot2.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "ggplot2", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), database = "world", edge.width.ggplot = 1)
dev.off()

# plot with ggplot2 and GADM
png(file="plot.test.ggplot2.gadm.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "ggplot2", geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), database = "gadm", gadm.level = 1, edge.width.ggplot = 1)
dev.off()

# plot with ggraph
png(file="plot.test.ggraph.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "ggraph", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), database = "world")
dev.off()

# plot with ggraph and GADM
png(file="plot.test.ggraph.gadm.png", type='cairo', width=15,height=15, units='in', res=300)
plot.spatial.network(package = "ggraph", geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), database = "gadm", gadm.level = 1)
dev.off()

# montly messages break down as following
summary(substr(as.Date(as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")), 1, 7))

# sample network
subgraph.month <- list()
subgraph.month[[1]] <- which(as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-03-31")
subgraph.month[[2]] <- which(as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-03-31" & as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-04-30")
subgraph.month[[3]] <- which(as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-04-30" & as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-05-31")
subgraph.month[[4]] <- which(as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-05-31" & as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-06-30")
subgraph.month[[5]] <- which(as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-06-30" & as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-07-31")
subgraph.month[[6]] <- which(as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-07-31" & as.POSIXct(E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-08-31")
names(subgraph.month) <- c("March", "April", "May", "June", "July", "August")
lapply(subgraph.month, length)

# plot montly snapshots with igraph
par(mfrow=c(2,3), mai = c(1, 0.1, 0.1, 0.1))
for(i in 1:length(subgraph.month)) {
  G <- igraph::subgraph.edges(graph=covid.ie.g, eids=subgraph.month[[i]], delete.vertices=TRUE)
  # plot the graph in a new window
  plot.spatial.network(geo.graph = G, database = "gadm", gadm.level = 1,
                       area=c("Ireland", "United Kingdom:Northern Ireland"),
                       package = "igraph", edge.width.igraph = 1)
  title(paste0("COVID-19 ", names(subgraph.month)[i]), cex.main=5.5, col.main="black")
  # legend(x="bottom", paste0("N=",cp.node.count.aggie[i]," cp=", cp.cetrality.aggie[i], " rand=",cp.cetrality.noagg[i]), title="", cex=2.5, bty = "n", xpd=TRUE, ncol=1)
}

# plot montly snapshots with ggraph
subgraph.month.g <- list()
for(i in 1:length(subgraph.month)) {
  G <- igraph::subgraph.edges(graph=covid.ie.g, eids=subgraph.month[[i]], delete.vertices=TRUE)
  # plot the graph in a new window
  subgraph.month.g[[i]] <- plot.spatial.network(geo.graph = G, database = "gadm", gadm.level = 1, nodeLabelnum = 15,
                                                area=c("Ireland", "United Kingdom:Northern Ireland"), package = "ggraph",
                                                vertex.label.cex.ggraph = 10)
  # title(paste0("COVID-19 ", names(subgraph.month)[i]), cex.main=5.5, col.main="black")
  # legend(x="bottom", paste0("N=",cp.node.count.aggie[i]," cp=", cp.cetrality.aggie[i], " rand=",cp.cetrality.noagg[i]), title="", cex=2.5, bty = "n", xpd=TRUE, ncol=1)
}
par(mfrow=c(2,3), mai = c(1, 0.1, 0.1, 0.1))
gridExtra::grid.arrange(subgraph.month.g[[1]], subgraph.month.g[[2]], subgraph.month.g[[3]], subgraph.month.g[[4]],
                        subgraph.month.g[[5]], subgraph.month.g[[6]], ncol=3, nrow =2)

# fin
