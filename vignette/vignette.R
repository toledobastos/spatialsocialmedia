###
### Vignette
###

# setwd
setwd("U:/sandbox/spatialsocialmedia/vignette")

# # load required functions
source("U:/sandbox/spatialsocialmedia/R/get.interactions.R")
source("U:/sandbox/spatialsocialmedia/R/plot.spatial.network.R")

# load required packages
lapply(c("igraph","maps","ggplot2","geosphere","SpatNet","ggraph","sp","GADMTools","ISOcodes","grDevices","gridExtra"), require, character.only=T)

# laod Irish COVID-19 network
load("U:/sandbox/spatialsocialmedia/data/covid.ie.g.rda")

# inspect graph
igraph::list.vertex.attributes(covid.ie.g)
igraph::list.edge.attributes(covid.ie.g)

# plot with igraph
plot.spatial.network(geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), package = "igraph", edge.width.igraph = 0.1)

# plot with igraph and GADM
plot.spatial.network(geo.graph = covid.ie.g, database = "gadm", gadm.level = 1, area=c("Ireland", "United Kingdom:Northern Ireland"), package = "igraph", edge.width.igraph = 0.1)

# plot with ggplot
plot.spatial.network(geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), package = "ggplot", database = "world", edge.width.ggplot = 10)

# plot with ggplot and GADM
plot.spatial.network(geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), package = "ggplot", database = "gadm", gadm.level = 1, edge.width.ggplot = 8)

# plot with ggplot2
plot.spatial.network(geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), package = "ggplot2", database = "world", edge.width.ggplot = 100)

# plot with ggplot2 and GADM
plot.spatial.network(geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), package = "ggplot2", database = "gadm", gadm.level = 1, edge.width.ggplot = 100)

# plot with ggraph
plot.spatial.network(geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), package = "ggraph", database = "world")

# plot with ggraph and GADM
plot.spatial.network(geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), package = "ggraph", database = "gadm", gadm.level = 1)

# montly messages break down as following
substr(as.Date(as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")), 1, 7)

# sample network
subgraph.month <- list()
subgraph.month[[1]] <- which(as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")<="2020-03-31")
subgraph.month[[2]] <- which(as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")>"2020-03-31" & as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")<="2020-04-30")
subgraph.month[[3]] <- which(as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")>"2020-04-30" & as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")<="2020-05-31")
subgraph.month[[4]] <- which(as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")>"2020-05-31" & as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")<="2020-06-30")
subgraph.month[[5]] <- which(as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")>"2020-06-30" & as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")<="2020-07-31")
subgraph.month[[6]] <- which(as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")>"2020-07-31" & as.POSIXct(E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01")<="2020-08-31")
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
