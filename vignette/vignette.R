###
### Spatial Social Media: Geographic Visualization of Social Media Data
### Vignette available at https://github.com/toledobastos/spatialsocialmedia
###

# install package with devtools::install_github("toledobastos/spatialsocialmedia")
# alternatively, install package from the source tarball with install.packages(repos=NULL, type="source", "SpatialSocialMedia_0.0.1.tar.gz")

# load package
options(warn=-1)
library(SpatialSocialMedia)

# load COVID-19 Ireland test data
data("covid.ie.g")

# inspect covid.ie.g graph
igraph::list.vertex.attributes(covid.ie.g)

igraph::list.edge.attributes(covid.ie.g)

table(igraph::E(covid.ie.g)$type)

table(igraph::V(covid.ie.g)$country_code)

sort(table(igraph::V(covid.ie.g)$full_name), decreasing = T)[1:10]

hist(as.POSIXct(igraph::E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01"), 80, main="")

hist(as.POSIXct(igraph::V(covid.ie.g)$time_user_created_num, tz = "GMT", origin="1970-01-01"), 80, main="")

summary(as.POSIXct(igraph::E(covid.ie.g)$time_num, tz = "GMT", origin="1970-01-01"))

# plot with igraph, network, and geosphere [R maps & GADM]
par(mfrow=c(2,3), mai = c(1, 0.1, 0.1, 0.1))
plot.spatial.network(package = "igraph", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), edge.width.igraph = 0.1)
plot.spatial.network(package = "network", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"))
plot.spatial.network(package = "geosphere", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), database = "world", edge.width.ggplot = 10)
plot.spatial.network(package = "igraph", geo.graph = covid.ie.g, database = "gadm", gadm.level = 1, area=c("Ireland", "United Kingdom:Northern Ireland"), edge.width.igraph = 0.1)
plot.spatial.network(package = "network", geo.graph = covid.ie.g, database = "gadm", gadm.level = 1, area=c("Ireland", "United Kingdom:Northern Ireland"))
plot.spatial.network(package = "geosphere", geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), database = "gadm", gadm.level = 1, edge.width.ggplot = 10)

# plot with ggraph [R maps & GADM]
ggraph.plots <- list()
ggraph.plots [[1]] <- plot.spatial.network(package = "ggraph", geo.graph = covid.ie.g, area=c("UK:Northern Ireland", "Ireland"), database = "world")
ggraph.plots [[2]] <- plot.spatial.network(package = "ggraph", geo.graph = covid.ie.g, area=c("Ireland", "United Kingdom:Northern Ireland"), database="gadm", gadm.level=1)
par(mfrow=c(1,2), mai = c(1, 0.1, 0.1, 0.1))
gridExtra::grid.arrange(ggraph.plots[[1]], ggraph.plots[[2]], ncol=2, nrow=1)

# monthly tweets break down as following
summary(substr(as.Date(as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")), 1, 7))

# sample network
subgraph.month <- list()
subgraph.month[[1]] <- which(as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-03-31")
subgraph.month[[2]] <- which(as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-03-31" & as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-04-30")
subgraph.month[[3]] <- which(as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-04-30" & as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-05-31")
subgraph.month[[4]] <- which(as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-05-31" & as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-06-30")
subgraph.month[[5]] <- which(as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-06-30" & as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-07-31")
subgraph.month[[6]] <- which(as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")>"2020-07-31" & as.POSIXct(igraph::E(covid.ie.g)$time, tz = "GMT", origin="1970-01-01")<="2020-08-31")
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
  title(paste0("COVID-19 ", names(subgraph.month)[i]), cex.main=1.5, col.main="black")
}

# plot montly snapshots with ggraph
subgraph.month.g <- list()
for(i in 1:length(subgraph.month)) {
  G <- igraph::subgraph.edges(graph=covid.ie.g, eids=subgraph.month[[i]], delete.vertices=TRUE)
  # plot the graph in a new window
  subgraph.month.g[[i]] <- plot.spatial.network(geo.graph = G, database = "gadm", gadm.level = 1, nodeLabelnum = 15, area=c("Ireland", "United Kingdom:Northern Ireland"), package = "ggraph", vertex.label.cex.ggraph = 1)
}
par(mfrow=c(2,3), mai = c(1, 0.1, 0.1, 0.1))
gridExtra::grid.arrange(subgraph.month.g[[1]], subgraph.month.g[[2]], subgraph.month.g[[3]], subgraph.month.g[[4]], subgraph.month.g[[5]], subgraph.month.g[[6]], ncol=3, nrow =2)

# fin
options(warn=0)
