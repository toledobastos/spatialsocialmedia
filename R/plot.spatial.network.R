#' The function plot.spatial.network() allows one to plot social network data over geographic grids using R base map library
#' and the GADM database, a free spatial database of the location of the world’s administrative areas or boundaries.
#' Network plots are generated using the capabilities of igraph, network, ggplot, ggplot2 (curved edges), or ggraph.
#' The function accepts the following optional arguments and parameters:
#'
#' @param geo.graph: network object
#' @param area: Geographic area to load base map. Defaults to UK
#' @param database: Database to load base map ("world" or "gadm"). Defaults to "world"
#' @param package: Preferred plotting package. Defaults to igraph
#' @param latitude: Latitude of nodes. Defaults to place_lat
#' @param longitude: Longitude of nodes. Defaults to place_lon
#' @param line.col: Color of line in the network plot. Defaults to black
#' @param nodeLabelnum: Node label size. Defaults to 5
#' @param vertex.size.igraph: Node size (igraph only). Defaults to a function of degree
#' @param edge.width.igraph: Width of edge (igraph only). Defaults to "auto"
#' @param edge.arrow.size.igraph: Arrow size (igraph only). Default is a function of density
#' @param edge.color.igraph: Edge color (igraph only). Defaults to edge attribute "color"
#' @param vertex.label.cex.igraph: Node label size (igraph only). Defaults to 0.4
#' @param vertex.label.color.igraph: Node label color (igraph only). Defaults to black
#' @param vertex.cex.network: Node size (network only). Defaults to a function of degree
#' @param vertex.col.network: Node color (network only). Defaults to orange
#' @param vertex.border.network: Node outline color (network only). Defaults to black
#' @param usearrows.network: (Logical) Use arrows in the plot. Defaults to TRUE
#' @param arrowhead.cex.network: Edge arrow size (network only). Defaults to 0.2
#' @param label.cex.network: Node label size (network only). Defaults to 0.5
#' @param edge.curve.network: Width of curved edge (network only). Defaults to 0.5
#' @param edge.width.ggplot: Width of edge (ggplot only). Defaults to 10 (less is thicker)
#' @param edge.width.ggraph: Width of edge (ggraph only). Defaults to node weight
#' @param avoid.overlap.ggraph: Avoid node label overlap (ggraph only). Defaults to TRUE
#' @param vertex.label.cex.ggraph: Node label size (ggraph only). Defaults to 3
#' @param gadm.level: Level of GADM base map to be loaded. Defaults to 2
#' @return network plot over a geographic grid
#' @export
plot.spatial.network <- function(geo.graph,
                                 area = "UK",
                                 database = "world",
                                 package = "igraph",
                                 latitude = igraph::V(geo.graph)$place_lat,
                                 longitude = igraph::V(geo.graph)$place_lon,
                                 line.col = "black",
                                 nodeLabelnum = 5,
                                 vertex.size.igraph = as.numeric(log(igraph::degree(geo.graph)+1)*2),
                                 edge.width.igraph = "auto",
                                 edge.arrow.size.igraph = (50/igraph::ecount(geo.graph))+0.02,
                                 edge.color.igraph = igraph::E(geo.graph)$color,
                                 vertex.label.cex.igraph = 0.4,
                                 vertex.label.color.igraph = "black",
                                 vertex.cex.network = as.numeric(log(sna::degree(geo.n)+1)/2),
                                 vertex.col.network = "orange",
                                 vertex.border.network = "black",
                                 usearrows.network = TRUE,
                                 arrowhead.cex.network = 0.2,
                                 label.cex.network = 0.5,
                                 edge.curve.network = 0.5,
                                 edge.width.ggplot = 1,
                                 edge.width.ggraph = "weight",
                                 avoid.overlap.ggraph = TRUE,
                                 vertex.label.cex.ggraph = 3,
                                 gadm.level = 2,
                                 ...)
{

  # create colors
  col.1 <- grDevices::adjustcolor("orange red", alpha=0.4)
  col.2 <- grDevices::adjustcolor("orange", alpha=0.4)

  # set nodeLabels & lat-lon
  if(class(geo.graph)=="igraph") {
    nodeLabels <- igraph::V(geo.graph)$name
    nodeLabels[!nodeLabels %in% names(sort(igraph::degree(geo.graph),decreasing=T)[1:nodeLabelnum])] <- ""
    nodeLabels <- iconv(nodeLabels, "UTF8")
    # consolidate geographic information
    if(is.null(igraph::V(geo.graph)$latitude)) { igraph::V(geo.graph)$latitude <- latitude}
    if(is.null(igraph::V(geo.graph)$longitude)) { igraph::V(geo.graph)$longitude <- longitude}
  } else {
    geo.graph.1 <- intergraph::asIgraph(geo.graph)
    nodeLabels <- igraph::V(geo.graph.1)$vertex.names
    nodeLabels[!nodeLabels %in% names(sort(igraph::degree(geo.graph.1),decreasing=T)[1:nodeLabelnum])] <- ""
    nodeLabels <- iconv(nodeLabels, "UTF8")
  }

  if(package=="igraph") {
    ### PLOT METHOD 1: igraph
    # calculate edge width
    if(is.character(edge.width.igraph)) {
      if(edge.width.igraph=="auto" & igraph::ecount(geo.graph)>10000) { edge.width.igraph <- 0.02 }
      if(edge.width.igraph=="auto" & igraph::ecount(geo.graph)>=3000 & igraph::ecount(geo.graph)<=10000) { edge.width.igraph <- "normalized" }
      if(edge.width.igraph=="normalized") {
        edge.weights <- igraph::count_multiple(geo.graph, eids = igraph::E(geo.graph))
        edge.weights[is.na(edge.weights)] <- 0
        linMap <- function(x, from, to) { (x - min(x)) / max(x - min(x)) * (to - from) + from }
        edge.width.igraph <- linMap(edge.weights, 0.01, 0.05) } }
    if(is.character(edge.width.igraph)) {
      if(edge.width.igraph=="auto" & igraph::ecount(geo.graph)<3000) { edge.width.igraph <- "baseline" }
      if(edge.width.igraph=="baseline") {
        edge.width.igraph <- (50/igraph::ecount(geo.graph))+0.02 }
    }
    # drawn base plot
    if(database=="gadm") {
      par(xpd=TRUE)
      plot.add <- F
      iso.db <- ISOcodes::ISO_3166_1
      print(paste0("Attempting to load GADM map at level ", gadm.level, ". Try lower values if base map is unavailable at this level"))
      iso.code <- iso.db$Alpha_3[grep(area[1], iso.db$Name, ignore.case = T)]
      try(map.new <- maps::SpatialPolygons2map(as(GADMTools::map_data(iso.code, level=gadm.level, basefile="./")$sf, "Spatial")), silent=T)
      if(exists("map.new")) {
        maps::map(map.new, col=line.col, fill=F, bg="white")
        plot.add <- T }
      if(length(area)>1) {
        for(i in 1:length(area)) {
          # clean matches to iso db
          if(length(iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)])>0) {
            iso.code <- iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)]
            map.new <- maps::SpatialPolygons2map(as(GADMTools::gadm_sf_loadCountries(iso.code, level=gadm.level, basefile="./")$sf, "Spatial"))
            if(exists("map.new")) {
              if(plot.add==F) {
                maps::map(map.new, col=line.col, fill=F, bg="white")
                plot.add <- T } }
            if(exists("map.new")) {
              if(plot.add==T) {
                maps::map(map.new, col=line.col, fill=F, bg="white", add=T)
                plot.add <- T } }
            }
          # requires subdivision
          if(grepl("\\:", area[i])) {
            sub.region <- strsplit(area[i], "\\:")[[1]]
            try(map.new.1 <- maps::SpatialPolygons2map(as(GADMTools::gadm_subset(GADMTools::gadm_sf_loadCountries(iso.db$Alpha_3[grep(sub.region[1], iso.db$Name, ignore.case = T)], level=gadm.level, basefile="./"), gadm.level, sub.region[2])$sf, "Spatial")), silent=T)
            if(exists("map.new.1")) {
              if(plot.add==F) {
                maps::map(map.new.1, col=line.col, fill=F, bg="white")
                plot.add <- T } }
            if(exists("map.new.1")) {
              if(plot.add==T) {
                maps::map(map.new.1, col=line.col, fill=F, bg="white", add=T)
                plot.add <- T } }
            }
          }
    } } else { maps::map(regions = area, database=database, boundary=T, col=line.col) }
    igraph::plot.igraph(geo.graph,
                        rescale = F,
                        layout= as.matrix(data.frame(lon=as.numeric(igraph::V(geo.graph)$longitude),lat=as.numeric(igraph::V(geo.graph)$latitude))),
                        xlim = c(min(as.numeric(igraph::V(geo.graph)$longitude)), max(as.numeric(igraph::V(geo.graph)$longitude))),
                        ylim = c(min(as.numeric(igraph::V(geo.graph)$latitude)), max(as.numeric(igraph::V(geo.graph)$latitude))),
                        edge.curved = TRUE,
                        vertex.label = nodeLabels,
                        edge.width = edge.width.igraph,
                        edge.color = edge.color.igraph,
                        edge.arrow.size = edge.arrow.size.igraph,
                        vertex.size = vertex.size.igraph,
                        vertex.label.color = vertex.label.color.igraph,
                        add = T)
    # legend(x="top", tolower(topic.description$hashtag), ncol=ncol.plot, title="hashtags", lwd=20, col=topic.description$col.spectrum, cex=2, bg="transparent")
  }
  if(package=="network") {
    ### PLOT METHOD 2: network
    # convert graph
    geo.n <- intergraph::asNetwork(geo.graph)
    # consolidate geographic information
    if(is.null(igraph::V(geo.graph)$latitude)) { igraph::V(geo.n)$latitude <- lat}
    if(is.null(igraph::V(geo.graph)$longitude)) { igraph::V(geo.n)$longitude <- lon}

    if(is.null(network::get.vertex.attribute(geo.n, "latitude")) | is.na(unique(network::get.vertex.attribute(geo.n, "latitude")))) {
      network::set.vertex.attribute(geo.n, "latitude", latitude)
      network::set.vertex.attribute(geo.n, "longitude", longitude)
    }
    # png(file="plot_network.png", type='cairo', width=30,height=20, units='in', res=300)
    if(database=="gadm") {
      par(xpd=TRUE)
      plot.add <- F
      iso.db <- ISOcodes::ISO_3166_1
      print(paste0("Attempting to load GADM map at level ", gadm.level, ". Try lower values if base map is unavailable at this level"))
      iso.code <- iso.db$Alpha_3[grep(area[1], iso.db$Name, ignore.case = T)]
      try(map.new <- maps::SpatialPolygons2map(as(GADMTools::map_data(iso.code, level=gadm.level, basefile="./")$sf, "Spatial")), silent=T)
      # if(!exists("map.new")) { try(map.new <- maps::SpatialPolygons2map(as(GADMTools::map_data(iso.code, level=1, basefile="./")$sf, "Spatial")), silent=T) }
      # if(!exists("map.new")) { try(map.new <- maps::SpatialPolygons2map(as(GADMTools::map_data(iso.code, level=0, basefile="./")$sf, "Spatial")), silent=T) }
      if(exists("map.new")) {
        maps::map(map.new, col=line.col, fill=F, bg="white")
        plot.add <- T }
      if(length(area)>1) {
        for(i in 1:length(area)) {
          # clean matches to iso db
          if(length(iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)])>0) {
            iso.code <- iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)]
            map.new <- maps::SpatialPolygons2map(as(GADMTools::gadm_sf_loadCountries(iso.code, level=gadm.level, basefile="./")$sf, "Spatial"))
            if(exists("map.new")) {
              if(plot.add==F) {
                maps::map(map.new, col=line.col, fill=F, bg="white")
                plot.add <- T } }
            if(exists("map.new")) {
              if(plot.add==T) {
                maps::map(map.new, col=line.col, fill=F, bg="white", add=T)
                plot.add <- T } }
          }
          # requires subdivision
          if(grepl("\\:", area[i])) {
            sub.region <- strsplit(area[i], "\\:")[[1]]
            try(map.new.1 <- maps::SpatialPolygons2map(as(GADMTools::gadm_subset(GADMTools::gadm_sf_loadCountries(iso.db$Alpha_3[grep(sub.region[1], iso.db$Name, ignore.case = T)], level=gadm.level, basefile="./"), gadm.level, sub.region[2])$sf, "Spatial")), silent=T)
            if(exists("map.new.1")) {
              if(plot.add==F) {
                maps::map(map.new.1, col=line.col, fill=F, bg="white")
                plot.add <- T } }
            if(exists("map.new.1")) {
              if(plot.add==T) {
                maps::map(map.new.1, col=line.col, fill=F, bg="white", add=T)
                plot.add <- T } }
          }
        }
      } } else { maps::map(regions = area, database=database, boundary=T, col=line.col) }
    network::plot.network(geo.n,
                          new = FALSE,
                          label = nodeLabels,
                          coord = cbind(as.numeric(network::get.vertex.attribute(geo.n, "longitude", unlist=TRUE)),
                                        as.numeric(network::get.vertex.attribute(geo.n, "latitude", unlist=TRUE))),
                          edge.col = network::get.edge.attribute(geo.n, "color"),
                          vertex.cex = vertex.cex.network,
                          vertex.col = vertex.col.network,
                          vertex.border= vertex.border.network,
                          usearrows = usearrows.network,
                          arrowhead.cex = arrowhead.cex.network,
                          label.cex = label.cex.network,
                          edge.curve = edge.curve.network,
                          usecurve = FALSE,
                          jitter = FALSE)
    # dev.off()
  }
  if(package=="ggplot") {
    ### PLOT METHOD 3: ggplot2
    geo.edges <- as.data.frame(igraph::get.edgelist(geo.graph), stringsAsFactors=F)
    geo.nodes <- data.frame(vertex=as.character(igraph::V(geo.graph)$name), latitude=as.numeric(igraph::V(geo.graph)$latitude), longitude=as.numeric(igraph::V(geo.graph)$longitude), stringsAsFactors=F)
    # plot black
    if(database=="gadm") {
      par(xpd=TRUE)
      plot.add <- F
      iso.db <- ISOcodes::ISO_3166_1
      print(paste0("Attempting to load GADM map at level ", gadm.level, ". Try lower values if base map is unavailable at this level"))
      iso.code <- iso.db$Alpha_3[grep(area[1], iso.db$Name, ignore.case = T)]
      try(map.new <- maps::SpatialPolygons2map(as(GADMTools::gadm_sf_loadCountries(iso.code, level=gadm.level, basefile="./")$sf, "Spatial")), silent=T)
      # if(!exists("map.new")) { try(map.new <- maps::SpatialPolygons2map(as(GADMTools::map_data(iso.code, level=1, basefile="./")$sf, "Spatial")), silent=T) }
      # if(!exists("map.new")) { try(map.new <- maps::SpatialPolygons2map(as(GADMTools::map_data(iso.code, level=0, basefile="./")$sf, "Spatial")), silent=T) }
      if(exists("map.new")) {
        maps::map(map.new, col=line.col, fill=F, bg="white")
        plot.add <- T }
      if(length(area)>1) {
        for(i in 1:length(area)) {
          # clean matches to iso db
          if(length(iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)])>0) {
            iso.code <- iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)]
            map.new <- maps::SpatialPolygons2map(as(GADMTools::gadm_sf_loadCountries(iso.code, level=gadm.level, basefile="./")$sf, "Spatial"))
            if(exists("map.new")) {
              if(plot.add==F) {
                maps::map(map.new, col=line.col, fill=F, bg="white")
                plot.add <- T } }
            if(exists("map.new")) {
              if(plot.add==T) {
                maps::map(map.new, col=line.col, fill=F, bg="white", add=T)
                plot.add <- T } }
          }
          # requires subdivision
          if(grepl("\\:", area[i])) {
            sub.region <- strsplit(area[i], "\\:")[[1]]
            try(map.new.1 <- maps::SpatialPolygons2map(as(GADMTools::gadm_subset(GADMTools::gadm_sf_loadCountries(iso.db$Alpha_3[grep(sub.region[1], iso.db$Name, ignore.case = T)], level=gadm.level, basefile="./"), gadm.level, sub.region[2])$sf, "Spatial")), silent=T)
            if(exists("map.new.1")) {
              if(plot.add==F) {
                maps::map(map.new.1, col=line.col, fill=F, bg="white")
                plot.add <- T } }
            if(exists("map.new.1")) {
              if(plot.add==T) {
                maps::map(map.new.1, col=line.col, fill=F, bg="white", add=T)
                plot.add <- T } }
          }
        }
      } } else { maps::map(regions = area, database=database, boundary=T, col=line.col) }
    # map("county", boundary=T, col=line.col)
    # locate nodes
    points(x=as.numeric(igraph::V(geo.graph)$longitude), y=as.numeric(igraph::V(geo.graph)$latitude), pch=19, cex=as.numeric(igraph::degree(geo.graph))/30, col="orange")
    edge.pal <- grDevices::colorRampPalette(c(col.1, col.2), alpha = TRUE)
    edge.col <- edge.pal(100)
    # prepare data
    geo.edges <- as.data.frame(igraph::get.edgelist(geo.graph), stringsAsFactors=F)
    geo.nodes <- data.frame(vertex=as.character(igraph::V(geo.graph)$name), latitude=as.numeric(igraph::V(geo.graph)$latitude), longitude=as.numeric(igraph::V(geo.graph)$longitude), stringsAsFactors=F)
    # plot edges
    for(i in 1:nrow(geo.edges)) {
      node1.lat <- geo.nodes$latitude[geo.nodes$vertex==geo.edges$V1[i]]
      node1.lon <- geo.nodes$longitude[geo.nodes$vertex==geo.edges$V1[i]]
      node2.lat <- geo.nodes$latitude[geo.nodes$vertex==geo.edges$V2[i]]
      node2.lon <- geo.nodes$longitude[geo.nodes$vertex==geo.edges$V2[i]]
      arc <- geosphere::gcIntermediate( c(node1.lon, node1.lat), c(node2.lon, node2.lat), n=1000, addStartEnd=TRUE )
      edge.ind <- round(100*as.numeric(igraph::degree(geo.graph)) / max(as.numeric(igraph::degree(geo.graph))))+edge.width.ggplot
      lines(arc, col=edge.col[edge.ind], lwd=edge.ind/edge.width.ggplot)
    }
  }
  if(package=="ggplot2") {
    # prepare data
    geo.edges <- as.data.frame(igraph::get.edgelist(geo.graph), stringsAsFactors=F)
    geo.nodes <- data.frame(name=as.character(igraph::V(geo.graph)$name), latitude=as.numeric(igraph::V(geo.graph)$latitude), longitude=as.numeric(igraph::V(geo.graph)$longitude), stringsAsFactors=F)
    edges_for_plot <- merge(merge(geo.edges, geo.nodes, by.x="V1", by.y="name"), geo.nodes, by.x="V2", by.y="name")
    edges_for_plot <- edges_for_plot[,c(2,1,3:6)]
    names(edges_for_plot) <- c("from", "to", "y", "x", "yend", "xend")
    geo.nodes$x <- geo.nodes$latitude
    geo.nodes$y <- geo.nodes$longitude
    geo.nodes$weight <- log(as.numeric(igraph::degree(geo.graph)))+1
    edges_for_plot <- merge(edges_for_plot, geo.nodes[,c("name", "weight")], by.x="from", by.y="name")
    edges_for_plot$category <- rep(1:4, nrow(edges_for_plot)+1)[1:nrow(edges_for_plot)]
    geo.nodes$id <- rownames(geo.nodes)
    geo.nodes <- geo.nodes[,c(7,2,3,1,6)]
    colnames(geo.nodes) <- c("id", "lon", "lat", "name", "weight")
    # jitter receiver location
    edges_for_plot$xend <- edges_for_plot$xend+0.001
    edges_for_plot$yend <- edges_for_plot$yend+0.001
    # create color scheme
    edge.pal <- grDevices::colorRampPalette(c(col.1, col.2), alpha = TRUE)
    edge.col <- edge.pal(nrow(edges_for_plot))
    geo.nodes$labels <- nodeLabels
    # plot black
    if(database=="gadm") {
      par(xpd=TRUE)
      plot.add <- F
      iso.db <- ISOcodes::ISO_3166_1
      print(paste0("Attempting to load GADM map at level ", gadm.level, ". Try lower values if base map is unavailable at this level"))
      iso.code <- iso.db$Alpha_3[grep(area[1], iso.db$Name, ignore.case = T)]
      try(map.new <- ggplot2::map_data(maps::SpatialPolygons2map(as(GADMTools::gadm_sf_loadCountries(iso.code, level=gadm.level, basefile="./")$sf, "Spatial"))), silent=T)
      if(length(area)>1) {
        for(i in 1:length(area)) {
          # clean matches to iso db
          if(length(iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)])>0) {
            iso.code <- iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)]
            map.new <- ggplot2::map_data(maps::SpatialPolygons2map(as(GADMTools::gadm_sf_loadCountries(iso.code, level=gadm.level, basefile="./")$sf, "Spatial")))
          }
          # requires subdivision
          if(grepl("\\:", area[i])) {
            sub.region <- strsplit(area[i], "\\:")[[1]]
            try(map.new.1 <- ggplot2::map_data(maps::SpatialPolygons2map(as(GADMTools::gadm_subset(GADMTools::gadm_sf_loadCountries(iso.db$Alpha_3[grep(sub.region[1], iso.db$Name, ignore.case = T)], level=gadm.level, basefile="./"), gadm.level, sub.region[2])$sf, "Spatial"))), silent=T)
        }
        } }
      if(exists("map.new.1")) { map.new <- rbind(map.new, map.new.1) } else { map.new <- ggplot2::map_data(map = database, region = area, boundary=T) }
    } else { map.new <- ggplot2::map_data(map = database, region = area, boundary=T) }
    # create country_shapes
    country_shapes <- ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group),
                                            data = map.new,
                                            fill = "white", color = "black",
                                            size = 0.15)
    mapcoords <- ggplot2::coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
    # plot with ggplot2
    ggplot2::ggplot(geo.nodes) + country_shapes + ggplot2::theme_void() + ggplot2::theme(legend.position = "none") +
      ggplot2::geom_curve(ggplot2::aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                              color = edge.col, size = weight/edge.width.ggplot),
                          data = edges_for_plot, curvature = 0.33, alpha = 0.5) +
      ggplot2::scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
      ggplot2::geom_point(ggplot2::aes(x = lat, y = lon, size = weight),           # draw nodes
                          shape = 21, fill = 'black',
                          color = 'white', stroke = 0.5) +
      # ggplot2::scale_size_continuous(guide = FALSE, range = c(1, 6)) +    # scale for node size
      ggplot2::geom_text(ggplot2::aes(x = lat, y = lon, label = labels))
  }
  if(package=="ggraph") {
    # load theme
    maptheme <- ggplot2::theme(panel.grid = element_blank()) +
      ggplot2::theme(axis.text = element_blank()) +
      ggplot2::theme(axis.ticks = element_blank()) +
      ggplot2::theme(axis.title = element_blank()) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(panel.grid = element_blank()) +
      ggplot2::theme(panel.background = element_rect(fill = "#596673")) +
      ggplot2::theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
    # prepare data
    geo.edges <- as.data.frame(igraph::get.edgelist(geo.graph), stringsAsFactors=F)
    geo.nodes <- data.frame(name=as.character(igraph::V(geo.graph)$name), latitude=as.numeric(igraph::V(geo.graph)$latitude), longitude=as.numeric(igraph::V(geo.graph)$longitude), stringsAsFactors=F)
    edges_for_plot <- merge(merge(geo.edges, geo.nodes, by.x="V1", by.y="name"), geo.nodes, by.x="V2", by.y="name")
    edges_for_plot <- edges_for_plot[,c(2,1,3:6)]
    names(edges_for_plot) <- c("from", "to", "y", "x", "yend", "xend")
    geo.nodes$x <- geo.nodes$latitude
    geo.nodes$y <- geo.nodes$longitude
    geo.nodes$weight <- log(as.numeric(igraph::degree(geo.graph)))+1
    edges_for_plot <- merge(edges_for_plot, geo.nodes[,c("name", "weight")], by.x="from", by.y="name")
    edges_for_plot$category <- rep(1:4, nrow(edges_for_plot)+1)[1:nrow(edges_for_plot)]
    geo.nodes$id <- rownames(geo.nodes)
    geo.nodes <- geo.nodes[,c(7,2,3,1,6)]
    colnames(geo.nodes) <- c("id", "lon", "lat", "name", "weight")
    # jitter receiver location
    edges_for_plot$xend <- edges_for_plot$xend+0.001
    edges_for_plot$yend <- edges_for_plot$yend+0.001
    edges_for_plot$edge.id <- rownames(edges_for_plot)
    # create color scheme
    edge.pal <- grDevices::colorRampPalette(c(col.1, col.2), alpha = TRUE)
    edge.col <- edge.pal(nrow(edges_for_plot))
    geo.nodes$labels <- nodeLabels
    # plot black
    if(database=="gadm") {
      par(xpd=TRUE)
      plot.add <- F
      iso.db <- ISOcodes::ISO_3166_1
      print(paste0("Attempting to load GADM map at level ", gadm.level, ". Try lower values if base map is unavailable at this level"))
      iso.code <- iso.db$Alpha_3[grep(area[1], iso.db$Name, ignore.case = T)]
      try(map.new <- ggplot2::map_data(maps::SpatialPolygons2map(as(GADMTools::gadm_sf_loadCountries(iso.code, level=gadm.level, basefile="./")$sf, "Spatial"))), silent=T)
      if(length(area)>1) {
        for(i in 1:length(area)) {
          # clean matches to iso db
          if(length(iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)])>0) {
            iso.code <- iso.db$Alpha_3[grep(area[i], iso.db$Name, ignore.case = T)]
            map.new <- ggplot2::map_data(maps::SpatialPolygons2map(as(GADMTools::gadm_sf_loadCountries(iso.code, level=gadm.level, basefile="./")$sf, "Spatial")))
          }
          # requires subdivision
          if(grepl("\\:", area[i])) {
            sub.region <- strsplit(area[i], "\\:")[[1]]
            try(map.new.1 <- ggplot2::map_data(maps::SpatialPolygons2map(as(GADMTools::gadm_subset(GADMTools::gadm_sf_loadCountries(iso.db$Alpha_3[grep(sub.region[1], iso.db$Name, ignore.case = T)], level=gadm.level, basefile="./"), gadm.level, sub.region[2])$sf, "Spatial"))), silent=T)
          }
        } }
      if(exists("map.new.1")) { map.new <- rbind(map.new, map.new.1) } else { map.new <- ggplot2::map_data(map = database, region = area, boundary=T) }
    } else { map.new <- ggplot2::map_data(map = database, region = area, boundary=T) }
    # create country_shapes
    country_shapes <- ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group),
                                            data = map.new,
                                            fill = "white", color = "black",
                                            size = 0.15)
    mapcoords <- ggplot2::coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
    # create node position table
    node_pos <- data.frame(x=geo.nodes$lon, y=geo.nodes$lat, stringsAsFactors=F)
    lay <- ggraph::create_layout(graph = geo.graph, layout = as.matrix(data.frame(lon=as.numeric(igraph::V(geo.graph)$longitude),lat=as.numeric(igraph::V(geo.graph)$latitude))))
    stopifnot(nrow(lay) == nrow(geo.nodes))
    # add node degree for scaling the node sizes
    lay$weight <- as.integer(igraph::degree(geo.graph))
    # if(edge.width.ggraph=="weight") { edge.width.ggraph <- lay$weight }
    # plot with ggraph
    ggraph::ggraph(lay) + country_shapes + ggplot2::theme_void() + ggplot2::theme(legend.position = "none") +
      ggraph::geom_edge_arc(ggplot2::aes(color = igraph::E(geo.graph)$color, edge_width = weight, circular = FALSE),
                            data = edges_for_plot, strength = 0.33, alpha = 0.5) +                    # draw edges as arcs
      ggraph::scale_edge_width_continuous(range = c(0.0001, 1), guide = FALSE) +                         # scale for edge widths
      ggraph::geom_node_point(ggplot2::aes(size=weight), shape=21, fill="white", color=igraph::V(geo.graph)$color, stroke=0.5) +  # draw nodes
      ggplot2::scale_size_continuous(range = c(1, 6), guide = FALSE) +                                 # scale for node sizes
      ggraph::geom_node_text(ggplot2::aes(label = nodeLabels), repel = avoid.overlap.ggraph, size = vertex.label.cex.ggraph, color = "black", fontface = "bold")
    # + maptheme
  }
}
