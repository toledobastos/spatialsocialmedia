#' COVID19 related tweets in NI and ROI from January to June 2020
#'
#' Data collected from Twitter Public Streaming API using geo-fencing (aka bounding box) around
#' Ireland and subsequently filtering relevant messages using the following regular expression:
#' "COVID|corona(-| )?(virus|epide|apocalypse|outbreak|pandem)|19nCoV|wuhan"
#'
#' @docType data
#'
#' @usage data(covid.ie.g)
#'
#' @format An object of class \code{"igraph"}
#'
#' @keywords datasets
#'
#' @references Bastos, M.T. (2020) Spatializing Social Media: Social Network Analysis Online and Offline
#'
#' @source \href{https://github.com/toledobastos/spatialsocialmedia}
#'
#' @examples
#' data(covid.ie.g)
#' igraph::list.vertex.attributes(covid.ie.g)
#' igraph::list.edge.attributes(covid.ie.g)
"covid.ie.g"
