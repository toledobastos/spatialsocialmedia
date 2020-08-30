#' Create network from social media data. Code is optimized for Twitter data but can be used with other data sources.
#' This function Transforms a tweet dataset into a network via the function get.interactions(). The function requires
#' the object tweets (character vector of tweets) and the object screenName (character vector containing user names),
#' along with a range of optional arguments to generate node (vertex) and edge (links) information, chief of which are
#' timestamps and geographic information for nodes such as latitude and longitude coordinates. Along with the required
#' arguments, the function accepts the following optional arguments and parameters:
#'
#' @param tweets: Character vector with tweets
#' @param screenName: Character vector with usernames
#' @param node.attributes.df: Data frame with node attributes. Defaults to NULL
#' @param edge.attributes.df: Data frame with edge attributes. Defaults to NULL
#' @param ascii: Whether text encoding is to be converted to ASCII. Defaults to TRUE
#' @param return.network: Network of "retweets," "mentions," or "all.interactions" (default)
#' @param return.graph: Return igraph object if TRUE (default) or data frame if FALSE
#' @param directed: Whether network is directed. Defaults to TRUE
#' @return igraph object or edge list (data frame)
#' @export
get.interactions <- function(tweets, 
                             screenName, 
                             node.attributes.df=NULL, 
                             edge.attributes.df=NULL, 
                             ascii=TRUE, 
                             return.network="all.interactions", 
                             return.graph=TRUE, 
                             directed=TRUE, 
                             ...) {
  
  # process text
  if(ascii==T) { tweets <- iconv(tweets, to="ASCII", sub="") }
  tweets <- gsub(" via @", " rt @", tolower(as.character(tweets)))
  
  if(!return.network=="mentions") {
    # get retweets
    rt.receiver <- as.character(screenName[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T)])
    rt.sender <- gsub("rt @", "", stringr::str_extract(tweets, "rt @[a-zA-Z0-9_]+"))
    rt.sender <- rt.sender[!is.na(rt.sender)]
    rt.sender[rt.sender==""] <- "<NA>"
    rt.receiver[rt.receiver==""] <- "<NA>"
    
    # create rt edge list
    if(!is.null(edge.attributes.df)) {
      if(!is.data.frame(edge.attributes.df)) { stop("edge.attributes.df needs to be a data frame") }
      edge.variable <- edge.attributes.df[grep("rt @[a-zA-Z0-9_]+", tweets, perl=T), ]
      rts.df <- data.frame(sender=rt.sender, receiver=rt.receiver, type="retweet", stringsAsFactors=FALSE) 
      rts.df <- cbind(rts.df, edge.variable) 
    }
    if(is.null(edge.attributes.df)) { rts.df <- data.frame(sender=rt.sender, receiver=rt.receiver, type="retweet", stringsAsFactors=FALSE) }
    rts.df <- rts.df[!is.na(rts.df$sender),]
    rts.df <- rts.df[!rts.df$sender==rts.df$receiver,]
    rownames(rts.df) <- NULL 
  }
  
  if(!return.network=="retweets") {
    # remove retweets from text corpora
    tweets <- gsub("rt @[a-z0-9_]{1,15}", "", tweets, perl=T)
    
    # get @-mentions edgelist
    mention.count.0 <- stringr::str_count(tweets, "@[a-zA-Z0-9_]+")
    mentioned <- stringr::str_extract_all(tweets, "@[a-zA-Z0-9_]+")
    at.receiver <- gsub("@", "", unlist(mentioned))
    at.sender <- rep(screenName, mention.count.0)
    
    # create edge list
    if(!is.null(edge.attributes.df)) {
      if(!is.data.frame(edge.attributes.df)) { stop("edge.attributes.df needs to be a data frame") }
      edge.variable <- as.data.frame(lapply(edge.attributes.df, rep, mention.count.0))
      ats.df <- data.frame(sender=at.sender, receiver=at.receiver, type="mention", stringsAsFactors=FALSE) 
      ats.df <- cbind(ats.df, edge.variable)
    }
    if(is.null(edge.attributes.df)) { ats.df <- data.frame(sender=at.sender, receiver=at.receiver, type="mention", stringsAsFactors=FALSE)
    }
    ats.df <- ats.df[!is.na(ats.df$sender),]
    ats.df <- ats.df[!ats.df$sender==ats.df$receiver,]
    rownames(ats.df) <- NULL
  }
  
  if(return.graph==FALSE){
    if(return.network=="retweets") { return(rts.df) }
    if(return.network=="mentions") { return(ats.df) }
    if(return.network=="all.interactions") {
      # merge retweet and @-mention data frames
      info.df <- rbind(ats.df, rts.df)
      return(info.df) } 
  }
  
  # graph networks
  if(return.graph==TRUE){
    if(return.network=="retweets") {
      # edgelist to graph
      # if(!is.null(rts.df$edge.attributes.df)) { rts.df$edge.attributes.df <- as.character(rts.df$edge.attributes.df) }
      # create node.attributes.df
      if(!is.null(node.attributes.df) & is.data.frame(node.attributes.df)) {
        screenName.df <- data.frame(screenName=unique(c(rts.df$sender, rts.df$receiver)), stringsAsFactors=FALSE)
        node.attributes.df$screenName <- screenName
        node.attributes.df <- merge(screenName.df, node.attributes.df[!duplicated(node.attributes.df$screenName),], by="screenName", all.x=T) }
      rt.graph <- graph.data.frame(rts.df, directed=TRUE, vertices=node.attributes.df)
      E(rt.graph)$color[E(rt.graph)$type=="retweet"] <- "darkred"
      E(rt.graph)$color[E(rt.graph)$type=="mention"] <- "darkblue"
      return(rt.graph) }
    if(return.network=="mentions") { 
      if(!is.null(node.attributes.df) & is.data.frame(node.attributes.df)) {
        screenName.df <- data.frame(screenName=unique(c(ats.df$sender, ats.df$receiver)), stringsAsFactors=FALSE)
        node.attributes.df$screenName <- screenName
        node.attributes.df <- merge(screenName.df, node.attributes.df[!duplicated(node.attributes.df$screenName),], by="screenName", all.x=T) }      
      at.graph <- igraph::graph.data.frame(ats.df, directed=TRUE, vertices=node.attributes.df)
      igraph::E(at.graph)$color[igraph::E(at.graph)$type=="retweet"] <- "darkred"
      igraph::E(at.graph)$color[igraph::E(at.graph)$type=="mention"] <- "darkblue"
      return(at.graph) }
    if(return.network=="all.interactions") {
      # merge retweet and @-mention data frames
      info.df <- rbind(ats.df, rts.df)
      if(!is.null(node.attributes.df) & is.data.frame(node.attributes.df)) {
        screenName.df <- data.frame(screenName=unique(c(info.df$sender, info.df$receiver)), stringsAsFactors=FALSE)
        node.attributes.df$screenName <- screenName
        node.attributes.df <- merge(screenName.df, node.attributes.df[!duplicated(node.attributes.df$screenName),], by="screenName", all.x=T) }
      info.graph <- igraph::graph.data.frame(info.df, directed=TRUE, vertices=node.attributes.df)
      igraph::E(info.graph)$color[igraph::E(info.graph)$type=="retweet"] <- "darkred"
      igraph::E(info.graph)$color[igraph::E(info.graph)$type=="mention"] <- "darkblue"
      return(info.graph) } }
}
