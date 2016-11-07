require(RColorBrewer)
require(ggplot2)
require(reshape)
require(zoo)
require(scales)
require(knitr)
require(ggmap)

eviction_heatmap <- function(df) {
  df[1:19] <- lapply(df[1:19],function(x) as.integer(x) - 1)
  evic_matrix <- aggregate(df[1:19], by = list(df$Neighborhood), FUN = sum)
  heatmatrix = data.matrix(evic_matrix[,2:20])
  row.names(heatmatrix) = evic_matrix[,1]
  grad = brewer.pal(9,"Blues")
  png('eviction_heatmap.png', units = 'in', width = 11, height = 8, res = 600)
  h <- heatmap(heatmatrix, Rowv = NA, Colv = NA, margins = c(10,4), col = grad)
  dev.off()
}

grouped_bar <- function(matrix, grouping) {
  # Graphs the grouped bar plot for the filtered dataframe passed in
  # x: Neighborhood, y: Evictions, bars: Eviction Type
  #
  # If grouping == neighborhood, pass in the dataframe outputted from fn: top_n_neighborhoods_for_reason
  # If grouping == reason, pass in the dataframe outputted from fn: top_n_reasons_for_neighborhood
  if (!(grouping %in% c('neighborhood', 'reason')) | missing(grouping)) {
    stop("Please provide a grouping of either 'neighborhood' or 'reason'.")
  }
  num_neighbor <- length(unique(matrix$Neighborhood))
  num_reason <- length(colnames(matrix[,!colnames(matrix) %in% c('Neighborhood', 'Eviction.Total')]))
  matrix <- matrix[, !colnames(matrix) %in% 'Eviction.Total']
  if (grouping == 'neighborhood'){
    filename = paste0('output/top_',num_neighbor,'_neighborhoods_for_top_', num_reason,'_reasons.png')
    m <- melt(matrix, id = 'Neighborhood')
    png(filename, units = 'in', width = 11, height = 8, res = 600)
    g <- ggplot(m, aes(x = Neighborhood, y = value, fill=variable), las = 2) +
      geom_bar(stat="identity",position="dodge")
    g <- g + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(x = 'Neighborhood', y = 'Evictions', title = "Evictions by Neighborhood and Reason", fill = "Reason")
  } else {
    filename = paste0('output/top_',num_reason,'_reasons_for_top_', num_neighbor,'_neighborhoods.png')
    m <- melt(matrix, id = 'Reason')
    png(filename, units = 'in', width = 11, height = 8, res = 600)
    g <- ggplot(m, aes(x = Reason, y = value, fill=variable), las = 2) +
      geom_bar(stat="identity",position="dodge")
    g <- g + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(x = 'Eviction Reason', y = 'Evictions', title = "Evictions by Neighborhood and Reason", fill = "Neighborhood")
  }
  plot(g)
  dev.off()
}

top_n_neighborhoods <- function(df, n){
  # Returns the top N neighborhoods based on total evictions
  df$Eviction.Total <- rowSums(df[,2:dim(df)[2], drop = FALSE])
  df <- head(df[order(df$Eviction.Total, decreasing = TRUE),], n = n)
  return(df)
}

top_n_neighborhoods_for_reason <- function(df, n, reason_df) {
  reason <- rownames(reason_df)
  # Returns the top N neighborhoods based on the provided eviction reasons
  df <- df[, colnames(df) %in% append(reason, 'Neighborhood')]
  return(top_n_neighborhoods(df, n))
}

top_n_reasons <- function(df, n) {
  # Returns the top N eviction reasons
  df_t <- as.data.frame(t(df[,-1]))
  colnames(df_t) <- df$Neighborhood
  df <- head(df_t[order(rowSums(df_t[1:dim(df_t)[1],, drop = FALSE]), decreasing = TRUE),], n = n)
  df$Reason <- rownames(df_t)[1:n]
  return(df)
}

top_n_reasons_for_neighborhood <- function(df, n, neighborhood_df) {
  # Returns the top N eviction reasons based on the provided neighborhoods
  df <- df[df$Neighborhood %in% neighborhood_df$Neighborhood,]
  return(top_n_reasons(df, n))
}

evict_by_neighborhood <- function(df, agg, neighborhood = NaN){
  if (!(agg %in% c('month', 'year')) | missing(agg)) {
    stop("Please specify either 'month' or 'year' as the aggregation type.")
  }
  t = 'San Francisco Evictions'
  if (!is.nan(neighborhood)) {
    df <- df[df$Neighborhood %in% neighborhood,]
    t = neighborhood
  }
  df$File.Date <- as.Date(as.character(factor(df$File.Date)), format = "%m/%d/%Y")
  ts <- df[order(df$File.Date, decreasing = FALSE),]
  ts$evictno <- 1:nrow(ts)
  if (agg == 'month') {
    moyr <- as.yearqtr(ts$File.Date, format = "%B %Y")
  } else {
    moyr <- format(ts$File.Date, '%Y')
  }

  df.agg <- aggregate(evictno ~ moyr, ts, FUN = length)
  df.agg$rnum <- 1:nrow(df.agg)
  if (agg == 'month') {
    f <- ggplot(df.agg, aes(reorder(as.yearmon(moyr),rnum), evictno, group = 1)) + geom_line(col = 'blue')
  } else{
    f <- ggplot(df.agg, aes(reorder(moyr,rnum), evictno, group = 1)) + geom_line(col = 'blue')
  }
  f <- f + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = 'Date', y = 'Evictions', title = t)
  return(f)
}

evict_map <- function(df, year = NaN) {
  if (is.nan(year)) {
    fname <- 'San Francisco Evictions.png'
  } else {
    fname <- paste0('San Francisco Evictions in ', year, '.png')
  }
  png(paste('output/gps/',fname), units = 'in', width = 11, height = 8, res = 400)
  m <- suppressMessages(get_map(location = c(-122.4431871, 37.7605004), zoom = 12, maptype = "roadmap"))
  m <- suppressMessages(ggmap(m) + geom_point(data = df, aes(x = Long, y = Lat), col = 'red', size = 0.25, alpha = 0.6) +
    scale_x_continuous(limits = c(-122.515, -122.355), expand = c(0, 0)) +
    scale_y_continuous(limits = c(37.705, 37.84), expand = c(0,0)) + labs(x = 'Longitude', y = 'Latitude', title = fname))
  plot(m)
  dev.off()
}

## @knitr part1
data <- read.csv('Eviction_Notices.csv')

names(data)[5] = 'Zip'
names(data)[28] = 'Neighborhood'
tmp <- as.character(data$Location)
tmp <- gsub("\\)","",gsub("\\(","",tmp))
tmp <- strsplit(tmp,split=",")
data$Lat <- as.numeric(sapply(tmp, function(x) x[1]))
data$Long <- as.numeric(sapply(tmp, function(x) x[2]))
data$Year <- format(as.Date(data$File.Date, '%m/%d/%Y'), "%Y")
drops <- c('State', 'City', 'Location')
data <- data[data$Neighborhood !="",]
data <- data[,!names(data) %in% drops]


#construct the evition matrix and neighborhood
data[5:23] <- lapply(data[5:23],function(x) as.integer(x) - 1)
evictionMatrix <- cbind(data[,c(5:23,26)], data$Lat, data$Long, data$Year)
agg_evict_matrix <- aggregate(evictionMatrix[1:19], by = list(evictionMatrix$Neighborhood), FUN = sum)
colnames(agg_evict_matrix)[1] <- 'Neighborhood'
data <- transform(data, evictCount = rowSums(data[,5:23]))

# # Plot the heatmap of Neighborhoods vs Reason
# eviction_heatmap(evictionMatrix)


a <- top_n_neighborhoods_for_reason(agg_evict_matrix, 10, top_n_reasons(agg_evict_matrix, 6))

# grouped_bar(a, 'neighborhood')
# grouped_bar(top_n_reasons_for_neighborhood(agg_evict_matrix, 5, top_n_neighborhoods(agg_evict_matrix, 10)), 'reason')

# data <- data[data$Neighborhood %in% top_n_neighborhoods(agg_evict_matrix, 4)$Neighborhood,]

# Multiplot the yearly eviction by neighborhood
# plots <- list()
# for (i in seq_along(unique(data$Neighborhood))) {
#   plots[[i]] <- evict_by_neighborhood(data, 'year', unique(data$Neighborhood)[i])
# }
# multiplot(plotlist = plots,cols = 7)


# # Plot GPS points of evictions by year
# for (i in sort(unique(data$Year))) {
#   evict_map(data[data$Year == i,], i)
# }