require(RColorBrewer)
require(ggplot2)
require(reshape)
require(zoo)
require(scales)
require(knitr)
require(ggmap)
source("multiplot.R")

eviction_heatmap <- function(df) {
  df[1:19] <- lapply(df[1:19],function(x) as.integer(x) - 1)
  evic_matrix <- aggregate(df[1:19], by = list(df$Neighborhood), FUN = sum)
  heatmatrix <- data.matrix(evic_matrix[,2:20])
  row.names(heatmatrix) = evic_matrix[,1]
  grad <- brewer.pal(9,"Blues")
  png('eviction_heatmap.png', units = 'in', width = 11, height = 8, res = 600)
  h <- heatmap(heatmatrix, Rowv = NA, Colv = NA, margins = c(10,4), col = grad)
  dev.off()
}

top_n_neighborhoods <- function(df, n){
  # Returns the top N neighborhoods based on total evictions
  # df is the aggregated matrix by neighborhood
  df$Eviction.Total <- rowSums(df[,2:dim(df)[2], drop = FALSE])
  df <- head(df[order(df$Eviction.Total, decreasing = TRUE),], n = n)
  return(df)
}

top_n_neighborhoods_for_reason <- function(df, n, reason_df) {
  reason <- rownames(reason_df)
  # Returns the top N neighborhoods based on the provided eviction reasons
  # df is the aggregated matrix by neighborhood
  df <- df[, colnames(df) %in% append(reason, 'Neighborhood')]
  return(top_n_neighborhoods(df, n))
}

top_n_reasons <- function(df, n) {
  # Returns the top N eviction reasons
  # df is the aggregated matrix by neighborhood
  df_t <- as.data.frame(t(df[,-1]))
  colnames(df_t) <- df$Neighborhood
  df <- head(df_t[order(rowSums(df_t[1:dim(df_t)[1],, drop = FALSE]), decreasing = TRUE),], n = n)
  df$Reason <- rownames(df_t)[1:n]
  return(df)
}

top_n_reasons_for_neighborhood <- function(df, n, neighborhood_df) {
  # Returns the top N eviction reasons based on the provided neighborhoods
  # df is the aggregated matrix by neighborhood
  df <- df[df$Neighborhood %in% neighborhood_df$Neighborhood,]
  return(top_n_reasons(df, n))
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

  matrix <- matrix[, !colnames(matrix) %in% 'Eviction.Total']
  if (grouping == 'neighborhood') {
    num_neighbor <- length(unique(matrix$Neighborhood))
    num_reason <- length(colnames(matrix[,!colnames(matrix) %in% c('Neighborhood', 'Eviction.Total')]))
    filename <- paste0('output/top_',num_neighbor,'_neighborhoods_for_top_', num_reason,'_reasons.png')
    m <- melt(matrix, id = 'Neighborhood')
    png(filename, units = 'in', width = 11, height = 8, res = 600)
    g <- ggplot(m, aes(x = Neighborhood, y = value, fill=variable), las = 2) +
      geom_bar(stat="identity",position="dodge")
    g <- g + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(x = 'Neighborhood', y = 'Evictions', title = "Evictions by Neighborhood and Reason", fill = "Reason")
  } else {
    num_neighbor <- length(colnames(matrix[,!colnames(matrix) %in% c('Reason')]))
    num_reason <- nrow(matrix)
    filename <- paste0('output/top_',num_reason,'_reasons_for_top_', num_neighbor,'_neighborhoods.png')
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
    f <- ggplot(df.agg, aes(reorder(as.yearmon(moyr),rnum), evictno, group = 1)) + geom_line(col = 'blue', size = 2)
  } else{
    f <- ggplot(df.agg, aes(reorder(moyr,rnum), evictno, group = 1)) + geom_line(col = 'blue', size = 2)
  }
  f <- f + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = 'Date', y = 'Evictions', title = t)
  return(f)
}

evict_map <- function(df, year = NaN) {
  if (is.nan(year)) {
    pt_size <- .1
    trans <- .25
    plt_title <- 'San Francisco Evictions'
  } else {
    plt_size <- 1
    trans <- 1
    plt_title <- paste0('San Francisco Evictions in ', year)
  }
  png(paste('output/gps/', paste0(plt_title, '.png')), units = 'in', width = 11, height = 8, res = 400)
  m <- suppressMessages(get_map(location = c(-122.4431871, 37.7605004), zoom = 12, maptype = "roadmap"))
  m <- suppressMessages(ggmap(m) + geom_point(data = df, aes(x = Long, y = Lat, colour = Reason), size = plt_size, alpha = trans) +
    scale_x_continuous(limits = c(-122.515, -122.355), expand = c(0, 0)) +
    scale_y_continuous(limits = c(37.705, 37.84), expand = c(0,0)) + labs(x = 'Longitude', y = 'Latitude', title = plt_title))
  plot(m)
  dev.off()
}

## @knitr part1
data <- read.csv('Eviction_Notices.csv')

names(data)[5] <- 'Zip'
names(data)[28] <- 'Neighborhood'
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
eviction_heatmap(evictionMatrix)

## @knitr part2
# grouped_bar(top_n_neighborhoods_for_reason(agg_evict_matrix, 10, top_n_reasons(agg_evict_matrix, 6)), 'neighborhood')
print(top_n_reasons_for_neighborhood(agg_evict_matrix, 5, top_n_neighborhoods(agg_evict_matrix, 10)))
grouped_bar(top_n_reasons_for_neighborhood(agg_evict_matrix, 5, top_n_neighborhoods(agg_evict_matrix, 10)), 'reason')

# # Multiplot the yearly eviction by neighborhood
# plots <- list()
# for (i in seq_along(unique(data$Neighborhood))) {
#   plots[[i]] <- evict_by_neighborhood(data, 'year', unique(data$Neighborhood)[i])
# }
# png('output/Yearly_Eviction_By_Neighborhood.png', units = 'in', width = 14, height = 9, res = 600)
# multiplot(plotlist = plots, cols = 7)
# dev.off()
png('output/SF_Evictions_by_Qtr.png', units = 'in', width = 14, height = 9, res = 600)

evict_by_neighborhood(data,'month')
dev.off()

## @knitr part3
# Plot GPS points of evictions by year
reason = list()
singleEvict <- data[data$evictCount == 1,]
top_5_reason <- unique(top_n_reasons(agg_evict_matrix, 5)$Reason)
for (i in 1:nrow(singleEvict)) {
  if (i %% 2000 == 0) {print(paste('processing', i, 'of', nrow(singleEvict)))}
  reason[[i]] <- colnames(singleEvict[,c(5:23)])[which(singleEvict[i, c(5:23)] == 1)]
}
evictReason = cbind(singleEvict, Reason = t(as.data.frame(reason)))
for (i in sort(unique(evictReason$Year))) {
  evict_map(evictReason[(evictReason$Year == i) & (evictReason$Reason %in% top_5_reason),], i)
}