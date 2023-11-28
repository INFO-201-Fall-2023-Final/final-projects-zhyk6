library(jsonlite)
library(dplyr)
# Numerical variable: 
#   Like_View_Ratio in xx_video dataframes
# Categorical variable:
#   higher_than_mean in top_1000 dataframe
# Summarization:
#   mean_ratio_and_view dataframe


# For xx_videos csv & json files.
### NUMERICAL: Like_View_Ratio
change_date_format <- function(xx_video) {
  pub_year <- substr(xx_video$publish_time, 1, 4)
  pub_month <- substr(xx_video$publish_time, 6, 7)
  pub_day <- substr(xx_video$publish_time, 9, 10)
  xx_video$publish_year <- as.integer(pub_year)
  xx_video$publish_month <- as.integer(pub_month)
  xx_video$publish_day <- as.integer(pub_day)
  xx_video <- subset(xx_video, select = -publish_time)
  
  tre_year <- paste0('20', substr(xx_video$trending_date, 1, 2))
  tre_month <- substr(xx_video$trending_date, 7, 8)
  tre_day <- substr(xx_video$trending_date, 4, 5)
  xx_video$trending_year <- as.integer(tre_year)
  xx_video$trending_month <- as.integer(tre_month)
  xx_video$trending_day <- as.integer(tre_day)
  xx_video <- subset(xx_video, select = -trending_date)
  
  return(xx_video)
}

convert_category <- function(file_name, xx_video) {
  xx_json <- jsonlite::fromJSON(file_name)
  xx_json <- data.frame(xx_json$items)
  xx_json <- data.frame(id = as.integer(xx_json$id), title = xx_json$snippet$title)
  
  xx_video <- merge(xx_video, xx_json, by.x = 'category_id', by.y = 'id')
  xx_video <- xx_video[, !(names(xx_video) %in% c('category_id', 'id'))]
  names(xx_video)[which(names(xx_video) == 'snippet.title')] <- 'title'
  
  xx_video$Like_View_Ratio <- xx_video$likes / xx_video$views
  
  return(xx_video)
}

open_file <- function(file_name, cate_file) {
  xx_video <- read.csv(file_name)
  xx_video <- xx_video[, c('trending_date', 'category_id', 'publish_time',
                           'views', 'likes', 'dislikes', 'comment_count')]
  xx_video <- change_date_format(xx_video)
  xx_video <- convert_category(cate_file, xx_video)
  xx_video$days_from_pub_to_trend <- (xx_video$trending_year - xx_video$publish_year) * 365 +
    (xx_video$trending_month - xx_video$publish_month) * 30 +
    (xx_video$trending_day - xx_video$publish_day)
  return(xx_video)
}


### SUMMARIZATION of MEAN Like-View Ratio and MEAN Views for each category each country.
merge_data <- function(us_video, ca_video, in_video) {
  # Merge data for the USA
  us_l_v_rate <- aggregate(cbind(Like_View_Ratio, views) ~ title, data = us_video, FUN = mean)
  
  # Merge data for Canada
  ca_l_v_rate <- aggregate(cbind(Like_View_Ratio, views) ~ title, data = ca_video, FUN = mean)
  
  # Merge data for India
  in_l_v_rate <- aggregate(cbind(Like_View_Ratio, views) ~ title, data = in_video, FUN = mean)
  
  # Merge USA and Canada data
  us_ca <- merge(us_l_v_rate, ca_l_v_rate, by = "title", all = TRUE)
  
  # Merge all countries' data
  mean_ratio_and_view <- merge(us_ca, in_l_v_rate, by = "title", all = TRUE)
  names(mean_ratio_and_view) <- c("title", "USA_ratio", "USA_view", "Canada_ratio", "Canada_view", "India_ratio", "India_view")
  
  return(mean_ratio_and_view)
}

# For top 1000 subscribed file.
### CATEGORICAL: higher_than_mean in subscribers
top1000 <- function(csv_file) {
  top_1000 <- read.csv(csv_file)
  
  # Filter rows where 'Video Count' is not '0'
  top_1000 <- top_1000[top_1000$Video.Count != '0', ]
  
  # Filter rows where 'Category' is not the error value
  err <- 'https://us.youtubers.me/global/all/top-1000-most_subscribed-youtube-channels'
  top_1000 <- top_1000[top_1000$Category != err, ]
  
  # Convert 'Video Views', 'Video Count', and 'Subscribers' to integers
  top_1000$Video.Views <- as.numeric(gsub(',', '', top_1000$Video.Views))
  top_1000$Video.Count <- as.integer(gsub(',', '', top_1000$Video.Count))
  top_1000$Subscribers <- as.integer(gsub(',', '', top_1000$Subscribers))
  top_1000$higher_than_mean <- top_1000$Subscribers >= mean(top_1000$Subscribers)
  
  return(top_1000)
}


# e.g. For us videos
us_file <- 'USvideos.csv'
us_cate_file <- 'US_category_id.json'
us_video <- open_file(us_file, us_cate_file)
us_video <- us_video[us_video$views >= 5e5, ]

in_file <- 'INvideos.csv'
in_cate_file <- 'IN_category_id.json'
in_video <- open_file(in_file, in_cate_file)
in_video <- in_video[in_video$views >= 5e5, ]

ca_file <- 'CAvideos.csv'
ca_cate_file <- 'CA_category_id.json'
ca_video <- open_file(ca_file, ca_cate_file)
ca_video <- ca_video[ca_video$views >= 5e5, ]


mean_ratio_and_view <- merge_data(us_video, ca_video, in_video)

top_1000 <- top1000('topSubscribed.csv')
