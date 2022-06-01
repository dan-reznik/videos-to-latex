library(httr)

# see: https://www.yuichiotsuka.com/youtube-data-extract-r/
key <- "AIzaSyCeBljlBtzlUB8oPYs4t9Y8GOhgCdJ12LA"
channel <- "UCJ3BPNdm0z1UzcJYTwxlsjA"
user <- "dreznik"
base <- "https://www.googleapis.com/youtube/v3/"

get_video_descr <- function(video) {
  api_params <- 
    paste(paste0("key=", key), 
          paste0("id=", video), 
          "part=snippet",
          sep = "&")
  api_call <- paste0(base, "videos?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
  channel.json <- fromJSON(json_result, flatten = T)
  # channel.df <- as.data.frame(channel.json)
  channel.json$items$snippet.description
}