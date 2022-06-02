#library(httr)
#library(stringr)

# see: https://www.yuichiotsuka.com/youtube-data-extract-r/
source("key.R")
base <- "https://www.googleapis.com/youtube/v3/"

# https://youtu.be/wiknzClcm4s

fmt_dur_str <- function(dur) {
  str_replace(dur,"^PT(\\d+?)M(\\d+?)S$","\\1m\\2s")
}

# "PT2M25S" => (2,25)
extr_dur_m_s <- function(dur) {
  m_s <- str_replace(dur,"^PT(\\d+?)M(\\d+?)S$","\\1,\\2")
  m_s_l <- str_split_fixed(m_s,",",2) %>% map_int(as.integer)
  m_s_l
}

extr_dur_mins <- function(dur) {
  m_s_l <- extr_dur_m_s(dur)
  m_s_l[[1]] + m_s_l[[2]]/60
}

get_video_descr <- function(video) {
  api_params <- 
    paste(paste0("key=", key), 
          paste0("id=", video), 
          # fileDetails: must be connected to my account
          "part=snippet,contentDetails",
          sep = "&")
  api_call <- paste0(base, "videos?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
  channel.json <- fromJSON(json_result, flatten = T)
  # channel.df <- as.data.frame(channel.json)
  thumb_url <- channel.json$items$snippet.thumbnails.high.url
  descr <- channel.json$items$snippet.description
  date <- channel.json$items$snippet.publishedAt
  # PT<int>M<int>S
  duration <- channel.json$items$contentDetails.duration
  list(thumb_url=thumb_url,
       descr=descr,
       date=as.Date(date),
       duration=duration)
}

get_videos_descr <- function(df,fname) {
  df_descr <- df %>%
    #head(10) %>%
    #slice(300:390) %>%
    mutate(video_id=str_remove_all(Video1,"http[s?]://youtu.be/")) %>%
    mutate(descr_url=video_id %>% map_dfr(get_video_descr)) %>%
    mutate(descr=descr_url$descr,
           thumb_url=descr_url$thumb_url,
           date=descr_url$date,
           duration=descr_url$duration)
  df_descr %>% write_rds(fname,"bz2")
  df_descr
}

download_thumb <- function(dir,url,video_id) {
  download.file(url, str_glue("{dir}/{video_id}.jpg"), quiet=T, mode = 'wb')
}