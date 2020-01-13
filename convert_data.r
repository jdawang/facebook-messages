library(purrr)
library(dplyr)
library(tidyr)

path <- "./data/raw_dump/"
json_files <- stringr::str_c(path, list.files(path, pattern = "*.json"))

clean_raw_json <- function(path_to_file) {
    message <- jsonlite::fromJSON(path_to_file)$messages

    message[, c("share_link", "uri", "share")] <- NULL
    message$reactions <- map2(
        message$reactions,
        message$sender_name,
        function(reactions, sender_name) {
            if (is.null(reactions)) NA_character_
            else {
                proposed <- reactions$reaction[reactions$actor != sender_name]
                if (length(proposed) > 0) proposed
                else NA_character_
            }
        }
    ) %>% unlist

    message <- message %>%
        mutate(
            reactions = case_when(
                is.na(reactions) ~ NA_character_,
                reactions == "ð\u009f\u0098\u0086" ~ 'laugh',
                reactions == "ð\u009f\u0091\u008d" ~ 'thumbs up',
                reactions == "ð\u009f\u0098\u008d" ~ 'heart eyes',
                reactions == "ð\u009f\u0098®" ~ "wow",
                reactions == "ð\u009f\u0091\u008e" ~ "thumbs down",
                reactions == "ð\u009f\u0098¢" ~ "sad",
                reactions == "ð\u009f\u0098 " ~ "angry",
                TRUE ~ NA_character_
            ),
            type = case_when(
                stringr::str_to_lower(type) == 'generic' ~ 'text',
                stringr::str_detect(content, "(Gabi|You) sent a link") |
                    stringr::str_detect(content, "(http|www)")  ~ "link",
                !is.null(photos) ~ 'photo',
                !is.null(videos) ~ 'video',
                !is.null(gifs)   ~ 'gif',
                !is.na(sticker$uri) ~ 'sticker',
                TRUE ~ NA_character_
            )) %>%
        select(-photos, -videos, -gifs, -sticker) %>%
        as_tibble() %>%
        mutate(datetime = lubridate::as_datetime(timestamp_ms/1000, tz ='EST'))

    return(message)
}

total_frame <- map_dfr(json_files, clean_raw_json)
