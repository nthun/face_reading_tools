library(tidyverse)
library(httr)
library(magrittr)

api_endpoint_url <- "https://api.kairos.com/v2/media?source="
uri_parameters <- "&timeout=15"

get_emotions_kairos <- function(img_url, api_id, api_key){
    req_url <- paste0(api_endpoint_url, 
                      img_url %>% URLencode(reserved = TRUE), 
                      uri_parameters)
    api_response <- POST(
        url = req_url,
        content_type('application/json'),
        add_headers(.headers = c("app_id" = api_id,
                                 "app_key" = api_key))
    )
    response_list <- content(api_response)
    if (response_list$frames[[1]]$people %>% length > 0){
        df <- 
            tibble(face_id = map_int(response_list$frames[[1]]$people, "person_id")+1) %>% 
            bind_cols(map_df(response_list$frames[[1]]$people, "face"),
                      map_df(response_list$frames[[1]]$people, "emotions")) %>% 
            mutate(xmin = x,
                   xmax = x + width,
                   ymin = y, # It is calculated from the top left
                   ymax = y + height) %>% 
            select(face_id, xmin:ymax, anger:surprise) %>% 
            gather(emotion, value, -(face_id:ymax)) %>% 
            mutate(value = value %>% divide(100) %>% round(2))
            return(df)        
    }
    else {tibble()}
}

