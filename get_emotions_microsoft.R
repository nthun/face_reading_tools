library(tidyverse)
library(httr)
library(stringr)

api_url <- "https://westus.api.cognitive.microsoft.com/emotion/v1.0/recognize?visualFeatures=Faces"

get_emotions_microsoft <- function(image_url, api_key){

    API_response <- POST(url = api_url,
                       content_type("application/json"),
                       add_headers(.headers = c('Ocp-Apim-Subscription-Key' = api_key)),
                       body = list(url = image_url),
                       encode = "json") 

    df <- content(API_response)
    
    map_dfr(df, "faceRectangle") %>% 
        mutate(face_id = row_number()) %>% 
        full_join(map_dfr(df, "scores") %>% 
                      mutate(face_id = row_number()),
                  by = "face_id") %>% 
        mutate(xmin = left,
               xmax = left + width,
               ymin = top, # It is calculated from the top left
               ymax = top + height) %>% 
        select(face_id, xmin:ymax, anger:surprise) %>% 
        gather(emotion, value, -(face_id:ymax)) %>% 
        mutate(value = value %>% round(2)) # Round values for 2 decimals
}