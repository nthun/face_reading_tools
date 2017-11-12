# MS Azure Cognitive services API
library(tidyverse)
library(httr)
library(stringr)
library(glue)

source("get_emotions_microsoft.R")
source("create_plot_labels.R")
source("plot_emotions.R")

credentials <- read_csv("MSA_credentials.csv")
api_key <- credentials$key1
api_endpoint_url <- "https://westus.api.cognitive.microsoft.com/emotion/v1.0/recognize"

image_url <- "https://raw.githubusercontent.com/nthun/face_reading_tools/master/image/Ekman_faces.jpg"

reqURL <- paste0(api_endpoint_url, "?visualFeatures=Faces")

APIresponse <- POST(url = reqURL,
                   content_type('application/json'),
                   add_headers(.headers = c('Ocp-Apim-Subscription-Key' = api_key)),
                   body=list(url = image_url),
                   encode = "json") 

df <- content(API_response)

## 
base_url <- "https://raw.githubusercontent.com/nthun/face_reading_tools/master/ck_2_faces/"
image_url <- 'https://raw.githubusercontent.com/nthun/face_reading_tools/master/ck_2_faces/S010_006_00000015.png'



urls <- paste0(base_url, list.files("ck_2_faces/"))

for (i in urls)

microsoft_plots <- 
get_emotions_microsoft("https://raw.githubusercontent.com/nthun/face_reading_tools/master/ck_2_faces/S010_006_00000015.png", api_key)

microsoft_plots %>% create_plot_labels() %>% 
    plot_emotions("ck_2_faces/S010_006_00000015.png")


microsoft_test_results <- data_frame()
prog <- 0
for (i in urls){
    microsoft_test_results <- bind_rows(microsoft_test_results, 
                                     get_emotions_microsoft(i, api_key) %>% 
                                         mutate(image_file = i)
    )
    prog <- prog + 1
    print(glue::glue("Finished picture {prog} of {length(urls)}: {i}"))
    Sys.sleep(3)
}

# save(microsoft_test_results, file = "microsoft_test_results.RData")
load("selected_pictures.RData")

selected_pictures <- 
    selected_pictures %>% 
    mutate(image_file = str_match(image_file, ".*/(.*.png$)")[,2])


microsoft_plots <-
    microsoft_test_results %>% 
    mutate(image_file = str_match(image_file, ".*/(.*.png$)")[,2],
           emotion = if_else(emotion == "happiness", "happy", emotion)) %>% 
    group_by(image_file) %>% 
    nest(.key = "emotion_df") %>% 
    mutate(pic_result = map2(emotion_df, image_file, ~plot_emotions(create_plot_labels(.x), paste0("ck_2_faces/", .y)))) %>% 
    left_join(selected_pictures, by = "image_file")

# Create foler structure
walk(emotion_codes$emotion, ~dir.create(glue("./results/microsoft_annotated_pictures/{.x}")))

# Save files
pwalk(list(read_csv("emotions.csv")$emotion, microsoft_plots$emotion, microsoft_plots$pic_id), ~ggsave(..1, filename = glue("./results/microsoft_annotated_pictures/{..2}/{..3}.jpg"), width = 11, height = 6.4, units = "in"))

## Evaluate google results compared to the labels
# See which emotions are identified well
microsoft_test_results %>% 
    mutate(image_file = str_match(image_file, ".*/(.*.png$)")[,2],
           emotion = if_else(emotion == "happiness", "happy", emotion %>% str_to_lower())) %>% 
    left_join(selected_pictures, by = "image_file") %>% 
    select(-image_file) %>% 
    mutate(match = emotion.x %>% str_to_lower() == emotion.y) %>% 
    group_by(pic_id) %>% 
    arrange(-value) %>% 
    slice(1) %>% # Get only he value with the highest probability for each pic
    ungroup() %>% 
    filter(match) %>% 
    count(emotion.y, match)

# Calculatet average % for correct match
microsoft_test_results %>% 
    mutate(image_file = str_match(image_file, ".*/(.*.png$)")[,2],
           emotion = if_else(emotion == "happiness", "happy", emotion %>% str_to_lower())) %>% 
    left_join(selected_pictures, by = "image_file") %>% 
    select(-image_file) %>% 
    mutate(match = emotion.x %>% str_to_lower() == emotion.y) %>% 
    group_by(pic_id) %>% 
    filter(emotion.x == emotion.y) %>% 
    group_by(emotion.x) %>% 
    summarise(mean = mean(value))


# Create confusion matrix for results
microsoft_test_results %>% 
    mutate(image_file = str_match(image_file, ".*/(.*.png$)")[,2],
           emotion = if_else(emotion == "happiness", "happy", emotion)) %>% 
    left_join(selected_pictures, by = "image_file") %>% 
    select(-image_file) %>% 
    # mutate(match = emotion.x %>% str_to_lower() == emotion.y) %>% 
    group_by(pic_id) %>% 
    arrange(-value) %>% 
    slice(1) %>% # Get only he value with the highest probability for each pic
    ungroup() %>% 
    select(prediction = emotion.x, reference = emotion.y) %>% 
    table() %>% 
    write.table("macs_ct.tsv")



