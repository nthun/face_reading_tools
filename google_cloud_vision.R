# This script uses the Google Cloud Vision API to recognize facial emotions
# The API can recognize faces, and 4 distinct emotions (joy, anger, sadness, surprise)

# devtools::install_github("cloudyr/RoogleVision")

library(tidyverse)
library(RoogleVision)
library(jsonlite)
library(stringr)
# library(grid)
library(jpeg)
library(glue)
source("get_emotions_google.R")
source("create_plot_labels.R")
source("plot_emotions.R")

# Set google authentification
# Have to register at google cloud first
# Have to create credentials to use the API
google_auth_cred <- fromJSON('google-credentials.json')
options("googleAuthR.client_id" = google_auth_cred$installed$client_id)
options("googleAuthR.client_secret" = google_auth_cred$installed$client_secret)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()

gca_result <- getGoogleVisionResponse(pic_ekman, feature = 'FACE_DETECTION')

result %>% 
    create_plot_labels() %>% 
    plot_emotions("image/faces-of-american-power.jpg")

# Get result with an ugly loop (can do progress bar in purrr:map)
google_test_results <- data_frame()
prog <- 0
for (i in selected_pictures$image_file){
    google_test_results <- bind_rows(google_test_results, 
                                     get_emotions_google(i) %>% 
                                         mutate(image_file = i)
    )
    prog <- prog + 1
    print(glue::glue("Finished picture {prog} of {nrow(selected_pictures)}: {i}"))
}

## Evaluate google results compared to the labels
# See whihch emotions are identified well
google_test_results %>% 
    mutate(emotion = if_else(emotion == "JOY", "happy", emotion %>% str_to_lower())) %>% 
    left_join(selected_pictures, by = "image_file") %>% 
    select(-image_file) %>% 
    mutate(match = emotion.x %>% str_to_lower() == emotion.y) %>% 
    filter(!str_detect(value, "unlikely") & match == TRUE) %>% # Exclude unlikely emotions
    count(value, emotion.y)
# Result:
# happy: 10/10, surprise: 10/10, sadness: 7/10, anger: 0/10, disgust: NA, 


google_test_results %>% 
    filter(image_file == "./Emotion images/S082/005/S082_005_00000017.png") %>% 
    create_plot_labels() %>%
    plot_emotions("./Emotion images/S082/005/S082_005_00000017.png")

# Plotting emotions
temp <-
    google_test_results %>% 
    group_by(image_file) %>% 
    nest(.key = "emotion_df") %>% 
    mutate(pic_result = map2(emotion_df, image_file, ~plot_emotions(create_plot_labels(.x), .y))) %>% 
    mutate(pic_id = str_match(image_file, ".*/(.*).png$")[,2]) %>% 
    left_join(selected_pictures)

# Create directory sructure for anotated pics
walk(emotion_codes$emotion, ~dir.create(glue("./results/google_annotated_pictures/{.x}")))

# Saving all plots to a specified folder structure
pwalk(list(temp$pic_result, temp$emotion, temp$pic_id), ~ggsave(..1, filename = glue("./results/annotated_pictures/{..2}/{..3}.jpg")))

temp %>% 
    sample_n(1) %>% 
    pull(pic_result)

google_test_results %>% 
    group_by(image_file) %>% 
    nest() %>%
    mutate(plot_label = map(data, ~create_plot_labels(.x)))

# Save the test result lest we need to download them again
load(file = "google_test_results.RData")
# save(google_test_results, file = "google_test_results.RData")




df %>% 
    filter(image_file == "./Emotion images/S082/005/S082_005_00000017.png") 



google_test_results %>% 
    filter(image_file == "./Emotion images/S082/005/S082_005_00000017.png") %>% 
    filter(!str_detect(value, "very unlikely")) %>% # Exclude very unlikely predictions
    group_by(id, xmax, xmin, ymax, ymin) %>%
    summarise(label = paste0(emotion, ": ", value) %>% paste(collapse = "\n")) %>%
    full_join(df %>% select(-emotion, -value), by = c("id","xmax","xmin","ymax","ymin")) %>%
    filter(!duplicated(id)) %>%
    # mutate(label = if_else(is.na(label), "not recognized", label)) %>%
    plot_emotions("./Emotion images/S082/005/S082_005_00000017.png")


get_emotions_google("image/Ekman_faces.jpg")


