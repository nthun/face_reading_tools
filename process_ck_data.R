# Process CK+ data, select random annotated pictures
library(tidyverse)
library(stringr)

emotion_codes <- read_csv("emotions.csv")

emotions <-
    tibble(coding_file = list.files("d:/Documents/research/face detection/Emotion" , full.names = TRUE, recursive = TRUE)) %>% 
    mutate(code = map_int(coding_file, ~read_lines(.x) %>% trimws() %>% as.integer())) %>% 
    left_join(emotion_codes, by = "code") %>% 
    select(-code)

emotions %>% 
    group_by(emotion) %>%
    count()

# Randomly select 10 of each emotional pictures (+ neutral)
set.seed(22)
selected_pictures <- 
    emotions %>% 
    group_by(emotion) %>% 
    sample_n(10) %>% 
    mutate(image_file = coding_file %>% str_replace("Emotion","Emotion images") %>% str_replace("_emotion.txt",".png"))  %>% 
    mutate(id = str_match(image_file, ".*/(.*).png$")[,2]) %>%
    select(id, emotion, everything())

# This copies all selected images to one folder for easy handling (and minimalizing size requirements)
walk(selected_pictures$image_file, ~file.copy(from = .x, to = "./ck_2_faces"))

save(selected_pictures, file = "selected_pictures.RData")


# Get sample pictures for each emotion
selected_pictures %>% 
    group_by(emotion) %>% 
    slice(1) %>% 
    select(image_file)


