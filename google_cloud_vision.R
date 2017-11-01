# This script uses the Google Cloud Vision API to recognize facial emotions
# The API can recognize faces, and 4 distinct emotions (joy, anger, sadness, surprise)

# devtools::install_github("cloudyr/RoogleVision")

library(tidyverse)
library(RoogleVision)
library(jsonlite)
library(stringr)
# library(grid)
library(jpeg)

source("plot_emotions_google.R")

# Set google authentification
# Have to register at google cloud first
# Have to create credentials to use the API
google_auth_cred <- fromJSON('google-credentials.json')
options("googleAuthR.client_id" = google_auth_cred$installed$client_id)
options("googleAuthR.client_secret" = google_auth_cred$installed$client_secret)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()

# Load pictures
pic_pulpfiction <- "image/vincent_and_jules_car.jpg"
pic_ekman <- "image/Ekman_faces.jpg"
pic_faces <- "image/faces.jpg"

gca_result <- getGoogleVisionResponse(pic_ekman, feature = 'FACE_DETECTION')

# TODO: Create pretty functional programming solution. Now it is in two parts
emotions <-
gca_result %>% 
    select(joy = joyLikelihood, sadness = sorrowLikelihood, anger = angerLikelihood, surprise = surpriseLikelihood) %>% 
        mutate(id = row_number())

coords <- 
    gca_result %>% 
    pull(fdBoundingPoly) %>%
    mutate(id = row_number()) %>% 
    unnest(vertices) %>% 
    group_by(id) %>% 
    summarise(
            xmin = nth(x,1),
            xmax = nth(x,2),
            ymin = nth(y,1),
            ymax = nth(y,3)
    ) %>% 
    correct_coord()
# Correct binding by calculating the average area (only applicable when faces are of the same size)data without missing 
# The logic is this: calculate are for all 
# Note that since we may have a max or min value missing, but hopefully not both, we have to 
# caluculate max twice
coords <-
    coords %>% 
    mutate(xlength = xmax - xmin,
           ylength = ymax - ymin,
           area = xlength * ylength,
           area = ifelse(is.na(area), mean(area, na.rm = TRUE), area), # Imput area with average
           xlength = ifelse(is.na(xlength), (area/ylength) %>% round(), xlength),
           ylength = ifelse(is.na(ylength), (area/ylength) %>% round(), ylength)) %>% 
    mutate(
        xmax = ifelse(is.na(xmax), (xlength - xmin) %>% round(), xmax),
        xmin = ifelse(is.na(xmin), (xlength - xmax) %>% round() %>% pmax(1), xmin),
        xmax = ifelse(is.na(xmax), (xlength - xmin) %>% round(), xmax),
        ymax = ifelse(is.na(ymax), (ylength - min) %>% round(), ymax),
        ymin = ifelse(is.na(ymin), (ylength - ymax) %>% round() %>% pmax(1), ymin),
        ymax = ifelse(is.na(ymax), (ylength - min) %>% round(), ymax)
    )
          
coords %>% 
    summarise(mean_area = mean(area),
              sd_area = sd(area))


# Make lm predictions using the existing coordinate and the length, based on average area of rectangles
# This is interesting, but not necessarily needed


# Putting together the emotions with the coordinates. Removing non-recognized emotions
emotion_df <- 
    coords %>% 
    full_join(emotions, by = "id") %>% 
    gather(emotion, value, joy:surprise) %>% 
    filter(!str_detect(value, "UNLIKELY"))


# Plot the pic and the bounding recs with emotion prediction
img <- readJPEG(pic_ekman)
mgk_info <- tibble(height = dim(img)[1], width = dim(img)[2])
g <- rasterGrob(img, interpolate = FALSE, width = unit(1, "npc"), height = unit(1, "npc"))

ggplot(emotion_df) +
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        x = (xmin + xmax)/2, y = ymin, # x and y needed for the labels
        group = id,
        color = emotion,
        label = paste0(emotion %>% str_to_upper(), ": ", value %>% str_to_lower())) +
    scale_x_continuous(limits = c(0, mgk_info$width)) +
    scale_y_reverse(limits = c(mgk_info$height, 0)) + # Y axis reversed in rastergrobs
    annotation_custom(g, xmin = 0, xmax = mgk_info$width, ymin = 0, ymax = -mgk_info$height) +
    geom_rect(alpha = 0.2, size = 2) +
    geom_label() +
    theme_void() + 
    theme(legend.position = "none")



get_emotions_google("image/Ekman_faces.jpg")
plot_emotions_google("image/Ekman_faces.jpg")

