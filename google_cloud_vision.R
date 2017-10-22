# devtools::install_github("cloudyr/RoogleVision")
# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")


# library(EBImage)
library(tidyverse)
library(RoogleVision)
library(jsonlite)

library(stringr)
library(grid)
library(jpeg)

# Set google authentification
google_auth_cred <- fromJSON('google-credentials.json')
options("googleAuthR.client_id" = google_auth_cred$installed$client_id)
options("googleAuthR.client_secret" = google_auth_cred$installed$client_secret)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
googleAuthR::gar_auth()

# Load picture
pic_pulpfiction <- "image/vincent_and_jules_car.jpg"
pic_ekman <- "image/Ekman_faces.jpg"
pic_faces <- "image/faces.jpg"


gca_result <- getGoogleVisionResponse(pic_ekman, feature = 'FACE_DETECTION')

# Create pretty functional programming solution
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
            xmin = min(x),
            xmax = max(x),
            ymin = min(y),
            ymax = max(y)
    )


emotion_df <- 
    coords %>% 
    full_join(emotions, by = "id") %>% 
    gather(emotion, value, joy:surprise) %>% 
    filter(!str_detect(value, "UNLIKELY"))


# Plot the pic and the bounding recs with emotion prediction
img <- readJPEG(pic_ekman)
mgk_info <- tibble(height = dim(img)[1], width = dim(img)[2])
g <- rasterGrob(img, interpolate = FALSE, width=unit(1,"npc"), height=unit(1,"npc"))

ggplot(emotion_df) +
    aes(xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        group = id,
        color = emotion,
        label = paste0(emotion %>% str_to_upper(), ": ", value %>% str_to_lower())) +
    geom_blank() +
    scale_x_continuous(limits=c(0,mgk_info$width)) +
    scale_y_reverse(limits=c(mgk_info$height,0)) + # Y axis reversed in rastergrobs
    annotation_custom(g, xmin = 0, xmax = mgk_info$width, ymin = 0, ymax = -mgk_info$height) +
    geom_rect(alpha = 0, size = 2) +
    geom_label(aes(x = (xmin+xmax)/2, y = ymin, group = id)) +
    theme_void() + 
    theme(legend.position = "none")






