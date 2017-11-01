# FUNCTION: Runs a picture through the google cloud face api, gets the result, converts to the correct format and plots it, using ggplot. We only keep labels that are not unlikely.
# INPUT: Path to a jpeg file
# OUTPUT: A ggplot object
# EXAMPLE: plot_emotions_google("image/Ekman_faces.jpg")
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(jpeg)
library(grid)

plot_emotions_google <- function(img_path){
    source("get_emotions_google.R")
    
    if (!file.exists(img_path)) return("No such file in the path")
    
    emotion_df <- get_emotions_google(img_path)
    
    # Plot the pic and the bounding recs with emotion prediction
    img <- readJPEG(img_path)
    mgk_info <- tibble(height = dim(img)[1], width = dim(img)[2])
    g <- rasterGrob(img, interpolate = FALSE, width = unit(1,"npc"), height = unit(1, "npc"))
    
    ggplot(emotion_df) +
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
            x = (xmin + xmax)/2, y = ymin, # x and y needed for the labels
            group = id,
            color = emotion,
            label = paste0(emotion %>% str_to_upper(), ": ", value %>% str_to_lower())) +
        scale_x_continuous(limits = c(0, mgk_info$width)) +
        scale_y_reverse(limits = c(mgk_info$height, 0)) + # Y axis reversed in rastergrobs
        annotation_custom(g, xmin = 0, xmax = mgk_info$width, ymin = 0, ymax = -mgk_info$height) +
        geom_rect(alpha = 0, size = 2) +
        geom_label() +
        theme_void() + 
        theme(legend.position = "none")
}