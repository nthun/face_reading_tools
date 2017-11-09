# FUNCTION: Plots a google cloud face api result using ggplot
# INPUT: emotion_df is a data frame that contains the transformed variables from get_emotions_google() and labels are created using the create_plot_labels() function, img_path is a path to the same jpeg file
# OUTPUT: A ggplot object
# EXAMPLE: plot_emotions(df, "image/Ekman_faces.jpg")
library(tidyr)
library(dplyr)
library(ggplot2)
library(jpeg)
library(png)
library(grid)
library(ggrepel)

plot_emotions_google <- function(emotion_df, img_path){
    if (!file.exists(img_path)) return("No such file in the path")

    # Plot the pic and the bounding recs with emotion prediction
    if (grepl(".jpg$|.jpeg$",img_path) == TRUE) {img <- readJPEG(img_path)}
    if (grepl(".png$",img_path) == TRUE) {img <- readPNG(img_path)}
    mgk_info <- tibble(height = dim(img)[1], width = dim(img)[2])
    g <- rasterGrob(img, interpolate = FALSE, width = unit(1,"npc"), height = unit(1, "npc"))
    
    ggplot(emotion_df) +
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
            x = (xmin + xmax)/2, y = ymin, # x and y needed for the labels
            group = id,
            label = label) +
        scale_x_continuous(limits = c(0, mgk_info$width)) +
        scale_y_reverse(limits = c(mgk_info$height, 0)) + # Y axis reversed in rastergrobs
        annotation_custom(g, xmin = 0, xmax = mgk_info$width, ymin = 0, ymax = -mgk_info$height) +
        geom_rect(alpha = 0, size = 2, color = "red") +
        geom_label_repel(direction = "y", nudge_y = 1) +
        theme_void() + 
        theme(legend.position = "none")
}