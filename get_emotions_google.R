# FUNCTION: Runs a picture through the google cloud face api, gets the result, converts to the correct format and returns a data frame
# INPUT: Path to a jpeg file
# OUTPUT: A tibble containing the coordinates and the classification(s) of the emotion
# EXAMPLE: get_emotions_google("image/Ekman_faces.jpg")
library(tidyr)
library(dplyr)
library(stringr)
library(RoogleVision)
source("correct_coord.R")

get_emotions_google <- function(img_path){
    if (!file.exists(img_path)) return("No such file in the path")
    
    # Make the call to the API through the wrapper
    gca_result <- getGoogleVisionResponse(img_path, feature = 'FACE_DETECTION')
    
    # Extracting emotions for all recognized faces
    emotions <-
        gca_result %>% 
        select(joy = joyLikelihood, 
               sadness = sorrowLikelihood, 
               anger = angerLikelihood, 
               surprise = surpriseLikelihood) %>% 
        mutate(id = row_number())
    
    # Extact bounding rectangle coordinates
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

    # Putting together the emotions with the coordinates. Making it tidy. Removing non-recognized emotions
        coords %>% 
            full_join(emotions, by = "id") %>% 
            gather(emotion, value, joy:surprise) %>% 
            mutate(emotion = emotion %>% str_to_upper(),
                   value = value %>% str_to_lower() %>% str_replace("_"," "))
}