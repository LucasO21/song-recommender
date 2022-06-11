# Function used to create hyperlinks for embedding Spotify song snippets in my blog article

library(tidyverse)

# Spotify Embed Function (For Blog Article) ----
embed_song <- function(data, data_raw, song_position = 2){
    
    concat <- data %>% 
        mutate(concat_id = paste0(track_name, artist_name)) %>% 
        slice(song_position) %>% 
        pull(concat_id)
    
    track_id <- data_raw %>% 
        select(track_name, artist_name, track_id) %>% 
        mutate(concat_id = paste0(track_name, artist_name)) %>% 
        filter(concat_id == concat) %>% 
        pull(track_id)
    
    url = paste0("https://open.spotify.com/embed/track/", track_id)
    
     return(url)   
}
