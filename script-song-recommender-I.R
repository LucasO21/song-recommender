# SPOTIFY SONG RECOMMERDER I ----

# Libraries ----
# devtools::install_github('charlie86/spotifyr') <- Only run if you have NOT installed previously
library(spotifyr)
library(tidyverse)

# Establish Connection With Spotify API ----
get_spotify_authorization_code(
    client_id = Sys.getenv("SPOTIFY_CLIENT_ID"), # <- Your Spotify Client ID
    client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET") # <- Your Spotify Secret
)

# Get Playlist URL ----
playlist_username <- playlist_username # <- Your Spotify username
playlist_url      <- url # <- Your playlist url


# Get Playlist Details ----
playlist_raw <- get_playlist_audio_features(
    playlist_username,
    url
) 

# Create Numeric Values for Key Name, Mode Name and Key Mode
key_name_tbl <- playlist_raw %>% 
    distinct(key_name) %>% 
    rowid_to_column() %>% 
    rename(key_name_id = rowid)

mode_name_tbl <- playlist_raw %>% 
    distinct(mode_name) %>% 
    rowid_to_column() %>% 
    rename(mode_name_id = rowid)

key_mode_tbl <- playlist_raw %>% 
    distinct(key_mode) %>% 
    rowid_to_column() %>% 
    rename(key_mode_id = rowid)


# Final Dataframe
playlist_clean_tbl <- playlist_raw %>% 
    
    # Select Needed Columns
    select(track.name, track.artists, danceability:tempo, key_name:key_mode) %>% 
    
    # Extract Artist Name
    mutate(artist.name = map_chr(track.artists, function(x) x$name[1])) %>% 
    select(-track.artists) %>% 
    select(track.name, artist.name, everything(.)) %>% 
    
    # Add Numeric Values For Key Name, Mode Name and Key Mode
    left_join(key_name_tbl) %>% 
    left_join(mode_name_tbl) %>% 
    left_join(key_mode_tbl) %>% 
    
    # Final Edits
    select(-c(key_name, mode_name, key_mode)) %>% 
    rowid_to_column() %>% 
    rename(track_id = rowid) %>% 
    setNames(names(.) %>% str_replace_all("\\.", "_"))


# Select "Reference" Song ----
ref_song <- playlist_clean_tbl %>% 
    filter(artist_name == "Niqo Nuevo" & track_name == "Ocean") 

ref_song_id <- ref_song %>% pull(track_id)


# Move "Reference" Song to Bottom of Dataset ----
prepared_tbl <- playlist_clean_tbl %>% 
    filter(track_id != ref_song_id) %>% 
    rbind(ref_song) 

prepared_tbl %>% tail()


# Get Track Name & Track Title ----
prepared_tbl_track_name <- prepared_tbl %>% 
    select(track_id, track_name, artist_name)

# Get Numeric Features ----
prepared_tbl_numeric <- prepared_tbl %>% 
    select_if(is.numeric) %>% 
    select(-track_id)

# Scale Numeric Features ----
prepared_tbl_scaled <- prepared_tbl_numeric %>% 
    mutate_all(., ~ (scale(., center = TRUE) %>% as.vector))
    
# Calculate Euclidean Distance From Reference Song
distance_tbl <- as.matrix(dist(prepared_tbl_scaled))[nrow(prepared_tbl_scaled),] %>% 
    as_tibble()

distance_tbl %>% tail()


# Combine Distance, Artist Name/Track Name & Scaled Features ----
prepared_tbl_dist <- prepared_tbl_track_name %>% 
    bind_cols(prepared_tbl_scaled) %>% 
    bind_cols(distance_tbl) %>% 
    filter(track_id != ref_song_id) %>% 
    rename(distance = value)

prepared_tbl_dist %>% tail()


# Combine Distance, Artist Name/Track Name & Scaled Features ----
results_tbl <- prepared_tbl_track_name %>% 
    bind_cols(prepared_tbl_numeric) %>%
    bind_cols(distance_tbl) %>% 
    arrange(value)


# Analyze Results - Top 5 Closest Songs ----
results_tbl %>% arrange(value) %>% 
    filter(track_id != ref_song_id) %>% 
    head(5)
