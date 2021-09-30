# SPOTIFY SONG RECOMMERDER I ----

# Libraries ----
# devtools::install_github('charlie86/spotifyr') <- Only run if you have NOT installed previously
library(spotifyr)
library(tidyverse)

# Establish Connection With Spotify API ----
get_spotify_authorization_code(
    client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
    client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET")
)

# Get Playlist URL ----
playlist_username <- playlist_username
playlist_url      <- url

# Get Playlist Details ----
playlist_songs_raw <- get_playlist_audio_features(
    playlist_username,
    url
) 

playlist_songs_raw %>% glimpse()

# Create Numeric Values for Key Name, Mode Name and Key Mode
key_name_tbl <- playlist_songs_raw %>% 
    distinct(key_name) %>% 
    rowid_to_column() %>% 
    rename(key_name_id = rowid)

mode_name_tbl <- playlist_songs_raw %>% 
    distinct(mode_name) %>% 
    rowid_to_column() %>% 
    rename(mode_name_id = rowid)

key_mode_tbl <- playlist_songs_raw %>% 
    distinct(key_mode) %>% 
    rowid_to_column() %>% 
    rename(key_mode_id = rowid)

# Final Dataframe
playlist_songs_clean <- playlist_songs_raw %>% 
    
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

playlist_songs_clean %>% glimpse()

# Select "Reference" Song ----
ref_song <- playlist_songs_clean %>% 
    filter(artist_name == "Niqo Nuevo" & track_name == "Ocean") 

ref_song_id <- ref_song %>% pull(track_id)
    
