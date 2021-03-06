# SPOTIFY SONG RECOMMENDER ----
# - Building A Song Recommender System Using Euclidean Distance
# - Debugging Resources - 
# -- https://community.spotify.com/t5/Spotify-for-Developers/INVALID-CLIENT-Invalid-redirect-URI/m-p/5229058#M2802
# *** ----

# Libraries ----
library(tidyverse)
library(janitor)
library(spotifyr)

# Establish Connection With Spotify API ----
get_spotify_authorization_code(
    client_id = Sys.getenv("SPOTIFY_CLIENT_ID"), # <- Your Spotify Client ID
    client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET") # <- Your Spotify Secret
)


# Get Playlist URL ----
playlist_username  <- playlist_username  # your spotify username
playlist_uris      <- playlist_uris # your spotify playlist url


# Get Playlist Details ----
# - Note: Playlist must be set to "Public" on Spotify
audio_features_raw_tbl <- get_playlist_audio_features(
    playlist_username,
    playlist_uris
) %>% 
    clean_names()


# Save Raw Data ----
audio_features_raw_tbl %>% write_rds("../Data/audio_features_raw_tbl.rds")

audio_features_raw_tbl %>% glimpse()
audio_features_raw_tbl %>% distinct(key_name) 
audio_features_raw_tbl %>% distinct(mode_name) 
audio_features_raw_tbl %>% distinct(key_mode)

# Format Categorical Audio Features ----
# - Create Numeric Values for Key Name, Mode Name and Key Mode
key_name_tbl <- audio_features_raw_tbl %>% 
    distinct(key_name) %>% 
    arrange(key_name) %>% 
    rowid_to_column() %>% 
    rename(key_name_id = rowid)

mode_name_tbl <- audio_features_raw_tbl %>% 
    distinct(mode_name) %>% 
    arrange(mode_name) %>% 
    rowid_to_column() %>% 
    rename(mode_name_id = rowid)

key_mode_tbl <- audio_features_raw_tbl %>% 
    distinct(key_mode) %>% 
    arrange(key_mode) %>% 
    rowid_to_column() %>% 
    rename(key_mode_id = rowid)


# Select Relevant Features ----
audio_features_clean_tbl <- audio_features_raw_tbl %>% 
    
    # select needed columns
    select(track_name, track_artists, danceability:tempo, key_name:key_mode) %>% 
    
    # extract artist names
    mutate(artist_name = map_chr(track_artists, function(x) x$name[1])) %>% 
    select(-track_artists) %>% 
    select(track_name, artist_name, everything(.)) %>% 
    
    # join numeric values for key name, mode name and key mode
    left_join(key_name_tbl) %>% 
    left_join(mode_name_tbl) %>% 
    left_join(key_mode_tbl) %>% 
    
    # final edits
    select(-c(key_name, mode_name, key_mode)) %>% 
    rowid_to_column()


# Save Audio Features Clean ----
# - Save data so as not to request data from API next time I open the project
audio_features_clean_tbl %>% write_rds("../Data/audio_features_clean_tbl")

# ******************************************************************************

# Load Clean Audio Features Data ----
audio_features_clean_tbl <- read_rds("../Data/audio_features_clean_tbl")


# Select "Reference" Song ----
ref_song <- audio_features_clean_tbl %>% 
    filter(artist_name == "Niqo Nuevo" & track_name == "Ocean")

ref_song_id <- ref_song %>% pull(rowid)


# Move Reference Song to Bottom of Dataset ----
prepared_tbl <- audio_features_clean_tbl %>% 
    filter(! rowid == ref_song_id) %>% 
    rbind(ref_song) 


# Get Track Name & Track Title ----
track_artist_tbl <- prepared_tbl %>% 
    select(rowid, track_name, artist_name)


# Get Numeric Features ----
audio_features_tbl <- prepared_tbl %>% 
    select_if(is.numeric) %>% 
    select(-rowid)


# Scale Numeric Features ----
audio_features_scaled_tbl <- audio_features_tbl %>% 
    mutate_all(., ~ (scale(., center = TRUE) %>% as.vector))

# Save Scaled Features ----
# audio_features_scaled_tbl %>% write_rds("../Data/audio_features_scaled_tbl.rds")


# Calculate Euclidean Distance From Reference Song ----
distance_tbl <- as.matrix(dist(audio_features_scaled_tbl))[nrow(audio_features_scaled_tbl),] %>% 
    as_tibble()


# Save Distance Dataframe ----
# distance_tbl %>% write_rds("../Data/distance_tbl.rds")


# Combine Distance, Artist Name/Track Name & Scaled Features ----
combined_tbl <- track_artist_tbl %>% 
    bind_cols(audio_features_tbl) %>% 
    bind_cols(distance_tbl) %>% 
    #filter(! rowid == ref_song_id) %>% 
    rename(distance = value)

# Save Combined Dataframe ----
# combined_tbl %>% write_rds("../Data/combined_tbl.rds")

# Analyze Results ----
combined_tbl %>% 
    arrange(distance) %>% 
    slice(2:6)
    

# Experimentation Function ----
get_closest_songs <- function(data, ref_song_artist, ref_song_title,
                              adj_tempo = FALSE, tempo_int = 2){
    
    # get row for reference song
    ref_song_tbl <- data %>% 
        filter(artist_name == ref_song_artist & track_name == ref_song_title)
    
    # reference song id
    ref_song_id <- ref_song_tbl %>% pull(rowid)
    
    # move ref song to bottom 
    prepared_tbl <- data %>% 
        filter(rowid != ref_song_id) %>% 
        rbind(ref_song_tbl)
    
    # get artist and song dataframe
    track_artist_tbl <- prepared_tbl %>% 
        select(rowid, track_name, artist_name)
    
    # get numeric features
    audio_features_tbl <- prepared_tbl %>% 
        select_if(is.numeric) %>% 
        select(-rowid)
    
    # scale audio features
    audio_features_scaled_tbl <- audio_features_tbl %>% 
        mutate_all(., ~ (scale(., center = TRUE) %>% as.vector))
    
    # euclidean distance
    distance_tbl <- as.matrix(dist(audio_features_scaled_tbl))[
        nrow(audio_features_scaled_tbl),] %>% 
        as_tibble()
    
    # combine distance values with artist, song and unscaled audio info
    combined_tbl <- track_artist_tbl %>% 
        bind_cols(audio_features_tbl) %>% 
        bind_cols(distance_tbl) %>% 
        rename(distance = value) %>% 
        arrange(distance)
    
    # set ref song tempo range
    ref_song_tempo <- combined_tbl %>% 
        slice(1) %>% 
        pull(tempo)
    
    # tempo bounds
    tempo_lower <- ref_song_tempo - tempo_int
    tempo_upper <- ref_song_tempo + tempo_int
    
    # view results
    final_output <- combined_tbl %>% 
        arrange(distance) %>% 
        slice(2:6)
    
    if (adj_tempo){
        final_output <- combined_tbl %>% 
            filter(tempo %>% between(tempo_lower, tempo_upper)) %>% 
            arrange(distance) %>% 
            slice(2:6)
    } else {
        final_output <- final_output
    }
    
    # ref_song_artist <- combined_tbl %>% slice(1) %>% pull(artist_name)
    # ref_song_title <- combined_tbl %>% slice(1) %>% pull(track_name)
    
    # message with artist name and song title    
    message(str_glue("Reference Song: {ref_song_title} by {ref_song_artist}"))
    
    # 5 closest songs
   return(final_output)

    
}

# Version 2 ----
version2_tbl <- get_closest_songs(
    data = prepared_tbl,
    ref_song_artist = "Niqo Nuevo",
    ref_song_title = "Ocean",
    adj_tempo = TRUE
) 

version2_tbl %>% write_rds("../Artifacts/version2_tbl.rds") 


# Version 3 ----
version3_tbl <- get_closest_songs(
    data = prepared_tbl %>% select(-c(loudness, liveness, valence)),
    ref_song_artist = "Niqo Nuevo",
    ref_song_title = "Ocean",
    adj_tempo = TRUE
)

version3_tbl %>% write_rds("../Artifacts/version3_tbl.rds") 


# Version 4 ----
version4_tbl <- get_closest_songs(
    data = prepared_tbl %>% filter(! artist_name == "Kewin Cosmos"),
    ref_song_artist = "Emily Normann",
    ref_song_title = "Sans détour",
    adj_tempo = FALSE
)

version4_tbl %>% write_rds("../Artifacts/version4_tbl.rds") 


# Version 5 ----
version5_tbl <- get_closest_songs(
    data = prepared_tbl %>% filter(! artist_name == "Kewin Cosmos"),
    ref_song_artist = "2Scratch",
    ref_song_title = "FROZEN",
    adj_tempo = TRUE
)

version5_tbl %>% write_rds("../Artifacts/version5_tbl.rds") 

# ******************************************************************************
# Spotify Embed Function (For Blog Article) ----
# audio_features_raw2_tbl <- audio_features_raw_tbl %>% 
#     select(track_name, track_artists, danceability:tempo, key_name:key_mode,
#            track_id) %>% 
#     mutate(artist_name = map_chr(track_artists, function(x) x$name[1])) %>% 
#     select(-track_artists) %>% 
#     select(track_name, artist_name, everything(.))
# 
# audio_features_raw2_tbl %>% glimpse()
# audio_features_raw2_tbl %>% write_rds("../Data/audio_features_raw2_tbl.rds")
# 
# embed_song <- function(data, song_position = 2){
#     
#     concat <- data %>% 
#         mutate(concat_id = paste0(track_name, artist_name)) %>% 
#         slice(song_position) %>% 
#         pull(concat_id)
#     
#     track_id <- audio_features_raw2_tbl %>% 
#         select(track_name, artist_name, track_id) %>% 
#         mutate(concat_id = paste0(track_name, artist_name)) %>% 
#         filter(concat_id == concat) %>% 
#         pull(track_id)
#     
#     url = paste0("https://open.spotify.com/embed/track/", track_id)
#     
#     embed_url <- knitr::include_url(
#         url, height = "80"
#     )
#     
#     return(embed_url)   
# }


