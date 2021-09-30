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
playlist_songs <- get_playlist_audio_features(
    playlist_username,
    url
)

playlist_songs
