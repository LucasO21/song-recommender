# Spotify Song Recommender

## A minimalist approach to a Spotify song recommender system

In this project I use audio features provided by Spotify to find similarities between songs in a playlist. Read the full blog article [here](https://lucasoblog.netlify.app/project/song-recommender-i/).

## Approach
I computed the  **Euclidean Distance** between a *reference* song (a particular song I like) and all other songs in the playlist. Then I sorted songs based on distance from my reference, picked the 5 closest songs and evaluated if the sounded similar to my reference song. 

## Results
Evaluating results in the project was subjective and based on how I hear music. Overall the approach did "ok" in identifying similar songs. One of the main features I look for when comparing similarity in audio features is the beat of the song, and none of the audio features provided by Spotify is indicative of the beat.

## Tools / Packages
* R
* Tidyverse
* Spotifyr

## Helpful Resources
* [Spotifyr](https://www.rcharlie.com/spotifyr/)
* [Discover Spotify's Features](https://developer.spotify.com/discover/)
