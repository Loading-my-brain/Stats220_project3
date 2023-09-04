library(tidyverse)
library(jsonlite)


# QUERIES
query1 <- "https://itunes.apple.com/search?term=jazz&limit=200&explicit=No&media=music"
query2 <- "https://itunes.apple.com/search?term=jazz&limit=200&media=music"
response <- fromJSON(query2)
initial_playlist <- response$results


# MY PLAYLIST 
my_playlist <- initial_playlist %>% 
  rename(artist = artistName,
         track_name = trackName, 
         genre = primaryGenreName,
         duration_ms = trackTimeMillis,
         img_url = artworkUrl100) %>% 
  select(artistId, artist,track_name, genre,  duration_ms, img_url) %>%
  arrange(artist) %>%
  mutate(track_name_length = nchar(track_name),
         duration_s = duration_ms / 1000 ,
         duration_min = round(duration_ms / 1000 / 60,2),
         released_year = as.numeric(substring(initial_playlist$releaseDate, 1,4))) %>% 
  filter(genre == "Jazz",
         released_year < 2010,
         artist != "Kenny G") %>%
  slice(1:20)

write_csv(my_playlist, "my_playlist.csv")




## INSPECT 
View(initial_playlist)
View(my_playlist)
glimpse(my_playlist)



#  SUMMARY TABLE
durations <- read_csv("my_playlist.csv") %>%
  mutate(song_era = ifelse(released_year < 1980, "<1980",
                           ifelse(released_year > 1979 & released_year < 2000, "1980-1999", ">1999"))) %>%
  group_by(song_era) %>%
  summarise(num_songs = n(),
            avg_duration_min = round(mean(duration_min),2),
            max_duration_min = round(max(duration_min),2),
            min_duration_min = round(min(duration_min),2),
            var_duration_min = round(var(duration_min),2),
            sd_duration_min = round(sd(duration_min),2))

durations
View(durations)



# ANIMATION GIF ----> THIS FOR ASSESSMENT
library(magick)
data <- read_csv("my_playlist.csv")

img_urls <- my_playlist$img_url

image_read(img_urls) %>%
  image_join() %>%
  image_scale(400) %>%
  image_animate(fps = 1) %>% 
  image_write("song.gif")



# ggplot  GRAPH PRESENTATION ----> NOT THIS ONE, BUT IT WORKS
durations <- read_csv("my_playlist.csv") %>%
  mutate(song_era = ifelse(released_year < 1980, "<1980",
                           ifelse(released_year > 1979 & released_year < 2000, "1980-1999", ">1999"))) %>%
  group_by(song_era) %>%
  summarise(num_songs = n(),
            avg_duration_min = mean(duration_min))


durations %>%
  ggplot(aes(x = song_era,
             y = num_songs,
             fill = avg_duration_min)) +
  geom_bar(stat = "identity") + 
  labs(title= "The average duration of song(minutes) for songs released in different time periods: <1980, 1980-2000, 1999<", 
       x = "Time periods: <1980, 1980-2000, 1999<", 
       fill = "Average song duration(min)")




# SQL

#### FIRST EXAMPLE 
```sql
SELECT artistId, artistName AS artist, trackName AS track_name,     primaryGenreName AS genre, trackTimeMillis AS duration_ms, artworkUrl100 AS img_url, CHAR_LENGTH(track_name) AS track_name_length, duration_ms / 1000 AS duration_s, ROUND(duration_ms / 1000 / 60, 2) AS duration_min, CAST(SUBSTR(releaseDate, 1, 4) AS INTEGER) AS released_year

FROM tbl_songs
WHERE genre = 'Jazz' AND released_year < 2010 AND artist != 'Kenny G'
ORDER BY artist ASC
LIMIT 20;
```

#### SECOND EXAMPLE
```sql
SELECT artistId, artistName AS artist, trackName AS track_name,  primaryGenreName AS genre, trackTimeMillis AS duration_ms, artworkUrl100 AS img_url, LEN(track_name) AS track_name_length, duration_ms / 1000 AS duration_s, ROUND(duration_ms / 1000 / 60, 2) AS duration_min, CAST(SUBSTR(releaseDate, 1, 4) AS INTEGER) AS released_year

FROM tbl_songs 
WHERE primaryGenreName LIKE '%Jazz%' AND released_year < 2010 AND NOT artistName == 'Kenny G'
ORDER BY artistName ASC
LIMIT 20;
```
  

  
  
### SCRAP ##############################################################################################
my_playlist <- initial_playlist %>% 
    rename(artist = artistName,
           track_name = trackName, 
           genre = primaryGenreName,
           duration_ms = trackTimeMillis,
           img_url = artworkUrl100) %>% 
    select(artistId, artist,track_name, genre,  duration_ms, img_url) %>%
    arrange(artist) %>%
    mutate(track_name_length = nchar(track_name),
           duration_s = duration_ms / 1000 ,
           duration_min = round(duration_ms / 1000 / 60,2)) 
           mutate(released_year = year(initital_plauylist$releaseDate)) %>% 
    filter(genre == "Jazz",
           released_year < 2010,
           artist != "Kenny G") %>%
    slice(1:20) 
  
  
  
durations <- read_csv("my_playlist.csv") %>%
             mutate(song_era = ifelse(released_year < 1980, "<1980",
                                      ifelse(released_year > 1979 & released_year < 2000, "1980-1999", "<2000")))%>%
             group_by(song_era) %>%
             summarise(num_songs = n(),
                       avg_duration_min = round(mean(duration_min),2),
                       max_duration_min = round(max(duration_min),2),
                       min_duration_min = round(min(duration_min),2),
                       var_duration_min = round(var(duration_min),2),
                       sd_duration_min = round(sd(duration_min),2))
           durations
#########################################################################################################

        
  
  
  


