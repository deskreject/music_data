#Bachelor Thesis - Data collection

#Packages to collect data from the Spotify API

library(spotifyr)
library(dplyr)
library(purrr)
library(knitr)


#Setting of the acess token so that the API can process the requests and authorize the extraction

Sys.setenv(SPOTIFY_CLIENT_ID = 'YOUR_OWN')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'YOUR_OWN')


access_token <- get_spotify_access_token()

#Artist collection of a genere-------------

Artist_collection <- function(x = "remix", limit = 800, Client_ID, Client_Secret){
  
  x <- as.character(x)
  limit <- as.numeric(limit)
  Client_ID <- as.character(Client_ID)
  Client_Secret <- as.character(Client_Secret)
  
  Sys.setenv(SPOTIFY_CLIENT_ID = Client_ID)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = Client_Secret)
  
  data_frame <- data.frame()
  counter <- 1
  
  
  
  for (i in seq(0,  limit, 50)) { 
    
    a <- get_genre_artists(genre = x, market = NULL, limit = 50,
                           offset = i, authorization = get_spotify_access_token())
    
    if(dim(a)[1] >= 1){
      for (j in seq(dim(a)[1])) {
        data_frame[counter,1] <- unlist(a[j,]$genres)[1]
        data_frame[counter,2] <- unlist(a[j,]$genres)[2]
        data_frame[counter,3] <- unlist(a[j,]$genres)[3]
        data_frame[counter,4] <- unlist(a[j,]$genres)[4]
        data_frame[counter,5] <- unlist(a[j,]$genres)[5]
        data_frame[counter,6] <- unlist(a[j,]$genres)[6]
        data_frame[counter,7] <- a[j,]$name
        data_frame[counter,8] <- a[j,]$id
        data_frame[counter,9] <- a[j,]$popularity
        data_frame[counter,10] <- a[j,]$followers.total
        data_frame[counter,11] <- a[j,]$genre
        
        counter <- counter + 1
      }
      
    }
    
  }
  
  
  colnames(data_frame)[1:6] <- "Genere"
  colnames(data_frame)[7] <- "Artist"
  colnames(data_frame)[8] <- "Artist_ID"
  colnames(data_frame)[9] <- "Popularity_Index"
  colnames(data_frame)[10] <- "Total_Followers"
  colnames(data_frame)[11] <- "Searched_Genere"
  
  data <- data_frame
  
  
  
}

Artists <-  Artist_collection("remix", limit = 800, '2817196d6a9648cca3fcb0602e28298d','718df2ae022c41c3b012733b2046bfb1')

write.csv(Artists, file = "Artists.csv", row.names = FALSE)

Artists <- read.csv("Artists.csv")

#Album collection of Atrists---------------

Album_collection <- function(data){
  
  dataframe <- data.frame()
  
  counter <- 1
  
  
  for (j in (seq(dim(data)[1]))) {
    
    for (q in seq(0,300,50)) {
      
      
      
      a <- get_artist_albums(data$Artist_ID[j], include_groups = c("album", "single", "appears_on",
                                                                   "compilation"), market = NULL, limit = 50, offset = q,
                             authorization = get_spotify_access_token(),
                             include_meta_info = FALSE)
      
      
      if(is.null(dim(a)[1]) == FALSE){
        for (k in seq(dim(a)[1])) {
          dataframe[counter, 1] <- data[j,1]
          dataframe[counter, 2] <- data[j,2]
          dataframe[counter, 3] <- data[j,3]
          dataframe[counter, 4] <- data[j,4]
          dataframe[counter, 5] <- data[j,5]
          dataframe[counter, 6] <- data[j,6]
          dataframe[counter, 7] <- data$Artist[j]
          dataframe[counter, 8] <- data$Artist_ID[j]
          dataframe[counter, 9] <- data$Popularity_Index[j]
          dataframe[counter, 10] <- data$Total_Followers[j]
          dataframe[counter, 11] <- a$album_type[k]
          dataframe[counter, 12] <- a$id[k]
          dataframe[counter, 13] <- a$name[k]
          dataframe[counter, 14] <- a$release_date[k]
          dataframe[counter, 15] <- a$total_tracks[k]
          dataframe[counter, 16] <- a[[3]][[k]]$name[1]
          dataframe[counter, 17] <- a[[3]][[k]]$id[1]
          dataframe[counter, 18] <- a[[3]][[k]]$name[2]
          dataframe[counter, 19] <- a[[3]][[k]]$id[2]
          
          
          
          
          
          counter <- counter + 1
          
        }
        
      }
      
    }
    
  }
  
  colnames(dataframe)[1:6] <- "Genere"
  colnames(dataframe)[7] <- "Artist"
  colnames(dataframe)[8] <- "Artist_ID"
  colnames(dataframe)[9] <- "Popularity_Index"
  colnames(dataframe)[10] <- "Total_Followers"
  colnames(dataframe)[11] <- "Type"
  colnames(dataframe)[12] <- "Album_ID"
  colnames(dataframe)[13] <- "Album_Name"
  colnames(dataframe)[14] <- "Release_Date"
  colnames(dataframe)[15] <- "Total_Tracks_Amount"
  
  data <- dataframe
  
}

Albums <- Album_collection(Artists)

write.csv(Albums, "Albums.csv", row.names = FALSE)

Albums <- read.csv("Albums.csv")



#Track collection of Albums----------------

#still need to adjust function

Track_collection <- function(data){
  
  dataframe <- data.frame()
  
  counter <- 1
  
  
  for (j in (seq(dim(data)[1]))) {
    
    for (q in seq(0,300,50)) {
      
      
      a <- get_album_tracks(data$Album_ID[j], limit = 50, offset = q, market = NULL,
                            authorization = get_spotify_access_token(),
                            include_meta_info = FALSE)
      
      ### Left off here with the adjustment
      if(is.null(dim(a)[1]) == FALSE){
        for (k in seq(dim(a)[1])) {
          dataframe[counter, 1] <- data[j,1]
          dataframe[counter, 2] <- data[j,2]
          dataframe[counter, 3] <- data[j,3]
          dataframe[counter, 4] <- data[j,4]
          dataframe[counter, 5] <- data[j,5]
          dataframe[counter, 6] <- data[j,6]
          dataframe[counter, 7] <- data[j,7]
          dataframe[counter, 8] <- data[j,8]
          dataframe[counter, 9] <- data[j,9]
          dataframe[counter, 10] <- data[j,10]
          dataframe[counter, 11] <- data[j,11]
          dataframe[counter, 12] <- data[j,12]
          dataframe[counter, 13] <- data[j,13]
          dataframe[counter, 14] <- data[j,14]
          dataframe[counter, 15] <- data[j,15]
          dataframe[counter, 16] <- data[j,16]
          dataframe[counter, 17] <- data[j,17]
          dataframe[counter, 18] <- data[j,18]
          dataframe[counter, 19] <- data[j,19]
          dataframe[counter, 20] <- a$duration_ms[k]
          dataframe[counter, 21] <- a$name[k]
          dataframe[counter, 22] <- a$id[k]
          
          counter <- counter + 1
          
        }
        
      }
      
    }
    
  }
  
  data <- dataframe
  
}

Data_1 <- Albums[1:1000,]
Data_2 <- Albums[1001:2000,]
Data_3 <- Albums[2001:3000,]
Data_4 <- Albums[3001:4000,]
Data_5 <- Albums[4001:5000,]
Data_6 <- Albums[5001:6000,]
Data_7 <- Albums[6001:7000,]
Data_8 <- Albums[7001:8000,]
Data_9 <- Albums[8001:9000,]
Data_10 <- Albums[9001:9844,]

write.csv(Tracks, "Track.csv", row.names = FALSE)
Tracks <- read.csv("Track.csv")



Track <- Track_collection(Data_1)
Track_2 <- Track_collection(Data_2)
Track_3 <- Track_collection(Data_3)
Track_4 <- Track_collection(Data_4)
Track_5 <- Track_collection(Data_5)
Track_6 <- Track_collection(Data_6)
Track_7 <- Track_collection(Data_7)
Track_8 <- Track_collection(Data_8)
Track_9 <- Track_collection(Data_9)
Track_10 <- Track_collection(Data_10)

Tracks <- rbind(Track,Track_2)
Tracks <- rbind(Tracks,Track_3)
Tracks <- rbind(Tracks,Track_4)
Tracks <- rbind(Tracks,Track_5)
Tracks <- rbind(Tracks,Track_6)
Tracks <- rbind(Tracks,Track_7)
Tracks <- rbind(Tracks,Track_8)
Tracks <- rbind(Tracks,Track_9)
Tracks <- rbind(Tracks,Track_10)




colnames(Tracks)[1:6] <- "Genere"
colnames(Tracks)[7] <- "Atrist"
colnames(Tracks)[8] <- "Artist_ID"
colnames(Tracks)[9] <- "Popularity"
colnames(Tracks)[10] <- "Followers"
colnames(Tracks)[11] <- "Album_Type"
colnames(Tracks)[12] <- "Album_ID"
colnames(Tracks)[13] <- "Album_Name"
colnames(Tracks)[14] <- "Release_Date"
colnames(Tracks)[15] <- "Tracks_in_Album"
colnames(Tracks)[16] <- "Artist_1"
colnames(Tracks)[17] <- "Artist_1_ID"
colnames(Tracks)[18] <- "Artist_2"
colnames(Tracks)[19] <- "Artist_2_ID"
colnames(Tracks)[20] <- "Song_Duration"
colnames(Tracks)[21] <- "Song_name"
colnames(Tracks)[22] <- "Song_ID"


write.csv(Tracks, "Tracks.csv", row.names = FALSE)
