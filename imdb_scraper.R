
# List of episodes rated
# http://www.imdb.com/title/tt0072562/epdate


# Ratings page
# http://www.imdb.com/title/tt0694449/ratings

library(rvest)
library(dplyr)
imdb_url <- "http://www.imdb.com/title/tt0072562/epdate"

iu <- imdb_url %>%
  read_html() %>%
  html_nodes(xpath="//table") %>%
  html_table()

iu <- iu[[1]]

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

colnames(iu) <- c("ep", "guests", "user_rating", "user_votes", "average_rating")
iu$average_rating2 <- substrRight(iu$average_rating, 10)
iu$average_rating2 <- gsub("/.*", "", iu$average_rating2)

iu_urls <- imdb_url %>% 
  read_html() %>%
  html_nodes(xpath="//td//a") %>%
  html_attr("href") %>%
  data.frame() %>%
  filter(!grepl("vote", .)) %>%
  filter(!grepl("amazon", .)) %>%
  filter(!grepl("dpreview", .)) %>%
  filter(!grepl("audible", .))

colnames(iu_urls) <- "link"

iu <- cbind(iu, iu_urls)
iu$ratings_link <- paste0("http://www.imdb.com", iu$link, "ratings")

#####

# //a+//table//a  

rating_url <- "http://www.imdb.com/title/tt0694432/ratings"

# Ratings

for (i in 1:nrow(iu)) {

  rating_url <- iu$ratings_link[i]
  
iu_ratings <- rating_url %>%
  read_html() %>%
  html_nodes(xpath="//table") %>%
  html_table(fill=T)

iu_ratings <- iu_ratings[[2]]
colnames(iu_ratings) <- c("type", "votes", "average")
iu_ratings <- filter(iu_ratings, type !="") %>%
  filter(!is.na(votes))

iu_ratings$ep <- iu$ep[i]
iu_ratings$guests <- iu$guests[i]
iu_ratings$user_rating <- iu$user_rating[i]
iu_ratings$user_votes <- iu$user_votes[i]
iu_ratings$avg_vote <- iu$average_rating2[i]

if (i == 1) {
  iu_all_ratings <- iu_ratings
} else {
  iu_all_ratings <- rbind(iu_all_ratings, iu_ratings)
}

Sys.sleep(3)	
print(iu$ep[i])
}

### specifics now

specific_list <- c("/ratings-male", "/ratings-female", "/ratings-age_1", "/ratings-male_age_1",
                   "/ratings-female_age_1", "/ratings-age_2", "/ratings-male_age_2", "/ratings-female_age_2",
                   "/ratings-age_3", "/ratings-male_age_3", "/ratings-female_age_3", "/ratings-age_4", 
                    "/ratings-male_age_4", "/ratings-female_age_4")

specific_list_name <- c("Males", "Females", "Aged under 18", "Males under 18", "Females under 18",
                        "Aged 18-29", "Males aged 18-29", "Females aged 18-29",
                        "Aged 30-44", "Males aged 30-44", "Females aged 30-44",
                        "Aged 45+", "Males aged 45+", "Females aged 45+")
specifics <- data.frame(specific_list, specific_list_name)

test_url <- "http://www.imdb.com/title/tt0694432/ratings-male"


for (i in 1:nrow(iu)) {
  
    for (x in 1:nrow(specifics)) {
      url1 <- gsub("/ratings", "", iu$ratings_link[i])
      rating_url <- paste0(url1, specifics$specific_list[x])
      
      iu_ratings_spec <- rating_url %>%
        read_html() %>%
        html_nodes(xpath="//table") %>%
        html_table(fill=T)
      iu_ratings_spec <- iu_ratings_spec[[1]]
      iu_ratings_spec <- iu_ratings_spec[2:nrow(iu_ratings_spec),]
      colnames(iu_ratings_spec) <- c("votes", "percent", "rating")
      
      iu_ratings_spec$category <- specifics$specific_list_name[x]
      iu_ratings_spec$ep <- iu$ep[i]
      iu_ratings_spec$guests <- iu$guests[i]
      iu_ratings_spec$user_rating <- iu$user_rating[i]
      iu_ratings_spec$user_votes <- iu$user_votes[i]
      iu_ratings_spec$avg_vote <- iu$average_rating2[i]
      
      if (x == 1) {
        iu_all_ratings_spec <- iu_ratings_spec
      } else {
        iu_all_ratings_spec <- rbind(iu_all_ratings_spec, iu_ratings_spec)
      }
    }
  
  
  if (i == 1) {
    iu_all_ratings_spec_mega <- iu_all_ratings_spec
  } else {
    iu_all_ratings_spec_mega <- rbind(iu_all_ratings_spec_mega, iu_all_ratings_spec)
  }
  
  Sys.sleep(3)	
  print(iu$ep[i])
  
}


##



for (i in 145:nrow(iu)) {
  
  for (x in 1:nrow(specifics)) {
    url1 <- gsub("/ratings", "", iu$ratings_link[i])
    rating_url <- paste0(url1, specifics$specific_list[x])
    
    iu_ratings_spec <- rating_url %>%
      read_html() %>%
      html_nodes(xpath="//table") %>%
      html_table(fill=T)
    iu_ratings_spec <- iu_ratings_spec[[1]]
    iu_ratings_spec <- iu_ratings_spec[2:nrow(iu_ratings_spec),]
    colnames(iu_ratings_spec) <- c("votes", "percent", "rating")
    
    iu_ratings_spec$category <- specifics$specific_list_name[x]
    iu_ratings_spec$ep <- iu$ep[i]
    iu_ratings_spec$guests <- iu$guests[i]
    iu_ratings_spec$user_rating <- iu$user_rating[i]
    iu_ratings_spec$user_votes <- iu$user_votes[i]
    iu_ratings_spec$avg_vote <- iu$average_rating2[i]
    
    if (x == 1) {
      iu_all_ratings_spec <- iu_ratings_spec
    } else {
      iu_all_ratings_spec <- rbind(iu_all_ratings_spec, iu_ratings_spec)
    }
  }
  
  
  if (i == 1) {
    iu_all_ratings_spec_mega <- iu_all_ratings_spec
  } else {
    iu_all_ratings_spec_mega <- rbind(iu_all_ratings_spec_mega, iu_all_ratings_spec)
  }
  
  Sys.sleep(3)	
  print(iu$ep[i])
  
}


