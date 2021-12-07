## server.R
# load functions

source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

system1 = function (genre, algo,n){
  
  candidates = genres[genres$Genre == genre,]  
  candidates_info = merge(candidates,movies_info)
  
  if (algo == 1){
    
    candidates_sorted = candidates_info[order(-candidates_info$sum_Rating),]
    recommendation = candidates_sorted[1:n,]
  }
  
  if (algo == 2){
    
    current_year = as.integer(format(Sys.Date(), "%Y"))
    candidates_info$weight=1/(current_year-candidates_info$year)
    candidates_sorted = candidates_info[order(-candidates_info$sum_Rating*candidates_info$weight),]
    recommendation = candidates_sorted[1:n,]
  }
  recommendation$reviews=recommendation$sum_Rating/recommendation$avg_Rating
  recommendation[,c("MovieID","Title","avg_Rating","reviews")]
}

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = fread("data/movies_reshaped.csv")
# movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
# movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
# movies = data.frame(movies, stringsAsFactors = FALSE)
# colnames(movies) = c('MovieID', 'Title', 'Genres')
# movies$MovieID = as.integer(movies$MovieID)
# movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
movies=movies[order(movies$Genres1),]
movies$display = paste(movies$Genres1 , ": " , movies$Title)
#movies_reshaped <- fread("data/movies_reshaped.csv")

Genres = unique(movies$Genres1)

i=1

m=dim(movies[movies$Genres1==Genres[i],])[1]

selection = movies[movies$Genres1==Genres[i],][ceiling(m/2),]

for (i in 2:length(Genres)){
  
  m=dim(movies[movies$Genres1==Genres[i],])[1]
  
  selection = rbind(selection,movies[movies$Genres1==Genres[i],][ceiling(m/2),])
}


genres=melt(movies[,c("MovieID","Genres1","Genre2","Genre3","Genre4","Genre5","Genre6")], 
            id.vars = "MovieID" , na.rm = TRUE)

genres=genres[,c(1,3)]
colnames(genres)=c("MovieID","Genre")

myurl = "https://liangfgithub.github.io/MovieData/"


ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

 sum_rating = ratings %>% group_by(MovieID) %>%
   summarise(sum_Rating = sum(Rating))
# 
avg_rating = ratings %>% group_by(MovieID) %>%
  summarise(avg_Rating = mean(Rating))
# 
movies_stats = merge(sum_rating,avg_rating)
movies_info = merge(movies_stats,movies)


shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 3
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = selection$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(selection$display[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", selection$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 1)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)

      
       genre = movies[as.numeric(user_ratings$MovieID[1])==as.numeric(movies$MovieID),]$Genres1[1]
       res2 = system1(genre,1,10)
       res = merge(res2,movies,by="MovieID")
       
       user_results = (1:10)/10
       user_predicted_ids = 1:10
       recom_results <- data.table(Rank = 1:10, 
                                   MovieID = res$MovieID, 
                                   Title = res$display, 
                                   Predicted_rating =  user_results,
                                   url=res$image_url)

    }) # still busy
    
  }) # clicked on button
  
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_result$url[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function