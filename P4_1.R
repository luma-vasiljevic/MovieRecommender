## server.R
# load functions

source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures


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
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
rownames(movies) = movies$MovieID
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


myurl = "https://liangfgithub.github.io/MovieData/"


ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

ratingmat <- sparseMatrix(ratings$MovieID, ratings$UserID, x=ratings$Rating) # movie user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
dimnames(ratingmat) <- list(MovieID = as.character(1:max(ratings$MovieID)), UserID = as.character(sort(unique(ratings$UserID))))

shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
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
      user_ratings <- data.frame(MovieID=c("1"),Rating=c(1))
      
      user_ratings_fmt <- sparseMatrix(i = 1, 
                                       j = rep(1,nrow(user_ratings)), 
                                       x = 1, 
                                       dims = c(nrow(ratingmat), 1))
      
      rmat <- cbind(user_ratings_fmt, ratingmat)
      
      # get the indices of which cells in the matrix should be predicted
      # predict all books the current user has not yet rated
      
      items_to_predict <- which(rmat[, 1] == 0)
      prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
      
      # run the ubcf-alogrithm
      res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
      
      user_results <- sort(res[, 1], decreasing = TRUE)
      user_predicted_ids <- as.numeric(names(user_results))
      #    user_results = (1:10)/10
      #    user_predicted_ids = user_ratings$MovieID
      recom_results1 <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results,
                                  url=movies$image_url[user_predicted_ids])
      recom_results=recom_results1[!is.na(recom_results1$MovieID),][1:20,]