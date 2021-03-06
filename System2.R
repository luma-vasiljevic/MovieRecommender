library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(dplyr)
library(ggplot2)
library(DT)
library(reshape2)
library(readr)     

source('functions/helpers.R')
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# Get movie data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

movies_reshaped <- read_csv("data/movies_reshaped.csv")

# reshape movies further
genres=melt(movies_reshaped[,c("MovieID","Genres1","Genre2","Genre3","Genre4","Genre5","Genre6")], 
            id.vars = "MovieID" , na.rm = TRUE)

genres=genres[,c(1,3)]
colnames(genres)=c("MovieID","Genre")


movies_reshaped2=movies_reshaped[,1:3]

myurl = "https://liangfgithub.github.io/MovieData/"


ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

RMSE = rep(0,10)
cnt= rep(0,10)
#
RMSE2 = rep(0,10)
#
#
elapsed=rep(0,10)

 for (k in 1:10){
#
# #splits
set.seed(1000+81*k)
dt = sort(sample(nrow(ratings), nrow(ratings)*.7))
train<-ratings[dt,]
test<-ratings[-dt,]
#
 ptm <- proc.time()
# # reshape to movies x user matrix
ratingmat <- sparseMatrix(train$MovieID, train$UserID, x=train$Rating) # movie user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
dimnames(ratingmat) <- list(MovieID = as.character(1:max(train$MovieID)), UserID = as.character(sort(unique(train$UserID))))
testratingmat <- sparseMatrix(test$MovieID, test$UserID, x=test$Rating) # movie user matrix
testratingmat <- testratingmat[, unique(summary(testratingmat)$j)] # remove users with no ratings
dimnames(testratingmat) <- list(MovieID = as.character(1:max(test$MovieID)), UserID = as.character(sort(unique(test$UserID))))
predmat = testratingmat
predmat[,] = 0

users=unique(test$UserID)

for (i in 1:length(unique(test$UserID))){
  user = as.character(users[i])
  curr_user=testratingmat[,user]
  items_to_predict <- which(curr_user > 0)
  prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
  rm=which(dimnames(ratingmat)[[2]] == user)
  ratingmat_usr = cbind(ratingmat[,user],ratingmat[,-rm])
  res <- predict_cf(ratingmat_usr, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 3000)
  res2 <- predict_cf(ratingmat_usr, prediction_indices, "ibcf", TRUE, cal_cos, 1000, FALSE, 2000, 3000)
#
  pred_user = res[items_to_predict,1]
  act = curr_user[items_to_predict]
  pred_user2 = res2[items_to_predict,1]
  RMSE[k] = RMSE[k] + sum((act-pred_user)^2)
  RMSE2[k] = RMSE[k] + sum((act-pred_user2)^2)
  cnt[k] = cnt[k]+length(items_to_predict)
}
#
RMSE2[k]=sqrt(RMSE2[k]/cnt[k])
elapsed[k] =  (proc.time() - ptm)/60
RMSE[k]=sqrt(RMSE[k]/cnt[k])
#
 print(paste(k,": elapsed:        ",elapsed[k]))
 print(paste(k,": UBCF RMSE:      ",RMSE[k]))
 print(paste(k,": IBCF RMSE:      ",RMSE2[k]))
}
#
 output=data.frame(cbind(seq(1:10), elapsed,RMSE, RMSE2,cnt))
 colnames(output)=c("Split","Run time in minutes",  "UBCF - RMSE","IBCF - RMSE", "Ratings tested")
#
# write.table(output, file = "System210SplitOtput.txt",
#             row.names = FALSE, col.names = TRUE  ,sep='\t')
