library('ROptSpace')
library('tidyverse')
library('Matrix')
library('tictoc')
#detach("package:tictoc", unload=TRUE)
#######
# usage of OptSpace function
test = diag(10)
test[1,2] = 3
test[2,4] = 5
test[4,2] = 1
input=test
input[input == 0] = NA
input
tic("sleeping") # 시작 시간 기록
factorization = OptSpace(input, ropt = 2, niter = 10, tol = 1e-06, showprogress = TRUE)
toc() # 종료 지점(시간) 기록 - 프로세스 실행 시간 출력(tic에 입력된 텍스트가 있다면 같이 출력됨)
factorization$X
factorization$S
factorization$Y
recover = factorization$X %*% factorization$S %*% t(factorization$Y)
norm(test, 'F')
norm(test-recover, 'F')
###########
folder = 'C:/Users/Owner/Desktop/SH/New beginning/SNU/9. Intern(2020S)/mcpaik/GraphUCB_code/RealData/MovieLens_1M'
mypath = paste0(folder,'/ratings.dat')
mypath
########
df_ratings = read.csv(mypath, header = FALSE, sep=':')
head(df_ratings)
########
df_ratings = select(df_ratings, V1, V3, V5)
df_ratings = rename(df_ratings, user_id = V1, movie_id = V3, rating = V5)
# R은 index가 1부터 시작
# df_ratings['user_id'] =df_ratings['user_id'] -1
# df_ratings['movie_id'] = df_ratings['movie_id'] -1
head(df_ratings)
total_data = dim(df_ratings)[1]
########
user_valid = unique(df_ratings['user_id'])
movie_valid = unique(df_ratings['movie_id'])
user_valid_vector = unlist(user_valid, use.names=FALSE)
movie_valid_vector = unlist(movie_valid, use.names=FALSE)

user_range = max(user_valid)
movie_range = max(movie_valid)

count_na_user = 0
count_na_movie = 0
total_user = 0
total_movie = 0

for(i in 1:user_range){
  if (i %in% user_valid_vector){
    total_user = total_user + 1
  }
  else{
    count_na_user = count_na_user  + 1
  }
  
}

for(i in 1:movie_range){
  if (i %in% movie_valid_vector){
    total_movie = total_movie + 1
  }
  else{
    count_na_movie = count_na_movie  + 1
  }
  
}

sprintf("user_range: %d / 총 user_id 수: %d / 없는 user_id 수: %d", user_range, total_user, count_na_user)
sprintf("movie_range: %d / 총 movie_id 수: %d / 없는 movie_id 수: %d", movie_range, total_movie, count_na_movie)
###########
df_ratings_modified = df_ratings
head(df_ratings_modified)

fff = as.factor(df_ratings_modified$movie_id)
vector_re_movie_id = as.numeric(fff)

df_ratings_modified$re_movie_id = vector_re_movie_id
head(df_ratings_modified)

# 아래의 방법도 가능하지만 factor를 쓰는게 훨씬 낫다.
# movie_valid_vector = sort(movie_valid_vector, decreasing = FALSE)
# map_to_valid_movie_numbering = rep(0, movie_range)
# for (i in 1:total_movie){
#   map_to_valid_movie_numbering[movie_valid_vector[i]]=i 
# }
# df_ratings_modified = df_ratings
# head(df_ratings_modified)
# df_ratings_modified = mutate(df_ratings_modified,
#                              re_movie_id = map_to_valid_movie_numbering[movie_id])
# head(df_ratings_modified)

sparse_M_E <- sparseMatrix(
  i = df_ratings_modified$user_id, 
  j = df_ratings_modified$re_movie_id, 
  x = df_ratings_modified$rating,
  dims = c(total_user, total_movie), 
)
sparse_E <- sparseMatrix(
  i = df_ratings_modified$user_id, 
  j = df_ratings_modified$re_movie_id, 
  x = 1,
  dims = c(total_user, total_movie), 
)

temp = as(sparse_M_E, "Matrix")
M_E = as.matrix(temp)
E = as.matrix(sparse_E)

M_E[M_E == 0] = NA
View(M_E) #179MB
View(E)
#############
# Run OptSpace : 이게 어마어마하게 걸림...
tic("sleeping") 
factorization = OptSpace(M_E, ropt = 10, niter = 50, tol = 1e-05, showprogress = TRUE)
toc()
#######
#check the results of OptSpace
recover = factorization$X %*% factorization$S %*% t(factorization$Y)
M_E[is.na(M_E) == 1] = 0
norm(M_E,'f')
norm(M_E * E, 'f')
norm(M_E - recover, 'f')
norm((M_E - recover)*E, 'f')
########
# Now, save the results of OptSpace (as .txt files)
mtx = as.matrix(factorization$X) # n x r
write.table(mtx, file = "movielens_R_optspace_X_ropt_10_niter_50_tol_1e-05.txt", sep = ",", row.names = FALSE, col.names = FALSE)
# how_to_read = read.table('movielens_R_optspace_X_ropt_10_niter_50_tol_1e-05.txt',header = FALSE, sep = ",")

mtx = as.matrix(factorization$S) # r x r : note that not diagonal
write.table(mtx, file = "movielens_R_optspace_S_ropt_10_niter_50_tol_1e-05.txt", sep = ",", row.names = FALSE, col.names = FALSE)
# how_to_read = read.table('movielens_R_optspace_S_ropt_10_niter_50_tol_1e-05.txt',header = FALSE, sep = ",")

mtx = as.matrix(factorization$Y) # m x r
write.table(mtx, file = "movielens_R_optspace_Y_ropt_10_niter_50_tol_1e-05.txt", sep = ",", row.names = FALSE, col.names = FALSE)
# how_to_read = read.table('movielens_R_optspace_Y_ropt_10_niter_50_tol_1e-05.txt',header = FALSE, sep = ",")
