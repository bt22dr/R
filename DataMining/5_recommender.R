# install.packages('recommenderlab')
library(recommenderlab)

data(MovieLense)
MovieLense

image(sample(MovieLense, 500))

r <- Recommender(MovieLense, method="UBCF")
recom <- predict(r, MovieLense[1:2], n=3)
as(recom, "list")