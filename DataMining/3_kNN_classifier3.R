# 참고자료 : Machine Learning for Hackers, Machine Learning in Action
# setwd('/Users/bt22dr/Documents/Programming/R/kku')

library('class')

# 숫자 이미지를 벡터 형태로 변환
img2vector <- function(filename) {
  vec <- rep(0, 1024)
  
  f <- file(filename, "rt")
  lines <- readLines(f)
  vec <- as.integer(unlist(strsplit(paste(lines, collapse=''), NULL)))
  # nchar(vec) # 1024
  close(f)
  
  return(vec)
}

# img2vector 테스트
test_vec <- img2vector('./data/digits/trainingDigits/0_0.txt') # 0
image(matrix(test_vec, ncol=32))
test_vec <- img2vector('./data/digits/trainingDigits/3_0.txt') # 3
image(matrix(test_vec, ncol=32))

# training data 생성 
hwLabels <- vector('integer')
training_file_list <- list.files('./data/digits/trainingDigits')
m <- length(training_file_list)
training_mat <- matrix(nrow=m, ncol=1024)
for(i in 1:m) {
  file_name <- training_file_list[i]
  print(file_name)
  file_str <- unlist(strsplit(file_name, split="\\."))[1] # 0_0, txt
  class_num <- as.integer(unlist(strsplit(file_str, split="_"))[1]) # 0, 0
  hwLabels <- append(hwLabels, class_num)
  training_mat[i,] <- img2vector(paste('./data/digits/trainingDigits/', file_name, sep=""))
}

test_file_list <- list.files('./data/digits/testDigits')
error_cnt <- as.integer(0)
m_test <- length(test_file_list)
for(i in 1:m_test) {
  file_name <- test_file_list[i]
  print(file_name)
  file_str <- unlist(strsplit(file_name, split="\\."))[1] # 0_0, txt
  class_num <- as.integer(unlist(strsplit(file_str, split="_"))[1]) # 0, 0
  test_vec <- img2vector(paste('./data/digits/testDigits/', file_name, sep=""))
  cl_result <- knn(training_mat, test_vec, hwLabels, k=3)
  print(sprintf("knn() result : %s, the real answer : %d", cl_result, class_num))
  print("---------------")
  if(cl_result != class_num) error_cnt <- error_cnt + 1
}
print(sprintf("the total error rate is : %f %%", error_cnt/m_test*100))
