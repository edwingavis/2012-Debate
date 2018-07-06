library(readr)
library(textir)
library(kernlab)
library(plyr)

#loading DTM -> tf-idf matrix
debate1 <- read_csv("debate1.csv")
words_matrix <- as.matrix(debate1[,-(1:2)])
tf_idf_matrix = tfidf(words_matrix, normalize=FALSE)

#euclidean distances
euc_matrix <- as.matrix(dist(words_matrix))
#euclidean tf-idf weighted 
euc_idf_matrix <-as.matrix(dist(tf_idf_matrix))
#most/least similar w/ euclidean tf-idf
current_spot <- c()
max <- 0
for(z in 1:nrow(euc_idf_matrix)){
  for(y in 1:nrow(euc_idf_matrix)){
    if (z != y) {
      if (!is.na(euc_idf_matrix[z,y])) {
        if (euc_idf_matrix[z,y] > max) {
          current_spot <- c(z, y)
          max <- euc_idf_matrix[z,y] 
        }
      }
    }
  }
}
max <- current_spot
current_spot <- c()
min <- 1
for(z in 1:nrow(euc_idf_matrix)){
  for(y in 1:nrow(euc_idf_matrix)){
    if (z != y) {
      if (!is.na(euc_idf_matrix[z,y])) {
        if (euc_idf_matrix[z,y] < min) {
          current_spot <- c(z, y)
          min <- euc_idf_matrix[z,y] 
        }
      }
    }
  }
}
min <- current_spot

#cosine similarity
cosine<- function(x, y){
  x.norm <- x/sqrt(x%*%x)
  y.norm <- y/sqrt(y%*%y)
  rv <- x.norm%*%y.norm
  return(rv)
}
cos_sim_matrix <- matrix(NA, nrow = nrow(words_matrix), ncol = nrow(words_matrix))
for(z in 1:nrow(cos_sim_matrix)){
  for(y in 1:nrow(cos_sim_matrix)){
    cos_sim_matrix[z,y] <- cosine(words_matrix[z,], words_matrix[y,])
  }
}
#tf-idf weighted cosine
cos_idf_matrix <- matrix(NA, nrow = nrow(words_matrix), ncol = nrow(words_matrix))
for(z in 1:nrow(cos_idf_matrix)){
  for(y in 1:nrow(cos_idf_matrix)){
    cos_idf_matrix[z,y] <- cosine(tf_idf_matrix[z,], tf_idf_matrix[y,])
  }
} 
#most/least similar w/ cosine tf-idf
current_spot <- c()
max <- 0
for(z in 1:nrow(cos_idf_matrix)){
  for(y in 1:nrow(cos_idf_matrix)){
    if (z != y) {
      if (!is.na(cos_idf_matrix[z,y])) {
        if (cos_idf_matrix[z,y] > max) {
          current_spot <- c(z, y)
          max <- cos_idf_matrix[z,y] 
        }
      }
    }
  }
}
max <- current_spot

current_spot <- c()
min <- 1
for(z in 1:nrow(cos_idf_matrix)){
  for(y in 1:nrow(cos_idf_matrix)){
    if (z != y) {
      if (!is.na(cos_idf_matrix[z,y])) {
        if (cos_idf_matrix[z,y] < min) {
          current_spot <- c(z, y)
          min <- cos_idf_matrix[z,y] 
        }
      }
    }
  }
}
min <- current_spot

#normalizing matrix/Gaussian Kernel
words_n_matrix <- matrix(NA, nrow = nrow(words_matrix), ncol = ncol (words_matrix))
for(z in 1:nrow(words_n_matrix)){
  words_n_matrix[z,] <- words_matrix[z,] / sum(words_matrix[z,])
}s
kernel_n_matrix <- kernelMatrix(rbfdot(), words_n_matrix)
#Gaussian Kernel w/ tf-idf
tf_idf_n_matrix <- tfidf(words_matrix, normalize=TRUE)
kernel_idf_n_matrix <- kernelMatrix(rbfdot(), words_n_matrix)
#Most/Least Similar
current_spot <- c()
max <- 0
for(z in 1:nrow(kernel_idf_n_matrix)){
  for(y in 1:nrow(kernel_idf_n_matrix)){
    if (z != y) {
      if (!is.na(kernel_idf_n_matrix[z,y])) {
        if (kernel_idf_n_matrix[z,y] > max) {
          current_spot <- c(z, y)
          max <- kernel_idf_n_matrix[z,y] 
        }
      }
    }
  }
}
max <- current_spot
current_spot <- c()
min <- 1
for(z in 1:nrow(kernel_idf_n_matrix)){
  for(y in 1:nrow(kernel_idf_n_matrix)){
    if (z != y) {
      if (!is.na(kernel_idf_n_matrix[z,y])) {
        if (kernel_idf_n_matrix[z,y] < min) {
          current_spot <- c(z, y)
          min <- kernel_idf_n_matrix[z,y] 
        }
      }
    }
  }
}
min <- current_spot

#Comparing statements within and across candidates w/ cosine tf-idf
speakers = debate1$Speaker
obama <- c()
romney <- c()
both <- c()
obama_numbers <- which(speakers == "OBAMA", arr.ind = TRUE)
romney_numbers <- which(speakers == "ROMNEY", arr.ind = TRUE)
for(z in 1:nrow(cos_idf_matrix)){
  for(y in 1:nrow(cos_idf_matrix)){
    if (z != y) {
      if (z %in% obama_numbers & y %in% obama_numbers) {
        obama <- c(obama, cos_idf_matrix[z,y]) 
      } else if(z %in% romney_numbers & y %in% romney_numbers) {
        romney <- c(romney, cos_idf_matrix[z,y])
      } else {
        both <- c(both, cos_idf_matrix[z,y])
      }
    }
  }
} 
obama_avg <- mean(obama, na.rm = T)
romney_avg <- mean(romney, na.rm = T)
both <- mean(both, na.rm = T)
Comparisons <- c("OBAMA", "ROMNEY", "ALL STATEMENTS" )
Average <- c(obama_avg, romney_avg, both)
data.frame(Comparisons, Average)
