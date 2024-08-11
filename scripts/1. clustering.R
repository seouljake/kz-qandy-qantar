# set working directory
setwd("C:/Users/seoul/Dropbox/00 technical/github/kz-qandy-qantar/scripts")

# install.packages("readxl")
# library("readxl")

# Replace 'your_file.csv' with the actual file path or URL
file_path <- "../data/keyword frequencies.csv"

# Read the CSV file into a data frame
dat_raw <- read.csv(file_path,fileEncoding = "UTF-8-BOM")

# Display the first few rows of the data frame
head(dat_raw)


##############################
# Cluster Media Sources
##############################


# select data columns
dat <- as.matrix(t(dat_raw[,2:7]))
head(dat)
class(dat)

# calculate distance 
dist_matrix <- dist(dat, method = "euclidean", diag = TRUE, upper = TRUE)
dist_matrix

# heatmap
heatmap(as.matrix(dist_matrix))

# Perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hclust_result, main = "Hierarchical Clustering Dendrogram")



# Set the file path for the TIFF file
tiff_file_path <- "analysis/dendrogram.tiff"

# Open a TIFF device
# tiff(tiff_file_path, width = 2000, height = 2000, units = "px", res = 300)

# Your plot code here (again)
plot(hclust_result, main = "Hierarchical Clustering Dendrogram")

# Close the TIFF device
# dev.off()




# Set the file path for the JPEG file
jpeg_file_path <- "../analysis/dendrogram.jpeg"

# Open a JPG device
jpeg(jpeg_file_path, width = 800, height = 600, quality = 100)

# Your plot code here (again)
plot(hclust_result, main = "Hierarchical Clustering Dendrogram")

# Close the JPEG device
dev.off()



# Cut the tree to create clusters
# You can choose the number of clusters by specifying the 'k' parameter
clusters <- cutree(hclust_result, k = 2)

# Print the cluster assignments
print(clusters)


##############################
# Vertical 95% CI of Difference of Group Means 
##############################

# Cluster 1: akhorda.kz, khabar.kz, nur.kz
# Cluster 2: zakon.kz, sputnik.kz, tengrinews.kz

# add rows for
## Cluster 1 mean
## Cluster 2 mean
## Cluster 1 - Cluster 2 means


# add statistics to dat
ncol <- dim(dat)[2]
nullrow <- rep(NA,ncol)
extrarows <- data.frame(
  mean1 = nullrow,
  mean2 = nullrow,
  diff = nullrow,
  lb = nullrow,
  ub = nullrow,
  pvalue = nullrow
)
extrarows <- t(extrarows); extrarows
dat<-rbind(dat,extrarows); dat


# populate statistics

dat.orig <- dat

for(i in 1:ncol){
  
  sample1 <- dat[1:3,i]
  sample2 <- dat[4:6,i]
  ttest <- t.test(sample1,sample2)
  mean1 <- ttest$estimate[1]
  mean2 <- ttest$estimate[2]
  diff <- mean1-mean2
  lb <- ttest$conf.int[1]
  ub <- ttest$conf.int[2]
  pvalue <- ttest$p.value
  
  dat[c("mean1","mean2","diff","lb","ub","pvalue"),i] <- c(mean1,mean2,diff,lb,ub,pvalue)
  
}

# add in topic
dat_raw$topic
dat <- rbind(dat,dat_raw$topic)

# sort by pvalues, from smallest to largest
d <- t(dat[, order(dat["pvalue", ])])
d

# export data
write.csv(d, "../analysis/keyword ttests.csv")
