rm(list = all())
setwd("C:/Users/MY HOME/Desktop/yoga2")

## loading libraries
library(dplyr)
library(tidyr)
library(gmodels)
library(tableHTML)
options(digits = 3)
library(ggfortify)
library(factoextra)


## Reading  data
dataset = read.csv("data.csv")

####################################
## understanding the data
colnames(dataset)
str(dataset)##39 obs, 48 vars(22 pre, 22 post, 4 common)
#######################
# checking for missing values
colSums(is.na(dataset))# no missing values 
###########
##  pre_post_data  - including  gender and group variables
x <- c("pre", "PRE", 'Pre')
y <- c("post","Post", 'POST')
pre_data <- dataset %>% dplyr:: select(grep(paste(x, collapse = "|"),names(dataset)))
#pre_data$gender = dataset$Gender
pre_data$group = dataset$Group
str(pre_data)###39 obs, 23 vars
post_data <- dataset %>% dplyr:: select(grep(paste(y, collapse = "|"),names(dataset)))
#post_data$gender = dataset$Gender
post_data$group = dataset$Group
str(post_data)###39 obs, 23 vars


## separating yoga (pre, post) data and control (pre, post) data 
## total 4 data sets
yoga_pre = pre_data %>% filter(group == 1)
str(yoga_pre)## 19 obs, 23 vars
write.csv(yoga_pre,"yoga_pre.csv")
yoga_post = post_data %>% filter(group == 1)
str(yoga_post)## 19 obs, 23 vars
write.csv(yoga_post,"yoga_post.csv")

control_pre = pre_data %>% filter(group == 2)
str(control_pre)## 20 obs, 23 vars
write.csv(control_pre,"control_pre.csv")
control_post = post_data %>% filter(group == 2)
str(control_post)## 20 obs, 23 vars
write.csv(control_post,"control_post.csv")

### PCA and elbow plot 
yoga_pre.pca <- prcomp(yoga_pre[,c(1:22)], center = TRUE,scale. = TRUE)
summary(yoga_pre.pca)
plot(summary(yoga_pre.pca)$importance[2,]*100, xlab = 'PC Number', ylab = 'percentage of variances', main = 'Elbow Graph_yoga_pre')
lines(summary(yoga_pre.pca)$importance[2,]*100)
#####################
yoga_post.pca <- prcomp(yoga_post[,c(1:22)], center = TRUE,scale. = TRUE)
summary(yoga_post.pca)
plot(summary(yoga_post.pca)$importance[2,]*100, xlab = 'PC Number', ylab = 'percentage of variances', main = 'Elbow Graph_yoga_post')
lines(summary(yoga_post.pca)$importance[2,]*100)
##################
control_pre.pca <- prcomp(control_pre[,c(1:22)], center = TRUE,scale. = TRUE)
summary(control_pre.pca)
plot(summary(control_pre.pca)$importance[2,]*100, xlab = 'PC Number', ylab = 'percentage of variances', main = 'Elbow Graph_control_pre')
lines(summary(control_pre.pca)$importance[2,]*100)
#####################
control_post.pca <- prcomp(control_post[,c(1:22)], center = TRUE,scale. = TRUE)
summary(control_post.pca)
plot(summary(control_post.pca)$importance[2,]*100, xlab = 'PC Number', ylab = 'percentage of variances', main = 'Elbow Graph_control_post')
lines(summary(control_post.pca)$importance[2,]*100)

##############################################
post_data.pca <- prcomp(post_data[,c(1:22)], center = TRUE,scale. = TRUE)
summary(post_data.pca)
summary(post_data.pca)$importance[2,]
plot(summary(post_data.pca)$importance[2,]*100, xlab = 'PC Number', ylab = 'percentage of variances', main = 'Elbow Graph_Post_control_yoga')
lines(summary(post_data.pca)$importance[2,]*100)

post_data$group[post_data$group == 1]<- 'yoga'
post_data$group[post_data$group == 2]<- 'control'
post_data.pca.plot <- autoplot(post_data.pca, data = post_data, colour = 'group',scale = 0,loadings = TRUE, loadings.colour = 'blue',
                               loadings.label = TRUE, loadings.label.size = 2)
post_data.pca.plot
post_data.pca
############################################################



###Trial using covar matrix and cor matrix 
covmatrix <- cov(post_data[,1:22])
covmatrix
sum(diag(covmatrix))
##Compute the eigenvalues and corresponding eigenvectors of covar matrix

covmatrix.eigen <- eigen(covmatrix)
covmatrix.eigen

##The eigenvectors represent the principal components of covmatrix. 
##The eigenvalues of covmatrix are used to find the proportion of the total variance explained by the components.

for (s in covmatrix.eigen$values) {
  print(s / sum(covmatrix.eigen$values))
}

###The first two principal components account for 93% of the total variance. 
##A scree graph of the eigenvalues can be plotted to visualize 
###the proportion of variance explained by each subsequential eigenvalue.

plot(covmatrix.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph')
lines(covmatrix.eigen$values)


###The first two principal components account for 93% of the total variance. 
##A scree graph of the eigenvalues can be plotted to visualize 
###the proportion of variance explained by each subsequential eigenvalue.



### with cor matrix
cormatrix <- cor(post_data[,1:22])
cormatrix.eigen <- eigen(cormatrix)
cormatrix.eigen
###As with the covariance matrix, we can compute the proportion of total variance 
##explained by the eigenvalues.

for (r in cormatrix.eigen$values) {
  print(r / sum(cormatrix.eigen$values))
}
###The first two principal components account for 43% of the total variance using cormatrix
####against 93% with covar matrix. 
#######################3
















