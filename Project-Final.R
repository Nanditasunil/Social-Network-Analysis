# Social Network Analysis

#Installing packages
install.packages("tidyverse")
install.packages("igraph")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")

#Importing libraries
library(tidyverse)
library(igraph)
library(dplyr)
library(ggplot2)
library(corrplot)

# Read Data File
data <- read.csv(file.choose(), header = TRUE)

# Network Measures
# Degree Centrality (All, In, Out)
degree_all <- degree(net, mode = 'all')
degree_in <- degree(net, mode = 'in')
degree_out <- degree(net, mode = 'out')

cat("Degree (All):", degree_all, "\n")
cat("Degree (In):", degree_in, "\n")
cat("Degree (Out):", degree_out, "\n")


# Diameter (assuming an undirected network)
diameter_undirected <- diameter(net, directed = FALSE, weights = NA)
cat("Diameter of undirected : ",diameter_undirected )

# Edge Density
edge_density <- ecount(net) / (vcount(net) * (vcount(net) - 1))
cat("Diameter of undirected : ",diameter_undirected )

# Reciprocity
reciprocity <- reciprocity(net)
cat("Reciprocity : ",reciprocity )

# Closeness Centrality
closeness_all <- closeness(net, mode = 'all', weights = NA)
cat("Closeness Centrality : ",closeness_all)

# Betweenness Centrality
betweenness_directed <- betweenness(net, directed = TRUE, weights = NA)
cat("Betweenness  : ",betweenness_directed)

# Edge Betweenness
edge_betweenness_directed <- edge_betweenness(net, directed = TRUE, weights = NA)


# View the dimensions of the original dataset (Rows and columns)
original_dimensions <- dim(data)
cat("Original Dataset Dimensions: Rows =", original_dimensions[1], "Columns =", original_dimensions[2], "\n")

# Get a summary of the original dataset
summary(data)

#View first six rows of the dataset
head(data)

# Clean the data to remove rows with missing entries
cleaned_data <- na.omit(data)

# View the dimensions of the cleaned dataset
cleaned_dimensions <- dim(cleaned_data)
cat("Cleaned Dataset Dimensions: Rows =", cleaned_dimensions[1], "Columns =", cleaned_dimensions[2], "\n")

# Get a summary of the cleaned dataset
summary(cleaned_data)


z <- data.frame(cleaned_data$followers1 , cleaned_data$followers2)
x <- data.frame(cleaned_data$first, cleaned_data$second , cleaned_data$followers1 , cleaned_data$followers2 , cleaned_data$likes1 , cleaned_data$likes2)
l <- data.frame(cleaned_data$likes1 , cleaned_data$likes2)
y <- data.frame(cleaned_data$first, cleaned_data$second)

#Minimum
min(cleaned_data$followers1)
min(cleaned_data$followers2)

min(cleaned_data$likes1)
min(cleaned_data$likes2)

#Maximum
max(cleaned_data$followers1)
max(cleaned_data$followers2)

max(cleaned_data$likes1)
max(cleaned_data$likes2)

#Sapply to return as a vector

mean_vals <- sapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], mean)
sd_vals <- sapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], sd)
median_vals <- sapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], median)
range_vals <- sapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], range)
min_vals <- sapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], min)
max_vals <- sapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], max)

#Lapply to return as list

mean_vals1 <- lapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], mean)
sd_vals1 <- lapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], sd)
median_vals1 <- lapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], median)
range_vals1 <- lapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], range)
min_vals1 <- lapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], min)
max_vals1 <- lapply(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")], max)
#``````````````````````````````````KKKKKKKKKKSSSSSSSSSSSS````````````````````````````
# Use of dplyr for data manipulation

filtered_data <- cleaned_data %>%
  filter(followers1 > 50 & followers2 > 50)

data_summary <- cleaned_data %>%
  summarise(
    across(c(followers1, followers2, likes1, likes2), list(
      mean = ~ mean(.),
      sd = ~ sd(.),
      median = ~ median(.),
      range = ~ max(.) - min(.)
    ), .names = "{.col}_{.fn}")
  )

# Split data into training and testing sets (e.g., 80% training, 20% testing)
set.seed(123)  # Set a seed for reproducibility
sample_indices <- sample(1:nrow(filtered_data), 0.8 * nrow(filtered_data))
train_data <- filtered_data[sample_indices, ]
test_data <- filtered_data[-sample_indices, ]

# Train a linear regression model for "likes1" based on "followers1"
model_likes1 <- lm(likes1 ~ followers1, data = train_data)

# Train a linear regression model for "likes2" based on "followers2"
model_likes2 <- lm(likes2 ~ followers2, data = train_data)

# Generate predictions for "likes1" based on "followers1" on the test data
predictions_likes1 <- predict(model_likes1, newdata = test_data)

# Generate predictions for "likes2" based on "followers2" on the test data
predictions_likes2 <- predict(model_likes2, newdata = test_data)

# Create a data frame with original and predicted values
prediction_results <- data.frame(
  OriginalLikes1 = test_data$likes1,
  PredictedLikes1 = predictions_likes1,
  OriginalLikes2 = test_data$likes2,
  PredictedLikes2 = predictions_likes2
)

# Print the results
print(prediction_results)

# Create plots to visualize the predictions
par(mfrow = c(2, 2))  # Create a 2x2 grid for plots

X11()

# Plot 1: Likes1 - Original vs. Predicted
plot(test_data$likes1, predictions_likes1,
     xlab = "Original Likes1", ylab = "Predicted Likes1",
     main = "Likes1 - Original vs. Predicted",
     pch = 19, col = "blue")

# Plot 2: Likes2 - Original vs. Predicted
plot(test_data$likes2, predictions_likes2,
     xlab = "Original Likes2", ylab = "Predicted Likes2",
     main = "Likes2 - Original vs. Predicted",
     pch = 19, col = "red")

# Generate predictions for "likes1" based on "followers1" on the test data
predictions_likes1 <- predict(model_likes1, newdata = test_data)

# Generate predictions for "likes2" based on "followers2" on the test data
predictions_likes2 <- predict(model_likes2, newdata = test_data)

# Calculate residuals for Likes1 and Likes2
residuals_likes1 <- test_data$likes1 - predictions_likes1
residuals_likes2 <- test_data$likes2 - predictions_likes2

# Plot 3: Residuals for Likes1
plot(predictions_likes1, residuals_likes1,
     xlab = "Predicted Likes1", ylab = "Residuals for Likes1",
     main = "Residuals for Likes1",
     pch = 19, col = "green")

# Plot 4: Residuals for Likes2
plot(predictions_likes2, residuals_likes2,
     xlab = "Predicted Likes2", ylab = "Residuals for Likes2",
     main = "Residuals for Likes2",
     pch = 19, col = "purple")

# Create Network

net <- graph.data.frame(z, directed = TRUE)
net <- graph.data.frame(x, directed = TRUE)
net <- graph.data.frame(l, directed = TRUE)
net <- graph.data.frame(y, directed = TRUE)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

substring(cleaned_data$first, 1, 4)
substring(cleaned_data$second, 1, 4)

toupper(cleaned_data$first)
toupper(cleaned_data$second)


#create histogram of dataset
ggplot(data = cleaned_data, aes(x = followers1)) +
  geom_histogram(fill = "steelblue", color = "black") +
  ggtitle("Histogram of Dataset for People in Column 1")

ggplot(data = cleaned_data, aes(x = followers2)) +
  geom_histogram(fill = "steelblue", color = "black") +
  ggtitle("Histogram of Dataset for People in Column 2")


# Count total missing values in each column
sapply(cleaned_data, function(y) sum(is.na(x)))

# Scatterplot which compares the followers and likes
ggplot(data=cleaned_data, aes(x=followers1, y=likes1)) +
  geom_point()

ggplot(data=cleaned_data, aes(x=followers2, y=likes2)) +
  geom_point()


# Calculate the correlation matrix
correlation_matrix <- cor(cleaned_data[, c("followers1", "followers2", "likes1", "likes2")])



# Visualize the correlation matrix using corrplot
corrplot(correlation_matrix, method = "color")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~NNNNNN~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Histogram of Node Degree
hist(V(net)$degree,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')





# Network Diagram
set.seed(222)
plot(net ,
     vertex.color = 'green' ,
     vertext.size = 2 ,
     edge.arrow.size = 0.05 ,
     vertex.label.cex = 0.6
)

# Highlighting degrees and layouts
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree * 1,
     edge.arrow.size = 0.2,
     layout = layout.fruchterman.reingold)

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree * 0.4,
     edge.arrow.size = 0.1,
     layout = layout.graphopt)

plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree * 1,
     edge.arrow.size = 0.3,
     layout = layout.graphopt)

# Hub and Authorities
hs <- hub_score(net)$vector
as <- authority_score(net)$vector


par(mfrow = c(1, 2))
set.seed(123)
plot(net,
     vertex.size = hs * 30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size = as * 30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
par(mfrow = c(1, 1))

# Community Detection
net <- graph.data.frame(y, directed = FALSE)

cnet <- cluster_edge_betweenness(net)

plot(cnet,
     net,
     vertex.size = 10,
     vertex.label.cex = 0.8)

