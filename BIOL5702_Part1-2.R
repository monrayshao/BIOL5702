### Author: Mon-Ray Shao
### Department: Topp Lab 
### Institute: Donald Danforth Plant Science Center
### Created: February 20 2019
### Last Updated: February 26 2018
### Description: BIOL5702 Part 1



# INSTALL NEEDED PACKAGES -------------------------------------------------

# install.packages("ggplot2")
# install.packages("RColorBrewer")
# install.packages("tidyr")
library(ggplot2)
library(RColorBrewer)
library(tidyr)



# BRIEF INTRO TO SOME R ---------------------------------------------------

### Some examples of R objects
### NewObjectName <- FunctionName(ObjectName)

n <- 1 # Save a single number to a new object called "n"
print(n) # prints "n"
n # prints "n"
n + 1 # Add 1 to the value of "n"
x <- c(10, 11, 12, 13, 14) # Create a vector
print(x) 
length(x) # gives number of elements in vector "x"
x + 1 # Add 1 to every element in vector "x"
x * 2 # Add 2 to every element in vector "x"
x * x # Calculates "x" times "x"
x[3] # Select the third element of vector "x"
x[c(1, 2)] # Select the first 2 elements of vector "x"
x[1:3] # Select the first 3 elements of vector "x"

# Create a data frame called "y" with the following values
y <- data.frame(Column1 = x, 
                Column2 = x^2, 
                Column3 = c("Yes", "No", "Yes", "No", "Yes"), 
                Column4 = rep("Batch1", 5))
print(y)
nrow(y) # number of rows in "y"
ncol(y) # number of columns in "y"
dim(y) # dimensions (rows and columns) in "y"
class(y$Column1) # Column 1 is numeric
class(y$Column3) # Column 3 is a factor
y$Column3 <- as.character(y$Column3) # Convert Column 3 to a character
class(y$Column3) # Column3 is now a character class

print(y)
y$Column1 # Select the column "Column1"
y[ ,1] # Select the first column
y[1, ] # Select the first row
y[1,1] # Select the cell located in column 1, row 1
y[n,n] # Select the cell located in column "n", row "n"
y[ ,"Column1"] # Select the column "Column1"
y[ ,-1] # Omit the first column

print(y)
y[which(y$Column1 > 11), ] # Select all rows in which the the value of Column1 > 110
y[which(y$Column1 >= 11 & y$Column2 < 180), ]  # Using an "&" for the AND operator
y[which(y$Column3 == "Yes"), ] # Select based on characters matching certain value
y[which(y$Column3 == "No"), ]
y[which(y$Column3 != "Yes"), ] # Select based on characters *not* matching certain value
y[grep("es", y$Column3), ] # Select based on a regular expression matching a string
y[which(y$Column1 > 11 & y$Column3 == "Yes"), ] # Select based values AND characters
y[which(y$Column1 > 11 | y$Column3 == "Yes"), ] # Using an "|" for the OR operator

subset(y, Column1 > 11) # Using the subset function for easy selection
subset(y, Column3 == "Yes")
subset(y, Column1 > 11 & Column3 == "Yes") # Combining AND operator with subset function

# IMPORT AND PLOT DATA ------------------------------------------

data <- read.csv(file.choose(), header = T) # Import CSV file

head(data) # Preview first few lines
dim(data) # Find out how many rows and columns in the table
data$Solidity # View one column
class(data$Solidity) # Check object class of "Solidity" column
class(data$Label) # Check object class of "Label" column
data$Label <- as.character(data$Label) # Convert "Label" column from factor to character
class(data$Label) # Check new class
class(data) # Check object class of data frame

data$Group <- NA # Create new column of NA values
grep("arabidopsis", data$Label) # Use grep function to search for letter matches
# Assign any row with a Label matching "arabidopsis" to have a Group value of "Arabidopsis
ar <- grep("arabidopsis", data$Label) # Find which rows have an image name containing "arabidopsis"
data[ar, ]$Group <- "Arabidopsis" # Select the Arabidopsis rows and indicate under Group column
data[grep("apple", data$Label), ]$Group <- "Apple" # Repeat for Apple
data[grep("grape", data$Label), ]$Group <- "Grape" # Repeat for Grape
data[grep("poaceae", data$Label), ]$Group <- "Poaceae" # Repeat for Poaceae
data[grep("tomato", data$Label), ]$Group <- "Tomato" # Repeat for Tomato
head(data) # View the result

arabidopsis <- data[which(data$Group == "Arabidopsis"), ] # Select Arabidopsis rows into its own new data frame
head(arabidopsis)
tail(arabidopsis)
dim(arabidopsis)

tomato <- subset(data, Group == "Tomato") # Select Tomato rows into its own new data frame, using subset
head(tomato)

print(tomato$Solidity) # Check Tomato Solidity column
mean(tomato$Solidity) # Calculate mean Tomato Solidity
?mean # Bring up help page on "mean" function
median(tomato$Solidity) # Calculate median Tomato Solidity
sd(tomato$Solidity) # Calculate standard deviation of Tomato Solidity
min(tomato$Solidity) # Calculate minimum value of Tomato Solidity
max(tomato$Solidity) # Calculate maximum value of Tomato Solidity
range(tomato$Solidity) # Calculate min and max Tomato Solidity
summary(tomato$Solidity) # Calculate summary of Tomato Solidity

### GO BACK TO SLIDES

plot(tomato$Solidity) # Use base graphics to plot Tomato Solidity, note x-axis is just the index (ordered by row)
plot(tomato$Solidity, tomato$AR) # Make scatterplot of Tomato Solidity vs AR
colnames(tomato) # Show a list of all column names in Tomato data frame
plot(tomato[ ,c(6, 4)]) # We can make Tomato scatterplot again but using column number instead of name
plot(arabidopsis[ ,c(6, 4)], col = "blue", main = "Arabidopsis") # Compare to similar Arabidopsis scatterplot

plot(data[ ,c(6, 4)]) # If you try to plot Solidity vs AR using all data, it's a mess

### GO BACK TO SLIDES

library(ggplot2)
# ggplot scatterplot of tomato Solidity vs AR
ggplot(tomato, aes(x = Solidity, y = AR)) + geom_point()
# ggplot scatterplot of all Solidity vs AR - cannot tell each Group, problem with scale
ggplot(data, aes(x = Solidity, y = AR)) + geom_point()
# ggplot of all Solidity vs AR - problem with scale
ggplot(data, aes(x = Solidity, y = AR)) + geom_point(aes(color = Group))
# ggplot of all Solidity vs AR, faceted by Group - still problem with scale
ggplot(data, aes(x = Solidity, y = AR)) + geom_point(aes(color = Group)) + facet_wrap(~Group)
# ggplot of all Solidity vs AR, faceted by Group, free scale for each facet
ggplot(data, aes(x = Solidity, y = AR)) + geom_point(aes(color = Group)) + facet_wrap(~Group, scales = "free", nrow = 1)

# For illustration purposes, create a new dummy variable called "Batch"
dataBatched <- data
dataBatched$Batch <- rep(x = c("Batch1", "Batch2"), times = nrow(dataBatched)/2)
head(dataBatched) # View the results
# ggplot of all Solidity vs AR, faceted by Group, colored by Batch
ggplot(dataBatched, aes(x = Solidity, y = AR)) + geom_point(aes(color = Batch)) + 
  facet_wrap(~Group, scales = "free", nrow = 1)
# ggplot of all Solidity vs AR, faceted by Group and Batch, colored by Group
ggplot(dataBatched, aes(x = Solidity, y = AR)) + geom_point(aes(color = Group)) +
  facet_grid(Group ~ Batch, scales = "free")
# ggplot of all Solidity vs AR, faceted by Group and Batch, colored by Batch
ggplot(dataBatched, aes(x = Solidity, y = AR)) + geom_point(aes(color = Batch)) +
  facet_grid(Group ~ Batch, scales = "free")

boxplot(data$Solidity ~ data$Group) # Create boxplot of all Solidity data with base graphics
# Boxplot with ggplot
ggplot(data, aes(x = Group, y = Solidity)) + geom_boxplot()
# Jitterplot with ggplot
ggplot(data, aes(x = Group, y = Solidity)) + geom_jitter(width = 0.05)
# Overlay boxplot and jitterplot
ggplot(data, aes(x = Group, y = Solidity)) + 
  geom_boxplot(outlier.colour = NA) + 
  geom_jitter(width = 0.1, alpha = 0.1)

### GO BACK TO SLIDES

library(tidyr)
# Two ways to convert wide data to long data using tidyr package
data2 <- gather(data, key = "Trait", value = "value", Circ., AR, Round, Solidity)
data2 <- gather(data, key = "Trait", value = "value", -Group, -Label, -X) # Same thing
head(data2)
tail(data2)
dim(data2)

# Use aggregate function to make calculates by some grouping in the table
aggregate(value ~ Trait, data2, FUN = mean) # Mean by Trait
aggregate(value ~ Group + Trait, data2, FUN = mean) # Mean by Trait and Group 
aggregate(value ~ Group + Trait, data2, FUN = sd) # Std Dev by Trait and Group
aggregate(value ~ Group + Trait, data2, FUN = summary) # Summary by Trait and Group

# Boxplot of all data, faceted by Trait
ggplot(subset(data2, Group != "Poaceae"), aes(x = Group, y = value)) + geom_boxplot() + 
  facet_wrap(~Trait, scales = "free", nrow = 1) 

# Same thing by with angled x-axis
ggplot(data2, aes(x = Group, y = value)) + geom_boxplot() + 
  facet_wrap(~Trait, scales = "free", nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Re-order Traits
data2$Trait <- factor(data2$Trait, c("Circ.", "AR", "Solidity", "Round"))
ggplot(data2, aes(x = Group, y = value)) + geom_boxplot() + 
  facet_wrap(~Trait, scales = "free", nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Re-order Groups
data2$Group <- factor(data2$Group, c("Arabidopsis", "Tomato", "Grape", "Apple", "Poaceae"))
ggplot(data2, aes(x = Group, y = value)) + geom_boxplot() + 
  facet_wrap(~Trait, scales = "free", nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Make a more visually interesting plot
library(RColorBrewer)
ggplot(data2, aes(x = Group, y = value)) + geom_boxplot(aes(fill = Group), width = 0.5, outlier.colour = NA) + 
  facet_wrap(~Trait, scales = "free", nrow = 1) +
  theme(axis.text.x = element_text(color = "black", angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = "black", size = 11), 
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_blank()) +
  labs(x = "", y = "Trait Value\n") +
  scale_fill_brewer(palette = "Set2") + 
  guides(fill = FALSE) 

### GO BACK TO SLIDES



# DAY 2 BEGIN -------------------------------------------------------------

# Read back in needed packages and data
library(ggplot2)
library(RColorBrewer)
library(tidyr)

data <- read.csv(file.choose(), header = T) # Import CSV file
data$Label <- as.character(data$Label) # Convert "Label" column from factor to character

data$Group <- NA # Create new column of NA values
ar <- grep("arabidopsis", data$Label) # Find which rows have an image name containing "arabidopsis"
data[ar, ]$Group <- "Arabidopsis" # Select the Arabidopsis rows and indicate under Group column
data[grep("apple", data$Label), ]$Group <- "Apple" # Repeat for Apple
data[grep("grape", data$Label), ]$Group <- "Grape" # Repeat for Grape
data[grep("poaceae", data$Label), ]$Group <- "Poaceae" # Repeat for Poaceae
data[grep("tomato", data$Label), ]$Group <- "Tomato" # Repeat for Tomato

arabidopsis <- data[which(data$Group == "Arabidopsis"), ] # Select Arabidopsis rows into its own new data frame
tomato <- subset(data, Group == "Tomato") # Select Tomato rows into its own new data frame, using subset

# DIAGNOSTICS -------------------------------------------------------------

# Make a histogram
hist(tomato$Solidity) # Tomato almost normally distributed
hist(arabidopsis$Solidity) # Arabidopsis has a long left tail

# Make a QQ Plot
qqnorm(tomato$Solidity) # Make a QQ plot of Tomato Solidity
qqline(tomato$Solidity, col = "red") # Add a line representing perfect normality

# Compare this to Arabidopsis, which is clearly non-normal
qqnorm(arabidopsis$Solidity); qqline(arabidopsis$Solidity, col = "red") 

# Perform a Kolmogorov-Smirnoff Test against a theoretical normal distribution of same mean and std dev
ks.test(tomato$Solidity, "pnorm", 
        mean = mean(tomato$Solidity), 
        sd = sd(tomato$Solidity)) # Tomato OK, with a little danger
ks.test(arabidopsis$Solidity, "pnorm", 
        mean = mean(arabidopsis$Solidity), 
        sd = sd(arabidopsis$Solidity)) # Arabidopsis clearly not normally distributed

# Multivariate normality can be assessed using the "MVN" package

### GO BACK TO SLIDES



# HOMEWORK 1 --------------------------------------------------------------

hwData <- subset(data, Group != "Arabidopsis" & Group != "Poaceae")
ggplot(hwData, aes(x = Circ., y = Round)) + geom_point(aes(color = Group)) +
  facet_wrap(~ Group) + guides(color = F)

grape <- subset(data, Group == "Grape")
par(mfrow = c(1,2))
hist(grape$AR); qqnorm(grape$AR); qqline(grape$AR)
hist(grape$Round); qqnorm(grape$Round); qqline(grape$Round)
hist(grape$Solidity); qqnorm(grape$Solidity); qqline(grape$Solidity)
hist(grape$Circ.); qqnorm(grape$Circ.); qqline(grape$Circ.)
par(mfrow = c(1,2))

### GO BACK TO SLIDES



# DIAGNOSTICS CONTINUED ---------------------------------------------------

# Simple example of a loop
for (i in 1:5){
  print(i + 1)
}
# Index of loop doesn't need to be integers
for (i in c("A", "B", "C")){
  print(i)
}

# Make a loop for Circularity histograms for all Groups
par(mfrow=c(1,5)) # Designates that there should be 1 x 5 plots together
# Run the loop
for (i in c("Apple", "Arabidopsis", "Grape", "Poaceae", "Tomato")){
  hist(subset(data, Group == i)$Circ, main = i) 
}
par(mfrow=c(1,1)) # Resets to default 1 plot per window

# Make a loop for Circularity QQ plots for all Groups
par(mfrow=c(1,5))
# Run the loop
for (i in c("Apple", "Arabidopsis", "Grape", "Poaceae", "Tomato")){
  qqnorm(subset(data, Group == i)$Circ., main = i)
  qqline(subset(data, Group == i)$Circ., col = "red") # Make QQ-line red color
}
par(mfrow=c(1,1))

# Example of two-sample statistical test
poaceae <- subset(data, Group == "Poaceae")
grape <- subset(data, Group == "Grape")
wilcox.test(poaceae$Circ., grape$Circ.)

### GO BACK TO SLIDES



# OUTLIERS ----------------------------------------------------------------

# Univariate outlier identification example
tomato2 <- subset(data, Group == "Tomato") # Let's identify any outliers for Tomato
q1 <- quantile(tomato2$Round, probs = 0.25) # Extract value corresponding to 25th percentile for Roundness
q3 <- quantile(tomato2$Round, probs = 0.75) # Extract value corresponding to 75th percentile for Roundness
iqr <- IQR(tomato2$Round) # Calculate inter-quantile range value
lowThreshold <- q1 - (1.5 * iqr) # Set lower cutoff for outliers
highTreshold <- q3 + (1.5 * iqr) # Set upper cutoff for outliers
bad <- which(tomato2$Round < lowThreshold | tomato2$Round > highTreshold) # Select Apple Circularity values beyond thresholds
tomato2[bad,] # Show the rows containing the outlier labels
tomato2[bad,]$Round # Show the outlier Circularity values in the object to be changed
tomato2[bad,]$Round <- NA # Change these values to NA
tomato2[bad,] # Now we see that it worked

# Compare the QQ plot for Grape Circularity before and after outlier removal
par(mfrow = c(1,2))
qqnorm(tomato$Round, main = "Before"); qqline(tomato$Round, col = "red") # QQ plot of original Tomato Roundness
qqnorm(tomato2$Round, main = "After"); qqline(tomato2$Round, col = "red") # QQ plot of Tomato Roundness after outlier removal
par(mfrow = c(1,1))

# Multivariate outlier identification example example
arabidopsis <- data[which(data$Group == "Arabidopsis"), ] # Subset Arabidopsis data
# Calculate Mahalanobis distance
mdistArabidopsis <- mahalanobis(arabidopsis[ ,3:6], 
                                center = colMeans(arabidopsis[ ,3:6]), 
                                cov = cov(arabidopsis[ ,3:6]))
# Mahalanobis distance is compared to against a chi-square distribution
qqplot(mdistArabidopsis^2, qchisq(ppoints(390), df = 4)) # QQ plot of squared distance against chi-square
# Simple calculation of which samples have a value beyond the desired threshold
moutArabidopsis <- which(mdistArabidopsis > qchisq(0.975, df = 4))
print(arabidopsis[moutArabidopsis,]) # View this multivariate outliers
# View new QQ plot without outliers
qqplot(mdistArabidopsis[-moutArabidopsis]^2, qchisq(ppoints(390 - length(moutArabidopsis)), df = 4))
# Although still doesn't follow a straight line, we can see we've removed most extreme values

# Run a loop to identify all multivariate outliers within each Group
dataPruned2 <- data # Create a new object to run through the multivariate outlier loop
for (i in c("Arabidopsis", "Apple", "Grape", "Poaceae", "Tomato")){ # for every Group...
  dataSubset <- subset(subset(data, Group == i)) # Select the data corresponding to that Group
  mdist <- mahalanobis(dataSubset[ ,3:6], center = colMeans(dataSubset[ ,3:6]), cov = cov(dataSubset[ ,3:6])) # Calculate Mahalanobis
  mout <- which(mdist > qchisq(0.975, df = 4)) # Identify which values are beyond the threshold for this Group
  mout_labels <- dataSubset$Label[mout] # Save the labels corresponding to those outliers
  dataPruned2[dataPruned2$Label %in% mout_labels, 3:6] <- NA # Replace the data values of the outlier with NA
  cat(i, length(mout), "\n") # Print out the total number of outliers in the Group
}
dataPruned2 <- na.omit(dataPruned2) # Remove any rows with an NA value, as these are the multivariate outliers

install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
# Save scatterplot of original data
p1 <- ggplot(data, aes(x = Solidity, y = AR)) + geom_point(aes(color = Group)) + 
  facet_wrap(~Group, scales = "free", nrow = 1) + labs(title = "Original") + guides(color = F)
# Save scatterplot of data without outliers
p2 <- ggplot(dataPruned2, aes(x = Solidity, y = AR)) + geom_point(aes(color = Group)) + 
  facet_wrap(~Group, scales = "free", nrow = 1) + labs(title = "Multivariate Outliers Removed") + guides(color = F)
grid.arrange(p1, p2) # plot both scatterplots in same window

### GO BACK TO SLIDES



# PRINCIPAL COMPONENT ANALYSIS --------------------------------------------

scaledData <- dataPruned2 # Save outlier-removed data to a new object to be scaled
# Perform scaling: zero-centers and divides by standard deviation
scaledData[ ,3:6] <- scale(scaledData[ ,3:6]) 
pcaData <- as.matrix(scaledData[ ,3:6]) # PCA function was matrix format
head(pcaData)

pcaRes <- prcomp(pcaData) # Perform PCA
summary(pcaRes) # Check proportion of variance explained by PC's
plot(pcaRes, type = "l") # Plot proportion of variance explained by each PC

names(pcaRes) # See what else prcomp generated
pcaRes$rotation[, c(1,2)] # See loadings for PC1 and PC2
biplot(pcaRes, cex = 0.75) # One way to plot PCA results

# Note that the PC values results are in the same number and order as the intput data
nrow(pcaRes$x)
nrow(scaledData)
# Extract loadings and Treatment info for ggplot
pcaGG <- data.frame(pcaRes$x, Group = scaledData$Group, Label = scaledData$Label) 
ggplot(pcaGG, aes(x = PC1, y = PC2)) + geom_point(aes(color = Group, shape = Group)) # Plot PC1 and PC2 in ggplot
ggplot(pcaGG, aes(x = PC1, y = PC3)) + geom_point(aes(color = Group, shape = Group)) # Plot PC1 and PC3 in ggplot
ggplot(pcaGG, aes(x = PC2, y = PC3)) + geom_point(aes(color = Group, shape = Group)) # Plot PC2 and PC3 in ggplot

subset(pcaGG, Group == "Poaceae" & (PC1 > 5 | PC1 < 0) & (PC2 > 2 | PC2 < 2)) # Get extreme Poaceae leaves
# Check poaceae_107.jpg vs poaceae_109.jpg

### GO BACK TO SLIDES


# LINEAR DISCRIMINANT ANALYSIS --------------------------------------------

install.packages("MASS") # Install the MASS package for LDA function
library(MASS) # Load MASS library

# For LDA, subset the data to not include Arabidopsis or Poaceae
ldaData <- subset(dataPruned2, Group != "Arabidopsis" & Group != "Poaceae")
# Scale the data. We don't re-use the previously scaled data beacuse that scaled with all Groups
ldaData[ ,3:6] <- scale(ldaData[ ,3:6])
# Perform LDA, classifying Group by the four traits, with equal prior probabilities per Group
lda1 <- lda(Group ~ Circ. + AR + Round + Solidity, ldaData, prior = c(1/3, 1/3, 1/3), CV = F)

lda1Pred <- predict(lda1) # Output and save the LDA results for each observation
class(lda1Pred) # predict() function for LDA outputs a list
names(lda1Pred) # Use names function to see what might be in the list
head(lda1Pred$x) # Preview the LD values for each observation
# Create a new data frame for plotting in ggplot
lda1GG <- data.frame(LD1 = lda1Pred$x[,1], 
                     LD2 = lda1Pred$x[,2], 
                     Treatment = ldaData$Group)
ggplot(lda1GG, aes(x = LD1, y = LD2)) + geom_point(aes(color = Treatment)) # Plot LD1 and LD2
print(lda1$scaling) # See what linear combination LD1 and LD2 are made of
# More positive or negative (basically, further away from 0) means it was more important
# Seems Roundness and Aspect Ratio are most important for LD1 (Grape vs Apple/Tomato)
# Solidity and Circularity are most important for LD1 (Apple vs Tomato)

# Make boxplots again of LDA data
library(tidyr)
ldaData2 <- gather(ldaData, key = "Trait", value = "value", Circ., AR, Round, Solidity)
ggplot(ldaData2, aes(x = Group, y = value)) +
  geom_boxplot() + facet_wrap(~Trait, scales = "free")
# Plots seems to support LDA loading results

# Perform LDA again but with cross-validation to estimate accuracy
lda2 <- lda(Group ~ Circ. + AR + Round + Solidity, ldaData, prior = c(1/3, 1/3, 1/3), CV = T)
# Use this custom function to tabulate results
summarize.class <- function(original, classify){
  class.table <- table(original, classify)
  overall <- round(sum(diag(class.table)) / sum(class.table), 4)
  list(class.table = class.table, overall.correct = overall)  
}
# Accuracy is 93.39%
summarize.class(original = ldaData$Group, classify = lda2$class)

### GO BACK TO SLIDES



# RANDOM FOREST -----------------------------------------------------------

install.packages("randomForest") # Install package for random forests
library(randomForest) # Load random forest package

# Since random forest has few data assumptions, we can use non-normal data, etc.
rfData <- dataPruned2
set.seed(100) # Set a fixed seed value so the results are reproducible (no randomness)
# Perform random forest classifying Group by the four traits
rfRes <- randomForest(Group ~ Circ. + AR + Round + Solidity, rfData)
# Uh-oh! Why the error?
class(rfData$Group)
# Random forest function thinks "character" means we wanted regression
# Change the Group class to "factor"
rfData$Group <- as.factor(rfData$Group)
rfRes <- randomForest(Group ~ Circ. + AR + Round + Solidity, rfData) # Now random forest works
print(rfRes) # Results show ~93.21% accuracy, with all 5 Groups!
varImpPlot(rfRes, sort = T, main = "Variable Importance", cex = 0.75) # Plot variable importance
# Mean Decrease Gini estimates suggest that Roundness and AR were most important, overall

### GO BACK TO SLIDES



# HIERARCHICAL CLUSTERING -------------------------------------------------

install.packages("gplots") # Install gplots package
library(gplots) # Load gplots package

# Because Poaceae variation dominates and blows out other Groups, let's omit it for visualization
clustData <- subset(dataPruned2, Group != "Poaceae")
set.seed(9) # Set a seed to get same results every time
# randomly select 40 data points for heatmap using "sample" function
roll <- sample(nrow(clustData), 40, replace = F) # sample(pool, how many, replace/no)
print(roll) # see what sample() chose
sampleData <- clustData[roll, ] # Subset only the 40 sampled observations
dim(sampleData) # Check that it worked
heatmapData <- scale(sampleData[, 3:6]) # Scale the new data (for best color results)
head(heatmapData) # No Sample information!
rownames(heatmapData) <- sampleData$Label # Add Sample names to the rownames
head(heatmapData)

# Produce heatmap
heatmap.2(scale(heatmapData), trace = "none", margins = c(6,8),
          col = colorRampPalette(brewer.pal(11, "Spectral"))(100))
# As you can see, clustering is unsupervised so the results are mixed

# We can generate a dendrogram with dist() and hclust()
distMat <- dist(heatmapData, method = "euclidean") # Generate distance matrix
clustMat <- hclust(distMat, method = "complete") # Perform hierarchical clustering
plot(clustMat) # Plot results
cutree(clustMat, k = 4) # Give back cluster identity, given 4 clusters
cutree(clustMat, h = 2) # Give back cluster identity, given height cutoff of 3

### GO BACK TO SLIDES
