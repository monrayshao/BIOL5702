### Author: Mon-Ray Shao
### Department: Topp Lab 
### Institute: Donald Danforth Plant Science Center
### Created: February 20 2019
### Last Updated: February 26 2018
### Description: BIOL5702 Part 1



# INSTALL NEEDED PACKAGES -------------------------------------------------

install.packages("tidyr")
install.packages("ggplot2")
install.packages("RColorBrewer")
library(tidyr)
library(ggplot2)
library(RColorBrewer)



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

y$Column1 # Select the column "Column1"
y[ ,1] # Select the first column
y[1, ] # Select the first row
y[1,1] # Select the cell located in column 1, row 1
y[n,n] # Select the cell located in column "n", row "n"
y[ ,"Column1"] # Select the column "Column1"

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

data <- read.csv("https://raw.githubusercontent.com/monrayshao/BIOL5702/master/Results.csv") # Import CSV file

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

plot(tomato$Solidity) # Use base graphics to plot Tomato Solidity, note x-axis is just the index (ordered by row)
plot(tomato$Solidity, tomato$AR) # Make scatterplot of Tomato Solidity vs AR
colnames(tomato) # Show a list of all column names in Tomato data frame
plot(tomato[ ,c(6, 4)]) # We can make Tomato scatterplot again but using column number instead of name
plot(arabidopsis[ ,c(6, 4)], col = "blue", main = "Arabidopsis") # Compare to similar Arabidopsis scatterplot

plot(data[ ,c(6, 4)]) # If you try to plot Solidity vs AR using all data, it's a mess

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
ggplot(data, aes(x = Group, y = Solidity)) + geom_jitter(width = 0.1)
# Overlay boxplot and jitterplot
ggplot(data, aes(x = Group, y = Solidity)) + 
  geom_boxplot(outlier.colour = NA) + 
  geom_jitter(width = 0.1, alpha = 0.1)

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
ggplot(data2, aes(x = Group, y = value)) + geom_boxplot() + 
  facet_wrap(~Trait, scales = "free", nrow = 1) 

# Same thing by with angled x-axis
ggplot(data2, aes(x = Group, y = value)) + geom_boxplot() + 
  facet_wrap(~Trait, scales = "free", nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Re-order Traits
data2$Trait <- factor(data2$Trait, c("Circ.", "AR", "Round", "Solidity"))
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



# OUTLIERS ----------------------------------------------------------------

### Univariate outlier identification example
dataPruned <- data # Save original data to new object for use
dataSubset <- subset(data, Group == "Apple") # Let's identify any outliers for Apple
q1 <- quantile(dataSubset$Circ., probs = 0.25) # Extract value corresponding to first quantile for Circularity
q3 <- quantile(dataSubset$Circ., probs = 0.75) # Extract value corresponding to third quantile for Circularity
iqr <- IQR(dataSubset$Circ.) # Calculate inter-quantile range value
lowThreshold <- q1 - (1.5 * iqr) # Set lower cutoff for outliers
highTreshold <- q3 + (1.5 * iqr) # Set upper cutoff for outliers
bad <- which(dataSubset$Circ. < lowThreshold | dataSubset$Circ. > highTreshold) # Select Apple Circularity values beyond thresholds
print(dataSubset$Label[bad]) # Show the label names of the outliers
bad_labels <- dataSubset$Label[bad] # Save the label names of the outliers
dataPruned[dataPruned$Label %in% bad_labels,] # Show the rows containing the outlier labels
dataPruned[dataPruned$Label %in% bad_labels,]$Circ. # Show the outlier Circularity values in the object to be changed
dataPruned[dataPruned$Label %in% bad_labels,]$Circ. <- NA # Change these values to NA
dataPruned[dataPruned$Label %in% bad_labels,] # Now we see that it worked

### Multivariate outlier identification example example
arabidopsis <- data[which(data$Group == "Arabidopsis"), ] # Subset Arabidopsis data
# Calculate Mahalanobis distance
mdistArabidopsis <- mahalanobis(arabidopsis[ ,3:6], center = colMeans(arabidopsis[ ,3:6]), cov = cov(arabidopsis[ ,3:6]))
# Mahalanobis distance is compared to against a chi-square distribution
qqplot(mdistArabidopsis^2, qchisq(ppoints(390), df = 4)) # QQ plot of squared distance against chi-square
# Simple calculation of which samples have a value beyond the desired threshold
moutArabidopsis <- which(mdistArabidopsis > qchisq(0.975, df = 4))
print(arabidopsis[moutArabidopsis,])

# Run a loop to identify all multivariate outliers within each Group
dataPruned2 <- data # Create a new object to run through the multivariate outlier loop
for (i in c("Arabidopsis", "Apple", "Grape", "Poaceae", "Tomato")){ # for every Group...
  dataSubset <- subset(subset(data, Group == i)) # Select the data corresponding to that Group
  mdist <- mahalanobis(dataSubset[ ,3:6], center = colMeans(dataSubset[ ,3:6]), cov = cov(dataSubset[ ,3:6])) # Calculate Mahalanobis
  mout <- which(mdist > qchisq(0.975, df = 4)) # Identify which values are beyond the threshold for this Group
  if(length(mout) >= 1){ # If there's at least one outlier...
    mout_labels <- dataSubset$Label[mout] # Save the labels corresponding to those outliers
    dataPruned2[dataPruned2$Label %in% mout_labels, 3:6] <- NA # Replace the data values of the outlier with NA
    cat(i, length(mout), "\n") # Print out the total number of outliers in the Group
  }
}
dataPruned2 <- na.omit(dataPruned2) # Remove any rows with an NA value, as these are the multivariate outliers

library(gridExtra)
# Save scatterplot of original data
p1 <- ggplot(data, aes(x = Solidity, y = AR)) + geom_point(aes(color = Group)) + 
  facet_wrap(~Group, scales = "free", nrow = 1) + labs(title = "Original") + guides(color = F)
# Save scatterplot of data without outliers
p2 <- ggplot(dataPruned2, aes(x = Solidity, y = AR)) + geom_point(aes(color = Group)) + 
  facet_wrap(~Group, scales = "free", nrow = 1) + labs(title = "Multivariate Outliers Removed") + guides(color = F)
grid.arrange(p1, p2) # plot both scatterplots in same window



# PRINCIPAL COMPONENT ANALYSIS --------------------------------------------

scaledData <- dataPruned2 # Save outlier-removed data to a new object to be scaled
scaledData[ ,3:6] <- scale(scaledData[ ,3:6], scale = TRUE, center = TRUE) # Scaling; zero-centers and divides by standard deviation
pcaData <- as.matrix(scaledData[ ,3:6]) # PCA function was matrix format
head(pcaData)

pcaRes <- prcomp(pcaData) # Perform PCA
summary(pcaRes) # Check proportion of variance explained by PC's
plot(pcaRes) # Plot proportion of variance explained by each PC

pcaRes$rotation[, c(1,2)] # See loadings for PC1 and PC2
biplot(pcaRes, cex = 0.75) # One way to plot PCA results

pcaGG <- data.frame(pcaRes$x, Group = scaledData$Group, Label = scaledData$Label) # Extract loadings and Treatment info for ggplot
ggplot(pcaGG, aes(x = PC1, y = PC2)) + geom_point(aes(color = Group)) + labs(title = "PC1 and PC2") # Plot PC1 and PC2 in ggplot
ggplot(pcaGG, aes(x = PC1, y = PC3)) + geom_point(aes(color = Group)) + labs(title = "PC1 and PC3") # Plot PC1 and PC3 in ggplot
ggplot(pcaGG, aes(x = PC2, y = PC3)) + geom_point(aes(color = Group)) + labs(title = "PC2 and PC3") # Plot PC2 and PC3 in ggplot

subset(pcaGG, Group == "Poaceae" & (PC1 > 5 | PC1 < 0) & (PC2 > 2 | PC2 < 2)) # Get extreme Poaceae leaves
# Check poaceae_108.jpg and poaceae_109.jpg
