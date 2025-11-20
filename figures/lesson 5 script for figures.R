####################################
### for lecture note lesson 5 EDA
####################################

data(iris)
cor(iris$Sepal.Length, iris$Sepal.Width)

# Load necessary libraries
library(ggplot2)

# Load the iris dataset
data(iris)

# Create a scatter plot of sepal length and sepal width
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(col = "dark green") +
  labs(x = "Sepal Length (cm)", y = "Petal Length (cm)") +
  theme_bw() +
  theme_classic()

# histogram
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(col = "black", fill = "lightgreen", 
                 bins = 15) +
  labs(x = "Sepal Length (cm)", y = "Count") +
  theme_bw() +
  theme_classic()

ggplot(iris, aes(y = Sepal.Length)) +
  geom_boxplot(col = "black", fill = "lightgreen", 
               width = 0.6, staplewidth = 0.5
               ) +
  xlim(-1, 1) +
  labs(y = "Sepal Length (cm)") +
  coord_flip() +
  theme_bw() +
  theme_classic()



ggplot(iris, aes(sample = Sepal.Length)) +
  stat_qq(col = "blue") +
  stat_qq_line(col = "red", lwd = 1) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_bw() +
  theme_classic()

#### covariance and correlation
cov(iris$Sepal.Length, iris$Petal.Length)
cor(iris$Sepal.Length, iris$Petal.Length)

l_model <- lm(Sepal.Length ~ 1, iris)
confint(l_model, level=0.95)

#######################################################
##### Lesson 6
#######################################################
x <- rnorm(70)
y <- 2 * x + 5 + rnorm(70, 1, 3)
df <- data.frame(x, y)
df2 <- subset(df, x > -0.5 & x < 1)
model <- lm(y ~ x, data = df)
par(mfrow = c(2, 1))
par(mar = c(3, 2.5, 2, 2))
plot(df$x, df$y, pch =19, col = "darkgreen",
     bty = "L", xaxt = "n", yaxt = "n",
     xlab = NA, ylab = NA,
     cex.lab = 1.5, cex.axis = 2,
     xlim = range(df$x),
     ylim = range(df$y), main = "Full range of x")
mtext("y", side = 2, line = 1, 
      cex = 1.5 ,las = 2, col="black")

abline(model, col = "brown", lty = 5, lwd = 1.5)
anova(model)

plot(df2$x, df2$y, pch =19, col = "darkgreen",
     bty = "L", xaxt = "n", yaxt = "n",
     xlab = "x\n", ylab = "\ny", 
     cex.lab = 1.5, cex.axis = 2, xlim = range(df$x),
     ylim = range(df$y),
     main = "Inadequate sampling of x")
points(df$x, df$y, col = "darkgreen")
model2 <- lm(y ~ x, data = df2)
segments(-0.5, mean(df2$y), 1, mean(df2$y), lty = 5,
      lwd = 1.5, col = "brown") 
anova(model2)
abline(v = -0.5, lwd = 1.5, lty = 2)
abline(v = 1, lwd = 1.5, lty = 2)
mtext("y", side = 2, line = 1, 
      cex = 1.5 ,las = 2, col="black")
