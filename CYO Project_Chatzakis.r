##### Choose Your Own Project: Analysis of Banking Indicators #####
 
##### Loading Libraries and Dataset #####
library(missMDA)
library(FactoMineR)
library(ggfortify)
library(cluster)
library(knitr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(readxl)

##### Loading Dataset #####

# The dataset was sourced from http://www.bnm.gov.my/index.php?ch=statistic
# It was transformed and named ´Transformed Data.xlsx´ and was submitted with the other 3 files (.r, .rmd,.pdf)
# Please adjust the following read_excel command according to where you save the ´Transformed Data.xlsx´file
indicators.raw <- read_excel("/Users/Gast/Desktop/Dataset/Transformed Data.xlsx", sheet = "Transformed Data")
indicators <- data.frame(indicators.raw)

# Obtain year-month format
indicators.raw$MonthYr <- format(as.Date(indicators.raw$Month), "%Y-%m")

# Convert "Month" to date class
indicators.raw$Month <- as.Date(indicators.raw$Month)

raw.summary.stats <- data.frame(Minimum = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, min),
                                Maximum = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, max),
                                Mean = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, mean),
                                Median = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, median),
                                StDev = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, sd),
                                Count = apply(na.omit(indicators.raw[, c(2:10, 12:15)]), 2, length))

# Impute missing values using regularized iterative PCA
indicators.imputed.adj <- imputePCA(indicators[-21, c(2:10, 12:15)],
                                    ncp = 5, scale = TRUE,
                                    method = "Regularized")

# Impute missing values for dataset including npl.r
indicators.imputed.nplr <- imputePCA(indicators[-21, 2:15],
                                    ncp = 10, scale = TRUE,
                                    method = "Regularized")

pca.ews <- PCA(indicators.imputed.adj$completeObs)

# In-built R PCA procedure
pca.ews.R <- prcomp(indicators.imputed.adj$completeObs, scale. = TRUE)

# Eigenvalues row vector
indicators.pca.ews <- data.frame(cbind(indicators.imputed.adj$completeObs, pca.ews.R$x[, 1:3]))
indicators.pca.ews.econyy <- data.frame(cbind(indicators.pca.ews, indicators.imputed.nplr$completeObs[, 10], indicators$econ.yy[-21]))
colnames(indicators.pca.ews.econyy)[17] = "npl.r"
colnames(indicators.pca.ews.econyy)[18] = "econ.yy"

# Summary statistics
imputed.summary.stats <- data.frame(Minimum = apply(indicators.pca.ews[, 1:13], 2, min),
                                    Maximum = apply(indicators.pca.ews[, 1:13], 2, max),
                                    Mean = apply(indicators.pca.ews[, 1:13], 2, mean),
                                    Median = apply(indicators.pca.ews[, 1:13], 2, median),
                                    StDev = apply(indicators.pca.ews[, 1:13], 2, sd), 
                                    Count = apply(indicators.pca.ews[, 1:13], 2, length))

# Raw dataset (incomplete)
ggplot(indicators.raw, aes(Month, npl.r)) +
  scale_x_date(date_labels = "%b-%Y") +
  geom_line(size=0.8, color = "navy") +
  geom_point(size=0.7) +
  xlab("Date") +
  ylab("NPL Ratio") +
  scale_x_date(breaks = "24 months", date_labels = "%b %y") +
  theme_light()

# Dataset completed with imputed values
ggplot(indicators.pca.ews.econyy, aes(indicators.raw$Month[-21], npl.r)) +
  scale_x_date(date_labels = "%b-%Y") +
  geom_line(size=0.8,color = "navy") +
  geom_point(size=0.7) +
  xlab("Date") +
  ylab("NPL Ratio") +
  scale_x_date(breaks = "24 months", date_labels = "%b %y") +
  theme_light()

# Incomplete dataset pairs plot
pairs(na.omit(indicators.raw[, 5:10]), panel = panel.smooth)

# Dataset completed with imputed values pairs plot
pairs(indicators.pca.ews.econyy[, 4:9], panel = panel.smooth)

### PCA results
kable(round(pca.ews$eig, digits = 4), caption = "Table of eigenvalues")

# Screenplot
barplot(pca.ews$eig[, "eigenvalue"],
        border = TRUE, col = "blue", ylim = c(0, 5), las = 2,
        ylab = "Eigenvalue",
        names.arg = rownames(pca.ews$eig))

# Table of correlations
kable(round(pca.ews$var$coord[, 1:2], digits = 4), caption = "Correlation between variables and PCs")

# Circle of correlations
plot(pca.ews, choix = "var")

# Contribution of each variable to each principal component
kable(round(rbind(pca.ews$var$contrib, TOTAL = colSums(pca.ews$var$contrib)), digit = 4), caption = "Contributions of variables on each principal component")

# Dataset summaries
kable(round(raw.summary.stats, digits = 4), caption = "Before transformation")
kable(round(imputed.summary.stats, digits = 4), caption = "After transformation")

# Influence of each macrofinancial indicator on PC1
ggplot(as.data.frame(pca.ews$var$contrib), aes(x = row.names(as.data.frame(pca.ews$var$contrib)), y = Dim.1)) +
  geom_bar(position = "dodge", stat = "identity", fill = "dodgerblue2") +
  geom_text(aes(label = round(pca.ews$var$contrib[, 1], digits = 2), vjust = -0.3)) +
  labs(subtitle = "Note: Overall percentage of variance captured by PC1 is 31.03%") +
  xlab("Macrofinancial Indicators") +
  ylab("% contribution to PC1") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.background = element_rect(fill = "gray95", colour = "gray70"),
        panel.grid.major = element_line(colour = "gray90", size = rel(0.5)),
        panel.grid.minor = element_line(colour = "gray90", size = rel(0.25)))

# Influence of each macrofinancial indicator on PC2
ggplot(as.data.frame(pca.ews$var$contrib), aes(x = row.names(as.data.frame(pca.ews$var$contrib)), y = Dim.2)) +
  geom_bar(position = "dodge", stat = "identity", fill = "dodgerblue2") +
  geom_text(aes(label = round(pca.ews$var$contrib[, 2], digits = 2), vjust = -0.3)) +
  labs(subtitle = "Note: Overall percentage of variance captured by PC2 is 26.54%") +
  xlab("Macrofinancial Indicators") +
  ylab("% contribution to PC2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.background = element_rect(fill = "gray95", colour = "gray70"),
        panel.grid.major = element_line(colour = "gray90", size = rel(0.5)),
        panel.grid.minor = element_line(colour = "gray90", size = rel(0.25)))

# Influence of each macrofinancial indicator on PC3
ggplot(as.data.frame(pca.ews$var$contrib), aes(x = row.names(as.data.frame(pca.ews$var$contrib)), y = Dim.3)) +
  geom_bar(position = "dodge", stat = "identity", fill = "dodgerblue2") +
  geom_text(aes(label = round(pca.ews$var$contrib[, 3], digits = 2), vjust = -0.3)) +
  labs(subtitle = "Note: Overall percentage of variance captured by PC3 is 9.66%") +
  xlab("Macrofinancial Indicators") +
  ylab("% contribution to PC3") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.background = element_rect(fill = "gray95", colour = "gray70"),
        panel.grid.major = element_line(colour = "gray90", size = rel(0.5)),
        panel.grid.minor = element_line(colour = "gray90", size = rel(0.25)))

# Results

### PC explanatory performance
ggplot(indicators.pca.ews.econyy, aes(PC1, PC2, PC3, color = npl.r)) +
  geom_hline(yintercept = 0, color = "gray60") +
  geom_vline(xintercept = 0, color = "gray60") +
  geom_point() +
  scale_colour_gradient(low = "lightsteelblue", high = "darkblue") +
  labs(title = "Early Warning System PCA plot of observations", subtitle = "with percentage of variance in parentheses") +
  xlab("PC1 (31.03%)") +  # The corresponding percentage of variance for PC1
  ylab("PC2 (26.54%)") +  # The corresponding percentage of variance for PC2
  labs(color = "NPL Ratio", size = "NPL Ratio") +
  theme_light()


