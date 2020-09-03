# ===== BIPLOT ANALYSIS - MULTIVARIATE COURSE =====

library(ggplot2)
library(ggfortify)

# ===== READ THE DATA
data.biplot = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Multivariate Analysis/Assignment/5th Assignment/Dataset/Data Social and Poverty Indonesia 2010.csv',
                       header = TRUE,
                       sep = ',')
colnames(data.biplot)
data.biplot = data.biplot[,-10]
View(data.biplot)
str(data.biplot)

# Correlation
cor(data.biplot[,-1])

# Province as Rownames
data.biplot.no.province = data.biplot[,-1]
View(data.biplot.no.province)
rownames(data.biplot.no.province) = data.biplot[,1]
data.biplot = data.biplot.no.province

# Making Graph
biplot.graph = prcomp(data.biplot.no.province, scale = TRUE)
biplot(biplot.graph)
autoplot(biplot.graph, 
         label = TRUE, 
         label.size = 3, 
         loadings = TRUE,
         loadings.label = TRUE,
         loadings.label.size = 3,
         frame = TRUE
         #,
         #xlim = c(-0.1,0.2), # Zoom in biplot
         #ylim = c(-0.1,0.2)
) +
  labs(title = 'Biplot of Data social and Poverty in Indonesia',
       subtitle = '2010',
       caption = 'Multivariate Course')+
  theme_bw()

# ===== ANALYSIS OF BIPLOT
y = svd(data.biplot)
# Matrix L, A, and U
L = diag(y$d)
A = y$v
U = y$u
# If alfa = 1
G = U
H = A%*%L

# Eigenvalues
eig = (biplot.graph$sdev)^2

# Variances in percentage
variance = eig*100/sum(eig)

# Cumulative variances
cumvar = cumsum(variance)

eig.data.poverty = data.frame(eig = eig, variance = variance,
                              cumvariance = cumvar)
head(eig.data.poverty)
