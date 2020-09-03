# ===== FACTOR ANALYSIS - DATA BADA EKONOMI KREATIF 2016 =====

library(factoextra)
library(FactoMineR)

# ===== READ DATA
# 
data.ca = read.csv2(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Multivariate Analysis/Assignment/6th Assignment/Data Badan Ekonomi Kreatif.csv',
                    header = TRUE,
                    sep = ';')
View(data.ca)
colnames(data.ca)
data.ca = data.ca[,c(1,2,5,8,9,17)] # Select 5 predictor variables
# Edit First Column as Rownames of Data
data.ca.rownames = data.ca[,1]
data.ca = data.ca[,-1]
row.names(data.ca) = data.ca.rownames

# GRAPH OF CONTINGENCY TABLES
# Convert the data as a table
dt = as.table(as.matrix(data.ca))
# Graph
balloonplot(t(dt), 
            main = 'Sub-Sector of Creative Industries', 
            xlab = '',
            ylab = '',
            label = FALSE, 
            show.margins = FALSE)

# ROW PROFILES
data.ca.row = data.ca/rowSums(data.ca) # For each cell, divided by its total of rows
View(data.ca.row)
mass.row = colMeans(data.ca.row)

# COLUMN PROFILES
data.ca.col = t(data.ca)/colSums(data.ca) # For each cell, divided by its total of columns
View(data.ca.col)
mass.col = rowMeans(t(data.ca.col))

# CHIS-SQUARE TEST
# To evaluate whether there is a significant dependence between row and column categories
chisq = chisq.test(data.ca)
chisq

# CORRESPONDENCE ANALYSIS
res.ca = CA(data.ca, graph = TRUE)
print(res.ca)
fviz_ca_biplot(res.ca, repel = TRUE) # Make correspondence plot

# EIGENVALUE 
eig.val = get_eigenvalue(res.ca)
eig.val
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))

# GRAPH OF ROW VARIABLES
# Coordinates of rows
row = get_ca_row(res.ca)
row$coord
# Squared Cosine of rows
row$cos2
# Color by cos2 values: quality on the factor map
fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
# Cos2 of rows on Dim.1 and Dim.2
fviz_cos2(res.ca, choice = "row", axes = 1:2)
# Contributions of individuals to dimension 1
row$contrib
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)
# Contribution of rows in Correpondence Plot
fviz_ca_row(res.ca, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)

# GRAPH OF COLUMN VARIABLES
# Coordinates of columns
column = get_ca_col(res.ca)
column$coord
# Squared Cosine of columns
column$cos2
# Color by cos2 values: quality on the factor map
fviz_ca_col(res.ca, col.col = "cos2", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)
# Cos2 of columns on Dim.1 and Dim.2
fviz_cos2(res.ca, choice = "col", axes = 1:2)
# Contributions of columns to dimension 1
column$contrib
fviz_contrib(res.ca, choice = "col", axes = 1, top = 10)
# Contributions of columns to dimension 2
fviz_contrib(res.ca, choice = "col", axes = 2, top = 10)
# Contribution of columns in Correpondence Plot
fviz_ca_col(res.ca, col.col = "contrib", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)