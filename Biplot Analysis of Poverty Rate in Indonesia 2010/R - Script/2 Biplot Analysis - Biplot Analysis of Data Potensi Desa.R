# ===== PRINCIPAL COMPONENT ANALYSIS AND BIPLOT =====

library(factoextra)
library(FactoMineR)

# ===== READ THE DATA
data.biplot = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Multivariate Analysis/Assignment/5th Assignment/Dataset/Data Social and Poverty Indonesia 2010.csv',
                       header = TRUE,
                       sep = ',')
colnames(data.biplot)
data.biplot = data.biplot[,-10]
View(data.biplot)
str(data.biplot)

# Province as Rownames
data.biplot.no.province = data.biplot[,-1]
View(data.biplot.no.province)
rownames(data.biplot.no.province) = data.biplot[,1]
data.biplot = data.biplot.no.province

# PRINCIPAL COMPONENT ANALYSIS
res.pca = PCA(data.biplot, graph = FALSE)
print(res.pca)

# BIPLOT GRAPHS
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# EIGENVALUES
eig.val = get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# GRAPH OF INDIVIDUALS
ind = get_pca_ind(res.pca)
ind
# Coordinates of individual
ind$coord
# Squared Cosine of individuals
ind$cos2
# Color by cos2 values: quality on the factor map
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Cos2 of individuals on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "ind", axes = 1:2)
# Contributions of rows to dimension 1
ind$contrib
fviz_contrib(res.pca, choice = "ind", axes = 1, top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.pca, choice = "ind", axes = 2, top = 10)
# Contribution of rows in Correpondence Plot
fviz_pca_ind(res.pca, col.ind = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_contrib(res.pca, choice = "ind", axes = 1:2)

# GRAPH OF VARIABLES
var = get_pca_var(res.pca)
var
# Coordinates of variables
var$coord
# Squared Cosine of variables
var$cos2
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
# Contributions of variables to dimension 1
var$contrib
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to dimension 2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contribution of variables in Correpondence Plot
fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_contrib(res.pca, choice = "var", axes = 1:2)
