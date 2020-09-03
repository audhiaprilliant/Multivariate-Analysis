# ===== DATA PREPROCESSING BIPLOT ANALYSIS - MULTIVARIATE COURSE =====

# ===== READ DATA
data.podes = read.csv("D:/Audhi Aprilliant/Personal Data/Data of Statistics Department/Pelatihan RStudio/1st Session/data.podes.csv",
                      header = TRUE,
                      sep = ',')
data.poverty = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/5th Semester/Exploration Data Analysis/Datasets/penduduk-miskin-indeks-kemiskinan-2007-2010.csv', 
                        header = TRUE, 
                        sep = ',')
View(data.podes)
str(data.podes)

# ===== AGGREGATE THE DATA POTENSI DESA
# 1 - Men Population each Province
# 2 - Women Population each Province
# 3 - Family Population each Province
colnames(data.podes)
data.podes = data.podes[,-c(1,3,4,5,6,7,8,13,14)]
colnames(data.podes) = c('Province',
                         'Men.Population',
                         'Women.Population',
                         'Family.Population',
                         'Farmer.Perc')
data.podes.mwf = data.podes[,c(1,2,3,4)]
data.podes.mwf = aggregate(data = data.podes.mwf,
                           .~Province,
                           FUN = sum)
View(data.podes.mwf)
colnames(data.podes.mwf)

# 4 - Farmer Percentages each Province
data.podes.perc = aggregate(x = data.podes$Farmer.Perc,
                            by = list(data.podes$Province),
                            FUN = mean)
colnames(data.podes.perc) = c('Province',
                              'Farmer.Perc')
# Merge Aggregate Data
data.podes.agg = merge(x = data.podes.mwf,
                       y = data.podes.perc,
                       by = 'Province',
                       all.x = TRUE) # Inner Join
View(data.podes.agg)


# ===== AGGREGATE THE DATA OF POVERTY
colnames(data.poverty)
data.poverty = data.poverty[which(data.poverty$tahun == 2010),]
data.poverty = data.poverty[,c(2,6:10)]
colnames(data.poverty) = c('Province',
                           'Total.Poverty',
                           'Perc.Poverty',
                           'P1.index',
                           'P2.Index',
                           'Poverty.Line')
# 1 - Poverty Percentages each Province
# 2 - P1 Index each Province
# 3 - P2 Index each Province
# 4 - Poverty Line each Province
data.poverty.p4 = data.poverty[,c(1,3:6)]
data.poverty.p4 = aggregate(data = data.poverty.p4,
                            .~Province,
                            FUN = mean)
# 5 - Total Poverty each Province
data.poverty.pov = aggregate(x = data.poverty$Total.Poverty,
                            by = list(data.poverty$Province),
                            FUN = sum)
colnames(data.poverty.pov) = c('Province',
                              'Total.Poverty')

# Merge Aggregate Data
data.poverty.agg = merge(x = data.poverty.p4,
                         y = data.poverty.pov,
                         by = 'Province',
                         all.x = TRUE) # Inner Join
View(data.poverty.agg)

# ===== STRING MANIPULATION
# Data Potensi Desa
data.podes.agg$Province
data.podes.agg$Province = gsub('[.]', '', data.podes.agg$Province) # Remove dots in string
                                                                   # We can use gsub('.', '', data.podes.agg$Province, fixed = TRUE)
                                                                   # Or gsub('\\.', '', data.podes.agg$Province)
data.podes.agg$Province = tolower(data.podes.agg$Province) # Make it lowercase
data.podes.agg$Province = gsub('(^|[[:space:]])([[:alpha:]])', '\\1\\U\\2', data.podes.agg$Province, perl = TRUE) #capitalize first words
data.podes.agg$Province = gsub('Kepulauan', '', data.podes.agg$Province, perl = TRUE) # Remove 'Kepulauan'
data.podes.agg$Province = gsub('Di', 'DI', data.podes.agg$Province, perl = TRUE) # Manipulation 'DI'
data.podes.agg$Province = gsub('Dki', 'DKI', data.podes.agg$Province, perl = TRUE) # Manipulation 'DKI'
data.podes.agg$Province = gsub('\\s$','', data.podes.agg$Province, perl = TRUE) # Space in the end of word
View(data.podes.agg)

# Data of Poverty
data.poverty.agg$Province
data.poverty.agg$Province = gsub('Prov. ', '', data.poverty.agg$Province, perl = TRUE) # Manipulation 'Prov.'
data.poverty.agg$Province = gsub('Kepulauan', '', data.poverty.agg$Province, perl = TRUE) # Remove 'Kepulauan'
data.poverty.agg$Province = gsub('D I', 'DI', data.poverty.agg$Province, perl = TRUE) # Manipulation 'DI'
data.poverty.agg$Province = gsub('\\s$','', data.poverty.agg$Province, perl = TRUE) # Space in the end of word
View(data.poverty.agg)

# Space in Bangka Belitung and Kalimantan Timur
# Bangka Belitung
nchar(data.podes.agg[2,1])
nchar(data.poverty.agg[15,1])
data.poverty.agg[15,1] = 'Bangka Belitung'
# Kalimantan Timur
nchar(data.podes.agg[15,1])
nchar(data.poverty.agg[14,1])
data.podes.agg[15,1] = 'Kalimantan Timur'
  
# ===== MERGE TWO DATA INTO ONE DATA
data.biplot.poverty = merge(x = data.podes.agg,
                            y = data.poverty.agg,
                            by = 'Province',
                            all.x = TRUE) # Inner Join
View(data.biplot.poverty)

# Check Inconsistency
table((data.biplot.poverty$Men.Population+data.biplot.poverty$Men.Population > data.biplot.poverty$Total.Poverty))
table( data.biplot.poverty$Family.Population > data.biplot.poverty$Total.Poverty)

# ===== SAVE THE DATA
write.csv(x = data.biplot.poverty,
          file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/6th Semester/Multivariate Analysis/Assignment/5th Assignment/Dataset/Data Social and Poverty Indonesia 2010.csv',
          row.names = FALSE)
