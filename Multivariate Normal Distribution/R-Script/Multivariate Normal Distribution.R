# ===== QQ-PLOT MULTIVARIATE NORMAL =====
# Location:
# D:\Audhi Aprilliant\IPB Works\Statistics Department\6th Semester\Multivariate Analysis\Discussion Session\2nd Session\Multivariate Normal

library(ggplot2)
data.mvn = read.delim2('clipboard')
ggplot(data.mvn)+
  geom_point(aes(x = di.2, y = Chi.Square))+
  labs(title = 'Quantile Plot Multivariate Normal',
       subtitle = 'Scatterplot between di^2 and Chi Square')+
  xlab('di^2')+
  ylab('Chi-Square')

# NORMALITY TEST WITH KOLMOGOROV SMIRNOV
library(nortest)
data.mvn.full = read.delim2('clipboard')
str(data.mvn.full)
# DENSITY variable
ad.test(data.mvn.full$DENSITY)
# URBAN variable
ad.test(data.mvn.full$URBAN)
# LIFEEXPF variable
ad.test(data.mvn.full$LIFEEXPF)
# LIFEEXPM variable
ad.test(data.mvn.full$LIFEEXPM)
# LITERACY variable
ad.test(data.mvn.full$LITERACY)
# BABYMORT variable
ad.test(data.mvn.full$BABYMORT)
# BABYMORT variable
ad.test(data.mvn.full$GDP.CAP)
