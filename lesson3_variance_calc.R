sqrt(4)

a <- 3
var_name <- 'Hello world'
sam <- 10

library(tidyverse)
library(readxl)
# install.packages('tidyverse') # если не установлен
# комментарий
df <- readxl::read_xlsx('data/ANOVA single location EPBA6.xlsx')
df$YIELD
str(df)

# среднее и медиана
mean(df$YIELD)
median(df$YIELD)
# график
plot(df$YIELD)
# дисперсия
var(df$YIELD)
# стандартное отклонение
sd(df$YIELD)

# вычисление дисперсии 
sum((df$YIELD - mean(df$YIELD)) ^ 2) / (length(df$YIELD) - 1)
var(df$YIELD)
# вычисление стандартного отклонения
sqrt(sum((df$YIELD - mean(df$YIELD)) ^ 2) / (length(df$YIELD) - 1))
sd(df$YIELD)

# тест на нормальность
shapiro.test(df$YIELD)
