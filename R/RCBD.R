#1.запускаем библиотеки
library(agricolae)
library(ggplot2)

#2.загружаем данные. Название датасета - RCBD_dat

#3 смотрим, что загрузили
str(RCBD_dat)

#4 назначаем независимые переменные, как факторы 
RCBD_dat$Block <- as.factor(RCBD_dat$Block)
RCBD_dat$Cultivar <- as.factor(RCBD_dat$Cultivar)

#5 проверяем, что получилось
str(RCBD_dat)

#6 проверяем - сбалансированные ли наши данные
attach(RCBD_dat)
table(Cultivar, Block)

#7 исследуем данные на графиках
ggplot(RCBD_dat) +
  aes(x = Cultivar, y = Yield) +
  geom_boxplot() + geom_jitter(width = 0.1) + theme(axis.text.x = element_text(angle = 90))

ggplot(RCBD_dat) +
  aes(x = Block, y = Yield) +
  geom_boxplot() + geom_jitter(width = 0.1) + theme(axis.text.x = element_text(angle = 90))

#8 ANOVA
model1 <- aov( Yield ~ Block + Cultivar )
summary(model1)

#9 тест на нормальность остатков
shapiro.test(residuals(model1))

#10 множественные сравнения - тест Тьюки
print(HSD.test(model1,"Cultivar"))

plot(HSD.test(model1,"Cultivar"), las = 2)

##11 множественные сравнения - LSD (наименьшая существенная разница!!)
print(LSD.test(model1,"Cultivar"))
#алтернативное написание команды (можно включить поправку Бонферони):
out <- LSD.test(model1,"Cultivar", p.adj="bonferroni", group=TRUE)
out
plot(out)

