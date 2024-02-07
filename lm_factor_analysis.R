# установка пакетов 
install.packages('ggplot2')
install.packages('readxl')

# загрузка пакетов
library(readxl) # для чтения Excel-таблиц
library(ggplot2)

### Загружаем данные
# можно загрузить с помощью кнопки import dataset в правом верхнем углу вкладки Environment
WT <- Wheat_density_terms
# или можно загрузить таким образом (нужно прописать путь, где загружен файл с таблицей):
# WT <- read_xlsx('Wheat_density_terms.xlsx')


#### Смотрим структуру данных
str(WT)

### Устанавливаем факторы
WT$VARIETY <- as.factor(WT$VARIETY)
WT$DENSITY <- as.factor(WT$DENSITY)
WT$TERM <- as.factor(WT$TERM)
WT$REP <- as.factor(WT$REP)
# пример как изменить базовый уровень фактора
# WT$DENSITY <- relevel(WT$DENSITY, ref = '5,5 млн')

str(WT)
# строим графики

ggplot(WT, aes(x = VARIETY, y = YIELD, fill = VARIETY)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Урожайность по сортам", x = "Сорт", y = "Урожайность")

#диаграмма рассеяния
ggplot(WT, aes(x = DENSITY, y = YIELD, color = TERM)) +
  geom_point() +
  # geom_boxplot(alpha = 0.2)+
  # geom_jitter(width = 0.2)+
  facet_wrap(~VARIETY) +
  labs(title = "Взаимосвязь между густотой посева, сроком сева и урожайностью по сортам", 
       x = "Густота посева", y = "Урожайность")+
  theme_bw()
# диаграмма рассеяния с боксплотом
ggplot(WT, aes(x = DENSITY, y = YIELD, color = TERM)) +
  geom_point(position = position_jitterdodge(seed=1, jitter.width = 0.1),
             alpha = 0.8) +
  # geom_jitter(position = position_jitterdodge(width = 0.2, height = 0))+
  geom_boxplot(alpha = 0.2, aes(fill = TERM))+
  facet_wrap(~VARIETY) +
  labs(title = "Взаимосвязь между густотой посева, сроком сева и урожайностью по сортам", 
       x = "Густота посева", y = "Урожайность")+
  theme_bw()


#Гипотеза о влиянии густоты посева
lm1 <- lm(YIELD ~ DENSITY, data = WT)
summary(lm1)

ggplot(WT, aes(x = DENSITY, y = YIELD, fill = DENSITY)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Урожайность по густоте", x = "Густота", y = "Урожайность")

#Гипотеза о влиянии срока сева
# на самостоятельную интерпретацию
lm2 <- lm(YIELD ~ TERM, data = WT)
summary(lm2)

#Гипотеза о взаимодействии густоты посева и срока сева:
lm3 <- lm(YIELD ~ DENSITY * TERM, data = WT)
summary(lm3)

#Гипотеза о сравнении сортов пшеницы
lm4 <- lm(YIELD ~ VARIETY + TERM + DENSITY + TERM:DENSITY, data = WT)
# lm4 <- lm(YIELD ~ VARIETY + TERM * DENSITY, data = WT)

summary(lm4)

#Гипотеза об адаптивности сортов к условиям выращивания:
lm5 <- lm(YIELD ~ VARIETY * TERM * DENSITY, data = WT)
summary(lm5)

###ВИЗУАЛИЗАЦИЯ

#(Boxplot): распределение урожайности для каждого сорта, срока сева или уровня густоты посева.
boxplot(YIELD ~ VARIETY, data = WT, main = "Урожайность по сортам", xlab = "Сорт", ylab = "Урожайность")
boxplot(YIELD ~ TERM, data = WT, main = "Урожайность по срокам сева", xlab = "Срок сева", ylab = "Урожайность")
boxplot(YIELD ~ DENSITY, data = WT, main = "Урожайность по густоте посева", xlab = "Густота посева", ylab = "Урожайность")

#График взаимодействия (Interaction Plot) - взаимодействие между сроком сева и густотой посева в отношении урожайности.
interaction.plot(WT$TERM, WT$DENSITY, WT$YIELD, main = "Взаимодействие срока сева и густоты посева", xlab = "Срок сева", ylab = "Урожайность", legend = TRUE)


#ДЛЯ ОТДЕЛЬНЫХ СОРТОВ
#создаем подмножество данных с помощью функции subset
Gomer_data <- subset(WT, VARIETY == 'Гомер')

#отдельные модели для каждого сорта, чтобы оценить, как срок сева и густота посева влияют на урожайность конкретного сорта.
#отдельно влияние факторов
lm_Gomer <- lm(YIELD ~ TERM + DENSITY, data = Gomer_data)
summary(lm_Gomer)

#с взаимодействием факторов
lm_Gomer_2 <- lm(YIELD ~ TERM * DENSITY, data = Gomer_data)
summary(lm_Gomer_2)

# Можно попробовать тоже самое для других сортов
# ...

