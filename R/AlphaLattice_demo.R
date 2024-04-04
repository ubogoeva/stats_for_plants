library(agridat)
data(burgueno.alpha)
dat <- burgueno.alpha

library(agricolae)

library(ggplot2)
ggplot(dat, aes(x = col, y = row, fill = yield)) +
  geom_tile() +
  geom_text(aes(label = gen), size = 3) +
  scale_fill_gradient(low = "blue", high = "green") +
  labs(title = 'burgueno.alpha', x = 'Column', y = 'Row') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

head(dat)
str(dat)
hist(dat$yield)

?PBIB.test

#вариант 1
modelPBIB <- with(dat, PBIB.test(block, gen, rep, yield, k=4, group=TRUE, console=TRUE))
modelPBIB$means
modelPBIB$groups	


#вариант 2
modelPBIB2 <- with(dat, PBIB.test(block, gen, rep, yield, k=4, console = TRUE, method =c("VC"),test = "lsd", alpha = 0.05, group = TRUE))
modelPBIB2$means
modelPBIB2$comparison

#без корректировки 
m1 <- lm(yield ~ 0 + gen + rep + rep:block, dat)
anova(m1)
print(LSD.test(m1,"gen"))


