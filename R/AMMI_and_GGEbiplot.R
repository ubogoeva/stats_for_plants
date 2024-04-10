library(agridat)

data(omer.sorghum)
dat <- omer.sorghum
str(dat)

install.packages("metan")
library(metan)
library(ggplot2)

############################# factors with unique levels ####################
dat$env <- factor(dat$env, levels=unique(dat$env))
dat$gen <- factor(dat$gen, levels=unique(dat$gen))
dat$rep <- factor(dat$rep, levels=unique(dat$rep))
str(dat)

###инспекция данных###
inspect(dat, plot=TRUE, threshold = 18)
?inspect
##################### ищем выбросы ###################################
find_outliers(dat, var=yield, plots=TRUE)
find_outliers(dat, var = yield, by = env)
find_text_in_num(dat$yield)

######################### анализ данных ##################################
###################### описательные статистики ################################
desc_stat(dat)
desc_stat(dat, stats="all")
ds <- desc_stat(dat, stats="all") 
ds
View(ds)
class(ds)

######################## mean performance ##############################
####################### средние значения генотипов #############################
mg <- mean_by(dat, gen)
mg
View(mg)
######################### средние значения в локациях #######################
me <- mean_by(dat, env)
me
View(me)
########### средние значения генотипов по локациям ###########
mge <- dat %>% 
  group_by(env, gen) %>%
  desc_stat(yield, stats="mean")
mge
View(mge)
############### plotting performace across environments ################
## урожайность
pyd <- ge_plot(dat, env, gen, yield)
pyd
pyd2 <- ge_plot(dat, env, gen, yield, type=2)
pyd2
######################## лучшие генотипы в каждой локации ###########################
win <- ge_winners(dat, env, gen, resp = everything())
View(win)
####################### ранжирование генотипов #################################
ranks <- ge_winners(dat, env, gen, resp = everything(), type = "ranks")
View(ranks)

##### больше деталей по производительности
ge_details(dat, env, gen, resp = everything())

########################## fixed effect models #############################
########################### anova внутри локаций####################################
indav <- anova_ind(dat, env, gen, rep, resp = c(yield))
# anova для урожайност и
indav$yield$individual
iayd <- indav$yield$individual
View(iayd)
write_xlsx(iayd, "yieldanv.xlsx")

######################## anova для всего опыта#####################################
# pooled anova for yield
panv2 <- anova_joint(dat, env, gen, rep, yield)
pavt2 <- panv2$YLD$anova
View(pavt2)

###################### ammi models #####################################
## YLD
amod <- performs_ammi(dat, env, gen, rep, yield)
print(amod)
View(amod$yield$ANOVA)
get_model_data(amod, "ipca_pval")

########################## ammi biplots ###################################
a2 <- plot_scores(amod)
a2

a2 <- plot_scores(amod, x.lab = "Yield")
a2

b2 <- plot_scores(amod, type = 2, polygon = TRUE)
b2

c2 <- plot_scores(amod, type = 4, size.tex.gen = 2,
                  x.lab = "PC1 of E",
                  y.lab = "Nominal Yield")
c2

####### индексы стабильности на основе AMMI#####
model_indexes <- ammi_indexes(amod)
print(model_indexes)
?ammi_indexes

######каждый индекс можно рассчитывать отлельно############
################ ammi based on waas #################################
?waas
waas2 <- waas(dat, env, gen, rep, yield)
View(waas2$yield$anova)

wp2_3 <- plot_scores(waas2, type = 3)
wp2_3

####Параметрические методы
?Annicchiarico
ann <- Annicchiarico(dat, env, gen, rep, yield)
print(ann)
View(ann2$YLD$environments)

#######Непараметрические методы
# Shukla
?Shukla
shu <- Shukla(dat, env, gen, rep, yield)
shu

###посчитать все параметрические и непараметрческие индексы стабильности сразу
?ge_stats
stat_p <- ge_stats(dat, env, gen, rep, yield, verbose = TRUE, prob = 0.05)
stat_p
ranks <- get_model_data(stat_p, "ranks")
View(ranks)

##########GGE biplot##############################
######пишем модель (не указывам rep!)
mod <- gge(dat, env, gen, yield)
plot(mod)

# 1 Basic biplot
bbp2 <- plot(mod)
bbp2

#2 Mean performance vs. stability
plot(mod, type = 2)

#5
plot(mod, type = 5, sel_env = "E1")
plot(mod, type = 5, sel_env = "E6")

#6
plot(mod, type = 6)

#7
plot(mod, type = 7, sel_gen = "G08")
plot(mod, type = 7, sel_gen = "G18")

#8
plot(mod, type = 8)

#9
plot(mod, type = 9, sel_gen1 = "G08", sel_gen2 = "G06")

#10
plot(mod, type = 6)