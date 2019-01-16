set.seed (17)

#define parameters
ncity = 5
ndistrict = 4
b0 = -1
b1 = .005
sdr = 2
sd = 1

#simulate variables and organize a balanced data
( city = rep(LETTERS[1:ncity], each = ndistrict) )
( district = letters[1:(ncity*ndistrict)] )
( fe = runif(ncity*ndistrict, 2, 75))#fixed effect,mean=2, standard variance=75
( re = rnorm(ncity, 0, sdr) )#random intercept
( re = rep(re, each = ndistrict) )
( sigma = rnorm(ncity*ndistrict, 0, sd) )#error term
( df = data.frame(city,fe,district, re, sigma) )
( houseprice = b0 + b1*fe + re + sigma )#the response

#fit the model using reml
library(lme4)
riml = lmer(houseprice ~ fe + (1|re), data = df, REML = FALSE)
riml

#make a random intercept linear mixed effect function for simulation
simml = function(ncity = 5, ndistrict = 4, b0 = -1,b1 = .005, sdr = 2, sd = 1) {
  city = rep(LETTERS[1:ncity], each = ndistrict) 
  district = letters[1:(ncity*ndistrict)] 
  fe = runif(ncity*ndistrict, 2, 75)
  re = rnorm(ncity, 0, sdr)
  sigma = rnorm(ncity*ndistrict, 0, sd)
  houseprice = b0 + b1*fe + re + sigma
  df = data.frame(city,fe,district, re, sigma)
  lmer(houseprice ~ fe + (1|re), data = df,REML = FALSE)
}
simml()
#test the function
set.seed (16)
simml()

###explore the effect of sample size on random effect variance and fixed effect slope estimation

library(purrr) # v. 0.2.4
suppressPackageStartupMessages( library(dplyr) ) # v. 0.7.4
library(ggplot2) # v. 2.2.1

##simulate the data and extract
city_simmls = c(5, 20, 100) %>% #(three city sample size is 5,20,100)
  set_names() %>%
  map(~replicate(1000, simml(ncity = .x) ) )#simulate the data, fit a model 1000 time for each one

re_vars_ml = city_simmls %>%# extracted random effect variances from the model.
  modify_depth(2, ~tidy( .x, effects = "ran_pars", scales = "vcov") ) %>%
  map_dfr(bind_rows, .id = "city_num") %>%
  filter(group == "re")
head(re_vars_ml)

fe_slo_ml = city_simmls %>%# extracted fixed effect slope from the model.
  modify_depth(2, ~tidy( .x, effects = "fixed")) %>%
  map_dfr(bind_rows, .id = "city_num")
head(fe_slo_ml)

##draw density plots
#(distributions of the random effect variances and fixed effect slope for each sample size)

#get my factor levels in order
re_vars_ml = mutate(re_vars_ml, city_num = forcats::fct_inorder(city_num) )
fe_slo_ml =  mutate(fe_slo_ml,city_num = forcats::fct_inorder(city_num))

#add some clearer labels about each sample size
prefix_add = function(string) {
  paste("Number citys:", string, sep = " ")
}
#add the median of each distribution as a second vertical line
groupmedre_ml = re_vars_ml %>%#group mean distribution of random effect
  group_by(city_num) %>%
  summarise(mvre_ml = median(estimate) )#mean variance of random effect

groupsldfe_ml = fe_slo_ml %>%#group slope distribution of fixed effect
  group_by(city_num) %>%
  summarise(msfe_ml = median(estimate) )#mean slope of fixed effect

#draw density plots of random effect and fixed effect
ggplot(re_vars_ml, aes(x = estimate) ) + 
  geom_density(fill = "blue", alpha = .25) +
  facet_wrap(~city_num, labeller = as_labeller(prefix_add) ) +
  geom_vline(aes(xintercept = sdr^2, linetype = "True variance"), size = .5 ) +
  geom_vline(data = groupmedre_ml, aes(xintercept = mvre_ml, linetype = "Median variance"),
             size = .5) +
  theme_bw() +
  scale_linetype_manual(name = "", values = c(2, 1) ) +
  theme(legend.position = "bottom",
        legend.key.width = unit(.1, "cm") ) +
  labs(x = "Estimated Variance", y = NULL)

ggplot(fe_slo_ml, aes(x = estimate) ) + 
  geom_density(fill = "blue", alpha = .25) +
  facet_wrap(~city_num, labeller = as_labeller(prefix_add) ) +
  geom_vline(aes(xintercept = b1, linetype = "True slope"), size = .5 ) +
  geom_vline(data = groupsldfe_ml, aes(xintercept = msfe_ml, linetype = "Median slope"),
             size = .5) +
  theme_bw() +
  scale_linetype_manual(name = "", values = c(2, 1) ) +
  theme(legend.position = "bottom",
        legend.key.width = unit(.1, "cm") ) +
  labs(x = "Estimated slope", y = NULL)