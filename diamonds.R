# Diamond paradox

# How can there be a negative correlation between the quality of a diamond
# and its price? This work explains this fascinating and educational puzzle. 

# Dataset
# 
# Diamonds is a famous training dataset with almost 54,000 observations.
# It comes with ggplot2, which is a part of tidyverse.
# 
# This dataset has been analysed countless times and for a good reason:
# It shows a very educational misleading correlation, which I will be
# covering here.
# 
# In this work I'll be using tidyverse techniques and philosophies mentioned
# in the "R for Data Science" book by Hadley Wickham (https://r4ds.had.co.nz/)



#----- Preparation

library(tidyverse)

cut_colors = c("#C76E32", "#A8A846", "#46A846",
               "#3584B8", "#555599")


# Price of diamonds is capped at around $18,000:

diamonds |>
  slice_max(order_by = price, n = 15)

diamonds |>
  group_by(cut) |>
  slice_max(order_by = price, n = 3)

# This ceiling can hide the real connection between the size and the price
# especially for bigger diamonds, so I'll this analysis I'm going to focus on diamonds 0.2 >= and > 2.2 Ct

d <- diamonds |>
  filter(carat < 1.75)

d |>
  group_by(cut) |>
  slice_max(order_by = price, n = 3)


paste("Only", nrow(diamonds) - nrow(d), "rows were removed.")

# Paradox

# How does the quality of the cut affect the price of diamonds?

ggplot(d, aes(price, after_stat(density), colour = cut)) +
  geom_freqpoly(binwidth = 500, size = 2) +
  scale_color_manual(values = cut_colors) +
  labs(title = "Lower quality cut diamonds are more valuable?",
       caption = "The area under each curve is one.")

ggplot(d, mapping = aes(x = price, fill = cut))+
  geom_boxplot(aes(y = cut), alpha = 0.7) +
  scale_fill_manual(values = cut_colors) +
  labs(title = "Another plot showing that fair diamonds are more valuable than the ideal")

# Surprising answer seems to be that lowest quality cut diamonds are far more
# valuable than the best quality cuts. How to explain this paradox? 

# This is not only interesting, but also a very educational case. There are some
# other correlations at play here.


#--------- Explanation

# This paradox can be explain by two other correlations:

ggplot(d, aes(x = carat, y = ..density..)) +
  geom_freqpoly(aes(colour = cut), size = 2, bins = 20) +
  scale_color_manual(values = cut_colors) +
  labs(title = "Explanation 1:",
       subtitle= "There is correlation between lower quality and bigger size!",
       caption = "Density plot: the area under each curve is one.")

ggplot(d, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1))) +
  labs(title = "Explanation 2:",
       subtitle= "Size and price are strongly correlated!",
       caption = "")

# I'm using box plot here instead of scatter plot, because the latter takes too much
# computing power.




#-------- Actual correlation

# New question: How does quality of the 

ggplot(data = d) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  facet_wrap(~ cut, nrow = 2)



# The 


#-----------------------

invert_geom_defaults() # Back to light mode
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.3)))



smaller <- diamonds %>% 
  filter(carat < 3)



ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))


diamonds |>
  mutate(size = case_when(
    carat >= 0.2 & carat <= 0.7 ~ "small",
    carat >= 2 & carat <= 2.5 ~ "large",
    TRUE                      ~ "other"
  )
  ) |>
  filter(size == "small" | size == "large") |>
  ggplot(aes(x = price, after_stat(density), color = size)) +
  geom_freqpoly(size = 2)



diamonds |>
  ggplot(mapping = aes(x = carat, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = cut), size = 2)

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1))) +
  dark_theme_gray()

d <- d |>
  select(price, carat, cut, color)

## Exploratory Data Analysis
# The main question is: how do the other variables affect the price.

# Let's start by exploring the price variable

d |>
  ggplot(aes(price)) +
  geom_histogram(bins = 50)





summary(d)




d |>
  transmute(cut_interval(price, length = 5000)) |>
  table()


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.01)
coord_cartesian(xlim = c(3,10), ylim = c(0, 400))

d |>
  count(color, cut)


# Explor

d |>
  lm(log(price) ~ log(carat), data=_) |> summary()


library(modelr)

d2 <- d %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

mod_diamond <- lm(lprice ~ lcarat, data = d2)

grid <- d2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(d2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)



# 1) Take logarithms (since )

mod <- lm(log(price) ~ log(carat), data = d)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))



v1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
v2 = v1^4

t <- tibble(v1 = v1, v2 = v2)

ggplot(t3, aes(x = v1, y = v2))+
  geom_point()

summary(lm(v2~v1, t))

summary(lm(log10(v2)~log10(v1), t))

t
t2 <- tibble(v1 = log2(v1), v2 = log2(v2))
t2

t3 <- tibble(v1 = log10(v1), v2 = log10(v2))
## Including Plots

# You can also embed plots, for example:
#   
#   ```{r pressure, echo=FALSE}
# plot(pressure)
# ```
# 
# Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
# 



diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))


ggplot(d, aes(carat, price)) + 
  geom_hex(bins = 30)

ggplot(d, aes(log2(carat), log2(price))) + 
  geom_hex(bins = 30)


ggplot(d, aes(carat, price)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis") +
  scale_x_log10() + 
  scale_y_log10()


ggplot(d, aes(carat, price)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis")


lm(price ~ carat, d)$qr
summary(lm(price ~ carat, d))

summary(lm(price ~ carat, d))$r.squared

summary(lm(price ~ carat, d))$r.squared

paste0("Trying to fit linear model to the unmodified data gives R-squared of ",
      round(summary(lm(price ~ carat, d))$r.squared, 2),
      ", but taking the logarithmic data gives ",
      round(summary(lm(log2(price) ~ log2(carat), d))$r.squared, 2),
      ".")


ggplot(d, aes(carat, price)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(formula = y ~ x, method = "lm", color = "white") +
  scale_x_log10() + 
  scale_y_log10() +
  labs(title = "Logarithmic scale fits the data well")



mod <- lm(log(price) ~ log(carat), data = diamonds)

d2 <- d %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))


d |>
  lm(log(price) ~ log(carat), data = _) |>
  add_residuals(d, model = _) -> d
View(d)


summary(lm(log(price) ~ log(carat), data = d))

exp(1)

ggplot(data = d) + 
  geom_point(mapping = aes(x = carat, y = resid, color = cut), alpha = 0.7)

11816 * 1.700120 + 8.465095


exp(1.71) * exp(1.700120) + exp(8.465095)

#Log(price)=0.18+3.97carat1/3−0.474carat

1.700120 * 1.71 + 8.465095
log(11816)

d |>
  filter(abs(resid) < 0.0001)

log(1.71) * 1.700120 + 8.465095
log(11816)


paste0("The formula that linear model gives is ",
       round(summary(lm(log(price) ~ log(carat), d))$coefficients[2], 2),
       " * log(carat) + ",
       round(summary(lm(log(price) ~ log(carat), d))$coefficients[1], 2),
       " = log(price)")

summary(lm(log(price) ~ log(carat), d))$coefficients[2]


model <- lm(log2(price) ~ log2(carat), d) 


d |>
  filter(abs(resid) < 0.0001)

d |> mutate()


d |>
  lm(model, data = _) |>
  add_residuals(d, model = _) |>
  mutate(resid_exp = exp(resid)) |> View()



log(1.71) * 1.700120 + 8.465095
log(11816)


d |>
  slice_min(abs(resid)) -> low_resid

predicted <- exp(slope * log(low_resid$carat) + intercept)

paste("Precited vs actual price:",
       round(exp(slope * log(low_resid$carat) + intercept), 2),
       "vs",
       low_resid$price)

# Predicted price:

d |>
  slice_min(abs(resid), n = 3, with_ties = FALSE) -> low_resids


d |>
  slice_max(abs(resid), n = 3, with_ties = FALSE) -> high_resids

test_formula <- function(diamond){
  paste("Precited vs actual price:",
        round(exp(slope * log(diamond$carat) + intercept), 2),
        "vs",
        diamond$price)
}

test_formula(low_resid)


v = c(2,3,2,3)

vapply(v, sum)

mtcars

mpg

mpg |>
  rowwise() |>
  la

ggplot(d, mapping = aes(x = resid, fill = cut))+
  geom_boxplot(aes(y = cut), alpha = 0.7) +
  scale_fill_manual(values = cut_colors) +
  labs(title = "")

exp(1.35076) * 613

#2366
# 613

#price_predicted_by_size = round(exp(slope * log(carat) + intercept)))

tibble(residual = rep(c(1, 0.5, 0, -0.5, -1),2),
       carat = c(rep(0.5, 5), rep(1,5)))

