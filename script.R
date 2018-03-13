############################################################
#                                                          #
#    Script to produce figures presented at satRday2018    #
#                                                          #
############################################################

###############
#             #
#   Slide 4   #
#             #
###############
# Quick look
## devtools::install_github('kamermanpr/carrots')
library(carrots)

print(carrots)

###############
#             #
#   Slide 5   #
#             #
###############
## Quick look
library(tidyverse)

## Not shown on slides to reduce slide text
theme_set(new = theme_grey(base_size = 20))

## Gather
carrots_G <- gather(data = carrots,
                    key = "Attribute",
                    value = "Rating",
                    Bitterness,
                    Crispness,
                    Sweetness)

## Plot
ggplot(data = carrots_G) +
    aes(y = Preference,
        x = Product) +
    geom_jitter(colour = "blue") +
    geom_boxplot(alpha = 0.8) +
    facet_grid(Attribute ~ .)

###############
#             #
#   Slide 6   #
#             #
###############
# Divide and conquer

## Extract
bolero <- carrots[carrots$Product == 'Bolero', ]

## Plot
par(mfcol = c(1, 3))

plot(Preference ~ Bitterness, data = bolero)
lines(loess.smooth(x = bolero$Bitterness,
                   y = bolero$Preference))

plot(Preference ~ Crispness, data = bolero)
lines(loess.smooth(x = bolero$Crispness,
                   y = bolero$Preference))

plot(Preference ~ Sweetness, data = bolero)
lines(loess.smooth(x = bolero$Sweetness,
                   y = bolero$Preference))

par(mfcol = c(1, 2))

###############
#             #
#   Slide 8   #
#             #
###############
# Split and apply

## Split
foo <- split(x = carrots_G,
             f = carrots_G$Product)

## Apply
lapply(X = foo,
       FUN = function(x) {ggplot(data = x) +
           aes(x = Rating,
               y = Preference) +
           geom_point() +
           geom_smooth() +
           facet_grid(~ Attribute)})

################
#              #
#   Slide 11   #
#              #
################
# Nest and map

## Nest
foo <- carrots_G %>%
    group_by(Product) %>%
    nest()

print(foo)

print(foo$data[[1]])

################
#              #
#   Slide 12   #
#              #
################
# Nest and map
library(magrittr)

## MAP
foo %<>%
    # Add a new column
    mutate(plot = map(# tell map where the data are
                      .x = data,
                      # Add a function (~)
                      ~ ggplot(data = .x) +
                          aes(x = Rating,
                              y = Preference) +
                          geom_point() +
                          geom_smooth() +
                          facet_grid(~ Attribute)))


print(foo)

################
#              #
#   Slide 13   #
#              #
################
## Walk
walk(.x = foo$plot, ~ print(.x))

################
#              #
#   Slide 14   #
#              #
################
## MAP2…adding a second variable
foo %<>%
    mutate(plot2 = map2(.x = data,
                        .y = as.character(Product),
                        ~ ggplot(data = .x) +
                            aes(x = Rating,
                                y = Preference) +
                            geom_point() +
                            geom_smooth() +
                            labs(title = str_glue("{.y} carrots")) +
                            facet_grid(~ Attribute)))

print(foo)

################
#              #
#   Slide 15   #
#              #
################
## Walk
walk(.x = foo$plot2, ~ print(.x))

################
#              #
#   Slide 16   #
#              #
################

foo %<>%
    mutate(data2 = map(.x = data,
                       ~ .x %>%
                           group_by(Age_group) %>%
                           summarise(Median = median(Preference, na.rm = TRUE),
                                     Q25 = quantile(Preference,
                                                    probs = 0.25, na.rm = TRUE),
                                     Q75 = quantile(Preference,
                                                    probs = 0.75, na.rm = TRUE))))

print(foo)

walk2(.x = foo$data2,
      .y = as.character(foo$Product),
      ~ print(knitr::kable(x = .x,
                     caption = str_glue("{.y} carrots: median preference"))))


foo %<>%
    mutate(plot3 = pmap(.l = list(data, data2, as.character(Product)),
                        ~ ggplot(data = ..1) +
                            aes(x = Rating,
                                y = Preference) +
                            geom_point() +
                            geom_smooth() +
                            geom_hline(data = ..2,
                                       aes(yintercept = Median),
                                       colour = 'red') +
                            labs(title = str_glue("{..3} carrots")) +
                            facet_grid(Attribute ~ Age_group)))

walk(.x = foo$plot3, ~ print(.x))
