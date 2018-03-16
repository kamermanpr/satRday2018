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
## install.packages('devtools')
## devtools::install_github('kamermanpr/carrots')
library(carrots)

print(carrots)

###############
#             #
#   Slide 6   #
#             #
###############
## Gather
library(tidyverse)

theme_set(new = theme_grey(base_size = 20)) # Not shown on slide

carrots_G <- gather(data = carrots,
                    key = "Attribute",
                    value = "Rating",
                    Bitterness,
                    Crispness,
                    Sweetness)

print(carrots_G)

###############
#             #
#   Slide 7   #
#             #
###############
## Plot
ggplot(data = carrots_G) +
    aes(y = Preference,
        x = Product) +
    geom_jitter(colour = "blue") +
    geom_boxplot(alpha = 0.8) +
    facet_grid(Attribute ~ .)

###############
#             #
#  Slide 8/9  #
#             #
###############
# Divide and conquer

## Extract
bolero <- carrots[carrots$Product == 'Bolero', ]

## Plot
par(mfrow = c(3, 1))

par(mar = c(4, 5, 1, 5)) # Not shown on slide
par(cex = 1) # Not shown on slide

plot(Preference ~ Bitterness, data = bolero)
lines(loess.smooth(x = bolero$Bitterness,
                   y = bolero$Preference))

plot(Preference ~ Crispness, data = bolero)
lines(loess.smooth(x = bolero$Crispness,
                   y = bolero$Preference))

plot(Preference ~ Sweetness, data = bolero)
lines(loess.smooth(x = bolero$Sweetness,
                   y = bolero$Preference))

par(mfcol = c(1, 1))

################
#              #
#   Slide 11   #
#              #
################
# Split and apply

## Split
foo_list <- split(x = carrots_G,
                  f = carrots_G$Product)

################
#              #
#   Slide 12   #
#              #
################
# Split and apply

lapply(foo_list,
       head,
       n = 2)

################
#              #
#   Slide 13   #
#              #
################
# Split and apply

## Apply
lapply(X = foo_list,
       FUN = function(x) {ggplot(data = x) +
           aes(x = Rating,
               y = Preference) +
           geom_point() +
           geom_smooth() +
           facet_grid(~ Attribute)})

################
#              #
#   Slide 17   #
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
#   Slide 20   #
#              #
################
library(magrittr) # Not shown on slide

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
#   Slide 21   #
#              #
################
## Walk
walk(.x = foo$plot,
     ~ print(.x))

################
#              #
#   Slide 23   #
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
#   Slide 24   #
#              #
################
## Walk
walk(.x = foo$plot2,
     ~ print(.x))

################
#              #
#   Slide 26   #
#              #
################
## Generate summary stats
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

################
#              #
#   Slide 27   #
#              #
################
## Walk

### Note that to get the table in html format, the code needs to be run in an
### Rmarkdown document.
walk2(.x = foo$data2,
      .y = as.character(foo$Product),
      ~ print(knitr::kable(x = .x,
                     caption = str_glue("{.y} carrots: median preference"))))

################
#              #
#   Slide 29   #
#              #
################
## PMAP...getting carried away
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

print(foo)

################
#              #
#   Slide 26   #
#              #
################
## Walk
walk(.x = foo$plot3,
     ~ print(.x))
