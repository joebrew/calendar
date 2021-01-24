library(tidyverse)

dates <- seq(as.Date('2021-01-23'),
             as.Date('2021-01-23') + 99,
             by = 1)
xs <- rep(1:7, 20)
ys <- rep(15:1, each = 7)
xs <- xs[1:100]
ys <- ys[1:length(xs)]

df <- tibble(
  date = dates,
  x = xs,
  y = ys
) %>%
  mutate(dow = weekdays(dates)) %>%
  mutate(type = ifelse(dow %in% c('Saturday', 'Sunday'), 'Weekend', 'Weekday')) %>%
  mutate(dummy = as.numeric(dow == 'Monday')) %>%
  mutate(cs = cumsum(dummy))
cols <- rainbow(length(unique(df$cs)))
cols <- sample(cols, size = length(cols))
ggplot(data = df,
       aes(x = x,
           y = y,
           pch = type)) +
  geom_point(size = 15,
             aes(color = cs)) +
  geom_point(size = 14,
             aes(color = cs),
             alpha = 0.8) +
  geom_point(size = 13,
             aes(color = cs),
             alpha = 0.8) +
  geom_point(size = 12,
             aes(color = cs),
             alpha = 0.5) +
  theme_void() +
  scale_shape_manual(name = '',
                     values = c(0, 5)) +
  scale_colour_gradientn(colors = cols) +
  theme(legend.position = 'none') +
  geom_text(aes(label = format(date, '%d\n%b')),
            alpha = 0.6,
            size = 3) #+
  # geom_point(data = df %>% filter(type == 'Weekend'),
  #            size = 16,
  #            color = 'black') +
  # geom_point(data = df %>% filter(type == 'Weekend'),
  #            size = 17,
  #            color = 'black') 
ggsave('~/Desktop/calendar.png', height = 13, width = 10)

