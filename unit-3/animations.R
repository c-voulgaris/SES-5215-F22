library(datasauRus)
library(ggplot2)
library(gganimate)
library(faux)

# ggplot(datasaurus_dozen, aes(x=x, y=y))+
#   geom_point()+
#   theme_minimal() +
#   transition_states(dataset, 3, 1)
# 
# gganimate::anim_save('datasaurus.gif')

cor_data <- tibble(X = rnorm(n = 100, mean = 0, sd = 1)) %>%
  mutate(Y = 10 - X) %>%
  mutate(label = "r = -1\nR-square = 1")

for (i in seq(-0.8, 0.8, by=0.2)) {
  cor_data_next <- rnorm_multi(n = 100, 
                               mu = c(0, 10),
                               sd = c(1, 1),
                               r = i, 
                               varnames = c("X", "Y")) %>%
    mutate(label = paste0("r = ", i, "\nR-square = ", i^2))
  
  cor_data <- rbind(cor_data, cor_data_next)
}

cor_data_next <- tibble(X = rnorm(n = 100, mean = 0, sd = 1)) %>%
  mutate(Y = 10 + X) %>%
  mutate(label = "r = 1\nR-square = 1")

cor_data <- rbind(cor_data, cor_data_next) %>%
  mutate(label = fct_inorder(label))

ggplot(cor_data, aes(x=X, y=Y))+
  geom_point(size = 0.1)+
  #annotate(geom = "text", )
 # theme_minimal() +
    facet_wrap(vars(label))
  #transition_states(r, 3, 10)
