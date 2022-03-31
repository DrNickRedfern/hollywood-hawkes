
# I use pacman for package management so install if necessary
# install.packages("pacman")

# Load the required packages ----------
pacman::p_load(plotly, here, tidyverse, ggpubr, ggtext, viridis, Cairo)

# Load the results data
df <- read_csv(here("data_ouput", "hp_exp_results.csv"))

# Filter out films that were not fitted by the model ----------
df <- df %>%
  filter(!title %in% c("Brief Encounter", "Beneath the Planet of the Apes")) %>%
  arrange(genre, year, title)

# Parameters against time ----------
# lambda0 by time
time_lambda <- ggplot(data = df) +
  geom_point(aes(x = year, y = lambda0, colour = genre), size = 2) +
  geom_smooth(aes(x = year, y = lambda0), colour = "black") +
  scale_x_continuous(name = " ", breaks = seq(1935, 2005, 5)) +
  scale_y_continuous(name = bquote("Background rate ("*lambda[0]*")"), limits = c(0, 0.2)) +
  scale_colour_viridis_d(name = NULL) +
  guides(title.position = "top") +
  theme_light() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank())

# Spearman trend test
cor.test(df$year, df$lambda0, method=c("spearman"))
DescTools::SpearmanRho(df$year, df$lambda0, conf.level = 0.95)

# mu by time
time_mu <- ggplot(data = df) +
  geom_point(aes(x = year, y = mu, colour = genre), size = 2) +
  geom_smooth(aes(x = year, y = mu), colour = "black") +
  scale_x_continuous(name = " ", breaks = seq(1935, 2005, 5)) +
  scale_y_continuous(name = bquote("Reproduction mean ("*mu*")")) +
  scale_colour_viridis_d(name = NULL) +
  guides(title.position = "top") +
  theme_light() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank())

# Spearman trend test
cor.test(df$year, df$mu, method=c("spearman"))
DescTools::SpearmanRho(df$year, df$mu, conf.level = 0.95)

# beta by time
time_beta <- ggplot(data = df) +
  geom_point(aes(x = year, y = beta, colour = genre), size = 2) +
  geom_smooth(aes(x = year, y = beta), colour = "black") +
  scale_x_continuous(name = "\nYear", breaks = seq(1935, 2005, 5)) +
  scale_y_continuous(name = bquote("Decay rate ("*beta*")"), limits = c(0, 0.1)) +
  scale_colour_viridis_d(name = NULL) +
  theme_light() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank())

# Spearman trend test
cor.test(df$year, df$beta, method=c("spearman"))
DescTools::SpearmanRho(df$year, df$beta, conf.level = 0.95)

# time figure
fig <- ggarrange(time_lambda, time_mu, time_beta, 
                 nrow = 3, ncol = 1, align = "v", labels = "AUTO",
                 common.legend = TRUE, legend="bottom")

ggsave(plot = fig, "time.pdf", 
       width = 15.92, height = 21, units = "cm", dpi = 600, device = cairo_pdf)

# Parameters against genre ----------
# lambda0 by genre
genre_lambda <- ggplot(data = df) +
  geom_boxplot(aes(x = genre, y = lambda0, colour = genre)) +
  geom_jitter(aes(x = genre, y = lambda0, colour = genre), size = 2, width = 0.2) +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name = bquote("Background rate ("*lambda[0]*")"), limits = c(0, 0.2)) +
  scale_colour_viridis_d() +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())

# Non-parametric anova with post-hoc test
kruskal.test(lambda0 ~ genre, data = df)
pairwise.wilcox.test(df$lambda0, df$genre, p.adjust.method = "bonferroni")

# mu by genre
genre_mu <- ggplot(data = df) +
  geom_boxplot(aes(x = genre, y = mu, colour = genre)) +
  geom_jitter(aes(x = genre, y = mu, colour = genre), size = 2, width = 0.2) +
  scale_x_discrete(name = " ") +
  scale_y_continuous(name = bquote("Reproduction mean ("*mu*")")) +
  scale_colour_viridis_d() +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())

# Non-parametric anova with post-hoc test
kruskal.test(br ~ genre, data = df)
pairwise.wilcox.test(df$br, df$genre, p.adjust.method = "bonferroni")

# beta by genre
genre_beta <- ggplot(data = df) +
  geom_boxplot(aes(x = genre, y = beta, colour = genre)) +
  geom_jitter(aes(x = genre, y = beta, colour = genre), size = 2, width = 0.2) +
  scale_x_discrete(name = "\nGenre") +
  scale_y_continuous(name = bquote("Decay rate ("*beta*")"), limits = c(0, 0.1)) +
  scale_colour_viridis_d() +
  theme_light() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())

# Non-parametric anova with post-hoc test
kruskal.test(beta ~ genre, data = df)
pairwise.wilcox.test(df$beta, df$genre, p.adjust.method = "bonferroni")

# genre figure
fig <- ggarrange(genre_lambda, genre_mu, genre_beta, 
                 nrow = 3, ncol = 1, align = "v", labels = "AUTO")

ggsave(plot = fig, "genre.pdf", 
       width = 15.92, height = 21, units = "cm", dpi = 600, device = cairo_pdf)


# Use Plotly to create 3D figure
# Select only required columns
df_a <- df %>% select(title, year, genre, lambda0, alpha, beta) 

# Get colours for palette
p_col <- viridis(5)

# Fit linear model
my_lm <- lm(beta ~ lambda0 + alpha, data = df_a)
cf <- coef(my_lm)

# Calculate z on a grid of x-y values using linear model for 2D surface
x1_seq <- seq(min(df$lambda0), max(df$lambda0), length.out = length(df$lambda0))
x2_seq <- seq(min(df$alpha), max(df$alpha), length.out = length(df$alpha))
z_mtx <- t(outer(x1_seq, x2_seq, function(x1, x2) cf[1] + cf[2]*x1 + cf[3]*x2))

# Plot 2D surface
p3 <- plot_ly(x = ~x1_seq, y = ~x2_seq, z = ~z_mtx, width = 1000, height = 750,
              type = "surface", colorscale = list(c(0, 1), c("#dddddd", "#dddddd")), 
              opacity = 0.7, showscale = FALSE) %>%
  style(hoverinfo = 'none')

# Add data points to plot
p3a <- p3 %>% add_trace(data = df_a, x = ~lambda0, y = ~alpha, z = ~beta, 
                        color = ~genre, colors = p_col,
                        mode = "markers", type = "scatter3d", opacity = 1,
                        hoverinfo = 'text',
                        text = ~paste('</br>', title,
                                      '</br>', year,
                                      '</br> genre: ', genre,
                                      '</br> \U03BB<sub>0</sub>: ', lambda0,
                                      '</br> \U03B1: ', alpha,
                                      '</br> \U03B2: ', beta)) %>%
  layout(autosize = FALSE, margin = list(l = 0, r = 0, b = 0, t = 0, pad = 2), 
         legend = list(orientation = 'h', xanchor = "center", x = 0.5),
         scene = list(xaxis = list(title = paste0("\U03BB", "<sub>0</sub>")),
                      yaxis = list(title = paste0("\U03B1")), # alpha: \U03B1 mu: \U03BC
                      zaxis = list(title = paste0("\U03B2")))
  )

# Suppress warnings about labels
storeWarn<- getOption("warn")
options(warn = -1) 

p3a
