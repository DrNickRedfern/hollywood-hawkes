# I use pacman for package management. If this is not installed on your computer uncomment and run
# install.packages("pacman")

# Load the required packages
pacman::p_load(hawkesbow, here, tidyverse, ggpubr)

# The list films and their genre
film_genre <- read_csv(here("data", "film_genre.csv"))

# Load shot length data
dat <- read_csv(here("data", "cutting.csv"))

# Get column names
names <- colnames(dat)

# Create an empty data frame for result
df <- data.frame()

# Loop over all the films
for(i in 1:dim(dat)[2]){
  
  # Set names of outputs based on column headings of the input data ----------
  name <- names[i]
  
  # Display names for plotting
  title <- str_split_fixed(name, pattern = "\\(", n = 2)[1] %>%
    str_trim(., "right")
  
  year <- str_split_fixed(name, pattern = "\\(", n = 2)[2] %>%
    str_split_fixed(., pattern = "\\)", n = 2) %>% .[1]
  
  # Output names for files
  out_name <- str_split_fixed(name, pattern = "\\(", n = 2)[1] %>%
    str_trim(., "right") %>%
    str_replace_all(., " ", "_")
  
  count_data <- paste0(out_name, "_exp_Nt.csv")
  ks_data <- paste0(out_name, "_exp_KS.csv")
  intensity_data <- paste0(out_name, "_exp_L.csv")
  plots <- paste0(out_name, "_exp_plots.pdf")
  
  # Select data for individual film ----------
  d <- dat[i] %>% drop_na() %>% cumsum()
  d <- d[[1]]
  
  # Tracking progress through the sample ----------
  print(paste0("Processing ", i, " of ", dim(dat)[2], ": ", title))
  
  # Fit exponential kernel by MLE ----------
  res_exp <- mle(d, "exp", max(d), init = c(0.2, 0.2, 0.2))
  
  # Get the parameters of the fitted model
  lambda0 <- round(res_exp$model$param[1], 4)  # background rate
  mu <- round(res_exp$model$param[2], 4)  # reproduction mean
  beta <- round(res_exp$model$param[3], 4)  # decay rate of reproduction kernel
  alpha <- round(res_exp$model$param[2] * res_exp$model$param[3], 4)  # jump value
  
  # Calculate and scale the compensator
  com <- compensator(d, d, 
                     fun = lambda0, repr = mu, family = "exp", rate = beta)
  com_scale <- com/max(com)
  
  # Kolmogorov-Smirnov test for goodness-of-fit against a standard Uniform distribution
  ks_res <- ks.test(com_scale, "punif")
  D <- round(ks_res$statistic, 3) # D statistic
  p <- round(ks_res$p.value, 3) # p-value
  
  # Gather parameters and goodness-of-fit results for export
  df_a <- cbind.data.frame(title, year, lambda0, mu, beta, alpha, D, p)
  
  # Update results data frame
  df <- rbind.data.frame(df, df_a)
  
  # Plot the results ----------
  # Get the total length of a film
  n <- length(d)
  
  # Set breaks for time axis
  if (max(d) > 8500) {
    t_breaks <- seq(0, 2000 * max(d)%/%2000, 2000)
  } else {
    t_breaks <- seq(0, 1000 * max(d)%/%1000, 1000)
  }
  
  # Set breaks for count axis
  if (n <= 600) {
    c_breaks <- seq(0, 100 * n%/%100, 100)
  } else if (n > 600 && n <= 1400) {
    c_breaks <- seq(0, 200 * n%/%200, 200)
  } else if (n > 1400 && n <= 2000) {
    c_breaks <- seq(0, 300 * n%/%300, 300)
  } else {
    c_breaks <- seq(0, 500 * n%/%500, 500)
  }
  
  # Create a rug plot ----------
  df_rug <- data.frame("times" = d, "ones" = rep(1, n))
  rug_plot <- ggplot(data = df_rug, aes(x = times, y = ones)) +
    geom_vline(aes(xintercept = times), size = 0.1) +
    scale_x_continuous(name = element_blank(), expand = c(0, 0)) +
    scale_y_continuous(name = element_blank(), expand = c(0, 0)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"))
  
  # Counting process plot ----------
  # Arrange the data and export to as a csv file
  df_Nb <- data.frame("time" = c(0, d), "count" = 0:n)
  write_csv(df_N, here("results", "data_output", count_data))
  
  # Format y-axis label
  N_y_label <- bquote(italic("N")*"(0,"*italic("t")*")")
  
  # Create the counting process plot
  N_plot <- ggplot(data = df_N) +
    #geom_line(aes(x = time, y = count)) +
    geom_step(aes(x = time, y = count)) +
    scale_x_continuous(name = "Time (s)", breaks = t_breaks, expand = c(0, 0)) +
    scale_y_continuous(name = N_y_label, breaks = c_breaks, expand = c(0, 0)) +
    theme_classic() +
    theme( plot.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"))
  
  # Combine the counting process plot with the rug plot
  count_plot <- ggarrange(rug_plot, N_plot, 
                          nrow = 2, ncol = 1, align = "v", heights = c(0.15, 0.85), labels = "A")
  
  # ks plot ----------
  # arrange the data and export to a csv file
  cdf <- sort(c(rep((1:n-1)/(n-1), 2)))
  ks_times <- sort(c(0, rep(com[1:(n - 1)], 2), max(com)))
  unif <- sort(rep(c((1:n-1)/n), 2))
  df_ks <- data.frame("r_times" = ks_times, "cdf" = cdf, ref = unif)
  write_csv(df_ks, here("results", "data_output", ks_data))
  
  # Polygon for 99% confidence interval
  v <- 1.63/sqrt(n)
  x <- c(0, n*v, n, n, n*(1-v), 0)
  y <- c(0, 0, 1-v, 1, 1, v)
  df_poly <- data.frame(x = x, y = y)
  
  # Plot the goodness-of-fit
  cdf_plot <- ggplot(data = df_ks) +
    geom_polygon(data = df_poly, aes(x = x, y = y), fill = "#dddddd") +
    geom_line(aes(x = r_times, y = cdf), size = 0.6, colour = "black") +
    scale_x_continuous(name = "Rescaled times", breaks = c_breaks, expand = c(0,0)) +
    scale_y_continuous(name = "Cumulative distribution function", breaks = seq(0, 1, 0.2),
                       expand = c(0,0)) +
    theme_classic() +
    theme(legend.position = "none", 
          plot.title = element_text(lineheight = 1.2, hjust = 0.5, size = 11),
          axis.title = element_text(size = 11),
          plot.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"))
  
  # Create a small plot containing only text to enforce formatting in the final figure
  text <- paste0("Kolmogorov-Smirnov test:\n D = ", D, "  p = ", p)
  text_plot <- ggplot() +
    annotate("text", x = n/2, y = 0.5, size  = 3.5, label = text) +
    theme_void()
  
  # Combine plots
  ks_plot <- ggarrange(text_plot, cdf_plot, 
                       nrow = 2, ncol = 1, align = "v", heights = c(0.15, 0.85), labels = "B")
  
  # Intensity function plot ----------
  # Calcualte the conditional intensity function
  lambda <- intensity(d, seq(0, max(d), length.out = 10000), 
                      fun = lambda0, repr = mu, family = "exp", rate = beta)
  
  # Arrange the data and export as a csv file
  int_times <- seq(0, max(d), length.out = 10000)
  df_int <- cbind.data.frame(int_times, lambda); names(df_int)[1] <- "time"
  write_csv(df_int, here("results", "data_output", intensity_data))
  
  # Format y-axis label
  int_y_label <- bquote(lambda*"("*italic(t)*"|"*italic(H[t])*")")
  
  # Plot the conditional intensity function
  int_plot <- ggplot(data = df_int) +
    geom_line(aes(x = time, y = lambda)) +
    geom_hline(aes(yintercept = lambda0), linetype = "dotted") +
    coord_cartesian(clip = "off") +
    scale_x_continuous(name = "Time (s)", breaks = t_breaks, expand = c(0, 0)) +
    scale_y_continuous(name = int_y_label, 
                       limits = c(0, max(df_int$lambda)), expand = c(0, 0)) +
    theme_classic() +
    theme( plot.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"))
  
  # Combine the intensity function plot with the rug plot
  intensity_plot <- ggarrange(rug_plot, int_plot, 
                              nrow = 2, ncol = 1, align = "v", heights = c(0.15, 0.85), labels = "C")
  
  # Create the final figure for each film ----------
  figure <- ggarrange(count_plot, ks_plot, nrow = 1, ncol = 2, align = "h")
  figure <- ggarrange(figure, intensity_plot, nrow = 2, align = "v")
  output <- annotate_figure(figure, top = text_grob(name, face = "bold", size = 16))
  
  # Export as a pdf file
  ggsave(here("results", "plots", plots), width = 20, height = 20, units = "cm", dpi = 600)
  
}

# Merge with the genre of the film with the results and reorgainse the data frame ----------
df <- left_join(df, film_genre) %>%
  relocate(genre, .after = year)

# Save the results fitting the model for all films as a csv file ----------
write_csv(df, here("results", "hp_exp_results.csv"))