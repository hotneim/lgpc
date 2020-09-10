# R-code for reproducing the simulations and empirical results in ?The locally
# Gaussian partial correlation? For prefect reproduction, run this script in its
# entirety, and notice that the random seed is sett just below the package
# loadings. Output from the sessionInfo() of the system used to create these
# figures is listed in the end of this document.

# Packages needed
library(lg)          # <-- The main package, see documentation for details.
library(mvtnorm)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)
library(readxl)
library(tseries)

# Load the necessary packages and set random seed for full reproducibility
SEED <- 1
set.seed(SEED)

# EXAMPLE: THE MULTIVARIATE GAUSSIAN DISTRIBUTION
# -----------------------------------------------

# We generate samples from three-variate Gaussian distributions with different
# sample sizes. We then calculate the partial correlations between X1 and X2
# given X3 and save the results.

# Estimation parameters
n_gaussian                           <- c(100, 500, 2000)       # The sample sizes
level                                <- .95                     # Conf. level for asymptotic conf. int.
grid_size                            <- 7                       # No. of grid points in each direction
bw_select                            <- "plugin"                # Bandwidth selection method
plugin_constant                      <- 4                       # Proportionality constant for plugin bandwidth selector
est_method                           <- "1par"                  # Estimation method
transform_to_marginal_normality      <- TRUE                    # Transform to marginal normality

# Create the grid
grid <- expand.grid(X1 = seq(-3.5, 3.5, length.out = grid_size),
                    X2 = seq(-3.5, 3.5, length.out = grid_size))

# This will be a list of plots that we print out later
gaussian_example <- list()

# One set of plots for each sample size
for(i in seq_along(n_gaussian)) {
    
    # Generate data
    x <- rmvnorm(n_gaussian[i], sigma = diag(3))
    
    # Estimate the local correlation
    lg_object <- lg_main(x, 
                         bw_method = bw_select,
                         plugin_constant_joint = plugin_constant,
                         est_method = "1par", 
                         transform_to_marginal_normality = TRUE)
    
    partial_correlations <- partial_cor(lg_object, grid = grid, condition = 0)
    
    cplot <- corplot(partial_correlations,
                     plot_obs = FALSE, 
                     alpha_point = 0.1,
                     plot_thres = 0.005,
                     alpha_tile = .001,
                     main = "",
                     xlab = "",
                     ylab = "",
                     label_size = 4)
    
    gaussian_example[[i]] <- cplot + theme_classic() + theme(legend.position = "none")
    ggsave(paste0("figures/gaussian-example-plots-", i, ".pdf"), width = 4, height = 3)
}

# EXAMPLE: we calculate the LGPC for the case with bivariate normals and stochastic correlation. 
# -----------------------------------------------------------------------------------

# Sample size
n_simex2 <- 1000

# This is the function that we use to generate data:
gen <- function(n) {
    
    out <- matrix(NA, nrow = n, ncol = 3)
    colnames(out) <- c("x1", "x2", "rho")
    
    for(i in 1:n) {
        rho <- runif(1, min = -.95, max = .95)
        x <- rmvnorm(1, mean = c(0,0), sigma = matrix(c(1, rho, rho, 1), nrow = 2))
        
        out[i, ] <- c(x, rho)
    }
    
    as_tibble(out)
}

# Generate the data
x <- gen(n_simex2)

c     <-  2       # Smoothing
thres <- .02      # Plotting threshold

low_val  <- -.9
mid_val  <- 0
high_val <- .9

plot_dim <- 5

# Make a plot of just the observations:
simex2_points <- x %>% 
    ggplot(aes(x = x1, y = x2)) +
    xlab("") + 
    ylab("") +
    geom_point() +
    theme_classic() 
ggsave("figures/simex2-plots-1.pdf", height = plot_dim, width = plot_dim)

# Create partial correlation maps for low, mid and high values of X3:
lg_object <- lg_main(x %>% select(x1, x2, rho), 
                     plugin_constant_joint = c, 
                     est_method = "trivariate")

# LOW
grid_low <- expand.grid(x1 = seq(-3.5, 3.5, length.out = 10),
                        x2 = seq(-3.5, 3.5, length.out = 10)) %>% 
    as_tibble %>% 
    mutate(dens = dmvnorm(cbind(.$x1, .$x2), 
                          sigma = matrix(c(1, low_val, low_val, 1), ncol = 2))) %>% 
    mutate(plot = dens > thres*max(dens)) %>% 
    filter(plot) %>% 
    select(x1, x2)

partial_correlations_low <- partial_cor(lg_object, 
                                        grid = grid_low, 
                                        condition = low_val, 
                                        level = NULL)

simex2_low <- corplot(partial_correlations_low,
                      label_size = 4,
                      main = "",
                      xlab = "",
                      ylab = "",
                      alpha_tile = 0.001) +
    geom_point(aes(x = x1, y = x2), 
               alpha = .1,
               data = x) +
    theme_classic() +
    theme(legend.position = "none")
ggsave("figures/simex2-plots-2.pdf", height = plot_dim, width = plot_dim)

# MID
grid_mid <- expand.grid(x1 = seq(-3.5, 3.5, length.out = 10),
                        x2 = seq(-3.5, 3.5, length.out = 10)) %>% 
    as_tibble %>% 
    mutate(dens = dmvnorm(cbind(.$x1, .$x2), 
                          sigma = matrix(c(1, mid_val, mid_val, 1), ncol = 2))) %>% 
    mutate(plot = dens > thres*max(dens)) %>% 
    filter(plot) %>% 
    select(x1, x2)

partial_correlations_mid <- partial_cor(lg_object, 
                                        grid = grid_mid, 
                                        condition = mid_val, 
                                        level = NULL)

simex2_mid <- corplot(partial_correlations_mid,
                      label_size = 4,
                      main = "",
                      xlab = "",
                      ylab = "",
                      alpha_tile = 0.001) +
    geom_point(aes(x = x1, y = x2), 
               alpha = .1,
               data = x) +
    theme_classic() +
    theme(legend.position = "none")
ggsave("figures/simex2-plots-3.pdf", height = plot_dim, width = plot_dim)

# HIGH
grid_high <- expand.grid(x1 = seq(-3.5, 3.5, length.out = 10),
                         x2 = seq(-3.5, 3.5, length.out = 10)) %>% 
    as_tibble %>% 
    mutate(dens = dmvnorm(cbind(.$x1, .$x2), 
                          sigma = matrix(c(1, high_val, high_val, 1), ncol = 2))) %>% 
    mutate(plot = dens > thres*max(dens)) %>% 
    filter(plot) %>% 
    select(x1, x2)

partial_correlations_high <- partial_cor(lg_object, 
                                         grid = grid_high, 
                                         condition = high_val, 
                                         level = NULL)

simex2_high <- corplot(partial_correlations_high,
                       label_size = 4,
                       main = "",
                       xlab = "",
                       ylab = "",
                       alpha_tile = 0.001) +
    geom_point(aes(x = x1, y = x2), 
               alpha = .1,
               data = x) +
    theme_classic() +
    theme(legend.position = "none")
ggsave("figures/simex2-plots-4.pdf", height = plot_dim, width = plot_dim)



# EXAMPLE: THE STRUCTURAL EXAMPLE
# -------------------------------

n                                    <- 500                     # The sample sizes
level                                <- .95                     # Conf. level for asymptotic conf. int.
a                                    <- 1
grid_size_biv                        <- 100                     # No. of grid points in each direction for bivariate plots
grid_size_uni                        <- 400                     # No. of grid points for the univariate plot
bw_select                            <- "plugin"                # Bandwidth selection method
plugin_constant                      <- 4                       # Proportionality constant for choosing bandwidth
est_method                           <- "1par"                  # Estimation method
transform_to_marginal_normality      <- TRUE                    # Transform to marginal normality

# Create the bivariate grid
grid_biv <- expand.grid(X1 = seq(-4, 4, length.out = grid_size_biv),
                        X2 = seq(-5, 10, length.out = grid_size_biv))

# Create the univariate grid
x0 <- seq(from = -3.5, 3.5, length.out = grid_size_uni)
grid_uni <- cbind(x0, x0^2)

# Generate data
temp <- rmvnorm(n, sigma = diag(2))
x <- cbind(temp[,1], a*temp[,1]^2 + temp[,2], temp[,2])

# Estimate the local correlation
lg_object <- lg_main(x, 
                     bw_method = bw_select,
                     plugin_constant_joint = plugin_constant, 
                     est_method = "1par", 
                     transform_to_marginal_normality = TRUE)

# For the bivariate plot
partial_correlations_biv <- partial_cor(lg_object, grid = grid_biv, condition = 0)

# For the univariate plot
partial_correlations_uni <- partial_cor(lg_object, grid = grid_uni, condition = 0)

# We then generate the plots
cplot_biv <- corplot(partial_correlations_biv,
                     plot_obs = TRUE, 
                     alpha_point = 1,
                     point_size = 0.7,
                     plot_thres = 0.005,
                     main = "",
                     xlab = "",
                     ylab = "",
                     label_size = 4,
                     plot_labels = FALSE)

uni_grid <- data.frame(partial_correlations_uni$grid)
colnames(uni_grid) <- c("x", "y")

# Add the univariate grid to the bivariate plot.
cplot_biv <- cplot_biv + ggplot2::geom_line(ggplot2::aes(x = x, y = y), data = uni_grid, lty = 2) +
    theme_classic() + theme(legend.position = "none")

# The univariate plot
uni <- data_frame(x = partial_correlations_uni$grid[,1],
                  pc = partial_correlations_uni$partial_correlations)

uniplot <- uni %>% ggplot() + geom_line(mapping = aes(x  = x, y = pc), size = 1.3) +
    xlab("") + ylab("") +
    labs(title = "") +
    theme_classic() + 
    theme(legend.position = "none")

ggsave("figures/structural-plot-1.pdf", plot = cplot_biv, height = 4, width = 5)
ggsave("figures/structural-plot-2.pdf", plot = uniplot, height = 4, width = 5)

# Calculate the ordinary Gaussian partial correlation in the structural example
partial_cor <- -cov2cor(solve(cor(x)))[1,2]



# EXAMPLE: REAL DATA: -------------------
#
# We look at the S&P500 example from Cheng & Huang (2012). Make plots and run
# our CI-test on the same data.

RERUN_CI_TESTS_REAL <- FALSE     # Run the CI tests for the real data example in Section 3

# Parameters --------
c_plotting         <- 3.5       # Bandwidth proportionality constant for making plots
c_test             <- 1.75      # Bandwidth proportionality constant for CI test
grid_size_plotting <- 9         # Grid points in each direction for plotting
plot_thres         <- .00001    # Density threshold for plots
alpha_point        <- .05       # Transparency when plotting data points
label_size         <- 4.5       # Label size in plots
plot_obs           <- TRUE      # Plot the observations at all
extend             <- 1.2       # Extend xlim,ylim beyond tiles for plot
n_rep              <- 10        # The number of bootstrap samples for the CI test

# Plotting range
r_range <- c(-.06, .06)
v_range <- c(-1, 1)

# Load the S&P500 data, clean it up, and make log-returns. --------
sp500 <- read_csv("data/sp500.csv") %>% 
    mutate(ret = c(NA, diff(log(Close))),
           vol = c(NA, diff(log(Volume)))) %>% 
    mutate(ret_lagged = c(NA, ret[-nrow(.)]),
           vol_lagged = c(NA, vol[-nrow(.)])) %>% 
    select(Date, ret, vol, ret_lagged, vol_lagged)

# Calculate the LGPC in the case where we investigate whether returns Granger causes volume

# Create the data set
sp500_ret_to_vol <- sp500 %>%  filter(!is.na(vol_lagged)) %>% 
    select(vol, ret_lagged, vol_lagged)

# Create lg-object for plotting
ret_to_vol_lg_object <- lg_main(sp500_ret_to_vol,
                                plugin_constant_joint = c_plotting, 
                                est_method = "trivariate")

# Define a grid for plotting
grid <- expand.grid(x = seq(v_range[1], v_range[2], length.out = grid_size_plotting),
                    y = seq(r_range[1], r_range[2], length.out = grid_size_plotting))

# Estimate the LGPC
ret_to_vol_partial_cor <- partial_cor(ret_to_vol_lg_object, 
                                      grid = grid,
                                      condition = 0)

# Create the plot
ret_to_vol_plot <- corplot(ret_to_vol_partial_cor,
                           main = "",
                           xlab = "",
                           ylab = "",
                           xlim = v_range*extend,
                           ylim = r_range*extend,
                           plot_thres =plot_thres,
                           plot_obs = plot_obs,
                           alpha_point = alpha_point,
                           label_size = label_size) +
    theme_classic() +
    theme(legend.position = "none")

# Calculate the LGPC in the case where we investigate whether volume Granger causes returns

# The data set
sp500_vol_to_ret <- sp500 %>%  filter(!is.na(vol_lagged)) %>% 
    select(ret, vol_lagged, ret_lagged)

# Create lg-object for plotting
vol_to_ret_lg_object <- lg_main(sp500_vol_to_ret,
                                plugin_constant_joint = c_plotting, 
                                est_method = "trivariate")

# Define a grid for plotting
grid <- expand.grid(x = seq(r_range[1], r_range[2], length.out = grid_size_plotting),
                    y = seq(v_range[1], v_range[2], length.out = grid_size_plotting))

# Estimate the LGPC
vol_to_ret_partial_cor <- partial_cor(vol_to_ret_lg_object, 
                                      grid = grid,
                                      condition = 0)
# Create the plot
vol_to_ret_plot <- corplot(vol_to_ret_partial_cor,
                           main = "",
                           xlab = "",
                           ylab = "",
                           xlim = r_range*extend,
                           ylim = v_range*extend,
                           plot_thres = plot_thres,
                           plot_obs = plot_obs,
                           alpha_point = alpha_point,
                           label_size = label_size) +
    theme_classic() +
    theme(legend.position = "none")

# Create the LOW and HIGH plots as well
vol_to_ret_partial_cor_low <- partial_cor(vol_to_ret_lg_object, 
                                          grid = grid,
                                          condition = -0.05)

vol_to_ret_plot_low <- corplot(vol_to_ret_partial_cor_low,
                               main = "",
                               xlab = "",
                               ylab = "",
                               xlim = r_range*extend,
                               ylim = v_range*extend,
                               plot_thres = plot_thres,
                               plot_obs = plot_obs,
                               alpha_point = alpha_point,
                               label_size = label_size) +
    theme_classic() +
    theme(legend.position = "none")

vol_to_ret_partial_cor_high <- partial_cor(vol_to_ret_lg_object, 
                                           grid = grid,
                                           condition = 0.05)

vol_to_ret_plot_high <- corplot(vol_to_ret_partial_cor_high,
                                main = "",
                                xlab = "",
                                ylab = "",
                                xlim = r_range*extend,
                                ylim = v_range*extend,
                                plot_thres = plot_thres,
                                plot_obs = plot_obs,
                                alpha_point = alpha_point,
                                label_size = label_size) +
    theme_classic() +
    theme(legend.position = "none")

# Do the nonlinear Granger causality test for returns to volume

# Create a new lg object, because we use a different bandwidth proportionality
# constant when testing as opposed to plotting
ret_to_vol_lg_object_test <- lg_main(sp500_ret_to_vol,         
                                     plugin_constant_joint = c_test)

# Run the test
# ret_to_vol <- ci_test(ret_to_vol_lg_object_test)

# Do the nonlinear Granger causality test for volum to returns

# Create a new lg object, because we use a different bandwidth proportionality
# constant when testing as opposed to plotting
vol_to_ret_lg_object_test <- lg_main(sp500_vol_to_ret,
                                     plugin_constant_joint = c_test)

# Run the test
# vol_to_ret <- ci_test(vol_to_ret_lg_object_test, n_rep = n_rep)

# Make the plots
ggsave("figures/realplots-1.pdf", plot = ret_to_vol_plot, height = 6, width = 6)
ggsave("figures/realplots-2.pdf", plot = vol_to_ret_plot, height = 6, width = 6)
ggsave("figures/realplots-3.pdf", plot = vol_to_ret_plot_low, height = 6, width = 6)
ggsave("figures/realplots-4.pdf", plot = vol_to_ret_plot_high, height = 6, width = 6)

# Save a timestamp and session info in a separate file

sink("time-stamp-and-sessioninfo.txt")
cat("The rcode.R-script producing all figures in the paper was run on\n\n")
cat(format(Sys.time()))
cat("\n\n")
cat("Output from sessionInfo():")
cat()
sessionInfo()
sink()




