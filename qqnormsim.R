qqnormsim <- function(data) {
  sample_length <- length(data)
  sample_mean <- mean(data)
  sample_sd <- sd(data)
  number_of_simulations <- 8
  sim_run_number <- rep("Normal Q-Q Plot (Data)", number_of_simulations + 1)
  sim_run_number[-1] <- paste(
    rep("Normal Q-Q Plot (Sim)", number_of_simulations),
    as.character(1:number_of_simulations), sep = " ")
  sim_run_number <- forcats::as_factor(rep(sim_run_number,
                                           each = sample_length))

  sim_norm_all <-
    tibble::tibble(x = data) %>%
    dplyr::bind_rows(
      purrr::map_df(rep(sample_length, number_of_simulations),
                    ~tibble::tibble(x = rnorm(., mean = sample_mean, sd = sample_sd)))) %>%
    dplyr::mutate(sim_run_number = sim_run_number)

  sim_norm_mean_sd <- sim_norm_all %>%
    dplyr::group_by(sim_run_number) %>%
    dplyr::summarize(x_mean = mean(x), x_sd = sd(x), x_min = min(x),
              x_max = max(x),
              slope = diff(
                quantile(x = x, probs = c(0.25, 0.75), type = 1)) /
                diff(qnorm(p = c(0.25, 0.75))),
              intercept = quantile(x = x, probs = c(0.25), type = 1) -
                diff(
                quantile(x = x, probs = c(0.25, 0.75), type = 1)) /
                diff(qnorm(p = c(0.25, 0.75))) * qnorm(p = c(0.25))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(sim_run_number)

  global_min <- -3.1
  global_max <- 3.1
  qqline_pair <- 1.10 * c(global_min, global_max)

  sim_norm_mean_sd <- sim_norm_mean_sd %>%
    dplyr::mutate(qqline_min = global_min * slope + intercept,
                  qqline_max = global_max * slope + intercept) %>%
    tidyr::gather(qqline_min:qqline_max, key = "qqline_key", value = "qq_y") %>%
    dplyr::select(-qqline_key) %>%
    dplyr::arrange(sim_run_number, qq_y) %>%
    dplyr::mutate(qq_x = rep(qqline_pair, number_of_simulations + 1))

  sim_norm_all_plot <- ggplot2::ggplot(data = sim_norm_all) +
    ggplot2::geom_qq(mapping = ggplot2::aes(sample = x),
                     color = "turquoise4") +
    ggplot2::geom_line(data = sim_norm_mean_sd,
                       mapping = ggplot2::aes(x = qq_x, y = qq_y),
                       color = "indianred3", size = 0.75) +
    ggplot2::facet_wrap(~sim_run_number) +
    ggplot2::coord_cartesian(xlim = c(global_min, global_max))

  return(sim_norm_all_plot)
}
