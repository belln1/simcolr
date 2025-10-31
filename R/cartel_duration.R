# Functions to calculate cartel durations  --------------------------------------------------------------

get_starttimes <- function(cartels) {
  d <- dplyr::lag(cartels)
  d[is.na(d)] = 0
  e <- as.numeric((cartels - d) == 1)
  starttimes <- matrix(e, ncol = ncol(cartels))
}

get_endtimes <- function(cartels) {
  d <- dplyr::lead(cartels)
  d[is.na(d)] = 0
  e <- as.numeric((cartels - d) == 1)
  endtimes <- matrix(e, ncol = ncol(cartels))
}

get_cartel_duration <- function(cartels) {
  starttimes <- get_starttimes(cartels)
  endtimes <- get_endtimes(cartels)
  start <- as_tibble(which(starttimes==1, arr.ind = TRUE))
  start <- dplyr::rename(start, "start" = row)
  end <- as_tibble(which(endtimes==1, arr.ind = TRUE))
  end <- dplyr::rename(end, "end" = row)
  df <- cbind(start, end=end$end)
  df$duration <- df$end - df$start + 1
  # Logarithm of duration: log(n+1)
  df$lduration <- log(df$duration+1)
  df <- df %>%
    dplyr::rename(industry = col) %>%
    group_by(industry) %>%
    arrange(industry, start) %>%  # order rows
    relocate(industry, start, end, duration)  # order columns
  return(df)
}

# get_r_mean <- function(r_all, industry, parm_id, start, end){
#   r_array <- r_all[start:end, industry, parm_id]
#   return(mean(r_array))
# }
# # get_r_mean(r_all)
#
# add_r_sumstats <- function(cartels_duration, r_all){
#   cd <- cartels_duration %>%
#     mutate(r_mean = get_r_mean(r_all, industry, parm_id, start, end))
# }

calculate_mean_delta <- function(cartel_duration, delta_all) {
  cartel_duration$mean_delta <- mapply(
    function(start, end, industry, parm) {
      mean(delta_all[start:end, industry, parm], na.rm = TRUE)
    },
    start = cartel_duration$start,
    end = cartel_duration$end,
    industry = cartel_duration$industry,
    parm = cartel_duration$parm_id
  )
  return(cartel_duration)
}

calculate_mean_r <- function(cartel_duration, r_all) {
  cartel_duration$mean_r <- mapply(
    function(start, end, industry, parm) {
      mean(r_all[start:end, industry, parm], na.rm = TRUE)
    },
    start = cartel_duration$start,
    end = cartel_duration$end,
    industry = cartel_duration$industry,
    parm = cartel_duration$parm_id
  )
  return(cartel_duration)
}

calculate_var_r <- function(cartel_duration, r_all) {
  cartel_duration$var_r <- mapply(
    function(start, end, industry, parm) {
      var(r_all[start:end, industry, parm], na.rm = TRUE)
    },
    start = cartel_duration$start,
    end = cartel_duration$end,
    industry = cartel_duration$industry,
    parm = cartel_duration$parm_id
  )
  return(cartel_duration)
}

#cartels = cartels_detected
get_enforcement_duration <- function(cartels, parms, model) {
  cd <- apply(cartels, MARGIN=3, FUN=get_cartel_duration)
  for (i in 1:nrow(parms)) {
    cd[[i]]$n_firms <- parms$n_firms[i]
    cd[[i]]$sigma_start <- parms$sigma_start[i]
    cd[[i]]$sigma_all_t <- get_sigma_all_t(parms$sigma_start[i])
    cd[[i]]$parm_id <- i
    if (model==3){
      cd[[i]]$theta <- parms$theta[i]
      cd[[i]]$gamma <- parms$gamma[i]
      cd[[i]]$structured <- parms$structured[i]
    }
  }
  cartels_duration <- do.call(rbind, cd)
}

#' Cartel Duration
#'
#' Function to combine paneldata cartels and detected cartels returned by function sim_col_r into a cross-sectional dataset with duration
#' @param cartels_detected binary collusive state that eventually got detected
#' @param cartels_undetected binary collusive state that never got detected
#' @param r_all numeric stochastic interest rates
#' @param delta_all numeric discount factor based on interest rates
#' @param model int 1, 2, 3
#'
#' @return cross-sectional dataset with duration
#' @examples
#' sim_list <- sim_col_r();
#' df <- get_cartel_duration(sim_list$cartels_detected, sim_list$cartels_undetected, sim_list$interest_r, sim_list$deltas, sim_list$parms, model=1)
#' @export
get_durations <- function(cartels_detected, cartels_undetected, r_all, delta_all, parms, model){
  cartels_detected_duration <- get_enforcement_duration(cartels_detected, parms, model)
  df <- cartels_detected_duration %>%
    dplyr::mutate(detected = 1,
                  nTc = 1) %>%
    group_by(industry, parm_id) %>%
    arrange(start) %>%
    dplyr::mutate(nTc = cumsum(nTc)) %>%
    relocate(industry, parm_id, detected, nTc) %>%
    arrange(parm_id, industry, nTc)
  cartels_detected_duration <- df

  cartels_undetected_duration <- get_enforcement_duration(cartels_undetected, parms, model)
  df <- cartels_undetected_duration %>%
    dplyr::mutate(detected = 0,
                  nTc = 0) %>%
    relocate(industry, parm_id, detected, nTc) %>%
    arrange(parm_id, industry, nTc)
  cartels_undetected_duration <- df

  cartels_duration <- rbind(cartels_detected_duration, cartels_undetected_duration)
  # Cartel id is unique in industry
  df <- cartels_duration %>%
    mutate(cartel = 1,
           in_cartel = 1) %>%
    group_by(parm_id, industry) %>%
    arrange(start) %>%
    dplyr::mutate(cartel = cumsum(cartel)) %>%
    arrange(parm_id, industry, start) %>%
    dplyr::mutate(nTc = ifelse((detected == 0 & cartel > 1), lag(nTc), nTc),
                  rep_off = ifelse(nTc > 1, 1, 0)) %>%
    arrange(industry, parm_id, start) %>%
    relocate(parm_id, industry, cartel, detected, nTc, rep_off, start)
  data1 <- calculate_mean_r(df, r_all)
  data1 <- calculate_var_r(data1, r_all)
  data1$var_r <- ifelse(is.na(data1$var_r), 0, data1$var_r)
  data1 <- calculate_mean_delta(data1, delta_all)
  cartels_duration <- data1 %>%
    select(-sigma_start) %>%
    rename(sigma = sigma_all_t)
}



# Functions to extend dataset for estimations --------------------------------------------------------------

# Add nonlinear variables, used for Lasso CV and Random Forest
#' Nonlinear Variables
#'
#' Function to add nonlinear interdependenciens
#' @param data cross-sectional cartel dataset
#' @param model int 1, 2, 3
#'
#' @return cross-sectional cartel dataset with nonlinear interdependencies
#' @examples
#' sim_list <- sim_col_r();
#' df <- get_cartel_duration(sim_list$cartels_detected, sim_list$cartels_undetected, sim_list$interest_r, sim_list$deltas, sim_list$parms, model=1)
#' df <- add_nonlinears(df, model=1)
#' @export
add_nonlinears <- function(data, model) {
  if (model==3) {
    data <- data %>%
      mutate(nfirms_theta = n_firms * theta)
  }
  data <- data %>%
    mutate(nfirms_sigma = n_firms * sigma) %>%
    arrange(industry, parm_id, start)
}

get_sumstats <- function(df){
  df_stats <- describe(df, fast = FALSE)
  df_stats <- select(df_stats, mean, median, sd, min, max, skew, n)
  df_stats$mean <- round(df_stats$mean,2)
  df_stats$median <- round(df_stats$median,2)
  df_stats$sd <- round(df_stats$sd,2)
  df_stats$min <- round(df_stats$min,2)
  df_stats$max <- round(df_stats$max,2)
  df_stats$skew <- round(df_stats$skew,2)
  k <- kbl(df_stats, "latex", booktabs = T, linesep = "")
}
