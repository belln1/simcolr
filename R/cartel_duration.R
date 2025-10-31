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
#' Function to combine paneldata cartels, detected cartels, leniency cases returned by function sim_col into a cross-sectional dataset with duration
#' @param cartel_dates binary collusive state
#' @param detect_dates binary collusive state that eventually got detected
#' @param leniency_dates binary collusive state that eventually got found due to leniency application
#'
#' @return cross-sectional dataset with duration
#' @examples
#' sim_list <- sim_col();
#' df <- get_cartel_duration(sim_list$cartels, sim_list$detection, sim_list$leniency);
#' @export
get_cartel_duration <- function(cartels_detected, cartels_undetected, r_all, delta_all, parms, model){
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
  cartels_duration <- df
}



# Functions to extend dataset for estimations --------------------------------------------------------------

# Add industries without cartels, used for Heckman Sample Selection Correction
add_non_collusive_industries <- function(parms, n_industries, cartels_duration) {
  parms$parm_id <- 1:nrow(parms)
  data_grid <- expand.grid(industry = 1:n_industries, parm_id = parms$parm_id)
  data_grid <- data_grid[order(data_grid$industry, data_grid$parm_id),]

  # Join unique parms with basic grid
  data_parms <- full_join(parms, data_grid, by="parm_id")

  # Join parms_grid with all cartels
  data_all <- full_join(cartels_duration, data_parms)  #by all parms

  # Add unique industry ID
  df <- data_all %>%
    replace(is.na(data_all), 0) %>%
    relocate(industry, parm_id, cartel) %>%
    group_by(parm_id, industry) %>%
    dplyr::mutate(industry_id = cur_group_id()) %>%
    arrange(parm_id, industry, start)
  data_all <- df
  data_all$sigma_all_t  <- get_sigma_all_t(data_all$sigma_start)
  data_all
}

# Add nonlinear variables, used for Lasso CV - Model 1 and 2
add_nonlinears <- function(data, model) {
  if (model==3) {
    data <- data %>%
      mutate(gamma2 = gamma^2,
             gamma3 = gamma^3,
             nfirms_theta = n_firms * theta)
  }
  data <- data %>%
    mutate(nfirms2 = n_firms^2,
           nfirms3 = n_firms^3,
           sigma2 = sigma_all_t^2,
           sigma3 = sigma_all_t^3,
           nfirms_sigma = n_firms * sigma_all_t) %>%
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
