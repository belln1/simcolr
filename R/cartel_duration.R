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
  df$lduration <- log(df$duration+1)
  df <- df %>%
    dplyr::rename(industry = col) %>%
    group_by(industry) %>%
    arrange(industry, start) %>%
    relocate(industry, start, end, duration)
  return(df)
}

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


# Create Paneldata  --------------------------------------------------------------

get_endtimes_vector <- function(in_cartel) {
  d <- lead(in_cartel)
  d[is.na(d)] <- 0
  e <- as.numeric((in_cartel - d) == 1)
}
get_rep_off <- function(in_cartel) {
  e <- get_endtimes_vector(in_cartel)
  ec <- cumsum(e)
  lec <- lag(ec)
  lec[is.na(lec)] <- 0
  rep <- ifelse(lec>0, 1, 0)
}

# Create Paneldata
#'
#' Function to construct a panel dataset from sample and population of collusive and not collusive states over time
#' @param cartels_population collusive and not collusive states of industries (population)
#' @param cartels_detected detected and undetected collusive and not collusive states of industries (sample)
#' @param parms parameter combinations used for simulation
#' @param interest_r stochastic values of interest rate over time
#' @param sigmas probability of detection over time
#'
#' @return panel dataset with cartels
#' @examples
#' sim_list <- sim_col_r();
#' df_panel <- get_paneldata(sim_list$cartels_population, sim_list$cartels_detected, sim_list$parms, sim_list$interest_r, sim_list$sigmas)
#' @export
get_paneldata <- function(cartels_population, cartels_detected, parms, r_all, sigma_all) {
  parms$parm_id <- 1:nrow(parms)

  # # Convert the 3D array into a long dataframe
  df_sample <- reshape2::melt(cartels_detected)
  colnames(df_sample) <- c("period", "industry_id", "parm_id", "detected")

  df_pop <- reshape2::melt(cartels_population)
  colnames(df_pop) <- c("period", "industry_id", "parm_id", "in_cartel")

  df_sample$ind_ID <- paste(df_sample$parm_id, df_sample$industry_id, sep="_")
  df_sample$in_cartel <- df_pop$in_cartel

  # Read in changing variables (r)
  r_df <- reshape2::melt(r_all)
  colnames(r_df) <- c("period", "industry_id", "parm_id", "r")
  # Create the ind_ID column to match your existing dataframe
  r_df$ind_ID <- paste(r_df$parm_id, r_df$industry_id, sep="_")
  # # Remove the now-unnecessary dimension columns
  r_df <- r_df[, c("period", "ind_ID", "r")]
  # Combine df with r_df
  df <- df_sample %>%
    left_join(r_df, by=c("ind_ID", "period"))

  # Read in changing sigmas
  sigmas_df <- reshape2::melt(sigma_all)
  colnames(sigmas_df) <- c("period", "industry_id", "parm_id", "sigma_t")
  sigmas_df$ind_ID <- paste(sigmas_df$parm_id, sigmas_df$industry_id, sep="_")
  sigmas_df <- sigmas_df[, c("period", "ind_ID", "sigma_t")]
  df1 <- df %>%
    left_join(sigmas_df, by=c("ind_ID", "period")) %>%
    mutate(sigma_all_t = get_sigma_all_t(sigma_t))

  # Combine df with parms
  df2 <- df1 %>%
    left_join(parms, by=c("parm_id")) %>%
    select(-c(sigma_start, parm_id, industry_id))

  # Add Repeat Offender
  df3 <- df2 %>%
    group_by(ind_ID) %>%
    mutate(rep_off = get_rep_off(detected)) %>%
    ungroup() %>%
    relocate(ind_ID, period, in_cartel, detected, rep_off, n_firms, sigma_all_t)
}
