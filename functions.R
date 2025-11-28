## Pre-Processing Functions

# Function to generate missing times
generate_missing_times <- function(n_missing, mean_time, sd_time) {
  map2_dbl(mean_time[1:n_missing], sd_time[1:n_missing], ~ rnorm(1, .x, .y))
}

extract_excluded_couples <- function(exclusion_reasons, line_pattern) {
  # Find the line that matches the pattern
  matching_line <- grep(line_pattern, exclusion_reasons, value = TRUE)
  
  if(length(matching_line) > 0) {
    # Extract the text after the colon
    numbers_string <- trimws(sub(".*:", "", matching_line))
    
    # Remove surrounding quotes and square brackets
    numbers_string <- gsub('^"?\\[?1\\]?"?\\s*"?', '', numbers_string)
    numbers_string <- gsub('"?$', '', numbers_string)
    
    # Split numbers and convert to numeric
    if(nchar(numbers_string) > 0) {
      return(as.numeric(unlist(strsplit(numbers_string, ", "))))
    }
  }
  
  return(numeric(0))
}

safe_kalman <- function(x) {
  if(sum(!is.na(x)) >= 3) {
    # Suppress warnings about convergence
    return(suppressWarnings(na_kalman(ts(x))))
  } else {
    return(x)
  }
}

extract_tech_error <- function(exclusion_reasons, line_pattern) {
  # Find the line that matches the pattern
  matching_line <- grep(line_pattern, exclusion_reasons, value = TRUE)
  
  if(length(matching_line) > 0) {
    # Remove quotes and brackets
    cleaned_line <- gsub('^"?\\[1\\]"?\\s*', '', matching_line)
    cleaned_line <- gsub('"?$', '', cleaned_line)
    
    # Extract the number after the colon
    numbers_string <- trimws(sub(".*: *", "", cleaned_line))
    
    # Convert to numeric
    return(as.numeric(numbers_string))
  }
  
  return(numeric(0))
}

identify_nonresponding_partners <- function(data, min_responses_per_day = 2) {
  # First check: Find couples where at least one partner has zero responses
  zero_response_partners <- ES_data %>%
    group_by(CoupleID, Role) %>%
    summarise(
      total_responses = sum(Responded, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(CoupleID) %>%
    filter(any(total_responses == 0) | n() == 1) %>%  # Added n() == 1 to catch missing partners
    pull(CoupleID) %>%
    unique()
  
  # Second check: Find couples where neither partner has a day with min_responses_per_day responses
  insufficient_daily_responses <- data %>%
    group_by(CoupleID, Role, StudyDay) %>%
    summarise(
      responses_per_day = sum(Responded, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(CoupleID, Role) %>%
    summarise(
      has_good_day = any(responses_per_day >= min_responses_per_day),
      .groups = 'drop'
    ) %>%
    group_by(CoupleID) %>%
    filter(!all(has_good_day)) %>%
    pull(CoupleID) %>%
    unique()
  
  # Combine both exclusion criteria
  excluded_couples <- unique(c(zero_response_partners, insufficient_daily_responses))
  
  # Print summary of excluded couples
  if(length(excluded_couples) > 0) {
    message("\nExcluded couples summary:")
    
    # Print detailed response patterns for excluded couples
    response_summary <- data %>%
      filter(CoupleID %in% excluded_couples) %>%
      group_by(CoupleID, Role, StudyDay) %>%
      summarise(
        responses_per_day = sum(Responded, na.rm = TRUE),
        .groups = 'drop'
      )
    
    for(couple in sort(unique(response_summary$CoupleID))) {
      message("\nCouple ", couple, " response pattern:")
      print(response_summary %>% 
              filter(CoupleID == couple) %>%
              arrange(Role, StudyDay))
    }
    
    message("\nExclusion reasons:")
    if(length(zero_response_partners) > 0) {
      message("- Couples with zero responses from at least one partner: ", 
              paste(sort(zero_response_partners), collapse = ", "))
    }
    if(length(insufficient_daily_responses) > 0) {
      message("- Couples without a day of ", min_responses_per_day, 
              "+ responses: ", paste(sort(insufficient_daily_responses), collapse = ", "))
    }
  } else {
    message("No couples met exclusion criteria.")
  }
  
  return(excluded_couples)
}

preprocess_ES_data <- function(data_path, scales, scales_full, min_responses_per_day = 2, time_threshold_minutes = 3) {
  # Read and initial preprocessing of ES data
  # Read data
  ES_data <- read.csv(data_path)
  
  # Add diagnostics
  initial_couples <- unique(ES_data$CoupleID)
  print(paste("Initial number of couples:", length(initial_couples)))
  
  # Diagnostic filtering of couples
  zero_response_partners <- ES_data %>%
    group_by(CoupleID, Role) %>%
    summarise(
      total_responses = sum(Responded, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(CoupleID) %>%
    filter(all(total_responses == 0)) %>%
    pull(CoupleID) %>%
    unique()
  
  print(paste("Couples excluded due to zero responses:", length(zero_response_partners)))
  print(paste("CoupleIDs excluded due to zero responses:", paste(zero_response_partners, collapse = ", ")))
  
  insufficient_daily_responses <- ES_data %>%
    group_by(CoupleID, Role, StudyDay) %>%
    summarise(
      responses_per_day = sum(Responded, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(CoupleID, Role) %>%
    summarise(
      has_good_day = any(responses_per_day >= min_responses_per_day),
      .groups = 'drop'
    ) %>%
    group_by(CoupleID) %>%
    filter(!all(has_good_day)) %>%
    pull(CoupleID) %>%
    unique()
  
  print(paste("Couples excluded due to insufficient daily responses:", length(insufficient_daily_responses)))
  print(paste("CoupleIDs excluded due to insufficient daily responses:", paste(insufficient_daily_responses, collapse = ", ")))
  
  # Identify and remove single-participant "couples"
  invalid_couples <- ES_data %>%
    group_by(CoupleID) %>%
    summarise(
      total_roles = n_distinct(Role),
      total_participants = n_distinct(ParticipantID)
    ) %>%
    filter(
      total_roles == 1,  # Must have both Mother and Father
      total_participants == 1  # Must have distinct participants
    ) %>%
    pull(CoupleID)
  
  print(paste("Invalid Couples (one respondent):", length(invalid_couples)))
  print(paste("Invalid CoupleIDs:", paste(invalid_couples, collapse = ", ")))
  
  # Combine exclusions
  excluded_couples <- unique(c(zero_response_partners, insufficient_daily_responses, invalid_couples))
  print(paste("Total couples excluded:", length(excluded_couples)))
  
  # Filter the dataset
  ES_data_filtered <- ES_data %>%
    filter(!CoupleID %in% excluded_couples)
  
  # Convert time threshold to hours
  time_threshold <- time_threshold_minutes / 60
  
  # Remove pings that are too close together, keeping the first ping
  ES_data <- ES_data_filtered %>%
    arrange(CoupleID, ParticipantID, StudyDay, SubmittedTime) %>%
    group_by(CoupleID, ParticipantID, StudyDay) %>%
    mutate(
      TimeDecimals = strptime(SubmittedTime, format = "%H:%M:%S"),
      TimeDecimals = as.numeric(format(TimeDecimals, "%H")) + 
        as.numeric(format(TimeDecimals, "%M")) / 60 + 
        as.numeric(format(TimeDecimals, "%S")) / 3600,
      time_diff = TimeDecimals - lag(TimeDecimals)
    ) %>%
    filter(is.na(time_diff) | time_diff >= time_threshold) %>%  # Keep first ping and pings that are far enough apart
    ungroup() %>%
    mutate(
      TimeDecimals = strptime(SubmittedTime, format = "%H:%M:%S"),
      TimeDecimals = as.numeric(format(TimeDecimals, "%H")) + 
        as.numeric(format(TimeDecimals, "%M")) / 60 + 
        as.numeric(format(TimeDecimals, "%S")) / 3600,
      TimeDecimalsContinuous = TimeDecimals + (StudyDay - 1) * 24,
      TimeDecimalsScaled = scale(TimeDecimalsContinuous)[,1],
      CoupleID = as.factor(CoupleID),
      RoleCoupleID = as.factor(interaction(CoupleID, Role)),
      series = RoleCoupleID,
      Role = as.factor(Role),
      StudyDay = as.numeric(StudyDay),
      Valence = Valence / 100,
      Arousal = Arousal / 100
    ) %>%
    group_by(StudyDay, RoleCoupleID) %>%
    mutate(time = row_number()) %>%
    ungroup() %>%
    group_by(RoleCoupleID) %>%
    mutate(
      Valence_s = scale(Valence)[,1],
      Arousal_s = scale(Arousal)[,1]
    )
  
  reference_times <- ES_data %>%
    filter(StudyDay != 1) %>%
    group_by(time) %>%
    summarise(mean_time = mean(TimeDecimals), sd_time = sd(TimeDecimals)) %>%
    filter(time != 7)
  
  # Process missing data
  missing_times_df_some_data <- ES_data %>%
    filter(StudyDay == 1) %>%
    group_by(RoleCoupleID, CoupleID, ParticipantID, series) %>%
    summarise(
      n = n(),
      n_missing = 6 - n,
      .groups = 'keep'
    ) %>%
    filter(n_missing > 0) %>%
    rowwise() %>%
    mutate(
      simulated_time = list(generate_missing_times(n_missing, reference_times$mean_time, reference_times$sd_time))
    ) %>%
    unnest(simulated_time) %>%
    ungroup()
  
  missing_times_df_no_data <- ES_data %>%
    group_by(RoleCoupleID, CoupleID, ParticipantID, series) %>%
    filter(all(StudyDay != 1)) %>%
    summarise(
      n_missing = 6,
      .groups = 'keep'
    ) %>%
    rowwise() %>%
    mutate(
      simulated_time = list(generate_missing_times(n_missing, reference_times$mean_time, reference_times$sd_time))
    ) %>%
    unnest(simulated_time) %>%
    ungroup()
  
  # Combine missing data
  missing_times_df <- bind_rows(missing_times_df_some_data, missing_times_df_no_data) %>%
    mutate(
      StudyDay = 1,
      TimeDecimals = simulated_time,
      Role = str_split(series, "[.]") %>% map_chr(2),
      CoupleID = str_split(series, "[.]") %>% map_chr(1),
      Responded = 0
    ) %>%
    dplyr::select(-simulated_time, -n_missing)
  
  # Add missing columns
  required_columns <- colnames(ES_data)
  missing_columns <- setdiff(required_columns, colnames(missing_times_df))
  for (col in missing_columns) {
    missing_times_df[[col]] <- NA
  }
  missing_times_df <- missing_times_df[required_columns]
  
  # Combine datasets
  combined_data <- rbind(ES_data, missing_times_df) %>%
    arrange(CoupleID, ParticipantID, StudyDay, TimeDecimals) %>%
    group_by(RoleCoupleID) %>%
    mutate(time = row_number()) %>%
    ungroup()
  
  # After combined datasets
  n_combined <- length(unique(combined_data$CoupleID))
  #print(paste("After combining datasets:", n_combined))
  
  # Show response patterns for couples with < 84 observations
  non_full_couples <- combined_data %>%
    group_by(CoupleID) %>%
    summarise(n = n()) %>%
    filter(n < 84) %>%
    pull(CoupleID)
  
  if(length(non_full_couples) > 0) {
    print("\nResponse patterns for couples with < 84 observations:")
    response_summary <- ES_data %>%
      filter(CoupleID %in% non_full_couples) %>%
      group_by(CoupleID, Role) %>%
      summarise(
        total_responses = sum(Responded, na.rm = TRUE),
        .groups = 'drop'
      )
    print(response_summary, n = Inf)
  }
  
  # Filter for full dataset
  full_data_from_couples <- combined_data %>%
    group_by(CoupleID) %>%
    summarise(n = n()) %>%
    filter(n >= 84)
  
  # After full dataset filter
  print(paste("Technical Error (see above):", length(unique(non_full_couples))))
  
  # Apply all filters and create final dataset
  d <- combined_data %>%
    filter(CoupleID %in% full_data_from_couples$CoupleID) %>%
    arrange(CoupleID, ParticipantID, StudyDay, TimeDecimals) %>%
    group_by(RoleCoupleID) %>%
    mutate(time = row_number()) %>%
    ungroup() 
    #left_join(scales, by = "ParticipantID") %>%
    #left_join(scales_full, by = "ParticipantID")
  
  #print(paste("After scales joins:", length(unique(d$CoupleID))))
  
  # Split and process by role
  d_mothers <- d %>%
    filter(Role == "Mother") %>%
    rename_with(~paste0("Mother", .), c(Valence, Arousal, Valence_s, Arousal_s,
                                        SubmittedTime, TimeDecimalsScaled, TimeDecimalsContinuous,
                                        SleepinessState)) %>%
                                        #Agree, Close, Conflict, Support, 
                                        #Undermining, Endorse, Division,
                                        #FullAgree, FullClose, FullConflict, FullSupport, 
                                        #FullUndermining, FullEndorse, FullDivision)) %>%
    rename(MotherLocation = Location)
  
  d_fathers <- d %>%
    filter(Role == "Father") %>%
    rename_with(~paste0("Father", .), c(Valence, Arousal, Valence_s, Arousal_s, 
                                        SubmittedTime, TimeDecimalsScaled, TimeDecimalsContinuous,
                                        SleepinessState)) %>%
                                        #Agree, Close, Conflict, Support, 
                                        #Undermining, Endorse, Division,
                                        #FullAgree, FullClose, FullConflict, FullSupport, 
                                        #FullUndermining, FullEndorse, FullDivision)) %>%
    rename(FatherLocation = Location)
  
  # Create multivariate dataset
  d_multivariate <- d_fathers %>%
    left_join(d_mothers, by = c("StudyDay", "CoupleID", "time")) %>%
    mutate(
      across(
        c(FatherLocation, MotherLocation),
        list(
          Latitude = ~as.numeric(sapply(strsplit(., ","), `[`, 1)),
          Longitude = ~as.numeric(sapply(strsplit(., ","), `[`, 2))
        )
      ),
      DistanceDiff = pmap_dbl(
        list(FatherLocation_Longitude, FatherLocation_Latitude,
             MotherLocation_Longitude, MotherLocation_Latitude),
        ~as.numeric(distm(c(..1, ..2), c(..3, ..4), fun = distHaversine))
      ),
      DistanceDiff_s = scale(DistanceDiff)[,1],
      
      MotherTimeDecimals = strptime(MotherSubmittedTime, format = "%H:%M:%S"),
      FatherTimeDecimals = strptime(FatherSubmittedTime, format = "%H:%M:%S"),
      
      MotherTimeDecimals = 
        as.numeric(format(MotherTimeDecimals, "%H")) + 
        as.numeric(format(MotherTimeDecimals, "%M")) / 60 + 
        as.numeric(format(MotherTimeDecimals, "%S")) / 3600,
      
      FatherTimeDecimals = 
        as.numeric(format(FatherTimeDecimals, "%H")) + 
        as.numeric(format(FatherTimeDecimals, "%M")) / 60 + 
        as.numeric(format(FatherTimeDecimals, "%S")) / 3600,
      
      MotherTimeDecimalsContinuous = MotherTimeDecimals + (StudyDay - 1) * 24,
      FatherTimeDecimalsContinuous = FatherTimeDecimals + (StudyDay - 1) * 24,
      
      TimeDifference = FatherTimeDecimalsContinuous - MotherTimeDecimalsContinuous,
      TimeDifference_s = scale(TimeDifference)[,1]
      ) %>%
    group_by(CoupleID, StudyDay) %>%
    arrange(CoupleID, time) %>%
    mutate(
      across(
        c(MotherArousal_s, MotherValence_s, 
          FatherArousal_s, FatherValence_s),
        list(
          Prev = ~lag(.),
          PrevPrev = ~lag(., 2),
          PrevPrevPrev = ~lag(., 3),
          PrevPrevPrevPrev = ~lag(., 4),
          PrevPrevPrevPrevPrev = ~lag(., 5)
        )
      )
    ) %>%
    ungroup()
    #mutate(
    #  MotherCoparenting = (MotherAgree + MotherClose + MotherSupport + 
    #                         MotherEndorse + MotherConflict + 
    #                         MotherUndermining + MotherDivision) / 7,
    #  FatherCoparenting = (FatherAgree + FatherClose + FatherSupport + 
    #                         FatherEndorse + FatherConflict + 
    #                         FatherUndermining + FatherDivision) /7,
    #  MotherCoparenting_s = scale(MotherCoparenting)[,1],
    #  FatherCoparenting_s = scale(FatherCoparenting)[,1],
    #  MotherFullCoparenting = (MotherFullAgree + MotherFullClose + MotherFullSupport + 
    #                             MotherFullEndorse + MotherFullConflict + 
    #                             MotherFullUndermining + MotherFullDivision) / 7,
    # FatherFullCoparenting = (FatherFullAgree + FatherFullClose + FatherFullSupport + 
    #                             FatherFullEndorse + FatherFullConflict + 
    #                             FatherFullUndermining + FatherFullDivision) /7,
    #  MotherFullCoparenting_s = scale(MotherFullCoparenting)[,1],
    #  FatherFullCoparenting_s = scale(FatherFullCoparenting)[,1]
    #)
  
  print(paste("Final couples in multivariate:", length(unique(d_multivariate$CoupleID))))
  
  return(d_multivariate)
}

#Function to extract relevant hypothesis data from models:
extract_relevant_data <- function(model, TestDescription, hypothesisformula) {
  
  hypothesis <- hypothesis(model, hypothesisformula)
  
  relevant_data <- round(hypothesis$hypothesis[, c("Estimate", "CI.Lower", "CI.Upper", "Evid.Ratio")], 2)
  
  relevant_data$TestDescription <- TestDescription
  
  relevant_data$Hypothesis <- hypothesisformula
  
  return(relevant_data)
}

extract_residual_valence_correlation <- function(model, TestDescription) {
  # Extract posterior samples
  post_samples <- as_draws_df(model)
  
  # Create a data frame with the correlation details
  relevant_data <- data.frame(
    Estimate = round(mean(post_samples$rescor__FatherValences__MotherValences), 2),
    CI.Lower = round(quantile(post_samples$rescor__FatherValences__MotherValences, prob = 0.025), 2),
    CI.Upper = round(quantile(post_samples$rescor__FatherValences__MotherValences, prob = 0.975), 2),
    Evid.Ratio = round(
      (sum(post_samples$rescor__FatherValences__MotherValences > 0) / 
         length(post_samples$rescor__FatherValences__MotherValences)) / 
        (sum(post_samples$rescor__FatherValences__MotherValences <= 0) / 
           length(post_samples$rescor__FatherValences__MotherValences)), 
      2
    ),
    TestDescription = paste(TestDescription),
    Hypothesis = "rescor__FatherValences__MotherValences > 0")
  
  rownames(relevant_data) <- NULL
  
  return(relevant_data)
}

extract_residual_arousal_correlation <- function(model, TestDescription) {
  # Extract posterior samples
  post_samples <- as_draws_df(model)
  
  # Create a data frame with the correlation details
  relevant_data <- data.frame(
    Estimate = round(mean(post_samples$rescor__FatherArousals__MotherArousals), 2),
    CI.Lower = round(quantile(post_samples$rescor__FatherArousals__MotherArousals, prob = 0.025), 2),
    CI.Upper = round(quantile(post_samples$rescor__FatherArousals__MotherArousals, prob = 0.975), 2),
    Evid.Ratio = round(
      (sum(post_samples$rescor__FatherArousals__MotherArousals > 0) / 
         length(post_samples$rescor__FatherArousals__MotherArousals)) / 
        (sum(post_samples$rescor__FatherArousals__MotherArousals <= 0) / 
           length(post_samples$rescor__FatherArousals__MotherArousals)), 
      2
    ),
    TestDescription = paste(TestDescription),
    Hypothesis = "rescor__FatherArousals__MotherArousals > 0")
  
  rownames(relevant_data) <- NULL
  
  return(relevant_data)
}

extract_all_tests <- function(model) {
  # General Difference between Leave Periods
  mother_valence_diff <- extract_relevant_data(model,
                                               "MotherValenceDiff",
                                               "MotherValences_LeaveMaternityLeave > MotherValences_LeavePaternityLeave")
  
  father_valence_diff <- extract_relevant_data(model,
                                               "FatherValenceDiff",
                                               "FatherValences_LeaveMaternityLeave > FatherValences_LeavePaternityLeave")
  
  mother_arousal_diff <- extract_relevant_data(model,
                                               "MotherArousalDiff",
                                               "MotherArousals_LeaveMaternityLeave < MotherArousals_LeavePaternityLeave")
  
  father_arousal_diff <- extract_relevant_data(model,
                                               "FatherArousalDiff",
                                               "FatherArousals_LeaveMaternityLeave < FatherArousals_LeavePaternityLeave")
  
  # Self-Regulation Tests
  mother_mat_self_reg <- extract_relevant_data(model,
                                               "MotherSelfRegMat",
                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s < 0")
  
  mother_pat_self_reg <- extract_relevant_data(model,
                                               "MotherSelfRegPat",
                                               "MotherValenceAcc_LeavePaternityLeave:MotherValence_s < 0")
  
  father_mat_self_reg <- extract_relevant_data(model,
                                               "FatherSelfRegMat",
                                               "FatherValenceAcc_LeaveMaternityLeave:FatherValence_s < 0")
  
  father_pat_self_reg <- extract_relevant_data(model,
                                               "FatherSelfRegPat",
                                               "FatherValenceAcc_LeavePaternityLeave:FatherValence_s < 0")
  
  # Momentum/Inertia Tests
  mother_mat_inertia <- extract_relevant_data(model,
                                              "MotherInertiaMat",
                                              "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel > 0")
  
  mother_pat_inertia <- extract_relevant_data(model,
                                              "MotherInertiaPat", 
                                              "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel > 0")
  
  father_mat_inertia <- extract_relevant_data(model,
                                              "FatherInertiaMat",
                                              "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel > 0")
  
  father_pat_inertia <- extract_relevant_data(model,
                                              "FatherInertiaPat",
                                              "FatherValenceAcc_LeavePaternityLeave:FatherValenceVel > 0")
  
  # Arousal Moderation Tests 
  mat_self_reg_mothers_arousal <- extract_relevant_data(model,
                                                        "MatSelfRegArousalMothers",
                                                        "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s > 0")
  
  pat_self_reg_mothers_arousal <- extract_relevant_data(model,
                                                        "PatSelfRegArousalMothers",
                                                        "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s > 0")
  
  mat_self_reg_fathers_arousal <- extract_relevant_data(model,
                                                        "MatSelfRegArousalFathers",
                                                        "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s > 0")
  
  pat_self_reg_fathers_arousal <- extract_relevant_data(model,
                                                        "PatSelfRegArousalFathers",
                                                        "FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s < 0")
  
  # Co-Regulation Tests
  mother_prox_effect_mat <- extract_relevant_data(model,
                                                  "MotherProxEffectMat",
                                                  "MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:isProximate > 0")
  
  mother_prox_effect_pat <- extract_relevant_data(model,
                                                  "MotherProxEffectPat",
                                                  "MotherValenceAcc_LeavePaternityLeave:FatherValence_s:isProximate < 0")
  
  father_prox_effect_mat <- extract_relevant_data(model,
                                                  "FatherProxEffectMat",
                                                  "FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:isProximate < 0")
  
  father_prox_effect_pat <- extract_relevant_data(model,
                                                  "FatherProxEffectPat",
                                                  "FatherValenceAcc_LeavePaternityLeave:MotherValence_s:isProximate < 0")
  
  # Co-Regulation Tests
  mother_prox_effect_change_matpat <- extract_relevant_data(model,
                                                  "mother_prox_effect_change_matpat",
                                                  "MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:isProximate > MotherValenceAcc_LeavePaternityLeave:FatherValence_s:isProximate")

  father_prox_effect_change_matpat <- extract_relevant_data(model, 
                                                  "father_prox_effect_change_matpat",
                                                  "FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:isProximate < FatherValenceAcc_LeavePaternityLeave:MotherValence_s:isProximate")
  
  
  # Comparing mothers and fathers during maternity leave
  mat_self_reg_compare <- extract_relevant_data(model,
                                                "MatSelfRegCompare",
                                                "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s > FatherValenceAcc_LeaveMaternityLeave:FatherValence_s")
  
  mat_inertia_compare <- extract_relevant_data(model,
                                               "MatInertiaCompare",
                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel < FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel")
  
  # Comparing mothers and fathers during paternity leave
  pat_self_reg_compare <- extract_relevant_data(model,
                                                "PatSelfRegCompare",
                                                "MotherValenceAcc_LeavePaternityLeave:MotherValence_s > FatherValenceAcc_LeavePaternityLeave:FatherValence_s")
  
  pat_inertia_compare <- extract_relevant_data(model,
                                               "PatInertiaCompare",
                                               "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel < FatherValenceAcc_LeavePaternityLeave:FatherValenceVel")
  
  # Comparing fathers between leave periods
  pat_self_reg_compare_fathers <- extract_relevant_data(model,
                                                        "PatSelfRegCompareFathers",
                                                        "FatherValenceAcc_LeaveMaternityLeave:FatherValence_s > FatherValenceAcc_LeavePaternityLeave:FatherValence_s")
  
  pat_inertia_compare_fathers <- extract_relevant_data(model,
                                                       "PatInertiaCompareFathers",
                                                       "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel < FatherValenceAcc_LeavePaternityLeave:FatherValenceVel")
  
  # Comparing mothers between leave periods
  pat_self_reg_compare_mothers <- extract_relevant_data(model,
                                                        "PatSelfRegCompareMothers",
                                                        "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s < MotherValenceAcc_LeavePaternityLeave:MotherValence_s")
  
  pat_inertia_compare_mothers <- extract_relevant_data(model,
                                                       "PatInertiaCompareMothers",
                                                       "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel > MotherValenceAcc_LeavePaternityLeave:MotherValenceVel")
  
  # Arousal moderation comparisons
  pat_inertia_compare_mothers_arousal <- extract_relevant_data(model,
                                                               "PatInertiaCompareMothersArousal",
                                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s < MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s")
  
  pat_inertia_compare_fathers_arousal <- extract_relevant_data(model,
                                                               "PatInertiaCompareFathersArousal",
                                                               "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s > FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s")
  
  # Cross-partner arousal comparisons
  mat_arousal_compare <- extract_relevant_data(model,
                                               "MatArousalCompare",
                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s < FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s")
  
  pat_arousal_compare <- extract_relevant_data(model,
                                               "PatArousalCompare",
                                               "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s > FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s")
  
  
  # Tests for emotional stability (velocity effects)
  mother_mat_stability <- extract_relevant_data(model,
                                                "MotherMatStability",
                                                "MotherValenceVel_LeaveMaternityLeave < 0")
  
  mother_pat_stability <- extract_relevant_data(model,
                                                "MotherPatStability",
                                                "MotherValenceVel_LeavePaternityLeave < 0")
  
  # Tests for velocity-based coupling
  mother_vel_coupling_mat <- extract_relevant_data(model,
                                                   "MotherVelCouplingMat",
                                                   "MotherValenceAcc_LeaveMaternityLeave:isProximate:FatherValenceVel < 0")
  
  mother_vel_coupling_pat <- extract_relevant_data(model,
                                                   "MotherVelCouplingPat",
                                                   "MotherValenceAcc_LeavePaternityLeave:isProximate:FatherValenceVel > 0")
  
  # Tests for arousal moderation of position effects
  mother_pos_arousal_mat <- extract_relevant_data(model,
                                                  "MotherPosArousalMat",
                                                  "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s:MotherArousal_s < 0")
  
  father_pos_arousal_mat <- extract_relevant_data(model,
                                                  "FatherPosArousalMat",
                                                  "FatherValenceAcc_LeaveMaternityLeave:FatherValence_s:FatherArousal_s < 0")
  
  mother_pos_arousal_pat <- extract_relevant_data(model,
                                                  "MotherPosArousalPat",
                                                  "MotherValenceAcc_LeavePaternityLeave:MotherValence_s:MotherArousal_s < 0")
  
  father_pos_arousal_pat <- extract_relevant_data(model,
                                                  "FatherPosArousalPat",
                                                  "FatherValenceAcc_LeavePaternityLeave:FatherValence_s:FatherArousal_s > 0")
  
  # Tests for combined effects of proximity and arousal
  mother_prox_arousal_mat <- extract_relevant_data(model,
                                                   "MotherProxArousalMat",
                                                   "(MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:isProximate + MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:isProximate:FatherArousal_s) > 0")
  
  father_prox_arousal_mat <- extract_relevant_data(model,
                                                   "FatherProxArousalMat",
                                                   "(FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:isProximate - FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:isProximate:MotherArousal_s) < 0")
  
  mother_prox_arousal_pat <- extract_relevant_data(model,
                                                   "MotherProxArousalPat",
                                                   "(MotherValenceAcc_LeavePaternityLeave:FatherValence_s:isProximate + MotherValenceAcc_LeavePaternityLeave:FatherValence_s:isProximate:FatherArousal_s) < 0")
  
  father_prox_arousal_pat <- extract_relevant_data(model,
                                                   "FatherProxArousalPat",
                                                   "(FatherValenceAcc_LeavePaternityLeave:MotherValence_s:isProximate - FatherValenceAcc_LeavePaternityLeave:MotherValence_s:isProximate:MotherArousal_s) < 0")
  
  # Tests for asymmetry in arousal effects between leave periods
  arousal_asymmetry_mothers <- extract_relevant_data(model,
                                                     "ArousalAsymmetryMothers",
                                                     "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s > MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s")
  
  arousal_asymmetry_fathers <- extract_relevant_data(model,
                                                     "ArousalAsymmetryFathers",
                                                     "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s > FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s")
  
  father_vel_coupling_mat <- extract_relevant_data(model,
                                                   "FatherVelCouplingMat",
                                                   "FatherValenceAcc_LeaveMaternityLeave:isProximate:MotherValenceVel > 0")
  
  father_vel_coupling_pat <- extract_relevant_data(model,
                                                   "FatherVelCouplingPat", 
                                                   "FatherValenceAcc_LeavePaternityLeave:isProximate:MotherValenceVel > 0")
  
  # Co-Regulation Comparison of interpersonal emotions:
  MotherFatherProxEffectMatCompare <- extract_relevant_data(CoupledOscillatorModel,
                                                            "MotherFatherProxEffectMatCompare",
                                                            "MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:isProximate > FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:isProximate")
  
  MotherFatherProxEffectPatCompare <- extract_relevant_data(CoupledOscillatorModel,
                                                            "MotherFatherProxEffectPatCompare",
                                                            "MotherValenceAcc_LeavePaternityLeave:FatherValence_s:isProximate > FatherValenceAcc_LeavePaternityLeave:MotherValence_s:isProximate")
  
  # Combine all hypothesis tests into a dataframe
  all_tests <- rbind(
    # General differences
    mother_valence_diff,
    father_valence_diff,
    mother_arousal_diff,
    father_arousal_diff,
    
    # Self-regulation tests
    mother_mat_self_reg, mother_pat_self_reg,
    father_mat_self_reg, father_pat_self_reg,
    
    # Momentum/Inertia tests  
    mother_mat_inertia, mother_pat_inertia,
    father_mat_inertia, father_pat_inertia,
    
    # Arousal moderation tests
    mat_self_reg_mothers_arousal, pat_self_reg_mothers_arousal,
    mat_self_reg_fathers_arousal, pat_self_reg_fathers_arousal,
    
    # Co-regulation tests
    mother_prox_effect_mat, mother_prox_effect_pat,
    father_prox_effect_mat, father_prox_effect_pat,
    
    # Mother vs Father comparisons during leave periods
    mat_self_reg_compare, mat_inertia_compare,
    pat_self_reg_compare, pat_inertia_compare,
    
    # Within-parent comparisons across leave periods
    pat_self_reg_compare_fathers, pat_inertia_compare_fathers,
    pat_self_reg_compare_mothers, pat_inertia_compare_mothers,
    
    # Arousal comparison tests
    pat_inertia_compare_mothers_arousal, pat_inertia_compare_fathers_arousal,
    mat_arousal_compare, pat_arousal_compare,
    
    # Stability tests
    mother_mat_stability, mother_pat_stability,
    
    # Velocity coupling tests
    mother_vel_coupling_mat, mother_vel_coupling_pat,
    father_vel_coupling_mat, father_vel_coupling_pat,
    
    # Position-Arousal interaction tests
    mother_pos_arousal_mat, father_pos_arousal_mat,
    mother_pos_arousal_pat, father_pos_arousal_pat,
    
    # Proximity-Arousal interaction tests
    mother_prox_arousal_mat, father_prox_arousal_mat,
    mother_prox_arousal_pat, father_prox_arousal_pat,
    
    mother_prox_effect_change_matpat, father_prox_effect_change_matpat,
    
    # Arousal asymmetry tests
    arousal_asymmetry_mothers, arousal_asymmetry_fathers,
    MotherFatherProxEffectMatCompare, MotherFatherProxEffectPatCompare
  )
  
  return(all_tests)
}

extract_all_tests_time_model <- function(model) {
  # General Difference between Leave Periods
  mother_valence_diff <- extract_relevant_data(model,
                                               "MotherValenceDiff",
                                               "MotherValences_LeaveMaternityLeave > MotherValences_LeavePaternityLeave")
  
  father_valence_diff <- extract_relevant_data(model,
                                               "FatherValenceDiff",
                                               "FatherValences_LeaveMaternityLeave > FatherValences_LeavePaternityLeave")
  
  mother_arousal_diff <- extract_relevant_data(model,
                                               "MotherArousalDiff",
                                               "MotherArousals_LeaveMaternityLeave < MotherArousals_LeavePaternityLeave")
  
  father_arousal_diff <- extract_relevant_data(model,
                                               "FatherArousalDiff",
                                               "FatherArousals_LeaveMaternityLeave < FatherArousals_LeavePaternityLeave")
  
  # Self-Regulation Tests
  mother_mat_self_reg <- extract_relevant_data(model,
                                               "MotherSelfRegMat",
                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s < 0")
  
  mother_pat_self_reg <- extract_relevant_data(model,
                                               "MotherSelfRegPat",
                                               "MotherValenceAcc_LeavePaternityLeave:MotherValence_s < 0")
  
  father_mat_self_reg <- extract_relevant_data(model,
                                               "FatherSelfRegMat",
                                               "FatherValenceAcc_LeaveMaternityLeave:FatherValence_s < 0")
  
  father_pat_self_reg <- extract_relevant_data(model,
                                               "FatherSelfRegPat",
                                               "FatherValenceAcc_LeavePaternityLeave:FatherValence_s < 0")
  
  # Momentum/Inertia Tests
  mother_mat_inertia <- extract_relevant_data(model,
                                              "MotherInertiaMat",
                                              "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel > 0")
  
  mother_pat_inertia <- extract_relevant_data(model,
                                              "MotherInertiaPat", 
                                              "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel > 0")
  
  father_mat_inertia <- extract_relevant_data(model,
                                              "FatherInertiaMat",
                                              "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel > 0")
  
  father_pat_inertia <- extract_relevant_data(model,
                                              "FatherInertiaPat",
                                              "FatherValenceAcc_LeavePaternityLeave:FatherValenceVel > 0")
  
  # Arousal Moderation Tests 
  mat_self_reg_mothers_arousal <- extract_relevant_data(model,
                                                        "MatSelfRegArousalMothers",
                                                        "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s > 0")
  
  pat_self_reg_mothers_arousal <- extract_relevant_data(model,
                                                        "PatSelfRegArousalMothers",
                                                        "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s > 0")
  
  mat_self_reg_fathers_arousal <- extract_relevant_data(model,
                                                        "MatSelfRegArousalFathers",
                                                        "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s > 0")
  
  pat_self_reg_fathers_arousal <- extract_relevant_data(model,
                                                        "PatSelfRegArousalFathers",
                                                        "FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s < 0")
  
  # Co-Regulation Tests
  mother_prox_effect_mat <- extract_relevant_data(model,
                                                  "MotherProxEffectMat",
                                                  "MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:isProximate > 0")
  
  mother_prox_effect_pat <- extract_relevant_data(model,
                                                  "MotherProxEffectPat",
                                                  "MotherValenceAcc_LeavePaternityLeave:FatherValence_s:isProximate < 0")
  
  father_prox_effect_mat <- extract_relevant_data(model,
                                                  "FatherProxEffectMat",
                                                  "FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:isProximate < 0")
  
  father_prox_effect_pat <- extract_relevant_data(model,
                                                  "FatherProxEffectPat",
                                                  "FatherValenceAcc_LeavePaternityLeave:MotherValence_s:isProximate < 0")
  
  # Comparing mothers and fathers during maternity leave
  mat_self_reg_compare <- extract_relevant_data(model,
                                                "MatSelfRegCompare",
                                                "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s > FatherValenceAcc_LeaveMaternityLeave:FatherValence_s")
  
  mat_inertia_compare <- extract_relevant_data(model,
                                               "MatInertiaCompare",
                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel < FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel")
  
  # Comparing mothers and fathers during paternity leave
  pat_self_reg_compare <- extract_relevant_data(model,
                                                "PatSelfRegCompare",
                                                "MotherValenceAcc_LeavePaternityLeave:MotherValence_s > FatherValenceAcc_LeavePaternityLeave:FatherValence_s")
  
  pat_inertia_compare <- extract_relevant_data(model,
                                               "PatInertiaCompare",
                                               "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel < FatherValenceAcc_LeavePaternityLeave:FatherValenceVel")
  
  # Comparing fathers between leave periods
  pat_self_reg_compare_fathers <- extract_relevant_data(model,
                                                        "PatSelfRegCompareFathers",
                                                        "FatherValenceAcc_LeaveMaternityLeave:FatherValence_s > FatherValenceAcc_LeavePaternityLeave:FatherValence_s")
  
  pat_inertia_compare_fathers <- extract_relevant_data(model,
                                                       "PatInertiaCompareFathers",
                                                       "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel < FatherValenceAcc_LeavePaternityLeave:FatherValenceVel")
  
  # Comparing mothers between leave periods
  pat_self_reg_compare_mothers <- extract_relevant_data(model,
                                                        "PatSelfRegCompareMothers",
                                                        "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s < MotherValenceAcc_LeavePaternityLeave:MotherValence_s")
  
  pat_inertia_compare_mothers <- extract_relevant_data(model,
                                                       "PatInertiaCompareMothers",
                                                       "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel > MotherValenceAcc_LeavePaternityLeave:MotherValenceVel")
  
  # Arousal moderation comparisons
  pat_inertia_compare_mothers_arousal <- extract_relevant_data(model,
                                                               "PatInertiaCompareMothersArousal",
                                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s < MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s")
  
  pat_inertia_compare_fathers_arousal <- extract_relevant_data(model,
                                                               "PatInertiaCompareFathersArousal",
                                                               "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s > FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s")
  
  # Cross-partner arousal comparisons
  mat_arousal_compare <- extract_relevant_data(model,
                                               "MatArousalCompare",
                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s < FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s")
  
  pat_arousal_compare <- extract_relevant_data(model,
                                               "PatArousalCompare",
                                               "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s > FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s")
  
  
  # Tests for emotional stability (velocity effects)
  mother_mat_stability <- extract_relevant_data(model,
                                                "MotherMatStability",
                                                "MotherValenceVel_LeaveMaternityLeave < 0")
  
  mother_pat_stability <- extract_relevant_data(model,
                                                "MotherPatStability",
                                                "MotherValenceVel_LeavePaternityLeave < 0")
  
  # Tests for velocity-based coupling
  mother_vel_coupling_mat <- extract_relevant_data(model,
                                                   "MotherVelCouplingMat",
                                                   "MotherValenceAcc_LeaveMaternityLeave:isProximate:FatherValenceVel < 0")
  
  mother_vel_coupling_pat <- extract_relevant_data(model,
                                                   "MotherVelCouplingPat",
                                                   "MotherValenceAcc_LeavePaternityLeave:isProximate:FatherValenceVel > 0")
  
  # Tests for arousal moderation of position effects
  mother_pos_arousal_mat <- extract_relevant_data(model,
                                                  "MotherPosArousalMat",
                                                  "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s:MotherArousal_s < 0")
  
  father_pos_arousal_mat <- extract_relevant_data(model,
                                                  "FatherPosArousalMat",
                                                  "FatherValenceAcc_LeaveMaternityLeave:FatherValence_s:FatherArousal_s < 0")
  
  mother_pos_arousal_pat <- extract_relevant_data(model,
                                                  "MotherPosArousalPat",
                                                  "MotherValenceAcc_LeavePaternityLeave:MotherValence_s:MotherArousal_s < 0")
  
  father_pos_arousal_pat <- extract_relevant_data(model,
                                                  "FatherPosArousalPat",
                                                  "FatherValenceAcc_LeavePaternityLeave:FatherValence_s:FatherArousal_s > 0")
  
  # Tests for combined effects of proximity and arousal
  mother_prox_arousal_mat <- extract_relevant_data(model,
                                                   "MotherProxArousalMat",
                                                   "MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:isProximate:FatherArousal_s > 0")
  
  father_prox_arousal_mat <- extract_relevant_data(model,
                                                   "FatherProxArousalMat",
                                                   "FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:isProximate:MotherArousal_s > 0")
  
  mother_prox_arousal_pat <- extract_relevant_data(model,
                                                   "MotherProxArousalPat",
                                                   "MotherValenceAcc_LeavePaternityLeave:FatherValence_s:isProximate:FatherArousal_s < 0")
  
  father_prox_arousal_pat <- extract_relevant_data(model,
                                                   "FatherProxArousalPat",
                                                   "FatherValenceAcc_LeavePaternityLeave:MotherValence_s:isProximate:MotherArousal_s > 0")
  
  # Tests for asymmetry in arousal effects between leave periods
  arousal_asymmetry_mothers <- extract_relevant_data(model,
                                                     "ArousalAsymmetryMothers",
                                                     "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s > MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s")
  
  arousal_asymmetry_fathers <- extract_relevant_data(model,
                                                     "ArousalAsymmetryFathers",
                                                     "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s > FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s")
  
  father_vel_coupling_mat <- extract_relevant_data(model,
                                                   "FatherVelCouplingMat",
                                                   "FatherValenceAcc_LeaveMaternityLeave:isProximate:MotherValenceVel > 0")
  
  father_vel_coupling_pat <- extract_relevant_data(model,
                                                   "FatherVelCouplingPat", 
                                                   "FatherValenceAcc_LeavePaternityLeave:isProximate:MotherValenceVel > 0")
  
  # Time-based coupling tests for fathers
  father_time_coupling_mat <- extract_relevant_data(model,
                                                    "FatherTimeCouplingMat",
                                                    "FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:isProximate:TimeDifference_s < 0")
  
  father_time_coupling_pat <- extract_relevant_data(model,
                                                    "FatherTimeCouplingPat",
                                                    "FatherValenceAcc_LeavePaternityLeave:MotherValence_s:isProximate:TimeDifference_s > 0")
  
  # Time-based coupling tests for mothers
  mother_time_coupling_mat <- extract_relevant_data(model,
                                                    "MotherTimeCouplingMat",
                                                    "MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:isProximate:TimeDifference_s < 0")
  
  mother_time_coupling_pat <- extract_relevant_data(model,
                                                    "MotherTimeCouplingPat",
                                                    "MotherValenceAcc_LeavePaternityLeave:FatherValence_s:isProximate:TimeDifference_s < 0")
  
  # Time-Arousal interaction tests
  father_time_arousal_mat <- extract_relevant_data(model,
                                                   "FatherTimeArousalMat",
                                                   "FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:isProximate:TimeDifference_s:MotherArousal_s < 0")
  
  father_time_arousal_pat <- extract_relevant_data(model,
                                                   "FatherTimeArousalPat",
                                                   "FatherValenceAcc_LeavePaternityLeave:MotherValence_s:isProximate:TimeDifference_s:MotherArousal_s > 0")
  
  mother_time_arousal_mat <- extract_relevant_data(model,
                                                   "MotherTimeArousalMat",
                                                   "MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:isProximate:TimeDifference_s:FatherArousal_s < 0")
  
  mother_time_arousal_pat <- extract_relevant_data(model,
                                                   "MotherTimeArousalPat",
                                                   "MotherValenceAcc_LeavePaternityLeave:FatherValence_s:isProximate:TimeDifference_s:FatherArousal_s > 0")
  
  # Combine all hypothesis tests into a dataframe
  all_tests <- rbind(
    # General differences
    mother_valence_diff,
    father_valence_diff,
    mother_arousal_diff,
    father_arousal_diff,
    
    # Self-regulation tests
    mother_mat_self_reg, mother_pat_self_reg,
    father_mat_self_reg, father_pat_self_reg,
    
    # Momentum/Inertia tests  
    mother_mat_inertia, mother_pat_inertia,
    father_mat_inertia, father_pat_inertia,
    
    # Arousal moderation tests
    mat_self_reg_mothers_arousal, pat_self_reg_mothers_arousal,
    mat_self_reg_fathers_arousal, pat_self_reg_fathers_arousal,
    
    # Co-regulation tests
    mother_prox_effect_mat, mother_prox_effect_pat,
    father_prox_effect_mat, father_prox_effect_pat,
    
    # Mother vs Father comparisons during leave periods
    mat_self_reg_compare, mat_inertia_compare,
    pat_self_reg_compare, pat_inertia_compare,
    
    # Within-parent comparisons across leave periods
    pat_self_reg_compare_fathers, pat_inertia_compare_fathers,
    pat_self_reg_compare_mothers, pat_inertia_compare_mothers,
    
    # Arousal comparison tests
    pat_inertia_compare_mothers_arousal, pat_inertia_compare_fathers_arousal,
    mat_arousal_compare, pat_arousal_compare,
    
    # Stability tests
    mother_mat_stability, mother_pat_stability,
    
    # Velocity coupling tests
    mother_vel_coupling_mat, mother_vel_coupling_pat,
    father_vel_coupling_mat, father_vel_coupling_pat,
    
    # Position-Arousal interaction tests
    mother_pos_arousal_mat, father_pos_arousal_mat,
    mother_pos_arousal_pat, father_pos_arousal_pat,
    
    # Proximity-Arousal interaction tests
    mother_prox_arousal_mat, father_prox_arousal_mat,
    mother_prox_arousal_pat, father_prox_arousal_pat,
    
    # Arousal asymmetry tests
    arousal_asymmetry_mothers, arousal_asymmetry_fathers,
    
    father_time_coupling_mat, father_time_coupling_pat,
    mother_time_coupling_mat, mother_time_coupling_pat,
    father_time_arousal_mat, father_time_arousal_pat,
    mother_time_arousal_mat, mother_time_arousal_pat
  )
  
  return(all_tests)
}


extract_all_tests_distant_parents <- function(model) {
  # General Difference between Leave Periods
  mother_valence_diff <- extract_relevant_data(model,
                                               "MotherValenceDiff",
                                               "MotherValences_LeaveMaternityLeave > MotherValences_LeavePaternityLeave")
  
  father_valence_diff <- extract_relevant_data(model,
                                               "FatherValenceDiff",
                                               "FatherValences_LeaveMaternityLeave > FatherValences_LeavePaternityLeave")
  
  mother_arousal_diff <- extract_relevant_data(model,
                                               "MotherArousalDiff",
                                               "MotherArousals_LeaveMaternityLeave < MotherArousals_LeavePaternityLeave")
  
  father_arousal_diff <- extract_relevant_data(model,
                                               "FatherArousalDiff",
                                               "FatherArousals_LeaveMaternityLeave < FatherArousals_LeavePaternityLeave")
  
  # Self-Regulation Tests
  mother_mat_self_reg <- extract_relevant_data(model,
                                               "MotherSelfRegMat",
                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s < 0")
  
  mother_pat_self_reg <- extract_relevant_data(model,
                                               "MotherSelfRegPat",
                                               "MotherValenceAcc_LeavePaternityLeave:MotherValence_s < 0")
  
  father_mat_self_reg <- extract_relevant_data(model,
                                               "FatherSelfRegMat",
                                               "FatherValenceAcc_LeaveMaternityLeave:FatherValence_s < 0")
  
  father_pat_self_reg <- extract_relevant_data(model,
                                               "FatherSelfRegPat",
                                               "FatherValenceAcc_LeavePaternityLeave:FatherValence_s < 0")
  
  # Momentum/Inertia Tests
  mother_mat_inertia <- extract_relevant_data(model,
                                              "MotherInertiaMat",
                                              "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel > 0")
  
  mother_pat_inertia <- extract_relevant_data(model,
                                              "MotherInertiaPat", 
                                              "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel > 0")
  
  father_mat_inertia <- extract_relevant_data(model,
                                              "FatherInertiaMat",
                                              "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel > 0")
  
  father_pat_inertia <- extract_relevant_data(model,
                                              "FatherInertiaPat",
                                              "FatherValenceAcc_LeavePaternityLeave:FatherValenceVel > 0")
  
  # Arousal Moderation Tests 
  mat_self_reg_mothers_arousal <- extract_relevant_data(model,
                                                        "MatSelfRegArousalMothers",
                                                        "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s > 0")
  
  pat_self_reg_mothers_arousal <- extract_relevant_data(model,
                                                        "PatSelfRegArousalMothers",
                                                        "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s > 0")
  
  mat_self_reg_fathers_arousal <- extract_relevant_data(model,
                                                        "MatSelfRegArousalFathers",
                                                        "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s > 0")
  
  pat_self_reg_fathers_arousal <- extract_relevant_data(model,
                                                        "PatSelfRegArousalFathers",
                                                        "FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s < 0")
  
  # Co-Regulation Tests (maintaining original names but now without isProximate)
  mother_prox_effect_mat <- extract_relevant_data(model,
                                                  "MotherProxEffectMat",
                                                  "MotherValenceAcc_LeaveMaternityLeave:FatherValence_s > 0")
  
  mother_prox_effect_pat <- extract_relevant_data(model,
                                                  "MotherProxEffectPat",
                                                  "MotherValenceAcc_LeavePaternityLeave:FatherValence_s < 0")
  
  father_prox_effect_mat <- extract_relevant_data(model,
                                                  "FatherProxEffectMat",
                                                  "FatherValenceAcc_LeaveMaternityLeave:MotherValence_s < 0")
  
  father_prox_effect_pat <- extract_relevant_data(model,
                                                  "FatherProxEffectPat",
                                                  "FatherValenceAcc_LeavePaternityLeave:MotherValence_s < 0")
  # Comparing mothers and fathers during maternity leave
  mat_self_reg_compare <- extract_relevant_data(model,
                                                "MatSelfRegCompare",
                                                "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s > FatherValenceAcc_LeaveMaternityLeave:FatherValence_s")
  
  mat_inertia_compare <- extract_relevant_data(model,
                                               "MatInertiaCompare",
                                               "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel < FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel")
  
  # Comparing mothers and fathers during paternity leave
  pat_self_reg_compare <- extract_relevant_data(model,
                                                "PatSelfRegCompare",
                                                "MotherValenceAcc_LeavePaternityLeave:MotherValence_s > FatherValenceAcc_LeavePaternityLeave:FatherValence_s")
  
  pat_inertia_compare <- extract_relevant_data(model,
                                               "PatInertiaCompare",
                                               "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel < FatherValenceAcc_LeavePaternityLeave:FatherValenceVel")
  
  # Comparing fathers between leave periods
  pat_self_reg_compare_fathers <- extract_relevant_data(model,
                                                        "PatSelfRegCompareFathers",
                                                        "FatherValenceAcc_LeaveMaternityLeave:FatherValence_s > FatherValenceAcc_LeavePaternityLeave:FatherValence_s")
  
  pat_inertia_compare_fathers <- extract_relevant_data(model,
                                                       "PatInertiaCompareFathers",
                                                       "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel < FatherValenceAcc_LeavePaternityLeave:FatherValenceVel")
  
  # Comparing mothers between leave periods
  pat_self_reg_compare_mothers <- extract_relevant_data(model,
                                                        "PatSelfRegCompareMothers",
                                                        "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s < MotherValenceAcc_LeavePaternityLeave:MotherValence_s")
  
  pat_inertia_compare_mothers <- extract_relevant_data(model,
                                                       "PatInertiaCompareMothers",
                                                       "MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel > MotherValenceAcc_LeavePaternityLeave:MotherValenceVel")
  
  # Tests for emotional stability (velocity effects)
  mother_mat_stability <- extract_relevant_data(model,
                                                "MotherMatStability",
                                                "MotherValenceVel_LeaveMaternityLeave < 0")
  
  mother_pat_stability <- extract_relevant_data(model,
                                                "MotherPatStability",
                                                "MotherValenceVel_LeavePaternityLeave < 0")
  
  # Tests for velocity-based coupling
  mother_vel_coupling_mat <- extract_relevant_data(model,
                                                   "MotherVelCouplingMat",
                                                   "MotherValenceAcc_LeaveMaternityLeave:FatherValenceVel < 0")
  
  mother_vel_coupling_pat <- extract_relevant_data(model,
                                                   "MotherVelCouplingPat",
                                                   "MotherValenceAcc_LeavePaternityLeave:FatherValenceVel > 0")
  
  father_vel_coupling_mat <- extract_relevant_data(model,
                                                   "FatherVelCouplingMat",
                                                   "FatherValenceAcc_LeaveMaternityLeave:MotherValenceVel > 0")
  
  father_vel_coupling_pat <- extract_relevant_data(model,
                                                   "FatherVelCouplingPat",
                                                   "FatherValenceAcc_LeavePaternityLeave:MotherValenceVel > 0")
  
  # Tests for arousal moderation of position effects
  mother_pos_arousal_mat <- extract_relevant_data(model,
                                                  "MotherPosArousalMat",
                                                  "MotherValenceAcc_LeaveMaternityLeave:MotherValence_s:MotherArousal_s < 0")
  
  father_pos_arousal_mat <- extract_relevant_data(model,
                                                  "FatherPosArousalMat",
                                                  "FatherValenceAcc_LeaveMaternityLeave:FatherValence_s:FatherArousal_s < 0")
  
  mother_pos_arousal_pat <- extract_relevant_data(model,
                                                  "MotherPosArousalPat",
                                                  "MotherValenceAcc_LeavePaternityLeave:MotherValence_s:MotherArousal_s < 0")
  
  father_pos_arousal_pat <- extract_relevant_data(model,
                                                  "FatherPosArousalPat",
                                                  "FatherValenceAcc_LeavePaternityLeave:FatherValence_s:FatherArousal_s > 0")
  
  # Proximity-Arousal interaction tests (maintaining original names but now without isProximate)
  mother_prox_arousal_mat <- extract_relevant_data(model,
                                                   "MotherProxArousalMat",
                                                   "MotherValenceAcc_LeaveMaternityLeave:FatherValence_s:FatherArousal_s > 0")
  
  father_prox_arousal_mat <- extract_relevant_data(model,
                                                   "FatherProxArousalMat",
                                                   "FatherValenceAcc_LeaveMaternityLeave:MotherValence_s:MotherArousal_s > 0")
  
  mother_prox_arousal_pat <- extract_relevant_data(model,
                                                   "MotherProxArousalPat",
                                                   "MotherValenceAcc_LeavePaternityLeave:FatherValence_s:FatherArousal_s < 0")
  
  father_prox_arousal_pat <- extract_relevant_data(model,
                                                   "FatherProxArousalPat",
                                                   "FatherValenceAcc_LeavePaternityLeave:MotherValence_s:MotherArousal_s > 0")
  
  # Tests for asymmetry in arousal effects between leave periods
  arousal_asymmetry_mothers <- extract_relevant_data(model,
                                                     "ArousalAsymmetryMothers",
                                                     "MotherValenceAcc_LeavePaternityLeave:MotherValenceVel:MotherArousal_s > MotherValenceAcc_LeaveMaternityLeave:MotherValenceVel:MotherArousal_s")
  
  arousal_asymmetry_fathers <- extract_relevant_data(model,
                                                     "ArousalAsymmetryFathers",
                                                     "FatherValenceAcc_LeaveMaternityLeave:FatherValenceVel:FatherArousal_s > FatherValenceAcc_LeavePaternityLeave:FatherValenceVel:FatherArousal_s")
  
  # Combine all hypothesis tests into a dataframe
  all_tests <- rbind(
    # General differences
    mother_valence_diff,
    father_valence_diff,
    mother_arousal_diff,
    father_arousal_diff,
    
    # Self-regulation tests
    mother_mat_self_reg, mother_pat_self_reg,
    father_mat_self_reg, father_pat_self_reg,
    
    # Momentum/Inertia tests  
    mother_mat_inertia, mother_pat_inertia,
    father_mat_inertia, father_pat_inertia,
    
    # Arousal moderation tests
    mat_self_reg_mothers_arousal, pat_self_reg_mothers_arousal,
    mat_self_reg_fathers_arousal, pat_self_reg_fathers_arousal,
    
    # Co-regulation tests (maintaining original names)
    mother_prox_effect_mat, mother_prox_effect_pat,
    father_prox_effect_mat, father_prox_effect_pat,
    
    # Mother vs Father comparisons during leave periods
    mat_self_reg_compare, mat_inertia_compare,
    pat_self_reg_compare, pat_inertia_compare,
    
    # Within-parent comparisons across leave periods
    pat_self_reg_compare_fathers, pat_inertia_compare_fathers,
    pat_self_reg_compare_mothers, pat_inertia_compare_mothers,
    
    # Stability tests
    mother_mat_stability, mother_pat_stability,
    
    # Velocity coupling tests
    mother_vel_coupling_mat, mother_vel_coupling_pat,
    father_vel_coupling_mat, father_vel_coupling_pat,
    
    # Position-Arousal interaction tests
    mother_pos_arousal_mat, father_pos_arousal_mat,
    mother_pos_arousal_pat, father_pos_arousal_pat,
    
    # Proximity-Arousal interaction tests
    mother_prox_arousal_mat, father_prox_arousal_mat,
    mother_prox_arousal_pat, father_prox_arousal_pat,
    
    # Arousal asymmetry tests
    arousal_asymmetry_mothers, arousal_asymmetry_fathers
  )
  
  return(all_tests)
}

# This is ugly as hell, apologies to everyone in advance, but here are functions to calculate consistency
ExtractConsistencyLowVLowA <- function(ArousalBin_thres, ValenceBin_thres) {
  consistency_results <- couple_transitions %>%
    filter(abs(ValenceBin) > ValenceBin_thres) %>%
    filter(ArousalBin < ArousalBin_thres) %>%
    group_by(Leave) %>%
    summarise(
      n_consistent = sum(
        (mean_valence_change > 0 & lb_valence_change > 0) |
          (mean_valence_change < 0 & ub_valence_change < 0)
      ),
      n = n(),
      consistency = n_consistent/n,
      ci = list(binom.test(n_consistent, n)$conf.int),
      consistency_lb = map_dbl(ci, ~.x[1]),
      consistency_ub = map_dbl(ci, ~.x[2]),
      evidence_ratio = (n_consistent + 1) / (n - n_consistent + 1)
    ) %>%
    select(-ci)
  
  return(consistency_results)
}

ExtractConsistencyHighVLowA <- function(ArousalBin_thres, ValenceBin_thres) {
  consistency_results <- couple_transitions %>%
    filter(ValenceBin > ValenceBin_thres) %>%
    filter(ArousalBin < ArousalBin_thres) %>%
    group_by(Leave) %>%
    summarise(
      n_consistent = sum(
        (mean_valence_change > 0 & lb_valence_change > 0) |
          (mean_valence_change < 0 & ub_valence_change < 0)
      ),
      n = n(),
      consistency = n_consistent/n,
      ci = list(binom.test(n_consistent, n)$conf.int),
      consistency_lb = map_dbl(ci, ~.x[1]),
      consistency_ub = map_dbl(ci, ~.x[2]),
      evidence_ratio = (n_consistent + 1) / (n - n_consistent + 1)
    ) %>%
    select(-ci)
  
  return(consistency_results)
}

ExtractConsistencyHighVHighA <- function(ArousalBin_thres, ValenceBin_thres) {
  consistency_results <- couple_transitions %>%
    filter(abs(ValenceBin) > ValenceBin_thres) %>%
    filter(ArousalBin > ArousalBin_thres) %>%
    group_by(Leave) %>%
    summarise(
      n_consistent = sum(
        (mean_valence_change > 0 & lb_valence_change > 0) |
          (mean_valence_change < 0 & ub_valence_change < 0)
      ),
      n = n(),
      consistency = n_consistent/n,
      ci = list(binom.test(n_consistent, n)$conf.int),
      consistency_lb = map_dbl(ci, ~.x[1]),
      consistency_ub = map_dbl(ci, ~.x[2]),
      evidence_ratio = (n_consistent + 1) / (n - n_consistent + 1)
    ) %>%
    select(-ci)
  
  return(consistency_results)
}

ExtractConsistencyLowVHighA <- function(ArousalBin_thres, ValenceBin_thres) {
  consistency_results <- couple_transitions %>%
    filter(ValenceBin < ValenceBin_thres) %>%
    filter(ArousalBin < ArousalBin_thres) %>%
    group_by(Leave) %>%
    summarise(
      n_consistent = sum(
        (mean_valence_change > 0 & lb_valence_change > 0) |
          (mean_valence_change < 0 & ub_valence_change < 0)
      ),
      n = n(),
      consistency = n_consistent/n,
      ci = list(binom.test(n_consistent, n)$conf.int),
      consistency_lb = map_dbl(ci, ~.x[1]),
      consistency_ub = map_dbl(ci, ~.x[2]),
      evidence_ratio = (n_consistent + 1) / (n - n_consistent + 1)
    ) %>%
    select(-ci)
  
  return(consistency_results)
}

create_posterior_plots <- function(model_posterior, model_prior) {
  Posterior <- as_draws_df(model_posterior)
  Prior <- as_draws_df(model_prior)
  
  # Positions data
  positions_data <- rbind(
    # Prior (combined)
    data.frame(
      density = c(Prior$b_FatherValences_LeaveMaternityLeave, Prior$b_MotherValences_LeaveMaternityLeave),
      type = "Prior",
      parent = "Prior",
      Leave = "Maternity Leave"
    ),
    data.frame(
      density = c(Prior$b_FatherValences_LeavePaternityLeave, Prior$b_MotherValences_LeavePaternityLeave),
      type = "Prior",
      parent = "Prior",
      Leave = "Paternity Leave"
    ),
    # Posteriors
    data.frame(
      density = c(Posterior$b_FatherValences_LeaveMaternityLeave, Posterior$b_MotherValences_LeaveMaternityLeave),
      type = "Posterior",
      parent = rep(c("Father", "Mother"), each = nrow(Posterior)),
      Leave = "Maternity Leave"
    ),
    data.frame(
      density = c(Posterior$b_FatherValences_LeavePaternityLeave, Posterior$b_MotherValences_LeavePaternityLeave),
      type = "Posterior",
      parent = rep(c("Father", "Mother"), each = nrow(Posterior)),
      Leave = "Paternity Leave"
    )
  )
  
  # Velocities data
  velocities_data <- rbind(
    # Prior (combined)
    data.frame(
      density = c(Prior$b_FatherValenceVel_LeaveMaternityLeave, Prior$b_MotherValenceVel_LeaveMaternityLeave),
      type = "Prior",
      parent = "Prior",
      Leave = "Maternity Leave"
    ),
    data.frame(
      density = c(Prior$b_FatherValenceVel_LeavePaternityLeave, Prior$b_MotherValenceVel_LeavePaternityLeave),
      type = "Prior",
      parent = "Prior",
      Leave = "Paternity Leave"
    ),
    # Posteriors
    data.frame(
      density = c(Posterior$b_FatherValenceVel_LeaveMaternityLeave, Posterior$b_MotherValenceVel_LeaveMaternityLeave),
      type = "Posterior",
      parent = rep(c("Father", "Mother"), each = nrow(Posterior)),
      Leave = "Maternity Leave"
    ),
    data.frame(
      density = c(Posterior$b_FatherValenceVel_LeavePaternityLeave, Posterior$b_MotherValenceVel_LeavePaternityLeave),
      type = "Posterior",
      parent = rep(c("Father", "Mother"), each = nrow(Posterior)),
      Leave = "Paternity Leave"
    )
  )
  
  # Accelerations data
  accelerations_data <- rbind(
    # Prior (combined)
    data.frame(
      density = c(Prior$b_FatherValenceAcc_LeaveMaternityLeave, Prior$b_MotherValenceAcc_LeaveMaternityLeave),
      type = "Prior",
      parent = "Prior",
      Leave = "Maternity Leave"
    ),
    data.frame(
      density = c(Prior$b_FatherValenceAcc_LeavePaternityLeave, Prior$b_MotherValenceAcc_LeavePaternityLeave),
      type = "Prior",
      parent = "Prior",
      Leave = "Paternity Leave"
    ),
    # Posteriors
    data.frame(
      density = c(Posterior$b_FatherValenceAcc_LeaveMaternityLeave, Posterior$b_MotherValenceAcc_LeaveMaternityLeave),
      type = "Posterior",
      parent = rep(c("Father", "Mother"), each = nrow(Posterior)),
      Leave = "Maternity Leave"
    ),
    data.frame(
      density = c(Posterior$b_FatherValenceAcc_LeavePaternityLeave, Posterior$b_MotherValenceAcc_LeavePaternityLeave),
      type = "Posterior",
      parent = rep(c("Father", "Mother"), each = nrow(Posterior)),
      Leave = "Paternity Leave"
    )
  )
  
  # Common theme
  theme_common <- theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size=20), 
          axis.text.x = element_text(size = 20, color = "black"),
          axis.title.x = element_text(size = 20, color = "black"),
          axis.text.y = element_text(size = 0),
          axis.ticks.y = element_line(linewidth = 0),
          axis.title.y = element_text(size = 0, color = "black"),
          strip.background = element_rect(color="white", fill="white", linewidth=1.5, linetype="solid"),
          strip.text.x = element_text(size = 20, color = "black"),
          legend.position = "none")
  
  # Plotting positions with facets
  Positions <- ggplot(positions_data, aes(x = density, fill = interaction(type, parent))) + 
    ggtitle('Prior-Posterior Updates') +
    geom_density(alpha = 0.9) +
    facet_wrap(~Leave) +
    scale_fill_manual(values = c("Prior.Prior" = "#440154FF",
                                 "Posterior.Father" = "#21908CFF",
                                 "Posterior.Mother" = "#f2cc84"),
                      labels = c("Prior", "Posterior Father", "Posterior Mother")) +
    theme_common +
    xlab('Position Parameters') +
    scale_x_continuous(breaks = seq(-2, 2, 1), limits = c(-2, 2))
  
  # Plotting velocities with facets
  Velocities <- ggplot(velocities_data, aes(x = density, fill = interaction(type, parent))) + 
    geom_density(alpha = 0.9) +
    facet_wrap(~Leave) +
    scale_fill_manual(values = c("Posterior.Father" = "#21908CFF",
                                 "Prior.Prior" = "#440154FF",
                                 "Posterior.Mother" = "#f2cc84"),
                      labels = c("Posterior Father", "Posterior Mother", "Prior")) +
    theme_common +
    theme(legend.text = element_text(size = 20, color = "black"),
          legend.position = c(0.5, 0.7),
          legend.title = element_blank()) +
    xlab('Velocity Parameters') +
    scale_x_continuous(breaks = seq(-2, 2, 1), limits = c(-2, 2))
  
  # Plotting accelerations with facets
  Accelerations <- ggplot(accelerations_data, aes(x = density, fill = interaction(type, parent))) + 
    geom_density(alpha = 0.9) +
    facet_wrap(~Leave) +
    scale_fill_manual(values = c("Prior.Prior" = "#440154FF",
                                 "Posterior.Father" = "#21908CFF",
                                 "Posterior.Mother" = "#f2cc84"),
                      labels = c("Prior", "Posterior Father", "Posterior Mother")) +
    theme_common +
    xlab('Acceleration Parameters') +
    scale_x_continuous(breaks = seq(-2, 2, 1), limits = c(-2, 2))
  
  # Combine all plots
  plot_grid <- cowplot::plot_grid(Positions, Velocities, Accelerations, 
                                  ncol = 1, rel_heights = c(1, 0.8, 0.8))
  
  return(plot_grid)
}

anonymize_data <- function(original_data) {
  
  couples <- unique(original_data$CoupleID)
  anonymized_data <- list()
  
  for (couple in couples) {
    couple_data <- original_data %>% filter(CoupleID == couple)
    new_couple_id <- sprintf("C%02d", which(couples == couple))
    n_rows <- nrow(couple_data)
    
    anon_couple <- couple_data
    
    # 1. Change the CoupleID
    anon_couple$CoupleID <- new_couple_id
    
    # 2. Apply small jitter to numeric values
    numeric_cols <- c("FatherValence_s", "MotherValence_s", 
                      "FatherArousal_s", "MotherArousal_s",
                      "FatherValenceVel", "MotherValenceVel",
                      "FatherValenceAcc", "MotherValenceAcc")
    
    for (col in numeric_cols) {
      # Skip if the column doesn't exist
      if (!(col %in% colnames(anon_couple))) next
      
      # Only apply noise to non-NA values
      non_na_indices <- which(!is.na(anon_couple[[col]]))
      
      if (length(non_na_indices) > 0) {
        col_sd <- sd(anon_couple[[col]][non_na_indices], na.rm = TRUE)
        
        if (is.na(col_sd) || col_sd == 0) next
        
        # Apply small noise - 2% of standard deviation
        noise_factor <- 0.02
        anon_couple[[col]][non_na_indices] <- anon_couple[[col]][non_na_indices] + 
          rnorm(length(non_na_indices), 0, col_sd * noise_factor)
      }
    }
    
    # 3. Shuffle a small percentage (15%) of study days
    if ("StudyDay" %in% colnames(anon_couple) && n_rows >= 5) {
      n_to_shuffle <- max(1, round(n_rows * 0.15))
      shuffle_indices <- sample(1:n_rows, n_to_shuffle)
      
      available_days <- unique(anon_couple$StudyDay)
      
      for (idx in shuffle_indices) {
        anon_couple$StudyDay[idx] <- sample(available_days, 1)
      }
    }
    
    anonymized_data[[couple]] <- anon_couple
  }
  
  # Combine all couples' data
  result <- bind_rows(anonymized_data)
  
  return(result)
}


# Function to calculate residual correlations by leave period
calculate_residual_correlations <- function(model, 
                                            mother_resp, 
                                            father_resp,
                                            mother_var,
                                            father_var) {
  # Get posterior predictions
  post_pred_mother <- posterior_epred(model, resp = mother_resp, ndraws = NULL)
  post_pred_father <- posterior_epred(model, resp = father_resp, ndraws = NULL)
  
  # Extract model data
  model_data <- model$data
  
  # Calculate residual correlations for each posterior draw
  n_draws <- nrow(post_pred_mother)
  correlation_results <- vector("list", n_draws)
  
  for (i in 1:n_draws) {
    # Calculate residuals for this draw
    resid_mother <- model_data[[mother_var]] - post_pred_mother[i, ]
    resid_father <- model_data[[father_var]] - post_pred_father[i, ]
    
    # Create temporary dataframe with residuals and grouping variables
    temp_df <- data.frame(
      resid_mother = resid_mother,
      resid_father = resid_father,
      Leave = model_data$Leave,
      isProximate = model_data$isProximate
    )
    
    # Calculate correlations by Leave and isProximate
    cors <- temp_df %>%
      group_by(Leave, isProximate) %>%
      summarise(
        correlation = cor(resid_mother, resid_father, use = "pairwise.complete.obs"),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(draw = i)
    
    correlation_results[[i]] <- cors
  }
  
  # Calculate correlations by Leave period (for co-located partners: isProximate == 1)
  correlation_by_leave <- bind_rows(correlation_results) %>%
    filter(isProximate == 1) %>%
    group_by(Leave, draw) %>%
    summarise(correlation = mean(correlation), .groups = "drop") %>%
    group_by(Leave) %>%
    summarise(
      median_r = median(correlation),
      lower_95 = quantile(correlation, 0.025),
      upper_95 = quantile(correlation, 0.975),
      prob_positive = mean(correlation > 0),
      prob_negative = mean(correlation < 0),
      evidence_ratio_positive = mean(correlation > 0) / mean(correlation < 0)
    )
  
  return(correlation_by_leave)
}

# Function to calculate residual correlations by leave period
calculate_residual_correlations_surrogate <- function(model, 
                                            mother_resp, 
                                            father_resp,
                                            mother_var,
                                            father_var) {
  # Get posterior predictions
  post_pred_mother <- posterior_epred(model, resp = mother_resp, ndraws = NULL)
  post_pred_father <- posterior_epred(model, resp = father_resp, ndraws = NULL)
  
  # Extract model data
  model_data <- model$data
  
  # Calculate residual correlations for each posterior draw
  n_draws <- nrow(post_pred_mother)
  correlation_results <- vector("list", n_draws)
  
  for (i in 1:n_draws) {
    # Calculate residuals for this draw
    resid_mother <- model_data[[mother_var]] - post_pred_mother[i, ]
    resid_father <- model_data[[father_var]] - post_pred_father[i, ]
    
    # Create temporary dataframe with residuals and grouping variables
    temp_df <- data.frame(
      resid_mother = resid_mother,
      resid_father = resid_father,
      Leave = model_data$Leave,
      isProximate = model_data$isProximate
    )
    
    # Calculate correlations by Leave and isProximate
    cors <- temp_df %>%
      group_by(Leave, isProximate) %>%
      summarise(
        correlation = cor(resid_mother, resid_father, use = "pairwise.complete.obs"),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(draw = i)
    
    correlation_results[[i]] <- cors
  }
  
  # Calculate correlations by Leave period (for co-located partners: isProximate == 1)
  correlation_by_leave <- bind_rows(correlation_results) %>%
    #filter(isProximate == 1) %>%
    group_by(Leave, draw) %>%
    summarise(correlation = mean(correlation), .groups = "drop") %>%
    group_by(Leave) %>%
    summarise(
      median_r = median(correlation),
      lower_95 = quantile(correlation, 0.025),
      upper_95 = quantile(correlation, 0.975),
      prob_positive = mean(correlation > 0),
      prob_negative = mean(correlation < 0),
      evidence_ratio_positive = mean(correlation > 0) / mean(correlation < 0)
    )
  
  return(correlation_by_leave)
}

# Function to calculate residual correlations by leave period
calculate_residual_correlations_farproximity <- function(model, 
                                                      mother_resp, 
                                                      father_resp,
                                                      mother_var,
                                                      father_var) {
  # Get posterior predictions
  post_pred_mother <- posterior_epred(model, resp = mother_resp, ndraws = NULL)
  post_pred_father <- posterior_epred(model, resp = father_resp, ndraws = NULL)
  
  # Extract model data
  model_data <- model$data
  
  # Calculate residual correlations for each posterior draw
  n_draws <- nrow(post_pred_mother)
  correlation_results <- vector("list", n_draws)
  
  for (i in 1:n_draws) {
    # Calculate residuals for this draw
    resid_mother <- model_data[[mother_var]] - post_pred_mother[i, ]
    resid_father <- model_data[[father_var]] - post_pred_father[i, ]
    
    # Create temporary dataframe with residuals and grouping variables
    temp_df <- data.frame(
      resid_mother = resid_mother,
      resid_father = resid_father,
      Leave = model_data$Leave
      #isProximate = model_data$isProximate
    )
    
    # Calculate correlations by Leave and isProximate
    cors <- temp_df %>%
      group_by(Leave) %>%
      summarise(
        correlation = cor(resid_mother, resid_father, use = "pairwise.complete.obs"),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(draw = i)
    
    correlation_results[[i]] <- cors
  }
  
  # Calculate correlations by Leave period (for co-located partners: isProximate == 1)
  correlation_by_leave <- bind_rows(correlation_results) %>%
    #filter(isProximate == 1) %>%
    group_by(Leave, draw) %>%
    summarise(correlation = mean(correlation), .groups = "drop") %>%
    group_by(Leave) %>%
    summarise(
      median_r = median(correlation),
      lower_95 = quantile(correlation, 0.025),
      upper_95 = quantile(correlation, 0.975),
      prob_positive = mean(correlation > 0),
      prob_negative = mean(correlation < 0),
      evidence_ratio_positive = mean(correlation > 0) / mean(correlation < 0)
    )
  
  return(correlation_by_leave)
}

create_multiple_surrogates <- function(data, n_surrogates = 2, seed_val = 20) {
  # Get unique couples
  couples <- unique(data$CoupleID)
  
  # Function to create one surrogate dataset with a given seed
  create_one_surrogate <- function(i) {  # Changed parameter name
    set.seed(seed_val + i - 1)  # Use seed_val from parent scope, offset by i
    shuffled_fathers <- sample(couples)
    
    # If any pair matches original, reshuffle
    while(any(couples == shuffled_fathers)) {
      shuffled_fathers <- sample(couples)
    }
    
    # Create surrogate dataset
    surrogate_data <- data
    
    # Initialize SurrogateFatherID column with original CoupleID
    surrogate_data$SurrogateFatherID <- surrogate_data$CoupleID
    
    # For each original couple
    for(j in seq_along(couples)) {  # Changed from i to j to avoid confusion
      orig_id <- couples[j]
      surr_id <- shuffled_fathers[j]
      
      # Get indices for the current couple
      orig_indices <- which(data$CoupleID == orig_id)
      surr_indices <- which(data$CoupleID == surr_id)
      
      # Replace father variables with surrogate father data
      surrogate_data$FatherValence_s[orig_indices] <- data$FatherValence_s[surr_indices]
      surrogate_data$FatherArousal_s[orig_indices] <- data$FatherArousal_s[surr_indices]
      surrogate_data$FatherValenceVel[orig_indices] <- data$FatherValenceVel[surr_indices]
      surrogate_data$FatherValenceAcc[orig_indices] <- data$FatherValenceAcc[surr_indices]
      
      # Assign the surrogate father ID to the current couple's rows
      surrogate_data$SurrogateFatherID[orig_indices] <- surr_id
    }
    
    # Add identifiers
    surrogate_data$SurrogateID <- paste0("S", seed_val + i - 1)
    surrogate_data$isSurrogate <- TRUE
    
    return(surrogate_data)
  }
  
  # Create all surrogate datasets using different seeds
  surrogate_list <- map(1:n_surrogates, create_one_surrogate)
  
  # Add original data with appropriate identifiers
  original_data <- data %>%
    mutate(
      SurrogateID = "Original",
      isSurrogate = FALSE,
      SurrogateFatherID = CoupleID
    )
  
  # Combine all datasets
  all_data <- bind_rows(surrogate_list) %>%
    bind_rows(original_data)
  
  return(all_data)
}




