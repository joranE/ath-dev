MAJ_INT <- c('WC','WSC','OWG','TDS')
tech_colors <- brewer.pal(n = 3,name = "Set1")
tech_colors <- setNames(tech_colors,c("C","F","FC"))
tech_labels <- c("Classic","Freestyle","Pursuit")

comparison_grp <- function(top,times,events){
  if (length(events == 1)) events <- c(events,events)
  elite_id <- DATA %>%
    filter(cat1 %in% events & rank <= top) %>%
    group_by(fisid) %>%
    summarise(n = n()) %>%
    filter(n >= times) %>%
    collect()
  
  elite <- DATA %>%
    filter(fisid %in% elite_id$fisid & !is.na(fispoints) & type != 'Stage') %>%
    collect() 
    #left_join(PENALTY,by = "raceid")
  
  #Summary by gender
  n_gender <- elite %>%
    collect() %>%
    group_by(gender) %>%
    summarise(n = n_distinct(fisid)) %>%
    magrittr::extract2("n")
  
  #Summary for FIS plots
  elite_sum_fis <- elite %>%
    filter(age >= 17 & age <= 35) %>%
    collect() %>%
    group_by(gender,type,age) %>% 
    summarise(lower = quantile(fispoints,probs = 0.25,na.rm = TRUE),
              mid = median(fispoints,na.rm = TRUE),
              upper = quantile(fispoints,probs = 0.75,na.rm = TRUE),
              n = n())
  
  #Summary for start plots
  elite_sum_start <- elite %>% 
    collect() %>%
    group_by(gender,type,name,age) %>% 
    summarise(maj_starts = sum(cat1 %in% MAJ_INT),
              start_quality = mean(penalty,na.rm = TRUE)) %>%
    group_by(gender,type,age) %>% 
    summarise(maj_starts_low = quantile(maj_starts,0.25),
              start_quality_low = quantile(start_quality,0.25,na.rm = TRUE),
              maj_starts_mid = median(maj_starts) * 1.0,
              start_quality_mid = median(start_quality,na.rm = TRUE) * 1.0,
              maj_starts_hi = quantile(maj_starts,0.75),
              start_quality_hi = quantile(start_quality,0.75,na.rm = TRUE)) %>%
    filter(age >= 17 & age <= 35)
  
  return(list(elite_sum_fis = elite_sum_fis,elite_sum_start = elite_sum_start,n_gender = n_gender))
}

ath_data <- function(nms,by_tech = FALSE){
  if (missing(nms) || length(nms) == 0 || nms == "") return(NULL)
  if (length(nms) == 1) nms <- c(nms,nms)
  
  ath <- filter(DATA,name %in% nms & type != "Stage") %>% 
    collect() 
    #left_join(PENALTY,by = "raceid")
  
  if (by_tech){
    grp <- lapply(c("gender","name","type","tech","age"),as.symbol)
  }else{
    grp <- lapply(c("gender","name","type","age"),as.symbol)
  }
  
  ath_sum_fis <- ath %>%
    filter(age >= 17 & age <= 35) %>%
    group_by_(.dots = grp) %>%
    summarise(lower = quantile(fispoints,probs = 0.25,na.rm = TRUE),
              mid = median(fispoints,na.rm = TRUE),
              upper = quantile(fispoints,probs = 0.75,na.rm = TRUE),
              n = n()) %>%
    ungroup()
  names(ath_sum_fis) <- paste0(names(ath_sum_fis),"_ath")
  
  ath_sum_start <- filter(ath,age >= 17 & age <= 35) %>%
    group_by(gender,type,name,age) %>% 
    summarise(maj_starts_ath = sum(cat1 %in% MAJ_INT),
              start_quality_low_ath = quantile(penalty,0.25,na.rm = TRUE),
              start_quality_mid_ath = quantile(penalty,0.5,na.rm = TRUE),
              start_quality_hi_ath = quantile(penalty,0.75,na.rm = TRUE),
              n = n_distinct(raceid)) %>%
    ungroup()
  
  return(list(ath_sum_fis = ath_sum_fis,
              ath_sum_start = ath_sum_start))
  
}

ath_dev_fis <- function(ath_sum_fis,
                        elite_sum_fis,
                        by_tech = FALSE){
  if (nrow(ath_sum_fis) == 0) return(NULL)
  elite_sum_fis <- elite_sum_fis %>% collect()
  full_data <- left_join(ath_sum_fis,
                         elite_sum_fis,
                         by = c("gender_ath" = "gender",
                                "type_ath" = "type",
                                "age_ath" = "age"),
                         copy = TRUE)
  if (by_tech){
    full_data$age_ath_tech <- full_data$age_ath + 
      c('C' = -0.1,'F' = 0,'FC' = 0.1)[full_data$tech_ath]
    pt_rng <- geom_pointrange(aes(x = age_ath_tech,
                                  y = mid_ath,
                                  ymin = lower_ath,
                                  ymax = upper_ath,
                                  color = tech_ath,
                                  group = tech_ath))
    tech_scale <- scale_color_manual(name = "Technique",
                                     values = tech_colors,
                                     labels = tech_labels)
  }else{
    pt_rng <- geom_pointrange(aes(y = mid_ath,
                                  ymin = lower_ath,
                                  ymax = upper_ath),
                              color = "black")
    tech_scale <- NULL
  }
  ggplot(full_data,aes(x = age_ath)) +
    facet_grid(name_ath~type_ath,scales = "free_y") +
    geom_ribbon(aes(ymin = lower,ymax = upper),alpha = 0.25) + 
    geom_line(aes(y = mid),color = "blue") + 
    pt_rng + 
    tech_scale +
    ggtitle(label = "Result by age relative to comparison group") +
    labs(x = "Age",y = "FIS Points") + 
    theme(legend.position = "bottom",
          legend.direction = "horizontal")
  
}

ath_dev_start <- function(ath_sum_start,
                          elite_sum_start,
                          type = c("quality","starts")){
  #if (nrow(ath_sum_start) == 0) return(NULL)
  res <- left_join(ath_sum_start,
                   elite_sum_start,
                   by = c("gender","type","age"))
  
  if (type == "quality"){
    p1 <- ggplot(res) + 
      facet_grid(name~type) + 
      geom_ribbon(aes(x = age,
                      y = start_quality_mid,
                      ymin = start_quality_low,
                      ymax = start_quality_hi),alpha = 0.25) + 
      geom_line(aes(x = age,y = start_quality_mid,group = 1)) + 
      geom_pointrange(aes(x = age, 
                          y = start_quality_mid_ath,
                          ymin = start_quality_low_ath,
                          ymax = start_quality_hi_ath),color = "red") + 
      labs(x = "Age",y = "Start Quality (FIS Point Penalty)") + 
      ggtitle(label = "Start quality by age relative to comparison group",
              subtitle = "Quality is approximated by race penalty")
    return(p1)
  }
  if (type == "starts"){
    p2 <- ggplot(res,aes(x = age)) + 
      facet_grid(name~type) + 
      geom_ribbon(aes(ymin = maj_starts_low,ymax = maj_starts_hi),alpha = 0.25) + 
      geom_line(aes(y = maj_starts_mid)) + 
      geom_line(aes(y = maj_starts_ath),color = "red") + 
      labs(x = "Age",y = "Major Int Starts") + 
      ggtitle(label = "Number of major international starts by\nage relative to comparison group")
    return(p2)
  }
}