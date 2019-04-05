
Ancy <-
  dplyr::tibble(
    group = c(rep("Ancy", 7), rep("Control", 8)),
    group_num = 3 - as.numeric(factor(group)),
    height =  c(13.2, 19.5, 11.0,  5.8, 12.8,  7.1, 7.7,
                10.0, 13.2, 19.8, 19.3, 21.2, 13.9, 20.3, 9.6)
  ) %>% arrange(group_num)

usethis::use_data(Ancy, overwrite = TRUE)
