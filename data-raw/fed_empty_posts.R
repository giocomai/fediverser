## code to prepare `fed_empty_posts` dataset goes here
fed_empty_posts <- get_timeline_hashtag(hashtag = "#fediverse") %>%
  dplyr::mutate(internal_id = as.character(NA)) %>%
  dplyr::mutate(
    in_reply_to_account_id = as.character(in_reply_to_account_id),
    language = as.character(language)
  ) %>%
  dplyr::slice(0)

usethis::use_data(fed_empty_posts, overwrite = TRUE)
