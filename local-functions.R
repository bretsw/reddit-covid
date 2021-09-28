################################################################################
# First, we need to define a function, `get_posts()`,
# to retrieve subreddit posts for us.
################################################################################

get_posts <-
  function(subreddit, max_posts = 100, starting_point = "", starting_data = NULL) {
    #if (max_posts > 1000) {max_posts <- 1000}

    authentication <-
      httr::POST(
        url = "https://www.reddit.com/api/v1/access_token",
        httr::add_headers("User-Agent" = Sys.getenv('reddit_user_agent')),
        httr::authenticate(Sys.getenv('reddit_client_id'),
                           Sys.getenv('reddit_client_token')
        )
      )

    httr::stop_for_status(authentication, "authenticate with Reddit")

    subreddit_posts <-
      httr::GET(
        url = paste0("https://www.reddit.com/r/", subreddit,
                     "/new.json?limit=100&after=",
                     starting_point),
        httr::add_headers("User-Agent" = Sys.getenv('reddit_user_agent')),
        httr::authenticate(Sys.getenv('reddit_client_id'),
                           Sys.getenv('reddit_client_token')
        )
      )

    subreddit_post_page <- jsonlite::fromJSON(httr::content(subreddit_posts, as = "text"))

    subreddit_data <- starting_data

    if (!exists("subreddit_data")) {
      subreddit_data <- tibble::as_tibble(subreddit_post_page$data$children$data)
    } else {
      subreddit_data <-
        dplyr::bind_rows(subreddit_data,
                         tibble::as_tibble(subreddit_post_page$data$children$data)
        )
    }

    message(paste(nrow(subreddit_data), "posts found so far..."))

    if (nrow(subreddit_data) >= max_posts |
        starting_point == paste0("t3_", tail(subreddit_data$id, 1))
    ) {

      message(paste("Done retrieving. Returned", nrow(subreddit_data), "posts in total."))
      return(subreddit_data)

    } else {

      message("Going to sleep for a minute...")
      Sys.sleep(60)  # Wait 60 seconds before running again to keep below API rate limit
      message("Waking up...")

      get_posts(subreddit = subreddit_data$subreddit[1],
                starting_data = subreddit_data,
                starting_point = paste0("t3_", tail(subreddit_data$id, 1)),
                max_posts = max_posts
      )
    }

  }


################################################################################
# We also need to clean the raw data
# because it is retrieved in a somewhat messy JSON format.
# We define a function, `clean_posts()`, to accomplish this.
################################################################################

clean_posts <- function(x) {
  x <- jsonlite::flatten(x)
  x <- dplyr::relocate(x)

  x <- dplyr::rename(x,
                     post_id = id,
                     post_author = author,
                     post_date_time = created_utc,
                     post_title = title,
                     post_text = selftext,
                     post_score = score,
                     post_link = permalink)
  x <- dplyr::relocate(x, subreddit, post_id, post_author, post_date_time)

  x <-
    dplyr::mutate(
      x,
      post_date_time = anytime::anytime(post_date_time, asUTC=TRUE),
      post_date_time = lubridate::ymd_hms(lubridate::as_datetime(post_date_time)),
      post_date_time = lubridate::with_tz(post_date_time, tzone='US/Eastern')
    )

  special_flatten <-
    function(y) {
      y <- purrr::map(y, function(z) paste(unlist(z), collapse=", "))
      y <- unlist(y)
      y
    }

  x <- dplyr::mutate(x, dplyr::across(tidyselect:::where(is.list), special_flatten))
}


################################################################################
# Now, we need to define another function, `get_comments()`,
# to retrieve subreddit comments (i.e., responses) to all collected posts.
################################################################################

get_post_json <-
  function(subreddit, post_id) {
    full_json <-
      httr::GET(
        url = paste0("https://www.reddit.com/r/", subreddit,
                     "/comments/",
                     post_id,
                     "/.json"),
        httr::add_headers("User-Agent" = Sys.getenv('reddit_user_agent')),
        httr::authenticate(Sys.getenv('reddit_client_id'),
                           Sys.getenv('reddit_client_token')
        )
      )

    httr::content(full_json, as = "parsed")
  }


get_comments_roomba <-
  function(json_data)  {
    comments <-
      roomba::roomba(json_data,
                     cols = c("subreddit", "id", "author", "created_utc", "body",
                              "score", "permalink", "parent_id"),
                     keep = all)
    if(nrow(comments) != 0) {
      comments <-
        mutate(comments,
               id = as.character(id),
               post_id = as.character(json_data[[1]]$data$children[[1]]$data$id)
        )
    } else {comments <- NULL}

    return(comments)
  }

clean_comments <-
  function(comment_tibble) {
    if(length(comment_tibble) == 9) {
      cleaned_tibble <-
        comment_tibble %>%
        filter(!is.na(subreddit),
               body != "[deleted]") %>%
        separate(parent_id, into = c("parent_type", "parent_id"), sep = "_") %>%
        mutate(parent_type =
                 ifelse(parent_type == 't3',
                        'post',
                        'comment')
        ) %>%
        mutate(comment_date_time =
                 created_utc %>%
                 as.numeric() %>%
                 anytime(asUTC = TRUE) %>%
                 as_datetime %>%
                 ymd_hms() %>%
                 with_tz(tzone = "US/Eastern")
        ) %>%
        select(subreddit,
               post_id,
               comment_id = id,
               comment_author = author,
               comment_date_time,
               comment_text = body,
               parent_type,
               parent_id,
               comment_score = score,
               comment_link = permalink)
    } else {cleaned_tibble <- NULL}

    return(cleaned_tibble)
  }

get_comments_for_one_post <-
  function(subreddit, post_id) {
    post_json_data <- get_post_json(subreddit, post_id)
    comments <- get_comments_roomba(post_json_data)
    comments <- clean_comments(comments)
    return(comments)
  }

get_comments <-
  function(posts_tibble) {
    posts_tibble <- filter(posts_tibble, num_comments > 0)
    subreddit <- posts_tibble$subreddit[1]
    new_tibble <- NULL
    new_tibble <- map_df(posts_tibble$post_id,
                         ~ bind_rows(new_tibble,
                                     get_comments_for_one_post(subreddit,
                                                               .x)
                         )
    )
    return(new_tibble)
  }


################################################################################
# With posts and comments, `merge_posts_comments()`,
#to retrieve subreddit comments to all collected posts.
################################################################################

merge_posts_comments <-
  function(posts_tibble, comments_tibble) {
    new_tibble <-
      posts_tibble %>%
      left_join(comments_tibble, by = c('subreddit', 'post_id'))

    return(new_tibble)
  }
