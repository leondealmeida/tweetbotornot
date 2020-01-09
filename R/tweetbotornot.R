

#' tweetbotornot
#'
#' Classify users/accounts in Twitter data as bots or not bots.
#'
#' @param x Object to be classified. Can be user IDs, screen names, or
#'   data frames returned by rtweet.
#' @param fast Logical indicating whether to use the fast (lighter) model. The
#'   default (fast = FALSE) method uses the most recent 100 tweets posted by
#'   users to determine the probability of bot. The fast (fast = TRUE) method
#'   only uses users-level data, which is easier to get in large quantities from
#'   Twitter's APIS but overall less accurate.
#' @return Classifications for all users expressed as probability of whether
#'   each account is a bot.
#' @examples
#' \dontrun{
#'
#' ## vector of screen names
#' sns <- c("kearneymw", "geoffjentry", "p_barbera",
#'   "tidyversetweets", "rstatsbot1234", "RStatsStExBot")
#'
#' ## get and view bot probability estimates
#' twb <- tweetbotornot(sns)
#' twb
#'
#' ## ask for the fast (user-level data only) version
#' twbf <- tweetbotornot(sns, fast = TRUE)
#' twbf
#'
#' }
#' @export
tweetbotornot <- function(x, fast = FALSE) UseMethod("tweetbotornot")


get_timeline_unlimited <- function(users, n){
  
  if (length(users) == 0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- rtweet::get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- rtweet::get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

#' @export
tweetbotornot.default <- function(x, fast = FALSE) {
  botornot(x, fast = fast)
}


#' Identical to \code{tweetbotornot}
#' @rdname tweetbotornot
#' @inheritParams tweetbotornot
#' @export
botornot <- function(x, fast = FALSE) UseMethod("botornot")

#' @export
botornot.data.frame <- function(x, fast = FALSE) {
  stopifnot(nrow(x) > 0, "user_id" %in% names(x))
  ## store screen and user names
  uu <- x[!duplicated(x$user_id), ]
  if (fast) {
    ## extract features
    x <- extract_features_ntweets(x)
    ## get model
    m <- botornot_models$ntweets
  } else {
    ## extract features
    x <- extract_features_ytweets(x)
    ## get model
    m <- botornot_models$ytweets
  }
  if (!"status_text_n_chars" %in% names(x)) {
    m$var.names <- sub("^description_", "dsc_", m$var.names)
    m$var.names <- sub("^location_", "loc_", m$var.names)
    m$var.names <- sub("^status_text_", "txt_", m$var.names)
    m$var.names <- sub("^name_", "nm_", m$var.names)
  }

  ## classify data
  p <- classify_data(x, m)
  ## rearrange ot match uu
  p <- p[match(uu$user_id, x$user_id)]
  ## return as tibble
  tibble::tibble(
    screen_name = uu$screen_name,
    user_id = uu$user_id,
    prob_bot = p)
}

#' @export
botornot.factor <- function(x, fast = FALSE) {
  x <- as.character(x)
  botornot(x, fast = fast)
}

#' @export
botornot.character <- function(x, fast = FALSE) {
  ## remove NA and duplicates
  x <- x[!is.na(x) & !duplicated(x)]
  ## get most recent 100 tweets
  x <- get_timeline_unlimited(x, n = 100)
  ## pass to next method
  botornot(x, fast = fast)
}


#' Esimated probability of being a bot
#'
#' Returns a numeric vector of probabilities
#'
#' @inheritParams tweetbotornot
#' @rdname tweetbotornot
#' @return A named (screen names or user IDs depending on input) numeric
#'   vector of probabilities
#' @export
botornot_dbl <- function(x, fast = TRUE) {
  if (is.data.frame(x)) {
    usrs <- x$screen_name
  } else {
    usrs <- x
  }
  p <- botornot(x, fast = TRUE)
  if (any(usrs %in% p$screen_name)) {
    p <- p$prob_bot[match(usrs, p$screen_name)]
  } else {
    p <- p$prob_bot[match(usrs, p$user_id)]
  }
  names(p) <- usrs
  p
}
