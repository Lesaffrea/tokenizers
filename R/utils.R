simplify_list <- function(x, simplify) {
  stopifnot(is.logical(simplify))
  if (simplify && length(x) == 1) x[[1]] else x
}

check_input <- function(x) {
  check_character <- is.character(x) |
  if (is.list(x)) {
       check_list <- all(vapply(x, is.character, logical(1))) &
         all(vapply(x, length, integer(1)) == 1L)
  } else {
    check_list <- FALSE
  }
  if (!(check_character | check_list))
    stop("Input must be a character vector of any length or a list of character\n",
         "  vectors, each of which has a length of 1.")
}

remove_stopwords <- function(x, stopwords) {
  out <- x[!x %in% stopwords]
  if (!length(out)) {
    return(NA_character_)
  }
  return(out)
}

add_class <- function(x) {
  if (is.data.frame(x)) {
    class(x) <- c("tokens", "data.frame")
  } else if (is.list(x) && !is.data.frame(x)) {
    class(x) <- c("tokens_list", "list")
  }
  x
}

#' @title Interoperability with other text analysis packages
#'
#' @description These functions are provided for interoperability with other R
#'   text analysis packages. They convert a named list of tokens to a data.frame
#'   that meets the specifications for interopertability, or vice versa.
#'
#' @examples
#' tokens_l <- tokenize_words(c(a = "A short text.", b = "Even shorter."))
#' as.data.frame(tokens_l)
#' as.list(as.data.frame(tokens_l))
#' @rdname interoperability
#' @aliases interopertability
#' @method as.data.frame tokens_list
#' @export
as.data.frame.tokens_list <- function(x) {
  out <- lapply(seq_along(x), function(i) {
    if (is.null(names(x))) id <- as.character(i) else id <- names(x)[i]
    data.frame(doc_id = id,
               token_index = 1L:length(x[[i]]),
               token = x[[i]],
               stringsAsFactors = FALSE)
  })
  out <- do.call("rbind", out)
  add_class(out)
}

#' @method as.list tokens
#' @rdname interoperability
#' @export
as.list.tokens <- function(x) {
  stopifnot(is.data.frame(x))
  docid <- as.factor(x$doc_id)
  x$doc_id <- NULL
  out <- split(x, f = doc_id)
  out <- lapply(out, function(i) i$token)
  add_class(out)
}

return_type <- function(l, return, simplify) {
  if (return == "df" && simplify) {
    stop("You cannot set `simplify = TRUE` and `return = 'df'`")
  } else if (return == "df" && !simplify) {
    as.data.frame(l)
  } else if (return == "list" && simplify) {
    simplify_list(l, simplify)
  } else if (return == "list" && !simplify) {
    l
  }
}
