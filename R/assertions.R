#' @title Test list item is not null
#' @param l the list the check
#' @param item_name the item name in the list to check
test_list_item_not_null <- function(l, item_name) {
  
  x <- sapply(l, function(list_item) !is.null(list_item[[item_name]]),
              simplify = TRUE, USE.NAMES = FALSE)
  
  f <- which(x == F)
  
  assertthat::assert_that(length(f) == 0,
                          msg  = paste(paste(names(f), collapse = ", "), "is missing", item_name, sep = " "))
  
}
