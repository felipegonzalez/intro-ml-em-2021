step_select_corr <- function(recipe,
                            ...,
                            outcome,
                            role = "predictor",
                            trained = FALSE,
                            threshold = NA,
                            top_p = NA,
                            exclude = NULL,
                            skip = FALSE,
                            id = recipes::rand_id("select_corr")) {
  recipes::add_step(
    recipe,
    step_select_corr_new(
      terms = recipes::ellipse_check(...),
      outcome = outcome,
      role = role,
      trained = trained,
      threshold = threshold,
      top_p = top_p,
      exclude = exclude,
      skip = skip,
      id = id
    )
  )
}

step_select_corr_new <-
  function(terms, outcome, role, trained, threshold, top_p, exclude, skip, id) {
    recipes::step(
      subclass = "select_corr",
      terms = terms,
      outcome = outcome,
      role = role,
      trained = trained,
      threshold = threshold,
      top_p = top_p,
      exclude = exclude,
      skip = skip,
      id = id
    )
  }

corr_calc <- function(x, y) {
  suppressMessages(
    suppressWarnings(
      {
        res <- try(stats::cor(x, y), silent = TRUE)
      }
    )
  )
  res
}

prep.step_select_corr <- function(x, training, info = NULL, ...) {
  y_name <- recipes::terms_select(x$outcome, info = info)
  y_name <- x$outcome[1]
  recipes::check_type(training[, y_name], quant = TRUE)
  x_names <- recipes::terms_select(x$terms, info = info, empty_fun = I)

  if(length(x_names) > 0) {

    recipes::check_type(training[, x_names])

    # check criteria
    #check_criteria(x$top_p, x$threshold, match.call())
    #check_zero_one(x$threshold)
    #x$top_p <- check_top_p(x$top_p, length(x_names))

    # filter
    scores <- purrr::map_dbl(training[, x_names], ~ corr_calc(.x, training[[y_name]]))
    exclude_chr <- recipeselectors:::dual_filter(scores, x$top_p, x$threshold, maximize = TRUE)
  } else {
    exclude_chr <- character()
  }

  step_select_corr_new(
    terms = x$terms,
    outcome = x$outcome,
    role = x$role,
    trained = TRUE,
    threshold = x$threshold,
    top_p = x$top_p,
    exclude = exclude_chr,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_corr <- function(object, new_data, ...) {
  if (length(object$exclude) > 0) {
    new_data <- new_data %>% dplyr::select(-dplyr::one_of(object$exclude))
  }
  new_data
}

#' @export
print.step_select_corr <- function(x, width = max(20, options()$width - 30), ...) {
  cat("Correlation selection")

  if(recipes::is_trained(x)) {
    n <- length(x$exclude)
    cat(paste0(" (", n, " excluded)"))
  }
  cat("\n")

  invisible(x)
}


tunable.step_select_corr <- function(x, ...) {
  tibble::tibble(
    name = c("top_p", "threshold"),
    call_info = list(
      list(pkg = "recipeselectors", fun = "top_p"),
      list(pkg = "dials", fun = "threshold", range = c(0, 1))
    ),
    source = "recipe",
    component = "step_select_corr",
    component_id = x$id
  )
}
