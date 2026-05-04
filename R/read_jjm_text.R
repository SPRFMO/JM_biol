read_jjm_sections <- function(path) {
  lines <- readLines(path, warn = FALSE)
  keys <- grep("^#", lines)
  if (!length(keys)) {
    stop("No JJM section markers found in ", path, call. = FALSE)
  }

  values <- vector("list", length(keys))
  names(values) <- sub("^#", "", trimws(lines[keys]))

  for (i in seq_along(keys)) {
    start <- keys[[i]] + 1L
    end <- if (i < length(keys)) keys[[i + 1L]] - 1L else length(lines)
    block <- lines[start:end]
    values[[i]] <- trimws(block[nzchar(trimws(block))])
  }

  values
}

as_numeric_block <- function(x) {
  as.numeric(strsplit(paste(x, collapse = " "), "\\s+")[[1]])
}

as_character_block <- function(x) {
  strsplit(paste(x, collapse = ""), "%", fixed = TRUE)[[1]]
}

model_paths <- function(root = "..", model = "1.14") {
  jjm <- file.path(root, "jjm", "assessment")
  list(
    input = file.path(jjm, "input", paste0(model, ".dat")),
    h1_ctl = file.path(jjm, "config", paste0("h1_", model, ".ctl")),
    h2_ctl = file.path(jjm, "config", paste0("h2_", model, ".ctl"))
  )
}

summarise_model_data <- function(dat) {
  years <- as_numeric_block(dat$years)
  ages <- as_numeric_block(dat$ages)
  fleets <- as_character_block(dat$Fnames)
  indices <- if (!is.null(dat$Inames)) as_character_block(dat$Inames) else character()

  data.frame(
    item = c("Model years", "Ages", "Fleets", "Indices"),
    value = c(
      paste(years, collapse = "-"),
      paste(ages, collapse = "-"),
      paste(fleets, collapse = ", "),
      paste(indices, collapse = ", ")
    )
  )
}

summarise_biology <- function(ctl) {
  n_stocks <- as_numeric_block(ctl$nStocks)[[1]]
  stock_names <- if (!is.null(ctl$nameStock)) as_character_block(ctl$nameStock) else paste("Stock", seq_len(n_stocks))
  natural_mortality <- as_numeric_block(ctl$N_Mort)[seq_len(n_stocks)]
  maturity <- split(as_numeric_block(ctl$Pmatatage), rep(seq_len(n_stocks), each = 12))
  weight_at_age <- split(as_numeric_block(ctl$Pwtatage), rep(seq_len(n_stocks), each = 12))

  data.frame(
    model = ctl$modelName[[1]],
    stock = stock_names,
    natural_mortality = natural_mortality,
    maturity_at_age = I(maturity),
    population_weight_at_age = I(weight_at_age)
  )
}

read_h2_spawning_contribution <- function(rep_path, h2_bio, years, spawn_month) {
  lines <- readLines(rep_path, warn = FALSE)
  n_stocks <- nrow(h2_bio)
  n_years <- length(years)
  n_ages <- length(h2_bio$maturity_at_age[[1]])
  n_rows <- n_stocks * n_years
  spawn_fraction <- (spawn_month - 1) / 12

  numeric_rows_after <- function(marker, n) {
    start <- grep(marker, lines, fixed = TRUE)[[1]] + 1L
    rows <- list()
    i <- start
    while (length(rows) < n && i <= length(lines)) {
      values <- suppressWarnings(as.numeric(strsplit(trimws(lines[[i]]), "\\s+")[[1]]))
      values <- values[!is.na(values)]
      if (length(values) >= n_ages) {
        rows[[length(rows) + 1L]] <- tail(values, n_ages)
      }
      i <- i + 1L
    }
    do.call(rbind, rows)
  }

  z <- numeric_rows_after("Total mortality (Z)", n_rows)
  numbers <- numeric_rows_after("Estimated numbers of fish", n_rows)

  out <- vector("list", n_stocks)
  for (stock_index in seq_len(n_stocks)) {
    row_index <- ((stock_index - 1L) * n_years + 1L):(stock_index * n_years)
    contribution <- numbers[row_index, , drop = FALSE] *
      exp(-z[row_index, , drop = FALSE] * spawn_fraction) *
      matrix(h2_bio$population_weight_at_age[[stock_index]] * h2_bio$maturity_at_age[[stock_index]],
             nrow = n_years, ncol = n_ages, byrow = TRUE)

    out[[stock_index]] <- do.call(rbind, lapply(seq_len(n_years), function(y_index) {
      data.frame(
        stock = h2_bio$stock[[stock_index]],
        year = years[[y_index]],
        age = seq_len(n_ages),
        spawning_biomass = contribution[y_index, ]
      )
    }))
  }

  do.call(rbind, out)
}
