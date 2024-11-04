#' Plot a percentage stacked bar chart for likert data
#'
#' This function generates a percentage stacked bar chart for the input likert
#' data using ggplot2::geom_bar.
#'
#' @param data A data.frame object containing information of the likert data.
#'   The data frame can have one of two forms:
#'   (1) Wide form: columns represent questions, rows represent samples, and
#'   each element is a response; or
#'   (2) Long form: a data frame that contains at least the following three
#'   columns:
#'     - `Question`: The questions.
#'     - `Response`: The responses.
#'     - `Count`: The count of each response for each question.
#'   Additional columns in the data frame are allowed and will be ignored by the
#'   function.
#' @param questions A character vector of names of columns (questions) that will
#'   be included in the bar chart. Only applicable if data is in wide form.
#'   (default is all columns)
#' @param long_form Logical. Indicates if the input data frame is in long form.
#'   If `TRUE`, the function assumes the data is in long form; if `FALSE`
#'   (default), the function assumes the data is in wide form.
#' @param question_col A string specifying the name of the column that contains
#'   the questions. The first column will be used if not provided. Only
#'   applicable if data is in long form.
#' @param response_col A string specifying the name of the column that contains
#'   the responses. The second column will be used if not provided. Only
#'   applicable if data is in long form.
#' @param count_col A string specifying the name of the column that contains the
#'   count of each response for each question. The third column will be used if
#'   not provided. Only applicable if data is in long form.
#' @param diverging Logical. Indicates if the responses are diverging. If `TRUE`
#'   (default), the function assumes the responses are diverging; if `FALSE`,
#'   the function assumes the responses are sequential.
#' @param colors A character vector of colors to be used for the different
#'   response types. The number of colors provided must match the number of
#'   unique responses in the data.
#' @param palette_name A string specifying the color palette to be used with
#'   `RColorBrewer` to generate colors for responses. Ignored if `colors` is
#'   provided. If neither `colors` nor `palette_name` is provided, the default
#'   palettes will be used ("RdBu" if diverging and "Blues" if not).
#'   See `RColorBrewer::display.brewer.all()` for available palette names.
#' @param horizontal Logical. If `TRUE` (default), the bars will be horizontal;
#'   if `FALSE`, the bars will be vertical.
#' @param reverse Logical. Indicates the sequence of stacked responses based on
#'   response levels. If `FALSE` (default), the response with the lower level
#'   appears on the left (if horizontal) or at the bottom (if vertical); if
#'   `TRUE`, the order is reversed, with the response with the larger level
#'   appearing on the left or bottom.
#' @param include_na Logical. Indicates whether `NA` values should be included
#'   in the graph. If `TRUE`, `NA` will be plotted as a response; If `FALSE`
#'   (default), `NA` will be dropped.
#' @param na_label A string specifying the label to use for `NA` values in the
#'   plot. Only applicable if `include_na` is `TRUE`. (default is "No Response")
#' @param na_color A string specifying the color to use for `NA` values in the
#'   plot. Only applicable if `include_na` is `TRUE`. (default is "gray")
#' @param order_by A string specifying the criterion used to sort the questions.
#'   Must be `"positive"`, `"negative"`, `"extreme positive"`, `"extreme negative"`,
#'   or `"missing"`. It assumes that the response level is from disagree to agree
#'   (or from negative to positive)
#'   - `"positive"`: Sorts questions by the total count of responses at the
#'     agree levels.
#'   - `"negative"`: Sorts questions by the total count of responses at the
#'     disagree levels.
#'   - `"extreme positive"`: Sorts questions by the count of responses at the
#'     highest levels.
#'   - `"extreme negative"`: Sorts questions by the count of responses at the
#'     lowest levels.
#'   - `"missing"`: Sorts questions by the count of `NA` (missing) responses.
#'   If not provided, the questions are plotted in the order they appear in the
#'   data frame.
#' @param extreme A positive integer indicating the number of extreme levels to
#'   consider if `"extreme positive"` or `"extreme negative"` is chosen for
#'   `order_by`. The default value is 1, meaning only the most extreme level
#'   (e.g., "Strongly Agree" or "Strongly Disagree") will be considered. Higher
#'   values include additional levels toward the center of the response scale.
#' @param xlab, ylab, title Strings specifying the labels for the plot:
#'   - `xlab`: The label for the x-axis.
#'   - `ylab`: The label for the y-axis.
#'   - `title`: The main title of the plot.
#' @return No return value
#' @export
#' @examples
#' library(tidyverse)
#' library(likert)
#' # Load the PISA dataset, which contains Likert-style responses
#' data("pisaitems")
#'
#' # Plot a percentage stacked bar chart using the original dataset in wide form
#' likertbar(pisaitems, questions = c("ST24Q01", "ST24Q02", "ST24Q03"),
#'           colors = c("#768CE6", "#B3BAE6", "#E8ADB7", "#E16A85"),
#'           include_na = TRUE, order_by = "extreme positive",
#'           xlab = "Percentage", ylab = "Questions")
#'
#' # Transform the data into a long form, with columns "Question", "Response",
#' # and "Count"
#' pisaitems_long <- pisaitems |>
#'   pivot_longer(cols = ST24Q01: ST24Q03, names_to = "Question", values_to = "Response") |>
#'   group_by(Question, Response) |>
#'   summarise(Count = n(), .groups = "drop")
#'
#' # Plot a vertical percentage stacked bar chart using the dataset in long form
#' likertbar(pisaitems_long, long_form = TRUE, palette_name = "PiYG",
#'           horizontal = FALSE, order_by = "negative", xlab = "Questions")
likertbar <- function(data,
                      questions = everything(),
                      long_form = FALSE,
                      question_col = names(data)[1],
                      response_col = names(data)[2],
                      count_col = names(data)[3],
                      diverging = TRUE,
                      colors = NULL,
                      palette_name = NULL,
                      horizontal = TRUE,
                      reverse = FALSE,
                      include_na = FALSE,
                      na_label = "No Response",
                      na_color = "gray",
                      order_by = NULL,
                      extreme = 1,
                      xlab = if (horizontal) "Percent" else "",
                      ylab = if (horizontal) "" else "Percent",
                      title = "") {
  order_by <- match.arg(order_by, c("positive", "negative", "extreme positive", "extreme negative", "missing"))

  if (long_form == FALSE) {
    likert_data <- data |>
      tidyr::pivot_longer(cols = questions, names_to = "Question", values_to = "Response") |>
      dplyr::group_by(Question, Response) |>
      dplyr::summarise(Count = n(), .groups = "drop")
  } else {
    if (ncol(data) < 3) {
      stop("The dataframe must have at least three columns if `long_form` is TRUE.")
    }
    likert_data <- data |>
      dplyr::rename("Question" = question_col, "Response" = response_col, "Count" = count_col)
  }

  n <- nlevels(likert_data$Response)

  if (is.null(colors)) {
    colors <- colorFunction(n, palette_name, diverging)
  } else if (length(colors) != n) {
    stop(paste0("The number of colors provided (", length(colors),
               ") must match the number of unique responses (", n, ")."))
  }
  colors <- rev(colors)

  if (!is.null(order_by)) {
    likert_data <- questionReorder(likert_data, n, diverging, order_by, extreme)
    likert_data$Question <- factor(likert_data$Question)
    likert_data$Question <- forcats::fct_reorder(likert_data$Question, likert_data$order_count, .desc = TRUE)
    if (horizontal == TRUE) {
      likert_data$Question <- forcats::fct_rev(likert_data$Question)
    }
  }

  if (reverse == FALSE) {
    likert_data$Response <- forcats::fct_rev(likert_data$Response)
  }

  if (include_na == FALSE) {
    likert_data <- likert_data |> dplyr::filter(!is.na(Response))
  } else if (any(is.na(likert_data$Response))) {
    likert_data$Response <- forcats::fct_na_value_to_level(likert_data$Response, na_label)
    likert_data$Response <- forcats::fct_relevel(likert_data$Response, na_label)
    colors <- c(na_color, colors)
  }

  p <- ggplot(likert_data, aes(x = Question, y = Count, fill = Response)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(values = colors) +
    theme_classic()

  if (horizontal == TRUE) {
    p <- p + coord_flip()
    p <- p + labs(x = ylab, y = xlab, title = title)
  } else {
    p <- p + labs(x = xlab, y = ylab, title = title)
  }

  print(p)
}

# Helper function to get colors for responses
colorFunction <- function(nc, palette_name, diverging) {
  if (is.null(palette_name)) {
    palette_name <- ifelse(diverging == TRUE, "RdBu", "Blues")
  }

  colors <- if (nc <= RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]) {
    RColorBrewer::brewer.pal(nc, palette_name)
  } else {
    colorRampPalette(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[palette_name, "maxcolors"],
                                              palette_name))(nc)
  }

  if (diverging && nc %% 2 == 1) {
    middle_index <- ceiling(nc / 2)
    colors[middle_index] <- "gray90"
  }

  return(colors)
}

# Helper function to count different responses in each question to help order the questions
questionReorder <- function(data, n_levels, diverging, order_by, extreme) {
  if (order_by == "negative") {
    if (diverging == TRUE) {
      reorder_data <- data |>
        dplyr::filter(Response %in% head(levels(Response), floor(n_levels / 2)))
    } else {
      warning('The levels are sequential and the order will not change. Try "extreme
              negative" instead.')
    }
  } else if (order_by == "positive") {
    if (diverging == TRUE) {
      reorder_data <- data |>
        dplyr::filter(Response %in% tail(levels(Response), floor(n_levels / 2)))
    } else {
      warning('The levels are sequential and the order will not change. Try "extreme
              positive" instead.')
    }
  } else if (order_by != "missing") {
    extreme_limit = ifelse(diverging == TRUE, floor(n_levels / 2), n_levels)
    if (extreme < 1 || extreme > extreme_limit || extreme %% 1 != 0) {
      warning(paste0('extreme = ', extreme, ' is too small / too large / not an integer.',
                     ' Number of extreme levels (', extreme_limit, ') will be used.'))
      extreme = extreme_limit
    }
    if (order_by == "extreme negative") {
      reorder_data <- data |>
        dplyr::filter(Response %in% head(levels(Response), extreme))
    } else if (order_by == "extreme positive") {
      reorder_data <- data |>
        dplyr::filter(Response %in% tail(levels(Response), extreme))
    }
  } else if (order_by == "missing") {
    reorder_data <- data |>
      dplyr::filter(is.na(Response))
  }

  reorder_data <- reorder_data |>
    dplyr::group_by(Question) |>
    dplyr::summarise(order_count = sum(Count))

  joined_data <- data |> dplyr::left_join(reorder_data, by = 'Question')

  return(joined_data)
}
