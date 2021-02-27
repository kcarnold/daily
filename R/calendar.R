
#' Calendar CSS
#'
#' Emit CSS for calendars
#'
#' @export
calendar_css <- function() {
  path <- system.file("calendar.css", package="calendar")
  lines <- getSrcLines(srcfile(path), 1, 10000)
  paste(lines, collapse = "\n")
}

days2mask <- function(x) {
  if (is.character(x)) {
    return(
      which(
        c("N", "M", "T", "W", "R", "F", "S") %in%
          strsplit(x, split = "")[[1]]
        )
    )
  }
  if (is.numeric(x)) {
    x <- unique(x %% 7)
    x[x==0] <- 7
    return(x)
  }
  NULL
}

#' Render Daily File as HTML
#'
#' Render Daily File as HTML
#'
#' @importFrom rlang %||%
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom lubridate today floor_date mdy weeks month wday mday days
#' @importFrom dplyr mutate arrange filter select group_by ungroup
#' @importFrom dplyr slice pull do right_join bind_rows
#' @importFrom dplyr %>%
#' @importFrom glue glue
#' @importFrom stringr str_split
#' @import ggplot2
#'
#' @param path  path to "daily" file
#' @param start start date
#' @param end   end date
#' @param days a character string or a numeric vector indicating
#' which days of the week to use by default.
#' (1 = N = Sunday, 5 = R = Thursday, 7 = S = Saturday)
#' @param show a numeric vector of days to show (Sunday = 1)
#' @param items a character vector of items to display (or NULL for all)
#' @param width width of HTML table used for calendar.
#' @inheritParams daily2cal
#' @return a character string of HTML code for the calendar
#'
#' @export
daily2html <-
  function(
    path  = "daily.txt",
    start = NULL,
    end   = NULL,
    days  = NULL,
    show  = 1:7,
    items = NULL,
    width = "95%",
    delim = "::\\s+",
    daysep = "^===+")
  {
    opt <- rmarkdown::yaml_front_matter(path)
    opt$css <- opt$css %||% "default"
    if (opt$css == "default") {
      opt$css <- system.file("calendar.css", package = "calendar")
    }
    if (opt$css != "none") {
      css <- readLines(opt$css) %>% paste(collapse = "\n")
    }

    Cal  <- daily2cal(path = path, start = start, end = end, days = days, delim = delim)
    html <- html_calendar(Cal, show = show, items = items, width = width)

    return(
      paste(css, html, collapse = "\n")
    )
  }

#' Create Calendar from Daily File
#'
#' Create Calendar from Daily File
#'

#'
#' @param path  path to "daily" file
#' @param start start date
#' @param end   end date
#' @param days a character string or a numeric vector indicating
#' which days of the week to use by default.
#' (1 = N = Sunday, 5 = R = Thursday, 7 = S = Saturday)
#' @param delim delimeter for parsing items a day's entry
#' @param daysep between day separator
#' @seealso [html_calendar()], [gg_calendar()]
#' @return a data frame representing the calendar
#' @export
daily2cal <-
  function(
    path  = "daily.txt",
    start = NULL,
    end   = NULL,
    days  = NULL,
    delim = "::\\s*",
    daysep = "^===+"
  )
  {
    current_date  <- NA
    Cal <- data.frame(date = NA)

    opt <- rmarkdown::yaml_front_matter(path)


    # determine mask (ie, default days)
    mask <- days2mask(days) %||% days2mask(opt$days) %||% 1:7

    start <-
      start %||%
      lubridate::mdy(opt$start) %||%
      (lubridate::today() - lubridate::weeks(2))

    end <-
      end %||%
      lubridate::mdy(opt$end) %||%
      (lubridate::today() + lubridate::weeks(4))

    lines <- getSrcLines(srcfile(path), 1, 10000)
    state = 0
    current_row <- data.frame(date = current_date)
    label <- "Note"
    done_with_cal <- FALSE
    skip_day <- FALSE
    for (line in lines) {
      if (done_with_cal) break

      # skip commented lines
      if (grepl("^#", line)) {
        next
      }

      # stop processing if we hit endofcal
      if (grepl(daysep, line) &&
          grepl("endofcal", line, ignore.case = TRUE)
          ) {
        done_with_cal <- TRUE
      }

      # next day
      if (grepl(daysep, line)) {
        if (! skip_day) {
          # if not skipping, add to calendar and bump date
          Cal <-
            dplyr::bind_rows(
              Cal,
              current_row
            )
          new_date <- lubridate::mdy(line, quiet = TRUE)
          if (is.na(new_date)) {
            current_date <- next_cal_day(current_date, mask)
          } else {
            current_date <- new_date
          }
        }
        # whether skipped or not, flush current_date
        skip_day <- FALSE
        current_row <- data.frame(date = current_date)

        # set flag to skip new day's content
        if (grepl(paste0(daysep, "skip"), line)) {
          skip_day <- TRUE
        }

      } else { # end if ^===
        keep_line <- TRUE
        # item to place on calendar of form label:: value
        parts <- stringr::str_split(line, pattern = delim, n = 2)[[1]]
        if (length(parts) == 2) {
          # New label.
          label <- stringr::str_trim(parts[1], side = "right")
          line <- stringr::str_trim(parts[2], side = "left")
        } else if (stringr::str_starts(parts[1], "  ")) {
          # Continue previous label iff it starts with two spaces; drop them
          line <- stringr::str_sub(parts[1], 3L)
        } else if (stringr::str_trim(parts[1]) == "") {
          line <- ""
        } else {
          keep_line <- FALSE
        }

        if (keep_line) {
          if (!is.null(current_row[1, label])) {
            current_row[1, label] <-
              paste0(current_row[1, label], "\n", line)
          } else {
            current_row[1, label] <- line
          }
        }
      }
    }  # end for line
  Cal <-
    Cal %>%
    filter(!is.na(date), date >= start)

  start <- lubridate::floor_date(start, unit = "week")
  suppressMessages(
    result <- Cal %>%
      right_join(
        data.frame(
          date = start +
            lubridate::days(1: difftime(max(Cal$date), start,
                                        units = "days"))
        )
      ) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(week  = as.numeric(ceiling(difftime(date, start, units = "weeks"))))
    # month = lubridate::month(date, label = TRUE),
    # day   = lubridate::wday(date),
    # wday  = lubridate::wday(date, label = TRUE))
  )
  if (length(end) == 1) {
    result <- filter(result, date <= end)
  }
  result
}

next_cal_day <- function(date, mask = 1:5) {
  wd <- lubridate::wday(date)
  date + lubridate::days(days_til_next(mask)[wd])
}

days_til_next <- function(mask) {
  mask <- c(mask, 7 + mask) # make it two weeks
  sapply(1:7, function(x) min(mask[mask > x]) - x)
}

#' Create gg Calendar
#'
#' Create gg calendar
#'
#' @param calendar a calendar object
#' @param show a numeric vector of days to show (Sunday = 1)
#' @param size a number for scaling text size
#' @return a ggplot object
#' @export
gg_calendar <- function(calendar, show = 1:7, size = 3) {
  ggplot(data = calendar %>% filter(wday(date) %in% show)) +
    geom_text(aes(y = 4, x = -1,
                  label = paste0(lubridate::month(date), "/",  lubridate::mday(date))),
              color = "black", hjust = 0, size = size * .8) +
    geom_text(aes(y = 3, x = 0, label = Topic),
              color = "navy", hjust = 0, size = size) +
    geom_text(aes(y = 2, x = 0, label = Note),
              color = "forestgreen", hjust = 0, size = size) +
    facet_grid(week ~ wday(date, label = TRUE)) +
    lims(y = c(0,5), x = c(-1,5)) +
    labs(x = "", y = "") +
    theme_bw() +
    theme(
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank())
}


#' Create HTML Calendar
#'
#' Create HTML Calendar
#'
#' @param calendar a calendar object
#' @param show a numeric vector of days to show (Sunday = 1)
#' @param items a character vector of items to display (or NULL for all)
#' @param width width of HTML table used for calendar.
#' @param past_week weeks less than this get a class ".past"
#' @return a character string of HTML code for the calendar
#' @export
html_calendar <-
  function(calendar, show = 1:7, items = NULL, width = "95%", past_week = NULL) {
    Cal <- calendar %>% filter(wday(date) %in% show)
    Cal <- Cal %>%
      dplyr::group_by(date, week) %>%
      dplyr::do(html_reduce(.)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(date)
    last_week <- -Inf
    res <- paste0("<table class=\"daily-calendar\" border=1 width=", width, ">")

    res <- c(res, '<colgroup>')
    for (s in show) {
      res <- c(res, '<col width = "10%">')  # equal widths for each column
    }
    res <- c(res, '</colgroup>')

    res <- c(res, "<thead><tr>")
    for (wday in c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[show]) {
      res <- c(res, glue::glue("<th>{day}</th>", day = wday))
    }
    res <- c(res, "</tr></thead><tbody><tr>")

    for (i in 1:nrow(Cal)) {
      if (Cal[i, "week"] > last_week) {
        res <- c(res, paste("<!-- ", Cal[i, "week"], ">", last_week, "-->"))
        is_past <- !is.null(past_week) && (Cal[i, "week"] < past_week)
        res <- c(res, if (is_past) "</tr><tr class=\"past\">" else "</tr><tr>")
        last_week <- Cal[i, "week"]
      }
      res <- c(res, render_day(Cal, i, items))
    }
    res <- c(res, "</tr></tbody></table>")
    res %>% paste(collapse = "\n")
  }

render_day <- function(calendar, row, items) {
  available_items <- setdiff(names(calendar), c("date", "week"))
  if (is.null(items)) {
    items <- available_items
  } else {
    items <- intersect(items, available_items)
  }

  month <-
    calendar %>%
    dplyr::slice(row) %>%
    dplyr::pull(date) %>%
    lubridate::month()
  res <-
    glue::glue('<td class = "{parity}"><div>{month}/{day}<br>',
               parity = if(month %% 2 == 0) "even" else "odd",
               month = month,
               day   =
                 calendar %>%
                 dplyr::slice(row) %>%
                 dplyr::pull(date) %>%
                 lubridate::mday()
    )
  # res <- c(res, "<table><tr><td>")
  for (i in items) {
    if (! is.na(calendar[row, i])) {
      res <-
        paste0(res,
               glue::glue('<span class = "{item}">{value}</span><br>',
                          item = i, value = calendar[row, i]
               )
        )
    }
  }
  # res <- c(res, "</td></tr></table>")
  res <- paste0(res, "<br></div></td>")
  res
}

#' @importFrom stats na.omit

html_reduce <- function(data) {
  lapply(
    data %>% dplyr::select(-date, -week),
    function(x) paste0(unique(stats::na.omit(x)), collapse = "<br>")
  ) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

# calendar(start = mdy("8/15/2018"), mask = c(2,3,5,6)) %>%
#   gg_calendar(show = 2:6)
#
# calendar(start = mdy("8/15/2018"), mask = c(2,3,5,6)) %>%
#   html_calendar()
