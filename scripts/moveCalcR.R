local({
  packages <- c("readr", "ggplot2", "tidyr", "dplyr", "patchwork", "signal")
  
  installed <- packages %in% installed.packages()
  if (any(!installed)) {
    install.packages(packages[!installed])
  }
  
  lapply(packages, library, character.only = TRUE)
})




#' Read MVD File (Main Data)
#'
#' Reads and processes the main part of an MVD file, skipping metadata and renaming columns if needed.
#'
#' @param path Path to the MVD file (CSV-like with semicolon separators).
#'
#' @return A `data.frame` with the main data section of the MVD file.
#' @export
read_mvd <- function(path) {
  
  library(readr)
  
  meta <- read_delim(path, 
                     delim = ";", 
                     escape_double = FALSE, 
                     trim_ws = TRUE, 
                     skip = 2,
                     show_col_types = FALSE)
  
  
  for (ix in 1:nrow(meta)) {
    value <- meta[[2]][ix]  
    
    if (is.na(suppressWarnings(as.numeric(value)))) {
      break
    }
  }
  
  main <- read_delim(path, 
                     delim = ";", 
                     escape_double = FALSE, 
                     trim_ws = TRUE, 
                     skip = ix+2,
                     show_col_types = FALSE)
  
  main <- main[, colSums(!is.na(main)) > 0]
  
  first_row <- main[1, ]
  
  cols_to_rename <- sapply(names(main), function(col) {
    any(!is.na(main[-1, col])) && !is.na(first_row[[col]])
  })
  
  new_names <- names(main)
  new_names[cols_to_rename] <- paste0(new_names[cols_to_rename], "", as.character(first_row[cols_to_rename]))
  
  colnames(main) <- new_names
  
  main <- main[-1, ]
  
  
  
}


#' Read MVD Metadata
#'
#' Extracts metadata from the top section of an MVD file.
#'
#' @param path Path to the MVD file.
#'
#' @return A `data.frame` containing the metadata section.
#' @export
read_mvd_meta <- function(path) {
  
  meta <- read_delim(path, 
                     delim = ";", 
                     escape_double = FALSE, 
                     trim_ws = TRUE, 
                     skip = 2,
                     show_col_types = FALSE)
  
  
  for (ix in 1:nrow(meta)) {
    value <- meta[[2]][ix]  
    
    if (is.na(suppressWarnings(as.numeric(value)))) {
      break
    }
  }
  
  meta <- meta[1:ix-1,]
  
  meta <- meta[, colSums(!is.na(meta)) > 0]
  
  
  
}



#' Add Group Information to Data
#'
#' Matches animal IDs between data and metadata and adds a group column.
#'
#' @param data Main data `data.frame`.
#' @param metadata Metadata `data.frame`.
#' @param animal_col Column name for animal ID in both `data` and `metadata`.
#' @param group_col_meta Column name for group information in `metadata`.
#'
#' @return Updated `data.frame` with a new `group` column.
#' @export
group_mdv <- function(data, metadata, animal_col, group_col_meta) {
  
  match_idx <- match(data[[animal_col]], metadata[[animal_col]])
  data$group <- metadata[[group_col_meta]][match_idx]
  
  return(data)
  
}



#' Generate Heatmap with Grouping
#'
#' Creates a heatmap of animal data over time with an associated group annotation.
#'
#' @param data Data `data.frame`.
#' @param animal_col Column name for animal ID.
#' @param time_col Column name for time points.
#' @param stat_col Column name for the statistic/value to plot.
#' @param group_col Column name for group information.
#' @param group_sort Optional character vector to define group order.
#'
#' @return A `ggplot` patchwork object showing the heatmap and group map.
#' @export
mvd_map <- function(data, animal_col, time_col, stat_col, group_col, group_sort = NaN) {
  
  
  data[[time_col]] <- factor(data[[time_col]], levels = unique(data[[time_col]]))
  data[[stat_col]] <- as.numeric(as.character(data[[stat_col]]))
  
  if (!is.nan(group_sort)[1]) {
    group_sort <- rev(group_sort)
    unique_groups <- unique(data[[group_col]])
    if (!all(group_sort %in% unique_groups)) {
      stop("Error: Some groups from group_sort do not appear in column group_col")
    }
  } else {
    
    group_sort <- unique(data[[group_col]])
    
  }
  
  animals <- unique(data[[animal_col]])
  
  if (is.na(suppressWarnings(as.numeric(animals[1])))) {
    animals <- sort(unique(data[[animal_col]]))
  } else {
    animals <- as.character(sort(as.numeric(unique(data[[animal_col]]))))
  }
  
  
  data <- data[order(data[[group_col]], data[[animal_col]]), ]
  
  
  
  data[[group_col]] <- factor(data[[group_col]], levels = group_sort)
  data[[animal_col]] <- factor(data[[animal_col]], levels = animals)
  
  data <- data[order(data[[group_col]], data[[animal_col]]), ]
  
  data[[group_col]] <- factor(data[[group_col]], levels = unique(data[[group_col]]))
  data[[animal_col]] <- factor(data[[animal_col]], levels = unique(data[[animal_col]]))
  
  
  data[[group_col]] <- factor(data[[group_col]], levels = rev(levels(data[[group_col]])))
  
  
  
  breaks_vec <- levels(data[[time_col]])[seq(1, length(levels(data[[time_col]])), by = 60)]
  
  
  
  
  group_plot <- ggplot(data, aes(y = .data[[animal_col]], x = 1, fill = .data[[group_col]])) +
    geom_tile() +
    scale_fill_brewer(palette = "Set2", name = "Group") +
    theme_void() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(5, 0, 5, 5)
    )
  
  heatmap_plot <- ggplot(data, aes(x = .data[[time_col]], y = .data[[animal_col]], fill = .data[[stat_col]])) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma") +
    scale_x_discrete(breaks = breaks_vec) +
    labs(x = "Time", y = "Animal", fill = "Value") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid = element_blank()
    )
  
  ret = group_plot + heatmap_plot + plot_layout(widths = c(0.05, 1), guides = "collect") &
    theme(legend.position = "right")
  
  
  return(ret)
}



#' Rescale Column to [0, 1]
#'
#' Linearly rescales a numeric column to the range [0, 1].
#'
#' @param data A `data.frame`.
#' @param col The name of the column to rescale.
#'
#' @return A `data.frame` with the rescaled column.
#' @export
rescale <- function(data, col) {
  if (!col %in% names(data)) {
    stop(paste("Error: column", col, "does not exist"))
  }
  
  data[[col]] <- as.integer(data[[col]])
  data[[col]] <- (data[[col]] - min(data[[col]], na.rm = TRUE)) /
    (max(data[[col]], na.rm = TRUE) - min(data[[col]], na.rm = TRUE))
  
  return(data)
}




#' Rescale Values by Group
#'
#' Rescales values in a column within each group to the range [0, 1].
#'
#' @param data A `data.frame`.
#' @param value_col Column name of the values to rescale.
#' @param group_col Column name of the grouping variable.
#'
#' @return A `data.frame` with rescaled values per group.
#' @export
rescale_by_group <- function(data, value_col, group_col) {
  
  library(dplyr)
  
  if (!value_col %in% names(data)) {
    stop(paste("Error: column", value_col, "does not exist"))
  }
  
  if (!group_col %in% names(data)) {
    stop(paste("Error: column", group_col, "does not exist"))
  }
  
  data <- data %>%
    group_by(.data[[group_col]]) %>%
    mutate(
      !!value_col := as.integer(.data[[value_col]]),
      !!value_col := (.data[[value_col]] - min(.data[[value_col]], na.rm = TRUE)) /
        (max(.data[[value_col]], na.rm = TRUE) - min(.data[[value_col]], na.rm = TRUE))
    ) %>%
    ungroup()
  
  return(data)
}



#' Subset Data by Time Range
#'
#' Trims data to a specified time window.
#'
#' @param data A `data.frame`.
#' @param time_col Column name containing time values.
#' @param down Optional: starting time (inclusive).
#' @param up Optional: ending time (inclusive).
#'
#' @return A `data.frame` with rows between the specified times.
#' @export
set_hour <- function(data, time_col, down = NaN, up = NaN) {
  
  data[[time_col]] <- as.character(data[[time_col]])
  times <- as.character(unique(data[[time_col]]))
  
  if (!is.nan(down)) {
    down <- as.character(down)
    
    down_rm <- c()
    
    for (val in rev(times)) {
      if (val == down) {
        break
      }
      
      down_rm <- c(down_rm, val)
      
    } 
    
    data <- data[!as.character(data[[time_col]]) %in% down_rm,]
  }
  
  
  if (!is.nan(up)) {
    up <- as.character(up)
  
    up_rm <- c()
    
    for (val in times) {
      if (val == up) {
        break
      }
      
      up_rm <- c(up_rm, val)
      
      
      
    } 
    
    data <- data[!as.character(data[[time_col]]) %in% up_rm,]
  }
  
  return(data)
  
}



#' Sample Entropy
#'
#' Calculates the sample entropy of a numeric time series.
#'
#' @param series Numeric vector (time series).
#' @param m Embedding dimension.
#' @param r Tolerance as a proportion of the standard deviation.
#'
#' @return Numeric value representing sample entropy. Returns `Inf` if undefined.
#' @export
sample_entropy <- function(series, m = 2, r = 0.2) {
  N <- length(series)
  sd_series <- sd(series)
  r_scaled <- r * sd_series
  
  phi <- function(m) {
    x <- embed(series, m)
    count <- 0
    for (i in 1:(nrow(x) - 1)) {
      for (j in (i + 1):nrow(x)) {
        if (max(abs(x[i, ] - x[j, ])) <= r_scaled) {
          count <- count + 1
        }
      }
    }
    return(count)
  }
  
  A <- phi(m + 1)
  B <- phi(m)
  if (B == 0 || A == 0) return(Inf)
  return(-log(A / B))
}


#' Compute Regularity Disruption Index (RDI)
#'
#' Calculates the Regularity Disruption Index (RDI) from time series activity data 
#' using a band-pass filter and sample entropy.
#'
#' @param activity Numeric vector of activity values.
#' @param lambda Numeric threshold for activity; values below are set to 0. Default is 0.005.
#' @param m Integer; the embedding dimension for sample entropy. Default is 2.
#' @param r Numeric; the tolerance (similarity) parameter for sample entropy. Default is 0.2.
#' @param flow Numeric; low cutoff frequency for the band-pass filter. Default is 1/2000.
#' @param fhigh Numeric; high cutoff frequency for the band-pass filter. Default is 1/300.
#' @param fs Numeric; sampling frequency. Default is 1.
#'
#' @return A single numeric value representing the RDI.
#' @export
#'
#' @examples
compute_rdi <- function(activity, lambda = 0.005, m = 2, r = 0.2,
                        flow = 1/2000, fhigh = 1/300, fs = 1) {
  activity[activity < lambda] <- 0
  
  order <- 4
  wn <- c(flow, fhigh) / (fs / 2)
  bf <- butter(order, wn, type = "pass")
  filtered <- filtfilt(bf, activity)
  
  rdi <- sample_entropy(filtered, m = m, r = r)
  
  return(rdi)
}



#' Calculate RDI for Each Animal in Multiple Groups
#'
#' Computes the RDI for individual animals across different experimental groups.
#'
#' @param data A data frame containing the activity data.
#' @param animal_col Character; name of the column indicating animal ID. Default is "Animal No.".
#' @param group_col Character; name of the column indicating experimental group. Default is "group".
#' @param groups Optional vector of groups to include; if not specified (default is `NaN`), all groups are used.
#' @param stat_col Character; name of the column with activity values for RDI calculation.
#' @param lambda Threshold below which activity is set to zero. Default is 0.005.
#' @param m Embedding dimension for sample entropy. Default is 2.
#' @param r Tolerance parameter for sample entropy. Default is 0.2.
#' @param flow Low cutoff frequency for the filter. Default is 1/2000.
#' @param fhigh High cutoff frequency for the filter. Default is 1/300.
#' @param fs Sampling frequency. Default is 1.
#'
#' @return A data frame with columns: Group, Animal, RDI.
#' @export
#'
#' @examples
mvd_rdi <- function(data,
                    animal_col = 'Animal No.',
                    group_col = 'group',
                    groups = NaN,
                    stat_col,
                    lambda = 0.005, 
                    m = 2, 
                    r = 0.2,
                    flow = 1/2000, 
                    fhigh = 1/300, 
                    fs = 1) {
  
  
  full <- data.frame()
  
  if (is.nan(groups)[1]) {
    groups <- unique(data[[group_col]])
  }
  
  
  for (g in groups) {
    
    tmp <- data[data[[group_col]] == g, ]
    animals <- unique(tmp[[animal_col]])
    
    for (f in animals) {
      
      activity <- tmp[tmp[[animal_col]] == f, ][[stat_col]]
      
      rdi_value <- compute_rdi(as.numeric(activity) ,lambda, m, r, flow, fhigh, fs)
      
      full <- rbind(full, data.frame(
        Group = g,
        Animal = f,
        RDI = rdi_value
        
      ))
      
    }
  }
  
  return(full)
}




#' Compute RDI Over Time Intervals
#'
#' Calculates the RDI for each animal within specified time intervals.
#'
#' @param data A data frame containing the time series data.
#' @param interval Integer; number of time points per interval (e.g., 60). Default is 60.
#' @param time_col Character; name of the column representing time. Default is "Time".
#' @param animal_col Character; name of the column indicating animal ID. Default is "Animal No.".
#' @param group_col Character; name of the column indicating experimental group. Default is "group".
#' @param groups Optional vector of groups to include; if not specified, all groups are included.
#' @param stat_col Character; name of the column with activity values for RDI calculation.
#' @param lambda Threshold below which activity is set to zero. Default is 0.005.
#' @param m Embedding dimension for sample entropy. Default is 2.
#' @param r Tolerance parameter for sample entropy. Default is 0.2.
#' @param flow Low cutoff frequency for the filter. Default is 1/2000.
#' @param fhigh High cutoff frequency for the filter. Default is 1/300.
#' @param fs Sampling frequency. Default is 1.
#'
#' @return A data frame with columns: Group, Animal, RDI, interval.
#' @export
#'
#' @examples
mvd_rdi_interval <- function(data,
                             interval = 60,
                             time_col = 'Time',
                             animal_col = 'Animal No.',
                             group_col = 'group',
                             groups = NaN,
                             stat_col,
                             lambda = 0.005, 
                             m = 2, 
                             r = 0.2,
                             flow = 1/2000, 
                             fhigh = 1/300, 
                             fs = 1) {
  
  inter <- unique(data[[time_col]])
  
  max_i <- length(inter)
  
  break_len <- floor(max_i/interval)
  
  full <- data.frame()
  
  for (b in 1:break_len) {
    
    tmp <- set_hour(data, 
                    time_col = time_col, 
                    down = inter[((interval*b) + 1)], 
                    up = inter[(((interval*b) - interval) + 1)]) 
    
    
    tmp_rdi = mvd_rdi(tmp, 
                      animal_col = animal_col,
                      group_col = group_col,
                      groups = groups,
                      stat_col = stat_col,
                      lambda = lambda, 
                      m = m, 
                      r = r,
                      flow = flow, 
                      fhigh = fhigh, 
                      fs = fs) 
    
    tmp_rdi$interval <- paste(inter[(((interval*b) - interval) + 1)], '-', inter[((interval*b) + 1)])
    
    full <- rbind(full, tmp_rdi)
    
  }
  
  
  return(full)
  
}