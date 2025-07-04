local({
  packages <- c("readr", "ggplot2", "tidyr", "dplyr", "patchwork")
  
  installed <- packages %in% installed.packages()
  if (any(!installed)) {
    install.packages(packages[!installed])
  }
  
  lapply(packages, library, character.only = TRUE)
})





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



group_mdv <- function(data, metadata, animal_col, group_col_meta) {
  
  match_idx <- match(data[[animal_col]], metadata[[animal_col]])
  data$group <- metadata[[group_col_meta]][match_idx]
  
  return(data)
  
}



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


rescale <- function(data, col) {
  if (!col %in% names(data)) {
    stop(paste("Error: column", col, "does not exist"))
  }
  
  data[[col]] <- as.integer(data[[col]])
  data[[col]] <- (data[[col]] - min(data[[col]], na.rm = TRUE)) /
    (max(data[[col]], na.rm = TRUE) - min(data[[col]], na.rm = TRUE))
  
  return(data)
}





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




set_hour <- function(data, time_col, down = NaN, up = NaN) {
  
  times <- as.character(unique(data[[time_col]]))
  
  if (!is.nan(down)) {
    
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
