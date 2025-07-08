
source('https://raw.githubusercontent.com/jkubis96/MoveCalcR/refs/heads/main/scripts/moveCalcR.R')

# loading scripts for statistics and visualization --> https://github.com/jkubis96/JStatML-R
source("https://raw.githubusercontent.com/jkubis96/JStatML-R/main/scripts/statML-R.R")



example_data <- read_mvd(path = 'https://raw.githubusercontent.com/jkubis96/MoveCalcR/refs/heads/main/data/example_data.csv')


example_metadata <- read_mvd_meta(path = 'https://raw.githubusercontent.com/jkubis96/MoveCalcR/refs/heads/main/data/example_data.csv')



data <- group_mdv(data = example_data, metadata = example_metadata, animal_col = "Animal No.", group_col_meta = "Text1")

data <- set_hour(data, time_col = 'Time', down = '16:00:00', up = '08:00:00') 



# variables to calculate 
print(colnames(data))
  
plot_movment <- mvd_map(data, animal_col = "Animal No.", 
                       time_col = "Time", 
                       stat_col = "XT+YT[Cnts]", 
                       group_col = 'group', 
                       group_sort = c('HEALTHY', 'DISEASE'))


plot_movment

ggsave('../fig/movement_plot.png', plot_movment, dpi = 300, width = 8, height = 4)


  
  
rescaled_data <- rescale_by_group(data, value_col =  "XT+YT[Cnts]", group_col  = "Animal No.")



plot_movment_rescaled <- mvd_map(rescaled_data, animal_col = "Animal No.", 
                                time_col = "Time", 
                                stat_col = "XT+YT[Cnts]", 
                                group_col = 'group', 
                                group_sort = c('HEALTHY', 'DISEASE'))


plot_movment_rescaled

ggsave('../fig/movement_plot_rescaled.png', plot_movment_rescaled, dpi = 300, width = 8, height = 4)





full_time_rdi = mvd_rdi(data, 
              animal_col = 'Animal No.',
              group_col = 'group',
              groups = c('HEALTHY', 'DISEASE'),
              stat_col = "XT+YT[Cnts]",
              lambda = 0.005, 
              m = 2, 
              r = 0.2,
              flow = 1/2000, 
              fhigh = 1/300, 
              fs = 1) 


  
results <- two_groups_analysis(value_column = 'RDI', 
                               grouping_column = 'group', 
                               data = full_time_rdi, 
                               bar_queue = c('HEALTHY', 'DISEASE'), 
                               x_label = 'Group', 
                               x_angle = 30, 
                               y_label = 'RDI', 
                               size = 10, 
                               bar_size = 0.5, 
                               parametric = FALSE, 
                               paired = FALSE, 
                               bars = 'sd', 
                               bars_size = 1, 
                               stat_plot_ratio = 0.15, 
                               y_break = NaN, 
                               brew_colors = 'Dark2') 

  
results@bar_plot

ggsave('../fig/total_time_RDI.png', results@bar_plot, dpi = 300, width = 8, height = 4)

  
  
  


interval_rdi <- mvd_rdi_interval(data,
                           interval = 60,
                           time_col = 'Time',
                           animal_col = 'Animal No.',
                           group_col = 'group',
                           groups =  c('HEALTHY', 'DISEASE'),
                           stat_col =  "XT+YT[Cnts]",
                           lambda = 0.005, 
                           m = 2, 
                           r = 0.2,
                           flow = 1/2000, 
                           fhigh = 1/300, 
                           fs = 1) 






interval_rdi_plot <- multi_var_groups_analysis(interval_rdi,
                                   stat_col = "RDI", 
                                   interval_col = 'interval', 
                                   group_col = 'group',
                                   parametric = TRUE,
                                   paired = FALSE,
                                   adj = NA,
                                   error = 'sd',
                                   tx_pos = 0.01)



interval_rdi_plot@plot

ggsave('../fig/interval_time_RDI.jpg', interval_rdi_plot@plot, dpi = 300, width = 8, height = 4)

  