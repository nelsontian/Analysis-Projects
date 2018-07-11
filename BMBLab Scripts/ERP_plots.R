# this script uses csv files that were created from ERP_merge.R
library(dplyr)
library(ggplot2)

# set working directory, should contain 3 excel files
setwd("/BMBLAB/Processed Data/")

# read in data for heritage speakers, early bilinguals, and spanish controls
AllHS = read.csv("AllHS.csv")
AllEB = read.csv("AllEB.csv")
AllSC = read.csv("AllSC.csv")

# reads in the dataframe and a specified location string
# then plots the averaged voltage per condition and location and saves to png
filtered_plot = function(df, loc, group)
{
  # filters it to specified location and the three desired contitions
  df_filt = df %>%
    filter(Location == loc) %>%
    filter(Condition %in% c("Caus","PsCaus","Trans"))
  # adds a time column
  df_filt$Time = seq(-200,996,by = 4)
  # removes the unncessary columns like participant and obs
  df_filt = df_filt[,c(4,6:15)]
  # finds the mean of each region grouped by time and condition
  df_byT = aggregate(.~ Time + Condition, df_filt, mean)
  
  # loop to plot each region graph
  for(i in 1:length(regcol))
  {
    # make a plot
    p_temp = ggplot(df_byT, aes(x = Time, y = get(regcol[i]), color = Condition, group = Condition)) +
      geom_line(aes(linetype = Condition)) +
      ylab(paste(regcol[i])) +
      # change scales
      scale_x_continuous(minor_breaks = seq(-100, 1000, 100),
                         breaks = seq(-100, 1000, 100),
                         limits = c(-100,1000),
                         expand = c(0, 0)) +
      scale_y_continuous(minor_breaks = seq(-3, 3, .5),
                         breaks = seq(-3, 3, .5), 
                         limits = c(-3, 3),
                         expand = c(0,0)) +
      # add lines at origin
      geom_hline(yintercept=0, linetype = "dashed") +
      geom_vline(xintercept=0, linetype = "dashed") +
      # condition colors (does not work)
      scale_color_manual(values = c("red1", "blue1", "green3")) +
      # remove grey background
      theme_bw()
    # print out the plot
    print(p_temp)
    # set the output name
    pltname = sprintf("%s_%s_%s.png",group, loc, regcol[i])
    # use ggsave to export to png
    ggsave(pltname,p_temp, width = 5, height = 3)
  }
}


setwd("/BMBLAB/Processed Data/R plots/HS")
for(i in 1:length(levels(AllHS$Location)))
{
  filtered_plot(AllHS, levels(AllHS$Location)[i], "HS")
}

setwd("/BMBLAB/Processed Data/R plots/EB")
for(i in 1:length(levels(AllEB$Location)))
{
  filtered_plot(AllEB, levels(AllEB$Location)[i], "EB")
}

setwd("/BMBLAB/Processed Data/R plots/SC")
for(i in 1:length(levels(AllSC$Location)))
{
  filtered_plot(AllSC, levels(AllSC$Location)[i], "SC")
}
