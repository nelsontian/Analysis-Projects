# Set working directory
# working directory should contain 4 folders: "Group 1", ... , "Group 4", each subject folders inside
setwd("/BMBLAB/Processed Data/")
dir()

# used for the setnames function to rename an added column
library(data.table)

# names of the 9 region columns to be added
regcol = c("AntLeft","AntMid","AntRight","CentLeft","CentMid","CentRight","PostLeft","PostMid","PostRight")

# 64 channel net locations for each region (e.g., al = Anterior Left)
al = c(17,18,13,11,12,10)
am = c(8,9,6,3)
ar = c(1,5,2,60,59,58)
cl = c(24,25,22,20,26,27)
cm = c(21,65,41)
cr = c(50,46,49,48,52)
pl = c(30,32,31,28)
pm = c(34,36,33,35,37)
pr = c(38,40,42,45,44)
# save the names of the location lists onto one master 64 list
reg64 = c("al","am","ar","cl","cm","cr","pl","pm","pr")

# 128 channel net locations for each region
AL = c(26,33,34,28,25,24,23,27)
AM = c(17,22,18,19,16,11,10,15,14)
AR = c(8,9,3,2,1,124,123,122)
CL = c(47,42,37,51,48,43,38)
CM = c(32,129,81,88,80,55)
CR = c(105,104,103,94,99,98)
PL = c(54,53,52,61,60,59,58)
PM = c(62,68,73,72,77,67,71,76)
PR = c(87,93,79,78,86,92,97)
# do the same for 128
reg128 = c("AL","AM","AR","CL","CM","CR","PL","PM","PR")

# the below 3 functions will be called by the group_merge function below

# function to add a column for the selected region and give it an appropriate name
# used to create columns AntLeft, ..., PostRight
region_columns = function(df, colname, region)
{
  df$newcol = rowMeans(df[,region])
  setnames(df, old = "newcol", new = colname)
  return(df)
}

# function used to add a specific column into a dataframe
# used for auxillary info such as group, ID, Location, etc.
add_column = function(df, colname, values)
{
  df$newcol = values
  setnames(df, old = "newcol", new = colname)
  return(df)
}

# function to reorder columns so that group, ID, etc. is sorted first and region later
reorder_column = function(df)
{
  df = df[,c(10:14, 1:9)]
  return(df)
}

# subjects in groups 1 and 2 who are early bilinguals (not heritage)
eb1 = c(102,108,110,114,116,118,119,120,121)
eb2 = c(203,213,215)

# subjects in groups 1 and 2 who are heritage speakers
hs1 = c(101,103,107,111:113,115,117)
hs2 = c(214,219)

# subjects in group 3 which are spanish controls
sc1 = c(301,303,304,305,306,308,309,311,312,314,315,316,317,318,322,327)

# csv file with Location and Condition information, should be 36 rows long
setwd("/BMBLAB/Processed Data/")
LC = read.csv("LocationCondition.csv")
nrow(LC) # should be 36
head(LC)


# nested looper function to read in every dataset in one group
# participant is a vector of subject #
group_merge = function(participant)
{
  for(g in participant)
  {
    # find the folder of a specific participant
    location = sprintf("EH%s txt", g)
    setwd(paste(location))
    
    # read in every text file, add data columns, and merge them into a full per-subject dataframe
    for (i in 1:36)
    {
      # contains the name of the variable that the data will be stored in
      # e.g. "CausDOA101" will be varname if location is DOA, condition is Caus, and subject # is 101
      varname = sprintf("%s%s%s", LC[i,2], LC[i,1], g)
      
      # read in text file and save it to the variable name saved in varname (e.g. save text file data to CausDOA101)
      # use assign() instead of <- because it works with the value in varname
      assign(varname, read.delim(sprintf("%s.%s, Ave EH%s.txt", LC[i,2], LC[i,1], g), header = FALSE))
      # treat 128ch nets and 64ch nets differently
      if (ncol(get(varname)) == 129)
      {
        # assign the 9 regions
        for(j in 1:length(regcol))
        {
          # call region_columns 9 times to create columns for AntLeft, ..., PostRight
          assign(varname, region_columns(get(varname), regcol[j], get(reg128[j])))
        }
        # remove the other variables now that the region columns are set
        assign(varname, get(varname)[130:138])
      }
      else
      {
        # assign the 9 regions, but for 64ch
        for(j in 1:length(regcol))
        {
          assign(varname, region_columns(get(varname), regcol[j], get(reg64[j])))
        }
        assign(varname, get(varname)[66:74])
      }
      # call add_column to make a new column for group, ID, location, condition, and observation
      assign(varname, add_column(get(varname), "Group", rep(1, 300)))
      assign(varname, add_column(get(varname), "ID", rep(g, 300)))
      assign(varname, add_column(get(varname), "Location", rep(LC[i, 1], 300)))
      assign(varname, add_column(get(varname), "Condition", rep(LC[i, 2], 300)))
      assign(varname, add_column(get(varname), "Obs", 1:300))
      # call reorder_column to reorder the columns to Group, ..., Obs, AntLeft, ..., PostRight
      assign(varname, reorder_column(get(varname)))
      
      # the name of the complete dataset per subject (e.g. "All101" for everything in subject 101)
      cname = sprintf("All%s", g)
      # if this is the first text file, make a variable to store all of the data
      # then delete the condition-specific dataset
      if(i == 1)
      {
        assign(cname, get(varname))
      }
      # else, update this variable
      else
      {
        assign(cname, rbind(get(cname), get(varname)))
      }
      # we don't need the variable named by varname anymore
      # e.g. delete CausDOA101 after merging into All101
      rm(list=paste(varname))
    }
    
    # if this is the first participant, make an AllGroup variable
    if(g == participant[1])
    {
      AllGroup = get(cname)
    }
    # else, update this variable
    else
    {
      AllGroup = rbind(AllGroup, get(cname))
    }
    rm(list=paste(cname))
    # move back up to the Group folder
    setwd("..")
  }
  # will eventually add a write.csv to export complete group dataset to an excel file
  # write.csv to save the file
  return(AllGroup)
}

# call the looper

# heritage speakers
setwd("/BMBLAB/Processed Data/Group 1")
AllHS1 = group_merge(hs1)
setwd("/BMBLAB/Processed Data/Group 2")
AllHS2 = group_merge(hs2)
AllHS = rbind(AllHS1, AllHS2)
write.csv(AllHS, file = "AllHS.csv", row.names = FALSE)

# all spanish controls
setwd("/BMBLAB/Processed Data/Group 3")
AllSC = group_merge(sc1)
write.csv(AllSC, file = "AllSC.csv", row.names = FALSE)

# early bilinguals who aren't heritage speakers
setwd("/BMBLAB/Processed Data/Group 1")
AllEB1 = group_merge(eb1)
setwd("/BMBLAB/Processed Data/Group 2")
AllEB2 = group_merge(eb2)
AllEB = rbind(AllEB1, AllEB2)
write.csv(AllEB, file = "AllEB.csv", row.names = FALSE)