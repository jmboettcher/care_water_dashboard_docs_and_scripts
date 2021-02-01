library(dplyr)
library(tidyr)
library(writexl)
library(readxl)
# NOTE: if you have any trouble parsing any file, try saving or reading it as an excel file rather than a csv
# Some of the delimiters may be finicky based on the content of columns. read.csv occurs once, but could
# be replaced with read_excel if it's giving you trouble

# Use this function if you'd like to clean/process data from one raw data file. See initial_processing and save_manual_ready_split for more info on how
# it's processed.
# 
# origin_file: file containing raw data
# cleaning_guide_file: file mapping column names in origin_file to Tableau field names
# date_string: date ("mm-dd-yyyy") for the spreadsheet data
# cleaned_dest: file name and path for your cleaned data
# manual_dest: file name and path for data to manually check
clean_file<-function(origin_file,cleaning_guide_file,date_string,cleaned_dest,manual_dest){
  care.df<-initial_processing(origin_file,cleaning_guide_file,date_string)
  save_manual_ready_split(care.df,cleaned_dest,manual_dest)
}

# Use this function if you'd like to clean/process data from multiple raw data file, that all have the same column names
# (and thus the same original column name to Tableau field name mapping)
# See initial_processing and save_manual_ready_split for more info on how it's processed.
# 
# REQUIREMENT: you need to be in a directory with only files that you'd like to process
# cleaning_guide_file: file mapping column names in origin_file to Tableau field names
# date_string_list: list of dates ("mm-dd-yyyy") for the spreadsheets, with order corresponding to file name order in folder
# cleaned_dest: file name and path for your cleaned data
# manual_dest: file name and path for data to manually check
clean_files<-function(cleaning_guide_file,date_string_list,cleaned_dest,manual_dest){
  origin_file_list<-list.files()
  counter = 1
  care.df = tibble()
  for (origin_file in origin_file_list) {
    care.df <- bind_rows(care.df,initial_processing(origin_file,cleaning_guide_file,date_string_list[counter]))
    counter = counter + 1
  }
  save_manual_ready_split(care.df,cleaned_dest,manual_dest)
}

# This function joins multiple cleaned sets. This is useful if you have multiple files that have different column names
# and thus can't be run under clean_files(), but you'd like to join them.
# 
# REQUIREMENT: you need to be in a directory with only files that you'd like to join
# destination: file name and path for combined file
save_joined_cleaned<-function(destination){
  origin_file_list<-list.files()
  care.df = tibble()
  for (origin_file in origin_file_list) {
    care.df <- bind_rows(care.df,tibble(read_excel(origin_file)))
  }
  write_xlsx(care.df,destination)
}

# HELPER FUNCTION
# This function processes raw data in order to produce a cleaned spreadsheet with only desired columns
# (mapping from raw data column names to Tableau field names provided in cleaning_guide.csv. raw data column
# names can be altered to adjust for different original raw data column names).
# It ensures that columns of each intended type follow the expected data format.
# 
# origin_file: file containing raw data
# cleaning_guide_file: file mapping column names in origin_file to Tableau field names
# date_string: date ("mm-dd-yyyy") for the spreadsheet data
initial_processing<-function(origin_file,cleaning_guide_file,date_string){
  # read in origin_file with raw data
  care.df<-tibble(read.csv(origin_file))
  # read in file mapping origin_file column names to field names expected in Tableau
  cleaning_guide<-tibble(read.csv(cleaning_guide_file))
  
  # split up cleaning guide by which fields have which expected type
  str_guide<-filter(cleaning_guide,intended_type=="str")
  bool_guide<-filter(cleaning_guide,intended_type=="bool")
  int_guide<-filter(cleaning_guide,intended_type=="int")
  dbl_guide<-filter(cleaning_guide,intended_type=="dbl")
  rm(cleaning_guide)
  
  # create lists of original column names by expected type
  str_cols<-str_guide$original_selected
  bool_cols<-bool_guide$original_selected
  int_cols<-int_guide$original_selected
  dbl_cols<-dbl_guide$original_selected
  
  # list of new column names
  new_colnames<-c(str_guide$new_name,bool_guide$new_name,int_guide$new_name,dbl_guide$new_name)
  
  # eliminates exact duplicate rows, only selects WASH programs, renames columns by replacing space with period for easier reference
  care.df<-care.df %>%
    distinct() %>%
    filter(WASH=="WASH") %>% 
    select(matches(c(str_cols,bool_cols,int_cols,dbl_cols))) %>%
    rename_with(function(x){gsub("[^a-zA-Z]",".",x)})
  
  # select columns that match cleaning guide columns
  care.df<-select(care.df, all_of(c(str_cols,bool_cols,int_cols,dbl_cols)))
  
  # processes/cleans data
  # sets string columns to -- if no data
  # sets boolean columns to T or F depending on if they're yes
  # sets integer columns to be integers rather than string formats, 0 if NA
  # sets double columns to change from % to decimal, 0 if NA
  care.df <- care.df %>% 
    mutate(across(contains(str_cols),function(x){gsub("^ *0 *$","--",x)})) %>% 
    mutate(across(contains(bool_cols),function(x){x=="YES"})) %>% 
    mutate(across(contains(int_cols),function(x){replace_na(as.integer(gsub("-","0",gsub(",","",x))),0)})) %>% 
    mutate(across(contains(dbl_cols),function(x){replace_na(as.double(gsub("%","",x))/100,0)}))
  
  care.df<-rename_with(care.df,function(x){return(new_colnames)})
  #This can sometimes give you trouble because of the carriage returns, use the tableau data interpreter and you should be fine
  
  Date<-rep(date_string,248)
  care.df<-tibble(Date,care.df)
  
  return(care.df)
}

# HELPER FUNCTION
# It sets aside duplicated rows and rows whose numbers don't add up for manual checking.
# It saves spreadsheet of items that do not require manual checking and can go straight into the dashboard.
# cleaned_dest: file name and path for your cleaned data
# manual_dest: file name and path for data to manually check
save_manual_ready_split<-function(care.df,cleaned_dest,manual_dest){
  # identifies duplicated project name - date combinations to check
  dup_project_names<-care.df$`Project/Initiative Name`[duplicated(select(care.df,Date,`Project/Initiative Name`))]
  
  # filters out parts of spreadsheet with duplicates
  manual_checka<-filter(care.df,`Project/Initiative Name` %in% dup_project_names)
  
  #filters out parts of the spreadsheet where numbers don't add up logically
  manual_checkb<-filter(care.df,(`Total Indirect Participants For Year`<`WASH Development Indirect Participants For Year`+`WASH Humanitarian Indirect Participants For Year`))
  manual_checkc<-filter(care.df,(`Total Direct Participants For Year`<`WASH Development Direct Participants For Year`+`WASH Humanitarian Direct Participants For Year`))
  
  # includes description of issue
  manual_checka<-mutate(manual_checka,problema="duplicated project name in a single year")
  manual_checkb<-mutate(manual_checkb,problemb="total indirect participants for year < sum of WASH indirect participants for year")
  manual_checkc<-mutate(manual_checkc,problemc="total direct participants for year < sum of WASH direct participants for year")
  
  # joins these parts of the spreadsheet that require a manual eye
  manual_check<-full_join(full_join(manual_checka,manual_checkb),manual_checkc)
  
  # extracts the parts of the spreadsheet that are ready to go
  tableau_ready<-setdiff(care.df,select(manual_check,-problema,-problemb,-problemc))
  
  # optionally, include unique codes for projects. this will provide for at least 17576 projects.
  codes<- c()
  letters<- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
  proj_names<-distinct(select(tableau_ready,`Project/Initiative Name`))
  for (x in 1:nrow(proj_names)) {
    one <- letters[((x-1)%/%676)+1]
    remainder <- ((x-1)%%676)+1
    two <- letters[((remainder-1)%/%26)+1]
    three <- letters[((remainder-1)%%26)+1]
    codes <- c(codes,paste0("~",one,two,three,"~"))
  }
  
  proj_names<-bind_cols(proj_names,tibble(codes))
  tableau_ready<-left_join(tableau_ready,proj_names)
  
  # write cleaned and manual check documents
  write_xlsx(tableau_ready,cleaned_dest)
  write_xlsx(manual_check,manual_dest)
}


