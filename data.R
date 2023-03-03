# install packages if not installed

list_of_packages <-
    c("readxl", "zoo", "tidyverse") # might add as the project goes on
new_packages <-
    list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])] # checks whether the packages is in installed packages and returns the ones that are not installed
if (length(new_packages))
    install.packages(new_packages) # if there are any, installs the new packages

# libraries to be used

lapply(list_of_packages, library, character.only = TRUE)

# reading the data

# note: don't forget to set your working directory, and put your data to a folder named "data"

filenames = list.files("data") # gets the list of names in the data directory
for (filename in filenames) {
    # loop through all files in the data folder
    my_data <-
        read_excel(
            paste0("data/", filename),
            na = "-",
            skip = 1,
            guess_max = min(8400, n_max = NULL)
        ) #reads the excel
    station_names <-
        read_excel(paste0("data/", filename),
                   range = cell_rows(1),
                   col_names = FALSE) %>% slice(1) %>% unlist(use.names = FALSE) # gets the station names for parsing
    while (ncol(my_data) != length(station_names)) {
        # this is for matching the length of parameters while renaming
        station_names <- c(station_names, NA)
    }
    station_names <-
        na.locf(station_names) # NA values are changed in order to match the previous ones
    
    custom_names <-
        function(n)
            paste0(n, " (", station_names, ")") # parsing function
    my_data <-
        read_excel(
            paste0("data/", filename),
            na = "-",
            skip = 1,
            .name_repair = custom_names,
            guess_max = min(nrow(my_data), n_max = NULL)
        ) %>% rename("Date" = " (Tarih)") %>% as.data.frame() # naming corrections
    for (i in 2:ncol(my_data)) {
        my_data[, i] <-
            sub(",", ".", sub("\\.", "", my_data[, i]))
    } # replacing commas with dots
    
    my_data <-
        my_data %>% as_tibble() %>% mutate_if(is.character, as.numeric) # changing the type of elements in the data.frame and turning it into tibble
    
    
    dir.create("figs", showWarnings = FALSE) # creating a file for the figures
    for (station in colnames(my_data)[colnames(my_data) != "Date"]) {
        plt <-
            ggplot(my_data, aes(x = Date, y = .data[[station]])) + geom_line() + theme_minimal()
        print(plt) # shows in the RStudio
        ggsave(
            paste0("figs/", gsub(" ", "", gsub(
                "/", "", station
            )), ".png"),
            width = 6,
            height = 4,
            dpi = 300
        ) # saves in the folder
        Sys.sleep(2)
    }
}
