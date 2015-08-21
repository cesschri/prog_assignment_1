# Part 1
# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
# across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 
# 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate 
# matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant 
# across all of the monitors, ignoring any missing values coded as NA.
pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Set the working directory
    #setwd(directory)
    
    #classes <- c("factor", "numeric", "numeric", "integer")
    #names(classes) <- c("Date", "sulfate", "nitrate", "ID")
    
    # Create an empty data frame that has the same column types as the csv files
    df <- data.frame(Date=as.Date(character()),
                     sulfate=numeric(), 
                     nitrate=numeric(),
                     ID=integer()
    )
    
    classes <- sapply(df, class)
    
    # Loop through the ID vector    
    for (i in id) {
        # Get length of the current ID
        i_len <- nchar(i)
        
        # Create a prefix string the the zeros
        f_0prefix <- substr('0000', 1, 3-i_len)
        
        # Set filename
        filename <- paste(directory, "\\", f_0prefix, i, ".csv", sep="")

        
        # Read csv file data into a temp data frame
        temp_df <- read.csv(file=filename, colClasses = classes, header=T, sep=",")
        
        # Append temp_df to df
        df <- rbind(df, temp_df)
    }
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    mean_val <- if (pollutant == "sulfate") {
        mean(df$sulfate, na.rm = TRUE)
    } else {
        mean(df$nitrate, na.rm = TRUE)
    }
    
}
