

# Part 2
# Write a function that reads a directory full of files and reports the number of completely 
# observed cases in each data file. The function should return a data frame where the first column 
# is the name of the file and the second column is the number of complete cases.
complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # Create an empty data frame that has the same column types as the csv files
    # It is a global variable which can be referenced in code outside this function
    df_tot <<- data.frame(Date=as.Date(character()),
                     sulfate=numeric(), 
                     nitrate=numeric(),
                     ID=integer()
    )
    
    classes <- sapply(df_tot, class)
    
    obs_df <- data.frame(id=numeric(),
                         nobs=integer(),
                         stringsAsFactors=FALSE
    )
    
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
        
        
        # load data frame obs with all complete rows 
        temp_df <- temp_df[!is.na(temp_df$sulfate) & !is.na(temp_df$nitrate), ]
        #print(obs)
        
        # get number of rows
        nr <- nrow(temp_df)
        
        # Append complete rows to total data frame
        df_tot <<- rbind(df_tot, temp_df)
        
        # Append new row to obs_df data frame
        obs_df[nrow(obs_df) + 1, ] <- c(i, nr)
        
    }
    obs_df    # return vector of id, nobs
}
