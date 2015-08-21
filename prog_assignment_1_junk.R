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
    setwd(directory)
    
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
        filename <- paste(f_0prefix, i, ".csv", sep="")
        #print(filename)
        
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
    df <- data.frame(Date=as.Date(character()),
                     sulfate=numeric(), 
                     nitrate=numeric(),
                     ID=integer()
    )
    
    classes <- sapply(df, class)
    
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
        filename <- paste(f_0prefix, i, ".csv", sep="")
        #print(filename)
        
        # Read csv file data into a temp data frame
        temp_df <- read.csv(file=filename, colClasses = classes, header=T, sep=",")
        
        # 
        obs <- temp_df[!is.na(temp_df$sulfate) & !is.na(temp_df$nitrate), ]
        #print(obs)
        nr <- nrow(obs)
        
        # Append new row to obs_df data frame
        obs_df[nrow(obs_df) + 1, ] <- c(i, nr)

    }
    
    obs_df
}



corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
}



# Set the path variable
dirpath <- "C:\\Users\\cesschri\\Coursera\\2 - R_Programming\\week2\\prog_assignment_1\\specdata\\"

# Run some tests
theMean <- pollutantmean(dirpath, "sulfate", 1:10)
print(theMean)

theMean <- pollutantmean(dirpath, "nitrate", 70:72)
print(theMean)

writeLines('\n')

obs <- complete(dirpath, 1)
print(obs)
writeLines('\n')

obs <- complete("specdata", c(2, 4, 8, 10, 12))
print(obs)
writeLines('\n')

obs <- complete(dirpath, 30:25)
print(obs)
writeLines('\n')

obs <- complete(dirpath, 3)
print(obs)
writeLines('\n')

print("done!!!!")
