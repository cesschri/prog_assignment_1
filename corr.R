## function: corr
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
## NOTE: Do not round the result!
corr <- function(directory, threshold = 0) {
    
    # Get data frame containing ID and Number of complete observations for each monitor
    df_obs <- complete(directory)
    
    # Get the number of rows
    nr <- nrow(df_obs)
    
    # if number of rows returned > 0
    if (nr > 0) {
        
        # Define an empty vector
        v_all_cr <- vector()
        
        # Loop through the observations
        for (i in 1:nr) {
            
            # If the number of complete cases > the threshold then
            # calculate the correlation
            if (df_obs$nobs[i] > threshold) {
                
                # Load a temp DF with the rows from the df_tot global variable.
                # df_tot is defined in the function "complete".
                temp_df <- df_tot[df_tot$ID == i, ]
                
                # calculate correlation
                df_cor <<- cor(temp_df$sulfate, temp_df$nitrate)
                
                # Append correlation value to numeric vector
                v_all_cr <- append(v_all_cr, df_cor)
                
            }
            
        }    # end for (i in 1:nr)
        
    }    # end if (nr > 0) 
    
    v_all_cr    # return numeric vector of correlations
    
}    # end function corr
