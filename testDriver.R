# Programming Assignment 1: Air Pollution
# Test Cases


# Set the path variable
dirpath <- "C:\\Users\\cesschri\\Coursera\\2 - R_Programming\\week2\\prog_assignment_1\\specdata\\"

dirpath <- "specdata"

################## Part 1

source("C:\\Users\\cesschri\\Coursera\\2 - R_Programming\\week2\\prog_assignment_1\\pollutantmean.R")

# Run some tests
theMean <- pollutantmean(dirpath, "sulfate", 1:10)
print(theMean)

theMean <- pollutantmean(dirpath, "nitrate", 70:72)
print(theMean)

theMean <- pollutantmean(dirpath, "nitrate", 23)
print(theMean)

writeLines('\n')

print("done!!!!")


################## Part 2

source("C:\\Users\\cesschri\\Coursera\\2 - R_Programming\\week2\\prog_assignment_1\\complete.R")

complete(dirpath, 1)
complete(dirpath, c(2, 4, 8, 10, 12))
complete(dirpath, 30:25)
complete(dirpath, 3)


################## Part 3

#source("C:\\Users\\cesschri\\Coursera\\2 - R_Programming\\week2\\prog_assignment_1\\complete.R")
source("C:\\Users\\cesschri\\Coursera\\2 - R_Programming\\week2\\prog_assignment_1\\corr.R")
cr <- corr(dirpath, 150)
head(cr)
summary(cr)

cr <- corr(dirpath, 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)
