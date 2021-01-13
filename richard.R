library(tidyverse)
library(tidyr)

physicians = read.delim('physicians.csv', sep=',')

# General info on overall dataset
summary(physicians)
head(physicians)

# Drop columns which are not relevant for us, since they are unique to a person.
# - First_Name, Middle_Name, Last_Name, Name_Suffix
physicians = subset(physicians, select=-c(First_Name, Middle_Name, Last_Name, Name_Suffix))

# Country
# "UNITED STATES" = 5999
# "UNITED STATES MINOR OUTLYING ISLANDS" = 1
# Is kind of always US, not relevant -> DROP
unique(physicians$Country)
table(physicians$Country)

physicians = subset(physicians, select=-c(Country))

# Province
# All have "N/A" value -> DROP
unique(physicians$Province)

physicians = subset(physicians, select=-c(Province))

# Set
# "train" or "test" set
# TODO: Split the data frame into a "test" and a "train" set.
unique(physicians$set)

# City
# 1846 different city values, might be quite a lot to deal with initially.
unique(physicians$City)
length(unique(physicians$City))

# State
# Quite a nice distribution, we've got physicians for all the 52 states.
plot(table(physicians$State))
length(unique(physicians$State))
plot(table(physicians$State))

# Zipcode
# TODO: Does someone know how ZIP codes work in the US? They are always 5 digits, then sometimes separated by a dash (-) follow another 4 digits. Maybe the first one is more generic and the second part more specific? Then we could narrow the number of values a bit down?
# 5491 unique values are quite a lot. Unless we drill it down or join it somehow, I see no value in them.
length(unique(physicians$Zipcode))

physicians = subset(physicians, select=-c(Zipcode))

# Primary_Specialty
# TODO: Multiple values joined by "|" character, needs to be split, we could then turn them into boolean flags? Or count the number of specialties each physician has?
# This does not work yet, because it has a fixed set of columns: separate(data=physicians, col=Primary_Specialty, into=c('a', 'b', 'c'), sep="\\|")
# Here a list of all unique Specialty values:
unique(unlist(strsplit(unique(physicians$Primary_Specialty), '|', fixed=TRUE)))

# License_State_(1-5)
# A list of columns which all contain one state in case the physician has a license there. It's kind of the inverse situation as the "Primary_Specialty".
# TODO: We could either aggregate to count the total number of license, which could be a indicator of the physicians skill level / market size?
# TODO: We could match their license states with other geographic information, e.g. the state of the companies, or the number of companies in the states they have a license, and see if there are some overlaps?


# Merge all license state fields to have a better overview
physicians = physicians %>% unite(License_State, c(License_State_1, License_State_2, License_State_3, License_State_4, License_State_5), sep=' ')


# DONE

head(physicians)
