# Load packages for this section
library(abdwr3edata)
library(Lahman)
library(tidyverse)

# Get & set working directory
getwd()
setwd("C:/Users/peter/OneDrive/Documents/Baseball/R/ABDWR")

# 2.4.2

# Displays the first three rows and columns 1 through 10 of the spahn data frame
spahn |>
  slice(1:3) |>
  select(1:10)

# The variables Age, W, L, ERA for the first 10 seasons can be displayed by use of slice() with arguments 1:10 and select() with arguments Age, W, L, ERA
spahn |>
  slice(1:10) |>
  select(Age, W, L, ERA)

# We use this function to obtain some summary statistics such as the median, lower and upper quartiles, and low and high values for the ERA measure
spahn |>
  summarize(
    LO = min(ERA),
    QL = quantile(ERA, .25),
    QU = quantile(ERA, .75),
    M = median(ERA),
    HI = max(ERA)
  )

# We can find the age when Spahn had his lowest ERA by use of the following expression
spahn |>
  filter(ERA == min(ERA)) |>
  select(Age)


# 2.4.3

# We add a new variable to a current data frame using the mutate() function
spahn <- spahn |>
  mutate(FIP = (13 * HR + 3 * BB - 2 * SO) / IP)

# The arrange() function sorts the data frame by the FIP measure, the select() function selects a group of variables, and slice() displays the first six rows of the data frame
spahn |>
  arrange(FIP) |>
  select(Year, Age, W, L, ERA, FIP) |>
  slice(1:6)

# We first use the filter() function with a logical condition indicating that we want the Tm variable to be either BSN or MLN. (We introduce the logical OR operator |). To compare various pitching statistics for the two teams, we use the summarize() function. By using the group_by() argument, the spahn data frame is grouped by Tm, and the mean values of the variables are computed for each group
spahn |>
  filter(Tm == "BSN" | Tm == "MLN") |>
  group_by(Tm) |>
  summarize(
    mean_W.L = mean(W.L, na.rm = TRUE),
    mean_ERA = mean(ERA),
    mean_WHIP = mean(WHIP),
    mean_FIP = mean(FIP)
  )


# 2.4.4

# To append two data frames vertiaclly, we can use the bind_rows()
batting <- bind_rows(NLbatting, ALbatting)

# We wish to match rows from one data frame to rows of the other using a particular variable as a key. We use the function inner_join() where we specify the two data frames and the by argument indicates the common variable to merge by
NL <- inner_join(NLbatting, NLpitching, by = "Tm")

# We use the filter() function - the argument is the logical condition that describes how teams are selected
NL_150 <- NLbatting |>
  filter(HR > 150)


# 2.4.5

# A basic way of creating a vector is by means of the c() (combine) function. We create two vectors by use of the c() function, the games won are stored in the vector W and the games lost are stored in the vector L. The symbol <- is the assignment character in R. R is case sensitive, so R will distinguish the vector L from the vector l
W <- c(8, 21, 15, 21, 21, 22, 14)
L <- c(5, 10, 12, 14, 17, 14, 19)
l

# We want to compute the fraction of winning games and multiply this fraction by 100 to convert it to a percentage. We create a new vector named win_pct by use of the basic multiplication (*) and division (/) operators
win_pct <- 100 * W / (W + L)

# We can display these winning percentages by simply typing the variable name
win_pct

# A convenient way of creating patterned data is by use of the function seq(). We use this function to generate the season years from 1946 to 1952 and store the output to the variable Year
Year <- seq(from = 1946, to = 1952)
Year

# For a sequence of consecutive integer values, the colon notation will also work
# Year <- 1946 : 1952

# Spahn was born in April 1921 and we can compute his age by subtracting 1921 from each season value - the resulting vector is stored in the variable Age
Age <- Year - 1921

# We construct a simple scatterplot of Spahn's winning percentages (vertical) against his age (horizontal) by use of the plot() function
plot(Age, win_pct)


# 2.5.2

# One can use the mean() function to find the average winning percentage of Spahn during this seven-season period
mean(win_pct)

# One can compute this career winning percentage by means of the following R expression
100 * sum(W) / (sum(W) + sum(L))

# One can sort the win numbers from low to high with the sort() function
sort(W)

# The cumsum() function is useful for displaying cumulative totals of a vector
cumsum(W)

# The summary() function applied on the winning percentages displays several summary statistics of the vector values such as the extremes, the quartiles, the median, and the mean
summary(win_pct)


# 2.5.3

# To extract portions of vectors, a square bracket is often used
W[c(1, 2, 5)]

# The first four values of the vector can be extracted by typing
W[1:4]

# By use of a minus index, we remove entries from a vector. If we wish to remove the first and sixth entries of W, we would type
W[-c(1, 6)]

# A logical variable is created in R by the use of a vector together with the operations >, <, == (logical equals), and != (logical not equals). Suppose we are interested in the values in the winning percentage vector Win.Pct that exceed 60%
win_pct > 60

# We use the logical & (AND) operator to find the years where W > 20 and Win.Pct > 60
(W > 20) & (win_pct > 60)

# When did Spahn have his highest winning percentage?
win_pct == max(win_pct)

# We select the corresponding year by indexing Year by this logical vector
Year[win_pct == max(win_pct)]

# We create a logical vector based on W + L > 30, and then choose the seasons using this logical vector
Year[W + L > 30]


# 2.6.1

# We create three character vectors NL, AL, and Winner. Note that we represent each character value by a string of letters enclosed by double quotes. We also define two numberic vectors: N_Games contains the number of games of each series, and Year gives the corresponding seasons
Year <- 2008 : 2017
NL <- c("PHI", "PHI", "SFN", "SLN", "SFN",
        "SLN", "SFN", "NYN", "CHN", "LAN")
AL <- c("TBA", "NYA", "TEX", "TEX", "DET",
        "BOS", "KCA", "KCA", "CLE", "HOU")
Winner <- c("NL", "AL", "NL", "NL", "NL",
            "AL", "NL", "AL", "NL", "AL")
N_Games <- c(5, 6, 5, 7, 4, 7, 7, 5, 7, 7)

# Suppose we want to create a data frame. The above vectors are used to populate the data frame, and we indicate the names of the data frame variables
WS_results <- tibble(
  Year = Year, NL_Team = NL, AL_Team = AL,
  N_Games = N_Games, Winner = Winner)
WS_results

# To find the teams from New York that played in these World Series, we use grep() to match patterns in the text
grep("NY", c(AL, NL), value = TRUE)

# To learn about the number of wins by each league in the 10 World Series, we count the rows by use of the n() function
WS <- WS_results |>
  group_by(Winner) |>
  summarize(N = n())
WS

# The ggplot() function indicates that we are using the data frame WS with variables Winner and N, and the geom_col() function says to graph each frequency value with a column
ggplot(WS, aes(x = Winner, y = N)) +
  geom_col()

# We could let ggplot2 do the summarizing for us by using the geom_bar() function
ggplot(WS_results, aes(x = Winner)) +
  geom_bar()

# We construct a frequency table of the National League representatives in the character vector NL_Team
WS_results |>
  group_by(NL_Team) |>
  summarize(N = n())

# It may be preferable to organize the teams by the division. We can change the organization of the team labels by converting this character type to a factor. The basic arguments to factor() are the vector of data to be converted and a vector levels that gives the ordered values of the variable. Here we list the values ordered by the divisions
WS_results <- WS_results |>
  mutate(
    NL_Team = factor(
      NL_Team,
      levels = c("NYN", "PHI", "CHN", "SLN", "LAN", "SFN")
    )
  )

# One can understand how factor variables are stored by using the str() function to examine the structure of the variable
str(WS_results$NL_Team)

# We reconstruct the table by use of the summarize() function, grouping by the variable NL_Team, we obtain the same frequencies as before, but the teams are now listed in the order specified in the factor() function
WS_results |>
  group_by(NL_Team) |>
  summarize(N = n())


# 2.6.3

# Using the list() function, we create a new list world_series with components Winner, Number.Games, and Seasons
world_series <- list(
  Winner = Winner,
  Number_Games = N_Games,
  Seasons = "2008 to 2017"
)

# If we wish to display the number of games played Number.Games, we can use the list variable name together with the $ symbol and the component name
world_series$Number_Games

# Or we can use the double square brackets to display the second component of the list
world_series[[2]]

# The pluck() function from the purrr package also extracts elements from a list
pluck(world_series, "Number_Games")

# We can use the single square brackets with the name of the component in quotes
world_series["Number_Games"]

# The dollar sign operator can be used to extract a vector from a data.frame
WS_results$NL_Team

# The pull() function achieves the same effect
pull(WS_results, NL_Team)


# 2.7.1

# We create a new data frame ws containing data from all of the World Series with fewer than 8 games played. Using the ggplot2 package, we construct the bar graph of the number of games played in all "best of seven" World Series
ws <- SeriesPost |>
  filter(yearID >= 1903, round == "WS", wins + losses < 8)
crcblue <- "#2905a1"
ggplot(ws, aes(x = wins + losses)) +
  geom_bar(fill = crcblue) +
  labs(x = "Number of games", y = "Frequency")


# 2.7.2

# All functions start with the syntax name_of_function <- function(arguments), where arguments is a list of input variables. All of the work in the function goes inside the curly brackets that follow. The result of the last line of the function is returned as the output
hr_rates <- function(age, hr, ab) {
  rates <- round(100 * hr / ab, 1)
  list(x = age, y = rates)
}

# We enter Mickey Mantle's home run counts in the vector HR, the corresponding at-bats in AB, and the ages in Age. We apply the function hr_rates() with inputes Age, HR, AB and the output is a list
HR <- c(13, 23, 21, 27, 37, 52, 34, 42, 31, 40, 54)
AB <- c(341, 549, 461, 543, 517, 533, 474, 519, 541, 527, 514)
Age <- 19 : 29
hr_rates(Age, HR, AB)

# One can easily construct a scatterplot by the plot() function on the output of the function
plot(hr_rates(Age, HR, AB))


# 2.8.1

# Check the location of the current working directory by typing getwd()
# getwd()

# Change the working directory by use of the setwd() function
# setwd()

# An alternative method of importing data from a file uses the read_csv() function
# spahn <- read_csv(here::here("data/spahn.csv"))


# 2.8.2

# We return to the Mickey Mantle example
mantle_hr_rates <- hr_rates(Age, HR, AB)
Mantle <- tibble(
  Age, HR, AB, Rates = mantle_hr_rates$y
)

# We use the write_csv() function to save the data to the current working directory. This function has two arguments: the R object Mantle, and the output file path
write_csv(Mantle, here::here("C:/Users/peter/OneDrive/Documents/Baseball/R/ABDWR/mantle.csv"))

# Confirm that a new file exists in the curren working directory
list.files(here::here("C:/Users/peter/OneDrive/Documents/Baseball/R/ABDWR"), pattern = "mantle")


# 2.9

# One can install the package into R by means of the command
# install.packages("Lahman")

# One then needs to load the package into R
# library(Lahman)

# To confirm that the package has been loaded correctly, we use the help() function
?Batting


# 2.10

# The filter() function is used to select batting data only for the seasons between 1960 and 1969
Batting_60 <- Batting |>
  filter(yearID >= 1960, yearID <= 1969)

# The splitting is accomplished by the group_by() argument, and the sum of home runs is computed using the summarize() function
hr_60 <- Batting_60 |>
  group_by(playerID) |>
  summarize(HR = sum(HR))

# Using the arrange() function with the desc() argument, we sort this data frame in descending order and display the first four lines
hr_60 |>
  arrange(desc(HR)) |>
  slice(1:4)

# We could perform this sequence in a single pipeline so as to not clutter the workspace with unnecessary intermediate data sets
Batting |>
  filter(yearID >= 1960, yearID <= 1969) |>
  group_by(playerID) |>
  summarize(HR = sum(HR)) |>
  arrange(desc(HR)) |>
  slice(1:4)


# 2.10.1

# Suppose we want to identify the player who hit the most home runs in each decade across baseball history. First, we write a simple function
hr_leader <- function(data) {
  data |>
    group_by(playerID) |>
    summarize(HR = sum(HR)) |>
    arrange(desc(HR)) |>
    slice(1)
}

# We need to split the Batting data frame into pieces based on their decade. We use mutate() to create a new variable called decade that computes the first year of the decade for every value of yearID. Then, we use the group_by() function to organize Batting into pieces that share common values of decade
Batting_decade <- Batting |>
  mutate(decade = 10 * floor(yearID / 10)) |>
  group_by(decade)

# We use the group_keys() function to retrieve a vector of the first year in each decade
decades <- Batting_decade |>
  group_keys() |>
  pull("decade")
decades

# We use the group_split() function to break Batting_decade into pieces, and the map() function from the purrr package to apply our hr_leader() function to each of those data frames. The set_names() function and the .id argument to bind_rows() ensure that the variable displaying the first year of the decade gets the right name
Batting_decade |>
  group_split() |>
  map(hr_leader) |>
  set_names(decades) |>
  bind_rows(.id = "decade")


# 2.10.2

# The function summarize() in the dplyr package is useful for the first operation. We want to compute the sum of AB over the seasons of a player's career. The group_by() function indicates we wish to split the Batting data frame by the playerID variable, and the tAB = sum(AB, na.rm = TRUE) indicates we wish to summarize each data frame "part" by computing the sum of the AB
long_careers <- Batting |>
  group_by(playerID) |>
  summarize(
    tAB = sum(AB, na.rm = TRUE),
    tHR = sum(HR, na.rm = TRUE),
    tSO = sum(SO, na.rm = TRUE)
  )

# One can now use the filter() function to choose only the players with 5000 AB
Batting_5000 <- long_careers |>
  filter(tAB >= 5000)

# The first six lines of the data frame are displayed by the slice() function
Batting_5000 |>
  slice(1:6)

# Using the geom_point() function, we construct a scatterplot of HR/AB and SO/AB. Using the geom_smooth() function, we add a smoothing curve
ggplot(Batting_5000, aes(x = tHR / tAB, y = tSO / tAB)) +
  geom_point() + geom_smooth(color = crcblue)


# 2.11

# By typing in the Console window a question mark followed by the function name, you see a long description of this function including all of the possible function arguments

# To find out about related functions, one can preface a function by two question marks to find all objects that contain the character string