# Get & set working directory
getwd()
setwd("C:/Users/peter/OneDrive/Documents/Baseball/R/ABDWR")

# Load packages
library(abdwr3edata)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Rename the column containing player names to "Player"
names(hof_pitching)[names(hof_pitching) == '...2'] <- 'Player'


# 1. Hall of Fame Pitching Dataset

# Group the pitchers by this variable using the intervals (0, 10,000), (10,000, 15,000), (15,000, 20,000), (20,000, 30,000). One can reexpress the variable BF to the grouped variable BF_group by use of the cut() function
hof_pitching <- hof_pitching |>
  mutate(
    BF_group = cut(
      BF,
      c(0, 10000, 15000, 20000, 30000),
      labels = c("Less than 10000", "(10000, 15000)",
      "(15000, 20000)", "more than 20000")
    )
  )

# a. Construct a frequency table of BF.group using the summarize() function
hof_bf <- hof_pitching |>
  group_by(BF_group) |>
  summarize(N = n())
hof_bf

# b. Construct a bar graph of the output from summarize(). How many HOF pitchers faced more than 20,000 batters in their career?
ggplot(hof_pitching, aes(x = BF_group)) + geom_bar()

# 14 HOF pitchers faced more than 20,000 batters in their career

# c. Construct an alternative graph of the BF.group variable. Compare the effectiveness of the bar graph and the new graph in comparing the frequencies in the four intervals
ggplot(hof_bf, aes(BF_group, N)) +
  geom_point(color = "red") +
  coord_flip()


# 2. Hall of Fame Pitching Dataset (Continued)

# a. Using the geom_histogram() function, construct a histogram of WAR for the pitchers in the Hall of Fame dataset
ggplot(hof_pitching, aes(WAR)) +
  geom_histogram()

# b. There are two pitchers who stand out among all of the Hall of Famers on the total WAR variable. Identify these two pitchers
ggplot(hof_pitching, aes(WAR)) +
  geom_histogram() +
  geom_text_repel(
    data = filter(hof_pitching, WAR > 125),
    aes(WAR, y = 3, label = Player), color = "red"
  )


# 3. Hall of Fame Pitching Dataset (Continued)

# Define the new variable WAR_Season
hof_pitching <- hof_pitching |>
  mutate(WAR_Season = WAR / Yrs)

# a. Use the geom_point() function to construct parallel one-dimensional scatterplots of WAR_Season for the different levels of BF_group
ggplot(hof_pitching, aes(WAR_Season, BF_group)) +
  geom_jitter(height = 0.1)

# b. Use the geom_boxplot() function to construct parallel boxplots of WAR_Season across BF_group
ggplot(hof_pitching, aes(BF_group, WAR_Season)) +
  geom_boxplot(color = "brown", fill = "orange") +
  coord_flip()

# Based on your graphs, how does the wins above replacement per season depend on the number of batters faced?
# The median WAR/Season increases as the number of batters faced increases


# 4.Hall of Fame Pitching Dataset (Continued)

# Define the MidYear variable and then use the filter() function to construct a data frame consisting of only these 1960+ pitchers
hof_pitching <- hof_pitching |>
  mutate(MidYear = (From + To) / 2)
hof_pitching_recent <- hof_pitching |>
  filter(MidYear >= 1960)

# a. Use of the arrange() function, order the rows of the data frame by the value of WAR_Season
hof_pitching_recent <- hof_pitching_recent |>
  arrange(desc(WAR_Season))

# b. Construct a dot plot of the values of WAR_Season where the labels are the pitcher names
ggplot(hof_pitching_recent, aes(MidYear, WAR_Season)) +
  geom_point(color = "red") +
  geom_text_repel(
    aes(MidYear, WAR_Season, label = Player), color = "red"
  )

# c. Which two 1960+ pitchers stand out with respect to wins above replacement per season?
# Bob Gibson was one of two pitchers above 4.5 WAR/Season and Tom Seaver was the only pitcher above 5.0 WAR/Season


# 5. Hall of Fame Pitching Dataset (Continued)

# a. Construct a scatterplot of MidYear (horizontal) against WAR_Season (vertical)
ggplot(hof_pitching, aes(MidYear, WAR_Season)) + geom_point()

# b. Is there a general pattern in this scatterplot? Explain
# There appears to be a downward trend in WAR/Season going from earlier MidYears to more recent MidYears

# c. There are two pitchers whose mid careers were in the 1800s who had relatively low WAR_Season values. By use of the filter() and geom_text() functions, add the names of these two pitchers to the scatterplot
ggplot(hof_pitching, aes(MidYear, WAR_Season)) +
  geom_point() +
  geom_text_repel(
    data = filter(hof_pitching, MidYear < 1900 & WAR_Season < 2),
    aes(MidYear, WAR_Season, label = Player)
  )


# 6. Working with the Lahman Batting Dataset

# a. Read the Lahman People and Batting data frames into R
library(Lahman)
PeopleInfo <- People
BattingInfo <- Batting

# b. Collect in a single data frame the season batting statistics for the great hitters Ty Cobb, Ted Williams, and Pete Rose
great_hitters_info <- PeopleInfo |>
  filter(
    nameLast %in% c(
      "Cobb", "Williams", "Rose"
    ) & nameFirst %in% c(
      "Ty", "Ted", "Pete"
    )
  ) |>
  mutate(
    mlb_birthyear = if_else(
      birthMonth >= 7, birthYear + 1, birthYear
    )
  ) |>
  select(playerID, nameFirst, nameLast, mlb_birthyear)
great_hitters_info

great_hitters <- BattingInfo |>
  filter(
    playerID %in% c(
      "cobbty01", "rosepe01", "willite01"
    )
  )

# c. Add the variable Age to each data frame corresponding to the ages of the three players
great_hitters <- great_hitters |>
  inner_join(great_hitters_info, by = "playerID") |>
  mutate(Age = yearID - mlb_birthyear) |>
  select(playerID, Age, H) |>
  group_by(playerID) |>
  mutate(cH = cumsum(H))

# d. Using the geom_line() function, construct a line graph of the cumulative hit totals against age for Pete Rose
ggplot(great_hitters, aes(x = Age, y = cH)) +
  geom_line(data = filter(great_hitters, playerID == "rosepe01"))

# e. Using the geom_line() function, overlay the cumulative hit totals for Cobb and Williams
crc_fc <- c("#2905a1", "#e41a1c", "#4daf4a")
ggplot(great_hitters, aes(x = Age, y = cH, color = playerID)) +
  geom_line() +
  scale_color_manual(values = crc_fc)

# f. Write a short paragraph summarizing what you have learned about the hitting pattern of these three players
# Of the three players, Cobb & Rose had the most similar progression throughout their careers. Cobb started playing professionally earlier and so there is a leftward shift of his progression compared to Rose's. But their cumulative lines run almost parallel to each other. Williams did not amass nearly as many hits as those two despite being a phenomenal hitter. Williams has two distinct plateaus in his progression which shows the combined 5 seasons where he missed all or part of the season due to military service. His first hiatus was during WWII and his second hiatus was during the Korean War.


# 7. Working with the Lahman Batting Dataset (Continued)

# a. Read the Teams data frame into R
TeamsInfo <- Teams

# b. Create a new variable win_pct defined to be the team winning percentage W / (W + L)
TeamsInfo <- TeamsInfo |>
  mutate(
    win_pct = W / (W + L)
  )

# c. For the teams in the 2022 season, construct a scatterplot of the team ERA and its winning percentage
x2022Info <- TeamsInfo |>
  filter(
    yearID == 2022
  ) |>
  select(yearID, teamID, ERA, win_pct)

ggplot(x2022Info, aes(win_pct, ERA)) +
  geom_point()

# d. Use the geom_mlb_scoreboard_logos() function from the mlbplotR package to put the team logos on the scatterplot as plotting marks
# Load package
library(mlbplotR)

# Place Savant abbreviations in a vector
team_abbr <- valid_team_names()

# Remove NL, AL & MLB abbreviations
team_abbr <- team_abbr[!team_abbr %in% c("NL", "AL", "MLB")]

# Place Lahman abbreviations in a vector that align with Savant abbreviation vector
xabbr <- c("OAK", "ATL", "ARI", "BAL", "BOS", "CHN", "CIN", "CLE", "COL", "CHA", "DET", "HOU", "KCA", "LAA", "LAN", "MIA", "MIL", "MIN", "NYN", "NYA", "PHI", "PIT", "SDN", "SEA", "SFN", "SLN", "TBA", "TEX", "TOR", "WAS")

# Create an abbreviation data frame
team_abbr_df <- data.frame(xabbr, team_abbr)

# Rename xabbr to teamID
names(team_abbr_df)[names(team_abbr_df) == 'xabbr'] <- 'teamID'

# Join data frames
x2022Info <- x2022Info |>
  inner_join(team_abbr_df, by = "teamID")

# Plot
ggplot(x2022Info, aes(x = win_pct, y = ERA)) +
  geom_mlb_logos(aes(team_abbr = team_abbr), width = 0.05)


# 8. Working with the Retrosheet Play-by-Play Dataset

# a. Create the two data frames mac_data and sosa_data containing the batting data for the two players

# Read in the 1998 play-by-play data and storing it in the data frame retro1998
library(devtools)
source_gist("https://gist.github.com/bayesball/8892981",
            filename="parse.retrosheet2.pbp.R")

parse.retrosheet2.pbp(1998)

# Create retro1998 data frame
retro1998 <- read.csv("download.folder/unzipped/all1998.csv", header = FALSE)

# Create fields object and add fields as headers to retro1998 data frame
fields <- read.csv("https://raw.githubusercontent.com/beanumber/baseball_R/master/data/fields.csv")
names(retro1998) <- fields[, "Header"]

# Pull retroID for McGwire & Sosa
mac_id <- People |>
  filter(nameFirst == "Mark", nameLast == "McGwire") |>
  pull(retroID)
sosa_id <- People |>
  filter(nameFirst == "Sammy", nameLast == "Sosa") |>
  pull(retroID)

# Create individual data frames for McGwire & Sosa's 1998 seasons
mac_data <- retro1998 |>
  filter(BAT_ID == "mcgwm001")
sosa_data <- retro1998 |>
  filter(BAT_ID == "sosas001")

# b. Use the following R commands to restrict the two data frames to the plays where a batting event occurred
mac_data <- filter(mac_data, BAT_EVENT_FL == TRUE)
sosa_data <- filter(sosa_data, BAT_EVENT_FL == TRUE)

# c. Create a new variable PA that numbers the plate appearances 1, 2, … (The function nrow() gives the number of rows of a data frame)
mac_data <- mutate(mac_data, PA = 1:nrow(mac_data))
sosa_data <- mutate(sosa_data, PA = 1:nrow(sosa_data))

# d. Return the numbers of the plate appearances when the players hit home runs
mac_HRPA <- mac_data |>
  filter(EVENT_CD == 23) |>
  pull(PA)
sosa_HRPA <- sosa_data |>
  filter(EVENT_CD == 23) |>
  pull(PA)

# e. The following commands compute the spacings between the occurrences of home runs
mac_spacings <- diff(c(0, mac_HRPA))
sosa_spacings <- diff(c(0, sosa_HRPA))

# f. Create a new data frame HR_Spacing with two variables, Player, the player name, and Spacing, the value of the spacing. Use the summarize() and geom_histogram() functions on the data frame HR_Spacing, compare the home run spacings of the two players
summary(mac_spacings)
hist(mac_spacings)

summary(sosa_spacings)
hist(sosa_spacings)

# McGwire had shorter intervals between home runs hit where Sosa had larger intervals including larger "slumps" of not hitting home runs