#1
seq(from = 2, to = 100, by = 2)
(2:100)[(2:100) %% 2 == 0]
cumsum(rep(2, 50))

#----
#2
Names <- c("Alana", "Bettie", "Consuela", "Dona", "Elaine", "Frances", "Gerri", "Helene", "Ichabod", "Jin", "Kenyatta", "Larry", "Mikahilo", "Nick", "Odin")
Sex <- c(rep("F", 8), rep("M", 7))
Scores <- round(runif(15, 50, 100))
st90 <- Scores >= 90
low_scores_names <- Names[Scores < 60]
Scores_M <- Scores[Sex == "M"]
Scores_F <- Scores[Sex == "F"]
avg_M <- sum(Scores_M) / length(Scores_M)
avg_F <- sum(Scores_F) / length(Scores_F)

#----
#3
# Load necessary library
library(readr)

# Load the data
player_stats <- read_csv("ncaa_womens_volleyball_playerstats_2023.csv")

# View the column headers
head(player_stats)

# Create a table for total attacks by team
team_attacks_table <- table(player_stats$team, player_stats$total_attacks)
print(team_attacks_table)

# Create the AK_Ratio function
AK_Ratio <- function(total_attacks, kills) {
  ifelse(kills == 0, 0, total_attacks / kills)  # Avoid division by zero
}

# Load necessary library
library(dplyr)

# Apply the AK_Ratio function
player_stats <- player_stats %>%
  mutate(attack_kill_ratio = AK_Ratio(total_attacks, kills))

# Find the team with the largest average attack/kill ratio
team_largest_ratio <- player_stats %>%
  group_by(team) %>%
  summarize(avg_ratio = mean(attack_kill_ratio, na.rm = TRUE)) %>%
  arrange(desc(avg_ratio)) %>%
  slice(1)

# Find the player with the largest attack/kill ratio
player_largest_ratio <- player_stats %>%
  arrange(desc(attack_kill_ratio)) %>%
  slice(1)

# Print results
print(team_largest_ratio)
print(player_largest_ratio)

#----
#4
#a) Vector Recycling in R
#When you perform the operation `c(0, 2) + c(3, 4, 5, 6)`, R applies a phenomenon known as vector recycling. 
#Here's how it works:
#- The first vector, `c(0, 2)`, has a length of 2.
#- The second vector, `c(3, 4, 5, 6)`, has a length of 4.
#R will recycle the first vector to match the length of the second vector. The operation can be visualized as follows:
#- The first vector gets recycled: `c(0, 2, 0, 2)`
#- Now, the operation proceeds as:
# - `0 + 3 = 3`
# - `2 + 4 = 6`
# - `0 + 5 = 5`
# - `2 + 6 = 8`
#So the result of the operation is `c(3, 6, 5, 8)`.

#b. Vectors vs. Lists
#- **Vectors**: A vector is a basic data structure in R that contains elements of the same type (e.g., numeric, character, logical). Vectors are one-dimensional and can be thought of as arrays.
#- **Lists**: A list is a more complex data structure that can contain elements of different types, including other lists, vectors, matrices, or data frames. Lists are more flexible but can be less intuitive than vectors.

#c. Functions to Return Characteristics of an Object
#Two commonly used functions in R that return characteristics of an object are:
#- `str()`: This function provides a structured overview of an object, showing its type, structure, and contents.
#- `summary()`: This function gives a statistical summary of an object, providing key statistics for data frames, vectors, and other types.

#d. Finding Object Type Descriptions in R
#To find a description of the type of object a function returns, you can:
#- Use the `?` or `help()` function on the function name. For example, `?mean` will bring up the documentation for the `mean` function, which often includes details about the return type.
#- Check the **R documentation** online (CRAN) or in R itself. Additionally, functions like `class()` and `typeof()` can be used to determine the type of an object directly.

#-----
#5. a) To make text bold in R Markdown, you can use either of the following methods:
#•	Double asterisks: **text**
#•	Double underscores: __text__
#b) to make text italic in R Markdown, you can use either of the following methods:
#•	Single asterisk: *text*
#•	Single underscore: _text_
#c) To create bullet points in R Markdown, use the following format:
#•	Start each bullet point with a hyphen -, plus sign +, or asterisk *.
#•	Make sure to leave a space after the symbol.
#d) In R Markdown, you can create headers (which change the font size) by using the hash # symbol:
# This is Header 1
## This is Header 2
### This is Header 3


