# Homework exercises

# section 03
library(data.table)
library(magrittr)
# 1
ratings_dt <- fread("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/BX-Book-Ratings.csv")
colnames(ratings_dt) <- gsub("-", "_", colnames(ratings_dt))
ratings_dt[, "High_Rating" := ifelse(Book_Rating > 7, 1, 0)]
# faster: ratings_dt[, "High_Rating" := fifelse(Book_Rating > 7, 1, 0)]

# 2
number_books_high_rating <- ratings_dt[High_Rating == 1, .N]
ratio <- number_books_high_rating/ratings_dt[, .N]

# 3
users_dt <- fread(file.path("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/BX-Users.csv"))
colnames(users_dt) <- gsub("-", "_", colnames(users_dt))
user_id_not_rating <- users_dt[!(User_ID %in% ratings_dt$User_ID)]$User_ID

# 4
users_per_age_rated <- users_dt[User_ID %in% ratings_dt$User_ID, .N, by = Age]
setorder(users_per_age_rated, -N)
# most common age: NA (after that: 26)
# without NA: users_per_age_rated <- users_dt[User_ID %in% ratings_dt$User_ID & !is.na(Age), .N, by = Age]
# alt.: users_per_age_rated[N == max(N)]

# 5
average_ratings_rated <- ratings_dt[, .N]/ratings_dt[, uniqueN(User_ID)]
# alt.: ratings_dt[, .N, by = User_ID][, mean(N, na.rm = TRUE)]
average_ratings_all <- ratings_dt[, .N]/users_dt[, uniqueN(User_ID)]

# 6
ordered_rankings <- copy(ratings_dt)
setorder(ordered_rankings, -Book_Rating, Year_Of_Publication)
ordered_rankings[1, Book_Title]
# alt.: ordered_ratings[order(Year_Of_Publication, -Book_Rating),
#                       .(Book_Title, Book_Rating, Year_Of_Publication)] %>% head(1)

# 7
ratings_per_book <- ratings_dt[, .N, by = ISBN]
max_rated_book <- ratings_per_book[which.max(ratings_per_book$N)]$ISBN
year_book_larges_number_ratings <- ratings_dt[ISBN == max_rated_book]$Year_Of_Publication[1]
# alt.: ratings_dt[, "Rating_Count" := .N, by = ISBN]
#       ratings_dt[Rating_count == max(Rating_Count), max(Year_Of_Publictaion)]

# 8
ratings_dt[, "Max_Book_Ranking" := max(Book_Rating), by = ISBN]

# 9
authors <- c("Agatha Christie", "William Shakespeare", "Stephen King", "Ann M. Martin",
             "Carolyn Keene", "Francine Pascal", "Isaac Asimov", "Nora Roberts",
             "Barbara Cartland", "Charles Dickens")
ratings_authors <- ratings_dt[Book_Author %in% authors]

# 10
ratings_per_author <- ratings_authors[, .N, by = Book_Author]
mean_per_author <- ratings_authors[, mean(Book_Rating), by = Book_Author]
max_per_author <- ratings_authors[, max(Book_Rating), by = Book_Author]
# alt.: ratings_authors[, `:=` .("Mean" = mean(Book_Rating), "Max" = max(Book_Rating)),
#                       by = Book_Author]

# ---

# section 04
# install.packages("readxl")
library(readxl)
# 1
summer_olympic_medals <- read_excel("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/summer_olympic_medals.xlsx")
summer_olympic_medals_dt <- as.data.table(summer_olympic_medals)
beat_bronze <- summer_olympic_medals_dt[Medal == "Bronze"][, .N, by = Athlete][N == max(N), unique(Athlete)]

# 2
inconsistent_gender <- summer_olympic_medals_dt[
                          ifelse((Gender == "Men" & Event_gender == "W") |
                                   (Gender == "Women" & Event_gender == "M"), TRUE, FALSE)]

# 3
country_most_medals <- summer_olympic_medals_dt[, .N, by = NOC]
setorder(country_most_medals, -N)
# most medals: USA

# 4
medals <- summer_olympic_medals_dt[, unique(Medal)]
# None? -> no NA or NULL in medals column
