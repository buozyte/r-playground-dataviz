# Tutorial excercises

# section 00
library(data.table)
library(magrittr)

# ---

# section 01
# 1
users_dt <- fread(file.path("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/BX-Users.csv"))
books_dt <- fread(file.path("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/BX-Books.csv"))
ratings_dt <- fread(file.path("/Users/donatabuozyte/Desktop/Uni/Master/Datasets/extdata/BX-Book-Ratings.csv"))

# 2
"data.table" %in% class(users_dt)
"data.table" %in% class(books_dt)
"data.table" %in% class(ratings_dt)

# 3
str(users_dt)
# column names: User-ID (`int`), Location (`chr`), Age (`chr`)
#               -> or via colnames(users_dt)
# classes of `users_dt`: `"data.table"`, `"data.frame"`
users_dt$Age <- users_dt[, sapply(Age, as.numeric)]
# Alternative: users_dt$Age <- sapply(users_dt$Age, as.numeric)

# 4
summary(users_dt$`User-ID`)
summary(users_dt$Location)
summary(users_dt$Age)

# 5
first <- head(ratings_dt, 5)
last <- tail(ratings_dt, 5)

# 6
# colnames(users_dt)
# colnames(books_dt)
# colnames(ratings_dt)
colnames(users_dt) <- gsub("-", "_", colnames(users_dt))
colnames(books_dt) <- gsub("-", "_", colnames(books_dt))
colnames(ratings_dt) <- gsub("-", "_", colnames(ratings_dt))
# alt.: setnames(users_dt, colnames(users_dt), gsub("-", "_", colnames(users_dt)))
# alt.: setnames(books_dt, colnames(books_dt), gsub("-", "_", colnames(books_dt)))
# alt.: setnames(ratings_dt, colnames(ratings_dt), gsub("-", "_", colnames(ratings_dt)))
# colnames(users_dt)
# colnames(books_dt)
# colnames(ratings_dt)

# 7
# colnames(books_dt)
books_dt[, ":=" (Image_URL_S = NULL, Image_URL_M = NULL, Image_URL_L = NULL)]
# alt.: books_dt[, c("Image_URL_S", "Image_URL_M", "Image_URL_L") := NULL]
# "not inplace": books_dt[, - c("Image_URL_S", "Image_URL_M", "Image_URL_L")]
# colnames(books_dt)

# 8
book_dt_2 <- copy(books_dt[Year_Of_Publication > 1899 & Year_Of_Publication < 2020])
# alt.: book_dt_2 <- copy(books_dt[Year_Of_Publication %in% 1900:2019])

# ---

# section 02
# 1
unique_authors <- books_dt[, uniqueN(Book_Author)]
# alt.: books_dt[, .N, by=Book_Author]
# alt.: lenght(books_dt[, unique(Book_Author)])

# 2
authors_per_year <- books_dt[Year_Of_Publication %in% 2000:2010, uniqueN(Book_Author), by = Year_Of_Publication]

# 3
number_user_NA_age <- users_dt[is.na(Age), .N]

# 4
maximal_rating <- ratings_dt[, max(Book_Rating, na.rm = TRUE)]

# 5
books_per_rating <- ratings_dt[Book_Rating > 0, .N, by=Book_Rating]
# books_per_rating[which.max(books_per_rating$N)]
# books_per_rating[which.max(books_per_rating$N)]$Book_Rating
# most common: 8
# alt.: ratings_dt[Book_Rating > 0, .N, by=Book_Rating][N == max(N)] :)

# 6
ISBN_best_rating <- ratings_dt[max(Book_Rating, na.rm = TRUE) == Book_Rating, ISBN] # %>% head()
amount_of_best_rated <- ratings_dt[max(Book_Rating) == Book_Rating, .N]

# 7
setorder(ratings_dt, -Book_Rating)
