library(data.table)

source("fake.R")
source("compare.R")

d1 <- fake_dt(
  n = 5,
  n_cols = c(character = 2, date = 1, integer = 1, numeric = 2)
)

diff_table(d1, fuzz_dt(d1))


# Casting a candidate to the reference types -----------------------------------

# x is the baseline: it fixes the type every column should have.
x <- data.table(id = 1L, score = 1.5, day = as.Date("2024-01-01"), flag = TRUE)

# y is the candidate, 5 rows, as it might arrive from a CSV:
# numbers as text, the day as a timestamp, the flag as a literal.
ids <- c("1", "2", "3", "4", "5")
scores <- c("1.5", "2", "3.25", "4", "5.5")
stamps <- paste0("2024-01-0", 1:5, " 23:30:00")
days <- as.POSIXct(stamps, tz = "Europe/Lisbon")
flags <- c("TRUE", "FALSE", "TRUE", "TRUE", "FALSE")

y <- data.table(id = ids, score = scores, day = days, flag = flags)

summarize_dt(y) # character, character, POSIXct, character

cast_dt(x, y) # modifies y by reference

summarize_dt(y) # integer, numeric, Date, logical

print(y)

# 23:30 in Lisbon stays the 1st; the UTC default would roll it to the 2nd.

# Fractions go through as.integer(): truncated toward zero.
frac <- data.table(id = c("1", "2", "3.7", "4", "5"))

cast_dt(x, frac)

print(frac$id) # 1 2 3 4 5

# What stays refused: text that is not a number at all.
bad <- data.table(id = c("1", "2", "abc", "4", "5"))

try(cast_dt(x, bad))

# Melting the numeric columns --------------------------------------------------

d <- fake_dt(10)

# melt_dt() picks the measure columns by type, not by name.
long <- melt_dt(d) # measures = "numeric" by default

# id.vars is every column not measured: 10 rows x 5 metrics = 50.
dim(long)

# row_id ties each molten row back to the row of d it came from.
print(long[, .(row_id, CHR_01, INT_01, metric, value)][1:6])

# The integers too; value widens to double, so melt() says so.
both <- melt_dt(d, measures = "numeric+integer")

print(unique(both$metric))

# Comparing when the ids do not tell rows apart ---------------------------------

# Ids capped at 2 distinct values, so some rows share every id column.
spec <- c(rep(2, 10), 2, 2, 2, 2, 2, rep(Inf, 5))
d <- fake_dt(200, n_distinct = spec, seed = 1)

# Fuzz only the measures, so the keys still line up between the two tables.
num_cols <- summarize_dt(d)[type == "numeric", colname]
cand <- fuzz_dt(d, pct_replace = 0.05, cols = num_cols, seed = 7)

lx <- melt_dt(d)
ly <- melt_dt(cand)

unique_by_key(lx) # FALSE: the ids repeat

# Step 1: aggregate. Order-free and exact, so it answers "is anything
# different?" without pairing rows that cannot be paired.
ax <- aggregate_by_key(lx, stats = "exact")
ay <- aggregate_by_key(ly, stats = "exact")

grp <- c(key(ax), "metric")
both <- merge(ax, ay, by = grp, suffixes = c("_x", "_y"))
hits <- both[value_key_x != value_key_y, ..grp]

nrow(hits) # 50 groups differ

# Step 2: only now disambiguate, to look inside the groups that differ.
disambiguate_by_key(lx, "rowid")
disambiguate_by_key(ly, "rowid")

pair <- merge(
  lx[hits, on = grp],
  ly[hits, on = grp],
  by = c(grp, "KEY_SEQ"),
  suffixes = c("_x", "_y")
)

print(pair[value_x != value_y][
  1:3,
  .(metric, KEY_SEQ, row_id_x, row_id_y, value_x, value_y)
])
