# The baseline table the original cases used.
d1 <- fake_dt(
  n = 3,
  n_cols = c(character = 2, date = 1, integer = 1, numeric = 2),
  seed = 42
)

# Hand-built tables where every row is identifiable, so the expectations are
# about the code and not about what fake_dt() happened to draw.
base <- data.table(id = c("a", "b", "c"), v = c(1, 2, 3))
