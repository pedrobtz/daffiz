create_fake_dt <- function(n = 100, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  rand_code <- function(n, len = 6) {
    vapply(seq_len(n), function(i)
      paste0(sample(c(LETTERS, 0:9), len, replace = TRUE), collapse = ""),
      character(1))
  }

  data.table(
    # 10 string columns
    full_name  = ch_name(n),
    job_title  = ch_job(n),
    company    = ch_company(n),
    phone      = ch_phone_number(n),
    color_name = ch_color_name(n),
    hex_color  = ch_hex_color(n),
    category   = sample(c("A", "B", "C", "D", "E"), n, replace = TRUE),
    status     = sample(c("active", "inactive", "pending", "closed"), n, replace = TRUE),
    region     = sample(c("North", "South", "East", "West", "Central"), n, replace = TRUE),
    code       = rand_code(n),

    # 1 date column
    event_date = as.Date(
      sample(seq(as.Date("2020-01-01"), as.Date("2025-12-31"), by = "day"), n, replace = TRUE)
    ),

    # 3 integer columns
    age        = sample(18L:80L, n, replace = TRUE),
    score      = sample(1L:100L, n, replace = TRUE),
    item_count = sample(0L:500L, n, replace = TRUE),

    # 1 logical column
    is_active  = sample(c(TRUE, FALSE), n, replace = TRUE),

    # 5 numeric columns
    height_cm  = round(runif(n, 150, 200), 1),
    weight_kg  = round(runif(n, 50, 120), 1),
    salary     = round(runif(n, 30000, 150000), 2),
    temperature = round(runif(n, -10, 40), 2),
    ratio      = runif(n, 0, 1)
  )
}
