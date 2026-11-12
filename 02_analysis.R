
# ============================================================
# BAN 5100 - Final Exam: JLL / GSA Lease Portfolio
# ============================================================
#
# Student: Alisha Pokharel
# Course: BAN 5100 - Data Visualization & Inferential Statistics
# Professor: Omar El Mghari
# Semester: Fall 2026
# Part 2: Analysis and Visualization
# Dataset: cs1_gsa_leases_cleaned.csv
#
# ============================================================
# PROJECT OBJECTIVES
# ============================================================
#
# Objective 1:
# Where does the federal government pay the most rent?
#
# Objective 2:
# Where are large portions of that rent coming up for
# renewal in the next two years?
#
# Objective 3:
# Can you map this information by state and explain
# what it means for our leasing strategy?
#
# ============================================================
# ANALYSIS APPROACH
# ============================================================
#
# I will use the cleaned GSA lease dataset from Part 1
# to answer the three questions.
#
# For each objective, I will explain my method, show the
# results using tables and charts, and discuss what the
# findings mean for JLL.
#
# I will also improve my Web Exercise 4 code by adding
# more detailed comparisons and using actual lease
# expiration dates for the renewal analysis.
#
# ============================================================


# ============================================================
# STEP 1: LOAD AND CHECK THE CLEANED DATASET
# ============================================================

# I use tidyverse to organize the data and create charts.
library(tidyverse)


# I load the cleaned GSA dataset from my RStudio project folder.
# The CSV is already saved in the same folder as my R script.

leases <- read_csv(
  "cs1_gsa_leases_cleaned.csv",
  show_col_types = FALSE
)

# I check that the cleaned dataset loaded correctly.
dim(leases)

# I review the available columns before starting the analysis.
names(leases)



# ============================================================
# STEP 2: VALIDATE THE DATASET BEFORE ANALYSIS
# ============================================================

# I check the main fields needed to answer JLL's questions.
# I also check that each lease has a unique lease number.

leases |>
  summarise(
    total_leases = n(),
    unique_leases = n_distinct(lease_num),
    missing_state = sum(is.na(state)),
    missing_annual_rent = sum(is.na(annual_rent)),
    missing_expiration = sum(is.na(lease_expiration)),
    total_annual_rent = sum(annual_rent, na.rm = TRUE)
  )



# ============================================================
# STEP 3: DEFINE THE ANALYSIS PERIOD
# ============================================================

# I use August 31, 2026, as the reporting date for this dataset.
snapshot_date <- as.Date("2026-08-31")

# I define the next two years for the renewal analysis.
renewal_start <- as.Date("2026-09-01")
renewal_end <- as.Date("2028-08-31")

# I check the dates before using them in the analysis.
tibble(
  reporting_date = snapshot_date,
  renewal_start = renewal_start,
  renewal_end = renewal_end
)


# ============================================================
# STEP 4: CHECK LEASES THAT ARE ALREADY DUE
# ============================================================

# I analyze leases that reached their expiration date
# on or before the reporting date separately from the
# upcoming two-year renewal window.

already_due_leases <- leases %>%
  filter(!is.na(lease_expiration),
         lease_expiration <= snapshot_date)

# I calculate how many leases are already due and
# how much annual rent they represent.

already_due_summary <- already_due_leases %>%
  summarise(
    number_of_leases = n(),
    annual_rent = sum(annual_rent, na.rm = TRUE)
  ) %>%
  mutate(
    annual_rent_millions = round(annual_rent / 1000000, 2)
  )

print(already_due_summary)

# I separate leases expiring exactly on the reporting date
# from leases that had expired before the reporting date.

already_due_breakdown <- leases %>%
  filter(!is.na(lease_expiration),
         lease_expiration <= snapshot_date) %>%
  mutate(
    expiration_status = if_else(
      lease_expiration == snapshot_date,
      "Expires on snapshot date",
      "Expired before snapshot date"
    )
  ) %>%
  group_by(expiration_status) %>%
  summarise(
    number_of_leases = n(),
    annual_rent_millions = round(
      sum(annual_rent, na.rm = TRUE) / 1000000, 2
    ),
    .groups = "drop"
  )

print(already_due_breakdown)

# I check the exact annual rent for leases already due.
already_due_summary %>%
  select(number_of_leases, annual_rent, annual_rent_millions)


# I compare leases already due with leases scheduled
# to expire during the next two years.

upcoming_2yr_summary <- leases %>%
  filter(
    !is.na(lease_expiration),
    lease_expiration >= renewal_start,
    lease_expiration <= renewal_end
  ) %>%
  summarise(
    number_of_leases = n(),
    annual_rent = sum(annual_rent, na.rm = TRUE)
  )

# I calculate the combined expiration exposure.
# The two groups do not overlap because their date ranges differ.

combined_exposure <- tibble(
  exposure_category = c(
    "Already due",
    "Upcoming two years",
    "Combined exposure"
  ),
  number_of_leases = c(
    already_due_summary$number_of_leases,
    upcoming_2yr_summary$number_of_leases,
    already_due_summary$number_of_leases +
      upcoming_2yr_summary$number_of_leases
  ),
  annual_rent = c(
    already_due_summary$annual_rent,
    upcoming_2yr_summary$annual_rent,
    already_due_summary$annual_rent +
      upcoming_2yr_summary$annual_rent
  )
) %>%
  mutate(
    annual_rent_millions = round(annual_rent / 1000000, 2),
    portfolio_pct = round(
      annual_rent / sum(leases$annual_rent, na.rm = TRUE) * 100, 2
    )
  )

print(combined_exposure)


# ------------------------------------------------------------
# BUSINESS INTERPRETATION
# ------------------------------------------------------------
#
# The results show that 233 GSA leases, representing
# approximately $145.23 million in annual rent, had already
# reached or passed their expiration dates by August 31, 2026.
#
# Another 1,687 leases, representing approximately $1.16 billion
# in annual rent, are scheduled to expire within the next two years.
#
# Together, these 1,920 leases represent approximately
# $1.30 billion, or 22.3% of the total annual rent portfolio.
#
# For JLL, the already-due leases should receive immediate
# attention, while the upcoming expirations can help prioritize
# future leasing discussions and portfolio planning.
#
# These amounts represent annual rent exposure, not guaranteed
# renewal revenue or confirmed lease terminations.


# ------------------------------------------------------------
# CHART: ALREADY-DUE AND NEXT-TWO-YEAR EXPIRATIONS
# ------------------------------------------------------------

# I show the already-due group separately so it is not
# hidden by the original two-year expiration window.

expiration_status_chart <- combined_exposure %>%
  filter(exposure_category != "Combined exposure") %>%
  ggplot(aes(x = exposure_category, y = annual_rent_millions,
             fill = exposure_category)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0("$", round(annual_rent_millions, 1), "M")),
            vjust = -0.4, size = 4) +
  scale_fill_manual(values = c("Already due" = "darkorange",
                               "Upcoming two years" = "steelblue")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Annual Rent: Already Due vs. Next Two Years",
       subtitle = "August 31, 2026 reporting date; two-year window ends August 31, 2028",
       x = NULL, y = "Annual Rent (USD Millions)",
       caption = "Separate non-overlapping groups; annual rent is not guaranteed renewal revenue") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

expiration_status_chart


# ============================================================
# STEP 5: CHECK SOFT-TERM LEASES
# ============================================================

# I check how many leases are identified as being in soft term.
# This helps me understand potential termination-related risk.

table(leases$in_soft_term, useNA = "ifany")

# I also check the type of values stored in this column.
unique(leases$in_soft_term)

# I calculate the number of leases and annual rent
# for each soft-term category.

soft_term_summary <- leases %>%
  group_by(in_soft_term) %>%
  summarise(
    number_of_leases = n(),
    annual_rent = sum(annual_rent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    annual_rent_millions = round(annual_rent / 1000000, 2),
    portfolio_pct = round(
      annual_rent / sum(leases$annual_rent, na.rm = TRUE) * 100, 2
    )
  )

print(soft_term_summary)

# I also check the termination right dates because they
# help explain why some leases are already in soft term.

termination_right_check <- leases %>%
  group_by(in_soft_term) %>%
  summarise(
    number_of_leases = n(),
    missing_termination_dates = sum(is.na(termination_right_date)),
    termination_dates_on_or_before_snapshot = sum(
      termination_right_date <= snapshot_date,
      na.rm = TRUE
    ),
    termination_dates_after_snapshot = sum(
      termination_right_date > snapshot_date,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(termination_right_check)


# ------------------------------------------------------------
# BUSINESS INTERPRETATION
# ------------------------------------------------------------
#
# I checked the termination-right dates to verify the
# soft-term classification in the cleaned dataset.
#
# All 2,765 leases marked as being in soft term have
# termination-right dates on or before August 31, 2026.
#
# Among the 4,603 leases not in soft term, 4,602 have
# termination-right dates after the reporting date.
# Only one lease has a missing termination-right date.
#
# These results support the soft-term classification
# used in my analysis.
#
# For JLL, I would review these 2,765 leases because
# their termination provisions may create additional
# leasing uncertainty beyond normal expiration dates.
#
# However, being in soft term does not mean that a
# lease will definitely terminate. The actual lease
# provisions would need to be reviewed first.

# I check how soft-term leases overlap with the
# already-due and upcoming two-year expiration groups.

soft_term_overlap <- leases %>%
  mutate(
    expiration_group = case_when(
      lease_expiration <= snapshot_date ~ "Already due",
      lease_expiration >= renewal_start &
        lease_expiration <= renewal_end ~ "Upcoming two years",
      TRUE ~ "Beyond two-year window"
    )
  ) %>%
  group_by(expiration_group, in_soft_term) %>%
  summarise(
    number_of_leases = n(),
    annual_rent_millions = round(
      sum(annual_rent, na.rm = TRUE) / 1000000, 2
    ),
    .groups = "drop"
  )

print(soft_term_overlap)

# ============================================================
# STEP 6: CALCULATE TOTAL UNIQUE LEASE EXPOSURE
# ============================================================

# I identify leases that are either already due,
# expiring within two years, or currently in soft term.
# Each lease is counted only once.

unique_risk_exposure <- leases %>%
  filter(
    (!is.na(lease_expiration) &
       lease_expiration <= renewal_end) |
      in_soft_term == "Y"
  ) %>%
  summarise(
    number_of_leases = n(),
    annual_rent = sum(annual_rent, na.rm = TRUE)
  ) %>%
  mutate(
    annual_rent_millions = round(annual_rent / 1000000, 2),
    portfolio_pct = round(
      annual_rent / sum(leases$annual_rent, na.rm = TRUE) * 100, 2
    )
  )

print(unique_risk_exposure)

# ------------------------------------------------------------
# BUSINESS INTERPRETATION
# ------------------------------------------------------------
#
# My analysis identifies 3,182 unique GSA leases representing
# approximately $1.85 billion in annual rent, or 31.7% of
# the total portfolio.
#
# These leases either reached their expiration date by
# August 31, 2026, are scheduled to expire within the
# following two years, or are currently identified as
# being in soft term.
#
# An important finding is that 1,262 soft-term leases,
# representing approximately $548 million in annual rent,
# have expiration dates beyond the two-year window.
#
# This means that focusing only on lease expiration dates
# would overlook additional potential termination exposure.
#
# For JLL, I would prioritize leases that are already due,
# review high-value upcoming expirations, and separately
# examine soft-term leases with later expiration dates.
#
# This is a measure of leases requiring further review,
# not a prediction of actual terminations or lost rent.

# ============================================================
# STEP 7: INFERENTIAL STATISTICS
# ============================================================

# I compare annual rent per lease in DC and Virginia
# with the remaining jurisdictions in the portfolio.

rent_comparison <- leases %>%
  filter(
    !is.na(annual_rent),
    annual_rent > 0,
    !is.na(state)
  ) %>%
  mutate(
    region_group = if_else(
      state %in% c("DC", "VA"),
      "DC and Virginia",
      "Other jurisdictions"
    )
  )

# I check the number of leases and rent distribution
# in each group before selecting a statistical test.

rent_group_summary <- rent_comparison %>%
  group_by(region_group) %>%
  summarise(
    number_of_leases = n(),
    mean_annual_rent = round(mean(annual_rent), 2),
    median_annual_rent = round(median(annual_rent), 2),
    .groups = "drop"
  )

print(rent_group_summary)

# I use a Wilcoxon rank-sum test to compare annual rent
# between DC/Virginia and the other jurisdictions.
# This nonparametric test does not require normally distributed
# rent values. Lease observations should be independent; related
# leases in a building or landlord portfolio may limit that assumption.
# A significant result tests a difference in distributions, not
# the cause of the difference or a guaranteed difference in medians.

rent_wilcoxon_test <- wilcox.test(
  annual_rent ~ region_group,
  data = rent_comparison,
  alternative = "two.sided",
  exact = FALSE
)

print(rent_wilcoxon_test)

# ------------------------------------------------------------
# STATISTICAL INTERPRETATION
# ------------------------------------------------------------
#
# I used a Wilcoxon rank-sum test to compare annual rent
# per lease in DC/Virginia with other jurisdictions.
#
# The test produced a p-value below 2.2e-16, which is
# much smaller than the 0.05 significance level.
# Therefore, I reject the null hypothesis and conclude
# that the annual rent distributions differ significantly.
#
# The descriptive sample medians are approximately $914,316
# in DC/Virginia and $254,699 elsewhere. These medians help
# explain the business magnitude; the Wilcoxon p-value alone
# is not a causal finding or a direct test of median differences.
#
# For JLL, this suggests that DC/Virginia contain
# higher-value individual lease commitments that may
# deserve closer attention during portfolio reviews.
#
# This comparison does not establish the cause of the
# difference or indicate which leases will terminate.


# ============================================================
# STEP 8: IDENTIFY HIGH-VALUE INDIVIDUAL LEASES
# ============================================================

# I check the available columns before identifying
# the largest leases approaching expiration.

names(leases)

# I identify the 10 largest individual leases that are
# scheduled to expire within the next two years.
# I rank these leases by their annual rent.

top_10_expiring_leases <- leases %>%
  filter(
    !is.na(lease_expiration),
    lease_expiration >= renewal_start,
    lease_expiration <= renewal_end
  ) %>%
  arrange(desc(annual_rent)) %>%
  slice_head(n = 10) %>%
  select(
    lease_num,
    city,
    state,
    address,
    lessor,
    lease_expiration,
    annual_rent,
    in_soft_term,
    renewal_options_left
  )

print(top_10_expiring_leases, n = 10, width = Inf)

# ------------------------------------------------------------
# BUSINESS INTERPRETATION
# ------------------------------------------------------------
#
# I identified the 10 highest-rent individual GSA leases
# scheduled to expire within the next two years.
#
# Eight of these leases are located in Washington, DC
# or Virginia, which supports my earlier findings about
# the concentration of federal leasing activity.
#
# The largest lease is located at 300 E St SW in
# Washington, DC, with approximately $32.38 million
# in annual rent and an expiration date of August 3, 2028.
#
# Another important lease is located at 7700 Arlington Blvd
# in Falls Church, Virginia. It represents approximately
# $27.86 million in annual rent and expires in December 2026.
#
# All 10 leases have zero remaining renewal options
# recorded in the dataset, and two are flagged as
# being in soft term.
#
# For JLL, I would prioritize reviewing these specific
# properties, their landlords, termination provisions,
# and agency space requirements before recommending
# renewal negotiations or alternative leasing arrangements.
#
# These are properties requiring further investigation,
# not confirmed lease renewals or available opportunities.

# ============================================================
# STEP 9: NEW JERSEY LEASE PORTFOLIO ANALYSIS
# ============================================================

# I examine New Jersey separately to understand its
# contribution to the federal lease portfolio.

nj_leases <- leases %>%
  filter(state == "NJ")

# I calculate New Jersey's lease count, annual rent,
# and average annual rent per lease.

nj_summary <- nj_leases %>%
  summarise(
    number_of_leases = n(),
    total_annual_rent = sum(annual_rent, na.rm = TRUE),
    average_annual_rent = mean(annual_rent, na.rm = TRUE)
  ) %>%
  mutate(
    annual_rent_millions = round(total_annual_rent / 1000000, 2),
    portfolio_pct = round(
      total_annual_rent / sum(leases$annual_rent, na.rm = TRUE) * 100, 2
    )
  )

print(nj_summary)

# I examine New Jersey's lease expiration and soft-term
# exposure to identify areas requiring closer attention.

nj_risk_summary <- nj_leases %>%
  mutate(
    risk_category = case_when(
      lease_expiration <= snapshot_date ~ "Already due",
      lease_expiration >= renewal_start &
        lease_expiration <= renewal_end ~ "Upcoming two years",
      TRUE ~ "Beyond two-year window"
    )
  ) %>%
  group_by(risk_category) %>%
  summarise(
    number_of_leases = n(),
    annual_rent_millions = round(
      sum(annual_rent, na.rm = TRUE) / 1000000, 2
    ),
    .groups = "drop"
  )

print(nj_risk_summary)

# I separately calculate New Jersey's soft-term exposure.

nj_soft_term_summary <- nj_leases %>%
  group_by(in_soft_term) %>%
  summarise(
    number_of_leases = n(),
    annual_rent_millions = round(
      sum(annual_rent, na.rm = TRUE) / 1000000, 2
    ),
    .groups = "drop"
  )

print(nj_soft_term_summary)

# ------------------------------------------------------------
# BUSINESS INTERPRETATION
# ------------------------------------------------------------
#
# New Jersey ranks 16th among jurisdictions by annual rent.
# It has 135 GSA leases representing approximately
# $86.70 million in annual rent, or 1.48% of the portfolio.

# Four leases representing $4.24 million in annual rent
# have already reached or passed their expiration dates.
#
# Another 40 leases representing approximately $20.3 million
# in annual rent are scheduled to expire within two years.
#
# This means 44 leases representing approximately
# $24.54 million in annual rent require expiration-related review.
#
# I also found that 70 New Jersey leases, representing
# approximately $30.1 million in annual rent, are in soft term.
#
# For JLL, I would first review the four already-due leases,
# followed by the highest-value upcoming expirations.
# I would also examine termination rights for soft-term leases,
# including those with expiration dates beyond two years.
#
# Soft-term and expiration groups may overlap, so their
# annual rent amounts should not be added together.


# I rank all jurisdictions by their total annual rent
# to confirm New Jersey's position in the portfolio.

state_rent_ranking <- leases %>%
  group_by(state) %>%
  summarise(
    number_of_leases = n(),
    total_annual_rent = sum(annual_rent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_annual_rent)) %>%
  mutate(
    rent_rank = row_number()
  )

# I check New Jersey's ranking.
state_rent_ranking %>%
  filter(state == "NJ")

# ============================================================
# OBJECTIVE 1: WHERE DOES THE FEDERAL GOVERNMENT PAY THE MOST RENT?
# ============================================================


# ------------------------------------------------------------
# 1. QUESTION
# ------------------------------------------------------------

# Where does the federal government pay the most rent?

# I want to identify the jurisdictions with the highest
# annual federal rent and understand what drives that spending.

# I will compare total rent, lease counts, and average rent
# per lease to see which markets are most important for JLL.


# ------------------------------------------------------------
# 2. METHOD
# ------------------------------------------------------------

# I group the cleaned GSA lease data by state.
# I calculate total annual rent, lease count, and average rent
# per lease for each jurisdiction.

# I also calculate each jurisdiction's percentage of the
# total annual rent to understand how spending is distributed.

by_state_rent <- leases |>
  group_by(state) |>
  summarise(
    lease_count = n(),
    total_annual_rent = sum(annual_rent, na.rm = TRUE),
    avg_rent_per_lease = mean(annual_rent, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    rent_share_pct = total_annual_rent /
      sum(total_annual_rent) * 100
  ) |>
  arrange(desc(total_annual_rent))


# ------------------------------------------------------------
# 3. RESULTS
# ------------------------------------------------------------


# ------------------------------------------------------------
# TABLE 1: JURISDICTIONS WITH THE HIGHEST ANNUAL RENT
# ------------------------------------------------------------

# I show annual rent and average rent in millions.
# This makes the large dollar amounts easier to compare.

top_10_rent <- by_state_rent |>
  head(10)

top_10_rent |>
  mutate(
    annual_rent_millions = round(total_annual_rent / 1000000, 2),
    avg_rent_millions = round(avg_rent_per_lease / 1000000, 2),
    rent_share_pct = round(rent_share_pct, 2)
  ) |>
  select(
    state,
    lease_count,
    annual_rent_millions,
    avg_rent_millions,
    rent_share_pct
  )


# ------------------------------------------------------------
# TABLE 2: HOW CONCENTRATED IS FEDERAL RENT?
# ------------------------------------------------------------

# I calculate the total rent paid across the five
# highest-spending jurisdictions.

# This helps me see how much of the federal portfolio
# is concentrated in a small number of markets.

top_5_share <- by_state_rent |>
  head(5) |>
  summarise(
    combined_annual_rent = sum(total_annual_rent),
    share_of_total_pct = round(sum(rent_share_pct), 2)
  )

top_5_share


# ------------------------------------------------------------
# CHART 1: TOP 10 JURISDICTIONS BY ANNUAL RENT
# ------------------------------------------------------------

# I use a horizontal bar chart because it makes the
# differences between jurisdictions easier to compare.

# I also show the dollar values on the bars so the
# reader can identify the largest markets quickly.

chart_rent <- ggplot(
  top_10_rent,
  aes(
    x = reorder(state, total_annual_rent),
    y = total_annual_rent / 1000000
  )
) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(
    aes(
      label = paste0(
        "$",
        round(total_annual_rent / 1000000, 1),
        "M"
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Top 10 Jurisdictions by Federal Annual Rent",
    subtitle = "Recorded annual rent as of August 2026",
    x = NULL,
    y = "Annual Rent (USD Millions)",
    caption = "Source: GSA Lease Inventory, August 2026"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# I display the completed chart.
chart_rent


# ------------------------------------------------------------
# TABLE 3: JURISDICTIONS WITH THE MOST FEDERAL LEASES
# ------------------------------------------------------------

# I compare the number of leases with total annual rent.
# This helps me understand whether having more leases
# always means the government spends more money.

top_10_lease_count <- by_state_rent |>
  arrange(desc(lease_count)) |>
  head(10)

top_10_lease_count |>
  mutate(
    annual_rent_millions = round(total_annual_rent / 1000000, 2),
    avg_rent_millions = round(avg_rent_per_lease / 1000000, 2)
  ) |>
  select(
    state,
    lease_count,
    annual_rent_millions,
    avg_rent_millions
  )


# ------------------------------------------------------------
# TABLE 4: COMPARING VIRGINIA, DC, AND CALIFORNIA
# ------------------------------------------------------------

# I compare the three highest-spending jurisdictions
# to understand why their total rental commitments differ.

by_state_rent |>
  filter(state %in% c("VA", "DC", "CA")) |>
  mutate(
    annual_rent_millions = round(total_annual_rent / 1000000, 2),
    avg_rent_millions = round(avg_rent_per_lease / 1000000, 2),
    rent_share_pct = round(rent_share_pct, 2)
  ) |>
  select(
    state,
    lease_count,
    annual_rent_millions,
    avg_rent_millions,
    rent_share_pct
  )


# ------------------------------------------------------------
# 4. BUSINESS IMPLICATIONS AND RECOMMENDATIONS
# ------------------------------------------------------------

# The federal government's highest annual rent commitments
# are in Virginia ($723.05 million), Washington, DC
# ($700.25 million), and California ($534.91 million).

# Together, these three jurisdictions account for about
# one-third of the total annual rent in the dataset.

# California has the most federal leases (649), but
# Virginia has higher total annual rent with 357 leases.
# This shows that lease count alone does not explain spending.

# DC is particularly interesting because it has only
# 147 leases, but the average annual rent is about
# $4.76 million per lease.

# This suggests that DC has fewer but higher-value
# federal lease commitments compared with other markets.

# Virginia and DC together account for approximately
# $1.42 billion in annual federal rent.

# For JLL, I would begin by examining high-value leases
# in Virginia and DC, including their locations,
# rental costs, lease terms, and renewal options.

# California and Texas may require a different approach.
# These jurisdictions have the largest numbers of leases,
# which makes them relevant for broader portfolio analysis.

# I would consider California and Texas for research
# involving portfolio management and space optimization.

# The five highest-spending jurisdictions account for
# about 46.7% of the total annual rent.

# This suggests that JLL could concentrate its initial
# market research on a few jurisdictions with significant
# federal rental commitments.

# My recommendation is to prioritize Virginia and DC
# for high-value lease research and consider California
# and Texas for broader portfolio-level analysis.

# However, high annual rent does not automatically mean
# that leases are approaching renewal.

# I will use the actual expiration dates in Objective 2
# to identify the jurisdictions with the greatest
# near-term renewal exposure.



# ============================================================
# OBJECTIVE 2: WHERE ARE LARGE PORTIONS OF THAT RENT
# COMING UP FOR RENEWAL IN THE NEXT TWO YEARS?
# ============================================================


# ------------------------------------------------------------
# 1. QUESTION
# ------------------------------------------------------------

# Where are large portions of that rent coming up for
# renewal in the next two years?

# I want to identify where the largest amounts of federal
# annual rent are associated with upcoming lease expirations.

# I will compare the dollar amount, percentage of annual rent,
# and expiration timing to understand which markets may need
# the most attention from JLL.


# ------------------------------------------------------------
# 2. METHOD
# ------------------------------------------------------------

# I use September 1, 2026, through August 31, 2028,
# as the two-year period based on the August 2026 snapshot.

# I use actual expiration dates instead of only expiration
# years so leases outside the two-year period are not included.

# I add a column to identify leases expiring in this period.
leases <- leases |>
  mutate(
    expiring_2yr = lease_expiration >= renewal_start &
      lease_expiration <= renewal_end
  )


# ------------------------------------------------------------
# 3. RESULTS
# ------------------------------------------------------------

# ------------------------------------------------------------
# TABLE 1: OVERALL TWO-YEAR EXPIRATION EXPOSURE
# ------------------------------------------------------------


# I calculate how many leases expire in the next two years.
# I also calculate the annual rent associated with them.

renewal_summary <- leases |>
  summarise(
    total_leases = n(),
    expiring_leases = sum(expiring_2yr, na.rm = TRUE),
    total_annual_rent = sum(annual_rent, na.rm = TRUE),
    expiring_annual_rent = sum(
      annual_rent[expiring_2yr],
      na.rm = TRUE
    )
  ) |>
  mutate(
    rent_expiring_pct = round(
      100 * expiring_annual_rent / total_annual_rent, 2
    )
  )

renewal_summary


# ------------------------------------------------------------
# TABLE 2: TWO-YEAR EXPIRATION EXPOSURE BY JURISDICTION
# ------------------------------------------------------------

# I group the leases by state and calculate how much
# annual rent is connected to upcoming expirations.

# I also calculate the share of each jurisdiction's
# annual rent that will reach its expiration date.

by_state_renewal <- leases |>
  group_by(state) |>
  summarise(
    total_leases = n(),
    expiring_leases = sum(expiring_2yr, na.rm = TRUE),
    total_annual_rent = sum(annual_rent, na.rm = TRUE),
    expiring_annual_rent = sum(
      annual_rent[expiring_2yr],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    rent_expiring_pct = ifelse(
      total_annual_rent > 0,
      100 * expiring_annual_rent / total_annual_rent,
      NA_real_
    )
  ) |>
  arrange(desc(expiring_annual_rent))

# I select the 10 jurisdictions with the largest
# amounts of annual rent approaching expiration.

top_10_renewal <- by_state_renewal |>
  head(10)

# I show the dollar amounts in millions for easier reading.
top_10_renewal |>
  mutate(
    total_rent_millions = round(total_annual_rent / 1000000, 2),
    expiring_rent_millions = round(expiring_annual_rent / 1000000, 2),
    rent_expiring_pct = round(rent_expiring_pct, 1)
  ) |>
  select(
    state,
    total_leases,
    expiring_leases,
    total_rent_millions,
    expiring_rent_millions,
    rent_expiring_pct
  )


# ------------------------------------------------------------
# CHART 1: HIGHEST EXPIRING ANNUAL RENT
# ------------------------------------------------------------

# I use a horizontal bar chart to show where the largest
# dollar amounts of federal rent are approaching expiration.

# I add labels so the reader can see the values directly
# without needing to compare the bars with the table.

chart_renewal_dollars <- ggplot(
  top_10_renewal,
  aes(
    x = reorder(state, expiring_annual_rent),
    y = expiring_annual_rent / 1000000
  )
) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(
    aes(
      label = paste0(
        "$",
        round(expiring_annual_rent / 1000000, 1),
        "M"
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Top 10 Jurisdictions by Expiring Annual Rent",
    subtitle = "Leases expiring from September 2026 through August 2028",
    x = NULL,
    y = "Expiring Annual Rent (USD Millions)",
    caption = "Source: GSA Lease Inventory, August 2026"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

chart_renewal_dollars


# ------------------------------------------------------------
# TABLE 3: HIGHEST PERCENTAGE OF ANNUAL RENT EXPIRING
# ------------------------------------------------------------

# I also compare the percentage of rent expiring.
# This shows which jurisdictions have more of their
# current federal rent tied to upcoming expirations.

# I include jurisdictions with at least $10 million in
# total annual rent so very small markets do not dominate
# the ranking based only on percentages.

# This threshold is my choice for comparing larger markets.
# I still keep every jurisdiction in the full analysis.

high_pct_renewal <- by_state_renewal |>
  filter(total_annual_rent >= 10000000) |>
  arrange(desc(rent_expiring_pct))

top_10_pct <- high_pct_renewal |>
  head(10)

top_10_pct |>
  mutate(
    expiring_rent_millions = round(
      expiring_annual_rent / 1000000, 2
    ),
    rent_expiring_pct = round(rent_expiring_pct, 1)
  ) |>
  select(
    state,
    expiring_leases,
    expiring_rent_millions,
    rent_expiring_pct
  )


# ------------------------------------------------------------
# CHART 2: HIGHEST SHARE OF ANNUAL RENT EXPIRING
# ------------------------------------------------------------

# I create another horizontal bar chart to show the
# percentage of each jurisdiction's rent approaching expiration.

# This chart helps me compare relative exposure instead
# of only looking at the largest dollar amounts.

chart_renewal_pct <- ggplot(
  top_10_pct,
  aes(
    x = reorder(state, rent_expiring_pct),
    y = rent_expiring_pct
  )
) +
  geom_col(fill = "darkorange", width = 0.7) +
  geom_text(
    aes(label = paste0(round(rent_expiring_pct, 1), "%")),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 55),
    breaks = seq(0, 50, 10),
    expand = c(0, 0)
  ) +
  labs(
    title = "Jurisdictions With the Highest Two-Year Rent Exposure",
    subtitle = "Share of annual rent tied to leases expiring by August 2028",
    x = NULL,
    y = "Annual Rent Expiring (%)",
    caption = "Includes jurisdictions with at least $10M in annual rent"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

chart_renewal_pct


# ------------------------------------------------------------
# TABLE 4: TIMING OF UPCOMING LEASE EXPIRATIONS
# ------------------------------------------------------------

# I check how much annual rent is reaching expiration
# in each calendar year within the two-year period.

# The year 2026 only covers September through December,
# while 2028 only covers January through August.
# I will consider this difference when interpreting the totals.

renewal_timing <- leases |>
  filter(expiring_2yr) |>
  group_by(expiration_year) |>
  summarise(
    expiring_leases = n(),
    expiring_annual_rent = sum(annual_rent, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(expiration_year)

renewal_timing


# ------------------------------------------------------------
# CHART 3: WHEN DOES THE RENT REACH EXPIRATION?
# ------------------------------------------------------------

# I use a bar chart to show the timing of upcoming expirations.
# This helps me understand when JLL may need to focus
# more attention on lease reviews and renewal planning.

chart_renewal_timing <- ggplot(
  renewal_timing,
  aes(
    x = factor(expiration_year),
    y = expiring_annual_rent / 1000000
  )
) +
  geom_col(fill = "seagreen", width = 0.6) +
  geom_text(
    aes(
      label = paste0(
        "$",
        round(expiring_annual_rent / 1000000, 1),
        "M"
      )
    ),
    vjust = -0.3,
    size = 3.5
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Timing of Federal Lease Expirations",
    subtitle = "Annual rent associated with leases expiring in the two-year period",
    x = "Expiration Year",
    y = "Expiring Annual Rent (USD Millions)",
    caption = "2026: Sep-Dec | 2027: Full year | 2028: Jan-Aug"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

chart_renewal_timing


# ------------------------------------------------------------
# VALIDATION: CHECK THE CALCULATIONS
# ------------------------------------------------------------

# I compare the state-level totals with the full portfolio.
# Both calculations should give the same amounts.

validation_renewal <- tibble(
  check = c("Total Annual Rent", "Expiring Annual Rent"),
  portfolio_total = c(
    renewal_summary$total_annual_rent,
    renewal_summary$expiring_annual_rent
  ),
  state_total = c(
    sum(by_state_renewal$total_annual_rent),
    sum(by_state_renewal$expiring_annual_rent)
  )
)

validation_renewal


# ------------------------------------------------------------
# 4. BUSINESS IMPLICATIONS AND RECOMMENDATIONS
# ------------------------------------------------------------

# The 1,687 leases below are only the forward-looking
# September 2026 to August 2028 group. I also found 233
# already-due leases worth $145.23M in annual rent.
# The combined 1,920 leases represent $1.30B, or 22.3%
# of the portfolio, as shown in my earlier checks.
# Separately, 2,765 soft-term leases involve $1.24B.
# These categories overlap, so I do not add them directly.

# Across the federal lease portfolio, 1,687 leases are
# scheduled to expire between September 2026 and August 2028.

# These leases represent approximately $1.16 billion in
# recorded annual rent, or 19.8% of the total portfolio.

# Washington, DC has the largest amount of expiring annual
# rent at $192.62 million across 38 leases.
# This represents 27.5% of DC's total annual federal rent.

# Virginia ranks second with $144.23 million in annual rent
# approaching expiration across 71 leases, followed by
# California with $115.07 million across 169 leases.

# Together, DC, Virginia, and California account for
# approximately $451.91 million in expiring annual rent.
# This is about 39.1% of the portfolio's two-year exposure.

# For JLL, I would prioritize DC and Virginia for reviewing
# high-value federal leases and upcoming renewal decisions.

# California would also be a priority because it has
# a larger number of leases approaching expiration.
# JLL could investigate these leases for portfolio-wide
# renewal planning, consolidation, or relocation analysis.

# Florida and New York would be my next markets to review.
# Their expiring annual rent is approximately $64.90 million
# and $60.93 million, respectively.

# Alabama stands out when I compare percentage exposure.
# About 43.4% of its current annual rent is tied to leases
# expiring in the next two years, or roughly $31.01 million.

# Although Alabama has a smaller dollar amount than DC,
# its high percentage suggests a more concentrated
# expiration exposure within its federal lease portfolio.

# The timing analysis also shows that 2027 has the largest
# calendar-year expiration total, with 840 leases and
# approximately $538.09 million in annual rent.

# However, 2027 covers 12 months, while 2026 and 2028
# include only part of the year in this analysis.
# I would not compare these totals as equal time periods.

# My recommendation is for JLL to use both financial
# exposure and expiration timing to prioritize lease reviews.

# I would investigate DC, Virginia, and California first
# because they have the largest dollar amounts approaching
# expiration, followed by Florida and New York.

# I would also review Alabama as a targeted market because
# a particularly large share of its annual rent is expiring.

# Before recommending specific actions, JLL should examine
# renewal options, individual lease terms, agency space
# needs, and possible consolidation or relocation plans.

# These figures represent existing annual rent connected
# to upcoming lease expirations, not guaranteed renewals
# or the value of future lease contracts.



# ------------------------------------------------------------
# ADDITIONAL CHECK: COMBINED EXPIRATION EXPOSURE BY STATE
# ------------------------------------------------------------

# I also include leases already due when checking which
# markets need attention first. This is separate from my
# original two-year ranking and does not change that ranking.

by_state_combined <- leases %>%
  group_by(state) %>%
  summarise(
    already_due_leases = sum(lease_expiration <= snapshot_date, na.rm = TRUE),
    already_due_rent = sum(annual_rent[!is.na(lease_expiration) &
                                             lease_expiration <= snapshot_date], na.rm = TRUE),
    upcoming_rent = sum(annual_rent[!is.na(lease_expiration) &
                                      lease_expiration >= renewal_start &
                                      lease_expiration <= renewal_end], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(combined_annual_rent = already_due_rent + upcoming_rent) %>%
  arrange(desc(combined_annual_rent))

# This table shows DC's combined exposure, including
# leases due on or before the August 2026 reporting date.
print(by_state_combined %>%
        filter(state == "DC") %>%
        mutate(across(c(already_due_rent, upcoming_rent, combined_annual_rent),
                      ~ round(.x / 1000000, 2))))


# ============================================================
# OBJECTIVE 3: CAN YOU MAP THIS INFORMATION BY STATE
# AND EXPLAIN WHAT IT MEANS FOR OUR LEASING STRATEGY?
# ============================================================


# ------------------------------------------------------------
# 1. QUESTION
# ------------------------------------------------------------

# Can you map this information by state and explain
# what it means for our leasing strategy?

# I want to understand where federal rent is concentrated
# and which jurisdictions have the most rent expiring soon.

# I will compare the geographic patterns with the results
# from Objectives 1 and 2 to identify where JLL should
# focus its lease advisory and renewal planning research.


# ------------------------------------------------------------
# 2. METHOD
# ------------------------------------------------------------

# I use the same mapping packages from Web Exercise 4.
# The sf package works with geographic boundaries, while
# spData provides the state map used for this analysis.

library(sf)
library(spData)

# I combine the state-level rent results from Objective 1
# with the expiration results from Objective 2.

# This helps me compare current federal rent with
# the annual rent approaching expiration.

map_summary <- by_state_rent |>
  select(
    state,
    total_annual_rent,
    lease_count
  ) |>
  left_join(
    by_state_renewal |>
      select(
        state,
        expiring_annual_rent,
        expiring_leases,
        rent_expiring_pct
      ),
    by = "state"
  )


# ------------------------------------------------------------
# CONVERT STATE ABBREVIATIONS TO FULL NAMES
# ------------------------------------------------------------

# My lease data uses abbreviations such as VA and CA,
# but the map data uses full state names.

# I convert the abbreviations so the two datasets
# can be joined using the same geographic names.

map_summary <- map_summary |>
  mutate(
    NAME = ifelse(
      state == "DC",
      "District of Columbia",
      state.name[match(state, state.abb)]
    )
  )

# I check which jurisdictions were not converted.
# Some territories are not included in the state.name list.

map_summary |>
  filter(is.na(NAME)) |>
  select(state)


# ------------------------------------------------------------
# JOIN THE LEASE DATA WITH THE STATE MAP
# ------------------------------------------------------------

# I join the state-level rent and expiration results
# with the geographic boundaries from spData.

map_data <- us_states |>
  left_join(
    map_summary,
    by = "NAME"
  )

# I check whether any map boundaries are missing
# their corresponding lease information.

map_data |>
  filter(is.na(total_annual_rent)) |>
  st_drop_geometry() |>
  select(NAME)


# ------------------------------------------------------------
# MAKE WASHINGTON, DC VISIBLE ON THE MAPS
# ------------------------------------------------------------

# DC is hard to see on a national map because it is small.
# I use a close-up map so its color follows the same scale
# as the other states, instead of marking it in red.

# I use cowplot to place the close-up inside each map.
# If it is not installed, I can run install.packages("cowplot") once.
if (!requireNamespace("cowplot", quietly = TRUE)) {
  stop("Please install the cowplot package: install.packages('cowplot')")
}

# This function makes a larger view of DC and nearby areas.
# The limits match the color scale in the main map.
make_dc_inset <- function(data, rent_column, scale_limits, label) {
  ggplot(data) +
    geom_sf(aes(fill = .data[[rent_column]]),
            color = "white", linewidth = 0.4) +
    coord_sf(
      xlim = c(-77.20, -76.88),
      ylim = c(38.78, 39.05),
      default_crs = sf::st_crs(4326),
      expand = FALSE
    ) +
    scale_fill_viridis_c(limits = scale_limits, guide = "none",
                         na.value = "grey90") +
    labs(title = label) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = "grey40"),
      plot.margin = margin(6, 6, 6, 6)
    )
}

# I place the close-up in the lower-right part of each map.
# It is a real geographic view of DC, not an extra data point.
# The smaller inset leaves more space for the map legend/caption.
add_dc_inset <- function(main_map, dc_inset) {
  cowplot::ggdraw(main_map) +
    cowplot::draw_plot(dc_inset, x = 0.69, y = 0.11,
                       width = 0.19, height = 0.25)
}

# ------------------------------------------------------------
# 3. RESULTS
# ------------------------------------------------------------


# ------------------------------------------------------------
# MAP 1: TOTAL ANNUAL FEDERAL RENT BY STATE
# ------------------------------------------------------------

# My first map shows where the federal government has
# the largest recorded annual rental commitments.

# I divide annual rent by one million to make
# the legend easier to read.

map_total_rent <- ggplot(map_data) +
  geom_sf(
    aes(fill = total_annual_rent / 1000000),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    name = "Annual Rent\n(USD Millions)",
    labels = scales::label_number(accuracy = 1),
    na.value = "grey90"
  ) +
  labs(
    title = "Federal Annual Rent by State",
    subtitle = "National overview with a magnified DC view",
    caption = paste(
      "Source: GSA Lease Inventory, August 2026",
      "Map excludes AK, HI, and U.S. territories.",
      sep = "\n"
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    plot.caption = element_text(size = 8, hjust = 0)
  )

# I display the first map.

# I use the same values and color limits for the DC close-up.
map_total_rent_values <- map_data %>%
  mutate(dc_value = total_annual_rent / 1000000)
map_total_rent_dc <- map_total_rent_values %>%
  filter(state == "DC") %>%
  st_drop_geometry() %>%
  pull(dc_value)
map_total_rent_inset <- make_dc_inset(
  map_total_rent_values, "dc_value",
  range(map_total_rent_values$dc_value, na.rm = TRUE),
  sprintf("DC | $%.2fM", map_total_rent_dc)
)
map_total_rent <- add_dc_inset(map_total_rent, map_total_rent_inset)

map_total_rent


# ------------------------------------------------------------
# MAP 2: ANNUAL RENT EXPIRING WITHIN TWO YEARS
# ------------------------------------------------------------

# My second map shows where the largest dollar amounts
# of annual rent are tied to leases expiring soon.

# I use the two-year period from September 1, 2026,
# through August 31, 2028.

map_expiring_rent <- ggplot(map_data) +
  geom_sf(
    aes(fill = expiring_annual_rent / 1000000),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    name = "Expiring Rent\n(USD Millions)",
    labels = scales::label_number(accuracy = 1),
    na.value = "grey90"
  ) +
  labs(
    title = "Federal Annual Rent Approaching Expiration",
    subtitle = "Leases expiring from September 2026 through August 2028",
    caption = paste(
      "Source: GSA Lease Inventory, August 2026",
      "Map excludes AK, HI, and U.S. territories.",
      sep = "\n"
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    plot.caption = element_text(size = 8, hjust = 0)
  )

# I display the second map.

# I use the same values and color limits for the DC close-up.
map_expiring_rent_values <- map_data %>%
  mutate(dc_value = expiring_annual_rent / 1000000)
map_expiring_rent_dc <- map_expiring_rent_values %>%
  filter(state == "DC") %>%
  st_drop_geometry() %>%
  pull(dc_value)
map_expiring_rent_inset <- make_dc_inset(
  map_expiring_rent_values, "dc_value",
  range(map_expiring_rent_values$dc_value, na.rm = TRUE),
  sprintf("DC | $%.2fM", map_expiring_rent_dc)
)
map_expiring_rent <- add_dc_inset(map_expiring_rent, map_expiring_rent_inset)

map_expiring_rent


# ------------------------------------------------------------
# MAP 3: PERCENTAGE OF ANNUAL RENT EXPIRING
# ------------------------------------------------------------

# My third map shows the share of each jurisdiction's
# annual federal rent approaching expiration.

# This is different from Map 2 because a state can
# have a high percentage but a smaller dollar amount.

map_expiring_pct <- ggplot(map_data) +
  geom_sf(
    aes(fill = rent_expiring_pct),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_viridis_c(
    name = "Rent Expiring\n(%)",
    labels = scales::label_number(
      accuracy = 1,
      suffix = "%"
    ),
    na.value = "grey90"
  ) +
  labs(
    title = "Share of Federal Annual Rent Expiring by State",
    subtitle = "Two-year expiration share, with DC shown in a close-up",
    caption = paste(
      "Source: GSA Lease Inventory, August 2026",
      "Map excludes AK, HI, and U.S. territories.",
      sep = "\n"
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    plot.caption = element_text(size = 8, hjust = 0)
  )

# I display the third map.

# I use the same values and color limits for the DC close-up.
map_expiring_pct_values <- map_data %>%
  mutate(dc_value = rent_expiring_pct)
map_expiring_pct_dc <- map_expiring_pct_values %>%
  filter(state == "DC") %>%
  st_drop_geometry() %>%
  pull(dc_value)
map_expiring_pct_inset <- make_dc_inset(
  map_expiring_pct_values, "dc_value",
  range(map_expiring_pct_values$dc_value, na.rm = TRUE),
  sprintf("DC | %.1f%%", map_expiring_pct_dc)
)
map_expiring_pct <- add_dc_inset(map_expiring_pct, map_expiring_pct_inset)

map_expiring_pct


# ------------------------------------------------------------
# ADDITIONAL MAP: INCLUDING ALREADY-DUE LEASES
# ------------------------------------------------------------

# My professor also asked me to include leases already due.
# I show this broader expiration measure on a separate map,
# rather than changing the definition of my original maps.

map_combined_data <- map_data %>%
  left_join(by_state_combined %>%
              select(state, combined_annual_rent), by = "state")

map_combined_expiration <- ggplot(map_combined_data) +
  geom_sf(aes(fill = combined_annual_rent / 1000000),
          color = "white", linewidth = 0.25) +
  scale_fill_viridis_c(name = "Annual Rent\n(USD Millions)",
                       labels = scales::label_number(accuracy = 1),
                       na.value = "grey90") +
  labs(
    title = "Combined Expiration Exposure by State",
    subtitle = "Already due plus next two years, with DC shown in a close-up",
    caption = "Source: GSA Lease Inventory, August 2026. Excludes AK, HI and U.S. territories."
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        legend.position = "right",
        plot.caption = element_text(hjust = 0))


# I use the same values and color limits for the DC close-up.
map_combined_expiration_values <- map_combined_data %>%
  mutate(dc_value = combined_annual_rent / 1000000)
map_combined_expiration_dc <- map_combined_expiration_values %>%
  filter(state == "DC") %>%
  st_drop_geometry() %>%
  pull(dc_value)
map_combined_expiration_inset <- make_dc_inset(
  map_combined_expiration_values, "dc_value",
  range(map_combined_expiration_values$dc_value, na.rm = TRUE),
  sprintf("DC | $%.2fM", map_combined_expiration_dc)
)
map_combined_expiration <- add_dc_inset(map_combined_expiration, map_combined_expiration_inset)

map_combined_expiration


# ============================================================
# COMPARISON MAP: TOTAL RENT VS. UPCOMING EXPIRATIONS
# ============================================================

# I combine my first two maps to compare where the federal
# government pays the most rent with where that rent is
# approaching expiration within the next two years.
#
# This comparison helps me see whether the highest-spending
# jurisdictions also have the greatest renewal exposure.
#
# I keep separate legends because total annual rent and
# expiring annual rent measure different dollar amounts.

map_rent_comparison <- cowplot::plot_grid(
  map_total_rent,
  map_expiring_rent,
  ncol = 2,
  labels = c("A", "B"),
  label_size = 14,
  align = "h"
)

# I display the combined comparison map.
print(map_rent_comparison)


# ------------------------------------------------------------
# TABLE 1: COMPARE FEDERAL RENT AND UPCOMING EXPIRATIONS
# ------------------------------------------------------------

# I compare total annual rent with the amount
# approaching expiration in the next two years.

# This helps me identify markets where JLL may have
# significant renewal-related work to investigate.

state_strategy <- map_summary |>
  mutate(
    total_rent_millions = round(
      total_annual_rent / 1000000, 2
    ),
    expiring_rent_millions = round(
      expiring_annual_rent / 1000000, 2
    ),
    expiring_pct = round(rent_expiring_pct, 1)
  ) |>
  arrange(desc(expiring_annual_rent))

# I show the 10 jurisdictions with the largest amount
# of annual federal rent approaching expiration.

state_strategy |>
  select(
    state,
    lease_count,
    expiring_leases,
    total_rent_millions,
    expiring_rent_millions,
    expiring_pct
  ) |>
  head(10)


# ------------------------------------------------------------
# TABLE 2: COMPARE THE FIVE LARGEST FEDERAL RENT MARKETS
# ------------------------------------------------------------

# I check whether the five highest-spending jurisdictions
# also have large amounts of rent approaching expiration.

# This connects the findings from Objectives 1 and 2.

top_5_markets <- by_state_rent |>
  arrange(desc(total_annual_rent)) |>
  head(5) |>
  select(state)

state_strategy |>
  filter(state %in% top_5_markets$state) |>
  select(
    state,
    total_rent_millions,
    expiring_rent_millions,
    expiring_pct,
    expiring_leases
  ) |>
  arrange(desc(expiring_rent_millions))


# ------------------------------------------------------------
# TABLE 3: JURISDICTIONS WITH HIGH PERCENTAGE EXPOSURE
# ------------------------------------------------------------

# I also compare the percentage of rent expiring.
# This helps identify markets that may have a
# concentrated expiration workload.

# I keep the same $10 million minimum from Objective 2
# so this percentage comparison uses a consistent method.

state_strategy |>
  filter(total_annual_rent >= 10000000) |>
  arrange(desc(expiring_pct)) |>
  select(
    state,
    expiring_leases,
    expiring_rent_millions,
    expiring_pct
  ) |>
  head(10)


# ------------------------------------------------------------
# VALIDATION: CHECK THE MAP COVERAGE
# ------------------------------------------------------------

# I compare the full federal lease portfolio with
# the rent totals represented in the state maps.

# The totals may differ because the geographic boundary
# dataset does not include every U.S. jurisdiction.

map_validation <- tibble(
  measure = c(
    "Total Annual Rent",
    "Expiring Annual Rent"
  ),
  full_portfolio = c(
    sum(by_state_rent$total_annual_rent),
    sum(by_state_renewal$expiring_annual_rent)
  ),
  mapped_portfolio = c(
    sum(map_data$total_annual_rent, na.rm = TRUE),
    sum(map_data$expiring_annual_rent, na.rm = TRUE)
  )
) |>
  mutate(
    difference = full_portfolio - mapped_portfolio,
    mapped_pct = round(
      100 * mapped_portfolio / full_portfolio,
      2
    )
  )

# I check the totals and percentage represented by the maps.
map_validation


# ------------------------------------------------------------
# CHECK WHICH JURISDICTIONS ARE NOT INCLUDED IN THE MAP
# ------------------------------------------------------------

# I identify the jurisdictions missing from the
# geographic boundaries used for the maps.

# I keep these jurisdictions in the financial analysis
# instead of removing them from the portfolio.

missing_map_states <- map_summary |>
  filter(!(state %in% map_data$state)) |>
  mutate(
    annual_rent_millions = round(
      total_annual_rent / 1000000, 2
    ),
    expiring_rent_millions = round(
      expiring_annual_rent / 1000000, 2
    )
  ) |>
  select(
    state,
    annual_rent_millions,
    expiring_rent_millions
  ) |>
  arrange(desc(annual_rent_millions))

# I display the jurisdictions excluded from the maps.
missing_map_states


# ------------------------------------------------------------
# MAP COVERAGE AND LIMITATIONS
# ------------------------------------------------------------

# The state map does not include Alaska, Hawaii,
# Puerto Rico, Guam, Northern Mariana Islands,
# U.S. Virgin Islands, or American Samoa.

# These jurisdictions account for approximately
# $148.06 million in annual rent and $23.60 million
# in annual rent approaching expiration.

# I kept these jurisdictions in the complete analysis
# because they are part of the federal lease portfolio.

# Washington, DC is very small on the national maps.
# The inset magnifies DC without changing its color scale.
# I also use the state ranking tables for exact amounts.


# ------------------------------------------------------------
# 4. BUSINESS IMPLICATIONS AND RECOMMENDATIONS
# ------------------------------------------------------------

# Based on the maps and state-level analysis, I found that
# federal rent and upcoming lease expirations are concentrated
# in certain jurisdictions rather than evenly distributed.

# I think JLL should consider both the dollar amount of
# expiring rent and the percentage of rent expiring before
# deciding which markets need the most attention.


# ------------------------------------------------------------
# PRIORITY 1: WASHINGTON, DC AND VIRGINIA
# ------------------------------------------------------------

# My first recommendation is for JLL to focus its initial
# research on Washington, DC and Virginia because they have
# the largest amounts of annual rent approaching expiration.

# Washington, DC has approximately $192.62 million in
# annual rent associated with 38 expiring leases, while
# Virginia has $144.23 million across 71 expiring leases.

# Together, these two jurisdictions represent approximately
# $336.85 million in annual rent approaching expiration.

# I would begin by reviewing the largest individual leases
# in these markets, including their expiration dates,
# renewal options, and current space requirements.

# This could help JLL identify which leases may require
# further analysis for renegotiation, consolidation,
# or relocation.


# ------------------------------------------------------------
# PRIORITY 2: CALIFORNIA
# ------------------------------------------------------------

# California would be my next priority because it has
# approximately $115.07 million in annual rent associated
# with 169 leases expiring within the next two years.

# I noticed that California has more expiring leases
# than DC or Virginia, even though its expiring annual
# rent is lower than both jurisdictions.

# This suggests that JLL may need a different approach
# in California, focusing on reviewing a larger number
# of leases rather than only a few high-value contracts.

# I would examine whether these leases are concentrated
# in certain cities or spread across the state before
# considering portfolio management or space optimization.


# ------------------------------------------------------------
# PRIORITY 3: FLORIDA AND NEW YORK
# ------------------------------------------------------------

# Florida and New York would be my next markets to investigate.
# Florida has approximately $64.90 million in annual rent
# approaching expiration, followed by New York at $60.93 million.

# Although their dollar exposure is lower than DC, Virginia,
# and California, these amounts are still significant.

# I would review the largest upcoming expirations in these
# markets to understand which properties may require
# earlier lease planning or further evaluation.


# ------------------------------------------------------------
# TARGETED REVIEW: ALABAMA AND GEORGIA
# ------------------------------------------------------------

# Alabama stands out because approximately 43.4% of its
# annual federal rent is approaching expiration.

# This represents about $31.01 million in expiring annual
# rent, which is much smaller than DC's dollar exposure.

# Georgia also has relatively high exposure, with 28.6%
# of its annual rent approaching expiration, representing
# approximately $36.63 million.

# I think these jurisdictions deserve targeted attention
# because upcoming expirations affect a relatively large
# share of their existing federal lease portfolios.

# I would investigate whether these expirations are
# concentrated in a few large properties or spread
# across several smaller leases.

# ------------------------------------------------------------
# FINAL LEASING STRATEGY
# ------------------------------------------------------------

# Based on these findings, I would not recommend using
# the same leasing strategy for every jurisdiction.

# The dollar amount of expiring rent helps identify where
# the largest existing rental commitments are approaching
# expiration, while the percentage shows how much of each
# jurisdiction's portfolio may be affected.

# The number of expiring leases also matters because
# managing many smaller leases may require a different
# approach from reviewing a few high-value contracts.

# For JLL, I think combining these three measures would
# provide a better starting point for prioritizing
# lease advisory research and renewal planning.

# Before recommending specific leasing actions, I would
# also examine individual lease terms, renewal options,
# agency space requirements, and possible relocation costs.

# One limitation is that a lease approaching expiration
# does not necessarily mean it will be renewed, relocated,
# or become a new business opportunity.

# Overall, the geographic analysis helps identify which
# markets deserve closer investigation and provides a
# clearer basis for allocating JLL's research resources.


# ============================================================
# ANSWERS TO THE THREE OBJECTIVES
# ============================================================


# ------------------------------------------------------------
# QUESTION 1: WHERE DOES THE FEDERAL GOVERNMENT PAY THE MOST RENT?
# ------------------------------------------------------------

# Based on my analysis, the federal government pays the
# most annual rent in Virginia, with approximately
# $723.05 million. Washington, DC comes second with
# $700.25 million, followed by California with $534.91 million.

# These three jurisdictions account for approximately
# 33.5% of the total annual rent in the GSA lease dataset.


# ------------------------------------------------------------
# QUESTION 2: WHERE ARE LARGE PORTIONS OF THAT RENT
# COMING UP FOR RENEWAL IN THE NEXT TWO YEARS?
# ------------------------------------------------------------

# I separated the 233 already-due leases ($145.23M)
# from the 1,687 leases expiring in the next two years.
# Together, they represent $1.30B in recorded annual rent.
# DC's combined expiration exposure rises to about $230.52M.
# I also checked soft-term leases separately because
# some leases allow earlier termination than the end date.

# I found that the largest amount of annual rent approaching
# expiration in the next two years is in Washington, DC,
# with approximately $192.62 million.

# Virginia comes next with $144.23 million, followed
# by California with $115.07 million.

# Overall, 1,687 leases are scheduled to expire between
# September 1, 2026, and August 31, 2028. These leases
# represent approximately $1.16 billion, or 19.8%
# of the total annual rent in the dataset.


# ------------------------------------------------------------
# QUESTION 3: CAN YOU MAP THIS INFORMATION BY STATE
# AND EXPLAIN WHAT IT MEANS FOR OUR LEASING STRATEGY?
# ------------------------------------------------------------

# Yes, I was able to map the federal lease information
# by state and compare annual rent with upcoming expirations.

# From the maps and state-level results, I found that
# Virginia and DC are important markets because of their
# high federal rental commitments. However, DC has more
# annual rent approaching expiration than Virginia.

# I also noticed that Alabama has a high expiration
# percentage of 43.4%, even though its total expiring
# annual rent is much smaller than DC's.

# This shows that the leasing strategy should consider
# both how much rent is approaching expiration and what
# percentage of a jurisdiction's rent it represents.

# The maps exclude Alaska, Hawaii, and U.S. territories,
# but these jurisdictions are still included in my
# overall financial analysis.


# ============================================================
# SAVE ALL CHARTS AND MAPS FOR MY PRESENTATION
# ============================================================

# I want to save all the charts and maps from Part 2
# as PNG files so I can use them in my final presentation.

# I create a separate folder to keep all the figures
# together instead of saving them in different places.

if (!dir.exists("part2_figures")) {
  dir.create("part2_figures")
}


# ------------------------------------------------------------
# OBJECTIVE 1: ANNUAL FEDERAL RENT
# ------------------------------------------------------------

# I save the bar chart showing the 10 jurisdictions
# where the federal government pays the most annual rent.

ggsave(
  "part2_figures/Objective_1_Top_10_Annual_Rent.png",
  plot = chart_rent,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------
# OBJECTIVE 2: UPCOMING LEASE EXPIRATIONS
# ------------------------------------------------------------

# I save the chart showing which jurisdictions have
# the largest dollar amounts of annual rent expiring.

ggsave(
  "part2_figures/Objective_2_Expiring_Rent_Dollars.png",
  plot = chart_renewal_dollars,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

# I also save the chart showing the percentage of
# annual rent approaching expiration in each jurisdiction.

ggsave(
  "part2_figures/Objective_2_Expiring_Rent_Percentage.png",
  plot = chart_renewal_pct,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

# I save the expiration timing chart to show when
# the federal leases are scheduled to expire.

ggsave(
  "part2_figures/Objective_2_Expiration_Timing.png",
  plot = chart_renewal_timing,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------
# OBJECTIVE 3: GEOGRAPHIC ANALYSIS
# ------------------------------------------------------------

# I save the first map showing total annual
# federal rent across the states.

ggsave(
  "part2_figures/Objective_3_Map_Total_Annual_Rent.png",
  plot = map_total_rent,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white"
)

# I save the second map showing where annual rent
# is approaching expiration within the next two years.

ggsave(
  "part2_figures/Objective_3_Map_Expiring_Rent.png",
  plot = map_expiring_rent,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white"
)

# I save the third map showing the percentage
# of annual rent approaching expiration by state.

ggsave(
  "part2_figures/Objective_3_Map_Expiring_Percentage.png",
  plot = map_expiring_pct,
  width = 14,
  height = 8,
  dpi = 300,
  bg = "white"
)


# I also save the new charts that address my professor's
# feedback about already-due leases and DC's exposure.

ggsave(
  "part2_figures/Objective_2_Already_Due_vs_Upcoming.png",
  plot = expiration_status_chart,
  width = 10, height = 7, dpi = 300, bg = "white"
)

ggsave(
  "part2_figures/Objective_3_Map_Combined_Expiration.png",
  plot = map_combined_expiration,
  width = 14, height = 8, dpi = 300, bg = "white"
)

# I save the combined map to compare total annual rent
# with the rent approaching expiration in one figure.

ggsave(
  "part2_figures/Objective_3_Combined_Rent_Comparison.png",
  plot = map_rent_comparison,
  width = 20,
  height = 10,
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------
# CHECK THE SAVED FILES
# ------------------------------------------------------------

# I check whether all ten PNG files were saved
# successfully in my presentation figures folder.

saved_figures <- list.files(
  "part2_figures",
  pattern = "\\.png$",
  full.names = TRUE
)

# I display the names of the saved figures.
print(saved_figures)

# I check the number of PNG files in the folder.
length(saved_figures)

# ============================================================
# END OF PART 2: ANALYSIS AND VISUALIZATION
# ============================================================