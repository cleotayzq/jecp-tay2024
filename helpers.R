all_data <- read.csv("data_shiny.csv")

dropdown_choices <- list(
  `demographics & counterbalancing` = list(`Age (Months)` = "ageMonths",
                                           Gender = "gender",
                                           `Experimental Order (Presented First)` = "expt_order"),
  `executive function` = list(`Backward Digit Span` = "bds"),
  `second-order theory of mind` = list(`Second-Order ToM (Total)` = "tom_2orderTotal",
                                       `Second-Order Belief (Total)` = "tom_2orderBelief",
                                       `Birthday Puppy Knowledge-Perception` = "tomS_mumKnowSopSaw",
                                       `Birthday Puppy Belief-Belief` = "tomS_mumThinkSopTell",
                                       `Three Goals Knowledge-Desire 1 & 2` = "tomR_KnowWant",
                                       `Three Goals Belief-Knowledge` = "tomR_dadSayRobKnow",
                                       `Three Goals Belief-Belief` = "tomR_dadSayRobThink"),
  `sentential complements` = list(`Sentential Complements (Total)` = "SC_total",
                                  `Sentential Complements (Think)` = "SC_thinkTotal",
                                  `Sentential Complements (Say)` = "SC_sayTotal")
)

continuous_vars <- c("ageMonths", "bds", "tom_2orderTotal", "tom_2orderBelief", "tomR_KnowWant",
                     "SC_total", "SC_thinkTotal", "SC_sayTotal")

sidebarPanel2 <- function (..., out = NULL, width = 4) {
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}