library(haven)
library(dplyr)
# ------------------------------------------------------------
# Prepare CGSS 2017/2018/2021 gender-attitude items for IRTree analysis in Mplus
#
# Steps:
#   1) Read CGSS data files (2017, 2018, 2021)
#   2) Extract the five gender-attitude items and harmonize variable names
#   3) Recode nonsubstantive/invalid responses to a single category (coded as 6)
#   4) Reverse-code item a425 (keeping category 6 unchanged)
#   5) Expand the responses into pseudo-items using an IRTree mapping matrix
#   6) Export the wide-format pseudo-item dataset to a tab-delimited .txt file for Mplus
# ------------------------------------------------------------
# ---- 1) Read data ----
cgss2021 <- read_dta("CGSS2021.dta")
cgss2018 <- read_dta("cgss2018.dta")
cgss2017 <- read_sav("cgss2017.sav")

# ---- 2) Extract and harmonize the five items ----
g2021 <- cgss2021[, c("id", "A42_1", "A42_2", "A42_3", "A42_4", "A42_5")]
g2021$year <- "2021"
colnames(g2021) <- c("id", "a421", "a422", "a423", "a424", "a425", "year")

g2018 <- cgss2018[, c("id", "a421", "a422", "a423", "a424", "a425")]
g2018$year <- "2018"

g2017 <- cgss2017[ ,c("id","a421", "a422", "a423", "a424", "a425")]
g2017$year <- "2017"

# Combine three waves
gdata <- rbind(g2017, g2018, g2021)

# ---- 3) Recode responses ----
# Convert to integers and collapse all non-1..5 values (nonsubstantive answers) to 6

gdata[, 2:6] <- lapply(gdata[, 2:6], function(x) {
  x <- as.integer(as.numeric(x))
  x[!(x %in% 1:5)] <- 6
  x
})

# Reverse-code a425 for substantive responses; keep category 6 unchanged
gdata$a425 <- ifelse(gdata$a425 %in% 1:5, 6 - gdata$a425, 6)

# ---- 4) IRTree mapping matrix ----
# Rows correspond to observed response categories (1..5 and 6 = nonsubstantive).
# Columns correspond to pseudo-items (nodes) in the IRTree representation.
#map data for irtree models
mapgdata <- cbind(c(0, 0, 0, 0, 0, 1), 
                  c(0, 0, 1, 0, 0, NA), 
                  c(0, 0, NA, 1, 1, NA),
                  c(1, 0, NA, 0, 1, NA))

# ---- 5) Expand observed responses into pseudo-items ----
# The dendrify2() function is adapted from FLIRT (Flexible Item Response Theory Modeling).
# It converts item responses into a pseudo-item representation used by the IRTree model.

dendrify2 <- function(mat, cmx, missing.omit=FALSE, wide=FALSE) {

# mat: original item response data 
# cmx: mapping matrix 
# missing.omit: for a long-form data, the lines with 
# a missing item response are removed
# wide: for a wide-form expanded data 

ff <- factor((m1 <- tolong(mat) )[["value"]])
stopifnot(is.matrix(cmx) | is.data.frame(mat),
          (nr <- nrow(cmx)) == length(levels(ff)),
          (nc <- ncol(cmx)) < nr)
if (missing.omit ==TRUE) {
  long <-  subset(within(data.frame(value = 
                                      as.vector(cmx[as.integer(ff), ]),
                                    item = rep(m1$item, nc),
                                    person = rep(m1$person, nc),
                                    node = gl(nc, nrow(m1),
                                              labels = sprintf(paste("node%0", nchar(nc), "d", sep=''),
                                                               seq_len(nc)))),sub <- item:node), !is.na(value))        
  
  return(long)
} else { 
  if (wide==FALSE) {   
    long <- within(data.frame(value = as.vector(cmx[as.integer(ff), ]),
                              item = rep(m1$item, nc),
                              person = rep(m1$person, nc),
                              node = gl(nc, nrow(m1),
                                        labels = sprintf(paste("node%0", nchar(nc), "d", sep=''),
                                                         seq_len(nc)))), sub <- item:node )
    return(long)
  } else {     
    long <- within(data.frame(value = as.vector(cmx[as.integer(ff), ]),
                              item = rep(m1$item, nc),
                              person = rep(m1$person, nc),
                              node = gl(nc, nrow(m1),
                                        labels = sprintf(paste("node%0", nchar(nc), "d", sep=''),
                                                         seq_len(nc)))), sub <- item:node )
    options(warn=-1)       
    wide <-  reshape(long, v.names = "value", idvar = "person",
                     timevar = "sub",  direction = "wide", drop=c("item","node")) 
    return(wide)
  }                               
}
} 
# Helper: convert a wide item-response matrix to long format with item/person indices
tolong <- function(mat) {
  stopifnot(is.matrix(mat)| is.data.frame(mat))
  mat <- as.matrix(mat)
  nr <- nrow(mat)
  nc <- ncol(mat)
  data.frame(value = as.vector(mat),
             item = gl(nc, nr,
                       labels = sprintf(paste("i%0", nchar(nc), "d", sep=''), 
                                        seq_len(nc))),
             person = gl(nr, 1, length=length(mat),
                         labels = sprintf(paste("p%0", nchar(nr), "d", sep=''), 
                                          seq_len(nr))))
}
# Create the wide pseudo-item dataset
tree_data <- dendrify2(gdata[, 2:6], mapgdata, wide = TRUE)

# ---- 6) Attach identifiers and export for Mplus ----
# The specific numeric values of this identifier are not substantively meaningful. 

tree_data$person <- seq(45955, 79471, 1)
tree_data$year <- gdata$year

# Mplus uses "." for missing values
tree_data[is.na(tree_data)] <- "."

#output the data 
write.table(tree_data, "1721tree.txt", sep = "\t", row.names = FALSE, 
            col.names = FALSE)



