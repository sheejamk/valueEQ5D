## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(EQ5DmapR)

## ---- echo=TRUE, results='asis'------------------------------------------
value3LUK_responses(1,2,3,2,2)

## ---- echo=TRUE, results='asis'------------------------------------------
valueEQ5D5LUK_DevlinMethod(23434)
valueEQ5D5LUK_DevlinMethod(2,3,4,3,4)
valueEQ5D5LUK_DevlinMethod(c(2,3,4,3,4))

## ---- echo=TRUE, results='asis'------------------------------------------
eq5dmap5Lto3L_VanHoutMethod(11125)
eq5dmap5Lto3L_VanHoutMethod(1,1,1,2,5)
eq5dmap5Lto3L_VanHoutMethod(c(1,1,1,2,5))

