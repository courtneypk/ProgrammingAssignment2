open.account <- function(total) {
  list(
    deposit = function(amount) {
      if(amount <= 0)
        stop("Deposits must be positive!\n")
      total <<- total + amount
      cat(amount, "deposited. Your balance is", total, "\n\n")
      },
    withdraw = function(amount) {
      if(amount > total)
        stop("You don’t have that much money!\n")
      total <<- total - amount
      cat(amount, "withdrawn. Your balance is", total, "\n\n")
      },
    balance = function() {
      cat("Your balance is", total, "\n\n")
    }
  )
}
