
load("chains/data_products_corr_AR4.RData")


R.mean <- R.025 <- R.975 <- matrix(NA, dch$J, dch$J)

for (j in 1:dch$J) {
    for (k in 1:dch$J) {
        
        R.mean[j,k] <- mean(sapply(dch$R.chain, function (x) x[j,k]))
        R.025[j,k] <- quantile(sapply(dch$R.chain, function (x) x[j,k]), p=0.025)
        R.975[j,k] <- quantile(sapply(dch$R.chain, function (x) x[j,k]), p=0.975)
    }
}

print(round(R.mean, 3))

contains.zero <- R.025 <= 0 & R.975 >= 0

print(R.025)
print(R.975)


for (j in 1:dch$J) {
    for (k in 1:dch$J) {

        if (k <= j) {

            if (!contains.zero[j,k]) {

                cat("\\textbf{")
            }

            cat(prettyNum(round(R.mean[j,k],3), nsmall=3))

            if (!contains.zero[j,k]) {

                cat("}")
            }

        } else {
        }

        cat(" & ")
    }
    cat("\n")
}
