# Title     : LDAfunction.R
# Author    : wujie
# Time      : 2021/4/4


LDAfunction = function (theta, mu_list, pi_list, x){
  lda_male = x %*% theta %*% mu_list$mu_male - 0.5 * mu_list$mu_male %*% theta %*% mu_list$mu_male + log(pi_list$male_pi)
  lda_female = x %*% theta %*% mu_list$mu_female - 0.5 * mu_list$mu_female %*% theta %*% mu_list$mu_female + log(pi_list$female_pi)
  if(lda_female > lda_male) return ("female")
  else return ("male")
}

