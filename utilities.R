library(latex2exp)
all_combos <- expand.grid(c(0,1,2,3,4),c(0,1,2,3,4))
colnames(all_combos) = c("ns", "ys")

reset_summaries <- function(){
  empty_trials = all_combos
  empty_trials$cnt = seq(0,0,len(all_combos))
  return(get_summaries(empty_trials))
}

get_trials <- function(m, p) {
  ns <- sample(c(1,2,3,4), m, replace = TRUE)
  ys <- apply(matrix(ns, ncol=1), 1, rbinom, prob = p, n=1)
  df <- data.frame(ns = ns, ys = ys, cnt = 1)
  out_df <- merge(df, all_combos, all.y = TRUE)
  out_df$cnt[is.na(out_df$cnt)] <- 0
  return(out_df)
}

get_summaries <- function(df) {
  cnts = aggregate(df$cnt, list(n = df$ns, y = df$ys), sum)
  y1 <- cnts[cnts$n == 1, 3]
  y2 <- cnts[cnts$n == 2, 3]
  y3 <- cnts[cnts$n == 3, 3]
  y4 <- cnts[cnts$n == 4, 3]
  y_counts = y1 + y2+ y3 + y4
  n_counts = c(sum(y1), sum(y2), sum(y3), sum(y4))
  return(list(y_counts=y_counts,
              n_counts=n_counts,
              y1 = y1,
              y2 = y2,
              y3 = y3,
              y4 = y4))
}

update_counts <- function(old_cnts, size, p) {
  out = old_cnts
  new_cnts = get_summaries(get_trials(size, p))
  out$y1 = new_cnts$y1 + out$y1
  out$y2 = new_cnts$y2 + out$y2
  out$y3 = new_cnts$y3 + out$y3
  out$y4 = new_cnts$y4 + out$y4
  out$y_counts = new_cnts$y_counts + out$y_counts
  out$n_counts = new_cnts$n_counts + out$n_counts
  return(out)
}


#
#
### Plotting functions
#
#


num_labels = c("0","1", "2", "3", "4")

cond_exp_labels <- function(p){
  return(p*c(1,2,3,4))
}

plot_summaries <- function(p, s){
  layout(rbind(c(1,1,1,1,3),
               c(1,1,1,1,4),
               c(2,2,7,7,5),
               c(2,2,7,7,6)))
  barplot(s$y_counts, names.arg = num_labels,
          main="Combined distribution of Y",
          sub=paste("E(Y) \u2248 ", round(sum(c(0,1,2,3,4)*s$y_counts)/sum(s$y_counts), 3),
                    "(num. of trials =", sum(s$y_counts), ")"),
          xlab = "Y")
  barplot(s$n_counts, names.arg = cond_exp_labels(p),
          main='Distribution of E(Y | N)',
          sub=paste("E[E(Y|N)] \u2248 ", round(sum(p*c(1,2,3,4)*s$n_counts)/sum(s$n_counts), 3),
          "(num. of trials =", sum(s$y_counts),")"),
          xlab='E(Y | N)')
  barplot(s$y1, names.arg = num_labels,
          main='Y|N=1',
          xlab=paste('E(Y|N) = np\n = ',1,'*',p,'=',1*p))
  barplot(s$y2, names.arg = num_labels,
          main='Y|N=2',
          xlab=paste('E(Y|N) = np\n = ',2,'*',p,'=',2*p))
  barplot(s$y3, names.arg = num_labels,
          main='Y|N=3',
          xlab=paste('E(Y|N) = np\n = ',3,'*',p,'=',3*p))
  barplot(s$y4, names.arg = num_labels,
          main='Y|N=4',
          xlab=paste('E(Y|N) = np\n = ',4,'*',p,'=',4*p))
  barplot(s$n_counts, names.arg = cond_exp_labels(1),
          main="Combined distribution of N",
          xlab = "N")
}


