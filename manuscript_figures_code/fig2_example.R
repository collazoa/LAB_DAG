source("./manuscript_figures_code/load_packages.R")
# settings for styling visualization
cols <- RColorBrewer::brewer.pal(6, "Dark2")
theme_set(theme_classic(base_size = 12))
letter <- theme(plot.title = element_text(face = "bold", 
                                          size = 14), 
                axis.title = element_text(face = "bold", 
                                          size = 14), 
                axis.text = element_text(face = "bold", 
                                         size = 12))


#######################################
# function to simulate collider bias
#######################################

sim.dat <- function(N,
                    b_0,
                    b_1,
                    b_2,
                    g_0,
                    g_1,
                    g_2,
                    sd_w,
                    sd_y,
                    cutoff_W) {
  A <- rep(0:1, each = N / 2)
  
  # causal model
  L <- rnorm(n = 2 * N, mean = 25, sd = 5)
  W <- g_0 + g_1 * A + g_2 * L + rnorm(N, 0, sd_w)
  Y <- b_0  + b_1 * A + b_2 * L + rnorm(N, 0, sd_y)
  S <- W >= quantile(W, probs = cutoff_W)
  
  return(data.frame(A, L, Y, W, S))
}


############################################
# creating a illustrative example dataset
# for Fig 2 and Fig 3
############################################

set.seed(290) # reproduces a specific random sample

#########################################################
# d is the complete data set (corresponding to Approach 1)
# d_s is the censored data set (corresponding to Approach 2)
# attrition is moderate (30%), negative side-effects on
# animal welfare are moderate
# total sample size chosen is n = 20


d <- sim.dat(
  N = 20,
  b_0 = 50,
  b_1 = 0 ,
  b_2 = 4,
  g_0 = 0,
  g_1 = -3,
  g_2 = -1,
  sd_w = 2,
  sd_y = 10,
  cutoff_W = 0.3
)


d_s <- d %>% filter(S == 1)

#########################################################


#########################################################
#mutating data to prepare for visualization
d <- d %>%
  mutate(
    cols = factor(d$A, labels = c(cols[1], cols[4])),
    pch = recode(A, `0` = 1, `1` = 17),
    S1 = factor(d$S, labels = c(0, 1)),
    bg = recode(S1, `0` = 0.3, `1` = 1),
    X1 = factor(recode(A, `0` = "Control", `1` = "Drug"))
  )

d_s <- d_s %>%
  mutate(
    cols = factor(d_s$A, labels = c(cols[1], cols[4])),
    pch = recode(A, `0` = 1, `1` = 17),
    X1 = factor(recode(A, `0` = "Control", `1` = "Drug"))
  )

d_summary <- d %>% group_by(A) %>%
  summarize(
    mean_L = mean(L),
    mean_Y = mean(Y),
    sd_L = sd(L),
    sd_Y = sd(Y)
  ) %>%
  mutate(
    cols = c(cols[1], cols[4]),
    xend = c(1.3, 2.3),
    x = c(0.7, 1.7)
  )

d_s_summary <- d_s %>% group_by(A) %>%
  summarize(
    mean_L = mean(L),
    mean_Y = mean(Y),
    sd_L = sd(L),
    sd_Y = sd(Y)
  ) %>%
  mutate(
    cols = c(cols[1], cols[4]),
    xend = c(1.3, 2.3),
    x = c(0.7, 1.7)
  )

#######################################################



######################################################
# visualization for Figure 2


fig2 <- grid.arrange(
  ggplot() +
    geom_quasirandom(
      data = d,
      aes(y = L, x = X1, fill = cols),
      shape = 21,
      color = "darkblue",
      alpha = d$bg,
      size = 5,
      width = 0.3
    ) +
    scale_fill_manual(values = c(cols[1], cols[4]))+
    geom_segment(
      data = d_summary,
      aes(
        x = x,
        y = mean_L,
        yend = mean_L,
        xend = xend, 
      ),
      color = d_summary$cols,
      linewidth = 1,
      alpha = 0.3
    ) +
    geom_segment(
      data = d_s_summary,
      aes(
        x = x,
        y = mean_L,
        yend = mean_L,
        xend = xend, 
      ),
      color = d_summary$cols,
      linewidth = 1,
      alpha = 1
    ) +
    ylab(expression("Initial infarct size (mm" ^ 3 * ")")) +
    xlab("") +
    ylim(10, 40) +
    ggtitle("A") +
    letter+
    theme(legend.position = "none"),
  
  
  ggplot() +
    geom_quasirandom(
      data = d,
      aes(y = Y, x = X1, group = X1, fill = cols),
      shape = 21,
      color = "darkblue",
      alpha = d$bg,
      size = 5,
      width = 0.3
    ) +
    scale_fill_manual(values = c(cols[1], cols[4]))+
    geom_segment(
      data = d_summary,
      aes(
        x = x,
        y = mean_Y,
        yend = mean_Y,
        xend = xend
      ),
      color = d_summary$cols,
      linewidth = 1,
      alpha = 0.3
    ) +
    geom_segment(
      data = d_s_summary,
      aes(
        x = x,
        y = mean_Y,
        yend = mean_Y,
        xend = xend
      ),
      color = d_summary$cols,
      linewidth = 1,
      alpha = 1
    ) +
    ylab(expression("Final infarct size (mm" ^ 3 * ")")) +
    xlab("") +
    ylim(100, 220) +
    ggtitle("B") +
    letter + 
    theme(
      legend.position = "none"
    ),
  
  nrow = 1,
  ncol = 2,
  top = text_grob("Impact of collider bias stratification on initial infarct size and effect estimates", 
                  color = "black", 
                  face = "bold", 
                  size = 20
                  )
)


fig2


annotate_figure(fig2, 
                top = text_grob("Biased estimates through collider bias stratification", 
                                color = "black", 
                                face = "bold", 
                                size = 20, 
                                just = "right"))
###########################################################
