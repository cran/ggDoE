## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## -----------------------------------------------------------------------------
ggDoE::adapted_epitaxial

## -----------------------------------------------------------------------------
ggDoE::original_epitaxial

## -----------------------------------------------------------------------------
ggDoE::aliased_design

## -----------------------------------------------------------------------------
library(ggDoE)

## -----------------------------------------------------------------------------
alias_matrix(design=aliased_design)

## -----------------------------------------------------------------------------
alias_matrix(design=aliased_design, showplot=FALSE)

## -----------------------------------------------------------------------------
model <- lm(s2 ~ (A+B+C+D),data = adapted_epitaxial)
boxcox_transform(model,lambda = seq(-5,5,0.2))

## -----------------------------------------------------------------------------
boxcox_transform(model,lambda = seq(-5,5,0.2),
                 showplot = FALSE)

## -----------------------------------------------------------------------------
model <-  lm(s2 ~ (A+B+C)^2,data=original_epitaxial)
lambda_plot(model)

## -----------------------------------------------------------------------------
lambda_plot(model, showplot=FALSE)

## -----------------------------------------------------------------------------
data <- ToothGrowth
data$dose <- factor(data$dose,levels = c(0.5, 1, 2),
                    labels = c("D0.5", "D1", "D2"))

## -----------------------------------------------------------------------------
head(data)

## -----------------------------------------------------------------------------
gg_boxplots(data,response = 'len',
            factor = 'dose')

## ----fig.height=5,fig.width=7-------------------------------------------------
gg_boxplots(data,response = 'len',
            factor = 'dose',
            group_var = 'supp',
            color_palette = 'viridis',
            jitter_points = TRUE)

## ----fig.height=6,fig.width=8-------------------------------------------------
model <- lm(mpg ~ wt + am + gear + vs * cyl, data = mtcars)

## ----fig.height=8,fig.width=9-------------------------------------------------
diagnostic_plots(model, which_plots=1:6)

## ----fig.height=6,fig.width=9-------------------------------------------------
diagnostic_plots(model, which_plots=c(1,5),
                 standard_errors = TRUE)

## -----------------------------------------------------------------------------
m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
half_normal(m1)

## -----------------------------------------------------------------------------
half_normal(m1,method='Daniel',alpha=0.1,
            ref_line=TRUE,label_active=TRUE,
            margin_errors=TRUE)

## -----------------------------------------------------------------------------
half_normal(m1,method='Daniel',alpha=0.1,
            showplot=FALSE)

## ----fig.width=8,fig.height=6-------------------------------------------------
interaction_effects(adapted_epitaxial,response = 'ybar',
                    exclude_vars = c('s2','lns2'))

## ----fig.width=9,fig.height=4-------------------------------------------------
interaction_effects(adapted_epitaxial,response = 'ybar',
                    exclude_vars = c('A','s2','lns2'),
                    n_columns=3)

## -----------------------------------------------------------------------------
main_effects(original_epitaxial,
             response='s2',
             exclude_vars = c('ybar','lns2'))

## -----------------------------------------------------------------------------
main_effects(original_epitaxial,
             response='s2',
             exclude_vars = c('A','ybar','lns2'),
             color_palette = 'viridis',
             n_columns=3)

## -----------------------------------------------------------------------------
heli.rsm <- rsm::rsm(ave ~ SO(x1, x2, x3, x4), 
                     data = rsm::heli)

## ----fig.height=8,fig.width=10------------------------------------------------
gg_rsm(heli.rsm,form = ~x1+x2+x3,
       at = rsm::xs(heli.rsm),
       n_columns=2)

## ----fig.height=8,fig.width=10------------------------------------------------
gg_rsm(heli.rsm,form = ~x1+x2+x3,
       at = rsm::xs(heli.rsm),
       filled = TRUE,
       n_columns=2)

## -----------------------------------------------------------------------------
m1 <- lm(lns2 ~ (A+B+C+D)^4,data=original_epitaxial)
pareto_plot(m1)

## -----------------------------------------------------------------------------
pareto_plot(m1,method='Daniel',alpha=0.1)

## -----------------------------------------------------------------------------
pareto_plot(m1,method='Daniel',
            alpha=0.1, showplot=FALSE)

## ----fig.height=6,fig.width=8-------------------------------------------------
set.seed(10)
random_LHS <- lhs::randomLHS(n=15, k=4)
twoD_projections(random_LHS,n_columns=3,grid = TRUE)

## ----fig.height=6,fig.width=8-------------------------------------------------
maximin_LHS <- lhs::maximinLHS(n=15, k=4)
twoD_projections(maximin_LHS,n_columns=3,point_color = 'red')

