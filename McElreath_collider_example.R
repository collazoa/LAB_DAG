#Example from McElreath: Statistical Rethinking, p. 162 

set.seed(1914)
N <- 200 # num grant proposals
p <- 0.1 # proportion to select
# uncorrelated newsworthiness and trustworthiness
nw <- rnorm(N)
tw <- rnorm(N)
cor(tw, nw)
# select top 10% of combined scores
s <- nw + tw # total score
q <- quantile( s , 1-p ) # top 10% threshold
selected <- ifelse( s >= q , TRUE , FALSE )
cor( tw[selected] , nw[selected] )
# high correlation among the selected (in our case censored animals)




notselected<-ifelse(s< q, TRUE, FALSE)

cor(tw[notselected], nw[notselected])

# smaller correlation in the set of uncensored animals for our case 

# effect of collider bias is strong for group which has high level of nw and tw 
# in the stroke example: high baseline infarct volume and treatment (if treatment is detrimental to well-being)
# however these animals are excluded 
# among the uncensored animals, the effect is not as strong 



