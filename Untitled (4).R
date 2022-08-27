
# Simlulating the data

# setting the seed
set.seed(35097)

# Sample size
N <- 800

# Exposure variables
age <- as.numeric(sample(30:90, N, replace = T))
sex <- sample(c("M","Males","F","Females","female","males"),N,replace = T)

hpvs <- replicate(10,rbinom(N,2,0.5))
colnames(hpvs) <- paste0("hpv_",c(1:10))

hpv_numb <- as.factor(rowSums(hpvs>0))

# Linear combination

xb <- -5+0.05*age + 2*as.numeric(hpv_numb %in% c("6","7","8","9","10"))

# probabilities
pr <- boot::inv.logit(xb)
plot(xb,pr)

# Outcome
opmd <- rbinom(N,1,pr)
table(opmd)

# Data frame
data <- data.frame(age=age,sex=sex,hpv_numb=hpv_numb,opmd=opmd)
data <- cbind(data,hpvs)

# Overview
skimr::skim(data)

# Classes
sapply(data[1:14], class)

# Data manipulation using dplyr
dat <- data %>% 
        mutate(sex_2=as.factor(case_when(sex %in% c("M","Males","males") ~"Male",
                              sex %in% c("F","Females","female")~"Female")),
               hpv_numb = as.numeric(as.character(hpv_numb)),
               age_cat = cut(age, breaks = quantile(age, probs=c(0,0.25,0.50,0.75,1)), include.lowest = TRUE))


# Check the manipulation
table(dat$sex, dat$sex_2, exclude = NULL)
class(hpv_numb)
table(dat$age, dat$age_cat, exclude = NULL)

# Subsetting based on regexp
hs <- dat %>% select(matches("hpv_[1-9]")) 

hs <- hs %>% mutate_all(.funs = function(x){as.numeric(x>0)}) %>%
        mutate(hpv_numb = rowSums(hs))

g1 <- glm(opmd~hpv_numb+age_cat+sex_2, data=dat, 
          family=binomial(link="logit"))

summary(g1)

cfs <- exp(coef(g1))
cis <- exp(confint.default(g1))

data.frame(vars=factor(rownames(cis)),OR=cfs, lower=cis[,1], upper=cis[,2]) %>%
  ggplot(aes(x=vars,y=OR, label=round(OR,2), color=vars))+
  geom_point()+
  geom_text(nudge_x = 0.5)+
  geom_errorbar(aes(ymin=lower,ymax=upper), width=0.3)+
  coord_trans(y="log")+
  theme_minimal()+
  labs(y="Odds ratios (95%CI) \n log scale", x="", 
       title="Associations")+
  theme(legend.position ="none")


