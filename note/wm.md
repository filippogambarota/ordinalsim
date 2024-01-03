For testing independence in contingency tables with ordinal variables, Section 2.5 showed
that ordinal test statistics are usually more appropriate and provide greater power than ordinary chi-squared tests that treat the variables as qualitative (nominal-scale). Likewise, cumulative logit models, which utilize the ordinality of Y , usually have a power advantage over
baseline-category logit models, which treat Y as nominal-scale.

agresti intro to categorical

For example, if a continuous variable measuring political ideology satisfies a linear
model with some explanatory variables, then the same effect parameters apply to a discrete version of political ideology with the categories (liberal, moderate, conservative) or
(very liberal, slightly liberal, moderate, slightly conservative, very conservative). Therefore, two researchers who use different response categories in studying a predictor’s effect
should obtain similar {βˆj}, apart from sampling error. This nice feature of the model
makes it possible to compare estimates from studies using different scales for the response
variable


mettere che il tutorial non si occupa di model checking, residuals, etc. e mettere qualche riferimento per questo

vedere questa order restriction che le probabilità cumulate devono essere ordinate (se fitto il non proportional odds può succedere di no, anche tutz lo diceva credo)

log(odds ratio): 
	- positive = higher numerator
	- negative = higher denominator
	

forse quando faccio P(Y = 1) devo condizionare a x (grassetto) P(Y = 1|x)
vedere se nell'equazione devo fare il trasposto di qualche cosa X o \beta

vedi agresti 2012 categorical 8.2.3 per equazione della variabile latente





> The name, cumulative link models is adopted from Agresti (2002), but the model class has
been referred to by several other names in the literature, such as ordered logit models and
ordered probit models (Greene and Hensher 2010) for the logit and probit link functions. The
cumulative link model with a logit link is widely known as the proportional odds model due
to McCullagh (1980) and with a complementary log-log link, the model is sometimes referred
to as the proportional hazards model for grouped survival times.