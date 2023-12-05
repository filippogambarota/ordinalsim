## Code

- mancano funzioni per plottare nel caso di probit

## Simulations

- odds ratio and cohen's d
- numerical predictor
- categorical predictor
- interaction

# Notes

## @Liddell2018-wu

> The ubiquity of ordinal data is due in large part to the widespread use of Likert-style response items (Likert, 1932).

> Metric methods assume that the data are on an interval or ratio scale (Stevens, 1946, 1955). Interval scales define distances between points (not only ordering), and ratio scales furthermore specify a zero point so that ratios of magnitudes can be defined.

- citare la ricerca bibliografica che hanno fatto dove praticamente tutti che usano la likert trattano il modello come metrico
- possiamo citare che anche @Liddell2018-wu usa un modello cumulative, però qui modellano le varianze ed il modello in generale è più complesso

> thresholded cumulative normal distribution is used by traditional “or- dered-probit” models (e.g., Becker & Kennedy, 1992). The key differ- ence between the metric-scale and ordinal-scale models is that the metric model describes a datum's probability as the normal probability density at a corresponding metric value, whereas the ordinal model describes a datum's probability as the cumulative normal probability between two thresholds on an underlying latent continuum.

>But real ordinal data (if assumed to fall on a metric scale) are often strongly skewed, heavy- tailed or thin-tailed, or multi-modal. Thus, treating ordinal data as if they were normally-distributed equally-separated metric values is not appropriate. But does the practice actually lead to problems? Or, is the practice innocuous and desirable for its simplicity?

- magari vedere se c'è un esempio di simulazione dove una distribuzione skewed di dati ordinali analizzati in modo metrico fa sbagliare di molto 


> Despite the lack of correspondence between metric-model SD and ordered-probit-model SD, it is true that if the theoretical metric-model σ’s (of ordinal values generated from the ordered-probit model) are equal, then the metric-model means are monotonically related to the underlying ordered-probit means, but the metric-model effect size is less than the ordered-probit model effect size. (A detailed example is provided by Fig. 14 in the Appendix.) Because real data have finite samples, the ordered-probit model is more powerful at detecting dif- ferences of means than the metric model when the metric σ’s are equal.

## @Christensen2018-ig

una piccola overview della nomenclatura potrebbe essere utile e anche i pacchetti principali per fare le analisi

> The name, cumulative link models is adopted from Agresti (2002), but the model class has been referred to by several other names in the literature, such as ordered logit models and ordered probit models (Greene and Hensher 2010) for the logit and probit link functions. The cumulative link model with a logit link is widely known as the proportional odds model due to McCullagh (1980) and with a complementary log-log link, the model is sometimes referred to as the proportional hazards model for grouped survival times. CLMs is one of several types of models specifically developed for ordinal data. Alternatives to CLMs include continuation ratio models, adjacent category models, and stereotype models (Ananth and Kleinbaum 1997) but only models in the CLM framework will be considered in this paper.

# TODO

- capire le varie thresholds, equidistant, flexible etc.
- capire come simulare le th, forse come krusche o in modo standardizzato  
- per simulare con una variabile latente https://stats.stackexchange.com/questions/374413/how-to-simulate-likert-scale-data-in-r