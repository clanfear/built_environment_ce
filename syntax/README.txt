Most of the contents are unambiguous, but three require explanation:

misc: Extra syntax that doesn't fit under other categories. In this case, just some diagnostics of the models.

sensitivity: Sensitivity tests for the Criminology R&R response letter. These may not be not fully reproducible and the numbering is wrong---and there may even be some circular referencing since I ran these interactively. Results vary on each run since I use monte carlo methods. If replicating, run everything a few times. If you do replicate and have issue, email me.

weirdness: This contains some script files examining a bug in piecewiseSEM. Basically, the way psem() tests d-separation is based on estimating the specified model, then re-estimating it with the d-separated variable added. It then compares these two models. If, however, there are convergence issues on the second model, it can produce invalid, overly-precise standard errors. This will produce an invalid rejection of d-separation and does not produce any error messages. There may also be problems with using the formula-based update() with glmer() models that causes a similar issue.