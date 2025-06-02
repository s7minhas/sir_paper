# Social Influence Regression

Authors
---
[Shahryar Minhas](http://s7minhas.com/) & [Peter D. Hoff](https://pdhoff.github.io/)

Abstract
---
Understanding network influence and its determinants are key challenges in political science and network analysis. Traditional latent variable models position actors within a social space based on network dependencies but often do not elucidate the underlying factors driving these interactions. To overcome this limitation, we propose the Social Influence Regression (SIR) model, an extension of vector autoregression tailored for relational data that incorporates exogenous covariates into the estimation of influence patterns. The SIR model captures influence dynamics via a pair of $n \times n$ matrices that quantify how the actions of one actor affect the future actions of another. This framework not only provides a statistical mechanism for explaining actor influence based on observable traits but also improves computational efficiency through an iterative block coordinate descent method. We showcase the SIR model's capabilities by applying it to monthly conflict events between countries, using data from the Integrated Crisis Early Warning System (ICEWS). Our findings demonstrate the SIR model's ability to elucidate complex influence patterns within networks by linking them to specific covariates. This paper's main contributions are: (1) introducing a model that explains third-order dependencies through exogenous covariates and (2) offering an efficient estimation approach that scales effectively with large, complex networks.

Replication Instructions
---
Follow instructions in replArchive directory. Any questions should be addressed to [Shahryar Minhas](http://s7minhas.com/).

Publication Outlet
---
Political Analysis (2025)
