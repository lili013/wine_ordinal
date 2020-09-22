Ordinal Classification of White Wines
================
Émilie Hamel
2020-09-21






-   [Introduction](#introduction)
-   [Dataset](#dataset)
-   [Methods](#methods)
-   [Results](#results)
-   [References](#references)

Introduction
============

This report explores different techniques to classify white wines as
either low (1), medium (2) or superior (3) quality. The particularity of
this problem is the natural order of the target variable. Moreover, this
is an interesting problem because even science cannot decide on the
chemical profile of great wine (Hodgson [2009](#ref-hodgson_2009)).
Let’s see if machine learning models can make sense out of them!

Dataset
=======

The dataset was downloaded from [UCI
Repository](https://archive.ics.uci.edu/ml/datasets/wine+quality). This
data was originally used in a paper by Cortez et al., 2009)(Cortez et
al. [2009](#ref-CORTEZ2009547)). This project only focuses on white
wines “Vinho Verde” from Portugal. The dataset has 4898 observations. It
has 11 numeric independent variables representing different
physicochemical properties. The target variable is wine quality. The
classes ranges from 3 to 9. Since the classes are unbalanced, we
simplify our task by merging the classes 3 to 5 together (low quality),
6 was keep by itself (medium quality) and from 7 to 9 together (high
quality). Moreover, the 1000 observations were randomly sampled from
each 3 classes.

Methods
=======

The paper by Gutiérrez et. al.(Gutiérrez et al.
[2015](#ref-Gutierrez_2015)) was used as an inspiration for the types of
method used to predict an ordinal variable. This article reviewed many
methods and proposed a taxonomy of approches to deal with ordinal target
variable:

-   Naïve approaches
-   Ordinal binary decomposition approaches
-   Threshold models

In this project I tried the following models.

| Types of Methods                        | Models Used                                |
|-----------------------------------------|--------------------------------------------|
| Naïve approaches                        | Multinominal regression, random forest     |
| Ordinal binary decomposition approaches | Logistic regression 1-on-1                 |
| Threshold models                        | polr (stepwise), Lasso, Elastic net, Ridge |

Results
=======

    load("Data_final.RData")
    knitr::kable(resultats)

|                | NbVariable | Precision | PrecisionClasse1 | PrecisionClasse2 | PrecisionClasse3 |
|:---------------|:-----------|:----------|:-----------------|:-----------------|:-----------------|
| multinomSimple | 13         | 0.56      | 0.72             | 0.56             | 0.71             |
| polrSimple     | -1         | 0.58      | 0.74             | 0.58             | 0.73             |
| lasso          | 7          | 0.56      | 0.72             | 0.55             | 0.71             |
| elasticNet     | 9          | 0.56      | 0.73             | 0.56             | 0.71             |
| ridge          | 9          | 0.56      | 0.72             | 0.56             | 0.71             |
| rf1            | NA         | 0.6       | 0.78             | 0.59             | 0.72             |
| rf2            | NA         | 0.58      | 0.75             | 0.6              | 0.71             |
| 1on1RL         | NA         | 0.44      | NA               | NA               | NA               |

The table above shows the results obtained from the 8 ajusted models.
The best model is rf2 with a global precision of 60%. The parameters of
the model are: mtry = 1, ntree = 800 et maxnodes = 30.

References
==========

<div id="refs" class="references hanging-indent">

<div id="ref-CORTEZ2009547">

Cortez, Paulo, António Cerdeira, Fernando Almeida, Telmo Matos, and José
Reis. 2009. “Modeling Wine Preferences by Data Mining from
Physicochemical Properties.” *Decision Support Systems* 47 (4): 547–53.
<https://doi.org/https://doi.org/10.1016/j.dss.2009.05.016>.

</div>

<div id="ref-Gutierrez_2015">

Gutiérrez, Pedro Antonio, María Pérez-Ortiz, Javier Sánchez-Monedero,
Francisco Fernandez-Navarro, and Cesar Martínez. 2015. “Ordinal
Regression Methods: Survey and Experimental Study.” *IEEE Transactions
on Knowledge and Data Engineering* 28 (July).
<https://doi.org/10.1109/TKDE.2015.2457911>.

</div>

<div id="ref-hodgson_2009">

Hodgson, Robert T. 2009. “How Expert Are ‘Expert’ Wine Judges?” *Journal
of Wine Economics* 4 (2): 233–41.
<https://doi.org/10.1017/S1931436100000821>.

</div>

</div>
