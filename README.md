bossMaps
=====

## Description
Package introduced with _Integrating occurrence data and expert maps for improved species range predictions_ by Cory Merow, Adam Wilson, and Walter Jetz [http://onlinelibrary.wiley.com/doi/10.1111/geb.12539/abstract](http://onlinelibrary.wiley.com/doi/10.1111/geb.12539/abstract)

## Paper Abstract
Knowledge of species' geographic distributions is critical for many ecological and evolutionary questions and underpins effective conservation decision-making, yet usually is limited in spatial resolution or reliability. Over large spatial extents, range predictions are typically derived from expert knowledge or, increasingly, species distribution models based on individual occurrence records. Expert maps are useful at coarse resolution where they are suitable for delineating unoccupied regions. In contrast, point records typically provide finer-scale occurrence information that can be characterized for its environmental association, but usually suffers from observer biases and does not address the geographic or environmental range occupied by a species representatively or fully. 

We develop a new modeling methodology to combine the complementary informative attributes of both data types to enable improved fine-scale, large extent predictions. Specifically, we use expert delineations to constrain predictions of a species distribution model parameterized with incidental point records. We introduce a maximum entropy approach for combining the two data types and generalize it to Poisson point process models.  We illustrate critical decision making during model construction using a detailed case study of the Montaine Woodcreeper (_Lepidocolaptes lacrymiger_) across South America and illustrate features more generally with applications to species with vastly different range and data attributes.

The presented modeling strategy flexibly accommodates expert maps with different levels of bias and precision. The approach can also be useful with other coarse sources of spatially explicit information, including habitat associations, elevational bands, or vegetation types. The flexible nature of this methodological innovation is likely able to support improved characterization of species distributions for a variety of applications and is being implemented as a standard element underpinning integrative species distribution predictions in Map of Life.

## QuickStart guide

```
library(bossMaps)
?rangeOffset
```
Run the example in the help file. 