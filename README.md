Gibrat
======

Gibrat pop growth simulator

### Use

#### Loading data
When loading a dataset (by `Choose a file` or `Load test dataset`), you have to select the **temporal columns**, *i.e.*, the columns who'll be used for the mean growth computation.
Those columns must have a header containing their date, in numeric format (*i.e.* 1954 and not "year1954")

#### Growth
As soon as data are loaded, they're displayed in the `Base Data` tab, and a table containing the mean growth for each period is also computed. This table can be seen in the `Growth` tab, along with it's standard deviation, and a plot representing it's evolution (the polygon around the curve represent +/- 1 standard deviation).

#### Gibrat simulation
The `Simulation` tab then allows to perform some Gibrat simulations, *i.e.* a computation where, starting from first observed populations, we compute randomly, for each period, a growth rate for each city. Those growth rates are based on a normal distribution around the observed mean and standard deviation.
Computation – although optimised to run on each core of the server – can still be long, so, start with a low number of replications (the default 10 for example) and grow according to the time it takes.

<hr>


This website is released under the <a href="LICENSE.txt">**GNU AFFERO GENERAL PUBLIC LICENSE v3**</a>
