---
output: pdf_document
---
# README for QFRM R Package

### What is QFRM
QFRM is a result of nearly  6 month-long effort by lecturer (Oleg Melnikov) and students of Quantiative Financial Risk Management (QFRM, 2015) course at Rice University. The QFRM R package is a collection of exotic option pricing methods via four key algorithms: Black Scholes (BS), lattice trees (binomial, trinomial, etc) (LT), Monte Carlo Simulation (MC), and finite differencing (FD).

### Why use QFRM?
If you are analyzing portfolio risks or pricing exotic derivatives, QFRM will be of help. The examples and source code can help you advance your own algorithm. In such a case, we would be delighted to have you as a contributor or be credited for any code/algorithm that you found helpful in your projects/research.

### How to use QFRM?
The contributors inclduded many examples that you can evaluate. Likewise, all functions have default values and can be executed without any changedd arguments (for usability learning). Basically, you will need to instatiate <code>Opt</code> object (takes basic option parameters) then instantiate <code>OptPx</code> object (takes common pricing parameters), which is passed into a specific pricing function. The latter may require additional parameters specific to the exotic option at hand. 
A good reference for QFRM is John C. Hull's textbook "Options, futures, and Other Derivatives." Many pricing functions reference examples in the textbook, so you yourself can verify the pricing calculation.

### Future updates
Planned updates to include visualization, vectorization of existing functions, as well as pricing/analysis of portfolios of options. 

### Contact us
If you discover a bug, possible improvement or just have a question, please contact Oleg Melnikov, xisreal@gmail.com. I will then pass the request, if needed, to the particular contributor. Still, each contributor's contact information is in the DESCRIPTION file. Also, each function identifies the developer.


[Markdown help](https://bitbucket.org/tutorials/markdowndemo)