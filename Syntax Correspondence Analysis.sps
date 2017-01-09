***In order to run correspondence analysis, you must have the 'Categories' module in SPSS.

**Weight cases by continuous variable - counts.
WEIGHT BY counts.

***Correspondence Analysis - Make sure that you have your clusters defined as numbers.
***The 'attributes' in this case are concept statements, there are a total of 31.
CORRESPONDENCE TABLE=LabelNumb(1 4) BY Concept(1 13)
  /DIMENSIONS=3
  /MEASURE=CHISQ
  /STANDARDIZE=RCMEAN
  /NORMALIZATION=SYMMETRICAL
  /PRINT=TABLE RPOINTS CPOINTS RPROFILES CPROFILES
  /PLOT=NDIM(1,MAX) BIPLOT(20).

***Output - 
***Go to Overview Row/Column Points
***The results under "Score in Dimension" are your X & Y axis coordinates. 
           ***The rows should be labeled by segment/statement #.
***Copy output to Excel and create a scatter plot.