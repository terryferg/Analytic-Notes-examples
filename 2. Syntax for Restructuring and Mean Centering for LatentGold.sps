***Remove all attributes from the taste file except for RespID, Product Variable, and Overall LIking - SaveAs something else.

***Restructure the data so that each respondent has 1 row and each column is the liking score for each product.
***eg. Panelst   Product1   Product2   Product3   Product4   Product5
            101             7              8              9              4              5
            102             6              9              4              3              7.

SORT CASES BY RespID Wine. 
CASESTOVARS 
  /ID=RespID 
  /INDEX=Wine 
  /GROUPBY=VARIABLE.

***Calculate an average for both red and white wines.

COMPUTE RedAverage=(T1.111+T1.291+T1.617+T1.806+T1.843+T1.990)/6. 
EXECUTE. 

COMPUTE WhiteAverage=(T1.247+T1.377+T1.439+T1.500+T1.638+T1.885)/6. 
EXECUTE.


***Mean center all respondents by subtracting the person's avg from each wine.

COMPUTE p111=T1.111 - RedAverage.
EXECUTE.

COMPUTE p291=T1.291 - RedAverage.
EXECUTE.

COMPUTE p617vg=T1.617 - RedAverage.
EXECUTE.

COMPUTE p806=T1.806 - RedAverage.
EXECUTE.

COMPUTE p843=T1.843 - RedAverage.
EXECUTE.

COMPUTE p990=T1.990 - RedAverage.
EXECUTE.

COMPUTE p247=T1.247 - WhiteAverage.
EXECUTE.

COMPUTE p377=T1.377 - WhiteAverage.
EXECUTE.

COMPUTE p439=T1.439 - WhiteAverage.
EXECUTE.

COMPUTE p500=T1.500 - WhiteAverage.
EXECUTE.

COMPUTE p638=T1.638 - WhiteAverage.
EXECUTE.

COMPUTE p885=T1.885 - WhiteAverage.
EXECUTE.

***Split the files into a red file and a white file. 
***You can import .sav files into LatentGold directly.
