
# A Basic Example

We’ll start with a simple example. Here is the adjacency matrix
representing a patient demand system.

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Market Detection

## Firm-Centered Markets

We begin by constructing the unipartite (firm-firm) matrix.

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The diagonal of this matrix is simply the total volume. The off-diagonal
elements tell us, for a given hospital (row), the total number of its
patients who are drawn from similar geographies as the paired hospital
(column).

Take the first row, for example. Cell \[1,1\] tells us Hospital A has a
total volume of 23 patients. Cell \[1,2\] tells us that all 23 of
Hospital A’s patients are drawn from geographies from which Hospital B
also draws its patients. By comparison, only 12 of Hospital A’s patients
are drawn from common geographies as Hospital C.

We can verify all this by looking at the bipartite adjacency matrix.
Hospital A draws 11 patients from GEO_1 and 12 from GEO_2. Hospital C’s
catchment area includes GEO_2, GEO_3, GEO_4, GEO_5, and GEO_6. There is
one area of overlap (GEO_2) and from Hospital A’s perspective 12
patients are drawn from this area. Alternatively, when pairing Hospital
C and Hospital A (cell \[3,1\]) we see that 81 of Hospital C’s 481 total
patients are drawn from common geographies with Hospital A.

We can also normalize the matrix:

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Application

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

| Hospital System                          | Frequency on Frontier |
|:-----------------------------------------|:----------------------|
| HCA Healthcare                           | 44.1%                 |
| Ascension Healthcare                     | 38.2%                 |
| Methodist Le Bonheur Healthcare          | 38.2%                 |
| Covenant Health                          | 29.4%                 |
| Baptist Memorial Health Care Corporation | 14.7%                 |
| Vanderbilt Health                        | 11.8%                 |
| Ballad Health                            | 8.8%                  |
| West Tennessee Healthcare                | 5.9%                  |
| University of Tennessee Medical Center   | 2.9%                  |
