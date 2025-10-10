
set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set style histogram clustered gap 5 title textcolor lt -1
set style data histograms
set key outside top center horizontal
set xtics rotate by -45
set grid
set xlabel "DFA state count"
set ylabel "Percentage of DFAs per state count"
set title "DFA State distribution for Rte 64-"



$MyData << EOD
"DFA state count" "tree-split-linear samples=493" "tree-split-mid samples=483" "tree-split-edge samples=484" "flajolet samples=483" "comb samples=480"
"1" 38.540 46.584 37.603 42.236 33.542 
"2" 6.491 9.524 4.545 6.004 4.167 
"3" 14.807 5.383 19.628 12.008 25.417 
"4" 5.071 2.070 5.579 2.899 10.833 
"5" 4.462 4.762 6.818 4.348 9.375 
"6" 5.274 2.692 4.132 3.934 6.042 
"7" 2.637 1.863 2.066 2.070 2.708 
"8" 2.434 2.692 2.273 2.484 2.083 
"9" 2.028 2.070 2.066 1.449 1.875 
">= 10" 18.256 22.360 15.289 22.567 3.958 
EOD

plot $MyData using 2:xtic(1) ti col, \
     $MyData using 3 ti col, \
     $MyData using 4 ti col, \
     $MyData using 5 ti col, \
     $MyData using 6 ti col,