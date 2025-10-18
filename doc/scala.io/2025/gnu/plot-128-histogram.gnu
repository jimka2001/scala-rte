
set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set style histogram clustered gap 5 title textcolor lt -1
set style data histograms
set key outside top center horizontal
set xtics rotate by -45
set grid
set xlabel "DFA state count"
set ylabel "Percentage of DFAs per state count"
set title "DFA State distribution for Rte 128-" font ",10"



$MyData << EOD
"DFA state count" "tree-split-linear samples=144" "tree-split-gauss samples=128" "tree-split-inv-gauss samples=135" "flajolet samples=122" "comb samples=130"
"1" 45.139 50.781 42.222 54.098 25.385 
"2" 6.944 7.813 2.222 4.918 1.538 
"3" 14.583 3.906 11.852 4.098 34.615 
"4" 4.167 0.781 5.926 4.098 14.615 
"5" 4.167 8.889 3.279 10.769 
"6" 1.389 0.781 2.963 0.820 3.077 
"7" 2.083 0.781 4.444 2.459 2.308 
"8" 0.694 1.563 1.481 0.820 1.538 
"9" 0.694 1.563 0.741 1.538 
">= 10" 20.139 32.031 19.259 25.410 4.615 
EOD

plot $MyData using 2:xtic(1) ti col, \
     $MyData using 3 ti col, \
     $MyData using 4 ti col, \
     $MyData using 5 ti col, \
     $MyData using 6 ti col,