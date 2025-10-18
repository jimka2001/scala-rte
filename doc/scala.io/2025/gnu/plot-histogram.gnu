
set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set style histogram clustered gap 5 title textcolor lt -1
set style data histograms
set key outside top center horizontal
set xtics rotate by -45
set grid
set xlabel "DFA state count"
set ylabel "Percentage of DFAs per state count"
set title "DFA State distribution for Rte " font ",10"



$MyData << EOD
"DFA state count" "tree-split-linear samples=233" "tree-split-gauss samples=234" "tree-split-inv-gauss samples=240" "flajolet samples=236" "comb samples=240"
"1" 45.064 47.863 38.333 46.186 26.667 
"2" 4.292 7.265 3.750 5.508 3.333 
"3" 14.163 8.547 20.000 9.746 31.667 
"4" 5.150 1.709 6.667 3.390 13.333 
"5" 3.004 2.137 3.750 2.119 8.750 
"6" 2.146 1.709 5.000 2.119 4.167 
"7" 4.721 2.137 2.917 2.119 2.917 
"8" 1.717 1.709 0.417 0.847 1.250 
"9" 3.004 2.137 2.083 1.695 1.250 
">= 10" 16.738 24.786 17.083 26.271 6.667 
EOD

plot $MyData using 2:xtic(1) ti col, \
     $MyData using 3 ti col, \
     $MyData using 4 ti col, \
     $MyData using 5 ti col, \
     $MyData using 6 ti col,