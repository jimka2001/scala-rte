
set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set style histogram clustered gap 5 title textcolor lt -1
set style data histograms
set key outside top center horizontal
set xtics rotate by -45
set grid
set xlabel "DFA state count"
set ylabel "Percentage of DFAs per state count"
set title "DFA State distribution for Rte 50-"



$MyData << EOD
"DFA state count" "naive-mid samples=896" "naive-edge samples=902" "balanced samples=889"
"1" 54.688 47.007 46.232 
"2" 5.804 3.880 6.074 
"3" 4.018 14.191 8.774 
"4" 4.241 6.098 4.162 
"5" 3.571 6.098 2.925 
"6" 3.795 3.326 2.812 
"7" 2.455 3.326 3.150 
"8" 0.781 1.774 1.350 
"9" 1.897 1.552 1.687 
">= 10" 18.750 12.749 22.835 
EOD

plot $MyData using 2:xtic(1) ti col, \
     $MyData using 3 ti col, \
     $MyData using 4 ti col,