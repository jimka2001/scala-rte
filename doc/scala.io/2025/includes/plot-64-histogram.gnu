
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
"DFA state count" "naive-mid samples=1331" "naive-edge samples=1318" "balanced samples=1302"
"1" 54.621 43.323 49.693 
"2" 6.987 4.249 4.992 
"3" 5.485 14.795 9.370 
"4" 2.554 6.904 4.147 
"5" 2.479 5.463 4.071 
"6" 2.254 4.097 2.074 
"7" 3.306 2.959 2.611 
"8" 1.578 2.200 1.920 
"9" 2.179 1.745 1.843 
">= 10" 18.557 14.264 19.278 
EOD

plot $MyData using 2:xtic(1) ti col, \
     $MyData using 3 ti col, \
     $MyData using 4 ti col,