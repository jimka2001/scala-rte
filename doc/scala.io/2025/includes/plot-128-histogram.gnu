
set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set style histogram clustered gap 5 title textcolor lt -1
set style data histograms
set key outside top center horizontal
set xtics rotate by -45
set grid
set xlabel "DFA state count"
set ylabel "Percentage of DFAs per state count"
set title "DFA State distribution for Rte 128-"



$MyData << EOD
"DFA state count" "naive-mid samples=405" "naive-edge samples=407" "balanced samples=379"
"1" 62.716 46.683 54.881 
"2" 6.914 4.668 5.541 
"3" 3.210 11.057 4.749 
"4" 2.469 5.160 4.222 
"5" 1.975 7.125 2.111 
"6" 3.210 2.457 2.111 
"7" 0.988 1.966 1.583 
"8" 0.988 2.457 0.792 
"9" 1.481 1.966 1.055 
">= 10" 16.049 16.462 22.955 
EOD

plot $MyData using 2:xtic(1) ti col, \
     $MyData using 3 ti col, \
     $MyData using 4 ti col,