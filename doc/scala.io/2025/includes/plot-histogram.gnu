
set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set style histogram clustered gap 5 title textcolor lt -1
set style data histograms
set key outside top center horizontal
set xtics rotate by -45
set grid
set xlabel "DFA state count"
set ylabel "Percentage of DFAs per state count"
set title "DFA State distribution for Rte"



$MyData << EOD
"DFA state count" "naive-mid samples=845" "naive-edge samples=849" "balanced samples=844"
"1" 56.331 46.172 47.512 
"2" 6.154 3.887 4.976 
"3" 5.207 13.663 9.597 
"4" 4.142 8.716 5.569 
"5" 3.314 5.536 4.739 
"6" 2.604 3.062 3.199 
"7" 1.538 3.534 2.725 
"8" 0.710 1.885 2.488 
"9" 1.183 1.413 1.659 
">= 10" 18.817 12.132 17.536 
EOD

plot $MyData using 2:xtic(1) ti col, \
     $MyData using 3 ti col, \
     $MyData using 4 ti col,