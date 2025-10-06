
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
"DFA state count" "naive-mid samples=995" "naive-edge samples=978" "balanced samples=968"
"1" 54.874 42.025 50.310 
"2" 6.332 4.806 4.752 
"3" 5.628 15.440 9.607 
"4" 2.412 7.055 4.442 
"5" 2.513 5.317 4.132 
"6" 1.709 3.988 2.066 
"7" 3.618 3.476 2.479 
"8" 1.910 1.840 2.066 
"9" 2.412 1.636 1.963 
">= 10" 18.593 14.417 18.182 
EOD

plot $MyData using 2:xtic(1) ti col, \
     $MyData using 3 ti col, \
     $MyData using 4 ti col,