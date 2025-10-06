
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
"DFA state count" "naive-mid samples=271" "naive-edge samples=279" "balanced samples=272"
"1" 63.838 50.179 54.412 
"2" 6.273 5.735 5.882 
"3" 2.952 9.677 5.147 
"4" 1.845 3.584 4.779 
"5" 1.845 5.376 2.206 
"6" 2.583 2.509 2.574 
"7" 1.107 2.151 2.206 
"8" 1.107 2.509 0.368 
"9" 2.214 2.151 1.471 
">= 10" 16.236 16.129 20.956 
EOD

plot $MyData using 2:xtic(1) ti col, \
     $MyData using 3 ti col, \
     $MyData using 4 ti col,