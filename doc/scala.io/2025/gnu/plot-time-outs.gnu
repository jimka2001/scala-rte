# 
set xlabel "RTE leaf count" font ",10"
set ylabel "Frequency" font ",10"
set grid
set key font ',10'
set xtics font ',10'
set ytics font ',10'
set style line 1 pt 7 ps 2.0
set style line 2 pt 7 ps 2.0
set style line 3 pt 7 ps 2.0
set style line 4 pt 7 ps 2.0
set style line 5 pt 7 ps 2.0
set key horizontal bmargin
set title "Time Outs" font ",12"
plot "-" using 1:2 with points ls 1 title "tree-split-linear - 894 samples",\
    "-" using 1:2 with points ls 2 title "tree-split-gauss - 870 samples",\
    "-" using 1:2 with points ls 3 title "tree-split-inv-gauss - 869 samples",\
    "-" using 1:2 with points ls 4 title "flajolet - 859 samples",\
    "-" using 1:2 with points ls 5 title "comb - 851 samples"
#tree-split-linear - 894 samples
64.000 8.000
67.000 1.000
81.000 1.000
112.000 1.000
121.000 1.000
128.000 12.000
end
#tree-split-gauss - 870 samples
64.000 7.000
102.000 1.000
106.000 1.000
108.000 1.000
116.000 1.000
126.000 1.000
128.000 13.000
end
#tree-split-inv-gauss - 869 samples
64.000 6.000
122.000 1.000
128.000 3.000
end
#flajolet - 859 samples
64.000 6.000
81.000 1.000
101.000 1.000
103.000 1.000
112.000 1.000
128.000 8.000
end
#comb - 851 samples
81.000 1.000
end
