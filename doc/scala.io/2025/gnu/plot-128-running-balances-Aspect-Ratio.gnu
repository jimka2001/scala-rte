# 
set xlabel "Aspect-Ratio" font ",15"
set ylabel "Percentage count >= 2 for imbalance <= x" font ",15"
set grid
set key font ',10'
set xtics font ',15'
set ytics font ',15'
set key horizontal bmargin
set title "Running Balances 128- for Aspect-Ratio"
plot "-" using 1:2 with lines title "tree-split-linear 144 samples",\
    "-" using 1:2 with lines title "tree-split-gauss 128 samples",\
    "-" using 1:2 with lines title "tree-split-inv-gauss 135 samples",\
    "-" using 1:2 with lines title "flajolet 122 samples",\
    "-" using 1:2 with lines title "comb 130 samples"
#tree-split-linear 144 samples
2.000 2.083
2.143 4.167
2.286 9.028
2.429 16.667
2.571 25.694
2.714 29.167
2.857 40.278
3.000 41.667
3.143 45.139
3.286 48.611
3.429 50.000
3.571 50.694
3.714 51.389
4.143 52.083
end
#tree-split-gauss 128 samples
1.571 2.344
1.714 7.031
1.857 19.531
2.000 23.438
2.143 40.625
2.286 50.781
2.429 56.250
2.571 57.031
2.714 57.813
2.857 58.594
end
#tree-split-inv-gauss 135 samples
3.000 0.000
3.143 0.741
3.286 2.963
3.429 5.926
3.571 9.630
3.714 13.333
3.857 17.037
4.000 23.704
4.143 25.926
4.286 27.407
4.429 31.111
4.571 34.074
4.714 35.556
4.857 37.778
5.000 39.259
5.143 39.259
5.286 40.000
5.429 42.222
5.571 42.963
5.714 43.704
5.857 44.444
end
#flajolet 122 samples
1.857 2.459
2.000 9.016
2.143 18.033
2.286 30.328
2.429 41.803
2.571 48.361
2.714 52.459
2.857 55.738
3.000 57.377
3.143 59.016
3.714 59.016
end
#comb 130 samples
9.571 0.769
10.143 1.538
10.286 2.308
10.429 3.846
10.571 5.385
10.714 5.385
10.857 6.923
11.000 9.231
11.143 12.308
11.286 13.846
11.429 15.385
11.571 20.000
11.714 21.538
11.857 21.538
12.000 22.308
12.143 22.308
12.286 23.846
12.429 25.385
12.571 25.385
12.714 26.154
12.857 26.923
13.143 26.923
end
