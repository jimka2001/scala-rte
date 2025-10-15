# 
set xlabel "Aspect-Ratio" font ",15"
set ylabel "Percentage count >= 2 for imbalance <= x" font ",15"
set grid
set key font ',10'
set xtics font ',15'
set ytics font ',15'
set key horizontal bmargin
set title "Running Balances 64- for Aspect-Ratio"
plot "-" using 1:2 with lines title "tree-split-linear 493 samples",\
    "-" using 1:2 with lines title "tree-split-gauss 483 samples",\
    "-" using 1:2 with lines title "tree-split-inv-gauss 484 samples",\
    "-" using 1:2 with lines title "flajolet 483 samples",\
    "-" using 1:2 with lines title "comb 480 samples"
#tree-split-linear 493 samples
1.833 1.014
2.000 5.882
2.167 12.373
2.333 21.704
2.500 28.398
2.667 33.266
2.833 36.917
3.000 41.582
3.167 43.205
3.333 44.016
3.500 44.625
3.667 44.828
3.833 45.030
end
#tree-split-gauss 483 samples
1.500 3.313
1.667 14.493
1.833 29.400
2.000 38.302
2.167 46.584
2.333 51.139
2.500 53.416
2.667 54.865
2.833 55.694
3.000 56.108
end
#tree-split-inv-gauss 484 samples
2.500 0.000
2.667 0.207
2.833 1.446
3.000 4.132
3.167 5.579
3.333 8.264
3.500 14.050
3.667 16.736
3.833 20.661
4.000 23.760
4.167 28.719
4.333 32.851
4.500 35.537
4.667 37.603
4.833 38.017
5.000 39.669
5.167 40.702
5.333 41.736
5.500 41.942
5.667 42.149
5.833 42.149
6.833 42.149
end
#flajolet 483 samples
1.667 1.035
1.833 7.660
2.000 16.149
2.167 27.122
2.333 36.232
2.500 41.201
2.667 45.342
2.833 47.205
3.000 48.033
3.167 48.240
3.500 48.240
end
#comb 480 samples
5.500 0.417
5.667 1.250
5.833 2.708
6.000 5.208
6.167 8.750
6.333 12.292
6.500 18.125
6.667 22.292
6.833 27.083
7.000 30.000
7.167 32.083
7.333 34.792
7.500 36.042
7.667 36.667
7.833 37.292
8.000 37.500
8.167 37.708
8.333 37.708
8.500 37.708
9.000 37.708
end
