# 
set xlabel "RTE leaf count" font ",15"
set ylabel "Frequency" font ",15"
set grid
set key font ',15'
set xtics font ',15'
set ytics font ',15'
set key horizontal bmargin
set title "Time Outs"
plot "-" using 1:2 with points title "naive-mid - 852 samples",\
    "-" using 1:2 with points title "naive-edge - 852 samples",\
    "-" using 1:2 with points title "balanced - 852 samples"
#naive-mid - 852 samples
41.0 1.0
80.0 1.0
101.0 1.0
110.0 1.0
113.0 1.0
115.0 1.0
119.0 1.0
end
#naive-edge - 852 samples
101.0 1.0
124.0 1.0
127.0 1.0
end
#balanced - 852 samples
46.0 1.0
77.0 1.0
80.0 1.0
85.0 1.0
103.0 1.0
105.0 1.0
106.0 1.0
126.0 1.0
end
