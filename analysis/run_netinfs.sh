#for year in `seq 1998 2 2016`
for N in 1000
do 
    echo 'Starting session' $year
    tmux new-session -d -s "${N}_2016_netinf" Rscript network_inference.R 2016 $N
done

