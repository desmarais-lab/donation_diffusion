for year in `seq 1998 2 2016`
do 
    echo 'Starting session' $year
    tmux new-session -d -s "$year netinf" Rscript network_inference.R $year
done

