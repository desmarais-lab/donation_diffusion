for N in 300 500 1000 2000 5000 10000 15000 20000
#for N in 300
    do 
        tmux new-session -d -s "${N}_netinf" Rscript network_inference.R ${N}
    done
