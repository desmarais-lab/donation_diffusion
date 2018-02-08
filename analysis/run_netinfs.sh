for N in 300 500 800 1000 2000 5000 8000 10000
 do 
     tmux new-session -d -s "${N}_netinf" Rscript network_inference.R $N > ${N}_netinf_out.txt
 done
