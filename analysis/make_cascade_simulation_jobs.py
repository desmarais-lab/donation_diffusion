import os
import subprocess
import time

def submit_job(job_id, template, queue, n_per_job):

    job = template.format(queue=queue, job_id=job_id, n_per_job=n_per_job)
    jobname = f'casc_sim_{job_id}'
    with open(jobname, 'w') as jobfile:
        jobfile.write(job)
    
    response = None
    ntry = 0
    while response == None:
        try:
            ntry += 1
            response = subprocess.getoutput("qsub {}".format(jobname))

        except subprocess.CalledProcessError as error:
            # Raise exception and halt program after 5 unsuccesful tries 
            if ntry > 5:
                raise
                c -= 1
                # Wait before re-trying
                time.sleep(5)
                pass

    os.remove(jobname)


if __name__ == '__main__':

    queues = [{'name': 'bbd5087_a_g_hc_default', 'n_jobs': 34}, 
              {'name': 'bbd5087_a_g_sc_default', 'n_jobs': 20}, 
              {'name': 'bbd5087_b_g_sc_default', 'n_jobs': 20}]

    total_cores = 74
    n_candidates = 740
    candidates_per_job = 10

    with open('cascade_simulation_job_template.txt') as infile:
        template = infile.read()
    
    job_id = 0
    for queue in queues:

        for i in range(queue['n_jobs']):

            job_id += 1 # 1-indexed for R
            q_name = queue['name']
            q = f'#PBS -A {q_name}'
            submit_job(job_id, template, q, candidates_per_job)
            time.sleep(0.2)

