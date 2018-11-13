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
            print(response)

        except subprocess.CalledProcessError as error:
            print('error')
            # Raise exception and halt program after 5 unsuccesful tries 
            if ntry > 5:
                raise
                # Wait before re-trying
                time.sleep(5)
                pass

    os.remove(jobname)


if __name__ == '__main__':

    queues = [
            #{'name': 'bbd5087_a_g_hc_default', 'n_jobs': 76}, 
            #{'name': 'bbd5087_a_g_sc_default', 'n_jobs': 36}, 
            {'name': 'bbd5087_b_g_sc_default', 'n_jobs': 74}
              ]

    total_cores = sum([x['n_jobs'] for x in queues])
    n_candidates = 740
    candidates_per_job = n_candidates / total_cores
    if total_cores % 1 != 0:
        raise ValueError('total_cores needs to be a factor of n_candidates')

    with open('cascade_simulation_job_template.pbs') as infile:
        template = infile.read()
    
    job_id = 0
    for queue in queues:
        for i in range(queue['n_jobs']):
            job_id += 1 # 1-indexed for R
            print(f'Submitting job {job_id}')
            q_name = queue['name']
            q = f'#PBS -A {q_name}'
            submit_job(job_id, template, q, candidates_per_job)
            time.sleep(0.1)

