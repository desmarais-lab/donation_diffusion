with open('job_template.pbs') as infile:
    template = infile.read()

for n in range(4, 15):
    fname = f"run_netinf_{n}.pbs"
    with open(fname, 'w') as outfile:
        out = template.format(threshold = n)
        outfile.write(out)
