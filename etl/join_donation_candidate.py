import sys
import pandas as pd

donation_file = sys.argv[1]
candidate_file = sys.argv[2]
out_file = sys.argv[3]

don = pd.read_csv(donation_file)
can = pd.read_csv(candidate_file)

df = don.merge(can, on='CAND_ID')
df.to_csv(out_file, index=False)
