import csv
import sys

input_filename = sys.argv[1]
header_filename = sys.argv[2]
output_filename = sys.argv[3]

with open(header_filename) as infile:
    header = infile.read().strip('\r\n').split(',')

ncol = len(header)

with open(input_filename) as infile, open(output_filename, 'w') as outfile:
    reader = csv.reader(infile, delimiter='|')
    writer = csv.writer(outfile, delimiter=',', quotechar='"', 
                        quoting=csv.QUOTE_NONNUMERIC)
    writer.writerow(header)
    for i,row in enumerate(reader):
        writer.writerow(row)

print(f'Read {i} rows and {ncol} columns')
