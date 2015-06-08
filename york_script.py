#!/usr/bin/env python3


"""\
usage: york_script.py INPUT_FILE OUTPUT_FILE
"""


import csv
import sys


def field(input_field):
    parts = input_field.split('-')
    return '{}{} ({})'.format(parts[1][:1], parts[2], parts[0])


def set_billid2(row):
    row['BillID2'] = field(row['BillID'])
    return row


def main():
    args = sys.argv[1:]
    if len(args) != 2 or '-h' in args or '--help' in args or 'help' in args:
        print(__doc__)
        raise SystemExit()

    input_name, output_name = args

    with open(input_name) as fin, open(output_name, 'w') as fout:
        reader = csv.DictReader(fin)
        fieldnames = list(reader.fieldnames)
        fieldnames.insert(2, 'BillID2')
        writer = csv.DictWriter(fout, fieldnames)

        writer.writeheader()
        writer.writerows(set_billid2(row) for row in reader)


if __name__ == '__main__':
    main()
