#!/usr/bin/env python

import os

dataset_dir = '../data/dataset'
os.system('cp -r ' + dataset_dir + ' train_data')
os.system('mkdir -p test_data')

for f in os.listdir('train_data'):
    os.system('mkdir -p ' + os.path.join('test_data', f))
    d = os.path.join('train_data', f)
    files_to_mv = os.listdir(d)[:4]
    for ff in files_to_mv:
        os.system('mv ' + os.path.join(d, ff) + ' ' + \
                os.path.join('test_data', f, ff))
