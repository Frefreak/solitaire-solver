#!/usr/bin/env python

import os
from keras.models import load_model
import cv2

model_file = './final_model.h5'
labels = open('../data/label_list.txt', 'r').read().splitlines() + ['none']

def load_img(fn):
    img = cv2.imread(fn)
    img = cv2.resize(img, (24, 24))
    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    img = img.reshape((-1, 24, 24, 3))
    img = img / 255.
    return img

model = load_model(model_file)
tot = 0
wrong = 0
img_dir = './test_data'
for f in os.listdir(img_dir):
    l = f.split('/')[-1]
    ldir = os.path.join(img_dir, f)
    for png in os.listdir(ldir):
        if png == '.directory':
            continue
        img = load_img(os.path.join(ldir, png))
        l_p = labels[model.predict_classes(img)[0]]
        tot += 1
        if l != l_p:
            wrong += 1
            print(os.path.join(f, png), l, l_p)
print(tot)
print(wrong)
print(1-(wrong/tot))
