#!/usr/bin/env python

import os
from keras.models import load_model
import numpy as np
from PIL import Image

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'
boxsize = 24
topleft = (122, 26)
botleft1_1 = (122, 290)
botleft1_2 = (122, 321)
botleft2_1 = (274, 290)
hua = (689, 26)

model_file = './final_model.h5'
model = load_model(model_file)
labels = open('../data/label_list.txt', 'r').read().splitlines() + \
            ['none']
labels = np.array(labels)

def _identify(imgs):
    numbers = model.predict_classes(imgs, verbose=0)
    return list(labels[numbers])

def slices(img, ts):
    imgs = []
    for t in ts:
        imgs.append(img[t[1]:t[1] + boxsize, t[0]:t[0] + boxsize, :])
    return np.stack(imgs, axis=0)

def identify(img, ts):
    return _identify(slices(img, ts))

def load_image(im):
    if isinstance(im, str):
        img = Image.open(im)
        img = np.asarray(img.convert('RGB')) / 255.
    elif isinstance(im, np.ndarray):
        img = im
    else:
        raise ValueError("must be image path or numpy's ndarray")
    return img

def read(im):
    img = load_image(im)
    h_offset = botleft2_1[0] - botleft1_1[0]
    horizontal_coords = np.linspace(botleft1_1[0], \
            botleft1_1[0] + 7 * h_offset, 8)
    horizontal_coords = list(map(int, horizontal_coords))
    top_y_coord = topleft[1]

    result = {}

    # toprow
    xs = horizontal_coords[:3] + [hua[0]] + horizontal_coords[-3:]
    ys = [top_y_coord] * 7
    coords = zip(xs, ys)
    result['top'] = identify(img, coords)

    # main stack
    v_offset = botleft1_2[1] - botleft1_1[1]
    co = botleft1_1
    for i in range(8):
        k = 'stack{}'.format(i)
        result[k] = []
        la = identify(img, [co])
        if la[0] == 'empty':
            result[k].append('empty')
            co = (co[0] + h_offset, co[1])
            continue
        result[k].append(la[0])
        co = (co[0], co[1] + v_offset)
        while True:
            la = identify(img, [co])
            if la[0] == 'none': # next stack
                co = (co[0] + h_offset, botleft1_1[1])
                break
            result[k].append(la[0])
            co = [co[0], co[1] + v_offset]

    return result

r = read('./wvxnki9nwfxhKrtlGFynmdvfuHmPthrl.png')
