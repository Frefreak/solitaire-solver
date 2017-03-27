#!/usr/bin/env python

import tensorflow as tf
from tensorflow.python.platform import gfile
import numpy as np
import glob
import os

import time

graph_file = './output_graph.pb'
label_file = './output_labels.txt'
labels = gfile.FastGFile(label_file).read().splitlines()

class Predictor():
    def __init__(self, inp, oup):
        self.input = inp
        self.output = oup

def initialize():
    with gfile.FastGFile(graph_file, 'rb') as f:
        graph_def = tf.GraphDef()
        graph_def.ParseFromString(f.read())
    inp, oup = tf.import_graph_def(graph_def,
            return_elements=[
                'DecodeJpeg/contents:0',
                'final_result:0'])
    return Predictor(inp, oup)

def predict(img, pred):
    img_ = gfile.FastGFile(img, 'rb').read()
    with tf.Session():
        result = pred.output.eval(feed_dict={pred.input: img_})
    return labels[np.argmax(result)]

pred = initialize()
print(predict('../data/jpeg_dataset/None/094ed1d0968b4987b9eaaa483a9533bf.png.jpg',pred))
