#!/usr/bin/env python3

from __future__ import division
import sys
import os
import glob
from functools import partial
import presenter.util as util
# pylint: disable=no-name-in-module
from PyQt5.QtWidgets import (QApplication, QWidget, QComboBox, \
        QPushButton, qApp, QHBoxLayout, QVBoxLayout, QLabel, \
        QGridLayout)
from PyQt5.QtGui import QPixmap
# pylint: enable=no-name-in-module

bulk_image_dir = './bulk_images'
label_file = './label_list.txt'
dataset_dir = './dataset'
batch_size = 30

labels = util.read_labels(label_file) + ['None']
for l in labels:
    os.makedirs(os.path.join(dataset_dir, l), exist_ok=True)
labels = [''] + labels
imgs = glob.glob(os.path.join(bulk_image_dir, '*.png'))

# pylint: disable=too-few-public-methods
class Annotator(QWidget):
    def __init__(self, batch_, callback):
        super().__init__()
        self.init(batch_, callback)

    def init(self, batch_, callback):
# pylint: disable=global-statement
        global labels
# pylint: enable=global-statement
        self.labels = labels
        rootLayout = QVBoxLayout()
        # buttons
        btns = QHBoxLayout()
        quitBtn = QPushButton('quit')
        confBtn = QPushButton('confirm')
        btns.addStretch(1)
        btns.addWidget(quitBtn)
        btns.addWidget(confBtn)
        quitBtn.clicked.connect(lambda _x: qApp.quit())
        confBtn.clicked.connect(partial(callback, self))

        # pics and combo box
        self.choices = []
        gdl = QGridLayout()
        for i, pic in enumerate(batch_):
            p = QPixmap(pic)
            lbl = QLabel(self)
            lbl.setPixmap(p)
            combo = QComboBox()
            combo.addItems(self.labels)
            tempLayout = QHBoxLayout()
            tempLayout.addWidget(lbl)
            tempLayout.addWidget(combo)
            gdl.addLayout(tempLayout, i // 3, i % 3)
            self.choices.append((pic, combo))

        # setting root layout
        rootLayout.addLayout(gdl)
        rootLayout.addLayout(btns)
        self.setLayout(rootLayout)

        # center
        self.move(QApplication.desktop().screen().rect().center() - \
                self.rect().center())
        self.show()

batch = imgs[:batch_size]
imgs = imgs[batch_size:]

def process(choices):
    for fn, combo in choices:
        text = combo.currentText()
        if text:
            bn = os.path.basename(fn)
            os.rename(fn, os.path.join(dataset_dir, text, bn))

def allocate_batch(win, _b):
# pylint: disable=global-statement
    global imgs, batch
# pylint: enable=global-statement
    process(win.choices)
    win.close()
    if imgs:
        batch = imgs[:batch_size]
        imgs = imgs[batch_size:]
        _anno = Annotator(batch, allocate_batch)

app = QApplication(sys.argv)
anno = Annotator(batch, allocate_batch)
sys.exit(app.exec_())
