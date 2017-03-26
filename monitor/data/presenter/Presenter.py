#!/usr/bin/env python

from __future__ import division
from __future__ import absolute_import
import os
import hashlib
import cv2

from .util import (puttext, randomw, popup)

# pylint: disable=too-many-instance-attributes
class Presenter():
    mouse_down = False
    cur_x = -1
    cur_y = -1
    def __init__(self, fn):
        self.img = cv2.imread(fn)
        if self.img is None:
            raise ValueError('fail to read ' + fn)
        self.height, self.width = self.img.shape[0], self.img.shape[1]
        self.last_clicked_pos = (self.width // 5, self.height - 80)
        self.cur_text_pos = (self.width // 5 * 2, self.height - 80)
        self.boxsize_pos = (int(self.width // 5 * 3.5), \
                            self.height - 80)
        puttext(self.img, 'in-point', (self.last_clicked_pos[0], \
                self.last_clicked_pos[1] - 28), (255, 255, 0)) # cyan
        puttext(self.img, 'current', (self.cur_text_pos[0], \
                self.cur_text_pos[1] - 28), (255, 255, 0))
        puttext(self.img, 'boxsize', (self.boxsize_pos[0], \
                self.boxsize_pos[1] - 28), (255, 255, 0))

        self.imghex = hashlib.md5(os.path.basename(fn).encode()) \
                .hexdigest()
        self.boxes = []
        self.color = (0, 0, 255)

        self.label_file = './label_list.txt'
        self.bulk_image_dir = './bulk_images'
        cv2.namedWindow(self.imghex)
        cv2.setMouseCallback(self.imghex, self._mouse_event_handler)

    #pylint: disable=too-many-arguments
    def _mouse_event_handler(self, event, x, y, _flags, _param):
    #pylint: enable=too-many-arguments
        if event == cv2.EVENT_LBUTTONDOWN:
            self.mouse_down = True
            self.boxes.append((x, y, x, y))
        elif event == cv2.EVENT_LBUTTONUP:
            self.mouse_down = False
        elif event == cv2.EVENT_MOUSEMOVE:
            self.cur_x = x
            self.cur_y = y
            if self.mouse_down:
                old_box = self.boxes[-1]
                new_box = (old_box[0], old_box[1], x, y)
                self.boxes = self.boxes[:-1] + [new_box]
        elif event == cv2.EVENT_RBUTTONUP:
            self.boxes = self.boxes[:-1]

    def set_color(self, color):
        bet = lambda v: v >= 0 and v <= 255
        try:
            assert len(color) == 3 and bet(color[0]) and \
                    bet(color[1]) and bet(color[2])
            # rgb -> bgr
            self.color = (color[2], color[1], color[0])
        except AssertionError:
            print('color format not recognized')

    def set_label_file(self, fn):
        self.label_file = fn

    def set_bulk_image_dir(self, d):
        self.bulk_image_dir = d

    def _save_images(self):
        os.makedirs(self.bulk_image_dir, exist_ok=True)
        for x1, y1, x2, y2 in self.boxes:
            (x1_, y1_) = min((x1, y1), (x2, y2))
            (x2_, y2_) = max((x1, y1), (x2, y2))
            sample = self.img[y1_:y2_, x1_:x2_, :]
            cv2.imwrite(os.path.join(self.bulk_image_dir, \
                        randomw() + '.png'), sample)

    def show(self):
        while True:
            img = self.img.copy()
            puttext(img, 'x: {} y: {}'.format(self.cur_x, \
                    self.cur_y), self.cur_text_pos)
            if self.boxes:
                inpx, inpy, inpx_, inpy_ = self.boxes[-1]
                puttext(img, 'x: {} y: {}'.format(inpx, inpy), \
                        self.last_clicked_pos)
                absx = abs(inpx_ - inpx)
                absy = abs(inpy_ - inpy)
                puttext(img, 'x: {} y: {}'.format(absx, absy), \
                        self.boxsize_pos)
            for box in self.boxes:
                x1, y1, x2, y2 = box
                cv2.rectangle(img, (x1, y1), (x2, y2), self.color, 1)
            cv2.imshow(self.imghex, img)
            ret = cv2.waitKey(1) & 0xff
            if ret == ord('q'):
                cv2.destroyWindow(self.imghex)
                break
            elif ret == ord('s'):
                self._save_images()
                popup('save images', 'success!')
