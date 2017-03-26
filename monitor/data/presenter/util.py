#!/usr/bin/env python

import uuid
import cv2
import dbus

def read_labels(fn='./label_list.txt'):
    with open(fn, 'r') as f:
        return f.read().splitlines()

def puttext(img, text, coord, color=(0, 255, 255)):
    """
    simplified cv2.putText
    """
    cv2.putText(img, text, coord, cv2.FONT_HERSHEY_DUPLEX, 1, color)

def randomw():
    """
    make random name to store unlabeled image
    """
    return uuid.uuid4().hex

def popup(title, text):
    item = "org.freedesktop.Notifications"
    path = "/org/freedesktop/Notifications"
    interface = "org.freedesktop.Notifications"
    time = 3000

    bus = dbus.SessionBus()
    notif = bus.get_object(item, path)
    notify = dbus.Interface(notif, interface)
    notify.Notify('', 0, '', title, text, '', '', time)
