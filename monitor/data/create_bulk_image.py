import os
import glob
from presenter.Presenter import Presenter

orig_dir = 'original_images'

for f in glob.glob(os.path.join(orig_dir, '*.png')):
    p = Presenter(f)
    p.show()
