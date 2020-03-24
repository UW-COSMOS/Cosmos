import os
from ingest.process.detection.src.preprocess import pad_image
from PIL import Image
import glob
from ingest.process.proposals.connected_components import get_proposals
import shutil
from joblib import Parallel, delayed


def process_f(f, pth):
    basename = os.path.basename(f)[:-4]
    img = Image.open(f)
    proposals = get_proposals(img)
    with open(os.path.join(pth, 'proposals', f'{basename}.csv'), 'w') as wf:
        for proposal in proposals:
            x1, y1, x2, y2 = proposal
            wf.write(f'{x1},{y1},{x2},{y2}\n')
    basename = os.path.basename(f)
    img.save(os.path.join(pth, 'images_before_preprocess', basename))
    
    pimg = pad_image(img)
    pimg.save(f)

def write_proposals(pth):
    if os.path.exists(os.path.join(pth, 'proposals')):
        print('Found existing proposals. Wiping')
        shutil.rmtree(os.path.join(pth, 'proposals'))

    if os.path.exists(os.path.join(pth, 'images_before_preprocess')):
        print('Original images found. Overwriting images dir')
        shutil.rmtree(os.path.join(pth, 'images'))
        os.path.rename(os.path.join(pth, 'images_before_preprocess'), os.path.join(pth, 'images'))

    os.makedirs(os.path.join(pth, 'proposals'))
    os.makedirs(os.path.join(pth, 'images_before_preprocess'))
    num_processes = int(os.environ['NUM_PROCESSES'])
    Parallel(n_jobs=num_processes)(delayed(process_f)(f, pth) for f in glob.glob(os.path.join(pth, 'images', '*.png')))


if __name__ == '__main__':
    train_dir = '/data/train'
    val_dir = '/data/val'
    write_proposals(train_dir)
    write_proposals(val_dir)

