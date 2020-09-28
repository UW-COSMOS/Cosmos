import os
from ingest.process.detection.src.preprocess import pad_image
from PIL import Image
import glob
import click
from ingest.process.proposals.connected_components import get_proposals
import shutil
from joblib import Parallel, delayed
import shutil


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
        shutil.move(os.path.join(pth, 'images_before_preprocess'), os.path.join(pth, 'images'))

    os.makedirs(os.path.join(pth, 'proposals'))
    os.makedirs(os.path.join(pth, 'images_before_preprocess'))
    num_processes = int(os.environ['NUM_PROCESSES'])
    Parallel(n_jobs=num_processes)(delayed(process_f)(f, pth) for f in glob.glob(os.path.join(pth, 'images', '*.png')))


@click.command()
@click.option('--train-path', type=str, help='Path to training data')
@click.option('--val-path', type=str, help='Path to training data')
def run(train_path, val_path):
    write_proposals(train_path)
    write_proposals(val_path)


if __name__ == '__main__':
    run()
