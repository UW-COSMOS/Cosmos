"""
This is an implementation of the proposal algorithm in "CNN Based Page Object Detection in Document Images"
"""

from PIL import Image
from connected_components.connected_components import balance_margins, get_components
import numpy as np
from itertools import product

def write_proposals(img_p, output_dir='tmp/cc_proposals', white_thres=245):
    img = Image.open(img_p)
    fn = lambda x : 0 if x > white_thres else 255
    img_np = np.array(img.convert('RGB'))
    bmap_np = np.array(img.convert('L').point(fn, mode='1')).astype(np.uint8)
    img_np_orig = img_np
    bmap_np, img_np, left_shave = balance_margins(bmap_np, img_np)
    proposals = get_components(bmap_np, numpy=True)
    crossed_proposals = product(proposals, repeat=2)
    white_rows = get_blank_rows(bmap_np, 1)
    rows = []
    for i in range(len(white_rows)-1):
        curr = white_rows[i]
        nxt = white_rows[i+1]
        rows.append((bmap_np[curr:nxt, :], curr, nxt))
    block_coords = set()
    block_coords2 = {}
    blocks_list = []
    for row, top_coord, bottom_coord in rows:
        num_cols = get_columns_for_row(row)
        blocks, coords, col_idx = divide_row_into_columns(row, num_cols)





@click.command()
@click.argument('img_dir')
@click.argument('output_dir')
def run_write_proposals(img_dir, output_dir):
    pool = mp.Pool(processes=240)
    results = [pool.apply_async(write_proposals, (os.path.join(img_dir,x),), dict(output_dir=output_dir)) for x in os.listdir(img_dir)]
    [r.get() for r in results]

if __name__ == '__main__':
    run_write_proposals()

