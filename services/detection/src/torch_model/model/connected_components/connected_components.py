"""
Connected components algorithm for region proposal
"""

import multiprocessing as mp
import torch
from PIL import Image, ImageFilter
import numpy as np
#np.set_printoptions(threshold=np.nan)
from skimage import io
from torchvision.transforms import ToTensor, ToPILImage
from timeit import default_timer as timer
import math
import numbers
from torch import nn
from torch.nn import functional as F
import matplotlib.pyplot as plt
import os


def get_components(bmap, numpy=False):
    '''
    Given a binary map, output an 8-connected components region
    :param bmap: Input binary map
    :param numpy: By default we accept pytorch maps, but change this to True to pass in numpy array
    :return: List of coordinates (tl_x, tl_y, br_x, br_y) corresponding to connected components 
    '''
    if numpy:
        bmap = torch.from_numpy(bmap)
    label_map = np.zeros(bmap.shape)
    current_label = 1
    label_dict = {}
    # pass 1
    for y in range(bmap.shape[1]):
        for x in range(bmap.shape[0]):
            # Background pixel, pass it
            if bmap[x, y].item() == 0:
                continue
            # Top left corner
            if x == 0 and y == 0:
                label_map[x, y] = current_label
                current_label += 1
                continue
            # top row pixel
            if y == 0:
                # Check only the west pixel
                west = label_map[x-1, y]
                if west == 0:
                    label_map[x, y] = current_label
                    current_label += 1
                    continue
                else:
                    label_map[x, y] = west
                    continue
            # right column pixel
            if x == bmap.shape[0]-1:
                # Check all but northeast (since we're on the right most pixel and that is out of range
                west = label_map[x-1, y]
                north_west = label_map[x-1, y-1]
                north = label_map[x, y-1].item()
                points = [west, north_west, north]
                filtered_points = [p for p in points if p != 0]
                if len(filtered_points) == 0:
                    label_map[x, y] = current_label
                    current_label += 1
                    continue
                min_val = min(filtered_points)
                for f in filtered_points:
                    if f != min_val:
                        if min_val in label_dict:
                            label_dict[f] = label_dict[min_val]
                        else:
                            label_dict[f] = min_val
                label_map[x, y] = min_val
                continue
            # left column pixel
            if x == 0:
                # Check north and northeast
                north = label_map[x, y-1]
                north_east = label_map[x+1, y-1]
                points = [north, north_east]
                filtered_points = [p for p in points if p != 0]
                if len(filtered_points) == 0:
                    label_map[x, y] = current_label
                    current_label += 1
                    continue
                min_val = min(filtered_points)
                for f in filtered_points:
                    if f != min_val:
                        if min_val in label_dict:
                            label_dict[f] = label_dict[min_val]
                        else:
                            label_dict[f] = min_val
                label_map[x, y] = min_val
                continue
            # Normal pixel
            # finally, do all the west, north west, north, and north east points
            west = label_map[x-1, y]
            north_west = label_map[x-1, y-1]
            north = label_map[x, y-1]
            north_east = label_map[x+1, y-1]
            points = [west, north_west, north, north_east]
            filtered_points = [p for p in points if p != 0]
            if len(filtered_points) == 0:
                label_map[x, y] = current_label
                current_label += 1
                continue
            min_val = min(filtered_points)
            for f in filtered_points:
                if f != min_val:
                    if min_val in label_dict:
                        label_dict[f] = label_dict[min_val]
                    else:
                        label_dict[f] = min_val
            label_map[x, y] = min_val

                
    # pass 2
    components_list = {}
    for y in range(label_map.shape[1]):
        for x in range(label_map.shape[0]):
            if label_map[x, y] == 0:
                continue
            val = label_map[x, y]
            if val in label_dict:
                val = label_dict[val]
            tl_x, tl_y, br_x, br_y = x, y, x, y
            if val in components_list:
                tl_x, tl_y, br_x, br_y = components_list[val]
                if x < tl_x:
                    tl_x = x
                if x > br_x:
                    br_x = x
                # don't really need this but i think it makes the code clear
                if y < tl_y:
                    tl_y = y
                if y > br_y:
                    br_y = y
            components_list[val] = (tl_x, tl_y, br_x, br_y)
    return list(components_list.values())


def balance_margins(bmap, img):
    '''
    Given an input binary map, balance possibly unequal margins. The motivation is to better determine the column number
    :param bmap: Binary input map (numpy nd array)
    :param img: Map of original image (np nd array)
    :return: Adjusted bmap, Adjusted img, left margin difference (must adjust downstream)
    '''
    img_height = bmap.shape[0]
    zero_col = np.zeros(img_height)
    left_w, right_w = 0, 0
    stop_left, stop_right = False, False
    for i in range(1, bmap.shape[1]):
        left = bmap[:, i]
        right = bmap[:, bmap.shape[1]-i]
        if not (left == zero_col).all():
            stop_left = True
        if not (right == zero_col).all():
            stop_right = True
        if stop_left and stop_right:
            diff = abs(left_w - right_w)
            if left_w < right_w:
                img = img[:, :bmap.shape[1]-diff, :]
                bmap = bmap[:, :bmap.shape[1]-diff]
            else:
                img = img[:, diff:, :]
                bmap = bmap[:, diff:]
            break
        elif stop_left:
            right_w += 1
        elif stop_right:
            left_w += 1
        else:
            right_w += 1
            left_w += 1
    l_diff = left_w - right_w if left_w > right_w else 0
    return bmap, img, l_diff


def write_proposals(img_p, output_dir='tmp/cc_proposals', white_thresh=245, blank_row_height=10):
    img = Image.open(img_p)
    fn = lambda x : 0 if x > white_thresh else 255
    img_np = np.array(img.convert('RGB'))
    bmap_np = np.array(img.convert('L').point(fn, mode='1')).astype(np.uint8)
    img_np_orig = img_np
    bmap_np, img_np, left_shave = balance_margins(bmap_np, img_np)
    img_height = bmap_np.shape[0]
    num_sections = int(img_height / blank_row_height)
    blank_row = np.zeros((blank_row_height, bmap_np.shape[1]))
    curr_top = 0
    white_rows = []
    for section in range(num_sections):
        curr_bot = curr_top + blank_row_height
        sub_img = bmap_np[curr_top:curr_bot, :]
        if (sub_img == blank_row).all():
            if len(white_rows) == 0:
                white_rows.append(curr_bot)
                curr_top += blank_row_height
                continue
            last_white_bot = white_rows[len(white_rows)-1]
            if last_white_bot == curr_top:
                white_rows[len(white_rows)-1] = curr_bot
            else:
                white_rows.append(curr_bot)
        curr_top += blank_row_height
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
        for ind, b in enumerate(blocks):
            c = coords[ind]
            column_index = col_idx[ind]
            num_sections = int(b.shape[0] / blank_row_height)
            blank_row = np.zeros((blank_row_height, b.shape[1]))
            curr_top = 0
            curr_bot = blank_row_height
            white_rows = []
            while curr_bot < b.shape[0]-1:
                sub_img = b[curr_top:curr_bot, :]
                if (sub_img == blank_row).all():
                    if len(white_rows) == 0:
                        white_rows.append(curr_bot)
                        curr_top += 1
                        curr_bot = curr_top + blank_row_height
                        continue
                    last_white_bot = white_rows[len(white_rows)-1]
                    if last_white_bot == curr_bot-1:
                        white_rows[len(white_rows)-1] = curr_bot
                    else:
                        white_rows.append(curr_bot)
                elif curr_top == 0:
                    white_rows.append(0)
                curr_top += 1
                curr_bot = curr_top + blank_row_height
            rows2 = []
            for i in range(len(white_rows)-1):
                curr = white_rows[i]
                nxt = white_rows[i+1]
                rows2.append((b[curr:nxt, :], curr, nxt))
            for r, c2, n in rows2:
                components = get_components(r, numpy=True)
                x1 = min(components, key=lambda x: x[1])
                x1 = x1[1]
                y1 = min(components, key=lambda x: x[0])
                y1 = y1[0]
                x2 = max(components, key=lambda x: x[3])
                x2 = x2[3]
                y2 = max(components, key=lambda x: x[2])
                y2 = y2[2]

                key = (num_cols, column_index)
                val = (top_coord + c2 + y1, c[0] + x1, top_coord + c2 + y2, c[0]+x2)
                if key in block_coords2:
                    block_coords2[key].append(val)
                else:
                    block_coords2[key] = [val]
    for key in block_coords2:
        coords_list = block_coords2[key]
        for ind2, bc in enumerate(coords_list):
            tl_y1, tl_x1, br_y1, br_x1 = bc
            adjusted = (left_shave + tl_x1, tl_y1, left_shave + br_x1, br_y1)
            print(adjusted)
            block_coords.add(adjusted)
    print('----')

    block_coords = list(block_coords)
    img_p = os.path.basename(img_p)
    write_p = os.path.join(output_dir, img_p[:-4] + '.csv')
    write_img_p = os.path.join(output_dir, img_p)
    with open(write_p, 'w', encoding='utf-8') as wp:
        for coord in block_coords:
            wp.write(f'{coord[0]},{coord[1]},{coord[2]},{coord[3]}\n')
    print('CC Proposals img shape')
    print(img_np_orig.shape)
    print('-----')
    draw_cc(img_np_orig, block_coords, write_img_p=write_img_p)
    return


"""
Debug functions
"""

def draw_grid(img_np, block_coords):
    for coords in block_coords:
        print(coords)
        img_np[coords[0]:coords[2], coords[1]-1:coords[1]+1, :] = 50
        img_np[coords[0]:coords[2], coords[3]-1:coords[3]+1, :] = 50
        img_np[coords[0]-1:coords[0]+1, coords[1]:coords[3], :] = 50
        img_np[coords[2]-1:coords[2]+1, coords[1]:coords[3], :] = 50
    Image.fromarray(img_np).save('test.png')


def draw_cc(img_np, cc_list, write_img_p=None):
    for coords in cc_list:
        img_np[coords[1]:coords[3], coords[0]-2:coords[0]+2, :] = 50
        img_np[coords[1]:coords[3], coords[2]-2:coords[2]+2, :] = 50
        img_np[coords[1]-2:coords[1]+2, coords[0]:coords[2], :] = 50
        img_np[coords[3]-2:coords[3]+2, coords[0]:coords[2], :] = 50
    write_p = 'test.png' if write_img_p is None else write_img_p
    Image.fromarray(img_np).save(write_p)


def get_columns_for_row(row):
    # 3/100 width = test width. We need half that for later
    test_width = int(math.ceil(row.shape[1] / 200))
    half_test_width = int(math.ceil(test_width / 2))
    curr_c = 1
    for c in range(2, 6):
        # Attempt to divide rows into c columns
        row_w = row.shape[1]
        # Check the row at the middle positions for column
        test_points = []
        for i in range(1, c):
            test_points.append(int(row_w / c * i))
        def mark_empty_block(p):
            block = row[:, p-half_test_width:p+half_test_width]
            test_col = np.zeros((block.shape[0], block.shape[1]))
            return (block == test_col).all()
        test_blocks = [mark_empty_block(p) for p in test_points]
        if False not in test_blocks:
            curr_c = c
    return curr_c


def divide_row_into_columns(row, n_columns):
    splits = []
    coords = []
    col_idx = []
    for c in range(1, n_columns):
        prev_row_div = int(row.shape[1] / n_columns * (c - 1))
        row_div = int(row.shape[1] / n_columns * c)
        coords.append((prev_row_div, row_div))
        splits.append(row[:, prev_row_div:row_div])
        col_idx.append(c)
    final_col = int(row.shape[1] / n_columns * (n_columns - 1))
    splits.append(row[:, final_col:])
    coords.append((final_col, row.shape[1]))
    col_idx.append(n_columns)
    return splits, coords, col_idx


if __name__ == '__main__':
    pool = mp.Pool(processes=240)
    results = [pool.apply_async(write_proposals, args=(os.path.join('img',x),)) for x in os.listdir('img')]
    [r.get() for r in results]



