"""
Connected components algorithm for region proposal
"""

import numpy as np
import math
import os


def balance_margins(bmap, img):
    """
    Given an input binary map, balance possibly unequal margins. The motivation is to better determine the column number
    :param bmap: Binary input map (numpy nd array)
    :param img: Map of original image (np nd array)
    :return: Adjusted bmap, Adjusted img, left margin difference (must adjust downstream)
    """
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


def get_blank_rows(inp_np, blank_row_h):
    """
    Helper function to get blank rows in input nd array
    :param inp_np: Input nd_array
    :param blank_row_h: Blank row height
    :return: [integer denoting separation locations via y axis]
    """
    blank_row = np.zeros((blank_row_h, inp_np.shape[1]))
    curr_top = 0
    curr_bot = blank_row_h
    white_rows = []
    while curr_bot < inp_np.shape[0]-1:
        sub_img = inp_np[curr_top:curr_bot, :]
        if (sub_img == blank_row).all():
            if len(white_rows) == 0:
                white_rows.append(curr_bot)
                curr_top += 1
                curr_bot = curr_top + blank_row_h
                continue
            last_white_bot = white_rows[len(white_rows)-1]
            if last_white_bot >= curr_top:
                white_rows[len(white_rows)-1] = curr_bot
            else:
                white_rows.append(curr_bot)
        elif curr_top == 0:
            white_rows.append(0)
        elif curr_bot == inp_np.shape[0]-2:
            white_rows.append(inp_np.shape[0]-1)
        curr_top += 1
        curr_bot = curr_top + blank_row_h
    return white_rows

def get_proposals(img, white_thresh=245, blank_row_height=15, filter_thres=5, min_col_width=50):
    """
     Function that handles writing of object proposals
    :param img_p: Path to image
    :param output_dir: Path to output directory
    :param white_thres: Threshold to filter non white pixels
    :param blank_row_height: row height parameter
    :param filter_thres: Filter object size threshold parameter
    """
    fn = lambda x : 0 if x > white_thresh else 255
    img_np = np.array(img.convert('RGB'))
    bmap_np = np.array(img.convert('L').point(fn, mode='1')).astype(np.uint8)
    img_np_orig = img_np
    bmap_np, img_np, left_shave = balance_margins(bmap_np, img_np)
    img_height = bmap_np.shape[0]
    num_sections = int(img_height / blank_row_height)
    blank_row = np.zeros((blank_row_height, bmap_np.shape[1]))
    curr_top = 0
    white_rows = get_blank_rows(bmap_np, blank_row_height)
    rows = []
    for i in range(len(white_rows)-1):
        curr = white_rows[i]
        nxt = white_rows[i+1]
        rows.append((bmap_np[curr:nxt, :], curr, nxt))
    block_coords = set()
    block_coords2 = {}
    blocks_list = []
    obj_count = 0
    obj_heights = 0
    for row, top_coord, bottom_coord in rows:
        blocks = coords = col_idx = num_cols = None
        # Old way
        if row.shape[0] < 10 * blank_row_height:
            num_cols = get_columns_for_row(row)
            blocks, coords, col_idx = divide_row_into_columns(row, num_cols)
        else:
            # New way
            rowT = row.T
            col_height = blank_row_height
            white_cols = get_blank_rows(rowT, col_height)
            num_cols = len(white_cols)
            # This should be something reasonable, like less than 6
            while num_cols > 5:
                col_height += 5
                white_cols = get_blank_rows(rowT, col_height)
                num_cols = len(white_cols)

            cols = []
            blocks = []
            coords = []
            col_idx = []
            curr_white_cols = []
            # columns are fundamentally larger than rows, so we can lowerbound their width
            #for i in range(len(white_cols)-1):
            #    curr = white_cols[i]
            #    nxt = white_cols[i+1]
            #    assert nxt - curr - col_height > 0
            #    if nxt - curr - col_height > min_col_width:
            #        curr_white_cols.append(curr)
            #    else:
            #        continue
            #curr_white_cols.append(white_cols[-1])
            #white_cols = curr_white_cols


            for i in range(len(white_cols)-1):
                curr = white_cols[i]
                nxt = white_cols[i+1]

                spl = rowT[curr:nxt, :]
                spl = spl.T
                blocks.append(spl)
                coords.append((curr, nxt))
                col_idx.append(i)

        for ind, b in enumerate(blocks):
            c = coords[ind]
            column_index = col_idx[ind]

            white_rows = get_blank_rows(b, blank_row_height)
            rows2 = []
            for i in range(len(white_rows)-1):
                curr = white_rows[i]
                nxt = white_rows[i+1]
                rows2.append((b[curr:nxt, :], curr, nxt))
            for r, c2, n in rows2:
                # Replacing components with finding the proper pixel vals
                one_inds = np.argwhere(r)
                if len(one_inds) == 0:
                    continue
                h_one_inds = np.hsplit(one_inds, 2)

                x1 = int(np.min(h_one_inds[1]))
                y1 = int(np.min(h_one_inds[0]))
                x2 = int(np.max(h_one_inds[1]))
                y2 = int(np.max(h_one_inds[0]))

                key = (num_cols, column_index)
                val = (top_coord + c2 + y1, c[0] + x1, top_coord + c2 + y2, c[0]+x2)
                obj_count += 1
                obj_heights += y2 - y1

                if key in block_coords2:
                    block_coords2[key].append(val)
                else:
                    block_coords2[key] = [val]

    if obj_count > 0:
        if obj_count > 19:
            block_coords = get_proposals(img, white_thresh=white_thresh, blank_row_height=5 + blank_row_height, filter_thres=filter_thres)
        else:
            for key in block_coords2:
                coords_list = block_coords2[key]
                for ind2, bc in enumerate(coords_list):
                    tl_y1, tl_x1, br_y1, br_x1 = bc
                    # Filter objs that are too small
                    height = br_y1 - tl_y1
                    width = br_x1 - tl_x1
                    if height <= filter_thres or width <= filter_thres:
                        continue
                    adjusted = (left_shave + tl_x1, tl_y1, left_shave + br_x1, br_y1)
                    block_coords.add(adjusted)

    block_coords = list(block_coords)
    return block_coords


def get_columns_for_row(row):
    """
    Detect number of columns in a row
    :param row: nd array denoting row
    :return: number of columns
    """
    # 3/100 width = test width. We need half that for later
    test_width = int(math.ceil(row.shape[1] / 200))
    half_test_width = int(math.ceil(test_width / 2))
    curr_c = 1
    for c in range(2, 4):
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
    """
    Divide a row into columns
    :param row: nd_array representing the row
    :param n_columns: number of columns to split into
    :return: [nd_arrays of splits], [coords of splits], [column indices of splits]
    """
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


