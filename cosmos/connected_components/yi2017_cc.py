"""
This is an implementation of the proposal algorithm in "CNN Based Page Object Detection in Document Images"
"""

import math
import click
import multiprocessing as mp
import os
from PIL import Image
from connected_components.connected_components import balance_margins, get_components, get_blank_rows, get_columns_for_row, divide_row_into_columns, draw_cc
import numpy as np
from itertools import product

def write_proposals_yi(img_p, output_dir='tmp/cc_proposals', white_thres=245):
    img = Image.open(img_p)
    fn = lambda x : 0 if x > white_thres else 255
    img_np = np.array(img.convert('RGB'))
    bmap_np = np.array(img.convert('L').point(fn, mode='1')).astype(np.uint8)
    img_np_orig = img_np
    bmap_np, img_np, left_shave = balance_margins(bmap_np, img_np)
    proposals = get_components(bmap_np, numpy=True)
    cross_proposals = product(proposals, repeat=2)
    crossed_proposals = set()
    for left, right in cross_proposals:
        if left == right:
            continue
        tl_y1, tl_x1, br_y1, br_x1 = left
        tl_y2, tl_x2, br_y2, br_x2 = right
        if br_y2 < tl_y1 or br_x2 < tl_x1:
            continue
        combined = (tl_x1, tl_y1, br_x2, br_y2)
        crossed_proposals.add(combined)
    crossed_proposals = list(crossed_proposals)
   
    white_rows = get_blank_rows(bmap_np, 1, thres=0.99)
    rows = []
    for i in range(len(white_rows)-1):
        curr = white_rows[i]
        nxt = white_rows[i+1]
        rows.append((bmap_np[curr:nxt, :], curr, nxt))
    row_list = []
    for ind, (row, top_coord, bottom_coord) in enumerate(rows):
        num_cols = get_columns_for_row(row)
        splits, x_coords, col_idx = divide_row_into_columns(row, num_cols)
        if len(row_list) == 0:
            row_list.append((top_coord, bottom_coord, num_cols, x_coords))
            continue
        last_row = row_list[-1]
        lr_top, lr_bottom, lr_col_num, lr_x_coords = last_row
        if lr_col_num != num_cols:
            row_list.append((top_coord, bottom_coord, num_cols, x_coords))
            continue
        new_top, new_bottom = min(lr_top, top_coord), max(lr_bottom, bottom_coord)
        row_list[-1] = (new_top, new_bottom, num_cols, x_coords)
    box_coords = []
    for row in row_list:
        row_top, row_bottom, num_cols, x_coords = row
        for x_coord in x_coords:
            left_x, right_x = x_coord
            box_coords.append((left_x, row_top, right_x, row_bottom))

    # Now prune
    proposals_map = {}
    for proposal in crossed_proposals:
        tl_x, tl_y, br_x, br_y = proposal 
        if tl_x == br_x or tl_y == br_y:
            continue
        # Rule 1: Prune based on iff the box is in the col box
        # First prune based if the box is in the col box
        for box in box_coords:
            box_tl_x, box_tl_y, box_br_x, box_br_y = box
            assert box_tl_x < box_br_x
            assert box_tl_y < box_br_y
            if box_tl_x <= tl_x <= box_br_x and box_tl_x <= br_x <= box_br_x:
                if box_tl_y <= tl_y <= box_br_y and box_tl_y <= br_y <= box_br_y:
                    if box in proposals_map:
                        proposals_map[box].add((tl_x, tl_y, br_x, br_y))
                    else:
                        proposals_map[box] = set()
                        proposals_map[box].add((tl_x, tl_y, br_x, br_y))
    # Rule 2: Prune boxes that are not the leftmost or rightmost box in their row, or touch the column boundary
    # It's not immediately obvious how to associate proposals with their respective row. I came up with something here
    final_proposal_list = []
    r1_total = 0
    for box in proposals_map:
        box_tl_x, box_tl_y, box_br_x, box_br_y = box
        box_proposal_set = proposals_map[box]
        r1_total += len(box_proposal_set)
        left_boundary_proposal_map = {}
        right_boundary_proposal_map = {}
        top_boundary_proposal_map = {}
        bottom_boundary_proposal_map = {}
        def check_map(boundary_proposal_map, proposal, key_ind, target_ind, less_than):
            tl_x, tl_y, br_x, br_y = proposal
            rounded = int(round(proposal[key_ind], -1))
            if rounded in boundary_proposal_map:
                existing_proposal = boundary_proposal_map[rounded][0]
                if less_than and proposal[target_ind] < existing_proposal[target_ind]:
                    boundary_proposal_map[rounded] = [proposal]
                if not less_than and proposal[target_ind] > existing_proposal[target_ind]:
                    boundary_proposal_map[rounded] = [proposal]
                elif proposal[target_ind] == existing_proposal[target_ind]:
                    boundary_proposal_map[rounded].append(proposal)
            else:
                boundary_proposal_map[rounded] = [proposal]
            return boundary_proposal_map
        def check_left_map(proposal):
            return check_map(left_boundary_proposal_map, proposal, 1, 0, True)
        def check_right_map(proposal):
            return check_map(right_boundary_proposal_map, proposal, 3, 2, False)
        def check_top_map(proposal):
            return check_map(top_boundary_proposal_map, proposal, 0, 1, True)
        def check_bottom_map(proposal):
            return check_map(bottom_boundary_proposal_map, proposal, 2, 3, False)

        for proposal in box_proposal_set:
            left_boundary_proposal_map = check_left_map(proposal)
            right_boundary_proposal_map = check_right_map(proposal)
            top_boundary_proposal_map = check_top_map(proposal)
            bottom_boundary_proposal_map = check_bottom_map(proposal)

        for ind in left_boundary_proposal_map:
            prop_list = left_boundary_proposal_map[ind]
            for proposal in prop_list:
                tl_x, tl_y, br_x, br_y = proposal
                rounded_key = round(br_y, -1)
                if rounded_key in right_boundary_proposal_map:
                    # By definition, each object has the same r_br_x, which we're interested in comparing to
                    r_tl_x, r_tl_y, r_br_x, r_br_y = right_boundary_proposal_map[rounded_key][0]
                    if br_x == r_br_x:
                        final_proposal_list.append(proposal)
            
        for ind in top_boundary_proposal_map:
            prop_list = top_boundary_proposal_map[ind]
            for proposal in prop_list:
                tl_x, tl_y, br_x, br_y = proposal
                if br_x in bottom_boundary_proposal_map:
                    # By definition, each object has the same b_br_y, which we're interested in comparing to
                    b_tl_x, b_tl_y, b_br_x, b_br_y = bottom_boundary_proposal_map[br_x][0]
                    if br_y == b_br_y:
                        proposal = (proposal[0] + left_shave, proposal[1], proposal[2] + left_shave, proposal[3])
                        final_proposal_list.append(proposal)

    print(len(final_proposal_list))
    img_p = os.path.basename(img_p)
    write_p = os.path.join(output_dir, img_p[:-4] + '.csv')
    write_img_p = os.path.join(output_dir, img_p)
    with open(write_p, 'w', encoding='utf-8') as wp:
        for coord in final_proposal_list:
            wp.write(f'{coord[0]},{coord[1]},{coord[2]},{coord[3]}\n')
    draw_cc(img_np_orig, final_proposal_list, write_img_p=write_img_p)




@click.command()
@click.argument('img_dir')
@click.argument('output_dir')
def run_write_proposals(img_dir, output_dir):
    for x in os.listdir(img_dir):
        write_proposals_yi(os.path.join(img_dir, x), output_dir=output_dir)
    #pool = mp.Pool(processes=240)
    #results = [pool.apply_async(write_proposals_yi, (os.path.join(img_dir,x),), dict(output_dir=output_dir)) for x in os.listdir(img_dir)]
    #[r.get() for r in results]

if __name__ == '__main__':
    run_write_proposals()

