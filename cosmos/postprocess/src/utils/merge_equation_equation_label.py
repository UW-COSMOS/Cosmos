"""
We don't need to label equation labels, they should be a part of equations
This script takes a set of annotations and merges them.
"""

import click
from converters.xml2list import xml2list
import glob
import os
from PIL import Image
import numpy as np
from pascal_voc_writer import Writer


@click.command()
@click.argument('xml_dir')
@click.argument('output_dir')
def convert_annotations(xml_dir, output_dir):
    for xml in glob.glob(os.path.join(xml_dir, '*.xml')):
        bname = os.path.basename(xml)[:-4]
        xlist = xml2list(xml)
        writer = Writer(f'{bname}.png', 1920, 1920)
        # three lists. One Equation/equation label, the other not those
        eq_label_list = [x for x in xlist if x[0] == 'Equation label']
        eq_list = [x for x in xlist if x[0] == 'Equation']
        not_eq_list = [x for x in xlist if x not in eq_list and x not in eq_label_list]
        for x in not_eq_list:
            if x[0] == 'Table Note':
                writer.addObject('Body Text', *x[1])
            elif x[0] == 'Figure Note':
                writer.addObject('Body Text', *x[1])
            elif x[0] == 'Abstract':
                writer.addObject('Body Text', *x[1])
            else:
                writer.addObject(x[0], *x[1])

        # Now for each equation label, we associate the closest equation to the left of the equation label
        # Remember that el[1] and x[1] are coordinates in (tl_x, tl_y, br_x, br_y) form
        eq_el_map = {}
        print(eq_label_list)
        print('---')
        print(eq_list)
        print(xml)
        for el in eq_label_list:
            el_midpoint = el[1][1] + int((el[1][3] - el[1][1]) / 2)
            in_row = [x for x in eq_list if x[1][1] <= el_midpoint <= x[1][3]]
            # simple interval checks
            #in_row = [x for x in eq_list if x[1][1] <= el[1][1] <= x[1][3] or x[1][1] <= el[1][3] <= x[1][3] or el[1][1] <= x[1][1] <= el[1][3]]

            dists = [el[1][0] - x[1][2] for x in in_row]
            # only consider positive distances (left of obj)
            dists = [x if x >= 0 else float('inf') for x in dists]
            # Sometimes the equation label is really weirdly formatted. In this case 
            # just drop the equation label
            ind = None
            if len(dists) == 0:
                continue
            min_dist = min(dists)
            if min_dist == float('inf') or min_dist > 700:
                continue
            for i, d in enumerate(dists):
                if d == min_dist:
                    ind = i
                    break
            assert ind is not None
            assoc = in_row[ind]
            eq_list = [x for x in eq_list if x != assoc]
            el = (el[0], tuple(el[1]))
            eq_el_map[el] = assoc
        for eq in eq_list:
            writer.addObject(eq[0], *eq[1])

        for el in eq_el_map:
            eq = eq_el_map[el]
            print(eq)
            print(el)
            print('----')
            new_coords = [eq[1][0], min(eq[1][1], el[1][1]), el[1][2], max(eq[1][3], el[1][3])]
            assert new_coords[3] > new_coords[1]
            assert new_coords[2] > new_coords[0]
            writer.addObject(eq[0], *new_coords)
        save_path = os.path.join(output_dir, f'{bname}.xml')
        writer.save(save_path)


if __name__ == '__main__':
    convert_annotations()

