from argparse import ArgumentParser
import os
import glob
import xml.etree.ElementTree as ET
from tqdm import tqdm

mapper = {"figureRegion":"Figure",
         "formulaRegion":"Equation",
         "tableRegion":"Table"
        }
def main(dir, prefix):
    files = get_files(dir, prefix)

def get_files(dir, prefix):
    files = glob.glob(f"{dir}/{prefix}*")
    for file in tqdm(files):
        rewrite_file(file)

def rewrite_file(path):
    tree = ET.parse(path)
    root = tree.getroot()
    objs = root.findall("object")
    if len(objs) == 0:
        print(path)
    



if __name__ == "__main__":
    parser = ArgumentParser(description="rewrite POD VOC annotations as COSMOS VOC")
    parser.add_argument("dir", type=str)
    parser.add_argument("prefix", type=str)
    args = parser.parse_args()
    main(args.dir, args.prefix)

