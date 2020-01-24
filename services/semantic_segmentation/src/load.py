import click
import glob
import os
import xmltodict
import json
from schema import DetectionExample
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

engine = create_engine(f'mysql://{os.environ["MYSQL_USER"]}:{os.environ["MYSQL_PASSWORD"]}@database/cosmos')
Session = sessionmaker()
Session.configure(bind=engine)

@click.command()
@click.argument('annotation_dir')
def load(annotation_dir):
    for split in glob.glob(os.path.join(annotation_dir, '*')):
        spl = os.path.basename(split)
        session = Session()
        for annotation_f in glob.glob(os.path.join(annotation_dir, spl, 'annotations', '*')):
            with open(annotation_f, 'r') as rf:
                xml = rf.read()
                o = xmltodict.parse(xml)
                annotation = json.dumps(o)
                image_name = o['annotation']['filename']
                with open(os.path.join(split, 'images', image_name), 'rb') as imagef:
                    img = imagef.read()
                    e = DetectionExample(image_name=image_name, bytes=img, annotation=o, split=spl)
                    session.add(e)
                    session.commit()



if __name__ == '__main__':
    load()
