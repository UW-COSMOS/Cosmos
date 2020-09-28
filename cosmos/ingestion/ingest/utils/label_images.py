from  ingest.utils.visualize import visualize_region
from ingest.utils.pdf_helpers import prepare_pdf_objs
from ingest.ingest import pdf_to_images
from ingest.process_page import process_page
from pathlib import Path
from pascal_voc_writer import Writer
import os
import shutil
from IPython.display import clear_output


def run_labelling(pdf_dir, image_dir, annotation_dir, classes, client):
    dataset_id = 'example'
    pdfs = client.submit(prepare_pdf_objs, pdf_dir, dataset_id)
    pdfs = pdfs.result()

    client.scatter(pdfs) # PDFs have lots of bytes, good to scatter them first
    pdf_images = [client.submit(pdf_to_images, pdf, False, image_dir) for pdf in pdfs]
    pdf_images = [p.result() for p in pdf_images]
    xpath = os.path.join(annotation_dir, 'annotations')
    Path(xpath).mkdir(parents=True, exist_ok=True)
    ipath = os.path.join(annotation_dir, 'images')
    Path(ipath).mkdir(parents=True, exist_ok=True)
    ppath = os.path.join(annotation_dir, 'proposals')
    Path(ppath).mkdir(parents=True, exist_ok=True)
    images = [i for j in pdf_images for i in j]
    proposals = client.map(process_page, images)
    proposals = [p.result() for p in proposals]
    for proposal in proposals:
        new_path = os.path.join(ipath, f'{proposal["page_id"]}.png')
        writer = Writer(new_path, 900, 900)
        os.rename(proposal['img_path'], new_path)
        prop_path = os.path.join(ppath, f'{proposal["page_id"]}.csv')
        with open(prop_path, 'w') as wf:
            for bb in proposal['proposals']:
                x1, y1, x2, y2 = [int(x) for x in bb]
                wf.write(f'{x1},{y1},{x2},{y2}\n')
                visualize_region(new_path, bb)
                label = None
                while label not in classes:
                    label = input()
                    if label not in classes:
                        print('Invalid input, must be part of classes')
                clear_output(wait=True)
                writer.addObject(label, *bb)
        anno_path = os.path.join(xpath, f'{proposal["page_id"]}.xml')
        writer.save(anno_path)
    shutil.rmtree(image_dir)






