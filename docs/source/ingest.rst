Ingestion
=========

The ingestion pipeline performs a variety of useful tasks, such as reading PDFs, extracting text, detecting page regions,
and building word embeddings over the output.

Here we detail the end to end ingestion algorithm for reference.

Reading PDFs
------------

First, we use _PDFMiner.six to extract the metadata layer from the document, if it has one. As one might expect, the PDF
text layer, especially for more recent PDFs, is of considerably higher quality than the text that is OCR'd.

The metadata is read into a dataframe, which indexes the text by its location on the page. Later, these indices will be
used to map the text back to the detected objects.

We use _Ghostscript to convert the PDF to a list of images, one for each page. Images are preprocessed using Pillow to
resize the longer edge to 1920 pixels, then padded with white space along the shorter side to produce a standard
1920x1920 image for each page.

Grid Proposals
--------------

Given the outputted images, we turn to proposing regions on the page which must be classified.
We did not obtain sufficiently good results using object detectors which automatically specify regions of interest
(e.g. Faster-RCNN). For the most part, high quality results can be obtained by algorithmically proposing regions on the
page. This is done using a simple grid algorithm:

First, the margins of the page are balanced by balancing the white space on each side of the page. Then the page is
divided into a grid: first divided into rows, then each row is then divided into columns and finally the columns are again
divided into rows. For each final cell in the grid, the nearest non white pixel to the left, bottom, right, and top are
found to calculate the final region proposal.

This algorithm was designed with scientific papers in mind, and it works well in that respect. It also
can work for other document types, but your mileage may vary.

Object Classification
---------------------

For each proposed region, we crop the region and use a deep neural network to classify as one of the input labels.
We use an architecture called Attentive-RCNN, which was developed by our team to take into account region information
surrounding the proposed region. For more information, see our _preprint.

We also incorporate text and higher level features by passing the results of our deep learning model to an XGBoost
model. Optionally, these results can also be augmented with a set of high level rules.

Aggregations
------------

The classified objects alone lose a lot of information. For example, we organize papers into sections, not blobs of body
text. We associate captions with tables and figures, which explain the visual diagrams. To recover these structures,
we aggregate intuitive groups of initial predictions into superclasses. For example, in the sections aggregation,
we look for labelled section headers, then append all proceeding body text sections to produce a full section.

For more information on the aggregations, see the :ref:`Aggregations` section.

Word Embeddings
---------------

We provide the option to train word embeddings on top of the extracted corpuses. We use _FastText to train over the extracted
corpus at ingestion time. The resulting embeddings are saved to disk.


.. _preprint: https://arxiv.org/abs/1910.12462
.. _PDFMiner.six: https://github.com/pdfminer/pdfminer.six
.. _Ghostscript: https://www.ghostscript.com/


