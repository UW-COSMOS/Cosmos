import click
from distributed.diagnostics.plugin import WorkerPlugin
import os
import spacy
from scispacy.linking import EntityLinker
from scispacy.abbreviation import AbbreviationDetector


class LinkingPlugin(WorkerPlugin):
    def __init__(self):
        nlp = spacy.load('en_core_sci_lg')
        abbreviation_pipe = AbbreviationDetector(nlp)
        nlp.add_pipe(abbreviation_pipe)
        self.linker = EntityLinker(resolve_abbreviations=True, name="umls")
        nlp.add_pipe(self.linker)
        self.nlp = nlp

@click.command()
def dask_setup(worker):
    worker._pending_plugins = [LinkingPlugin()]

    

