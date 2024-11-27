from pymupdf.__main__ import gettext
from tempfile import NamedTemporaryFile
from typing import List
import re


# Load bearing regexes

# Github URLs are of primary concern, this regex is decent at finding them
# Looks for github.com/user-name/repo-name, accepting line breaks after delimiter characters (/, -, _)
# Note: This excludes repositories with '.' in the name, but is better at filtering out false positives
GITHUB_RE = re.compile(
r"""(
    github\.com(           # github.com
        \n?/\n?            # Forward slash with potential line break on either side
        (\w|[-_]\n?)+\n?   # Letters/numbers and delimiter characters (-_), with a potential line break following delimiters
    ){2}                   # Twice, for the username and then repo name
)""", re.VERBOSE)

# A more generous regex for an arbitrary url, more prone to false positives
ANY_SITE_RE = re.compile(
r"""(
    ([A-Za-z]\w+\.)+       # any number of dot-separated sub-domain names (eg 'www.github.')
    [a-z]\w{1,2}(          # top level domain (eg '.com')
        \n?/\n?            # Forward slash with potential line break on either side
        (\w|[-_.]\n?)+\n?  # Letters/numbers and delimiter characters (-_.), with a potential line break following delimiters
    )*                     # Any number of path parameters
    (\w|/)                 # Don't let the url end on a line-breakable delimiter
)""", re.VERBOSE)

class PyMuPDFGetTextWrapper:
    """ Duck-type copy of the argparser used for pymupdf's command line arguments to gettext
    TODO this is very fragile since the API for gettext might change
    """
    mode: str
    input: str
    ouput: str

    pages = '1-N'
    noligatures = False
    convert_white = False
    extra_spaces = False
    password = None
    noformfeed=False
    skip_empty=False
    grid=2
    fontsize=3

    _text: str = None

    def __init__(self, _input, mode='layout'):
        self.input = _input
        self.mode = mode

    @property
    def text(self):
        if self._text is None:
            self._text = self._get_text()
        return self._text

    def _get_text(self):
        with NamedTemporaryFile(mode='r+') as tf:
            self.output = tf.name
            gettext(self)
            tf.seek(0)
            return tf.read()

    def search_text(self, re_list: List[re.Pattern]):
        """ Given a list of regexes, find all matching occurrences in the text for any regex,
        then return a deduplicated set. """
        matches = []
        for regex in re_list:
            matches.extend(m[0].replace('\n','') for m in regex.findall(self.text))

        return list(set(matches))

