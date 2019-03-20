from Naked.toolshed.shell import muterun_js
from lxml import etree
import click
import os

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))

def unparse(node):
    """
    Transform a tree node to a string representation
    :param node: An etree node
    :return: String representation of a given etree node
    """

    if isinstance(node, str):
        return node

    if node.tag == 'msub':
        return '{%s}_{%s}' % (unparse(node[0]), unparse(node[1]))

    if node.tag == 'msup':
        return '{%s}^{%s}' % (unparse(node[0]), unparse(node[1]))

    if node.tag == 'mrow':
        return '{'+''.join(unparse(n) for n in node)+'}'

    return node.text

def get_variables(latex):
    """
    Parse latex code into a list of strings where each string represents one variable
    :param latex: Latex code
    :return: List of strings if successful; ['-1'] if parse error
    """
    processed = latex.replace(' ', '[space]')
    processed = processed.replace('\\', '[backslash]')
    processed = processed.replace('(', '{')
    processed = processed.replace(')', '}')

    response = muterun_js(os.path.join(CURRENT_DIR, 'katex/parse.js'), processed)
    if response.stdout == b'-1\n' or not response.stdout:
        yield -1
        return

    tree = etree.fromstring(response.stdout)
    ast = tree.xpath('.//semantics')[0]
    count = 0

    for c in ast.xpath('.//*'):
        c.attrib['id'] = str(count)
        count += 1

    ngram_kv = {}
    ngram = []
    for row in ast.xpath('.//mrow'):
        ngram.append([])
        for mi in row:
            if mi.text and mi.tag == 'mi' and mi.text:
                ngram_kv[mi.attrib['id']] = row.attrib['id']
                if mi.text in ['=', '+', '-', '*', '/']:
                    ngram.append([])
                    continue
                ngram[-1].append(mi.text)
            else:
                ngram.append([])

    for sup in tree.xpath('.//msup'):
        if 'None' not in unparse(sup):
            yield unparse(sup)

    for sub in tree.xpath('.//msub'):
        if 'None' not in unparse(sub):
            yield unparse(sub)

    for id in tree.xpath('.//mi'):
        if id.attrib['id'] not in ngram_kv:
            if 'None' not in unparse(id):
                yield unparse(id.text)
    
    for _x in ngram:
        if len(_x) > 0:
            yield unparse(''.join(_x))

# @click.command()
@click.option('--latex', required=True, help='Source latex code to be parsed')
def get_variables_cli(latex):
    """Variable extractor that finds variables in the Latex code"""
    for g in get_variables(latex):
        click.echo(unparse(g))


if __name__ == '__main__':
    get_variables_cli()



