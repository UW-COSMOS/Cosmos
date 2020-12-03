"""
Quick script to load the linker (so we can cache all the files in the docker file)
"""
from ingest.preload_plugins.linking_setup import LinkingPlugin

if __name__ == '__main__':
    LinkingPlugin()


