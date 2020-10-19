from setuptools import setup, find_packages
setup(
    name="cosmos-enrich",
    version="0.1",
    author="Iain McConnell",
    author_email="iain.mcconnell@wisc.edu",
    description="The COSMOS ingestion pipeline output file enrichment package",
    url="https://github.com/UW-COSMOS/Cosmos",
    packages=find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache Software License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.7.7',
    install_requires=[
        "dask"
        , "dask[array]"
        , "dask[bag]"
        , "dask[dataframe]"
        , "dask[delayed]"
        , "dask[distributed]"
        , "NumPy"
        , "Pandas"
        , "Toolz"
        , "Tornado"
    ]
)

