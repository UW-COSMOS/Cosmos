from setuptools import setup, find_packages
setup(
    name="cosmos-retrieve",
    version="0.1",
    author="Ankur Goswami",
    author_email="ankur.goswami12@gmail.com",
    description="The COSMOS retrieval package",
    url="https://github.com/UW-COSMOS/Cosmos",
    packages=find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache Software License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)
