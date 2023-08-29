import hashlib
from typing import BinaryIO


def hash_file(file_data: BinaryIO, hash_method = hashlib.sha1, block_size = 4096):
    hash = hash_method()
    while True:
        buffer = file_data.read(block_size)
        if not buffer:
            break
        hash.update(buffer)
    return hash.hexdigest()

