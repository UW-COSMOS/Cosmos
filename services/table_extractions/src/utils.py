import pickle
from itertools import zip_longest


def grouper(iterable: iter, n: int, fillvalue=None) -> list:
    args = [iter(iterable)] * n
    return list(zip_longest(*args, fillvalue=fillvalue))


def is_picklable(obj):
    """
    Helpful function to debug whether objects are pickleable (requirement for multiprocessing)
    """
    try:
        pickle.dumps(obj)

    except pickle.PicklingError:
        return False
    return True
