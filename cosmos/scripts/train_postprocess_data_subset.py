import click
import os

@click.command()
@click.option("--source-dir", type=click.Path(exists=True), help="Path source subfolders")
@click.option("--dest-dir", type=click.Path(exists=True), help="destination path for files subfolders")
@click.option("--file-count", type=int, help="number of files to copy")
def subset_training_data(source_dir, dest_dir, file_count):
    source_train_dir_path = source_dir + 'train_dir'
    source_val_dir_path = source_dir + 'val_dir'
    for root, dirs, files in os.walk(source_train_dir_path):
        for dir in dirs:
            print(dir)
            for filename in files[:file_count]:
                print(filename)
    # get list of folders in source path
    # make destination folders in destination path
    # get list of (No. of files) from first source path
    # for each item in list of files
    #     strip file extension
    #     create rootfile name list
    #
    # for each source folder:
    #     for each file name in rootfile list
    #     copy files beginnging with that name to the destination folder of the same name


subset_training_data()