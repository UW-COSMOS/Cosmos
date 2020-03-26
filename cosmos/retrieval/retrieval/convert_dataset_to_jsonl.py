import json
import argparse
import os 

#def convert_dataset(args):
#    print("Converting dataset to Anserini jsonl file")
#    file_index = 0
#    i = 0
#    # id_map = {} # TODO: Use this if you want a mapping between id in anserini indexing to dataset id
#    #TODO: Load the dataset using args.dataset_path
#    #for each section in the dataset:
#    #    content = "" # TODO: Read the section content from dataset
#    #    # id_map[i] = <section_id_from_dataset> # TODO: Use this if index_table required
#    #    if i % args.max_docs_per_file == 0:
#    #        if i > 0:
#    #            output_jsonl_file.close()
#    #        output_path = os.path.join(args.output_folder, 'docs{:02d}.json'.format(file_index))
#    #        output_jsonl_file = open(output_path, 'w', encoding='utf-8', newline='\n')
#    #        file_index += 1
#    #    output_dict = {'id': str(i), 'contents': content}
#    #    output_jsonl_file.write(json.dumps(output_dict) + '\n')
#
#    #    if i % 100000 == 0:
#    #        print('Converted {} docs in {} files'.format(i, file_index))
#    #    i += 1
#    #output_jsonl_file.close()
#
#    # TODO: Use the following if id mapping required for any reason
#    # with open(args.output_folder+'/id_map.json', 'w') as fp:
#    #     json.dump(id_map, fp)
#
#if __name__=='__main__':
#
#    parser = argparse.ArgumentParser(description='''Converts processedx dataset to Anserini jsonl files.''')
#    parser.add_argument('--dataset_path', required=True, help='path to dataset')
#    parser.add_argument('--output_folder', required=True, help='output file')
#    parser.add_argument('--max_docs_per_file', default=1000000, type=int, help='maximum number of documents in each jsonl file.')
#
#    args = parser.parse_args()
#
#    if not os.path.exists(args.output_folder):
#        os.makedirs(args.output_folder)
#
#    convert_dataset(args)
#    print('Done!')
#    
#
