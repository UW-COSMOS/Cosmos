from ingest.ingest import Ingest

if __name__ == '__main__':
    model_config = 'ingest/process/configs/model_config.yaml'
    weights_pth = '../weights/model_weights.pth'
    device_str = 'cpu'
    #ingest = Ingest(tmp_dir='/Users/ankur/Projects/Cosmos/cosmos/ingestion/tmp')
    ingest = Ingest(tmp_dir='/Users/ankur/Projects/Cosmos/cosmos/ingestion/tmp', model_config=model_config,
                    weights_pth=weights_pth, device_str=device_str, use_semantic_detection=False)
    ingest.ingest('test_contract', 'test', 'result.pkl')
