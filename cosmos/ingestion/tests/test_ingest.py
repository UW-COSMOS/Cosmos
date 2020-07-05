from ingest.ingest import Ingest

if __name__ == '__main__':
    model_config = 'ingest/process/configs/model_config.yaml'
    weights_pth = '../weights/model_weights.pth'
    device_str = 'cpu'
    #ingest = Ingest(tmp_dir='/Users/ankur/Projects/Cosmos/cosmos/ingestion/tmp')
    ingest = Ingest('tcp://localhost:8786', tmp_dir='ingestion/tmp', use_semantic_detection=True)
    ingest.ingest('c', 'contracts', 'contracts.pkl', remove_watermark=False)
