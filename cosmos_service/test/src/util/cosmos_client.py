import requests
import os
import configparser
import time

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__),'../..'))

config = configparser.ConfigParser()
config.read(os.path.join(BASE_DIR, 'config.ini'))

def poll_for_cosmos_output(status_endpoint, results_endpoint, output_path):
    """ Poll the Cosmos Status endpoint for the input PDF, then download output upon completion """
    poll_interval = int(config['cosmos']['POLL_INTERVAL'])
    poll_count = int(config['cosmos']['MAX_POLL_COUNT'])

    job_done = False

    for i in range(poll_count):
        response = requests.get(status_endpoint)
        response_data = response.json()
        print(f"Polled status endpoint {i} times:\n{response_data}")
        job_done = response_data['error'] or response_data['job_completed']
        if job_done:
            break
        time.sleep(poll_interval)

    if not job_done or response_data['error']:
        raise ValueError("Unable to retrieve COSMOS output")

    with open(output_path, 'wb') as writer:
        writer.write(requests.get(results_endpoint).content)

def submit_pdf_to_cosmos(input_path):
    """ Submit the input pdf to the online COSMOS service """
    submit_endpoint = config['cosmos']['BASE_URL'] + '/process/'
    with open(input_path, 'rb') as pdf_to_parse:
        file_form = {'pdf': pdf_to_parse }
        data_form = {'compress_images': False }
        response = requests.post(submit_endpoint, files=file_form, data=data_form)
        response_data = response.json()

        status_endpoint = response_data['status_endpoint']
        results_endpoint = response_data['result_endpoint']
        return (status_endpoint, results_endpoint)
