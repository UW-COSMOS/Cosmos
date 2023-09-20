import os
import pytest
import configparser
import requests
import time

BASE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__),'..'))

class BaseAnnotationComparisonTest:
    pdf_name: str
    pdf_remote_url: str
    config: configparser.ConfigParser

    def _setup(self, pdf_name, pdf_remote_url = None):
        self.pdf_name = pdf_name
        self.pdf_remote_url = pdf_remote_url
        self.config = self._get_config()



    def _get_config(self):
        config = configparser.ConfigParser()
        config.read(os.path.join(BASE_DIR, 'config.ini'))
        return config
    
    def _get_pdf_path(self):
        return os.path.join(BASE_DIR,'resources', 'pdfs', f'{self.pdf_name}.pdf')

    def _get_cosmos_path(self):
        return os.path.join(BASE_DIR,'resources', 'cosmos_output', f'{self.pdf_name}.zip')


    def _get_pdf(self):
        """Confirm the existence of the source PDF, and download it if it doesn't exist"""
        pdf_path = self._get_pdf_path()
        if self.pdf_remote_url is None and not os.path.exists(pdf_path):
            # PDF doesn't exist and no way to attain it, error out
            raise ValueError(f"No PDF found at {pdf_path} and no remote URL given")
        
        elif os.path.exists(pdf_path) and (
                self.pdf_remote_url is None or self.config['cache'].getboolean('CACHE_PDFS')):
            # PDF exists and config enables using a cached PDF
            return
        
        else:
            with open(pdf_path, 'wb') as pdf_writer:
                pdf_writer.write(requests.get(self.pdf_remote_url).content)

    
    def _get_cosmos_output(self):
        """Confirm the existence of COSMOS output for the source PDF, and download it if it doesn't exist"""
        cosmos_path = self._get_cosmos_path()
        if os.path.exists(cosmos_path) and self.config['cache'].getboolean('CACHE_COSMOS_OUTPUT'):
            # output already exists, return
            return
        
        self._poll_for_cosmos_output()


    def _poll_for_cosmos_output(self):
        poll_interval = int(self.config['cosmos']['POLL_INTERVAL'])
        poll_count = int(self.config['cosmos']['MAX_POLL_COUNT'])
        status_endpoint, results_endpoint = self._submit_pdf_to_cosmos()

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

        with open(self._get_cosmos_path(), 'wb') as writer:
            writer.write(requests.get(results_endpoint).content)

    def _submit_pdf_to_cosmos(self):
        submit_endpoint = self.config['cosmos']['BASE_URL'] + '/process/'
        with open(self._get_pdf_path(), 'rb') as pdf_to_parse:
            file_form = {'pdf': pdf_to_parse }
            data_form = {'compress_images': False }
            response = requests.post(submit_endpoint, files=file_form, data=data_form)
            response_data = response.json()

            status_endpoint = response_data['status_endpoint']
            results_endpoint = response_data['result_endpoint']
            return (status_endpoint, results_endpoint)



