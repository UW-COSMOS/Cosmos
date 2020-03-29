import flask
import pytest
import sys
sys.path.append("..")
import reflex
from flask import Flask
import json

def create_app():
    flask_app = Flask(__name__)
    flask_app.app_context().push()
    return flask_app
app = create_app()

@app.route("/", methods = ['GET'])
def fetch_data():
    data={"templates":["[X] plays for [Y]", "[X] was selected by [Y]"], "label": "drafted by", "samples": [{"sub_label": "Nicolas Petan", "evidences": [{"masked_sentence": "Nicolas Petan (born March 22, 1995) is a Canadian ice hockey player, who is currently playing for the Manitoba Moose in the American Hockey League after being reassigned from the Winnipeg Jets in the National Hockey League."}]}, 
{"sub_label": "Austin Watson", "evidences": [{"masked_sentence": "Austin Watson (born September 4, 1986) is an American professional wrestler, who is currently signed to WWE under the ring name Xavier Woods as of February 2016."}]}, 
{"sub_label": "Tariq Abdul-Wahad", "evidences": [{"masked_sentence": "Tariq Abdul-Wahad (born Olivier Michael Saint-Jean; November 3, 1974) is a French basketball coach and former player."}]}, 
{ "sub_label": "Chris Carrawell", "evidences": [{"masked_sentence": "Chris Carrawell (born November 25, 1977) is an American professional basketball player who was selected by the San Antonio Spurs in the 2nd round (41st overall) of the 2000 NBA Draft."}]} ]}
    return flask.jsonify(data)

def test_reflex():
    client = app.test_client()
    response = client.get('/')

    preds_res = reflex.run_reflex(data=json.loads(response.data))
    print(preds_res)
    assert preds_res == ['the american hockey league', 'the ring name', 'olivier michael saint jean','the 2000 nba draft']
if __name__ == '__main__':

    client = app.test_client()
    response = client.get('/')
    print(json.loads(response.data)['samples'][1])
    print(type(json.loads(response.data)["samples"][1]["evidences"][0]))  
    print("Done testing")


