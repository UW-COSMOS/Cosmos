from workerA import sub_nums

from flask import Flask, request
import time

app = Flask(__name__)

@app.route('/sub')
def sub():
    app.logger.debug("sending request")
    first_num = request.args.get('first_num')
    second_num = request.args.get('second_num')
    result = sub_nums.delay(first_num, second_num)
    return "Done"
