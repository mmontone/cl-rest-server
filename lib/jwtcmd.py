#!/usr/bin/env python

import sys
import jwt
import json

secret = 'lalala'
algorithm = 'HS256'

if __name__ == '__main__':

    action = sys.argv[1]
    string = sys.argv[2]
    
    if action == 'encode':
        print jwt.encode(json.loads(string), secret, algorithm=algorithm)
    elif action == 'decode':
        print json.dumps(jwt.decode(string, secret, algorithms=[algorithm]))
    else:
        raise Exception('Invalid action') 
