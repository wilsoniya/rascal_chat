#!/usr/bin/env python

import sys
import socket
import time

from gevent import monkey; monkey.patch_all()
import gevent
from gevent.socket import wait_read

PORT = 20000
HOST = 'wayne.wallitt.com'
BUFSIZE = 2**10

OP_REGISTER = 'register'
OP_SEND = 'send'

if len(sys.argv) > 1:
    message = sys.argv[1]
else:
    message = 'ADFASDFASDF'

def socket_reader(sock):
    while True:
        recvd = sock.recv(BUFSIZE)
        sender, message = recvd.split(';')
        print '{}: {}'.format(sender, message)

def client(sock):
    gevent.spawn(socket_reader, sock)
    while True:
        sys.stdout.write('message: ')
        sys.stdout.flush()
        wait_read(sys.stdin.fileno())
        outbound_msg = sys.stdin.readline()
        send_command = ';'.join((OP_SEND, outbound_msg))
        sock.send(send_command)

def startup():
    while True:
        username = raw_input('Your username: ')
        if len(username.strip()) > 0:
            break

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))

    register_command= ';'.join((OP_REGISTER, username))
    s.send(register_command)

    register_response = s.recv(BUFSIZE)
    if register_response == "ok":
        print 'connected'
        client(s)
    elif register_response == "username_exists":
        print 'username in use. exiting.'
        s.close() 

def main():
    g = gevent.spawn(startup)
    g.join()

if __name__ == '__main__':
    main()
