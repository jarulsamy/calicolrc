import threading
import System
from System.Text import Encoding
import ZeroMQ
import time

context = ZeroMQ.ZmqContext.Create()
server = context.CreateSocket(ZeroMQ.SocketType.REP)
server.Bind("tcp://127.0.0.1:5558")
exiting = False

def server_loop():
    while not exiting:
        ## Wait for next request from client
        array = System.Array[System.Byte]([0] * 100)
        message = server.Receive(array)
        print("Received request:", message)
        ## Do Some 'work'
        time.sleep(1)
        ## Send reply back to client
        server.Send("World", Encoding.Unicode)

#thread = threading.Thread(target=server_loop)
#thread.start()

#client = context.CreateSocket(ZeroMQ.SocketType.REQ)
#client.Connect("tcp://localhost:5555")

#request = "Hello"
#for requestNum in range(10):
#    print("Sending request %s..." % requestNum)
#    request = System.Array[System.Byte]([ord(b) for b in "Hello"])
#    client.Send(request, 5, 0)
#    array = System.Array[System.Byte]([0] * 100)
#    reply = client.Receive(array)
#    print("Received reply %s: %s" % (requestNum, reply))

#exiting = True
#thread.join()
