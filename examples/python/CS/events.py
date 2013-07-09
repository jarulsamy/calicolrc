
import Myro
import Events

Events.init()

def message0(o, e):
    Myro.wait(1)
    print("Message 1 received by 0:", o, e)

def message1(o, e):
    Myro.wait(1)
    print("Message 1 received by 1:", o, e)

def message2(o, e):
    Myro.wait(1)
    print("Message 2 received:", o, e)

Events.subscribe("message1", message0)
Events.subscribe("message1", message1)
Events.subscribe("message2", message2)

print(1)
Events.publish("message1")
print(2)
Events.publishAndWait("message2")
print(3)
Events.publish("message1")
print(4)
