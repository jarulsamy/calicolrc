require('fsm')

require('state1');
require('state2');
require('state3');

sm = fsm.new(state1);
sm:add_state(state2);
sm:add_state(state3);

function notify(message)
    return function ()
              print("   Transition: " .. message);
           end
end

sm:set_transition(state1, "timeout", state2, notify("1 to 2"));
sm:set_transition(state2, "lost", state3, notify("2 to 3"));
sm:set_transition(state2, "timeout", state2, notify("2 to 2"));
sm:set_transition(state3, "ball", state2, notify("3 to 2"));
sm:set_transition(state3, "timeout", state3, notify("3 to 3"));

sm:entry();
while 1 do
    sm:update();
    print("Current State: " .. sm.currentState._NAME);
    if sm.currentState.accept then
        break
    end
end
