require('fsm')

require('state1');
require('state2');
require('state3');

sm = fsm.new(state1);
sm:add_state(state2);
sm:add_state(state3);

sm:set_transition(state1, "timeout", state2);
sm:set_transition(state2, "lost", state3);
sm:set_transition(state2, "timeout", state2);
sm:set_transition(state3, "ball", state2);
sm:set_transition(state3, "timeout", state3);

sm:entry();
while 1 do
    sm:update();
end
