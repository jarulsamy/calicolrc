require('fsm')

package.loaded.state1 = nil; -- Force reload, to reset variables
package.loaded.state2 = nil;
package.loaded.state3 = nil;
package.loaded.state4 = nil;

require('state1');
require('state2');
require('state3');
require('state4');

sm = fsm.new(state1);
sm:add_state(state2);
sm:add_state(state3);
sm:add_state(state4);

function notify(message)
    return function ()
                print("   Transition: " .. message);
           end
end

sm:set_transition(state1, "timeout", state2, notify("1 to 2"));
sm:set_transition(state2, "lost", state3, notify("2 to 3"));
sm:set_transition(state2, "timeout", state2, notify("2 to 2"));
sm:set_transition(state3, "accept", state4, notify("3 to 4"));

sm:entry();
while 1 do
    sm:update();
    print("Current State: " .. sm.currentState._NAME);
    if sm.currentState._NAME == "state4" then
        break
    end
end
