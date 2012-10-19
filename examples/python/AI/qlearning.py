from random import random, randrange, choice

class TDLearning:
    """
    An implementation of temporal difference learning.
    """
    def __init__(self, states, actions, verbose=False):
        self.states = states
        self.actions = actions
        self.verbose = verbose
        self.discount = 0.9
        self.lrate = 0.3
        self.qValues = []
        for i in range(len(states)):
            # initialize with small random values
            self.qValues.append([random()*0.3-0.15 \
                                 for i in range(len(actions))])
            # initialize with 0's
            #self.qValues.append([0] * len(actions))
        self.rateOfExploration = 0.5
        # Create a dictionary that maps state names to their row in the
        # qValues table.
        self.stateDict = {}
        for i in range(len(states)):
            self.stateDict[states[i]] = i
        # Create a dictionary that maps action names to their col in the
        # qValues table.
        self.actionDict = {}
        for i in range(len(actions)):
            self.actionDict[actions[i]] = i
        print("Initial Q-Table")
        print(self)
        answer = input("Enter to continue...")
        if answer is None:
            raise KeyboardInterrupt()

    def __str__(self):
        result = "%10s \n" % "Qvalues"
        for i in range(len(self.qValues)):
            result += "%10s " % str(self.states[i])
            for j in range(len(self.actions)):
                result += "%.2f " % self.qValues[i][j]
            result += "\n"
        return result

    def qLearning(self, startState, maxSteps):
        state = startState
        for i in range(maxSteps):
            if not self.stateDict.has_key(state):
                exit("Error: unknown state %s", str(state))
            if random() < self.rateOfExploration:
                if self.verbose:
                    print("Choosing a random action")
                action = self.actions[randrange(0,len(self.actions))]
            else:
                values = self.qValues[self.stateDict[state]]
                action = self.actions[self.bestAction(values)]
            nextState = self.execute(state, action)
            reward = self.reinforcement(nextState)
            if self.verbose:
                print("Step:", i, "State:", state, "Action:", action,
                      "Next state:", nextState, "Reward:", reward)
            self.updateQValues(state, action, nextState, reward)
            if self.verbose and reward != 0:
                print(self)
                answer = input("Enter to continue...")
                if answer is None:
                    raise KeyboardInterrupt()
            if self.terminalState(nextState):
                state = startState
            else:
                state = nextState

    def updateQValues(self, state, action, nextState, reward):
        if self.terminalState(nextState):
            expectedReturn = reward
        else:
            expectedReturn = reward+(self.discount*self.bestValue(nextState))
        if self.verbose and expectedReturn != 0:
            print("expectedReturn %.2f" % expectedReturn)
        s = self.stateDict[state]
        a = self.actionDict[action]
        self.qValues[s][a] += self.lrate*(expectedReturn-self.qValues[s][a])

    def bestAction(self, values):
        """
        Returns the index of the action with the best q value. If values
        are equal randomly chooses one of the actions.
        """
        # First, find the best value:
        best_value = sorted(values)[-1]
        # Next, pick randomly among those who have best_value
        return choice([index for index in range(len(values)) if
                       values[index] == best_value])

    def oldBestAction(self, values):
        """
        Returns the index of the action with the best q value. If values
        are equal randomly chooses one of the actions.
        """
        best = 0
        for i in range(1, len(values)):
            if values[i] > values[best]:
                best = i
            if values[i] == values[best] and random() < 0.5:
                best = i
        return best

    def bestValue(self, state):
        """
        Returns the best q value for the given state in the table.
        """
        values = self.qValues[self.stateDict[state]]
        bestVal = values[0]
        for val in values:
            if val > bestVal:
                bestVal = val
        return bestVal

    def terminalState(self, state):
        """
        Returns True if the given state is terminal, False otherwise.
        """
        pass

    def execute(self, state, action):
        """
        Executes the given action in the current state and returns
        the next state.
        """
        pass

    def reinforcement(self, state):
        """
        Returns the amount of reward for the given state (may be 0).
        """
        pass
            
class LinearWorld(TDLearning):
    def terminalState(self, state):
        return state == -1 or state == 5
    def execute(self, state, action):
        if action == "left" and state != -1:
            return state - 1
        if action == "right" and state != 5:
            return state + 1
        return state
    def reinforcement(self, state):
        if state == -1:
            return -1
        if state == 5:
            return 1
        return 0
        
class GridWorld(TDLearning):
    """
    Consider the following grid-based environment, where the rewards of
    being in each location are shown.  

      -------------
    2 | 0 | 0 | +1|
      -------------
    1 | 0 | 0 | -1|
      -------------
    0 | 0 | 0 | 0 |
      -------------
        0   1   2  
    """
    def terminalState(self, state):
        return state == "2,2" or state == "2,1"
    def execute(self, state, action):
        if action == "n" and int(state[2]) < 2:
            return state[0]+","+str(int(state[2])+1)
        elif action == "e" and int(state[0]) < 2:
            return str(int(state[0])+1)+","+state[2]
        elif action == "s" and int(state[2]) > 0:
            return state[0]+","+str(int(state[2])-1)
        elif action == "w" and int(state[0]) > 0:
            return str(int(state[0])-1)+","+state[2]
        else:
            return state
    def reinforcement(self, state):
        if state == "2,2":
            return 1
        if state == "2,1":
            return -1
        return 0
    
if __name__ == '<module>':
    states = []
    for i in range(3):
        for j in range(3):
            states.append(str(i)+","+str(j))
    print(states)
    grid = GridWorld(states, ["n", "e", "s", "w"], True)
    grid.qLearning("0,0", 1000)
    print(grid)
    
    #linear = LinearWorld(range(-1,6), ["left", "right"], True)
    #linear.qLearning(2, 250)
    #print(linear)