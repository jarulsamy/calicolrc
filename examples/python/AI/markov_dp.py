from random import choice, random
from collections import namedtuple

"""
Created a simple simulation of a Markov Decision Process based on an
example from Wikipedia [1].  There are three states and two actions.

In some cases an action can result in several different outcomes based
on an assigned probability.

Based on this MDP have created three policies.  One, called
goodPolicy, chooses next states to move toward the positive reward.
One, called badPolicy, chooses next staes to move toward the negative
reward.  The last, called randomPolicy, chooses randomly at each
opportunity.

[1] - http://en.wikipedia.org/wiki/Markov_decision_process
"""

transition = namedtuple("transition", "state probability reward")

class DeterministicPolicy:
    """
    Deterministic Decision Policy.
    Although the decision may be to be "random".
    """
    def __init__(self, transitions):
        ## {"state1": "action1", ...}
        self.transitions = transitions

    def decide(self, state):
        return self.transitions[state]

class World:
    def __init__(self, graph):
        self.graph = graph
        # Build cache of next possible actions:
        #self.actions = {}
        #for state in set([key[0] for key in self.graph.keys()]):
        #    self.actions[state] = [key[1] for key in self.graph.keys() if key[0] == state]
        # not much faster

    def getActions(self, state):
        return [key[1] for key in self.graph.keys() if key[0] == state]

    def getNextStates(self, state, action):
        return self.graph[(state, action)]

    def makeAction(self, state, action):
        """
        Make action given state, probabilisticaly.
        """
        nextStates = self.getNextStates(state, action)
        probability = 0
        nextState = None, 0
        test = random()
        for j in range(len(nextStates)):
            probability += nextStates[j].probability
            if test < probability:
                return nextStates[j].state, nextStates[j].reward
        raise Exception("Couldn't make move!")

    def simulate(self, policy, startState, steps):
        """
        Given a policy, will simulate it for the given number of steps and
        return the total accumulated reward over that time.
        """
        currentState = startState
        totalReward = 0
        for i in range(steps):
            action = policy.decide(currentState)
            if action == "random":
                action = choice(self.getActions(currentState))
            nextState, currentReward = self.makeAction(currentState, action)
            totalReward += currentReward
            currentState = nextState
        return totalReward

    def averageReward(self, policy, steps, startState, runs):
        """
        Test a particular policy for the given number of steps and average
        its performance over the given number of runs.
        """
        total = 0
        for i in range(runs):
            total += self.simulate(policy, startState, steps)
        return total/float(runs)

graph = {("s0", "a0"): [transition(probability=0.50, state="s0", reward=0),
                        transition(probability=0.50, state="s2", reward=0)],
         ("s0", "a1"): [transition(probability=1.00, state="s2", reward=0)],
         ("s1", "a0"): [transition(probability=0.70, state="s0", reward=5),  # reward!
                        transition(probability=0.10, state="s1", reward=0),
                        transition(probability=0.20, state="s2", reward=0)],
         ("s1", "a1"): [transition(probability=0.95, state="s1", reward=0),
                        transition(probability=0.05, state="s2", reward=0)],
         ("s2", "a0"): [transition(probability=0.60, state="s2", reward=0),
                        transition(probability=0.40, state="s0", reward=0)],
         ("s2", "a1"): [transition(probability=0.4, state="s2", reward=0),
                        transition(probability=0.3, state="s1", reward=0),
                        transition(probability=0.3, state="s0", reward=-1)]} # avoid!

world = World(graph)

goodPolicy = DeterministicPolicy({"s0":"a1", "s1":"a0", "s2":"a1"})
badPolicy = DeterministicPolicy({"s0":"a1", "s1":"a1", "s2":"a1"})
randomPolicy = DeterministicPolicy({"s0":"random", "s1":"random", "s2":"random"})

print("Test policies over 500 steps for 10 separate runs")
print("Average reward for good policy  :", world.averageReward(goodPolicy, 500, "s0", 10))
print("Average reward for bad policy   :", world.averageReward(badPolicy, 500, "s0", 10))
print("Average reward for random policy:", world.averageReward(randomPolicy, 500, "s0", 10))

