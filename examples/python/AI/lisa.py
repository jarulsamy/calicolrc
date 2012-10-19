from random import choice, random

"""
Created a simple simulation of a Markov Decision Process based on an
example from Wikipedia.  There are three states and two actions.

The transitions for the MDP are stored in a dictionary keyed on the
current state and the action taken.  In some cases an action can
result in several different outcomes based on an assigned probability.
Each value stored in the transitions dictionary is a list of lists.
The first list gives all of the probabilities.  The second lists gives
the next states that will result based on those probabilities.

The rewards are also stored in a dictionary that is keyed on the
previous state, the action taken, and the resulting state.  In this
example, most rewards are 0.  There is only one positive reward and
one negative reward.

Based on this MDP have created three policies.  One, called
goodPolicy, chooses next states to move toward the positive reward.
One, called badPolicy, chooses next staes to move toward the negative
reward.  The last, called randomPolicy, chooses randomly at each
opportunity.
"""

states = ["s0", "s1", "s2"]

actions = ["a0", "a1"]

transitions = {"s0a0": [[0.5, 0.5], ["s0", "s2"]],
               "s0a1": [[1.0], ["s2"]],
               "s1a0": [[0.7, 0.1, 0.2], ["s0", "s1", "s2"]],
               "s1a1": [[0.95, 0.05], ["s1", "s2"]],
               "s2a0": [[0.6, 0.4], ["s2", "s0"]],
               "s2a1": [[0.4, 0.3, 0.3], ["s2", "s1", "s0"]]}

rewards = {}
for current in states:
    for a in actions:
        for next in states:
            rewards[current+a+next] = 0
rewards["s1a0s0"] = 5
rewards["s2a1s0"] = -1

goodPolicy = {"s0":"a1", "s1":"a0", "s2":"a1"}
badPolicy = {"s0":"a1", "s1":"a1", "s2":"a1"}
randomPolicy = {"s0":"random", "s1":"random", "s2":"random"}

def simulate(policy, steps):
    """
    Given a policy, will simulate it for the given number of steps and
    return the total accumulated reward over that time.
    """
    currentState = "s0"
    totalReward = 0
    for i in range(steps):
        action = policy[currentState]
        if action == "random":
            action = choice(actions)
        possibilities = transitions[currentState+action]
        probability = 0
        test = random()
        for j in range(len(possibilities[0])):
            probability += possibilities[0][j]
            if test < probability:
                nextState = possibilities[1][j]
                break
        currentReward = rewards[currentState+action+nextState]
        #print
        #print "Step:", i
        #print "Current state:", currentState, "Action:", action,
        #print "Next state:", nextState, "Reward:", currentReward
        totalReward += currentReward
        currentState = nextState
    #print "Total reward:", totalReward
    return totalReward

def averageReward(policy, steps, runs):
    """
    Test a particular policy for the given number of steps and average
    its performance over the given number of runs.
    """
    total = 0
    for i in range(runs):
        total += simulate(policy, steps)
    return total/float(runs)

print("Test policies over 500 steps for 10 separate runs")
print("Average reward for good policy:", averageReward(goodPolicy,500,10))
print("Average reward for bad policy:", averageReward(badPolicy,500,10))
print("Average reward for random policy:", averageReward(randomPolicy,500,10))