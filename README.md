# Gossip-algorithm-Erlang
Implemented gossip algorithm using Erlang for different topologies and number of nodes, using actor model.

 Gossip type algorithms can be used both for group communication and for aggregate computation. The goal of this project is to determine the convergence of such algorithms through a simulator based on actors written in Erlang. Since actors are fully asynchronous, the particular type of Gossip implemented is the so-called Asynchronous Gossip.

Gossip Algorithm for information propagation The Gossip algorithm involves the following:

Starting: A participant(actor) told/sent a rumor (fact) by the main process
Step: Each actor selects a random neighbor and tells it the rumor.
Termination: Each actor keeps track of rumors and how many times he has heard the rumor. It stops transmitting once it has heard the rumor 10 times (10 is arbitrary, you can select other values).
Push-Sum algorithm for sum computation

State: Each actor Ai maintains two quantities: s and w. Initially, s  = xi = i (that is actor number i has value i, play with other distribution if you so desire) and w = 1
Starting: Ask one of the actors to start from the main process.
Receive: Messages sent and received are pairs of the form (s, w). Upon receiving, an actor should add the received pair to its own corresponding values. Upon receiving, each actor selects a random neighbor and sends it a message.
Send: When sending a message to another actor, half of s and w is kept by the sending actor, and half is placed in the message.
Sum Estimate: At any given moment of time, the sum estimate is s/w where s and w are the current values of an actor.
Termination: If an actor's ratio s/w did not change more than 10−10 in 3 consecutive rounds the actor terminates. WARNING: the values s and w independently never converge, only the ratio does.
Topologies: The actual network topology plays a critical role in the dissemination speed of Gossip protocols. As part of this project, you have to experiment with various topologies. The topology determines who is considered a neighbor in the above algorithms. 

Full Network: Every actor is a neighbor of all other actors. That is, every actor can talk directly to any other actor.
2D Grid: Actors form a 2D grid. The actors can only talk to the grid neighbors
Line: Actors are arranged in a line. Each actor has only 2 neighbors (one left and one right, unless you are the first or last actor).
Imperfect 3D Grid: Grid arrangement but one random other neighbor is selected from the list of all actors (8+1 neighbors).
