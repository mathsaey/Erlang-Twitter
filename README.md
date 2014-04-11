# About

This is the repository for project of the Multicore Programming course of Mathijs Saey at the Vrije Universiteit Brussels. The goal of this project is to create a scalable twitter clone in erlang.
The full project assignment can be found in the Project-Erlang.pdf file. A shorter version can be found in this README.

# Assignment

This project consists of three parts: the implementation of a scalable Twitter-like service, an evaluation of this system, and a report that describes and the implementation and evaluation.

## A Scalable Twitter-like Service

Your task is to implement a scalable Twitter-like service. To keep the conceptual overhead small, this service only needs to store the minimal amount of information and will not support any of the advanced features such services offer. Furthermore, concentrate on the backend functionality. An actual web front end is not required and not desired. Focus on parallel and concurrency aspects.

Your service needs to store information about users, and their subscriptions to other users (“followers”). The tweets of a user only contain a timestamp and the text, which typically is limited to 140 characters. 

Your application should encode this information somehow, but it does not need to encode it as depicted. In fact, the actual data representation probably should be different to realize the different goals. Furthermore, you can add other relations, for instance to directly represent the reverse of subscribed to (following vs. followed by): if the query for the subscribers is done often, it might pay off to keep this list as well.

### Implementation Requirements

* Represent client connections as one Erlang process per client, for instance for load generation. In other words, in your load generation code you will create a number of processes, each of which is a client that requests some information from the server(s).
* Represent the server as one or more processes. Entry processes are the processes that communicate directly with client processes. In the given example implementation client and entry processes are mapped one to one. However, this is an implementation detail and you are free to change it.
* Only a subset of the conceivable operations is required. The set of operations that need to be supported is given as part of the public API definition in server.erl. Its semantics are supposed to remain unchanged:
	* *get_timeline* Get own timeline. A user can ask the server for his/her current timeline. The result includes a (perhaps partially stale) view on all own tweets, and all tweets of the users the user is subscribed to.
	* *tweet* Tweet. A user can submit a new tweet. The length is not restricted but should be around the typical size of 140 characters. The content may be generated randomly. The timestamp should represent the wall clock time when the tweet was received.
	* *get_tweets* Get tweets. A user can ask for all tweets of another user. The given interface allows to paginate the results to all user requests, thus, if you see it fit, you can restrict the amount of data of a response and require the client to make additional requests to obtain all data.
* Beyond these basic operations, you will need additional functionality to set up your evaluation experiments, like loading generated data sets, creating load, etc.
* This assignment is about modeling a scalable system using Erlang processes, message passing, etc. You are supposed to experiment with different strategies to see which impact they have on performance/consistency. Thus, do not use Erlang databases like Mnesia or Riak. They will obscure your results and make it harder to compare approaches.

### Scalability over consistency

As this project focuses on scalability, it is explicitly allowed and encouraged to sacrifice data consistency in order to increase scalability. For example, some requests might return stale data, or, in case you keep both a list of subscribers and subscriptions, these lists might not always be consistent.
In your report, describe the situations in which data might be in an inconsistent state. Describe your design, and detail your motivations: how do your design decisions improve the scalability of your system?
Furthermore, during the evaluation, try to quantify the degree of inconsistency a response contains. Look at typical use cases as well as worst case scenarios: in which cases can inconsistent data negatively affect the user experience?

### Example Implementation

The given implementation consists of two files:

* *server.erl* An interface definition to interact with an implementation of a Twitter-like system. It defines the semantics and should remain unchanged.
* *server_single_actor.erl* A simplified example implementation providing a basic ser- vice based on a single data actor (Erlang process).