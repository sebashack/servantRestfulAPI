# A Booking Restful API With Haskell Servant

This is an example of a Restful-API, for handling the domain operations of a Booking
System, made with Servant Haskell and which integrates libraries and services
like Persistent MySQL, Elastic-Search, JWT for token based authentication,
Juicy-Pixels for image manipulation, etc...

This Restful-API was made according to the principles of Functional Domain
Modeling in which APIs are defined as abstract algebras with abstract data types,
and with a set of constraints over the the types and operations of that algebra.
Also, the code which implements the specification of an algebraic API is
defined as an Interpreter, which is the specific implementation of the API subject
to all its contraints and conditions. Here we have interpreters both for the
APIs which define the domains for a booking system - Reservation, User, Cancellation,
etc. - and for the Apis which define the operations and types of a Repository.
Repositories are the modules concerned with the actions of a persistent storage
system.

