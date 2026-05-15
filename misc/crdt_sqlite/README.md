> [!IMPORTANT]
> was trying to write a "fun" blog post. :(

# CRDTs (Conflict-Free Replicated Data Types) Implementation Example

Background:

We need consistency between replicas.

> Cassandra, Spanner/F1, BigTable?

But we do not want to be dependent on a central server, suffer through
latency issues, expose information to the server to achieve other properties,
and we may be also disconnected from the rest of the system. (CAP theorem
constraint).

> ...

> ### Hmm. Why Not Cryptography? 🤓
> 
> - Needs consensus.
> - Cannot go offline.

---

## Central Issue: How Do We Handle Conflicts in Replicas?

**Approach 1**: Last write wins.

- simple
- information can be easily lost

**Approach 2**: Store both and let user decide

- No loss
- Painful UX

**Approach 3**: Store everything within specified constraints so they can be resolved automatically.

- CRDTs allow this. <https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type>
- Only for specific data types
- Doesn't need ordering.
- Duplicates & Grouping does not matter (idempotent and associative).

---

## What Is CRDT?

CRDTs (Conflict-Free Replicated Data Types) is a data type that guarantees that
updates can be performed concurrently without locking or coordination all while
keeping everything consistent.

It provides strong eventual consistency / optimistic replication. It will make
sure the right thing will happen and wrong thing will not happen. More than
that the value of CRDT is that it allows for casual consistency to be
achieved.

Types (ref: <http://hal.upmc.fr/docs/00/55/55/88/PDF/techreport.pdf>) include:

## State Based CRDT

You have state, actions, and functions to apply actions to state.
The merge function must be commutative, associative, and idempotent.

> i.e. one of the functions is the function that takes in two different states
> of the same type and tries to resolve the local one. This is called merge
> and it's implementation must be such that above properties are satisfied.

On every update the replica sends their full state to other replicas. So, that
received state needs to be resolved with local copy.

- Needs all state to be transmitted
- Needs something like gossip protocol

## Operating Based CRDT

The update actions are sent to all replicas. Example:
`remove string 's' from position 3`. It is also commutative, associative, but
not idempotent. Duplicate update actions must not be transmitted.

- Can go brrr.
- Brr can make things worse so have to be careful.
- Needs casual ordering guarantee.

---

Implementation:

- [src/main.c](./src/main.c)

---

## MISC

- <https://en.wikipedia.org/wiki/Operational_transformation>
- "Approaches to Conflict-free Replicated Data Types" by paulo sérgio almeida: <https://dl.acm.org/doi/epdf/10.1145/3695249>
- Awesome CRDT: <https://github.com/alangibson/awesome-crdt>
