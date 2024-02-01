# Change log

v0.2.0 - 2024-01-23

Added
- New dispatch option `atomic_priority_set` to set priority and dispatch event in one atomic operation.
- New dispatch option for `members` called `external` to dispatch event to nodes except the local node.
- New dispatch option for `members` to dispatch event to list of selected nodes.

v0.2.1 - 2024-02-02

Includes a bugfix and new features.

Added
- Start option to disabled automatic priority synchronization with other nodes.

Fixed
- Priority was set incorrectly with dispatching
