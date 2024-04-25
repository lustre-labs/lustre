This example makes sure that patches that add or remove different children in a
server component are correctly applied on the client. At one point we realised
patches were being sent in reverse order and that meant the client ended up
incorrectly reusing newly-created children from the _current patch_ when diffing
new nodes.
