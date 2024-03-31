This example tests to make sure changing tag (aka a replace, not a morph) does
not cause a runtime error when diffing children. Previously we mistakenly assumed
we were _always_ doing a morph of children, but this is not the case.
