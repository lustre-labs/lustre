This example exists because setting some SVG attributes like `viewBox` was causing
a runtime error. These attributes were mirrored as DOM properties but they were
marked as **read-only**.
