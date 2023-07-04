# mastersdiss
A library for creating and managing serialized trees.

### TODO
 - Serializing from the get-go. Abstracting away the logic in a library and using it to define a a tree directly in a serialized format. I will use the AVL logic to automatically create a bst and also allow users to write trees in normal form the tree.
 - Use of tries (not sure how much more complexity this will add)
 - Generating a lot of fake data for different types of trees. I will then proceed to implement logic to handle
      - red, black tree (i still have to think of a way to assign values for red and black nodes)
      - B and B+ trees
      - Segment tress

 - Creating test cases to handle. i will use a combination of hunit, doctest (this is used in gibbon as well) and criterion for benchmarking.