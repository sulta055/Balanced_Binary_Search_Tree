PROGRAM AVL

USE treeType_mod
USE  AVL_mod

IMPLICIT NONE


INTEGER, PARAMETER :: N = 9  ! number of nodes that will be inserted 

TYPE(node), POINTER :: head, root
TYPE(node), POINTER :: T,S,P,Q,R
INTEGER :: a


! intialize pointer to root node
ALLOCATE(head)
! initialize entire tree
CALL tree_init(head,N)


T => null()
S => null()
P => head
Q => head%node_R
R => null()
a = 1

CALL delete_node(T,S,P,Q,R,a,1)

CALL print_tree(head%node_R)


END PROGRAM AVL