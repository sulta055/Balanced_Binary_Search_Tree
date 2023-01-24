PROGRAM AVL

USE treeType_mod
USE  AVL_mod

IMPLICIT NONE


INTEGER, PARAMETER :: N = 60  ! number of nodes that will be inserted 

TYPE(node), POINTER :: head, root
TYPE(node), POINTER :: T,S,P,Q,R
INTEGER :: a,i


! intialize pointer to root node
ALLOCATE(head)
! initialize entire tree
CALL tree_init(head,N)


DO i=2,60,2
    CALL delete_node(head,i)   
    CALL print_tree(head)
END DO



END PROGRAM AVL