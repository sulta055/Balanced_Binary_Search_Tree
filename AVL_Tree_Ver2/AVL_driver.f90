PROGRAM AVL

USE treeType_mod
USE  AVL_mod

IMPLICIT NONE


INTEGER, PARAMETER :: N = 1280000  ! number of nodes that will be inserted 

TYPE(node), POINTER :: head, root
TYPE(node), POINTER :: T,S,P,Q,R
INTEGER :: a,i, N_tree
REAL*8 :: ti, tf, rn


N_tree = N

! intialize pointer to root node
ALLOCATE(head)
! initialize entire tree
CALL CPU_TIME(ti)
CALL tree_init(head,N_tree)
CALL CPU_TIME(tf)



!CALL print_tree(head)


PRINT*,'# of nodes currently in tree = ', N_tree




PRINT*,'Insertion time (seconds) = ', tf-ti


CALL CPU_TIME(ti)   
DO i=1,N-30

    CALL RANDOM_NUMBER(rn)
    a = 1+rn*N_tree

    CALL delete_node(head,a, N_tree) 
    
END DO
CALL CPU_TIME(tf)

CALL print_tree(head)

PRINT*,'# of nodes currently in tree = ', N_tree


PRINT*,'Deletion time (seconds) = ', tf-ti



END PROGRAM AVL