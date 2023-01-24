MODULE treeType_mod

IMPLICIT NONE

! tree node derived data type
TYPE node
    ! type data
    INTEGER :: key = 0                         ! key for ordering the nodes
    INTEGER :: bf = 0                          ! balance factor(= height of right sub-tree - height of left sub-tree)
    INTEGER :: rank = 0                        ! rank (i.e. 1+ #of nodes in left sub-tree = # of nodes with smaller key)
                                               ! can be used to determine relative position of a node within a sub-tree                 
    TYPE(node), POINTER :: node_L => null()    ! pointer to left sub-tree/child
    TYPE(node), POINTER :: node_R => null()    ! pointer to right sub-tree/child

END TYPE node

! auxiliary path pointer
TYPE path_ptr
    INTEGER :: a = 0
    TYPE(node), POINTER :: node
    TYPE(path_ptr), POINTER :: next => null()
    TYPE(path_ptr), POINTER :: prev => null()
END TYPE path_ptr

TYPE node_ptr
    TYPE(node), POINTER :: p
END TYPE node_ptr

!TYPE path_ptr_stack
!    TYPE(path_ptr), ALLOCATABLE :: pnode(:)
!END TYPE path_ptr_stack

END MODULE treeType_mod
