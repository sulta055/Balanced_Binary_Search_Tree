MODULE treeType_mod

IMPLICIT NONE

! tree node derived data type
TYPE node
    ! type data
    INTEGER :: key = 0                         ! key for ordering the nodes
    INTEGER :: bf = 0                          ! balance factor(= height of right sub-tree - height of left sub-tree)
    TYPE(node), POINTER :: node_L => null()    ! pointer to left sub-tree/child
    TYPE(node), POINTER :: node_R => null()    ! pointer to right sub-tree/child

END TYPE node


TYPE path_node
    INTEGER :: a = 0
    TYPE(node), POINTER :: node => null()
    TYPE(path_node), POINTER :: next => null()
END TYPE path_node

END MODULE treeType_mod
