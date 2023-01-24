MODULE AVL_mod

USE treeType_mod

IMPLICIT NONE



CONTAINS


! this subroutine inserts N-1 nodes into a tree containing just a root node
! (no repeated keys!)

SUBROUTINE tree_init(head, N)
    
    TYPE(node), POINTER, INTENT(INOUT) :: head
    INTEGER, INTENT(IN) :: N

    TYPE(node), POINTER :: root, current, T, P, S, Q, R ! temp pointers
    INTEGER :: counter, keys(N), a    

    ! clear temp pointers
    T => null()
    P => null()
    S => null()
    Q => null()
    R => null()

    counter =  0
    a = 0

    keys=(/ 1, 3, 10, 15, 13, 44, 9, 78 /)

    ! Allocate memory for root node
    ALLOCATE(head%node_R)
    
    ! set root node key 
    head%node_R%key = 2
    ! sey root balance factor
    head%node_R%bf = 0

    ! insert N-1 nodes into the tree
    DO WHILE(counter .LT. N-1)

        T => head
        S => head%node_R
        P => head%node_R
        Q => null()

        CALL print_tree(head%node_R)

        PRINT*,'Inserting new node with key',keys(counter+1)


        CALL insert_node(T,S,P,Q,keys(counter+1))

        !update balance factors
        CALL adjust_bf(S,P,Q,R,a,keys(counter+1))

        PRINT*,'Finished inserting new node.'

        CALL print_tree(head%node_R)

        ! check for imbalance

        CALL check_balance_insert(T,S,R,P,a,keys(counter+1))

        CALL print_tree(head%node_R)
        
       
        counter = counter + 1
    END DO



END SUBROUTINE tree_init


! Routine for a standard Binary Search Tree node insertion
!    Note: Balance factors are adjuated along the way to the insertion location
!   (Only need to update balance factor for sub-trees that lie on the traversal path).

RECURSIVE SUBROUTINE insert_node(T,S,P,Q,key)

    TYPE(node), POINTER, INTENT(INOUT) :: T,S,P,Q
    INTEGER, INTENT(IN) :: key


    ! traverse tree to the insertion location and insert new node

    ! Go right if key is greater
    IF(key .GT. P%key) THEN
            
        Q => P%node_R

        ! insert here if empty
        IF(.NOT. ASSOCIATED(Q)) THEN
            ! allocate memory for the new node
            ALLOCATE(Q)
            ! link newly created node
            P%node_R => Q
            ! set key and balance factor
            Q%key = key
            Q%bf = 0
  
        ! otherwise keep traversing
        ELSE
            ! advance temp pointers
            IF(Q%bf .NE. 0) THEN
                T => P
                S => Q
            END IF
            P => Q
            CALL insert_node(T,S,P,Q,key)
        END IF    
    ! Go left if key is smaller
    ELSE

         Q => P%node_L

        ! insert here if empty
        IF(.NOT. ASSOCIATED(Q)) THEN
            ! allocate memory for the new node
            ALLOCATE(Q)
            ! link newly created node
            P%node_L => Q
            ! set key and balance factor
            Q%key = key
            Q%bf = 0

        ! otherwise keep traversing
        ELSE
            ! advance temp pointers
            IF(Q%bf .NE. 0) THEN
                T => P
                S => Q
            END IF
            P => Q
            CALL insert_node(T,S,P,Q,key)
        END IF

    END IF


END SUBROUTINE insert_node


! Alogirithm for AVL Node Deletion
!
! Steps: -> Start at head
!        -> traverse to location of node that will be deleted
!        -> Store the path via pointers: 
!          construct a list of pointers that record all nodes along the path: { (P_i,a_i) |i = 0,1,...,l }
!                                                                              P_0 = head, a_0 = +1, and LINK(P_l,a_l) = null  
!                                                                              a_i = +1 if right, -1 if left, so that P_i+1 = LINK(p_i,a_i) = P_i%node_L if a_i = -1 
!                                                                                                                                          OR P_i%node_R if a_i = +1 
!           Store these pointers in a linked list. The last node in the list is P_l is the one that gets deleted.
!        -> To delete P_l: 
!
!
!                          CASE 1: P_l has only one or no children:
!
!                          1) set LINK(P_l-1,a_l-1) => LINK(P_l, -a_l)
!                          2) adjust balance factor at predecessor node P_l-1 (because the height 
!                            of the a_l-1 subtree of P_l-1 has changed upon removal of P_l from it)
!                            
!                          CASE 2: P_l has two children. In this case, can reduce the problem down to case 1.
!                                  To do this, need to find a "successor" node from the right sub-tree of P_l.
!
!                          1) Traverse down the left sub-tree of right P_l until a node with only one or 
!                             no children is found. Add pointers P_l+k (k=1,2,...) to all nodes along the path.
!                             Once found, the successor node is assigned as the new delete node P_l+k (k= # of steps
!                             to get to successor). Copy contents of this P_l+k to P_l, then delete P_l+k using CASE 1 rules.                                 
!
!
!

SUBROUTINE delete_node(T,S,P,Q,R,a,key)

    TYPE(node), POINTER, INTENT(INOUT) :: T,S,P,Q,R
    INTEGER, INTENT(INOUT) :: a
    INTEGER, INTENT(IN) :: key
  
    TYPE(path_ptr), POINTER:: p0,current, temp, pl

    ALLOCATE(p0)
    p0%node => P
    p0%a = 1
    IF(ASSOCIATED(P%node_R)) ALLOCATE(p0%next)
    current => p0%next
    current%prev => p0    

    PRINT*,'p0=',p0%node%key

    DO WHILE(key .NE. Q%key)

        P => Q

        ! Go right if key is greater
        IF(key .GT. P%key) THEN
            
            Q => P%node_R
            a = 1
              
        ! Go left if key is smaller
        ELSE IF(key .LT. P%key) THEN

            Q => P%node_L
            a = -1

        END IF

        current%node => P     
        current%a = a
        ALLOCATE(current%next)
        current%next%prev => current
        current => current%next

    END DO

    PRINT*,'Found delete node: ', Q%key

    T => Q  ! Q and T both point to the node that will get deleted

    current%node => Q
   

    ! Case 1: Q has empty right node
    IF(.NOT. ASSOCIATED(T%node_R)) THEN
        PRINT*,'Case 1.'
        IF(a .EQ. -1) THEN            
            P%node_L => T%node_L
 
        ELSE IF(a .EQ. 1) THEN
            P%node_R => T%node_L
        END IF
 
        current%a = -a 
        !ALLOCATE(current%next)
        !current%next%prev => current
        !current => current%next
        !current%node => T%node_L

    ! Case 2: Q has empty left node
    ELSE IF(.NOT. ASSOCIATED(T%node_L)) THEN
        PRINT*,'Case 2.'
        IF(a .EQ. -1) THEN            
            P%node_L => T%node_R  
        ELSE IF(a .EQ. 1) THEN
            P%node_R => T%node_R
        END IF
 
        current%a = -a
        !ALLOCATE(current%next)
        !current%next%prev => current
        !current => current%next
        !current%node => T%node_R

    ! Case 3: Q has non-empty left and right nodes
    ELSE
        ! find a successor node 
        ! (i.e. a node on the left branch of right sub-tree of Q, that has only one or no child)
        R => T%node_R

        ! check for null left sub-tree of R 
        !If null, then replace with R
        IF(.NOT. ASSOCIATED(R%node_L)) THEN
            R%node_L => T%node_L
            IF(a .EQ. -1) THEN            
                P%node_L => R  
            ELSE IF(a .EQ. 1) THEN
                P%node_R => R
            END IF
        ! Otherwise, find replacement from left sub-tree of R
        ELSE
            ! traverse to nearest left node with a non-empty subtree
            S => R%node_L

            DO WHILE(ASSOCIATED(S%node_L))
                R => S
                S => R%node_L
            END DO       
 
            S%node_L => T%node_L                
            R%node_L => S%node_R
            S%node_R => T%node_R

            IF(a .EQ. -1) THEN            
                P%node_L => S  
            ELSE IF(a .EQ. 1) THEN
                P%node_R => S
            END IF

        END IF

    END IF


    temp => current
    PRINT*,'Path node backtrace:'
    PRINT*,temp%node%key,temp%a

    DO WHILE(ASSOCIATED(temp%prev))
        temp => temp%prev
        PRINT*,temp%node%key,temp%a
    END DO

    ! Free memory of deleted node
    DEALLOCATE(T)

    PRINT*,'Node has been deleted.'

    ! check balance    
    CALL check_balance_delete(current)

END SUBROUTINE delete_node



SUBROUTINE check_balance_delete(P_k)

    TYPE(path_ptr), POINTER, INTENT(INOUT):: P_k

    TYPE(node), POINTER :: T,S,P,R

    ! Adjust balance factors and rebalance if an unbalance node is found
    ! starting with P_l-1
    
    P_k => P_k%prev
    DO WHILE(ASSOCIATED(P_k%prev))
        PRINT*,'Checking node ',P_k%node%key
        IF(P_k%node%bf .EQ. P_k%a) THEN
            P_k%node%bf = 0
        ELSE IF(P_k%node%bf .EQ. 0 ) THEN
            P_k%node%bf = -P_k%a
        ELSE IF(P_k%node%bf .EQ. -P_k%a) THEN
            PRINT*,'Node is out of balance.'

            ! set temp pointers
            ! S points to imbalanced node
            ! R points to opposite sub-tree of S to where th deletion occured
            ! P points to the parent of S
            
            S => P_k%node
            IF(P_k%a .EQ. 1) THEN
                R => S%node_L
            ELSE IF(P_k%a .EQ. -1) THEN
                R => S%node_R
            END IF
            T => P_k%prev%node

            PRINT*,'S,R,T=',S%key,R%key,P%key
            !P => 
            !Q =>
            CALL rebalance_delete(T,S,R,P,P_k%a) 

        END IF
        P_k => P_k%prev
    END DO


END SUBROUTINE check_balance_delete



SUBROUTINE rebalance_delete(T,S,R,P,a)

    TYPE(node), POINTER, INTENT(INOUT) :: T,S,R,P
    INTEGER, INTENT(IN) :: a

    ! Case 1: Right-heavy imbalance
    !         Perform single rotation
    IF(R%bf .EQ. -a) THEN
        PRINT*,'Case 1. Performing single rotation.'
        P => R
        IF(a .EQ. 1) THEN
            S%node_R => R%node_L
            R%node_L => S 
        ELSE IF(a .EQ. -1)THEN
            S%node_L => R%node_R
            R%node_R => S
        END IF

        ! update balance factors
        S%bf = 0
        R%bf = 0

    ! Case 2: Left-Heavy imbalance
    !         Perform double rotation
    ELSE IF(R%bf .EQ. a) THEN
        PRINT*,'Case 2. Performing double rotation.'

        IF(a .EQ. 1) THEN
            P => R%node_L
            R%node_L => P%node_R 
            P%node_R => R
            S%node_R => P%node_L
            P%node_L => S
        ELSE IF(a .EQ. -1)THEN
            P => R%node_R
            R%node_R => P%node_L 
            P%node_L => R
            S%node_L => P%node_R
            P%node_R => S
        END IF

        ! update balance factors
        IF(P%bf .EQ. a) THEN
            S%bf = -a
            R%bf = 0
        ELSE IF(P%bf .EQ. 0) THEN
            S%bf = 0
            R%bf = 0
        ELSE IF(P%bf .EQ. -a) THEN
            S%bf = 0
            R%bf = a
        END IF

    END IF


    ! Assign new sub-tree root
    IF(ASSOCIATED(S,T%node_R)) THEN
        T%node_R => P
    ELSE IF(ASSOCIATED(S,T%node_L)) THEN
        T%node_L => P
    END IF

END SUBROUTINE rebalance_delete


! this subroutine adjusts the balance factors between nodes S and Q after an insert

SUBROUTINE adjust_bf(S,P,Q,R,a,key)

    TYPE(node), POINTER, INTENT(INOUT) :: S,P,Q,R
    INTEGER, INTENT(INOUT) :: a
    INTEGER, INTENT(IN) :: key

    PRINT*,'Adjusting balance factors...'

    IF(key .GT.  S%key) THEN
       a = 1                   ! a keeps track of the sub-tree of S in which insertion occured 
       R => S%node_R
       P => S%node_R   
    ELSE
       a = -1
       R => S%node_L
       P => S%node_L
    END IF

    DO WHILE(P%key .NE. Q%key)  
        IF(key .GT.  P%key) THEN
            P%bf = 1
            P => P%node_R   
        ELSE
            P%bf = -1
            P => P%node_L
        END IF
    END DO

END SUBROUTINE adjust_bf

! S points to imbalanced node
! R points to sub-tree of S in which the insertion occured
! T points to the parent of S
! P, Q both point to the newly inserted node 
SUBROUTINE check_balance_insert(T,S,R,P,a,key)

    TYPE(node), POINTER, INTENT(INOUT) :: T,S,R,P
    INTEGER, INTENT(IN) :: a,key

    PRINT*,'Checking for imbalance...'

    ! no imbalances 
    IF(S%bf .EQ. 0) THEN
        S%bf = a
    ELSE IF(S%bf .EQ. -a) THEN
        S%bf = 0
    ! imbalance
    ELSE IF (S%bf .EQ. a) THEN
        
        PRINT*,'Found imbalance! Rebalancing sub-tree...'

        CALL rebalance_insert(T,S,R,P,a,key)
    END IF

END SUBROUTINE check_balance_insert


SUBROUTINE rebalance_insert(T,S,R,P,a,key)

    TYPE(node), POINTER, INTENT(INOUT) :: T,S,R,P
    INTEGER, INTENT(IN) :: a,key

    ! Case 1: Right-heavy imbalance
    !         Perform single rotation
    IF(R%bf .EQ. a) THEN
        PRINT*,'Case 1. Performing single rotation.'
        P => R
        IF(a .EQ. 1) THEN
            S%node_R => R%node_L
            R%node_L => S 
        ELSE IF(a .EQ. -1)THEN
            S%node_L => R%node_R
            R%node_R => S
        END IF

        ! update balance factors
        S%bf = 0
        R%bf = 0

    ! Case 2: Left-Heavy imbalance
    !         Perform double rotation
    ELSE IF(R%bf .EQ. -a) THEN
        PRINT*,'Case 2. Performing double rotation.'

        IF(a .EQ. 1) THEN
            P => R%node_L
            R%node_L => P%node_R 
            P%node_R => R
            S%node_R => P%node_L
            P%node_L => S
        ELSE IF(a .EQ. -1)THEN
            P => R%node_R
            R%node_R => P%node_L 
            P%node_L => R
            S%node_L => P%node_R
            P%node_R => S
        END IF

        ! update balance factors
        IF(P%bf .EQ. a) THEN
            S%bf = -a
            R%bf = 0
        ELSE IF(P%bf .EQ. 0) THEN
            S%bf = 0
            R%bf = 0
        ELSE IF(P%bf .EQ. -a) THEN
            S%bf = 0
            R%bf = a
        END IF

    END IF


    ! Assign new sub-tree root
    IF(ASSOCIATED(S,T%node_R)) THEN
        T%node_R => P
    ELSE IF(ASSOCIATED(S,T%node_L)) THEN
        T%node_L => P
    END IF

END SUBROUTINE rebalance_insert



SUBROUTINE print_tree(root)

TYPE(node), POINTER, INTENT(INOUT) :: root

TYPE(node), POINTER :: temp0, temp1L, temp1R, temp2LL, temp2LR, temp2RL, temp2RR, &
                       temp2LLL,temp2LLR, temp2LRL, temp2LRR, &
                       temp2RLL, temp2RLR, temp2RRL, temp2RRR

INTEGER :: L1(2), L2(4), L3(8)
INTEGER :: B1(2), B2(4), B3(8)

PRINT*,' '
PRINT*,' '

L1 = 0
L2 = 0
L3 = 0 
B1 = 0
B2 = 0
B3 = 0 


temp0 => root
temp1L => root%node_L
temp1R => root%node_R

IF(ASSOCIATED(temp1L)) THEN
    L1(1) = temp1L%key
    B1(1) = temp1L%bf
    IF(ASSOCIATED(temp1L%node_L)) THEN
        temp2LL => temp1L%node_L
        L2(1) = temp2LL%key
        B2(1) = temp2LL%bf

        IF(ASSOCIATED(temp2LL%node_L)) THEN
            temp2LLL => temp2LL%node_L
            L3(1) = temp2LLL%key
            B3(1) = temp2LLL%bf
        END IF
        IF(ASSOCIATED(temp2LL%node_R)) THEN
            temp2LLR => temp2LL%node_R
            L3(2) = temp2LLR%key
            B3(2) = temp2LLR%bf
        END IF

    END IF

    IF(ASSOCIATED(temp1L%node_R)) THEN
        temp2LR => temp1L%node_R
        L2(2) = temp2LR%key
        B2(2) = temp2LR%bf
        IF(ASSOCIATED(temp2LR%node_L)) THEN
            temp2LRL => temp2LR%node_L
            L3(3) = temp2LLL%key
            B3(3) = temp2LLL%bf
        END IF
        IF(ASSOCIATED(temp2LR%node_R)) THEN
            temp2LRR => temp2LR%node_R
            L3(4) = temp2LRR%key
            B3(4) = temp2LRR%bf
        END IF

    END IF
END IF


IF(ASSOCIATED(temp1R)) THEN
    L1(2) = temp1R%key
    B1(2) = temp1R%bf
    IF(ASSOCIATED(temp1R%node_L)) THEN
        temp2RL => temp1R%node_L
        L2(3) = temp2RL%key
        B2(3) = temp2RL%bf
        IF(ASSOCIATED(temp2RL%node_L)) THEN
            temp2RLL => temp2RL%node_L
            L3(5) = temp2RLL%key
            B3(5) = temp2RLL%bf
        END IF

        IF(ASSOCIATED(temp2RL%node_R)) THEN
            temp2RLR => temp2RL%node_R
            L3(6) = temp2RLR%key
            B3(6) = temp2RLR%bf
        END IF

    END IF

    IF(ASSOCIATED(temp1R%node_R)) THEN
        temp2RR => temp1R%node_R
        L2(4) = temp2RR%key
        B2(4) = temp2RR%bf

        IF(ASSOCIATED(temp2RR%node_L)) THEN
            temp2RRL => temp2RR%node_L
            L3(7) = temp2RRL%key
            B3(7) = temp2RRL%bf
        END IF
        IF(ASSOCIATED(temp2RR%node_R)) THEN
            temp2RRR => temp2RR%node_R
            L3(8) = temp2RRR%key
            B3(8) = temp2RRR%bf
        END IF


    END IF
END IF


PRINT*,'Keys:'
WRITE(*,FMT ='(a25)',ADVANCE='no') ,''
WRITE(*,FMT =('(i3)')) root%key
PRINT*,' '

WRITE(*,FMT ='(a4)',ADVANCE='no') ,''
WRITE(*,FMT =('(2(i16))')) L1(1),L1(2)
PRINT*,' ' 

WRITE(*,FMT ='(a8)',ADVANCE='no') ,''
WRITE(*,FMT =('(4(i8))')) L2(1),L2(2),L2(3),L2(4)
PRINT*,' '

WRITE(*,FMT ='(a10)',ADVANCE='no') ,''
WRITE(*,FMT =('(8(i4))')) L3(1),L3(2),L3(3),L3(4),L3(5),L3(6),L3(7),L3(8)


PRINT*,' '
PRINT*,'Balance Factors:'
WRITE(*,FMT ='(a25)',ADVANCE='no') ,''
WRITE(*,FMT =('(i3)')) root%bf
PRINT*,' '

WRITE(*,FMT ='(a4)',ADVANCE='no') ,''
WRITE(*,FMT =('(2(i16))')) B1(1),B1(2)
PRINT*,' ' 

WRITE(*,FMT ='(a8)',ADVANCE='no') ,''
WRITE(*,FMT =('(4(i8))')) B2(1),B2(2),B2(3),B2(4)
PRINT*,' '

WRITE(*,FMT ='(a10)',ADVANCE='no') ,''
WRITE(*,FMT =('(8(i4))')) B3(1),B3(2),B3(3),B3(4),B3(5),B3(6),B3(7),B3(8)


PRINT*,' '



END SUBROUTINE print_tree


END MODULE AVL_mod
