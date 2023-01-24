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
    INTEGER :: counter, keys(N), a,i    

    ! clear temp pointers
    T => null()
    P => null()
    S => null()
    Q => null()
    R => null()

    counter =  0
    a = 0

    DO i=1,N-1
        keys(i) = i+1
    END DO

    ! Allocate memory for root node
    ALLOCATE(head%node_R)
    
    ! set root balance factor
    head%node_R%bf = 0
    ! set root node rank
    head%node_R%rank = 1
    ! set root node key
    head%node_R%key = 1

    ! insert N-1 nodes into the tree
    DO WHILE(counter .LT. N-1)

        T => head
 
        !CALL print_tree(head)

        !PRINT*,'Inserting new node with Rank, key =',counter+2, keys(counter+1)


        CALL insert_node(T,counter+2, keys(counter+1))

        
        !PRINT*,'Finished inserting new node.'

        !CALL print_tree(head)

               
        counter = counter + 1
    END DO



END SUBROUTINE tree_init


! Routine for a standard Binary Search Tree node insertion
!    Note: Balance factors are adjuated along the way to the insertion location
!   (Only need to update balance factor for sub-trees that lie on the traversal path).

SUBROUTINE insert_node(T,k,key)

    TYPE(node), POINTER, INTENT(INOUT) :: T
    INTEGER, INTENT(IN) ::  k, key ! k=N+1 (where N = # number of nodes in tree)
    
    TYPE(node), POINTER :: S,P,Q,R, head
    INTEGER :: M, U


    head => T

    ! Reset temp pointers
    R => null()
    S => T%node_R
    P => T%node_R
    Q => null()

    U = k   
    M = k


    M = M - P%rank
    R => P%node_R
    
    ! Traverse to insert location (insert is going to be the right-most leaf)

    DO WHILE(ASSOCIATED(R))
    
        ! advance temp pointers
        IF(R%bf .NE. 0) THEN
            T => P
            S => R
            U = M
        END IF
      
        P => R
        M = M - P%rank
        R => P%node_R    

    END DO

    ! allocate memory for the new node
    ALLOCATE(Q)
    ! link newly created node
    P%node_R => Q
    ! set rank and balance factor
    Q%rank = 1
    Q%bf = 0
    Q%key = key
    

    M = U

    
    !PRINT*,'Node inserted.'
    !CALL print_tree(head)
  
    !update balance factors (only if the tree was initially non-empty)
	IF(k .GT. 1) THEN
		CALL adjust_bf(T,S,P,Q,R,U)
	END IF

END SUBROUTINE insert_node



! this subroutine adjusts the balance factors between nodes S and Q after an insert

SUBROUTINE adjust_bf(T,S,P,Q,R,U)

    TYPE(node), POINTER, INTENT(INOUT) :: T,S,P,Q,R
    INTEGER, INTENT(IN) :: U

    INTEGER :: M, a

    !PRINT*,'Updating balance factors...'


    M = U

    !IF(M .LT. S%rank) THEN
    !    R => S%node_L
    !    P => S%node_L
    !    a = -1
    !ELSE
        R => S%node_R
        P => S%node_R
        a = 1 
    !END IF

    !PRINT*,'S,R,P,M=',S%key,R%key,P%key,M

    !PRINT*,'Rank(S),Rank(P)=',S%rank,P%rank

    DO WHILE(.NOT. ASSOCIATED(P,Q)) ! loop exits when P=Q  
        !IF(M .LT. P%rank) THEN
        !    P%bf = -1              
        !    P => P%node_L
        !ELSE
            P%bf = 1       
            M = M - P%rank
            P => P%node_R
        !END IF
    END DO

    !PRINT*,'M,Rank(P)=',M,P%rank

    CALL check_balance_insert(T,S,P,Q,R,U,a)

END SUBROUTINE adjust_bf

! S points to imbalanced node
! R points to sub-tree of S in which the insertion occured
! T points to the parent of S
! P, Q both point to the newly inserted node 
SUBROUTINE check_balance_insert(T,S,P,Q,R,U,a)

    TYPE(node), POINTER, INTENT(INOUT) :: T,S,P,Q,R
    INTEGER, INTENT(IN) :: U,a

    !PRINT*,'Checking for imbalance...'

    ! no imbalances 
    IF(S%bf .EQ. 0) THEN
        S%bf = a
    ELSE IF(S%bf .EQ. -a) THEN
        S%bf = 0
    ! imbalance
    ELSE IF (S%bf .EQ. a) THEN
        
       ! PRINT*,'Found imbalance! Rebalancing sub-tree...'

        CALL rebalance_insert(T,S,P,Q,R,U,a)
    END IF

END SUBROUTINE check_balance_insert


SUBROUTINE rebalance_insert(T,S,P,Q,R,U,a)

    TYPE(node), POINTER, INTENT(INOUT) :: T,S,P,Q,R
    INTEGER, INTENT(IN) :: U,a

    ! Case 1: Perform single rotation
    !IF(R%bf .EQ. a) THEN
        !PRINT*,'Case 1. Performing single rotation.'

        P => R        
        !IF(a .EQ. 1)THEN
            S%node_R => R%node_L
            R%node_L => S 
        !ELSE
        !    S%node_L => R%node_R
        !    R%node_R => S
        !END IF

        ! update balance factors and ranks
        S%bf = 0
        R%bf = 0
        !IF(a .EQ. 1) THEN
            R%rank = R%rank + S%rank
        !ELSE
        !    S%rank = S%rank - R%rank
        !END IF

    ! Case 2:  Perform double rotation
    !ELSE IF(R%bf .EQ. -a) THEN
    !    !PRINT*,'Case 2. Performing double rotation.'

    !   IF(a .EQ. 1) THEN
    !        P => R%node_L
    !        R%node_L => P%node_R 
    !        P%node_R => R
    !        S%node_R => P%node_L
    !        P%node_L => S
    !    ELSE IF(a .EQ. -1)THEN
    !        P => R%node_R
    !        R%node_R => P%node_L 
    !        P%node_L => R
    !        S%node_L => P%node_R
    !        P%node_R => S
    !    END IF

        ! update balance factors
     !   IF(P%bf .EQ. a) THEN
     !       S%bf = -a
     !       R%bf = 0
     !   ELSE IF(P%bf .EQ. 0) THEN
     !       S%bf = 0
     !       R%bf = 0
     !   ELSE IF(P%bf .EQ. -a) THEN
     !       S%bf = 0
     !       R%bf = a
     !   END IF
     !   P%bf = 0

        ! update ranks
    !    IF(a .EQ. 1) THEN
    !        R%rank = R%rank - P%rank
    !        P%rank = P%rank + S%rank
    !    ELSE
    !        P%rank = P%rank + R%rank
    !        S%rank = S%rank - P%rank
    !    END IF

    !END IF


    ! Assign new sub-tree root
   ! IF(ASSOCIATED(S,T%node_R)) THEN
        T%node_R => P
   ! ELSE IF(ASSOCIATED(S,T%node_L)) THEN
   !     T%node_L => P
   ! END IF


END SUBROUTINE rebalance_insert



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
!                          1) Find the left-most node (i.e. the successor node) from the right sub-tree of P_l. Add 
!                             pointers P_l+k (k=1,2,...) to all nodes along the path.
!                             Once found, the successor node is assigned as the new delete node P_l+k (k= # of steps
!                             to get to successor). Copy contents of this P_l+k to P_l, then delete P_l+k using CASE 1 rules.                                 
!
!


SUBROUTINE delete_node(head,k,N)

    TYPE(node), POINTER, INTENT(IN) :: head
    INTEGER, INTENT(IN) :: k
    INTEGER, INTENT(INOUT) :: N

    TYPE(node), POINTER:: T,S,P,Q,R  
    TYPE(path_ptr), POINTER:: p0,current, temp
    INTEGER :: M, a

    ! initialize temp pointers
    T => null()
    S => null()
    P => head
    Q => head%node_R    
    R => null()
    a = 1

    M = k

    ALLOCATE(p0)
    p0%node => P
    p0%a = 1
    IF(ASSOCIATED(P%node_R)) THEN
        ALLOCATE(p0%next)
        current => p0%next
        current%prev => p0
    END IF

    !PRINT*,'p0,Q=',p0%node%key,Q%key

    P => Q

    ! search for delete node
    DO WHILE(M .NE. P%rank)

        ! Go right if key is greater
        IF(M .GT. P%rank) THEN
            
            Q => P%node_R
            a = 1
            M = M - P%rank
              
        ! Go left if key is smaller
        ELSE IF(M .LT. P%rank) THEN

            Q => P%node_L
            a = -1

            ! update rank of predecessor node after every left turn
            P%rank = P%rank-1

        END IF

        current%node => P     
        current%a = a
        ALLOCATE(current%next)
        current%next%prev => current
        current => current%next

        P => Q

    END DO

    !PRINT*,'Found delete node: ', Q%key

    T => Q  ! Q and T both point to the node that will get deleted

    current%node => Q
   
    ! Case 1: Q is a leaf node
    IF(.NOT. ASSOCIATED(T%node_L) .AND. .NOT. ASSOCIATED(T%node_R)) THEN
        !PRINT*,'Case 1.'
               
        ! point T at the deletion node
        T => Q
        IF(a .EQ. -1)THEN
            current%prev%node%node_L => null()
        ELSE IF(a .EQ. 1)THEN
            current%prev%node%node_R => null()
        END IF

    ! Case 2: Q has empty right node
    !         Choose left node as sucecssor
    ELSE IF(.NOT. ASSOCIATED(T%node_R)) THEN
        !PRINT*,'Case 2.'
 
        R => T%node_L
      
        ! copy contents of successor node into the original deletion node
        Q%key = R%key
        Q%bf = R%bf
        Q%rank = R%rank !******************
        Q%node_R => R%node_R
        Q%node_L => R%node_L

        ! point T at the successor node
        T => R

        current%a = -a         
       
    ! Case 3: Q has empty left node
    ELSE IF(.NOT. ASSOCIATED(T%node_L)) THEN
        !PRINT*,'Case 3.'


        R => T%node_R
      
        ! copy contents of successor node into the original deletion node
        Q%key = R%key
        Q%bf = R%bf
        Q%rank = R%rank !*********
        Q%node_R => R%node_R
        Q%node_L => R%node_L

        ! point T at the successor node
        T => R

        current%a = -a
       
    ! Case 4: Q has non-empty left and right nodes. 
    ! Find a successor node.
    ELSE

        !PRINT*,'Case 4.'
        ! find a successor node from the right sub-tree of Q
        ! (i.e. a node on the left branch of right sub-tree of Q, that has only one or no child)
        R => T%node_R

        current%a = 1
        ALLOCATE(current%next)
        current%next%prev => current
        current => current%next
        current%node => R

        ! check for null left sub-tree of R 
        ! If null, then choose right node of R as the successor
        IF(.NOT. ASSOCIATED(R%node_L)) THEN

            ! copy contents of successor node into the original deletion node
            ! (leave rank unchanged)
            Q%key = R%key
            Q%node_R => R%node_R

            ! point T at the successor node
            T => R

            current%a = -1

        ! Otherwise, find successor from left sub-tree of R
        ELSE
            ! traverse to nearest left node with a non-empty subtree
            S => R%node_L

            current%a = -1
            ALLOCATE(current%next)
            current%next%prev => current
            current => current%next
            current%node => S  

            ! update rank of R
            R%rank = R%rank -1            

            DO WHILE(ASSOCIATED(S%node_L))
                R => S
                S => S%node_L

                current%a = -1
                ALLOCATE(current%next)
                current%next%prev => current
                current => current%next
                current%node => S                  

                ! update rank of R
                R%rank = R%rank -1            

            END DO       
 
            R%node_L => S%node_R

            ! copy contents of successor node into the original deletion node
            Q%key = S%key
             
            ! point T at the successor node
            T => S

            current%a = -1

        END IF

    END IF


   ! temp => current
   ! PRINT*,'Deletion path backtrace:'
   ! PRINT*,temp%node%key,temp%a
   ! DO WHILE(ASSOCIATED(temp%prev))
   !     temp => temp%prev
   !     PRINT*,temp%node%key,temp%a
   ! END DO

    ! Destroy deleted node

    DEALLOCATE(T)

    N = N - 1

    !CALL print_tree(p0%node)


    !PRINT*,'Node has been deleted.'

    ! check balance    
    CALL check_balance_delete(current,p0)

    !CALL print_tree(p0%node)

    ! Destroy temp pointers
    current => p0
    DO WHILE(ASSOCIATED(current%next))
        current => current%next
        DEALLOCATE(current%prev)
    END DO
    DEALLOCATE(current)    


END SUBROUTINE delete_node



SUBROUTINE check_balance_delete(P_k, p0)

    TYPE(path_ptr), POINTER, INTENT(INOUT):: P_k, p0

    TYPE(node), POINTER :: T,S,P,R
    LOGICAL :: CASE3_FLAG 

    ! clear temp pointers
    T => null()
    P => null()
    R => null()
    S => null()
 
    ! reset case 3 flag
    CASE3_FLAG = .FALSE.


    ! Adjust balance factors and ranks and rebalance if an unbalance node is found.
    ! Bottom up, starting from P_l-1 ... P_1
    
    !PRINT*,'Checking balance. P_l =', P_k%node%key
   
    P_k => P_k%prev
    DO WHILE(ASSOCIATED(P_k%prev) .AND. .NOT. CASE3_FLAG)

        !PRINT*,'Checking node ',P_k%node%key
        !PRINT*,'B(P_k),P_k%a=',P_k%node%bf,P_k%a

        IF(P_k%node%bf .EQ. P_k%a) THEN
            P_k%node%bf = 0

        ELSE IF(P_k%node%bf .EQ. 0 ) THEN
            P_k%node%bf = -P_k%a
            EXIT

        ELSE IF(P_k%node%bf .EQ. -P_k%a) THEN
            !PRINT*,'Node is out of balance.'

            ! set temp pointers
            ! S points to imbalanced node
            ! R points to opposite sub-tree of S to where th deletion occured
            ! T points to the parent of S
            
            S => P_k%node
            IF(P_k%a .EQ. 1) THEN
                R => S%node_L
            ELSE IF(P_k%a .EQ. -1) THEN
                R => S%node_R
            END IF
            T => P_k%prev%node

            !PRINT*,'S,R,T=',S%key,R%key,T%key

            CALL rebalance_delete(T,S,R,P,P_k%a, CASE3_FLAG) 

            ! clear temp pointers
            T => null()
            P => null()
            R => null()
            S => null()

            !PRINT*,'Node has been balanced.'

        END IF

        !CALL print_tree(p0%node)

        P_k => P_k%prev

        !PRINT*,'Moving on to P_k-1=',P_k%node%key

    END DO


END SUBROUTINE check_balance_delete



SUBROUTINE rebalance_delete(T,S,R,P,a,CASE3_FLAG)

    TYPE(node), POINTER, INTENT(INOUT) :: T,S,R,P
    INTEGER, INTENT(IN) :: a
    LOGICAL, INTENT(INOUT) :: CASE3_FLAG

    ! Case 1: Perform single rotation
    IF(R%bf .EQ. -a) THEN
        !PRINT*,'Case 1. Performing single rotation.'
        P => R
        IF(a .EQ. -1) THEN
            S%node_R => R%node_L
            R%node_L => S 
        ELSE IF(a .EQ. 1)THEN
            S%node_L => R%node_R
            R%node_R => S
        END IF

        ! update balance factors
        S%bf = 0
        R%bf = 0

        ! update ranks
        IF(a .EQ. -1) THEN
            R%rank = R%rank + S%rank
        ELSE
            S%rank = S%rank - R%rank
        END IF


    ! Case 2: Perform double rotation
    ELSE IF(R%bf .EQ. a) THEN
        !PRINT*,'Case 2. Performing double rotation.'

        !PRINT*,'T,S,R=',T%key,S%key,R%key

        IF(a .EQ. -1) THEN
            P => R%node_L
            R%node_L => P%node_R 
            P%node_R => R
            S%node_R => P%node_L
            P%node_L => S
        ELSE IF(a .EQ. 1)THEN
            P => R%node_R
            R%node_R => P%node_L 
            P%node_L => R
            S%node_L => P%node_R
            P%node_R => S
        END IF

        IF(P%bf .EQ. -a) THEN
            S%bf = a
            R%bf = 0
        ELSE IF(P%bf .EQ. 0) THEN
            S%bf = 0
            R%bf = 0
        ELSE IF(P%bf .EQ. a) THEN
            S%bf = 0
            R%bf = -a
        END IF

        P%bf = 0

        ! update ranks
        IF(a .EQ. -1) THEN
            R%rank = R%rank - P%rank
            P%rank = P%rank + S%rank
        ELSE
            P%rank = P%rank + R%rank
            S%rank = S%rank - P%rank
        END IF

    ELSE IF(R%bf .EQ. 0) THEN
    ! Case 3: Sub-tree balanced
    !         Perform single rotation (same as Case 1, except balance factors of A,B remain unchanged)
    !         Rebalancing sequence needs to be terminated for Case 3.
        !PRINT*,'Case 3. Performing single rotation.'

        P => R

        !PRINT*,'a=',a

        IF(a .EQ. -1) THEN
            S%node_R => R%node_L
            R%node_L => S 
        ELSE IF(a .EQ. 1)THEN
            S%node_L => R%node_R
            R%node_R => S
        END IF

        ! update balance factors
        R%bf = a

        ! update ranks
        IF(a .EQ. -1) THEN
            R%rank = R%rank + S%rank
        ELSE
            S%rank = S%rank - R%rank
        END IF

        CASE3_FLAG = .TRUE.

    END IF

    ! Assign new sub-tree root
    IF(ASSOCIATED(S,T%node_R)) THEN
        T%node_R => P
    ELSE IF(ASSOCIATED(S,T%node_L)) THEN
        T%node_L => P
    END IF

    !PRINT*,'Rebalancing completed.'

END SUBROUTINE rebalance_delete






SUBROUTINE print_tree(head)

TYPE(node), POINTER, INTENT(IN) :: head

TYPE(node_ptr) :: temp0, temp1(2), temp2(4), temp3(8), temp4(16), temp5(32), temp6(64) 

INTEGER :: L0, L1(2), L2(4), L3(8), L4(16), L5(32), L6(64)
INTEGER :: B0, B1(2), B2(4), B3(8), B4(16), B5(32), B6(64) 
INTEGER :: R0, R1(2), R2(4), R3(8), R4(16), R5(32), R6(64) 
INTEGER :: i,j,k

PRINT*,' '
PRINT*,' '

L0 = 0
L1 = 0
L2 = 0
L3 = 0 
L4 = 0
L5 = 0
L6 = 0 
B0 = 0
B1 = 0
B2 = 0
B3 = 0 
B4 = 0
B5 = 0
B6 = 0 
R0 = 0
R1 = 0
R2 = 0
R3 = 0 
R4 = 0
R5 = 0 
R6 = 0

temp0%p => head%node_R
IF(ASSOCIATED(temp0%p)) THEN
    L0 = temp0%p%key
    B0 = temp0%p%bf
    R0 = temp0%p%rank
    temp1(1)%p => temp0%p%node_L
    temp1(2)%p => temp0%p%node_R
ELSE
    temp1(1)%p => null()
    temp1(2)%p => null()
END IF


! Get Tree Level 1 values
DO i = 1,2
    IF(ASSOCIATED(temp1(i)%p))THEN 
        L1(i) = temp1(i)%p%key
        B1(i) = temp1(i)%p%bf
        R1(i) = temp1(i)%p%rank
    END IF
END DO


! Get Tree Level 2 values
DO i = 1,4
    j = MOD(i,2)+i/2
    temp2(i)%p => null()
    IF(MOD(i,2) .NE. 0)THEN
        IF(ASSOCIATED(temp1(j)%p))THEN
            temp2(i)%p => temp1(j)%p%node_L
        END IF
    ELSE
        IF(ASSOCIATED(temp1(j)%p))THEN
            temp2(i)%p => temp1(j)%p%node_R
        END IF
    END IF

    IF(ASSOCIATED(temp2(i)%p))THEN
        L2(i) = temp2(i)%p%key
        B2(i) = temp2(i)%p%bf
        R2(i) = temp2(i)%p%rank            
    END IF
    
END DO


! Get Tree Level 3 values
DO i = 1,8
    j = MOD(i,2)+i/2
    temp3(i)%p => null()
    IF(MOD(i,2) .NE. 0)THEN
        IF(ASSOCIATED(temp2(j)%p)) THEN
            temp3(i)%p => temp2(j)%p%node_L
        END IF
    ELSE
        IF(ASSOCIATED(temp2(j)%p)) THEN
            temp3(i)%p => temp2(j)%p%node_R
        END IF    
    END IF

    IF(ASSOCIATED(temp3(i)%p))THEN
        L3(i) = temp3(i)%p%key
        B3(i) = temp3(i)%p%bf  
        R3(i) = temp3(i)%p%rank                     
    END IF
    
END DO


! Get Tree Level 4 values
DO i = 1, 16
    j = MOD(i,2)+i/2
    temp4(i)%p => null()
    IF(MOD(i,2) .NE. 0)THEN
        IF(ASSOCIATED(temp3(j)%p)) THEN
            temp4(i)%p => temp3(j)%p%node_L
        END IF
    ELSE
        IF(ASSOCIATED(temp3(j)%p)) THEN
            temp4(i)%p => temp3(j)%p%node_R
        END IF    
    END IF

    IF(ASSOCIATED(temp4(i)%p))THEN
        L4(i) = temp4(i)%p%key
        B4(i) = temp4(i)%p%bf                 
        R4(i) = temp4(i)%p%rank                 
    END IF
    
END DO

! Get Tree Level 5 values
DO i = 1, 32
    j = MOD(i,2)+i/2
    temp5(i)%p => null()
    IF(MOD(i,2) .NE. 0)THEN
        IF(ASSOCIATED(temp4(j)%p)) THEN
            temp5(i)%p => temp4(j)%p%node_L
        END IF
    ELSE
        IF(ASSOCIATED(temp4(j)%p)) THEN
            temp5(i)%p => temp4(j)%p%node_R
        END IF    
    END IF

    IF(ASSOCIATED(temp5(i)%p))THEN
        L5(i) = temp5(i)%p%key
        B5(i) = temp5(i)%p%bf
        R5(i) = temp5(i)%p%rank                  
    END IF
    
END DO

! Get Tree Level 6 values
DO i = 1, 64
    j = MOD(i,2)+i/2
    temp6(i)%p => null()

    IF(MOD(i,2) .NE. 0)THEN
        IF(ASSOCIATED(temp5(j)%p)) THEN
            temp6(i)%p => temp5(j)%p%node_L
        END IF
    ELSE
        IF(ASSOCIATED(temp5(j)%p)) THEN
            temp6(i)%p => temp5(j)%p%node_R
        END IF    
    END IF

    IF(ASSOCIATED(temp6(i)%p))THEN
        L6(i) = temp6(i)%p%key
        B6(i) = temp6(i)%p%bf
        R6(i) = temp6(i)%p%rank       
    END IF
    
END DO



PRINT*,'Keys:'
WRITE(*,FMT ='(a60)',ADVANCE='no') ,''
WRITE(*,FMT =('(i5)')) L0
PRINT*,' '
PRINT*,' '

!WRITE(*,FMT ='(a1)',ADVANCE='no') ,''
DO i= 1,2
    WRITE(*,FMT =('(i44)'),ADVANCE='no') L1(i)
END DO
PRINT*,' '
PRINT*,' ' 

WRITE(*,FMT ='(a7)',ADVANCE='no') ,''
DO i=1,4
    WRITE(*,FMT =('(i24)'),ADVANCE='no') L2(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a12)',ADVANCE='no') ,''
DO i = 1,8  
    WRITE(*,FMT =('(i12)'),ADVANCE='no') L3(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a14)',ADVANCE='no') ,''
DO i = 1,16  
    WRITE(*,FMT =('(i6)'),ADVANCE='no') L4(i)
END DO
PRINT*,' '
PRINT*,' '


WRITE(*,FMT ='(a15)',ADVANCE='no') ,''
DO i = 1,32  
    WRITE(*,FMT =('(i3)'),ADVANCE='no') L5(i)
END DO
PRINT*,' '
PRINT*,' '
PRINT*,' '


DO i = 1,64  
    WRITE(*,FMT =('(i3)'),ADVANCE='no') L6(i)
END DO
PRINT*,' '
PRINT*,' '
PRINT*,' '


!************************************************
PRINT*,'Ranks:'
WRITE(*,FMT ='(a95)',ADVANCE='no') ,''
WRITE(*,FMT =('(i3)')) R0
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a2)',ADVANCE='no') ,''
DO i= 1,2
    WRITE(*,FMT =('(i64)'),ADVANCE='no') R1(i)
END DO
PRINT*,' ' 
PRINT*,' '

WRITE(*,FMT ='(a17)',ADVANCE='no') ,''
DO i=1,4
    WRITE(*,FMT =('(i32)'),ADVANCE='no') R2(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a25)',ADVANCE='no') ,''
DO i = 1,8  
    WRITE(*,FMT =('(i16)'),ADVANCE='no') R3(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a29)',ADVANCE='no') ,''
DO i = 1,16  
    WRITE(*,FMT =('(i8)'),ADVANCE='no') R4(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a31)',ADVANCE='no') ,''
DO i = 1,32  
    WRITE(*,FMT =('(i4)'),ADVANCE='no') R5(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a32)',ADVANCE='no') ,''
DO i = 1,64  
    WRITE(*,FMT =('(i2)'),ADVANCE='no') R6(i)
END DO
PRINT*,' '
PRINT*,' '
PRINT*,' '



!************************************************
PRINT*,'Balance Factors:'
WRITE(*,FMT ='(a95)',ADVANCE='no') ,''
WRITE(*,FMT =('(i3)')) B0
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a2)',ADVANCE='no') ,''
DO i= 1,2
    WRITE(*,FMT =('(i64)'),ADVANCE='no') B1(i)
END DO
PRINT*,' ' 
PRINT*,' '

WRITE(*,FMT ='(a17)',ADVANCE='no') ,''
DO i=1,4
    WRITE(*,FMT =('(i32)'),ADVANCE='no') B2(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a25)',ADVANCE='no') ,''
DO i = 1,8  
    WRITE(*,FMT =('(i16)'),ADVANCE='no') B3(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a29)',ADVANCE='no') ,''
DO i = 1,16  
    WRITE(*,FMT =('(i8)'),ADVANCE='no') B4(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a31)',ADVANCE='no') ,''
DO i = 1,32  
    WRITE(*,FMT =('(i4)'),ADVANCE='no') B5(i)
END DO
PRINT*,' '
PRINT*,' '

WRITE(*,FMT ='(a32)',ADVANCE='no') ,''
DO i = 1,64  
    WRITE(*,FMT =('(i2)'),ADVANCE='no') B6(i)
END DO
PRINT*,' '
PRINT*,' '
PRINT*,' '


END SUBROUTINE print_tree



END MODULE AVL_mod
