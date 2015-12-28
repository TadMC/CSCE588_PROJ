!     
! File:   FortranPy.F90
! Author: TadMC
!
! Created on April 22, 2015, 6:38 AM
!





module TrainLayer
    implicit none

    type LayerObjects
        integer :: N,M,P
        real,dimension(:,:), allocatable :: DAT, W1, W2
        real,dimension(:), allocatable :: BIN, BOUT

    end type LayerObjects

    type ResArrays
        integer :: N, M, P
        real, dimension(:,:), allocatable :: AIN, AOUT, dAIN, dAOUT
        real, dimension(:,:), allocatable :: ERR, H2OE, I2HE, H, O
        real, dimension(:,:), allocatable :: DWIN, DWOUT, OLDDW1, OLDDW2
        real, dimension(:),   allocatable :: DBIN, DBOUT
    end type ResArrays


    interface InitializeLayerObjects
        module procedure Initialize
    end interface 

    interface PrintLayObj
        module procedure PrintStuff
    end interface



contains
subroutine FeedForward(LAYOBJ, RESARR)
    implicit none
    type(ResArrays), intent(inout):: RESARR
    type(LayerObjects), intent(inout):: LAYOBJ
    print *, 'FF'
    call GetAIN(LAYOBJ, RESARR)
    call GetH(RESARR)
    call GetdAIN(RESARR)
end subroutine FeedForward

subroutine Reconstruct(LAYOBJ, RESARR)
    implicit none
    type(ResArrays), intent(inout) :: RESARR
    type(LayerObjects), intent(inout):: LAYOBJ
    print *, 'REC'
    call GetAOUT(LAYOBJ, RESARR)
    call GetO(RESARR)
    call GetdAOUT(RESARR)
end subroutine Reconstruct

subroutine BackProp(LAYOBJ, RESARR)
    implicit none
    type(ResArrays), intent(inout) :: RESARR
    type(LayerObjects), intent(inout):: LAYOBJ
    print *, 'BP'
    call GetERRMSE(LAYOBJ, RESARR)
    call GetDeltas(LAYOBJ, RESARR)
    call GetDBs(LAYOBJ, RESARR)
    call GetDWs(LAYOBJ, RESARR)
end subroutine BackProp

subroutine PrintStuff(LAYOBJ)
    type(LayerObjects), intent(inout) :: LAYOBJ
    print *, shape(LAYOBJ%DAT)
    print *, shape(LAYOBJ%W1)
    print *, LAYOBJ%N, LAYOBJ%M, LAYOBJ%P
    print *, LAYOBJ%DAT(LAYOBJ%N, LAYOBJ%M)
    print *, LAYOBJ%W1(1:10,1:10)
    print *, LAYOBJ%BIN(1:10)
end subroutine PrintStuff

subroutine Initialize(D_H, D_W, NUM_HIDD, OBJS, TEMPS)
    implicit none
    integer :: D_H, D_W, NUM_HIDD, D_ID, W1_ID, W2_ID
    real, dimension(:,:), allocatable :: TEMP
    type(LayerObjects), intent(inout) :: OBJS
    type(ResArrays), intent(inout) :: TEMPS
    OBJS%N = D_H
    OBJS%M = D_W
    OBJS%P = NUM_HIDD
    TEMPS%N = D_H
    TEMPS%M = D_W
    TEMPS%P = NUM_HIDD
    allocate(TEMP(D_W, D_H))
    open(newunit=D_ID, file='Data/DATA0.txt')
    read(D_ID, *) TEMP
    close(D_ID)
    allocate(OBJS%DAT(D_H, D_W))
    OBJS%DAT = transpose(TEMP)
    deallocate(TEMP)
    allocate(OBJS%W1(NUM_HIDD, NUM_HIDD))
    open(newunit=W1_ID, file='Weights/weight1.txt')
    read(W1_ID, *) OBJS%W1
    close(W1_ID)
    allocate(OBJS%W2(NUM_HIDD, NUM_HIDD))
    open(newunit=W2_ID, file='Weights/weight2.txt')
    read(W2_ID, *) OBJS%W2
    close(W2_ID)
    allocate(OBJS%BIN(OBJS%P))
    OBJS%BIN = 0.0
    allocate(OBJS%BOUT(OBJS%M))
    OBJS%BOUT = 0.0
end subroutine Initialize

subroutine GetAIN(LAY, RES) 
    implicit none
    type(ResArrays), intent(inout) :: RES
    type(LayerObjects), intent(inout) :: LAY
    allocate(RES%AIN(RES%N,RES%P))
    print *, 'AIN'
    RES%AIN = matmul(LAY%DAT, LAY%W1)
    RES%AIN = RES%AIN + spread(LAY%BIN, dim=1, ncopies=RES%N)
end subroutine GetAIN

subroutine GetdAIN(RES)
    implicit none
    type(ResArrays), intent(inout) :: RES
    allocate(RES%dAIN(RES%N, RES%P))
    print *, 'dAIN'
    RES%dAIN = .5 * (tanh(RES%AIN*.5)+1)
    RES%dAIN = RES%dAIN*(1 - (.5*(tanh(RES%AIN*.5)+1)))
end subroutine GetdAIN

subroutine GetAOUT(LAY, RES)
    implicit none
    type(ResArrays), intent(inout) :: RES
    type(LayerObjects), intent(inout) :: LAY
    allocate(RES%AOUT(RES%N,RES%M))
    print *, 'AOUT'
    RES%AOUT = matmul(RES%H, LAY%W2) 
    RES%AOUT = RES%AOUT + spread(LAY%BOUT, dim=1, ncopies=RES%N)
end subroutine GetAOUT

subroutine GetdAOUT(RES)
    implicit none
    type(ResArrays), intent(inout) :: RES
    allocate(RES%dAOUT(RES%N, RES%M))
    print *, 'dAOUT'
    RES%dAOUT = .5 * (tanh(RES%AOUT *.5) +1)
    RES%dAOUT = RES%dAOUT*(1 - (.5*(tanh(RES%AOUT*.5)+1)))
end subroutine GetdAOUT

subroutine GetH(RES)
    implicit none
    type(ResArrays), intent(inout) :: RES
    allocate(RES%H(RES%N, RES%P))
    print *, 'H'
    RES%H = .5 * (tanh(RES%AIN *.5)+1)
end subroutine GetH

subroutine GetO(RES)
    implicit none
    type(ResArrays), intent(inout) :: RES
    allocate(RES%O(RES%N, RES%M))
    print *, 'O'
    RES%O = .5 * (tanh(RES%AOUT *.5)+1)
end subroutine GetO

subroutine GetERRMSE(LAY, RES)
    type(ResArrays), intent(inout) :: RES
    type(LayerObjects), intent(inout) :: LAY
    allocate(RES%ERR(RES%N, RES%M))
    print *, 'ERR'
    RES%ERR = - LAY%DAT - RES%O
end subroutine GetERRMSE

subroutine GetDeltas(LAY, RES)
    implicit none
    type(ResArrays), intent(inout) :: RES
    type(LayerObjects), intent(inout) :: LAY
    allocate(RES%H2OE(RES%N, RES%M))
    allocate(RES%I2HE(RES%N, RES%P))
    print *, 'DEL'
    RES%H2OE = transpose(RES%ERR * RES%dAOUT)
    RES%I2HE = matmul(transpose(transpose(LAY%W2)), RES%H2OE) * transpose(RES%dAIN)
    deallocate(RES%dAIN)
    deallocate(RES%dAOUT)
    deallocate(RES%ERR)
    deallocate(RES%AIN)
    deallocate(RES%AOUT)
    deallocate(RES%O)
end subroutine GetDeltas

subroutine GetDBs(LAY, RES)
    implicit none
    type(ResArrays), intent(inout) :: RES
    type(LayerObjects), intent(inout):: LAY
    allocate(RES%DBIN(RES%P))
    allocate(RES%DBOUT(RES%M))
    print *, 'DB'
    RES%DBIN = sum(RES%I2HE, 1)
    RES%DBOUT = sum(RES%H2OE,1)
    LAY%BIN = LAY%BIN + RES%DBIN
    LAY%BOUT = LAY%BOUT + RES%DBOUT
    deallocate(RES%DBIN)
    deallocate(RES%DBOUT)
end subroutine GetDBs


subroutine CalcMSE(LAY, RES)
    implicit none
    type(ResArrays), intent(inout) :: RES
    type(LayerObjects), intent(inout) :: LAY
    real :: MSE
    print *, 'MSE PRINT'
    call FeedForward(LAY, RES)
    call Reconstruct(LAY, RES)
    call GetERRMSE(LAY, RES)
    MSE = sum(sum(RES%ERR * RES%ERR, 1), 1)
    print *, MSE
    deallocate(RES%AIN)
    deallocate(RES%AOUT)
    deallocate(RES%dAIN)
    deallocate(RES%dAOUT)
    deallocate(RES%ERR)
    deallocate(RES%H)
    deallocate(RES%O)
end subroutine CalcMSE

subroutine GetDWs(LAY, RES)
    implicit none
    type(ResArrays), intent(inout) :: RES
    type(LayerObjects), intent(inout):: LAY
    real, dimension(:,:), allocatable :: T1, T2
    allocate(RES%DWIN(RES%M, RES%P))
    allocate(RES%DWOUT(RES%P, RES%M))
    print *, 'DW'
    RES%DWIN = transpose(matmul(RES%I2HE, transpose(transpose(LAY%DAT))))
    RES%DWOUT = transpose(matmul(RES%H2OE, transpose(transpose(RES%H))))
    if (.not.allocated(RES%OLDDW1)) then
        allocate(RES%OLDDW1(RES%M, RES%P))
        RES%OLDDW1(RES%M, RES%P) = 0.0
    end if
    if (.not.allocated(RES%OLDDW2)) then
        allocate(RES%OLDDW2(RES%P,RES%M))
        RES%OLDDW2(RES%P,RES%M) = 0.0
    end if 
    allocate(T1(RES%M, RES%P))
    allocate(T2(RES%P, RES%M))
    T1 = LAY%W1 + RES%DWIN + .0005 * RES%OLDDW1
    T2 = LAY%W2 + RES%DWOUT + .0005 * RES%OLDDW2
    LAY%W1 = T1
    LAY%W2 = T2
    deallocate(T1)
    deallocate(T2)
    RES%OLDDW1 = RES%DWIN
    RES%OLDDW2 = RES%DWOUT
    deallocate(RES%DWIN)
    deallocate(RES%DWOUT)
    deallocate(RES%H)
    deallocate(RES%H2OE)
    deallocate(RES%I2HE)
end subroutine GetDWs


subroutine Train(LAY, RES, N, M, P, MAX_EPS)
    implicit none
    integer :: N, M, P, MAX_EPS, i
    type(LayerObjects), intent(inout) :: LAY
    type(ResArrays), intent(inout) :: RES

    call InitializeLayerObjects(N,M,P, LAY, RES)
    call PrintLayObj(LAY)
    do i = 1, MAX_EPS
        print *, i
        call FeedForward(LAY, RES)
        call Reconstruct(LAY, RES)
        call BackProp(LAY, RES)
        call PrintLayObj(LAY)
        call CalcMSE(LAY, RES)
    enddo     
    
    call PrintLayObj(LAY)
end subroutine Train

end module TrainLayer



program main
    use TrainLayer
    implicit none
    integer :: N,M,P, WFID1, WFID2, i, MAX_EPS
    type(LayerObjects)      :: LAY
    type(ResArrays)         :: RES

    call Train(LAY, RES, 1000,784,784,100)

end program main
