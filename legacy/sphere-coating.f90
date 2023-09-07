program main
        implicit none
    integer,parameter :: Nx = 100, Ny = 100, Nz = 100
    integer i,j,k,l,ii,jj,kk,m,n,p,q,r,particle_num,ns,ntry,xi,xj,xk,nCoat,nflag
    real*8 rr,d0,rr0,vCoat,hCoat
    real*8 sphere(4,10000) !the first 4 describes the coordinate and raidus, 15 means 15 spheres, 8x8x8 means 8x8x8 cells
    real*8 rd(30000000),rd2(10*Nx*Ny*Nz)
    real*8 dd(nx,ny,nz)
    integer phaseCoat(nx,ny,nz),phasePolymer(nx,ny,nz),phaseCeramic(nx,ny,nz),phaseID(nx,ny,nz)
    real*8 sphere0(4),vfCoat,vfPolymer,vfCeramic

    call random_seed()
    call random_number(rd)
    call random_number(rd2)

    open(unit = 3, file = 'structure.dat')
    open(unit = 4, file = 'struct.in')
    write(1,*)Nx,Ny,Nz
    write(3,*)Nx,Ny,Nz
    write(4,*)Nx,Ny,Nz

phaseCoat(:,:,:) = 0
phasePolymer(:,:,:) = 1
phaseCeramic(:,:,:) = 0

    !phaseID = 1 for polymer phase, phaseID = 2 for Coating layer, phaseID = 3 for Ceramic particle

    rr0 = 6  !radius of ceramics particle
    hCoat = rr0*0.1 !thickness of the coating layer
    particle_num = 800 !total particle number
    ns = 1
    ntry = 1

!   call create_random(sphere0)
    sphere(1,1) = rd(1)*(nx-0*rr) !!+rr
    sphere(2,1) = rd(2)*(ny-0*rr) !+rr
    sphere(3,1) = rd(3)*(nz-0*rr) !+rr
    sphere(4,1) = rr0+3*rd2(1)

111 if(ns >= particle_num) goto 112
        print*,'ns = ',ns,'x = ',sphere(1,ns),'y = ',sphere(2,ns),'z = ',sphere(3,ns),'ntry = ',ntry
!       call create_random(sphere0)
        sphere0(1) = rd(1+3*ntry)*(nx-0*rr) !+rr
        sphere0(2) = rd(2+3*ntry)*(ny-0*rr) !+rr
        sphere0(3) = rd(3+3*ntry)*(nz-0*rr) !+rr
        sphere0(4) = rr0+3*rd2(1+3*ntry)
        ntry = ntry + 1
        nflag = 0
        do i=1,ns
           d0 = dsqrt((sphere0(1)-sphere(1,i))**2 + (sphere0(2)-sphere(2,i))**2 + (sphere0(3)-sphere(3,i))**2)
           if(d0 < sphere0(4) + sphere(4,i) + 2*hCoat) then
               goto 111
!          elseif((d0 >=sphere0(4) +  sphere(4,i)+0.01).and.(d0<=sphere0(4)+sphere(4,i)+1.0)) then
!              nflag = nflag + 1
           endif
           if(ns>30.or.ntry>=29999900) goto 112
        enddo
!       ntry = 0
        ns = ns + 1
        sphere(1,ns) = sphere0(1)
        sphere(2,ns) = sphere0(2)
        sphere(3,ns) = sphere0(3)
        sphere(4,ns) = sphere0(4)
goto 111


112  do i=1,Nx
        do j=1,Ny
            do k=1,Nz
                do m=1,particle_num
                     dd(i,j,k) = dsqrt((i-sphere(1,m))**2+(j-sphere(2,m))**2+(k-sphere(3,m))**2)
                     if(dd(i,j,k)<=sphere(4,m)) then
                         phaseCoat(i,j,k) = 0
                         phasePolymer(i,j,k) = 0
                         phaseCeramic(i,j,k) = 1
                     elseif(dd(i,j,k)>sphere(4,m).and.dd(i,j,k)<=sphere(4,m)+hCoat) then
                         phaseCoat(i,j,k) = 1
                         phasePolymer(i,j,k) = 0
                         phaseCeramic(i,j,k) = 0
                     endif
                enddo
            enddo
        enddo
    enddo

goto 222

!Expand the volume by 8 times
!volume 1
114     do i=1,Nx
                do j=1,Ny
                        do k=1,Nz
                                phaseCoat(i,j,k) = phaseCoat(i,j,k)
                                phasePolymer(i,j,k) = phasePolymer(i,j,k)
                                phaseCeramic(i,j,k) = phaseCeramic(i,j,k)
                        enddo
                enddo
        enddo
            
!volume 2
        do i=Nx+1,2*Nx
                do j=1,Ny
                        do k=1,Nz
                                phaseCoat(i,j,k) = phaseCoat(i-Nx,j,k)
                                phasePolymer(i,j,k) = phasePolymer(i-Nx,j,k)
                                phaseCeramic(i,j,k) = phaseCeramic(i-Nx,j,k)
                        enddo
                enddo
        enddo
            
!volume 3
        do i=1,Nx
                do j=Ny+1,2*Ny
                        do k=1,Nz
                                phaseCoat(i,j,k) = phaseCoat(i,j-Ny,k)
                                phasePolymer(i,j,k) = phasePolymer(i,j-Ny,k)
                                phaseCeramic(i,j,k) = phaseCeramic(i,j-Ny,k)
                        enddo
                enddo
        enddo
            
!volume 4
        do i=1,Nx
                do j=1,Ny
                        do k=Nz+1,2*Nz
                                phaseCoat(i,j,k) = phaseCoat(i,j,k-Nz)
                                phasePolymer(i,j,k) = phasePolymer(i,j,k-Nz)
                                phaseCeramic(i,j,k) = phaseCeramic(i,j,k-Nz)
                        enddo
                enddo
        enddo
            
!volume 5
        do i=Nx+1,2*Nx
                do j=Ny+1,2*Ny
                        do k=1,Nz
                                phaseCoat(i,j,k) = phaseCoat(i-Nx,j-Ny,k)
                                phasePolymer(i,j,k) = phasePolymer(i-Nx,j-Ny,k)
                                phaseCeramic(i,j,k) = phaseCeramic(i-Nx,j-Ny,k)
                        enddo
                enddo
        enddo
            
!volume 6
        do i=Nx+1,2*Nx
                do j=1,Ny
                        do k=Nz+1,2*Nz
                                phaseCoat(i,j,k) = phaseCoat(i-Nx,j,k-Nz)
                                phasePolymer(i,j,k) = phasePolymer(i-Nx,j,k-Nz)
                                phaseCeramic(i,j,k) = phaseCeramic(i-Nx,j,k-Nz)
                        enddo
                enddo
        enddo
            
!volume 7
        do i=1,Nx
                do j=Ny+1,2*Ny
                        do k=Nz+1,2*Nz
                                phaseCoat(i,j,k) = phaseCoat(i,j-Ny,k-Nz)
                                phasePolymer(i,j,k) = phasePolymer(i,j-Ny,k-Nz)
                                phaseCeramic(i,j,k) = phaseCeramic(i,j-Ny,k-Nz)
                        enddo
                enddo
        enddo
            
!volume 8
        do i=Nx+1,2*Nx
                do j=Ny+1,2*Ny
                        do k=Nz+1,2*Nz
                                phaseCoat(i,j,k) = phaseCoat(i-Nx,j-Ny,k-Nz)
                                phasePolymer(i,j,k) = phasePolymer(i-Nx,j-Ny,k-Nz)
                                phaseCeramic(i,j,k) = phaseCeramic(i-Nx,j-Ny,k-Nz)
                        enddo
                enddo
        enddo
            

222 vfCoat = 0.0
    vfPolymer = 0.0
    vfCeramic = 0.0
    do i=1,Nx 
        do j=1,Ny 
            do k=1,Nz
                if(phaseCoat(i,j,k) == 1) then
                        vfCoat = vfCoat + 1.0d0
                        phaseID(i,j,k) = 2
                elseif(phasePolymer(i,j,k) == 1) then
                        vfPolymer = vfPolymer + 1.0d0
                        phaseID(i,j,k) = 1
                elseif(phaseCeramic(i,j,k) == 1) then
                        vfCeramic = vfCeramic + 1.0d0
                        phaseID(i,j,k) = 3
                else
                endif
                write(3,*)i,j,k,phaseCoat(i,j,k),phasePolymer(i,j,k),phaseCeramic(i,j,k)
                write(4,*)i,j,k,phaseID(i,j,k)
            enddo
        enddo
    enddo
    vfCoat = vfCoat/dble(Nx*Ny*Nz)
    vfPolymer = vfPolymer/dble(Nx*Ny*Nz)
    vfCeramic = vfCeramic/dble(Nx*Ny*Nz)
    print*, 'VfCoat = ',vfCoat
    print*, 'VfPolymer = ',vfPolymer
    print*, 'VfCeramic = ',vfCeramic
    print*, 'VfCoat + VfPolymer + VfPolymer = ',vfCoat+vfPolymer+VfCeramic
    close(3)
    close(4)
        

end


    SUBROUTINE create_random(sphere0)
    INTEGER :: clock,i,n
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
    REAL*8 :: sphere0(4)

    CALL RANDOM_SEED(size = n)

    ALLOCATE(seed(n))

    CALL SYSTEM_CLOCK(count = clock)
       
    seed = clock + 100*(/(i-1, i=1, 4)/)

    CALL RANDOM_SEED(put = seed)

    CALL RANDOM_NUMBER(sphere0)
              
    DEALLOCATE(seed)

    END SUBROUTINE
