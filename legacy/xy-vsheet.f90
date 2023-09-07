Program main
IMPLICIT NONE

INTEGER :: ii,jj,kk,ic,nn,particle_num1,h,mm,i
REAL*8 :: circle(4),circles(3000,4),dd,ddx,ddy,ddz,x,y,z,hsheet,vCoat,hCoat
REAL*8 :: vfCoat,vfPolymer,vfCeramic,Rx,Ry
INTEGER, PARAMETER :: nx = 128, ny = 128, nz = 128
INTEGER :: R,phaseCoat(nx,ny,nz),phasePolymer(nx,ny,nz),phaseCeramic(nx,ny,nz),phaseID(nx,ny,nz)
REAL*8,allocatable :: phi(:,:,:),phip(:,:,:),phim(:,:,:)

allocate (phi(nx,ny,nz),phip(nx,ny,nz),phim(nx,ny,nz))
ic=1
 !print*, "hello"
open(unit = 3, file = 'structure.dat')
    open(unit = 4, file = 'struct.in')
    write(1,*)Nx,Ny,Nz
    write(3,*)Nx,Ny,Nz
    write(4,*)Nx,Ny,Nz
phaseCoat(:,:,:) = 0
phasePolymer(:,:,:) = 1
phaseCeramic(:,:,:) = 0

 particle_num1= 2100!509!72 !182 2.5%!364  这个才是5%!728  !2.5%   !2912  !5%  r=2,n=3130
 call random_number(y)
 circle(1) = DBLE(x*(nx-16)+8)
 call random_number(y)
 circle(2) = DBLE(y*(ny-16)+8)
 call random_number(z)
 circle(3) = DBLE(z*(nz-16)+8)
 R=5 ! the distance of two sheets
 Rx=5*0.1 !Y axis
 Ry=5 !Z axis
 hsheet=0.5!X axis  thickness of the sheet
!save the first circle to circles
 circles(ic,1)=circle(1)
 circles(ic,2)=circle(2)
 circles(ic,3)=circle(3)
 circles(ic,4)=R
 ! print*,circle(1)
! print*,circle(2)
! print*,circle(3)

!print*, "hello"

111 IF(ic>=particle_num1) GOTO 112
 call random_number(x)
 circle(1) = DBLE(x*(nx-16)+8)
 call random_number(y)
 circle(2) = DBLE(y*(ny-16)+8)
 call random_number(z)
 circle(3) = DBLE(z*(nz-16)+8)
 
  DO i=1,ic !determine if the created circle is wanted
  dd = sqrt((circle(1)-circles(i,1))**2 +(circle(2)-circles(i,2))**2+(circle(3)-circles(i,3))**2)
  IF(dd<R) THEN   !random
! IF(dd<R*ABS((circles(i,3)-64)/8)+1*R) THEN   !sbs
 ! IF(dd<R*14*ABS((circles(i,3)-64)/64)+1*R) THEN   !sbs
!print*,dd 
 !IF (((circles(i,3) <= 64).and.(dd <= R*circles(i,3)/8+1*R)) .or. &
  !   ((circles(i,3) > 64).and.(dd <= R*(128-circles(i,3))/8+1*R))) then         !bsb
 !IF(dd<(R*5/(ABS(((circles(i,3)-64)/128))**(1/2))+2*R)) THEN   !32 change the distance 
  GOTO 111
   END IF
  END DO
 ic=ic+1
!save the first circle to circles
 circles(ic,1)=circle(1)
 circles(ic,2)=circle(2)
 circles(ic,3)=circle(3)
 circles(ic,4)=R
! print*,'ic=',ic
! print*,circle(1)
! print*,circle(2)
! print*,circle(3)
 !print*,"22222"
!x----1 end
goto 111
 
112 DO nn=1,particle_num1
   ! print*,nn
 DO kk=INT(circles(nn,3))-(R+2),INT(circles(nn,3)) +(R+2)! h is half length of fiber
   DO jj=INT(circles(nn,2))-(R+2),INT(circles(nn,2)) + (R + 2)
     DO ii=INT(circles(nn,1))-(R + 2),INT(circles(nn,1))+(R + 2)
     ! print*, "hello12345"
       ddx=ABS(DBLE(ii)-circles(nn,1))
       ddy=ABS(DBLE(jj)-circles(nn,2))
       ddz=ABS(DBLE(kk)-circles(nn,3))
       IF((ddx <= Rx) .and. (ddy<=Ry) .and. (ddz<=hsheet)) THEN
          ! print*,"234455"
                         phaseCoat(ii,jj,kk) = 0
                         phasePolymer(ii,jj,kk) = 0
                         phaseCeramic(ii,jj,kk) = 1

       END IF
      END DO
    END DO
 END DO
END DO
vfCoat = 0.0
    vfPolymer = 0.0
    vfCeramic = 0.0
    do ii=1,Nx 
     do kk=1,Nz
        do jj=1,Ny
                    if(phaseCoat(ii,jj,kk) == 1) then
                        vfCoat = vfCoat + 1.0d0
                        phaseID(ii,jj,kk) = 2
                elseif(phasePolymer(ii,jj,kk) == 1) then
                        vfPolymer = vfPolymer + 1.0d0
                        phaseID(ii,jj,kk) = 1
                elseif(phaseCeramic(ii,jj,kk) == 1) then
                        vfCeramic = vfCeramic + 1.0d0
                        phaseID(ii,jj,kk) = 3
                else
                endif
                write(3,*)ii,kk,jj,phaseCoat(ii,jj,kk),phasePolymer(ii,jj,kk),phaseCeramic(ii,jj,kk)
                write(4,*)ii,kk,jj,phaseID(ii,jj,kk)
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

DEALLOCATE(phi,phip,phim)
END Program main
