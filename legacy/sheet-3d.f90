Program main
IMPLICIT NONE

INTEGER :: ii,jj,kk,ic,nn,particle_num1,h,mm,i
REAL*8 :: circle(4),circles(3000,4),dd,ddx,ddy,ddz,x,y,z
INTEGER :: R
INTEGER, PARAMETER :: nx = 128, ny = 128, nz = 128

REAL*8,allocatable :: phi(:,:,:),phip(:,:,:),phim(:,:,:)

allocate (phi(nx,ny,nz),phip(nx,ny,nz),phim(nx,ny,nz))
ic=1
 !print*, "hello"

 particle_num1= 947!509!72 !182 2.5%!364  这个才是5%!728  !2.5%   !2912  !5%  r=2,n=3130
 call random_number(y)
 circle(1) = DBLE(x*(nx-16)+8)
 call random_number(y)
 circle(2) = DBLE(y*(ny-16)+8)
 call random_number(z)
 circle(3) = DBLE(z*(nz-16)+8)
 R=4 ! the distance of two sheets
!save the first circle to circles
 circles(ic,1)=circle(1)
 circles(ic,2)=circle(2)
 circles(ic,3)=circle(3)
 circles(ic,4)=R
  print*,circle(1)
 print*,circle(2)
 print*,circle(3)

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
 print*,'ic=',ic
 print*,circle(1)
 print*,circle(2)
 print*,circle(3)
 !print*,"22222"
!x----1 end
goto 111
 
112  phip=0.0
DO nn=1,particle_num1
    print*,nn
 DO kk=INT(circles(nn,3))-(R+2),INT(circles(nn,3)) +(R+2)! h is half length of fiber
   DO jj=INT(circles(nn,2))-(R+2),INT(circles(nn,2)) + (R + 2)
     DO ii=INT(circles(nn,1))-(R + 2),INT(circles(nn,1))+(R + 2)
      print*, "hello12345"
       ddx=ABS(DBLE(ii)-circles(nn,1))
       ddy=ABS(DBLE(jj)-circles(nn,2))
       ddz=ABS(DBLE(kk)-circles(nn,3))
       IF((ddx <= 6) .and. (ddy<=1) .and. (ddz<=6)) THEN
           print*,"234455"
        phip(ii,jj,kk) = 1.0
       END IF
      END DO
    END DO
 END DO
END DO

phim(:,:,:)=1.0-phip(:,:,:)
  Open(unit=1,file='sheet-6-1-6-13%.dat')
 DO ii=1,nx
     DO jj=1,ny
         Do kk=1,nz
             write(1,*) ii,jj,kk,phip(ii,jj,kk)!,0.0,phim(ii,jj,kk),0.0
         END DO
     END DO
END DO

Open(unit=1,file='sheet-6-1-6-13%-plot.dat')
write(1,*) 128,128,128
 DO ii=1,nx
     DO jj=1,ny
         Do kk=1,nz
             write(1,*) ii,jj,kk,phip(ii,jj,kk)
         END DO
     END DO
END DO

DEALLOCATE(phi,phip,phim)
END Program main