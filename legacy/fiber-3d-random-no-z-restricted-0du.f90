Program main   !z方向的间距不受限制
IMPLICIT NONE

INTEGER :: ii,jj,kk,ic,nn,particle_num1,h,i
REAL*8 :: circle(6),circles(2000,6),dd,line(2000,10),ddz,ddx,ddy,seta,y,x,z,ddd
INTEGER :: R,mm
INTEGER, PARAMETER :: nx = 128, ny = 128, nz = 128

REAL*8,allocatable :: phi(:,:,:),phip(:,:,:),phim(:,:,:)

allocate (phi(nx,ny,nz),phip(nx,ny,nz),phim(nx,ny,nz))
 ic=1
 h=5.0     !2.5% volume fraction
 print*, "hello"
 call random_number(z)
 circle(3)=  DBLE(z*(nz-4)+2)
 call random_number(y)
 circle(2) = DBLE(y*(ny-4)+2)
 call random_number(x)
 circle(1) = DBLE(x*(nx-4)+2)
 !call random_number(seta)
 circle(4) = 0.0!seta*3.14 !3.14/4
 R=2
!save the first circle to circles
 circles(ic,1)=circle(1)
 circles(ic,2)=circle(2)
 circles(ic,3)=circle(3)
 circles(ic,4)=R
 circles(ic,5)=circle(4) !angle
line(ic,1)=h*cos(circles(ic,5))+circles(ic,1)   !x1
line(ic,2)=h*sin(circles(ic,5))+circles(ic,2)   !y1
line(ic,3)=line(ic,1)-circles(ic,1) !delta   x
line(ic,4)=line(ic,2)-circles(ic,2)  !delta   y  delta z=0
line(ic,5)=circles(ic,1)*line(ic,2)-circles(ic,2)*line(ic,1) !c=x0*y1-x1*y0
line(ic,6)=line(ic,4)/line(ic,3)  !k=a/b
line(ic,7)=line(ic,5)/line(ic,4)  !c=c/b  y=kx+b  kx-y+b=0
 print*, "hello"
phi=0.0
particle_num1=834


111  IF(ic>=particle_num1) GOTO 112
 call random_number(z)
 circle(3)=  DBLE(z*(nz-4)+2)
 call random_number(y)
 circle(2) = DBLE(y*(ny-4)+2)
 call random_number(x)
 circle(1) = DBLE(x*(nx-4)+2)
 call random_number(seta)
 circle(4)=0.0!seta*3.14 !3.14/4
!save the circle to circles

DO i=1,ic !determine if the created circle is wanted
 ddz = ABS(circle(3)-circles(i,3))! z轴之间的距离
 ddx = ABS(circle(1)-circles(i,1))
 ddy = ABS(circle(2)-circles(i,2))
dd = sqrt((circle(1)-circles(i,1))**2 +(circle(2)-circles(i,2))**2+(circle(3)-circles(i,3))**2)
IF ((ddz<=10).and. (dd<=20) .and. (ddx <= 10)) then
    goto 111
 END IF
END DO
ic=ic+1
circles(ic,1)=circle(1)
circles(ic,2)=circle(2)
circles(ic,3)=circle(3)
circles(ic,4)=R
circles(ic,5)=circle(4)
line(ic,1)=h*cos(circles(ic,5))+circles(ic,1)   !x1
line(ic,2)=h*sin(circles(ic,5))+circles(ic,2)   !y1
line(ic,3)=line(ic,1)-circles(ic,1) !delta   x
line(ic,4)=line(ic,2)-circles(ic,2)  !delta   y  delta z=0
line(ic,5)=circles(ic,1)*line(ic,2)-circles(ic,2)*line(ic,1) !c=x0*y1-x1*y0
line(ic,6)=line(ic,4)/line(ic,3)  !k=a/b
line(ic,7)=line(ic,5)/line(ic,4)  !c=c/b  y=kx+b  kx-y+b=0
 
 print*,"22222 end the fiber"
!END DO
!END IF
goto 111
print*,"1111"

112 DO nn=1,particle_num1
  DO jj=INT(circles(nn,2))-h,INT(circles(nn,2))+h! h is half length of fiber
   DO kk=INT(circles(nn,3))-(R+1),INT(circles(nn,3)) + (R + 1)
     DO ii=INT(circles(nn,1)) - h,INT(circles(nn,1))+h
       print*, "hello1234567"
dd =SQRT((0-(kk-circles(nn,3))*line(nn,4))**2+((kk-circles(nn,3))*line(nn,3))**2&
   +((ii-circles(nn,1))*line(nn,4)-line(nn,3)*(jj-circles(nn,2)))**2)&
!dd=ABS(line(ic,3)*jj+line(ic,3)*kk-line(ic,4)*ii+circles(ic,1)*line(ic,4)-line(ic,3)*(circles(ic,2)+circles(ic,3)))&
       /SQRT((line(nn,3))**2+(line(nn,4))**2)  !点到直线的距离
ddd=((ii-circles(nn,1))*line(nn,3)+(jj-circles(nn,2))*line(nn,4))/SQRT((line(nn,3))**2+(line(nn,4))**2) 
    !点到圆心的距离在中心线上的投影
       IF((dd <= circles(nn,4)) .and. (ddd<=h) ) THEN
        phip(ii,jj,kk) = 1.0
        print*,'phip(ii,jj,kk)'
       ENDIF
      END DO
    END DO
  END DO
END DO

phim(:,:,:)=1.0-phip(:,:,:)
 Open(unit=1,file='fiber-10-2-5%.dat')
 DO ii=1,nx
     DO jj=1,ny
         Do kk=1,nz
             write(1,*) ii,jj,kk,phip(ii,jj,kk)!,0.0,phim(ii,jj,kk),0.0
         END DO
     END DO
END DO

Open(unit=1,file='fiber-10-2-5%-plot.dat')
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