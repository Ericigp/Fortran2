        program principal
        implicit none
        real*8 f, u,i

        open(20,file='datos.txt')
        do i=0d0, 10d0,0.5d0
        write(20,*) i, f(i)
        enddo
        pause
        stop
        end


        real*8 function f(u)
        implicit none
        real*8 u
        f=2d0*exp(u**2d0)*(2d0*u**2d0+1d0)
        return
        end
