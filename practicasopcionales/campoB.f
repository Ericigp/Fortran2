         program campoB
        implicit none
        real*8 a, b, n,z,R
        real*8 trapecio, simpson, f

        open (20, file='trapecio.txt')
        open (21, file='simpson.txt')
        a=0.d0
        b=acos(-1.d0)

        n=100
        R=1.d0
        
        do z=0.d0, 10.d0, 0.01
        
        call metodotrapecio(b, a, n, trapecio, z)
        call metodosimpson (b, a, n, simpson, z)


        write(20,*) z, 2*r**3/2.d0*trapecio
        write(21,*) z, 2*r**3/2.d0*simpson

        enddo

        pause
        stop
        end

        subroutine metodotrapecio (b, a, n, trapecio, z)
        real*8 a, b, n, trapecio, z
        real*8 sumatoriot, h, xk
        real*8 f
        integer k
        external f

        h=(b-a)/n

        sum1=0.d0
        do k=1, n-1
            xk=a+k*h
            sum1=sum1+f(xk, z)
        enddo

        trapecio=h/2*(f(a, z)+f(b, z)+2*sum1)

        end subroutine

        subroutine metodosimpson (b, a, n, simpson, z)
        real*8 b, a, n, simpson
        real*8 h, sum1, sum2, xk
        real*8 f
        integer k
        external f

        h=(b-a)/(2*n)

        sum1=0.d0
        do k=1, n-1
            xk=a+2*k*h
            sum1=sum1+f(xk, z)
        enddo

        sum2=0.d0
        do k=1, n
            xk=a+(2*k-1)*h
            sum2=sum2+f(xk, z)
        enddo

        simpson=(h/3)*(f(a, z)+f(b, z)+2*sum1+4*sum2)

        end subroutine



        real*8 function f(x,z)
        implicit none
        real*8 x, z, r
        R=1.d0
        if (x.le.1d-14) then
           f=0.d0
        else
            f=sin(x)**3/(z**2+r**2-2*r*z*cos(x))**(3.d0/2)
        endif
        return
        end

