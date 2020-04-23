        program integrales
        implicit none
        real*8 x, a, b, h , trapecio, sum, res, xk,i
        real*8 mif
        real*8 campo1, campo2, z, R, mu, muM

        integer*8 k, n
        write(*,*) 'Introduzca el grado del polinomio'
        read(*,*) n
        write(*,*) 'Introduzca el valor de z'
        read(*,*) z
        
        R=1d0
        
        mif=x**2
        a=0d0
        b=1d0
        h=(b-a)/(dble(n))

        sum=0d0
        do k=1, n-1
        xk=a+k*h
        sum=sum+xk**2
        enddo
        
        res=0d0
        res=res+a**2+b**2+(2*sum)
        trapecio=(h*res)/2d0
        
        write(*,*) 'Integral calculada por el metodo del trapecio'
        write(*,*) trapecio
        
        
        h=(b-a)/(2d0*n)
        
        !Calculo de B
        write(*,*) 'CampoB calculado por el metodo del trapecio'
        campo1=campo1+((muM*R**3)/2)*trapecio
        write(*,*) campo1
        write(*,*) 'CampoB calculado por el metodo de simson'
        campo2=campo2+((muM*R**3)/2)*simpson
        write(*,*) campo2
        

        pause
        stop
        end
        
        
        
       real*8 function mif(x)
       implicit none
       real*8 x,z,R
       R=1d0
       mif=sin(x)**3/((z**2+R**2-2*R*z*cos(x))**(3/2))
       return
       end
        
        
        

