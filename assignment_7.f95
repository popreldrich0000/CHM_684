program integration
    implicit none



    real(kind = 8) :: sin_integral1
    real(kind = 8) :: gaussian_integral1
    real(kind = 8) :: sin_by_x_integral1
    real(kind = 8) :: x_cos_integral1
    real(kind = 8) :: x_3_integral1

    
    real(kind = 8) :: x   
    
    real(kind = 8) :: sin_integral2     
    real(kind = 8) :: gaussian_integral2
    real(kind = 8) :: sin_by_x_integral2
    real(kind = 8) :: x_cos_integral2   
    real(kind = 8) :: x_3_integral2 

    

    real(kind = 8) :: h1   
    real(kind = 8) :: h2   

    integer :: i
    sin_integral1 = 0     
    gaussian_integral1 = 0 
    sin_by_x_integral1 = 0 
    x_cos_integral1   = 0 
    x_3_integral1     = 0
    sin_integral2 = 0     
    gaussian_integral2 = 0 
    sin_by_x_integral2 = 0 
    x_cos_integral2   = 0 
    x_3_integral2     = 0
    x = -10
    h1 = 0.1
    h2 = 0.01
    do i = 1, 201 
            

        if (( i==1 ).or.(i==201)) then
            sin_integral1 = sin_integral1           + func_sin(x + (i-1)*h1 )
            gaussian_integral1 = gaussian_integral1 + func_gaussian(x + (i-1)*h1 )
            sin_by_x_integral1 = sin_by_x_integral1 + func_sin_by_x(x + (i-1)*h1 )
            x_cos_integral1   =  x_cos_integral1    + func_x_cos(x + (i-1)*h1 )   
            x_3_integral1     =  x_3_integral1      + func_x_3(x + (i-1)*h1 )     
            cycle
        end if



        if (mod(i,2)==0) then
            sin_integral1 = sin_integral1           + 4*func_sin(x + (i-1)*h1 )
            gaussian_integral1 = gaussian_integral1 + 4*func_gaussian(x + (i-1)*h1 )
            sin_by_x_integral1 = sin_by_x_integral1 + 4*func_sin_by_x(x + (i-1)*h1 )
            x_cos_integral1   =  x_cos_integral1    + 4*func_x_cos(x + (i-1)*h1 )   
            x_3_integral1     =  x_3_integral1      + 4*func_x_3(x + (i-1)*h1 ) 
        end if

        if (mod(i,2)==1) then
            sin_integral1 = sin_integral1           + 2*func_sin(x + (i-1)*h1 )
            gaussian_integral1 = gaussian_integral1 + 2*func_gaussian(x + (i-1)*h1 )
            sin_by_x_integral1 = sin_by_x_integral1 + 2*func_sin_by_x(x + (i-1)*h1 )
            x_cos_integral1   =  x_cos_integral1    + 2*func_x_cos(x + (i-1)*h1 )   
            x_3_integral1     =  x_3_integral1      + 2*func_x_3(x + (i-1)*h1 ) 
        end if

    end do
    
    sin_integral1 = sin_integral1*h1/3
    gaussian_integral1 = gaussian_integral1*h1/3
    sin_by_x_integral1 = sin_by_x_integral1*h1/3
    x_cos_integral1   =  x_cos_integral1*h1/3
    x_3_integral1     =  x_3_integral1*h1/3


    do i = 1, 2001 
            

        if (( i==1 ).or.(i==2001)) then
            sin_integral2 = sin_integral2           + func_sin(x + (i-1)*h2 )
            gaussian_integral2 = gaussian_integral2 + func_gaussian(x + (i-1)*h2 )
            sin_by_x_integral2 = sin_by_x_integral2 + func_sin_by_x(x + (i-1)*h2 )
            x_cos_integral2   =  x_cos_integral2    + func_x_cos(x + (i-1)*h2 )   
            x_3_integral2     =  x_3_integral2      + func_x_3(x + (i-1)*h2 )     
            cycle
        end if



        if (mod(i,2)==0) then
            sin_integral2 = sin_integral2           + 4*func_sin(x + (i-1)*h2 )
            gaussian_integral2 = gaussian_integral2 + 4*func_gaussian(x + (i-1)*h2 )
            sin_by_x_integral2 = sin_by_x_integral2 + 4*func_sin_by_x(x + (i-1)*h2 )
            x_cos_integral2   =  x_cos_integral2    + 4*func_x_cos(x + (i-1)*h2 )   
            x_3_integral2     =  x_3_integral2      + 4*func_x_3(x + (i-1)*h2 ) 
        end if

        if (mod(i,2)==1) then
            sin_integral2 = sin_integral2           + 2*func_sin(x + (i-1)*h2 )
            gaussian_integral2 = gaussian_integral2 + 2*func_gaussian(x + (i-1)*h2 )
            sin_by_x_integral2 = sin_by_x_integral2 + 2*func_sin_by_x(x + (i-1)*h2 )
            x_cos_integral2   =  x_cos_integral2    + 2*func_x_cos(x + (i-1)*h2 )   
            x_3_integral2     =  x_3_integral2      + 2*func_x_3(x + (i-1)*h2 ) 
        end if

    end do
    
    sin_integral2 = sin_integral2*h2/3
    gaussian_integral2 = gaussian_integral2*h2/3
    sin_by_x_integral2 = sin_by_x_integral2*h2/3
    x_cos_integral2   =  x_cos_integral2*h2/3
    x_3_integral2     =  x_3_integral2*h2/3


    open(unit=1, file='outputh_0.01.new.txt', status='new')
    write(1,*) "sinx function integration when h  = 0.01 = " , sin_integral1     
    write(1,*) "gaussian function integration when h  = 0.01 = " , gaussian_integral1
    write(1,*) "sinx/x integration when h  = 0.01 = " , sin_by_x_integral1
    write(1,*) "xcosx integration when h  = 0.01 = " , x_cos_integral1   
    write(1,*) "xcube integration when h  = 0.01 = " , x_3_integral1  
  

    open(unit=1, file='outputh_0.1_new.txt', status='new')
    write(1,*) "sinx function integration when h  = 0.1 = " , sin_integral2     
    write(1,*) "gaussian function integration when h  = 0.1 = " , gaussian_integral2
    write(1,*) "sinx/x integration when h  = 0.1 = " , sin_by_x_integral2
    write(1,*) "xcosx integration when h  = 0.1 = " , x_cos_integral2   
    write(1,*) "xcube integration when h  = 0.1 = " , x_3_integral2  
  
    open(unit=1, file='error_0.01.new.txt', status='new')
    write(1,*) "error for sinx function integration when h  = 0.01 = " , sin_integral1     
    write(1,*) "error for gaussian function integration when h  = 0.01 = " , gaussian_integral1 - 1.7724538509055160
    write(1,*) "error for sinx/x integration when h  = 0.01 = " , sin_by_x_integral1 - 3.3166951884377480
    write(1,*) "error for xcosx integration when h  = 0.01 = " , x_cos_integral1   
    write(1,*) "error for xcube integration when h  = 0.01 = " , x_3_integral1  
  

    open(unit=1, file='error_0.1_new.txt', status='new')
    write(1,*) "error for  sinx function integration when h  = 0.1 = " , sin_integral2     
    write(1,*) "error for  gaussian function integration when h  = 0.1 = " , gaussian_integral2 - 1.7724538509055160
    write(1,*) "error for  sinx/x integration when h  = 0.1 = " , sin_by_x_integral2 - 3.3166951884377480
    write(1,*) "error for  xcosx integration when h  = 0.1 = " , x_cos_integral2   
    write(1,*) "error for  xcube integration when h  = 0.1 = " , x_3_integral2   


    contains 

        real(kind = 8) function func_sin(x1)
            implicit none
            real(kind = 8) :: x1  
            func_sin = sin(x1)
            return
        end function func_sin

        function func_gaussian(x1)
            implicit none
            real(kind = 8) :: x1   
            real(kind = 8) :: func_gaussian  
            func_gaussian = exp(-x1*x1)
            return

        end function func_gaussian

        function func_sin_by_x(x1)
            implicit none
            real(kind = 8) :: x1   
            real(kind = 8) :: func_sin_by_x   
            func_sin_by_x = sin(x1)/x1
            return

        end function func_sin_by_x

        function func_x_cos(x1)
            implicit none
            real(kind = 8) :: x1 
            real(kind = 8) :: func_x_cos
            func_x_cos = x1*cos(x1)
            return


        end function func_x_cos

        function func_x_3(x1)
            implicit none
            real(kind = 8) :: func_x_3
            real(kind = 8) :: x1 

            func_x_3 = x1*x1*x1
            return
            

        end function func_x_3

    
end program integration  
