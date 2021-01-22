module vector_class
  !
  !   This module implements an abstract vector class.
  !
  !
  implicit none

  ! type definition
  type, abstract :: vec

     ! instance variables.
     real :: x
     real :: y

   contains

     ! bound procedures
     procedure(addx), pass, deferred :: add
     procedure(subtrx), pass, deferred :: subtr

  end type vec

  abstract interface
     subroutine addx(this, vec2, vec_out)

       import vec
       implicit none

       class(vec), intent(in)  :: this    ! vector object
       class(vec), intent(in)  :: vec2    ! second vector object
       class(vec), intent(out) :: vec_out ! output vector

     end subroutine addx

     subroutine subtrx(this, vec2, vec_out)

       import vec
       implicit none

       class(vec), intent(in)  :: this    ! vector object
       class(vec), intent(in)  :: vec2    ! second vector object
       class(vec), intent(out) :: vec_out ! output vector

     end subroutine subtrx

  end interface

end module vector_class

module vector2d_class
  !
  !   This module implements a 2d vector class.
  !
  !
  use vector_class
  implicit none

  ! type definition
  type, extends(vec), public :: vec2d

     ! instance variables.
     ! no extra

   contains

     ! bound procedures
     procedure, public :: add => vector_add_sub
     procedure, public :: subtr => vector_minus_sub

  end type vec2d

  private :: vector_add_sub, vector_minus_sub

contains

  subroutine vector_add_sub(this, vec2, vec_out)

    implicit none

    class(vec2d), intent(in)  :: this    ! vector object
    class(vec),   intent(in)  :: vec2    ! second vector object
    class(vec),   intent(out) :: vec_out ! output vector

    ! add vectors
    vec_out%x = this%x + vec2%x
    vec_out%y = this%y + vec2%y

  end subroutine vector_add_sub

  subroutine vector_minus_sub(this, vec2, vec_out)


    implicit none

    class(vec2d), intent(in)  :: this    ! vector object
    class(vec),   intent(in)  :: vec2    ! second vector object
    class(vec),   intent(out) :: vec_out ! output vector

    ! subtract vectors
    vec_out%x = this%x - vec2%x
    vec_out%y = this%y - vec2%y

  end subroutine vector_minus_sub

end module vector2d_class

module vector3d_class
  !
  !   This module implements a 3d vector class.
  !
  !
  use vector_class
  implicit none

  ! type definition
  type, extends(vec), public :: vec3d

     ! instance variables.
     real :: z

   contains

     ! bound procedures
     procedure, public :: add => vector_add_sub
     procedure, public :: subtr => vector_minus_sub

  end type vec3d

  private :: vector_add_sub, vector_minus_sub

contains

  subroutine vector_add_sub(this, vec2, vec_out)

    implicit none

    class(vec3d), intent(in)  :: this    ! vector object
    class(vec),   intent(in)  :: vec2    ! second vector object
    class(vec),   intent(out) :: vec_out ! output vector

    ! add vectors
    vec_out%x = this%x + vec2%x
    vec_out%y = this%y + vec2%y
    select type(vec2)
    class is (vec3d)
       select type(vec_out)
       class is (vec3d)
          vec_out%z = this%z + vec2%z
       class default
          print *, 'vec_out must be vec3d'
          stop
       end select
    class default
       print *, 'vec_out must be vec3d'
    end select

  end subroutine vector_add_sub

  subroutine vector_minus_sub(this, vec2, vec_out)


    implicit none

    class(vec3d), intent(in)  :: this    ! vector object
    class(vec),   intent(in)  :: vec2    ! second vector object
    class(vec),   intent(out) :: vec_out ! output vector

    ! subtract vectors
    vec_out%x = this%x - vec2%x
    vec_out%y = this%y - vec2%y
    !vec_out%z = this%z - vec2%z !can't do this!!

  end subroutine vector_minus_sub

end module vector3d_class

program test_vector
  !
  !  This program tests polymorphism using the vector class
  !  and its subclasses.
  !
  use vector2d_class
  use vector3d_class
  implicit none

  ! Declare variables
  type(vec2d), target :: vec1, vec2, vec3  ! 2D vector objects
  type(vec3d), target :: vec4, vec5, vec6  ! 2D vector objects
  integer              :: i                 ! Loop index

  ! create an array of shape pointers
  type :: vec_ptr
     class(vec), pointer :: p     ! pointer to shapes
  end type vec_ptr
  type(vec_ptr), dimension(6) :: vecs

  ! Create and initialize some vectors
  vec1%x = 5.0
  vec1%y = 6.0

  vec2%x = 10.0
  vec2%y = 12.0

  vec4%x = 1.0
  vec4%y = -1.0
  vec4%z = -10.0

  vec5%x = 2.0
  vec5%y = 5.0
  vec5%z = 21.0

  ! Create the array of vector pointers
  vecs(1)%p => vec1
  vecs(2)%p => vec2
  vecs(3)%p => vec3
  vecs(4)%p => vec4
  vecs(5)%p => vec5
  vecs(6)%p => vec6

  ! Add some stuff
  call vec1%add(vec2, vec3)
  write(*,'(A, F8.5, A, F8.5)') "V1 + V2 = ", vec3%x, " , ", vec3%y

  call vecs(2)%p%subtr(vecs(1)%p, vecs(3)%p)
  write(*,'(A, F8.5, A, F8.5)') "V2 - V1 = ", vec3%x, " , ", vec3%y

  call vec4%add(vec5, vec6)
  write(*, '(A, F8.5, A, F8.5, A, F8.5)') "V4 + V5 = ", vec6%x, ", ", vec6%y, ", ", vec6%z

end program test_vector
