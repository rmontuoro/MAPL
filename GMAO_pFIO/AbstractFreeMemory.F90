module AbstractFreeMemMod
  implicit none
  private
  public :: AbstractFreeMemory
  type, abstract :: AbstractFreeMemory
  contains
      procedure(free_memory), deferred :: free_memory
  end type
  abstract interface
     subroutine free_memory (this)
        import :: AbstractFreeMemory
        class(AbstractFreeMemory), intent(inout) :: this
     end subroutine
  end interface
end module
