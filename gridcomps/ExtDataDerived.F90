#include "MAPL_ErrLog.h"
module MAPL_ExtDataDerived
   use yaFyaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   type, public :: ExtDataDerived
      character(:), allocatable :: expression
      character(:), allocatable :: refresh_time
      character(:), allocatable :: refresh_frequency
      character(:), allocatable :: refresh_offset
      character(:), allocatable :: refresh_template !temporary to get working
      contains
         procedure :: display
   end type

   interface ExtDataDerived
      module procedure new_ExtDataDerived_from_yaml
   end interface

contains

   function new_ExtDataDerived_from_yaml(config,unusable,rc) result(rule)
      type(Configuration), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataDerived), target :: rule
      logical :: is_present
      integer :: status
    
      _UNUSED_DUMMY(unusable)

      call config%get(rule%expression,"function",default='',is_present=is_present,rc=status)
      _VERIFY(status)
      _ASSERT(is_present,"Missing function in ExtDataDerived")

      call config%get(rule%refresh_time,"update_reff_time",default='',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_frequency,"update_frequency",default='PT0S',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_offset,"update_offset",default='PT0S',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_template,"refresh_template",default='0',rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function new_ExtDataDerived_from_yaml

   subroutine display(this)
      class(ExtDataDerived) :: this
      write(*,*)"function: ",trim(this%expression)
   end subroutine display
 
end module MAPL_ExtDataDerived

module MAPL_ExtDataDerivedMap
   use MAPL_ExtDataDerived

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataDerived)
#define _alt

#define _map ExtDataDerivedMap
#define _iterator ExtDataDerivedMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataDerivedMap
