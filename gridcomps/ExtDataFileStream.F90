#include "MAPL_ErrLog.h"
module MAPL_ExtDataFileStream
   use yaFyaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   type, public :: ExtDataFileStream
      character(:), allocatable :: file_template
      character(:), allocatable :: file_reference_date
      character(:), allocatable :: file_frequency
      character(:), allocatable :: old_file_freq
      integer, allocatable :: valid_range(:)
      contains 
         procedure :: display
   end type

   interface ExtDataFileStream
      module procedure new_ExtDataFileStream_from_yaml
   end interface

contains

   function new_ExtDataFileStream_from_yaml(config,unusable,rc) result(data_set)
      type(Configuration), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataFileStream), target :: data_set
      logical :: is_present
      integer :: status
      _UNUSED_DUMMY(unusable)

      call config%get(data_Set%file_template,"file_template",default='',is_present=is_present,rc=status)
      _VERIFY(status)
      _ASSERT(is_present,"Missing file template in dataset")
      call config%get(data_Set%file_reference_date,"file_reference_time",default='',rc=status)
      _VERIFY(status)
      call config%get(data_set%file_frequency,"file_frequency",default='',rc=status)
      _VERIFY(status)
      data_set%valid_range = config%at("valid_range")
      call config%get(data_Set%old_file_freq,"old_file_freq",default='',rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   end function new_ExtDataFileStream_from_yaml

   subroutine display(this)
      class(ExtDataFileStream) :: this
      write(*,*)'dataset_ftempl: ',trim(this%file_template)
      write(*,*)'dataset_reff: ',trim(this%file_reference_date)
      write(*,*)'dataset_freq: ',trim(this%file_frequency)
   end subroutine display

end module MAPL_ExtDataFileStream

module MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataFileStream

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataFileStream)
#define _alt

#define _map ExtDataFileStreamMap
#define _iterator ExtDataFileStreamMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataFileStreamMap
