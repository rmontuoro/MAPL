#include "MAPL_ErrLog.h"
module MAPL_ExtDataRule
   use yaFyaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   type, public :: ExtDataRule
      character(:), allocatable :: file_template_key
      character(:), allocatable :: file_var
      logical :: allow_extrapolation
      real :: scaling
      real :: shift
      logical :: time_interpolation
      character(:), allocatable :: climatology
      character(:), allocatable :: regrid_method
      character(:), allocatable :: refresh_time
      character(:), allocatable :: refresh_frequency
      character(:), allocatable :: refresh_offset
      character(:), allocatable :: vector_partner
      character(:), allocatable :: vector_component
      character(:), allocatable :: vector_file_partner
      character(:), allocatable :: refresh_template !temporary to get working
      contains
         procedure :: display
         procedure :: split_vector
   end type

   interface ExtDataRule
      module procedure new_ExtDataRule_from_yaml
   end interface

contains

   function new_ExtDataRule_from_yaml(config,unusable,rc) result(rule)
      type(Configuration), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataRule), target :: rule
      logical :: is_present
      integer :: status
    
      _UNUSED_DUMMY(unusable)

      call config%get(rule%file_template_key,"file_template_key",default='',is_present=is_present,rc=status)
      _VERIFY(status)
      _ASSERT(is_present,"Missing file_template_key in ExtDataRule")

      call config%get(rule%file_var,"file_var",default='',is_present=is_present,rc=status)
      _VERIFY(status)
      _ASSERT(is_present,"Missing file_var in ExtDataRule")

      call config%get(rule%allow_extrapolation,"allow_extrapolation",default=.false.,rc=status)
      _VERIFY(status)

      call config%get(rule%climatology,"climatology",default='N',rc=status)
      _VERIFY(status)

      call config%get(rule%scaling,"scaling",default=0.0,rc=status) 
      _VERIFY(status)

      call config%get(rule%shift,"shift",default=0.0,rc=status)
      _VERIFY(status)

      call config%get(rule%time_interpolation,"time_interpolation",default=.true.,rc=status)
      _VERIFY(status)

      call config%get(rule%regrid_method,"regrid_method",default='REGRID_METHOD_BILINEAR',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_time,"update_reff_time",default='00',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_frequency,"update_frequency",default='PT0S',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_offset,"update_offset",default='PT0S',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_template,"refresh_template",default='0',rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function new_ExtDataRule_from_yaml

   subroutine display(this)
      class(ExtDataRule) :: this
      write(*,*)"file_template_key: ",trim(this%file_template_key)
      write(*,*)"file_var: ",trim(this%file_var)
      write(*,*)"allow_extrapolation: ",this%allow_extrapolation
      write(*,*)"scaling: ",this%scaling
      write(*,*)"shift: ",this%shift
      write(*,*)"time_interpolation: ",this%time_interpolation
      write(*,*)"climatology: ",this%climatology
   end subroutine display
 
   subroutine split_vector(this,original_key,ucomp,vcomp,unusable,rc)
      class(ExtDataRule), intent(in) :: this
      character(len=*), intent(in) :: original_key
      type(ExtDataRule), intent(inout) :: ucomp,vcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: semi_pos
      character(len=:),allocatable :: uname,vname
    
      _UNUSED_DUMMY(unusable)

      semi_pos = index(this%file_var,";")
      _ASSERT(semi_pos > 0,"vector rule does not have 2 variables in the file_var")
      uname = this%file_var(1:semi_pos-1)
      vname = this%file_var(semi_pos+1:len_trim(this%file_var))
      ucomp = this
      vcomp = this
      semi_pos = index(original_key,";")
      ucomp%vector_partner = original_key(semi_pos+1:len_trim(original_key))
      vcomp%vector_partner = original_key(1:semi_pos-1)
      ucomp%file_var = uname
      vcomp%file_var = vname
      ucomp%vector_file_partner = vname
      vcomp%vector_file_partner = uname
      ucomp%vector_component = "EW"
      vcomp%vector_component = "NS"
      _RETURN(_SUCCESS)

   end subroutine split_vector

end module MAPL_ExtDataRule

module MAPL_ExtDataRuleMap
   use MAPL_ExtDataRule

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataRule)
#define _alt

#define _map ExtDataRuleMap
#define _iterator ExtDataRuleMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataRuleMap
