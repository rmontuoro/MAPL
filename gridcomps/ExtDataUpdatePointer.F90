#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module MAPL_ExtDataPointerUpdate
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_SimpleAlarm
   use MAPL_TimeStringConversion
   implicit none

   type :: ExtDataPointerUpdate
      private
      logical :: disabled = .false.
      type(SimpleAlarm) :: update_alarm
      type(ESMF_TimeInterval) :: offset
      contains
         procedure :: create_from_parameters
         procedure :: check_update
   end type

   contains

   subroutine create_from_parameters(this,update_time,update_freq,update_offset,time,rc)
      class(ExtDataPointerUpdate), intent(inout) :: this
      character(len=*), intent(in) :: update_time
      character(len=*), intent(in) :: update_freq
      character(len=*), intent(in) :: update_offset
      type(ESMF_Time), intent(inout) :: time
      integer, optional, intent(out) :: rc

      type(ESMF_Time) :: reference_time
      type(ESMF_TimeInterval) :: reference_freq
      integer :: status,int_time,year,month,day,hour,minute,second

      if (update_freq == "-") then
         this%disabled = .true.
      else if (update_freq /= "PT0S") then
         int_time = string_to_integer_time(update_time)
         hour=int_time/10000
         minute=mod(int_time/100,100)
         second=mod(int_time,100)
         call ESMF_TimeGet(time,yy=year,mm=month,dd=day,__RC__)
         call ESMF_TimeSet(reference_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,__RC__)
         reference_freq = string_to_esmf_timeinterval(update_freq,__RC__)
         this%update_alarm = simpleAlarm(reference_time,reference_freq,__RC__)
      end if
      this%offset=string_to_esmf_timeinterval(update_offset,__RC__)

   end subroutine create_from_parameters

   subroutine check_update(this,do_update,working_time,current_time,rc)
      class(ExtDataPointerUpdate), intent(inout) :: this
      logical, intent(out) :: do_update
      type(ESMF_Time), intent(inout) :: working_time
      type(ESMF_Time), intent(inout) :: current_time
      integer, optional, intent(out) :: rc

      integer :: status

      if (this%disabled) then
         do_update = .false.
         _RETURN(_SUCCESS)
      end if
      if (this%update_alarm%check_if_created()) then
         do_update = this%update_alarm%is_ringing(current_time,__RC__)
      else
         do_update = .true.
      end if
      working_time = current_time+this%offset

   end subroutine check_update

end module MAPL_ExtDataPointerUpdate
