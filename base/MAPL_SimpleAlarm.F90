#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_SimpleAlarm
   use ESMF
   use MAPL_ExceptionHandling

   implicit none
   private

   public SimpleAlarm

   type :: SimpleAlarm
      private
      type(ESMF_Time) :: reference_time
      type(ESMF_TimeInterval) :: ring_interval
      type(ESMF_Time) :: last_ring
      type(ESMF_Time) :: stop_time
      type(ESMF_Time), allocatable :: ring_times(:)
      logical :: sticky
      logical :: still_ringing
      contains
         procedure :: is_ringing
         procedure :: turn_off
   end type SimpleAlarm

   interface SimpleAlarm
      module procedure new_simple_alarm
   end interface SimpleAlarm

contains

   function new_simple_alarm(ring_times,reference_time,ring_interval,stop_time,sticky,rc) result(new_alarm)
      type(ESMF_Time), intent(in), optional :: ring_times(:)
      type(ESMF_Time), intent(in), optional :: reference_time
      type(ESMF_TimeInterval), intent(in), optional :: ring_interval
      type(ESMF_Time), intent(in), optional :: stop_time
      logical, intent(in), optional :: sticky
      integer, optional, intent(out) :: rc

      type(SimpleAlarm) :: new_alarm

      if (present(reference_time)) then
         new_alarm%reference_time = reference_time
         new_alarm%last_ring = reference_time
         _ASSERT(present(ring_interval),'ring interval must be present with reff time')
      end if
      if (present(ring_interval)) then
         new_alarm%ring_interval = ring_interval
         _ASSERT(present(reference_time),'ref time must be present with ring interval')
      end if
      if (present(ring_times)) then
         new_alarm%ring_times=ring_times
         _ASSERT((.not.present(ring_interval)) .and. (.not.present(reference_time)),'message here')
      end if
      if (present(stop_time)) then
          new_alarm%stop_time = stop_time
      end if
      if (present(sticky)) then
         new_alarm%sticky=sticky
      else
         new_alarm%sticky=.false.
      end if
      new_alarm%still_ringing=.false.
      _RETURN(_SUCCESS)
   end function new_simple_alarm

   subroutine turn_off(this)
      class(SimpleAlarm), intent(inout) :: this
      if (this%still_ringing) then
         this%still_ringing = .false.
      end if
   end subroutine turn_off
    
   function is_ringing(this,clock,rc) result(ringing)
      class(SimpleAlarm), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status,i
      logical :: ringing

      type(ESMF_Time) :: temp_time, current_time

      ! sticky wins
      if (this%sticky .and. this%still_ringing) then
         ringing = this%still_ringing
         _RETURN(_SUCCESS)
      endif

      call ESMF_ClockGet(clock,currTime=current_time,rc=status)
      _VERIFY(status)
      ringing = .false.

      ! first check if have ring times
      if (allocated(this%ring_times)) then
         do i=1,size(this%ring_times)
            ringing=in_interval(this%ring_times(i),clock,rc=status)
            if (ringing) exit
         enddo
         if (this%sticky .and. ringing) this%still_ringing=.true.
         _RETURN(_SUCCESS)
      end if
      
      ! do not have ring times, must be reff time + interval
      ! if outside time range don't ring
      if (current_time < this%reference_time) then
         _RETURN(_SUCCESS)
      end if

      call ESMF_TimeValidate(this%stop_time,rc=status)
      if (status == _SUCCESS) then
         if (current_time >= this%stop_time) then
            _RETURN(_SUCCESS)
         end if
      end if
        

      ! finally check if we can advance to the current time, if not don't ring
      if (current_time == this%last_ring) then
         ringing = .true.
      else if (current_time > this%last_ring) then
         temp_time = this%last_ring
         do while (current_time > temp_time)
            temp_time = temp_time + this%ring_interval
            ringing = in_interval(temp_time,clock,rc=status)
            _VERIFY(status)
            if (ringing) then
               this%last_ring = temp_time
               exit
            end if
         enddo
      else if (current_time < this%last_ring) then
         temp_time = this%last_ring
         do while (current_time < temp_time)
            temp_time = temp_time - this%ring_interval
            ringing = in_interval(temp_time,clock,rc=status)
            _VERIFY(status)
            if (ringing) then
               this%last_ring = temp_time
               exit
            end if
         enddo
      end if
   
      ! if we are sticky, remember we rang 
      if (this%sticky .and. ringing) this%still_ringing=.true.

      _RETURN(_SUCCESS)
   end function is_ringing

   function in_interval(time,clock,rc) result(in_range)
      type(ESMF_Time), intent(in) :: time
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc

      type(ESMF_Time) :: current_time, previous_time
      type(ESMF_TimeInterval) :: dt
      logical :: in_range
      type(ESMF_DIRECTION_FLAG) :: direction
      integer :: status

      in_range = .false.
      call ESMF_ClockGet(clock,currTime=current_time,direction=direction, &
            timeStep=dt,rc=status)
      _VERIFY(status)
      if (direction==ESMF_DIRECTION_FORWARD) then
         previous_time=current_time-dt
         if (time > previous_time .and. time  <= current_time) in_range = .true.
      else if (direction==ESMF_DIRECTION_REVERSE) then
         previous_time=current_time+dt
         if (time < previous_time .and. time >= current_time) in_range = .true.
      end if

      _RETURN(_SUCCESS)

   end function in_interval

end module MAPL_SimpleAlarm
