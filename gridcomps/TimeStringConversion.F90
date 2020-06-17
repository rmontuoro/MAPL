#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_TimeStringConversion
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: string_to_esmf_time
   public :: string_to_esmf_timeinterval

contains

   function string_to_esmf_time(time_string,unusable,rc) result(time)
      character(len=*), intent(in) :: time_string
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ESMF_Time) :: time
      integer :: status
      integer ypos(2), mpos(2), dpos(2), hpos(2), spos(2)
      integer strlen
      integer firstdash, lastdash
      integer firstcolon, lastcolon
      integer lastspace
      integer year,month,day,hour,min,sec

      _UNUSED_DUMMY(unusable)

      strlen = LEN_TRIM (time_string)

      firstdash = index(time_string, '-')
      lastdash  = index(time_string, '-', BACK=.TRUE.)

      if (firstdash .LE. 0 .OR. lastdash .LE. 0) then
        rc = -1
        return
      endif

      ypos(2) = firstdash - 1
      mpos(1) = firstdash + 1
      ypos(1) = ypos(2) - 3

      mpos(2) = lastdash - 1
      dpos(1) = lastdash + 1
      dpos(2) = dpos(1) + 1

      read ( time_string(ypos(1):ypos(2)), * ) year
      read ( time_string(mpos(1):mpos(2)), * ) month
      read ( time_string(dpos(1):dpos(2)), * ) day

      firstcolon = index(time_string, ':')
      if (firstcolon .LE. 0) then

        ! If no colons, check for hour.

        ! Logic below assumes a null character or something else is after the hour
        ! if we do not find a null character add one so that it correctly parses time
        !if (time_string(strlen:strlen) /= char(0)) then
           !time_string = trim(time_string)//char(0)
           !strlen=len_trim(time_string)
        !endif
        lastspace = index(TRIM(time_string), ' ', BACK=.TRUE.)
        if ((strlen-lastspace).eq.2 .or. (strlen-lastspace).eq.3) then
          hpos(1) = lastspace+1
          hpos(2) = strlen-1
          read (time_string(hpos(1):hpos(2)), * ) hour
          min  = 0
          sec  = 0
        else
          hour = 0
          min  = 0
          sec  = 0
        endif

      else
        hpos(1) = firstcolon - 2
        hpos(2) = firstcolon - 1
        lastcolon =  index(time_string, ':', BACK=.TRUE.)
        if ( lastcolon .EQ. firstcolon ) then
          mpos(1) = firstcolon + 1
          mpos(2) = firstcolon + 2
          read (time_string(hpos(1):hpos(2)), * ) hour
          read (time_string(mpos(1):mpos(2)), * ) min
          sec = 0
        else
          mpos(1) = firstcolon + 1
          mpos(2) = lastcolon - 1
          spos(1) = lastcolon + 1
          spos(2) = lastcolon + 2
          read (time_string(hpos(1):hpos(2)), * ) hour
          read (time_string(mpos(1):mpos(2)), * ) min
          read (time_string(spos(1):spos(2)), * ) sec
        endif
      endif

      write(*,*)'bmaa time: ',year,month,day,hour,min,sec
      call ESMF_TimeSet(time,yy=year,mm=month,dd=day,h=hour,m=min,s=sec,__RC__)
      _RETURN(_SUCCESS)

   end function string_to_esmf_time

   function string_to_esmf_timeinterval(time_interval_string,unusable,rc) result(time_interval)
      character(len=*), intent(in) :: time_interval_string
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ESMF_TimeInterval) :: time_interval
      integer :: status

      integer :: strlen,ppos,cpos,lpos,tpos
      integer year,month,day,hour,min,sec
      character(len=:), allocatable :: date_string,time_string
      _UNUSED_DUMMY(unusable)

      year=0
      month=0
      day=0
      hour=0
      min=0
      sec=0
      strlen = len_trim(time_interval_string)
      tpos = index(time_interval_string,'T')
      ppos = index(time_interval_string,'P')
      _ASSERT(time_interval_string(1:1) == 'P','Not valid time duration')

      if (tpos /= 0) then
         if (tpos /= ppos+1) then
            date_string = time_interval_string(ppos+1:tpos-1)
         end if
         time_string = time_interval_string(tpos+1:strlen)
      else
         date_string = time_interval_string(ppos+1:strlen)
      end if

      if (allocated(date_string)) then
         strlen = len_trim(date_string)
         lpos = 0
         cpos = index(date_string,'Y')
         if (cpos /= 0) then
            read(date_string(lpos+1:cpos-1),*)year
            lpos = cpos
         end if
         cpos = index(date_string,'M')
         if (cpos /= 0) then
            read(date_string(lpos+1:cpos-1),*)month
            lpos = cpos
         end if
         cpos = index(date_string,'D')
         if (cpos /= 0) then
            read(date_string(lpos+1:cpos-1),*)day
            lpos = cpos
         end if
      end if
      if (allocated(time_string)) then
         strlen = len_trim(time_string)
         lpos = 0
         cpos = index(time_string,'H')
         if (cpos /= 0) then
            read(time_string(lpos+1:cpos-1),*)hour
            lpos = cpos
         end if
         cpos = index(time_string,'M')
         if (cpos /= 0) then
            read(time_string(lpos+1:cpos-1),*)min
            lpos = cpos
         end if
         cpos = index(time_string,'S')
         if (cpos /= 0) then
            read(time_string(lpos+1:cpos-1),*)sec
            lpos = cpos
         end if
      end if

      write(*,*)'bmaa interval: ',year,month,day,hour,min,sec
      call ESMF_TimeIntervalSet(time_interval,yy=year,mm=month,d=day,h=hour,m=min,s=sec,__RC__)
      _RETURN(_SUCCESS)

   end function string_to_esmf_timeinterval

end module MAPL_TimeStringConversion
