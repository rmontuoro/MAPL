#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataFileSeries
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ExtDataFileStream
   use MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataCollectionMod
   use MAPL_CollectionVectorMod
   use MAPL_ExtDataCollectionManagerMod
   use MAPL_FileMetadataUtilsMod
   use MAPL_TimeStringConversion
   use MAPL_StringTemplate
   implicit none
   private

   type, public :: ExtDataFileSeries
      character(:), allocatable :: file_template
      type(ESMF_TimeInterval) :: frequency
      type(ESMF_Time) :: reff_time
      integer :: collection_id
      contains 
         procedure :: get_file_bracket
         procedure :: get_time_on_file
         procedure :: get_file
   end type

   interface ExtDataFileSeries
      module procedure new_ExtDataFileSeries_from_filestream
   end interface

contains

   function new_ExtDataFileSeries_from_filestream(file_stream,current_time,unusable,rc) result(file_series)
      type(ExtDataFileStream), intent(in) :: file_stream
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataFileSeries) :: file_series
      integer :: status
      integer :: last_token
      integer :: iyy,imm,idd,ihh,imn,isc 
      character(len=2) :: token

      _UNUSED_DUMMY(unusable)

      file_series%file_template = file_stream%file_template
      if (file_stream%file_frequency /= '') then
         file_series%frequency = string_to_esmf_timeinterval(file_stream%file_frequency)
      else
         last_token = index(file_stream%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            token = file_series%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeIntervalSet(file_series%frequency,yy=1,__RC__)
            case("m2")
               call ESMF_TimeIntervalSet(file_series%frequency,mm=1,__RC__)
            case("d2")
               call ESMF_TimeIntervalSet(file_series%frequency,d=1,__RC__)
            case("h2")
               call ESMF_TimeIntervalSet(file_series%frequency,h=1,__RC__)
            case("n2")
               call ESMF_TimeIntervalSet(file_series%frequency,m=1,__RC__)
            end select
         else
            ! couldn't find any tokens so all the data must be on one file
            call ESMF_TimeIntervalSet(file_series%frequency,__RC__)
         end if
      end if

      if (file_stream%file_reference_date /= '') then
         file_series%reff_time = string_to_esmf_time(file_stream%file_reference_date)
      else
         last_token = index(file_stream%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            call ESMF_TimeGet(current_time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,__RC__)
            token = file_series%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeSet(file_series%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,__RC__)
            case("m2")
               call ESMF_TimeSet(file_series%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,__RC__)
            case("d2")
               call ESMF_TimeSet(file_series%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,__RC__)
            case("h2")
               call ESMF_TimeSet(file_series%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,__RC__)
            case("n2")
               call ESMF_TimeSet(file_series%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,__RC__)
            end select
         else
            file_series%reff_time = current_time 
         end if
      end if
      file_series%collection_id = MAPL_ExtDataAddCollection(file_series%file_template)

   end function new_ExtDataFileSeries_from_filestream

   subroutine get_file_bracket(this, target_time, bracketside, output_file, time_index, output_time, rc)
      class(ExtDataFileSeries), intent(inout) :: this
      type(ESMF_Time), intent(in) :: target_time
      character(len=*), intent(in) :: bracketside
      character(len=*), intent(inout) :: output_file
      integer, intent(out) :: time_index
      type(ESMF_Time), intent(out) :: output_time
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_TimeInterval) :: zero

      call ESMF_TimeIntervalSet(zero,__RC__)      
      if (this%frequency == zero) then
         output_file = this%file_template
         call this%get_time_on_file(output_file,target_time,bracketside,time_index,output_time,__RC__)
         output_file = output_file
      else
         call this%get_file(output_file,target_time,0,__RC__)
         call this%get_time_on_file(output_file,target_time,bracketside,time_index,output_time,__RC__)
         if (status /=  _SUCCESS) then
            if ( bracketside == 'R') then
               call this%get_file(output_file,target_time,1,__RC__)
               call this%get_time_on_file(output_file,target_time,bracketside,time_index,output_time,__RC__)
            else if (bracketside == 'L') then 
               call this%get_file(output_file,target_time,-1,__RC__)
               call this%get_time_on_file(output_file,target_time,bracketside,time_index,output_time,__RC__)
            end if
         end if
      end if
      _RETURN(_SUCCESS)
   
   end subroutine get_file_bracket

   subroutine get_file(this,filename,target_time,shift,rc)
      class(ExtDataFileSeries), intent(inout) :: this
      character(len=*), intent(out) :: filename
      type(ESMF_Time) :: target_time
      integer, intent(in) :: shift
      integer, intent(out), optional :: rc

      type(ESMF_Time) :: ftime
      integer :: n,status
      logical :: file_found

      n = (target_time-this%reff_time)/this%frequency
      ftime = this%reff_time+(n+shift)*this%frequency
      call string_template(filename,this%file_template,time=ftime,__RC__)
      inquire(file=trim(filename),exist=file_found)
      _ASSERT(file_found,"get_file did not file a file using: "//trim(this%file_template))
      _RETURN(_SUCCESS)

   end subroutine get_file

   subroutine get_time_on_file(this,filename,target_time,bracketside,time_index,output_time,rc)
      class(ExtDataFileSeries), intent(inout) :: this
      character(len=*), intent(inout) :: filename
      type(ESMF_Time), intent(in) :: target_time
      character(len=*), intent(in) :: bracketside
      integer, intent(Out) :: time_index
      type(ESMF_Time), intent(out) :: output_time
      integer, optional, intent(out) :: rc
      integer :: status

      type(FileMetadataUtils), pointer :: file_metadata
      type(ESMF_Time), allocatable :: time_series(:)
      logical :: in_bounds, found_time
      integer :: i,num_times

      call MakeMetadata(filename,this%collection_id,file_metadata,__RC__) 
      call file_metadata%get_time_info(timeVector=time_series,__RC__)
      num_times = size(time_series)
      found_time = .false.
      if (bracketside == 'L') then
         in_bounds = .not.(target_time < time_series(1))
         if (in_bounds) then
            do i=num_times,1,-1
               if (target_time >= time_series(i)) then
                  output_time = time_series(i)
                  time_index = i
                  found_time = .true.
                  exit
               end if
            enddo
         end if
      else if (bracketside == 'R') then
         in_bounds = .not.(target_time >= time_series(num_times))  
         if (in_bounds) then
            do i=1,num_times
               if (target_time < time_series(i)) then
                  output_time = time_series(i)
                  time_index = i
                  found_time = .true.
                  exit
               end if
            enddo
         end if
      else
         _ASSERT(.false.,"unknown bracket side")
      end if

      if (found_time .and. in_bounds) then
         _RETURN(_SUCCESS)
      else
         _RETURN(_FAILURE)
      end if 

   end subroutine get_time_on_file

   subroutine makeMetadata(file,collection_id,metadata,rc)
      character(len=*), intent(in   ) :: file
      integer, intent(in)                       :: collection_id
      type(FileMetadataUtils), pointer, intent(inout)   :: metadata
      integer, optional,          intent(out  ) :: rc
      type(MAPLExtDataCollection), pointer :: collection => null()
 
      Collection => ExtDataCollections%at(collection_id)
      metadata => collection%find(file)
     _RETURN(_SUCCESS)

  end subroutine makeMetadata
 

end module MAPL_ExtDataFileSeries
