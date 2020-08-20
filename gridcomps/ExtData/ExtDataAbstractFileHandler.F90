#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtdataAbstractFileHandler
   use ESMF
   use yafYaml
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
   public :: ExtDataAbstractFileHandler

   type, abstract :: ExtDataAbstractFileHandler
      character(:), allocatable :: file_template
      type(ESMF_TimeInterval) :: frequency
      type(ESMF_Time) :: reff_time
      integer :: collection_id
      integer, allocatable :: valid_range(:)
      contains
         procedure :: initialize 
         procedure :: make_metadata
         procedure :: get_time_on_file
         procedure(get_file_bracket), deferred :: get_file_bracket
   end type

   abstract interface
      subroutine get_file_bracket(this,target_time, bracketside, source_time, output_file, time_index, output_time, rc)
         use ESMF
         import ExtDataAbstractFileHandler
         class(ExtDataAbstractFileHandler), intent(inout)  :: this
         type(ESMF_Time), intent(in) :: target_time
         character(len=*), intent(in) :: bracketside
         integer, intent(in) :: source_time(:)
         character(len=*), intent(inout) :: output_file
         integer, intent(out) :: time_index
         type(ESMF_Time), intent(out) :: output_time
         integer, optional, intent(out) :: rc
      end subroutine get_file_bracket

   end interface
         

contains

   subroutine initialize(this,config,current_time,unusable,rc)
      class(ExtDataAbstractFileHandler), intent(inout)  :: this
      type(Configuration), intent(in) :: config
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: last_token
      integer :: iyy,imm,idd,ihh,imn,isc 
      character(len=2) :: token
      character(len=:), allocatable :: file_frequency, file_reff_time
      logical :: is_present

      _UNUSED_DUMMY(unusable)

      call config%get(this%file_template,"file_template",default='',is_present=is_present,rc=status)
      _VERIFY(status)
      call config%get(file_frequency,"file_frequency",default='',rc=status)
      _VERIFY(status)
      if (file_frequency /= '') then
         this%frequency = string_to_esmf_timeinterval(file_frequency)
      else
         last_token = index(this%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            token = this%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeIntervalSet(this%frequency,yy=1,__RC__)
            case("m2")
               call ESMF_TimeIntervalSet(this%frequency,mm=1,__RC__)
            case("d2")
               call ESMF_TimeIntervalSet(this%frequency,d=1,__RC__)
            case("h2")
               call ESMF_TimeIntervalSet(this%frequency,h=1,__RC__)
            case("n2")
               call ESMF_TimeIntervalSet(this%frequency,m=1,__RC__)
            end select
         else
            ! couldn't find any tokens so all the data must be on one file
            call ESMF_TimeIntervalSet(this%frequency,__RC__)
         end if
      end if

      call config%get(file_reff_time,"file_reference_time",default='',rc=status)
      _VERIFY(status)
      if (file_reff_time /= '') then
         this%reff_time = string_to_esmf_time(file_reff_time)
      else
         last_token = index(this%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            call ESMF_TimeGet(current_time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,__RC__)
            token = this%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,__RC__)
            case("m2")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,__RC__)
            case("d2")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,__RC__)
            case("h2")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,__RC__)
            case("n2")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,__RC__)
            end select
         else
            this%reff_time = current_time 
         end if
      end if

      this%valid_range = config%at("valid_range")
      this%collection_id = MAPL_ExtDataAddCollection(this%file_template)

   end subroutine initialize

   subroutine get_time_on_file(this,filename,target_time,bracketside,time_index,output_time,rc)
      class(ExtdataAbstractFileHandler), intent(inout) :: this
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

      call this%make_metadata(filename,file_metadata,__RC__) 
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

   subroutine make_metadata(this,file,metadata,rc)
      class(ExtdataAbstractFileHandler), intent(inout) :: this
      character(len=*), intent(in   ) :: file
      type(FileMetadataUtils), pointer, intent(inout)   :: metadata
      integer, optional,          intent(out  ) :: rc
      type(MAPLExtDataCollection), pointer :: collection => null()
 
      Collection => ExtDataCollections%at(this%collection_id)
      metadata => collection%find(file)
     _RETURN(_SUCCESS)

  end subroutine make_metadata
 

end module MAPL_ExtdataAbstractFileHandler
