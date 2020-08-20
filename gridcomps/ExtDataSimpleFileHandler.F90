#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtdataSimpleFileHandler
   use ESMF
   use MAPL_ExtDataAbstractFileHandler
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
   public ExtDataSimpleFileHandler

   type, extends(ExtDataAbstractFileHandler) :: ExtDataSimpleFileHandler
      contains
         procedure :: get_file_bracket
         procedure :: get_file
   end type

contains

   subroutine get_file_bracket(this, target_time, bracketside, source_time, output_file, time_index, output_time, rc)
      class(ExtdataSimpleFileHandler), intent(inout) :: this
      type(ESMF_Time), intent(in) :: target_time
      character(len=*), intent(in) :: bracketside
      integer, intent(in) :: source_time(:)
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
      class(ExtdataSimpleFileHandler), intent(inout) :: this
      character(len=*), intent(out) :: filename
      type(ESMF_Time) :: target_time
      integer, intent(in) :: shift
      integer, intent(out), optional :: rc

      type(ESMF_Time) :: ftime
      integer :: n,status
      logical :: file_found

      n = (target_time-this%reff_time)/this%frequency
      ftime = this%reff_time+(n+shift)*this%frequency
      call fill_grads_template(filename,this%file_template,time=ftime,__RC__)
      inquire(file=trim(filename),exist=file_found)
      _ASSERT(file_found,"get_file did not file a file using: "//trim(this%file_template))
      _RETURN(_SUCCESS)

   end subroutine get_file

end module MAPL_ExtdataSimpleFileHandler
