#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataBracket
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_BaseMod, only: MAPL_UNDEF
   implicit none
   private

   type, public :: ExtDataBracket
      type(ESMF_Field) :: left_field,right_field
      type(ESMF_Time)  :: left_time,right_time
      character(len=ESMF_MAXPATHLEN) :: left_file, right_file
      integer :: left_index, right_index
      real             :: scale_factor = 0.0
      real             :: offset = 0.0
      logical          :: disable_interpolation = .false.
      contains
         procedure :: interpolate_to_time
         procedure :: set_parameters
         procedure :: get_parameters
         procedure :: swap_fields
   end type

contains

   subroutine set_parameters(this, unusable, left_time, right_time, left_field, right_field, scale_factor, offset, disable_interpolation, rc)
      class(ExtDataBracket), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(in) :: left_time
      type(ESMF_Time), optional, intent(in) :: right_time
      type(ESMF_Field), optional, intent(in) :: left_field
      type(ESMF_Field), optional, intent(in) :: right_field
      real, optional, intent(in) :: scale_factor
      real, optional, intent(in) :: offset
      logical, optional, intent(in) :: disable_interpolation
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      if (present(left_time)) this%left_time = left_time
      if (present(right_time)) this%right_time = right_time
      if (present(left_field)) this%left_field = left_field
      if (present(right_field)) this%right_field = right_field
      if (present(scale_factor)) this%scale_factor = scale_factor
      if (present(offset)) this%offset=offset
      if (present(disable_interpolation)) this%disable_interpolation = disable_interpolation
      _RETURN(_SUCCESS)

   end subroutine set_parameters

   subroutine get_parameters(this, unusable, left_field, right_field, rc)
      class(ExtDataBracket), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Field), optional, intent(inout) :: left_field
      type(ESMF_Field), optional, intent(inout) :: right_field
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      if (present(left_field))  left_field = this%left_field
      if (present(right_field)) right_field = this%right_field
      _RETURN(_SUCCESS)

   end subroutine get_parameters

   subroutine interpolate_to_time(this,field,time,rc)
      class(ExtDataBracket), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_Time), intent(in) :: time
      integer, optional, intent(out) :: rc

      type(ESMF_TimeInterval)    :: tinv1, tinv2
      real                       :: alpha
      real, pointer              :: var2d(:,:)   => null()
      real, pointer              :: var3d(:,:,:) => null()
      real, pointer              :: var2d_left(:,:)   => null()
      real, pointer              :: var2d_right(:,:)   => null()
      real, pointer              :: var3d_left(:,:,:) => null()
      real, pointer              :: var3d_right(:,:,:) => null()
      integer                    :: field_rank
      integer :: status

      call ESMF_FieldGet(field,dimCount=field_rank,__RC__)
      alpha = 0.0
      if (.not.this%disable_interpolation) then
         tinv1 = time - this%left_time
         tinv2 = this%right_time - this%left_time
         alpha = tinv1/tinv2
      end if
      if (field_rank==2) then
         call ESMF_FieldGet(field,localDE=0,farrayPtr=var2d,__RC__)
         call ESMF_FieldGet(this%right_field,localDE=0,farrayPtr=var2d_right,__RC__)
         call ESMF_FieldGet(this%left_field,localDE=0,farrayPtr=var2d_left,__RC__)
         if (time == this%left_time .or. this%disable_interpolation) then
            var2d = var2d_left
         else if (time == this%right_time) then
            var2d = var2d_right
         else
            where( (var2d_left /= MAPL_UNDEF) .and. (var2d_right /= MAPL_UNDEF))
               var2d = var2d_left + alpha*(var2d_right-var2d_left)
            elsewhere
               var2d = MAPL_UNDEF
            endwhere
         end if

         if (this%scale_factor == 0.0 .and. this%offset /= 0.0) then
            where(var2d /= MAPL_UNDEF) var2d=var2d+this%offset
         end if
         if (this%scale_factor /= 0.0 .and. this%offset == 0.0) then
            where(var2d /= MAPL_UNDEF) var2d=var2d*this%scale_factor
         end if
         if (this%scale_factor /= 0.0 .and. this%offset /= 0.0) then
            where(var2d /= MAPL_UNDEF) var2d=var2d*this%scale_factor+this%offset
         end if

      else if (field_rank==3) then
         call ESMF_FieldGet(field,localDE=0,farrayPtr=var3d,__RC__)
         call ESMF_FieldGet(this%right_field,localDE=0,farrayPtr=var3d_right,__RC__)
         call ESMF_FieldGet(this%left_field,localDE=0,farrayPtr=var3d_left,__RC__)
         if (time == this%left_time .or. this%disable_interpolation) then
            var3d = var3d_left
         else if (time == this%right_time) then
            var3d = var3d_right
         else
            where( (var3d_left /= MAPL_UNDEF) .and. (var3d_right /= MAPL_UNDEF))
               var3d = var3d_left + alpha*(var3d_right-var3d_left)
            elsewhere
               var3d = MAPL_UNDEF
            endwhere
         end if

         if (this%scale_factor == 0.0 .and. this%offset /= 0.0) then
            where(var3d /= MAPL_UNDEF) var3d=var3d+this%offset
         end if
         if (this%scale_factor /= 0.0 .and. this%offset == 0.0) then
            where(var3d /= MAPL_UNDEF) var3d=var3d*this%scale_factor
         end if
         if (this%scale_factor /= 0.0 .and. this%offset /= 0.0) then
            where(var3d /= MAPL_UNDEF) var3d=var3d*this%scale_factor+this%offset
         end if

      end if
      _RETURN(_SUCCESS)

   end subroutine interpolate_to_time

   subroutine swap_fields(this,rc)
      class(ExtDataBracket), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: field_rank
      real, pointer :: var3d_left(:,:,:),var3d_right(:,:,:)
      real, pointer :: var2d_left(:,:),var2d_right(:,:)

      this%left_time = this%right_time
      call ESMF_FieldGet(this%left_field,dimCount=field_rank,__RC__)
      if (field_rank == 2) then
         call ESMF_FieldGet(this%right_field,localDE=0,farrayPtr=var2d_right,__RC__)
         call ESMF_FieldGet(this%left_field,localDE=0,farrayPtr=var2d_left,__RC__)
         var2d_left = var2d_right
      else if (field_rank ==3) then
         call ESMF_FieldGet(this%right_field,localDE=0,farrayPtr=var3d_right,__RC__)
         call ESMF_FieldGet(this%left_field,localDE=0,farrayPtr=var3d_left,__RC__)
         var3d_left = var3d_right
      end if
      _RETURN(_SUCCESS)
   end subroutine swap_fields

end module MAPL_ExtDataBracket
