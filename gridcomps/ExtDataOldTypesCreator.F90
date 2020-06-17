#include "MAPL_ErrLog.h"

module MAPL_ExtDataOldTypesCreator
   use ESMF
   use MAPL_BaseMod
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ExtDataTypeDef
   use MAPL_ExtDataYamlConfig
   use MAPL_ExtDataFileStream
   use MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataRule
   use MAPL_ExtDataRuleMap
   use MAPL_ExtDataDerived
   use MAPL_ExtDataDerivedMap
   use MAPL_RegridderSpecMod
   implicit none
   public :: ExtDataOldTypesCreator

   type, extends(ExtDataYamlConfig) :: ExtDataOldTypesCreator
      private
      contains
         procedure :: fillin_primary
         procedure :: fillin_derived
   end type ExtDataOldTypesCreator

   interface ExtDataOldTypesCreator
      module procedure :: new_ExtDataOldTypesCreator
   end interface

   contains

   function new_ExtDataOldTypesCreator(config_file,unusable,rc ) result(ExtDataObj)
      character(len=*), intent(in) :: config_file
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataOldTypesCreator) :: ExtDataObj

      integer :: status

      _UNUSED_DUMMY(unusable)
      ExtDataObj%ExtDataYamlConfig = ExtDataYamlConfig(config_file,rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function new_ExtDataOldTypesCreator

   
   subroutine fillin_primary(this,item_name,primary_item,unusable,rc)
      class(ExtDataOldTypesCreator), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      type(PrimaryExport), intent(inout) :: primary_item
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataRule), pointer :: rule
      type(ExtDataFileStream),  pointer :: dataset
      integer :: status, semi_pos

      rule => this%rule_map%at(trim(item_name))
      primary_item%isVector = (rule%vector_partner /= '')
      ! name and file var
      primary_item%name = trim(item_name)
      primary_item%vartype = MAPL_FieldItem
      if (primary_item%isVector) then
         primary_item%vcomp1 = trim(item_name)
         primary_item%vcomp2 = trim(rule%vector_partner)
         primary_item%var = rule%file_var
         primary_item%fcomp1 = rule%file_var
         primary_item%fcomp2 = rule%vector_file_partner
         primary_item%fileVars%itemType = ItemTypeVector
         primary_item%fileVars%xname  = trim(rule%file_var)
         primary_item%fileVars%yname  = trim(rule%vector_file_partner)
      else
         primary_item%vcomp1 = trim(item_name)
         primary_item%var = rule%file_var
         primary_item%fcomp1 = rule%file_var
         primary_item%fileVars%itemType = ItemTypeScalar
         primary_item%fileVars%xname  = trim(rule%file_var)
      end if
      
      ! units
      primary_item%units = ''
      ! climatology
      primary_item%cyclic = ESMF_UtilStringLowerCase(trim(rule%climatology))
      ! regrid method
      if (trim(rule%regrid_method) == "REGRID_METHOD_BILINEAR") then
         primary_item%trans = REGRID_METHOD_BILINEAR
      else if (trim(rule%regrid_method) == "REGRID_METHOD_CONSERVE") then
         primary_item%trans = REGRID_METHOD_CONSERVE
      else if (trim(rule%regrid_method) == "REGRID_METHOD_VOTE") then
         primary_item%trans = REGRID_METHOD_VOTE
      else if (index(rule%regrid_method,"REGRID_METHOD_FRACTION;")>0) then
         semi_pos = index(rule%regrid_method,";")
         read(rule%regrid_method(semi_pos+1:),*) primary_item%fracVal
         primary_item%trans = REGRID_METHOD_FRACTION
      else 
         _ASSERT(.false.,"Invalid regridding method")
      end if
      ! refresh_template
      if (rule%refresh_template(1:1)=='F') then
         primary_item%doInterpolate = .false.
         primary_item%refresh_template=rule%refresh_template(2:)
      else
         primary_item%doInterpolate = .true.
         primary_item%refresh_template=rule%refresh_template
      end if
      ! scale/shift
      if (rule%scaling == 0.0) then
         primary_item%do_scale = .false.
      else
         primary_item%do_scale = .true.
         primary_item%scale = rule%scaling
      end if
      if (rule%shift == 0.0) then
         primary_item%do_offset = .false.
      else
         primary_item%do_offset = .true.
         primary_item%offset = rule%shift
      end if

      ! file_template
      primary_item%isConst = .false.
      dataset => this%file_stream_map%at(trim(rule%file_template_key))
      primary_item%file = dataset%file_template
      if (primary_item%file(1:9) == '/dev/null') then
         primary_item%isConst = .true.
         semi_pos = index(primary_item%file,':')
         if (semi_pos > 9) then
            read(primary_item%file(semi_pos+1:),*)primary_item%const
         else
            primary_item%const=0.0
         end if
      end if
      if (dataset%old_file_freq /= '') then
         primary_item%fileReffTime = dataset%old_file_freq
         primary_item%hasFileReffTime = .true.
      else
         primary_item%hasFileReffTime = .false.
      end if 
      if (dataset%file_frequency /= '' .or. dataset%file_reference_date /= '') then
         _ASSERT(.false.,'Not yet support')
      end if

      _RETURN(_SUCCESS)

   end subroutine fillin_primary

   subroutine fillin_derived(this,item_name,derived_item,unusable,rc)
      class(ExtDataOldTypesCreator), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      type(DerivedExport), intent(inout) :: derived_item
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataDerived), pointer :: rule
      integer :: status

      rule => this%derived_map%at(trim(item_name))
      derived_item%name = trim(item_name)
      derived_item%expression = rule%expression
      derived_item%refresh_template = rule%refresh_template
      derived_item%masking=.false.
      if (index(derived_item%expression,"mask") /= 0 ) then
         derived_item%masking=.true.
      end if

      _RETURN(_SUCCESS)
 
   end subroutine fillin_derived

end module MAPL_ExtDataOldTypesCreator
