#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_CapOptionsMod
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ErrorHandlingMod
   implicit none
   private

   public :: MAPL_CapOptions

   type :: MAPL_CapOptions

      integer :: comm
      logical :: use_comm_world = .true.
      character(:), allocatable :: egress_file
      character(:), allocatable :: cap_rc_file
      character(:), allocatable :: shared_obj
      character(:), allocatable :: proc_name
      type (ESMF_LogKind_Flag) :: esmf_logging_mode = ESMF_LOGKIND_NONE
      integer :: npes_model = -1
      ! only one of the next two options can be nonzero
      integer :: npes_input_server  = 0
      integer :: nodes_input_server = 0
      ! only one of the next two options can be nonzero
      integer :: npes_output_server = 0
      integer :: nodes_output_server= 0
      ! ensemble options
      integer :: n_members = 1
      character(:), allocatable :: ensemble_subdir_prefix

   end type MAPL_CapOptions

   interface MAPL_CapOptions
      module procedure new_CapOptions
   end interface

contains

   function new_CapOptions(unusable, cap_rc_file, egress_file, ensemble_subdir_prefix, proc_name, shared_obj, rc) result (cap_options)
      type (MAPL_CapOptions) :: cap_options
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: cap_rc_file
      character(*), optional, intent(in) :: egress_file
      character(*), optional, intent(in) :: ensemble_subdir_prefix 
      character(*), optional, intent(in) :: proc_name
      character(*), optional, intent(in) :: shared_obj

      integer, optional, intent(out) :: rc
      integer :: status

      _UNUSED_DUMMY(unusable)

      cap_options%cap_rc_file = 'CAP.rc'
      cap_options%egress_file = 'EGRESS'
      cap_options%ensemble_subdir_prefix = 'mem'
      cap_options%proc_name = "setservices"
      cap_options%shared_obj="none"

      if (present(cap_rc_file)) cap_options%cap_rc_file = cap_rc_file
      if (present(egress_file)) cap_options%egress_file = egress_file
      if (present(ensemble_subdir_prefix)) cap_options%ensemble_subdir_prefix = ensemble_subdir_prefix
      if (present(proc_name)) cap_options%proc_name = proc_name
      if (present(shared_obj)) cap_options%shared_obj = shared_obj

      _RETURN(_SUCCESS)

   end function

end module MAPL_CapOptionsMod

