module MAPL_ExtDataTypeDef
   use ESMF
   use MAPL_SimpleAlarm
   use MAPL_newCFIOItemMod
   use MAPL_ExtDataBracket

   implicit none

   public PrimaryExport
   public DerivedExport
   public BracketingFields

  integer, parameter         :: MAPL_ExtDataNullFrac      = -9999

  type BracketingFields
     ! fields to store endpoints for interpolation of a vector pair
     type(ExtDataBracket) :: comp1
     type(ExtDataBracket) :: comp2
     ! if vertically interpolating vector fields
     type(ExtDataBracket) :: auxiliary1
     type(ExtDataBracket) :: auxiliary2
  end type BracketingFields

  type PrimaryExport
     character(len=ESMF_MAXSTR)   :: name
     character(len=ESMF_MAXSTR)   :: units
     character(len=ESMF_MAXSTR)   :: cyclic
     character(len=ESMF_MAXSTR)   :: refresh_template
     integer                      :: Trans
     character(len=ESMF_MAXSTR)   :: var
     character(len=ESMF_MAXPATHLEN)   :: file
     logical                      :: hasFileReffTime
     character(len=ESMF_MAXSTR)   :: FileReffTime

     type(ESMF_Time), pointer     :: refresh_time => null()
     logical                      :: isConst
     real                         :: Const
     integer                      :: vartype ! MAPL_FieldItem or MAPL_BundleItem

     type(ESMF_FieldBundle)       :: binterp1, binterp2
     type(ESMF_Time)              :: time1, time2
     type(ESMF_Time)              :: interp_time1, interp_time2
     integer                      :: tindex1,tindex2
     integer                      :: climyear
     type(ESMF_TimeInterval)      :: frequency
     type(ESMF_Time)              :: reff_time

     ! if primary export represents a pair of vector fields
     logical                      :: isVector, foundComp1, foundComp2
     type(BracketingFields)       :: modelGridFields

     ! names of the two vector components in the gridded component where import is declared
     character(len=ESMF_MAXSTR)   :: vcomp1, vcomp2
     ! the corresponding names of the two vector components on file
     character(len=ESMF_MAXSTR)   :: fcomp1, fcomp2
     type(newCFIOitem)            :: fileVars
     type(SimpleAlarm)            :: update_alarm

     integer                      :: pfioCollection_id
     integer                      :: iclient_collection_id

     logical                      :: ExtDataAlloc
     ! time shifting during continuous update
     type(ESMF_TimeInterval)      :: tshift
     logical                      :: alarmIsEnabled = .false.
     integer                      :: FracVal = MAPL_ExtDataNullFrac
     ! do we have to do vertical interpolation
     logical                      :: do_VertInterp = .false.
     logical                      :: do_Fill = .false.
     integer                      :: LM
     real, allocatable            :: levs(:)
     character(len=4)             :: importVDir = "down"
     character(len=4)             :: fileVDir = "down"
     character(len=ESMF_MAXSTR)   :: levUnit
     logical                      :: havePressure = .false.
  end type PrimaryExport
  
  type DerivedExport
     character(len=ESMF_MAXSTR)     :: name
     character(len=ESMF_MAXPATHLEN) :: expression
     character(len=ESMF_MAXSTR)     :: refresh_template
     logical                        :: ExtDataAlloc
     logical                        :: masking
     type(ESMF_Time), pointer       :: refresh_time => null()
     ! time shifting during continuous update
     type(ESMF_TimeInterval)      :: tshift
     type(SimpleAlarm)            :: update_alarm
     logical                      :: alarmIsEnabled = .false.
  end type DerivedExport


end module  MAPL_ExtDataTypeDef
