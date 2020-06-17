program test_dataset
   use esmf
   USE yaFyaml
   USE gFTL_IntegerVector
   USE gftl_StringIntegerMap
   USE MAPL_ExtDataFileStream
   USE MAPL_ExtDataFileStreamMap
   USE MAPL_ExtDataRule
   USE MAPL_ExtDataRuleMap
   use MAPL_ExtDataYamlConfig
   use MAPL_TimeStringConversion
   implicit none

   type(ExtDataFileStreamMapIterator) :: ds_iter
   type(ExtDataFileStream), pointer :: dsp

   type(ExtDataRuleMap) :: rule_map
   type(ExtDataRuleMapIterator) :: rule_iter
   class(ExtDataRule), pointer :: rulep
   character(len=:), pointer :: key

   integer :: status
   type(ExtDataYamlConfig),target :: config
   
   config = ExtDataYamlConfig("extdata.yaml") 

   write(*,*)'bmaa report in tester'
   ds_iter = config%file_stream_map%begin()
   do while (ds_iter /= config%file_stream_map%end())
      key => ds_iter%key()
      dsp => ds_iter%value()
      write(*,*)'data key: ',trim(key)
      call dsp%display()
      call ds_iter%next()
   enddo

   rule_iter = config%rule_map%begin()
   do while (rule_iter /= config%rule_map%end())
      key => rule_iter%key()
      rulep => rule_iter%value()
      write(*,*)'rule key: ',trim(key)
      call rulep%display()
      call rule_iter%next()
   enddo

end program test_dataset
   
