esma_set_this (OVERRIDE MAPL.gridcomps)

set (srcs
     ExtDataFileStream.F90
     ExtDataRule.F90
     ExtDataDerived.F90
     ExtDataYamlConfig.F90
     ExtDataGridCompNG.F90
     TimeStringConversion.F90
     ExtDataTypeDef.F90
     ExtDataOldTypesCreator.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.base)
target_link_libraries (${this} PUBLIC gftl gftl-shared ${ESMF_LIBRARIES}
                               PRIVATE OpenMP::OpenMP_Fortran MPI::MPI_Fortran)
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF}
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

ecbuild_add_executable (TARGET test_ext.x SOURCES test_ext.F90)
target_link_libraries (test_ext.x ${this} MAPL.pfio ${ESMF_LIBRARIES} ${MPI_Fortran_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
set_target_properties (test_ext.x PROPERTIES Fortran_MODULE_DIRECTORY ${esma_include}/${this} LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
