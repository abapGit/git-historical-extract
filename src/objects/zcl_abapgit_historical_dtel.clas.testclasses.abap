CLASS ltcl_dtel DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_dtel DEFINITION LOCAL FRIENDS ltcl_dtel.

CLASS ltcl_dtel DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_historical_dtel.

    METHODS setup.
    METHODS maps_domain FOR TESTING RAISING zcx_abapgit_exception.
    METHODS maps_predefined_type FOR TESTING RAISING zcx_abapgit_exception.
    METHODS maps_references FOR TESTING RAISING zcx_abapgit_exception.
    METHODS builds_deletion FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.


CLASS ltcl_dtel IMPLEMENTATION.

  METHOD setup.

    mo_cut = NEW #( VALUE #(
      object   = 'DTED'
      obj_name = 'ZTEST_DTEL' ) ).

  ENDMETHOD.


  METHOD maps_domain.

    DATA(ls_aff) = mo_cut->map_to_aff( VALUE #(
      ddlanguage = 'E'
      ddtext     = 'Test data element'
      refkind    = 'D'
      domname    = 'ZTEST_DOMAIN'
      scrtext_s  = 'Short'
      scrlen1    = 8
      scrtext_m  = 'Medium'
      scrlen2    = 15
      scrtext_l  = 'Long label'
      scrlen3    = 20
      reptext    = 'Heading'
      headlen    = 12
      shlpname   = 'ZTEST_SHLP'
      shlpfield  = 'VALUE'
      memoryid   = 'ZTEST'
      deffdname  = 'VALUE'
      logflag    = abap_true
      ltrflddis  = abap_true
      bidictrlc  = abap_true
      nohistory  = abap_true ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-data_type_information-category
      exp = zif_abapgit_aff_dtel_v1=>co_category-domain ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-data_type_information-type_name
      exp = 'ZTEST_DOMAIN' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-field_labels-short_length
      exp = 8 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-field_labels-heading_length
      exp = 12 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-additional_properties-search_help-name
      exp = 'ZTEST_SHLP' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-additional_properties-default_component_name
      exp = 'VALUE' ).
    cl_abap_unit_assert=>assert_true( ls_aff-additional_properties-no_input_history ).

  ENDMETHOD.


  METHOD maps_predefined_type.

    DATA(ls_aff) = mo_cut->map_to_aff( VALUE #(
      ddlanguage = 'E'
      ddtext     = 'Built-in type'
      datatype   = 'CHAR'
      leng       = 30 ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-data_type_information-category
      exp = zif_abapgit_aff_dtel_v1=>co_category-predefined_type ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-data_type_information-predefined_type-data_type
      exp = zif_abapgit_aff_ddic_types_v1=>co_data_type-char ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-data_type_information-predefined_type-length
      exp = 30 ).

  ENDMETHOD.


  METHOD maps_references.

    DATA(ls_aff) = mo_cut->map_to_aff( VALUE #(
      ddlanguage = 'E'
      ddtext     = 'Built-in reference'
      refkind    = 'R'
      reftype    = 'B'
      datatype   = 'CHAR'
      leng       = 10 ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-data_type_information-category
      exp = zif_abapgit_aff_dtel_v1=>co_category-reference_to_predefined_type ).

    ls_aff = mo_cut->map_to_aff( VALUE #(
      ddlanguage = 'E'
      ddtext     = 'Dictionary reference'
      refkind    = 'R'
      reftype    = 'E'
      domname    = 'ZOTHER_DTEL' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-data_type_information-category
      exp = zif_abapgit_aff_dtel_v1=>co_category-reference_dictionary_type ).

    ls_aff = mo_cut->map_to_aff( VALUE #(
      ddlanguage = 'E'
      ddtext     = 'Class reference'
      refkind    = 'R'
      reftype    = 'C'
      domname    = 'ZCL_TEST' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-data_type_information-category
      exp = zif_abapgit_aff_dtel_v1=>co_category-reference_clas_int_type ).

  ENDMETHOD.


  METHOD builds_deletion.

    DATA(lt_files) = mo_cut->zif_abapgit_historical_object~build_deleted_files( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 1 ]-filename
      exp = 'ztest_dtel.dtel.json' ).
    cl_abap_unit_assert=>assert_true( lt_files[ 1 ]-deleted ).

  ENDMETHOD.
ENDCLASS.
