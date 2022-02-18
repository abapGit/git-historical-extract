CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_extract DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_historical_extract.

    METHODS:
      setup,
      single_clas_version FOR TESTING,
      clas_with_meth FOR TESTING,
      two_clas_versions FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD single_clas_version.

    DATA(ls_tadir) = VALUE zif_abapgit_definitions=>ty_tadir(
      object   = 'CLAS'
      obj_name = 'ZCL_FOOBAR' ).

    DATA(lt_vrsd) = VALUE zcl_abapgit_historical_extract=>ty_vrsd_tt(
      ( objtype = 'CPUB' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'cpub' )
      ( objtype = 'CPRO' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'cpro' )
      ( objtype = 'CPRI' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'cpri' ) ).

    DATA(lt_files) = mo_cut->build(
      is_tadir = ls_tadir
      it_vrsd  = lt_vrsd ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 1 ]-source
      exp = |cpub\ncpro\ncpri\nCLASS zcl_foobar IMPLEMENTATION.\nENDCLASS.| ).

  ENDMETHOD.

  METHOD two_clas_versions.

    DATA(ls_tadir) = VALUE zif_abapgit_definitions=>ty_tadir(
      object   = 'CLAS'
      obj_name = 'ZCL_FOOBAR' ).

    DATA(lt_vrsd) = VALUE zcl_abapgit_historical_extract=>ty_vrsd_tt(
      ( objtype = 'CPUB' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'cpub' )
      ( objtype = 'CPRO' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'cpro1' )
      ( objtype = 'CPRO' objname = 'ZCL_FOOBAR' versno = '00002' datum = sy-datum zeit = sy-uzeit + 1 source = 'cpro2' )
      ( objtype = 'CPRI' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'cpri' ) ).

    DATA(lt_files) = mo_cut->build(
      is_tadir = ls_tadir
      it_vrsd  = lt_vrsd ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 1 ]-source
      exp = |cpub\ncpro1\ncpri\nCLASS zcl_foobar IMPLEMENTATION.\nENDCLASS.| ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 2 ]-source
      exp = |cpub\ncpro2\ncpri\nCLASS zcl_foobar IMPLEMENTATION.\nENDCLASS.| ).

  ENDMETHOD.

  METHOD clas_with_meth.

    DATA(ls_tadir) = VALUE zif_abapgit_definitions=>ty_tadir(
      object   = 'CLAS'
      obj_name = 'ZCL_FOOBAR' ).

    DATA(lt_vrsd) = VALUE zcl_abapgit_historical_extract=>ty_vrsd_tt(
      ( objtype = 'CPUB' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'cpub' )
      ( objtype = 'CPRO' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'cpro' )
      ( objtype = 'METH' objname = 'ZCL_FOOBAR                    ABC' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'method abc' )
      ( objtype = 'CPRI' objname = 'ZCL_FOOBAR' versno = '00001' datum = sy-datum zeit = sy-uzeit source = 'cpri' ) ).

    DATA(lt_files) = mo_cut->build(
      is_tadir = ls_tadir
      it_vrsd  = lt_vrsd ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 1 ]-source
      exp = |cpub\ncpro\ncpri\nCLASS zcl_foobar IMPLEMENTATION.\nmethod abc\nENDCLASS.| ).

  ENDMETHOD.

ENDCLASS.
