CLASS ltcl_fugr_src DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS names_main_program FOR TESTING.
    METHODS names_namespaced_group FOR TESTING.
    METHODS parses_includes FOR TESTING.
    METHODS parses_chained_includes FOR TESTING.
    METHODS ignores_structure_includes FOR TESTING.
    METHODS ignores_commented_includes FOR TESTING.
    METHODS keeps_group_includes FOR TESTING.
    METHODS drops_function_includes FOR TESTING.
    METHODS drops_foreign_includes FOR TESTING.
    METHODS strips_regeneration_stamp FOR TESTING.
    METHODS strips_generation_header FOR TESTING.
    METHODS keeps_ordinary_source FOR TESTING.
    METHODS maps_parameters FOR TESTING.
    METHODS maps_tables_and_exceptions FOR TESTING.
    METHODS maps_documentation FOR TESTING.
ENDCLASS.


CLASS ltcl_fugr_src IMPLEMENTATION.

  METHOD names_main_program.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_hist_fugr_source=>get_main_program( 'ZTEST' )
      exp = 'SAPLZTEST' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_hist_fugr_source=>get_include_prefix( 'ZTEST' )
      exp = 'LZTEST' ).

  ENDMETHOD.


  METHOD names_namespaced_group.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_hist_fugr_source=>get_main_program( '/NSP/TEST' )
      exp = '/NSP/SAPLTEST' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_hist_fugr_source=>get_include_prefix( '/NSP/TEST' )
      exp = '/NSP/LTEST' ).

  ENDMETHOD.


  METHOD parses_includes.

    DATA lt_exp TYPE zcl_abapgit_hist_fugr_source=>ty_names.

    DATA(lt_includes) = zcl_abapgit_hist_fugr_source=>parse_includes(
      |*******************************************************************\n| &&
      |*   System-defined Include-files.                                 *\n| &&
      |*******************************************************************\n| &&
      |  INCLUDE LZTESTTOP.                         " Global Declarations\n| &&
      |  INCLUDE LZTESTUXX.                         " Function Modules\n| &&
      |\n| &&
      |*******************************************************************\n| &&
      |*   User-defined Include-files (if necessary).                    *\n| &&
      |*******************************************************************\n| &&
      |  include lztestf01.\n| &&
      |  INCLUDE zinclude IF FOUND.\n| &&
      |* INCLUDE LZTESTO...                         " PBO-Modules\n| ).

    INSERT `LZTESTTOP` INTO TABLE lt_exp.
    INSERT `LZTESTUXX` INTO TABLE lt_exp.
    INSERT `LZTESTF01` INTO TABLE lt_exp.
    INSERT `ZINCLUDE` INTO TABLE lt_exp.
    cl_abap_unit_assert=>assert_equals(
      act = lt_includes
      exp = lt_exp ).

  ENDMETHOD.


  METHOD parses_chained_includes.

    DATA lt_exp TYPE zcl_abapgit_hist_fugr_source=>ty_names.

    DATA(lt_includes) = zcl_abapgit_hist_fugr_source=>parse_includes(
      |INCLUDE: lztestf01,\n| &&
      |         lztestf02.\n| ).

    INSERT `LZTESTF01` INTO TABLE lt_exp.
    INSERT `LZTESTF02` INTO TABLE lt_exp.
    cl_abap_unit_assert=>assert_equals(
      act = lt_includes
      exp = lt_exp ).

  ENDMETHOD.


  METHOD ignores_structure_includes.

    DATA(lt_includes) = zcl_abapgit_hist_fugr_source=>parse_includes(
      |FUNCTION-POOL ztest.\n| &&
      |DATA BEGIN OF gs_data.\n| &&
      |INCLUDE STRUCTURE t000.\n| &&
      |DATA END OF gs_data.\n| &&
      |TYPES BEGIN OF ty_data.\n| &&
      |INCLUDE TYPE ty_base.\n| &&
      |TYPES END OF ty_data.\n| ).

    cl_abap_unit_assert=>assert_initial( lt_includes ).

  ENDMETHOD.


  METHOD ignores_commented_includes.

    DATA(lt_includes) = zcl_abapgit_hist_fugr_source=>parse_includes(
      |* INCLUDE lztestf01.\n| &&
      |WRITE 'INCLUDE lztestf02.'. " INCLUDE lztestf03.\n| ).

    cl_abap_unit_assert=>assert_initial( lt_includes ).

  ENDMETHOD.


  METHOD keeps_group_includes.

    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'LZTESTTOP' ) ).
    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'LZTESTF01' ) ).
    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'LZTESTT00' ) ).
* a longer include name starting with U is not a function module include
    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'LZTESTUTILS' ) ).
    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = '/NSP/TEST'
      iv_include = '/NSP/LTESTTOP' ) ).
* the TADIR entry decides for includes outside the naming scheme of function groups
    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'ZINCLUDE' ) ).

  ENDMETHOD.


  METHOD drops_function_includes.

    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'LZTESTUXX' ) ).
    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'LZTESTU01' ) ).
    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = '/NSP/TEST'
      iv_include = '/NSP/LTESTU01' ) ).

  ENDMETHOD.


  METHOD drops_foreign_includes.

    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'LSVIMFTX' ) ).
    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'LZOTHERTOP' ) ).
    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = '/NSP/LOTHERF01' ) ).
    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_fugr_source=>is_group_include(
      iv_group   = 'ZTEST'
      iv_include = 'ZTRANSFORMATION===============XTI' ) ).

  ENDMETHOD.


  METHOD strips_regeneration_stamp.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_hist_fugr_source=>strip_generation_comments(
        |*regenerated at 01.02.2020 10:00:00\n| &&
        |FUNCTION-POOL ztest MESSAGE-ID sv.| )
      exp = |FUNCTION-POOL ztest MESSAGE-ID sv.| ).

  ENDMETHOD.


  METHOD strips_generation_header.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_hist_fugr_source=>strip_generation_comments(
        |*---------------------------------------------------------------------*\n| &&
        |*    view related data declarations\n| &&
        |*   generation date: 01.02.2020 at 10:00:00\n| &&
        |*   view maintenance generator version: #001407#\n| &&
        |*---------------------------------------------------------------------*\n| &&
        |*...processing: ZTEST..........................................*| )
      exp = |*---------------------------------------------------------------------*\n| &&
            |*    view related data declarations\n| &&
            |*---------------------------------------------------------------------*\n| &&
            |*...processing: ZTEST..........................................*| ).

  ENDMETHOD.


  METHOD keeps_ordinary_source.

    DATA(lv_source) =
      |FUNCTION z_test.\n| &&
      |*"----------------------------------------------------------------------\n| &&
      |*"*"Local Interface:\n| &&
      |*"----------------------------------------------------------------------\n| &&
      |\n| &&
      |ENDFUNCTION.|.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_hist_fugr_source=>strip_generation_comments( lv_source )
      exp = lv_source ).

  ENDMETHOD.


  METHOD maps_parameters.

    DATA lt_parameters TYPE zcl_abapgit_hist_fugr_source=>ty_parameters.

* rows come in any order, the position decides
    INSERT VALUE #( parameter = 'IV_LIKE' paramtype = 'I' pposition = 2 structure = 'T000-MANDT'
                    optional = 'X' ) INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'IV_TYPE' paramtype = 'I' pposition = 1 structure = 'STRING'
                    type = 'X' reference = 'X' defaultval = `'A'` ) INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'IO_REF' paramtype = 'I' pposition = 3 structure = 'CL_ABAP_TYPEDESCR'
                    type = 'X' ref_class = 'X' ) INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'IV_FIELD' paramtype = 'I' pposition = 4 structure = 'T000-MANDT'
                    type = 'O' ) INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'EV_RESULT' paramtype = 'E' pposition = 1 structure = 'I'
                    type = 'X' reference = 'X' ) INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'CV_COUNT' paramtype = 'C' pposition = 1 structure = 'I'
                    type = 'X' ) INTO TABLE lt_parameters.

    DATA(ls_interface) = zcl_abapgit_hist_fugr_source=>map_interface(
      it_parameters = lt_parameters
      it_texts      = VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_interface-import )
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_interface-import[ 1 ]
      exp = VALUE zcl_abapgit_hist_fugr_source=>ty_importing(
        parameter = 'IV_TYPE'
        default   = `'A'`
        reference = 'X'
        typ       = 'STRING' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_interface-import[ 2 ]
      exp = VALUE zcl_abapgit_hist_fugr_source=>ty_importing(
        parameter = 'IV_LIKE'
        dbfield   = 'T000-MANDT'
        optional  = 'X' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_interface-import[ 3 ]
      exp = VALUE zcl_abapgit_hist_fugr_source=>ty_importing(
        parameter = 'IO_REF'
        typ       = 'CL_ABAP_TYPEDESCR'
        ref_class = 'X' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_interface-import[ 4 ]
      exp = VALUE zcl_abapgit_hist_fugr_source=>ty_importing(
        parameter = 'IV_FIELD'
        types     = 'O'
        typ       = 'T000-MANDT' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_interface-export
      exp = VALUE zcl_abapgit_hist_fugr_source=>ty_exporting_tt( (
        parameter = 'EV_RESULT'
        reference = 'X'
        typ       = 'I' ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_interface-changing
      exp = VALUE zcl_abapgit_hist_fugr_source=>ty_importing_tt( (
        parameter = 'CV_COUNT'
        typ       = 'I' ) ) ).

  ENDMETHOD.


  METHOD maps_tables_and_exceptions.

    DATA lt_parameters TYPE zcl_abapgit_hist_fugr_source=>ty_parameters.

    INSERT VALUE #( parameter = 'ET_LIKE' paramtype = 'T' pposition = 1 structure = 'T000'
                    optional = 'X' ) INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'ET_TYPE' paramtype = 'T' pposition = 2 structure = 'STRING_TABLE'
                    type = 'X' ) INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'NOT_FOUND' paramtype = 'X' pposition = 1 ) INTO TABLE lt_parameters.

    DATA(ls_interface) = zcl_abapgit_hist_fugr_source=>map_interface(
      it_parameters = lt_parameters
      it_texts      = VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_interface-tables
      exp = VALUE zcl_abapgit_hist_fugr_source=>ty_tables_tt(
        ( parameter = 'ET_LIKE'
          dbstruct  = 'T000'
          optional  = 'X' )
        ( parameter = 'ET_TYPE'
          typ       = 'STRING_TABLE' ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_interface-exception
      exp = VALUE zcl_abapgit_hist_fugr_source=>ty_exception_tt(
        ( exception = 'NOT_FOUND' ) ) ).

  ENDMETHOD.


  METHOD maps_documentation.

    DATA lt_parameters TYPE zcl_abapgit_hist_fugr_source=>ty_parameters.
    DATA lt_texts      TYPE zcl_abapgit_hist_fugr_source=>ty_texts.

    INSERT VALUE #( parameter = 'NOT_FOUND' paramtype = 'X' pposition = 1 ) INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'ET_DATA' paramtype = 'T' pposition = 1 structure = 'T000' ) INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'CV_COUNT' paramtype = 'C' pposition = 1 structure = 'I' type = 'X' )
      INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'EV_RESULT' paramtype = 'E' pposition = 1 structure = 'I' type = 'X' )
      INTO TABLE lt_parameters.
    INSERT VALUE #( parameter = 'IV_NAME' paramtype = 'I' pposition = 1 structure = 'STRING' type = 'X' )
      INTO TABLE lt_parameters.

    INSERT VALUE #( parameter = 'IV_NAME' kind = 'P' stext = 'Name' ) INTO TABLE lt_texts.
    INSERT VALUE #( parameter = 'NOT_FOUND' kind = 'X' stext = 'Nothing found' ) INTO TABLE lt_texts.
* a text of another kind than the parameter is not taken over
    INSERT VALUE #( parameter = 'EV_RESULT' kind = 'X' stext = 'Wrong kind' ) INTO TABLE lt_texts.

    DATA(ls_interface) = zcl_abapgit_hist_fugr_source=>map_interface(
      it_parameters = lt_parameters
      it_texts      = lt_texts ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_interface-documentation
      exp = VALUE zcl_abapgit_hist_fugr_source=>ty_documentation_tt(
        ( parameter = 'IV_NAME' kind = 'P' stext = 'Name' )
        ( parameter = 'EV_RESULT' kind = 'P' )
        ( parameter = 'CV_COUNT' kind = 'P' )
        ( parameter = 'ET_DATA' kind = 'P' )
        ( parameter = 'NOT_FOUND' kind = 'X' stext = 'Nothing found' ) ) ).

  ENDMETHOD.

ENDCLASS.
