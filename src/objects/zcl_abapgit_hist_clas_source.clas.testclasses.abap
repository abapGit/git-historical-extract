CLASS ltcl_clas_src DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS parses_methods FOR TESTING.
    METHODS parses_chains FOR TESTING.
    METHODS ignores_comments_and_literals FOR TESTING.
    METHODS parses_interfaces FOR TESTING.
    METHODS checks_declared FOR TESTING.
    METHODS checks_content FOR TESTING.
    METHODS builds_main_source FOR TESTING.
    METHODS builds_without_methods FOR TESTING.
    METHODS keeps_existing_endclass FOR TESTING.
ENDCLASS.


CLASS ltcl_clas_src IMPLEMENTATION.

  METHOD parses_methods.

    DATA(ls_declarations) = zcl_abapgit_hist_clas_source=>parse_declarations(
      |class ZCL_TEST definition\n| &&
      |  public\n| &&
      |  create public .\n| &&
      |\n| &&
      |public section.\n| &&
      |  methods CONSTRUCTOR\n| &&
      |    importing\n| &&
      |      !IV_NAME type STRING .\n| &&
      |  class-methods CREATE\n| &&
      |    returning\n| &&
      |      value(RO_RESULT) type ref to ZCL_TEST .\n| &&
      |  methods ZIF_TEST~RUN\n| &&
      |    redefinition .\n| &&
      |  data METHODS type I .\n| ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_declarations-methods
      exp = VALUE zcl_abapgit_hist_clas_source=>ty_names(
        ( `CONSTRUCTOR` )
        ( `CREATE` )
        ( `ZIF_TEST~RUN` ) ) ).
    cl_abap_unit_assert=>assert_initial( ls_declarations-interfaces ).

  ENDMETHOD.


  METHOD parses_chains.

    DATA(ls_declarations) = zcl_abapgit_hist_clas_source=>parse_declarations(
      |  PRIVATE SECTION.\n| &&
      |    METHODS:\n| &&
      |      first IMPORTING iv_a TYPE i,\n| &&
      |      second,\n| &&
      |      third RETURNING VALUE(rv_x) TYPE i.\n| &&
      |    TYPES: BEGIN OF ty_s,\n| &&
      |             methods TYPE i,\n| &&
      |           END OF ty_s.\n| ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_declarations-methods
      exp = VALUE zcl_abapgit_hist_clas_source=>ty_names(
        ( `FIRST` )
        ( `SECOND` )
        ( `THIRD` ) ) ).

  ENDMETHOD.


  METHOD ignores_comments_and_literals.

    DATA(ls_declarations) = zcl_abapgit_hist_clas_source=>parse_declarations(
      |* METHODS commented_out.\n| &&
      |  METHODS real "METHODS in a comment.\n| &&
      |    IMPORTING iv_sep TYPE c DEFAULT ',' iv_end TYPE c DEFAULT '.'.\n| &&
      |  "! METHODS abap_doc.\n| &&
      |  CONSTANTS c_text TYPE string VALUE `a: METHODS b, c.`.\n| &&
      |  METHODS after_literals.\n| ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_declarations-methods
      exp = VALUE zcl_abapgit_hist_clas_source=>ty_names(
        ( `AFTER_LITERALS` )
        ( `REAL` ) ) ).

  ENDMETHOD.


  METHOD parses_interfaces.

    DATA(ls_declarations) = zcl_abapgit_hist_clas_source=>parse_declarations(
      |public section.\n| &&
      |  interfaces ZIF_FIRST .\n| &&
      |  interfaces:\n| &&
      |    ZIF_SECOND ,\n| &&
      |    ZIF_THIRD all methods abstract .\n| ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_declarations-interfaces
      exp = VALUE zcl_abapgit_hist_clas_source=>ty_names(
        ( `ZIF_FIRST` )
        ( `ZIF_SECOND` )
        ( `ZIF_THIRD` ) ) ).

  ENDMETHOD.


  METHOD checks_declared.

    DATA(ls_declarations) = VALUE zcl_abapgit_hist_clas_source=>ty_declarations(
      methods    = VALUE #( ( `RUN` ) ( `ZIF_OTHER~REDEFINED` ) )
      interfaces = VALUE #( ( `ZIF_TEST` ) ) ).

    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_clas_source=>is_declared(
      is_declarations = ls_declarations
      iv_method       = `RUN` ) ).
    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_clas_source=>is_declared(
      is_declarations = ls_declarations
      iv_method       = `ZIF_TEST~ANY` ) ).
    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_clas_source=>is_declared(
      is_declarations = ls_declarations
      iv_method       = `ZIF_OTHER~REDEFINED` ) ).
    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_clas_source=>is_declared(
      is_declarations = ls_declarations
      iv_method       = `REMOVED` ) ).
    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_clas_source=>is_declared(
      is_declarations = ls_declarations
      iv_method       = `ZIF_GONE~RUN` ) ).

  ENDMETHOD.


  METHOD checks_content.

    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_clas_source=>has_content( `` ) ).
    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_clas_source=>has_content(
      |*"* use this source file for any type of declarations (class\n| &&
      |*"* definitions, interfaces or type declarations) you need for\n| &&
      |*"* components in the private section\n| ) ).
    cl_abap_unit_assert=>assert_false( zcl_abapgit_hist_clas_source=>has_content( |*\n| ) ).
    cl_abap_unit_assert=>assert_true( zcl_abapgit_hist_clas_source=>has_content(
      |*"* use this source file for your ABAP unit test classes\n| &&
      |CLASS ltcl_test DEFINITION FOR TESTING.\n| ) ).

  ENDMETHOD.


  METHOD builds_main_source.

    DATA lt_methods TYPE zcl_abapgit_hist_clas_source=>ty_methods.

* inserted out of order, the sorted table puts them in alphabetical order
    INSERT VALUE #( name = `SECOND` source = |  METHOD second.\n  ENDMETHOD.\n\n| ) INTO TABLE lt_methods.
    INSERT VALUE #( name = `FIRST` source = |  METHOD first.\n  ENDMETHOD.| ) INTO TABLE lt_methods.

    DATA(lv_source) = zcl_abapgit_hist_clas_source=>build_main_source(
      iv_class_name = `ZCL_TEST`
      iv_public     = |class ZCL_TEST definition\n  public\n  create public .\n\npublic section.\n|
      iv_protected  = |protected section.|
      iv_private    = |private section.|
      it_methods    = lt_methods ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_source
      exp = |class ZCL_TEST definition\n| &&
            |  public\n| &&
            |  create public .\n| &&
            |\n| &&
            |public section.\n| &&
            |\n| &&
            |protected section.\n| &&
            |private section.\n| &&
            |ENDCLASS.\n| &&
            |\n| &&
            |\n| &&
            |\n| &&
            |CLASS ZCL_TEST IMPLEMENTATION.\n| &&
            |\n| &&
            |\n| &&
            |  METHOD first.\n| &&
            |  ENDMETHOD.\n| &&
            |\n| &&
            |\n| &&
            |  METHOD second.\n| &&
            |  ENDMETHOD.\n| &&
            |ENDCLASS.\n| ).

  ENDMETHOD.


  METHOD builds_without_methods.

    DATA(lv_source) = zcl_abapgit_hist_clas_source=>build_main_source(
      iv_class_name = `ZCL_TEST`
      iv_public     = |class ZCL_TEST definition public.\npublic section.|
      iv_protected  = ``
      iv_private    = ``
      it_methods    = VALUE #( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_source
      exp = |class ZCL_TEST definition public.\n| &&
            |public section.\n| &&
            |ENDCLASS.\n| &&
            |\n| &&
            |\n| &&
            |\n| &&
            |CLASS ZCL_TEST IMPLEMENTATION.\n| &&
            |ENDCLASS.\n| ).

  ENDMETHOD.


  METHOD keeps_existing_endclass.

    DATA(lv_source) = zcl_abapgit_hist_clas_source=>build_main_source(
      iv_class_name = `ZCL_TEST`
      iv_public     = |class ZCL_TEST definition public.\npublic section.|
      iv_protected  = ``
      iv_private    = |private section.\nendclass. "ZCL_TEST definition\n|
      it_methods    = VALUE #( ) ).

* one ENDCLASS closes the definition, the other the implementation
    cl_abap_unit_assert=>assert_equals(
      act = count( val = to_upper( lv_source ) sub = `ENDCLASS.` )
      exp = 2 ).

  ENDMETHOD.
ENDCLASS.
