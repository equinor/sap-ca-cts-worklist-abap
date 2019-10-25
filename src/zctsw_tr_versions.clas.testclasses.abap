CLASS ltcl_ca_cts_tr_versions DEFINITION FINAL FOR TESTING
  DURATION LONG
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS: setup.

    METHODS:
      compare_text_content FOR TESTING RAISING cx_static_check,
      read_source_code FOR TESTING,
      tr_from_different_change for testing.

    DATA mo_cut TYPE REF TO zcl_ca_cts_tr_versions.
ENDCLASS.


CLASS ltcl_ca_cts_tr_versions IMPLEMENTATION.

  METHOD setup.
*-----------------------------------------------------------------------
* Initialize class object under test.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHGxxxxxxx ADKRA\774888 07.10.2019 Method created
*-----------------------------------------------------------------------
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD compare_text_content.
*-----------------------------------------------------------------------
* A quick test for comparing text content - generate HTML file.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHGxxxxxxx ADKRA\774888 07.10.2019 Method created
*-----------------------------------------------------------------------
    DATA: l_content1 TYPE string VALUE 'Adam Krawczyk',
          l_content2 TYPE string VALUE 'adam'.

    DATA(l_compare_html) = mo_cut->compare_text_content(
      EXPORTING
        i_file1_content = l_content1
        i_file2_content = l_content2
    ).

    DATA wb_object         TYPE REF TO cl_wb_object.
*
    "mo_cut->read_version_file( i_version = '00007' ).

*/sap/bc/adt/oo/classes/ZOTIF_CL_PO_CONF_NOTIF_IN/includes/main/versions/20190927152407/00007/content
*
*    DATA(version_info_provider) = cl_wb_object_version_provider=>create_instance( ).
*    DATA(version) = version_info_provider->get_version( pgmid = pgmid object_type = object_type object_name = object_name
*                                                          version_id = versionnumber timestamp = version_timestamp read_transport_description = abap_false ).
*
*        if version is bound.
*          version_content = me->get_vers_content_data_ref( ).
*          assign version_content->* to <version_content>.
*          version->get_content( importing data = <version_content> ).
*          response->set_body_data( content_handler = me->get_handler_for_vers_content( ) data = <version_content> ).
*
*    DATA(lo_class_reader) = new lcl_oo_class_version_provider( ).
*    DATA(lo_source_code) = lo_class_reader->get_version_source_public(
*      EXPORTING
*        object_name = 'ZOTIF_CL_PO_CONF_NOTIF_IN'
*        object_type = 'REPO'
*        versno      = '00007'
*    ).
  ENDMETHOD.



  METHOD read_source_code.
    DATA: l_object_type TYPE trobjtype VALUE 'REPS',
          l_object_name TYPE trobj_name VALUE 'ROIO_RT_CHANGE_DOCUMENTS'.


*  methods GET_VERSIONS
*    importing
*      !PGMID type PGMID optional
*      !OBJECT_TYPE type TROBJTYPE
*      !OBJECT_NAME type TROBJ_NAME
*    returning
*      value(RESULT) type ref to IF_WB_OBJECT_VERSION_LIST
*    raising
*      CX_WB_OBJECT_VERSIONING .
*  methods GET_VERSION
*    importing
*      !PGMID type PGMID optional
*      !OBJECT_TYPE type TROBJTYPE
*      !OBJECT_NAME type TROBJ_NAME
*      !VERSION_ID type VERSNO
*      !TIMESTAMP type TIMESTAMP optional
*      !read_transport_description type abap_bool default abap_true
*    returning
*      value(RESULT) type ref to IF_WB_OBJECT_VERSION
*    raising
*      CX_WB_OBJECT_VERSIONING .
*endinterface.

*    DATA(lo_code_reader) = CL_WB_OBJECT_VERSION_PROVIDER=>create_instance( ).
*    DATA(lt_code_versions) = lo_code_reader->get_versions(
*      EXPORTING
**        pgmid       =
*        object_type = l_object_type                 " Object Type
*        object_name = l_object_name                 " Object Name in Object List
**      RECEIVING
**        result      =
*    ).
**    CATCH cx_wb_object_versioning. " Exception class for object versioning
*
*    DATA(lt_versions_objects) = lt_code_versions->get_list( ).
*    DATA l_source_file_content type rswsourcet.
*    LOOP AT lt_versions_objects ASSIGNING FIELD-SYMBOL(<fs_version_object>).
*        DATA(ls_version) = <fs_version_object>->get_info( ).
*        <fs_version_object>->get_content(
*          IMPORTING
*            data = l_source_file_content
*        ).
**        CATCH cx_wb_object_versioning. " Exception class for object versioning
*    "read table lt_code_versions with key
*    endloop.
    DATA(l_source_code_ver_1) = mo_cut->read_version_file(
  EXPORTING
    l_object_type  = 'CLAS'
    l_object_name  = 'ZCL_PSMM_MATERIAL_TRACKING'
    i_version      = '00001'
*  RECEIVING
*    r_file_content =
    ).

      DATA(l_source_code_ver_3) = mo_cut->read_version_file(
  EXPORTING
    l_object_type  = 'PROG'
    l_object_name  = 'ROIO_RT_CHANGE_DOCUMENTS'
    i_version      = '00001'
*  RECEIVING
*    r_file_content =
    ).

        DATA(l_source_code_ver_3b) = mo_cut->read_version_file(
  EXPORTING
    l_object_type  = 'REPS'
    l_object_name  = 'ROIO_RT_CHANGE_DOCUMENTS'
    i_version      = '00001'
*  RECEIVING
*    r_file_content =
    ).

    cl_abap_unit_assert=>assert_equals(
        act =  l_source_code_ver_3
        exp =  l_source_code_ver_3b
        msg = 'REPS and PROG should be the same'
    ).


      DATA(l_source_code_ver_4) = mo_cut->read_version_file(
  EXPORTING
    l_object_type  = 'PROG'
    l_object_name  = 'ZXWOCI01'
    i_version      = '00082'
*  RECEIVING
*    r_file_content =
    ).

      DATA(l_source_code_ver_4b) = mo_cut->read_version_file(
  EXPORTING
    l_object_type  = 'REPS'
    l_object_name  = 'ZXWOCI01'
    i_version      = '00082'
*  RECEIVING
*    r_file_content =
    ).

     cl_abap_unit_assert=>assert_equals(
        act =  l_source_code_ver_4
        exp =  l_source_code_ver_4b
        msg = 'REPS and PROG should be the same'
    ).

    mo_cut->compare_text_content(
      EXPORTING
        i_file1_content = l_source_code_ver_1
        i_file2_content = l_source_code_ver_3
      RECEIVING
        r_compare_text  = DATA(l_compare_text)
    ).


  ENDMETHOD.

  METHOD tr_from_different_change.
*-----------------------------------------------------------------------
* Test that TR is coming from different or same change.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHGxxxxxxx ADKRA\774888 17.10.2019 Method created
*-----------------------------------------------------------------------

    cl_abap_unit_assert=>assert_false(
        act =  mo_cut->tr_from_different_change(
            i_description_tr1 = 'CHG0181927 - SCM Add logic for receiving new Y0 event from B'
            i_description_tr2 = 'CHG0181927 - SCM Add logic for receiving new Y0 event from B' )
        msg = 'Exactly same description comes from same change'
    ).

    cl_abap_unit_assert=>assert_false(
        act =  mo_cut->tr_from_different_change(
            i_description_tr1 = 'CHG0181927 - SCM Add logic for receiving new Y0 event from B'
            i_description_tr2 = ' CHG0181927 - SCM Add logic for receiving new Y0 event from ' )
        msg = 'Exactly same description ignoring first whitespace comes from same change'
    ).

   cl_abap_unit_assert=>assert_false(
        act =  mo_cut->tr_from_different_change(
            i_description_tr1 = 'CHG0181927 - SCM Add logic for receiving new Y0 event from B'
            i_description_tr2 = 'CHG0181927 - Let us say same change but different descriptio' )
        msg = 'Even if text differs but change number is the same, tr comes from same change'
    ).

       cl_abap_unit_assert=>assert_false(
        act =  mo_cut->tr_from_different_change(
            i_description_tr1 = 'CHG0181927 - Capital letters in description'
            i_description_tr2 = 'chg0181927 - Small leters in description' )
        msg = 'Transport are from same change even if desdcription uses small / capital letters'
    ).

    cl_abap_unit_assert=>assert_true(
        act =  mo_cut->tr_from_different_change(
            i_description_tr1 = 'CHG0181927 - SCM Add logic for receiving new Y0 event from B'
            i_description_tr2 = 'CHG1234567 - SCM Add logic for receiving new Y0 event from B' )
        msg = 'Different change number even if same description comes from different change'
    ).

   cl_abap_unit_assert=>assert_true(
        act =  mo_cut->tr_from_different_change(
            i_description_tr1 = ''
            i_description_tr2 = 'CHG1234567 - SCM Add logic for receiving new Y0 event from B' )
        msg = 'Empty description and non empty comes from different change'
    ).

    cl_abap_unit_assert=>assert_true(
        act =  mo_cut->tr_from_different_change(
            i_description_tr1 = ''
            i_description_tr2 = '' )
        msg = 'Two empty descriptions comes not from same change as we do not know it'
    ).

  ENDMETHOD.

ENDCLASS.
