class ZCTSW_TR_VERSIONS definition
  public
  final
  create public .

public section.

  class-methods TR_FROM_DIFFERENT_CHANGE
    importing
      !I_DESCRIPTION_TR1 type AS4TEXT
      !I_DESCRIPTION_TR2 type AS4TEXT
    returning
      value(R_CHANGE_DIFFERS) type ABAP_BOOL .
  methods COMPARE_TEXT_CONTENT
    importing
      !I_FILE1_CONTENT type STRING
      !I_FILE2_CONTENT type STRING
      !I_FILE1_VERSION type IF_WB_OBJECT_VERSION=>TY_WB_OBJECT_VERSION optional
      !I_FILE2_VERSION type IF_WB_OBJECT_VERSION=>TY_WB_OBJECT_VERSION optional
      !I_OBJECT_NAME type TROBJ_NAME optional
    returning
      value(R_COMPARE_TEXT) type STRING .
  methods READ_VERSION_FILE
    importing
      !L_OBJECT_TYPE type TROBJTYPE default 'REPS'
      !L_OBJECT_NAME type TROBJ_NAME default 'ROIO_RT_CHANGE_DOCUMENTS'
      !I_VERSION type LS_VERSION
    exporting
      !E_VERSION_INFO type IF_WB_OBJECT_VERSION=>TY_WB_OBJECT_VERSION           " Version 00000 is active version
    returning
      value(R_FILE_CONTENT) type STRING .
  methods FIND_LAST_CHANGE_VERSIONS
    importing
      !L_OBJECT_TYPE type ZCTSW_COMPARISON_S-OBJECT_TYPE
      !L_OBJECT_NAME type ZCTSW_COMPARISON_S-OBJECT_NAME
    exporting
      !E_VERSION_PREVIOUS_CHANGE type ZCTSW_COMPARISON_S-VERSION_RIGHT
      !E_VERSION_LATEST type ZCTSW_COMPARISON_S-VERSION_LEFT .
  PROTECTED SECTION.
  PRIVATE SECTION.




    METHODS convert_to_xstring
      IMPORTING
        i_string         TYPE string
      RETURNING
        VALUE(r_xstring) TYPE xstring.

ENDCLASS.



CLASS ZCTSW_TR_VERSIONS IMPLEMENTATION.


  METHOD COMPARE_TEXT_CONTENT.
*-----------------------------------------------------------------------
* Compare two files represented as 2 string texts.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHGxxxxxxx ADKRA\774888 04.10.2019 Method created
*-----------------------------------------------------------------------

    DATA: ls_page_difference TYPE  zcl_cts_compare_code=>ty_file_diff,
          ls_file2           TYPE  zcl_cts_compare_code=>ty_file_diff.


    CREATE OBJECT ls_page_difference-o_diff
      EXPORTING
        iv_new = convert_to_xstring( i_file1_content )
        iv_old = convert_to_xstring( i_file2_content ).

    ls_page_difference-changed_by = i_file1_version-author.
    ls_page_difference-filename = |{ i_object_name } @@@ LOCAL #{ i_file1_version-versno } - "{ i_file1_version-trequest_text }"   @@@ REMOTE #{ i_file2_version-versno } - "{ i_file2_version-trequest_text }"|.

    if ( i_file2_content is initial and i_file1_content is not initial ).
        ls_page_difference-lstate = 'A'.
    elseif ( i_file1_content is initial and i_file2_content is not initial ).
        ls_page_difference-rstate = 'A'.
    elseif ( i_file1_content = i_file2_content ).
        ls_page_difference-lstate = '='.    " This will not be shown in renderer as modified
        ls_page_difference-rstate = '='.
    else.
        ls_page_difference-lstate = 'M'.
        ls_page_difference-rstate = 'M'.
    endif.

    DATA(lo_html) = zcl_cts_compare_code=>render_diff_public( ls_page_difference ).

    r_compare_text = lo_html->render( ).

  ENDMETHOD.


  METHOD CONVERT_TO_XSTRING.
*-----------------------------------------------------------------------
* Convert a single string into xstring.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHGxxxxxxx ADKRA\774888 07.10.2019 Method created
*-----------------------------------------------------------------------
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = i_string
*       mimetype = space
*       encoding =
      IMPORTING
        buffer = r_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD FIND_LAST_CHANGE_VERSIONS.
*-----------------------------------------------------------------------
* Find the latest version and previous version from other change.
* Search by comparing first string with change number.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHGxxxxxxx ADKRA\774888 17.10.2019 Method created
*-----------------------------------------------------------------------
    DATA(lo_code_reader) = cl_wb_object_version_provider=>create_instance( ).
    DATA(lt_code_versions) = lo_code_reader->get_versions(
      EXPORTING
*        pgmid       =
        object_type = l_object_type                 " Object Type
        object_name = l_object_name                 " Object Name in Object List
    ).
    "*    CATCH cx_wb_object_versioning. " Exception class for object versioning

    DATA(lt_versions_objects) = lt_code_versions->get_list( ).
    DATA l_source_file_content TYPE rswsourcet.
    DATA: ls_entity              TYPE REF TO if_rest_entity,
          ls_latest_version_info TYPE if_wb_object_version=>ty_wb_object_version.
    DATA: lt_version_info            TYPE TABLE OF if_wb_object_version=>ty_wb_object_version,
          ls_latest_version          LIKE LINE OF lt_version_info,
          ls_previous_change_version LIKE LINE OF lt_version_info.

    LOOP AT lt_versions_objects ASSIGNING FIELD-SYMBOL(<fs_version_object>).
      DATA(ls_version) = <fs_version_object>->get_info( ).
      INSERT ls_version INTO TABLE lt_version_info.
    ENDLOOP.

    SORT lt_version_info BY versno DESCENDING.
    LOOP AT lt_version_info ASSIGNING FIELD-SYMBOL(<fs_local_version>).
      IF ( sy-tabix = 1 ).
        ls_latest_version = <fs_local_version>.
         e_version_latest = ls_latest_version-versno.
      ELSEIF ( ls_previous_change_version IS INITIAL ).
        IF ( tr_from_different_change(
                i_description_tr1 = ls_latest_version-trequest_text
                i_description_tr2 = <fs_local_version>-trequest_text ) = abap_true ).
          ls_previous_change_version = <fs_local_version>.
          e_version_latest = ls_latest_version-versno.
          e_version_previous_change = ls_previous_change_version-versno.
          RETURN.
        ENDIF.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD READ_VERSION_FILE.
*-----------------------------------------------------------------------
* Try to read file version.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHGxxxxxxx ADKRA\774888 07.10.2019 Method created
*-----------------------------------------------------------------------
    DATA(lo_code_reader) = cl_wb_object_version_provider=>create_instance( ).
    DATA(lt_code_versions) = lo_code_reader->get_versions(
      EXPORTING
*        pgmid       =
        object_type = l_object_type                 " Object Type
        object_name = l_object_name                 " Object Name in Object List
    ).
*    CATCH cx_wb_object_versioning. " Exception class for object versioning

    DATA(lt_versions_objects) = lt_code_versions->get_list( ).
    DATA l_source_file_content TYPE rswsourcet.
    DATA: ls_entity              TYPE REF TO if_rest_entity,
          ls_latest_version_info TYPE if_wb_object_version=>ty_wb_object_version.

    LOOP AT lt_versions_objects ASSIGNING FIELD-SYMBOL(<fs_version_object>).
      DATA(ls_version) = <fs_version_object>->get_info( ).

      IF ( ls_version-versno = i_version ).

        <fs_version_object>->get_content(
          IMPORTING
            data = l_source_file_content
        ).

        CONCATENATE LINES OF l_source_file_content INTO r_file_content SEPARATED BY cl_abap_char_utilities=>cr_lf.
        e_version_info = ls_version.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD TR_FROM_DIFFERENT_CHANGE.
*-----------------------------------------------------------------------
* Check if there is different change based on transport request TR
* description.
* We assume that change format is CHG followed by 7 digits, like
* CHG1234567. As probably white spaces may occur, string is trimmed first
* to remove leading spaces.
* These are assumptions:
* - If both descriptions are empty we assume different change, as we
* cannot confirm if this is same change - edge case scenario
* - We ignore white spaces in the beginning, then check 10 characters
*   and only if they are the same then we get same change
* - Upper / lower case are ignored as same change number is regardless
*   of small or capital letters are used.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHGxxxxxxx ADKRA\774888 17.10.2019 Method created
*-----------------------------------------------------------------------

    DATA(l_description_tr1) = i_description_tr1.
    DATA(l_description_tr2) = i_description_tr2.

    CONDENSE l_description_tr1.
    CONDENSE l_description_tr2.

    DATA(l_change1_nr) = l_description_tr1(10).
    DATA(l_change2_nr) = l_description_tr2(10).

    translate l_change1_nr to UPPER CASE.
    translate l_change2_nr to UPPER CASE.

    if ( i_description_tr1 IS INITIAL and i_description_tr2 is initial ).
        r_change_differs = abap_true.
        return.
    endif.
    IF ( l_change1_nr <> l_change2_nr ).
      r_change_differs = abap_true.
    ELSE.
      r_change_differs = abap_false.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
