CLASS zctsw_tr_versions DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS tr_from_different_change
      IMPORTING
        !i_description_tr1      TYPE as4text
        !i_description_tr2      TYPE as4text
      RETURNING
        VALUE(r_change_differs) TYPE abap_bool .
    METHODS compare_text_content
      IMPORTING
        !i_file1_content      TYPE string
        !i_file2_content      TYPE string
        !i_file1_version      TYPE if_wb_object_version=>ty_wb_object_version OPTIONAL
        !i_file2_version      TYPE if_wb_object_version=>ty_wb_object_version OPTIONAL
        !i_object_name        TYPE trobj_name OPTIONAL
      RETURNING
        VALUE(r_compare_text) TYPE string .
    METHODS read_version_file
      IMPORTING
        !l_object_type        TYPE trobjtype DEFAULT 'REPS'
        !l_object_name        TYPE trobj_name DEFAULT 'ROIO_RT_CHANGE_DOCUMENTS'
        !i_version            TYPE ls_version
      EXPORTING
        !e_version_info       TYPE if_wb_object_version=>ty_wb_object_version           " Version 00000 is active version
      RETURNING
        VALUE(r_file_content) TYPE string .
    METHODS find_last_change_versions
      IMPORTING
        !l_object_type             TYPE zctsw_comparison_s-object_type
        !l_object_name             TYPE zctsw_comparison_s-object_name
      EXPORTING
        !e_version_previous_change TYPE zctsw_comparison_s-version_right
        !e_version_latest          TYPE zctsw_comparison_s-version_left .
  PROTECTED SECTION.
  PRIVATE SECTION.




    METHODS convert_to_xstring
      IMPORTING
        i_string         TYPE string
      RETURNING
        VALUE(r_xstring) TYPE xstring.

ENDCLASS.



CLASS ZCTSW_TR_VERSIONS IMPLEMENTATION.


  METHOD compare_text_content.
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

    IF ( i_file2_content IS INITIAL AND i_file1_content IS NOT INITIAL ).
      ls_page_difference-lstate = 'A'.
    ELSEIF ( i_file1_content IS INITIAL AND i_file2_content IS NOT INITIAL ).
      ls_page_difference-rstate = 'A'.
    ELSEIF ( i_file1_content = i_file2_content ).
      ls_page_difference-lstate = '='.    " This will not be shown in renderer as modified
      ls_page_difference-rstate = '='.
    ELSE.
      ls_page_difference-lstate = 'M'.
      ls_page_difference-rstate = 'M'.
    ENDIF.

    DATA(lo_html) = zcl_cts_compare_code=>render_diff_public( ls_page_difference ).

    r_compare_text = lo_html->render( ).

  ENDMETHOD.


  METHOD convert_to_xstring.
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


  METHOD find_last_change_versions.
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

*   At this point, versions are sorted by creation time descending. If the transport is released
*   version 0 will be listed 2nd from the top of the list, and it will not be related to a transport
*   number. We need to remove it from the list, as it is not valid as a comparison object (the lastest
*   version will be used).
*
*   For an unreleased transport, version 0 is the latest version and it is related to a transport. The
*   current sorting is correct.
    DELETE lt_version_info WHERE versno = '0000' AND korrnum IS INITIAL.

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


  METHOD read_version_file.
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


  METHOD tr_from_different_change.
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

    TRANSLATE l_change1_nr TO UPPER CASE.
    TRANSLATE l_change2_nr TO UPPER CASE.

    IF ( i_description_tr1 IS INITIAL AND i_description_tr2 IS INITIAL ).
      r_change_differs = abap_true.
      RETURN.
    ENDIF.
    IF ( l_change1_nr <> l_change2_nr ).
      r_change_differs = abap_true.
    ELSE.
      r_change_differs = abap_false.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
