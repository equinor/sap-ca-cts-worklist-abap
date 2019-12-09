CLASS zctsw_transport_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zctsw_transport_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /iwbep/if_mgw_appl_srv_runtime~execute_action
        REDEFINITION .
  PROTECTED SECTION.

    METHODS import_systemset_get_entity
        REDEFINITION .
    METHODS import_systemset_get_entityset
        REDEFINITION .
    METHODS objectset_get_entityset
        REDEFINITION .
    METHODS requestset_get_entity
        REDEFINITION .
    METHODS requestset_get_entityset
        REDEFINITION .
    METHODS taskset_get_entity
        REDEFINITION .
    METHODS taskset_get_entityset
        REDEFINITION .
    METHODS userset_get_entityset
        REDEFINITION .
    METHODS userset_get_entity
        REDEFINITION .
    METHODS compareversionss_get_entity
        REDEFINITION.
  PRIVATE SECTION.
    METHODS set_date_range
      IMPORTING
        i_since_x_days TYPE i
      CHANGING
        ct_date_range  TYPE trgr_date.
    METHODS sort_and_compress
      CHANGING
        ct_entityset TYPE zctsw_transport_mpc=>tt_object.

ENDCLASS.



CLASS zctsw_transport_dpc_ext IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

    DATA: ls_return TYPE zctsw_transport_mpc=>ts_returncode.

    CASE iv_action_name.
      WHEN 'Release'.
        zctsw_transport_dao=>release_transport(
          EXPORTING
            i_trkorr     = CONV #( it_parameter[ name = 'Trkorr' ]-value )
          IMPORTING
            e_error_code = ls_return-error_code
            e_error_text = ls_return-text
        ).

        IF NOT ls_return-error_code IS INITIAL.
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid  = /iwbep/cx_mgw_busi_exception=>business_error
              message = CONV #( ls_return-text ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD compareversionss_get_entity.
*-----------------------------------------------------------------------
* Compare data for two source code objects - source code content
* must be provided in two key parameters, this will generate third
* entity atribute with HTML rendered difference.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHGxxxxxxx ADKRA\774888 08.10.2019 Method created
*-----------------------------------------------------------------------

    DATA: ls_compare_entity    TYPE zctsw_transport_mpc_ext=>ts_compareversions,
          l_version_info_left  TYPE if_wb_object_version=>ty_wb_object_version,
          l_version_info_right TYPE if_wb_object_version=>ty_wb_object_version.


    io_tech_request_context->get_converted_keys(
      IMPORTING
        es_key_values = ls_compare_entity
    ).


    DATA(lo_version_comparison) = NEW zctsw_tr_versions( ).

    IF ( ls_compare_entity-version_left = '00000' AND ls_compare_entity-version_right = '00000' ).
      lo_version_comparison->find_last_change_versions(
          EXPORTING
           l_object_type = ls_compare_entity-object_type
           l_object_name = ls_compare_entity-object_name
          IMPORTING
           e_version_previous_change  = ls_compare_entity-version_right
           e_version_latest           = ls_compare_entity-version_left
           ).
    ENDIF.


    DATA(l_source_code_left) = lo_version_comparison->read_version_file(
        EXPORTING
            l_object_type = ls_compare_entity-object_type
            l_object_name = ls_compare_entity-object_name
            i_version     = ls_compare_entity-version_left
        IMPORTING
            e_version_info = l_version_info_left
                 ).

    DATA(l_source_code_right) = lo_version_comparison->read_version_file(
        EXPORTING
            l_object_type = ls_compare_entity-object_type
            l_object_name = ls_compare_entity-object_name
            i_version     = ls_compare_entity-version_right
        IMPORTING
            e_version_info = l_version_info_right ).

    IF ( ls_compare_entity-version_right = 0 AND ls_compare_entity-version_right = 0 ).
      " New object without version history
      CLEAR l_source_code_right.
    ENDIF.

    ls_compare_entity-rendered_compare_html =  lo_version_comparison->compare_text_content(
      EXPORTING
        i_file1_content = l_source_code_left
        i_file2_content = l_source_code_right
        i_file1_version = l_version_info_left
        i_file2_version = l_version_info_right
        i_object_name   = ls_compare_entity-object_name
     ).

    "ls_compare_entity-rendered_compare_html = '<DIV>Adam test #</DIV>'.
    er_entity = ls_compare_entity.


  ENDMETHOD.

  METHOD import_systemset_get_entity.
*-----------------------------------------------------------------------
* Get single import system for transport
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: lo_dao    TYPE REF TO zctsw_transport_dao.

    CREATE OBJECT lo_dao.

    DATA(ls_migration_status) = lo_dao->get_migration_status( CONV #( it_key_tab[ name = 'Trkorr' ]-value ) ).

    LOOP AT ls_migration_status-systems INTO DATA(ls_system) WHERE systemid = it_key_tab[ name = 'Systemid' ]-value.
      er_entity-systemid = ls_system-systemid.
      er_entity-trkorr   = it_key_tab[ name = 'Trkorr' ]-value.
      er_entity-rc       = ls_system-rc.
    ENDLOOP.


  ENDMETHOD.


  METHOD import_systemset_get_entityset.
*-----------------------------------------------------------------------
* Get list of import system for transport
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: lo_dao    TYPE REF TO zctsw_transport_dao,
          ls_entity TYPE zctsw_transport_mpc=>ts_import_system.

    CREATE OBJECT lo_dao.

    DATA(ls_migration_status) = lo_dao->get_migration_status( CONV #( it_key_tab[ 1 ]-value ) ).

    LOOP AT ls_migration_status-systems INTO DATA(ls_system).
      ls_entity-systemid = ls_system-systemid.
      ls_entity-trkorr   = it_key_tab[ 1 ]-value.
      ls_entity-rc       = ls_system-rc.
      APPEND ls_entity TO et_entityset.
    ENDLOOP.

  ENDMETHOD.


  METHOD objectset_get_entityset.
*-----------------------------------------------------------------------
* Get all objects in a transport
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: lo_dao            TYPE REF TO zctsw_transport_dao,
          ls_transport_line TYPE cnvc_scwb_trs,
          lt_transports     TYPE cnvc_scwb_tr,
          ls_filter         TYPE /iwbep/s_mgw_select_option,
          lt_is_source      TYPE fkk_sg_range_xfield_tab,
          lt_objects        TYPE /iwbep/t_cod_select_options.

    CREATE OBJECT lo_dao.

    DATA(lt_filters) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    IF NOT it_key_tab IS INITIAL.
      ls_transport_line-sign = 'I'.
      ls_transport_line-option = 'EQ'.

      LOOP AT it_key_tab INTO DATA(ls_key).
        ls_transport_line-low = ls_key-value.
        APPEND ls_transport_line TO lt_transports.
      ENDLOOP.
    ELSE.
      READ TABLE lt_filters WITH KEY property = 'TRKORR' INTO ls_filter.
      IF sy-subrc = 0.
        lt_transports = CORRESPONDING #( ls_filter-select_options ).
      ENDIF.
      READ TABLE lt_filters WITH KEY property = 'OBJ_NAME' INTO ls_filter.
      IF sy-subrc = 0.
        lt_objects = CORRESPONDING #( ls_filter-select_options ).
      ENDIF.
    ENDIF.

    IF NOT lt_transports IS INITIAL OR NOT lt_objects IS INITIAL.
      APPEND LINES OF lo_dao->get_objects( it_transports = lt_transports it_objects = lt_objects ) TO et_entityset.
    ENDIF.

    "Filter on IS_SOURCE, as this is not handled correctly by the standard filter method
    READ TABLE lt_filters WITH KEY property = 'IS_SOURCE' INTO ls_filter.
    IF sy-subrc = 0.
      lt_is_source = CORRESPONDING #( ls_filter-select_options ).
    ENDIF.
    DELETE et_entityset WHERE NOT is_source IN lt_is_source.

    sort_and_compress(
      CHANGING
        ct_entityset = et_entityset ).


*    "Filter
*    /iwbep/cl_mgw_data_util=>filtering(
*      EXPORTING
*        it_select_options = it_filter_select_options
*      CHANGING
*        ct_data           = et_entityset
*    ).

    "Paging
    /iwbep/cl_mgw_data_util=>paging(
      EXPORTING
        is_paging = is_paging
      CHANGING
        ct_data   = et_entityset ).

    "Order by
    /iwbep/cl_mgw_data_util=>orderby(
      EXPORTING
        it_order = it_order
      CHANGING
        ct_data  = et_entityset
    ).




  ENDMETHOD.


  METHOD requestset_get_entity.
*-----------------------------------------------------------------------
* Get single request
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: lo_dao              TYPE REF TO zctsw_transport_dao,
          lt_transports_range TYPE cnvc_scwb_tr,
          ls_transport_line   TYPE cnvc_scwb_trs.

    CREATE OBJECT lo_dao.

    ls_transport_line-sign = 'I'.
    ls_transport_line-option = 'EQ'.

    LOOP AT it_key_tab INTO DATA(ls_key).
      ls_transport_line-low = ls_key-value.
      APPEND ls_transport_line TO lt_transports_range.
    ENDLOOP.

    DATA(lt_transports) = lo_dao->get_requests( it_transports = lt_transports_range ).
    IF NOT lt_transports IS INITIAL.
      er_entity = lt_transports[ 1 ].
    ENDIF.

  ENDMETHOD.


  METHOD requestset_get_entityset.
*-----------------------------------------------------------------------
* Get list of request
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: lo_dao    TYPE REF TO zctsw_transport_dao,
          lt_users  TYPE trgr_user,
          lt_dates  TYPE trgr_date,
          lt_text   TYPE /iwbep/t_cod_select_options,
          lt_trkorr TYPE cnvc_scwb_tr,
          ls_filter TYPE /iwbep/s_mgw_select_option.

    CREATE OBJECT lo_dao.

    "Get filters
    READ TABLE it_filter_select_options WITH KEY property = 'Trkorr' INTO ls_filter.
    IF sy-subrc = 0.
      lt_trkorr = CORRESPONDING #( ls_filter-select_options ).
    ENDIF.

    READ TABLE it_filter_select_options WITH KEY property = 'As4user' INTO ls_filter.
    IF sy-subrc = 0.
      lt_users = CORRESPONDING #( ls_filter-select_options ).
    ENDIF.

    READ TABLE it_filter_select_options WITH KEY property = 'As4date' INTO ls_filter.
    IF sy-subrc = 0.
      lt_dates = CORRESPONDING #( ls_filter-select_options ).
    ENDIF.

    READ TABLE it_filter_select_options WITH KEY property = 'As4text' INTO ls_filter.
    IF sy-subrc = 0.
      lt_text = CORRESPONDING #( ls_filter-select_options ).
    ENDIF.

    "Set some default filters
    IF lt_dates IS INITIAL AND lt_text IS INITIAL AND lt_trkorr IS INITIAL.
      set_date_range(
      EXPORTING
        i_since_x_days = 90
      CHANGING
        ct_date_range  = lt_dates ).
    ENDIF.

    IF lt_dates IS INITIAL AND lt_users IS INITIAL AND lt_text IS INITIAL.
      set_date_range(
      EXPORTING
        i_since_x_days = 7
      CHANGING
        ct_date_range  = lt_dates ).
    ENDIF.

    APPEND LINES OF lo_dao->get_requests( it_transports = lt_trkorr it_users = lt_users it_dates = lt_dates it_text = lt_text ) TO et_entityset.

    "Paging
    /iwbep/cl_mgw_data_util=>paging(
      EXPORTING
        is_paging = is_paging
      CHANGING
        ct_data   = et_entityset ).

    "Order by
    /iwbep/cl_mgw_data_util=>orderby(
      EXPORTING
        it_order = it_order
      CHANGING
        ct_data  = et_entityset ).

  ENDMETHOD.


  METHOD set_date_range.

    DATA: ls_date   TYPE trgs_datum.

    ls_date-option = 'BT'.
    ls_date-sign   = 'I'.
    ls_date-low    = sy-datum - i_since_x_days.
    ls_date-high   = sy-datum.
    APPEND ls_date TO ct_date_range.

  ENDMETHOD.


  METHOD sort_and_compress.

    SORT ct_entityset BY pgmid objecttype obj_name.
    DELETE ADJACENT DUPLICATES FROM ct_entityset COMPARING pgmid objecttype obj_name.

  ENDMETHOD.


  METHOD taskset_get_entity.
*-----------------------------------------------------------------------
* Get single task
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------


    DATA: lo_dao            TYPE REF TO zctsw_transport_dao,
          r_transports      TYPE cnvc_scwb_tr,
          ls_transport_line TYPE cnvc_scwb_trs.

    CREATE OBJECT lo_dao.

    ls_transport_line-sign = 'I'.
    ls_transport_line-option = 'EQ'.

    LOOP AT it_key_tab INTO DATA(ls_key).
      ls_transport_line-low = ls_key-value.
      APPEND ls_transport_line TO r_transports.
    ENDLOOP.

    DATA(lt_transports) = lo_dao->get_tasks( it_transports = r_transports ).
    IF NOT lt_transports IS INITIAL.
      er_entity = lt_transports[ 1 ].
    ENDIF.

  ENDMETHOD.


  METHOD taskset_get_entityset.
*-----------------------------------------------------------------------
* Get list of tasks
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0134314 fstod\906575 12.03.2018 Method created
*-----------------------------------------------------------------------

    DATA: lo_dao            TYPE REF TO zctsw_transport_dao,
          lt_transport_key  TYPE  cnvc_scwb_tr,
          ls_transport_key  TYPE cnvc_scwb_trs,
          lv_parent_request TYPE trkorr,
          lt_dates          TYPE trgr_date,
          lt_users          TYPE trgr_user,
          ls_filter         TYPE /iwbep/s_mgw_select_option.

    CREATE OBJECT lo_dao.

    IF iv_source_name = 'Request'.
      READ TABLE it_key_tab INDEX 1 INTO DATA(ls_request_key).
      IF sy-subrc = 0.
        lv_parent_request = ls_request_key-value.
      ENDIF.
    ELSE.
      LOOP AT it_key_tab INTO DATA(ls_key).
        ls_transport_key-option = 'EQ'.
        ls_transport_key-sign   = 'I'.
        ls_transport_key-low = ls_key-value.
        APPEND ls_transport_key TO lt_transport_key.
      ENDLOOP.
    ENDIF.

    "Get filters
    READ TABLE it_filter_select_options WITH KEY property = 'As4user' INTO ls_filter.
    IF sy-subrc = 0.
      lt_users = CORRESPONDING #( ls_filter-select_options ).
    ENDIF.

    READ TABLE it_filter_select_options WITH KEY property = 'As4date' INTO ls_filter.
    IF sy-subrc = 0.
      lt_dates = CORRESPONDING #( ls_filter-select_options ).
    ENDIF.

    READ TABLE it_filter_select_options WITH KEY property = 'Trkorr' INTO ls_filter.
    IF sy-subrc = 0.
      lt_transport_key = CORRESPONDING #( ls_filter-select_options ).
    ENDIF.

    APPEND LINES OF lo_dao->get_tasks( i_parent_request = lv_parent_request
                                       it_transports    = lt_transport_key
                                       it_users         = lt_users
                                       it_dates         = lt_dates ) TO et_entityset.

    "Filter
    /iwbep/cl_mgw_data_util=>filtering(
      EXPORTING
        it_select_options =  it_filter_select_options
      CHANGING
        ct_data           = et_entityset
    ).

    "Paging
    /iwbep/cl_mgw_data_util=>paging(
      EXPORTING
        is_paging =   is_paging
      CHANGING
        ct_data   = et_entityset ).

    "Order by
    /iwbep/cl_mgw_data_util=>orderby(
      EXPORTING
        it_order = it_order
      CHANGING
        ct_data  = et_entityset
    ).

  ENDMETHOD.


  METHOD userset_get_entity.

    DATA(lt_key) = io_tech_request_context->get_keys( ).
    READ TABLE lt_key WITH KEY name = 'BNAME' INTO DATA(ls_key).

    SELECT SINGLE * FROM user_addr INTO er_entity       "#EC CI_NOORDER
        WHERE bname = ls_key-value.

  ENDMETHOD.


  METHOD userset_get_entityset.

    DATA: ls_filter        TYPE /iwbep/s_mgw_select_option,
          lt_user_range    TYPE /pra/tt_pn_r_user,
          ls_user_range    LIKE LINE OF lt_user_range,
          lv_search_string TYPE string.

    lv_search_string = iv_search_string.
    TRANSLATE lv_search_string TO UPPER CASE.
    IF NOT lv_search_string IS INITIAL.

      ls_user_range-low = lv_search_string && '*'.
      ls_user_range-sign = 'I'.
      ls_user_range-option = 'CP'.
      APPEND ls_user_range TO lt_user_range.
      SELECT * FROM user_addr INTO TABLE et_entityset
          WHERE bname IN lt_user_range OR
          mc_namefir IN lt_user_range OR
          mc_namelas IN lt_user_range.
    ELSE.
      READ TABLE it_filter_select_options WITH KEY property = 'Bname' INTO ls_filter.
      IF sy-subrc = 0.
        lt_user_range = CORRESPONDING #( ls_filter-select_options ).
        SELECT * FROM user_addr INTO TABLE et_entityset
            WHERE bname IN lt_user_range.
      ELSE.
        READ TABLE it_filter_select_options WITH KEY property = 'NameTextc' INTO ls_filter.
        IF sy-subrc = 0.
          SELECT * FROM user_addr INTO TABLE et_entityset
            WHERE mc_namefir IN ls_filter-select_options
            OR    mc_namelas IN ls_filter-select_options.
        ENDIF.

      ENDIF.
    ENDIF.

    SORT et_entityset BY bname.



  ENDMETHOD.
ENDCLASS.
