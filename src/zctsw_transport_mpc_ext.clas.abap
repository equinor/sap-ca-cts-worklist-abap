CLASS zctsw_transport_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zctsw_transport_mpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS define
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS set_date_format_for_property
      IMPORTING
        !i_entity              TYPE /iwbep/if_mgw_med_odata_types=>ty_e_med_entity_name
        !i_property_name       TYPE /iwbep/if_mgw_med_odata_types=>ty_e_med_entity_name
        !i_add_interval_filter TYPE abap_bool DEFAULT abap_false
      RAISING
        /iwbep/cx_mgw_med_exception .

    METHODS create_entity_valuehelp
      IMPORTING
        !i_target_entity    TYPE /iwbep/med_external_name
        !i_collection       TYPE string
        i_column1           TYPE string
        i_column2           TYPE string
        i_column3           TYPE string OPTIONAL
        i_entity_ref_column TYPE string
        i_search_supported  TYPE abap_bool DEFAULT abap_false.

ENDCLASS.



CLASS zctsw_transport_mpc_ext IMPLEMENTATION.


  METHOD define.

    super->define( ).
    set_date_format_for_property( i_entity = me->gc_request i_property_name = 'As4date' i_add_interval_filter = abap_true ).

    create_entity_valuehelp( i_target_entity = me->gc_request
                           i_collection = 'UserSet'
                           i_entity_ref_column = 'As4user'
                           i_column1 = 'Bname'
                           i_column2 = 'NameTextc'
                           i_search_supported = abap_true ).


  ENDMETHOD.


  METHOD set_date_format_for_property.


    DATA(lo_entity_type) = model->get_entity_type( i_entity ).


    DATA(lo_property_date) = lo_entity_type->get_property(  iv_property_name = i_property_name ).
    DATA(lo_annotation_date) = lo_property_date->/iwbep/if_mgw_odata_annotatabl~create_annotation( /iwbep/if_mgw_med_odata_types=>gc_sap_namespace ).

    lo_annotation_date->add(
      iv_key = /iwbep/if_ana_odata_types=>gcs_ana_odata_annotation_key-display_format
      iv_value = /iwbep/if_ana_odata_types=>gcs_ana_odata_annotation_value-common-date ).

    IF ( i_add_interval_filter = abap_true ).

      lo_annotation_date->add(
       iv_key = /iwbep/if_ana_odata_types=>gcs_ana_odata_annotation_key-filter_restriction
       iv_value = /iwbep/if_ana_odata_types=>gcs_ana_odata_annotation_value-filter-interval ).

    ENDIF.


  ENDMETHOD.


  METHOD create_entity_valuehelp.
*-----------------------------------------------------------------------
* A method that defines annotations and creates value search help (F4).
* Input parameters:
* - i_target_entity - target entity for which search help is applied
* - i_entity_ref_column - a single column of target entity for which
*   we want to define search help
* - i_column1, i_column2, i_column3 - columns visible in output table
*   of search help
* - i_search_supported - if true then search dialog is shown on F4
*   window and user can type in directly value so search will be performed.
*   Search must be implemented in ODATA backend so that IV_SEARCH_STRING
*   is used in DPC_EXT class.
*-----------------------------------------------------------------------
* Change     Developer    Date       Description
* CHG0175211 ADKRA\774888 03.07.2019 Method extracted, commented
*-----------------------------------------------------------------------
    DATA: lo_ann_target TYPE REF TO /iwbep/if_mgw_vocan_ann_target.   " Vocabulary Annotation Target                     "#EC NEEDED
    DATA: lo_annotation TYPE REF TO /iwbep/if_mgw_vocan_annotation.   " Vocabulary Annotation                            "#EC NEEDED
    DATA: lo_collection TYPE REF TO /iwbep/if_mgw_vocan_collection.   " Vocabulary Annotation Collection                 "#EC NEEDED
    DATA: lo_property   TYPE REF TO /iwbep/if_mgw_vocan_property.     " Vocabulary Annotation Property                   "#EC NEEDED
    DATA: lo_record     TYPE REF TO /iwbep/if_mgw_vocan_record.       " Vocabulary Annotation Record                     "#EC NEEDED
    DATA: lo_simp_value TYPE REF TO /iwbep/if_mgw_vocan_simple_val.   " Vocabulary Annotation Simple Value               "#EC NEEDED


    "Annotations
    lo_ann_target = vocab_anno_model->create_annotations_target( i_target_entity && '/' && i_entity_ref_column ). "#EC NOTEXT
    lo_ann_target->set_namespace_qualifier( 'ZCTSW_TRANSPORT_SRV' ). "#EC NOTEXT

    "Annotation term
    lo_annotation = lo_ann_target->create_annotation( iv_term = 'com.sap.vocabularies.Common.v1.ValueList' ). "#EC NOTEXT

    "Record
    lo_record = lo_annotation->create_record( ).            "#EC NOTEXT

    "PropertyValue
    lo_property = lo_record->create_property( 'CollectionPath' ). "#EC NOTEXT
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_string( i_collection ).              "#EC NOTEXT

    IF ( i_search_supported = abap_true ).
      "PropertyValue
      lo_property = lo_record->create_property( 'SearchSupported' ). "#EC NOTEXT
      lo_simp_value = lo_property->create_simple_value( ).
      lo_simp_value->set_boolean( abap_true ).              "#EC NOTEXT
    ENDIF.

    "PropertyValue
    lo_property = lo_record->create_property( 'Parameters' ). "#EC NOTEXT

    "Collection
    lo_collection = lo_property->create_collection( ).

    "Record
    lo_record = lo_collection->create_record( 'com.sap.vocabularies.Common.v1.ValueListParameterOut' ). "#EC NOTEXT

    "Property
    lo_property = lo_record->create_property( 'LocalDataProperty' ). "#EC NOTEXT
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_property_path( i_entity_ref_column ). "#EC NOTEXT

    "Property
    lo_property = lo_record->create_property( 'ValueListProperty' ). "#EC NOTEXT
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_string( i_column1 ).                 "#EC NOTEXT

    "Record
    lo_record = lo_collection->create_record( 'com.sap.vocabularies.Common.v1.ValueListParameterDisplayOnly' ). "#EC NOTEXT

    "Property
    lo_property = lo_record->create_property( 'ValueListProperty' ). "#EC NOTEXT
    lo_simp_value = lo_property->create_simple_value( ).
    lo_simp_value->set_string( i_column2 ).                 "#EC NOTEXT

    IF ( i_column3 IS NOT INITIAL ).

      "Record
      lo_record = lo_collection->create_record( 'com.sap.vocabularies.Common.v1.ValueListParameterDisplayOnly' ). "#EC NOTEXT

      "Property
      lo_property = lo_record->create_property( 'ValueListProperty' ). "#EC NOTEXT
      lo_simp_value = lo_property->create_simple_value( ).
      lo_simp_value->set_string( i_column3 ).               "#EC NOTEXT

    ENDIF.

  ENDMETHOD.


ENDCLASS.
