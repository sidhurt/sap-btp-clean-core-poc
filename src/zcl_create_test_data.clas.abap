CLASS zcl_create_test_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_create_test_data IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    " Generate a test UUID for the Business Partner link
    DATA(lv_test_bp_uuid) = cl_system_uuid=>create_uuid_x16_static( ).

    " Get current timestamp in the correct format
    GET TIME STAMP FIELD DATA(lv_current_timestamp).

    " Prepare the data row structure
    DATA ls_esg_score TYPE zesg_scores.

    ls_esg_score = VALUE #(
      client  = sy-mandt " Use current client
      bp_uuid = lv_test_bp_uuid
      esg_score = 85 " Example score
      " Admin fields will be handled by framework if BDEF was active,
      " but we set them manually for direct insert test
      created_by      = sy-uname
      created_at      = lv_current_timestamp " Use the correctly typed timestamp
      last_changed_by = sy-uname
      last_changed_at = lv_current_timestamp " Use the correctly typed timestamp
    ).

    " --- Insert the data ---
    TRY.
        INSERT INTO zesg_scores VALUES @ls_esg_score.
        IF sy-subrc = 0.
          out->write( |Test data inserted successfully for BP UUID: { lv_test_bp_uuid }| ).
        ELSE.
          out->write( |Error inserting data. SUBRC: { sy-subrc }| ).
        ENDIF.
      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_error).
        out->write( |SQL Exception: { lx_sql_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


