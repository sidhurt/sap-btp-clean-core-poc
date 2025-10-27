CLASS zcl_esg_score_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS create_esg_score
      IMPORTING
        iv_bp_uuid      TYPE sysuuid_x16
        iv_esg_score    TYPE int4
      RAISING
        cx_sy_open_sql_db.

    METHODS update_esg_score
      IMPORTING
        iv_bp_uuid         TYPE sysuuid_x16
        iv_new_esg_score TYPE int4
      RAISING
        cx_sy_open_sql_db.

    METHODS delete_esg_score
      IMPORTING
        iv_bp_uuid TYPE sysuuid_x16
      RAISING
        cx_sy_open_sql_db.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_esg_score_manager IMPLEMENTATION.

  METHOD create_esg_score.

    " Get current timestamp
    GET TIME STAMP FIELD DATA(lv_current_timestamp).

    " Prepare data structure
    DATA ls_esg_score TYPE zesg_scores.

    ls_esg_score = VALUE #(
        client          = sy-mandt
        bp_uuid         = iv_bp_uuid
        esg_score       = iv_esg_score
        created_by      = sy-uname
        created_at      = lv_current_timestamp
        last_changed_by = sy-uname
        last_changed_at = lv_current_timestamp
      ).

    " Insert data (raises cx_sy_open_sql_db on error)
    INSERT INTO zesg_scores VALUES @ls_esg_score.

  ENDMETHOD.

  METHOD update_esg_score.
    " Implementation for UPDATE would go here
    " Example:
    " GET TIME STAMP FIELD DATA(lv_update_timestamp).
    " UPDATE zesg_scores SET
    "   esg_score       = @iv_new_esg_score,
    "   last_changed_by = @sy-uname,
    "   last_changed_at = @lv_update_timestamp
    " WHERE bp_uuid = @iv_bp_uuid.
  ENDMETHOD.

  METHOD delete_esg_score.
    " Implementation for DELETE would go here
    " Example:
    " DELETE FROM zesg_scores WHERE bp_uuid = @iv_bp_uuid.
  ENDMETHOD.

ENDCLASS.

