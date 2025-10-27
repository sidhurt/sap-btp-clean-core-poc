CLASS zcl_test_s4_connection DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_test_s4_connection IMPLEMENTATION.

METHOD if_oo_adt_classrun~main.

    DATA: lo_client   TYPE REF TO if_web_http_client,
          lo_request  TYPE REF TO if_web_http_request,
          lo_response TYPE REF TO if_web_http_response,
          lv_url      TYPE string.

    " --- 1. Define the Target URL ---
    lv_url = 'https://sandbox.api.sap.com/s4hanacloud/sap/opu/odata/sap/API_BUSINESS_PARTNER'.

    " --- 2. Create the HTTP client ---
    TRY.
        lo_client = cl_web_http_client_manager=>create_by_http_destination(
          i_destination = cl_http_destination_provider=>create_by_url( lv_url )
        ).
      CATCH cx_http_dest_provider_error cx_web_http_client_error INTO DATA(lx_error_create).
        out->write( |HTTP Client Creation FAILED: { lx_error_create->get_text( ) }| ).
        RETURN.
    ENDTRY.

    " --- 3. Create the request object ---
    lo_request = lo_client->get_http_request( ).

    " --- 4. Set the API Key header ---
    lo_request->set_header_field(
      i_name  = 'apikey'
      i_value = 'C7oXXqVHsnYAcGA2uFMUSGU7TALK3Nas' " <-- PASTE YOUR KEY HERE
    ).

    " --- 5. Execute the GET request ---
    TRY.
        lo_response = lo_client->execute( if_web_http_client=>get ).
        " --- If this line is reached without exception, the call was successful ---
        out->write( 'HTTP Call Successful. Connection Confirmed.' ).

      CATCH cx_web_http_client_error INTO DATA(lx_error_exec).
        " --- If this block is reached, the call failed ---
        out->write( |HTTP Execute FAILED: { lx_error_exec->get_text( ) }| ).
        RETURN.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
