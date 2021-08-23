CLASS zcl_push_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Fill return</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_appl | <p class="shorttext synchronized">Application</p>
    CLASS-METHODS fill_return
      IMPORTING
        !iv_type         TYPE bapi_mtype DEFAULT zif_push_data=>cs_message-type-success
        !iv_id           TYPE arbgb DEFAULT 'ZPUSH'
        !iv_number       TYPE any
        !iv_message_v1   TYPE any OPTIONAL
        !iv_message_v2   TYPE any OPTIONAL
        !iv_message_v3   TYPE any OPTIONAL
        !iv_message_v4   TYPE any OPTIONAL
        !iv_langu        TYPE sylangu DEFAULT sy-langu
      RETURNING
        VALUE(rs_return) TYPE zif_push_data=>ts_return.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_push_utils IMPLEMENTATION.
  METHOD fill_return.

    CLEAR rs_return.
    DATA(ls_bapi_return) = VALUE bapiret2( type = iv_type
                                           id = iv_id
                                           number = iv_number
                                           message_v1 = iv_message_v1
                                           message_v2 = iv_message_v2
                                           message_v3 = iv_message_v3
                                           message_v4 = iv_message_v4 ).

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = ls_bapi_return-id
        number     = ls_bapi_return-number
        language   = iv_langu
        textformat = 'ASC'
        message_v1 = ls_bapi_return-message_v1
        message_v2 = ls_bapi_return-message_v2
        message_v3 = ls_bapi_return-message_v3
        message_v4 = ls_bapi_return-message_v4
      IMPORTING
        message    = ls_bapi_return-message.

    rs_return-type = ls_bapi_return-type.
    rs_return-message = ls_bapi_return-message.

  ENDMETHOD.
ENDCLASS.
