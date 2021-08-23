CLASS zcl_push_provider_base DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
    INTERFACES zif_push_provider.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_appl | <p class="shorttext synchronized">Application</p>
    METHODS constructor
      IMPORTING
                iv_langu    TYPE sylangu DEFAULT sy-langu
                iv_provider TYPE zpush_e_provider
      RAISING   zcx_push.

    "! <p class="shorttext synchronized">Return the object instance of the provider</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_provider | <p class="shorttext synchronized">Provider</p>
    CLASS-METHODS get_instance
      IMPORTING iv_provider        TYPE zpush_e_provider
                iv_langu           TYPE sylangu DEFAULT sy-langu
      RETURNING VALUE(ro_provider) TYPE REF TO zif_push_provider
      RAISING   zcx_push.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_provider_conf,
             param_key    TYPE zpush_t003-param_key,
             param_value  TYPE zpush_t003-param_value,
             connect_user TYPE zpush_t003-connect_user,
             connect_pass TYPE zpush_t003-connect_pass,
             service_path TYPE zpush_t003-service_path,
           END OF ts_provider_conf.
    TYPES: tt_provider_conf TYPE STANDARD TABLE OF ts_provider_conf WITH EMPTY KEY.
    DATA mv_langu TYPE sylangu.
    DATA ms_provider_conf TYPE ts_provider_conf.
    DATA mo_http_client TYPE REF TO if_http_client .

    "! <p class="shorttext synchronized">Load configuration of the provider</p>
    "! @parameter iv_provider | <p class="shorttext synchronized">Provider</p>
    METHODS load_provider_configuration
      IMPORTING
                iv_provider TYPE zpush_e_provider
      RAISING   zcx_push.
    "! <p class="shorttext synchronized">Create HTTP Connection</p>
    "! @parameter iv_url | <p class="shorttext synchronized">URL</p>
    METHODS create_http_client_by_url
      IMPORTING
                !iv_url TYPE string
      RAISING   zcx_push.
    "! <p class="shorttext synchronized">Set "Authorization" value in the header</p>
    METHODS set_http_token_auth
      IMPORTING
        !iv_token TYPE string .
    "! <p class="shorttext synchronized">Set header value</p>
    "! @parameter iv_name | <p class="shorttext synchronized">Name</p>
    "! @parameter iv_value | <p class="shorttext synchronized">Value</p>
    METHODS set_http_header_value
      IMPORTING
        !iv_name  TYPE any
        !iv_value TYPE any .
    "! <p class="shorttext synchronized">Set request method</p>
    "! @parameter iv_method | <p class="shorttext synchronized">Method:GET, POST,etc.</p>
    METHODS set_http_request_method
      IMPORTING
        !iv_method TYPE any .
    "! <p class="shorttext synchronized">Send data to HTTP connection</p>
    "! @parameter iv_data | <p class="shorttext synchronized">Data to send</p>
    "! @parameter iv_pretty_name | <p class="shorttext synchronized">Pretty format to conver JSON</p>
    "! @parameter iv_compress_json_data | <p class="shorttext synchronized">Compress data of JSON</p>
    "! @parameter iv_convert_data_2_json | <p class="shorttext synchronized">Convert data to JSON</p>
    METHODS send_http
      IMPORTING
        !iv_data                TYPE data OPTIONAL
        !iv_pretty_name         TYPE /ui2/cl_json=>pretty_name_mode OPTIONAL
        iv_compress_json_data   TYPE sap_bool DEFAULT abap_true
        !iv_convert_data_2_json TYPE sap_bool DEFAULT abap_false
      RAISING
        zcx_push .
    "! <p class="shorttext synchronized">Receive data to HTTP connection</p>
    "! @parameter iv_pretty_name | <p class="shorttext synchronized">Pretty format to conver JSON</p>
    "! @parameter ev_data | <p class="shorttext synchronized">Data received</p>
    METHODS receive_http
      IMPORTING
        !iv_pretty_name TYPE /ui2/cl_json=>pretty_name_mode OPTIONAL
      EXPORTING
        !ev_data        TYPE data
      RAISING
        zcx_push .
    "! <p class="shorttext synchronized">Set data to the request</p>
    "! @parameter iv_data | <p class="shorttext synchronized">Data to send</p>
    "! @parameter iv_pretty_name | <p class="shorttext synchronized">Pretty format to conver JSON</p>
    "! @parameter iv_compress_json_data | <p class="shorttext synchronized">Compress data of JSON</p>
    "! @parameter iv_convert_data_2_json | <p class="shorttext synchronized">Convert data to JSON</p>
    METHODS set_http_request_data
      IMPORTING
        !iv_data                TYPE any
        !iv_pretty_name         TYPE /ui2/cl_json=>pretty_name_mode OPTIONAL
        iv_compress_json_data   TYPE sap_bool DEFAULT abap_true
        !iv_convert_data_2_json TYPE sap_bool DEFAULT abap_false .
    "! <p class="shorttext synchronized">Set basic authentification</p>
    "! @parameter iv_user | <p class="shorttext synchronized">User</p>
    "! @parameter iv_password | <p class="shorttext synchronized">Password</p>
    METHODS set_http_basic_auth
      IMPORTING
        !iv_user     TYPE string
        !iv_password TYPE string.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_push_provider_base IMPLEMENTATION.
  METHOD get_instance.

    " Se busca la clase controladora. Si no tiene o no existe el provider se lanza la excepción que no esta configurado
    SELECT SINGLE classcontroller INTO @DATA(lv_class)
           FROM zpush_t002
           WHERE provider = @iv_provider.
    IF lv_class IS NOT INITIAL.


      " Si la instancia da error se devuelve una excepción indicandolo
      TRY.
          CREATE OBJECT ro_provider TYPE (lv_class)
            EXPORTING
              iv_langu    = iv_langu
              iv_provider = iv_provider.
        CATCH cx_root.
          RAISE EXCEPTION TYPE zcx_push
            EXPORTING
              textid   = zcx_push=>fail_instance_class
              mv_msgv1 = CONV #( lv_class ).
      ENDTRY.

    ELSE.
      RAISE EXCEPTION TYPE zcx_push
        EXPORTING
          textid   = zcx_push=>provider_not_configured
          mv_msgv1 = CONV #( iv_provider ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_push_provider~send_push.

  ENDMETHOD.

  METHOD constructor.
    mv_langu = iv_langu.

    " Lee la configuración del proveedor.
    load_provider_configuration( iv_provider ).

  ENDMETHOD.


  METHOD load_provider_configuration.
    CLEAR: ms_provider_conf.

    " Lectura de la configuración de los provider. Puede ser que el proveedor no la tenga
    " porque funcione de una manera especial. En ese caso, cada envio push se encargará de gestionar
    " si le falta algo por configurar.
    SELECT SINGLE param_key param_value connect_user connect_pass service_path INTO ms_provider_conf
           FROM zpush_t003
           WHERE provider = iv_provider.


  ENDMETHOD.

  METHOD create_http_client_by_url.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = iv_url
      IMPORTING
        client             = mo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    IF sy-subrc NE 0.

      RAISE EXCEPTION TYPE zcx_push
        EXPORTING
          textid = zcx_push=>error_create_http_connection.
    ENDIF.
  ENDMETHOD.

  METHOD set_http_header_value.
    DATA lv_name TYPE string.
    DATA lv_value TYPE string.

    lv_name = iv_name.
    lv_value = iv_value.

    CALL METHOD mo_http_client->request->set_header_field
      EXPORTING
        name  = lv_name
        value = lv_value.
  ENDMETHOD.

  METHOD set_http_token_auth.
    set_http_header_value( EXPORTING iv_name = 'Authorization' iv_value = iv_token ).
  ENDMETHOD.

  METHOD set_http_request_method.
    CALL METHOD mo_http_client->request->set_header_field
      EXPORTING
        name  = '~request_method'
        value = iv_method.
  ENDMETHOD.

  METHOD receive_http.
    CLEAR ev_data.

    mo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    IF sy-subrc NE 0. " Si hay error no lanzo a excepción salvo que no se puede recuperar bien el status_code.
      DATA(ls_return) = zcl_push_utils=>fill_return( iv_type       = sy-msgty
                                                       iv_id         = sy-msgid
                                                       iv_number     = sy-msgno
                                                       iv_message_v1 = sy-msgv1
                                                       iv_message_v2 = sy-msgv2
                                                       iv_message_v3 = sy-msgv3
                                                       iv_message_v4 = sy-msgv4 ).

    ENDIF.

* Obtengo el resultado de la petición. Si todo va bien el codigo ha de ser 200.
    CALL METHOD mo_http_client->response->get_status
      IMPORTING
        code   = DATA(lv_status_code)
        reason = DATA(lv_status_text).

* Recupero el contenido. Si hay ido bien recupero los datos y si ha ido mal el mensaje de error
    DATA(lv_content) = mo_http_client->response->get_cdata( ).


* Si no hay código de status y se ha producido un error en la recepcion devuelvo ese posible
    IF lv_status_code IS INITIAL AND ls_return IS NOT INITIAL.

      RAISE EXCEPTION TYPE zcx_push
        EXPORTING
          textid   = zcx_push=>error_receive_data
          mv_msgv1 = ls_return-message.

    ELSEIF lv_status_code NE '200' AND lv_status_code NE '201' AND lv_status_code NE '202'.

      RAISE EXCEPTION TYPE zcx_push
        EXPORTING
          textid              = zcx_push=>error_receive_data
          mv_msgv1            = CONV #( lv_status_code )
          mv_msgv2            = lv_status_text
          mv_content_response = lv_content.
    ENDIF.

* Miro que tipo de datos me han pasado.
    DATA(lo_data) = cl_abap_typedescr=>describe_by_data( ev_data ).

* Si es estructura o tabla de diccionario entonces convierto la respuesta a los datos pasados. En caso contrario
* lo devuelvo tal cual
    IF lo_data->kind = cl_abap_typedescr=>kind_struct OR lo_data->kind = cl_abap_typedescr=>kind_table.
      /ui2/cl_json=>deserialize( EXPORTING json = lv_content pretty_name = iv_pretty_name CHANGING data = ev_data ).
    ELSE.
      ev_data = lv_content.
    ENDIF.
  ENDMETHOD.

  METHOD send_http.
* Si se le pasan datos, llamo al método para pasar los datos
    IF iv_data IS NOT INITIAL.
      set_http_request_data( iv_data = iv_data
                             iv_pretty_name = iv_pretty_name
                             iv_convert_data_2_json = iv_convert_data_2_json
                             iv_compress_json_data = iv_compress_json_data ).
    ENDIF.

    mo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_push
        EXPORTING
          textid = zcx_push=>error_send_data.
    ENDIF.
  ENDMETHOD.

  METHOD set_http_request_data.
    " Pasamos los datos a formato JSON si se especifica en la cabecera. Esto permite pasar, o bien, estructuras de diccionario, o bien,
    " el json directamente.
    IF iv_convert_data_2_json = abap_true.
      DATA(lv_json) = /ui2/cl_json=>serialize( data = iv_data compress = iv_compress_json_data pretty_name = iv_pretty_name ).
    ELSE.
      lv_json = iv_data.
    ENDIF.

    " Pasa los datos del JSON
    CALL METHOD mo_http_client->request->set_cdata
      EXPORTING
        data = lv_json.
  ENDMETHOD.

  METHOD set_http_basic_auth.
    " Parámetros fijos de la cabecera
    DATA(lv_auth) = |{ iv_user }:{ iv_password }|.
    TRY.
        " Se pasa el usuario y password a xstring y luego a base64
        DATA(lv_xauth) = cl_bcs_convert=>string_to_xstring( lv_auth ).
        lv_auth = cl_http_utility=>encode_x_base64( lv_xauth ).

        lv_auth = |Basic { lv_auth }|.

        set_http_token_auth( iv_token = lv_auth ).

*        mo_http_client->request->set_authorization(
*                           auth_type  = ihttp_auth_type_basic_auth
*                           username   = iv_user
*                           password   = iv_password ).

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
