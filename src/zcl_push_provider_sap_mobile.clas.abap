CLASS zcl_push_provider_sap_mobile DEFINITION
  PUBLIC
  INHERITING FROM zcl_push_provider_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_push_provider~send_push REDEFINITION.
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ts_push_body,
        users TYPE STANDARD TABLE OF string WITH EMPTY KEY,
        BEGIN OF notification,
          alert TYPE string,
          badge TYPE int2,
          sound TYPE string,
        END OF notification,
      END OF ts_push_body .
    TYPES:
      BEGIN OF ts_push_results,
        target          TYPE string,
        notification_id TYPE string,
      END OF ts_push_results .
    TYPES:
      tt_push_results TYPE STANDARD TABLE OF ts_push_results WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_result,
        push_results TYPE tt_push_results,
      END OF ts_result .

    CONSTANTS cv_url_base_service TYPE string VALUE 'https://{servicepath}/mobileservices/push/v1/backend/applications/{applicationId}/notifications/users' ##NO_TEXT.
    DATA mv_url_service TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_push_provider_sap_mobile IMPLEMENTATION.


  METHOD zif_push_provider~send_push.
    DATA lv_response TYPE string.
    DATA ls_result TYPE ts_result.
    CLEAR: et_return.

    DATA(lv_url_service) = cv_url_base_service.
    REPLACE FIRST OCCURRENCE OF '{servicepath}' IN lv_url_service WITH ms_provider_conf-service_path.
    REPLACE FIRST OCCURRENCE OF '{applicationId}' IN lv_url_service WITH is_sending_conf-id_appl.

    TRY.

        " Se genera el objeto para las comunicaciones HTTP
        create_http_client_by_url( iv_url = lv_url_service ).

        set_http_header_value( iv_name  = 'Accept' iv_value = 'application/json' ).
        set_http_header_value( iv_name  = 'Content-Type' iv_value = 'application/json' ).
        set_http_request_method( 'POST' ).
        set_http_basic_auth(
          EXPORTING
            iv_user     = CONV #( ms_provider_conf-connect_user )
            iv_password = CONV #( ms_provider_conf-connect_pass ) ).

        " Listado de notificaciones
        LOOP AT it_notifications ASSIGNING FIELD-SYMBOL(<ls_notifications>).
          DATA(ls_body) = VALUE ts_push_body( users = VALUE #( FOR <wa> IN it_users ( CONV #( <wa> ) ) )
                                              notification = VALUE #( alert = <ls_notifications>-message
                                                                      badge = CONV #( <ls_notifications>-badge )
                                                                      sound = <ls_notifications>-sound ) ).


          " Envio de datos
          send_http( EXPORTING iv_data = ls_body
              iv_pretty_name         =  /ui2/cl_json=>pretty_mode-low_case
              iv_compress_json_data = abap_false
              iv_convert_data_2_json = abap_true ).

          " Recepción de la respuesta
          receive_http( EXPORTING iv_pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                        IMPORTING ev_data = ls_result ).

        ENDLOOP.

        " Si llega hasta aquí es que se han enviado las notificaciones y todo ha ido bien, supuestamente. Así que devuelve un mensaje
        " genérico
        INSERT  zcl_push_utils=>fill_return( iv_type = zif_push_data=>cs_message-type-success
                                                  iv_number = '012' ) INTO TABLE et_return.

      CATCH zcx_push INTO DATA(lx_push).
        INSERT zcl_push_utils=>fill_return( iv_type = zif_push_data=>cs_message-type-error
                                                                   iv_number     = lx_push->if_t100_message~t100key-msgno
                                                                   iv_message_v1 = lx_push->mv_msgv1
                                                                   iv_message_v2 = lx_push->mv_msgv2
                                                                   iv_message_v3 = lx_push->mv_msgv3
                                                                   iv_message_v4 = lx_push->mv_msgv4
                                                                   iv_langu      = mv_langu ) INTO TABLE et_return.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
