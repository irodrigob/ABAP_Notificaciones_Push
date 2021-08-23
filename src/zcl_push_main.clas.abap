CLASS zcl_push_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tt_r_users TYPE RANGE OF zpush_e_user.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Language</p>
    "! @parameter iv_appl | <p class="shorttext synchronized">Application</p>
    METHODS constructor
      IMPORTING
                iv_langu TYPE sylangu DEFAULT sy-langu
                iv_appl  TYPE zpush_e_appl
      RAISING   zcx_push.
    "! <p class="shorttext synchronized">Register device</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Username</p>
    "! @parameter iv_hardware_id | <p class="shorttext synchronized">Hardware ID</p>
    "! @parameter iv_device_id | <p class="shorttext synchronized">Device ID</p>
    "! @parameter iv_device_type | <p class="shorttext synchronized">Device type</p>
    METHODS register
      IMPORTING
        iv_user        TYPE zpush_e_user OPTIONAL
        iv_hardware_id TYPE zpush_e_hardware_id OPTIONAL
        iv_device_id   TYPE zpush_e_device_id OPTIONAL
        iv_device_type TYPE zpush_e_device_type
      EXPORTING
        ev_id_register TYPE zpush_e_id_register
        et_return      TYPE zif_push_data=>tt_return.
    "! <p class="shorttext synchronized">Get register data</p>
    "! @parameter it_users | <p class="shorttext synchronized">Users</p>
    "! @parameter et_data | <p class="shorttext synchronized">Data of register</p>
    METHODS get_register_data
      IMPORTING
        it_users TYPE zif_push_data=>tt_users OPTIONAL
      EXPORTING
        et_data  TYPE zif_push_data=>tt_register_data.
    "! <p class="shorttext synchronized">Send notification</p>
    "! @parameter it_users | <p class="shorttext synchronized">Users</p>
    "! @parameter iv_message | <p class="shorttext synchronized">Message to send</p>
    "! @parameter et_return | <p class="shorttext synchronized">Message to send</p>
    METHODS send_push
      IMPORTING
                it_users        TYPE zif_push_data=>tt_users OPTIONAL
                it_notification TYPE zif_push_data=>tt_notification
      EXPORTING et_return       TYPE zif_push_data=>tt_return.

  PROTECTED SECTION.
    TYPES: BEGIN OF ts_appl,
             appl      TYPE zpush_t001-appl,
             active    TYPE zpush_t001-active,
             appl_desc TYPE zpush_t001t-description,
           END OF ts_appl.

    TYPES: BEGIN OF ts_appl_provider,
             provider        TYPE zpush_t004-provider,
             provider_desc   TYPE zpush_t002t-description,
             classcontroller TYPE zpush_t002-classcontroller,
             active          TYPE zpush_t002-active,
             id_appl         TYPE zpush_t004-id_appl,
           END OF ts_appl_provider.
    TYPES: tt_appl_provider TYPE STANDARD TABLE OF ts_appl_provider WITH EMPTY KEY.

    DATA ms_appl TYPE ts_appl.
    DATA mt_appl_provider TYPE tt_appl_provider.
    DATA mv_langu TYPE sylangu.

    "! <p class="shorttext synchronized">Load configuration of application</p>
    "! @parameter iv_appl | <p class="shorttext synchronized">Application</p>
    METHODS load_appl_configuration
      IMPORTING
                iv_appl TYPE zpush_e_appl
      RAISING   zcx_push.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_push_main IMPLEMENTATION.
  METHOD constructor.
    mv_langu = iv_langu.


    " Lectura de la configuración de la aplicación
    load_appl_configuration( iv_appl ).

  ENDMETHOD.


  METHOD load_appl_configuration.

    CLEAR: ms_appl, mt_appl_provider.

    " Datos generales
    SELECT SINGLE a~appl a~active b~description AS appl_desc
            INTO ms_appl
           FROM zpush_t001 AS a LEFT OUTER JOIN zpush_t001t AS b ON
                b~appl = a~appl
                AND b~spras = mv_langu
           WHERE a~appl = iv_appl.
    IF sy-subrc = 0.

      "Lectura de los proveedores asociados a la aplicación
      SELECT a~provider b~description AS provider_desc c~classcontroller c~active
             a~id_appl  INTO TABLE mt_appl_provider
             FROM zpush_t004 AS a
                  LEFT OUTER JOIN zpush_t002t AS b ON
                    b~provider = a~provider
                    AND b~spras = mv_langu
                  INNER JOIN zpush_t002 AS c ON
                        c~provider = a~provider
             WHERE a~appl = iv_appl.

    ELSE.
      RAISE EXCEPTION TYPE zcx_push
        EXPORTING
          textid   = zcx_push=>appl_not_configured
          mv_msgv1 = CONV #( iv_appl ).
    ENDIF.
  ENDMETHOD.

  METHOD register.

    CLEAR: ev_id_register, et_return.

    " Primero es buscar si para la aplicación y hardware existente un registro previo. En caso de haberlo se actualizan determinados
    " campos y listos. Esto hace que la tabla no aumente de tamaño de manera infinita cada vez que alguien entre en la aplicación desde
    " el mismo dispositivo.
    SELECT SINGLE * INTO @DATA(ls_register)
           FROM zpush_t005
           WHERE hardware_id = @iv_hardware_id
                 AND appl = @ms_appl-appl.
    IF sy-subrc NE 0.
      " Se informan los campos fijos que tendrá.
      ls_register-id_register = /bobf/cl_frw_factory=>get_new_key( ).
      ls_register-hardware_id = iv_hardware_id.
      ls_register-appl = ms_appl-appl.
    ENDIF.

    " Se actualizan los campos que pueden variar.
    ls_register-device_id = iv_device_id.
    ls_register-device_type = iv_device_type. " Tengo dudas que este pueda variar...
    ls_register-username = iv_user.
    ls_register-register_date = sy-datum.
    ls_register-register_time = sy-uzeit.

    MODIFY zpush_t005 FROM ls_register.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      " Se devuelve el ID del registro
      ev_id_register = ls_register-id_register.
      " Se devuelve un mensaje genérico
      INSERT zcl_push_utils=>fill_return( iv_number = '004' ) INTO TABLE et_return.
    ELSE.
      ROLLBACK WORK.
      INSERT zcl_push_utils=>fill_return( iv_type = zif_push_data=>cs_message-type-error
                                          iv_number = '003' ) INTO TABLE et_return.
    ENDIF.

  ENDMETHOD.

  METHOD get_register_data.

    CLEAR: et_data.

    " Pasamos los usuarios a ranges
    DATA(lt_r_users) = VALUE tt_r_users( FOR <wa> IN it_users ( sign = 'I' option = 'EQ' low = <wa> ) ).

    SELECT id_register username hardware_id device_id device_type INTO TABLE et_data
           FROM zpush_t005
           WHERE appl = ms_appl-appl
                 AND username IN lt_r_users.

  ENDMETHOD.

  METHOD send_push.

    " Obtengo a que dispositivos le tengo que enviar el mensaje
    get_register_data( EXPORTING it_users = it_users
                       IMPORTING et_data = DATA(lt_data) ).

    " La versión tiene que estar activa para el envio de push.
    IF ms_appl-active = abap_true.

      " Recorremos los proveedores asociados a la aplicación para enviare el mensaje
      LOOP AT mt_appl_provider ASSIGNING FIELD-SYMBOL(<ls_provider>) WHERE classcontroller IS NOT INITIAL
                                                                           AND active = abap_true.
        TRY.
            " Se instancia el objeto prov
            DATA(lo_provider) = zcl_push_provider_base=>get_instance( EXPORTING iv_langu = mv_langu
                                                                                iv_provider = <ls_provider>-provider ).

            TRY.
                lo_provider->send_push(
                  EXPORTING
                    is_sending_conf = VALUE #( id_appl = <ls_provider>-id_appl )
                    it_register_data = lt_data
                    it_notifications = it_notification
                    it_users = it_users
                  IMPORTING
                    et_return        = et_return ).

              CATCH zcx_push INTO DATA(lx_push).

                INSERT  zcl_push_utils=>fill_return( iv_type = zif_push_data=>cs_message-type-error
                                                     iv_number = '008' ) INTO TABLE et_return.

              CATCH cx_root.
                INSERT zcl_push_utils=>fill_return( iv_type = zif_push_data=>cs_message-type-error
                                                    iv_number     = lx_push->if_t100_message~t100key-msgno
                                                    iv_message_v1 = lx_push->mv_msgv1
                                                    iv_message_v2 = lx_push->mv_msgv2
                                                    iv_message_v3 = lx_push->mv_msgv3
                                                    iv_message_v4 = lx_push->mv_msgv4
                                                    iv_langu      = mv_langu ) INTO TABLE et_return.
            ENDTRY.

          CATCH zcx_push INTO lx_push.

            INSERT zcl_push_utils=>fill_return( iv_type = zif_push_data=>cs_message-type-error
                                                             iv_number     = lx_push->if_t100_message~t100key-msgno
                                                             iv_message_v1 = lx_push->mv_msgv1
                                                             iv_message_v2 = lx_push->mv_msgv2
                                                             iv_message_v3 = lx_push->mv_msgv3
                                                             iv_message_v4 = lx_push->mv_msgv4
                                                             iv_langu      = mv_langu ) INTO TABLE et_return.


        ENDTRY.
      ENDLOOP.
      IF sy-subrc NE 0.
        INSERT  zcl_push_utils=>fill_return( iv_type = zif_push_data=>cs_message-type-error
                                                       iv_number = '009' ) INTO TABLE et_return.
      ENDIF.

    ELSE.
      INSERT  zcl_push_utils=>fill_return( iv_type = zif_push_data=>cs_message-type-error
                                                           iv_number = '013' ) INTO TABLE et_return.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
