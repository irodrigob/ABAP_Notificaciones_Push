INTERFACE zif_push_provider
  PUBLIC .
  TYPES: BEGIN OF ts_sending_conf,
           id_appl      TYPE zpush_e_id_appl,
         END OF ts_sending_conf.

  "! <p class="shorttext synchronized">Send push</p>
  "! @parameter is_sending_conf | <p class="shorttext synchronized">Sending configuration</p>
  "! @parameter it_register_data | <p class="shorttext synchronized">Register data</p>
  "! @parameter it_notifications | <p class="shorttext synchronized">Notifications</p>
  "! @parameter it_user | <p class="shorttext synchronized">Users</p>
  "! @parameter et_return | <p class="shorttext synchronized">Return</p>
  METHODS send_push
    IMPORTING
      is_sending_conf  TYPE ts_sending_conf OPTIONAL
      it_register_data TYPE zif_push_data=>tt_register_data OPTIONAL
      it_notifications TYPE zif_push_data=>tt_notification
      it_users          TYPE zif_push_data=>tt_users OPTIONAL
    EXPORTING
      et_return        TYPE zif_push_data=>tt_return.

ENDINTERFACE.
