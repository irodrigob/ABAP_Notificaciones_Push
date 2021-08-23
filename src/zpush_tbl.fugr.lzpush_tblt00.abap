*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 14.06.2021 at 12:50:45
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPUSH_T001......................................*
DATA:  BEGIN OF STATUS_ZPUSH_T001                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPUSH_T001                    .
CONTROLS: TCTRL_ZPUSH_T001
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZPUSH_T002......................................*
DATA:  BEGIN OF STATUS_ZPUSH_T002                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPUSH_T002                    .
CONTROLS: TCTRL_ZPUSH_T002
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZPUSH_T003......................................*
DATA:  BEGIN OF STATUS_ZPUSH_T003                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPUSH_T003                    .
CONTROLS: TCTRL_ZPUSH_T003
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZPUSH_T004......................................*
DATA:  BEGIN OF STATUS_ZPUSH_T004                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPUSH_T004                    .
CONTROLS: TCTRL_ZPUSH_T004
            TYPE TABLEVIEW USING SCREEN '0004'.
*.........table declarations:.................................*
TABLES: *ZPUSH_T001                    .
TABLES: *ZPUSH_T001T                   .
TABLES: *ZPUSH_T002                    .
TABLES: *ZPUSH_T002T                   .
TABLES: *ZPUSH_T003                    .
TABLES: *ZPUSH_T004                    .
TABLES: ZPUSH_T001                     .
TABLES: ZPUSH_T001T                    .
TABLES: ZPUSH_T002                     .
TABLES: ZPUSH_T002T                    .
TABLES: ZPUSH_T003                     .
TABLES: ZPUSH_T004                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
