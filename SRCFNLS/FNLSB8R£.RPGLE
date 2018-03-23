000100031205      *------------------------------------------------------------------------*
000200080709      * nuovo pgm di stampa LDV
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600080704
000700080709     d fnlsb5ds      e ds
000800080709     d trulpovrds    e ds
000900080709     d printerDA       s             10    inz('FNLSB5PA4')
001000080709     d comman          s            150    inz('OVRPRTF  FILE(FNLSB6P)')
001100080709     d lung            s             15  5 inz(150)
001200080709     d
001300080709     D TRULPOVRC       pr                  extpgm('TRULPOVRC')
001400080709     D  printerda                    10
001500080709     D  trulpovrds                         likeds(trulpovrds)
001600080709     d
001700080709     D QCMDEXC         pr                  extpgm('QCMDEXC')
001800080709     D  COMMAN                      100
001900080709     D  LUNG                         15  5
002000071122      *------------------------------------------------------------------------*
002100080709     c     *entry        plist
002200080709     c                   parm                    fnlsb5ds
002300071116      /free
002400080709
002500071122       *inlr = *on   ;
002600080709
002700080709       // Richiamo pgm per avere la stringa per la ovrprtf
002800080709
002900080715       §ulptipmda='  '   ;
003000080715       §ulptipm_a='  '   ;
003100080709       TRULPOVRC (printerda:trulpovrds)    ;
003200071122
003300080709       if §ulpoerr=' '     ;
003400080709       comman =%trim(comman ) + ' ' + %trim(§ulpocmo) +' OVRSCOPE(*JOB)'   ;
003500080709       else   ;
003600080709       comman =%trim(comman ) + ' '             +' OVRSCOPE(*JOB)'   ;
003700080709       endif     ;
003800080709
003900080709       // eseguo QCMDEXC
004000080709
004100080709       callp(e) QCMDEXC  (comman:lung)    ;
004200071122
004300080709       //  Imposto i flag mancanti della DS
004400080709       if db0fa4=' ' ;
004500080709       db0fa4='1'    ;
004600080709       db0fl6='1'    ;
004700080709       endif          ;
004800071122
004900071116      /end-free
