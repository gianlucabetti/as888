000100031205      *------------------------------------------------------------------------*
000200080709      * nuovo pgm di stampa LDV
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500160302     h dftactgrp(*no) actgrp(*caller)
000600031205      *------------------------------------------------------------------------*
000700080704
000800080709     d fnlsb5ds      e ds
000900080709     d trulpovrds    e ds
001000080709     d printerDA       s             10    inz('FNLSB5PA4')
001100080709     d comman          s            150    inz('OVRPRTF  FILE(FNLSB6P)')
001200080709     d lung            s             15  5 inz(150)
001300080709     d
001400080709     D TRULPOVRC       pr                  extpgm('TRULPOVRC')
001500080709     D  printerda                    10
001600080709     D  trulpovrds                         likeds(trulpovrds)
001700080709     d
001800080709     D QCMDEXC         pr                  extpgm('QCMDEXC')
001900080709     D  COMMAN                      100
002000080709     D  LUNG                         15  5
002100071122      *------------------------------------------------------------------------*
002200080709     c     *entry        plist
002300080709     c                   parm                    fnlsb5ds
002400071116      /free
002500080709
002600071122       *inlr = *on   ;
002700080709
002800080709       // Richiamo pgm per avere la stringa per la ovrprtf
002900080709
003000080715       §ulptipmda='  '   ;
003100080715       §ulptipm_a='  '   ;
003200080709       TRULPOVRC (printerda:trulpovrds)    ;
003300071122
003400080709       if §ulpoerr=' '     ;
003500080709       comman =%trim(comman ) + ' ' + %trim(§ulpocmo) +' OVRSCOPE(*JOB)'   ;
003600080709       else   ;
003700080709       comman =%trim(comman ) + ' '             +' OVRSCOPE(*JOB)'   ;
003800080709       endif     ;
003900080709
004000080709       // eseguo QCMDEXC
004100080709
004200080709       callp(e) QCMDEXC  (comman:lung)    ;
004300071122
004400080709       //  Imposto i flag mancanti della DS
004500080709       if db0fa4=' ' ;
004600080709       db0fa4='1'    ;
004700080709       db0fl6='1'    ;
004800080709       endif          ;
004900071122
005000071116      /end-free
