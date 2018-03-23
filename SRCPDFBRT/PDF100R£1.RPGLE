000100110617      *---------------------------------------------------------------*
000200130309      * Reperimento parametri PDFTABEL                                *
000300110617      *---------------------------------------------------------------*
000400110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500130308     h dftactgrp(*no) actgrp(*caller)
000600110617
000700110617      *---------------------------------------------------------------*
000800130308     fpdftabel  if   e           k disk
000900110617
001000130309     D pTBKEY          s                   like(TBKEY)
001100130309     D pTBELE          s                   like(TBELE)
001200130416     D pPGM            s             10A
001300130309     D pTBDAT          s                   like(TBDAT)
001400130309     D pEsito          s              1A
001500130408
001600130408
001700130408     D TIS701C1        PR
001800130408     D                                     extpgm('TIS701C1')
001900130408     D wTO                          253A
002000130408     D wCC                          253A
002100130408     D wOBJ                          44A
002200130408     D wMSG                        5000A
002300130408     D wEsito                         1A
002400130408
002500130408     D wTO             s            253A
002600130408     D wCC             s            253A
002700130408     D wOBJ            s             44A
002800130408     D wMSG            s           5000A
002900130408     D wEsito          s              1A
003000110617
003100130309
003200110627     C     *ENTRY        PLIST
003300130309     C                   PARM                    pTBKEY
003400130309     C                   PARM                    pTBELE
003500130416     C                   PARM                    pPGM
003600130309     C                   PARM                    pTBDAT
003700130309     C                   PARM                    pEsito
003800130309
003900110617      /free
004000130309
004100130309       // setto esito a OK
004200130309       pEsito = '0';
004300130309
004400130309       // setto esito a OK
004500130309       chain (pTBKEY : pTBELE) pdftabel;
004600130309       if %found(pdftabel);
004700130309          pTBDAT = %trim(tbdat);
004800130309       else;
004900130309          pEsito = '1';
005000130408          // Inoltre invio una email a cedalert per segnalazione incongruenza
005100130408          wTO  = 'cedalert@brt.it';
005200130408          wCC  = *blanks;
005300130408          wOBJ = 'PDFTABEL - WARNING!!!';
005400130408          wMSG = 'ATTENZIONE: ' + ':/N' + ':/N' +
005500130408                 'richiesto elemento in PDFTABEL inesistente: ' + ':/N' +
005600130408                 'KEY: ' + %trim(pTBKEY) + ':/N' +
005700130408                 'ELE: ' + %trim(pTBELE) + ':/N' + ':/N' +
005800130416                 'Verificare file tabelle PDF e *pgm ' + %trim(pPGM) +
005900130416                 ' che utilizza PDF100R';
006000130408
006100130408          callP(e) TIS701C1 (wTO    :
006200130408                             wCC    :
006300130408                             wOBJ   :
006400130408                             wMSG   :
006500130408                             wEsito);
006600130309       endif;
006700130308
006800130313       eval *inlr = *on;
006900130308
