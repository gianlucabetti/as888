000100161014     H DFTACTGRP(*NO) ACTGRP(*caller)
000200110623
000701161014     D esito           S             10I 0
001100161013
001239161014      /COPY GAITRASRC/SRCprotopr,TRULSTAT
001240161014
001257161014     D stat            PR            10I 0 ExtProc('stat')
001258161014     D   path                          *   value options(*string)
001259161014     D   buf                           *   value
001260161014
001261161014     D StatDs          dS                  likeds(trulstatds)
001263161014     D MySize          S             10I 0
001264161014     D FileName        S           1000    varying
001265161014     D wFileName       S           1000    varying
001266161014
001273161014      /COPY QSYSINC/QRPGLESRC,QUSEC
001274161014
001280161012     C     *ENTRY        PLIST
001281161014     C                   PARM                    FileName
001282161014     C                   Parm                    StatDs
001283161014     C                   Parm                    Esito
001284161013
001500161013           clear  Esito;
001501161014
001502161014              wFileName = %trim(FileName)  + x'00';
001686161014         // get stat info into "MyStat":
001687161014                  esito = stat(wFileName:%addr(statDs));
001688161014
001689161014                 if esito < 0;
001690161014                       esito = -1;
001691161014                  endif;
001692161014
001698161014                *inrt = *on;
001713161014                Return;
001714161014
