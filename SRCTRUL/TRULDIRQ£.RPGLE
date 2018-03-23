000100110623     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000200110623
000300110624     Fprtf198   O    F  198        PRINTER OFLIND(*INOA) USROPN
000400110624
000500161011      /COPY GAITRASRC/SRCCONST,TRULDIR
000600161011      /COPY GAITRASRC/SRCPROTOPR,TRULDIR
000700110624
000701161012     D FileName        S            512    varying
000702161012     D Dir             S            512    varying
000800161012     D esito           S             10I 0
001100110624     D logDta          S            198A   VARYING
001101161012     D kpjba         E DS                  QUALIFIED
001200161011
001201161012     C     *ENTRY        PLIST
001202161012     C                   PARM                    kpjba
001203161012
001500110623       *INLR = *ON;
001600110623
001700161012       // '/home/edpPF/SCT esiti/scaricati'
001701161012       // '/qntc/Cedintesa/telemaco/prova_edppf/scaricati'
001702161012           dir = '/qntc/Cedintesa/telemaco/prova_edppf/scaricati';
001703161012        IF DIR_NewRichiamo(Dir:'a':'a') < DIR_ESITO_OK;
002000110624         RETURN;
002100110624       ENDIF;
002200161011
002207110624       OPEN prtf198;
002208110624
002300161011       DOU esito < DIR_ESITO_OK OR esito = DIR_ESITO_ROW_NOT_FOUND;
002301161012         clear FileName;
002400161012         esito = DIR_GetNextObjectName(FileName);
002500161011         IF esito = DIR_ESITO_ROW_NOT_FOUND OR esito < DIR_ESITO_OK;
002501110624           CLOSE prtf198;
002600161011           DIR_EndRichiamo();
002700110624           LEAVE;
002800110624         ENDIF;
002801161012         logDta = %trim(dir) + '/' + %trim(FileName) ;
003000110624         EXCEPT log;
003100110624       ENDDO;
003200110624
003300161012
005100110624     Oprtf198   E            log
005200110624     O                       logDta
