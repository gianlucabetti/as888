000100160610      //************************************************************************
000101160610      //
000102160616      // Questo programma, chiamato via DRDA da Strategi, riceve un ID filiale
000103160610      // e restituisce le statistiche delle spunte da VDL/CML.
000104160610      //
000105160610      //************************************************************************
000106160610
000108160610      /COPY GAITRASRC/SRCCONST,BRTLEHSPEC
000109160616       CTL-OPT DFTACTGRP(*NO);
000118160610
000119160610      //************************************************************************
000120160610      //
000121160610      // Costanti.
000122160610      //
000123160610      //************************************************************************
000124160610
000125160610      /COPY GAITRASRC/SRCCONST,FISPP080R
000126160616       DCL-C ERRCOD001 'SVR0001';
000127160616       DCL-C ERRTXT001 'Unrecognized Opcode';
000128160616
000129160616      //************************************************************************
000130160616      //
000131160616      // Strutture.
000132160616      //
000133160616      //************************************************************************
000134160616
000135160616       DCL-DS fispp080i EXT QUALIFIED END-DS;
000136160616       DCL-DS fispp080o EXT QUALIFIED INZ(*EXTDFT) END-DS;
000137160715
000142160616       DCL-DS rp§error QUALIFIED;
000143160616         errCode CHAR(7);
000144160616         errSpace CHAR(1);
000145160616         errText CHAR(200);
000146160616       END-DS;
000147160715
000148160715       DCL-DS this QUALIFIED INZ;
000149160715         esito INT(10);
000152160715       END-DS;
000153160715
000154160715      //************************************************************************
000155160715      //
000156160715      // Campi.
000157160715      //
000158160715      //************************************************************************
000159160715
000160160715       DCL-S scnSaver PACKED(1);
000162160616
000163160610      //************************************************************************
000164160610      //
000165160610      // Prototipi.
000166160610      //
000167160610      //************************************************************************
000168160610
000169160610      /COPY GAITRASRC/SRCPROTOPR,FISPP080R
000170160610
000171160610      //************************************************************************
000172160610      //
000173160610      // Parametri.
000174160610      //
000175160610      //************************************************************************
000176160610
000177160610       DCL-PI Fispp080r;
000178160616         rqsOpc CHAR(10) CONST;
000179160616         rqsDta CHAR(9999) CONST;
000180160616         rqsLen PACKED(4) CONST;
000181160616         rqsCty CHAR(10) CONST;
000182160616         rqsCid CHAR(10) CONST;
000183160616         rpyOpc CHAR(10);
000184160616         rpyDta CHAR(9999);
000185160616         rpyLen PACKED(4);
000207160610       END-PI ;
000231160610
000232160610      //************************************************************************
000233160610      //
000234160610      // Opzioni SQL.
000235160610      //
000236160610      //************************************************************************
000237160610
000238160610       EXEC SQL SET OPTION SQLCA = *NO;
000239160610
000240160610      //************************************************************************
000241160610      //
000242160610      // Main.
000243160610      //
000244160610      //************************************************************************
000245160610
000246160616       CLEAR rpyOpc;
000247160616       CLEAR rpyDta;
000248160616       CLEAR rpyLen;
000249160616       RESET fispp080o;
000250160715       RESET this;
000251160610
000252160616       SELECT;
000253160616         WHEN rqsOpc = 'GETDATI';
000254160616           fispp080i = %SUBST(rqsDta : 1 : rqsLen);
000255160616         OTHER;
000256160616           rpyOpc = '*ERROR';
000257160616           rp§error.errCode = ERRCOD001;
000258160616           rp§error.errText = ERRTXT001 + ' ' + rqsOpc;
000261160616           rpyDta = rp§error;
000262160616           rpyLen = %SIZE(rp§error);
000263160616           RETURN;
000264160616       ENDSL;
000265160616
000280160715       this.esito = GetDataFromFltr800f();
000315160610
000316160610       // Il reperimento dei dati non è riuscito,
000317160610       // quindi pulisco i parametri di output.
000318160610
000319160715       IF this.esito <> FISP_ESITO_OK;
000320160616         fispp080o.orl = *ALL'-';
000322160616         fispp080o.pnc = *ALL'-';
000324160616         fispp080o.pspValore = *ALL'-';
000326160616         fispp080o.ncl = *ALL'-';
000328160616         fispp080o.m01 = *ALL'-';
000329160616         fispp080o.m02 = *ALL'-';
000330160616         fispp080o.m03 = *ALL'-';
000331160616         fispp080o.m04 = *ALL'-';
000332160616         fispp080o.m05 = *ALL'-';
000333160616         fispp080o.m06 = *ALL'-';
000334160616         fispp080o.m07 = *ALL'-';
000335160616         fispp080o.m08 = *ALL'-';
000336160616         fispp080o.m09 = *ALL'-';
000337160616         fispp080o.m10 = *ALL'-';
000348160610       ENDIF;
000349160610
000351160616       rpyOpc = 'DONE';
000352160616       rpyDta = fispp080o;
000353160616       rpyLen = %SIZE(fispp080o);
000354160616
000355160610       RETURN;
000356160610
000357160610       // --------------------------------------------------
000358160610       // Procedure name: GetDataFromFltr800f
000359160610       // Purpose:
000360160610       // Returns:        Esito.
000361160610       // --------------------------------------------------
000362160610
000363160610       DCL-PROC GetDataFromFltr800f ;
000364160610
000365160610         DCL-PI *N INT(10);
000366160610         END-PI ;
000367160610
000368160715         DCL-DS local QUALIFIED STATIC;
000369160725           curTime TIMESTAMP INZ(*SYS);
000370160725           time TIMESTAMP;
000371160726           diffSec INT(10);
000372160715         END-DS;
000373160715
000374160610         DCL-S retField INT(10) STATIC;
000375160610
000376160610         RESET retField;
000377160725         RESET local;
000437160610
000474160610         EXEC SQL
000475160610           SELECT SUBSTR(tr8orl, 1, 2)
000476160610                  CONCAT ':' CONCAT
000477160616                  SUBSTR(tr8orl, 3, 2) CONCAT ':00' AS ORA_LETTURA
000479160616                , tr8brl AS ORA_LETTURA_DSPATR
000480160616                , CHAR(DEC(tr8pnc , 6, 0)) AS COLLI
000481160616                , tr8bnc AS COLLI_DSPATR
000482160616                , CHAR(DEC(SUBSTR(tr8psp, 2, 3) , 3, 0))
000483160616                  AS X100_SUL_GIORNO_PRECEDENTE_VALORE
000484160616                , SUBSTR(tr8psp, 1, 1) AS X100_SUL_GIORNO_PRECEDENTE_SEGNO
000485160616                , tr8bsp AS X100_SUL_GIORNO_PRECEDENTE_DSPATR
000486160616                , CHAR(DEC(tr8ncl, 3, 0)) AS COLLI_ULTIMO_MINUTO
000487160616                , tr8bcl AS COLLI_ULTIMO_MINUTO_DSPATR
000488160610                , tr8m01 AS COLLI_ULTIMO_MINUTO_BAIA_01
000489160616                , tr8b01 AS COLLI_ULTIMO_MINUTO_BAIA_01_DSPATR
000490160616                , tr8m02 AS COLLI_ULTIMO_MINUTO_BAIA_02
000491160616                , tr8b02 AS COLLI_ULTIMO_MINUTO_BAIA_02_DSPATR
000492160616                , tr8m03 AS COLLI_ULTIMO_MINUTO_BAIA_03
000493160616                , tr8b03 AS COLLI_ULTIMO_MINUTO_BAIA_03_DSPATR
000494160616                , tr8m04 AS COLLI_ULTIMO_MINUTO_BAIA_04
000495160616                , tr8b04 AS COLLI_ULTIMO_MINUTO_BAIA_04_DSPATR
000496160616                , tr8m05 AS COLLI_ULTIMO_MINUTO_BAIA_05
000497160616                , tr8b05 AS COLLI_ULTIMO_MINUTO_BAIA_05_DSPATR
000498160616                , tr8m06 AS COLLI_ULTIMO_MINUTO_BAIA_06
000499160616                , tr8b06 AS COLLI_ULTIMO_MINUTO_BAIA_06_DSPATR
000500160616                , tr8m07 AS COLLI_ULTIMO_MINUTO_BAIA_07
000501160616                , tr8b07 AS COLLI_ULTIMO_MINUTO_BAIA_07_DSPATR
000502160616                , tr8m08 AS COLLI_ULTIMO_MINUTO_BAIA_08
000503160616                , tr8b08 AS COLLI_ULTIMO_MINUTO_BAIA_08_DSPATR
000504160616                , tr8m09 AS COLLI_ULTIMO_MINUTO_BAIA_09
000505160616                , tr8b09 AS COLLI_ULTIMO_MINUTO_BAIA_09_DSPATR
000506160616                , tr8m10 AS COLLI_ULTIMO_MINUTO_BAIA_10
000507160616                , tr8b10 AS COLLI_ULTIMO_MINUTO_BAIA_10_DSPATR
000508160715                , tr8time
000509160616             INTO :fispp080o.orl
000510160616                , :fispp080o.brl
000511160616                , :fispp080o.pnc
000512160616                , :fispp080o.bnc
000513160616                , :fispp080o.pspValore
000514160616                , :fispp080o.pspSegno
000515160616                , :fispp080o.bsp
000516160616                , :fispp080o.ncl
000517160616                , :fispp080o.bcl
000518160616                , :fispp080o.m01
000519160616                , :fispp080o.b01
000520160616                , :fispp080o.m02
000521160616                , :fispp080o.b02
000522160616                , :fispp080o.m03
000523160616                , :fispp080o.b03
000524160616                , :fispp080o.m04
000525160616                , :fispp080o.b04
000526160616                , :fispp080o.m05
000527160616                , :fispp080o.b05
000528160616                , :fispp080o.m06
000529160616                , :fispp080o.b06
000530160616                , :fispp080o.m07
000531160616                , :fispp080o.b07
000532160616                , :fispp080o.m08
000533160616                , :fispp080o.b08
000534160616                , :fispp080o.m09
000535160616                , :fispp080o.b09
000536160616                , :fispp080o.m10
000537160616                , :fispp080o.b10
000538160715                , :local.time
000539160610             FROM fltr800f
000540160616            WHERE tr8fil = :fispp080i.filiale AND tr8tip = ''
000541160610         ;
000542160610
000543160610         SELECT;
000544160610           WHEN sqlCode < *ZERO;
000545160610             DUMP(A);
000546160610             RETURN sqlCode;
000547160610           WHEN sqlCode = 100;
000548160610             RETURN FISP_ESITO_NO_DATI;
000549160610         ENDSL;
000558160616
000559160725         local.curTime = %TIMESTAMP();
000560160725
000561160715         // Controllo attivazione salvaschermo.
000562160715
000563160715         IF fispp080i.scnSaverMn > *ZERO AND
000564160725           %DIFF(local.curTime : local.time : *MN) > fispp080i.scnSaverMn;
000565160715           IF scnSaver = *HIVAL;
000566160715             scnSaver = 1;
000567160715           ELSE;
000568160715             scnSaver += 1;
000569160715           ENDIF;
000570160715           fispp080o.scnSaver = scnSaver;
000574160715         ENDIF;
000590160725
000591160725         // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
000592160725
000593160725         RETURN retField ;
000594160610
000595160610       END-PROC ;
000596160610
