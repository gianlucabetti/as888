000100140113     H DFTACTGRP(*NO) ACTGRP('QILE') DATFMT(*ISO) BNDDIR('PJXBND')
000401151123
000402150320      *
000403150320      * Scittura file YHSCT00F e creazione file xml Distinta SEPA
000404150320      *
000405150320
001200140107     D KPJBA         E DS
001201140304     D sql             s           7000
001324140117
001325150323     D EXCEPTIONMESSAGEID...
001326150323     D                 S              7A   IMPORT('_EXCP_MSGID')
001328150323
001331140227     D MY_LOG          S                   SQLTYPE(CLOB_FILE) CCSID(1252)
001332160127     D ifsdir          S            500    varying
001334140424     D ifslog          S            500    varying
001335140227     D CRLF            S              2A   Inz(x'0D25')
001336140227
001337140109     D SocK58          S              3
001338140124     D NDtK58          S              9  0
001339140109     D DDtK58          S               D
001340140109     D KccK58          S              6
001341140109     D ElaK58          S              1
001342140109     D ErrK58          S              1
001343140117     D OpzK58          S              2
001344140227     D iidd            S             35
001345140226     D alfa35          S             35
001346140303     D alfa20          S             20
001347140306     D filename        S            200    varying
001348140110     D conteggio       S              3  0
001349140303     D limite          S             11  2
001350140305     D numero          S              5  0
001351140305     D somma           S             11  2
001352140307     D textstring      S           1000
001353140307     D testo           S          32740    varying
001354140306     D cmd             S            300
001355140120
001356140113     D LckLvlChg       C                   '1'
001357140113     D LckLvlAll       C                   '2'
001358140113     D CmtScopeActGrp  C                   '1'
001359140113     D
001360140113     D xStrCmt         PR                  EXTPROC('XSTRCMT')
001361140113     D  lckLvl                        1    CONST                                Livello vincolo
001362140113     D  cmtScope                      1    CONST                                Ambito definizione s
001363140307
001364140124     D* Reperimento nome PGM
001365140124     D                SDS           333
001366140124     D  DSPGM            *PROC
001367140124     D  PARMS            *PARMS
001368140227     D  utente               254    263
001369140307
001370140307     D/COPY QSYSINC/QRPGLESRC,QUSEC
001371140307     D/COPY GAITRASRC/SRCPROTOPR,QUILNGTX
001372140306     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
001373140113
001801160127
001805160127            //  exsr insert;
001806160127             exsr insertClob;
001807160127
001810140306              exsr Uscita;
001811140306
001812140306        begsr *inzsr;
002003140224
002004160127            exec sql set option decmpt = *period , commit = *chg;
002005160127
002010160127        endsr;
002014140113
003903140306        begsr uscita;
003904140306
003906160127            exec sql rollback;
003918160127            return;
003921140306
003922140306        endsr;
003923140110
003924160127        begsr insertclob;
003925160127
003926160127
003927160127
003928160127        ENDSR;
003929160127
004008140227        begsr inzsql;
004869140120
004870160127        sql = 'insert into YHSCT00F';
005071140120
005072140120         endsr;
005073140120
005108140227         begsr insert;
005109140227
005110140227           // compongo stringa senza validazione schema
005111140227         exsr inzsql;
005112140227         exec sql execute immediate :sql;
005113140227
005114140306         If sqlcod < 0;    // errore in insert
005115140307          exec sql get diagnostics :testo = all;
005116140307          exsr logsql;
005117140307          errK58 = '1';
005118140307          textString = 'Errore insert YHSCT00F --- '+ testo;
005119140307          EXSR displayText;
005120140307          exsr endback;
005122140306         endif;
005123140306
005124140227         If utente = 'EDPPF';         // LOG FELLETTI
005125140227         exsr logsql;
005126140227         endif;
005127140227
005128140227         endsr;
005129160127
005269160127       //********************************************************************************
005286140307       // Richiamo a Display Long Text (QUILNGTX) API.
005287140307       //********************************************************************************
005288140307            BEGSR displayText;
005289140307
005290140307                  RESET qusec;
005291140307                  qusbprv = %SIZE(qusec);
005292140307                  DisplayLongText( textString
005293140307                                 : %SIZE(textString)
005294140307                                 : 'BAR0019'
005295140307                                 : 'YBARMSG   *LIBL'
005296140307                                 : qusec
005297140307                                 );
005298140307
005299140307            ENDSR;
005300140307
005301140305
005302140227         begsr logsql;
005303140227
005304140227           my_log_name = ifslog + 'sql.log';
005305140227           my_log_fo = SQFAPP;
005306140227           my_log_nl = %len(%trim(my_log_name));
005307140224
005308140227           exec sql set :my_log = :crlf concat
005309140227            'LOG_SQL___' concat char(current timestamp)
005310140227            concat '_' concat trim(user) concat '_' concat :CRLF
005311140227             concat trim(:sql)  concat :CRLF;
005312140227
005313140224         ENDSR;
005314160127
005331140306         begsr endback;
005332140306
005333140306           exec sql rollback;
005334140306          return;
005335140306
005336140306         ENDSR;
005337150323
005338160127
