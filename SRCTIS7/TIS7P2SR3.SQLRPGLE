000100091228       //==============================================================
000200130403       // Driver scrittura WAS7IFS0F - PDF to SPOOL
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100091228
001200091228       //--------------------------------------------------------------
001300121106       // Specifiche di controllo.
001400091228       //--------------------------------------------------------------
001500091223
001600091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700100325     h dftactgrp(*no)
001800091223     h alwnull(*inputonly)
001900100325     h bnddir('UBBNDDIR')
002000091223
002100091223       //--------------------------------------------------------------
002200121106       // Dichiarazione file.
002300091223       //--------------------------------------------------------------
002400100112
002500130403     FWAS7IFS0F O    E             DISK    usropn
002600091223
002700091223       //--------------------------------------------------------------
002800121106       // Definizione costanti.
002900091223       //--------------------------------------------------------------
003000100302
003100091223
003200091223       //--------------------------------------------------------------
003300121106       // Definizione schiere.
003400091223       //--------------------------------------------------------------
003500091223
003600091223
003700091223       //--------------------------------------------------------------
003800121106       // Definizione aree dati.
003900091223       //--------------------------------------------------------------
004000091223
004100091223
004200091223       //--------------------------------------------------------------
004300121106       // Definizione strutture dati.
004400091223       //--------------------------------------------------------------
004500130403     D pdftabel      e ds
004600130403
004700121106       // - Status
004800110516     d Psds           sds
004900110516     d   SDSpgm          *proc
005000121106     d   JobName             244    253                                         Job name
005100121106     d   JobUser             254    263                                         User name
005200121106     d   JobNumber           264    269s 0                                      Job number
005300091223
005400091223       //--------------------------------------------------------------
005500121106       // Definizione variabili.
005600091223       //--------------------------------------------------------------
005700091223
005800130403       // chiamata a PDF100R
005900130403     D pTBKEY          s                   like(TBKEY)
006000130403     D pTBELE          s                   like(TBELE)
006100130416     D pPGM            s             10A
006200130403     D pTBDAT          s                   like(TBDAT)
006300130403     D pEsito          s              1A
006400130403
006500130403       // - Parametri
006600130403     d pIn_FolderPDF   s            256a
006700130403     d pIn_NamePDF     s            256a
006800130403       // '0'=OK '1'=errore
006900130403     d pOut_Esito      s              1A
007000091223
007100121106       // Stringa SQL da eseguire
007200130108     d wSQL            s           5000    inz  varying
007300121106
007400121106       // Campi di comodo
007500130403     d wTimeStamp      s             14s 0
007600121106     d wDate           s              8  0 inz
007700130403     d w1TBDAT         s                   like(pTBDAT)
007800130403     d w2TBDAT         s                   like(pTBDAT)
007900130408     d w3TBDAT         s                   like(pTBDAT)
008000130924     d w4TBDAT         s                   like(pTBDAT)
008100091223
008200130923
008300091223       //--------------------------------------------------------------
008400121106       // Definizione prototipi procedure.
008500091223       //--------------------------------------------------------------
008600130307
008700130403     d PDF100R         pr                  extpgm('PDF100R')
008800130403     d  pTBKEY                             like(pTBKEY)
008900130403     d  pTBELE                             like(pTBELE)
009000130416     d  pPGM                               like(pPGM)
009100130403     d  pTBDAT                             like(pTBDAT)
009200130403     d  pEsito                             like(pEsito)
009300130923
009400130403
009500091223       //--------------------------------------------------------------
009600121106       // Definizione key-list.
009700091223       //--------------------------------------------------------------
009800091223
009900091223
010000091223       //--------------------------------------------------------------
010100121107       // Definizione parametri procedura.
010200091223       //--------------------------------------------------------------
010300091223
010400091223     c     *Entry        plist
010500130403     c                   parm                    pIn_FolderPDF
010600130403     c                   parm                    pIn_NamePDF
010700130403     c                   parm                    pOut_Esito
010800091223
010900130307      /free
011000091223
011100091223       //--------------------------------------------------------------
011200121106       // M A I N - L I N E
011300091223       //--------------------------------------------------------------
011400130307
011500130307       // Operazioni iniziali?
011600130307       exsr sr_RoutInz;
011700130924
011800130923
011900130403       // prima scrittura WAS7IFS0F
012000130403       IFSINS    = wTimeStamp;
012100130403       IFSKEY    = %trim(JobName) + '/' + %trim(JobUser) + '/' +
012200130403                   %trim(%editc(JobNumber:'X'));
012300130403       IFSPTHS   = %trim(w1TBDAT) + '/';
012400130924       IFSFILS   = %trim(pIn_FolderPDF) + '_' + %trim(pIn_NamePDF);
012500130924       IFSOPZ    = 'DELN';
012600130923       IFSTRYTOT = 100;
012700130403       IFSSTS    = *blanks;
012800130403       write WAS7IFS0;
012900130403
013000130403       // seconda scrittura WAS7IFS0F
013100130403       IFSINS    = wTimeStamp;
013200130403       IFSKEY    = %trim(JobName) + '/' + %trim(JobUser) + '/' +
013300130403                   %trim(%editc(JobNumber:'X'));
013400130403       IFSPTHS   = %trim(w2TBDAT) + '/';
013500130924       IFSFILS   = %trim(pIn_FolderPDF) + '_' + %trim(pIn_NamePDF);
013600130924       IFSOPZ    = 'DELN';
013700130923       IFSTRYTOT = 100;
013800130403       IFSSTS    = *blanks;
013900130403       write WAS7IFS0;
014000130408
014100130408       // terza   scrittura WAS7IFS0F
014200130408       IFSINS    = wTimeStamp;
014300130408       IFSKEY    = %trim(JobName) + '/' + %trim(JobUser) + '/' +
014400130408                   %trim(%editc(JobNumber:'X'));
014500130408       IFSPTHS   = %trim(w3TBDAT) + '/' + %trim(pIn_FolderPDF) + '/';
014600130924       IFSFILS   = %trim(pIn_NamePDF);
014700130924       IFSOPZ    = 'DELN';
014800130923       IFSTRYTOT = 100;
014900130408       IFSSTS    = *blanks;
015000130408       write WAS7IFS0;
015100130924
015200130924       // quarta  scrittura WAS7IFS0F
015300130924       IFSINS    = wTimeStamp;
015400130924       IFSKEY    = %trim(JobName) + '/' + %trim(JobUser) + '/' +
015500130924                   %trim(%editc(JobNumber:'X'));
015600140708       IFSPTHS   = %trim(w4TBDAT) + '/';
015700130924       IFSFILS   = %trim(pIn_FolderPDF) + '_' + %trim(pIn_NamePDF);
015800130924       IFSOPZ    = 'DELN';
015900130924       IFSTRYTOT = 100;
016000130924       IFSSTS    = *blanks;
016100130924       write WAS7IFS0;
016200091223
016300121106       // Operazioni finali?
016400091223       exsr sr_RoutEnd;
016500130923
016600130923
016700091223
016800091223       //--------------------------------------------------------------
016900121107       // Operazioni iniziali.
017000091223       //--------------------------------------------------------------
017100091223       BEGSR  sr_RoutInz;
017200091223
017300091223         exec sql  set option  dynusrprf = *owner,
017400121107                               closqlcsr = *endmod;
017500130307
017600130403         // apro il file da scrivere, se chiuso
017700130403         if not %open(WAS7IFS0F);
017800130403           open WAS7IFS0F;
017900130403         endif;
018000130403
018100130403         pOut_Esito = '0';
018200121108
018300130403         // BRT/PATHWORK
018400130403         pTBKEY = 'BRT';
018500130403         pTBELE = 'PATHWORK';
018600130416         pPGM   = 'TIS7P2SR3';
018700130403         pTBDAT = *blanks;
018800130403         callP(e) PDF100R (pTBKEY :
018900130403         pTBELE :
019000130416         pPGM   :
019100130403         pTBDAT :
019200130403         pEsito);
019300130403         if pEsito = '1';
019400130403           pOut_Esito = '1';
019500130403           exsr sr_RoutEnd;
019600130403         else;
019700130403           w1TBDAT = pTBDAT;
019800130403         endif;
019900130403
020000130403         // DDT/ERRPATH
020100130403         pTBKEY = 'DDT';
020200130403         pTBELE = 'ERRPATH';
020300130416         pPGM   = 'TIS7P2SR3';
020400130403         pTBDAT = *blanks;
020500130403         callP(e) PDF100R (pTBKEY :
020600130403         pTBELE :
020700130416         pPGM   :
020800130403         pTBDAT :
020900130403         pEsito);
021000130403         if pEsito = '1';
021100130403           pOut_Esito = '1';
021200130403           exsr sr_RoutEnd;
021300130403         else;
021400130403           w2TBDAT = pTBDAT;
021500130403         endif;
021600130408
021700130408         // BRT/PATHORIGINE
021800130408         pTBKEY = 'BRT';
021900130408         pTBELE = 'PATHORIGINE';
022000130416         pPGM   = 'TIS7P2SR3';
022100130408         pTBDAT = *blanks;
022200130408         callP(e) PDF100R (pTBKEY :
022300130408         pTBELE :
022400130416         pPGM   :
022500130408         pTBDAT :
022600130408         pEsito);
022700130408         if pEsito = '1';
022800130408           pOut_Esito = '1';
022900130408           exsr sr_RoutEnd;
023000130408         else;
023100130408           w3TBDAT = pTBDAT;
023200130408         endif;
023300130924
023400130924         // DDT/DOCPATH
023500130924         pTBKEY = 'DDT';
023600130924         pTBELE = 'DOCPATH';
023700130924         pPGM   = 'TIS7P2SR3';
023800130924         pTBDAT = *blanks;
023900130924         callP(e) PDF100R (pTBKEY :
024000130924         pTBELE :
024100130924         pPGM   :
024200130924         pTBDAT :
024300130924         pEsito);
024400130924         if pEsito = '1';
024500130924           pOut_Esito = '1';
024600130924           exsr sr_RoutEnd;
024700130924         else;
024800130924           w4TBDAT = pTBDAT;
024900130924         endif;
025000130403
025100121108         wDate = %dec( %date() );
025200130403         wTimeStamp = %dec( %timestamp() ) / 1000000;
025300091223
025400091223       ENDSR;
025500121106
025600091223       //--------------------------------------------------------------
025700121107       // Operazioni finali.
025800091223       //--------------------------------------------------------------
025900091223       BEGSR  sr_RoutEnd;
026000130109
026100130109         // Chiusura pgm
026200130403         // se il pgm va in errore, chiudo in LR
026300130403         if pOut_Esito='1';
026400130307           *inlr = *on;
026500130307         else;
026600130307           *inrt = *on;
026700130307         endif;
026800130109         return;
026900091223
027000091223       ENDSR;
027100091223
027200091223      /end-free
