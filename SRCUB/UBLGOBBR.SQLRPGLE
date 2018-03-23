000100970526     H*------------------------------------------------------------------------*
000200161110     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('QILE')
000300970526     F*------------------------------------------------------------------------*
000400170327
000500170328     FUBLGOBBP  O    E             printer usropn
000600170328     f                                     infds(InfPrtF)
000700170328     f                                     oflind(*in44)
000800170327
000900170327       //--------------------------------------------------------------
001000170327       // Definizione costanti.
001100170327       //--------------------------------------------------------------
001200170327
001300170327
001400170327       //--------------------------------------------------------------
001500170327       // Definizione schiere.
001600170327       //--------------------------------------------------------------
001700170327
001800170327
001900170327       //--------------------------------------------------------------
002000170327       // Definizione aree dati.
002100170327       //--------------------------------------------------------------
002200170327
002300170328       // InfDS del PrtF?
002400170328     d InfPrtF         ds                  qualified
002500170328         // -?Controllo overflow?
002600170328     d   OvrFlw              367    368b 0
002700170327
002800170327       //--------------------------------------------------------------
002900170327       // Definizione strutture dati.
003000170327       //--------------------------------------------------------------
003100170327
003200170327       // parametro d'ingresso
003300170328     d wSQl            s           1024    varying
003400170328     d oError          s              1
003500170329     d oMsg            s            132
003600170327
003700170328     d WAFOB_fetch1    ds                  qualified
003800170328     d  Logica                        2a
003900170328     d  DtEstr                         d
004000170328     d  Lib                          10a
004100170328     d  ObjT                          7a
004200170328     d  Obj                          10a
004300170328     d  Attr                         10a
004400170328     d  Dati                        130a
004500170328
004600170327       // - Status
004700170327     d Psds           sds
004800170327     d   SDSpgm          *proc
004900170327     d   JobName             244    253                                         Job name
005000170327     d   JobUser             254    263                                         User name
005100170327     d   JobNumber           264    269s 0                                      Job number
005200170327
005300170327       //--------------------------------------------------------------
005400170327       // Definizione variabili.
005500170327       //--------------------------------------------------------------
005600170327
005700170327       // Campi di comodo
005800170327
005900170327       //--------------------------------------------------------------
006000170327       // Definizione prototipi procedure.
006100170327       //--------------------------------------------------------------
006200170327
006300170327       // Parametri API QCAPCMD (Process Commands)?
006400170327     d Qcmd            s           2048    inz  varying
006500170327      /copy qSysInc/qRpgleSrc,QCAPCMD
006600170327       // API QCAPCMD (Process Commands)?
006700170327      /copy gaitrasrc/srcProtoPR,QCAPCMD
006800170327
006900170327       // - Parametri gestione errori API
007000170327      /copy qSysInc/qRpgleSrc,QUSEC
007100170327
007200170327       //--------------------------------------------------------------
007300170327       // Definizione key-list.
007400170327       //--------------------------------------------------------------
007500170327
007600170327
007700170327       //--------------------------------------------------------------
007800170327       // Definizione parametri procedura.
007900170327       //--------------------------------------------------------------
008000170327
008100170327     c     *Entry        plist
008200170328     c                   parm                    wSQL
008300170328     c                   parm                    knmus
008400170328     c                   parm                    oError
008500170329     c                   parm                    oMsg
008600170327
008700170327       //--------------------------------------------------------------
008800170327       // M A I N - L I N E
008900170327       //--------------------------------------------------------------
009000170327
009100170327       // Operazioni iniziali?
009200170327       exsr  sr_RoutInz;
009300170327
009400170327       // Scorro il cursore e stampo
009500170327       exsr  PrtList;
009600170327
009700170327       // esco
009800170327       exsr sr_RoutEnd;
009900170327
010000170327       //--------------------------------------------------------------
010100170327       // Operazioni iniziali.                                         ?
010200170327       //--------------------------------------------------------------
010300170327       begsr  sr_RoutInz;
010400170327
010500170327         *inRT = *on;
010600170327         oError = '0';
010700170327
010800170327         // ctrl parametri in ingresso
010900170327         exsr CtrlParmIn;
011000170327
011100170327         // se non c'è uno statement SQL, vuol dire che devo stampare tutto
011200170327         if wSQL = *blank;
011300170328           WSQL = 'select +
011400170328                   foblogica, fobdtestr, foblib, fobobjt, fobobj, fobattr, +
011500170328                   fobdati +
011600170328                   from wafob00f';
011700170327         endif;
011800170328
011900170328         // apro il prtf
012000170328         if not %open(UBLGOBBP);
012100170328           open UBLGOBBP;
012200170328         endif;
012300170328
012400170328         // stampo la testata, l'istruzione SQL e l'intestazione di colonna
012500170328         write GOBBT01;
012600170328         // sicuramernte wSQL non è vuota, per cui la prima parte della stringa c'è
012700170328         if %len(wSQL) >= 132;
012800170328           t02stmt1 = %subst(wSQL:1:132);
012900170328         else;
013000170328           t02stmt1 = %subst(wSQL:1:%len(wSQL));
013100170328         endif;
013200170328         if %len(wSQL) >  132;
013300170328         if %len(wSQL) >= 264;
013400170328           t02stmt2 = %subst(wSQL:133:132);
013500170328         else;
013600170328           t02stmt2 = %subst(wSQL:133:%len(wSQL)-132);
013700170328         endif;
013800170328         if %len(wSQL) >  264;
013900170328         if %len(wSQL) >= 396;
014000170328           t02stmt3 = %subst(wSQL:265:132);
014100170328         else;
014200170328           t02stmt3 = %subst(wSQL:265:%len(wSQL)-264);
014300170328         endif;
014400170328         endif;
014500170328         endif;
014600170328         write GOBBT02;
014700170328         write GOBBT03;
014800170327
014900170327       endsr;
015000170327
015100170327       //--------------------------------------------------------------
015200170327       // Controllo parametri in ingresso                             ?
015300170327       //--------------------------------------------------------------
015400170327       begsr  CtrlParmIn;
015500170327
015600170327
015700170327       endsr;
015800160831
015900170327       //--------------------------------------------------------------
016000170328       // Stampa lista
016100170327       //--------------------------------------------------------------
016200170327       begsr  PrtList;
016300160901
016400160831         // Dichiarazione cursore
016500160831         exec sql   prepare S1   from :wSQL;
016600160831         exec sql   declare C1  asensitive   cursor for S1;
016700160831
016800160831         // Apertura del cursore
016900160831         exec sql   open C1;
017000170328
017100170328         // lettura cursore
017200170328         exec sql  fetch next  from C1  into :WAFOB_fetch1;
017300160831
017400170328         // errore SQL
017500170328         if SQLCode < 0;
017600170328           oError = '1';
017700170329           oMsg = 'UBLGOBBR - Errore SQL (ma la stampa è stata fatta)';
017800170328           exsr sr_RoutEnd;
017900160831         endif;
018000170328
018100170328         // nessun rcd
018200170328         if SQLCode = 100;
018300170328           oError = '2';
018400170329           oMsg = 'UBLGOBBR - Nessun record con lo statement SQL +
018500170329                  (ma la stampa è stata fatta)';
018600170329           exsr sr_RoutEnd;
018700170328         endif;
018800170327
018900170327         dou SQLCode = 100
019000170327          or SQLCode < 0;
019100170328
019200170328             // valorizzo campi in stampa
019300170328             P01Logica = WAFOB_fetch1.Logica;
019400170328             P01DtEstr = %char(WAFOB_fetch1.DtEstr:*eur);
019500170328             //P01DtEstrI = WAFOB_fetch1.DtEstr;
019600170328             P01Lib    = WAFOB_fetch1.Lib;
019700170328             P01ObjT   = WAFOB_fetch1.ObjT;
019800170328             P01Obj    = WAFOB_fetch1.Obj;
019900170328             P01Attr   = WAFOB_fetch1.Attr;
020000170328             P01UtC    = %subst(WAFOB_fetch1.Dati:1:10);
020100170328             //P01Dati   = WAFOB_fetch1.Dati;
020200170328             // stampo riga
020300170328             // Test overflow?
020400170328             //if  InfPrtF.OvrFlw > 60;
020500170328             if  *in44 = *on;
020600170328               write  GOBBT01;
020700170328               write  GOBBT03;
020800170328               *in44 = *off;
020900170328             endif;
021000170328             write  GOBBP01;
021100170328
021200170328           exec sql  fetch next  from C1  into :WAFOB_fetch1;
021300170328
021400170328         enddo;
021500170327
021600170327         // Chiudo il cursore
021700170327         exec sql  close C1;
021800160831
021900160831       ENDSR;
022000170328
022100170328       //--------------------------------------------------------------
022200170328       // Operazioni finali.                                           ?
022300170328       //--------------------------------------------------------------
022400170328       begsr  sr_RoutEnd;
022500170328
022600170328         // chiudo il prtf perché si disponibile
022700170328         close UBLGOBBP;
022800170328
022900170328         // Uscita dal *pgm?
023000170328         eval *inrt=*on;
023100170328         return;
023200170328
023300170328       endsr;
