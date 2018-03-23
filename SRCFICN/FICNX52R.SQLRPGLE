000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX52R *---------------------------------------------------*
000300081110     H*                                                              *
000400111123     H* Controlla se ci sono delle Tariffe x il Futuro già inserite
000500081209     H*                                                              *
000600111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000700111122     H*  e se deve eseguire l'aggiornamento                          *
000800000000     H*--------------------------------------------------------------*
000900090401     Ffiapd01L  iF   E           K DISK
001000090401      *
001100090424     FwxFGT00S  o  a E             DISK
001200081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300081211      * -------------------------------------------------------------
001400111122     D KPJBA         E DS
001500081110     D dsFGT         E DS                  ExtName(FIFGT00F)
001600081110     d                                     prefix(STP:3)
001700081110     D FGT_REC         s              9s 0
001800090424     D STP_DATSCAD     s              8s 0
001900081230      *
002000111123     D esito           s              9s 0
002100111123     D esito_ALF       s              9a
002200090109      *
002300111123     D DATA_eur        S               D   DATFMT(*eur)
002400111123     D DATA_iso        S               D   DATFMT(*iso)
002500111123     C*------------------------------------------------------------------------*
002600111123     C*        - OPERAZIONI INIZIALI
002700111123     C*------------------------------------------------------------------------*
002800111123     C     *ENTRY        PLIST
002900111123     C                   PARM                    DATADEC           8
003000111123     C                   PARM                    AGGIORNA          1
003100111123     C                   PARM                    ESITO_ALF
003200111123     C*
003300111123     C     AGGIORNA      comp      ' '                                    44
003400111123     C                   move      datadec       DATA_dec          8 0
003500111123     C     *iso          movel     DATA_DEC      data_iso
003600111123     c     data_iso      subdur    1:*days       data_iso
003700111123     C     *iso          movel     data_iso      data_sca          8 0
003800111123     C                   z-add     data_sca      DATAscad          8 0
003900111123     C*
004000111123     c     Key_APD       KLIST
004100111123     c                   kfld                    apdTIP
004200111123     c                   kfld                    apdPDR
004300111123      *
004400111123     c                   clear                   esito
004500111123     c                   clear                   esito_ALF
004600111123     c                   time                    datetime         14 0
004700111123     c                   move      datetime      data              8 0
004800111123     c                   movel     datetime      ora               6 0
004900111123      *
005000111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
005100111123     C                   clear                   UDATE_8           8 0
005200111123     C                   MOVEL     *DATE         UDATE_8
005300111122     c                   clear                   savSTPpdr         7 0
005400081209     C                   SETON                                        LR
005500090423      **
005600090423     C*=================================================================
005700111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
005800090423     C*=================================================================
005900090424      *
006000090422     C/EXEC SQL DECLARE A1 CURSOR FOR
006100111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
006200111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
006300111123     C+        and fgtsml=' ' and fgtddt >= :DATA_dec
006400111213     C+               and  fgtPDR * 1000 + fgtPRG
006500111213     C+                          IN
006600111213     C+    (Select tgtpdr * 1000 + tgtprg from fiTGT00F
006700111213     C+       where TGTsml=' '  and  TGTPCAR > 0)
006800111122     C+   order by Fgtpdr
006900111122     C/END-EXEC
007000111122
007100111122     C*=====
007200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
007300111122     C*=====
007400081110     c                   if        sqlCODE < 0
007500081110     c                   eval      esito = Sqlcode
007600111123     c                   move      esito         esito_ALF
007700081110     c                   dump(a)
007800081110     c                   return
007900081110     c                   end
008000081110     C/EXEC SQL
008100081110     C+ OPEN A1
008200081110     C/END-EXEC
008300111122     C*=====
008400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008500111122     C*=====
008600081110     c                   if        sqlCODE < 0
008700081110     c                   eval      esito = Sqlcode
008800111123     c                   move      esito         esito_ALF
008900081110     c                   dump(a)
009000081110     c                   return
009100081110     c                   else
009200111122      *
009300081110     c                   except    testa
009400111122      *
009500081110     c                   end
009600090109      * >>>>>>>
009700081209      * resto nel ciclo finchè....
009800081110     C                   DOU       SqlCod < 0
009900081110      * lettura cursore
010000081110     C/EXEC SQL
010100111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
010200081110     C/END-EXEC
010300111122     C*=====
010400111122      *  EoF
010500081110     C                   if        SqlCod = 100
010600081110     c                   Leave
010700111122     C*=====
010800081110      *  x errori
010900081110     C                   ElseIf    SqlCod < 0
011000081110     c                   eval      esito = Sqlcode
011100111123     c                   move      esito         esito_ALF
011200081110     c                   dump(a)
011300081110     c                   leave
011400081110     C                   ENDif
011500090428      *
011600111123      *----------------------------->>>>>
011700111123      *  se sono qui ho letto un record correttamente
011800090428      *----------------------------->>>>>
011900111122      * Controlli su Anagrafica
012000111123     c                   clear                   errore           15
012100111122     c                   exsr      Ctrl_APD
012200090428      *
012300090428      *----------------------------->>>>>
012400111206      *    Segnala
012500111206     c                   if        Errore = *blank
012600111206     c                   Eval      Errore = 'FGT dec.FUTURA'
012700111206     c                   else
012800111206     c                   end
012900111206      *
013000090424      *  scrive il dettaglio su un file
013100111123     c   Of              except    testa
013200111123     c                   except    detta
013300111123      *
013400090424     c                   exsr      wri_DETTA
013500111206      *
013600111123     C                   z-add     *all'1'       esito
013700081110      *
013800081110     C                   ENDDO
013900081110     C/EXEC SQL
014000081110     C+ Close A1
014100081110     C/END-EXEC
014200111123     c                   except    finest
014300111123     C*
014400081110     C                   if        Esito < 0
014500111123     c                   move      esito         esito_ALF
014600081110     c                   return
014700081110     c                   endIF
014800081110     C*
014900111212     c**********         move      esito         esito_ALF
015000111206     c                   if        esito = 0
015100111206     c                   clear                   esito_ALF
015200111206     c                   end
015300111206     C*
015400081110     C     FINE          TAG
015500081110     c                   return
015600111122     C*---------------------------------------------------------------*
015700111122      *  Controlli sull'Anagrafica
015800111122      *---------------------------------------------------------------*
015900111122     c     Ctrl_APD      begSR
016000111122      *
016100111122      * rileva dall'anagrafica
016200111122     c                   eval      apdTIP = 'A'
016300111122     c                   eval      apdPDR = stpPDR
016400111122     c     Key_APD       chain     fiapd01l
016500111122     c                   if        %Found(fiapd01l)
016600111122      *
016700111122     c                   if        apdATB <> *blank
016800111123     c                   movel(p)  'ANNULLATO'   errore
016900111122     c                   else
017000111122      *
017100111122      *-  Esluso da autofatturazione
017200111122     c                   if        apdPDD = 'S'
017300111123     c                   movel(p)  'AUTOFAT'     errore
017400111122     c                   end
017500111122      *
017600111122     c                   endIF
017700111122      *
017800111122     c                   else
017900111123     c                   movel(p)  'NO_APD'      errore
018000111122     c                   end
018100111122      *
018200111122     c                   endSR
018300111122     C*---------------------------------------------------------------*
018400111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
018500111122      *---------------------------------------------------------------*
018600111122     c     WRI_DETTA     begSR
018700111122     C*
018800090424     c                   clear                   WSFGT000
018900090424     C*
019000090424     c                   eval      WSFPDR  =  stpPDR
019100090424     c                   eval      WSFTSR  =  stpTSR
019200090424     c                   eval      WSFCTR  =  stpCTR
019300090424     c                   eval      WSFPRG  = stpPRG
019400090424     c                   eval      WSFDDT  = stpDDT
019500090424     c                   eval      WSFDST  = stpDST
019600090424     c                   eval      WSFDTS  = stpDTS
019700090424     c                   eval      WSFDCN  = stpDCN
019800111129     c                   eval      WSFFILE = 'FIFGT00F'
019900090424     c                   eval      WSFREC  = FGT_REC
020000111216     c                   eval      WSFPROGR= 'FICNX52R'
020100111123     c                   eval      WSFtime = datetime
020200090428     c**
020300090428     c                   eval      WSFNOTE = Errore
020400090424     c**
020500090424     c                   write     WSFGT000
020600090424     C*
020700090424     c                   endSR
020800111123     C*---------------------------------------------------------------*
020900111123     OQSYSPRT   E            TESTA            02
021000111216     O                                           10 'FICNX52R'
021100111123     O                                           25 'Autista'
021200111123     O                                           30 'TSR'
021300111123     O                                           38 'Tariffa'
021400111123     O                                           45 'Progr'
021500111123     O                                           55 'Data_Dec'
021600111123     O                                           65 'Data_Sca'
021700111123     O                                           75 'Data_Stp'
021800111123     O                                           85 'Data_Cnv'
021900111123     O                                          100 'N_Rec_FGT'
022000111123     O                                          111 'N O T E'
022100111123     O                       data               132 '  /  /    '
022200111123     O          E            TESTA       1
022300111123     O                                            1 ' '
022400111123     O                                          + 0 'TARIFFE FUTURE'
022500111123     O                                           25 'da fare'
022600111123     O                                          + 2 'ATTENZIONE'
022700111123     O                       ora                130 '  :  :  '
022800111123      *
022900111123     O          E            DETta       1
023000111123     O                       stpPDR        z     25
023100111123     O                       stpTSR              29
023200111123     O                       stpCTR              36
023300111123     O                       stpPRG        z     43
023400111123     O                       stpDDT        z     55
023500111123     O                       stpDST        z     65
023600111123     O                       stpDTS        z     75
023700111123     O                       stpDCN        z     85
023800111123     o                       FGT_REC       z    100
023900111123     O                       errore             116
024000111123      ****
024100111123     o          E            FINEST      2
024200111123     o                                            1 ' '
024300111216     o                                          + 0 '*** FICNX52R ***'
024400111123     o                                          + 9 '***  TARIFFE FUTURE  ***'
024500111123     o                                          + 9 '***   FINE  STAMPA  ***'
024600111123     O                       data               + 9 '  /  /    '
024700111123     O                       ora                + 3 '  :  :  '
024800111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
