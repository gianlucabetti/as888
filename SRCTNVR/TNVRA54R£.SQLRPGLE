000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111123     H* TNVRA52R *---------------------------------------------------*
000300081110     H*                                                              *
000400111129     H* Controlla se ci sono delle Testate x il Futuro già inserite
000500111129     H*  senza relativi FGT                                          *
000600081209     H*                                                              *
000700111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000800111122     H*  e se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000090401     Ffiapd01L  iF   E           K DISK
001100090401      *
001200090424     FwxFGT00S  o  a E             DISK
001300081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400081211      * -------------------------------------------------------------
001500111122     D KPJBA         E DS
001600111129     D dsTGT         E DS                  ExtName(FITGT00F)
001700081110     d                                     prefix(STP:3)
001800111129     D TGT_REC         s              9s 0
001900090424     D STP_DATSCAD     s              8s 0
002000081230      *
002100111123     D esito           s              9s 0
002200111123     D esito_ALF       s              9a
002300090109      *
002400111123     D DATA_eur        S               D   DATFMT(*eur)
002500111123     D DATA_iso        S               D   DATFMT(*iso)
002600111123     C*------------------------------------------------------------------------*
002700111123     C*        - OPERAZIONI INIZIALI
002800111123     C*------------------------------------------------------------------------*
002900111123     C     *ENTRY        PLIST
003000111123     C                   PARM                    DATADEC           8
003100111123     C                   PARM                    AGGIORNA          1
003200111123     C                   PARM                    ESITO_ALF
003300111123     C*
003400111123     C     AGGIORNA      comp      ' '                                    44
003500111123     C                   move      datadec       DATA_dec          8 0
003600111123     C     *iso          movel     DATA_DEC      data_iso
003700111123     c     data_iso      subdur    1:*days       data_iso
003800111123     C     *iso          movel     data_iso      data_sca          8 0
003900111123     C                   z-add     data_sca      DATAscad          8 0
004000111123     C*
004100111123     c     Key_APD       KLIST
004200111123     c                   kfld                    apdTIP
004300111123     c                   kfld                    apdPDR
004400111123      *
004500111123     c                   clear                   esito
004600111123     c                   clear                   esito_ALF
004700111123     c                   time                    datetime         14 0
004800111123     c                   move      datetime      data              8 0
004900111123     c                   movel     datetime      ora               6 0
005000111123      *
005100111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
005200111123     C                   clear                   UDATE_8           8 0
005300111123     C                   MOVEL     *DATE         UDATE_8
005400111122     c                   clear                   savSTPpdr         7 0
005500081209     C                   SETON                                        LR
005600090423      **
005700090423     C*=================================================================
005800111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
005900090423     C*=================================================================
006000090424      *
006100090422     C/EXEC SQL DECLARE A1 CURSOR FOR
006200111129     C+   select rrn(fitgt00f), fitgt00f.* FROM
006300111129     C+    fitgt00f WHERE
006400111129     C+    tgtsml=' ' and tgtddt >= :DATA_dec
006500111129     C+   order by tgtpdr
006600111122     C/END-EXEC
006700111122
006800111122     C*=====
006900081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
007000111122     C*=====
007100081110     c                   if        sqlCODE < 0
007200081110     c                   eval      esito = Sqlcode
007300111123     c                   move      esito         esito_ALF
007400081110     c                   dump(a)
007500081110     c                   return
007600081110     c                   end
007700081110     C/EXEC SQL
007800081110     C+ OPEN A1
007900081110     C/END-EXEC
008000111122     C*=====
008100081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008200111122     C*=====
008300081110     c                   if        sqlCODE < 0
008400081110     c                   eval      esito = Sqlcode
008500111123     c                   move      esito         esito_ALF
008600081110     c                   dump(a)
008700081110     c                   return
008800081110     c                   else
008900111122      *
009000081110     c                   except    testa
009100111122      *
009200081110     c                   end
009300090109      * >>>>>>>
009400081209      * resto nel ciclo finchè....
009500081110     C                   DOU       SqlCod < 0
009600081110      * lettura cursore
009700081110     C/EXEC SQL
009800111129     C+ Fetch Next From A1 Into :TGT_REC, :dsTGT
009900081110     C/END-EXEC
010000111122     C*=====
010100111122      *  EoF
010200081110     C                   if        SqlCod = 100
010300081110     c                   Leave
010400111122     C*=====
010500081110      *  x errori
010600081110     C                   ElseIf    SqlCod < 0
010700081110     c                   eval      esito = Sqlcode
010800111123     c                   move      esito         esito_ALF
010900081110     c                   dump(a)
011000081110     c                   leave
011100081110     C                   ENDif
011200090428      *
011300111123      *----------------------------->>>>>
011400111123      *  se sono qui ho letto un record correttamente
011500090428      *----------------------------->>>>>
011600111122      * Controlli su Anagrafica
011700111123     c                   clear                   errore           15
011800111122     c                   exsr      Ctrl_APD
011900090428      *
012000090428      *----------------------------->>>>>
012100090424      *  scrive il dettaglio su un file
012200111123     c   Of              except    testa
012300111123     c                   except    detta
012400111123      *
012500090424     c                   exsr      wri_DETTA
012600111123     C                   z-add     *all'1'       esito
012700111123     c                   move      esito         esito_ALF
012800081110      *
012900081110     C                   ENDDO
013000081110     C/EXEC SQL
013100081110     C+ Close A1
013200081110     C/END-EXEC
013300111123     c                   except    finest
013400111123     C*
013500081110     C                   if        Esito < 0
013600111123     c                   move      esito         esito_ALF
013700081110     c                   return
013800081110     c                   endIF
013900081110     C*
014000081110     C     FINE          TAG
014100081110     c                   return
014200111122     C*---------------------------------------------------------------*
014300111122      *  Controlli sull'Anagrafica
014400111122      *---------------------------------------------------------------*
014500111122     c     Ctrl_APD      begSR
014600111122      *
014700111122      * rileva dall'anagrafica
014800111122     c                   eval      apdTIP = 'A'
014900111122     c                   eval      apdPDR = stpPDR
015000111122     c     Key_APD       chain     fiapd01l
015100111122     c                   if        %Found(fiapd01l)
015200111122      *
015300111122     c                   if        apdATB <> *blank
015400111123     c                   movel(p)  'ANNULLATO'   errore
015500111122     c                   else
015600111122      *
015700111122      *-  Esluso da autofatturazione
015800111122     c                   if        apdPDD = 'S'
015900111123     c                   movel(p)  'AUTOFAT'     errore
016000111122     c                   end
016100111122      *
016200111122     c                   endIF
016300111122      *
016400111122     c                   else
016500111123     c                   movel(p)  'NO_APD'      errore
016600111122     c                   end
016700111122      *
016800111122     c                   endSR
016900111122     C*---------------------------------------------------------------*
017000111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
017100111122      *---------------------------------------------------------------*
017200111122     c     WRI_DETTA     begSR
017300111122     C*
017400090424     c                   clear                   WSFGT000
017500090424     C*
017600090424     c                   eval      WSFPDR  =  stpPDR
017700111129     c*******            eval      WSFTSR  =  stpTSR
017800111129     c*******            eval      WSFCTR  =  stpCTR
017900090424     c                   eval      WSFPRG  = stpPRG
018000090424     c                   eval      WSFDDT  = stpDDT
018100090424     c                   eval      WSFDST  = stpDST
018200090424     c                   eval      WSFDTS  = stpDTS
018300090424     c                   eval      WSFDCN  = stpDCN
018400111129     c                   eval      WSFREC  = TGT_REC
018500111129     c                   eval      WSFPROGR= 'TNVRA54R'
018600111123     c                   eval      WSFtime = datetime
018700090428     c**
018800111129     c                   eval      WSFNOTE = 'TESTATE FUTURE'
018900090424     c**
019000090424     c                   write     WSFGT000
019100090424     C*
019200090424     c                   endSR
019300111123     C*---------------------------------------------------------------*
019400111123     OQSYSPRT   E            TESTA            02
019500111129     O                                           10 'TNVRA54R'
019600111123     O                                           25 'Autista'
019700111129     O                                           30 ' '
019800111129     O                                           38 ' '
019900111123     O                                           45 'Progr'
020000111123     O                                           55 'Data_Dec'
020100111123     O                                           65 'Data_Sca'
020200111123     O                                           75 'Data_Stp'
020300111123     O                                           85 'Data_Cnv'
020400111129     O                                          100 'N_Rec_TGT'
020500111123     O                                          111 'N O T E'
020600111123     O                       data               132 '  /  /    '
020700111123     O          E            TESTA       1
020800111123     O                                            1 ' '
020900111129     O                                          + 0 'TESTATE FUTURE'
021000111123     O                                           25 'da fare'
021100111123     O                                          + 2 'ATTENZIONE'
021200111123     O                       ora                130 '  :  :  '
021300111123      *
021400111123     O          E            DETta       1
021500111123     O                       stpPDR        z     25
021600111129     O*******                stpTSR              29
021700111129     O*******                stpCTR              36
021800111123     O                       stpPRG        z     43
021900111123     O                       stpDDT        z     55
022000111123     O                       stpDST        z     65
022100111123     O                       stpDTS        z     75
022200111123     O                       stpDCN        z     85
022300111129     o                       TGT_REC       z    100
022400111123     O                       errore             116
022500111123      ****
022600111123     o          E            FINEST      2
022700111123     o                                            1 ' '
022800111129     o                                          + 0 '*** TNVRA54R ***'
022900111129     o                                          + 9 '***  TESTATE FUTURE  ***'
023000111123     o                                          + 9 '***   FINE  STAMPA  ***'
023100111123     O                       data               + 9 '  /  /    '
023200111123     O                       ora                + 3 '  :  :  '
023300111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
