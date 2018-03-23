000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111123     H* TNVRA52R *---------------------------------------------------*
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
006400111122     C+   order by Fgtpdr
006500111122     C/END-EXEC
006600111122
006700111122     C*=====
006800081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
006900111122     C*=====
007000081110     c                   if        sqlCODE < 0
007100081110     c                   eval      esito = Sqlcode
007200111123     c                   move      esito         esito_ALF
007300081110     c                   dump(a)
007400081110     c                   return
007500081110     c                   end
007600081110     C/EXEC SQL
007700081110     C+ OPEN A1
007800081110     C/END-EXEC
007900111122     C*=====
008000081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008100111122     C*=====
008200081110     c                   if        sqlCODE < 0
008300081110     c                   eval      esito = Sqlcode
008400111123     c                   move      esito         esito_ALF
008500081110     c                   dump(a)
008600081110     c                   return
008700081110     c                   else
008800111122      *
008900081110     c                   except    testa
009000111122      *
009100081110     c                   end
009200090109      * >>>>>>>
009300081209      * resto nel ciclo finchè....
009400081110     C                   DOU       SqlCod < 0
009500081110      * lettura cursore
009600081110     C/EXEC SQL
009700111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
009800081110     C/END-EXEC
009900111122     C*=====
010000111122      *  EoF
010100081110     C                   if        SqlCod = 100
010200081110     c                   Leave
010300111122     C*=====
010400081110      *  x errori
010500081110     C                   ElseIf    SqlCod < 0
010600081110     c                   eval      esito = Sqlcode
010700111123     c                   move      esito         esito_ALF
010800081110     c                   dump(a)
010900081110     c                   leave
011000081110     C                   ENDif
011100090428      *
011200111123      *----------------------------->>>>>
011300111123      *  se sono qui ho letto un record correttamente
011400090428      *----------------------------->>>>>
011500111122      * Controlli su Anagrafica
011600111123     c                   clear                   errore           15
011700111122     c                   exsr      Ctrl_APD
011800090428      *
011900090428      *----------------------------->>>>>
012000090424      *  scrive il dettaglio su un file
012100111123     c   Of              except    testa
012200111123     c                   except    detta
012300111123      *
012400090424     c                   exsr      wri_DETTA
012500111123     C                   z-add     *all'1'       esito
012600111123     c                   move      esito         esito_ALF
012700081110      *
012800081110     C                   ENDDO
012900081110     C/EXEC SQL
013000081110     C+ Close A1
013100081110     C/END-EXEC
013200111123     c                   except    finest
013300111123     C*
013400081110     C                   if        Esito < 0
013500111123     c                   move      esito         esito_ALF
013600081110     c                   return
013700081110     c                   endIF
013800081110     C*
013900081110     C     FINE          TAG
014000081110     c                   return
014100111122     C*---------------------------------------------------------------*
014200111122      *  Controlli sull'Anagrafica
014300111122      *---------------------------------------------------------------*
014400111122     c     Ctrl_APD      begSR
014500111122      *
014600111122      * rileva dall'anagrafica
014700111122     c                   eval      apdTIP = 'A'
014800111122     c                   eval      apdPDR = stpPDR
014900111122     c     Key_APD       chain     fiapd01l
015000111122     c                   if        %Found(fiapd01l)
015100111122      *
015200111122     c                   if        apdATB <> *blank
015300111123     c                   movel(p)  'ANNULLATO'   errore
015400111122     c                   else
015500111122      *
015600111122      *-  Esluso da autofatturazione
015700111122     c                   if        apdPDD = 'S'
015800111123     c                   movel(p)  'AUTOFAT'     errore
015900111122     c                   end
016000111122      *
016100111122     c                   endIF
016200111122      *
016300111122     c                   else
016400111123     c                   movel(p)  'NO_APD'      errore
016500111122     c                   end
016600111122      *
016700111122     c                   endSR
016800111122     C*---------------------------------------------------------------*
016900111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
017000111122      *---------------------------------------------------------------*
017100111122     c     WRI_DETTA     begSR
017200111122     C*
017300090424     c                   clear                   WSFGT000
017400090424     C*
017500090424     c                   eval      WSFPDR  =  stpPDR
017600090424     c                   eval      WSFTSR  =  stpTSR
017700090424     c                   eval      WSFCTR  =  stpCTR
017800090424     c                   eval      WSFPRG  = stpPRG
017900090424     c                   eval      WSFDDT  = stpDDT
018000090424     c                   eval      WSFDST  = stpDST
018100090424     c                   eval      WSFDTS  = stpDTS
018200090424     c                   eval      WSFDCN  = stpDCN
018300111129     c                   eval      WSFFILE = 'FIFGT00F'
018400090424     c                   eval      WSFREC  = FGT_REC
018500111123     c                   eval      WSFPROGR= 'TNVRA52R'
018600111123     c                   eval      WSFtime = datetime
018700090428     c**
018800090428     c                   eval      WSFNOTE = Errore
018900090424     c**
019000090424     c                   write     WSFGT000
019100090424     C*
019200090424     c                   endSR
019300111123     C*---------------------------------------------------------------*
019400111123     OQSYSPRT   E            TESTA            02
019500111123     O                                           10 'TNVRA52R'
019600111123     O                                           25 'Autista'
019700111123     O                                           30 'TSR'
019800111123     O                                           38 'Tariffa'
019900111123     O                                           45 'Progr'
020000111123     O                                           55 'Data_Dec'
020100111123     O                                           65 'Data_Sca'
020200111123     O                                           75 'Data_Stp'
020300111123     O                                           85 'Data_Cnv'
020400111123     O                                          100 'N_Rec_FGT'
020500111123     O                                          111 'N O T E'
020600111123     O                       data               132 '  /  /    '
020700111123     O          E            TESTA       1
020800111123     O                                            1 ' '
020900111123     O                                          + 0 'TARIFFE FUTURE'
021000111123     O                                           25 'da fare'
021100111123     O                                          + 2 'ATTENZIONE'
021200111123     O                       ora                130 '  :  :  '
021300111123      *
021400111123     O          E            DETta       1
021500111123     O                       stpPDR        z     25
021600111123     O                       stpTSR              29
021700111123     O                       stpCTR              36
021800111123     O                       stpPRG        z     43
021900111123     O                       stpDDT        z     55
022000111123     O                       stpDST        z     65
022100111123     O                       stpDTS        z     75
022200111123     O                       stpDCN        z     85
022300111123     o                       FGT_REC       z    100
022400111123     O                       errore             116
022500111123      ****
022600111123     o          E            FINEST      2
022700111123     o                                            1 ' '
022800111123     o                                          + 0 '*** TNVRA52R ***'
022900111123     o                                          + 9 '***  TARIFFE FUTURE  ***'
023000111123     o                                          + 9 '***   FINE  STAMPA  ***'
023100111123     O                       data               + 9 '  /  /    '
023200111123     O                       ora                + 3 '  :  :  '
023300111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
