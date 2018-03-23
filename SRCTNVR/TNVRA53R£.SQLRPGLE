000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111122     H* TNVRA50R *---------------------------------------------------*
000300081110     H*                                                              *
000400111122     H* Duplica le tariffe in essere x la data decorrenza            *
000500111122     H*  facendole scadere il giorno prima.                          *
000600081209     H*                                                              *
000700111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000800111122     H*  e se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000111123     Ffifgt00f  iF   E             DISK    rename(FIFGT000:FGTFisico)
001100111123     Ffitgt01L  iF   E           K DISK
001200111122      *
001300090401     Ffiapd01L  iF   E           K DISK
001400090401      *
001500090424     FwxFGT00S  o  a E             DISK
001600081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001700081211      * -------------------------------------------------------------
001800111122     D KPJBA         E DS
001900081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002000081110     d                                     prefix(STP:3)
002100081110     D FGT_REC         s              9s 0
002200090424     D STP_DATSCAD     s              8s 0
002300081230      *
002400111122     D oldPRG          s                   like(tgtPrg)
002500111122     D newPRG          s                   like(tgtPrg)
002600111123     D esito           s              9s 0
002700111123     D esito_ALF       s              9a
002800090109      *
002900111123     D DATA_eur        S               D   DATFMT(*eur)
003000111123     D DATA_iso        S               D   DATFMT(*iso)
003100111123      *
003200111122     D ficn06ds      E DS
003300111123     C*------------------------------------------------------------------------*
003400111123     C*        - OPERAZIONI INIZIALI
003500111123     C*------------------------------------------------------------------------*
003600111123     C     *ENTRY        PLIST
003700111123     C                   PARM                    DATADEC           8
003800111123     C                   PARM                    AGGIORNA          1
003900111123     C                   PARM                    ESITO_ALF
004000111123     C*
004100111123     C     AGGIORNA      comp      ' '                                    44
004200111123     C                   move      datadec       DATA_dec          8 0
004300111123     C     *iso          movel     DATA_DEC      data_iso
004400111123     c     data_iso      subdur    1:*days       data_iso
004500111123     C     *iso          movel     data_iso      data_sca          8 0
004600111123     C                   z-add     data_sca      DATAscad          8 0
004700111123     C*
004800111123      * duplica la testata TGT
004900111123     C     tgt01key      kLIST
005000111123     C                   kfld                    stpPDR
005100111123     C                   kfld                    stpSML
005200111123     C                   kfld                    stpPRG
005300111123     C*
005400111123     c     Key_APD       KLIST
005500111123     c                   kfld                    apdTIP
005600111123     c                   kfld                    apdPDR
005700111123      *
005800111123     c                   clear                   esito
005900111123     c                   clear                   esito_ALF
006000111123     c                   time                    datetime         14 0
006100111123     c                   move      datetime      data              8 0
006200111123     c                   movel     datetime      ora               6 0
006300111123      *
006400111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
006500111123     C                   clear                   UDATE_8           8 0
006600111123     C                   MOVEL     *DATE         UDATE_8
006700111122     c                   clear                   savSTPpdr         7 0
006800081209     C                   SETON                                        LR
006900090423      **
007000090423     C*=================================================================
007100111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007200090423     C*=================================================================
007300090424      *
007400090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007500111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
007600111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
007700111123     C+        and fgtsml=' ' and fgtddt >= :DATA_dec
007800111122     C+   order by Fgtpdr
007900111122     C/END-EXEC
008000111122
008100111122     C*=====
008200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008300111122     C*=====
008400081110     c                   if        sqlCODE < 0
008500081110     c                   eval      esito = Sqlcode
008600111123     c                   move      esito         esito_ALF
008700081110     c                   dump(a)
008800081110     c                   return
008900081110     c                   end
009000081110     C/EXEC SQL
009100081110     C+ OPEN A1
009200081110     C/END-EXEC
009300111122     C*=====
009400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009500111122     C*=====
009600081110     c                   if        sqlCODE < 0
009700081110     c                   eval      esito = Sqlcode
009800111123     c                   move      esito         esito_ALF
009900081110     c                   dump(a)
010000081110     c                   return
010100081110     c                   else
010200111122      *
010300081110     c                   except    testa
010400111122      *
010500081110     c                   end
010600090109      * >>>>>>>
010700081209      * resto nel ciclo finchè....
010800081110     C                   DOU       SqlCod < 0
010900081110      * lettura cursore
011000081110     C/EXEC SQL
011100111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
011200081110     C/END-EXEC
011300111122     C*=====
011400111122      *  EoF
011500081110     C                   if        SqlCod = 100
011600081110     c                   Leave
011700111122     C*=====
011800081110      *  x errori
011900081110     C                   ElseIf    SqlCod < 0
012000081110     c                   eval      esito = Sqlcode
012100111123     c                   move      esito         esito_ALF
012200081110     c                   dump(a)
012300081110     c                   leave
012400081110     C                   ENDif
012500090428      *
012600111123      *----------------------------->>>>>
012700111123      *  se sono qui ho letto un record correttamente
012800090428      *----------------------------->>>>>
012900111122      * Controlli su Anagrafica
013000111123     c                   clear                   errore           15
013100111122     c                   exsr      Ctrl_APD
013200090428      *
013300090428      *----------------------------->>>>>
013400090427     c   Of              except    testa
013500090427     c                   except    detta
013600090428      *
013700090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
013800111123      *----------------------------->>>>>
013900090428     c                   if        Errore  = *blank
014000090428      *----------------------------->>>>>
014100081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
014200081110     c                   if        Aggiorna = 'S'
014300111122      *  Deve scrivere la testata a rottura di autista
014400111123      *-+++++++++++++++--
014500111122     c                   if        stpPDR <> savSTPpdr
014600111123      *-+++++++++++++++--
014700111122      *  salva chiave di rottura
014800111122     c                   eval              savSTPpdr = stpPDR
014900111122      *   Aggancia la testata
015000111122     C     tgt01key      chain     fiTGT01L
015100111122     c                   if        %Found(fiTGT01L)
015200111122      *
015300111122     c                   end
015400111123      *-+++++++++++++++--
015500111122     c                   endIF
015600111122      *-+++++++++++++++--
015700111122      *   deve far scadere il record dell'FGT
015800081218     c     fgt_REC       chain     FIFGT00F
015900081218     c                   if        %Found(FIFGT00F)
016000111123      *
016100090423     c                   end
016200090112      *
016300090423     c                   endIF
016400090428      *----------------------------->>>>>
016500090428     c                   end
016600090428      *----------------------------->>>>>
016700090424      *  scrive il dettaglio su un file
016800090424     c                   exsr      wri_DETTA
016900111123     C                   z-add     *all'1'       esito
017000111123     c                   move      esito         esito_ALF
017100081110      *
017200081110     C                   ENDDO
017300081110     C/EXEC SQL
017400081110     C+ Close A1
017500081110     C/END-EXEC
017600111123     c                   except    finest
017700111123     C*
017800081110     C                   if        Esito < 0
017900111123     c                   move      esito         esito_ALF
018000081110     c                   return
018100081110     c                   endIF
018200081110     C*
018300081110     C     FINE          TAG
018400081110     c                   return
018500111122     C*---------------------------------------------------------------*
018600111122      *  Controlli sull'Anagrafica
018700111122      *---------------------------------------------------------------*
018800111122     c     Ctrl_APD      begSR
018900111122      *
019000111122      * rileva dall'anagrafica
019100111122     c                   eval      apdTIP = 'A'
019200111122     c                   eval      apdPDR = stpPDR
019300111122     c     Key_APD       chain     fiapd01l
019400111122     c                   if        %Found(fiapd01l)
019500111122      *
019600111122     c                   if        apdATB <> *blank
019700111123     c                   movel(p)  'ANNULLATO'   errore
019800111122     c                   else
019900111122      *
020000111122      *-  Esluso da autofatturazione
020100111122     c                   if        apdPDD = 'S'
020200111123     c                   movel(p)  'AUTOFAT'     errore
020300111122     c                   end
020400111122      *
020500111122     c                   endIF
020600111122      *
020700111122     c                   else
020800111123     c                   movel(p)  'NO_APD'      errore
020900111122     c                   end
021000111122      *
021100111122     c                   endSR
021200111122     C*---------------------------------------------------------------*
021300111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
021400111122      *---------------------------------------------------------------*
021500111122     c     WRI_DETTA     begSR
021600111122     C*
021700090424     c                   clear                   WSFGT000
021800090424     C*
021900090424     c                   eval      WSFPDR  =  stpPDR
022000090424     c                   eval      WSFTSR  =  stpTSR
022100090424     c                   eval      WSFCTR  =  stpCTR
022200090424     c                   eval      WSFPRG  = stpPRG
022300090424     c                   eval      WSFDDT  = stpDDT
022400090424     c                   eval      WSFDST  = stpDST
022500090424     c                   eval      WSFDTS  = stpDTS
022600090424     c                   eval      WSFDCN  = stpDCN
022700090424     c                   eval      WSFREC  = FGT_REC
022800111123     c                   eval      WSFPROGR= 'TNVRA52R'
022900111123     c                   eval      WSFtime = datetime
023000090428     c**
023100090428     c                   eval      WSFNOTE = Errore
023200090424     c**
023300090424     c                   write     WSFGT000
023400090424     C*
023500090424     c                   endSR
023600111123     C*---------------------------------------------------------------*
023700111123     OQSYSPRT   E            TESTA            02
023800111123     O                                           10 'TNVRA52R'
023900111123     O                                           25 'Autista'
024000111123     O                                           30 'TSR'
024100111123     O                                           38 'Tariffa'
024200111123     O                                           45 'Progr'
024300111123     O                                           55 'Data_Dec'
024400111123     O                                           65 'Data_Sca'
024500111123     O                                           75 'Data_Stp'
024600111123     O                                           85 'Data_Cnv'
024700111123     O                                          100 'N_Rec_FGT'
024800111123     O                                          111 'N O T E'
024900111123     O                       data               132 '  /  /    '
025000111123     O          E            TESTA       1
025100111123     O                                            1 ' '
025200111123     O                                          + 0 'TARIFFE FUTURE'
025300111123     O                                           25 'da fare'
025400111123     O                                          + 2 'ATTENZIONE'
025500111123     O                       ora                130 '  :  :  '
025600111123      *
025700111123     O          E            DETta       1
025800111123     O                       stpPDR        z     25
025900111123     O                       stpTSR              29
026000111123     O                       stpCTR              36
026100111123     O                       stpPRG        z     43
026200111123     O                       stpDDT        z     55
026300111123     O                       stpDST        z     65
026400111123     O                       stpDTS        z     75
026500111123     O                       stpDCN        z     85
026600111123     o                       FGT_REC       z    100
026700111123     O                       errore             116
026800111123      ****
026900111123     o          E            FINEST      2
027000111123     o                                            1 ' '
027100111123     o                                          + 0 '*** TNVRA52R ***'
027200111123     o                                          + 9 '***  TARIFFE FUTURE  ***'
027300111123     o                                          + 9 '***   FINE  STAMPA  ***'
027400111123     O                       data               + 9 '  /  /    '
027500111123     O                       ora                + 3 '  :  :  '
027600111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
