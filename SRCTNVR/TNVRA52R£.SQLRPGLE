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
002700090109      *
002800111123     D DATA_eur        S               D   DATFMT(*eur)
002900111123     D DATA_iso        S               D   DATFMT(*iso)
003000111123      *
003100111122     D ficn06ds      E DS
003200111123     C*------------------------------------------------------------------------*
003300111123     C*        - OPERAZIONI INIZIALI
003400111123     C*------------------------------------------------------------------------*
003500111123     C     *ENTRY        PLIST
003600111123     C                   PARM                    DATADEC           8
003700111123     C                   PARM                    AGGIORNA          1
003800111123     C                   PARM                    ESITO
003900111123     C*
004000111123     C     AGGIORNA      comp      ' '                                    44
004100111123     C                   move      datadec       DATA_dec          8 0
004200111123     C     *iso          movel     DATA_DEC      data_iso
004300111123     c     data_iso      subdur    1:*days       data_iso
004400111123     C     *iso          movel     data_iso      data_sca          8 0
004500111123     C                   z-add     data_sca      DATAscad          8 0
004600111123     C*
004700111123      * duplica la testata TGT
004800111123     C     tgt01key      kLIST
004900111123     C                   kfld                    stpPDR
005000111123     C                   kfld                    stpSML
005100111123     C                   kfld                    stpPRG
005200111123     C*
005300111123     c     Key_APD       KLIST
005400111123     c                   kfld                    apdTIP
005500111123     c                   kfld                    apdPDR
005600111123      *
005700111123     c                   clear                   esito
005800111123     c                   time                    datetime         14 0
005900111123     c                   move      datetime      data              8 0
006000111123     c                   movel     datetime      ora               6 0
006100111123      *
006200111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
006300111123     C                   clear                   UDATE_8           8 0
006400111123     C                   MOVEL     *DATE         UDATE_8
006500111122     c                   clear                   savSTPpdr         7 0
006600081209     C                   SETON                                        LR
006700090423      **
006800090423     C*=================================================================
006900111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007000090423     C*=================================================================
007100090424      *
007200090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007300111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
007400111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
007500111123     C+        and fgtsml=' ' and fgtddt >= :DATA_dec
007600111122     C+   order by Fgtpdr
007700111122     C/END-EXEC
007800111122
007900111122     C*=====
008000081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008100111122     C*=====
008200081110     c                   if        sqlCODE < 0
008300081110     c                   eval      esito = Sqlcode
008400081110     c                   dump(a)
008500081110     c                   return
008600081110     c                   end
008700081110     C/EXEC SQL
008800081110     C+ OPEN A1
008900081110     C/END-EXEC
009000111122     C*=====
009100081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009200111122     C*=====
009300081110     c                   if        sqlCODE < 0
009400081110     c                   eval      esito = Sqlcode
009500081110     c                   dump(a)
009600081110     c                   return
009700081110     c                   else
009800111122      *
009900081110     c                   except    testa
010000111122      *
010100081110     c                   end
010200090109      * >>>>>>>
010300081209      * resto nel ciclo finchè....
010400081110     C                   DOU       SqlCod < 0
010500081110      * lettura cursore
010600081110     C/EXEC SQL
010700111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
010800081110     C/END-EXEC
010900111122     C*=====
011000111122      *  EoF
011100081110     C                   if        SqlCod = 100
011200081110     c                   Leave
011300111122     C*=====
011400081110      *  x errori
011500081110     C                   ElseIf    SqlCod < 0
011600081110     c                   eval      esito = Sqlcode
011700081110     c                   dump(a)
011800081110     c                   leave
011900081110     C                   ENDif
012000090428      *
012100111123      *----------------------------->>>>>
012200111123      *  se sono qui ho letto un record correttamente
012300090428      *----------------------------->>>>>
012400111122      * Controlli su Anagrafica
012500111123     c                   clear                   errore           15
012600111122     c                   exsr      Ctrl_APD
012700090428      *
012800090428      *----------------------------->>>>>
012900090427     c   Of              except    testa
013000090427     c                   except    detta
013100090428      *
013200090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
013300111123      *----------------------------->>>>>
013400090428     c                   if        Errore  = *blank
013500090428      *----------------------------->>>>>
013600081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
013700081110     c                   if        Aggiorna = 'S'
013800111122      *  Deve scrivere la testata a rottura di autista
013900111123      *-+++++++++++++++--
014000111122     c                   if        stpPDR <> savSTPpdr
014100111123      *-+++++++++++++++--
014200111122      *  salva chiave di rottura
014300111122     c                   eval              savSTPpdr = stpPDR
014400111122      *   Aggancia la testata
014500111122     C     tgt01key      chain     fiTGT01L
014600111122     c                   if        %Found(fiTGT01L)
014700111122      *
014800111122     c                   end
014900111123      *-+++++++++++++++--
015000111122     c                   endIF
015100111122      *-+++++++++++++++--
015200111122      *   deve far scadere il record dell'FGT
015300081218     c     fgt_REC       chain     FIFGT00F
015400081218     c                   if        %Found(FIFGT00F)
015500111123      *
015600090423     c                   end
015700090112      *
015800090423     c                   endIF
015900090428      *----------------------------->>>>>
016000090428     c                   end
016100090428      *----------------------------->>>>>
016200090424      *  scrive il dettaglio su un file
016300090424     c                   exsr      wri_DETTA
016400111123     C                   z-add     *all'1'       esito
016500081110      *
016600081110     C                   ENDDO
016700081110     C/EXEC SQL
016800081110     C+ Close A1
016900081110     C/END-EXEC
017000111123     c                   except    finest
017100111123     C*
017200081110     C                   if        Esito < 0
017300081110     c                   return
017400081110     c                   endIF
017500081110     C*
017600081110     C     FINE          TAG
017700081110     c                   return
017800111122     C*---------------------------------------------------------------*
017900111122      *  Controlli sull'Anagrafica
018000111122      *---------------------------------------------------------------*
018100111122     c     Ctrl_APD      begSR
018200111122      *
018300111122      * rileva dall'anagrafica
018400111122     c                   eval      apdTIP = 'A'
018500111122     c                   eval      apdPDR = stpPDR
018600111122     c     Key_APD       chain     fiapd01l
018700111122     c                   if        %Found(fiapd01l)
018800111122      *
018900111122     c                   if        apdATB <> *blank
019000111123     c                   movel(p)  'ANNULLATO'   errore
019100111122     c                   else
019200111122      *
019300111122      *-  Esluso da autofatturazione
019400111122     c                   if        apdPDD = 'S'
019500111123     c                   movel(p)  'AUTOFAT'     errore
019600111122     c                   end
019700111122      *
019800111122     c                   endIF
019900111122      *
020000111122     c                   else
020100111123     c                   movel(p)  'NO_APD'      errore
020200111122     c                   end
020300111122      *
020400111122     c                   endSR
020500111122     C*---------------------------------------------------------------*
020600111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
020700111122      *---------------------------------------------------------------*
020800111122     c     WRI_DETTA     begSR
020900111122     C*
021000090424     c                   clear                   WSFGT000
021100090424     C*
021200090424     c                   eval      WSFPDR  =  stpPDR
021300090424     c                   eval      WSFTSR  =  stpTSR
021400090424     c                   eval      WSFCTR  =  stpCTR
021500090424     c                   eval      WSFPRG  = stpPRG
021600090424     c                   eval      WSFDDT  = stpDDT
021700090424     c                   eval      WSFDST  = stpDST
021800090424     c                   eval      WSFDTS  = stpDTS
021900090424     c                   eval      WSFDCN  = stpDCN
022000090424     c                   eval      WSFREC  = FGT_REC
022100111123     c                   eval      WSFPROGR= 'TNVRA52R'
022200111123     c                   eval      WSFtime = datetime
022300090428     c**
022400090428     c                   eval      WSFNOTE = Errore
022500090424     c**
022600090424     c                   write     WSFGT000
022700090424     C*
022800090424     c                   endSR
022900111123     C*---------------------------------------------------------------*
023000111123     OQSYSPRT   E            TESTA            02
023100111123     O                                           10 'TNVRA52R'
023200111123     O                                           25 'Autista'
023300111123     O                                           30 'TSR'
023400111123     O                                           38 'Tariffa'
023500111123     O                                           45 'Progr'
023600111123     O                                           55 'Data_Dec'
023700111123     O                                           65 'Data_Sca'
023800111123     O                                           75 'Data_Stp'
023900111123     O                                           85 'Data_Cnv'
024000111123     O                                          100 'N_Rec_FGT'
024100111123     O                                          111 'N O T E'
024200111123     O                       data               132 '  /  /    '
024300111123     O          E            TESTA       1
024400111123     O                                            1 ' '
024500111123     O                                          + 0 'TARIFFE FUTURE'
024600111123     O                                           25 'da fare'
024700111123     O                                          + 2 'ATTENZIONE'
024800111123     O                       ora                130 '  :  :  '
024900111123      *
025000111123     O          E            DETta       1
025100111123     O                       stpPDR        z     25
025200111123     O                       stpTSR              29
025300111123     O                       stpCTR              36
025400111123     O                       stpPRG        z     43
025500111123     O                       stpDDT        z     55
025600111123     O                       stpDST        z     65
025700111123     O                       stpDTS        z     75
025800111123     O                       stpDCN        z     85
025900111123     o                       FGT_REC       z    100
026000111123     O                       errore             116
026100111123      ****
026200111123     o          E            FINEST      2
026300111123     o                                            1 ' '
026400111123     o                                          + 0 '*** TNVRA52R ***'
026500111123     o                                          + 9 '***  TARIFFE FUTURE  ***'
026600111123     o                                          + 9 '***   FINE  STAMPA  ***'
026700111123     O                       data               + 9 '  /  /    '
026800111123     O                       ora                + 3 '  :  :  '
026900111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
