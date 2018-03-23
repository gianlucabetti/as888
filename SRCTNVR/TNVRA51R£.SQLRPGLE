000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111123     H* TNVRA51R *---------------------------------------------------*
000300081110     H*                                                              *
000400111122     H* Duplica le tariffe in essere x la data decorrenza            *
000500111122     H*  facendole scadere il giorno prima.                          *
000600081209     H*                                                              *
000700111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000800111122     H*  e se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000111122     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
001100111122     Ffitgt01L  uF a E           K DISK
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
007500111123     C+        and fgtsml=' ' and fgtdst > :DATAscad and Fgtpdr
007600111123     C+                   NOT in
007700111123     C+ (Select traKAU from Aitra00f where tradfi=0 and traann=' ')
007800111122     C+   order by Fgtpdr
007900111122     C/END-EXEC
008000111122
008100111122     C*=====
008200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008300111122     C*=====
008400081110     c                   if        sqlCODE < 0
008500081110     c                   eval      esito = Sqlcode
008600081110     c                   dump(a)
008700081110     c                   return
008800081110     c                   end
008900081110     C/EXEC SQL
009000081110     C+ OPEN A1
009100081110     C/END-EXEC
009200111122     C*=====
009300081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009400111122     C*=====
009500081110     c                   if        sqlCODE < 0
009600081110     c                   eval      esito = Sqlcode
009700081110     c                   dump(a)
009800081110     c                   return
009900081110     c                   else
010000111122      *
010100081110     c                   except    testa
010200111122      *
010300081110     c                   end
010400090109      * >>>>>>>
010500081209      * resto nel ciclo finchè....
010600081110     C                   DOU       SqlCod < 0
010700081110      * lettura cursore
010800081110     C/EXEC SQL
010900111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
011000081110     C/END-EXEC
011100111122     C*=====
011200111122      *  EoF
011300081110     C                   if        SqlCod = 100
011400081110     c                   Leave
011500111122     C*=====
011600081110      *  x errori
011700081110     C                   ElseIf    SqlCod < 0
011800081110     c                   eval      esito = Sqlcode
011900081110     c                   dump(a)
012000081110     c                   leave
012100081110     C                   ENDif
012200090428      *
012300111123      *----------------------------->>>>>
012400111123      *  se sono qui ho letto un record correttamente
012500090428      *----------------------------->>>>>
012600111122      * Controlli su Anagrafica
012700111123     c                   clear                   errore           15
012800111122     c                   exsr      Ctrl_APD
012900090428      *
013000090428      *----------------------------->>>>>
013100090427     c   Of              except    testa
013200090427     c                   except    detta
013300090428      *
013400090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
013500111123      *----------------------------->>>>>
013600090428     c                   if        Errore  = *blank
013700090428      *----------------------------->>>>>
013800081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
013900081110     c                   if        Aggiorna = 'S'
014000111122      *  Deve scrivere la testata a rottura di autista
014100111123      *-+++++++++++++++--
014200111122     c                   if        stpPDR <> savSTPpdr
014300111123      *-+++++++++++++++--
014400111122      *  salva chiave di rottura
014500111122     c                   eval              savSTPpdr = stpPDR
014600111122      *   Aggancia la testata
014700111122     C     tgt01key      chain     fiTGT01L
014800111122     c                   if        %Found(fiTGT01L)
014900111122      *   Mette in SCADENZA
015000111122     c                   z-add     DATA_sca      tgtDST
015100111122     c                   eval        oldPRG = tgtPRG
015200111122     c                   update    fiTGT000
015300111122     c                   end
015400111123      *-+++++++++++++++--
015500111122     c                   endIF
015600111122      *-+++++++++++++++--
015700111122      *   deve far scadere il record dell'FGT
015800081218     c     fgt_REC       chain     FIFGT00F
015900081218     c                   if        %Found(FIFGT00F)
016000111122      * aggiorna
016100111123     c                   eval      fgtDST = DATA_sca
016200111122     c                   update    fgtFisico
016300090423     c                   end
016400090112      *
016500090423     c                   endIF
016600090428      *----------------------------->>>>>
016700090428     c                   end
016800090428      *----------------------------->>>>>
016900090424      *  scrive il dettaglio su un file
017000090424     c                   exsr      wri_DETTA
017100081110      *
017200081110     C                   ENDDO
017300081110     C/EXEC SQL
017400081110     C+ Close A1
017500081110     C/END-EXEC
017600081110     C                   if        Esito < 0
017700081110     c                   return
017800081110     c                   endIF
017900081110     C*
018000081110     C     FINE          TAG
018100081110     c                   except    finest
018200081110     c                   return
018300111122     C*---------------------------------------------------------------*
018400111122      *  Controlli sull'Anagrafica
018500111122      *---------------------------------------------------------------*
018600111122     c     Ctrl_APD      begSR
018700111122      *
018800111122      * rileva dall'anagrafica
018900111122     c                   eval      apdTIP = 'A'
019000111122     c                   eval      apdPDR = stpPDR
019100111122     c     Key_APD       chain     fiapd01l
019200111122     c                   if        %Found(fiapd01l)
019300111122      *
019400111122     c                   if        apdATB <> *blank
019500111123     c                   movel(p)  'ANNULLATO'   errore
019600111122     c                   else
019700111122      *
019800111122      *-  Esluso da autofatturazione
019900111122     c                   if        apdPDD = 'S'
020000111123     c                   movel(p)  'AUTOFAT'     errore
020100111122     c                   end
020200111122      *
020300111122     c                   endIF
020400111122      *
020500111122     c                   else
020600111123     c                   movel(p)  'NO_APD'      errore
020700111122     c                   end
020800111122      *
020900111122     c                   endSR
021000111122     C*---------------------------------------------------------------*
021100111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
021200111122      *---------------------------------------------------------------*
021300111122     c     WRI_DETTA     begSR
021400111122     C*
021500090424     c                   clear                   WSFGT000
021600090424     C*
021700090424     c                   eval      WSFPDR  =  stpPDR
021800090424     c                   eval      WSFTSR  =  stpTSR
021900090424     c                   eval      WSFCTR  =  stpCTR
022000090424     c                   eval      WSFPRG  = stpPRG
022100090424     c                   eval      WSFDDT  = stpDDT
022200090424     c                   eval      WSFDST  = stpDST
022300090424     c                   eval      WSFDTS  = stpDTS
022400090424     c                   eval      WSFDCN  = stpDCN
022500090424     c                   eval      WSFREC  = FGT_REC
022600111123     c                   eval      WSFPROGR= 'TNVRA51R'
022700111123     c                   eval      WSFtime = datetime
022800090428     c**
022900090428     c                   eval      WSFNOTE = Errore
023000090424     c                   write     WSFGT000
023100090424     C*
023200090424     c                   endSR
023300081211     C*---------------------------------------------------------------*
023400081110     OQSYSPRT   E            TESTA            02
023500111123     O                                           10 'TNVRA51R'
023600081110     O                                           25 'Autista'
023700081110     O                                           30 'TSR'
023800081110     O                                           38 'Tariffa'
023900081110     O                                           45 'Progr'
024000081110     O                                           55 'Data_Dec'
024100081110     O                                           65 'Data_Sca'
024200081110     O                                           75 'Data_Stp'
024300081110     O                                           85 'Data_Cnv'
024400111123     O                                          100 'N_Rec_FGT'
024500111123     O                                          111 'N O T E'
024600111123     O                       data               132 '  /  /    '
024700111123     O          E            TESTA       1
024800111123     O                                            1 ' '
024900111123     O                                          + 0 'DISACCREDITATI'
025000111123     O                                           25 'da fare'
025100111123     O                                          + 2 'SCADERE'
025200111123     O                       ora                130 '  :  :  '
025300111123      *
025400081110     O          E            DETta       1
025500081110     O                       stpPDR        z     25
025600081110     O                       stpTSR              29
025700081110     O                       stpCTR              36
025800081110     O                       stpPRG        z     43
025900081110     O                       stpDDT        z     55
026000081110     O                       stpDST        z     65
026100081110     O                       stpDTS        z     75
026200081110     O                       stpDCN        z     85
026300111123     o                       FGT_REC       z    100
026400111123     O                       errore             116
026500081110      ****
026600081110     o          E            FINEST      2
026700111123     o                                            1 ' '
026800111123     o                                          + 0 '*** TNVRA51R ***'
026900111123     o                                          + 9 '***  DISACCREDITATI  ***'
027000111123     o                                          + 9 '***   FINE  STAMPA  ***'
027100111123     O                       data               + 9 '  /  /    '
027200111123     O                       ora                + 3 '  :  :  '
027300111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
