000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111123     H* TNVRA51R *---------------------------------------------------*
000300081110     H*                                                              *
000400111123     H* Controlla se ci sono dei DISACCREDITATI con tariffe APERTE   *
000500081209     H*                                                              *
000600111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000700111122     H*  e se deve eseguire l'aggiornamento                          *
000800000000     H*--------------------------------------------------------------*
000900111206     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
001000111206     F                                     commit
001100111122     Ffitgt01L  uF a E           K DISK
001200111206     F                                     commit
001300111122      *
001400090401     Ffiapd01L  iF   E           K DISK
001500090401      *
001600111129     FwfFGTXXF  o  a E             DISK
001700090424     FwxFGT00S  o  a E             DISK
001800081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001900081211      * -------------------------------------------------------------
002000111122     D KPJBA         E DS
002100081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002200081110     d                                     prefix(STP:3)
002300081110     D FGT_REC         s              9s 0
002400111206     D STP_DATSCAD     s              8s 0 inz(20111130)
002500081230      *
002600111122     D oldPRG          s                   like(tgtPrg)
002700111122     D newPRG          s                   like(tgtPrg)
002800111123     D esito           s              9s 0
002900111123     D esito_ALF       s              9a
003000090109      *
003100111123     D DATA_eur        S               D   DATFMT(*eur)
003200111123     D DATA_iso        S               D   DATFMT(*iso)
003300111123      *
003400111122     D ficn06ds      E DS
003500111123     C*------------------------------------------------------------------------*
003600111123     C*        - OPERAZIONI INIZIALI
003700111123     C*------------------------------------------------------------------------*
003800111123     C     *ENTRY        PLIST
003900111123     C                   PARM                    DATADEC           8
004000111123     C                   PARM                    AGGIORNA          1
004100111123     C                   PARM                    ESITO_ALF
004200111123     C*
004300111123     C     AGGIORNA      comp      ' '                                    44
004400111123     C                   move      datadec       DATA_dec          8 0
004500111123     C     *iso          movel     DATA_DEC      data_iso
004600111123     c     data_iso      subdur    1:*days       data_iso
004700111123     C     *iso          movel     data_iso      data_sca          8 0
004800111123     C                   z-add     data_sca      DATAscad          8 0
004900111123     C*
005000111123      * duplica la testata TGT
005100111123     C     tgt01key      kLIST
005200111123     C                   kfld                    stpPDR
005300111123     C                   kfld                    stpSML
005400111123     C                   kfld                    stpPRG
005500111123     C*
005600111123     c     Key_APD       KLIST
005700111123     c                   kfld                    apdTIP
005800111123     c                   kfld                    apdPDR
005900111123      *
006000111123     c                   clear                   esito
006100111123     c                   clear                   esito_ALF
006200111123     c                   time                    datetime         14 0
006300111123     c                   move      datetime      data              8 0
006400111123     c                   movel     datetime      ora               6 0
006500111123      *
006600111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
006700111123     C                   clear                   UDATE_8           8 0
006800111123     C                   MOVEL     *DATE         UDATE_8
006900111122     c                   clear                   savSTPpdr         7 0
007000081209     C                   SETON                                        LR
007100090423      **
007200090423     C*=================================================================
007300111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007400090423     C*=================================================================
007500090424      *
007600090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007700111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
007800111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
007900111123     C+        and fgtsml=' ' and fgtdst > :DATAscad and Fgtpdr
008000111123     C+                   NOT in
008100111123     C+ (Select traKAU from Aitra00f where tradfi=0 and traann=' ')
008200111129     C+           and Fgtpdr  NOT IN
008300111129     C+     (Select fgtpdr from WFFGTxxF)
008400111122     C+   order by Fgtpdr
008500111122     C/END-EXEC
008600111122
008700111122     C*=====
008800081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008900111122     C*=====
009000081110     c                   if        sqlCODE < 0
009100081110     c                   eval      esito = Sqlcode
009200111123     c                   move      esito         esito_ALF
009300081110     c                   dump(a)
009400081110     c                   return
009500081110     c                   end
009600081110     C/EXEC SQL
009700081110     C+ OPEN A1
009800081110     C/END-EXEC
009900111122     C*=====
010000081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
010100111122     C*=====
010200081110     c                   if        sqlCODE < 0
010300081110     c                   eval      esito = Sqlcode
010400111123     c                   move      esito         esito_ALF
010500081110     c                   dump(a)
010600081110     c                   return
010700081110     c                   else
010800111122      *
010900081110     c                   except    testa
011000111122      *
011100081110     c                   end
011200090109      * >>>>>>>
011300081209      * resto nel ciclo finchè....
011400081110     C                   DOU       SqlCod < 0
011500081110      * lettura cursore
011600081110     C/EXEC SQL
011700111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
011800081110     C/END-EXEC
011900111122     C*=====
012000111122      *  EoF
012100081110     C                   if        SqlCod = 100
012200081110     c                   Leave
012300111122     C*=====
012400081110      *  x errori
012500081110     C                   ElseIf    SqlCod < 0
012600081110     c                   eval      esito = Sqlcode
012700111123     c                   move      esito         esito_ALF
012800081110     c                   dump(a)
012900081110     c                   leave
013000081110     C                   ENDif
013100090428      *
013200111123      *----------------------------->>>>>
013300111123      *  se sono qui ho letto un record correttamente
013400090428      *----------------------------->>>>>
013500111122      * Controlli su Anagrafica
013600111123     c                   clear                   errore           15
013700111122     c                   exsr      Ctrl_APD
013800090428      *
013900090428      *----------------------------->>>>>
014000090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014100111123      *----------------------------->>>>>
014200111124     c************       if        Errore  = *blank
014300090428      *----------------------------->>>>>
014400081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
014500081110     c                   if        Aggiorna = 'S'
014600111122      *  Deve scrivere la testata a rottura di autista
014700111123      *-+++++++++++++++--
014800111122     c                   if        stpPDR <> savSTPpdr
014900111123      *-+++++++++++++++--
015000111122      *  salva chiave di rottura
015100111122     c                   eval              savSTPpdr = stpPDR
015200111122      *   Aggancia la testata
015300111122     C     tgt01key      chain     fiTGT01L
015400111122     c                   if        %Found(fiTGT01L)
015500111122      *   Mette in SCADENZA
015600111206     c                   z-add     STP_DATSCAD   tgtDST
015700111122     c                   eval        oldPRG = tgtPRG
015800111122     c                   update    fiTGT000
015900111122     c                   end
016000111123      *-+++++++++++++++--
016100111122     c                   endIF
016200111122      *-+++++++++++++++--
016300111122      *   deve far scadere il record dell'FGT
016400081218     c     fgt_REC       chain     FIFGT00F
016500081218     c                   if        %Found(FIFGT00F)
016600111122      * aggiorna
016700111206     c                   eval      fgtDST = STP_DATSCAD
016800111122     c                   update    fgtFisico
016900090423     c                   end
017000111129      *
017100111129      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
017200111129      *    se si fosse interrotto a metà
017300111129     c                   write     wfFGTxxx
017400090112      *
017500090423     c                   endIF
017600090428      *----------------------------->>>>>
017700111124     c************       end
017800090428      *----------------------------->>>>>
017900111123      *    Segnala
018000111123     c                   if        Errore = *blank
018100111123     c                   Eval      Errore = 'DISACCREDITATO'
018200111123     c                   else
018300111123     c                   end
018400111123      *
018500090424      *  scrive il dettaglio su un file
018600111123     c   Of              except    testa
018700111123     c                   except    detta
018800111123      *
018900090424     c                   exsr      wri_DETTA
019000081110      *
019100081110     C                   ENDDO
019200081110     C/EXEC SQL
019300081110     C+ Close A1
019400081110     C/END-EXEC
019500081110     C                   if        Esito < 0
019600111123     c                   move      esito         esito_ALF
019700081110     c                   return
019800081110     c                   endIF
019900081110     C*
020000081110     C     FINE          TAG
020100081110     c                   except    finest
020200081110     c                   return
020300111122     C*---------------------------------------------------------------*
020400111122      *  Controlli sull'Anagrafica
020500111122      *---------------------------------------------------------------*
020600111122     c     Ctrl_APD      begSR
020700111122      *
020800111122      * rileva dall'anagrafica
020900111122     c                   eval      apdTIP = 'A'
021000111122     c                   eval      apdPDR = stpPDR
021100111122     c     Key_APD       chain     fiapd01l
021200111122     c                   if        %Found(fiapd01l)
021300111122      *
021400111122     c                   if        apdATB <> *blank
021500111123     c                   movel(p)  'ANNULLATO'   errore
021600111122     c                   else
021700111122      *
021800111122      *-  Esluso da autofatturazione
021900111122     c                   if        apdPDD = 'S'
022000111123     c                   movel(p)  'AUTOFAT'     errore
022100111122     c                   end
022200111122      *
022300111122     c                   endIF
022400111122      *
022500111122     c                   else
022600111123     c                   movel(p)  'NO_APD'      errore
022700111122     c                   end
022800111122      *
022900111122     c                   endSR
023000111122     C*---------------------------------------------------------------*
023100111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
023200111122      *---------------------------------------------------------------*
023300111122     c     WRI_DETTA     begSR
023400111122     C*
023500090424     c                   clear                   WSFGT000
023600090424     C*
023700090424     c                   eval      WSFPDR  =  stpPDR
023800090424     c                   eval      WSFTSR  =  stpTSR
023900090424     c                   eval      WSFCTR  =  stpCTR
024000090424     c                   eval      WSFPRG  = stpPRG
024100090424     c                   eval      WSFDDT  = stpDDT
024200090424     c                   eval      WSFDST  = stpDST
024300090424     c                   eval      WSFDTS  = stpDTS
024400090424     c                   eval      WSFDCN  = stpDCN
024500111129     c                   eval      WSFFILE = 'FIFGT00F'
024600090424     c                   eval      WSFREC  = FGT_REC
024700111123     c                   eval      WSFPROGR= 'TNVRA51R'
024800111123     c                   eval      WSFtime = datetime
024900090428     c**
025000090428     c                   eval      WSFNOTE = Errore
025100090424     c                   write     WSFGT000
025200090424     C*
025300090424     c                   endSR
025400081211     C*---------------------------------------------------------------*
025500081110     OQSYSPRT   E            TESTA            02
025600111123     O                                           10 'TNVRA51R'
025700081110     O                                           25 'Autista'
025800081110     O                                           30 'TSR'
025900081110     O                                           38 'Tariffa'
026000081110     O                                           45 'Progr'
026100081110     O                                           55 'Data_Dec'
026200081110     O                                           65 'Data_Sca'
026300081110     O                                           75 'Data_Stp'
026400081110     O                                           85 'Data_Cnv'
026500111123     O                                          100 'N_Rec_FGT'
026600111123     O                                          111 'N O T E'
026700111123     O                       data               132 '  /  /    '
026800111123     O          E            TESTA       1
026900111123     O                                            1 ' '
027000111123     O                                          + 0 'DISACCREDITATI'
027100111123     O                                           25 'da fare'
027200111123     O                                          + 2 'SCADERE'
027300111123     O                       ora                130 '  :  :  '
027400111123      *
027500081110     O          E            DETta       1
027600081110     O                       stpPDR        z     25
027700081110     O                       stpTSR              29
027800081110     O                       stpCTR              36
027900081110     O                       stpPRG        z     43
028000081110     O                       stpDDT        z     55
028100081110     O                       stpDST        z     65
028200081110     O                       stpDTS        z     75
028300081110     O                       stpDCN        z     85
028400111123     o                       FGT_REC       z    100
028500111123     O                       errore             116
028600081110      ****
028700081110     o          E            FINEST      2
028800111123     o                                            1 ' '
028900111123     o                                          + 0 '*** TNVRA51R ***'
029000111123     o                                          + 9 '***  DISACCREDITATI  ***'
029100111123     o                                          + 9 '***   FINE  STAMPA  ***'
029200111123     O                       data               + 9 '  /  /    '
029300111123     O                       ora                + 3 '  :  :  '
029400111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
