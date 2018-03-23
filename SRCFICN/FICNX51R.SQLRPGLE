000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX51R *---------------------------------------------------*
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
004300120319     C*  NON DEVE AGGIORNARE     MAI   !!!!!!!!!
004400120319     C*        ATTENZIONE
004500120319     C*
004600120319     C                   move      aggiorna      savAGGIORNA       1
004700120319     c                   clear                   aggiorna
004800120319     C*
004900111123     C     AGGIORNA      comp      ' '                                    44
005000111123     C                   move      datadec       DATA_dec          8 0
005100111123     C     *iso          movel     DATA_DEC      data_iso
005200111123     c     data_iso      subdur    1:*days       data_iso
005300111123     C     *iso          movel     data_iso      data_sca          8 0
005400111123     C                   z-add     data_sca      DATAscad          8 0
005500111123     C*
005600111123      * duplica la testata TGT
005700111123     C     tgt01key      kLIST
005800111123     C                   kfld                    stpPDR
005900111123     C                   kfld                    stpSML
006000111123     C                   kfld                    stpPRG
006100111123     C*
006200111123     c     Key_APD       KLIST
006300111123     c                   kfld                    apdTIP
006400111123     c                   kfld                    apdPDR
006500111123      *
006600111123     c                   clear                   esito
006700111123     c                   clear                   esito_ALF
006800111123     c                   time                    datetime         14 0
006900111123     c                   move      datetime      data              8 0
007000111123     c                   movel     datetime      ora               6 0
007100111123      *
007200111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
007300111123     C                   clear                   UDATE_8           8 0
007400111123     C                   MOVEL     *DATE         UDATE_8
007500111122     c                   clear                   savSTPpdr         7 0
007600081209     C                   SETON                                        LR
007700090423      **
007800090423     C*=================================================================
007900111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
008000090423     C*=================================================================
008100090424      *
008200090422     C/EXEC SQL DECLARE A1 CURSOR FOR
008300111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
008400111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
008500111123     C+        and fgtsml=' ' and fgtdst > :DATAscad and Fgtpdr
008600111123     C+                   NOT in
008700120319     C+ (Select traKAU from Aitra00f where tradfi=0  OR  traann=' ')
008800111129     C+           and Fgtpdr  NOT IN
008900111129     C+     (Select fgtpdr from WFFGTxxF)
009000111122     C+   order by Fgtpdr
009100111122     C/END-EXEC
009200111122
009300111122     C*=====
009400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009500111122     C*=====
009600081110     c                   if        sqlCODE < 0
009700081110     c                   eval      esito = Sqlcode
009800111123     c                   move      esito         esito_ALF
009900081110     c                   dump(a)
010000081110     c                   return
010100081110     c                   end
010200081110     C/EXEC SQL
010300081110     C+ OPEN A1
010400081110     C/END-EXEC
010500111122     C*=====
010600081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
010700111122     C*=====
010800081110     c                   if        sqlCODE < 0
010900081110     c                   eval      esito = Sqlcode
011000111123     c                   move      esito         esito_ALF
011100081110     c                   dump(a)
011200081110     c                   return
011300081110     c                   else
011400111122      *
011500081110     c                   except    testa
011600111122      *
011700081110     c                   end
011800090109      * >>>>>>>
011900081209      * resto nel ciclo finchè....
012000081110     C                   DOU       SqlCod < 0
012100081110      * lettura cursore
012200081110     C/EXEC SQL
012300111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
012400081110     C/END-EXEC
012500111122     C*=====
012600111122      *  EoF
012700081110     C                   if        SqlCod = 100
012800081110     c                   Leave
012900111122     C*=====
013000081110      *  x errori
013100081110     C                   ElseIf    SqlCod < 0
013200081110     c                   eval      esito = Sqlcode
013300111123     c                   move      esito         esito_ALF
013400081110     c                   dump(a)
013500081110     c                   leave
013600081110     C                   ENDif
013700090428      *
013800111123      *----------------------------->>>>>
013900111123      *  se sono qui ho letto un record correttamente
014000090428      *----------------------------->>>>>
014100111122      * Controlli su Anagrafica
014200111123     c                   clear                   errore           15
014300111122     c                   exsr      Ctrl_APD
014400090428      *
014500090428      *----------------------------->>>>>
014600090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014700111123      *----------------------------->>>>>
014800111124     c************       if        Errore  = *blank
014900090428      *----------------------------->>>>>
015000081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
015100081110     c                   if        Aggiorna = 'S'
015200111122      *  Deve scrivere la testata a rottura di autista
015300111123      *-+++++++++++++++--
015400111122     c                   if        stpPDR <> savSTPpdr
015500111123      *-+++++++++++++++--
015600111122      *  salva chiave di rottura
015700111122     c                   eval              savSTPpdr = stpPDR
015800111122      *   Aggancia la testata
015900111122     C     tgt01key      chain     fiTGT01L
016000111122     c                   if        %Found(fiTGT01L)
016100111122      *   Mette in SCADENZA
016200111206     c                   z-add     STP_DATSCAD   tgtDST
016300111122     c                   eval        oldPRG = tgtPRG
016400111122     c                   update    fiTGT000
016500111122     c                   end
016600111123      *-+++++++++++++++--
016700111122     c                   endIF
016800111122      *-+++++++++++++++--
016900111122      *   deve far scadere il record dell'FGT
017000081218     c     fgt_REC       chain     FIFGT00F
017100081218     c                   if        %Found(FIFGT00F)
017200111122      * aggiorna
017300111206     c                   eval      fgtDST = STP_DATSCAD
017400111122     c                   update    fgtFisico
017500090423     c                   end
017600111129      *
017700111129      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
017800111129      *    se si fosse interrotto a metà
017900111129     c                   write     wfFGTxxx
018000090112      *
018100090423     c                   endIF
018200090428      *----------------------------->>>>>
018300111124     c************       end
018400090428      *----------------------------->>>>>
018500111123      *    Segnala
018600111123     c                   if        Errore = *blank
018700111123     c                   Eval      Errore = 'DISACCREDITATO'
018800111123     c                   else
018900111123     c                   end
019000111123      *
019100090424      *  scrive il dettaglio su un file
019200111123     c   Of              except    testa
019300111123     c                   except    detta
019400111123      *
019500090424     c                   exsr      wri_DETTA
019600081110      *
019700081110     C                   ENDDO
019800081110     C/EXEC SQL
019900081110     C+ Close A1
020000081110     C/END-EXEC
020100081110     C                   if        Esito < 0
020200111123     c                   move      esito         esito_ALF
020300081110     c                   return
020400081110     c                   endIF
020500081110     C*
020600081110     C     FINE          TAG
020700081110     c                   except    finest
020800120319     C*
020900120319     C*  NON DEVE AGGIORNARE     MAI   !!!!!!!!!
021000120319     C*        ATTENZIONE
021100120319     C*
021200120319     C*
021300120319     C*  reimposta il flag di aggiornamento per il programma successivo
021400120319     C                   eval      aggiorna = savAGGIORNA
021500120319     C*
021600081110     c                   return
021700111122     C*---------------------------------------------------------------*
021800111122      *  Controlli sull'Anagrafica
021900111122      *---------------------------------------------------------------*
022000111122     c     Ctrl_APD      begSR
022100111122      *
022200111122      * rileva dall'anagrafica
022300111122     c                   eval      apdTIP = 'A'
022400111122     c                   eval      apdPDR = stpPDR
022500111122     c     Key_APD       chain     fiapd01l
022600111122     c                   if        %Found(fiapd01l)
022700111122      *
022800111122     c                   if        apdATB <> *blank
022900111123     c                   movel(p)  'ANNULLATO'   errore
023000111122     c                   else
023100111122      *
023200111122      *-  Esluso da autofatturazione
023300111122     c                   if        apdPDD = 'S'
023400111123     c                   movel(p)  'AUTOFAT'     errore
023500111122     c                   end
023600111122      *
023700111122     c                   endIF
023800111122      *
023900111122     c                   else
024000111123     c                   movel(p)  'NO_APD'      errore
024100111122     c                   end
024200111122      *
024300111122     c                   endSR
024400111122     C*---------------------------------------------------------------*
024500111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
024600111122      *---------------------------------------------------------------*
024700111122     c     WRI_DETTA     begSR
024800111122     C*
024900090424     c                   clear                   WSFGT000
025000090424     C*
025100090424     c                   eval      WSFPDR  =  stpPDR
025200090424     c                   eval      WSFTSR  =  stpTSR
025300090424     c                   eval      WSFCTR  =  stpCTR
025400090424     c                   eval      WSFPRG  = stpPRG
025500090424     c                   eval      WSFDDT  = stpDDT
025600090424     c                   eval      WSFDST  = stpDST
025700090424     c                   eval      WSFDTS  = stpDTS
025800090424     c                   eval      WSFDCN  = stpDCN
025900111129     c                   eval      WSFFILE = 'FIFGT00F'
026000090424     c                   eval      WSFREC  = FGT_REC
026100111216     c                   eval      WSFPROGR= 'FICNX51R'
026200111123     c                   eval      WSFtime = datetime
026300090428     c**
026400090428     c                   eval      WSFNOTE = Errore
026500090424     c                   write     WSFGT000
026600090424     C*
026700090424     c                   endSR
026800081211     C*---------------------------------------------------------------*
026900081110     OQSYSPRT   E            TESTA            02
027000111216     O                                           10 'FICNX51R'
027100081110     O                                           25 'Autista'
027200081110     O                                           30 'TSR'
027300081110     O                                           38 'Tariffa'
027400081110     O                                           45 'Progr'
027500081110     O                                           55 'Data_Dec'
027600081110     O                                           65 'Data_Sca'
027700081110     O                                           75 'Data_Stp'
027800081110     O                                           85 'Data_Cnv'
027900111123     O                                          100 'N_Rec_FGT'
028000111123     O                                          111 'N O T E'
028100111123     O                       data               132 '  /  /    '
028200111123     O          E            TESTA       1
028300111123     O                                            1 ' '
028400111123     O                                          + 0 'DISACCREDITATI'
028500111123     O                                           25 'da fare'
028600111123     O                                          + 2 'SCADERE'
028700111123     O                       ora                130 '  :  :  '
028800111123      *
028900081110     O          E            DETta       1
029000081110     O                       stpPDR        z     25
029100081110     O                       stpTSR              29
029200081110     O                       stpCTR              36
029300081110     O                       stpPRG        z     43
029400081110     O                       stpDDT        z     55
029500081110     O                       stpDST        z     65
029600081110     O                       stpDTS        z     75
029700081110     O                       stpDCN        z     85
029800111123     o                       FGT_REC       z    100
029900111123     O                       errore             116
030000081110      ****
030100081110     o          E            FINEST      2
030200111123     o                                            1 ' '
030300111216     o                                          + 0 '*** FICNX51R ***'
030400111123     o                                          + 9 '***  DISACCREDITATI  ***'
030500111123     o                                          + 9 '***   FINE  STAMPA  ***'
030600111123     O                       data               + 9 '  /  /    '
030700111123     O                       ora                + 3 '  :  :  '
030800111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
