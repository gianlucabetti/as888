000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX66R *---------------------------------------------------*
000300081110     H*                                                              *
000400111201     H*  fa        scadere il giorno prima.     AFFL/DEFL            *
000500081209     H*                                                              *
000600111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000700111122     H*  e se deve eseguire l'aggiornamento                          *
000800000000     H*--------------------------------------------------------------*
000900111201     Ffiadt01L  uF   E           K DISK
001000111206     F                                     commit
001100111122      *
001200090401     Ffiapd01L  iF   E           K DISK
001300090401      *
001400111201     FwfADTXXF  o  a E             DISK
001500111201     FwxADT00S  o  a E             DISK
001600081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001700081211      * -------------------------------------------------------------
001800111122     D KPJBA         E DS
001900111201     D dsADT         E DS                  ExtName(FIADT00F)
002000081110     d                                     prefix(STP:3)
002100111201     D ADT_REC         s              9s 0
002200111206     D STP_DATSCAD     s              8s 0 inz(20111130)
002300081230      *
002400111201     D oldPRG          s                   like(adtPrg)
002500111201     D newPRG          s                   like(adtPrg)
002600111201      *
002700111123     D esito           s              9s 0
002800111123     D esito_ALF       s              9a
002900090109      *
003000111123     D DATA_eur        S               D   DATFMT(*eur)
003100111123     D DATA_iso        S               D   DATFMT(*iso)
003200111123     C*------------------------------------------------------------------------*
003300111123     C*        - OPERAZIONI INIZIALI
003400111123     C*------------------------------------------------------------------------*
003500111123     C     *ENTRY        PLIST
003600111123     C                   PARM                    DATADEC           8
003700111123     C                   PARM                    AGGIORNA          1
003800111123     C                   PARM                    ESITO_ALF
003900111123     C*
004000120319     C*
004100120319     C*  NON DEVE AGGIORNARE     MAI   !!!!!!!!!
004200120319     C*        ATTENZIONE
004300120319     C*
004400120319     C                   move      aggiorna      savAGGIORNA       1
004500120319     c                   clear                   aggiorna
004600120319     C*
004700111123     C     AGGIORNA      comp      ' '                                    44
004800111123     C                   move      datadec       DATA_dec          8 0
004900111123     C     *iso          movel     DATA_DEC      data_iso
005000111123     c     data_iso      subdur    1:*days       data_iso
005100111123     C     *iso          movel     data_iso      data_sca          8 0
005200111123     C                   z-add     data_sca      DATAscad          8 0
005300111123     C*
005400111201      * duplica la testata ADT
005500111201     C     adt01key      kLIST
005600111123     C                   kfld                    stpPDR
005700111123     C                   kfld                    stpPRG
005800111123     C*
005900111201     C     add01key      kLIST
006000111201     C                   kfld                    stpPDR
006100111201     C                   kfld                    oldPRG
006200111201     C*
006300111123     c     Key_APD       KLIST
006400111123     c                   kfld                    apdTIP
006500111123     c                   kfld                    apdPDR
006600111123      *
006700111123     c                   clear                   esito
006800111123     c                   clear                   esito_ALF
006900111123     c                   time                    datetime         14 0
007000111123     c                   move      datetime      data              8 0
007100111123     c                   movel     datetime      ora               6 0
007200111123      *
007300111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
007400111123     C                   clear                   UDATE_8           8 0
007500111123     C                   MOVEL     *DATE         UDATE_8
007600111122     c                   clear                   savSTPpdr         7 0
007700111201      *
007800081209     C                   SETON                                        LR
007900090423     C*=================================================================
008000111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
008100090423     C*=================================================================
008200111201      *   TESTATE AFFL.DEFL.
008300111201      *
008400090422     C/EXEC SQL DECLARE A1 CURSOR FOR
008500111201     C+   SELECT rrn(fiadt00f), fiadt00f.*
008600111201     C+     FROM fiadt00f
008700111201     C+    WHERE adtdst > :DATAscad and adtpdr
008800111201     C+                  NOT  in
008900120319     C+ (Select traBPT from Aitra00f where tradfi=0  OR  traann=' ')
009000111201     C+             and adtpdr  NOT IN
009100111201     C+        (Select adtpdr from WFADTXXF)
009200111201     C+               order by ADTpdr
009300111122     C/END-EXEC
009400111122
009500111122     C*=====
009600081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009700111122     C*=====
009800081110     c                   if        sqlCODE < 0
009900081110     c                   eval      esito = Sqlcode
010000111123     c                   move      esito         esito_ALF
010100081110     c                   dump(a)
010200081110     c                   return
010300081110     c                   end
010400081110     C/EXEC SQL
010500081110     C+ OPEN A1
010600081110     C/END-EXEC
010700111122     C*=====
010800081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
010900111122     C*=====
011000081110     c                   if        sqlCODE < 0
011100081110     c                   eval      esito = Sqlcode
011200111123     c                   move      esito         esito_ALF
011300081110     c                   dump(a)
011400081110     c                   return
011500081110     c                   else
011600111122      *
011700081110     c                   except    testa
011800111122      *
011900081110     c                   end
012000090109      * >>>>>>>
012100081209      * resto nel ciclo finchè....
012200081110     C                   DOU       SqlCod < 0
012300081110      * lettura cursore
012400081110     C/EXEC SQL
012500111201     C+ Fetch Next From A1 Into :ADT_REC, :dsADT
012600081110     C/END-EXEC
012700111122     C*=====
012800111122      *  EoF
012900081110     C                   if        SqlCod = 100
013000081110     c                   Leave
013100111122     C*=====
013200081110      *  x errori
013300081110     C                   ElseIf    SqlCod < 0
013400081110     c                   eval      esito = Sqlcode
013500111123     c                   move      esito         esito_ALF
013600081110     c                   dump(a)
013700081110     c                   leave
013800081110     C                   ENDif
013900090428      *
014000111123      *----------------------------->>>>>
014100111123      *  se sono qui ho letto un record correttamente
014200090428      *----------------------------->>>>>
014300111122      * Controlli su Anagrafica
014400111123     c                   clear                   errore           15
014500111122     c                   exsr      Ctrl_APD
014600090428      *
014700090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014800111201      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
014900111123      *----------------------------->>>>>
015000111201     c                   if        Errore  = *blank  and Aggiorna = 'S'
015100090428      *----------------------------->>>>>
015200111122      *   Aggancia la testata
015300111201     C     ADt01key      chain     fiADT01L
015400111201     c                   if        %Found(fiADT01L)
015500111201      *
015600111201      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
015700111201      *    se si fosse interrotto a metà
015800111201     c                   write     fiADTxxx
015900111201      *
016000111122      *   Mette in SCADENZA
016100111206     c                   z-add     STP_DATSCAD   adtDST
016200111201     c                   eval        oldPRG = adtPRG
016300111201     c                   update    fiADT000
016400111201      *
016500111201     c                   end
016600090428      *----------------------------->>>>>
016700111201     c                   endIF
016800090428      *----------------------------->>>>>
016900111206     c**
017000111206     c                   if        errore  = *blank
017100111206     c                   eval        Errore = 'DISACCREDITATO'
017200111206     c                   end
017300111206      *----
017400111206     c   Of              except    testa
017500111206     c                   except    detta
017600111206      *
017700090424      *  scrive il dettaglio su un file
017800090424     c                   exsr      wri_DETTA
017900081110      *
018000081110     C                   ENDDO
018100081110     C/EXEC SQL
018200081110     C+ Close A1
018300081110     C/END-EXEC
018400081110     C                   if        Esito < 0
018500111123     c                   move      esito         esito_ALF
018600081110     c                   return
018700081110     c                   endIF
018800081110     C*
018900081110     C     FINE          TAG
019000081110     c                   except    finest
019100120319     C*
019200120319     C*  NON DEVE AGGIORNARE     MAI   !!!!!!!!!
019300120319     C*        ATTENZIONE
019400120319     C*
019500120319     C*
019600120319     C*  reimposta il flag di aggiornamento per il programma successivo
019700120319     C                   eval      aggiorna = savAGGIORNA
019800120319     C*
019900081110     c                   return
020000111122     C*---------------------------------------------------------------*
020100111122      *  Controlli sull'Anagrafica
020200111122      *---------------------------------------------------------------*
020300111122     c     Ctrl_APD      begSR
020400111122      *
020500111122      * rileva dall'anagrafica
020600111201     c                   eval      apdTIP = 'D'
020700111122     c                   eval      apdPDR = stpPDR
020800111122     c     Key_APD       chain     fiapd01l
020900111122     c                   if        %Found(fiapd01l)
021000111122      *
021100111122     c                   if        apdATB <> *blank
021200111123     c                   movel(p)  'ANNULLATO'   errore
021300111122     c                   else
021400111122      *
021500111122      *-  Esluso da autofatturazione
021600111122     c                   if        apdPDD = 'S'
021700111123     c                   movel(p)  'AUTOFAT'     errore
021800111122     c                   end
021900111122      *
022000111122     c                   endIF
022100111122      *
022200111122     c                   else
022300111123     c                   movel(p)  'NO_APD'      errore
022400111122     c                   end
022500111122      *
022600111122     c                   endSR
022700111122     C*---------------------------------------------------------------*
022800111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
022900111122      *---------------------------------------------------------------*
023000111122     c     WRI_DETTA     begSR
023100111122     C*
023200111201     c                   clear                   WSADT000
023300090424     C*
023400090424     c                   eval      WSFPDR  =  stpPDR
023500090424     c                   eval      WSFTSR  =  stpTSR
023600111201     c                   eval      WSFPRG  =  stpPRG
023700111201     c                   eval      WSFDDT  =  stpDDT
023800111201     c                   eval      WSFDST  =  stpDST
023900111201     c                   eval      WSFDCN  =  stpDCN
024000111201     c                   eval      WSFDTS  =  stpDTS
024100111201     c                   eval      WSFDUV  =  stpDUV
024200111201     c                   eval      WSFDRC  =  stpDRC
024300111201     c                   eval      WSFNRC  =  stpNRC
024400111201     c                   eval      WSFCDF  =  stpCDF
024500111201     c                   eval      WSFCSF  =  stpCSF
024600111201     c                   eval      WSFFIL  =  stpFIL
024700111201      *
024800111201     c                   eval      WSFFILE = 'FIADT00F'
024900111201     c                   eval      WSFREC  = ADT_REC
025000111216     c                   eval      WSFPROGR= 'FICNX66R'
025100111123     c                   eval      WSFtime = datetime
025200090428     c                   eval      WSFNOTE = Errore
025300090424     c**
025400111201     c                   write     WSADT000
025500090424     C*
025600090424     c                   endSR
025700081211     C*---------------------------------------------------------------*
025800081110     OQSYSPRT   E            TESTA            02
025900111216     O                                           10 'FICNX66R'
026000081110     O                                           25 'Autista'
026100081110     O                                           30 'TSR'
026200081110     O                                           45 'Progr'
026300081110     O                                           55 'Data_Dec'
026400081110     O                                           65 'Data_Sca'
026500081110     O                                           75 'Data_Stp'
026600081110     O                                           85 'Data_Cnv'
026700111201     O                                          100 'N_Rec_ADT'
026800111123     O                                          111 'N O T E'
026900111123     O                       data               132 '  /  /    '
027000111123     O          E            TESTA       1
027100111123     O                       ora                130 '  :  :  '
027200111123      *
027300081110     O          E            DETta       1
027400081110     O                       stpPDR        z     25
027500081110     O                       stpTSR              29
027600081110     O                       stpPRG        z     43
027700081110     O                       stpDDT        z     55
027800081110     O                       stpDST        z     65
027900081110     O                       stpDTS        z     75
028000081110     O                       stpDCN        z     85
028100111201     o                       ADT_REC       z    100
028200111123     O                       errore             116
028300081110      ****
028400081110     o          E            FINEST      2
028500111123     o                                            1 ' '
028600111216     o                                          + 0 '*** FICNX66R ***'
028700111201     o                                          + 4 '*** AFF/DEF ***'
028800111201     o                                          + 4 '***  DISACCREDITATI  ***'
028900111201     o                                          + 4 '***   FINE  STAMPA  ***'
029000111201     O                       data               + 4 '  /  /    '
029100111123     O                       ora                + 3 '  :  :  '
029200111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
