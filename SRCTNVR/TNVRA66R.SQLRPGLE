000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111201     H* TNVRA66R *---------------------------------------------------*
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
004000111123     C     AGGIORNA      comp      ' '                                    44
004100111123     C                   move      datadec       DATA_dec          8 0
004200111123     C     *iso          movel     DATA_DEC      data_iso
004300111123     c     data_iso      subdur    1:*days       data_iso
004400111123     C     *iso          movel     data_iso      data_sca          8 0
004500111123     C                   z-add     data_sca      DATAscad          8 0
004600111123     C*
004700111201      * duplica la testata ADT
004800111201     C     adt01key      kLIST
004900111123     C                   kfld                    stpPDR
005000111123     C                   kfld                    stpPRG
005100111123     C*
005200111201     C     add01key      kLIST
005300111201     C                   kfld                    stpPDR
005400111201     C                   kfld                    oldPRG
005500111201     C*
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
007000111201      *
007100081209     C                   SETON                                        LR
007200090423     C*=================================================================
007300111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
007400090423     C*=================================================================
007500111201      *   TESTATE AFFL.DEFL.
007600111201      *
007700090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007800111201     C+   SELECT rrn(fiadt00f), fiadt00f.*
007900111201     C+     FROM fiadt00f
008000111201     C+    WHERE adtdst > :DATAscad and adtpdr
008100111201     C+                  NOT  in
008200111201     C+ (Select traBPT from Aitra00f where tradfi=0 and traann=' ')
008300111201     C+             and adtpdr  NOT IN
008400111201     C+        (Select adtpdr from WFADTXXF)
008500111201     C+               order by ADTpdr
008600111122     C/END-EXEC
008700111122
008800111122     C*=====
008900081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009000111122     C*=====
009100081110     c                   if        sqlCODE < 0
009200081110     c                   eval      esito = Sqlcode
009300111123     c                   move      esito         esito_ALF
009400081110     c                   dump(a)
009500081110     c                   return
009600081110     c                   end
009700081110     C/EXEC SQL
009800081110     C+ OPEN A1
009900081110     C/END-EXEC
010000111122     C*=====
010100081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
010200111122     C*=====
010300081110     c                   if        sqlCODE < 0
010400081110     c                   eval      esito = Sqlcode
010500111123     c                   move      esito         esito_ALF
010600081110     c                   dump(a)
010700081110     c                   return
010800081110     c                   else
010900111122      *
011000081110     c                   except    testa
011100111122      *
011200081110     c                   end
011300090109      * >>>>>>>
011400081209      * resto nel ciclo finchè....
011500081110     C                   DOU       SqlCod < 0
011600081110      * lettura cursore
011700081110     C/EXEC SQL
011800111201     C+ Fetch Next From A1 Into :ADT_REC, :dsADT
011900081110     C/END-EXEC
012000111122     C*=====
012100111122      *  EoF
012200081110     C                   if        SqlCod = 100
012300081110     c                   Leave
012400111122     C*=====
012500081110      *  x errori
012600081110     C                   ElseIf    SqlCod < 0
012700081110     c                   eval      esito = Sqlcode
012800111123     c                   move      esito         esito_ALF
012900081110     c                   dump(a)
013000081110     c                   leave
013100081110     C                   ENDif
013200090428      *
013300111123      *----------------------------->>>>>
013400111123      *  se sono qui ho letto un record correttamente
013500090428      *----------------------------->>>>>
013600111122      * Controlli su Anagrafica
013700111123     c                   clear                   errore           15
013800111122     c                   exsr      Ctrl_APD
013900090428      *
014000090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
014100111201      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
014200111123      *----------------------------->>>>>
014300111201     c                   if        Errore  = *blank  and Aggiorna = 'S'
014400090428      *----------------------------->>>>>
014500111122      *   Aggancia la testata
014600111201     C     ADt01key      chain     fiADT01L
014700111201     c                   if        %Found(fiADT01L)
014800111201      *
014900111201      *  MEMORIZZA IL RECORD COPIATO per capire da dove ripartire
015000111201      *    se si fosse interrotto a metà
015100111201     c                   write     fiADTxxx
015200111201      *
015300111122      *   Mette in SCADENZA
015400111206     c                   z-add     STP_DATSCAD   adtDST
015500111201     c                   eval        oldPRG = adtPRG
015600111201     c                   update    fiADT000
015700111201      *
015800111201     c                   end
015900090428      *----------------------------->>>>>
016000111201     c                   endIF
016100090428      *----------------------------->>>>>
016200111206     c**
016300111206     c                   if        errore  = *blank
016400111206     c                   eval        Errore = 'DISACCREDITATO'
016500111206     c                   end
016600111206      *----
016700111206     c   Of              except    testa
016800111206     c                   except    detta
016900111206      *
017000090424      *  scrive il dettaglio su un file
017100090424     c                   exsr      wri_DETTA
017200081110      *
017300081110     C                   ENDDO
017400081110     C/EXEC SQL
017500081110     C+ Close A1
017600081110     C/END-EXEC
017700081110     C                   if        Esito < 0
017800111123     c                   move      esito         esito_ALF
017900081110     c                   return
018000081110     c                   endIF
018100081110     C*
018200081110     C     FINE          TAG
018300081110     c                   except    finest
018400081110     c                   return
018500111122     C*---------------------------------------------------------------*
018600111122      *  Controlli sull'Anagrafica
018700111122      *---------------------------------------------------------------*
018800111122     c     Ctrl_APD      begSR
018900111122      *
019000111122      * rileva dall'anagrafica
019100111201     c                   eval      apdTIP = 'D'
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
021700111201     c                   clear                   WSADT000
021800090424     C*
021900090424     c                   eval      WSFPDR  =  stpPDR
022000090424     c                   eval      WSFTSR  =  stpTSR
022100111201     c                   eval      WSFPRG  =  stpPRG
022200111201     c                   eval      WSFDDT  =  stpDDT
022300111201     c                   eval      WSFDST  =  stpDST
022400111201     c                   eval      WSFDCN  =  stpDCN
022500111201     c                   eval      WSFDTS  =  stpDTS
022600111201     c                   eval      WSFDUV  =  stpDUV
022700111201     c                   eval      WSFDRC  =  stpDRC
022800111201     c                   eval      WSFNRC  =  stpNRC
022900111201     c                   eval      WSFCDF  =  stpCDF
023000111201     c                   eval      WSFCSF  =  stpCSF
023100111201     c                   eval      WSFFIL  =  stpFIL
023200111201      *
023300111201     c                   eval      WSFFILE = 'FIADT00F'
023400111201     c                   eval      WSFREC  = ADT_REC
023500111201     c                   eval      WSFPROGR= 'TNVRA66R'
023600111123     c                   eval      WSFtime = datetime
023700090428     c                   eval      WSFNOTE = Errore
023800090424     c**
023900111201     c                   write     WSADT000
024000090424     C*
024100090424     c                   endSR
024200081211     C*---------------------------------------------------------------*
024300081110     OQSYSPRT   E            TESTA            02
024400111201     O                                           10 'TNVRA66R'
024500081110     O                                           25 'Autista'
024600081110     O                                           30 'TSR'
024700081110     O                                           45 'Progr'
024800081110     O                                           55 'Data_Dec'
024900081110     O                                           65 'Data_Sca'
025000081110     O                                           75 'Data_Stp'
025100081110     O                                           85 'Data_Cnv'
025200111201     O                                          100 'N_Rec_ADT'
025300111123     O                                          111 'N O T E'
025400111123     O                       data               132 '  /  /    '
025500111123     O          E            TESTA       1
025600111123     O                       ora                130 '  :  :  '
025700111123      *
025800081110     O          E            DETta       1
025900081110     O                       stpPDR        z     25
026000081110     O                       stpTSR              29
026100081110     O                       stpPRG        z     43
026200081110     O                       stpDDT        z     55
026300081110     O                       stpDST        z     65
026400081110     O                       stpDTS        z     75
026500081110     O                       stpDCN        z     85
026600111201     o                       ADT_REC       z    100
026700111123     O                       errore             116
026800081110      ****
026900081110     o          E            FINEST      2
027000111123     o                                            1 ' '
027100111201     o                                          + 0 '*** TNVRA66R ***'
027200111201     o                                          + 4 '*** AFF/DEF ***'
027300111201     o                                          + 4 '***  DISACCREDITATI  ***'
027400111201     o                                          + 4 '***   FINE  STAMPA  ***'
027500111201     O                       data               + 4 '  /  /    '
027600111123     O                       ora                + 3 '  :  :  '
027700111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
