000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111205     H* TNVRA67R *---------------------------------------------------*
000300081110     H*                                                              *
000400111205     H* CONTROLLA testate ADT che non hanno corrispondente ADD       *
000500111205     H*     affluenze defluenze                                      *
000600111205     H*                                                              *
000700111123     H* riceve come parametro la Data decorrenza e                   *
000800111123     H*    se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000090401     Ffiapd01L  iF   E           K DISK
001100090401      *
001200111205     FwxADT00S  o  a E             DISK
001300081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400081211      * -------------------------------------------------------------
001500111122     D KPJBA         E DS
001600111205     D dsADT         E DS                  ExtName(FIADT00F)
001700081110     d                                     prefix(STP:3)
001800111205     D ADT_REC         s              9s 0
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
006200111205     C+ select RRN(fiADT00f), fiADT00f.*
006300111205     C+     from fiADT00f
006400111205     C+     where adtpdr*1000 + adtprg
006500111205     C+     not in   (select addpdr*1000 + addprg from fiadd00f)
006600111205     C+     ORDER BY ADTPDR
006700111122     C/END-EXEC
006800111122
006900111122     C*=====
007000081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
007100111122     C*=====
007200081110     c                   if        sqlCODE < 0
007300081110     c                   eval      esito = Sqlcode
007400111123     c                   move      esito         esito_ALF
007500081110     c                   dump(a)
007600081110     c                   return
007700081110     c                   end
007800081110     C/EXEC SQL
007900081110     C+ OPEN A1
008000081110     C/END-EXEC
008100111122     C*=====
008200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008300111122     C*=====
008400081110     c                   if        sqlCODE < 0
008500081110     c                   eval      esito = Sqlcode
008600111123     c                   move      esito         esito_ALF
008700081110     c                   dump(a)
008800081110     c                   return
008900081110     c                   else
009000111122      *
009100081110     c                   except    testa
009200111122      *
009300081110     c                   end
009400090109      * >>>>>>>
009500081209      * resto nel ciclo finchè....
009600081110     C                   DOU       SqlCod < 0
009700081110      * lettura cursore
009800081110     C/EXEC SQL
009900111205     C+ Fetch Next From A1 Into :ADT_REC, :dsADT
010000081110     C/END-EXEC
010100111122     C*=====
010200111122      *  EoF
010300081110     C                   if        SqlCod = 100
010400081110     c                   Leave
010500111122     C*=====
010600081110      *  x errori
010700081110     C                   ElseIf    SqlCod < 0
010800081110     c                   eval      esito = Sqlcode
010900111123     c                   move      esito         esito_ALF
011000081110     c                   dump(a)
011100081110     c                   leave
011200081110     C                   ENDif
011300090428      *
011400111123      *----------------------------->>>>>
011500111123      *  se sono qui ho letto un record correttamente
011600090428      *----------------------------->>>>>
011700111122      * Controlli su Anagrafica
011800111123     c                   clear                   errore           15
011900111122     c                   exsr      Ctrl_APD
012000090428      *
012100090428      *----------------------------->>>>>
012200111123      *    Segnala
012300111123     c                   if        Errore = *blank
012400111205     c                   Eval      Errore = 'ADT Senza ADD'
012500111123     c                   else
012600111123     c                   end
012700111123      *
012800090427     c   Of              except    testa
012900090427     c                   except    detta
013000090428      *----------------------------->>>>>
013100090424      *  scrive il dettaglio su un file
013200090424     c                   exsr      wri_DETTA
013300111123     C                   z-add     *all'1'       esito
013400111123     c                   move      esito         esito_ALF
013500081110      *
013600081110     C                   ENDDO
013700081110     C/EXEC SQL
013800081110     C+ Close A1
013900081110     C/END-EXEC
014000111123     c                   except    finest
014100111123     C*
014200081110     C                   if        Esito < 0
014300111123     c                   move      esito         esito_ALF
014400081110     c                   return
014500081110     c                   endIF
014600081110     C*
014700081110     C     FINE          TAG
014800081110     c                   return
014900111122     C*---------------------------------------------------------------*
015000111122      *  Controlli sull'Anagrafica
015100111122      *---------------------------------------------------------------*
015200111122     c     Ctrl_APD      begSR
015300111122      *
015400111122      * rileva dall'anagrafica
015500111122     c                   eval      apdTIP = 'A'
015600111122     c                   eval      apdPDR = stpPDR
015700111122     c     Key_APD       chain     fiapd01l
015800111122     c                   if        %Found(fiapd01l)
015900111122      *
016000111122     c                   if        apdATB <> *blank
016100111123     c                   movel(p)  'ANNULLATO'   errore
016200111122     c                   else
016300111122      *
016400111122      *-  Esluso da autofatturazione
016500111122     c                   if        apdPDD = 'S'
016600111123     c                   movel(p)  'AUTOFAT'     errore
016700111122     c                   end
016800111122      *
016900111122     c                   endIF
017000111122      *
017100111122     c                   else
017200111123     c                   movel(p)  'NO_APD'      errore
017300111122     c                   end
017400111122      *
017500111122     c                   endSR
017600111122     C*---------------------------------------------------------------*
017700111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
017800111122      *---------------------------------------------------------------*
017900111122     c     WRI_DETTA     begSR
018000111122     C*
018100111205     c                   clear                   WSADT000
018200090424     C*
018300111123     c                   eval      WSFPDR  = stpPDR
018400111129     c                   eval      WSFTSR  =  stpTSR
018500111205     c                   eval      WSFPRG  = stpPRG
018600111205     c                   eval      WSFDDT  =  stpDDT
018700111205     c                   eval      WSFDST  =  stpDST
018800111205     c                   eval      WSFDCN  =  stpDCN
018900111205     c                   eval      WSFDTS  =  stpDTS
019000111205     c                   eval      WSFDUV  =  stpDUV
019100111205     c                   eval      WSFDRC  =  stpDRC
019200111205     c                   eval      WSFNRC  =  stpNRC
019300111205     c                   eval      WSFCDF  =  stpCDF
019400111205     c                   eval      WSFCSF  =  stpCSF
019500111205     c                   eval      WSFFIL  =  stpFIL
019600111205     c                   eval      WSFFILE = 'FIADT00F'
019700111205     c                   eval      WSFREC  = ADT_REC
019800111205     c                   eval      WSFPROGR= 'TNVRA67R'
019900111123     c                   eval      WSFtime = datetime
020000090428     c**
020100090428     c                   eval      WSFNOTE = Errore
020200090424     c**
020300111205     c                   write     WSADT000
020400090424     C*
020500090424     c                   endSR
020600111123     C*---------------------------------------------------------------*
020700111123     OQSYSPRT   E            TESTA            02
020800111205     O                                           10 'TNVRA67R'
020900111123     O                                           25 'Autista'
021000111129     O                                           30 'TSR'
021100111123     O                                           45 'Progr'
021200111123     O                                           55 'Data_Dec'
021300111123     O                                           65 'Data_Sca'
021400111123     O                                           75 'Data_Stp'
021500111123     O                                           85 'Data_Cnv'
021600111129     O                                          100 'N_Rec_FPD'
021700111123     O                                          111 'N O T E'
021800111123     O                       data               132 '  /  /    '
021900111123     O          E            TESTA       1
022000111123     O                                            1 ' '
022100111129     O                                          + 0 'FPD senza FPT '
022200111123     O                                           25 'da fare'
022300111123     O                                          + 2 'ATTENZIONE'
022400111123     O                       ora                130 '  :  :  '
022500111123      *
022600111123     O          E            DETta       1
022700111123     O                       stpPDR        z     25
022800111129     O                       stpTSR              29
022900111205     O                       stpPRG        z     43
023000111205     O                       stpDDT        z     55
023100111205     O                       stpDST        z     65
023200111205     O                       stpDTS        z     75
023300111205     O                       stpDCN        z     85
023400111205     o                       ADT_REC       z    100
023500111123     O                       errore             116
023600111123      ****
023700111123     o          E            FINEST      2
023800111123     o                                            1 ' '
023900111205     o                                          + 0 '*** TNVRA67R ***'
024000111205     o                                          + 9 '***  ADT  Senza  ADD ***'
024100111123     o                                          + 9 '***   FINE  STAMPA  ***'
024200111123     O                       data               + 9 '  /  /    '
024300111123     O                       ora                + 3 '  :  :  '
024400111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
