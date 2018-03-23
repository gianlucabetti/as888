000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX70R *---------------------------------------------------*
000300081110     H*                                                              *
000400111123     H* Controlla se ci sono delle Tariffe x il Futuro già inserite
000500081209     H*                                                              *
000600111122     H* riceve come parametro la Data decorrenza e la data Scadenza  *
000700111122     H*  e se deve eseguire l'aggiornamento                          *
000800000000     H*--------------------------------------------------------------*
000900090401     Ffiapd01L  iF   E           K DISK
001000090401      *
001100111205     FwxADT00S  o  a E             DISK
001200081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300081211      * -------------------------------------------------------------
001400111122     D KPJBA         E DS
001500111205     D dsADT         E DS                  ExtName(FIADT00F)
001600081110     d                                     prefix(STP:3)
001700111205     D ADT_REC         s              9s 0
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
006100111205     C+   select rrn(fiADT00f), fiADT00f.* FROM
006200111205     C+   fiADT00f WHERE
006300111213     C+        ADTddt >= :DATA_dec and ADTPCAR >0
006400111205     C+   order by ADTpdr
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
009700111205     C+ Fetch Next From A1 Into :ADT_REC, :dsADT
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
011200111206      *
011300111123      *----------------------------->>>>>
011400111123      *  se sono qui ho letto un record correttamente
011500090428      *----------------------------->>>>>
011600111122      * Controlli su Anagrafica
011700111123     c                   clear                   errore           15
011800111122     c                   exsr      Ctrl_APD
011900090428      *
012000090428      *----------------------------->>>>>
012100111206     c                   if        errore = *blank
012200111206     c                   movel(p)  'TAR.FUTURA'  errore
012300111206     c                   end
012400111206     c**
012500090424      *  scrive il dettaglio su un file
012600111123     c   Of              except    testa
012700111123     c                   except    detta
012800111123      *
012900090424     c                   exsr      wri_DETTA
013000111123     C                   z-add     *all'1'       esito
013100081110      *
013200081110     C                   ENDDO
013300081110     C/EXEC SQL
013400081110     C+ Close A1
013500081110     C/END-EXEC
013600111123     c                   except    finest
013700111123     C*
013800081110     C                   if        Esito < 0
013900111123     c                   move      esito         esito_ALF
014000081110     c                   return
014100081110     c                   endIF
014200081110     C*
014300111206     c                   move      esito         esito_ALF
014400111206     c                   if        esito = 0
014500111206     c                   clear                   esito_ALF
014600111206     c                   end
014700111206     C*
014800081110     C     FINE          TAG
014900081110     c                   return
015000111122     C*---------------------------------------------------------------*
015100111122      *  Controlli sull'Anagrafica
015200111122      *---------------------------------------------------------------*
015300111122     c     Ctrl_APD      begSR
015400111122      *
015500111122      * rileva dall'anagrafica
015600111205     c                   eval      apdTIP = 'D'
015700111122     c                   eval      apdPDR = stpPDR
015800111122     c     Key_APD       chain     fiapd01l
015900111122     c                   if        %Found(fiapd01l)
016000111122      *
016100111122     c                   if        apdATB <> *blank
016200111123     c                   movel(p)  'ANNULLATO'   errore
016300111122     c                   else
016400111122      *
016500111122      *-  Esluso da autofatturazione
016600111122     c                   if        apdPDD = 'S'
016700111123     c                   movel(p)  'AUTOFAT'     errore
016800111122     c                   end
016900111122      *
017000111122     c                   endIF
017100111122      *
017200111122     c                   else
017300111123     c                   movel(p)  'NO_APD'      errore
017400111122     c                   end
017500111122      *
017600111122     c                   endSR
017700111122     C*---------------------------------------------------------------*
017800111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
017900111122      *---------------------------------------------------------------*
018000111205     c     WRI_DETTA     begSR
018100111205     C*
018200111205     c                   clear                   WSADT000
018300111205     C*
018400111205     c                   eval      WSFPDR  = stpPDR
018500111205     c                   eval      WSFTSR  =  stpTSR
018600111205     c                   eval      WSFPRG  = stpPRG
018700111205     c                   eval      WSFDDT  =  stpDDT
018800111205     c                   eval      WSFDST  =  stpDST
018900111205     c                   eval      WSFDCN  =  stpDCN
019000111205     c                   eval      WSFDTS  =  stpDTS
019100111205     c                   eval      WSFDUV  =  stpDUV
019200111205     c                   eval      WSFDRC  =  stpDRC
019300111205     c                   eval      WSFNRC  =  stpNRC
019400111205     c                   eval      WSFCDF  =  stpCDF
019500111205     c                   eval      WSFCSF  =  stpCSF
019600111205     c                   eval      WSFFIL  =  stpFIL
019700111205     c                   eval      WSFFILE = 'FIADT00F'
019800111205     c                   eval      WSFREC  = ADT_REC
019900111216     c                   eval      WSFPROGR= 'FICNX70R'
020000111205     c                   eval      WSFtime = datetime
020100111205     c                   eval      WSFNOTE = Errore
020200111205     c**
020300111205     c                   write     WSADT000
020400111205     C*
020500111205     c                   endSR
020600111205     C*---------------------------------------------------------------*
020700111205     OQSYSPRT   E            TESTA            02
020800111216     O                                           10 'FICNX70R'
020900111205     O                                           25 'Autista'
021000111205     O                                           30 'TSR'
021100111205     O                                           45 'Progr'
021200111205     O                                           55 'Data_Dec'
021300111205     O                                           65 'Data_Sca'
021400111205     O                                           75 'Data_Stp'
021500111205     O                                           85 'Data_Cnv'
021600111205     O                                          100 'N_Rec_ADT'
021700111205     O                                          111 'N O T E'
021800111205     O                       data               132 '  /  /    '
021900111205     O          E            TESTA       1
022000111205     O                                            1 ' '
022100111205     O                                          + 0 'TARIFFE FUTURE'
022200111205     O                                           25 'da fare'
022300111205     O                                          + 2 'ATTENZIONE'
022400111205     O                       ora                130 '  :  :  '
022500111205      *
022600111205     O          E            DETta       1
022700111205     O                       stpPDR        z     25
022800111205     O                       stpTSR              29
022900111205     O                       stpPRG        z     43
023000111205     O                       stpDDT        z     55
023100111205     O                       stpDST        z     65
023200111205     O                       stpDTS        z     75
023300111205     O                       stpDCN        z     85
023400111205     o                       ADT_REC       z    100
023500111205     O                       errore             116
023600111205      ****
023700111205     o          E            FINEST      2
023800111205     o                                            1 ' '
023900111216     o                                          + 0 '*** FICNX70R ***'
024000111205     o                                          + 9 '***  TARIFFE FUTURE  ***'
024100111205     o                                          + 9 '***   FINE  STAMPA  ***'
024200111205     O                       data               + 9 '  /  /    '
024300111205     O                       ora                + 3 '  :  :  '
024400111205     o               44                         132 ' ** NO AGGIORNAMENTO **'
