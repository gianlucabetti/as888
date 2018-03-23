000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX58R *---------------------------------------------------*
000300081110     H*                                                              *
000400111129     H* CONTROLLA i dettagli FPD se NON hanno le relative FGT testate*
000500111129     H*   Tariffe a Dettaglio non a Giornata o a Competenza          *
000600081209     H*                                                              *
000700111123     H* riceve come parametro la Data decorrenza e                   *
000800111123     H*    se deve eseguire l'aggiornamento                          *
000900000000     H*--------------------------------------------------------------*
001000090401     Ffiapd01L  iF   E           K DISK
001100090401      *
001200090424     FwxFGT00S  o  a E             DISK
001300081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400081211      * -------------------------------------------------------------
001500111122     D KPJBA         E DS
001600111129     D dsFPD         E DS                  ExtName(FIFPD00F)
001700081110     d                                     prefix(STP:3)
001800111129     D FPD_REC         s              9s 0
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
006200111129     C+ select RRN(fifPD00f), fifPD00f.*
006300111129     C+     from fifPD00f where fPDsml=' '
006400111129     C+      and fPDtsr in ('C', 'R') and fPDctr <> 999
006500111129     C+      and fPDpdr*1000 + fPDprg
006600111123     C+          NOT in
006700111129     C+ (select fgtpdr*1000 + fgtprg from fifgt00f where fgtsml=' '
006800111129     C+          and fgttsr in ('C', 'R'))
006900111129     C+  ORDER BY fPDPDR
007000111122     C/END-EXEC
007100111122
007200111122     C*=====
007300081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
007400111122     C*=====
007500081110     c                   if        sqlCODE < 0
007600081110     c                   eval      esito = Sqlcode
007700111123     c                   move      esito         esito_ALF
007800081110     c                   dump(a)
007900081110     c                   return
008000081110     c                   end
008100081110     C/EXEC SQL
008200081110     C+ OPEN A1
008300081110     C/END-EXEC
008400111122     C*=====
008500081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008600111122     C*=====
008700081110     c                   if        sqlCODE < 0
008800081110     c                   eval      esito = Sqlcode
008900111123     c                   move      esito         esito_ALF
009000081110     c                   dump(a)
009100081110     c                   return
009200081110     c                   else
009300111122      *
009400081110     c                   except    testa
009500111122      *
009600081110     c                   end
009700090109      * >>>>>>>
009800081209      * resto nel ciclo finchè....
009900081110     C                   DOU       SqlCod < 0
010000081110      * lettura cursore
010100081110     C/EXEC SQL
010200111129     C+ Fetch Next From A1 Into :FPD_REC, :dsFPD
010300081110     C/END-EXEC
010400111122     C*=====
010500111122      *  EoF
010600081110     C                   if        SqlCod = 100
010700081110     c                   Leave
010800111122     C*=====
010900081110      *  x errori
011000081110     C                   ElseIf    SqlCod < 0
011100081110     c                   eval      esito = Sqlcode
011200111123     c                   move      esito         esito_ALF
011300081110     c                   dump(a)
011400081110     c                   leave
011500081110     C                   ENDif
011600090428      *
011700111123      *----------------------------->>>>>
011800111123      *  se sono qui ho letto un record correttamente
011900090428      *----------------------------->>>>>
012000111122      * Controlli su Anagrafica
012100111123     c                   clear                   errore           15
012200111122     c                   exsr      Ctrl_APD
012300090428      *
012400090428      *----------------------------->>>>>
012500111123      *    Segnala
012600111123     c                   if        Errore = *blank
012700111206     c                   Eval      Errore = 'FPD Senza FGT dett'
012800111123     c                   else
012900111123     c                   end
013000111123      *
013100090427     c   Of              except    testa
013200090427     c                   except    detta
013300090428      *----------------------------->>>>>
013400090424      *  scrive il dettaglio su un file
013500090424     c                   exsr      wri_DETTA
013600111123     C                   z-add     *all'1'       esito
013700081110      *
013800081110     C                   ENDDO
013900081110     C/EXEC SQL
014000081110     C+ Close A1
014100081110     C/END-EXEC
014200111123     c                   except    finest
014300111123     C*
014400081110     C                   if        Esito < 0
014500111123     c                   move      esito         esito_ALF
014600081110     c                   return
014700081110     c                   endIF
014800081110     C*
014900111206     c                   move      esito         esito_ALF
015000111206     c                   if        esito = 0
015100111206     c                   clear                   esito_ALF
015200111206     c                   end
015300111206     C*
015400081110     C     FINE          TAG
015500081110     c                   return
015600111122     C*---------------------------------------------------------------*
015700111122      *  Controlli sull'Anagrafica
015800111122      *---------------------------------------------------------------*
015900111122     c     Ctrl_APD      begSR
016000111122      *
016100111122      * rileva dall'anagrafica
016200111122     c                   eval      apdTIP = 'A'
016300111122     c                   eval      apdPDR = stpPDR
016400111122     c     Key_APD       chain     fiapd01l
016500111122     c                   if        %Found(fiapd01l)
016600111122      *
016700111122     c                   if        apdATB <> *blank
016800111123     c                   movel(p)  'ANNULLATO'   errore
016900111122     c                   else
017000111122      *
017100111122      *-  Esluso da autofatturazione
017200111122     c                   if        apdPDD = 'S'
017300111123     c                   movel(p)  'AUTOFAT'     errore
017400111122     c                   end
017500111122      *
017600111122     c                   endIF
017700111122      *
017800111122     c                   else
017900111123     c                   movel(p)  'NO_APD'      errore
018000111122     c                   end
018100111122      *
018200111122     c                   endSR
018300111122     C*---------------------------------------------------------------*
018400111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
018500111122      *---------------------------------------------------------------*
018600111122     c     WRI_DETTA     begSR
018700111122     C*
018800090424     c                   clear                   WSFGT000
018900090424     C*
019000111123     c                   eval      WSFPDR  = stpPDR
019100111129     c                   eval      WSFTSR  =  stpTSR
019200111129     c                   eval      WSFCTR  =  stpCTR
019300090424     c                   eval      WSFPRG  = stpPRG
019400111129     c                   eval      WSFFILE = 'FIFPD00F'
019500111129     c                   eval      WSFREC  = FPD_REC
019600111216     c                   eval      WSFPROGR= 'FICNX58R'
019700111123     c                   eval      WSFtime = datetime
019800090428     c**
019900090428     c                   eval      WSFNOTE = Errore
020000090424     c**
020100090424     c                   write     WSFGT000
020200090424     C*
020300090424     c                   endSR
020400111123     C*---------------------------------------------------------------*
020500111123     OQSYSPRT   E            TESTA            02
020600111216     O                                           10 'FICNX58R'
020700111123     O                                           25 'Autista'
020800111129     O                                           30 'TSR'
020900111129     O                                           38 'Tariffa'
021000111123     O                                           45 'Progr'
021100111123     O                                           55 'Data_Dec'
021200111123     O                                           65 'Data_Sca'
021300111123     O                                           75 'Data_Stp'
021400111123     O                                           85 'Data_Cnv'
021500111129     O                                          100 'N_Rec_FPD'
021600111123     O                                          111 'N O T E'
021700111123     O                       data               132 '  /  /    '
021800111123     O          E            TESTA       1
021900111123     O                                            1 ' '
022000111129     O                                          + 0 'FPD senza FGT '
022100111123     O                                           25 'da fare'
022200111123     O                                          + 2 'ATTENZIONE'
022300111123     O                       ora                130 '  :  :  '
022400111123      *
022500111123     O          E            DETta       1
022600111123     O                       stpPDR        z     25
022700111129     O                       stpTSR              29
022800111129     O                       stpCTR              36
022900111123     O                       stpPRG        z     43
023000111129     o                       FPD_REC       z    100
023100111123     O                       errore             116
023200111123      ****
023300111123     o          E            FINEST      2
023400111123     o                                            1 ' '
023500111216     o                                          + 0 '*** FICNX58R ***'
023600111129     o                                          + 9 '***  FPD  Senza  FGT ***'
023700111123     o                                          + 9 '***   FINE  STAMPA  ***'
023800111123     O                       data               + 9 '  /  /    '
023900111123     O                       ora                + 3 '  :  :  '
024000111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
