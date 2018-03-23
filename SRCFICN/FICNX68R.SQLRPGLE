000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200111216     H* FICNX68R *---------------------------------------------------*
000300081110     H*                                                              *
000400111205     H* CONTROLLA dettagli ADD che non hanno corrispondente ADT      *
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
001600111205     D dsADD         E DS                  ExtName(FIADD00F)
001700081110     d                                     prefix(STP:3)
001800111205     D ADD_REC         s              9s 0
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
006200111205     C+ select RRN(fiADD00f), fiADD00f.*
006300111205     C+     from fiADD00f
006400111205     C+     where addpdr*1000 + addprg
006500111205     C+     not in   (select adtpdr*1000 + adtprg from fiadt00f)
006600111205     C+     ORDER BY ADDPDR
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
009900111205     C+ Fetch Next From A1 Into :ADD_REC, :dsADD
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
012400111205     c                   Eval      Errore = 'ADD Senza ADT'
012500111123     c                   else
012600111123     c                   end
012700111123      *
012800090427     c   Of              except    testa
012900090427     c                   except    detta
013000090428      *----------------------------->>>>>
013100090424      *  scrive il dettaglio su un file
013200090424     c                   exsr      wri_DETTA
013300111123     C                   z-add     *all'1'       esito
013400081110      *
013500081110     C                   ENDDO
013600081110     C/EXEC SQL
013700081110     C+ Close A1
013800081110     C/END-EXEC
013900111123     c                   except    finest
014000111123     C*
014100081110     C                   if        Esito < 0
014200111123     c                   move      esito         esito_ALF
014300081110     c                   return
014400081110     c                   endIF
014500081110     C*
014600111206     c                   move      esito         esito_ALF
014700111206     c                   if        esito = 0
014800111206     c                   clear                   esito_ALF
014900111206     c                   end
015000111206     C*
015100081110     C     FINE          TAG
015200081110     c                   return
015300111122     C*---------------------------------------------------------------*
015400111122      *  Controlli sull'Anagrafica
015500111122      *---------------------------------------------------------------*
015600111122     c     Ctrl_APD      begSR
015700111122      *
015800111122      * rileva dall'anagrafica
015900111205     c                   eval      apdTIP = 'D'
016000111122     c                   eval      apdPDR = stpPDR
016100111122     c     Key_APD       chain     fiapd01l
016200111122     c                   if        %Found(fiapd01l)
016300111122      *
016400111122     c                   if        apdATB <> *blank
016500111123     c                   movel(p)  'ANNULLATO'   errore
016600111122     c                   else
016700111122      *
016800111122      *-  Esluso da autofatturazione
016900111122     c                   if        apdPDD = 'S'
017000111123     c                   movel(p)  'AUTOFAT'     errore
017100111122     c                   end
017200111122      *
017300111122     c                   endIF
017400111122      *
017500111122     c                   else
017600111123     c                   movel(p)  'NO_APD'      errore
017700111122     c                   end
017800111122      *
017900111122     c                   endSR
018000111122     C*---------------------------------------------------------------*
018100111122      *  Scrive il Dettaglio che sta stampando x il controllo finale
018200111122      *---------------------------------------------------------------*
018300111122     c     WRI_DETTA     begSR
018400111122     C*
018500111205     c                   clear                   WSADT000
018600090424     C*
018700111123     c                   eval      WSFPDR  = stpPDR
018800111129     c                   eval      WSFTSR  =  stpTSR
018900111205     c                   eval      WSFPRG  = stpPRG
019000111205     c                   eval      WSFFILE = 'FIADD00F'
019100111205     c                   eval      WSFREC  = ADD_REC
019200111216     c                   eval      WSFPROGR= 'FICNX68R'
019300111123     c                   eval      WSFtime = datetime
019400090428     c**
019500090428     c                   eval      WSFNOTE = Errore
019600090424     c**
019700111205     c                   write     WSADT000
019800090424     C*
019900090424     c                   endSR
020000111123     C*---------------------------------------------------------------*
020100111123     OQSYSPRT   E            TESTA            02
020200111216     O                                           10 'FICNX68R'
020300111123     O                                           25 'Autista'
020400111129     O                                           30 'TSR'
020500111123     O                                           45 'Progr'
020600111205     O                                          100 'N_Rec_ADD'
020700111123     O                                          111 'N O T E'
020800111123     O                       data               132 '  /  /    '
020900111123     O          E            TESTA       1
021000111123     O                                            1 ' '
021100111205     O                                          + 0 'ADD senza ADT '
021200111123     O                                           25 'da fare'
021300111123     O                                          + 2 'ATTENZIONE'
021400111123     O                       ora                130 '  :  :  '
021500111123      *
021600111123     O          E            DETta       1
021700111123     O                       stpPDR        z     25
021800111129     O                       stpTSR              29
021900111205     O                       stpPRG        z     43
022000111205     o                       ADD_REC       z    100
022100111123     O                       errore             116
022200111123      ****
022300111123     o          E            FINEST      2
022400111123     o                                            1 ' '
022500111216     o                                          + 0 '*** FICNX68R ***'
022600111205     o                                          + 9 '***  ADD  Senza  ADT ***'
022700111123     o                                          + 9 '***   FINE  STAMPA  ***'
022800111123     O                       data               + 9 '  /  /    '
022900111123     O                       ora                + 3 '  :  :  '
023000111123     o               44                         132 ' ** NO AGGIORNAMENTO **'
