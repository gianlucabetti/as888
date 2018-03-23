000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200120726     H* FICNY1R5 *---------------------------------------------------*
000300081110     H*                                                              *
000400111205     H* CONTROLLA quali tariffe non sono state ancora stampate in    *
000500111205     H*   SEDE e cosa si deve fare su queste.                        *
000600081209     H*                                                              *
000700120726     H* riceve come parametro la Data decorrenza                     *
000800000000     H*--------------------------------------------------------------*
000900090401     Ffiapd01L  iF   E           K DISK
001000090401      *
001100081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200081211      * -------------------------------------------------------------
001300111122     D KPJBA         E DS
001400111205     D dsADT         E DS                  ExtName(FIADT00F)
001500081110     d                                     prefix(STP:3)
001600111205     D ADT_REC         s              9s 0
001700090424     D STP_DATSCAD     s              8s 0
001800081230      *
001900111123     D esito           s              9s 0
002000111123     D esito_ALF       s              9a
002100090109      *
002200111123     D DATA_eur        S               D   DATFMT(*eur)
002300111123     D DATA_iso        S               D   DATFMT(*iso)
002400111123     C*------------------------------------------------------------------------*
002500111123     C*        - OPERAZIONI INIZIALI
002600111123     C*------------------------------------------------------------------------*
002700111123     C     *ENTRY        PLIST
002800120726     C                   PARM                    SOCIETAold        3
002900120726     C                   PARM                    FORNITOREold      8
003000120726     C                   PARM                    DATA_decalfa      8
003100111123     C                   PARM                    ESITO_ALF
003200111123     C*
003300120726     C                   movel     DATA_decalfa  DATA_dec          8 0
003400111123     C     *iso          movel     DATA_DEC      data_iso
003500111123     c     data_iso      subdur    1:*days       data_iso
003600111123     C     *iso          movel     data_iso      data_sca          8 0
003700111123     C                   z-add     data_sca      DATAscad          8 0
003800111123     C*
003900111123     c     Key_APD       KLIST
004000111123     c                   kfld                    apdTIP
004100111123     c                   kfld                    apdPDR
004200111123      *
004300111123     c                   clear                   esito
004400111123     c                   clear                   esito_ALF
004500111123     c                   time                    datetime         14 0
004600111123     c                   move      datetime      data              8 0
004700111123     c                   movel     datetime      ora               6 0
004800111123      *
004900111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
005000111123     C                   clear                   UDATE_8           8 0
005100111123     C                   MOVEL     *DATE         UDATE_8
005200111122     c                   clear                   savSTPpdr         7 0
005300081209     C                   SETON                                        LR
005400090423      **
005500090423     C*=================================================================
005600111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
005700090423     C*=================================================================
005800090424      *
005900090422     C/EXEC SQL DECLARE A1 CURSOR FOR
006000111205     C+ select RRN(fiADT00f), fiADT00f.* from fiADT00f where
006100120727     C+       ADtdts=0 and ADTpdr
006200111205     C+                       IN
006300120726     C+  (select apdPDR from fiapd00f where apdatb=' ' and apdtip ='D'
006400120726     C+   and  APDcsf = :SOCIETAold
006500120726     C+   and  APDksc = :FORNITOREold )
006600111205     C+  ORDER BY ADTPDR
006700111122     C/END-EXEC
006800111130
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
009000120727     c                   seton                                        OF
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
011200111123      *----------------------------->>>>>
011300111123      *  se sono qui ho letto un record correttamente
011400090428      *----------------------------->>>>>
011500111122      * Controlli su Anagrafica
011600111123     c                   clear                   errore           15
011700111122     c                   exsr      Ctrl_APD
011800090428      *
011900090428      *----------------------------->>>>>
012000111123      *    Segnala
012100111123     c                   if        Errore = *blank
012200111205     c                   Eval      Errore = 'ADT NO STAMPA'
012300111123     c                   else
012400111123     c                   end
012500111123      *
012600090427     c   Of              except    testa
012700090427     c                   except    detta
012800120726      *-
012900111123     C                   z-add     *all'1'       esito
013000120727     c                   move      esito         esito_ALF
013100081110      *
013200081110     C                   ENDDO
013300081110     C/EXEC SQL
013400081110     C+ Close A1
013500081110     C/END-EXEC
013600120727     C                   IF         esito = *all'1'
013700111123     c                   except    finest
013800120727     c                   end
013900111123     C*
014000081110     C                   if        Esito < 0
014100111123     c                   move      esito         esito_ALF
014200081110     c                   return
014300081110     c                   endIF
014400081110     C*
014500081110     C     FINE          TAG
014600081110     c                   return
014700111122     C*---------------------------------------------------------------*
014800111122      *  Controlli sull'Anagrafica
014900111122      *---------------------------------------------------------------*
015000111122     c     Ctrl_APD      begSR
015100111122      *
015200111122      * rileva dall'anagrafica
015300111205     c                   eval      apdTIP = 'D'
015400111122     c                   eval      apdPDR = stpPDR
015500111122     c     Key_APD       chain     fiapd01l
015600111122     c                   if        %Found(fiapd01l)
015700111122      *
015800111122     c                   if        apdATB <> *blank
015900111123     c                   movel(p)  'ANNULLATO'   errore
016000111122     c                   else
016100111122      *
016200111122      *-  Esluso da autofatturazione
016300111122     c                   if        apdPDD = 'S'
016400111123     c                   movel(p)  'AUTOFAT'     errore
016500111122     c                   end
016600111122      *
016700111122     c                   endIF
016800111122      *
016900111122     c                   else
017000111123     c                   movel(p)  'NO_APD'      errore
017100111122     c                   end
017200111122      *
017300111122     c                   endSR
017400111205     C*---------------------------------------------------------------*
017500111205     OQSYSPRT   E            TESTA            02
017600120727     O                                           55 ' '
017700120727     O                                          + 0 'A T T E N Z I O N E'
017800120727     O          E            TESTA       1
017900120727     O                                            1 ' '
018000120727     O                                          + 0 'TARIFFE - AUTISTI Affl.'
018100120727     O                                          + 1 'Defl. NON CONFERMATE'
018200120727     O          E            TESTA       1
018300120727     O                                            1 ' '
018400120727     O                                          + 0 'Bisogna fare '
018500120727     O                                          + 2 'ATTENZIONE, controllarle'
018600120727     O                                          + 2 'e CONVALIDARLE prima di'
018700120727     O                                          + 2 'poter PROCEDERE.'
018800120727     O                       ora                130 '  :  :  '
018900120727      *
019000120727     O          E            TESTA       2  2
019100120726     O                                           10 'FICNY1R5'
019200111205     O                                           25 'Autista'
019300111205     O                                           30 'TSR'
019400111205     O                                           45 'Progr'
019500111205     O                                           55 'Data_Dec'
019600111205     O                                           65 'Data_Sca'
019700111205     O                                           75 'Data_Stp'
019800111205     O                                           85 'Data_Cnv'
019900111205     O                                          100 'N_Rec_ADT'
020000111205     O                                          111 'N O T E'
020100111205     O                       data               132 '  /  /    '
020200111205      *
020300111205     O          E            DETta       1
020400111205     O                       stpPDR        z     25
020500111205     O                       stpTSR              29
020600111205     O                       stpPRG        z     43
020700111205     O                       stpDDT        z     55
020800111205     O                       stpDST        z     65
020900111205     O                       stpDTS        z     75
021000111205     O                       stpDCN        z     85
021100111205     o                       ADT_REC       z    100
021200111205     O                       errore             116
021300111205      ****
021400111205     o          E            FINEST      2
021500111205     o                                            1 ' '
021600120726     o                                          + 0 '*** FICNY1R5 ***'
021700120727     o                                          + 5 '** Tariffe non STAMPATE **'
021800120727     o                                          + 2 '** Affluenze  Defluenze **'
021900120727     o                                          + 3 '***   FINE  STAMPA  ***'
022000120727     O                       data               + 5 '  /  /    '
022100120727     O                       ora                + 3 '  :  :  '
