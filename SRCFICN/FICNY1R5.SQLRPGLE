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
003200120801     c                   seton                                        OF
003300111123     C*
003400120726     C                   movel     DATA_decalfa  DATA_dec          8 0
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
006100111205     C+ select RRN(fiADT00f), fiADT00f.* from fiADT00f where
006200120727     C+       ADtdts=0 and ADTpdr
006300111205     C+                       IN
006400120726     C+  (select apdPDR from fiapd00f where apdatb=' ' and apdtip ='D'
006500120726     C+   and  APDcsf = :SOCIETAold
006600120726     C+   and  APDksc = :FORNITOREold )
006700111205     C+  ORDER BY ADTPDR
006800111122     C/END-EXEC
006900111130
007000111122     C*=====
007100081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
007200111122     C*=====
007300081110     c                   if        sqlCODE < 0
007400081110     c                   eval      esito = Sqlcode
007500111123     c                   move      esito         esito_ALF
007600081110     c                   dump(a)
007700081110     c                   return
007800081110     c                   end
007900081110     C/EXEC SQL
008000081110     C+ OPEN A1
008100081110     C/END-EXEC
008200111122     C*=====
008300081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008400111122     C*=====
008500081110     c                   if        sqlCODE < 0
008600081110     c                   eval      esito = Sqlcode
008700111123     c                   move      esito         esito_ALF
008800081110     c                   dump(a)
008900081110     c                   return
009000081110     c                   end
009100090109      * >>>>>>>
009200081209      * resto nel ciclo finchè....
009300081110     C                   DOU       SqlCod < 0
009400081110      * lettura cursore
009500081110     C/EXEC SQL
009600111205     C+ Fetch Next From A1 Into :ADT_REC, :dsADT
009700081110     C/END-EXEC
009800111122     C*=====
009900111122      *  EoF
010000081110     C                   if        SqlCod = 100
010100081110     c                   Leave
010200111122     C*=====
010300081110      *  x errori
010400081110     C                   ElseIf    SqlCod < 0
010500081110     c                   eval      esito = Sqlcode
010600111123     c                   move      esito         esito_ALF
010700081110     c                   dump(a)
010800081110     c                   leave
010900081110     C                   ENDif
011000090428      *
011100111123      *----------------------------->>>>>
011200111123      *  se sono qui ho letto un record correttamente
011300090428      *----------------------------->>>>>
011400111122      * Controlli su Anagrafica
011500120801     c                   clear                   errore           25
011600111122     c                   exsr      Ctrl_APD
011700090428      *
011800090428      *----------------------------->>>>>
011900111123      *    Segnala
012000111123     c                   if        Errore = *blank
012100120801     c                   Eval      Errore = 'Tariffa non convalidata'
012200111123     c                   else
012300111123     c                   end
012400111123      *
012500090427     c   Of              except    testa
012600090427     c                   except    detta
012700120726      *-
012800111123     C                   z-add     *all'1'       esito
012900120727     c                   move      esito         esito_ALF
013000081110      *
013100081110     C                   ENDDO
013200081110     C/EXEC SQL
013300081110     C+ Close A1
013400081110     C/END-EXEC
013500120727     C                   IF         esito = *all'1'
013600111123     c                   except    finest
013700120727     c                   end
013800111123     C*
013900081110     C                   if        Esito < 0
014000111123     c                   move      esito         esito_ALF
014100081110     c                   return
014200081110     c                   endIF
014300081110     C*
014400081110     C     FINE          TAG
014500081110     c                   return
014600111122     C*---------------------------------------------------------------*
014700111122      *  Controlli sull'Anagrafica
014800111122      *---------------------------------------------------------------*
014900111122     c     Ctrl_APD      begSR
015000120801      *
015100120801      * rileva dall'anagrafica
015200120801     c                   eval      apdTIP = 'A'
015300120801     c                   eval      apdPDR = stpPDR
015400120801     c     Key_APD       chain     fiapd01l
015500120801     c                   if        %Found(fiapd01l)
015600120801      *
015700120801     c                   if        apdATB <> *blank
015800120801     c                   movel(p)  'ANNULLATO'   errore
015900120801     c                   else
016000120801      *
016100120801      *-  Esluso da autofatturazione
016200120801     c                   if        apdPDD = 'S'
016300120801     c                   eval      errore = 'Autista in autofatturazione'
016400120801     c                   end
016500120801      *
016600120801     c                   endIF
016700120801      *
016800120801     c                   else
016900120801     c                   eval      errore = 'Manca anagrafica gestionale'
017000120801     c                   end
017100120801      *
017200111122     c                   endSR
017300111205     C*---------------------------------------------------------------*
017400111205     OQSYSPRT   E            TESTA            02
017500120727     O                                           55 ' '
017600120727     O                                          + 0 'A T T E N Z I O N E'
017700120727     O          E            TESTA       1
017800120727     O                                            1 ' '
017900120727     O                                          + 0 'TARIFFE - AUTISTI Affl.'
018000120727     O                                          + 1 'Defl. NON CONFERMATE'
018100120727     O          E            TESTA       1
018200120727     O                                            1 ' '
018300120727     O                                          + 0 'Bisogna fare '
018400120727     O                                          + 2 'ATTENZIONE, controllarle'
018500120727     O                                          + 2 'e CONVALIDARLE prima di'
018600120727     O                                          + 2 'poter PROCEDERE.'
018700120727     O                       ora                130 '  :  :  '
018800120727      *
018900120727     O          E            TESTA       2  2
019000120726     O                                           10 'FICNY1R5'
019100111205     O                                           25 'Autista'
019200111205     O                                           30 'TSR'
019300111205     O                                           45 'Progr'
019400111205     O                                           55 'Data_Dec'
019500111205     O                                           65 'Data_Sca'
019600111205     O                                           75 'Data_Stp'
019700111205     O                                           85 'Data_Cnv'
019800111205     O                                          111 'N O T E'
019900111205     O                       data               132 '  /  /    '
020000111205      *
020100111205     O          E            DETta       1
020200111205     O                       stpPDR        z     25
020300111205     O                       stpTSR              29
020400111205     O                       stpPRG        z     43
020500111205     O                       stpDDT        z     55
020600111205     O                       stpDST        z     65
020700111205     O                       stpDTS        z     75
020800111205     O                       stpDCN        z     85
020900111205     O                       errore             116
021000111205      ****
021100111205     o          E            FINEST      2
021200111205     o                                            1 ' '
021300120726     o                                          + 0 '*** FICNY1R5 ***'
021400120727     o                                          + 5 '** Tariffe non STAMPATE **'
021500120727     o                                          + 2 '** Affluenze  Defluenze **'
021600120727     o                                          + 3 '***   FINE  STAMPA  ***'
021700120727     O                       data               + 5 '  /  /    '
021800120727     O                       ora                + 3 '  :  :  '
