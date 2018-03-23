000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200120726     H* FICNY1R3 *---------------------------------------------------*
000300081110     H*                                                              *
000400120726     H* Controlla se ci sono delle Testate non CONFERMATE
000500081209     H*                                                              *
000600120726     H* riceve come parametro la Data decorrenza                     *
000700000000     H*--------------------------------------------------------------*
000800090401     Ffiapd01L  iF   E           K DISK
000900090401      *
001000081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100081211      * -------------------------------------------------------------
001200111122     D KPJBA         E DS
001300111129     D dsTGT         E DS                  ExtName(FITGT00F)
001400081110     d                                     prefix(STP:3)
001500111129     D TGT_REC         s              9s 0
001600090424     D STP_DATSCAD     s              8s 0
001700081230      *
001800111123     D esito           s              9s 0
001900111123     D esito_ALF       s              9a
002000090109      *
002100111123     D DATA_eur        S               D   DATFMT(*eur)
002200111123     D DATA_iso        S               D   DATFMT(*iso)
002300111123     C*------------------------------------------------------------------------*
002400111123     C*        - OPERAZIONI INIZIALI
002500111123     C*------------------------------------------------------------------------*
002600111123     C     *ENTRY        PLIST
002700120726     C                   PARM                    SOCIETAold        3
002800120726     C                   PARM                    FORNITOREold      8
002900120726     C                   PARM                    DATA_decalfa      8
003000111123     C                   PARM                    ESITO_ALF
003100111123     C*
003200120726     C                   movel     DATA_decalfa  DATA_dec          8 0
003300111123     C     *iso          movel     DATA_DEC      data_iso
003400111123     c     data_iso      subdur    1:*days       data_iso
003500111123     C     *iso          movel     data_iso      data_sca          8 0
003600111123     C                   z-add     data_sca      DATAscad          8 0
003700111123     C*
003800111123     c     Key_APD       KLIST
003900111123     c                   kfld                    apdTIP
004000111123     c                   kfld                    apdPDR
004100111123      *
004200111123     c                   clear                   esito
004300111123     c                   clear                   esito_ALF
004400111123     c                   time                    datetime         14 0
004500111123     c                   move      datetime      data              8 0
004600111123     c                   movel     datetime      ora               6 0
004700111123      *
004800111123     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
004900111123     C                   clear                   UDATE_8           8 0
005000111123     C                   MOVEL     *DATE         UDATE_8
005100111122     c                   clear                   savSTPpdr         7 0
005200081209     C                   SETON                                        LR
005300090423      **
005400090423     C*=================================================================
005500111122      **  Sono le Tariffe degli AUT Attive alla data Scadenza
005600090423     C*=================================================================
005700090424      *
005800090422     C/EXEC SQL DECLARE A1 CURSOR FOR
005900111129     C+   select rrn(fitgt00f), fitgt00f.* FROM
006000111129     C+    fitgt00f WHERE
006100120727     C+    tgtsml=' ' and tgtdts = 0 and tgtpdr
006200120726     C+                    IN
006300120726     C+  (select apdPDR from fiapd00f where apdatb=' ' and apdtip ='A'
006400120726     C+   and  APDcsf = :SOCIETAold
006500120726     C+   and  APDksc = :FORNITOREold )
006600111129     C+   order by tgtpdr
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
009000120727     c                   seton                                        OF
009100081110     c                   end
009200090109      * >>>>>>>
009300081209      * resto nel ciclo finchè....
009400081110     C                   DOU       SqlCod < 0
009500081110      * lettura cursore
009600081110     C/EXEC SQL
009700111129     C+ Fetch Next From A1 Into :TGT_REC, :dsTGT
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
012000111206      *    Segnala
012100111206     c                   if        Errore = *blank
012200120726     c                   Eval      Errore = 'Non CONFERMATE'
012300111206     c                   else
012400111206     c                   end
012500111206      *
012600090424      *  scrive il dettaglio su un file
012700111123     c   Of              except    testa
012800111123     c                   except    detta
012900111123      *
013000111123     C                   z-add     *all'1'       esito
013100120727     c                   move      esito         esito_ALF
013200081110      *
013300081110     C                   ENDDO
013400081110     C/EXEC SQL
013500081110     C+ Close A1
013600081110     C/END-EXEC
013700120727     C                   if         esito = *all'1'
013800111123     c                   except    finest
013900120727     c                   end
014000111123     C*
014100081110     C                   if        Esito < 0
014200111123     c                   move      esito         esito_ALF
014300081110     c                   return
014400081110     c                   endIF
014500081110     C*
014600111206     c                   if        esito = 0
014700111206     c                   clear                   esito_ALF
014800111206     c                   end
014900111206     C*
015000081110     C     FINE          TAG
015100081110     c                   return
015200111122     C*---------------------------------------------------------------*
015300111122      *  Controlli sull'Anagrafica
015400111122      *---------------------------------------------------------------*
015500111122     c     Ctrl_APD      begSR
015600111122      *
015700111122      * rileva dall'anagrafica
015800111122     c                   eval      apdTIP = 'A'
015900111122     c                   eval      apdPDR = stpPDR
016000111122     c     Key_APD       chain     fiapd01l
016100111122     c                   if        %Found(fiapd01l)
016200111122      *
016300111122     c                   if        apdATB <> *blank
016400111123     c                   movel(p)  'ANNULLATO'   errore
016500111122     c                   else
016600111122      *
016700111122      *-  Esluso da autofatturazione
016800111122     c                   if        apdPDD = 'S'
016900111123     c                   movel(p)  'AUTOFAT'     errore
017000111122     c                   end
017100111122      *
017200111122     c                   endIF
017300111122      *
017400111122     c                   else
017500111123     c                   movel(p)  'NO_APD'      errore
017600111122     c                   end
017700111122      *
017800111122     c                   endSR
017900111123     C*---------------------------------------------------------------*
018000111123     OQSYSPRT   E            TESTA            02
018100120727     O                                           55 ' '
018200120727     O                                          + 0 'A T T E N Z I O N E'
018300111123     O          E            TESTA       1
018400111123     O                                            1 ' '
018500120727     O                                          + 0 'TARIFFE - AUTISTI di'
018600120727     O                                          + 1 'città NON CONFERMATE'
018700120727     O          E            TESTA       1
018800120727     O                                            1 ' '
018900120727     O                                          + 0 'Bisogna fare '
019000120727     O                                          + 2 'ATTENZIONE, controllarle'
019100120727     O                                          + 2 'e CONVALIDARLE prima di'
019200120727     O                                          + 2 'poter PROCEDERE.'
019300111123     O                       ora                130 '  :  :  '
019400111123      *
019500120727     O          E            TESTA       2  2
019600120727     O                                           10 'FICNY1R3'
019700120727     O                                           25 'Autista'
019800120727     O                                           30 ' '
019900120727     O                                           38 ' '
020000120727     O                                           45 'Progr'
020100120727     O                                           55 'Data_Dec'
020200120727     O                                           65 'Data_Sca'
020300120727     O                                           75 'Data_Stp'
020400120727     O                                           85 'Data_Cnv'
020500120727     O                                          100 'N_Rec_TGT'
020600120727     O                                          111 'N O T E'
020700120727     O                       data               132 '  /  /    '
020800120727     **
020900111123     O          E            DETta       1
021000111123     O                       stpPDR        z     25
021100111123     O                       stpPRG        z     43
021200111123     O                       stpDDT        z     55
021300111123     O                       stpDST        z     65
021400111123     O                       stpDTS        z     75
021500111123     O                       stpDCN        z     85
021600111129     o                       TGT_REC       z    100
021700111123     O                       errore             116
021800111123      ****
021900111123     o          E            FINEST      2
022000111123     o                                            1 ' '
022100120726     o                                          + 0 '*** FICNY1R3 ***'
022200120726     o                                          + 9 '*** TAR.NON CONFERMATE ***'
022300111123     o                                          + 9 '***   FINE  STAMPA  ***'
022400111123     O                       data               + 9 '  /  /    '
022500111123     O                       ora                + 3 '  :  :  '
