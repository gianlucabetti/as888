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
003100120801     c                   seton                                        OF
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
006000111129     C+   select rrn(fitgt00f), fitgt00f.* FROM
006100111129     C+    fitgt00f WHERE
006200120727     C+    tgtsml=' ' and tgtdts = 0 and tgtpdr
006300120726     C+                    IN
006400120726     C+  (select apdPDR from fiapd00f where apdatb=' ' and apdtip ='A'
006500120726     C+   and  APDcsf = :SOCIETAold
006600120726     C+   and  APDksc = :FORNITOREold )
006700111129     C+   order by tgtpdr
006800111122     C/END-EXEC
006900111122
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
009600111129     C+ Fetch Next From A1 Into :TGT_REC, :dsTGT
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
011900111206      *    Segnala
012000111206     c                   if        Errore = *blank
012100120801     c                   Eval      Errore = 'Tariffe Non Confermate'
012200111206     c                   else
012300111206     c                   end
012400111206      *
012500090424      *  scrive il dettaglio su un file
012600111123     c   Of              except    testa
012700111123     c                   except    detta
012800111123      *
012900111123     C                   z-add     *all'1'       esito
013000120727     c                   move      esito         esito_ALF
013100081110      *
013200081110     C                   ENDDO
013300081110     C/EXEC SQL
013400081110     C+ Close A1
013500081110     C/END-EXEC
013600120727     C                   if         esito = *all'1'
013700111123     c                   except    finest
013800120727     c                   end
013900111123     C*
014000081110     C                   if        Esito < 0
014100111123     c                   move      esito         esito_ALF
014200081110     c                   return
014300081110     c                   endIF
014400081110     C*
014500111206     c                   if        esito = 0
014600111206     c                   clear                   esito_ALF
014700111206     c                   end
014800111206     C*
014900081110     C     FINE          TAG
015000081110     c                   return
015100111122     C*---------------------------------------------------------------*
015200111122      *  Controlli sull'Anagrafica
015300111122      *---------------------------------------------------------------*
015400111122     c     Ctrl_APD      begSR
015500120801      *
015600120801      * rileva dall'anagrafica
015700120801     c                   eval      apdTIP = 'A'
015800120801     c                   eval      apdPDR = stpPDR
015900120801     c     Key_APD       chain     fiapd01l
016000120801     c                   if        %Found(fiapd01l)
016100120801      *
016200120801     c                   if        apdATB <> *blank
016300120801     c                   movel(p)  'ANNULLATO'   errore
016400120801     c                   else
016500120801      *
016600120801      *-  Esluso da autofatturazione
016700120801     c                   if        apdPDD = 'S'
016800120801     c                   eval      errore = 'Autista in autofatturazione'
016900120801     c                   end
017000120801      *
017100120801     c                   endIF
017200120801      *
017300120801     c                   else
017400120801     c                   eval      errore = 'Manca anagrafica gestionale'
017500120801     c                   end
017600120801      *
017700111122     c                   endSR
017800111123     C*---------------------------------------------------------------*
017900111123     OQSYSPRT   E            TESTA            02
018000120727     O                                           55 ' '
018100120727     O                                          + 0 'A T T E N Z I O N E'
018200111123     O          E            TESTA       1
018300111123     O                                            1 ' '
018400120727     O                                          + 0 'TARIFFE - AUTISTI di'
018500120727     O                                          + 1 'città NON CONFERMATE'
018600120727     O          E            TESTA       1
018700120727     O                                            1 ' '
018800120801     O                                          + 2 'ATTENZIONE, controllare'
018900120801     O                                          + 2 'e CONVALIDARE prima di'
019000120801     O                                          + 2 'PROCEDERE.'
019100111123     O                       ora                130 '  :  :  '
019200111123      *
019300120727     O          E            TESTA       2  2
019400120727     O                                           10 'FICNY1R3'
019500120727     O                                           25 'Autista'
019600120727     O                                           30 ' '
019700120727     O                                           38 ' '
019800120727     O                                           45 'Progr'
019900120727     O                                           55 'Data_Dec'
020000120727     O                                           65 'Data_Sca'
020100120727     O                                           75 'Data_Stp'
020200120727     O                                           85 'Data_Cnv'
020300120727     O                                          111 'N O T E'
020400120727     O                       data               132 '  /  /    '
020500120727     **
020600111123     O          E            DETta       1
020700111123     O                       stpPDR        z     25
020800111123     O                       stpPRG        z     43
020900111123     O                       stpDDT        z     55
021000111123     O                       stpDST        z     65
021100111123     O                       stpDTS        z     75
021200111123     O                       stpDCN        z     85
021300111123     O                       errore             116
021400111123      ****
021500111123     o          E            FINEST      2
021600111123     o                                            1 ' '
021700120726     o                                          + 0 '*** FICNY1R3 ***'
021800120726     o                                          + 9 '*** TAR.NON CONFERMATE ***'
021900111123     o                                          + 9 '***   FINE  STAMPA  ***'
022000111123     O                       data               + 9 '  /  /    '
022100111123     O                       ora                + 3 '  :  :  '
