000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200120726     H* FICNY1R1 *---------------------------------------------------*
000300081110     H*                                                              *
000400111123     H* Controlla se ci sono delle Tariffe x il Futuro già inserite
000500081209     H*                                                              *
000600120726     H* riceve come parametro la Data decorrenza , SOC e FORNITORE   *
000700000000     H*--------------------------------------------------------------*
000800090401     Ffiapd01L  iF   E           K DISK
000900090401      *
001000081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100081211      * -------------------------------------------------------------
001200111122     D KPJBA         E DS
001300081110     D dsFGT         E DS                  ExtName(FIFGT00F)
001400081110     d                                     prefix(STP:3)
001500081110     D FGT_REC         s              9s 0
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
005900111122     C+   select rrn(fifgt00f), fifgt00f.* FROM
006000111122     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
006100111123     C+        and fgtsml=' ' and fgtddt >= :DATA_dec
006200111213     C+               and  fgtPDR * 1000 + fgtPRG
006300111213     C+                          IN
006400111213     C+    (Select tgtpdr * 1000 + tgtprg from fiTGT00F
006500120726     C+       where TGTsml=' '  and  TGTsoc = :SOCIETAold
006600120726     C+         and TGTcdf = :FORNITOREold)
006700111122     C+   order by Fgtpdr
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
009000081110     c                   else
009100120727     c                   seton                                        OF
009200081110     c                   end
009300090109      * >>>>>>>
009400081209      * resto nel ciclo finchè....
009500081110     C                   DOU       SqlCod < 0
009600081110      * lettura cursore
009700081110     C/EXEC SQL
009800111122     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
009900081110     C/END-EXEC
010000111122     C*=====
010100111122      *  EoF
010200081110     C                   if        SqlCod = 100
010300081110     c                   Leave
010400111122     C*=====
010500081110      *  x errori
010600081110     C                   ElseIf    SqlCod < 0
010700081110     c                   eval      esito = Sqlcode
010800111123     c                   move      esito         esito_ALF
010900081110     c                   dump(a)
011000081110     c                   leave
011100081110     C                   ENDif
011200090428      *
011300111123      *----------------------------->>>>>
011400111123      *  se sono qui ho letto un record correttamente
011500090428      *----------------------------->>>>>
011600111122      * Controlli su Anagrafica
011700111123     c                   clear                   errore           15
011800111122     c                   exsr      Ctrl_APD
011900090428      *
012000090428      *----------------------------->>>>>
012100111206      *    Segnala
012200111206     c                   if        Errore = *blank
012300111206     c                   Eval      Errore = 'FGT dec.FUTURA'
012400111206     c                   else
012500111206     c                   end
012600111206      *
012700090424      *  scrive il dettaglio su un file
012800111123     c   Of              except    testa
012900111123     c                   except    detta
013000111123      *
013100111123     C                   z-add     *all'1'       esito
013200120727     c                   move      esito         esito_ALF
013300081110      *
013400081110     C                   ENDDO
013500081110     C/EXEC SQL
013600081110     C+ Close A1
013700081110     C/END-EXEC
013800120727     C                   if         esito = *all'1'
013900120727     c                   except    finest
014000120727     c                   end
014100111123     C*
014200081110     C                   if        Esito < 0
014300111123     c                   move      esito         esito_ALF
014400081110     c                   return
014500081110     c                   endIF
014600081110     C*
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
015900111122     c                   eval      apdTIP = 'A'
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
018000111123     C*---------------------------------------------------------------*
018100111123     OQSYSPRT   E            TESTA            02
018200120727     O                                           61 ' '
018300120727     O                                          + 0 'A V V I S A R E    C E D'
018400111123     O          E            TESTA       1
018500111123     O                                            1 ' '
018600111123     O                                          + 0 'TARIFFE FUTURE'
018700111123     O                                           25 'da fare'
018800111123     O                                          + 2 'ATTENZIONE'
018900111123     O                       ora                130 '  :  :  '
019000120727     O          E            TESTA       2  2
019100120727     O                                           10 'FICNY1R1'
019200120727     O                                           25 'Autista'
019300120727     O                                           30 'TSR'
019400120727     O                                           38 'Tariffa'
019500120727     O                                           45 'Progr'
019600120727     O                                           55 'Data_Dec'
019700120727     O                                           65 'Data_Sca'
019800120727     O                                           75 'Data_Stp'
019900120727     O                                           85 'Data_Cnv'
020000120727     O                                          100 'N_Rec_FGT'
020100120727     O                                          111 'N O T E'
020200120727     O                       data               132 '  /  /    '
020300111123      *
020400111123     O          E            DETta       1
020500111123     O                       stpPDR        z     25
020600111123     O                       stpTSR              29
020700111123     O                       stpCTR              36
020800111123     O                       stpPRG        z     43
020900111123     O                       stpDDT        z     55
021000111123     O                       stpDST        z     65
021100111123     O                       stpDTS        z     75
021200111123     O                       stpDCN        z     85
021300111123     o                       FGT_REC       z    100
021400111123     O                       errore             116
021500111123      ****
021600120727     o          E            FINEST      2  2
021700111123     o                                            1 ' '
021800120726     o                                          + 0 '*** FICNY1R1 ***'
021900111123     o                                          + 9 '***  TARIFFE FUTURE  ***'
022000111123     o                                          + 9 '***   FINE  STAMPA  ***'
022100111123     O                       data               + 9 '  /  /    '
022200111123     O                       ora                + 3 '  :  :  '
022300120727     o          E            FINEST      2
022400120727     O                                           61 ' '
022500120727     O                                          + 0 'A V V I S A R E    C E D'
