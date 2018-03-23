000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200120726     H* FICNy1R4 *---------------------------------------------------*
000300081110     H*                                                              *
000400111123     H* Controlla se ci sono delle Tariffe x il Futuro già inserite
000500120726     H*  AFFL DEFL                                                   *
000600120726     H* riceve come parametro la Data decorrenza   SOC/FORNITORE     *
000700000000     H*--------------------------------------------------------------*
000800090401     Ffiapd01L  iF   E           K DISK
000900090401      *
001000081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100081211      * -------------------------------------------------------------
001200111122     D KPJBA         E DS
001300111205     D dsADT         E DS                  ExtName(FIADT00F)
001400081110     d                                     prefix(STP:3)
001500111205     D ADT_REC         s              9s 0
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
005500120726      **  Sono le Tariffe delle AFF/DEFL con data nel FUTURO
005600120726     C*=================================================================
005700090424      *
005800090422     C/EXEC SQL DECLARE A1 CURSOR FOR
005900111205     C+   select rrn(fiADT00f), fiADT00f.* FROM
006000111205     C+   fiADT00f WHERE
006100120726     C+        ADTddt >= :DATA_dec
006200120726     C+   and  ADTcsf =  :SOCIETAold
006300120726     C+   and  ADTcdf =  :FORNITOREold
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
008800120727     c                   seton                                        OF
008900081110     c                   end
009000090109      * >>>>>>>
009100081209      * resto nel ciclo finchè....
009200081110     C                   DOU       SqlCod < 0
009300081110      * lettura cursore
009400081110     C/EXEC SQL
009500111205     C+ Fetch Next From A1 Into :ADT_REC, :dsADT
009600081110     C/END-EXEC
009700111122     C*=====
009800111122      *  EoF
009900081110     C                   if        SqlCod = 100
010000081110     c                   Leave
010100111122     C*=====
010200081110      *  x errori
010300081110     C                   ElseIf    SqlCod < 0
010400081110     c                   eval      esito = Sqlcode
010500111123     c                   move      esito         esito_ALF
010600081110     c                   dump(a)
010700081110     c                   leave
010800081110     C                   ENDif
010900090428      *
011000111206      *
011100111123      *----------------------------->>>>>
011200111123      *  se sono qui ho letto un record correttamente
011300090428      *----------------------------->>>>>
011400111122      * Controlli su Anagrafica
011500111123     c                   clear                   errore           15
011600111122     c                   exsr      Ctrl_APD
011700090428      *
011800090428      *----------------------------->>>>>
011900111206     c                   if        errore = *blank
012000111206     c                   movel(p)  'TAR.FUTURA'  errore
012100111206     c                   end
012200111206     c**
012300090424      *  scrive il dettaglio su un file
012400111123     c   Of              except    testa
012500111123     c                   except    detta
012600111123      *
012700111123     C                   z-add     *all'1'       esito
012800120727     c                   move      esito         esito_ALF
012900081110      *
013000081110     C                   ENDDO
013100081110     C/EXEC SQL
013200081110     C+ Close A1
013300081110     C/END-EXEC
013400120727     C                   if         esito = *all'1'
013500111123     c                   except    finest
013600120727     c                   end
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
017700111205     C*---------------------------------------------------------------*
017800111205     OQSYSPRT   E            TESTA            02
017900120727     O                                           61 ' '
018000120727     O                                          + 0 'A V V I S A R E    C E D'
018100120727     O          E            TESTA       1
018200120727     O                                            1 ' '
018300120727     O                                          + 0 'TARIFFE - AFFLUENZE DEFL'
018400120727     O                                          + 0 'UENZ. con TARIFFE FUTURE'
018500120727     O                                          +10 'da fare'
018600120727     O                                          + 2 'ATTENZIONE'
018700120727     O                       ora                130 '  :  :  '
018800120727      *
018900120727     O          E            TESTA       2  2
019000120727     O                                           10 'FICNY1R4'
019100120727     O                                           25 'Autista'
019200120727     O                                           30 'TSR'
019300120727     O                                           45 'Progr'
019400120727     O                                           55 'Data_Dec'
019500120727     O                                           65 'Data_Sca'
019600120727     O                                           75 'Data_Stp'
019700120727     O                                           85 'Data_Cnv'
019800120727     O                                          100 'N_Rec_ADT'
019900120727     O                                          111 'N O T E'
020000120727     O                       data               132 '  /  /    '
020100111205      *
020200111205     O          E            DETta       1
020300111205     O                       stpPDR        z     25
020400111205     O                       stpTSR              29
020500111205     O                       stpPRG        z     43
020600111205     O                       stpDDT        z     55
020700111205     O                       stpDST        z     65
020800111205     O                       stpDTS        z     75
020900111205     O                       stpDCN        z     85
021000111205     o                       ADT_REC       z    100
021100111205     O                       errore             116
021200111205      ****
021300111205     o          E            FINEST      2
021400111205     o                                            1 ' '
021500120726     o                                          + 0 '*** FICNY1R4 ***'
021600120726     o                                          + 9 '***  TAR A/D FUTURE  ***'
021700111205     o                                          + 9 '***   FINE  STAMPA  ***'
021800111205     O                       data               + 9 '  /  /    '
021900111205     O                       ora                + 3 '  :  :  '
022000120727     o          E            FINEST      2
022100120727     O                                           61 ' '
022200120727     O                                          + 0 'A V V I S A R E    C E D'
