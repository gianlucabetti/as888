000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200100719     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300001221     H*--------------------------------------------------------------*
000400160315     H* CREAZIONE ED INVIO AUTOMATICO CAPPARIO "SEMPLIFICATO"
000500000000     H*--------------------------------------------------------------*
000600160315     FTABEL00F  IF   E           K DISK
000700050825     FTIVGD00F  UF A E             DISK    USROPN
000800001218     D*--------------------
000900001218     D* SCHIERA PER REPERIMENTO CLIENTI "FIGLI" DI UN UNIFICANTE
001000001218     D*--------------------
001100160315     D KUNI            S              7  0 DIM(1000)
001200160315     D AMBI            S              1    DIM(1000)
001300160315     D TFPI            S              3  0 DIM(1000)
001400160315     D*------------
001500160315     D* SCHIERA A TEMPO DI COMPILAZIONE
001600160315     D*------------
001700160622     D sqlstd          S            100    DIM(50) CTDATA PERRCD(1)
001800160622     D sqlsot          S            100    DIM(50) CTDATA PERRCD(1)
001900160622     D sqlsop          S            100    DIM(50) CTDATA PERRCD(1)
002000001218     D*--------------------
002100001218     D* DS ESTERNE
002200001218     D*--------------------
002300160315     D KPJBA         E DS
002400160315     D DS3C          E DS                  inz
002500160315     D trul47ds      e ds                  inz
002600001218     D*--------------------
002700001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
002800001218     D*--------------------
002900050825     D DSINPUT         DS
003000160315     D  VERSIONE                      5  0
003100160315     D  GGPRIMA                       2  0
003200160315     D  TIPFILE                       2
003300160315     D  CODCLI                        7  0
003400160315     D  LISTATFP                     30
003500160315     D*
003600160315     D DSLISTATFP      DS                  qualified inz
003700160315     D  TFP01                         3  0
003800160315     D  TFP02                         3  0
003900160315     D  TFP03                         3  0
004000160315     D  TFP04                         3  0
004100160315     D  TFP05                         3  0
004200160315     D  TFP06                         3  0
004300160315     D  TFP07                         3  0
004400160315     D  TFP08                         3  0
004500160315     D  TFP09                         3  0
004600160315     D  TFP10                         3  0
004700160315     D*
004800160315     D DSCAPSIMPLE     DS                  qualified inz
004900160315     D  ORGFIL                        3  0
005000160315     D  CPRVER                        5  0
005100160315     D  CPRDDE                        8  0
005200160315     D  AMBITO                        5
005300160315     D  CPCNAR                        3
005400160315     D  CPCPRV                        2
005500160315     D  CPCCAP                        9
005600160315     D  CPLLOC                       37
005700160315     D  CPCLNA                        3  0
005800160315     D  CPCZNC                        2  0
005900160315     D  CAETFE                        3  0
006000010605     D*--------------------
006100010605     D* VARIABILI DI WRK
006200010605     D*--------------------
006300160622     D wSQL            S           6000    inz
006400160315     D wLISTATFP       S            100    inz
006500160208     D*--------------------
006600001218
006700010605
006800010605
006900920812     C*---------------------------------------------------------------*
007000001218     C* MAIN
007100001218     C*---------------------------------------------------------------*
007200100719     C*
007300100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007400100719     C
007500100719     C/EXEC SQL
007600100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007700100719     C/END-EXEC
007800100719     C
007900100719     C*
008000160315     C                   seton                                        30
008100160315     C   30              exsr      chkpar
008200160315     C   30              exsr      chkver
008300160315     C   30              exsr      cartab
008400160315     C*
008500160315     C* Se sin qui tutto OK => elaboro
008600160315     C                   if        *in30
008700050909     C*
008800160315     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente
008900050909     C                   open      tivgd00f
009000050909     C                   clear                   trul47ds
009100050909     C                   eval      d47opz  = 'I'
009200120927     C                   eval      d47tip  = TIPFILE
009300160315     C***                eval      d47lck  = 'N'
009400160315     C                   eval      d47chkj = 'N'
009500160315     C                   eval      d47pgm  = 'TIS7VCSR'
009600050909     C                   call      'TRUL47R'
009700050909     C                   parm                    trul47ds
009800050909     C*
009900050909     C* Se elaborazione consentita => proseguo
010000160315     C                   if        d47sts = 'A'
010100160322     C                   setoff                                       30
010200160315     C                   else
010300001218     C                   exsr      procedi
010400050909     C                   endif
010500160315     C                   endif
010600050909     C*
010700050909     C* Chiudo il file di output
010800160315     C                   close     tivgd00f
010900050909     C*
011000160315     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente
011100050909     C                   clear                   trul47ds
011200050909     C                   eval      d47opz  = 'F'
011300120928     C                   eval      d47tip  = TIPFILE
011400050909     C                   call      'TRUL47R'
011500050909     C                   parm                    trul47ds
011600001218     C*
011700001218     C                   seton                                        LR
011800001218
011900001218
012000001218
012100001218     C*------------------------------------------------------------------------*
012200001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
012300001218     C*------------------------------------------------------------------------*
012400001218     C     CHKPAR        BEGSR
012500160315     C*
012600160315     C* Verifico i parametri di input
012700160315     C                   if        GGPRIMA = *zeros
012800160315     C                   eval      GGPRIMA = 14
012900160315     C                   endif
013000050825     C*
013100160315     C                   if        TIPFILE = *blanks
013200160315     C                   eval      TIPFILE = 'CS'
013300160315     C                   endif
013400160315     C*
013500160315     C                   if        LISTATFP <> *blanks AND LISTATFP <> *zeros
013600160315     C                   eval      DSLISTATFP = LISTATFP
013700160315     C                   eval      wLISTATFP =
013800160315     C                             %editc(DSLISTATFP.TFP01:'X')+', '+
013900160315     C                             %editc(DSLISTATFP.TFP02:'X')+', '+
014000160315     C                             %editc(DSLISTATFP.TFP03:'X')+', '+
014100160315     C                             %editc(DSLISTATFP.TFP04:'X')+', '+
014200160315     C                             %editc(DSLISTATFP.TFP05:'X')+', '+
014300160315     C                             %editc(DSLISTATFP.TFP06:'X')+', '+
014400160315     C                             %editc(DSLISTATFP.TFP07:'X')+', '+
014500160315     C                             %editc(DSLISTATFP.TFP08:'X')+', '+
014600160315     C                             %editc(DSLISTATFP.TFP09:'X')+', '+
014700160315     C                             %editc(DSLISTATFP.TFP10:'X')
014800160315     C                   seton                                        40
014900160315     C                   else
015000160315     C                   setoff                                       40
015100160315     C                   endif
015200001218     C*
015300001218     C                   ENDSR
015400001218     C*------------------------------------------------------------------------*
015500160315
015600160315
015700160315
015800160315     C*------------------------------------------------------------------------*
015900160315     C* CHKVER - Verifica versioni CAPPARIO
016000160315     C*------------------------------------------------------------------------*
016100160315     C     CHKVER        BEGSR
016200160315     C*
016300160315     C* Inizializzo variabili di wrk
016400160315     C                   z-add     *hival        wDATVER           8 0
016500160315     C                   z-add     *zeros        wCPRVER           5 0
016600160315     C*
016700160315     C* Se indicata una versione forzata in input considero OK e non eseguo verifica
016800160315     C                   if        VERSIONE > *zeros
016900160315     C                   eval      wDATVER = datcor
017000160315     C                   eval      wCPRVER = VERSIONE
017100160315     C                   else
017200160315     C*
017300160315     C* Verifico se presente una versione che decorre esattamente tra <GGPRIMA> giorni
017400160315     C                   eval      wDATVER = %dec(%date(datcor)+%days(GGPRIMA))
017500160315     C*
017600160315     C/EXEC SQL
017700160315     C+ declare C_CPR cursor for
017800160315     C+ select cprVER from azcpr00f
017900160315     C+ where cprDDE = :wDATVER and cprATB = ' '
018000160315     C+ for read only
018100160315     C/END-EXEC
018200160315     C
018300160315     C/EXEC SQL
018400160315     C+ open C_CPR
018500160315     C/END-EXEC
018600160315     C
018700160315     C/EXEC SQL
018800160315     C+ Fetch C_CPR into :wCPRVER
018900160315     C/END-EXEC
019000160315     C*
019100160315     C                   if        sqlcod = *zeros
019200160315     C                   else
019300160322     C                   setoff                                       30
019400160315     C                   endif
019500160315     C*
019600160315     C/EXEC SQL
019700160315     C+ close C_CPR
019800160315     C/END-EXEC
019900160315     C
020000160315     C*
020100160315     C                   endif
020200160315     C*
020300160315     C                   ENDSR
020400160315     C*------------------------------------------------------------------------*
020500160315
020600160315
020700160315
020800160315     C*------------------------------------------------------------------------*
020900160315     C* CARTAB - Routine di caricamento dati tabellati
021000160315     C*------------------------------------------------------------------------*
021100160315     C     CARTAB        BEGSR
021200160315     C*
021300160315     C                   Z-ADD     0             I                 4 0
021400160315     C                   MOVEL     '3C'          COD
021500160315     C     KTAB          CHAIN     TABEL00F                           31
021600160315     C     *IN31         DOWEQ     '0'
021700160315     C                   IF        TBLFLG = ' '
021800160315     C                   MOVEL     TBLUNI        DS3C
021900160315     C                   IF        (CODCLI > *zeros AND CODCLI = §3CCKS) OR
022000160315     C                              CODCLI = *zeros
022100160315     C     §3CCKS        LOOKUP    KUNI                                   50
022200160315     C                   IF        not %found AND §3CCKS > *zeros
022300160315     C                   ADD       1             I
022400160315     C                   MOVEL     §3CCKS        KUNI(I)
022500160315     C                   MOVEL     §3CCSAMB      AMBI(I)
022600160315     C                   MOVEL     §3CCSTFP      TFPI(I)
022700160315     C                   ENDIF
022800160315     C                   ENDIF
022900160315     C                   ENDIF
023000160315     C     KTAB          READE     TABEL00F                               31
023100160315     C                   ENDDO
023200160315     C*
023300160315     C* Se non presente in tabella '3C' considero come cliente stand-alone
023400160620     C* e ambito standard con terminal la linea del del codice cliente
023500160315     C                   IF        I = *zeros
023600160315     C                   ADD       1             I
023700160315     C                   MOVEL     CODCLI        KUNI(I)
023800160620     C                   MOVEL     '='           AMBI(I)
023900160620     C                   MOVEL     CODCLI        TFPI(I)
024000160315     C                   ENDIF
024100160315     C*
024200160315     C                   ENDSR
024300160315     C*------------------------------------------------------------------------*
024400001218
024500001218
024600001218
024700001218     C*------------------------------------------------------------------------*
024800001218     C* PROCEDI - Routine principale
024900001218     C*------------------------------------------------------------------------*
025000001218     C     PROCEDI       BEGSR
025100160315     C*
025200160315     C* Ciclo per tutti i clienti presenti in schiera
025300001218     C                   z-add     1             I
025400001218     C                   dow       KUNI(I) > *zeros
025500160315     C*
025600160315     C* A seconda della necessità "personalizzo" la stringa SQL
025700160620     C*
025800160620     C* - personalizzazioni per AMBITO
025900160620     C                   select
026000160620     C* Compongo la stringa SQL da eseguire per ambito SOTTO ("<")
026100160620     C                   when      AMBI(I) = '<'
026200160620     C                   eval      wSQL =
026300160620     C                             %trim(sqlsot(01))+' '+%trim(sqlsot(02))+' '+
026400160620     C                             %trim(sqlsot(03))+' '+%trim(sqlsot(04))+' '+
026500160620     C                             %trim(sqlsot(05))+' '+%trim(sqlsot(06))+' '+
026600160620     C                             %trim(sqlsot(07))+' '+%trim(sqlsot(08))+' '+
026700160620     C                             %trim(sqlsot(09))+' '+%trim(sqlsot(10))+' '+
026800160620     C                             %trim(sqlsot(11))+' '+%trim(sqlsot(12))+' '+
026900160620     C                             %trim(sqlsot(13))+' '+%trim(sqlsot(14))+' '+
027000160620     C                             %trim(sqlsot(15))+' '+%trim(sqlsot(16))+' '+
027100160620     C                             %trim(sqlsot(17))+' '+%trim(sqlsot(18))+' '+
027200160620     C                             %trim(sqlsot(19))+' '+%trim(sqlsot(20))+' '+
027300160620     C                             %trim(sqlsot(21))+' '+%trim(sqlsot(22))+' '+
027400160620     C                             %trim(sqlsot(23))+' '+%trim(sqlsot(24))+' '+
027500160620     C                             %trim(sqlsot(25))+' '+%trim(sqlsot(26))+' '+
027600160620     C                             %trim(sqlsot(27))+' '+%trim(sqlsot(28))+' '+
027700160622     C                             %trim(sqlsot(29))+' '+%trim(sqlsot(30))+' '+
027800160622     C                             %trim(sqlsot(31))+' '+%trim(sqlsot(32))+' '+
027900160622     C                             %trim(sqlsot(33))+' '+%trim(sqlsot(34))+' '+
028000160622     C                             %trim(sqlsot(35))+' '+%trim(sqlsot(36))+' '+
028100160622     C                             %trim(sqlsot(37))+' '+%trim(sqlsot(38))+' '+
028200160622     C                             %trim(sqlsot(39))+' '+%trim(sqlsot(40))+' '+
028300160622     C                             %trim(sqlsot(41))+' '+%trim(sqlsot(42))+' '+
028400160622     C                             %trim(sqlsot(43))+' '+%trim(sqlsot(44))+' '+
028500160622     C                             %trim(sqlsot(45))+' '+%trim(sqlsot(46))+' '+
028600160622     C                             %trim(sqlsot(47))+' '+%trim(sqlsot(48))+' '+
028700160622     C                             %trim(sqlsot(49))+' '+%trim(sqlsot(50))
028800160620     C* Compongo la stringa SQL da eseguire per ambito SOPRA (">")
028900160620     C                   when      AMBI(I) = '>'
029000160620     C                   eval      wSQL =
029100160622     C                             %trim(sqlsop(01))+' '+%trim(sqlsop(02))+' '+
029200160622     C                             %trim(sqlsop(03))+' '+%trim(sqlsop(04))+' '+
029300160622     C                             %trim(sqlsop(05))+' '+%trim(sqlsop(06))+' '+
029400160622     C                             %trim(sqlsop(07))+' '+%trim(sqlsop(08))+' '+
029500160622     C                             %trim(sqlsop(09))+' '+%trim(sqlsop(10))+' '+
029600160622     C                             %trim(sqlsop(11))+' '+%trim(sqlsop(12))+' '+
029700160622     C                             %trim(sqlsop(13))+' '+%trim(sqlsop(14))+' '+
029800160622     C                             %trim(sqlsop(15))+' '+%trim(sqlsop(16))+' '+
029900160622     C                             %trim(sqlsop(17))+' '+%trim(sqlsop(18))+' '+
030000160622     C                             %trim(sqlsop(19))+' '+%trim(sqlsop(20))+' '+
030100160622     C                             %trim(sqlsop(21))+' '+%trim(sqlsop(22))+' '+
030200160622     C                             %trim(sqlsop(23))+' '+%trim(sqlsop(24))+' '+
030300160622     C                             %trim(sqlsop(25))+' '+%trim(sqlsop(26))+' '+
030400160622     C                             %trim(sqlsop(27))+' '+%trim(sqlsop(28))+' '+
030500160622     C                             %trim(sqlsop(29))+' '+%trim(sqlsop(30))+' '+
030600160622     C                             %trim(sqlsop(31))+' '+%trim(sqlsop(32))+' '+
030700160622     C                             %trim(sqlsop(33))+' '+%trim(sqlsop(34))+' '+
030800160622     C                             %trim(sqlsop(35))+' '+%trim(sqlsop(36))+' '+
030900160622     C                             %trim(sqlsop(37))+' '+%trim(sqlsop(38))+' '+
031000160622     C                             %trim(sqlsop(39))+' '+%trim(sqlsop(40))+' '+
031100160622     C                             %trim(sqlsop(41))+' '+%trim(sqlsop(42))+' '+
031200160622     C                             %trim(sqlsop(43))+' '+%trim(sqlsop(44))+' '+
031300160622     C                             %trim(sqlsop(45))+' '+%trim(sqlsop(46))+' '+
031400160622     C                             %trim(sqlsop(47))+' '+%trim(sqlsop(48))+' '+
031500160622     C                             %trim(sqlsop(49))+' '+%trim(sqlsop(50))
031600160620     C* Compongo la stringa SQL da eseguire per ambito STANDARD ("=")
031700160620     C                   other
031800160620     C                   eval      wSQL =
031900160622     C                             %trim(sqlstd(01))+' '+%trim(sqlstd(02))+' '+
032000160622     C                             %trim(sqlstd(03))+' '+%trim(sqlstd(04))+' '+
032100160622     C                             %trim(sqlstd(05))+' '+%trim(sqlstd(06))+' '+
032200160622     C                             %trim(sqlstd(07))+' '+%trim(sqlstd(08))+' '+
032300160622     C                             %trim(sqlstd(09))+' '+%trim(sqlstd(10))+' '+
032400160622     C                             %trim(sqlstd(11))+' '+%trim(sqlstd(12))+' '+
032500160622     C                             %trim(sqlstd(13))+' '+%trim(sqlstd(14))+' '+
032600160622     C                             %trim(sqlstd(15))+' '+%trim(sqlstd(16))+' '+
032700160622     C                             %trim(sqlstd(17))+' '+%trim(sqlstd(18))+' '+
032800160622     C                             %trim(sqlstd(19))+' '+%trim(sqlstd(20))+' '+
032900160622     C                             %trim(sqlstd(21))+' '+%trim(sqlstd(22))+' '+
033000160622     C                             %trim(sqlstd(23))+' '+%trim(sqlstd(24))+' '+
033100160622     C                             %trim(sqlstd(25))+' '+%trim(sqlstd(26))+' '+
033200160622     C                             %trim(sqlstd(27))+' '+%trim(sqlstd(28))+' '+
033300160622     C                             %trim(sqlstd(29))+' '+%trim(sqlstd(30))+' '+
033400160622     C                             %trim(sqlstd(31))+' '+%trim(sqlstd(32))+' '+
033500160622     C                             %trim(sqlstd(33))+' '+%trim(sqlstd(34))+' '+
033600160622     C                             %trim(sqlstd(35))+' '+%trim(sqlstd(36))+' '+
033700160622     C                             %trim(sqlstd(37))+' '+%trim(sqlstd(38))+' '+
033800160622     C                             %trim(sqlstd(39))+' '+%trim(sqlstd(40))+' '+
033900160622     C                             %trim(sqlstd(41))+' '+%trim(sqlstd(42))+' '+
034000160622     C                             %trim(sqlstd(43))+' '+%trim(sqlstd(44))+' '+
034100160622     C                             %trim(sqlstd(45))+' '+%trim(sqlstd(46))+' '+
034200160622     C                             %trim(sqlstd(47))+' '+%trim(sqlstd(48))+' '+
034300160622     C                             %trim(sqlstd(49))+' '+%trim(sqlstd(50))
034400160620     C                   endsl
034500160315     C*
034600160315     C* - personalizzazioni per TFP
034700160315     C                   select
034800160315     C                   when      *in40
034900160315     C                   eval      wSQL = %scanrpl('%TFP%':'in ('+
035000160315     C                                              %trim(wLISTATFP)+')':wSQL)
035100160315     C                   when      not *in40
035200160315     C                   if        TFPI(I) = 999
035300160315     C                   eval      wSQL = %scanrpl('%TFP%':'> 000':wSQL)
035400160315     C                   else
035500160315     C                   eval      wSQL = %scanrpl('%TFP%':'= '+
035600160315     C                                              %editc(TFPI(I):'X'):wSQL)
035700160315     C                   endif
035800160315     C                   endsl
035900160315     C*
036000160315     C* - personalizzazioni per DATA
036100160315     C                   eval      wSQL = %scanrpl('%DATA%':%editc(wDATVER:'X')
036200160315     C                                             :wSQL)
036300160315     C*
036400160315     C* - personalizzazioni per VERSIONE CAPPARIO
036500160315     C                   eval      wSQL = %scanrpl('%VERS%':%editc(wCPRVER:'X')
036600160315     C                                             :wSQL)
036700160315     C*
036800160315     C* Quindi esegui stringa SQL così ottenuta
036900160315     C*
037000160315     C*
037100160315     C/EXEC SQL
037200160315     C+ prepare S_CAPPARIO from :wSQL
037300160315     C/END-EXEC
037400160315     C
037500160315     C/EXEC SQL
037600160315     C+ declare C_CAPPARIO asensitive cursor for S_CAPPARIO
037700160315     C/END-EXEC
037800160315     C
037900160315     C/EXEC SQL
038000160315     C+ open C_CAPPARIO
038100160315     C/END-EXEC
038200160315     C
038300160315     C/EXEC SQL
038400160315     C+ Fetch C_CAPPARIO into :DSCAPSIMPLE
038500160315     C/END-EXEC
038600160315     C*
038700160315     C                   dow       sqlcod = *zeros
038800160315     C                   exsr      WRITIVGD
038900160315     C
039000160315     C/EXEC SQL
039100160315     C+ Fetch C_CAPPARIO into :DSCAPSIMPLE
039200160315     C/END-EXEC
039300160315     C*
039400160315     C                   enddo
039500160315     C*
039600160315     C/EXEC SQL
039700160315     C+ close C_CAPPARIO
039800160315     C/END-EXEC
039900160315     C
040000001218     C*
040100001218     C                   add       1             I
040200001218     C                   enddo
040300001218     C*
040400001218     C                   ENDSR
040500001218     C*------------------------------------------------------------------------*
040600100719
040700100719
040800050825
040900050825     C*------------------------------------------------------------------------*
041000160315     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
041100050825     C*------------------------------------------------------------------------*
041200050825     C     WRITIVGD      BEGSR
041300050825     C*
041400050825     C                   clear                   tivgd000
041500160315     C                   eval      vgdDTA = DSCAPSIMPLE
041600100312     C                   eval      vgdTIP = TIPFILE
041700050825     C                   movel     *all'0'       vgdKSU
041800160315     C                   move      CODCLI        vgdKSU
041900050825     C                   eval      vgdTSC = 'WW'
042000050825     C                   eval      vgdDAT = datcor
042100160315     C                   eval      vgdPGM = 'TIS7VCSR'
042200080115     C*
042300050825     C                   write     tivgd000
042400050825     C*
042500050825     C                   ENDSR
042600050825     C*------------------------------------------------------------------------*
042700001218
042800001218
042900001218
043000001218     C*------------------------------------------------------------------------*
043100001218     C* *INZSR - ROUTINE INIZIALE
043200001218     C*------------------------------------------------------------------------*
043300001218     C     *INZSR        BEGSR
043400001218     C*
043500001218     C     *ENTRY        PLIST
043600160315     C                   PARM                    KPJBA
043700001218     C*
043800160315     C                   MOVEL     KPJBU         DSINPUT
043900001218     C                   Z-ADD     1             CODUT
044000160315     C*
044100160315     C* Definizioni chiavi
044200160315     C*
044300160315     C     KTAB          KLIST
044400160315     C                   KFLD                    CODUT             1 0
044500160315     C                   KFLD                    COD               2
044600001218     C*
044700160315     C* Determino la data corrente
044800100312     C                   Z-ADD     *zeros        DATCOR            8 0
044900100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
045000001218     C*
045100001218     C                   ENDSR
045200160622** SQLSTD -
045300160622with
045400160622TFP as (select orgfil from azorg00f where orgfil %TFP% and
045500160622orgfva = ' ' and orgfag in ('F', 'A')),
045600160622TFA as (select * from azcae00f where caeepa='A' and
045700160622%DATA% between caedde and caedsc and caeatb=' '),
045800160622ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
045900160622left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
046000160622cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
046100160622and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
046200160622and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
046300160622length(trim(cpccap))=5),
046400160622CAPNORM as (
046500160622select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
046600160622cpccap, ' ' as cplloc, cpclna, cpcznc
046700160622from ROUTE union all
046800160622select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
046900160622cpccap, cplloc,
047000160622case when cplliv='L' then cpllna
047100160622     else cpclna end as cpclna,
047200160622case when cplliv='L' then cplznc
047300160622     else cpcznc end as cpcznc
047400160622from ROUTE union all
047500160622select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
047600160622cpccap, cpssin as cplloc,
047700160622case when cplliv='L' then cpllna
047800160622     else cpclna end as cpclna,
047900160622case when cplliv='L' then cplznc
048000160622     else cpcznc end as cpcznc
048100160622from ROUTE),
048200160622TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
048300160622select TFPCAPNORM.*, TFA.CAETFE from TFPCAPNORM left join TFA
048400160622on cpclna = CAETFA
048500160622order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
048600160622for read only
048700160622
048800160622
048900160622
049000160622
049100160622
049200160622
049300160622
049400160622
049500160622
049600160622
049700160622
049800160622
049900160622
050000160622
050100160622
050200160622
050300160620** SQLSOT -
050400160620with
050500160620TFP as (select orgfil from azorg00f where orgfil %TFP% and
050600160620orgfva = ' ' and orgfag in ('F', 'A')),
050700160620TFA as (select * from azcae00f where caeepa='A' and
050800160620%DATA% between caedde and caedsc and caeatb=' '),
050900160620ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
051000160620left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
051100160620cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
051200160620and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
051300160620and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
051400160620length(trim(cpccap))=5),
051500160620CAPNORM as (
051600160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
051700160622cpccap, ' ' as cplloc,
051800160622case cpclos when 0 then cpclna else cpclos end as cpclos,
051900160622case cpczos when 0 then cpcznc else cpczos end as cpczos
052000160622from ROUTE union all
052100160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
052200160622cpccap, cplloc,
052300160622case when cplliv='L' and cpllos>0 then cpllos
052400160622     when cplliv='L' and cpllos=0 then cpllna
052500160622     when cplliv='C' and cpclos>0 then cpclos
052600160622     when cplliv='C' and cpclos=0 then cpclna
052700160622     else cpclna end as cpclos,
052800160622case when cplliv='L' and cplzos>0 then cplzos
052900160622     when cplliv='L' and cplzos=0 then cplznc
053000160622     when cplliv='C' and cpczos>0 then cpczos
053100160622     when cplliv='C' and cpczos=0 then cpcznc
053200160622     else cpcznc end as cpczos
053300160620from ROUTE union all
053400160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
053500160622cpccap, cpssin as cplloc,
053600160622case when cplliv='L' and cpllos>0 then cpllos
053700160622     when cplliv='L' and cpllos=0 then cpllna
053800160622     when cplliv='C' and cpclos>0 then cpclos
053900160622     when cplliv='C' and cpclos=0 then cpclna
054000160622     else cpclna end as cpclos,
054100160622case when cplliv='L' and cplzos>0 then cplzos
054200160622     when cplliv='L' and cplzos=0 then cplznc
054300160622     when cplliv='C' and cpczos>0 then cpczos
054400160622     when cplliv='C' and cpczos=0 then cpcznc
054500160622     else cpcznc end as cpczos
054600160620from ROUTE),
054700160620TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
054800160620select TFPCAPNORM.*, TFA.CAETFE from TFPCAPNORM left join TFA
054900160620on cpclos = CAETFA
055000160620order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
055100160620for read only
055200160620
055300160622
055400160622** SQLSOP -
055500160622with
055600160622TFP as (select orgfil from azorg00f where orgfil %TFP% and
055700160622orgfva = ' ' and orgfag in ('F', 'A')),
055800160622TFA as (select * from azcae00f where caeepa='A' and
055900160622%DATA% between caedde and caedsc and caeatb=' '),
056000160622ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
056100160622left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
056200160622cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
056300160622and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
056400160622and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
056500160622length(trim(cpccap))=5),
056600160622CAPNORM as (
056700160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
056800160622cpccap, ' ' as cplloc,
056900160622case cpclol when 0 then cpclna else cpclol end as cpclol,
057000160622case cpczol when 0 then cpcznc else cpczol end as cpczol
057100160622from ROUTE union all
057200160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
057300160622cpccap, cplloc,
057400160622case when cplliv='L' and cpllol>0 then cpllol
057500160622     when cplliv='L' and cpllol=0 then cpllna
057600160622     when cplliv='C' and cpclol>0 then cpclol
057700160622     when cplliv='C' and cpclol=0 then cpclna
057800160622     else cpclna end as cpclol,
057900160622case when cplliv='L' and cplzol>0 then cplzol
058000160622     when cplliv='L' and cplzol=0 then cplznc
058100160622     when cplliv='C' and cpczol>0 then cpczol
058200160622     when cplliv='C' and cpczol=0 then cpcznc
058300160622     else cpcznc end as cpczol
058400160622from ROUTE union all
058500160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
058600160622cpccap, cpssin as cplloc,
058700160622case when cplliv='L' and cpllol>0 then cpllol
058800160622     when cplliv='L' and cpllol=0 then cpllna
058900160622     when cplliv='C' and cpclol>0 then cpclol
059000160622     when cplliv='C' and cpclol=0 then cpclna
059100160622     else cpclna end as cpclol,
059200160622case when cplliv='L' and cplzol>0 then cplzol
059300160622     when cplliv='L' and cplzol=0 then cplznc
059400160622     when cplliv='C' and cpczol>0 then cpczol
059500160622     when cplliv='C' and cpczol=0 then cpcznc
059600160622     else cpcznc end as cpczol
059700160622from ROUTE),
059800160622TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
059900160622select TFPCAPNORM.*, TFA.CAETFE from TFPCAPNORM left join TFA
060000160622on cpclol = CAETFA
060100160622order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
060200160622for read only
060300160622
060400160622
