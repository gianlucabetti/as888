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
006000170216     D  CAETFP                        3  0
006100010605     D*--------------------
006200010605     D* VARIABILI DI WRK
006300010605     D*--------------------
006400160622     D wSQL            S           6000    inz
006500160315     D wLISTATFP       S            100    inz
006600160208     D*--------------------
006700001218
006800010605
006900010605
007000920812     C*---------------------------------------------------------------*
007100001218     C* MAIN
007200001218     C*---------------------------------------------------------------*
007300100719     C*
007400100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007500100719     C
007600100719     C/EXEC SQL
007700100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007800100719     C/END-EXEC
007900100719     C
008000100719     C*
008100160315     C                   seton                                        30
008200160315     C   30              exsr      chkpar
008300160315     C   30              exsr      chkver
008400160315     C   30              exsr      cartab
008500160315     C*
008600160315     C* Se sin qui tutto OK => elaboro
008700160315     C                   if        *in30
008800050909     C*
008900160315     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente
009000050909     C                   open      tivgd00f
009100050909     C                   clear                   trul47ds
009200050909     C                   eval      d47opz  = 'I'
009300120927     C                   eval      d47tip  = TIPFILE
009400160315     C***                eval      d47lck  = 'N'
009500160315     C                   eval      d47chkj = 'N'
009600160315     C                   eval      d47pgm  = 'TIS7VCSR'
009700050909     C                   call      'TRUL47R'
009800050909     C                   parm                    trul47ds
009900050909     C*
010000050909     C* Se elaborazione consentita => proseguo
010100160315     C                   if        d47sts = 'A'
010200160322     C                   setoff                                       30
010300160315     C                   else
010400001218     C                   exsr      procedi
010500050909     C                   endif
010600160315     C                   endif
010700050909     C*
010800050909     C* Chiudo il file di output
010900160315     C                   close     tivgd00f
011000050909     C*
011100160315     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente
011200050909     C                   clear                   trul47ds
011300050909     C                   eval      d47opz  = 'F'
011400120928     C                   eval      d47tip  = TIPFILE
011500050909     C                   call      'TRUL47R'
011600050909     C                   parm                    trul47ds
011700001218     C*
011800001218     C                   seton                                        LR
011900001218
012000001218
012100001218
012200001218     C*------------------------------------------------------------------------*
012300001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
012400001218     C*------------------------------------------------------------------------*
012500001218     C     CHKPAR        BEGSR
012600160315     C*
012700160315     C* Verifico i parametri di input
012800160315     C                   if        GGPRIMA = *zeros
012900160315     C                   eval      GGPRIMA = 14
013000160315     C                   endif
013100050825     C*
013200160315     C                   if        TIPFILE = *blanks
013300160315     C                   eval      TIPFILE = 'CS'
013400160315     C                   endif
013500160315     C*
013600160315     C                   if        LISTATFP <> *blanks AND LISTATFP <> *zeros
013700160315     C                   eval      DSLISTATFP = LISTATFP
013800160315     C                   eval      wLISTATFP =
013900160315     C                             %editc(DSLISTATFP.TFP01:'X')+', '+
014000160315     C                             %editc(DSLISTATFP.TFP02:'X')+', '+
014100160315     C                             %editc(DSLISTATFP.TFP03:'X')+', '+
014200160315     C                             %editc(DSLISTATFP.TFP04:'X')+', '+
014300160315     C                             %editc(DSLISTATFP.TFP05:'X')+', '+
014400160315     C                             %editc(DSLISTATFP.TFP06:'X')+', '+
014500160315     C                             %editc(DSLISTATFP.TFP07:'X')+', '+
014600160315     C                             %editc(DSLISTATFP.TFP08:'X')+', '+
014700160315     C                             %editc(DSLISTATFP.TFP09:'X')+', '+
014800160315     C                             %editc(DSLISTATFP.TFP10:'X')
014900160315     C                   seton                                        40
015000160315     C                   else
015100160315     C                   setoff                                       40
015200160315     C                   endif
015300001218     C*
015400001218     C                   ENDSR
015500001218     C*------------------------------------------------------------------------*
015600160315
015700160315
015800160315
015900160315     C*------------------------------------------------------------------------*
016000160315     C* CHKVER - Verifica versioni CAPPARIO
016100160315     C*------------------------------------------------------------------------*
016200160315     C     CHKVER        BEGSR
016300160315     C*
016400160315     C* Inizializzo variabili di wrk
016500160315     C                   z-add     *hival        wDATVER           8 0
016600160315     C                   z-add     *zeros        wCPRVER           5 0
016700160315     C*
016800160315     C* Se indicata una versione forzata in input considero OK e non eseguo verifica
016900160315     C                   if        VERSIONE > *zeros
017000160315     C                   eval      wDATVER = datcor
017100160315     C                   eval      wCPRVER = VERSIONE
017200160315     C                   else
017300160315     C*
017400160315     C* Verifico se presente una versione che decorre esattamente tra <GGPRIMA> giorni
017500160315     C                   eval      wDATVER = %dec(%date(datcor)+%days(GGPRIMA))
017600160315     C*
017700160315     C/EXEC SQL
017800160315     C+ declare C_CPR cursor for
017900160315     C+ select cprVER from azcpr00f
018000160315     C+ where cprDDE = :wDATVER and cprATB = ' '
018100160315     C+ for read only
018200160315     C/END-EXEC
018300160315     C
018400160315     C/EXEC SQL
018500160315     C+ open C_CPR
018600160315     C/END-EXEC
018700160315     C
018800160315     C/EXEC SQL
018900160315     C+ Fetch C_CPR into :wCPRVER
019000160315     C/END-EXEC
019100160315     C*
019200160315     C                   if        sqlcod = *zeros
019300160315     C                   else
019400160322     C                   setoff                                       30
019500160315     C                   endif
019600160315     C*
019700160315     C/EXEC SQL
019800160315     C+ close C_CPR
019900160315     C/END-EXEC
020000160315     C
020100160315     C*
020200160315     C                   endif
020300160315     C*
020400160315     C                   ENDSR
020500160315     C*------------------------------------------------------------------------*
020600160315
020700160315
020800160315
020900160315     C*------------------------------------------------------------------------*
021000160315     C* CARTAB - Routine di caricamento dati tabellati
021100160315     C*------------------------------------------------------------------------*
021200160315     C     CARTAB        BEGSR
021300160315     C*
021400160315     C                   Z-ADD     0             I                 4 0
021500160315     C                   MOVEL     '3C'          COD
021600160315     C     KTAB          CHAIN     TABEL00F                           31
021700160315     C     *IN31         DOWEQ     '0'
021800160315     C                   IF        TBLFLG = ' '
021900160315     C                   MOVEL     TBLUNI        DS3C
022000160315     C                   IF        (CODCLI > *zeros AND CODCLI = §3CCKS) OR
022100160315     C                              CODCLI = *zeros
022200160315     C     §3CCKS        LOOKUP    KUNI                                   50
022300160315     C                   IF        not %found AND §3CCKS > *zeros
022400160315     C                   ADD       1             I
022500160315     C                   MOVEL     §3CCKS        KUNI(I)
022600160315     C                   MOVEL     §3CCSAMB      AMBI(I)
022700160315     C                   MOVEL     §3CCSTFP      TFPI(I)
022800160315     C                   ENDIF
022900160315     C                   ENDIF
023000160315     C                   ENDIF
023100160315     C     KTAB          READE     TABEL00F                               31
023200160315     C                   ENDDO
023300160315     C*
023400160315     C* Se non presente in tabella '3C' considero come cliente stand-alone
023500160620     C* e ambito standard con terminal la linea del del codice cliente
023600160315     C                   IF        I = *zeros
023700160315     C                   ADD       1             I
023800160315     C                   MOVEL     CODCLI        KUNI(I)
023900160620     C                   MOVEL     '='           AMBI(I)
024000160620     C                   MOVEL     CODCLI        TFPI(I)
024100160315     C                   ENDIF
024200160315     C*
024300160315     C                   ENDSR
024400160315     C*------------------------------------------------------------------------*
024500001218
024600001218
024700001218
024800001218     C*------------------------------------------------------------------------*
024900001218     C* PROCEDI - Routine principale
025000001218     C*------------------------------------------------------------------------*
025100001218     C     PROCEDI       BEGSR
025200160315     C*
025300160315     C* Ciclo per tutti i clienti presenti in schiera
025400001218     C                   z-add     1             I
025500001218     C                   dow       KUNI(I) > *zeros
025600160315     C*
025700160315     C* A seconda della necessità "personalizzo" la stringa SQL
025800160620     C*
025900160620     C* - personalizzazioni per AMBITO
026000160620     C                   select
026100160620     C* Compongo la stringa SQL da eseguire per ambito SOTTO ("<")
026200160620     C                   when      AMBI(I) = '<'
026300160620     C                   eval      wSQL =
026400160620     C                             %trim(sqlsot(01))+' '+%trim(sqlsot(02))+' '+
026500160620     C                             %trim(sqlsot(03))+' '+%trim(sqlsot(04))+' '+
026600160620     C                             %trim(sqlsot(05))+' '+%trim(sqlsot(06))+' '+
026700160620     C                             %trim(sqlsot(07))+' '+%trim(sqlsot(08))+' '+
026800160620     C                             %trim(sqlsot(09))+' '+%trim(sqlsot(10))+' '+
026900160620     C                             %trim(sqlsot(11))+' '+%trim(sqlsot(12))+' '+
027000160620     C                             %trim(sqlsot(13))+' '+%trim(sqlsot(14))+' '+
027100160620     C                             %trim(sqlsot(15))+' '+%trim(sqlsot(16))+' '+
027200160620     C                             %trim(sqlsot(17))+' '+%trim(sqlsot(18))+' '+
027300160620     C                             %trim(sqlsot(19))+' '+%trim(sqlsot(20))+' '+
027400160620     C                             %trim(sqlsot(21))+' '+%trim(sqlsot(22))+' '+
027500160620     C                             %trim(sqlsot(23))+' '+%trim(sqlsot(24))+' '+
027600160620     C                             %trim(sqlsot(25))+' '+%trim(sqlsot(26))+' '+
027700160620     C                             %trim(sqlsot(27))+' '+%trim(sqlsot(28))+' '+
027800160622     C                             %trim(sqlsot(29))+' '+%trim(sqlsot(30))+' '+
027900160622     C                             %trim(sqlsot(31))+' '+%trim(sqlsot(32))+' '+
028000160622     C                             %trim(sqlsot(33))+' '+%trim(sqlsot(34))+' '+
028100160622     C                             %trim(sqlsot(35))+' '+%trim(sqlsot(36))+' '+
028200160622     C                             %trim(sqlsot(37))+' '+%trim(sqlsot(38))+' '+
028300160622     C                             %trim(sqlsot(39))+' '+%trim(sqlsot(40))+' '+
028400160622     C                             %trim(sqlsot(41))+' '+%trim(sqlsot(42))+' '+
028500160622     C                             %trim(sqlsot(43))+' '+%trim(sqlsot(44))+' '+
028600160622     C                             %trim(sqlsot(45))+' '+%trim(sqlsot(46))+' '+
028700160622     C                             %trim(sqlsot(47))+' '+%trim(sqlsot(48))+' '+
028800160622     C                             %trim(sqlsot(49))+' '+%trim(sqlsot(50))
028900160620     C* Compongo la stringa SQL da eseguire per ambito SOPRA (">")
029000160620     C                   when      AMBI(I) = '>'
029100160620     C                   eval      wSQL =
029200160622     C                             %trim(sqlsop(01))+' '+%trim(sqlsop(02))+' '+
029300160622     C                             %trim(sqlsop(03))+' '+%trim(sqlsop(04))+' '+
029400160622     C                             %trim(sqlsop(05))+' '+%trim(sqlsop(06))+' '+
029500160622     C                             %trim(sqlsop(07))+' '+%trim(sqlsop(08))+' '+
029600160622     C                             %trim(sqlsop(09))+' '+%trim(sqlsop(10))+' '+
029700160622     C                             %trim(sqlsop(11))+' '+%trim(sqlsop(12))+' '+
029800160622     C                             %trim(sqlsop(13))+' '+%trim(sqlsop(14))+' '+
029900160622     C                             %trim(sqlsop(15))+' '+%trim(sqlsop(16))+' '+
030000160622     C                             %trim(sqlsop(17))+' '+%trim(sqlsop(18))+' '+
030100160622     C                             %trim(sqlsop(19))+' '+%trim(sqlsop(20))+' '+
030200160622     C                             %trim(sqlsop(21))+' '+%trim(sqlsop(22))+' '+
030300160622     C                             %trim(sqlsop(23))+' '+%trim(sqlsop(24))+' '+
030400160622     C                             %trim(sqlsop(25))+' '+%trim(sqlsop(26))+' '+
030500160622     C                             %trim(sqlsop(27))+' '+%trim(sqlsop(28))+' '+
030600160622     C                             %trim(sqlsop(29))+' '+%trim(sqlsop(30))+' '+
030700160622     C                             %trim(sqlsop(31))+' '+%trim(sqlsop(32))+' '+
030800160622     C                             %trim(sqlsop(33))+' '+%trim(sqlsop(34))+' '+
030900160622     C                             %trim(sqlsop(35))+' '+%trim(sqlsop(36))+' '+
031000160622     C                             %trim(sqlsop(37))+' '+%trim(sqlsop(38))+' '+
031100160622     C                             %trim(sqlsop(39))+' '+%trim(sqlsop(40))+' '+
031200160622     C                             %trim(sqlsop(41))+' '+%trim(sqlsop(42))+' '+
031300160622     C                             %trim(sqlsop(43))+' '+%trim(sqlsop(44))+' '+
031400160622     C                             %trim(sqlsop(45))+' '+%trim(sqlsop(46))+' '+
031500160622     C                             %trim(sqlsop(47))+' '+%trim(sqlsop(48))+' '+
031600160622     C                             %trim(sqlsop(49))+' '+%trim(sqlsop(50))
031700160620     C* Compongo la stringa SQL da eseguire per ambito STANDARD ("=")
031800160620     C                   other
031900160620     C                   eval      wSQL =
032000160622     C                             %trim(sqlstd(01))+' '+%trim(sqlstd(02))+' '+
032100160622     C                             %trim(sqlstd(03))+' '+%trim(sqlstd(04))+' '+
032200160622     C                             %trim(sqlstd(05))+' '+%trim(sqlstd(06))+' '+
032300160622     C                             %trim(sqlstd(07))+' '+%trim(sqlstd(08))+' '+
032400160622     C                             %trim(sqlstd(09))+' '+%trim(sqlstd(10))+' '+
032500160622     C                             %trim(sqlstd(11))+' '+%trim(sqlstd(12))+' '+
032600160622     C                             %trim(sqlstd(13))+' '+%trim(sqlstd(14))+' '+
032700160622     C                             %trim(sqlstd(15))+' '+%trim(sqlstd(16))+' '+
032800160622     C                             %trim(sqlstd(17))+' '+%trim(sqlstd(18))+' '+
032900160622     C                             %trim(sqlstd(19))+' '+%trim(sqlstd(20))+' '+
033000160622     C                             %trim(sqlstd(21))+' '+%trim(sqlstd(22))+' '+
033100160622     C                             %trim(sqlstd(23))+' '+%trim(sqlstd(24))+' '+
033200160622     C                             %trim(sqlstd(25))+' '+%trim(sqlstd(26))+' '+
033300160622     C                             %trim(sqlstd(27))+' '+%trim(sqlstd(28))+' '+
033400160622     C                             %trim(sqlstd(29))+' '+%trim(sqlstd(30))+' '+
033500160622     C                             %trim(sqlstd(31))+' '+%trim(sqlstd(32))+' '+
033600160622     C                             %trim(sqlstd(33))+' '+%trim(sqlstd(34))+' '+
033700160622     C                             %trim(sqlstd(35))+' '+%trim(sqlstd(36))+' '+
033800160622     C                             %trim(sqlstd(37))+' '+%trim(sqlstd(38))+' '+
033900160622     C                             %trim(sqlstd(39))+' '+%trim(sqlstd(40))+' '+
034000160622     C                             %trim(sqlstd(41))+' '+%trim(sqlstd(42))+' '+
034100160622     C                             %trim(sqlstd(43))+' '+%trim(sqlstd(44))+' '+
034200160622     C                             %trim(sqlstd(45))+' '+%trim(sqlstd(46))+' '+
034300160622     C                             %trim(sqlstd(47))+' '+%trim(sqlstd(48))+' '+
034400160622     C                             %trim(sqlstd(49))+' '+%trim(sqlstd(50))
034500160620     C                   endsl
034600160315     C*
034700160315     C* - personalizzazioni per TFP
034800160315     C                   select
034900160315     C                   when      *in40
035000160315     C                   eval      wSQL = %scanrpl('%TFP%':'in ('+
035100160315     C                                              %trim(wLISTATFP)+')':wSQL)
035200160315     C                   when      not *in40
035300160315     C                   if        TFPI(I) = 999
035400160315     C                   eval      wSQL = %scanrpl('%TFP%':'> 000':wSQL)
035500160315     C                   else
035600160315     C                   eval      wSQL = %scanrpl('%TFP%':'= '+
035700160315     C                                              %editc(TFPI(I):'X'):wSQL)
035800160315     C                   endif
035900160315     C                   endsl
036000160315     C*
036100160315     C* - personalizzazioni per DATA
036200160315     C                   eval      wSQL = %scanrpl('%DATA%':%editc(wDATVER:'X')
036300160315     C                                             :wSQL)
036400160315     C*
036500160315     C* - personalizzazioni per VERSIONE CAPPARIO
036600160315     C                   eval      wSQL = %scanrpl('%VERS%':%editc(wCPRVER:'X')
036700160315     C                                             :wSQL)
036800160315     C*
036900160315     C* Quindi esegui stringa SQL così ottenuta
037000160315     C*
037100160315     C*
037200160315     C/EXEC SQL
037300160315     C+ prepare S_CAPPARIO from :wSQL
037400160315     C/END-EXEC
037500160315     C
037600160315     C/EXEC SQL
037700160315     C+ declare C_CAPPARIO asensitive cursor for S_CAPPARIO
037800160315     C/END-EXEC
037900160315     C
038000160315     C/EXEC SQL
038100160315     C+ open C_CAPPARIO
038200160315     C/END-EXEC
038300160315     C
038400160315     C/EXEC SQL
038500160315     C+ Fetch C_CAPPARIO into :DSCAPSIMPLE
038600160315     C/END-EXEC
038700160315     C*
038800160315     C                   dow       sqlcod = *zeros
038900170216     C                   exsr      WRITIVGD
039000160315     C
039100160315     C/EXEC SQL
039200160315     C+ Fetch C_CAPPARIO into :DSCAPSIMPLE
039300160315     C/END-EXEC
039400160315     C*
039500160315     C                   enddo
039600160315     C*
039700160315     C/EXEC SQL
039800160315     C+ close C_CAPPARIO
039900160315     C/END-EXEC
040000160315     C
040100001218     C*
040200001218     C                   add       1             I
040300001218     C                   enddo
040400001218     C*
040500001218     C                   ENDSR
040600001218     C*------------------------------------------------------------------------*
040700100719
040800100719
040900050825
041000050825     C*------------------------------------------------------------------------*
041100160315     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
041200050825     C*------------------------------------------------------------------------*
041300050825     C     WRITIVGD      BEGSR
041400050825     C*
041500050825     C                   clear                   tivgd000
041600160315     C                   eval      vgdDTA = DSCAPSIMPLE
041700100312     C                   eval      vgdTIP = TIPFILE
041800050825     C                   movel     *all'0'       vgdKSU
041900160315     C                   move      CODCLI        vgdKSU
042000050825     C                   eval      vgdTSC = 'WW'
042100050825     C                   eval      vgdDAT = datcor
042200160315     C                   eval      vgdPGM = 'TIS7VCSR'
042300080115     C*
042400050825     C                   write     tivgd000
042500050825     C*
042600050825     C                   ENDSR
042700050825     C*------------------------------------------------------------------------*
042800001218
042900001218
043000001218
043100001218     C*------------------------------------------------------------------------*
043200001218     C* *INZSR - ROUTINE INIZIALE
043300001218     C*------------------------------------------------------------------------*
043400001218     C     *INZSR        BEGSR
043500001218     C*
043600001218     C     *ENTRY        PLIST
043700160315     C                   PARM                    KPJBA
043800001218     C*
043900160315     C                   MOVEL     KPJBU         DSINPUT
044000001218     C                   Z-ADD     1             CODUT
044100160315     C*
044200160315     C* Definizioni chiavi
044300160315     C*
044400160315     C     KTAB          KLIST
044500160315     C                   KFLD                    CODUT             1 0
044600160315     C                   KFLD                    COD               2
044700001218     C*
044800160315     C* Determino la data corrente
044900100312     C                   Z-ADD     *zeros        DATCOR            8 0
045000100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
045100001218     C*
045200001218     C                   ENDSR
045300160622** SQLSTD -
045400160622with
045500160622TFP as (select orgfil from azorg00f where orgfil %TFP% and
045600160622orgfva = ' ' and orgfag in ('F', 'A')),
045700170216TFA as (select * from azcae00f where caeepa='A' and caetfp %TFP%
045800170216and %DATA% between caedde and caedsc and caeatb=' ' union
045900170216select * from azcae00f where caeepa='A' and caetfp=000
046000170216and %DATA% between caedde and caedsc and caeatb=' '),
046100160622ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
046200160622left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
046300160622cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
046400160622and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
046500160622and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
046600160622length(trim(cpccap))=5),
046700160622CAPNORM as (
046800160622select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
046900160622cpccap, ' ' as cplloc, cpclna, cpcznc
047000160622from ROUTE union all
047100160622select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
047200160622cpccap, cplloc,
047300160622case when cplliv='L' then cpllna
047400160622     else cpclna end as cpclna,
047500160622case when cplliv='L' then cplznc
047600160622     else cpcznc end as cpcznc
047700160622from ROUTE union all
047800160622select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
047900160622cpccap, cpssin as cplloc,
048000160622case when cplliv='L' then cpllna
048100160622     else cpclna end as cpclna,
048200160622case when cplliv='L' then cplznc
048300160622     else cpcznc end as cpcznc
048400160622from ROUTE),
048500160622TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
048600170216select TFPCAPNORM.*, TFA.CAETFE, TFA.CAETFP
048700170216from TFPCAPNORM left join TFA on cpclna = CAETFA
048800170216order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 12 desc, 10, 11
048900160622for read only
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
050300160622
050400160620** SQLSOT -
050500160620with
050600160620TFP as (select orgfil from azorg00f where orgfil %TFP% and
050700160620orgfva = ' ' and orgfag in ('F', 'A')),
050800170216TFA as (select * from azcae00f where caeepa='A' and caetfp %TFP%
050900170216and %DATA% between caedde and caedsc and caeatb=' ' union
051000170216select * from azcae00f where caeepa='A' and caetfp=000
051100170216and %DATA% between caedde and caedsc and caeatb=' '),
051200160620ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
051300160620left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
051400160620cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
051500160620and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
051600160620and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
051700160620length(trim(cpccap))=5),
051800160620CAPNORM as (
051900160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
052000160622cpccap, ' ' as cplloc,
052100160622case cpclos when 0 then cpclna else cpclos end as cpclos,
052200160622case cpczos when 0 then cpcznc else cpczos end as cpczos
052300160622from ROUTE union all
052400160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
052500160622cpccap, cplloc,
052600160622case when cplliv='L' and cpllos>0 then cpllos
052700160622     when cplliv='L' and cpllos=0 then cpllna
052800160622     when cplliv='C' and cpclos>0 then cpclos
052900160622     when cplliv='C' and cpclos=0 then cpclna
053000160622     else cpclna end as cpclos,
053100160622case when cplliv='L' and cplzos>0 then cplzos
053200160622     when cplliv='L' and cplzos=0 then cplznc
053300160622     when cplliv='C' and cpczos>0 then cpczos
053400160622     when cplliv='C' and cpczos=0 then cpcznc
053500160622     else cpcznc end as cpczos
053600160620from ROUTE union all
053700160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
053800160622cpccap, cpssin as cplloc,
053900160622case when cplliv='L' and cpllos>0 then cpllos
054000160622     when cplliv='L' and cpllos=0 then cpllna
054100160622     when cplliv='C' and cpclos>0 then cpclos
054200160622     when cplliv='C' and cpclos=0 then cpclna
054300160622     else cpclna end as cpclos,
054400160622case when cplliv='L' and cplzos>0 then cplzos
054500160622     when cplliv='L' and cplzos=0 then cplznc
054600160622     when cplliv='C' and cpczos>0 then cpczos
054700160622     when cplliv='C' and cpczos=0 then cpcznc
054800160622     else cpcznc end as cpczos
054900160620from ROUTE),
055000160620TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
055100170216select TFPCAPNORM.*, TFA.CAETFE, TFA.CAETFP
055200170216from TFPCAPNORM left join TFA on cpclos = CAETFA
055300170216order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 12 desc, 10, 11
055400160620for read only
055500160622** SQLSOP -
055600160622with
055700160622TFP as (select orgfil from azorg00f where orgfil %TFP% and
055800160622orgfva = ' ' and orgfag in ('F', 'A')),
055900170216TFA as (select * from azcae00f where caeepa='A' and caetfp %TFP%
056000170216and %DATA% between caedde and caedsc and caeatb=' ' union
056100170216select * from azcae00f where caeepa='A' and caetfp=000
056200170216and %DATA% between caedde and caedsc and caeatb=' '),
056300160622ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
056400160622left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
056500160622cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
056600160622and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
056700160622and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
056800160622length(trim(cpccap))=5),
056900160622CAPNORM as (
057000160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
057100160622cpccap, ' ' as cplloc,
057200160622case cpclol when 0 then cpclna else cpclol end as cpclol,
057300160622case cpczol when 0 then cpcznc else cpczol end as cpczol
057400160622from ROUTE union all
057500160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
057600160622cpccap, cplloc,
057700160622case when cplliv='L' and cpllol>0 then cpllol
057800160622     when cplliv='L' and cpllol=0 then cpllna
057900160622     when cplliv='C' and cpclol>0 then cpclol
058000160622     when cplliv='C' and cpclol=0 then cpclna
058100160622     else cpclna end as cpclol,
058200160622case when cplliv='L' and cplzol>0 then cplzol
058300160622     when cplliv='L' and cplzol=0 then cplznc
058400160622     when cplliv='C' and cpczol>0 then cpczol
058500160622     when cplliv='C' and cpczol=0 then cpcznc
058600160622     else cpcznc end as cpczol
058700160622from ROUTE union all
058800160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
058900160622cpccap, cpssin as cplloc,
059000160622case when cplliv='L' and cpllol>0 then cpllol
059100160622     when cplliv='L' and cpllol=0 then cpllna
059200160622     when cplliv='C' and cpclol>0 then cpclol
059300160622     when cplliv='C' and cpclol=0 then cpclna
059400160622     else cpclna end as cpclol,
059500160622case when cplliv='L' and cplzol>0 then cplzol
059600160622     when cplliv='L' and cplzol=0 then cplznc
059700160622     when cplliv='C' and cpczol>0 then cpczol
059800160622     when cplliv='C' and cpczol=0 then cpcznc
059900160622     else cpcznc end as cpczol
060000160622from ROUTE),
060100160622TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
060200170216select TFPCAPNORM.*, TFA.CAETFE, TFA.CAETFP
060300170216from TFPCAPNORM left join TFA on cpclol = CAETFA
060400170216order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 12 desc, 10, 11
060500160622for read only
