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
001700160315     D sqlstd          S            100    DIM(30) CTDATA PERRCD(1)
001800160620     D sqlsot          S            100    DIM(30) CTDATA PERRCD(1)
001900160620     D sqlsop          S            100    DIM(30) CTDATA PERRCD(1)
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
006300160315     D wSQL            S           5000    inz
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
027700160620     C                             %trim(sqlsot(29))+' '+%trim(sqlsot(30))
027800160620     C* Compongo la stringa SQL da eseguire per ambito SOPRA (">")
027900160620     C                   when      AMBI(I) = '>'
028000160620     C                   eval      wSQL =
028100160620     C                             %trim(sqlsop(01))+' '+%trim(sqlsop(02))+' '+
028200160620     C                             %trim(sqlsop(03))+' '+%trim(sqlsop(04))+' '+
028300160620     C                             %trim(sqlsop(05))+' '+%trim(sqlsop(06))+' '+
028400160620     C                             %trim(sqlsop(07))+' '+%trim(sqlsop(08))+' '+
028500160620     C                             %trim(sqlsop(09))+' '+%trim(sqlsop(10))+' '+
028600160620     C                             %trim(sqlsop(11))+' '+%trim(sqlsop(12))+' '+
028700160620     C                             %trim(sqlsop(13))+' '+%trim(sqlsop(14))+' '+
028800160620     C                             %trim(sqlsop(15))+' '+%trim(sqlsop(16))+' '+
028900160620     C                             %trim(sqlsop(17))+' '+%trim(sqlsop(18))+' '+
029000160620     C                             %trim(sqlsop(19))+' '+%trim(sqlsop(20))+' '+
029100160620     C                             %trim(sqlsop(21))+' '+%trim(sqlsop(22))+' '+
029200160620     C                             %trim(sqlsop(23))+' '+%trim(sqlsop(24))+' '+
029300160620     C                             %trim(sqlsop(25))+' '+%trim(sqlsop(26))+' '+
029400160620     C                             %trim(sqlsop(27))+' '+%trim(sqlsop(28))+' '+
029500160620     C                             %trim(sqlsop(29))+' '+%trim(sqlsop(30))
029600160620     C* Compongo la stringa SQL da eseguire per ambito STANDARD ("=")
029700160620     C                   other
029800160620     C                   eval      wSQL =
029900160620     C                             %trim(sqlstd(01))+' '+%trim(sqlstd(02))+' '+
030000160620     C                             %trim(sqlstd(03))+' '+%trim(sqlstd(04))+' '+
030100160620     C                             %trim(sqlstd(05))+' '+%trim(sqlstd(06))+' '+
030200160620     C                             %trim(sqlstd(07))+' '+%trim(sqlstd(08))+' '+
030300160620     C                             %trim(sqlstd(09))+' '+%trim(sqlstd(10))+' '+
030400160620     C                             %trim(sqlstd(11))+' '+%trim(sqlstd(12))+' '+
030500160620     C                             %trim(sqlstd(13))+' '+%trim(sqlstd(14))+' '+
030600160620     C                             %trim(sqlstd(15))+' '+%trim(sqlstd(16))+' '+
030700160620     C                             %trim(sqlstd(17))+' '+%trim(sqlstd(18))+' '+
030800160620     C                             %trim(sqlstd(19))+' '+%trim(sqlstd(20))+' '+
030900160620     C                             %trim(sqlstd(21))+' '+%trim(sqlstd(22))+' '+
031000160620     C                             %trim(sqlstd(23))+' '+%trim(sqlstd(24))+' '+
031100160620     C                             %trim(sqlstd(25))+' '+%trim(sqlstd(26))+' '+
031200160620     C                             %trim(sqlstd(27))+' '+%trim(sqlstd(28))+' '+
031300160620     C                             %trim(sqlstd(29))+' '+%trim(sqlstd(30))
031400160620     C                   endsl
031500160315     C*
031600160315     C* - personalizzazioni per TFP
031700160315     C                   select
031800160315     C                   when      *in40
031900160315     C                   eval      wSQL = %scanrpl('%TFP%':'in ('+
032000160315     C                                              %trim(wLISTATFP)+')':wSQL)
032100160315     C                   when      not *in40
032200160315     C                   if        TFPI(I) = 999
032300160315     C                   eval      wSQL = %scanrpl('%TFP%':'> 000':wSQL)
032400160315     C                   else
032500160315     C                   eval      wSQL = %scanrpl('%TFP%':'= '+
032600160315     C                                              %editc(TFPI(I):'X'):wSQL)
032700160315     C                   endif
032800160315     C                   endsl
032900160315     C*
033000160315     C* - personalizzazioni per DATA
033100160315     C                   eval      wSQL = %scanrpl('%DATA%':%editc(wDATVER:'X')
033200160315     C                                             :wSQL)
033300160315     C*
033400160315     C* - personalizzazioni per VERSIONE CAPPARIO
033500160315     C                   eval      wSQL = %scanrpl('%VERS%':%editc(wCPRVER:'X')
033600160315     C                                             :wSQL)
033700160315     C*
033800160315     C* Quindi esegui stringa SQL così ottenuta
033900160315     C*
034000160315     C*
034100160315     C/EXEC SQL
034200160315     C+ prepare S_CAPPARIO from :wSQL
034300160315     C/END-EXEC
034400160315     C
034500160315     C/EXEC SQL
034600160315     C+ declare C_CAPPARIO asensitive cursor for S_CAPPARIO
034700160315     C/END-EXEC
034800160315     C
034900160315     C/EXEC SQL
035000160315     C+ open C_CAPPARIO
035100160315     C/END-EXEC
035200160315     C
035300160315     C/EXEC SQL
035400160315     C+ Fetch C_CAPPARIO into :DSCAPSIMPLE
035500160315     C/END-EXEC
035600160315     C*
035700160315     C                   dow       sqlcod = *zeros
035800160315     C                   exsr      WRITIVGD
035900160315     C
036000160315     C/EXEC SQL
036100160315     C+ Fetch C_CAPPARIO into :DSCAPSIMPLE
036200160315     C/END-EXEC
036300160315     C*
036400160315     C                   enddo
036500160315     C*
036600160315     C/EXEC SQL
036700160315     C+ close C_CAPPARIO
036800160315     C/END-EXEC
036900160315     C
037000001218     C*
037100001218     C                   add       1             I
037200001218     C                   enddo
037300001218     C*
037400001218     C                   ENDSR
037500001218     C*------------------------------------------------------------------------*
037600100719
037700100719
037800050825
037900050825     C*------------------------------------------------------------------------*
038000160315     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
038100050825     C*------------------------------------------------------------------------*
038200050825     C     WRITIVGD      BEGSR
038300050825     C*
038400050825     C                   clear                   tivgd000
038500160315     C                   eval      vgdDTA = DSCAPSIMPLE
038600100312     C                   eval      vgdTIP = TIPFILE
038700050825     C                   movel     *all'0'       vgdKSU
038800160315     C                   move      CODCLI        vgdKSU
038900050825     C                   eval      vgdTSC = 'WW'
039000050825     C                   eval      vgdDAT = datcor
039100160315     C                   eval      vgdPGM = 'TIS7VCSR'
039200080115     C*
039300050825     C                   write     tivgd000
039400050825     C*
039500050825     C                   ENDSR
039600050825     C*------------------------------------------------------------------------*
039700001218
039800001218
039900001218
040000001218     C*------------------------------------------------------------------------*
040100001218     C* *INZSR - ROUTINE INIZIALE
040200001218     C*------------------------------------------------------------------------*
040300001218     C     *INZSR        BEGSR
040400001218     C*
040500001218     C     *ENTRY        PLIST
040600160315     C                   PARM                    KPJBA
040700001218     C*
040800160315     C                   MOVEL     KPJBU         DSINPUT
040900001218     C                   Z-ADD     1             CODUT
041000160315     C*
041100160315     C* Definizioni chiavi
041200160315     C*
041300160315     C     KTAB          KLIST
041400160315     C                   KFLD                    CODUT             1 0
041500160315     C                   KFLD                    COD               2
041600001218     C*
041700160315     C* Determino la data corrente
041800100312     C                   Z-ADD     *zeros        DATCOR            8 0
041900100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
042000001218     C*
042100001218     C                   ENDSR
042200160315** SQLSTD -
042300160315with
042400160315TFP as (select orgfil from azorg00f where orgfil %TFP% and
042500160315orgfva = ' ' and orgfag in ('F', 'A')),
042600160315TFA as (select * from azcae00f where caeepa='A' and
042700160315%DATA% between caedde and caedsc and caeatb=' '),
042800160315ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
042900160315left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
043000160315cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
043100160315and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
043200160315and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
043300160315length(trim(cpccap))=5),
043400160315CAPNORM as (
043500160315select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
043600160622cpccap, ' ' as cplloc, cpclna, cpcznc
043700160622from ROUTE union all
043800160315select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
043900160315cpccap, cplloc, case cpllna when 0 then cpclna else cpllna end as
044000160315cpclna, case cplznc when 0 then cpcznc else cplznc end as cpcznc
044100160315from ROUTE union all
044200160315select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
044300160315cpccap, cpssin as cplloc, case cpllna when 0 then cpclna else
044400160315cpllna end as cpclna,
044500160315case cplznc when 0 then cpcznc else cplznc end as cpcznc
044600160315from ROUTE),
044700160315TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
044800160315select TFPCAPNORM.*, TFA.CAETFE from TFPCAPNORM left join TFA
044900160315on cpclna = CAETFA
045000160315order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
045100160315for read only
045200160315
045300160620** SQLSOT -
045400160620with
045500160620TFP as (select orgfil from azorg00f where orgfil %TFP% and
045600160620orgfva = ' ' and orgfag in ('F', 'A')),
045700160620TFA as (select * from azcae00f where caeepa='A' and
045800160620%DATA% between caedde and caedsc and caeatb=' '),
045900160620ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
046000160620left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
046100160620cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
046200160620and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
046300160620and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
046400160620length(trim(cpccap))=5),
046500160620CAPNORM as (
046600160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
046700160620cpccap, ' ' as cplloc, cpclos, cpczos from ROUTE
046800160620union all
046900160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
047000160620cpccap, cplloc, case cpllos when 0 then cpclos else cpllos end as
047100160620cpclos, case cplzos when 0 then cpczos else cplzos end as cpczos
047200160620from ROUTE union all
047300160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
047400160620cpccap, cpssin as cplloc, case cpllos when 0 then cpclos else
047500160620cpllos end as cpclos,
047600160620case cplzos when 0 then cpczos else cplzos end as cpczos
047700160620from ROUTE),
047800160620TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
047900160620select TFPCAPNORM.*, TFA.CAETFE from TFPCAPNORM left join TFA
048000160620on cpclos = CAETFA
048100160620order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
048200160620for read only
048300160620
048400160620** SQLSOP -
048500160620with
048600160620TFP as (select orgfil from azorg00f where orgfil %TFP% and
048700160620orgfva = ' ' and orgfag in ('F', 'A')),
048800160620TFA as (select * from azcae00f where caeepa='A' and
048900160620%DATA% between caedde and caedsc and caeatb=' '),
049000160620ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
049100160620left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
049200160620cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
049300160620and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
049400160620and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
049500160620length(trim(cpccap))=5),
049600160620CAPNORM as (
049700160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
049800160620cpccap, ' ' as cplloc, cpclol, cpczol from ROUTE
049900160620union all
050000160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
050100160620cpccap, cplloc, case cpllol when 0 then cpclol else cpllol end as
050200160620cpclol, case cplzol when 0 then cpczol else cplzol end as cpczol
050300160620from ROUTE union all
050400160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
050500160620cpccap, cpssin as cplloc, case cpllol when 0 then cpclol else
050600160620cpllol end as cpclol,
050700160620case cplzol when 0 then cpczol else cplzol end as cpczol
050800160620from ROUTE),
050900160620TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
051000160620select TFPCAPNORM.*, TFA.CAETFE from TFPCAPNORM left join TFA
051100160620on cpclol = CAETFA
051200160620order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
051300160620for read only
051400160620
