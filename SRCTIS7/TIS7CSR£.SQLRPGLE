000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200100719     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300001221     H*--------------------------------------------------------------*
000400160315     H* CREAZIONE ED INVIO AUTOMATICO CAPPARIO "SEMPLIFICATO"
000500000000     H*--------------------------------------------------------------*
000600160315     FTABEL00F  IF   E           K DISK
000700170227     FTIVGD00F  UF A E             DISK    USROPN COMMIT
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
002300170227     D psds           sds
002400170227     D  procname         *PROC
002500160315     D KPJBA         E DS
002600160315     D DS3C          E DS                  inz
002700170227     D TIS7VASDS     E DS
002800001218     D*--------------------
002900001218     D* DS DI RIDEFINIZIOINE PARAMETRI IN INPUT
003000001218     D*--------------------
003100050825     D DSINPUT         DS
003200160315     D  VERSIONE                      5  0
003300160315     D  GGPRIMA                       2  0
003400160315     D  TIPFILE                       2
003500160315     D  CODCLI                        7  0
003600160315     D  LISTATFP                     30
003700160315     D*
003800160315     D DSLISTATFP      DS                  qualified inz
003900160315     D  TFP01                         3  0
004000160315     D  TFP02                         3  0
004100160315     D  TFP03                         3  0
004200160315     D  TFP04                         3  0
004300160315     D  TFP05                         3  0
004400160315     D  TFP06                         3  0
004500160315     D  TFP07                         3  0
004600160315     D  TFP08                         3  0
004700160315     D  TFP09                         3  0
004800160315     D  TFP10                         3  0
004900160315     D*
005000160315     D DSCAPSIMPLE     DS                  qualified inz
005100160315     D  ORGFIL                        3  0
005200160315     D  CPRVER                        5  0
005300160315     D  CPRDDE                        8  0
005400160315     D  AMBITO                        5
005500160315     D  CPCNAR                        3
005600160315     D  CPCPRV                        2
005700160315     D  CPCCAP                        9
005800160315     D  CPLLOC                       37
005900160315     D  CPCLNA                        3  0
006000160315     D  CPCZNC                        2  0
006100160315     D  CAETFE                        3  0
006200170216     D  CAETFP                        3  0
006300010605     D*--------------------
006400010605     D* VARIABILI DI WRK
006500010605     D*--------------------
006600160622     D wSQL            S           6000    inz
006700160315     D wLISTATFP       S            100    inz
006800160208     D*--------------------
006900001218
007000010605
007100010605
007200920812     C*---------------------------------------------------------------*
007300001218     C* MAIN
007400001218     C*---------------------------------------------------------------*
007500100719     C*
007600100719     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007700100719     C
007800100719     C/EXEC SQL
007900100719     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008000100719     C/END-EXEC
008100100719     C
008200170227     C*
008300170227     C* Inizializzo variabili d wrk
008400170227     C                   movel     'N'           wProcedi          1
008500100719     C*
008600160315     C                   seton                                        30
008700160315     C   30              exsr      chkpar
008800160315     C   30              exsr      chkver
008900160315     C   30              exsr      cartab
009000160315     C*
009100160315     C* Se sin qui tutto OK => elaboro
009200160315     C                   if        *in30
009300170227     C*
009400170227     C* Avvio il controllo sincronia
009500170227     C                   CLEAR                   TIS7VASDS
009600170227     C                   EVAL      i§7VASOPZ = 'SCM'
009700170227     C                   CALL(e)   'TIS7VASR1'
009800170227     C                   PARM                    TIS7VASDS
009900170227     C*
010000170227     C* Verifico esito chiamata
010100170227     C                   if        %error OR o§7VASOK = *off
010200170227     C                   exsr      EXEERR
010300170227     C                   endif
010400170227     C*
010500170227     C* Se tutto OK apro il file download
010600170227     C                   if        not %open(tivgd00f)
010700170227     C                   open      tivgd00f
010800170227     C                   endif
010900170227     C*
011000170227     C* Inizializzo la transazione
011100170227     C                   CLEAR                   TIS7VASDS
011200170227     C                   EVAL      i§7VASOPZ = 'PRG'
011300170227     C                   CALL(e)   'TIS7VASR1'
011400170227     C                   PARM                    TIS7VASDS
011500170227     C*
011600170227     C* Verifico esito chiamata
011700170227     C                   if        not %error AND
011800170227     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
011900170227     C                   movel     'S'           wProcedi
012000170227     C                   endif
012100170227     C*
012200170227     C* Se ok a procedere => elaboro
012300170227     C                   if        wProcedi = 'S'
012400001218     C                   exsr      procedi
012500050909     C                   endif
012600160315     C                   endif
012700170227     C*
012800170227     C* Chiudo il file download
012900170227     C                   if        %open(tivgd00f)
013000170227     C                   close     tivgd00f
013100170227     C*
013200170227     C* Finalizzo la transazione
013300170227     C                   EVAL      i§7VASOPZ  = 'RLS'
013400170227     C                   EVAL      i§7VASCMTE = '1'
013500170227     C                   EVAL      i§7VASTIP  = TIPFILE
013600170227     C                   EVAL      i§7VASKSU  = '0'+%editc(CODCLI:'X')
013700170227     C                   EVAL      i§7VASTSC  = 'WW'
013800170227     C                   EVAL      i§7VASSTO  = '?'
013900170227     C                   EVAL      i§7VASSTTO = 'L'
014000170227     C                   EVAL      i§7VASPRG  = o§7VASPRG
014100170227     C                   CALL(e)   'TIS7VASR1'
014200170227     C                   PARM                    TIS7VASDS
014300170227     C*
014400170227     C* Verifico esito chiamata
014500170227     C                   if        %error OR o§7VASOK = *off
014600170227     C                   exsr      EXEERR
014700170227     C                   endif
014800170227     C*
014900170227     C* Fine controllo sincronia
015000170227     C                   EVAL      i§7VASOPZ = 'ECM'
015100170227     C                   CALL(e)   'TIS7VASR1'
015200170227     C                   PARM                    TIS7VASDS
015300170227     C*
015400170227     C* Verifico esito chiamata
015500170227     C                   if        %error OR o§7VASOK = *off
015600170227     C                   exsr      EXEERR
015700170227     C                   endif
015800170227     C                   endif
015900001218     C*
016000001218     C                   seton                                        LR
016100001218
016200001218
016300001218
016400001218     C*------------------------------------------------------------------------*
016500001218     C* CHKPAR - Routine di controllo parametri ricevuti in input
016600001218     C*------------------------------------------------------------------------*
016700001218     C     CHKPAR        BEGSR
016800160315     C*
016900160315     C* Verifico i parametri di input
017000160315     C                   if        GGPRIMA = *zeros
017100160315     C                   eval      GGPRIMA = 14
017200160315     C                   endif
017300050825     C*
017400160315     C                   if        TIPFILE = *blanks
017500160315     C                   eval      TIPFILE = 'CS'
017600160315     C                   endif
017700160315     C*
017800160315     C                   if        LISTATFP <> *blanks AND LISTATFP <> *zeros
017900160315     C                   eval      DSLISTATFP = LISTATFP
018000160315     C                   eval      wLISTATFP =
018100160315     C                             %editc(DSLISTATFP.TFP01:'X')+', '+
018200160315     C                             %editc(DSLISTATFP.TFP02:'X')+', '+
018300160315     C                             %editc(DSLISTATFP.TFP03:'X')+', '+
018400160315     C                             %editc(DSLISTATFP.TFP04:'X')+', '+
018500160315     C                             %editc(DSLISTATFP.TFP05:'X')+', '+
018600160315     C                             %editc(DSLISTATFP.TFP06:'X')+', '+
018700160315     C                             %editc(DSLISTATFP.TFP07:'X')+', '+
018800160315     C                             %editc(DSLISTATFP.TFP08:'X')+', '+
018900160315     C                             %editc(DSLISTATFP.TFP09:'X')+', '+
019000160315     C                             %editc(DSLISTATFP.TFP10:'X')
019100160315     C                   seton                                        40
019200160315     C                   else
019300160315     C                   setoff                                       40
019400160315     C                   endif
019500001218     C*
019600001218     C                   ENDSR
019700001218     C*------------------------------------------------------------------------*
019800160315
019900160315
020000160315
020100160315     C*------------------------------------------------------------------------*
020200160315     C* CHKVER - Verifica versioni CAPPARIO
020300160315     C*------------------------------------------------------------------------*
020400160315     C     CHKVER        BEGSR
020500160315     C*
020600160315     C* Inizializzo variabili di wrk
020700160315     C                   z-add     *hival        wDATVER           8 0
020800160315     C                   z-add     *zeros        wCPRVER           5 0
020900160315     C*
021000160315     C* Se indicata una versione forzata in input considero OK e non eseguo verifica
021100160315     C                   if        VERSIONE > *zeros
021200160315     C                   eval      wDATVER = datcor
021300160315     C                   eval      wCPRVER = VERSIONE
021400160315     C                   else
021500160315     C*
021600160315     C* Verifico se presente una versione che decorre esattamente tra <GGPRIMA> giorni
021700160315     C                   eval      wDATVER = %dec(%date(datcor)+%days(GGPRIMA))
021800160315     C*
021900160315     C/EXEC SQL
022000160315     C+ declare C_CPR cursor for
022100160315     C+ select cprVER from azcpr00f
022200160315     C+ where cprDDE = :wDATVER and cprATB = ' '
022300160315     C+ for read only
022400160315     C/END-EXEC
022500160315     C
022600160315     C/EXEC SQL
022700160315     C+ open C_CPR
022800160315     C/END-EXEC
022900160315     C
023000160315     C/EXEC SQL
023100160315     C+ Fetch C_CPR into :wCPRVER
023200160315     C/END-EXEC
023300160315     C*
023400160315     C                   if        sqlcod = *zeros
023500160315     C                   else
023600160322     C                   setoff                                       30
023700160315     C                   endif
023800160315     C*
023900160315     C/EXEC SQL
024000160315     C+ close C_CPR
024100160315     C/END-EXEC
024200160315     C
024300160315     C*
024400160315     C                   endif
024500160315     C*
024600160315     C                   ENDSR
024700160315     C*------------------------------------------------------------------------*
024800160315
024900160315
025000160315
025100160315     C*------------------------------------------------------------------------*
025200160315     C* CARTAB - Routine di caricamento dati tabellati
025300160315     C*------------------------------------------------------------------------*
025400160315     C     CARTAB        BEGSR
025500160315     C*
025600160315     C                   Z-ADD     0             I                 4 0
025700160315     C                   MOVEL     '3C'          COD
025800160315     C     KTAB          CHAIN     TABEL00F                           31
025900160315     C     *IN31         DOWEQ     '0'
026000160315     C                   IF        TBLFLG = ' '
026100160315     C                   MOVEL     TBLUNI        DS3C
026200160315     C                   IF        (CODCLI > *zeros AND CODCLI = §3CCKS) OR
026300160315     C                              CODCLI = *zeros
026400160315     C     §3CCKS        LOOKUP    KUNI                                   50
026500160315     C                   IF        not %found AND §3CCKS > *zeros
026600160315     C                   ADD       1             I
026700160315     C                   MOVEL     §3CCKS        KUNI(I)
026800160315     C                   MOVEL     §3CCSAMB      AMBI(I)
026900160315     C                   MOVEL     §3CCSTFP      TFPI(I)
027000160315     C                   ENDIF
027100160315     C                   ENDIF
027200160315     C                   ENDIF
027300160315     C     KTAB          READE     TABEL00F                               31
027400160315     C                   ENDDO
027500160315     C*
027600160315     C* Se non presente in tabella '3C' considero come cliente stand-alone
027700160620     C* e ambito standard con terminal la linea del del codice cliente
027800160315     C                   IF        I = *zeros
027900160315     C                   ADD       1             I
028000160315     C                   MOVEL     CODCLI        KUNI(I)
028100160620     C                   MOVEL     '='           AMBI(I)
028200160620     C                   MOVEL     CODCLI        TFPI(I)
028300160315     C                   ENDIF
028400160315     C*
028500160315     C                   ENDSR
028600160315     C*------------------------------------------------------------------------*
028700001218
028800001218
028900001218
029000001218     C*------------------------------------------------------------------------*
029100001218     C* PROCEDI - Routine principale
029200001218     C*------------------------------------------------------------------------*
029300001218     C     PROCEDI       BEGSR
029400160315     C*
029500160315     C* Ciclo per tutti i clienti presenti in schiera
029600001218     C                   z-add     1             I
029700001218     C                   dow       KUNI(I) > *zeros
029800160315     C*
029900160315     C* A seconda della necessità "personalizzo" la stringa SQL
030000160620     C*
030100160620     C* - personalizzazioni per AMBITO
030200160620     C                   select
030300160620     C* Compongo la stringa SQL da eseguire per ambito SOTTO ("<")
030400160620     C                   when      AMBI(I) = '<'
030500160620     C                   eval      wSQL =
030600160620     C                             %trim(sqlsot(01))+' '+%trim(sqlsot(02))+' '+
030700160620     C                             %trim(sqlsot(03))+' '+%trim(sqlsot(04))+' '+
030800160620     C                             %trim(sqlsot(05))+' '+%trim(sqlsot(06))+' '+
030900160620     C                             %trim(sqlsot(07))+' '+%trim(sqlsot(08))+' '+
031000160620     C                             %trim(sqlsot(09))+' '+%trim(sqlsot(10))+' '+
031100160620     C                             %trim(sqlsot(11))+' '+%trim(sqlsot(12))+' '+
031200160620     C                             %trim(sqlsot(13))+' '+%trim(sqlsot(14))+' '+
031300160620     C                             %trim(sqlsot(15))+' '+%trim(sqlsot(16))+' '+
031400160620     C                             %trim(sqlsot(17))+' '+%trim(sqlsot(18))+' '+
031500160620     C                             %trim(sqlsot(19))+' '+%trim(sqlsot(20))+' '+
031600160620     C                             %trim(sqlsot(21))+' '+%trim(sqlsot(22))+' '+
031700160620     C                             %trim(sqlsot(23))+' '+%trim(sqlsot(24))+' '+
031800160620     C                             %trim(sqlsot(25))+' '+%trim(sqlsot(26))+' '+
031900160620     C                             %trim(sqlsot(27))+' '+%trim(sqlsot(28))+' '+
032000160622     C                             %trim(sqlsot(29))+' '+%trim(sqlsot(30))+' '+
032100160622     C                             %trim(sqlsot(31))+' '+%trim(sqlsot(32))+' '+
032200160622     C                             %trim(sqlsot(33))+' '+%trim(sqlsot(34))+' '+
032300160622     C                             %trim(sqlsot(35))+' '+%trim(sqlsot(36))+' '+
032400160622     C                             %trim(sqlsot(37))+' '+%trim(sqlsot(38))+' '+
032500160622     C                             %trim(sqlsot(39))+' '+%trim(sqlsot(40))+' '+
032600160622     C                             %trim(sqlsot(41))+' '+%trim(sqlsot(42))+' '+
032700160622     C                             %trim(sqlsot(43))+' '+%trim(sqlsot(44))+' '+
032800160622     C                             %trim(sqlsot(45))+' '+%trim(sqlsot(46))+' '+
032900160622     C                             %trim(sqlsot(47))+' '+%trim(sqlsot(48))+' '+
033000160622     C                             %trim(sqlsot(49))+' '+%trim(sqlsot(50))
033100160620     C* Compongo la stringa SQL da eseguire per ambito SOPRA (">")
033200160620     C                   when      AMBI(I) = '>'
033300160620     C                   eval      wSQL =
033400160622     C                             %trim(sqlsop(01))+' '+%trim(sqlsop(02))+' '+
033500160622     C                             %trim(sqlsop(03))+' '+%trim(sqlsop(04))+' '+
033600160622     C                             %trim(sqlsop(05))+' '+%trim(sqlsop(06))+' '+
033700160622     C                             %trim(sqlsop(07))+' '+%trim(sqlsop(08))+' '+
033800160622     C                             %trim(sqlsop(09))+' '+%trim(sqlsop(10))+' '+
033900160622     C                             %trim(sqlsop(11))+' '+%trim(sqlsop(12))+' '+
034000160622     C                             %trim(sqlsop(13))+' '+%trim(sqlsop(14))+' '+
034100160622     C                             %trim(sqlsop(15))+' '+%trim(sqlsop(16))+' '+
034200160622     C                             %trim(sqlsop(17))+' '+%trim(sqlsop(18))+' '+
034300160622     C                             %trim(sqlsop(19))+' '+%trim(sqlsop(20))+' '+
034400160622     C                             %trim(sqlsop(21))+' '+%trim(sqlsop(22))+' '+
034500160622     C                             %trim(sqlsop(23))+' '+%trim(sqlsop(24))+' '+
034600160622     C                             %trim(sqlsop(25))+' '+%trim(sqlsop(26))+' '+
034700160622     C                             %trim(sqlsop(27))+' '+%trim(sqlsop(28))+' '+
034800160622     C                             %trim(sqlsop(29))+' '+%trim(sqlsop(30))+' '+
034900160622     C                             %trim(sqlsop(31))+' '+%trim(sqlsop(32))+' '+
035000160622     C                             %trim(sqlsop(33))+' '+%trim(sqlsop(34))+' '+
035100160622     C                             %trim(sqlsop(35))+' '+%trim(sqlsop(36))+' '+
035200160622     C                             %trim(sqlsop(37))+' '+%trim(sqlsop(38))+' '+
035300160622     C                             %trim(sqlsop(39))+' '+%trim(sqlsop(40))+' '+
035400160622     C                             %trim(sqlsop(41))+' '+%trim(sqlsop(42))+' '+
035500160622     C                             %trim(sqlsop(43))+' '+%trim(sqlsop(44))+' '+
035600160622     C                             %trim(sqlsop(45))+' '+%trim(sqlsop(46))+' '+
035700160622     C                             %trim(sqlsop(47))+' '+%trim(sqlsop(48))+' '+
035800160622     C                             %trim(sqlsop(49))+' '+%trim(sqlsop(50))
035900160620     C* Compongo la stringa SQL da eseguire per ambito STANDARD ("=")
036000160620     C                   other
036100160620     C                   eval      wSQL =
036200160622     C                             %trim(sqlstd(01))+' '+%trim(sqlstd(02))+' '+
036300160622     C                             %trim(sqlstd(03))+' '+%trim(sqlstd(04))+' '+
036400160622     C                             %trim(sqlstd(05))+' '+%trim(sqlstd(06))+' '+
036500160622     C                             %trim(sqlstd(07))+' '+%trim(sqlstd(08))+' '+
036600160622     C                             %trim(sqlstd(09))+' '+%trim(sqlstd(10))+' '+
036700160622     C                             %trim(sqlstd(11))+' '+%trim(sqlstd(12))+' '+
036800160622     C                             %trim(sqlstd(13))+' '+%trim(sqlstd(14))+' '+
036900160622     C                             %trim(sqlstd(15))+' '+%trim(sqlstd(16))+' '+
037000160622     C                             %trim(sqlstd(17))+' '+%trim(sqlstd(18))+' '+
037100160622     C                             %trim(sqlstd(19))+' '+%trim(sqlstd(20))+' '+
037200160622     C                             %trim(sqlstd(21))+' '+%trim(sqlstd(22))+' '+
037300160622     C                             %trim(sqlstd(23))+' '+%trim(sqlstd(24))+' '+
037400160622     C                             %trim(sqlstd(25))+' '+%trim(sqlstd(26))+' '+
037500160622     C                             %trim(sqlstd(27))+' '+%trim(sqlstd(28))+' '+
037600160622     C                             %trim(sqlstd(29))+' '+%trim(sqlstd(30))+' '+
037700160622     C                             %trim(sqlstd(31))+' '+%trim(sqlstd(32))+' '+
037800160622     C                             %trim(sqlstd(33))+' '+%trim(sqlstd(34))+' '+
037900160622     C                             %trim(sqlstd(35))+' '+%trim(sqlstd(36))+' '+
038000160622     C                             %trim(sqlstd(37))+' '+%trim(sqlstd(38))+' '+
038100160622     C                             %trim(sqlstd(39))+' '+%trim(sqlstd(40))+' '+
038200160622     C                             %trim(sqlstd(41))+' '+%trim(sqlstd(42))+' '+
038300160622     C                             %trim(sqlstd(43))+' '+%trim(sqlstd(44))+' '+
038400160622     C                             %trim(sqlstd(45))+' '+%trim(sqlstd(46))+' '+
038500160622     C                             %trim(sqlstd(47))+' '+%trim(sqlstd(48))+' '+
038600160622     C                             %trim(sqlstd(49))+' '+%trim(sqlstd(50))
038700160620     C                   endsl
038800160315     C*
038900160315     C* - personalizzazioni per TFP
039000160315     C                   select
039100160315     C                   when      *in40
039200160315     C                   eval      wSQL = %scanrpl('%TFP%':'in ('+
039300160315     C                                              %trim(wLISTATFP)+')':wSQL)
039400160315     C                   when      not *in40
039500160315     C                   if        TFPI(I) = 999
039600160315     C                   eval      wSQL = %scanrpl('%TFP%':'> 000':wSQL)
039700160315     C                   else
039800160315     C                   eval      wSQL = %scanrpl('%TFP%':'= '+
039900160315     C                                              %editc(TFPI(I):'X'):wSQL)
040000160315     C                   endif
040100160315     C                   endsl
040200160315     C*
040300160315     C* - personalizzazioni per DATA
040400160315     C                   eval      wSQL = %scanrpl('%DATA%':%editc(wDATVER:'X')
040500160315     C                                             :wSQL)
040600160315     C*
040700160315     C* - personalizzazioni per VERSIONE CAPPARIO
040800160315     C                   eval      wSQL = %scanrpl('%VERS%':%editc(wCPRVER:'X')
040900160315     C                                             :wSQL)
041000160315     C*
041100160315     C* Quindi esegui stringa SQL così ottenuta
041200160315     C*
041300160315     C*
041400160315     C/EXEC SQL
041500160315     C+ prepare S_CAPPARIO from :wSQL
041600160315     C/END-EXEC
041700160315     C
041800160315     C/EXEC SQL
041900160315     C+ declare C_CAPPARIO asensitive cursor for S_CAPPARIO
042000160315     C/END-EXEC
042100160315     C
042200160315     C/EXEC SQL
042300160315     C+ open C_CAPPARIO
042400160315     C/END-EXEC
042500160315     C
042600160315     C/EXEC SQL
042700160315     C+ Fetch C_CAPPARIO into :DSCAPSIMPLE
042800160315     C/END-EXEC
042900160315     C*
043000160315     C                   dow       sqlcod = *zeros
043100170216     C                   exsr      WRITIVGD
043200160315     C
043300160315     C/EXEC SQL
043400160315     C+ Fetch C_CAPPARIO into :DSCAPSIMPLE
043500160315     C/END-EXEC
043600160315     C*
043700160315     C                   enddo
043800160315     C*
043900160315     C/EXEC SQL
044000160315     C+ close C_CAPPARIO
044100160315     C/END-EXEC
044200160315     C
044300001218     C*
044400001218     C                   add       1             I
044500001218     C                   enddo
044600001218     C*
044700001218     C                   ENDSR
044800001218     C*------------------------------------------------------------------------*
044900100719
045000100719
045100050825
045200050825     C*------------------------------------------------------------------------*
045300160315     C* WRITIVGD - Routine di scrittura file TIVGD (file VAS generico download)
045400050825     C*------------------------------------------------------------------------*
045500050825     C     WRITIVGD      BEGSR
045600050825     C*
045700050825     C                   clear                   tivgd000
045800160315     C                   eval      vgdDTA = DSCAPSIMPLE
045900100312     C                   eval      vgdTIP = TIPFILE
046000170227     C                   eval      vgdKSU = '0'+%editc(CODCLI:'X')
046100050825     C                   eval      vgdTSC = 'WW'
046200050825     C                   eval      vgdDAT = datcor
046300170227     C                   eval      vgdPRG = o§7VASPRG
046400170227     C                   eval      vgdPGM = procname
046500170227     C                   eval      vgdSTO = '?'
046600050825     C                   write     tivgd000
046700050825     C*
046800050825     C                   ENDSR
046900050825     C*------------------------------------------------------------------------*
047000170227
047100170227
047200170227
047300170227     C*------------------------------------------------------------------------*
047400170227     C* EXEERR - Routine di esecuzione azioni su Errore
047500170227     C*------------------------------------------------------------------------*
047600170227     C     EXEERR        BEGSR
047700170227     C*
047800170227     C                   dump(A)
047900170227     C                   rolbk
048000170227     C                   seton                                        lr
048100170227     C                   return
048200170227     C*
048300170227     C                   ENDSR
048400170227     C*------------------------------------------------------------------------*
048500001218
048600001218
048700001218
048800001218     C*------------------------------------------------------------------------*
048900001218     C* *INZSR - ROUTINE INIZIALE
049000001218     C*------------------------------------------------------------------------*
049100001218     C     *INZSR        BEGSR
049200001218     C*
049300001218     C     *ENTRY        PLIST
049400160315     C                   PARM                    KPJBA
049500001218     C*
049600160315     C                   MOVEL     KPJBU         DSINPUT
049700001218     C                   Z-ADD     1             CODUT
049800160315     C*
049900160315     C* Definizioni chiavi
050000160315     C*
050100160315     C     KTAB          KLIST
050200160315     C                   KFLD                    CODUT             1 0
050300160315     C                   KFLD                    COD               2
050400001218     C*
050500160315     C* Determino la data corrente
050600100312     C                   Z-ADD     *zeros        DATCOR            8 0
050700100312     C                   EVAL      DATCOR = %dec(%date() : *ISO)
050800001218     C*
050900001218     C                   ENDSR
051000160622** SQLSTD -
051100160622with
051200160622TFP as (select orgfil from azorg00f where orgfil %TFP% and
051300160622orgfva = ' ' and orgfag in ('F', 'A')),
051400170216TFA as (select * from azcae00f where caeepa='A' and caetfp %TFP%
051500170216and %DATA% between caedde and caedsc and caeatb=' ' union
051600170216select * from azcae00f where caeepa='A' and caetfp=000
051700170216and %DATA% between caedde and caedsc and caeatb=' '),
051800160622ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
051900160622left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
052000160622cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
052100160622and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
052200160622and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
052300160622length(trim(cpccap))=5),
052400160622CAPNORM as (
052500160622select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
052600160622cpccap, ' ' as cplloc, cpclna, cpcznc
052700160622from ROUTE union all
052800160622select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
052900160622cpccap, cplloc,
053000160622case when cplliv='L' then cpllna
053100160622     else cpclna end as cpclna,
053200160622case when cplliv='L' then cplznc
053300160622     else cpcznc end as cpcznc
053400160622from ROUTE union all
053500160622select distinct cprver, cprdde, 'STD' as Ambito, cpcnar, cpcprv,
053600160622cpccap, cpssin as cplloc,
053700160622case when cplliv='L' then cpllna
053800160622     else cpclna end as cpclna,
053900160622case when cplliv='L' then cplznc
054000160622     else cpcznc end as cpcznc
054100160622from ROUTE),
054200160622TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
054300170216select TFPCAPNORM.*, TFA.CAETFE, TFA.CAETFP
054400170216from TFPCAPNORM left join TFA on cpclna = CAETFA
054500170216order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 12 desc, 10, 11
054600160622for read only
054700160622
054800160622
054900160622
055000160622
055100160622
055200160622
055300160622
055400160622
055500160622
055600160622
055700160622
055800160622
055900160622
056000160622
056100160620** SQLSOT -
056200160620with
056300160620TFP as (select orgfil from azorg00f where orgfil %TFP% and
056400160620orgfva = ' ' and orgfag in ('F', 'A')),
056500170216TFA as (select * from azcae00f where caeepa='A' and caetfp %TFP%
056600170216and %DATA% between caedde and caedsc and caeatb=' ' union
056700170216select * from azcae00f where caeepa='A' and caetfp=000
056800170216and %DATA% between caedde and caedsc and caeatb=' '),
056900160620ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
057000160620left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
057100160620cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
057200160620and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
057300160620and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
057400160620length(trim(cpccap))=5),
057500160620CAPNORM as (
057600160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
057700160622cpccap, ' ' as cplloc,
057800160622case cpclos when 0 then cpclna else cpclos end as cpclos,
057900160622case cpczos when 0 then cpcznc else cpczos end as cpczos
058000160622from ROUTE union all
058100160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
058200160622cpccap, cplloc,
058300160622case when cplliv='L' and cpllos>0 then cpllos
058400160622     when cplliv='L' and cpllos=0 then cpllna
058500160622     when cplliv='C' and cpclos>0 then cpclos
058600160622     when cplliv='C' and cpclos=0 then cpclna
058700160622     else cpclna end as cpclos,
058800160622case when cplliv='L' and cplzos>0 then cplzos
058900160622     when cplliv='L' and cplzos=0 then cplznc
059000160622     when cplliv='C' and cpczos>0 then cpczos
059100160622     when cplliv='C' and cpczos=0 then cpcznc
059200160622     else cpcznc end as cpczos
059300160620from ROUTE union all
059400160622select distinct cprver, cprdde, 'SOT' as Ambito, cpcnar, cpcprv,
059500160622cpccap, cpssin as cplloc,
059600160622case when cplliv='L' and cpllos>0 then cpllos
059700160622     when cplliv='L' and cpllos=0 then cpllna
059800160622     when cplliv='C' and cpclos>0 then cpclos
059900160622     when cplliv='C' and cpclos=0 then cpclna
060000160622     else cpclna end as cpclos,
060100160622case when cplliv='L' and cplzos>0 then cplzos
060200160622     when cplliv='L' and cplzos=0 then cplznc
060300160622     when cplliv='C' and cpczos>0 then cpczos
060400160622     when cplliv='C' and cpczos=0 then cpcznc
060500160622     else cpcznc end as cpczos
060600160620from ROUTE),
060700160620TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
060800170216select TFPCAPNORM.*, TFA.CAETFE, TFA.CAETFP
060900170216from TFPCAPNORM left join TFA on cpclos = CAETFA
061000170216order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 12 desc, 10, 11
061100160620for read only
061200160622** SQLSOP -
061300160622with
061400160622TFP as (select orgfil from azorg00f where orgfil %TFP% and
061500160622orgfva = ' ' and orgfag in ('F', 'A')),
061600170216TFA as (select * from azcae00f where caeepa='A' and caetfp %TFP%
061700170216and %DATA% between caedde and caedsc and caeatb=' ' union
061800170216select * from azcae00f where caeepa='A' and caetfp=000
061900170216and %DATA% between caedde and caedsc and caeatb=' '),
062000160622ROUTE as (select * from azcpr00f join azcpc00f on cprver=cpcver
062100160622left join azcpl00f on cpcver=cplver and cpcnar=cplnar and
062200160622cpccap=cplcap left join azcps00f on cplver=cpsver and cplnar=cpsnar
062300160622and cplcap=cpscap and cplloc=cpsloc where cprver=%VERS% and cpcnar=' '
062400160622and cpratb=' ' and cpcatb=' ' and cplatb=' ' and cpsatb=' ' and
062500160622length(trim(cpccap))=5),
062600160622CAPNORM as (
062700160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
062800160622cpccap, ' ' as cplloc,
062900160622case cpclol when 0 then cpclna else cpclol end as cpclol,
063000160622case cpczol when 0 then cpcznc else cpczol end as cpczol
063100160622from ROUTE union all
063200160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
063300160622cpccap, cplloc,
063400160622case when cplliv='L' and cpllol>0 then cpllol
063500160622     when cplliv='L' and cpllol=0 then cpllna
063600160622     when cplliv='C' and cpclol>0 then cpclol
063700160622     when cplliv='C' and cpclol=0 then cpclna
063800160622     else cpclna end as cpclol,
063900160622case when cplliv='L' and cplzol>0 then cplzol
064000160622     when cplliv='L' and cplzol=0 then cplznc
064100160622     when cplliv='C' and cpczol>0 then cpczol
064200160622     when cplliv='C' and cpczol=0 then cpcznc
064300160622     else cpcznc end as cpczol
064400160622from ROUTE union all
064500160622select distinct cprver, cprdde, 'SOP' as Ambito, cpcnar, cpcprv,
064600160622cpccap, cpssin as cplloc,
064700160622case when cplliv='L' and cpllol>0 then cpllol
064800160622     when cplliv='L' and cpllol=0 then cpllna
064900160622     when cplliv='C' and cpclol>0 then cpclol
065000160622     when cplliv='C' and cpclol=0 then cpclna
065100160622     else cpclna end as cpclol,
065200160622case when cplliv='L' and cplzol>0 then cplzol
065300160622     when cplliv='L' and cplzol=0 then cplznc
065400160622     when cplliv='C' and cpczol>0 then cpczol
065500160622     when cplliv='C' and cpczol=0 then cpcznc
065600160622     else cpcznc end as cpczol
065700160622from ROUTE),
065800160622TFPCAPNORM as (select TFP.orgfil, CAPNORM.* from CAPNORM, TFP)
065900170216select TFPCAPNORM.*, TFA.CAETFE, TFA.CAETFP
066000170216from TFPCAPNORM left join TFA on cpclol = CAETFA
066100170216order by 1, 2, 3, 4, 5, 6, 7, 8, 9, 12 desc, 10, 11
066200160622for read only
