000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200960314     H* TNTL37R  *----------------------------------------------------*
000300960314     F*      INTERROGAZIONE FOGLI VIAGGIO PARTENZA    IN SEDE         *
000400951110     H*---------------------------------------------------------------*
000500960318     FTNTL37D   CF   E             WORKSTN
000600960318     F                                     SFILE(TL37S03:NRR3)
000700960318     F                                     SFILE(TL37S04:NRR4)
000800960318     F                                     SFILE(TL37S05:NRR5)
000900130214     F                                     SFILE(TL37S06:NRR6)
001000960315     FTNFVS01L  IF   E           K DISK
001100000504     FTNFWS01L  IF   E           K DISK
001200121105     FTNFGS01L  IF   E           K DISK    RENAME(fnfgs000:TNFgs000)
001300960318     FTNFVS03L  IF   E           K DISK
001400960318     F                                     RENAME(TNFVS000:TNFVS003)
001500960319     FFNFVD01L  IF   E           K DISK
001600960318     FFNFV203L  IF   E           K DISK
001700960228     FFNFV401L  IF   E           K DISK
001800960301     FAZORG01L  IF   E           K DISK
001900951110     FTABEL00F  IF   E           K DISK
002000121105     FTNPSC01L  IF   E           K DISK
002100951110     D*
002200951110     D* DEFINIZIONE SCHIERE
002300960315     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             MESSAGGI ERRORE
002400960315     D TES             S             36    DIM(1) CTDATA PERRCD(1)              TESTATA PGM   E
002500951110     D*
002600960315     D* DS PER TNTL37R - DS DI PASSAGGIO PARAMETRI PER PGM CHIAMANTI
002700960315     D DSTL37        E DS                  EXTNAME(TNTL37DS)
002800960315     D*
002900960315     D* DS PER FNLV15R - GESTIONE INTERROGAZIONE RITARDO PARTENZA
003000960315     D DSLV15        E DS                  EXTNAME(FNLV15DS)
003100960315     D*
003200960318     D* FILIALI DI ARRIVO
003300960318     D                 DS
003400000504     D  FFV                    1    900
003500000504     D                                     DIM(300)                             LINEE FOGLIO
003600960319     D  FVSLNA                 1      3  0
003700960319     D  FVSFFV                 4    243
003800960319     D  FVSFF2               244    450
003900000504     D  FWSFF3               451    690
004000000504     D  FWSFF4               691    900
004100960318     D* FILIALI DI SCARICO
004200960318     D                 DS
004300000504     D  FLP                    1    900
004400000504     D                                     DIM(300)                             LINEE TRANSITO
004500960319     D  WLNA2                  1      3  0
004600960319     D  FVSFLP                 4    243
004700960319     D  FVSFL2               244    450
004800000504     D  FWSFL3               451    690
004900000504     D  FWSFL4               691    900
005000960228     D                 DS
005100960228     D  AA                     1      4  0
005200960228     D  MM                     5      6  0
005300960228     D  GG                     7      8  0
005400960228     D  WDATA                  1      8  0
005500960318     D WLBDAT          DS
005600960318     D  G02DAT                 1      8  0
005700960318     D  G02INV                 9     16  0
005800960318     D  G02ERR                17     17
005900960318     D  G02TGI                18     22  0
006000960229     D*
006100960228     D DSTC          E DS
006200960320     D DSTV          E DS
006300951110     D KPJBA         E DS
006400960229     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006500121105     D Ctfp            S              3  0
006600121105     D Ctfa            S              3  0
006700121105     D Cdde            S              8  0
006800121105     D Cdsc            S              8  0
006900130214     D savnot          S                   like(fv4not)
007000121105     d
007100121105     d wrkgetlista     s           4096    varying
007200960319     D*
007300960319     D* DEFINIZIONE COSTANTI
007400960319     D CMAN            C                   CONST(' ABBINATO MANUALM-
007500960319     D                                     ENTE ')
007600960319     D CAUT            C                   CONST(' ABBINATO IN AUTO-
007700960319     D                                     MAT. ')
007800960319     D CNAB            C                   CONST('  NON  ABBINATO  ')
007900960319     D CNCT            C                   CONST(' NON CONTROLLATO ')
008000960319     D CFAT            C                   CONST(' GIA'' FATTURATO ')
008100960319     D CANF            C                   CONST(' ANNULLATO DA FIL. ')
008200960319     D CANS            C                   CONST(' ANNULLATO DA SEDE ')
008300960320     D CNUT            C                   CONST('NON UTILIZZATO IN SE-
008400960320     D                                     DE')
008500960320     D CTOT            C                   CONST('XXXXXXXX  TOTALE MC.')
008600951110     I/SPACE 3
008700951110     C*****************************************************************
008800951110     C* RIEPILOGO INDICATORI
008900951110     C*****************************************************************
009000130214     C* 02    - VISUALIZZO motivo sbinamento
009100960320     C* 03    - IN RI IL TOTALE MC
009200960320     C* 04    - VISUALIZZO SCRITTA "ABBINATO MANUALMENTE" E
009300960319     C*                            "ABBINATO IN AUTOMAT."
009400960319     C* 05    - VISUALIZZO SCRITTA "NON ABBINATO" E "NON CONTROLLATO"
009500960320     C* 06    - ABILITO L'OPZIONE "1" DI SELEZIONE F.VIAGGIO
009600960319     C* 07    - VISUALIZZO SCRITTA "ANNULLATO"
009700130214     C* 08    - VISUALIZZO punto di scambio
009800960315     C* 09    - IMMESSA DATA FOGLIO VIAGGIO AL
009900960315     C* 10    - IMMESSA LA DATA FOGLIO VIAGGIO
010000960315     C* 11    - IMMESSO UNO SPECIFICO FOGLIO VIAGGIO
010100960315     C* 12    - IMMESSA UNA SPECIFICA LINEA DI PARTENZA
010200960318     C* 16    - ESISTE DESCRIZIONE RITARDO/ANTICIPO: EVIDENZIO COMANDO
010300960321     C* 17    - INDICA SE C'E' DESCRIZIONE RITARDO
010400960319     C* 18    - VISUALIZZO SCRITTA "ECCEZIONALE"
010500010622     C* 19    - NON VISUALIZZO IL CELLULARE DEL CONDUCENTE
010600960315     C* 20    - PROGRAMMA RICHIAMATO
010700960318     C* 21    - PULIZIA SUBFILE  TL37C03
010800960318     C* 23    - PULIZIA SUBFILE  TL37C04
010900960318     C* 24    - PULIZIA SUBFILE  TL37C05
011000960315     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
011100960318     C* 29    - READC   SUBFILE  TL37S04
011200960318     C* 30/34 - DI COMODO
011300010919     C* 35    - CHAIN SU TNFWS
011400960315     C* 90    - INDICATORE DI ERRORE GENERICO
011500951110     C*****************************************************************
011600951110     C     *ENTRY        PLIST
011700951110     C                   PARM                    KPJBA
011800960315     C                   MOVEL     KPJBU         DSTL37
011900951113     C*
012000951110     C*---------------------------------------------------------------*
012100960320     C* CONTROLLO SE OCCORRE ABILITARE L'OPZIONE "1" DI SELEZIONE FV
012200960320     C     D37FL2        IFEQ      'S'
012300960320     C                   SETON                                        06
012400960320     C                   ENDIF
012500960320     C*
012600960315     C* CONTROLLO SE IL PROGRAMMA E' RICHIAMATO (PARFLG <> " ")
012700960315    1C     D37FLG        IFNE      ' '
012800960315     C                   SETON                                        20
012900960315     C                   Z-ADD     D37LNP        V1CLNP
013000960315     C                   Z-ADD     D37NFV        V1CNFV
013100960318     C                   Z-ADD     D37DFD        WDFD
013200960318     C                   Z-ADD     D37DFA        WDFA
013300960318     C* GIRO DATA FOGLIO VIAGGIO DAL / AL
013400960318    2C     WDFD          IFGT      0
013500960318     C                   Z-ADD     WDFD          WDATA
013600960318     C                   Z-ADD     MM            GGMM              4 0
013700960318     C                   MOVEL     GG            GGMM
013800960318     C                   Z-ADD     AA            V1CDFD
013900960318     C                   MOVEL     GGMM          V1CDFD
014000960318    3C     WDFA          IFGT      0
014100960318     C                   Z-ADD     WDFA          WDATA
014200960321     C                   Z-ADD     MM            GGMM
014300960318     C                   MOVEL     GG            GGMM
014400960318     C                   Z-ADD     AA            V1CDFA
014500960318     C                   MOVEL     GGMM          V1CDFA
014600960318    3C                   ENDIF
014700960318    2C                   ENDIF
014800960315     C*
014900960315     C* CONTROLLO SE DEVO VISUALIZZARE LA PRIMA VIDEATA CON LA
015000960315     C*   RICHIESTA PARAMETRI (D37FLG = "P")
015100960315    2C     D37FLG        IFNE      'P'
015200960315     C                   GOTO      CONTR
015300960315    2C                   ENDIF
015400960315    1C                   ENDIF
015500960315     C*
015600960315     C     INIZIO        TAG
015700960318     C                   EXSR      PULD01
015800960315     C*
015900960318     C     FMT01         TAG
016000960315     C                   WRITE     TL37T01
016100960315     C                   EXFMT     TL37D01
016200960315     C*
016300960315     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
016400960315     C                   CLEAR                   V1CMSG
016500960315     C                   SETOFF                                       284142
016600960315     C                   SETOFF                                       4344
016700960315     C*
016800960315     C* CMD3  - FINE LAVORO
016900960315     C   KC              GOTO      FINE
017000960315     C*
017100960315     C     CONTR         TAG
017200960315     C* CONTROLLI 1° VIDEATA
017300960315     C                   EXSR      CTRD01
017400960318     C   90              GOTO      FMT01
017500960318     C*
017600960318     C* CONTROLLO RICHIESTE EFFETTUATE
017700960318     C                   EXSR      CTRRIC
017800960315     C*
017900960318     C* 10 ON  - RICHIESTA SELEZIONE PER   D A T A   F. V I A G G I O
018000960318    1C     *IN10         IFEQ      *ON
018100960315     C*
018200960315     C* CARICAMENTO SUBFILE PER SCELTA FOGLIO VIAGGIO
018300960315     C                   EXSR      RIES04
018400960315     C*
018500960318     C* NRR4 = "1" ---> NON E' STATO CARICATO NULLA
018600960315    2C     NRR4          IFEQ      1
018700960318     C     *IN20         IFEQ      *ON
018800960318     C                   GOTO      FINE
018900960318     C                   ELSE
019000960319     C                   MOVEL     MSG(9)        V1CMSG
019100960318     C                   SETON                                        28
019200960318     C                   GOTO      FMT01
019300960318     C                   ENDIF
019400960315    2C                   ENDIF
019500960315     C*
019600960315     C* VISUALIZZAZIONE SFL E SCELTA FOGLIO
019700960315     C                   Z-ADD     1             Z                 3 0
019800960318     C     FMT04         TAG
019900960315     C                   WRITE     TL37T01
020000960315     C                   WRITE     TL37Z04
020100960315     C                   EXFMT     TL37C04
020200960320     C* F3    - FINE
020300960320     C   KC              GOTO      FINE
020400960315     C*
020500960315     C* CMD12 - RITORNO
020600960318    2C     *INKL         IFEQ      *ON
020700960320     C   20D37FLG        IFEQ      'P'
020800960320     C                   GOTO      INIZIO
020900960320     C                   ELSE
021000960320     C                   GOTO      FINE
021100960320     C                   ENDIF
021200960320     C*
021300960318     C  N20              GOTO      INIZIO
021400960318    2C                   ENDIF
021500960315     C*
021600960318     C* LETTURA F.VIAGGIO SELEZIONATI
021700960315     C     LEGGI         TAG
021800960315     C                   READC     TL37S04                                29
021900960315     C*
022000960318     C   29              GOTO      FMT04
022100960315     C**
022200960315     C* NON SELEZIONATO NULLA
022300960318    2C     V4CSCE        IFNE      '1'                                          *SELEZIONE
022400960318     C     V4CSCE        ANDNE     '5'                                          *VISUALIZZ.
022500960315     C                   GOTO      LEGGI
022600960315    2C                   ENDIF
022700960315     C*
022800960315     C* SE NON E' RICHIAMATO VISUALIZZO IL FOGLIO CON L'OPZIONE "5"
022900960315    2C     *IN20         IFEQ      *OFF
023000960315     C* PULISCO IL CAMPO DI SCELTA SOLO SE CONTIENE L'OPZIONE "1"
023100960315    3C     V4CSCE        IFEQ      '1'
023200960315     C                   MOVEL     ' '           V4CSCE
023300960315     C                   UPDATE    TL37S04
023400960315     C                   GOTO      LEGGI
023500960315    3C                   ENDIF
023600960315     C*
023700960315   X2C                   ELSE
023800960315     C*
023900960315     C* SE PGM RICHIAMATO SELEZIONO IL FOGLIO CON L'OPZIONE "1"
024000960315    3C     V4CSCE        IFEQ      '1'
024100960320     C*
024200960320    4C     D37FL2        IFEQ      'S'
024300960315     C* PULISCO IL CAMPO DI SCELTA
024400960315     C                   MOVEL     ' '           V4CSCE
024500960315     C                   UPDATE    TL37S04
024600960315     C*
024700960320     C* PASSO INDIETRO IL PRIMO NUMERO DI FV SELEZIONATO
024800960315     C                   Z-ADD     V4CLNP        D37LNP
024900960315     C                   Z-ADD     V4CNFV        D37NFV
025000960315     C                   GOTO      FINE
025100960320     C*
025200960320   X4C                   ELSE
025300960320     C                   MOVEL     ' '           V4CSCE
025400960320     C                   UPDATE    TL37S04
025500960320     C                   GOTO      LEGGI
025600960320    4C                   ENDIF
025700960320     C*
025800960315    3C                   ENDIF
025900960315    2C                   ENDIF
026000960315     C*
026100960315     C* PULISCO IL CAMPO DI SCELTA
026200960315     C                   MOVEL     ' '           V4CSCE
026300960315     C                   UPDATE    TL37S04
026400960315     C*
026500960315     C                   Z-ADD     V4CLNP        V1CLNP
026600960315     C                   Z-ADD     V4CNFV        V1CNFV
026700960315    1C                   ENDIF
026800960315     C*
026900960315     C* RIEMPIO CAMPI 2° VIDEATA
027000960315     C                   EXSR      RIED02
027100960315     C*
027200960315     C     FOR02         TAG
027300960315     C                   WRITE     TL37T01
027400960315     C                   EXFMT     TL37D02
027500960315     C*
027600960315     C* F12- RITORNO
027700960318     C     *INKL         IFEQ      *ON
027800960320     C  N10
027900960320     CANN20              GOTO      INIZIO
028000960320     C*
028100960320     C  N10
028200960320     CAN 20D37FLG        IFEQ      'P'
028300960320     C                   GOTO      INIZIO
028400960320     C                   ELSE
028500960320     C                   GOTO      FINE
028600960320     C                   ENDIF
028700960320     C*
028800960318     C   10              GOTO      LEGGI
028900960318     C                   ENDIF
029000960315     C*
029100960315     C* F8 - INTERROGA ORARI ARRIVO
029200960315    1C     *INKH         IFEQ      *ON
029300960315     C                   EXSR      INTORA
029400960315     C                   GOTO      FOR02
029500960315    1C                   ENDIF
029600130214     C* F15 - INTERROGA motivi sbinamento
029700130214    1C     *INKP         IFEQ      *ON
029800130214     C                   EXSR      INTmot
029900130214     C                   GOTO      FOR02
030000130214    1C                   ENDIF
030100960315     C*
030200960318     C     FOR03         TAG
030300960318     C                   WRITE     TL37T01                                      TESTATA VIDEO
030400960318     C                   WRITE     TL37Z03
030500960318     C                   EXFMT     TL37C03
030600960318     C* F12 - RITORNO
030700960318     C   KL              GOTO      FOR02
030800960318     C*
030900960315     C* SE SELEZIONE PER DATA VISUALIZZO TUTTI I FV SELEZIONATI
031000960315     C   10              GOTO      LEGGI
031100960315     C*
031200960321    1C     *IN20         IFEQ      *OFF
031300960321     C  N10
031400960321     CAN 11              GOTO      INIZIO
031500960321   X1C                   ELSE
031600960321     C     D37FLG        IFEQ      'P'
031700960321     C                   GOTO      INIZIO
031800960321     C                   ELSE
031900960321     C                   GOTO      FINE
032000960321     C                   ENDIF
032100960321    1C                   ENDIF
032200960307     C*
032300951110     C     FINE          TAG
032400960321     C* RICHIAMO PGM FNLV15R PER CHIUSURA FILES
032500960321    1C     WLV15         IFNE      ' '
032600960321     C                   CLEAR                   DSLV15
032700960321     C                   MOVEL     'C'           D15TLA
032800960321     C                   MOVEL     DSLV15        KPJBU
032900960321     C                   CALL      'FNLV15R'
033000960321     C                   PARM                    KPJBA
033100960321    1C                   ENDIF
033200960321     C*
033300960315     C   20              MOVEL     DSTL37        KPJBU
033400951113     C                   SETON                                        LR
033500960117     C*
033600960315     C*--- CONTROLLO RICHIESTE EFFETTUATE ----------------------------*
033700960315     C     CTRRIC        BEGSR
033800960315     C                   SETOFF                                       091011
033900960315     C                   SETOFF                                       12
034000960318     C*
034100960315     C* 12 ON  - IMMESSA UNA SPECIFICA LINEA DI PARTENZA
034200960318    1C     V1CLNP        IFGT      0
034300960315     C                   SETON                                        12
034400960315    1C                   ENDIF
034500960315     C*
034600960315     C* 11 ON  - IMMESSO UNO SPECIFICO FOGLIO VIAGGIO
034700960318    1C     V1CNFV        IFGT      0
034800960315     C                   SETON                                        1112
034900960315     C*
035000960315   X1C                   ELSE
035100960315     C*
035200960315     C* 10 ON  - IMMESSA LA DATA FOGLIO VIAGGIO
035300960318    2C     WDFD          IFGT      0
035400960315     C                   SETON                                        10
035500960315     C*
035600960318    3C     WDFA          IFEQ      0
035700960318     C                   Z-ADD     WDFD          WDFA
035800960315   X3C                   ELSE
035900960315     C* 09 ON  - IMMESSA DATA FOGLIO VIAGGIO AL
036000960318     C     WDFA          IFNE      WDFD
036100960315     C                   SETON                                        09
036200960315     C                   ENDIF
036300960315    3C                   ENDIF
036400960315     C*
036500960315    2C                   ENDIF
036600960315    1C                   ENDIF
036700960315     C*
036800960315     C                   ENDSR
036900960315     C*
037000960315     C*--- PULIZIA CAMPI 1° VIDEATA ----------------------------------*
037100960318     C     PULD01        BEGSR
037200960315     C                   SETOFF                                       10
037300960315     C                   CLEAR                   V1CLNP
037400960315     C                   CLEAR                   V1CNFV
037500960315     C                   CLEAR                   V1CDFD
037600960315     C                   Z-ADD     UDATE8        V1CDFA
037700960320     C                   MOVEL     'S'           V1CSCE
037800130318     C                   CLEAR                   V1Cbis
037900130318     C                   CLEAR                   V1Cpsc
038000960315     C                   ENDSR
038100960302     C*
038200960315     C*--- CONTROLLI 1° VIDEATA --------------------------------------*
038300960315     C     CTRD01        BEGSR
038400960315     C                   SETOFF                                       90
038500960315     C*
038600960315     C****  LINEA DI PARTENZA  ****
038700960315    1C     V1CLNP        IFNE      0
038800960315     C     V1CLNP        CHAIN     AZORG                              31
038900960315    2C     *IN31         IFEQ      *ON
039000971203     C     ORGFVA        ORNE      ' '
039100971203     C     ORGFAG        ORNE      'A'
039200971203     C     ORGFAG        ANDNE     'F'
039300960315     C                   MOVEL     MSG(1)        V1CMSG
039400960315     C                   SETON                                        412890
039500960315     C                   GOTO      ENDCTR
039600960315    2C                   ENDIF
039700960315    1C                   ENDIF
039800960315     C*
039900960315     C****  OCCORRE IMMETTERE O IL NUMERO FOGLIO O LA DATA  ****
040000960315    1C     V1CNFV        IFEQ      0
040100960315     C     V1CDFD        ANDEQ     0
040200960315     C                   MOVEL     MSG(3)        V1CMSG
040300960315     C                   SETON                                        422890
040400960315     C                   GOTO      ENDCTR
040500960315    1C                   ENDIF
040600960315     C*
040700960315     C****  IMPOSSIBILE INSERIRE ENTRAMBI  ****
040800960315    1C     V1CNFV        IFNE      0
040900960315     C     V1CDFD        ANDNE     0
041000960315     C                   MOVEL     MSG(4)        V1CMSG
041100960315     C                   SETON                                        422890
041200960315     C                   GOTO      ENDCTR
041300960315    1C                   ENDIF
041400960315     C*
041500960315     C****  NUMERO FOGLIO VIAGGIO  ****
041600960315    1C     V1CNFV        IFNE      0
041700960315     C* SE IMMESSO IL FOGLIO VIAGGIO OCCORRE INSERIRE ANCHE LA LNP
041800960315    2C     V1CLNP        IFEQ      0
041900960315     C                   MOVEL     MSG(5)        V1CMSG
042000960315     C                   SETON                                        412890
042100960315     C                   GOTO      ENDCTR
042200960315    2C                   ENDIF
042300960315     C*
042400960315     C* CONTROLLO VALIDITA' NUMERO FOGLIO VIAGGIO
042500960315     C     KFVS          CHAIN     TNFVS01L                           31
042600960315    2C     *IN31         IFEQ      *ON
042700960315     C                   MOVEL     MSG(6)        V1CMSG
042800960315     C                   SETON                                        422890
042900960315     C                   GOTO      ENDCTR
043000960315    2C                   ENDIF
043100960315    1C                   ENDIF
043200960315     C*
043300960315     C****  DATA FOGLIO VIAGGIO  DAL  ****
043400960315    1C     V1CDFD        IFNE      0
043500960315     C                   MOVEL     V1CDFD        G02DAT
043600960315     C                   MOVEL     *ZERO         G02INV
043700960315     C                   MOVEL     *BLANKS       G02ERR
043800960315     C                   CALL      'XSRDA8'
043900960315     C                   PARM                    WLBDAT
044000960315     C     G02ERR        IFEQ      '1'
044100960315     C                   MOVEL     MSG(7)        V1CMSG
044200960315     C                   SETON                                        432890
044300960315     C                   GOTO      ENDCTR
044400960315     C                   ENDIF
044500960315     C                   Z-ADD     G02DAT        V1CDFD
044600960315     C                   Z-ADD     G02INV        WDFD
044700960315    1C                   ENDIF
044800960315     C*
044900960315     C****  DATA FOGLIO VIAGGIO  AL  ****
045000960315    1C     V1CDFA        IFNE      0
045100960315     C                   MOVEL     V1CDFA        G02DAT
045200960315     C                   MOVEL     *ZERO         G02INV
045300960315     C                   MOVEL     *BLANKS       G02ERR
045400960315     C                   CALL      'XSRDA8'
045500960315     C                   PARM                    WLBDAT
045600960315     C     G02ERR        IFEQ      '1'
045700960315     C                   MOVEL     MSG(7)        V1CMSG
045800960315     C                   SETON                                        442890
045900960315     C                   GOTO      ENDCTR
046000960315     C                   ENDIF
046100960315     C                   Z-ADD     G02DAT        V1CDFA
046200960315     C                   Z-ADD     G02INV        WDFA
046300960315     C*
046400960315     C* DATA DAL NO MAGGIORE DATA AL
046500960315    2C     WDFD          IFGT      WDFA
046600960315     C                   MOVEL     MSG(8)        V1CMSG
046700960315     C                   SETON                                        432890
046800960315     C                   GOTO      ENDCTR
046900960315    2C                   ENDIF
047000960315    1C                   ENDIF
047100960315     C*
047200960315     C     ENDCTR        ENDSR
047300960315     C*
047400960315     C*--- CARICAMENTO SFL PER SCELTA F.VIAGGIO ----------------------*
047500960315     C     RIES04        BEGSR
047600960318     C                   Z-ADD     1             NRR4              5 0
047700960318     C*
047800960318     C****  PULIZIA SUBFILE  ****
047900960318     C                   SETON                                        23
048000960318     C                   WRITE     TL37C04
048100960318     C                   SETOFF                                       23
048200960315     C*
048300960318     C****  CARICAMENTO SUBFILE  ****
048400960318     C* 12 ON  - IMMESSA UNA SPECIFICA LINEA PARTENZA
048500960318    1C     *IN12         IFEQ      *ON
048600960315     C*
048700960315     C     KFVS2         SETLL     TNFVS03L
048800960315     C  N09KFVS2         READE     TNFVS03L                               30
048900960315     C   09V1CLNP        READE     TNFVS03L                               30
049000960315     C*
049100960318    2C     *IN30         DOWEQ     *OFF
049200960315     C*
049300960315     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
049400960318    3C   09FVSDFV        IFGT      WDFA
049500960315     C                   SETON                                        30
049600960318    3C                   ENDIF
049700960315     C*
049800960318    3C     *IN30         IFEQ      *OFF
049900960318     C* SCRIVO CAMPI DEL SFL
050000960318     C                   EXSR      WRTS04
050100960315     C*
050200960315     C  N09KFVS2         READE     TNFVS03L                               30
050300960315     C   09V1CLNP        READE     TNFVS03L                               30
050400960318    3C                   ENDIF
050500960318    2C                   ENDDO
050600960315     C*
050700960318   X1C                   ELSE
050800960318     C* 12 OFF - RICHIESTE TUTTE LE LINEE DI PARTENZA
050900960318     C*
051000960315     C                   CLEAR                   WFIL
051100960315     C                   SETOFF                                       30
051200960315     C*
051300960318    2C     *IN30         DOWEQ     *OFF
051400960315     C     WFIL          SETGT     TNFVS03L
051500960315     C                   READ      TNFVS03L                               30
051600960315     C*
051700960318    3C     *IN30         IFEQ      *OFF
051800960315     C                   Z-ADD     FVSLNP        WFIL
051900960315     C*
052000960320     C     KFVS3         SETLL     TNFVS03L
052100960315     C  N09KFVS3         READE     TNFVS03L                               31
052200960315     C   09WFIL          READE     TNFVS03L                               31
052300960315     C*
052400960318    4C     *IN31         DOWEQ     *OFF
052500960315     C*
052600960315     C* SE DATA FOGLIO VIAGGIO > DATA AL ---> LEGGO ALTRA LINEA
052700960318    5C   09FVSDFV        IFGT      WDFA
052800960318     C                   SETON                                        31
052900960318    5C                   ENDIF
053000960315     C*
053100960318    5C     *IN31         IFEQ      *OFF
053200960318     C* SCRIVO CAMPI DEL SFL
053300960318     C                   EXSR      WRTS04
053400960315     C*
053500960315     C  N09KFVS3         READE     TNFVS03L                               31
053600960315     C   09WFIL          READE     TNFVS03L                               31
053700960318    5C                   ENDIF
053800960318    4C                   ENDDO
053900960315     C*
054000960318    3C                   ENDIF
054100960318    2C                   ENDDO
054200960315     C*
054300960318    1C                   ENDIF
054400960315     C*
054500960315     C                   ENDSR
054600960315     C*
054700960318     C*--- SCRIVO CAMPI SFL SELEZIONE F.VIAGGIO ----------------------*
054800960318     C     WRTS04        BEGSR
054900960320     C                   MOVEL     'S'           WCARIC            1
055000960320     C                   CLEAR                   §TVDES
055100960320     C*
055200960320     C* CONTROLLO IL TIPO TRAINO
055300960320    1C     FVSTTR        IFNE      ' '
055400960320     C     V1CSCE        ANDNE     'T'
055500960320     C                   MOVEL     'TV'          COD
055600960320     C                   MOVEL(P)  FVSTTR        KEY
055700960320     C     KTAB          CHAIN     TABEL                              32
055800960320     C*
055900960320    2C     *IN32         IFEQ      *OFF
056000960320     C     TBLFLG        ANDEQ     ' '
056100960320     C                   MOVEL     TBLUNI        DSTV
056200960320     C* VISULIZZO SOLO I NON UTILIZZATI IN SEDE
056300960320    3C     §TVUTC        IFEQ      'S'
056400960320     C     V1CSCE        ANDEQ     'N'
056500960320     C                   MOVEL     'N'           WCARIC
056600960320    3C                   ENDIF
056700960320     C* VISULIZZO SOLO GLI UTILIZZATI IN SEDE
056800960320    3C     §TVUTC        IFNE      'S'
056900960320     C     V1CSCE        ANDEQ     'S'
057000960320     C                   MOVEL     'N'           WCARIC
057100960320    3C                   ENDIF
057200960320     C*
057300960320    2C                   ENDIF
057400960320    1C                   ENDIF
057500960320     C*
057600960320     C* VISULIZZO SOLO I NON UTILIZZATI IN SEDE
057700960320    3C     FVSTTR        IFEQ      ' '
057800960320     C     V1CSCE        ANDEQ     'N'
057900960320     C                   MOVEL     'N'           WCARIC
058000960320    3C                   ENDIF
058100130318     c
058200130318     c* Visualizzo solo i BIS
058300130318     c                   if        v1cbis='B' and fvsbis=' '
058400130318     C                   MOVEL     'N'           WCARIC
058500130318    3C                   ENDIF
058600130318     c                   if        v1cbis='N' and fvsbis='B'
058700130318     C                   MOVEL     'N'           WCARIC
058800130318    3C                   ENDIF
058900130318     c**
059000130318      ** Visualizzo solo quello con punto di scambio
059100130318     c                   if        v1cpsc<>' '
059200130318     C     kfWs          setll     tNFGs000
059300130318     c                   if        %equal(tNFGS01l) and v1cpsc='N'
059400130318     C                   MOVEL     'N'           WCARIC
059500130318    3C                   ENDIF
059600130318     c                   if        not %equal(tNFGS01l) and v1cpsc='S'
059700130318     C                   MOVEL     'N'           WCARIC
059800130318    3C                   ENDIF
059900130318    3C                   ENDIF
060000960320     C**
060100960320     C** DA CARICARE
060200960320     C     WCARIC        IFEQ      'S'
060300960315     C* CAMPO SELEZIONE
060400960315     C                   CLEAR                   V4CSCE
060500960315     C* LINEA PARTENZA
060600960315     C                   MOVEL     FVSLNP        V4CLNP
060700960315     C     FVSLNP        CHAIN     AZORG                              32
060800960320     C  N32ORGDE5        IFNE      *BLANKS
060900130319     C**                 MOVEL     ORGDE5        V4DLNP
061000960320     C                   ELSE
061100130319     C**                 MOVEL     ORGDES        V4DLNP
061200960320     C                   ENDIF
061300130319     C** 32              MOVEL     *BLANKS       V4DLNP
061400960315     C* NUMERO FOGLIO VIAGGIO
061500960315     C                   MOVEL     FVSNFV        V4CNFV
061600130318     C                   MOVEL     FVSbis        V4Cbis
061700960315     C* GIRO DATA FOGLIO VIAGGIO
061800960318     C                   Z-ADD     FVSDFV        WDATA
061900960321     C                   Z-ADD     MM            GGMM
062000960318     C                   MOVEL     GG            GGMM
062100130318     C                   Z-ADD     AA            V4CDFv
062200130318     C                   MOVEL     GGMM          V4CDFv
062300960315     C* TIPO TRAINO
062400130318     C****               MOVEL     FVSTTR        V4CTTR
062500960315     C* GIRO DATA PARTENZA REALE
062600130318     C**                 Z-ADD     FVSDRP        WDATA
062700130318     C**                 Z-ADD     MM            GGMM
062800130318     C**                 MOVEL     GG            GGMM
062900130318     C**                 Z-ADD     GGMM          V4CDRP
063000960315     C* LINEA FINALE DI ARRIVO
063100960315     C                   MOVEL     FVSLNA        V4CLNA
063200960315     C     FVSLNA        CHAIN     AZORG                              32
063300960320     C  N32ORGDE5        IFNE      *BLANKS
063400960320     C                   MOVEL     ORGDE5        V4DLNA
063500960320     C                   ELSE
063600960320     C                   MOVEL     ORGDES        V4DLNA
063700960320     C                   ENDIF
063800960315     C   32              MOVEL     *BLANKS       V4DLNA
063900960315     C* NUMERO TRAINO
064000960315     C                   MOVEL     FVSTRN        V4CTRN
064100960315     C* SE F.VIAGGIO ANNULLATO LO SEGNALO
064200960315     C                   CLEAR                   V4CANN
064300960315    1C     FVSATB        IFNE      ' '
064400960315     C                   MOVEL     'An'          V4CANN
064500960315    1C                   ENDIF
064600130318     c* indico se c'e' lo scambio
064700130319     C     kfWs          chain     tNFGs000
064800130319     c                   if        %found(tnfgs01l)
064900130319     c
065000130319     c                   if        fgstrns>0
065100130319     c                   eval      V4Cpsc=%editc(fgstrns:'Z')
065200130319     c                   else
065300130319     c                   movel     fgspsc        v4cpsc
065400130319     c                   endif
065500130319     c
065600130318     c                   else
065700130319     c                   clear                   V4Cpsc
065800130318     c                   endif
065900960320     C* TRAINO DI SEDE
066000960320     C                   MOVEL     FVSTRS        V4CTRS
066100960320     C* TRAZIONISTA
066200960320     C                   MOVEL     FVSDPD        V4CDPD
066300960315     C*
066400960315     C                   WRITE     TL37S04
066500960315     C                   ADD       1             NRR4
066600960320     C                   ENDIF
066700960320     C*
066800960315     C                   ENDSR
066900960315     C*
067000960315     C*--- RIEMPO CAMPI 2° VIDEATA -----------------------------------*
067100960315     C     RIED02        BEGSR
067200121105     C                   SETOFF                                       071608
067300960319     C                   SETOFF                                       180405
067400960319     C                   CLEAR                   V2DTFM
067500960319     C                   CLEAR                   V2DTFS
067600960319     C                   CLEAR                   V2DABN
067700960319     C                   CLEAR                   V2DAB2
067800061003     C                   CLEAR                   V2can1
067900061003     C                   CLEAR                   V2can2
068000960315     C*
068100960315     C     KFVS          CHAIN     TNFVS01L                           33
068200010919     C     KFVS          CHAIN     TNFWS01L                           35
068300960319     C*
068400960319    1C     *IN33         IFEQ      *OFF
068500960319     C                   MOVEL     FVSLNA        WLNA2
068600960315     C*
068700960319     C* SE FOGLIO VIAGGIO ANNULLATO ---> LO SEGNALO
068800960319    2C     FVSATB        IFNE      ' '
068900960315     C                   SETON                                        07
069000960319     C     FVSATB        IFEQ      '*'
069100960319     C                   MOVEL     CANF          V2DANN                         *DA FILIALE
069200960319     C                   ENDIF
069300960319     C     FVSATB        IFEQ      'A'
069400960319     C                   MOVEL     CANS          V2DANN                         *DA SEDE
069500960319     C                   ENDIF
069600960319    2C                   ENDIF
069700960315     C*
069800960319     C* NUMERO FOGLIO VIAGGIO
069900960315     C                   MOVEL     FVSNFV        V2CNFV
070000960315     C* GIRO DATA FOGLIO VIAGGIO
070100960315     C                   Z-ADD     FVSDFV        WDATA
070200960321     C                   Z-ADD     MM            GGMM
070300960315     C                   MOVEL     GG            GGMM
070400960315     C                   Z-ADD     AA            V2CDFV
070500960315     C                   MOVEL     GGMM          V2CDFV
070600960315     C*
070700960319     C* DATI RELATIVI AL TRAINO DI FILIALE
070800960315     C                   MOVEL     FVSTRN        V2CTRN
070900960319     C                   MOVEL     FVSTFM        V2CTFM
071000960319    2C     FVSTFM        IFEQ      'A'
071100960319     C                   MOVEL     'ANDATA '     V2DTFM
071200960319    2C                   ENDIF
071300960319    2C     FVSTFM        IFEQ      'R'
071400960319     C                   MOVEL     'RITORNO'     V2DTFM
071500960319    2C                   ENDIF
071600960319     C*
071700960319     C* DATI RELATIVI AL TRAINO DI SEDE
071800960315     C                   MOVEL     FVSTRS        V2CTRS
071900960319     C                   MOVEL     FVSTFS        V2CTFS
072000960319    2C     FVSTFS        IFEQ      'A'
072100960319     C                   MOVEL     'ANDATA '     V2DTFS
072200960319    2C                   ENDIF
072300960319    2C     FVSTFS        IFEQ      'R'
072400960319     C                   MOVEL     'RITORNO'     V2DTFS
072500960319    2C                   ENDIF
072600960315     C*
072700960319     C* SE TRAINO DI SEDE E' ECCEZIONALE LO SEGNALO
072800960319    2C     FVSTRS        IFGE      9000000
072900960319     C     FVSTRS        ANDLT     9999998
073000960319     C                   SETON                                        18
073100960319    2C                   ENDIF
073200960319     C*
073300960319     C* ABBINATO MANUALMENTE
073400960319    2C     FVSTRS        IFGT      0
073500960319     C     FVSTRS        ANDNE     FVSTRN
073600960319     C     FVSTRS        ANDLT     9999998
073700960319     C     FVSTRS        ORGE      9000000
073800960319     C     FVSTRS        ANDLT     9999998
073900960319     C                   SETON                                        04
074000960319     C                   MOVEL     CMAN          V2DABN
074100960319    2C                   ENDIF
074200960319     C* ABBINATO IN AUTOMATICO
074300960319    2C     FVSTRS        IFGT      0
074400960319     C     FVSTRS        ANDLT     9000000
074500960319     C     FVSTRS        ANDEQ     FVSTRN
074600960319     C                   SETON                                        04
074700960319     C                   MOVEL     CAUT          V2DABN
074800960319    2C                   ENDIF
074900960319     C* NON ABBINATO
075000960319    2C     FVSTRS        IFEQ      0
075100960319     C     FVSABN        ANDEQ     'S'
075200960319     C                   SETON                                        05
075300960319     C                   MOVEL     CNAB          V2DAB2
075400960319    2C                   ENDIF
075500960319     C* NON CONTROLLATO
075600960319    2C     FVSABN        IFEQ      ' '
075700960319     C                   SETON                                        05
075800960319     C                   MOVEL     CNCT          V2DAB2
075900960319    2C                   ENDIF
076000960319     C* GIA' FATTURATO
076100960319    2C     FVSTRS        IFEQ      9999998
076200960319     C                   SETON                                        05
076300960319     C                   MOVEL     CFAT          V2DAB2
076400960319    2C                   ENDIF
076500960320     C* NIN UTILIZZATO NEI CONTROLLI DI SEDE
076600960320     C     FVSTRS        IFEQ      9999999
076700960320     C     FVSATB        ANDEQ     ' '
076800960320     C                   SETON                                        04
076900960320     C                   MOVEL     CNUT          V2DABN
077000960320    2C                   ENDIF
077100960319     C*
077200960319     C* LINEA PARTENZA
077300960315     C                   MOVEL     FVSLNP        V2CLNP
077400960320     C                   MOVEL     FVSLNP        V2CLN2
077500960315     C     FVSLNP        CHAIN     AZORG                              32
077600960315     C  N32              MOVEL     ORGDES        V2DLNP
077700960315     C   32              MOVEL     *BLANKS       V2DLNP
077800960319     C* LINEA ARRIVO
077900960315     C                   MOVEL     FVSLNA        V2CLNA
078000960315     C     FVSLNA        CHAIN     AZORG                              32
078100960315     C  N32              MOVEL     ORGDES        V2DLNA
078200960315     C   32              MOVEL     *BLANKS       V2DLNA
078300960315     C*
078400960319     C* TIPO TRAINO
078500960315     C                   MOVEL     FVSTTR        V2CTTR
078600960315     C                   MOVEL     'TV'          COD
078700960315     C                   MOVEL(P)  FVSTTR        KEY
078800960315     C     KTAB          CHAIN     TABEL                              32
078900960315     C  N32              MOVEL     TBLUNI        V2DTTR
079000960315     C   32              CLEAR                   V2DTTR
079100960319     C* CATEGORIA TRAINO (BIS/ECCEZIONALE)
079200960315     C                   MOVEL     FVSBIS        V2CBIS
079300960319     C* TIPO AUTOMEZZO
079400960315     C                   MOVEL     FVSTMZ        V2CTMZ
079500960315     C                   MOVEL     'TU'          COD
079600960315     C                   MOVEL(P)  FVSTMZ        KEY
079700960315     C     KTAB          CHAIN     TABEL                              32
079800960315     C  N32              MOVEL     TBLUNI        V2DTMZ
079900960315     C   32              CLEAR                   V2DTMZ
080000960319     C* TRAZIONISTA
080100960315     C                   MOVEL     FVSPDR        V2CPDR
080200960315     C                   MOVEL     FVSDPD        V2CDPD
080300960319     C* CONDUCENTE
080400130429     C                   MOVEL     FVSpmb        V2CDCN
080500010622     C* CELLULARE CONDUCENTE
080600010702     C                   MOVE      'P'           WEPA
080700010702     C                   MOVE      *BLANKS       WLAI
080800010622     C                   MOVE      'L'           WTRC
080900010702     C     KFV4          CHAIN     FNFV401L                           30
081000010622     C     *IN30         IFEQ      *ON
081100010622     C                   MOVE      *ON           *IN19
081200010622     C                   ELSE
081300010622     C                   MOVEL     FV4NOT        V2CCNC
081400010622     C                   ENDIF
081500960319     C* TARGHE
081600960315     C                   MOVEL     FVSTRM        V2CTRM
081700960315     C                   MOVEL     FVSTRR        V2CTRR
081800960319     C* IMPORTI
081900010919     C**!!!                MOVELFVSIPT    V2CIPT
082000010919     C**!!!                MOVELFVSIES    V2CIES
082100010919     C                   CLEAR                   V2CIPT
082200010919     C                   CLEAR                   V2CIES
082300010919     C                   CLEAR                   V2CVIP
082400010919     C                   CLEAR                   V2CVES
082500010919      * Se il file dell'estensione non esiste o i campi sono a zero
082600010919      * devo prendere quello che c'è nel file principale
082700010919     C     *IN35         IFEQ      *ON
082800010919     C     FVSIPT        IFGT      *ZEROS
082900010919     C                   Z-ADD     FVSIPT        V2CIPT
083000010919     C                   MOVEL     'ITL'         V2CVIP
083100010919     C                   ENDIF
083200010919     C     FVSIES        IFGT      *ZEROS
083300010919     C                   Z-ADD     FVSIES        V2CIES
083400010919     C                   MOVEL     'ITL'         V2CVES
083500010919     C                   ENDIF
083600010919      * Se il file dell'estensione esiste e i campi sono a zero
083700010919      * devo prendere quello che c'è nel file principale
083800010919     C                   ELSE
083900010919     C     FWSIPT        IFEQ      *ZEROS
084000010919     C     FVSIPT        ANDGT     *ZEROS
084100010919     C                   Z-ADD     FVSIPT        V2CIPT
084200010919     C                   MOVEL     'ITL'         V2CVIP
084300010919     C                   ENDIF
084400010919     C     FWSIES        IFEQ      *ZEROS
084500010919     C     FVSIES        ANDGT     *ZEROS
084600010919     C                   Z-ADD     FVSIES        V2CIES
084700010919     C                   MOVEL     'ITL'         V2CVES
084800010919     C                   ENDIF
084900010919      * Se il file dell'estensione esiste e i campi sono > zero
085000010919      * devo prendere quello che c'è nel file dell'estensione
085100010919     C     FWSIPT        IFGT      *ZEROS
085200010919     C                   Z-ADD     FWSIPT        V2CIPT
085300010919     C                   MOVEL     FWSVIP        V2CVIP
085400010919     C                   ENDIF
085500010919     C     FWSIES        IFGT      *ZEROS
085600010919     C                   Z-ADD     FWSIES        V2CIES
085700010919     C                   MOVEL     FWSVES        V2CVES
085800010919     C                   ENDIF
085900010919     C                   ENDIF
086000960319     C* PIOMBI
086001130424     c                   clear                   v2cpmb
086002130424     C                   MOVE      'P'           WEPA
086003130424     C                   MOVE      *BLANKS       WLAI
086004130424     C                   MOVE      'P'           WTRC
086005130424     C     KFV4          CHAIN     FNFV401L
086006130424     c
086007130424     C                   if        %found(fnfv401l) and fv4atb=' '
086008130424     C                   MOVEL     FV4NOT        V2Cpmb
086009130424     c     KFV4          reade     FNFV401L
086010130424     C                   if        not %eof(fnfv401l) and fv4atb=' '
086011130424  156C                   eval      %subst(v2cpmb:36)=fv4not
086012130424     c                   endif
086013130424     c                   endif
086200960319     C* NOTE
086300960315     C                   MOVEL     FVSNT1        V2CNT1
086400960315     C                   MOVEL     FVSNT2        V2CNT2
086500061003     c* Annotazioni
086600061003     C                   MOVE      'P'           WEPA
086700061003     C                   MOVE      *BLANKS       WLAI
086800061003     C                   MOVE      'N'           WTRC
086900061003     C     KFV4          CHAIN     FNFV401L                           30
087000061003     c
087100061003     C                   if        not *in30 and fv4atb=' '
087200061003     C                   MOVEL     FV4NOT        V2Can1
087300061003     c     KFV4          reade     FNFV401L                               30
087400061003     C                   if        not *in30 and fv4atb=' '
087500061003     C                   MOVEL     FV4NOT        V2Can2
087600061003     c                   endif
087700061003     c                   endif
087800130214
087900130214     c* Verifico se presente motivo di sbinammento
088000130214     C                   MOVE      'P'           WEPA
088100130214     C                   MOVE      *BLANKS       WLAI
088200130214     C                   MOVE      'C'           WTRC
088300130214     C     KFV4          setll     FNFV401L                               02
088400121105     c*
088500121105     c* Carico punto di scambio
088600121105     c                   clear                   v2cpsc
088700121105     c                   clear                   v2dpsc
088800121105     c                   clear                   v2ctrns
088900121105     c                   clear                   v2dtrns
089000121105     c                   clear                   v2chhs
089100121105     c                   clear                   v2cdgs
089200121105     c                   clear                   v2clnar
089300121105     c                   clear                   v2dlnar2
089400121105     C     kfvs          chain     tNFGs000
089500121105if  1c                   if        (not %found(tNFGs01L)
089600121105     c                              or  FGsatb <> *blanks)
089700121105     C                   clear                   fgspsc
089800121105     c                   setoff                                       08
089900121105e   1C                   endif
090000121105     c
090100121105    1c                   if        fgspsc<>*blanks
090200121105     c                   seton                                        08
090300121105     c                   eval      v2cpsc=fgspsc
090400121105     c     v2cpsc        chain     tnpsc01l
090500121105    2c                   if        %found(tnpsc01l)
090600121105     c                   eval      v2dpsc=pscloc
090700121105     c***                eval      despind=pscind
090800121105     c***                eval      desploc=%trim(psccap) + ' ' +
090900121105     c***                          %trim(pscloc)+'  ' +pscprv+pscnar
091000121105     c
091100121105   x2c                   else
091200121105     c                   eval      v2dpsc=*all'?'
091300121105     c***                eval      despind=*all'?'
091400121105    2c                   endif
091500121105     c
091600121105    2c                   if        fgstrns>0
091700121105     c                   eval      v2ctrns=fgstrns
091800121105     c                   exsr      Ctrtrns
091900121105    2c                   endif
092000121105
092100121105     c                   eval      v2chhs=fgshhs
092200121105     C                   MOVE      FGsdgs        G02INV
092300121105     C                   MOVE      *ZEROS        G02DAT
092400121105     C                   MOVEL     '3'           G02ERR
092500121105     C                   CALL      'XSRDA8'
092600121105     C                   PARM                    WLBDAT
092700121105     C                   Z-ADD     G02DAT        v2cdgs
092800121105     c
092900121105     c* linea finale arrivo autista
093000121105    2c                   if        fgslnar>0
093100121105     c                   move      fgslnar       v2clnar
093200121105     C     fgslnar       CHAIN     AZORG                              32
093300121105     c***                movel     orgdes        deslnar
093400121105     c  n32              movel     orgdes        v2dlnar2
093500121105     c   32              eval      v2dlnar2=*all'?'
093600121105    2c                   endif
093700121105    1c                   endif
093800121105      *
093900061003     c
094000960315     C*
094100960318     C* CARICO LE LINEE DI ARRIVO E LE FILIALI DI SCARICO
094200960319     C                   EXSR      RIES03
094300960319    1C                   ENDIF
094400960315     C*
094500960315     C                   ENDSR
094600121105     C*-----------------------------------------------------***********
094700121105     C* CONTROLLO Traino da cui copiare-uso sql per non sporcarmi i campi
094800121105     C*-----------------------------------------------------***********
094900121105     C     CTRtrns       BEGSR
095000121105     c                   clear                   woktrns           1
095100121105     c                   eval      wrkgetlista =
095200121105     c                             'select tltdde, tltdsc, tlttfp, tlttfa +
095300121105     c                               from tntlt00f where tltatb= '' '' +
095400121105     c                               and tltdde<=' + %editc(fvsdfv:'X') +
095500121105     c                               ' and tlttrn= ' + %editc(v2ctrns:'X')
095600121105
095700121105     C/EXEC SQL
095800121105     C+ PREPARE s1 FROM :wrkgetlista
095900121105     C/END-EXEC
096000121105     C
096100121105     C/EXEC SQL
096200121105     C+ DECLARE A1 CURSOR FOR S1
096300121105     C/END-EXEC
096400121105     C
096500121105     C/EXEC SQL
096600121105     C+ OPEN a1
096700121105     C/END-EXEC
096800121105
096900121105      *?Leggo il file
097000121105     c                   do        *hival
097100121105     C/EXEC SQL
097200121105     C+ FETCH NEXT FROM a1 INTO : Cdde, :Cdsc, :Ctfp, :Ctfa
097300121105     C/END-EXEC
097400121105
097500121105     c                   select
097600121105
097700121105     c                   when      sqlcod = 100
097800121105     c                   leave
097900121105     c                   when      sqlcod < 0
098000121105     c                   seton                                        H1
098100121105     c                   leave
098200121105     c                   other
098300121105     c
098400121105     c                   if        Cdsc>=fvsdfv  and Cdde<=fvsdfv
098500121105     c                   eval      woktrns='S'
098600121105     c* Decodico  partenza e arrivo
098700121105     c                   eval      v2dtrns='Da '+%editc(Ctfp:'X') + ' a '+
098800121105     c                             %editc(Ctfa:'X')
098900121105     c                   endif
099000121105     c
099100121105     c                   endsl
099200121105
099300121105     c                   enddo
099400121105
099500121105     C/EXEC SQL
099600121105     C+ CLOSE a1
099700121105     C/END-EXEC
099800121105
099900121105     c                   ENDSR
100000960315     C*
100100960315     C*--- CONTROLLO SE ESISTE DESCRIZIONE RITARDO -------------------*
100200960315     C     CTRDRA        BEGSR
100300960315     C                   CLEAR                   W001A
100400960315     C*
100500960315     C     KFV4          SETLL     FNFV401L                               32
100600960315    1C     *IN32         IFEQ      *ON
100700960315     C     KFV4          READE     FNFV401L                               32
100800960315    2C     *IN32         DOWEQ     *OFF
100900960315    3C     FV4ATB        IFEQ      ' '
101000960318     C                   MOVEL     '1'           W001A             1
101100960315     C                   SETON                                        32
101200960315   X3C                   ELSE
101300960315     C     KFV4          READE     FNFV401L                               32
101400960315    3C                   ENDIF
101500960315    2C                   ENDDO
101600960315    1C                   ENDIF
101700960315     C*
101800960315     C                   ENDSR
101900960315     C*
102000960318     C*--- RIEMPIO SFL DELLE LINEE -----------------------------------*
102100960319     C     RIES03        BEGSR
102200000504     C*
102300000504     C     KFWS          CHAIN     TNFWS01L                           39
102400000504     C     *IN39         IFEQ      *ON
102500000504     C     FWSATB        ORNE      ' '
102600000504     C                   CLEAR                   FWSFF3
102700000504     C                   CLEAR                   FWSFF4
102800000504     C                   CLEAR                   FWSFL3
102900000504     C                   CLEAR                   FWSFL4
103000000504     C                   ENDIF
103100000504     C*
103200960318     C                   Z-ADD     1             NRR3              5 0
103300960318     C                   Z-ADD     1             REC3
103400960320     C                   CLEAR                   SAVFLP
103500960320     C                   CLEAR                   TERVOL
103600960320     C                   CLEAR                   V2CVOL
103700960320     C                   CLEAR                   WVOLTA
103800960319     C*
103900960319     C****  PULIZIA SUBFILE  ****
104000960319     C                   SETON                                        21
104100960318     C                   WRITE     TL37C03
104200960319     C                   SETOFF                                       21
104300960318     C*
104400960319     C****  CARICAMENTO SUBFILE  ****
104500960318     C                   Z-ADD     1             I                 3 0
104600000504     C     I             DOWLE     300
104700990825    1C     FFV(I)        ANDNE     *BLANKS
104800960318     C*
104900960318     C* FILIALI DI ARRIVO
105000960320     C*
105100960320     C* SE MODIFICA LA FILIALE DI SCARICO, EMETTO TOTALE MC
105200960320    2C     FLP(I)        IFNE      SAVFLP
105300960320    3C     SAVFLP        IFNE      *BLANKS
105400960320     C*
105500960320     C                   CLEAR                   VSCFFV
105600960320     C                   CLEAR                   VSDFFV
105700960320     C                   CLEAR                   TIPFFV
105800960320     C                   CLEAR                   TIPFLP
105900960320     C                   MOVEL     SAVFLP        VSCFLP
106000960320     C                   MOVEL     CTOT          VSDFLP
106100960320     C                   MOVEL     W008A         VSDFLP
106200960320     C                   Z-ADD     TERVOL        VSCVOL
106300960320     C                   CLEAR                   TERVOL
106400960320     C                   SETON                                        03
106500960320     C*
106600960320     C                   WRITE     TL37S03
106700960320     C                   SETOFF                                       03
106800960320     C                   ADD       1             NRR3
106900960320     C                   MOVEL     '1'           WVOLTA            1
107000960320    3C                   ENDIF
107100960320     C*
107200960320     C                   MOVEL     FLP(I)        SAVFLP            3
107300960320    2C                   ENDIF
107400960320     C*
107500960318     C                   MOVE      FFV(I)        VSCFFV
107600960318     C                   MOVE      VSCFFV        W0030             3 0
107700960318     C     W0030         CHAIN     AZORG01L                           32
107800960320    2C     *IN32         IFEQ      *OFF
107900960318     C                   MOVEL     ORGDES        VSDFFV
108000960318     C                   MOVEL     ORGFAG        TIPFFV
108100960320   X2C                   ELSE
108200960318     C                   CLEAR                   VSDFFV
108300960318     C                   CLEAR                   TIPFFV
108400960320    2C                   ENDIF
108500960318     C*
108600960318     C* FILIALI DI SCARICO
108700960320    2C     FLP(I)        IFGT      *ZEROS
108800960318     C                   MOVE      FLP(I)        VSCFLP
108900960318     C                   MOVE      VSCFLP        W0030
109000960318     C     W0030         CHAIN     AZORG01L                           32
109100960318    3C     *IN32         IFEQ      *OFF
109200960318     C                   MOVEL     ORGDES        VSDFLP
109300960320     C                   MOVEL     ORGDES        W008A             8
109400960318     C                   MOVEL     ORGFAG        TIPFLP
109500960318   X3C                   ELSE
109600960318     C                   CLEAR                   VSDFLP
109700960320     C                   CLEAR                   W008A
109800960318     C                   CLEAR                   TIPFLP
109900960318    3C                   ENDIF
110000960318     C*
110100960318   X2C                   ELSE
110200960318     C                   MOVE      *BLANKS       VSCFLP
110300960318     C                   MOVE      *BLANKS       VSDFLP
110400960318     C                   MOVE      *BLANKS       TIPFLP
110500960318    2C                   ENDIF
110600960318     C*
110700960319     C* METRICUBI REALI (SOLO DELL'AREA)
110800960319     C                   MOVEL     FFV(I)        WLNA
110900960319     C     KFVD          CHAIN     FNFVD01L                           34
111000960319    2C     *IN34         IFEQ      *OFF
111100960319     C                   Z-ADD(H)  FVDVL1        VSCVOL
111200960319     C                   ADD(H)    FVDVL2        VSCVOL
111300960319     C                   ADD(H)    FVDVL3        VSCVOL
111400960319     C                   ADD(H)    FVDVL4        VSCVOL
111500960319     C                   ADD(H)    FVDVL5        VSCVOL
111600960319     C                   ADD(H)    FVDVL6        VSCVOL
111700960319   X2C                   ELSE
111800960319     C                   CLEAR                   VSCVOL
111900960319    2C                   ENDIF
112000960320     C*
112100960320     C* SOMMO AL TOTALE DEL FV
112200960320     C                   ADD(H)    VSCVOL        V2CVOL
112300960320     C* SOMMO AL TOTALE DEL TERMINAL
112400960320     C                   ADD(H)    VSCVOL        TERVOL
112500960319     C*
112600960318     C                   WRITE     TL37S03
112700960318     C                   ADD       1             NRR3
112800960320     C                   ADD       1             I
112900960318    1C                   ENDDO
113000960320     C*
113100960320     C* ULTIMA FILIALE DI SCARICO SE NE HO TROVATA PIU' DI UNA
113200960320     C     SAVFLP        IFNE      *BLANKS
113300960320     C     WVOLTA        ANDEQ     '1'
113400960320     C*
113500960320     C                   CLEAR                   VSCFFV
113600960320     C                   CLEAR                   VSDFFV
113700960320     C                   CLEAR                   TIPFFV
113800960320     C                   CLEAR                   TIPFLP
113900960320     C                   MOVEL     SAVFLP        VSCFLP
114000960320     C                   MOVEL     CTOT          VSDFLP
114100960320     C                   MOVEL     W008A         VSDFLP
114200960320     C                   Z-ADD     TERVOL        VSCVOL
114300960320     C                   CLEAR                   TERVOL
114400960320     C*
114500960320     C                   SETON                                        03
114600960320     C                   WRITE     TL37S03
114700960320     C                   ADD       1             NRR3
114800960320     C                   SETOFF                                       03
114900960320     C                   ENDIF
115000960318     C*
115100960318     C                   ENDSR
115200960319     C*
115300960315     C*--- INTERROGO GLI ORARI DI ARRIVO -----------------------------*
115400960315     C     INTORA        BEGSR
115500960319     C*
115600960319     C* SOLO SE CAMBIA IL FOGLIO VIAGGIO RICARICO IL SUBFILE
115700960319    1C     FVSNFV        IFNE      SAVNFV
115800960315     C                   MOVEL     FVSNFV        SAVNFV
115900960315     C*
116000960315     C                   Z-ADD     0             NRR5              5 0
116100960321     C                   SETOFF                                       17
116200960321     C*
116300960321     C****  PULIZIA SFL  ****
116400960319     C                   SETON                                        24
116500960315     C                   WRITE     TL37C05
116600960319     C                   SETOFF                                       24
116700960321     C*
116800960321     C****  P A R T E N Z A   REALE  *****
116900960321     C                   EXSR      RI2S05
117000960315     C*
117100960321     C****  A R R I V O       REALE  *****
117200960321     C                   MOVEL     'A'           WEPA
117300960321     C     KFV2          SETLL     FNFV203L
117400960321     C     KFV2          READE     FNFV203L                               32
117500960321    2C     *IN32         DOWEQ     *OFF
117600960319     C     FV2ATB        ANDEQ     ' '
117700960319     C                   EXSR      RIES05
117800960319     C*
117900960321     C     KFV2          READE     FNFV203L                               32
118000960321    2C                   ENDDO
118100960319     C*
118200960319    1C                   ENDIF
118300960315     C*
118400960321     C****  EMISSIONE FORMATO  ****
118500960315     C     FOR05         TAG
118600960315     C                   WRITE     TL37T01
118700960315     C                   WRITE     TL37Z05
118800960318     C                   EXFMT     TL37C05
118900960315     C*
119000960315     C* F12 - RITORNO
119100960319    1C     *INKL         IFEQ      *OFF
119200960315     C                   CLEAR                   W001A
119300960321     C*
119400960315     C                   READC     TL37S05                                32
119500960319    2C     *IN32         DOWEQ     *OFF
119600960319    3C     V5CVIS        IFEQ      'V'
119700960315     C                   CLEAR                   V5CVIS
119800960315     C                   SETON                                        17
119900960315     C                   UPDATE    TL37S05
120000960315     C*
120100960315     C                   CLEAR                   DSLV15
120200960321    4C     V5CPAR        IFEQ      'PARTENZA'
120300960321     C                   MOVEL     'P'           D15EPA
120400960321   X4C                   ELSE
120500960315     C                   MOVEL     'A'           D15EPA
120600960321     C                   MOVEL     V5CFIL        D15LAI
120700960321    4C                   ENDIF
120800960321     C*
120900960321     C                   MOVEL     'I'           D15TUP
121000960315     C                   MOVEL     V5CRTC        D15RTC
121100960315     C                   MOVEL     FVSLNP        D15LNP
121200960315     C                   MOVEL     FVSNFV        D15NFV
121300960321     C                   MOVEL     'R'           D15TRC
121400960321     C                   MOVEL     FVSTRN        D15TRN
121500960321     C                   MOVEL     FVSTTR        D15TTR
121600960315     C                   MOVEL     FVSDFV        D15DFV
121700960315     C*
121800960315     C                   MOVEL     DSLV15        KPJBU
121900960315     C                   CALL      'FNLV15R'
122000960315     C                   PARM                    KPJBA
122100960321     C                   MOVEL     '1'           WLV15             1
122200960321     C*
122300960315     C                   MOVEL     '1'           W001A
122400960319    3C                   ENDIF
122500960315     C*
122600960315     C                   READC     TL37S05                                32
122700960319    2C                   ENDDO
122800960315     C*
122900960319    2C     W001A         IFEQ      '1'
123000960315     C                   GOTO      FOR05
123100960319    2C                   ENDIF
123200960319    1C                   ENDIF
123300960315     C*
123400960315     C                   ENDSR
123500960318     C*
123600960321     C*--- CARICAMENTO SFL DEGLI ORARI DI PARTENZA/ARRIVO ------------*
123700960319     C     RIES05        BEGSR
123800960321     C                   SETOFF                                       17
123900960321     C*
124000960321     C****  A R R I V O       REALE  *****
124100960321     C                   CLEAR                   V5DRT1
124200960321     C                   CLEAR                   V5DRT2
124300960321     C* ARRIVO
124400960321     C                   MOVEL     'ARRIVO  '    V5CPAR
124500960319     C* FILIALE DI INVIO
124600960319     C                   MOVEL     FV2LAI        V5CFIL
124700960319     C     FV2LAI        CHAIN     AZORG                              34
124800960319     C  N34              MOVEL     ORGDES        V5DFIL
124900960319     C   34              MOVEL     *BLANKS       V5DFIL
125000960319     C* DATA E ORA REALE
125100960319     C                   Z-ADD     FV2DPA        WDATA
125200960319     C                   Z-ADD     MM            GGMM
125300960319     C                   MOVEL     GG            GGMM
125400960319     C                   Z-ADD     AA            V5CDTR
125500960319     C                   MOVEL     GGMM          V5CDTR
125600960315     C                   MOVEL     FV2HPA        V5CHMR
125700960319     C* CAUSALE RITARDO
125800960319     C                   MOVEL     FV2RTC        V5CRTC
125900960319    1C     FV2RTC        IFNE      *BLANKS
126000960315     C                   MOVEL     'TC'          COD
126100960315     C                   MOVEL(P)  FV2RTC        KEY
126200960318     C     KTAB          CHAIN     TABEL                              34
126300960321    2C     *IN34         IFEQ      *OFF
126400960321     C                   MOVEL     TBLUNI        DSTC
126500960321     C                   MOVEL     §TCDES        V5DRT1
126600960321     C                   MOVE      §TCDES        V5DRT2
126700960321    2C                   ENDIF
126800960319     C*
126900960315     C* VERIFICO SE C'E' ANCHE LA DESCRIZIONE
127000960318     C                   MOVEL     'A'           WEPA
127100960318     C                   Z-ADD     FV2LAI        WLAI
127200960318     C                   MOVEL     'R'           WTRC
127300960315     C                   EXSR      CTRDRA
127400960319    2C     W001A         IFNE      ' '
127500960315     C                   SETON                                        17
127600960319    2C                   ENDIF
127700960319    1C                   ENDIF
127800960319     C*
127900960319     C                   ADD       1             NRR5
128000960319     C                   WRITE     TL37S05
128100960315     C                   ENDSR
128200960318     C*
128300960321     C*--- CARICAMENTO SFL DEGLI ORARI DI PARTENZA/ARRIVO ------------*
128400960321     C     RI2S05        BEGSR
128500960321     C                   SETOFF                                       17
128600960321     C*
128700960321     C****  P A R T E N Z A   REALE  *****
128800960321     C                   CLEAR                   V5DRT1
128900960321     C                   CLEAR                   V5DRT2
129000960321     C* PARTENZA
129100960321     C                   MOVEL     'PARTENZA'    V5CPAR
129200960321     C* FILIALE PARTENZA
129300960321     C                   MOVEL     FVSLNP        V5CFIL
129400960321     C     FVSLNP        CHAIN     AZORG                              34
129500960321     C  N34              MOVEL     ORGDES        V5DFIL
129600960321     C   34              MOVEL     *BLANKS       V5DFIL
129700960321     C* DATA E ORA
129800960321     C                   Z-ADD     FVSDRP        WDATA
129900960321     C                   Z-ADD     MM            GGMM
130000960321     C                   MOVEL     GG            GGMM
130100960321     C                   Z-ADD     AA            V5CDTR
130200960321     C                   MOVEL     GGMM          V5CDTR
130300960321     C                   MOVEL     FVSHRP        V5CHMR
130400960321     C* CAUSALE RITARDO
130500960321     C                   MOVEL     FVSRTP        V5CRTC
130600960321    1C     FVSRTP        IFNE      ' '
130700960321     C                   MOVEL     'TC'          COD
130800960321     C                   MOVEL(P)  FVSRTP        KEY
130900960321     C     KTAB          CHAIN     TABEL                              32
131000960321    2C     *IN32         IFEQ      *OFF
131100960321     C                   MOVEL     TBLUNI        DSTC
131200960321     C                   MOVEL     §TCDES        V5DRT1
131300960321     C                   MOVE      §TCDES        V5DRT2
131400960321    2C                   ENDIF
131500960321     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO: SE ESISTE
131600960321     C*   EVIDENZIO IL COMANDO
131700960321     C                   MOVEL     'P'           WEPA
131800960321     C                   MOVEL     'R'           WTRC
131900960321     C                   CLEAR                   WLAI
132000960321     C                   EXSR      CTRDRA
132100960321    2C     W001A         IFNE      ' '
132200960321     C                   SETON                                        17
132300960321    2C                   ENDIF
132400960321    1C                   ENDIF
132500960321     C*
132600960321     C                   ADD       1             NRR5
132700960321     C                   WRITE     TL37S05
132800960321     C                   ENDSR
132900130214     c*
133000130214     C*--- INTERROGa motivi sbinamento  -----------------------------*
133100130214     C     INTMot        BEGSR
133200130214     C*
133300130214     C                   Z-ADD     0             NRR6              5 0
133400130214     C                   SETOFF                                       17
133500130214     C*
133600130214     C****  PULIZIA SFL  ****
133700130214     C                   SETON                                        24
133800130214     C                   WRITE     TL37C06
133900130214     C                   SETOFF                                       24
134000130214     C*
134100130214     C                   MOVE      'P'           WEPA
134200130214     C                   MOVE      *BLANKS       WLAI
134300130214     C                   MOVE      'C'           WTRC
134400130214     c                   z-add     1             conta             2 0
134500130214     c                   clear                   v6dnot
134600130214     C     KFV4          SETLL     FNFV401L
134700130214     C     KFV4          READE     FNFV401L                               32
134800130214    2C     *IN32         DOWEQ     *OFF
134900130214     C     FV4ATB        ANDEQ     ' '
135000130214     C*
135100130214     c                   if        conta=1
135200130214     c                   eval      savnot=fv4not
135300130214     c                   eval      conta=conta+1
135400130214     c                   else
135500130214     c
135600130214     c                   eval      v6dnot=savnot+fv4not
135700130214     C                   ADD       1             NRR6
135800130214     C                   WRITE     TL37S06
135900130214     c                   clear                   savnot
136000130214     c                   clear                   v6dnot
136100130214     c                   eval      conta=1
136200130214     c                   endif
136300130214     C*
136400130214     C     KFV4          READE     FNFV401L                               32
136500130214    2C                   ENDDO
136600130214     C*
136700130214     c                   if        savnot<>*blanks
136800130214     c                   clear                   v6dnot
136900130214     c                   eval      v6dnot=savnot
137000130214     C                   ADD       1             NRR6
137100130214     C                   WRITE     TL37S06
137200130214     c                   endif
137300130214     C*
137400130214     C****  EMISSIONE FORMATO  ****
137500130214     C     FOR06         TAG
137600130214     C                   WRITE     TL37T01
137700130214     C                   WRITE     TL37Z05
137800130214     C                   EXFMT     TL37C06
137900130214     C*
138000130214     C* F12 - RITORNO
138100130214    1C     *INKL         IFEQ      *OFF
138200130214     C                   GOTO      FOR06
138300130214    1C                   ENDIF
138400130214     C*
138500130214     C                   ENDSR
138600130214     C*
138700960321     C*
138800960226     C*--- SR INIZIALE -----------------------------------------------*
138900951110     C     *INZSR        BEGSR
139000960307     C****
139100960307     C** KLIST
139200960307     C****
139300960315     C* ACCESSO TNFVS01L
139400960315     C     KFVS          KLIST
139500960315     C                   KFLD                    V1CLNP
139600960315     C                   KFLD                    V1CNFV
139700960315     C* ACCESSO TNFVS03L
139800960315     C     KFVS2         KLIST
139900960315     C                   KFLD                    V1CLNP
140000960315     C                   KFLD                    WDFD
140100960315     C     KFVS3         KLIST
140200960320     C                   KFLD                    WFIL
140300960318     C                   KFLD                    WDFD
140400960321     C* ACCESSO FNFV203L
140500960321     C     KFV2          KLIST
140600960318     C                   KFLD                    FVSLNP                                     FOGLIO
140700960318     C                   KFLD                    FVSNFV
140800960318     C                   KFLD                    WEPA
140900960318     C                   KFLD                    WTDH
141000960315     C* ACCESSO FNFV401L
141100960315     C     KFV4          KLIST
141200960315     C                   KFLD                    FVSLNP                                     FOGLIO
141300960315     C                   KFLD                    FVSNFV
141400960315     C                   KFLD                    WEPA
141500960315     C                   KFLD                    WLAI                                       NE
141600960315     C                   KFLD                    WTRC
141700000504     C     KFWS          KLIST
141800000504     C                   KFLD                    FVSLNP                                     FOGLIO
141900000504     C                   KFLD                    FVSNFV
142000960319     C* ACCESSO FNFVD01L
142100960319     C     KFVD          KLIST
142200960319     C                   KFLD                    FVSLNP
142300960319     C                   KFLD                    FVSNFV
142400960319     C                   KFLD                    WLNA
142500960319     C                   KFLD                    WTRC2
142600951110     C* ACCESSO TABEL00F
142700960315     C     KTAB          KLIST
142800951110     C                   KFLD                    CODUT
142900951110     C                   KFLD                    COD
143000960315     C                   KFLD                    KEY
143100951110     C*
143200960228     C****
143300960228     C** DEFINIZIONE CAMPI
143400960228     C****
143500960229     C     *LIKE         DEFINE    TBLCOD        COD
143600960315     C     *LIKE         DEFINE    TBLKEY        KEY
143700960315     C     *LIKE         DEFINE    FVSLNP        WFIL
143800960318     C     *LIKE         DEFINE    FVSNFV        SAVNFV
143900960318     C     *LIKE         DEFINE    D37DFD        WDFD
144000960318     C     *LIKE         DEFINE    D37DFA        WDFA
144100960318     C     *LIKE         DEFINE    FV2EPA        WEPA
144200960318     C     *LIKE         DEFINE    FV2LAI        WLAI
144300960318     C     *LIKE         DEFINE    FV2TDH        WTDH
144400960318     C     *LIKE         DEFINE    FV4TRC        WTRC
144500960319     C     *LIKE         DEFINE    FVDLNA        WLNA
144600960319     C     *LIKE         DEFINE    FVDTRC        WTRC2
144700960320     C     *LIKE         DEFINE    VSCVOL        TERVOL
144800960307     C*
144900960315     C****  IMPOSTAZIONE CAMPI FISSI  ****
145000960315     C                   MOVEL     'TNTL37R'     V1CPGM
145100960315     C                   MOVE      TES(1)        V1CTES
145200960319     C                   MOVEL     '1'           WTRC2
145300960319     C                   MOVEL     'R'           WTDH
145400960315     C*
145500960229     C                   Z-ADD     1             CODUT
145600960229     C                   CALL      'X§PARUT'
145700960229     C                   PARM                    UT§DSE
145800960315     C                   MOVEL     RAGUT         V1CRSU
145900960318     C*
146000960318     C****  PRENDO DATA E ORA CORRENTE  ****
146100960318     C                   TIME                    W0140            14 0
146200960318     C                   MOVE      W0140         UDATE8            8 0
146300960228     C*
146400960229     C                   ENDSR
146500960315     C*--------------------------------------------------------------*
146600960315**  MSG
146700971203Linea di partenza errata                                                      1
146800971203La linea di partenza immessa non e' un terminal di partenza                   2
146900960315Occorre immettere il numero f.viaggio o la data f.viaggio                     3
147000960315Immettere o il numero f.viaggio o la data f.viaggio                           4
147100960315Se immesso il numero foglio viaggio occorre inserire la linea di partenza     5
147200960315Numero foglio viaggio inesistente                                             6
147300960315Data errata                                                                   7
147400960315La data foglio DAL non puo' essere maggiore della data foglio AL              8
147500960315Non esistono dati da visualizzare per la selezione effettuata !!              9
147600960315                                                                              10
147700960315**  TES
147800960315 * INTERROG.FOGLI VIAGGIO PARTENZE *
