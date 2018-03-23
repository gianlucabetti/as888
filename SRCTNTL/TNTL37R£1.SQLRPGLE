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
037800960315     C                   ENDSR
037900960302     C*
038000960315     C*--- CONTROLLI 1° VIDEATA --------------------------------------*
038100960315     C     CTRD01        BEGSR
038200960315     C                   SETOFF                                       90
038300960315     C*
038400960315     C****  LINEA DI PARTENZA  ****
038500960315    1C     V1CLNP        IFNE      0
038600960315     C     V1CLNP        CHAIN     AZORG                              31
038700960315    2C     *IN31         IFEQ      *ON
038800971203     C     ORGFVA        ORNE      ' '
038900971203     C     ORGFAG        ORNE      'A'
039000971203     C     ORGFAG        ANDNE     'F'
039100960315     C                   MOVEL     MSG(1)        V1CMSG
039200960315     C                   SETON                                        412890
039300960315     C                   GOTO      ENDCTR
039400960315    2C                   ENDIF
039500960315    1C                   ENDIF
039600960315     C*
039700960315     C****  OCCORRE IMMETTERE O IL NUMERO FOGLIO O LA DATA  ****
039800960315    1C     V1CNFV        IFEQ      0
039900960315     C     V1CDFD        ANDEQ     0
040000960315     C                   MOVEL     MSG(3)        V1CMSG
040100960315     C                   SETON                                        422890
040200960315     C                   GOTO      ENDCTR
040300960315    1C                   ENDIF
040400960315     C*
040500960315     C****  IMPOSSIBILE INSERIRE ENTRAMBI  ****
040600960315    1C     V1CNFV        IFNE      0
040700960315     C     V1CDFD        ANDNE     0
040800960315     C                   MOVEL     MSG(4)        V1CMSG
040900960315     C                   SETON                                        422890
041000960315     C                   GOTO      ENDCTR
041100960315    1C                   ENDIF
041200960315     C*
041300960315     C****  NUMERO FOGLIO VIAGGIO  ****
041400960315    1C     V1CNFV        IFNE      0
041500960315     C* SE IMMESSO IL FOGLIO VIAGGIO OCCORRE INSERIRE ANCHE LA LNP
041600960315    2C     V1CLNP        IFEQ      0
041700960315     C                   MOVEL     MSG(5)        V1CMSG
041800960315     C                   SETON                                        412890
041900960315     C                   GOTO      ENDCTR
042000960315    2C                   ENDIF
042100960315     C*
042200960315     C* CONTROLLO VALIDITA' NUMERO FOGLIO VIAGGIO
042300960315     C     KFVS          CHAIN     TNFVS01L                           31
042400960315    2C     *IN31         IFEQ      *ON
042500960315     C                   MOVEL     MSG(6)        V1CMSG
042600960315     C                   SETON                                        422890
042700960315     C                   GOTO      ENDCTR
042800960315    2C                   ENDIF
042900960315    1C                   ENDIF
043000960315     C*
043100960315     C****  DATA FOGLIO VIAGGIO  DAL  ****
043200960315    1C     V1CDFD        IFNE      0
043300960315     C                   MOVEL     V1CDFD        G02DAT
043400960315     C                   MOVEL     *ZERO         G02INV
043500960315     C                   MOVEL     *BLANKS       G02ERR
043600960315     C                   CALL      'XSRDA8'
043700960315     C                   PARM                    WLBDAT
043800960315     C     G02ERR        IFEQ      '1'
043900960315     C                   MOVEL     MSG(7)        V1CMSG
044000960315     C                   SETON                                        432890
044100960315     C                   GOTO      ENDCTR
044200960315     C                   ENDIF
044300960315     C                   Z-ADD     G02DAT        V1CDFD
044400960315     C                   Z-ADD     G02INV        WDFD
044500960315    1C                   ENDIF
044600960315     C*
044700960315     C****  DATA FOGLIO VIAGGIO  AL  ****
044800960315    1C     V1CDFA        IFNE      0
044900960315     C                   MOVEL     V1CDFA        G02DAT
045000960315     C                   MOVEL     *ZERO         G02INV
045100960315     C                   MOVEL     *BLANKS       G02ERR
045200960315     C                   CALL      'XSRDA8'
045300960315     C                   PARM                    WLBDAT
045400960315     C     G02ERR        IFEQ      '1'
045500960315     C                   MOVEL     MSG(7)        V1CMSG
045600960315     C                   SETON                                        442890
045700960315     C                   GOTO      ENDCTR
045800960315     C                   ENDIF
045900960315     C                   Z-ADD     G02DAT        V1CDFA
046000960315     C                   Z-ADD     G02INV        WDFA
046100960315     C*
046200960315     C* DATA DAL NO MAGGIORE DATA AL
046300960315    2C     WDFD          IFGT      WDFA
046400960315     C                   MOVEL     MSG(8)        V1CMSG
046500960315     C                   SETON                                        432890
046600960315     C                   GOTO      ENDCTR
046700960315    2C                   ENDIF
046800960315    1C                   ENDIF
046900960315     C*
047000960315     C     ENDCTR        ENDSR
047100960315     C*
047200960315     C*--- CARICAMENTO SFL PER SCELTA F.VIAGGIO ----------------------*
047300960315     C     RIES04        BEGSR
047400960318     C                   Z-ADD     1             NRR4              5 0
047500960318     C*
047600960318     C****  PULIZIA SUBFILE  ****
047700960318     C                   SETON                                        23
047800960318     C                   WRITE     TL37C04
047900960318     C                   SETOFF                                       23
048000960315     C*
048100960318     C****  CARICAMENTO SUBFILE  ****
048200960318     C* 12 ON  - IMMESSA UNA SPECIFICA LINEA PARTENZA
048300960318    1C     *IN12         IFEQ      *ON
048400960315     C*
048500960315     C     KFVS2         SETLL     TNFVS03L
048600960315     C  N09KFVS2         READE     TNFVS03L                               30
048700960315     C   09V1CLNP        READE     TNFVS03L                               30
048800960315     C*
048900960318    2C     *IN30         DOWEQ     *OFF
049000960315     C*
049100960315     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
049200960318    3C   09FVSDFV        IFGT      WDFA
049300960315     C                   SETON                                        30
049400960318    3C                   ENDIF
049500960315     C*
049600960318    3C     *IN30         IFEQ      *OFF
049700960318     C* SCRIVO CAMPI DEL SFL
049800960318     C                   EXSR      WRTS04
049900960315     C*
050000960315     C  N09KFVS2         READE     TNFVS03L                               30
050100960315     C   09V1CLNP        READE     TNFVS03L                               30
050200960318    3C                   ENDIF
050300960318    2C                   ENDDO
050400960315     C*
050500960318   X1C                   ELSE
050600960318     C* 12 OFF - RICHIESTE TUTTE LE LINEE DI PARTENZA
050700960318     C*
050800960315     C                   CLEAR                   WFIL
050900960315     C                   SETOFF                                       30
051000960315     C*
051100960318    2C     *IN30         DOWEQ     *OFF
051200960315     C     WFIL          SETGT     TNFVS03L
051300960315     C                   READ      TNFVS03L                               30
051400960315     C*
051500960318    3C     *IN30         IFEQ      *OFF
051600960315     C                   Z-ADD     FVSLNP        WFIL
051700960315     C*
051800960320     C     KFVS3         SETLL     TNFVS03L
051900960315     C  N09KFVS3         READE     TNFVS03L                               31
052000960315     C   09WFIL          READE     TNFVS03L                               31
052100960315     C*
052200960318    4C     *IN31         DOWEQ     *OFF
052300960315     C*
052400960315     C* SE DATA FOGLIO VIAGGIO > DATA AL ---> LEGGO ALTRA LINEA
052500960318    5C   09FVSDFV        IFGT      WDFA
052600960318     C                   SETON                                        31
052700960318    5C                   ENDIF
052800960315     C*
052900960318    5C     *IN31         IFEQ      *OFF
053000960318     C* SCRIVO CAMPI DEL SFL
053100960318     C                   EXSR      WRTS04
053200960315     C*
053300960315     C  N09KFVS3         READE     TNFVS03L                               31
053400960315     C   09WFIL          READE     TNFVS03L                               31
053500960318    5C                   ENDIF
053600960318    4C                   ENDDO
053700960315     C*
053800960318    3C                   ENDIF
053900960318    2C                   ENDDO
054000960315     C*
054100960318    1C                   ENDIF
054200960315     C*
054300960315     C                   ENDSR
054400960315     C*
054500960318     C*--- SCRIVO CAMPI SFL SELEZIONE F.VIAGGIO ----------------------*
054600960318     C     WRTS04        BEGSR
054700960320     C                   MOVEL     'S'           WCARIC            1
054800960320     C                   CLEAR                   §TVDES
054900960320     C*
055000960320     C* CONTROLLO IL TIPO TRAINO
055100960320    1C     FVSTTR        IFNE      ' '
055200960320     C     V1CSCE        ANDNE     'T'
055300960320     C                   MOVEL     'TV'          COD
055400960320     C                   MOVEL(P)  FVSTTR        KEY
055500960320     C     KTAB          CHAIN     TABEL                              32
055600960320     C*
055700960320    2C     *IN32         IFEQ      *OFF
055800960320     C     TBLFLG        ANDEQ     ' '
055900960320     C                   MOVEL     TBLUNI        DSTV
056000960320     C* VISULIZZO SOLO I NON UTILIZZATI IN SEDE
056100960320    3C     §TVUTC        IFEQ      'S'
056200960320     C     V1CSCE        ANDEQ     'N'
056300960320     C                   MOVEL     'N'           WCARIC
056400960320    3C                   ENDIF
056500960320     C* VISULIZZO SOLO GLI UTILIZZATI IN SEDE
056600960320    3C     §TVUTC        IFNE      'S'
056700960320     C     V1CSCE        ANDEQ     'S'
056800960320     C                   MOVEL     'N'           WCARIC
056900960320    3C                   ENDIF
057000960320     C*
057100960320    2C                   ENDIF
057200960320    1C                   ENDIF
057300960320     C*
057400960320     C* VISULIZZO SOLO I NON UTILIZZATI IN SEDE
057500960320    3C     FVSTTR        IFEQ      ' '
057600960320     C     V1CSCE        ANDEQ     'N'
057700960320     C                   MOVEL     'N'           WCARIC
057800960320    3C                   ENDIF
057900960320     C**
058000960320     C** DA CARICARE
058100960320     C     WCARIC        IFEQ      'S'
058200960315     C* CAMPO SELEZIONE
058300960315     C                   CLEAR                   V4CSCE
058400960315     C* LINEA PARTENZA
058500960315     C                   MOVEL     FVSLNP        V4CLNP
058600960315     C     FVSLNP        CHAIN     AZORG                              32
058700960320     C  N32ORGDE5        IFNE      *BLANKS
058800960320     C                   MOVEL     ORGDE5        V4DLNP
058900960320     C                   ELSE
059000960320     C                   MOVEL     ORGDES        V4DLNP
059100960320     C                   ENDIF
059200960315     C   32              MOVEL     *BLANKS       V4DLNP
059300960315     C* NUMERO FOGLIO VIAGGIO
059400960315     C                   MOVEL     FVSNFV        V4CNFV
059500960315     C* GIRO DATA FOGLIO VIAGGIO
059600960318     C                   Z-ADD     FVSDFV        WDATA
059700960321     C                   Z-ADD     MM            GGMM
059800960318     C                   MOVEL     GG            GGMM
059900960318     C                   Z-ADD     GGMM          V4CDFV
060000960315     C* TIPO TRAINO
060100960315     C                   MOVEL     FVSTTR        V4CTTR
060200960315     C* GIRO DATA PARTENZA REALE
060300960318     C                   Z-ADD     FVSDRP        WDATA
060400960321     C                   Z-ADD     MM            GGMM
060500960318     C                   MOVEL     GG            GGMM
060600960318     C                   Z-ADD     GGMM          V4CDRP
060700960315     C* LINEA FINALE DI ARRIVO
060800960315     C                   MOVEL     FVSLNA        V4CLNA
060900960315     C     FVSLNA        CHAIN     AZORG                              32
061000960320     C  N32ORGDE5        IFNE      *BLANKS
061100960320     C                   MOVEL     ORGDE5        V4DLNA
061200960320     C                   ELSE
061300960320     C                   MOVEL     ORGDES        V4DLNA
061400960320     C                   ENDIF
061500960315     C   32              MOVEL     *BLANKS       V4DLNA
061600960315     C* NUMERO TRAINO
061700960315     C                   MOVEL     FVSTRN        V4CTRN
061800960315     C* SE F.VIAGGIO ANNULLATO LO SEGNALO
061900960315     C                   CLEAR                   V4CANN
062000960315    1C     FVSATB        IFNE      ' '
062100960315     C                   MOVEL     'An'          V4CANN
062200960315    1C                   ENDIF
062300960320     C* TRAINO DI SEDE
062400960320     C                   MOVEL     FVSTRS        V4CTRS
062500960320     C* TRAZIONISTA
062600960320     C                   MOVEL     FVSDPD        V4CDPD
062700960315     C*
062800960315     C                   WRITE     TL37S04
062900960315     C                   ADD       1             NRR4
063000960320     C                   ENDIF
063100960320     C*
063200960315     C                   ENDSR
063300960315     C*
063400960315     C*--- RIEMPO CAMPI 2° VIDEATA -----------------------------------*
063500960315     C     RIED02        BEGSR
063600121105     C                   SETOFF                                       071608
063700960319     C                   SETOFF                                       180405
063800960319     C                   CLEAR                   V2DTFM
063900960319     C                   CLEAR                   V2DTFS
064000960319     C                   CLEAR                   V2DABN
064100960319     C                   CLEAR                   V2DAB2
064200061003     C                   CLEAR                   V2can1
064300061003     C                   CLEAR                   V2can2
064400960315     C*
064500960315     C     KFVS          CHAIN     TNFVS01L                           33
064600010919     C     KFVS          CHAIN     TNFWS01L                           35
064700960319     C*
064800960319    1C     *IN33         IFEQ      *OFF
064900960319     C                   MOVEL     FVSLNA        WLNA2
065000960315     C*
065100960319     C* SE FOGLIO VIAGGIO ANNULLATO ---> LO SEGNALO
065200960319    2C     FVSATB        IFNE      ' '
065300960315     C                   SETON                                        07
065400960319     C     FVSATB        IFEQ      '*'
065500960319     C                   MOVEL     CANF          V2DANN                         *DA FILIALE
065600960319     C                   ENDIF
065700960319     C     FVSATB        IFEQ      'A'
065800960319     C                   MOVEL     CANS          V2DANN                         *DA SEDE
065900960319     C                   ENDIF
066000960319    2C                   ENDIF
066100960315     C*
066200960319     C* NUMERO FOGLIO VIAGGIO
066300960315     C                   MOVEL     FVSNFV        V2CNFV
066400960315     C* GIRO DATA FOGLIO VIAGGIO
066500960315     C                   Z-ADD     FVSDFV        WDATA
066600960321     C                   Z-ADD     MM            GGMM
066700960315     C                   MOVEL     GG            GGMM
066800960315     C                   Z-ADD     AA            V2CDFV
066900960315     C                   MOVEL     GGMM          V2CDFV
067000960315     C*
067100960319     C* DATI RELATIVI AL TRAINO DI FILIALE
067200960315     C                   MOVEL     FVSTRN        V2CTRN
067300960319     C                   MOVEL     FVSTFM        V2CTFM
067400960319    2C     FVSTFM        IFEQ      'A'
067500960319     C                   MOVEL     'ANDATA '     V2DTFM
067600960319    2C                   ENDIF
067700960319    2C     FVSTFM        IFEQ      'R'
067800960319     C                   MOVEL     'RITORNO'     V2DTFM
067900960319    2C                   ENDIF
068000960319     C*
068100960319     C* DATI RELATIVI AL TRAINO DI SEDE
068200960315     C                   MOVEL     FVSTRS        V2CTRS
068300960319     C                   MOVEL     FVSTFS        V2CTFS
068400960319    2C     FVSTFS        IFEQ      'A'
068500960319     C                   MOVEL     'ANDATA '     V2DTFS
068600960319    2C                   ENDIF
068700960319    2C     FVSTFS        IFEQ      'R'
068800960319     C                   MOVEL     'RITORNO'     V2DTFS
068900960319    2C                   ENDIF
069000960315     C*
069100960319     C* SE TRAINO DI SEDE E' ECCEZIONALE LO SEGNALO
069200960319    2C     FVSTRS        IFGE      9000000
069300960319     C     FVSTRS        ANDLT     9999998
069400960319     C                   SETON                                        18
069500960319    2C                   ENDIF
069600960319     C*
069700960319     C* ABBINATO MANUALMENTE
069800960319    2C     FVSTRS        IFGT      0
069900960319     C     FVSTRS        ANDNE     FVSTRN
070000960319     C     FVSTRS        ANDLT     9999998
070100960319     C     FVSTRS        ORGE      9000000
070200960319     C     FVSTRS        ANDLT     9999998
070300960319     C                   SETON                                        04
070400960319     C                   MOVEL     CMAN          V2DABN
070500960319    2C                   ENDIF
070600960319     C* ABBINATO IN AUTOMATICO
070700960319    2C     FVSTRS        IFGT      0
070800960319     C     FVSTRS        ANDLT     9000000
070900960319     C     FVSTRS        ANDEQ     FVSTRN
071000960319     C                   SETON                                        04
071100960319     C                   MOVEL     CAUT          V2DABN
071200960319    2C                   ENDIF
071300960319     C* NON ABBINATO
071400960319    2C     FVSTRS        IFEQ      0
071500960319     C     FVSABN        ANDEQ     'S'
071600960319     C                   SETON                                        05
071700960319     C                   MOVEL     CNAB          V2DAB2
071800960319    2C                   ENDIF
071900960319     C* NON CONTROLLATO
072000960319    2C     FVSABN        IFEQ      ' '
072100960319     C                   SETON                                        05
072200960319     C                   MOVEL     CNCT          V2DAB2
072300960319    2C                   ENDIF
072400960319     C* GIA' FATTURATO
072500960319    2C     FVSTRS        IFEQ      9999998
072600960319     C                   SETON                                        05
072700960319     C                   MOVEL     CFAT          V2DAB2
072800960319    2C                   ENDIF
072900960320     C* NIN UTILIZZATO NEI CONTROLLI DI SEDE
073000960320     C     FVSTRS        IFEQ      9999999
073100960320     C     FVSATB        ANDEQ     ' '
073200960320     C                   SETON                                        04
073300960320     C                   MOVEL     CNUT          V2DABN
073400960320    2C                   ENDIF
073500960319     C*
073600960319     C* LINEA PARTENZA
073700960315     C                   MOVEL     FVSLNP        V2CLNP
073800960320     C                   MOVEL     FVSLNP        V2CLN2
073900960315     C     FVSLNP        CHAIN     AZORG                              32
074000960315     C  N32              MOVEL     ORGDES        V2DLNP
074100960315     C   32              MOVEL     *BLANKS       V2DLNP
074200960319     C* LINEA ARRIVO
074300960315     C                   MOVEL     FVSLNA        V2CLNA
074400960315     C     FVSLNA        CHAIN     AZORG                              32
074500960315     C  N32              MOVEL     ORGDES        V2DLNA
074600960315     C   32              MOVEL     *BLANKS       V2DLNA
074700960315     C*
074800960319     C* TIPO TRAINO
074900960315     C                   MOVEL     FVSTTR        V2CTTR
075000960315     C                   MOVEL     'TV'          COD
075100960315     C                   MOVEL(P)  FVSTTR        KEY
075200960315     C     KTAB          CHAIN     TABEL                              32
075300960315     C  N32              MOVEL     TBLUNI        V2DTTR
075400960315     C   32              CLEAR                   V2DTTR
075500960319     C* CATEGORIA TRAINO (BIS/ECCEZIONALE)
075600960315     C                   MOVEL     FVSBIS        V2CBIS
075700960319     C* TIPO AUTOMEZZO
075800960315     C                   MOVEL     FVSTMZ        V2CTMZ
075900960315     C                   MOVEL     'TU'          COD
076000960315     C                   MOVEL(P)  FVSTMZ        KEY
076100960315     C     KTAB          CHAIN     TABEL                              32
076200960315     C  N32              MOVEL     TBLUNI        V2DTMZ
076300960315     C   32              CLEAR                   V2DTMZ
076400960319     C* TRAZIONISTA
076500960315     C                   MOVEL     FVSPDR        V2CPDR
076600960315     C                   MOVEL     FVSDPD        V2CDPD
076700960319     C* CONDUCENTE
076800960315     C                   MOVEL     FVSDCN        V2CDCN
076900010622     C* CELLULARE CONDUCENTE
077000010702     C                   MOVE      'P'           WEPA
077100010702     C                   MOVE      *BLANKS       WLAI
077200010622     C                   MOVE      'L'           WTRC
077300010702     C     KFV4          CHAIN     FNFV401L                           30
077400010622     C     *IN30         IFEQ      *ON
077500010622     C                   MOVE      *ON           *IN19
077600010622     C                   ELSE
077700010622     C                   MOVEL     FV4NOT        V2CCNC
077800010622     C                   ENDIF
077900960319     C* TARGHE
078000960315     C                   MOVEL     FVSTRM        V2CTRM
078100960315     C                   MOVEL     FVSTRR        V2CTRR
078200960319     C* IMPORTI
078300010919     C**!!!                MOVELFVSIPT    V2CIPT
078400010919     C**!!!                MOVELFVSIES    V2CIES
078500010919     C                   CLEAR                   V2CIPT
078600010919     C                   CLEAR                   V2CIES
078700010919     C                   CLEAR                   V2CVIP
078800010919     C                   CLEAR                   V2CVES
078900010919      * Se il file dell'estensione non esiste o i campi sono a zero
079000010919      * devo prendere quello che c'è nel file principale
079100010919     C     *IN35         IFEQ      *ON
079200010919     C     FVSIPT        IFGT      *ZEROS
079300010919     C                   Z-ADD     FVSIPT        V2CIPT
079400010919     C                   MOVEL     'ITL'         V2CVIP
079500010919     C                   ENDIF
079600010919     C     FVSIES        IFGT      *ZEROS
079700010919     C                   Z-ADD     FVSIES        V2CIES
079800010919     C                   MOVEL     'ITL'         V2CVES
079900010919     C                   ENDIF
080000010919      * Se il file dell'estensione esiste e i campi sono a zero
080100010919      * devo prendere quello che c'è nel file principale
080200010919     C                   ELSE
080300010919     C     FWSIPT        IFEQ      *ZEROS
080400010919     C     FVSIPT        ANDGT     *ZEROS
080500010919     C                   Z-ADD     FVSIPT        V2CIPT
080600010919     C                   MOVEL     'ITL'         V2CVIP
080700010919     C                   ENDIF
080800010919     C     FWSIES        IFEQ      *ZEROS
080900010919     C     FVSIES        ANDGT     *ZEROS
081000010919     C                   Z-ADD     FVSIES        V2CIES
081100010919     C                   MOVEL     'ITL'         V2CVES
081200010919     C                   ENDIF
081300010919      * Se il file dell'estensione esiste e i campi sono > zero
081400010919      * devo prendere quello che c'è nel file dell'estensione
081500010919     C     FWSIPT        IFGT      *ZEROS
081600010919     C                   Z-ADD     FWSIPT        V2CIPT
081700010919     C                   MOVEL     FWSVIP        V2CVIP
081800010919     C                   ENDIF
081900010919     C     FWSIES        IFGT      *ZEROS
082000010919     C                   Z-ADD     FWSIES        V2CIES
082100010919     C                   MOVEL     FWSVES        V2CVES
082200010919     C                   ENDIF
082300010919     C                   ENDIF
082400960319     C* PIOMBI
082500960315     C                   MOVEL     FVSPMB        V2CPMB
082600960319     C* NOTE
082700960315     C                   MOVEL     FVSNT1        V2CNT1
082800960315     C                   MOVEL     FVSNT2        V2CNT2
082900061003     c* Annotazioni
083000061003     C                   MOVE      'P'           WEPA
083100061003     C                   MOVE      *BLANKS       WLAI
083200061003     C                   MOVE      'N'           WTRC
083300061003     C     KFV4          CHAIN     FNFV401L                           30
083400061003     c
083500061003     C                   if        not *in30 and fv4atb=' '
083600061003     C                   MOVEL     FV4NOT        V2Can1
083700061003     c     KFV4          reade     FNFV401L                               30
083800061003     C                   if        not *in30 and fv4atb=' '
083900061003     C                   MOVEL     FV4NOT        V2Can2
084000061003     c                   endif
084100061003     c                   endif
084200130214
084300130214     c* Verifico se presente motivo di sbinammento
084400130214     C                   MOVE      'P'           WEPA
084500130214     C                   MOVE      *BLANKS       WLAI
084600130214     C                   MOVE      'C'           WTRC
084700130214     C     KFV4          setll     FNFV401L                               02
084800121105     c*
084900121105     c* Carico punto di scambio
085000121105     c                   clear                   v2cpsc
085100121105     c                   clear                   v2dpsc
085200121105     c                   clear                   v2ctrns
085300121105     c                   clear                   v2dtrns
085400121105     c                   clear                   v2chhs
085500121105     c                   clear                   v2cdgs
085600121105     c                   clear                   v2clnar
085700121105     c                   clear                   v2dlnar2
085800121105     C     kfvs          chain     tNFGs000
085900121105if  1c                   if        (not %found(tNFGs01L)
086000121105     c                              or  FGsatb <> *blanks)
086100121105     C                   clear                   fgspsc
086200121105     c                   setoff                                       08
086300121105e   1C                   endif
086400121105     c
086500121105    1c                   if        fgspsc<>*blanks
086600121105     c                   seton                                        08
086700121105     c                   eval      v2cpsc=fgspsc
086800121105     c     v2cpsc        chain     tnpsc01l
086900121105    2c                   if        %found(tnpsc01l)
087000121105     c                   eval      v2dpsc=pscloc
087100121105     c***                eval      despind=pscind
087200121105     c***                eval      desploc=%trim(psccap) + ' ' +
087300121105     c***                          %trim(pscloc)+'  ' +pscprv+pscnar
087400121105     c
087500121105   x2c                   else
087600121105     c                   eval      v2dpsc=*all'?'
087700121105     c***                eval      despind=*all'?'
087800121105    2c                   endif
087900121105     c
088000121105    2c                   if        fgstrns>0
088100121105     c                   eval      v2ctrns=fgstrns
088200121105     c                   exsr      Ctrtrns
088300121105    2c                   endif
088400121105
088500121105     c                   eval      v2chhs=fgshhs
088600121105     C                   MOVE      FGsdgs        G02INV
088700121105     C                   MOVE      *ZEROS        G02DAT
088800121105     C                   MOVEL     '3'           G02ERR
088900121105     C                   CALL      'XSRDA8'
089000121105     C                   PARM                    WLBDAT
089100121105     C                   Z-ADD     G02DAT        v2cdgs
089200121105     c
089300121105     c* linea finale arrivo autista
089400121105    2c                   if        fgslnar>0
089500121105     c                   move      fgslnar       v2clnar
089600121105     C     fgslnar       CHAIN     AZORG                              32
089700121105     c***                movel     orgdes        deslnar
089800121105     c  n32              movel     orgdes        v2dlnar2
089900121105     c   32              eval      v2dlnar2=*all'?'
090000121105    2c                   endif
090100121105    1c                   endif
090200121105      *
090300061003     c
090400960315     C*
090500960318     C* CARICO LE LINEE DI ARRIVO E LE FILIALI DI SCARICO
090600960319     C                   EXSR      RIES03
090700960319    1C                   ENDIF
090800960315     C*
090900960315     C                   ENDSR
091000121105     C*-----------------------------------------------------***********
091100121105     C* CONTROLLO Traino da cui copiare-uso sql per non sporcarmi i campi
091200121105     C*-----------------------------------------------------***********
091300121105     C     CTRtrns       BEGSR
091400121105     c                   clear                   woktrns           1
091500121105     c                   eval      wrkgetlista =
091600121105     c                             'select tltdde, tltdsc, tlttfp, tlttfa +
091700121105     c                               from tntlt00f where tltatb= '' '' +
091800121105     c                               and tltdde<=' + %editc(fvsdfv:'X') +
091900121105     c                               ' and tlttrn= ' + %editc(v2ctrns:'X')
092000121105
092100121105     C/EXEC SQL
092200121105     C+ PREPARE s1 FROM :wrkgetlista
092300121105     C/END-EXEC
092400121105     C
092500121105     C/EXEC SQL
092600121105     C+ DECLARE A1 CURSOR FOR S1
092700121105     C/END-EXEC
092800121105     C
092900121105     C/EXEC SQL
093000121105     C+ OPEN a1
093100121105     C/END-EXEC
093200121105
093300121105      *?Leggo il file
093400121105     c                   do        *hival
093500121105     C/EXEC SQL
093600121105     C+ FETCH NEXT FROM a1 INTO : Cdde, :Cdsc, :Ctfp, :Ctfa
093700121105     C/END-EXEC
093800121105
093900121105     c                   select
094000121105
094100121105     c                   when      sqlcod = 100
094200121105     c                   leave
094300121105     c                   when      sqlcod < 0
094400121105     c                   seton                                        H1
094500121105     c                   leave
094600121105     c                   other
094700121105     c
094800121105     c                   if        Cdsc>=fvsdfv  and Cdde<=fvsdfv
094900121105     c                   eval      woktrns='S'
095000121105     c* Decodico  partenza e arrivo
095100121105     c                   eval      v2dtrns='Da '+%editc(Ctfp:'X') + ' a '+
095200121105     c                             %editc(Ctfa:'X')
095300121105     c                   endif
095400121105     c
095500121105     c                   endsl
095600121105
095700121105     c                   enddo
095800121105
095900121105     C/EXEC SQL
096000121105     C+ CLOSE a1
096100121105     C/END-EXEC
096200121105
096300121105     c                   ENDSR
096400960315     C*
096500960315     C*--- CONTROLLO SE ESISTE DESCRIZIONE RITARDO -------------------*
096600960315     C     CTRDRA        BEGSR
096700960315     C                   CLEAR                   W001A
096800960315     C*
096900960315     C     KFV4          SETLL     FNFV401L                               32
097000960315    1C     *IN32         IFEQ      *ON
097100960315     C     KFV4          READE     FNFV401L                               32
097200960315    2C     *IN32         DOWEQ     *OFF
097300960315    3C     FV4ATB        IFEQ      ' '
097400960318     C                   MOVEL     '1'           W001A             1
097500960315     C                   SETON                                        32
097600960315   X3C                   ELSE
097700960315     C     KFV4          READE     FNFV401L                               32
097800960315    3C                   ENDIF
097900960315    2C                   ENDDO
098000960315    1C                   ENDIF
098100960315     C*
098200960315     C                   ENDSR
098300960315     C*
098400960318     C*--- RIEMPIO SFL DELLE LINEE -----------------------------------*
098500960319     C     RIES03        BEGSR
098600000504     C*
098700000504     C     KFWS          CHAIN     TNFWS01L                           39
098800000504     C     *IN39         IFEQ      *ON
098900000504     C     FWSATB        ORNE      ' '
099000000504     C                   CLEAR                   FWSFF3
099100000504     C                   CLEAR                   FWSFF4
099200000504     C                   CLEAR                   FWSFL3
099300000504     C                   CLEAR                   FWSFL4
099400000504     C                   ENDIF
099500000504     C*
099600960318     C                   Z-ADD     1             NRR3              5 0
099700960318     C                   Z-ADD     1             REC3
099800960320     C                   CLEAR                   SAVFLP
099900960320     C                   CLEAR                   TERVOL
100000960320     C                   CLEAR                   V2CVOL
100100960320     C                   CLEAR                   WVOLTA
100200960319     C*
100300960319     C****  PULIZIA SUBFILE  ****
100400960319     C                   SETON                                        21
100500960318     C                   WRITE     TL37C03
100600960319     C                   SETOFF                                       21
100700960318     C*
100800960319     C****  CARICAMENTO SUBFILE  ****
100900960318     C                   Z-ADD     1             I                 3 0
101000000504     C     I             DOWLE     300
101100990825    1C     FFV(I)        ANDNE     *BLANKS
101200960318     C*
101300960318     C* FILIALI DI ARRIVO
101400960320     C*
101500960320     C* SE MODIFICA LA FILIALE DI SCARICO, EMETTO TOTALE MC
101600960320    2C     FLP(I)        IFNE      SAVFLP
101700960320    3C     SAVFLP        IFNE      *BLANKS
101800960320     C*
101900960320     C                   CLEAR                   VSCFFV
102000960320     C                   CLEAR                   VSDFFV
102100960320     C                   CLEAR                   TIPFFV
102200960320     C                   CLEAR                   TIPFLP
102300960320     C                   MOVEL     SAVFLP        VSCFLP
102400960320     C                   MOVEL     CTOT          VSDFLP
102500960320     C                   MOVEL     W008A         VSDFLP
102600960320     C                   Z-ADD     TERVOL        VSCVOL
102700960320     C                   CLEAR                   TERVOL
102800960320     C                   SETON                                        03
102900960320     C*
103000960320     C                   WRITE     TL37S03
103100960320     C                   SETOFF                                       03
103200960320     C                   ADD       1             NRR3
103300960320     C                   MOVEL     '1'           WVOLTA            1
103400960320    3C                   ENDIF
103500960320     C*
103600960320     C                   MOVEL     FLP(I)        SAVFLP            3
103700960320    2C                   ENDIF
103800960320     C*
103900960318     C                   MOVE      FFV(I)        VSCFFV
104000960318     C                   MOVE      VSCFFV        W0030             3 0
104100960318     C     W0030         CHAIN     AZORG01L                           32
104200960320    2C     *IN32         IFEQ      *OFF
104300960318     C                   MOVEL     ORGDES        VSDFFV
104400960318     C                   MOVEL     ORGFAG        TIPFFV
104500960320   X2C                   ELSE
104600960318     C                   CLEAR                   VSDFFV
104700960318     C                   CLEAR                   TIPFFV
104800960320    2C                   ENDIF
104900960318     C*
105000960318     C* FILIALI DI SCARICO
105100960320    2C     FLP(I)        IFGT      *ZEROS
105200960318     C                   MOVE      FLP(I)        VSCFLP
105300960318     C                   MOVE      VSCFLP        W0030
105400960318     C     W0030         CHAIN     AZORG01L                           32
105500960318    3C     *IN32         IFEQ      *OFF
105600960318     C                   MOVEL     ORGDES        VSDFLP
105700960320     C                   MOVEL     ORGDES        W008A             8
105800960318     C                   MOVEL     ORGFAG        TIPFLP
105900960318   X3C                   ELSE
106000960318     C                   CLEAR                   VSDFLP
106100960320     C                   CLEAR                   W008A
106200960318     C                   CLEAR                   TIPFLP
106300960318    3C                   ENDIF
106400960318     C*
106500960318   X2C                   ELSE
106600960318     C                   MOVE      *BLANKS       VSCFLP
106700960318     C                   MOVE      *BLANKS       VSDFLP
106800960318     C                   MOVE      *BLANKS       TIPFLP
106900960318    2C                   ENDIF
107000960318     C*
107100960319     C* METRICUBI REALI (SOLO DELL'AREA)
107200960319     C                   MOVEL     FFV(I)        WLNA
107300960319     C     KFVD          CHAIN     FNFVD01L                           34
107400960319    2C     *IN34         IFEQ      *OFF
107500960319     C                   Z-ADD(H)  FVDVL1        VSCVOL
107600960319     C                   ADD(H)    FVDVL2        VSCVOL
107700960319     C                   ADD(H)    FVDVL3        VSCVOL
107800960319     C                   ADD(H)    FVDVL4        VSCVOL
107900960319     C                   ADD(H)    FVDVL5        VSCVOL
108000960319     C                   ADD(H)    FVDVL6        VSCVOL
108100960319   X2C                   ELSE
108200960319     C                   CLEAR                   VSCVOL
108300960319    2C                   ENDIF
108400960320     C*
108500960320     C* SOMMO AL TOTALE DEL FV
108600960320     C                   ADD(H)    VSCVOL        V2CVOL
108700960320     C* SOMMO AL TOTALE DEL TERMINAL
108800960320     C                   ADD(H)    VSCVOL        TERVOL
108900960319     C*
109000960318     C                   WRITE     TL37S03
109100960318     C                   ADD       1             NRR3
109200960320     C                   ADD       1             I
109300960318    1C                   ENDDO
109400960320     C*
109500960320     C* ULTIMA FILIALE DI SCARICO SE NE HO TROVATA PIU' DI UNA
109600960320     C     SAVFLP        IFNE      *BLANKS
109700960320     C     WVOLTA        ANDEQ     '1'
109800960320     C*
109900960320     C                   CLEAR                   VSCFFV
110000960320     C                   CLEAR                   VSDFFV
110100960320     C                   CLEAR                   TIPFFV
110200960320     C                   CLEAR                   TIPFLP
110300960320     C                   MOVEL     SAVFLP        VSCFLP
110400960320     C                   MOVEL     CTOT          VSDFLP
110500960320     C                   MOVEL     W008A         VSDFLP
110600960320     C                   Z-ADD     TERVOL        VSCVOL
110700960320     C                   CLEAR                   TERVOL
110800960320     C*
110900960320     C                   SETON                                        03
111000960320     C                   WRITE     TL37S03
111100960320     C                   ADD       1             NRR3
111200960320     C                   SETOFF                                       03
111300960320     C                   ENDIF
111400960318     C*
111500960318     C                   ENDSR
111600960319     C*
111700960315     C*--- INTERROGO GLI ORARI DI ARRIVO -----------------------------*
111800960315     C     INTORA        BEGSR
111900960319     C*
112000960319     C* SOLO SE CAMBIA IL FOGLIO VIAGGIO RICARICO IL SUBFILE
112100960319    1C     FVSNFV        IFNE      SAVNFV
112200960315     C                   MOVEL     FVSNFV        SAVNFV
112300960315     C*
112400960315     C                   Z-ADD     0             NRR5              5 0
112500960321     C                   SETOFF                                       17
112600960321     C*
112700960321     C****  PULIZIA SFL  ****
112800960319     C                   SETON                                        24
112900960315     C                   WRITE     TL37C05
113000960319     C                   SETOFF                                       24
113100960321     C*
113200960321     C****  P A R T E N Z A   REALE  *****
113300960321     C                   EXSR      RI2S05
113400960315     C*
113500960321     C****  A R R I V O       REALE  *****
113600960321     C                   MOVEL     'A'           WEPA
113700960321     C     KFV2          SETLL     FNFV203L
113800960321     C     KFV2          READE     FNFV203L                               32
113900960321    2C     *IN32         DOWEQ     *OFF
114000960319     C     FV2ATB        ANDEQ     ' '
114100960319     C                   EXSR      RIES05
114200960319     C*
114300960321     C     KFV2          READE     FNFV203L                               32
114400960321    2C                   ENDDO
114500960319     C*
114600960319    1C                   ENDIF
114700960315     C*
114800960321     C****  EMISSIONE FORMATO  ****
114900960315     C     FOR05         TAG
115000960315     C                   WRITE     TL37T01
115100960315     C                   WRITE     TL37Z05
115200960318     C                   EXFMT     TL37C05
115300960315     C*
115400960315     C* F12 - RITORNO
115500960319    1C     *INKL         IFEQ      *OFF
115600960315     C                   CLEAR                   W001A
115700960321     C*
115800960315     C                   READC     TL37S05                                32
115900960319    2C     *IN32         DOWEQ     *OFF
116000960319    3C     V5CVIS        IFEQ      'V'
116100960315     C                   CLEAR                   V5CVIS
116200960315     C                   SETON                                        17
116300960315     C                   UPDATE    TL37S05
116400960315     C*
116500960315     C                   CLEAR                   DSLV15
116600960321    4C     V5CPAR        IFEQ      'PARTENZA'
116700960321     C                   MOVEL     'P'           D15EPA
116800960321   X4C                   ELSE
116900960315     C                   MOVEL     'A'           D15EPA
117000960321     C                   MOVEL     V5CFIL        D15LAI
117100960321    4C                   ENDIF
117200960321     C*
117300960321     C                   MOVEL     'I'           D15TUP
117400960315     C                   MOVEL     V5CRTC        D15RTC
117500960315     C                   MOVEL     FVSLNP        D15LNP
117600960315     C                   MOVEL     FVSNFV        D15NFV
117700960321     C                   MOVEL     'R'           D15TRC
117800960321     C                   MOVEL     FVSTRN        D15TRN
117900960321     C                   MOVEL     FVSTTR        D15TTR
118000960315     C                   MOVEL     FVSDFV        D15DFV
118100960315     C*
118200960315     C                   MOVEL     DSLV15        KPJBU
118300960315     C                   CALL      'FNLV15R'
118400960315     C                   PARM                    KPJBA
118500960321     C                   MOVEL     '1'           WLV15             1
118600960321     C*
118700960315     C                   MOVEL     '1'           W001A
118800960319    3C                   ENDIF
118900960315     C*
119000960315     C                   READC     TL37S05                                32
119100960319    2C                   ENDDO
119200960315     C*
119300960319    2C     W001A         IFEQ      '1'
119400960315     C                   GOTO      FOR05
119500960319    2C                   ENDIF
119600960319    1C                   ENDIF
119700960315     C*
119800960315     C                   ENDSR
119900960318     C*
120000960321     C*--- CARICAMENTO SFL DEGLI ORARI DI PARTENZA/ARRIVO ------------*
120100960319     C     RIES05        BEGSR
120200960321     C                   SETOFF                                       17
120300960321     C*
120400960321     C****  A R R I V O       REALE  *****
120500960321     C                   CLEAR                   V5DRT1
120600960321     C                   CLEAR                   V5DRT2
120700960321     C* ARRIVO
120800960321     C                   MOVEL     'ARRIVO  '    V5CPAR
120900960319     C* FILIALE DI INVIO
121000960319     C                   MOVEL     FV2LAI        V5CFIL
121100960319     C     FV2LAI        CHAIN     AZORG                              34
121200960319     C  N34              MOVEL     ORGDES        V5DFIL
121300960319     C   34              MOVEL     *BLANKS       V5DFIL
121400960319     C* DATA E ORA REALE
121500960319     C                   Z-ADD     FV2DPA        WDATA
121600960319     C                   Z-ADD     MM            GGMM
121700960319     C                   MOVEL     GG            GGMM
121800960319     C                   Z-ADD     AA            V5CDTR
121900960319     C                   MOVEL     GGMM          V5CDTR
122000960315     C                   MOVEL     FV2HPA        V5CHMR
122100960319     C* CAUSALE RITARDO
122200960319     C                   MOVEL     FV2RTC        V5CRTC
122300960319    1C     FV2RTC        IFNE      *BLANKS
122400960315     C                   MOVEL     'TC'          COD
122500960315     C                   MOVEL(P)  FV2RTC        KEY
122600960318     C     KTAB          CHAIN     TABEL                              34
122700960321    2C     *IN34         IFEQ      *OFF
122800960321     C                   MOVEL     TBLUNI        DSTC
122900960321     C                   MOVEL     §TCDES        V5DRT1
123000960321     C                   MOVE      §TCDES        V5DRT2
123100960321    2C                   ENDIF
123200960319     C*
123300960315     C* VERIFICO SE C'E' ANCHE LA DESCRIZIONE
123400960318     C                   MOVEL     'A'           WEPA
123500960318     C                   Z-ADD     FV2LAI        WLAI
123600960318     C                   MOVEL     'R'           WTRC
123700960315     C                   EXSR      CTRDRA
123800960319    2C     W001A         IFNE      ' '
123900960315     C                   SETON                                        17
124000960319    2C                   ENDIF
124100960319    1C                   ENDIF
124200960319     C*
124300960319     C                   ADD       1             NRR5
124400960319     C                   WRITE     TL37S05
124500960315     C                   ENDSR
124600960318     C*
124700960321     C*--- CARICAMENTO SFL DEGLI ORARI DI PARTENZA/ARRIVO ------------*
124800960321     C     RI2S05        BEGSR
124900960321     C                   SETOFF                                       17
125000960321     C*
125100960321     C****  P A R T E N Z A   REALE  *****
125200960321     C                   CLEAR                   V5DRT1
125300960321     C                   CLEAR                   V5DRT2
125400960321     C* PARTENZA
125500960321     C                   MOVEL     'PARTENZA'    V5CPAR
125600960321     C* FILIALE PARTENZA
125700960321     C                   MOVEL     FVSLNP        V5CFIL
125800960321     C     FVSLNP        CHAIN     AZORG                              34
125900960321     C  N34              MOVEL     ORGDES        V5DFIL
126000960321     C   34              MOVEL     *BLANKS       V5DFIL
126100960321     C* DATA E ORA
126200960321     C                   Z-ADD     FVSDRP        WDATA
126300960321     C                   Z-ADD     MM            GGMM
126400960321     C                   MOVEL     GG            GGMM
126500960321     C                   Z-ADD     AA            V5CDTR
126600960321     C                   MOVEL     GGMM          V5CDTR
126700960321     C                   MOVEL     FVSHRP        V5CHMR
126800960321     C* CAUSALE RITARDO
126900960321     C                   MOVEL     FVSRTP        V5CRTC
127000960321    1C     FVSRTP        IFNE      ' '
127100960321     C                   MOVEL     'TC'          COD
127200960321     C                   MOVEL(P)  FVSRTP        KEY
127300960321     C     KTAB          CHAIN     TABEL                              32
127400960321    2C     *IN32         IFEQ      *OFF
127500960321     C                   MOVEL     TBLUNI        DSTC
127600960321     C                   MOVEL     §TCDES        V5DRT1
127700960321     C                   MOVE      §TCDES        V5DRT2
127800960321    2C                   ENDIF
127900960321     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO: SE ESISTE
128000960321     C*   EVIDENZIO IL COMANDO
128100960321     C                   MOVEL     'P'           WEPA
128200960321     C                   MOVEL     'R'           WTRC
128300960321     C                   CLEAR                   WLAI
128400960321     C                   EXSR      CTRDRA
128500960321    2C     W001A         IFNE      ' '
128600960321     C                   SETON                                        17
128700960321    2C                   ENDIF
128800960321    1C                   ENDIF
128900960321     C*
129000960321     C                   ADD       1             NRR5
129100960321     C                   WRITE     TL37S05
129200960321     C                   ENDSR
129300130214     c*
129400130214     C*--- INTERROGa motivi sbinamento  -----------------------------*
129500130214     C     INTMot        BEGSR
129600130214     C*
129700130214     C                   Z-ADD     0             NRR6              5 0
129800130214     C                   SETOFF                                       17
129900130214     C*
130000130214     C****  PULIZIA SFL  ****
130100130214     C                   SETON                                        24
130200130214     C                   WRITE     TL37C06
130300130214     C                   SETOFF                                       24
130400130214     C*
130500130214     C                   MOVE      'P'           WEPA
130600130214     C                   MOVE      *BLANKS       WLAI
130700130214     C                   MOVE      'C'           WTRC
130800130214     c                   z-add     1             conta             2 0
130900130214     c                   clear                   v6dnot
131000130214     C     KFV4          SETLL     FNFV401L
131100130214     C     KFV4          READE     FNFV401L                               32
131200130214    2C     *IN32         DOWEQ     *OFF
131300130214     C     FV4ATB        ANDEQ     ' '
131400130214     C*
131500130214     c                   if        conta=1
131600130214     c                   eval      savnot=fv4not
131700130214     c                   eval      conta=conta+1
131800130214     c                   else
131900130214     c
132000130214     c                   eval      v6dnot=savnot+fv4not
132100130214     C                   ADD       1             NRR6
132200130214     C                   WRITE     TL37S06
132300130214     c                   clear                   savnot
132400130214     c                   clear                   v6dnot
132500130214     c                   eval      conta=1
132600130214     c                   endif
132700130214     C*
132800130214     C     KFV4          READE     FNFV401L                               32
132900130214    2C                   ENDDO
133000130214     C*
133100130214     c                   if        savnot<>*blanks
133200130214     c                   clear                   v6dnot
133300130214     c                   eval      v6dnot=savnot
133400130214     C                   ADD       1             NRR6
133500130214     C                   WRITE     TL37S06
133600130214     c                   endif
133700130214     C*
133800130214     C****  EMISSIONE FORMATO  ****
133900130214     C     FOR06         TAG
134000130214     C                   WRITE     TL37T01
134100130214     C                   WRITE     TL37Z05
134200130214     C                   EXFMT     TL37C06
134300130214     C*
134400130214     C* F12 - RITORNO
134500130214    1C     *INKL         IFEQ      *OFF
134600130214     C                   GOTO      FOR06
134700130214    1C                   ENDIF
134800130214     C*
134900130214     C                   ENDSR
135000130214     C*
135100960321     C*
135200960226     C*--- SR INIZIALE -----------------------------------------------*
135300951110     C     *INZSR        BEGSR
135400960307     C****
135500960307     C** KLIST
135600960307     C****
135700960315     C* ACCESSO TNFVS01L
135800960315     C     KFVS          KLIST
135900960315     C                   KFLD                    V1CLNP
136000960315     C                   KFLD                    V1CNFV
136100960315     C* ACCESSO TNFVS03L
136200960315     C     KFVS2         KLIST
136300960315     C                   KFLD                    V1CLNP
136400960315     C                   KFLD                    WDFD
136500960315     C     KFVS3         KLIST
136600960320     C                   KFLD                    WFIL
136700960318     C                   KFLD                    WDFD
136800960321     C* ACCESSO FNFV203L
136900960321     C     KFV2          KLIST
137000960318     C                   KFLD                    FVSLNP                                     FOGLIO
137100960318     C                   KFLD                    FVSNFV
137200960318     C                   KFLD                    WEPA
137300960318     C                   KFLD                    WTDH
137400960315     C* ACCESSO FNFV401L
137500960315     C     KFV4          KLIST
137600960315     C                   KFLD                    FVSLNP                                     FOGLIO
137700960315     C                   KFLD                    FVSNFV
137800960315     C                   KFLD                    WEPA
137900960315     C                   KFLD                    WLAI                                       NE
138000960315     C                   KFLD                    WTRC
138100000504     C     KFWS          KLIST
138200000504     C                   KFLD                    FVSLNP                                     FOGLIO
138300000504     C                   KFLD                    FVSNFV
138400960319     C* ACCESSO FNFVD01L
138500960319     C     KFVD          KLIST
138600960319     C                   KFLD                    FVSLNP
138700960319     C                   KFLD                    FVSNFV
138800960319     C                   KFLD                    WLNA
138900960319     C                   KFLD                    WTRC2
139000951110     C* ACCESSO TABEL00F
139100960315     C     KTAB          KLIST
139200951110     C                   KFLD                    CODUT
139300951110     C                   KFLD                    COD
139400960315     C                   KFLD                    KEY
139500951110     C*
139600960228     C****
139700960228     C** DEFINIZIONE CAMPI
139800960228     C****
139900960229     C     *LIKE         DEFINE    TBLCOD        COD
140000960315     C     *LIKE         DEFINE    TBLKEY        KEY
140100960315     C     *LIKE         DEFINE    FVSLNP        WFIL
140200960318     C     *LIKE         DEFINE    FVSNFV        SAVNFV
140300960318     C     *LIKE         DEFINE    D37DFD        WDFD
140400960318     C     *LIKE         DEFINE    D37DFA        WDFA
140500960318     C     *LIKE         DEFINE    FV2EPA        WEPA
140600960318     C     *LIKE         DEFINE    FV2LAI        WLAI
140700960318     C     *LIKE         DEFINE    FV2TDH        WTDH
140800960318     C     *LIKE         DEFINE    FV4TRC        WTRC
140900960319     C     *LIKE         DEFINE    FVDLNA        WLNA
141000960319     C     *LIKE         DEFINE    FVDTRC        WTRC2
141100960320     C     *LIKE         DEFINE    VSCVOL        TERVOL
141200960307     C*
141300960315     C****  IMPOSTAZIONE CAMPI FISSI  ****
141400960315     C                   MOVEL     'TNTL37R'     V1CPGM
141500960315     C                   MOVE      TES(1)        V1CTES
141600960319     C                   MOVEL     '1'           WTRC2
141700960319     C                   MOVEL     'R'           WTDH
141800960315     C*
141900960229     C                   Z-ADD     1             CODUT
142000960229     C                   CALL      'X§PARUT'
142100960229     C                   PARM                    UT§DSE
142200960315     C                   MOVEL     RAGUT         V1CRSU
142300960318     C*
142400960318     C****  PRENDO DATA E ORA CORRENTE  ****
142500960318     C                   TIME                    W0140            14 0
142600960318     C                   MOVE      W0140         UDATE8            8 0
142700960228     C*
142800960229     C                   ENDSR
142900960315     C*--------------------------------------------------------------*
143000960315**  MSG
143100971203Linea di partenza errata                                                      1
143200971203La linea di partenza immessa non e' un terminal di partenza                   2
143300960315Occorre immettere il numero f.viaggio o la data f.viaggio                     3
143400960315Immettere o il numero f.viaggio o la data f.viaggio                           4
143500960315Se immesso il numero foglio viaggio occorre inserire la linea di partenza     5
143600960315Numero foglio viaggio inesistente                                             6
143700960315Data errata                                                                   7
143800960315La data foglio DAL non puo' essere maggiore della data foglio AL              8
143900960315Non esistono dati da visualizzare per la selezione effettuata !!              9
144000960315                                                                              10
144100960315**  TES
144200960315 * INTERROG.FOGLI VIAGGIO PARTENZE *
