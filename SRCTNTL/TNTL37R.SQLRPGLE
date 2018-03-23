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
003100151023     D tnfvsds       E DS                  EXTNAME(tnfvs00F) prefix(ds_)
003200960315     D*
003300960318     D* FILIALI DI ARRIVO
003400960318     D                 DS
003500000504     D  FFV                    1    900
003600000504     D                                     DIM(300)                             LINEE FOGLIO
003700960319     D  FVSLNA                 1      3  0
003800960319     D  FVSFFV                 4    243
003900960319     D  FVSFF2               244    450
004000000504     D  FWSFF3               451    690
004100000504     D  FWSFF4               691    900
004200960318     D* FILIALI DI SCARICO
004300960318     D                 DS
004400000504     D  FLP                    1    900
004500000504     D                                     DIM(300)                             LINEE TRANSITO
004600960319     D  WLNA2                  1      3  0
004700960319     D  FVSFLP                 4    243
004800960319     D  FVSFL2               244    450
004900000504     D  FWSFL3               451    690
005000000504     D  FWSFL4               691    900
005100960228     D                 DS
005200960228     D  AA                     1      4  0
005300960228     D  MM                     5      6  0
005400960228     D  GG                     7      8  0
005500960228     D  WDATA                  1      8  0
005600960318     D WLBDAT          DS
005700960318     D  G02DAT                 1      8  0
005800960318     D  G02INV                 9     16  0
005900960318     D  G02ERR                17     17
006000960318     D  G02TGI                18     22  0
006100960229     D*
006200960228     D DSTC          E DS
006300960320     D DSTV          E DS
006400951110     D KPJBA         E DS
006500960229     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006600121105     D Ctfp            S              3  0
006700121105     D Ctfa            S              3  0
006800121105     D Cdde            S              8  0
006900121105     D Cdsc            S              8  0
007000130214     D savnot          S                   like(fv4not)
007100151023     D v1ctrrt         S                   like(v1ctrr)
007200151102     D v1ctrmt         S                   like(v1ctrm)
007300151102     D wtrt            S                   like(v1ctrm)
007400151027     d XI              s              3  0
007500121105     d
007600151023     d $Finerec        s              1    inz(*off)
007700121105     d wrkgetlista     s           4096    varying
007800960319     D*
007900960319     D* DEFINIZIONE COSTANTI
008000960319     D CMAN            C                   CONST(' ABBINATO MANUALM-
008100960319     D                                     ENTE ')
008200960319     D CAUT            C                   CONST(' ABBINATO IN AUTO-
008300960319     D                                     MAT. ')
008400960319     D CNAB            C                   CONST('  NON  ABBINATO  ')
008500960319     D CNCT            C                   CONST(' NON CONTROLLATO ')
008600960319     D CFAT            C                   CONST(' GIA'' FATTURATO ')
008700960319     D CANF            C                   CONST(' ANNULLATO DA FIL. ')
008800960319     D CANS            C                   CONST(' ANNULLATO DA SEDE ')
008900960320     D CNUT            C                   CONST('NON UTILIZZATO IN SE-
009000960320     D                                     DE')
009100960320     D CTOT            C                   CONST('XXXXXXXX  TOTALE MC.')
009200951110     I/SPACE 3
009300951110     C*****************************************************************
009400951110     C* RIEPILOGO INDICATORI
009500951110     C*****************************************************************
009600130214     C* 02    - VISUALIZZO motivo sbinamento
009700960320     C* 03    - IN RI IL TOTALE MC
009800960320     C* 04    - VISUALIZZO SCRITTA "ABBINATO MANUALMENTE" E
009900960319     C*                            "ABBINATO IN AUTOMAT."
010000960319     C* 05    - VISUALIZZO SCRITTA "NON ABBINATO" E "NON CONTROLLATO"
010100960320     C* 06    - ABILITO L'OPZIONE "1" DI SELEZIONE F.VIAGGIO
010200960319     C* 07    - VISUALIZZO SCRITTA "ANNULLATO"
010300130214     C* 08    - VISUALIZZO punto di scambio
010400960315     C* 09    - IMMESSA DATA FOGLIO VIAGGIO AL
010500960315     C* 10    - IMMESSA LA DATA FOGLIO VIAGGIO
010600960315     C* 11    - IMMESSO UNO SPECIFICO FOGLIO VIAGGIO
010700960315     C* 12    - IMMESSA UNA SPECIFICA LINEA DI PARTENZA
010800960318     C* 16    - ESISTE DESCRIZIONE RITARDO/ANTICIPO: EVIDENZIO COMANDO
010900960321     C* 17    - INDICA SE C'E' DESCRIZIONE RITARDO
011000960319     C* 18    - VISUALIZZO SCRITTA "ECCEZIONALE"
011100010622     C* 19    - NON VISUALIZZO IL CELLULARE DEL CONDUCENTE
011200960315     C* 20    - PROGRAMMA RICHIAMATO
011300960318     C* 21    - PULIZIA SUBFILE  TL37C03
011400960318     C* 23    - PULIZIA SUBFILE  TL37C04
011500960318     C* 24    - PULIZIA SUBFILE  TL37C05
011600960315     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
011700960318     C* 29    - READC   SUBFILE  TL37S04
011800960318     C* 30/34 - DI COMODO
011900010919     C* 35    - CHAIN SU TNFWS
012000960315     C* 90    - INDICATORE DI ERRORE GENERICO
012100951110     C*****************************************************************
012200951110     C     *ENTRY        PLIST
012300951110     C                   PARM                    KPJBA
012400960315     C                   MOVEL     KPJBU         DSTL37
012500151022      /free
012600151022         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012700151022      /end-free
012800951113     C*
012900951110     C*---------------------------------------------------------------*
013000960320     C* CONTROLLO SE OCCORRE ABILITARE L'OPZIONE "1" DI SELEZIONE FV
013100960320     C     D37FL2        IFEQ      'S'
013200960320     C                   SETON                                        06
013300960320     C                   ENDIF
013400960320     C*
013500960315     C* CONTROLLO SE IL PROGRAMMA E' RICHIAMATO (PARFLG <> " ")
013600960315    1C     D37FLG        IFNE      ' '
013700960315     C                   SETON                                        20
013800960315     C                   Z-ADD     D37LNP        V1CLNP
013900960315     C                   Z-ADD     D37NFV        V1CNFV
014000960318     C                   Z-ADD     D37DFD        WDFD
014100960318     C                   Z-ADD     D37DFA        WDFA
014200960318     C* GIRO DATA FOGLIO VIAGGIO DAL / AL
014300960318    2C     WDFD          IFGT      0
014400960318     C                   Z-ADD     WDFD          WDATA
014500960318     C                   Z-ADD     MM            GGMM              4 0
014600960318     C                   MOVEL     GG            GGMM
014700960318     C                   Z-ADD     AA            V1CDFD
014800960318     C                   MOVEL     GGMM          V1CDFD
014900960318    3C     WDFA          IFGT      0
015000960318     C                   Z-ADD     WDFA          WDATA
015100960321     C                   Z-ADD     MM            GGMM
015200960318     C                   MOVEL     GG            GGMM
015300960318     C                   Z-ADD     AA            V1CDFA
015400960318     C                   MOVEL     GGMM          V1CDFA
015500960318    3C                   ENDIF
015600960318    2C                   ENDIF
015700960315     C*
015800960315     C* CONTROLLO SE DEVO VISUALIZZARE LA PRIMA VIDEATA CON LA
015900960315     C*   RICHIESTA PARAMETRI (D37FLG = "P")
016000960315    2C     D37FLG        IFNE      'P'
016100960315     C                   GOTO      CONTR
016200960315    2C                   ENDIF
016300960315    1C                   ENDIF
016400960315     C*
016500960315     C     INIZIO        TAG
016600960318     C                   EXSR      PULD01
016700960315     C*
016800960318     C     FMT01         TAG
016900960315     C                   WRITE     TL37T01
017000960315     C                   EXFMT     TL37D01
017100960315     C*
017200960315     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
017300960315     C                   CLEAR                   V1CMSG
017400960315     C                   SETOFF                                       284142
017500960315     C                   SETOFF                                       4344
017600960315     C*
017700960315     C* CMD3  - FINE LAVORO
017800960315     C   KC              GOTO      FINE
017900960315     C*
018000960315     C     CONTR         TAG
018100960315     C* CONTROLLI 1° VIDEATA
018200960315     C                   EXSR      CTRD01
018300960318     C   90              GOTO      FMT01
018400960318     C*
018500960318     C* CONTROLLO RICHIESTE EFFETTUATE
018600960318     C                   EXSR      CTRRIC
018700960315     C*
018800960318     C* 10 ON  - RICHIESTA SELEZIONE PER   D A T A   F. V I A G G I O
018900151022     c* 13 IN  - Richiesta selezione per numero targa
019000960318    1C     *IN10         IFEQ      *ON
019100151022    1C     *IN13         oreq      *ON
019200960315     C*
019300960315     C* CARICAMENTO SUBFILE PER SCELTA FOGLIO VIAGGIO
019400960315     C                   EXSR      RIES04
019500960315     C*
019600960318     C* NRR4 = "1" ---> NON E' STATO CARICATO NULLA
019700960315    2C     NRR4          IFEQ      1
019800960318     C     *IN20         IFEQ      *ON
019900960318     C                   GOTO      FINE
020000960318     C                   ELSE
020100960319     C                   MOVEL     MSG(9)        V1CMSG
020200960318     C                   SETON                                        28
020300960318     C                   GOTO      FMT01
020400960318     C                   ENDIF
020500960315    2C                   ENDIF
020600960315     C*
020700960315     C* VISUALIZZAZIONE SFL E SCELTA FOGLIO
020800960315     C                   Z-ADD     1             Z                 3 0
020900960318     C     FMT04         TAG
021000960315     C                   WRITE     TL37T01
021100960315     C                   WRITE     TL37Z04
021200960315     C                   EXFMT     TL37C04
021300960320     C* F3    - FINE
021400960320     C   KC              GOTO      FINE
021500960315     C*
021600960315     C* CMD12 - RITORNO
021700960318    2C     *INKL         IFEQ      *ON
021800960320     C   20D37FLG        IFEQ      'P'
021900960320     C                   GOTO      INIZIO
022000960320     C                   ELSE
022100960320     C                   GOTO      FINE
022200960320     C                   ENDIF
022300960320     C*
022400960318     C  N20              GOTO      INIZIO
022500960318    2C                   ENDIF
022600960315     C*
022700960318     C* LETTURA F.VIAGGIO SELEZIONATI
022800960315     C     LEGGI         TAG
022900960315     C                   READC     TL37S04                                29
023000960315     C*
023100960318     C   29              GOTO      FMT04
023200960315     C**
023300960315     C* NON SELEZIONATO NULLA
023400960318    2C     V4CSCE        IFNE      '1'                                          *SELEZIONE
023500960318     C     V4CSCE        ANDNE     '5'                                          *VISUALIZZ.
023600960315     C                   GOTO      LEGGI
023700960315    2C                   ENDIF
023800960315     C*
023900960315     C* SE NON E' RICHIAMATO VISUALIZZO IL FOGLIO CON L'OPZIONE "5"
024000960315    2C     *IN20         IFEQ      *OFF
024100960315     C* PULISCO IL CAMPO DI SCELTA SOLO SE CONTIENE L'OPZIONE "1"
024200960315    3C     V4CSCE        IFEQ      '1'
024300960315     C                   MOVEL     ' '           V4CSCE
024400960315     C                   UPDATE    TL37S04
024500960315     C                   GOTO      LEGGI
024600960315    3C                   ENDIF
024700960315     C*
024800960315   X2C                   ELSE
024900960315     C*
025000960315     C* SE PGM RICHIAMATO SELEZIONO IL FOGLIO CON L'OPZIONE "1"
025100960315    3C     V4CSCE        IFEQ      '1'
025200960320     C*
025300960320    4C     D37FL2        IFEQ      'S'
025400960315     C* PULISCO IL CAMPO DI SCELTA
025500960315     C                   MOVEL     ' '           V4CSCE
025600960315     C                   UPDATE    TL37S04
025700960315     C*
025800960320     C* PASSO INDIETRO IL PRIMO NUMERO DI FV SELEZIONATO
025900960315     C                   Z-ADD     V4CLNP        D37LNP
026000960315     C                   Z-ADD     V4CNFV        D37NFV
026100960315     C                   GOTO      FINE
026200960320     C*
026300960320   X4C                   ELSE
026400960320     C                   MOVEL     ' '           V4CSCE
026500960320     C                   UPDATE    TL37S04
026600960320     C                   GOTO      LEGGI
026700960320    4C                   ENDIF
026800960320     C*
026900960315    3C                   ENDIF
027000960315    2C                   ENDIF
027100960315     C*
027200960315     C* PULISCO IL CAMPO DI SCELTA
027300960315     C                   MOVEL     ' '           V4CSCE
027400960315     C                   UPDATE    TL37S04
027500960315     C*
027600960315     C                   Z-ADD     V4CLNP        V1CLNP
027700960315     C                   Z-ADD     V4CNFV        V1CNFV
027800960315    1C                   ENDIF
027900960315     C*
028000960315     C* RIEMPIO CAMPI 2° VIDEATA
028100960315     C                   EXSR      RIED02
028200960315     C*
028300960315     C     FOR02         TAG
028400960315     C                   WRITE     TL37T01
028500960315     C                   EXFMT     TL37D02
028600960315     C*
028700960315     C* F12- RITORNO
028800960318     C     *INKL         IFEQ      *ON
028900960320     C  N10
029000151103     cann13
029100960320     CANN20              GOTO      INIZIO
029200960320     C*
029300960320     C  N10
029400151103     cann13
029500960320     CAN 20D37FLG        IFEQ      'P'
029600960320     C                   GOTO      INIZIO
029700960320     C                   ELSE
029800960320     C                   GOTO      FINE
029900960320     C                   ENDIF
030000960320     C*
030100151103     c   13
030200151103     Cor 10              GOTO      LEGGI
030300960318     C                   ENDIF
030400960315     C*
030500960315     C* F8 - INTERROGA ORARI ARRIVO
030600960315    1C     *INKH         IFEQ      *ON
030700960315     C                   EXSR      INTORA
030800960315     C                   GOTO      FOR02
030900960315    1C                   ENDIF
031000130214     C* F15 - INTERROGA motivi sbinamento
031100130214    1C     *INKP         IFEQ      *ON
031200130214     C                   EXSR      INTmot
031300130214     C                   GOTO      FOR02
031400130214    1C                   ENDIF
031500960315     C*
031600960318     C     FOR03         TAG
031700960318     C                   WRITE     TL37T01                                      TESTATA VIDEO
031800960318     C                   WRITE     TL37Z03
031900960318     C                   EXFMT     TL37C03
032000960318     C* F12 - RITORNO
032100960318     C   KL              GOTO      FOR02
032200960318     C*
032300960315     C* SE SELEZIONE PER DATA VISUALIZZO TUTTI I FV SELEZIONATI
032400151103     c   13
032500151103     Cor 10              GOTO      LEGGI
032600960315     C*
032700960321    1C     *IN20         IFEQ      *OFF
032800960321     C  N10
032900151103     cann13
033000960321     CAN 11              GOTO      INIZIO
033100960321   X1C                   ELSE
033200960321     C     D37FLG        IFEQ      'P'
033300960321     C                   GOTO      INIZIO
033400960321     C                   ELSE
033500960321     C                   GOTO      FINE
033600960321     C                   ENDIF
033700960321    1C                   ENDIF
033800960307     C*
033900951110     C     FINE          TAG
034000960321     C* RICHIAMO PGM FNLV15R PER CHIUSURA FILES
034100960321    1C     WLV15         IFNE      ' '
034200960321     C                   CLEAR                   DSLV15
034300960321     C                   MOVEL     'C'           D15TLA
034400960321     C                   MOVEL     DSLV15        KPJBU
034500960321     C                   CALL      'FNLV15R'
034600960321     C                   PARM                    KPJBA
034700960321    1C                   ENDIF
034800960321     C*
034900960315     C   20              MOVEL     DSTL37        KPJBU
035000951113     C                   SETON                                        LR
035100960117     C*
035200960315     C*--- CONTROLLO RICHIESTE EFFETTUATE ----------------------------*
035300960315     C     CTRRIC        BEGSR
035400960315     C                   SETOFF                                       091011
035500151023     C                   SETOFF                                       1213
035600960318     C*
035700960315     C* 12 ON  - IMMESSA UNA SPECIFICA LINEA DI PARTENZA
035800960318    1C     V1CLNP        IFGT      0
035900960315     C                   SETON                                        12
036000960315    1C                   ENDIF
036100960315     C*
036200960315     C* 11 ON  - IMMESSO UNO SPECIFICO FOGLIO VIAGGIO
036300960318    1C     V1CNFV        IFGT      0
036400960315     C                   SETON                                        1112
036500960315     C*
036600960315   X1C                   ELSE
036700960315     C*
036800960315     C* 10 ON  - IMMESSA LA DATA FOGLIO VIAGGIO
036900960318    2C     WDFD          IFGT      0
037000960315     C                   SETON                                        10
037100960315     C*
037200960318    3C     WDFA          IFEQ      0
037300960318     C                   Z-ADD     WDFD          WDFA
037400960315   X3C                   ELSE
037500960315     C* 09 ON  - IMMESSA DATA FOGLIO VIAGGIO AL
037600960318     C     WDFA          IFNE      WDFD
037700960315     C                   SETON                                        09
037800960315     C                   ENDIF
037900960315    3C                   ENDIF
038000960315     C*
038100151023     c                   else
038200151023     c                   if        v1ctrr<>*blanks
038300151102     c                             or v1ctrm<>*blanks
038400151023     c                   seton                                        13
038500151023     c                   endif
038600960315    2C                   ENDIF
038700960315    1C                   ENDIF
038800960315     C*
038900960315     C                   ENDSR
039000960315     C*
039100960315     C*--- PULIZIA CAMPI 1° VIDEATA ----------------------------------*
039200960318     C     PULD01        BEGSR
039300960315     C                   SETOFF                                       10
039400960315     C                   CLEAR                   V1CLNP
039500960315     C                   CLEAR                   V1CNFV
039600960315     C                   CLEAR                   V1CDFD
039700960315     C                   Z-ADD     UDATE8        V1CDFA
039800960320     C                   MOVEL     'S'           V1CSCE
039900130318     C                   CLEAR                   V1Cbis
040000130318     C                   CLEAR                   V1Cpsc
040100960315     C                   ENDSR
040200960302     C*
040300960315     C*--- CONTROLLI 1° VIDEATA --------------------------------------*
040400960315     C     CTRD01        BEGSR
040500960315     C                   SETOFF                                       90
040600960315     C*
040700960315     C****  LINEA DI PARTENZA  ****
040800960315    1C     V1CLNP        IFNE      0
040900960315     C     V1CLNP        CHAIN     AZORG                              31
041000960315    2C     *IN31         IFEQ      *ON
041100971203     C     ORGFVA        ORNE      ' '
041200971203     C     ORGFAG        ORNE      'A'
041300971203     C     ORGFAG        ANDNE     'F'
041400960315     C                   MOVEL     MSG(1)        V1CMSG
041500960315     C                   SETON                                        412890
041600960315     C                   GOTO      ENDCTR
041700960315    2C                   ENDIF
041800960315    1C                   ENDIF
041900960315     C*
042000960315     C****  OCCORRE IMMETTERE O IL NUMERO FOGLIO O LA DATA  ****
042100151022     c****  o la targa
042200960315    1C     V1CNFV        IFEQ      0
042300960315     C     V1CDFD        ANDEQ     0
042400151022     C     V1Ctrr        ANDEQ     *blanks
042500151102     C     V1Ctrm        ANDEQ     *blanks
042600960315     C                   MOVEL     MSG(3)        V1CMSG
042700960315     C                   SETON                                        422890
042800960315     C                   GOTO      ENDCTR
042900960315    1C                   ENDIF
043000960315     C*
043100960315     C****  IMPOSSIBILE INSERIRE ENTRAMBI  ****
043200960315    1C     V1CNFV        IFNE      0
043300960315     C     V1CDFD        ANDNE     0
043400960315     C                   MOVEL     MSG(4)        V1CMSG
043500960315     C                   SETON                                        422890
043600960315     C                   GOTO      ENDCTR
043700960315    1C                   ENDIF
043800960315     C*
043900960315     C****  NUMERO FOGLIO VIAGGIO  ****
044000960315    1C     V1CNFV        IFNE      0
044100960315     C* SE IMMESSO IL FOGLIO VIAGGIO OCCORRE INSERIRE ANCHE LA LNP
044200960315    2C     V1CLNP        IFEQ      0
044300960315     C                   MOVEL     MSG(5)        V1CMSG
044400960315     C                   SETON                                        412890
044500960315     C                   GOTO      ENDCTR
044600960315    2C                   ENDIF
044700960315     C*
044800960315     C* CONTROLLO VALIDITA' NUMERO FOGLIO VIAGGIO
044900960315     C     KFVS          CHAIN     TNFVS01L                           31
045000960315    2C     *IN31         IFEQ      *ON
045100960315     C                   MOVEL     MSG(6)        V1CMSG
045200960315     C                   SETON                                        422890
045300960315     C                   GOTO      ENDCTR
045400960315    2C                   ENDIF
045500960315    1C                   ENDIF
045600960315     C*
045700960315     C****  DATA FOGLIO VIAGGIO  DAL  ****
045800151027     c                   clear                   wdfd
045900151027     c                   clear                   wdfa
046000960315    1C     V1CDFD        IFNE      0
046100960315     C                   MOVEL     V1CDFD        G02DAT
046200960315     C                   MOVEL     *ZERO         G02INV
046300960315     C                   MOVEL     *BLANKS       G02ERR
046400960315     C                   CALL      'XSRDA8'
046500960315     C                   PARM                    WLBDAT
046600960315     C     G02ERR        IFEQ      '1'
046700960315     C                   MOVEL     MSG(7)        V1CMSG
046800960315     C                   SETON                                        432890
046900960315     C                   GOTO      ENDCTR
047000960315     C                   ENDIF
047100960315     C                   Z-ADD     G02DAT        V1CDFD
047200960315     C                   Z-ADD     G02INV        WDFD
047300960315    1C                   ENDIF
047400960315     C*
047500960315     C****  DATA FOGLIO VIAGGIO  AL  ****
047600960315    1C     V1CDFA        IFNE      0
047700960315     C                   MOVEL     V1CDFA        G02DAT
047800960315     C                   MOVEL     *ZERO         G02INV
047900960315     C                   MOVEL     *BLANKS       G02ERR
048000960315     C                   CALL      'XSRDA8'
048100960315     C                   PARM                    WLBDAT
048200960315     C     G02ERR        IFEQ      '1'
048300960315     C                   MOVEL     MSG(7)        V1CMSG
048400960315     C                   SETON                                        442890
048500960315     C                   GOTO      ENDCTR
048600960315     C                   ENDIF
048700960315     C                   Z-ADD     G02DAT        V1CDFA
048800960315     C                   Z-ADD     G02INV        WDFA
048900960315     C*
049000960315     C* DATA DAL NO MAGGIORE DATA AL
049100960315    2C     WDFD          IFGT      WDFA
049200960315     C                   MOVEL     MSG(8)        V1CMSG
049300960315     C                   SETON                                        432890
049400960315     C                   GOTO      ENDCTR
049500960315    2C                   ENDIF
049600960315    1C                   ENDIF
049700960315     C*
049800960315     C     ENDCTR        ENDSR
049900960315     C*
050000960315     C*--- CARICAMENTO SFL PER SCELTA F.VIAGGIO ----------------------*
050100960315     C     RIES04        BEGSR
050200960318     C                   Z-ADD     1             NRR4              5 0
050300960318     C*
050400960318     C****  PULIZIA SUBFILE  ****
050500960318     C                   SETON                                        23
050600960318     C                   WRITE     TL37C04
050700960318     C                   SETOFF                                       23
050800960315     C*
050900960318     C****  CARICAMENTO SUBFILE  ****
051000151022     c                   if        *in10
051100960318     C* 12 ON  - IMMESSA UNA SPECIFICA LINEA PARTENZA
051200960318    1C     *IN12         IFEQ      *ON
051300960315     C*
051400960315     C     KFVS2         SETLL     TNFVS03L
051500960315     C  N09KFVS2         READE     TNFVS03L                               30
051600960315     C   09V1CLNP        READE     TNFVS03L                               30
051700960315     C*
051800960318    2C     *IN30         DOWEQ     *OFF
051900960315     C*
052000960315     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
052100960318    3C   09FVSDFV        IFGT      WDFA
052200960315     C                   SETON                                        30
052300960318    3C                   ENDIF
052400960315     C*
052500960318    3C     *IN30         IFEQ      *OFF
052600960318     C* SCRIVO CAMPI DEL SFL
052700960318     C                   EXSR      WRTS04
052800960315     C*
052900960315     C  N09KFVS2         READE     TNFVS03L                               30
053000960315     C   09V1CLNP        READE     TNFVS03L                               30
053100960318    3C                   ENDIF
053200960318    2C                   ENDDO
053300960315     C*
053400960318   X1C                   ELSE
053500960318     C* 12 OFF - RICHIESTE TUTTE LE LINEE DI PARTENZA
053600960318     C*
053700960315     C                   CLEAR                   WFIL
053800960315     C                   SETOFF                                       30
053900960315     C*
054000960318    2C     *IN30         DOWEQ     *OFF
054100960315     C     WFIL          SETGT     TNFVS03L
054200960315     C                   READ      TNFVS03L                               30
054300960315     C*
054400960318    3C     *IN30         IFEQ      *OFF
054500960315     C                   Z-ADD     FVSLNP        WFIL
054600960315     C*
054700960320     C     KFVS3         SETLL     TNFVS03L
054800960315     C  N09KFVS3         READE     TNFVS03L                               31
054900960315     C   09WFIL          READE     TNFVS03L                               31
055000960315     C*
055100960318    4C     *IN31         DOWEQ     *OFF
055200960315     C*
055300960315     C* SE DATA FOGLIO VIAGGIO > DATA AL ---> LEGGO ALTRA LINEA
055400960318    5C   09FVSDFV        IFGT      WDFA
055500960318     C                   SETON                                        31
055600960318    5C                   ENDIF
055700960315     C*
055800960318    5C     *IN31         IFEQ      *OFF
055900960318     C* SCRIVO CAMPI DEL SFL
056000960318     C                   EXSR      WRTS04
056100960315     C*
056200960315     C  N09KFVS3         READE     TNFVS03L                               31
056300960315     C   09WFIL          READE     TNFVS03L                               31
056400960318    5C                   ENDIF
056500960318    4C                   ENDDO
056600960315     C*
056700960318    3C                   ENDIF
056800960318    2C                   ENDDO
056900960315     C*
057000960318    1C                   ENDIF
057100151022     c                   else
057200151022     c* selezione per numero di targa: ricerco con sql
057300151022      /free
057400151027     c* prima compatto la targa
057500151102       if v1ctrm<>*blanks;
057600151102          wtrt=v1ctrm;
057700151102          exsr sr_comp;
057800151102          v1ctrmt=wtrt;
057900151102       else;
058000151102          wtrt=v1ctrr;
058100151102          exsr sr_comp;
058200151102          v1ctrrt=wtrt;
058300151102       endif;
058400151027     c* SQL
058500151102       if v1ctrm<>*blanks;
058600151102       Wrkgetlista  ='select * from tnfvs00f'                     +
058700151102                     ' where (fvstrm=' + '''' + v1ctrmt + ''''  +
058800151102                     ' or fvstrm=' + '''' + v1ctrm + ''')';
058900151102       else;
059000151023       Wrkgetlista  ='select * from tnfvs00f'                     +
059100151027                     ' where (fvstrr=' + '''' + v1ctrrt + ''''  +
059200151027                     ' or fvstrr=' + '''' + v1ctrr + ''')';
059300151102       endif;
059400151022       // presente data al
059500151023       if wdfa   >0;
059600151023          Wrkgetlista  =Wrkgetlista  +
059700151023          ' and fvsdfv<=' + %editc(wdfa:'X');
059800151023       endif;
059900151022       if v1clnp>0;
060000151022       // presente specifica lnp
060100151023          Wrkgetlista  =Wrkgetlista  +
060200151022          ' and fvslnp=' + %editc(v1clnp:'X');
060300151022       endif;
060400151118          Wrkgetlista  =Wrkgetlista  +
060500171006          ' order by fvsdfv desc';
060600151022        $finerec=*off;
060700151023        exec sql prepare STRINGASQL from :Wrkgetlista  ;
060800151022        exec sql declare fvsCsr cursor for StringaSql;
060900151022        exec sql open FVScsr;
061000151022        dow $finerec=*off;
061100151022           exec sql Fetch FVScsr into :tnfvsds;
061200151022           if sqlcod=100 or sqlcod<0;
061300151022              $finerec = *on;
061400151022              leave;
061500151022           endif;
061600151023           chain (ds_fvslnp:ds_fvsnfv) tnfvs01l;
061700151023           exsr wrts04;
061800151022        enddo;
061900151023        exec sql close FVScsr;
062000151022     c                   endif
062100960315     C*
062200960315     C                   ENDSR
062300960315     C*
062400960318     C*--- SCRIVO CAMPI SFL SELEZIONE F.VIAGGIO ----------------------*
062500960318     C     WRTS04        BEGSR
062600960320     C                   MOVEL     'S'           WCARIC            1
062700960320     C                   CLEAR                   §TVDES
062800960320     C*
062900960320     C* CONTROLLO IL TIPO TRAINO
063000960320    1C     FVSTTR        IFNE      ' '
063100960320     C     V1CSCE        ANDNE     'T'
063200960320     C                   MOVEL     'TV'          COD
063300960320     C                   MOVEL(P)  FVSTTR        KEY
063400960320     C     KTAB          CHAIN     TABEL                              32
063500960320     C*
063600960320    2C     *IN32         IFEQ      *OFF
063700960320     C     TBLFLG        ANDEQ     ' '
063800960320     C                   MOVEL     TBLUNI        DSTV
063900960320     C* VISULIZZO SOLO I NON UTILIZZATI IN SEDE
064000960320    3C     §TVUTC        IFEQ      'S'
064100960320     C     V1CSCE        ANDEQ     'N'
064200960320     C                   MOVEL     'N'           WCARIC
064300960320    3C                   ENDIF
064400960320     C* VISULIZZO SOLO GLI UTILIZZATI IN SEDE
064500960320    3C     §TVUTC        IFNE      'S'
064600960320     C     V1CSCE        ANDEQ     'S'
064700960320     C                   MOVEL     'N'           WCARIC
064800960320    3C                   ENDIF
064900960320     C*
065000960320    2C                   ENDIF
065100960320    1C                   ENDIF
065200960320     C*
065300960320     C* VISULIZZO SOLO I NON UTILIZZATI IN SEDE
065400960320    3C     FVSTTR        IFEQ      ' '
065500960320     C     V1CSCE        ANDEQ     'N'
065600960320     C                   MOVEL     'N'           WCARIC
065700960320    3C                   ENDIF
065800130318     c
065900130318     c* Visualizzo solo i BIS
066000130318     c                   if        v1cbis='B' and fvsbis=' '
066100130318     C                   MOVEL     'N'           WCARIC
066200130318    3C                   ENDIF
066300130318     c                   if        v1cbis='N' and fvsbis='B'
066400130318     C                   MOVEL     'N'           WCARIC
066500130318    3C                   ENDIF
066600130318     c**
066700130318      ** Visualizzo solo quello con punto di scambio
066800130318     c                   if        v1cpsc<>' '
066900130318     C     kfWs          setll     tNFGs000
067000130318     c                   if        %equal(tNFGS01l) and v1cpsc='N'
067100130318     C                   MOVEL     'N'           WCARIC
067200130318    3C                   ENDIF
067300130318     c                   if        not %equal(tNFGS01l) and v1cpsc='S'
067400130318     C                   MOVEL     'N'           WCARIC
067500130318    3C                   ENDIF
067600130318    3C                   ENDIF
067700151023      ** visualizzo solo se targa rimorchio uguale a quella richiesta
067800151023     c                   if        v1ctrr<>*blanks
067900151023     c                   if        fvstrr<>v1ctrr and
068000151027     c                             fvstrr<>v1ctrrt
068100151023     C                   MOVEL     'N'           WCARIC
068200151023     c                   endif
068300151023     c                   endif
068400151102      ** visualizzo solo se targa motrice  uguale a quella richiesta
068500151102     c                   if        v1ctrm<>*blanks
068600151102     c                   if        fvstrm<>v1ctrm and
068700151102     c                             fvstrm<>v1ctrmt
068800151102     C                   MOVEL     'N'           WCARIC
068900151102     c                   endif
069000151102     c                   endif
069100960320     C**
069200960320     C** DA CARICARE
069300960320     C     WCARIC        IFEQ      'S'
069400960315     C* CAMPO SELEZIONE
069500960315     C                   CLEAR                   V4CSCE
069600960315     C* LINEA PARTENZA
069700960315     C                   MOVEL     FVSLNP        V4CLNP
069800960315     C     FVSLNP        CHAIN     AZORG                              32
069900960320     C  N32ORGDE5        IFNE      *BLANKS
070000130319     C**                 MOVEL     ORGDE5        V4DLNP
070100960320     C                   ELSE
070200130319     C**                 MOVEL     ORGDES        V4DLNP
070300960320     C                   ENDIF
070400130319     C** 32              MOVEL     *BLANKS       V4DLNP
070500960315     C* NUMERO FOGLIO VIAGGIO
070600960315     C                   MOVEL     FVSNFV        V4CNFV
070700130318     C                   MOVEL     FVSbis        V4Cbis
070800960315     C* GIRO DATA FOGLIO VIAGGIO
070900960318     C                   Z-ADD     FVSDFV        WDATA
071000960321     C                   Z-ADD     MM            GGMM
071100960318     C                   MOVEL     GG            GGMM
071200130318     C                   Z-ADD     AA            V4CDFv
071300130318     C                   MOVEL     GGMM          V4CDFv
071400960315     C* TIPO TRAINO
071500130318     C****               MOVEL     FVSTTR        V4CTTR
071600960315     C* GIRO DATA PARTENZA REALE
071700130318     C**                 Z-ADD     FVSDRP        WDATA
071800130318     C**                 Z-ADD     MM            GGMM
071900130318     C**                 MOVEL     GG            GGMM
072000130318     C**                 Z-ADD     GGMM          V4CDRP
072100960315     C* LINEA FINALE DI ARRIVO
072200960315     C                   MOVEL     FVSLNA        V4CLNA
072300960315     C     FVSLNA        CHAIN     AZORG                              32
072400960320     C  N32ORGDE5        IFNE      *BLANKS
072500960320     C                   MOVEL     ORGDE5        V4DLNA
072600960320     C                   ELSE
072700960320     C                   MOVEL     ORGDES        V4DLNA
072800960320     C                   ENDIF
072900960315     C   32              MOVEL     *BLANKS       V4DLNA
073000960315     C* NUMERO TRAINO
073100960315     C                   MOVEL     FVSTRN        V4CTRN
073200960315     C* SE F.VIAGGIO ANNULLATO LO SEGNALO
073300960315     C                   CLEAR                   V4CANN
073400960315    1C     FVSATB        IFNE      ' '
073500960315     C                   MOVEL     'An'          V4CANN
073600960315    1C                   ENDIF
073700130318     c* indico se c'e' lo scambio
073800130319     C     kfWs          chain     tNFGs000
073900130319     c                   if        %found(tnfgs01l)
074000130319     c
074100130319     c                   if        fgstrns>0
074200130319     c                   eval      V4Cpsc=%editc(fgstrns:'Z')
074300130319     c                   else
074400130319     c                   movel     fgspsc        v4cpsc
074500130319     c                   endif
074600130319     c
074700130318     c                   else
074800130319     c                   clear                   V4Cpsc
074900130318     c                   endif
075000960320     C* TRAINO DI SEDE
075100960320     C                   MOVEL     FVSTRS        V4CTRS
075200960320     C* TRAZIONISTA
075300960320     C                   MOVEL     FVSDPD        V4CDPD
075400960315     C*
075500960315     C                   WRITE     TL37S04
075600960315     C                   ADD       1             NRR4
075700960320     C                   ENDIF
075800960320     C*
075900960315     C                   ENDSR
076000960315     C*
076100960315     C*--- RIEMPO CAMPI 2° VIDEATA -----------------------------------*
076200960315     C     RIED02        BEGSR
076300121105     C                   SETOFF                                       071608
076400960319     C                   SETOFF                                       180405
076500960319     C                   CLEAR                   V2DTFM
076600960319     C                   CLEAR                   V2DTFS
076700960319     C                   CLEAR                   V2DABN
076800960319     C                   CLEAR                   V2DAB2
076900061003     C                   CLEAR                   V2can1
077000061003     C                   CLEAR                   V2can2
077100960315     C*
077200960315     C     KFVS          CHAIN     TNFVS01L                           33
077300010919     C     KFVS          CHAIN     TNFWS01L                           35
077400960319     C*
077500960319    1C     *IN33         IFEQ      *OFF
077600960319     C                   MOVEL     FVSLNA        WLNA2
077700960315     C*
077800960319     C* SE FOGLIO VIAGGIO ANNULLATO ---> LO SEGNALO
077900960319    2C     FVSATB        IFNE      ' '
078000960315     C                   SETON                                        07
078100960319     C     FVSATB        IFEQ      '*'
078200960319     C                   MOVEL     CANF          V2DANN                         *DA FILIALE
078300960319     C                   ENDIF
078400960319     C     FVSATB        IFEQ      'A'
078500960319     C                   MOVEL     CANS          V2DANN                         *DA SEDE
078600960319     C                   ENDIF
078700960319    2C                   ENDIF
078800960315     C*
078900960319     C* NUMERO FOGLIO VIAGGIO
079000960315     C                   MOVEL     FVSNFV        V2CNFV
079100960315     C* GIRO DATA FOGLIO VIAGGIO
079200960315     C                   Z-ADD     FVSDFV        WDATA
079300960321     C                   Z-ADD     MM            GGMM
079400960315     C                   MOVEL     GG            GGMM
079500960315     C                   Z-ADD     AA            V2CDFV
079600960315     C                   MOVEL     GGMM          V2CDFV
079700960315     C*
079800960319     C* DATI RELATIVI AL TRAINO DI FILIALE
079900960315     C                   MOVEL     FVSTRN        V2CTRN
080000960319     C                   MOVEL     FVSTFM        V2CTFM
080100960319    2C     FVSTFM        IFEQ      'A'
080200960319     C                   MOVEL     'ANDATA '     V2DTFM
080300960319    2C                   ENDIF
080400960319    2C     FVSTFM        IFEQ      'R'
080500960319     C                   MOVEL     'RITORNO'     V2DTFM
080600960319    2C                   ENDIF
080700960319     C*
080800960319     C* DATI RELATIVI AL TRAINO DI SEDE
080900960315     C                   MOVEL     FVSTRS        V2CTRS
081000960319     C                   MOVEL     FVSTFS        V2CTFS
081100960319    2C     FVSTFS        IFEQ      'A'
081200960319     C                   MOVEL     'ANDATA '     V2DTFS
081300960319    2C                   ENDIF
081400960319    2C     FVSTFS        IFEQ      'R'
081500960319     C                   MOVEL     'RITORNO'     V2DTFS
081600960319    2C                   ENDIF
081700960315     C*
081800960319     C* SE TRAINO DI SEDE E' ECCEZIONALE LO SEGNALO
081900960319    2C     FVSTRS        IFGE      9000000
082000960319     C     FVSTRS        ANDLT     9999998
082100960319     C                   SETON                                        18
082200960319    2C                   ENDIF
082300960319     C*
082400960319     C* ABBINATO MANUALMENTE
082500960319    2C     FVSTRS        IFGT      0
082600960319     C     FVSTRS        ANDNE     FVSTRN
082700960319     C     FVSTRS        ANDLT     9999998
082800960319     C     FVSTRS        ORGE      9000000
082900960319     C     FVSTRS        ANDLT     9999998
083000960319     C                   SETON                                        04
083100960319     C                   MOVEL     CMAN          V2DABN
083200960319    2C                   ENDIF
083300960319     C* ABBINATO IN AUTOMATICO
083400960319    2C     FVSTRS        IFGT      0
083500960319     C     FVSTRS        ANDLT     9000000
083600960319     C     FVSTRS        ANDEQ     FVSTRN
083700960319     C                   SETON                                        04
083800960319     C                   MOVEL     CAUT          V2DABN
083900960319    2C                   ENDIF
084000960319     C* NON ABBINATO
084100960319    2C     FVSTRS        IFEQ      0
084200960319     C     FVSABN        ANDEQ     'S'
084300960319     C                   SETON                                        05
084400960319     C                   MOVEL     CNAB          V2DAB2
084500960319    2C                   ENDIF
084600960319     C* NON CONTROLLATO
084700960319    2C     FVSABN        IFEQ      ' '
084800960319     C                   SETON                                        05
084900960319     C                   MOVEL     CNCT          V2DAB2
085000960319    2C                   ENDIF
085100960319     C* GIA' FATTURATO
085200960319    2C     FVSTRS        IFEQ      9999998
085300960319     C                   SETON                                        05
085400960319     C                   MOVEL     CFAT          V2DAB2
085500960319    2C                   ENDIF
085600960320     C* NIN UTILIZZATO NEI CONTROLLI DI SEDE
085700960320     C     FVSTRS        IFEQ      9999999
085800960320     C     FVSATB        ANDEQ     ' '
085900960320     C                   SETON                                        04
086000960320     C                   MOVEL     CNUT          V2DABN
086100960320    2C                   ENDIF
086200960319     C*
086300960319     C* LINEA PARTENZA
086400960315     C                   MOVEL     FVSLNP        V2CLNP
086500960320     C                   MOVEL     FVSLNP        V2CLN2
086600960315     C     FVSLNP        CHAIN     AZORG                              32
086700960315     C  N32              MOVEL     ORGDES        V2DLNP
086800960315     C   32              MOVEL     *BLANKS       V2DLNP
086900960319     C* LINEA ARRIVO
087000960315     C                   MOVEL     FVSLNA        V2CLNA
087100960315     C     FVSLNA        CHAIN     AZORG                              32
087200960315     C  N32              MOVEL     ORGDES        V2DLNA
087300960315     C   32              MOVEL     *BLANKS       V2DLNA
087400960315     C*
087500960319     C* TIPO TRAINO
087600960315     C                   MOVEL     FVSTTR        V2CTTR
087700960315     C                   MOVEL     'TV'          COD
087800960315     C                   MOVEL(P)  FVSTTR        KEY
087900960315     C     KTAB          CHAIN     TABEL                              32
088000960315     C  N32              MOVEL     TBLUNI        V2DTTR
088100960315     C   32              CLEAR                   V2DTTR
088200960319     C* CATEGORIA TRAINO (BIS/ECCEZIONALE)
088300960315     C                   MOVEL     FVSBIS        V2CBIS
088400960319     C* TIPO AUTOMEZZO
088500960315     C                   MOVEL     FVSTMZ        V2CTMZ
088600960315     C                   MOVEL     'TU'          COD
088700960315     C                   MOVEL(P)  FVSTMZ        KEY
088800960315     C     KTAB          CHAIN     TABEL                              32
088900960315     C  N32              MOVEL     TBLUNI        V2DTMZ
089000960315     C   32              CLEAR                   V2DTMZ
089100960319     C* TRAZIONISTA
089200960315     C                   MOVEL     FVSPDR        V2CPDR
089300960315     C                   MOVEL     FVSDPD        V2CDPD
089400960319     C* CONDUCENTE
089500130429     C                   MOVEL     FVSpmb        V2CDCN
089600010622     C* CELLULARE CONDUCENTE
089700010702     C                   MOVE      'P'           WEPA
089800010702     C                   MOVE      *BLANKS       WLAI
089900010622     C                   MOVE      'L'           WTRC
090000010702     C     KFV4          CHAIN     FNFV401L                           30
090100010622     C     *IN30         IFEQ      *ON
090200010622     C                   MOVE      *ON           *IN19
090300010622     C                   ELSE
090400010622     C                   MOVEL     FV4NOT        V2CCNC
090500010622     C                   ENDIF
090600960319     C* TARGHE
090700960315     C                   MOVEL     FVSTRM        V2CTRM
090800960315     C                   MOVEL     FVSTRR        V2CTRR
090900960319     C* IMPORTI
091000010919     C**!!!                MOVELFVSIPT    V2CIPT
091100010919     C**!!!                MOVELFVSIES    V2CIES
091200010919     C                   CLEAR                   V2CIPT
091300010919     C                   CLEAR                   V2CIES
091400010919     C                   CLEAR                   V2CVIP
091500010919     C                   CLEAR                   V2CVES
091600010919      * Se il file dell'estensione non esiste o i campi sono a zero
091700010919      * devo prendere quello che c'è nel file principale
091800010919     C     *IN35         IFEQ      *ON
091900010919     C     FVSIPT        IFGT      *ZEROS
092000010919     C                   Z-ADD     FVSIPT        V2CIPT
092100010919     C                   MOVEL     'ITL'         V2CVIP
092200010919     C                   ENDIF
092300010919     C     FVSIES        IFGT      *ZEROS
092400010919     C                   Z-ADD     FVSIES        V2CIES
092500010919     C                   MOVEL     'ITL'         V2CVES
092600010919     C                   ENDIF
092700010919      * Se il file dell'estensione esiste e i campi sono a zero
092800010919      * devo prendere quello che c'è nel file principale
092900010919     C                   ELSE
093000010919     C     FWSIPT        IFEQ      *ZEROS
093100010919     C     FVSIPT        ANDGT     *ZEROS
093200010919     C                   Z-ADD     FVSIPT        V2CIPT
093300010919     C                   MOVEL     'ITL'         V2CVIP
093400010919     C                   ENDIF
093500010919     C     FWSIES        IFEQ      *ZEROS
093600010919     C     FVSIES        ANDGT     *ZEROS
093700010919     C                   Z-ADD     FVSIES        V2CIES
093800010919     C                   MOVEL     'ITL'         V2CVES
093900010919     C                   ENDIF
094000010919      * Se il file dell'estensione esiste e i campi sono > zero
094100010919      * devo prendere quello che c'è nel file dell'estensione
094200010919     C     FWSIPT        IFGT      *ZEROS
094300010919     C                   Z-ADD     FWSIPT        V2CIPT
094400010919     C                   MOVEL     FWSVIP        V2CVIP
094500010919     C                   ENDIF
094600010919     C     FWSIES        IFGT      *ZEROS
094700010919     C                   Z-ADD     FWSIES        V2CIES
094800010919     C                   MOVEL     FWSVES        V2CVES
094900010919     C                   ENDIF
095000010919     C                   ENDIF
095100960319     C* PIOMBI
095200130424     c                   clear                   v2cpmb
095300130424     C                   MOVE      'P'           WEPA
095400130424     C                   MOVE      *BLANKS       WLAI
095500130424     C                   MOVE      'P'           WTRC
095600130424     C     KFV4          CHAIN     FNFV401L
095700130424     c
095800130424     C                   if        %found(fnfv401l) and fv4atb=' '
095900130424     C                   MOVEL     FV4NOT        V2Cpmb
096000130424     c     KFV4          reade     FNFV401L
096100130424     C                   if        not %eof(fnfv401l) and fv4atb=' '
096200130424  156C                   eval      %subst(v2cpmb:36)=fv4not
096300130424     c                   endif
096400130424     c                   endif
096500960319     C* NOTE
096600960315     C                   MOVEL     FVSNT1        V2CNT1
096700960315     C                   MOVEL     FVSNT2        V2CNT2
096800061003     c* Annotazioni
096900061003     C                   MOVE      'P'           WEPA
097000061003     C                   MOVE      *BLANKS       WLAI
097100061003     C                   MOVE      'N'           WTRC
097200061003     C     KFV4          CHAIN     FNFV401L                           30
097300061003     c
097400061003     C                   if        not *in30 and fv4atb=' '
097500061003     C                   MOVEL     FV4NOT        V2Can1
097600061003     c     KFV4          reade     FNFV401L                               30
097700061003     C                   if        not *in30 and fv4atb=' '
097800061003     C                   MOVEL     FV4NOT        V2Can2
097900061003     c                   endif
098000061003     c                   endif
098100130214
098200130214     c* Verifico se presente motivo di sbinammento
098300130214     C                   MOVE      'P'           WEPA
098400130214     C                   MOVE      *BLANKS       WLAI
098500130214     C                   MOVE      'C'           WTRC
098600130214     C     KFV4          setll     FNFV401L                               02
098700121105     c*
098800121105     c* Carico punto di scambio
098900121105     c                   clear                   v2cpsc
099000121105     c                   clear                   v2dpsc
099100121105     c                   clear                   v2ctrns
099200121105     c                   clear                   v2dtrns
099300121105     c                   clear                   v2chhs
099400121105     c                   clear                   v2cdgs
099500121105     c                   clear                   v2clnar
099600121105     c                   clear                   v2dlnar2
099700121105     C     kfvs          chain     tNFGs000
099800121105if  1c                   if        (not %found(tNFGs01L)
099900121105     c                              or  FGsatb <> *blanks)
100000121105     C                   clear                   fgspsc
100100121105     c                   setoff                                       08
100200121105e   1C                   endif
100300121105     c
100400121105    1c                   if        fgspsc<>*blanks
100500121105     c                   seton                                        08
100600121105     c                   eval      v2cpsc=fgspsc
100700121105     c     v2cpsc        chain     tnpsc01l
100800121105    2c                   if        %found(tnpsc01l)
100900121105     c                   eval      v2dpsc=pscloc
101000121105     c***                eval      despind=pscind
101100121105     c***                eval      desploc=%trim(psccap) + ' ' +
101200121105     c***                          %trim(pscloc)+'  ' +pscprv+pscnar
101300121105     c
101400121105   x2c                   else
101500121105     c                   eval      v2dpsc=*all'?'
101600121105     c***                eval      despind=*all'?'
101700121105    2c                   endif
101800121105     c
101900121105    2c                   if        fgstrns>0
102000121105     c                   eval      v2ctrns=fgstrns
102100121105     c                   exsr      Ctrtrns
102200121105    2c                   endif
102300121105
102400121105     c                   eval      v2chhs=fgshhs
102500121105     C                   MOVE      FGsdgs        G02INV
102600121105     C                   MOVE      *ZEROS        G02DAT
102700121105     C                   MOVEL     '3'           G02ERR
102800121105     C                   CALL      'XSRDA8'
102900121105     C                   PARM                    WLBDAT
103000121105     C                   Z-ADD     G02DAT        v2cdgs
103100121105     c
103200121105     c* linea finale arrivo autista
103300121105    2c                   if        fgslnar>0
103400121105     c                   move      fgslnar       v2clnar
103500121105     C     fgslnar       CHAIN     AZORG                              32
103600121105     c***                movel     orgdes        deslnar
103700121105     c  n32              movel     orgdes        v2dlnar2
103800121105     c   32              eval      v2dlnar2=*all'?'
103900121105    2c                   endif
104000121105    1c                   endif
104100121105      *
104200061003     c
104300960315     C*
104400960318     C* CARICO LE LINEE DI ARRIVO E LE FILIALI DI SCARICO
104500960319     C                   EXSR      RIES03
104600960319    1C                   ENDIF
104700960315     C*
104800960315     C                   ENDSR
104900121105     C*-----------------------------------------------------***********
105000121105     C* CONTROLLO Traino da cui copiare-uso sql per non sporcarmi i campi
105100121105     C*-----------------------------------------------------***********
105200121105     C     CTRtrns       BEGSR
105300121105     c                   clear                   woktrns           1
105400121105     c                   eval      wrkgetlista =
105500121105     c                             'select tltdde, tltdsc, tlttfp, tlttfa +
105600121105     c                               from tntlt00f where tltatb= '' '' +
105700121105     c                               and tltdde<=' + %editc(fvsdfv:'X') +
105800121105     c                               ' and tlttrn= ' + %editc(v2ctrns:'X')
105900121105
106000121105     C/EXEC SQL
106100121105     C+ PREPARE s1 FROM :wrkgetlista
106200121105     C/END-EXEC
106300121105     C
106400121105     C/EXEC SQL
106500121105     C+ DECLARE A1 CURSOR FOR S1
106600121105     C/END-EXEC
106700121105     C
106800121105     C/EXEC SQL
106900121105     C+ OPEN a1
107000121105     C/END-EXEC
107100121105
107200121105      *?Leggo il file
107300121105     c                   do        *hival
107400121105     C/EXEC SQL
107500121105     C+ FETCH NEXT FROM a1 INTO : Cdde, :Cdsc, :Ctfp, :Ctfa
107600121105     C/END-EXEC
107700121105
107800121105     c                   select
107900121105
108000121105     c                   when      sqlcod = 100
108100121105     c                   leave
108200121105     c                   when      sqlcod < 0
108300121105     c                   seton                                        H1
108400121105     c                   leave
108500121105     c                   other
108600121105     c
108700121105     c                   if        Cdsc>=fvsdfv  and Cdde<=fvsdfv
108800121105     c                   eval      woktrns='S'
108900121105     c* Decodico  partenza e arrivo
109000121105     c                   eval      v2dtrns='Da '+%editc(Ctfp:'X') + ' a '+
109100121105     c                             %editc(Ctfa:'X')
109200121105     c                   endif
109300121105     c
109400121105     c                   endsl
109500121105
109600121105     c                   enddo
109700121105
109800121105     C/EXEC SQL
109900121105     C+ CLOSE a1
110000121105     C/END-EXEC
110100121105
110200121105     c                   ENDSR
110300960315     C*
110400960315     C*--- CONTROLLO SE ESISTE DESCRIZIONE RITARDO -------------------*
110500960315     C     CTRDRA        BEGSR
110600960315     C                   CLEAR                   W001A
110700960315     C*
110800960315     C     KFV4          SETLL     FNFV401L                               32
110900960315    1C     *IN32         IFEQ      *ON
111000960315     C     KFV4          READE     FNFV401L                               32
111100960315    2C     *IN32         DOWEQ     *OFF
111200960315    3C     FV4ATB        IFEQ      ' '
111300960318     C                   MOVEL     '1'           W001A             1
111400960315     C                   SETON                                        32
111500960315   X3C                   ELSE
111600960315     C     KFV4          READE     FNFV401L                               32
111700960315    3C                   ENDIF
111800960315    2C                   ENDDO
111900960315    1C                   ENDIF
112000960315     C*
112100960315     C                   ENDSR
112200960315     C*
112300960318     C*--- RIEMPIO SFL DELLE LINEE -----------------------------------*
112400960319     C     RIES03        BEGSR
112500000504     C*
112600000504     C     KFWS          CHAIN     TNFWS01L                           39
112700000504     C     *IN39         IFEQ      *ON
112800000504     C     FWSATB        ORNE      ' '
112900000504     C                   CLEAR                   FWSFF3
113000000504     C                   CLEAR                   FWSFF4
113100000504     C                   CLEAR                   FWSFL3
113200000504     C                   CLEAR                   FWSFL4
113300000504     C                   ENDIF
113400000504     C*
113500960318     C                   Z-ADD     1             NRR3              5 0
113600960318     C                   Z-ADD     1             REC3
113700960320     C                   CLEAR                   SAVFLP
113800960320     C                   CLEAR                   TERVOL
113900960320     C                   CLEAR                   V2CVOL
114000960320     C                   CLEAR                   WVOLTA
114100960319     C*
114200960319     C****  PULIZIA SUBFILE  ****
114300960319     C                   SETON                                        21
114400960318     C                   WRITE     TL37C03
114500960319     C                   SETOFF                                       21
114600960318     C*
114700960319     C****  CARICAMENTO SUBFILE  ****
114800960318     C                   Z-ADD     1             I                 3 0
114900000504     C     I             DOWLE     300
115000990825    1C     FFV(I)        ANDNE     *BLANKS
115100960318     C*
115200960318     C* FILIALI DI ARRIVO
115300960320     C*
115400960320     C* SE MODIFICA LA FILIALE DI SCARICO, EMETTO TOTALE MC
115500960320    2C     FLP(I)        IFNE      SAVFLP
115600960320    3C     SAVFLP        IFNE      *BLANKS
115700960320     C*
115800960320     C                   CLEAR                   VSCFFV
115900960320     C                   CLEAR                   VSDFFV
116000960320     C                   CLEAR                   TIPFFV
116100960320     C                   CLEAR                   TIPFLP
116200960320     C                   MOVEL     SAVFLP        VSCFLP
116300960320     C                   MOVEL     CTOT          VSDFLP
116400960320     C                   MOVEL     W008A         VSDFLP
116500960320     C                   Z-ADD     TERVOL        VSCVOL
116600960320     C                   CLEAR                   TERVOL
116700960320     C                   SETON                                        03
116800960320     C*
116900960320     C                   WRITE     TL37S03
117000960320     C                   SETOFF                                       03
117100960320     C                   ADD       1             NRR3
117200960320     C                   MOVEL     '1'           WVOLTA            1
117300960320    3C                   ENDIF
117400960320     C*
117500960320     C                   MOVEL     FLP(I)        SAVFLP            3
117600960320    2C                   ENDIF
117700960320     C*
117800960318     C                   MOVE      FFV(I)        VSCFFV
117900960318     C                   MOVE      VSCFFV        W0030             3 0
118000960318     C     W0030         CHAIN     AZORG01L                           32
118100960320    2C     *IN32         IFEQ      *OFF
118200960318     C                   MOVEL     ORGDES        VSDFFV
118300960318     C                   MOVEL     ORGFAG        TIPFFV
118400960320   X2C                   ELSE
118500960318     C                   CLEAR                   VSDFFV
118600960318     C                   CLEAR                   TIPFFV
118700960320    2C                   ENDIF
118800960318     C*
118900960318     C* FILIALI DI SCARICO
119000960320    2C     FLP(I)        IFGT      *ZEROS
119100960318     C                   MOVE      FLP(I)        VSCFLP
119200960318     C                   MOVE      VSCFLP        W0030
119300960318     C     W0030         CHAIN     AZORG01L                           32
119400960318    3C     *IN32         IFEQ      *OFF
119500960318     C                   MOVEL     ORGDES        VSDFLP
119600960320     C                   MOVEL     ORGDES        W008A             8
119700960318     C                   MOVEL     ORGFAG        TIPFLP
119800960318   X3C                   ELSE
119900960318     C                   CLEAR                   VSDFLP
120000960320     C                   CLEAR                   W008A
120100960318     C                   CLEAR                   TIPFLP
120200960318    3C                   ENDIF
120300960318     C*
120400960318   X2C                   ELSE
120500960318     C                   MOVE      *BLANKS       VSCFLP
120600960318     C                   MOVE      *BLANKS       VSDFLP
120700960318     C                   MOVE      *BLANKS       TIPFLP
120800960318    2C                   ENDIF
120900960318     C*
121000960319     C* METRICUBI REALI (SOLO DELL'AREA)
121100960319     C                   MOVEL     FFV(I)        WLNA
121200960319     C     KFVD          CHAIN     FNFVD01L                           34
121300960319    2C     *IN34         IFEQ      *OFF
121400960319     C                   Z-ADD(H)  FVDVL1        VSCVOL
121500960319     C                   ADD(H)    FVDVL2        VSCVOL
121600960319     C                   ADD(H)    FVDVL3        VSCVOL
121700960319     C                   ADD(H)    FVDVL4        VSCVOL
121800960319     C                   ADD(H)    FVDVL5        VSCVOL
121900960319     C                   ADD(H)    FVDVL6        VSCVOL
122000960319   X2C                   ELSE
122100960319     C                   CLEAR                   VSCVOL
122200960319    2C                   ENDIF
122300960320     C*
122400960320     C* SOMMO AL TOTALE DEL FV
122500960320     C                   ADD(H)    VSCVOL        V2CVOL
122600960320     C* SOMMO AL TOTALE DEL TERMINAL
122700960320     C                   ADD(H)    VSCVOL        TERVOL
122800960319     C*
122900960318     C                   WRITE     TL37S03
123000960318     C                   ADD       1             NRR3
123100960320     C                   ADD       1             I
123200960318    1C                   ENDDO
123300960320     C*
123400960320     C* ULTIMA FILIALE DI SCARICO SE NE HO TROVATA PIU' DI UNA
123500960320     C     SAVFLP        IFNE      *BLANKS
123600960320     C     WVOLTA        ANDEQ     '1'
123700960320     C*
123800960320     C                   CLEAR                   VSCFFV
123900960320     C                   CLEAR                   VSDFFV
124000960320     C                   CLEAR                   TIPFFV
124100960320     C                   CLEAR                   TIPFLP
124200960320     C                   MOVEL     SAVFLP        VSCFLP
124300960320     C                   MOVEL     CTOT          VSDFLP
124400960320     C                   MOVEL     W008A         VSDFLP
124500960320     C                   Z-ADD     TERVOL        VSCVOL
124600960320     C                   CLEAR                   TERVOL
124700960320     C*
124800960320     C                   SETON                                        03
124900960320     C                   WRITE     TL37S03
125000960320     C                   ADD       1             NRR3
125100960320     C                   SETOFF                                       03
125200960320     C                   ENDIF
125300960318     C*
125400960318     C                   ENDSR
125500960319     C*
125600960315     C*--- INTERROGO GLI ORARI DI ARRIVO -----------------------------*
125700960315     C     INTORA        BEGSR
125800960319     C*
125900960319     C* SOLO SE CAMBIA IL FOGLIO VIAGGIO RICARICO IL SUBFILE
126000960319    1C     FVSNFV        IFNE      SAVNFV
126100960315     C                   MOVEL     FVSNFV        SAVNFV
126200960315     C*
126300960315     C                   Z-ADD     0             NRR5              5 0
126400960321     C                   SETOFF                                       17
126500960321     C*
126600960321     C****  PULIZIA SFL  ****
126700960319     C                   SETON                                        24
126800960315     C                   WRITE     TL37C05
126900960319     C                   SETOFF                                       24
127000960321     C*
127100960321     C****  P A R T E N Z A   REALE  *****
127200960321     C                   EXSR      RI2S05
127300960315     C*
127400960321     C****  A R R I V O       REALE  *****
127500960321     C                   MOVEL     'A'           WEPA
127600960321     C     KFV2          SETLL     FNFV203L
127700960321     C     KFV2          READE     FNFV203L                               32
127800960321    2C     *IN32         DOWEQ     *OFF
127900960319     C     FV2ATB        ANDEQ     ' '
128000960319     C                   EXSR      RIES05
128100960319     C*
128200960321     C     KFV2          READE     FNFV203L                               32
128300960321    2C                   ENDDO
128400960319     C*
128500960319    1C                   ENDIF
128600960315     C*
128700960321     C****  EMISSIONE FORMATO  ****
128800960315     C     FOR05         TAG
128900960315     C                   WRITE     TL37T01
129000960315     C                   WRITE     TL37Z05
129100960318     C                   EXFMT     TL37C05
129200960315     C*
129300960315     C* F12 - RITORNO
129400960319    1C     *INKL         IFEQ      *OFF
129500960315     C                   CLEAR                   W001A
129600960321     C*
129700960315     C                   READC     TL37S05                                32
129800960319    2C     *IN32         DOWEQ     *OFF
129900960319    3C     V5CVIS        IFEQ      'V'
130000960315     C                   CLEAR                   V5CVIS
130100960315     C                   SETON                                        17
130200960315     C                   UPDATE    TL37S05
130300960315     C*
130400960315     C                   CLEAR                   DSLV15
130500960321    4C     V5CPAR        IFEQ      'PARTENZA'
130600960321     C                   MOVEL     'P'           D15EPA
130700960321   X4C                   ELSE
130800960315     C                   MOVEL     'A'           D15EPA
130900960321     C                   MOVEL     V5CFIL        D15LAI
131000960321    4C                   ENDIF
131100960321     C*
131200960321     C                   MOVEL     'I'           D15TUP
131300960315     C                   MOVEL     V5CRTC        D15RTC
131400960315     C                   MOVEL     FVSLNP        D15LNP
131500960315     C                   MOVEL     FVSNFV        D15NFV
131600960321     C                   MOVEL     'R'           D15TRC
131700960321     C                   MOVEL     FVSTRN        D15TRN
131800960321     C                   MOVEL     FVSTTR        D15TTR
131900960315     C                   MOVEL     FVSDFV        D15DFV
132000960315     C*
132100960315     C                   MOVEL     DSLV15        KPJBU
132200960315     C                   CALL      'FNLV15R'
132300960315     C                   PARM                    KPJBA
132400960321     C                   MOVEL     '1'           WLV15             1
132500960321     C*
132600960315     C                   MOVEL     '1'           W001A
132700960319    3C                   ENDIF
132800960315     C*
132900960315     C                   READC     TL37S05                                32
133000960319    2C                   ENDDO
133100960315     C*
133200960319    2C     W001A         IFEQ      '1'
133300960315     C                   GOTO      FOR05
133400960319    2C                   ENDIF
133500960319    1C                   ENDIF
133600960315     C*
133700960315     C                   ENDSR
133800960318     C*
133900960321     C*--- CARICAMENTO SFL DEGLI ORARI DI PARTENZA/ARRIVO ------------*
134000960319     C     RIES05        BEGSR
134100960321     C                   SETOFF                                       17
134200960321     C*
134300960321     C****  A R R I V O       REALE  *****
134400960321     C                   CLEAR                   V5DRT1
134500960321     C                   CLEAR                   V5DRT2
134600960321     C* ARRIVO
134700960321     C                   MOVEL     'ARRIVO  '    V5CPAR
134800960319     C* FILIALE DI INVIO
134900960319     C                   MOVEL     FV2LAI        V5CFIL
135000960319     C     FV2LAI        CHAIN     AZORG                              34
135100960319     C  N34              MOVEL     ORGDES        V5DFIL
135200960319     C   34              MOVEL     *BLANKS       V5DFIL
135300960319     C* DATA E ORA REALE
135400960319     C                   Z-ADD     FV2DPA        WDATA
135500960319     C                   Z-ADD     MM            GGMM
135600960319     C                   MOVEL     GG            GGMM
135700960319     C                   Z-ADD     AA            V5CDTR
135800960319     C                   MOVEL     GGMM          V5CDTR
135900960315     C                   MOVEL     FV2HPA        V5CHMR
136000960319     C* CAUSALE RITARDO
136100960319     C                   MOVEL     FV2RTC        V5CRTC
136200960319    1C     FV2RTC        IFNE      *BLANKS
136300960315     C                   MOVEL     'TC'          COD
136400960315     C                   MOVEL(P)  FV2RTC        KEY
136500960318     C     KTAB          CHAIN     TABEL                              34
136600960321    2C     *IN34         IFEQ      *OFF
136700960321     C                   MOVEL     TBLUNI        DSTC
136800960321     C                   MOVEL     §TCDES        V5DRT1
136900960321     C                   MOVE      §TCDES        V5DRT2
137000960321    2C                   ENDIF
137100960319     C*
137200960315     C* VERIFICO SE C'E' ANCHE LA DESCRIZIONE
137300960318     C                   MOVEL     'A'           WEPA
137400960318     C                   Z-ADD     FV2LAI        WLAI
137500960318     C                   MOVEL     'R'           WTRC
137600960315     C                   EXSR      CTRDRA
137700960319    2C     W001A         IFNE      ' '
137800960315     C                   SETON                                        17
137900960319    2C                   ENDIF
138000960319    1C                   ENDIF
138100960319     C*
138200960319     C                   ADD       1             NRR5
138300960319     C                   WRITE     TL37S05
138400960315     C                   ENDSR
138500960318     C*
138600960321     C*--- CARICAMENTO SFL DEGLI ORARI DI PARTENZA/ARRIVO ------------*
138700960321     C     RI2S05        BEGSR
138800960321     C                   SETOFF                                       17
138900960321     C*
139000960321     C****  P A R T E N Z A   REALE  *****
139100960321     C                   CLEAR                   V5DRT1
139200960321     C                   CLEAR                   V5DRT2
139300960321     C* PARTENZA
139400960321     C                   MOVEL     'PARTENZA'    V5CPAR
139500960321     C* FILIALE PARTENZA
139600960321     C                   MOVEL     FVSLNP        V5CFIL
139700960321     C     FVSLNP        CHAIN     AZORG                              34
139800960321     C  N34              MOVEL     ORGDES        V5DFIL
139900960321     C   34              MOVEL     *BLANKS       V5DFIL
140000960321     C* DATA E ORA
140100960321     C                   Z-ADD     FVSDRP        WDATA
140200960321     C                   Z-ADD     MM            GGMM
140300960321     C                   MOVEL     GG            GGMM
140400960321     C                   Z-ADD     AA            V5CDTR
140500960321     C                   MOVEL     GGMM          V5CDTR
140600960321     C                   MOVEL     FVSHRP        V5CHMR
140700960321     C* CAUSALE RITARDO
140800960321     C                   MOVEL     FVSRTP        V5CRTC
140900960321    1C     FVSRTP        IFNE      ' '
141000960321     C                   MOVEL     'TC'          COD
141100960321     C                   MOVEL(P)  FVSRTP        KEY
141200960321     C     KTAB          CHAIN     TABEL                              32
141300960321    2C     *IN32         IFEQ      *OFF
141400960321     C                   MOVEL     TBLUNI        DSTC
141500960321     C                   MOVEL     §TCDES        V5DRT1
141600960321     C                   MOVE      §TCDES        V5DRT2
141700960321    2C                   ENDIF
141800960321     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO: SE ESISTE
141900960321     C*   EVIDENZIO IL COMANDO
142000960321     C                   MOVEL     'P'           WEPA
142100960321     C                   MOVEL     'R'           WTRC
142200960321     C                   CLEAR                   WLAI
142300960321     C                   EXSR      CTRDRA
142400960321    2C     W001A         IFNE      ' '
142500960321     C                   SETON                                        17
142600960321    2C                   ENDIF
142700960321    1C                   ENDIF
142800960321     C*
142900960321     C                   ADD       1             NRR5
143000960321     C                   WRITE     TL37S05
143100960321     C                   ENDSR
143200130214     c*
143300130214     C*--- INTERROGa motivi sbinamento  -----------------------------*
143400130214     C     INTMot        BEGSR
143500130214     C*
143600130214     C                   Z-ADD     0             NRR6              5 0
143700130214     C                   SETOFF                                       17
143800130214     C*
143900130214     C****  PULIZIA SFL  ****
144000130214     C                   SETON                                        24
144100130214     C                   WRITE     TL37C06
144200130214     C                   SETOFF                                       24
144300130214     C*
144400130214     C                   MOVE      'P'           WEPA
144500130214     C                   MOVE      *BLANKS       WLAI
144600130214     C                   MOVE      'C'           WTRC
144700130214     c                   z-add     1             conta             2 0
144800130214     c                   clear                   v6dnot
144900130214     C     KFV4          SETLL     FNFV401L
145000130214     C     KFV4          READE     FNFV401L                               32
145100130214    2C     *IN32         DOWEQ     *OFF
145200130214     C     FV4ATB        ANDEQ     ' '
145300130214     C*
145400130214     c                   if        conta=1
145500130214     c                   eval      savnot=fv4not
145600130214     c                   eval      conta=conta+1
145700130214     c                   else
145800130214     c
145900130214     c                   eval      v6dnot=savnot+fv4not
146000130214     C                   ADD       1             NRR6
146100130214     C                   WRITE     TL37S06
146200130214     c                   clear                   savnot
146300130214     c                   clear                   v6dnot
146400130214     c                   eval      conta=1
146500130214     c                   endif
146600130214     C*
146700130214     C     KFV4          READE     FNFV401L                               32
146800130214    2C                   ENDDO
146900130214     C*
147000130214     c                   if        savnot<>*blanks
147100130214     c                   clear                   v6dnot
147200130214     c                   eval      v6dnot=savnot
147300130214     C                   ADD       1             NRR6
147400130214     C                   WRITE     TL37S06
147500130214     c                   endif
147600130214     C*
147700130214     C****  EMISSIONE FORMATO  ****
147800130214     C     FOR06         TAG
147900130214     C                   WRITE     TL37T01
148000130214     C                   WRITE     TL37Z05
148100130214     C                   EXFMT     TL37C06
148200130214     C*
148300130214     C* F12 - RITORNO
148400130214    1C     *INKL         IFEQ      *OFF
148500130214     C                   GOTO      FOR06
148600130214    1C                   ENDIF
148700130214     C*
148800130214     C                   ENDSR
148900130214     C*
149000960321     C*
149100960226     C*--- SR INIZIALE -----------------------------------------------*
149200951110     C     *INZSR        BEGSR
149300960307     C****
149400960307     C** KLIST
149500960307     C****
149600960315     C* ACCESSO TNFVS01L
149700960315     C     KFVS          KLIST
149800960315     C                   KFLD                    V1CLNP
149900960315     C                   KFLD                    V1CNFV
150000960315     C* ACCESSO TNFVS03L
150100960315     C     KFVS2         KLIST
150200960315     C                   KFLD                    V1CLNP
150300960315     C                   KFLD                    WDFD
150400960315     C     KFVS3         KLIST
150500960320     C                   KFLD                    WFIL
150600960318     C                   KFLD                    WDFD
150700960321     C* ACCESSO FNFV203L
150800960321     C     KFV2          KLIST
150900960318     C                   KFLD                    FVSLNP                                     FOGLIO
151000960318     C                   KFLD                    FVSNFV
151100960318     C                   KFLD                    WEPA
151200960318     C                   KFLD                    WTDH
151300960315     C* ACCESSO FNFV401L
151400960315     C     KFV4          KLIST
151500960315     C                   KFLD                    FVSLNP                                     FOGLIO
151600960315     C                   KFLD                    FVSNFV
151700960315     C                   KFLD                    WEPA
151800960315     C                   KFLD                    WLAI                                       NE
151900960315     C                   KFLD                    WTRC
152000000504     C     KFWS          KLIST
152100000504     C                   KFLD                    FVSLNP                                     FOGLIO
152200000504     C                   KFLD                    FVSNFV
152300960319     C* ACCESSO FNFVD01L
152400960319     C     KFVD          KLIST
152500960319     C                   KFLD                    FVSLNP
152600960319     C                   KFLD                    FVSNFV
152700960319     C                   KFLD                    WLNA
152800960319     C                   KFLD                    WTRC2
152900951110     C* ACCESSO TABEL00F
153000960315     C     KTAB          KLIST
153100951110     C                   KFLD                    CODUT
153200951110     C                   KFLD                    COD
153300960315     C                   KFLD                    KEY
153400951110     C*
153500960228     C****
153600960228     C** DEFINIZIONE CAMPI
153700960228     C****
153800960229     C     *LIKE         DEFINE    TBLCOD        COD
153900960315     C     *LIKE         DEFINE    TBLKEY        KEY
154000960315     C     *LIKE         DEFINE    FVSLNP        WFIL
154100960318     C     *LIKE         DEFINE    FVSNFV        SAVNFV
154200960318     C     *LIKE         DEFINE    D37DFD        WDFD
154300960318     C     *LIKE         DEFINE    D37DFA        WDFA
154400960318     C     *LIKE         DEFINE    FV2EPA        WEPA
154500960318     C     *LIKE         DEFINE    FV2LAI        WLAI
154600960318     C     *LIKE         DEFINE    FV2TDH        WTDH
154700960318     C     *LIKE         DEFINE    FV4TRC        WTRC
154800960319     C     *LIKE         DEFINE    FVDLNA        WLNA
154900960319     C     *LIKE         DEFINE    FVDTRC        WTRC2
155000960320     C     *LIKE         DEFINE    VSCVOL        TERVOL
155100960307     C*
155200960315     C****  IMPOSTAZIONE CAMPI FISSI  ****
155300960315     C                   MOVEL     'TNTL37R'     V1CPGM
155400960315     C                   MOVE      TES(1)        V1CTES
155500960319     C                   MOVEL     '1'           WTRC2
155600960319     C                   MOVEL     'R'           WTDH
155700960315     C*
155800960229     C                   Z-ADD     1             CODUT
155900960229     C                   CALL      'X§PARUT'
156000960229     C                   PARM                    UT§DSE
156100960315     C                   MOVEL     RAGUT         V1CRSU
156200960318     C*
156300960318     C****  PRENDO DATA E ORA CORRENTE  ****
156400960318     C                   TIME                    W0140            14 0
156500960318     C                   MOVE      W0140         UDATE8            8 0
156600960228     C*
156700960229     C                   ENDSR
156800151102      /free
156900151102       begsr sr_comp;
157000151102             if  wtrt <> *blank;
157100151102             xi=1  ;
157200151102               for  xi = 1  to  %len( %trimr( wtrt ) );
157300151102                 if  %subst( wtrt: xi : 1 ) = *blank;
157400151102                   wtrt= %subst( wtrt: 1 : xi - 1 ) +
157500151102                            %subst( wtrt: xi + 1 );
157600151102                   xi -= 1;
157700151102                 endif;
157800151102               endfor;
157900151102             endif;
158000151102       endsr ;
158100151102      /end-free
158200960315     C*--------------------------------------------------------------*
158300960315**  MSG
158400971203Linea di partenza errata                                                      1
158500971203La linea di partenza immessa non e' un terminal di partenza                   2
158600151027Occorre immettere il numero f.viaggio o la data f.viaggio oppure la targa     3
158700960315Immettere o il numero f.viaggio o la data f.viaggio                           4
158800960315Se immesso il numero foglio viaggio occorre inserire la linea di partenza     5
158900960315Numero foglio viaggio inesistente                                             6
159000960315Data errata                                                                   7
159100960315La data foglio DAL non puo' essere maggiore della data foglio AL              8
159200960315Non esistono dati da visualizzare per la selezione effettuata !!              9
159300960315                                                                              10
159400960315**  TES
159500960315 * INTERROG.FOGLI VIAGGIO PARTENZE *
