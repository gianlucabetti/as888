000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200950530     H* FNLRF1R *-----------------------------------------------------*
000300950530     H*              STAMPA FOGLI DISTINTE CONSEGNA     - FILTRO -    *
000400930225     H*---------------------------------------------------------------*
000500950530     FFNLRF1D   CF   E             WORKSTN
000600950530     FFNFVV01L  IF   E           K DISK
000700950530     FFNFVV02L  IF   E           K DISK
000800950530     F                                     RENAME(FNFVV000:FNFVV002)
000900110520     FFiDST01L  IF   E           K DISK
001000021203     Ffiapd01L  IF   E           K DISK
001100950524     D*
001200950524     D* DEFINIZIONE SCHIERE
001300950525     D TES             S             34    DIM(1) CTDATA PERRCD(1)              TESTATE
001400950531     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             MESSAGGI ERRORE
001500950530     D*
001600950530     D* RICHIAMO PGM DI ELABORAZIONE                  - FNLRF2R -
001700950530     D PARAM           DS
001800950530     D* FILIALE GESTIONE
001900950530     D  PARFGS                 1      3  0
002000950530     D* DATA FOGLIO DISTINTA IN GG/MM/AAAA
002100950530     D  PARDFV                 4     11  0
002200950530     D* DATA FOGLIO DISTINTA IN AAAA/MM/GG
002300950530     D  PARDFR                12     19  0
002400950530     D* NUMERO FOGLIO DISTINTA DAL
002500950530     D  PARNFD                20     24  0
002600950530     D* NUMERO FOGLIO DISTINTA  AL
002700950530     D  PARNFA                25     29  0
002800950530     D* CODICE PADRONCINO
002900950530     D  PARPDR                30     36  0
003000950530     D* PRESTAZIONE PADRONCINO
003100950530     D  PARFPP                37     37
003200950530     D* TIPO STAMPA
003300950531     D  PARTST                38     38
003400950612     D* NUMERO COPIE
003500950612     D  PARNCP                39     41  0
003600950913     D* STAMPA DEL NUMERO BOLLE SU EITCHETTE
003700950913     D  PARSNB                42     42
003800950607     D* STAMPANTE E MODULO ETICHETTE
003900950607     D  PARPRE               101    110
004000950607     D  PARMDE               111    120
004100950908     D* NOME PGM PER STAMPA ETICHETTE DISTINTA
004200950908     D  PARPSD               121    130
004300950523     D*
004400950523     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
004500950523     D DSLV50        E DS                  EXTNAME(FNLV50DS)
004600021203     D* DS PER FNLV24R - ricerca autotrasportatore
004700021203     D fnlv24ds      E DS
004800950523     D* DS PER FNLR02R - INTERROGAZIONE FOGLI VARI
004900950523     D DSLR01        E DS
005000950607      *
005100030724     D* DS PER TRUL90R - RICHIESTA STAMPANTI
005200030724     D trul90ds      E DS
005300950524     D*
005400950524     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
005500950524     D WLBDAT          DS                  INZ
005600950524     D  G02DAT                 1      8  0
005700950524     D  G02INV                 9     16  0
005800950524     D  G02ERR                17     17
005900950524     D  G02TGI                18     22  0
006000950523     D*
006100900514     D KPJBA         E DS
006200020506     D*                                       1   3 REM
006300020506     D*                                       4   6 REMFIL
006400950523     D CNCR80        E DS
006500000619     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006600950523     C*****************************************************************
006700950523     C* RIEPILOGO INDICATORI
006800950523     C*****************************************************************
006900950530     C* 01    - E' STATA IMMESSA LA DATA FOGLIO DISTINTA
007000950530     C* 03    - E' STATO IMMESSO IL NUMERO FOGLIO DISTINTA AL
007100950523     C* 04    - CAMBIO FILIALE IN GESTIONE PER CMD18
007200950523     C* 05    - SONO UN "REMXXX"
007300950523     C* 28    - INDICATORE DI EMISSIONE CAMPO MESSAGGI
007400950525     C* 30/32 - DI COMODO
007500950530     C* 35    - USATE PER TESTN
007600950530     C* 40/45 - ERRORI
007700950524     C* 90    - RIEMETTE IL FORMATO SENZA CHE CI SIANO STATI ERRORI
007800950523     C*****************************************************************
007900950607     C* SE SONO IN MANUTENZIONE RICHIESDO LE STAMPANTI
008000030724     C                   CLEAR                   trul90ds
008100950607     C*
008200030724     C                   MOVEL     'S'           D90RSE
008300030724     C                   CALL      'TRUL90R'
008400950607     C                   PARM                    KPJBA
008500030725     C                   PARM                    trul90ds
008600950607     C**
008700950607     C* F3 - FINE
008800030724     C     D90F3         IFEQ      '1'
008900950607     C                   GOTO      FINE
009000950607     C                   ENDIF
009100950530     C*
009200950530     C* PULIZIA CAMPI DEL VIDEO
009300950530     C                   MOVEL     UDATE8        V1CDFV
009400131029     C**********         CLEAR                   V1CTPD
009500950530     C                   CLEAR                   V1CNFD
009600950530     C                   CLEAR                   V1DNFD
009700950530     C                   CLEAR                   V1CNFA
009800950530     C                   CLEAR                   V1CPDR
009900950530     C                   CLEAR                   V1DPDR
010000950530     C                   CLEAR                   V1CFPP
010100950612     C                   Z-ADD     001           V1CNCP
010200950609     C                   MOVEL     'L'           V1CTST
010300030623     C                   MOVEL     'N'           V1CSNB
010400950523     C*
010500950524     C* EMISSIONE FORMATO
010600950523     C     FOR01         TAG
010700950530     C                   EXFMT     LRF1D01
010800950523     C*
010900950523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
011000950523     C                   CLEAR                   V1CMSG
011100950523     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
011200950530     C                   SETOFF                                       284041
011300950530     C                   SETOFF                                       4243
011400950530     C                   SETOFF                                       4445
011500950523     C*
011600950523     C** CMD3  - FINE LAVORO
011700950530     C   KC              GOTO      FINE
011800950523     C*
011900950523     C** CMD18 - CAMBIO FILIALE
012000950526    1C     *INKS         IFEQ      *ON
012100950526     C                   SETON                                        04
012200950526     C                   GOTO      FOR01
012300950526    1C                   ENDIF
012400950523     C*
012500950523     C* 04 ON  - CONTROLLO FILIALE IN GESTIONE
012600950524    1C     *IN04         IFEQ      *ON
012700950523     C                   EXSR      CTRFGS
012800950524     C   28              GOTO      FOR01
012900950524    1C                   ENDIF
013000950523     C*
013100950523     C* CONTROLLI VIDEATA
013200950523     C                   EXSR      CTRD01
013300950530     C  NKF
013400950530     COR 28              GOTO      FOR01
013500950524     C*
013600950530     C***
013700950530     C*  CMD6  - ELABORAZIONE
013800950530     C***
013900950530     C                   CLEAR                   PARAM
014000030724     C                   MOVEL     D90MDE        PARMDE
014100030724     C                   MOVEL     D90PRE        PARPRE
014200950908     C* NOME PGM PER STAMPA ETICHETTE DISTINTA
014300030724     C                   MOVEL     D90PSD        PARPSD
014400950530     C* FILIALE GESTIONE
014500950530     C                   MOVEL     V1CFGS        PARFGS
014600950530     C* DATA FOGLIO DISTINTA
014700950530     C     *IN01         IFEQ      *ON
014800950530     C                   MOVEL     V1CDFV        PARDFV
014900950530     C                   MOVEL     WDFV          PARDFR
015000950530     C                   ENDIF
015100950530     C* NUMERO FOGLIO DISTINTA
015200950530     C     V1CNFD        IFNE      *BLANKS
015300950530     C     V1CNFD        ANDNE     *ZEROS
015400950530     C                   MOVEL     V1CNFD        PARNFD
015500950530     C     V1CNFA        IFEQ      0
015600950530     C                   MOVEL     V1CNFD        PARNFA
015700950530     C                   ELSE
015800950530     C                   MOVEL     V1CNFA        PARNFA
015900950530     C                   ENDIF
016000950530     C                   ENDIF
016100950530     C* CODICE PADRONCINO
016200950530     C     V1CPDR        IFNE      *BLANKS
016300950530     C     V1CPDR        ANDNE     *ZEROS
016400950530     C                   MOVEL     V1CPDR        PARPDR
016500950530     C                   ENDIF
016600950530     C* PRESTAZIONE PADRONCINO
016700950530     C                   MOVEL     V1CFPP        PARFPP
016800000619     C* TIPO PADRONCINO
016900131029     C*************      MOVEL     V1CTPD        PARTPD
017000950530     C* TIPO STAMPA
017100950530     C                   MOVEL     V1CTST        PARTST
017200950612     C                   MOVEL     V1CNCP        PARNCP
017300950913     C                   MOVEL     V1CSNB        PARSNB
017400950530     C*
017500950530     C                   MOVEL     PARAM         KPJBU
017600950530     C                   MOVEL     'LRF2'        KCOAZ
017700950530     C                   CALL      'BCH10'
017800950530     C                   PARM                    KPJBA
017900950530     C*
018000950530     C     FINE          TAG
018100950530     C                   SETON                                        LR
018200950523     C*
018300950530     C*--- INZSR INIZIALE --------------------------------------------*
018400950530     C     *INZSR        BEGSR
018500950530     C*
018600950530     C     *ENTRY        PLIST
018700950530     C                   PARM                    KPJBA
018800950530     C                   Z-ADD     1             CODUT
018900950530     C                   CALL      'X§PARUT'
019000950530     C                   PARM                    UT§DSE
019100950530     C                   MOVEL     RAGUT         V1CRSU
019200950530     C                   MOVEL     REC80         CNCR80
019300950530     C*
019400950530     C***
019500950531     C* LA DATA DEL GIORNO LA PRENDO DA TIME
019600950530     C***
019700950530     C                   TIME                    W0140            14 0
019800950530     C                   MOVE      W0140         UDATE8            8 0
019900950530     C*
020000950530     C***
020100950530     C* VEDO SE SONO SIMFEL O REMOTO
020200950530     C***
020300020506    1C     SIMTPP        IFEQ      '2'
020400020506     C     SIMTPP        OREQ      *BLANKS
020500020506     C                   MOVEL     SIMPOU        V1CFGS
020600950530     C                   SETON                                        05
020700950530   X1C                   ELSE
020800950530     C                   MOVEL     SIMFEL        V1CFGS
020900950530    1C                   ENDIF
021000950530     C*
021100950530     C                   MOVE      TES(1)        V1CTES
021200950530     C                   MOVEL     'FNLRF1R'     V1CPGM
021300950530     C                   Z-ADD     4             WNPG
021400950530     C*
021500950530     C***
021600950530     C* DEFINIZIONE CAMPI
021700950530     C***
021800950530     C     *LIKE         DEFINE    DSTPDR        WPDR
021900950530     C     *LIKE         DEFINE    FVVNFV        WNFD
022000950530     C     *LIKE         DEFINE    FVVDFV        WDFV
022100950530     C     *LIKE         DEFINE    FVVNPG        WNPG
022200950530     C*
022300950530     C* ACCESSO FNFVV01L
022400950530     C     KFVV2         KLIST
022500950530     C                   KFLD                    WNPG
022600950530     C                   KFLD                    WNFD
022700950530     C                   KFLD                    V1CFGS
022800950530     C* ACCESSO FNFVV02L
022900950530     C     KFVV          KLIST
023000950530     C                   KFLD                    WNPG
023100950530     C                   KFLD                    WDFV
023200950530     C                   KFLD                    V1CFGS
023300110520     C* ACCESSO FiDST01L
023400950530     C     KDST          KLIST
023500110520     C                   KFLD                    Wnpg
023600110520     C                   KFLD                    dstnfv
023700950530     C                   KFLD                    V1CFGS
023800110520     c                   eval      dstnpg=4
023900021203     C* ACCESSO Fiapd01L
024000021203     C     Kapdw         KLIST
024100021203     C                   KFLD                    apdtip
024200021203     C                   KFLD                    wpdr
024300021203     c                   movel     'A'           apdtip
024400950530     C                   ENDSR
024500950530     C*
024600950523     C*--- CONTROLLO LA FILIALE IN GESTIONE --------------------------*
024700950523     C     CTRFGS        BEGSR
024800950523     C                   CLEAR                   DSLV50
024900950523     C                   MOVEL     KNMUS         D50PRU
025000950530     C                   MOVEL     V1CFGS        D50FGS
025100950523     C                   CALL      'FNLV50R'
025200950523     C                   PARM                    DSLV50
025300950523     C*
025400950524    1C     D50ERR        IFNE      ' '
025500950523     C                   MOVEL     D50MSG        V1CMSG
025600950530     C                   SETON                                        4528
025700950523     C                   GOTO      ENDCTF
025800950524    1C                   ENDIF
025900950523     C*
026000950523     C     ENDCTF        ENDSR
026100950523     C*
026200950523     C*--- CONTROLLO DATI VIDEATA ------------------------------------*
026300950523     C     CTRD01        BEGSR
026400950530     C                   SETOFF                                       010390
026500950531     C                   SETOFF                                       07
026600950530     C                   CLEAR                   V1DNFD
026700950530     C                   CLEAR                   V1DPDR
026800950523     C*
026900950530     C****  EFFETTUARE ALMENO UNA SELEZIONE  ****
027000950530    1C     V1CDFV        IFEQ      0
027100950530     C     V1CNFD        IFEQ      *BLANKS
027200950530     C     V1CNFD        OREQ      *ZEROS
027300950530     C     V1CPDR        IFEQ      *BLANKS
027400950530     C     V1CPDR        OREQ      *ZEROS
027500950530     C                   MOVEL     MSG(1)        V1CMSG
027600950530     C                   SETON                                        4028
027700950530     C                   GOTO      ENDCT1
027800950530     C                   ENDIF
027900950530     C                   ENDIF
028000950530    1C                   ENDIF
028100950530     C*
028200950530     C****  DATA FOGLIO DISTINTA  ****
028300950530    1C     V1CDFV        IFGT      0
028400950530     C* GIRO DATA DISTINTA
028500950530     C                   MOVE      V1CDFV        G02DAT
028600950530     C                   MOVEL     *BLANKS       G02ERR
028700950530     C                   CALL      'XSRDA8'
028800950530     C                   PARM                    WLBDAT
028900950530     C                   Z-ADD     G02INV        WDFV
029000950530     C                   Z-ADD     G02DAT        V1CDFV
029100950530     C*
029200950530     C* CONTROLLO SE ESISTONO FOGLI DISTINTE IN QUELLA DATA
029300950531     C     KFVV          SETLL     FNFVV02L
029400950531     C     KFVV          READE     FNFVV02L                               30
029500950531     C*
029600950531    2C     *IN30         IFEQ      *ON
029700950531     C                   MOVEL     MSG(2)        V1CMSG
029800950531     C                   SETON                                        4028
029900950531     C                   GOTO      ENDCT1
030000950531    2C                   ENDIF
030100950531     C*
030200950531    2C     *IN30         DOWEQ     *OFF
030300110520     C* CONTROLLO SE ESISTE IN FIDST00F
030400110523     C                   z-add     FVVNFV        dstnfv
030500110520     C     KDST          SETLL     FIDST01L                               31
030600950531     C   31              SETON                                        0730
030700950531     C*
030800950531     C  N30KFVV          READE     FNFVV02L                               30
030900950531    2C                   ENDDO
031000950531     C*
031100110520     C* 07 OFF - NON PRESENTI FOGLI IN FiDST00F PER QUELLA DATA
031200950531    2C     *IN07         IFEQ      *OFF
031300950531     C                   MOVEL     MSG(2)        V1CMSG
031400950531     C                   SETON                                        4028
031500950531     C                   GOTO      ENDCT1
031600950531    2C                   ENDIF
031700950531     C*
031800950530     C                   SETON                                        01
031900950530    1C                   ENDIF
032000950530     C*
032100950530     C****  NUMERO FOGLIO DISTINTA  ****
032200950530    1C     V1CNFD        IFEQ      *BLANKS
032300950530     C     V1CNFD        OREQ      *ZEROS
032400950530    2C     V1CNFA        IFGT      0
032500950530     C                   MOVEL     MSG(3)        V1CMSG
032600950530     C                   SETON                                        4128
032700950530     C                   GOTO      ENDCT1
032800950530    2C                   ENDIF
032900950530     C*
033000950530   X1C                   ELSE
033100950530     C* NUMERO DISTINTA > 0
033200950530     C*
033300950530     C* RICERCA
033400950530     C     '?'           SCAN      V1CNFD                                 90
033500950530    2C     *IN90         IFEQ      *ON
033600950530     C                   CLEAR                   DSLR01
033700950530     C                   Z-ADD     V1CFGS        DLRFGS
033800950530     C                   MOVEL     'FNLRF1R'     DLRPGM
033900950530     C                   MOVEL     'S'           DLRRIC
034000950530     C                   MOVEL     '2'           DLRTFV
034100950530     C                   Z-ADD     4             DLRNPG
034200950530     C* 01 ON  - IMMESSA DATA FOGLIO DISTINTA
034300950530    3C     *IN01         IFEQ      *ON
034400950530     C                   Z-ADD     V1CDFV        DLRGDA
034500950530     C                   Z-ADD     V1CDFV        DLRGAL
034600950530   X3C                   ELSE
034700950530     C                   CLEAR                   DLRGDA
034800950530     C                   Z-ADD     311299        DLRGAL
034900950530    3C                   ENDIF
035000950530     C                   MOVEL     DSLR01        KPJBU
035100950530     C                   CALL      'FNLR02R'
035200950530     C                   PARM                    KPJBA
035300950530     C                   MOVEL     KPJBU         DSLR01
035400950530     C                   MOVE      DLRNFV        V1CNFD
035500950530     C*
035600950530     C                   GOTO      ENDCT1
035700950530    2C                   ENDIF
035800950530     C*
035900950530     C* CONTROLLO VALIDITA' NUMERO FOGLIO DISTINTA
036000950530     C                   TESTN                   V1CNFD               35
036100950530     C*
036200950530    2C     *IN35         IFEQ      *ON
036300950530     C                   MOVE      V1CNFD        W001
036400950530     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
036500950530    3C     W001          IFLT      '0'
036600950530     C                   SETOFF                                       35
036700950530    3C                   ENDIF
036800950530    2C                   ENDIF
036900950530     C*
037000950530    2C     *IN35         IFEQ      *OFF
037100950530     C                   MOVEL     MSG(4)        V1CMSG
037200950530     C                   SETON                                        4128
037300950530     C                   GOTO      ENDCT1
037400950530    2C                   ENDIF
037500950530     C*
037600950530     C* SE NUMERO DISTINTA > 0  CONTROLLO SE ESISTE IN FNFVV00F
037700110520     C                   MOVEL     V1CNFD        WNFD
037800950530     C     KFVV2         CHAIN     FNFVV01L                           30
037900950530     C*
038000950530    2C     *IN30         IFEQ      *OFF
038100950530     C* SE NUMERO FOGLIO DISTINTA ANNULLATO: ERRORE
038200950530    3C     FVVATB        IFNE      ' '
038300950607     C     V1CNFA        ANDEQ     0
038400950530     C                   MOVEL     MSG(5)        V1CMSG
038500950530     C                   SETON                                        4128
038600950530     C                   GOTO      ENDCT1
038700950530     C*
038800950530   X3C                   ELSE
038900950530     C* SE NUMERO FOGLIO DISTINTA CHIUSO   : ERRORE
039000950530    4C     FVVFCF        IFNE      ' '
039100950607     C     V1CNFA        ANDEQ     0
039200950530     C                   MOVEL     MSG(6)        V1CMSG
039300950530     C                   SETON                                        4128
039400950530     C                   GOTO      ENDCT1
039500950530    4C                   ENDIF
039600950530     C*
039700950530     C* SE IMMESSA CONTROLLO DATA DISTINTA
039800950530    4C   01FVVDFV        IFNE      WDFV
039900950530     C                   MOVEL     MSG(7)        V1CMSG
040000950530     C                   SETON                                        4128
040100950530     C                   GOTO      ENDCT1
040200950530    4C                   ENDIF
040300950530     C*
040400950530     C* SE IMMESSO CONTROLLO FLAG PRESTAZIONE PADRONCINO
040500950530    4C     V1CFPP        IFNE      ' '
040600950530     C     V1CFPP        ANDNE     FVVFPP
040700950607     C     V1CNFA        ANDEQ     0
040800950530     C                   MOVEL     MSG(8)        V1CMSG
040900950530     C                   SETON                                        4228
041000950530     C                   GOTO      ENDCT1
041100950530    4C                   ENDIF
041200950530     C*
041300110520     C* SE IN FNFVV00F TUTTO OK CONTROLLO SE ESISTE IN FIDST00F
041400110520     C     KDST          CHAIN     FiDST000                           31
041500950530    4C     *IN31         IFEQ      *ON
041600950607     C     V1CNFA        ANDEQ     0
041700950530     C                   MOVEL     MSG(9)        V1CMSG
041800950530     C                   SETON                                        4128
041900950530     C                   GOTO      ENDCT1
042000950530    4C                   ENDIF
042100950530     C*
042200950530     C* 03 ON  - IMMESSO NUMERO FOGLIO DISTINTA AL
042300950530    4C     V1CNFA        IFGT      0
042400950530     C                   SETON                                        03
042500950530     C     V1CNFA        IFLT      WNFD
042600950530     C                   MOVEL     MSG(10)       V1CMSG
042700950530     C                   SETON                                        4328
042800950530     C                   GOTO      ENDCT1
042900950530     C                   ENDIF
043000950530    4C                   ENDIF
043100950530     C*
043200950530     C* DESCRIZIONE FOGLIO DISTINTA DAL
043300950530     C  N03              MOVEL     FVVDSF        V1DNFD
043400950530    3C                   ENDIF
043500950530     C*
043600950530   X2C                   ELSE
043700950530     C* 30 ON  - NUMERO FOGLIO DISTINTA INESISTENTE
043800950607    3C     V1CNFA        IFEQ      0
043900950530     C                   MOVEL     MSG(4)        V1CMSG
044000950530     C                   SETON                                        4128
044100950530     C                   GOTO      ENDCT1
044200950607    3C                   ENDIF
044300950607    2C                   ENDIF
044400950530     C*
044500950530    1C                   ENDIF
044600950530     C*
044700950523     C****  CODICE PADRONCINO  ****
044800950523     C* RICERCA
044900950530     C     '?'           SCAN      V1CPDR                                 90
045000950523    1C     *IN90         IFEQ      *ON
045100021203     c                   clear                   fnlv24ds
045200021203     C                   Z-ADD     V1CFGS        d24FIL
045300021203     C                   MOVEL     'R'           d24FLG
045400021203     C                   MOVEL     'A'           d24tip
045500021203     C                   MOVEL(p)  fnlv24ds      KPJBU
045600950523     C                   CALL      'FNLV24R'
045700950523     C                   PARM                    KPJBA
045800021203     C                   MOVEL     KPJBU         fnlv24ds
045900950523     C*
046000950523     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
046100021203     C     d24pdr        IFNE      0
046200021203     C                   MOVEL     d24pdr        V1CPDR
046300021203     C                   MOVEL     d24rsc        V1DPDR
046400950523     C                   ENDIF
046500950523     C*
046600950523     C                   GOTO      ENDCT1
046700950523    1C                   ENDIF
046800950523     C*
046900950523     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
047000950530    1C     V1CPDR        IFNE      *BLANKS
047100950530     C     V1CPDR        ANDNE     *ZEROS
047200950530     C                   TESTN                   V1CPDR               35
047300950523     C*
047400950524    2C     *IN35         IFEQ      *ON
047500950530     C                   MOVE      V1CPDR        W001              1
047600950523     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
047700950524    3C     W001          IFLT      '0'
047800950523     C                   SETOFF                                       35
047900950524    3C                   ENDIF
048000950524    2C                   ENDIF
048100950523     C*
048200950524    2C     *IN35         IFEQ      *OFF
048300950530     C                   MOVEL     MSG(11)       V1CMSG
048400950530     C                   SETON                                        4428
048500950523     C                   GOTO      ENDCT1
048600950524    2C                   ENDIF
048700950523     C*
048800950530     C* LA FIL.PADRONCINO DEVE ESSERE CONGRUENTE CON LA FIL.IN GESTIONE
048900950530     C                   MOVEL     V1CPDR        W0030             3 0
049000950530    2C     W0030         IFNE      V1CFGS
049100950530     C                   MOVEL     MSG(12)       V1CMSG
049200950530     C                   SETON                                        4428
049300950530     C                   GOTO      ENDCT1
049400950530    2C                   ENDIF
049500950530     C*
049600950523     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
049700950530     C                   MOVE      V1CPDR        WPDR
049800021203     C     kapdw         CHAIN     fiapd000                           30
049900950523     C*
050000950523    2C  N30APDATB        IFNE      ' '
050100950523     C                   SETON                                        30
050200950523    2C                   ENDIF
050300950523     C* CODICE PADRONCINO INESISTENTE
050400950523    2C     *IN30         IFEQ      *ON
050500950530     C                   MOVEL     MSG(11)       V1CMSG
050600950530     C                   SETON                                        4428
050700950523     C                   GOTO      ENDCT1
050800950523    2C                   ENDIF
050900950530     C*
051000950531     C* SE IMMESSO CONTROLLO IL CODICE PADRONCINO
051100950530    2C     V1CNFD        IFNE      *BLANKS
051200950530     C     V1CNFD        ANDNE     *ZEROS
051300950607     C     V1CNFA        ANDEQ     0
051400950530     C     WPDR          ANDNE     DSTPDR
051500950530     C                   MOVEL     MSG(13)       V1CMSG
051600950530     C                   SETON                                        4428
051700950530     C                   GOTO      ENDCT1
051800950530    2C                   ENDIF
051900950530     C*
052000950530     C* DECODIFICA CODICE PADRONCINO
052100950530     C                   MOVEL     APDRSC        V1DPDR
052200950523    1C                   ENDIF
052300950523     C*
052400950523     C     ENDCT1        ENDSR
052500950530     C*
052600950525     C*---------------------------------------------------------------*
052700970417**
052800950612 ** RIEPILOGO DISTINTE CONSEGNA **
052900950523**  MSG
053000950530Effettuare almeno una selezione                                               01
053100950530Data immessa errata o non presenti distinte                                   02
053200950530Immettere il numero Distinta Dal                                              03
053300970417Foglio Distinta inesistente o in gestione ad altro punto operativo            04
053400950530Foglio Distinta annullato                                                     05
053500950530Foglio Distinta chiuso                                                        06
053600950530Foglio Distinta non appartenente alla data immessa                            07
053700970417Prestazione Autotrasp. incongruente con il numero Distinta Dal immesso        08
053800950530Foglio Distinta non utilizzabile                                              09
053900950530Numero Distinta Al deve essere maggiore o uguale al numero Distinta Dal       10
054000970417Codice Autotrasportatore inesistente                                          11
054100970417P.O. Autotrasportatore incongruente con punto operativo apertura foglio
054200970417Codice Autotrasportatore incongruente con il numero Distinta Dal immesso
