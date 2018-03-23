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
001100160524     Fazorg01l  IF   E           K DISK
001200950524     D*
001300950524     D* DEFINIZIONE SCHIERE
001400950525     D TES             S             34    DIM(1) CTDATA PERRCD(1)              TESTATE
001500160524     D MSG             S             78    DIM(16) CTDATA PERRCD(1)             MESSAGGI ERRORE
001600950530     D*
001700950530     D* RICHIAMO PGM DI ELABORAZIONE                  - FNLRF2R -
001800950530     D PARAM           DS
001900950530     D* FILIALE GESTIONE
002000950530     D  PARFGS                 1      3  0
002100950530     D* DATA FOGLIO DISTINTA IN GG/MM/AAAA
002200950530     D  PARDFV                 4     11  0
002300950530     D* DATA FOGLIO DISTINTA IN AAAA/MM/GG
002400950530     D  PARDFR                12     19  0
002500950530     D* NUMERO FOGLIO DISTINTA DAL
002600950530     D  PARNFD                20     24  0
002700950530     D* NUMERO FOGLIO DISTINTA  AL
002800950530     D  PARNFA                25     29  0
002900950530     D* CODICE PADRONCINO
003000950530     D  PARPDR                30     36  0
003100950530     D* PRESTAZIONE PADRONCINO
003200950530     D  PARFPP                37     37
003300950530     D* TIPO STAMPA
003400950531     D  PARTST                38     38
003500950612     D* NUMERO COPIE
003600950612     D  PARNCP                39     41  0
003700950913     D* STAMPA DEL NUMERO BOLLE SU EITCHETTE
003800950913     D  PARSNB                42     42
003900160524     D* parcel/messaggerie/tutte
004000160524     D  PARtpm                43     43
004100160524     D* turno 1 2
004200160524     D  PARtur1               44     44
004300160524     D  PARtur2               45     45
004400950607     D* STAMPANTE E MODULO ETICHETTE
004500950607     D  PARPRE               101    110
004600950607     D  PARMDE               111    120
004700950908     D* NOME PGM PER STAMPA ETICHETTE DISTINTA
004800950908     D  PARPSD               121    130
004900950523     D*
005000950523     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
005100950523     D DSLV50        E DS                  EXTNAME(FNLV50DS)
005200021203     D* DS PER FNLV24R - ricerca autotrasportatore
005300021203     D fnlv24ds      E DS
005400950523     D* DS PER FNLR02R - INTERROGAZIONE FOGLI VARI
005500950523     D DSLR01        E DS
005600950607      *
005700030724     D* DS PER TRUL90R - RICHIESTA STAMPANTI
005800030724     D trul90ds      E DS
005900950524     D*
006000950524     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
006100950524     D WLBDAT          DS                  INZ
006200950524     D  G02DAT                 1      8  0
006300950524     D  G02INV                 9     16  0
006400950524     D  G02ERR                17     17
006500950524     D  G02TGI                18     22  0
006600950523     D*
006700900514     D KPJBA         E DS
006800020506     D*                                       1   3 REM
006900020506     D*                                       4   6 REMFIL
007000950523     D CNCR80        E DS
007100000619     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007200160524     D dfvvspn       E DS
007300160525     D og150         E DS
007400950523     C*****************************************************************
007500950523     C* RIEPILOGO INDICATORI
007600950523     C*****************************************************************
007700950530     C* 01    - E' STATA IMMESSA LA DATA FOGLIO DISTINTA
007800950530     C* 03    - E' STATO IMMESSO IL NUMERO FOGLIO DISTINTA AL
007900950523     C* 04    - CAMBIO FILIALE IN GESTIONE PER CMD18
008000950523     C* 05    - SONO UN "REMXXX"
008100950523     C* 28    - INDICATORE DI EMISSIONE CAMPO MESSAGGI
008200950525     C* 30/32 - DI COMODO
008300950530     C* 35    - USATE PER TESTN
008400950530     C* 40/45 - ERRORI
008500950524     C* 90    - RIEMETTE IL FORMATO SENZA CHE CI SIANO STATI ERRORI
008600950523     C*****************************************************************
008700950607     C* SE SONO IN MANUTENZIONE RICHIESDO LE STAMPANTI
008800030724     C                   CLEAR                   trul90ds
008900950607     C*
009000030724     C                   MOVEL     'S'           D90RSE
009100030724     C                   CALL      'TRUL90R'
009200950607     C                   PARM                    KPJBA
009300030725     C                   PARM                    trul90ds
009400950607     C**
009500950607     C* F3 - FINE
009600030724     C     D90F3         IFEQ      '1'
009700950607     C                   GOTO      FINE
009800950607     C                   ENDIF
009900950530     C*
010000950530     C* PULIZIA CAMPI DEL VIDEO
010100950530     C                   MOVEL     UDATE8        V1CDFV
010200131029     C**********         CLEAR                   V1CTPD
010300950530     C                   CLEAR                   V1CNFD
010400950530     C                   CLEAR                   V1DNFD
010500950530     C                   CLEAR                   V1CNFA
010600950530     C                   CLEAR                   V1CPDR
010700950530     C                   CLEAR                   V1DPDR
010800950530     C                   CLEAR                   V1CFPP
010900950612     C                   Z-ADD     001           V1CNCP
011000950609     C                   MOVEL     'L'           V1CTST
011100030623     C                   MOVEL     'N'           V1CSNB
011200160524     C                   MOVEL     *blanks       V1Ctpm
011300160524     C                   MOVEL     '1'           V1Ctur1
011400160524     C                   MOVEL     '2'           V1Ctur2
011500950523     C*
011600950524     C* EMISSIONE FORMATO
011700950523     C     FOR01         TAG
011800950530     C                   EXFMT     LRF1D01
011900950523     C*
012000950523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
012100950523     C                   CLEAR                   V1CMSG
012200950523     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
012300950530     C                   SETOFF                                       284041
012400950530     C                   SETOFF                                       4243
012500950530     C                   SETOFF                                       4445
012600950523     C*
012700950523     C** CMD3  - FINE LAVORO
012800950530     C   KC              GOTO      FINE
012900950523     C*
013000950523     C** CMD18 - CAMBIO FILIALE
013100950526    1C     *INKS         IFEQ      *ON
013200950526     C                   SETON                                        04
013300950526     C                   GOTO      FOR01
013400950526    1C                   ENDIF
013500950523     C*
013600950523     C* 04 ON  - CONTROLLO FILIALE IN GESTIONE
013700950524    1C     *IN04         IFEQ      *ON
013800950523     C                   EXSR      CTRFGS
013900950524     C   28              GOTO      FOR01
014000950524    1C                   ENDIF
014100950523     C*
014200950523     C* CONTROLLI VIDEATA
014300950523     C                   EXSR      CTRD01
014400950530     C  NKF
014500950530     COR 28              GOTO      FOR01
014600950524     C*
014700950530     C***
014800950530     C*  CMD6  - ELABORAZIONE
014900950530     C***
015000950530     C                   CLEAR                   PARAM
015100030724     C                   MOVEL     D90MDE        PARMDE
015200030724     C                   MOVEL     D90PRE        PARPRE
015300950908     C* NOME PGM PER STAMPA ETICHETTE DISTINTA
015400030724     C                   MOVEL     D90PSD        PARPSD
015500950530     C* FILIALE GESTIONE
015600950530     C                   MOVEL     V1CFGS        PARFGS
015700950530     C* DATA FOGLIO DISTINTA
015800950530     C     *IN01         IFEQ      *ON
015900950530     C                   MOVEL     V1CDFV        PARDFV
016000950530     C                   MOVEL     WDFV          PARDFR
016100950530     C                   ENDIF
016200950530     C* NUMERO FOGLIO DISTINTA
016300950530     C     V1CNFD        IFNE      *BLANKS
016400950530     C     V1CNFD        ANDNE     *ZEROS
016500950530     C                   MOVEL     V1CNFD        PARNFD
016600950530     C     V1CNFA        IFEQ      0
016700950530     C                   MOVEL     V1CNFD        PARNFA
016800950530     C                   ELSE
016900950530     C                   MOVEL     V1CNFA        PARNFA
017000950530     C                   ENDIF
017100950530     C                   ENDIF
017200950530     C* CODICE PADRONCINO
017300950530     C     V1CPDR        IFNE      *BLANKS
017400950530     C     V1CPDR        ANDNE     *ZEROS
017500950530     C                   MOVEL     V1CPDR        PARPDR
017600950530     C                   ENDIF
017700950530     C* PRESTAZIONE PADRONCINO
017800950530     C                   MOVEL     V1CFPP        PARFPP
017900000619     C* TIPO PADRONCINO
018000131029     C*************      MOVEL     V1CTPD        PARTPD
018100950530     C* TIPO STAMPA
018200950530     C                   MOVEL     V1CTST        PARTST
018300950612     C                   MOVEL     V1CNCP        PARNCP
018400950913     C                   MOVEL     V1CSNB        PARSNB
018500160524     C                   MOVEL     V1Ctpm        PARtpm
018600160524     C                   MOVEL     V1Ctur1       PARtur1
018700160524     C                   MOVEL     V1Ctur2       PARtur2
018800950530     C*
018900950530     C                   MOVEL     PARAM         KPJBU
019000950530     C                   MOVEL     'LRF2'        KCOAZ
019100950530     C                   CALL      'BCH10'
019200950530     C                   PARM                    KPJBA
019300950530     C*
019400950530     C     FINE          TAG
019500950530     C                   SETON                                        LR
019600950523     C*
019700950530     C*--- INZSR INIZIALE --------------------------------------------*
019800950530     C     *INZSR        BEGSR
019900950530     C*
020000950530     C     *ENTRY        PLIST
020100950530     C                   PARM                    KPJBA
020200950530     C                   Z-ADD     1             CODUT
020300950530     C                   CALL      'X§PARUT'
020400950530     C                   PARM                    UT§DSE
020500950530     C                   MOVEL     RAGUT         V1CRSU
020600950530     C                   MOVEL     REC80         CNCR80
020700950530     C*
020800950530     C***
020900950531     C* LA DATA DEL GIORNO LA PRENDO DA TIME
021000950530     C***
021100950530     C                   TIME                    W0140            14 0
021200950530     C                   MOVE      W0140         UDATE8            8 0
021300950530     C*
021400950530     C***
021500950530     C* VEDO SE SONO SIMFEL O REMOTO
021600950530     C***
021700020506    1C     SIMTPP        IFEQ      '2'
021800020506     C     SIMTPP        OREQ      *BLANKS
021900020506     C                   MOVEL     SIMPOU        V1CFGS
022000950530     C                   SETON                                        05
022100950530   X1C                   ELSE
022200950530     C                   MOVEL     SIMFEL        V1CFGS
022300950530    1C                   ENDIF
022400160524      * controlla abilitazioni filiale
022500160524     c                   exsr      srtpm
022600950530     C*
022700950530     C                   MOVE      TES(1)        V1CTES
022800950530     C                   MOVEL     'FNLRF1R'     V1CPGM
022900950530     C                   Z-ADD     4             WNPG
023000950530     C*
023100950530     C***
023200950530     C* DEFINIZIONE CAMPI
023300950530     C***
023400950530     C     *LIKE         DEFINE    DSTPDR        WPDR
023500950530     C     *LIKE         DEFINE    FVVNFV        WNFD
023600950530     C     *LIKE         DEFINE    FVVDFV        WDFV
023700950530     C     *LIKE         DEFINE    FVVNPG        WNPG
023800950530     C*
023900950530     C* ACCESSO FNFVV01L
024000950530     C     KFVV2         KLIST
024100950530     C                   KFLD                    WNPG
024200950530     C                   KFLD                    WNFD
024300950530     C                   KFLD                    V1CFGS
024400950530     C* ACCESSO FNFVV02L
024500950530     C     KFVV          KLIST
024600950530     C                   KFLD                    WNPG
024700950530     C                   KFLD                    WDFV
024800950530     C                   KFLD                    V1CFGS
024900110520     C* ACCESSO FiDST01L
025000950530     C     KDST          KLIST
025100110520     C                   KFLD                    Wnpg
025200110520     C                   KFLD                    dstnfv
025300950530     C                   KFLD                    V1CFGS
025400110520     c                   eval      dstnpg=4
025500021203     C* ACCESSO Fiapd01L
025600021203     C     Kapdw         KLIST
025700021203     C                   KFLD                    apdtip
025800021203     C                   KFLD                    wpdr
025900021203     c                   movel     'A'           apdtip
026000950530     C                   ENDSR
026100950530     C*
026200950523     C*--- CONTROLLO LA FILIALE IN GESTIONE --------------------------*
026300950523     C     CTRFGS        BEGSR
026400950523     C                   CLEAR                   DSLV50
026500950523     C                   MOVEL     KNMUS         D50PRU
026600950530     C                   MOVEL     V1CFGS        D50FGS
026700950523     C                   CALL      'FNLV50R'
026800950523     C                   PARM                    DSLV50
026900950523     C*
027000950524    1C     D50ERR        IFNE      ' '
027100950523     C                   MOVEL     D50MSG        V1CMSG
027200160524     C                   SETON                                        4628
027300950523     C                   GOTO      ENDCTF
027400950524    1C                   ENDIF
027500160524      *controlla abilitazioni filiale
027600160524     c                   exsr      srtpm
027700950523     C*
027800950523     C     ENDCTF        ENDSR
027900950523     C*
028000950523     C*--- CONTROLLO DATI VIDEATA ------------------------------------*
028100950523     C     CTRD01        BEGSR
028200950530     C                   SETOFF                                       010390
028300950531     C                   SETOFF                                       07
028400950530     C                   CLEAR                   V1DNFD
028500950530     C                   CLEAR                   V1DPDR
028600950523     C*
028700950530     C****  EFFETTUARE ALMENO UNA SELEZIONE  ****
028800950530    1C     V1CDFV        IFEQ      0
028900950530     C     V1CNFD        IFEQ      *BLANKS
029000950530     C     V1CNFD        OREQ      *ZEROS
029100950530     C     V1CPDR        IFEQ      *BLANKS
029200950530     C     V1CPDR        OREQ      *ZEROS
029300950530     C                   MOVEL     MSG(1)        V1CMSG
029400950530     C                   SETON                                        4028
029500950530     C                   GOTO      ENDCT1
029600950530     C                   ENDIF
029700950530     C                   ENDIF
029800950530    1C                   ENDIF
029900950530     C*
030000950530     C****  DATA FOGLIO DISTINTA  ****
030100950530    1C     V1CDFV        IFGT      0
030200950530     C* GIRO DATA DISTINTA
030300950530     C                   MOVE      V1CDFV        G02DAT
030400950530     C                   MOVEL     *BLANKS       G02ERR
030500950530     C                   CALL      'XSRDA8'
030600950530     C                   PARM                    WLBDAT
030700950530     C                   Z-ADD     G02INV        WDFV
030800950530     C                   Z-ADD     G02DAT        V1CDFV
030900950530     C*
031000950530     C* CONTROLLO SE ESISTONO FOGLI DISTINTE IN QUELLA DATA
031100950531     C     KFVV          SETLL     FNFVV02L
031200950531     C     KFVV          READE     FNFVV02L                               30
031300950531     C*
031400950531    2C     *IN30         IFEQ      *ON
031500950531     C                   MOVEL     MSG(2)        V1CMSG
031600950531     C                   SETON                                        4028
031700950531     C                   GOTO      ENDCT1
031800950531    2C                   ENDIF
031900950531     C*
032000950531    2C     *IN30         DOWEQ     *OFF
032100110520     C* CONTROLLO SE ESISTE IN FIDST00F
032200110523     C                   z-add     FVVNFV        dstnfv
032300110520     C     KDST          SETLL     FIDST01L                               31
032400950531     C   31              SETON                                        0730
032500950531     C*
032600950531     C  N30KFVV          READE     FNFVV02L                               30
032700950531    2C                   ENDDO
032800950531     C*
032900110520     C* 07 OFF - NON PRESENTI FOGLI IN FiDST00F PER QUELLA DATA
033000950531    2C     *IN07         IFEQ      *OFF
033100950531     C                   MOVEL     MSG(2)        V1CMSG
033200950531     C                   SETON                                        4028
033300950531     C                   GOTO      ENDCT1
033400950531    2C                   ENDIF
033500950531     C*
033600950530     C                   SETON                                        01
033700950530    1C                   ENDIF
033800950530     C*
033900950530     C****  NUMERO FOGLIO DISTINTA  ****
034000950530    1C     V1CNFD        IFEQ      *BLANKS
034100950530     C     V1CNFD        OREQ      *ZEROS
034200950530    2C     V1CNFA        IFGT      0
034300950530     C                   MOVEL     MSG(3)        V1CMSG
034400950530     C                   SETON                                        4128
034500950530     C                   GOTO      ENDCT1
034600950530    2C                   ENDIF
034700950530     C*
034800950530   X1C                   ELSE
034900950530     C* NUMERO DISTINTA > 0
035000950530     C*
035100950530     C* RICERCA
035200950530     C     '?'           SCAN      V1CNFD                                 90
035300950530    2C     *IN90         IFEQ      *ON
035400950530     C                   CLEAR                   DSLR01
035500950530     C                   Z-ADD     V1CFGS        DLRFGS
035600950530     C                   MOVEL     'FNLRF1R'     DLRPGM
035700950530     C                   MOVEL     'S'           DLRRIC
035800950530     C                   MOVEL     '2'           DLRTFV
035900950530     C                   Z-ADD     4             DLRNPG
036000950530     C* 01 ON  - IMMESSA DATA FOGLIO DISTINTA
036100950530    3C     *IN01         IFEQ      *ON
036200950530     C                   Z-ADD     V1CDFV        DLRGDA
036300950530     C                   Z-ADD     V1CDFV        DLRGAL
036400950530   X3C                   ELSE
036500950530     C                   CLEAR                   DLRGDA
036600950530     C                   Z-ADD     311299        DLRGAL
036700950530    3C                   ENDIF
036800950530     C                   MOVEL     DSLR01        KPJBU
036900950530     C                   CALL      'FNLR02R'
037000950530     C                   PARM                    KPJBA
037100950530     C                   MOVEL     KPJBU         DSLR01
037200950530     C                   MOVE      DLRNFV        V1CNFD
037300950530     C*
037400950530     C                   GOTO      ENDCT1
037500950530    2C                   ENDIF
037600950530     C*
037700950530     C* CONTROLLO VALIDITA' NUMERO FOGLIO DISTINTA
037800950530     C                   TESTN                   V1CNFD               35
037900950530     C*
038000950530    2C     *IN35         IFEQ      *ON
038100950530     C                   MOVE      V1CNFD        W001
038200950530     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
038300950530    3C     W001          IFLT      '0'
038400950530     C                   SETOFF                                       35
038500950530    3C                   ENDIF
038600950530    2C                   ENDIF
038700950530     C*
038800950530    2C     *IN35         IFEQ      *OFF
038900950530     C                   MOVEL     MSG(4)        V1CMSG
039000950530     C                   SETON                                        4128
039100950530     C                   GOTO      ENDCT1
039200950530    2C                   ENDIF
039300950530     C*
039400950530     C* SE NUMERO DISTINTA > 0  CONTROLLO SE ESISTE IN FNFVV00F
039500110520     C                   MOVEL     V1CNFD        WNFD
039600950530     C     KFVV2         CHAIN     FNFVV01L                           30
039700950530     C*
039800950530    2C     *IN30         IFEQ      *OFF
039900950530     C* SE NUMERO FOGLIO DISTINTA ANNULLATO: ERRORE
040000950530    3C     FVVATB        IFNE      ' '
040100950607     C     V1CNFA        ANDEQ     0
040200950530     C                   MOVEL     MSG(5)        V1CMSG
040300950530     C                   SETON                                        4128
040400950530     C                   GOTO      ENDCT1
040500950530     C*
040600950530   X3C                   ELSE
040700950530     C* SE NUMERO FOGLIO DISTINTA CHIUSO   : ERRORE
040800950530    4C     FVVFCF        IFNE      ' '
040900950607     C     V1CNFA        ANDEQ     0
041000950530     C                   MOVEL     MSG(6)        V1CMSG
041100950530     C                   SETON                                        4128
041200950530     C                   GOTO      ENDCT1
041300950530    4C                   ENDIF
041400950530     C*
041500950530     C* SE IMMESSA CONTROLLO DATA DISTINTA
041600950530    4C   01FVVDFV        IFNE      WDFV
041700950530     C                   MOVEL     MSG(7)        V1CMSG
041800950530     C                   SETON                                        4128
041900950530     C                   GOTO      ENDCT1
042000950530    4C                   ENDIF
042100160524     c* se parcel + messaggerie se scelta una sola distinta controllo che
042200160524     c* il tipo scelto sia congruente
042300160524     C                   MOVEL     FVVspn        dfvvspn
042400160524      *
042500160525     c                   if        *in10 and not *in28
042600160524     c                             and v1ctpm <> ' '
042700160524     c                   if        (v1ctpm = 'P' and §fvvtpm <>' ') or
042800160524     c                             (v1ctpm = 'M' and §fvvtpm <>'M')
042900160524     C                   SETON                                        414228
043000160525     C                   MOVEL     msg(16)       v1cmsg
043100160524     c                   end
043200160524     c                   end
043300160524     c* se turno e se scelta una sola distinta controllo che
043400160524     c* il tipo scelto sia congruente
043500160525     c                   if        *in11 and not *in28
043600160524     c                             and (v1ctur1 = ' ' or v1ctur2 = ' ')
043700160524     c                             and §fvvtpm= ' '
043800160524     c                   select
043900160524     c                   when      (v1ctur1 = '1' or
044000160524     c                              v1ctur2 = '1') and
044100160524     c                             §fvvturno<> ' '
044200160524     C                   SETON                                        28
044300160524     c                   when      (v1ctur1 = '2' or
044400160524     c                              v1ctur2 = '2') and
044500160524     c                             §fvvturno <> '2'
044600160524     C                   SETON                                        28
044700160524     c                   endsl
044800160524     c                   if        *in28
044900160524     c                   seton                                        45
045000160525     C                   MOVEL     msg(14)       v1cmsg
045100160525     C                   GOTO      ENDCT1
045200160524     c                   end
045300160524     c                   end
045400160524     c* se parcel con + distinte
045500160524     c                   if        *in11 and not *in28
045600160524     c                             and v1ctur1 = ' ' and v1ctur2 = ' '
045700160524     c                             and (v1ctpm= 'P' or v1ctpm = ' ')
045800160524     c                   seton                                        4528
045900160525     C                   MOVEL     msg(15)       v1cmsg
046000160525     C                   GOTO      ENDCT1
046100160524     c                   end
046200950530     C*
046300950530     C* SE IMMESSO CONTROLLO FLAG PRESTAZIONE PADRONCINO
046400950530    4C     V1CFPP        IFNE      ' '
046500950530     C     V1CFPP        ANDNE     FVVFPP
046600950607     C     V1CNFA        ANDEQ     0
046700950530     C                   MOVEL     MSG(8)        V1CMSG
046800950530     C                   SETON                                        4228
046900950530     C                   GOTO      ENDCT1
047000950530    4C                   ENDIF
047100950530     C*
047200110520     C* SE IN FNFVV00F TUTTO OK CONTROLLO SE ESISTE IN FIDST00F
047300110520     C     KDST          CHAIN     FiDST000                           31
047400950530    4C     *IN31         IFEQ      *ON
047500950607     C     V1CNFA        ANDEQ     0
047600950530     C                   MOVEL     MSG(9)        V1CMSG
047700950530     C                   SETON                                        4128
047800950530     C                   GOTO      ENDCT1
047900950530    4C                   ENDIF
048000950530     C*
048100950530     C* 03 ON  - IMMESSO NUMERO FOGLIO DISTINTA AL
048200950530    4C     V1CNFA        IFGT      0
048300950530     C                   SETON                                        03
048400950530     C     V1CNFA        IFLT      WNFD
048500950530     C                   MOVEL     MSG(10)       V1CMSG
048600950530     C                   SETON                                        4328
048700950530     C                   GOTO      ENDCT1
048800950530     C                   ENDIF
048900950530    4C                   ENDIF
049000950530     C*
049100950530     C* DESCRIZIONE FOGLIO DISTINTA DAL
049200950530     C  N03              MOVEL     FVVDSF        V1DNFD
049300950530    3C                   ENDIF
049400950530     C*
049500950530   X2C                   ELSE
049600950530     C* 30 ON  - NUMERO FOGLIO DISTINTA INESISTENTE
049700950607    3C     V1CNFA        IFEQ      0
049800950530     C                   MOVEL     MSG(4)        V1CMSG
049900950530     C                   SETON                                        4128
050000950530     C                   GOTO      ENDCT1
050100950607    3C                   ENDIF
050200950607    2C                   ENDIF
050300950530     C*
050400950530    1C                   ENDIF
050500950530     C*
050600950523     C****  CODICE PADRONCINO  ****
050700950523     C* RICERCA
050800950530     C     '?'           SCAN      V1CPDR                                 90
050900950523    1C     *IN90         IFEQ      *ON
051000021203     c                   clear                   fnlv24ds
051100021203     C                   Z-ADD     V1CFGS        d24FIL
051200021203     C                   MOVEL     'R'           d24FLG
051300021203     C                   MOVEL     'A'           d24tip
051400021203     C                   MOVEL(p)  fnlv24ds      KPJBU
051500950523     C                   CALL      'FNLV24R'
051600950523     C                   PARM                    KPJBA
051700021203     C                   MOVEL     KPJBU         fnlv24ds
051800950523     C*
051900950523     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
052000021203     C     d24pdr        IFNE      0
052100021203     C                   MOVEL     d24pdr        V1CPDR
052200021203     C                   MOVEL     d24rsc        V1DPDR
052300950523     C                   ENDIF
052400950523     C*
052500950523     C                   GOTO      ENDCT1
052600950523    1C                   ENDIF
052700950523     C*
052800950523     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
052900950530    1C     V1CPDR        IFNE      *BLANKS
053000950530     C     V1CPDR        ANDNE     *ZEROS
053100950530     C                   TESTN                   V1CPDR               35
053200950523     C*
053300950524    2C     *IN35         IFEQ      *ON
053400950530     C                   MOVE      V1CPDR        W001              1
053500950523     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
053600950524    3C     W001          IFLT      '0'
053700950523     C                   SETOFF                                       35
053800950524    3C                   ENDIF
053900950524    2C                   ENDIF
054000950523     C*
054100950524    2C     *IN35         IFEQ      *OFF
054200950530     C                   MOVEL     MSG(11)       V1CMSG
054300950530     C                   SETON                                        4428
054400950523     C                   GOTO      ENDCT1
054500950524    2C                   ENDIF
054600950523     C*
054700950530     C* LA FIL.PADRONCINO DEVE ESSERE CONGRUENTE CON LA FIL.IN GESTIONE
054800950530     C                   MOVEL     V1CPDR        W0030             3 0
054900950530    2C     W0030         IFNE      V1CFGS
055000950530     C                   MOVEL     MSG(12)       V1CMSG
055100950530     C                   SETON                                        4428
055200950530     C                   GOTO      ENDCT1
055300950530    2C                   ENDIF
055400950530     C*
055500950523     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
055600950530     C                   MOVE      V1CPDR        WPDR
055700021203     C     kapdw         CHAIN     fiapd000                           30
055800950523     C*
055900950523    2C  N30APDATB        IFNE      ' '
056000950523     C                   SETON                                        30
056100950523    2C                   ENDIF
056200950523     C* CODICE PADRONCINO INESISTENTE
056300950523    2C     *IN30         IFEQ      *ON
056400950530     C                   MOVEL     MSG(11)       V1CMSG
056500950530     C                   SETON                                        4428
056600950523     C                   GOTO      ENDCT1
056700950523    2C                   ENDIF
056800950530     C*
056900950531     C* SE IMMESSO CONTROLLO IL CODICE PADRONCINO
057000950530    2C     V1CNFD        IFNE      *BLANKS
057100950530     C     V1CNFD        ANDNE     *ZEROS
057200950607     C     V1CNFA        ANDEQ     0
057300950530     C     WPDR          ANDNE     DSTPDR
057400950530     C                   MOVEL     MSG(13)       V1CMSG
057500950530     C                   SETON                                        4428
057600950530     C                   GOTO      ENDCT1
057700950530    2C                   ENDIF
057800950530     C*
057900950530     C* DECODIFICA CODICE PADRONCINO
058000950530     C                   MOVEL     APDRSC        V1DPDR
058100950523    1C                   ENDIF
058200950523     C*
058300950523     C     ENDCT1        ENDSR
058400950530     C*
058500160524     C*--------------------------------------------------------------*
058600160524     C*  parcel + messaggerie
058700160524     C*--------------------------------------------------------------*
058800160524     C     srtpm         BEGSR
058900160524     c* leggo organigramma x gestire ind 10 parcel+messaggerie
059000160524     c     v1cfgs        chain     azorg01l
059100160524     c                   if        %found(azorg01l)
059200160524     c                   movel     orgdf0        og150
059300160524     c     §OGPKM        comp      'S'                                    10
059400160524     c     §OGturno      comp      'S'                                    11
059500160524     c                   end
059600160524     C                   ENDSR
059700950525     C*---------------------------------------------------------------*
059800970417**
059900950612 ** RIEPILOGO DISTINTE CONSEGNA **
060000950523**  MSG
060100950530Effettuare almeno una selezione                                               01
060200950530Data immessa errata o non presenti distinte                                   02
060300950530Immettere il numero Distinta Dal                                              03
060400970417Foglio Distinta inesistente o in gestione ad altro punto operativo            04
060500950530Foglio Distinta annullato                                                     05
060600950530Foglio Distinta chiuso                                                        06
060700950530Foglio Distinta non appartenente alla data immessa                            07
060800970417Prestazione Autotrasp. incongruente con il numero Distinta Dal immesso        08
060900950530Foglio Distinta non utilizzabile                                              09
061000950530Numero Distinta Al deve essere maggiore o uguale al numero Distinta Dal       10
061100970417Codice Autotrasportatore inesistente                                          11
061200160524P.O. Autotrasportatore incongruente con punto operativo apertura foglio       12
061300160524Codice Autotrasportatore incongruente con il numero Distinta Dal immesso      13
061400160524Distinta non congruente con il turno                                          14
061500160524Per distinte Parcel è obbligatoria la scelta del turno                        15
061600160524Distinta non congruente con il tipo distinta consegna                         16
