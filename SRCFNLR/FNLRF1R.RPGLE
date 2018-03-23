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
005400160719     D* DS PER Fidg31R - INTERROGAZIONE FOGLI VARI
005500160719     D fidg31ds      E DS
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
007400160719     d $FV6            s              6P 0 dim(600)                             F.V.  x FIDG31R
007500950523     C*****************************************************************
007600950523     C* RIEPILOGO INDICATORI
007700950523     C*****************************************************************
007800950530     C* 01    - E' STATA IMMESSA LA DATA FOGLIO DISTINTA
007900950530     C* 03    - E' STATO IMMESSO IL NUMERO FOGLIO DISTINTA AL
008000950523     C* 04    - CAMBIO FILIALE IN GESTIONE PER CMD18
008100950523     C* 05    - SONO UN "REMXXX"
008200950523     C* 28    - INDICATORE DI EMISSIONE CAMPO MESSAGGI
008300950525     C* 30/32 - DI COMODO
008400950530     C* 35    - USATE PER TESTN
008500950530     C* 40/45 - ERRORI
008600950524     C* 90    - RIEMETTE IL FORMATO SENZA CHE CI SIANO STATI ERRORI
008700950523     C*****************************************************************
008800950607     C* SE SONO IN MANUTENZIONE RICHIESDO LE STAMPANTI
008900030724     C                   CLEAR                   trul90ds
009000950607     C*
009100030724     C                   MOVEL     'S'           D90RSE
009200030724     C                   CALL      'TRUL90R'
009300950607     C                   PARM                    KPJBA
009400030725     C                   PARM                    trul90ds
009500950607     C**
009600950607     C* F3 - FINE
009700030724     C     D90F3         IFEQ      '1'
009800950607     C                   GOTO      FINE
009900950607     C                   ENDIF
010000950530     C*
010100950530     C* PULIZIA CAMPI DEL VIDEO
010200950530     C                   MOVEL     UDATE8        V1CDFV
010300131029     C**********         CLEAR                   V1CTPD
010400950530     C                   CLEAR                   V1CNFD
010500950530     C                   CLEAR                   V1DNFD
010600950530     C                   CLEAR                   V1CNFA
010700950530     C                   CLEAR                   V1CPDR
010800950530     C                   CLEAR                   V1DPDR
010900950530     C                   CLEAR                   V1CFPP
011000950612     C                   Z-ADD     001           V1CNCP
011100950609     C                   MOVEL     'L'           V1CTST
011200030623     C                   MOVEL     'N'           V1CSNB
011300160524     C                   MOVEL     *blanks       V1Ctpm
011400160524     C                   MOVEL     '1'           V1Ctur1
011500160524     C                   MOVEL     '2'           V1Ctur2
011600950523     C*
011700950524     C* EMISSIONE FORMATO
011800950523     C     FOR01         TAG
011900950530     C                   EXFMT     LRF1D01
012000950523     C*
012100950523     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
012200950523     C                   CLEAR                   V1CMSG
012300950523     C* AZZERO GLI INDICATORI REALTIVI AI MESSAGGI DI ERRORE
012400950530     C                   SETOFF                                       284041
012500950530     C                   SETOFF                                       4243
012600950530     C                   SETOFF                                       4445
012700950523     C*
012800950523     C** CMD3  - FINE LAVORO
012900950530     C   KC              GOTO      FINE
013000950523     C*
013100950523     C** CMD18 - CAMBIO FILIALE
013200950526    1C     *INKS         IFEQ      *ON
013300950526     C                   SETON                                        04
013400950526     C                   GOTO      FOR01
013500950526    1C                   ENDIF
013600950523     C*
013700950523     C* 04 ON  - CONTROLLO FILIALE IN GESTIONE
013800950524    1C     *IN04         IFEQ      *ON
013900950523     C                   EXSR      CTRFGS
014000950524     C   28              GOTO      FOR01
014100950524    1C                   ENDIF
014200950523     C*
014300950523     C* CONTROLLI VIDEATA
014400950523     C                   EXSR      CTRD01
014500950530     C  NKF
014600950530     COR 28              GOTO      FOR01
014700950524     C*
014800950530     C***
014900950530     C*  CMD6  - ELABORAZIONE
015000950530     C***
015100950530     C                   CLEAR                   PARAM
015200030724     C                   MOVEL     D90MDE        PARMDE
015300030724     C                   MOVEL     D90PRE        PARPRE
015400950908     C* NOME PGM PER STAMPA ETICHETTE DISTINTA
015500030724     C                   MOVEL     D90PSD        PARPSD
015600950530     C* FILIALE GESTIONE
015700950530     C                   MOVEL     V1CFGS        PARFGS
015800950530     C* DATA FOGLIO DISTINTA
015900950530     C     *IN01         IFEQ      *ON
016000950530     C                   MOVEL     V1CDFV        PARDFV
016100950530     C                   MOVEL     WDFV          PARDFR
016200950530     C                   ENDIF
016300950530     C* NUMERO FOGLIO DISTINTA
016400950530     C     V1CNFD        IFNE      *BLANKS
016500950530     C     V1CNFD        ANDNE     *ZEROS
016600950530     C                   MOVEL     V1CNFD        PARNFD
016700950530     C     V1CNFA        IFEQ      0
016800950530     C                   MOVEL     V1CNFD        PARNFA
016900950530     C                   ELSE
017000950530     C                   MOVEL     V1CNFA        PARNFA
017100950530     C                   ENDIF
017200950530     C                   ENDIF
017300950530     C* CODICE PADRONCINO
017400950530     C     V1CPDR        IFNE      *BLANKS
017500950530     C     V1CPDR        ANDNE     *ZEROS
017600950530     C                   MOVEL     V1CPDR        PARPDR
017700950530     C                   ENDIF
017800950530     C* PRESTAZIONE PADRONCINO
017900950530     C                   MOVEL     V1CFPP        PARFPP
018000000619     C* TIPO PADRONCINO
018100131029     C*************      MOVEL     V1CTPD        PARTPD
018200950530     C* TIPO STAMPA
018300950530     C                   MOVEL     V1CTST        PARTST
018400950612     C                   MOVEL     V1CNCP        PARNCP
018500950913     C                   MOVEL     V1CSNB        PARSNB
018600160524     C                   MOVEL     V1Ctpm        PARtpm
018700160524     C                   MOVEL     V1Ctur1       PARtur1
018800160524     C                   MOVEL     V1Ctur2       PARtur2
018900950530     C*
019000950530     C                   MOVEL     PARAM         KPJBU
019100950530     C                   MOVEL     'LRF2'        KCOAZ
019200950530     C                   CALL      'BCH10'
019300950530     C                   PARM                    KPJBA
019400950530     C*
019500950530     C     FINE          TAG
019600950530     C                   SETON                                        LR
019700950523     C*
019800950530     C*--- INZSR INIZIALE --------------------------------------------*
019900950530     C     *INZSR        BEGSR
020000950530     C*
020100950530     C     *ENTRY        PLIST
020200950530     C                   PARM                    KPJBA
020300950530     C                   Z-ADD     1             CODUT
020400950530     C                   CALL      'X§PARUT'
020500950530     C                   PARM                    UT§DSE
020600950530     C                   MOVEL     RAGUT         V1CRSU
020700950530     C                   MOVEL     REC80         CNCR80
020800950530     C*
020900950530     C***
021000950531     C* LA DATA DEL GIORNO LA PRENDO DA TIME
021100950530     C***
021200950530     C                   TIME                    W0140            14 0
021300950530     C                   MOVE      W0140         UDATE8            8 0
021400950530     C*
021500950530     C***
021600950530     C* VEDO SE SONO SIMFEL O REMOTO
021700950530     C***
021800020506    1C     SIMTPP        IFEQ      '2'
021900020506     C     SIMTPP        OREQ      *BLANKS
022000020506     C                   MOVEL     SIMPOU        V1CFGS
022100950530     C                   SETON                                        05
022200950530   X1C                   ELSE
022300950530     C                   MOVEL     SIMFEL        V1CFGS
022400950530    1C                   ENDIF
022500160524      * controlla abilitazioni filiale
022600160524     c                   exsr      srtpm
022700950530     C*
022800950530     C                   MOVE      TES(1)        V1CTES
022900950530     C                   MOVEL     'FNLRF1R'     V1CPGM
023000950530     C                   Z-ADD     4             WNPG
023100950530     C*
023200950530     C***
023300950530     C* DEFINIZIONE CAMPI
023400950530     C***
023500950530     C     *LIKE         DEFINE    DSTPDR        WPDR
023600950530     C     *LIKE         DEFINE    FVVNFV        WNFD
023700950530     C     *LIKE         DEFINE    FVVDFV        WDFV
023800950530     C     *LIKE         DEFINE    FVVNPG        WNPG
023900950530     C*
024000950530     C* ACCESSO FNFVV01L
024100950530     C     KFVV2         KLIST
024200950530     C                   KFLD                    WNPG
024300950530     C                   KFLD                    WNFD
024400950530     C                   KFLD                    V1CFGS
024500950530     C* ACCESSO FNFVV02L
024600950530     C     KFVV          KLIST
024700950530     C                   KFLD                    WNPG
024800950530     C                   KFLD                    WDFV
024900950530     C                   KFLD                    V1CFGS
025000110520     C* ACCESSO FiDST01L
025100950530     C     KDST          KLIST
025200110520     C                   KFLD                    Wnpg
025300110520     C                   KFLD                    dstnfv
025400950530     C                   KFLD                    V1CFGS
025500110520     c                   eval      dstnpg=4
025600021203     C* ACCESSO Fiapd01L
025700021203     C     Kapdw         KLIST
025800021203     C                   KFLD                    apdtip
025900021203     C                   KFLD                    wpdr
026000021203     c                   movel     'A'           apdtip
026100950530     C                   ENDSR
026200950530     C*
026300950523     C*--- CONTROLLO LA FILIALE IN GESTIONE --------------------------*
026400950523     C     CTRFGS        BEGSR
026500950523     C                   CLEAR                   DSLV50
026600950523     C                   MOVEL     KNMUS         D50PRU
026700950530     C                   MOVEL     V1CFGS        D50FGS
026800950523     C                   CALL      'FNLV50R'
026900950523     C                   PARM                    DSLV50
027000950523     C*
027100950524    1C     D50ERR        IFNE      ' '
027200950523     C                   MOVEL     D50MSG        V1CMSG
027300160524     C                   SETON                                        4628
027400950523     C                   GOTO      ENDCTF
027500950524    1C                   ENDIF
027600160524      *controlla abilitazioni filiale
027700160524     c                   exsr      srtpm
027800950523     C*
027900950523     C     ENDCTF        ENDSR
028000950523     C*
028100950523     C*--- CONTROLLO DATI VIDEATA ------------------------------------*
028200950523     C     CTRD01        BEGSR
028300950530     C                   SETOFF                                       010390
028400950531     C                   SETOFF                                       07
028500950530     C                   CLEAR                   V1DNFD
028600950530     C                   CLEAR                   V1DPDR
028700950523     C*
028800950530     C****  EFFETTUARE ALMENO UNA SELEZIONE  ****
028900950530    1C     V1CDFV        IFEQ      0
029000950530     C     V1CNFD        IFEQ      *BLANKS
029100950530     C     V1CNFD        OREQ      *ZEROS
029200950530     C     V1CPDR        IFEQ      *BLANKS
029300950530     C     V1CPDR        OREQ      *ZEROS
029400950530     C                   MOVEL     MSG(1)        V1CMSG
029500950530     C                   SETON                                        4028
029600950530     C                   GOTO      ENDCT1
029700950530     C                   ENDIF
029800950530     C                   ENDIF
029900950530    1C                   ENDIF
030000950530     C*
030100950530     C****  DATA FOGLIO DISTINTA  ****
030200950530    1C     V1CDFV        IFGT      0
030300950530     C* GIRO DATA DISTINTA
030400950530     C                   MOVE      V1CDFV        G02DAT
030500950530     C                   MOVEL     *BLANKS       G02ERR
030600950530     C                   CALL      'XSRDA8'
030700950530     C                   PARM                    WLBDAT
030800950530     C                   Z-ADD     G02INV        WDFV
030900950530     C                   Z-ADD     G02DAT        V1CDFV
031000950530     C*
031100950530     C* CONTROLLO SE ESISTONO FOGLI DISTINTE IN QUELLA DATA
031200950531     C     KFVV          SETLL     FNFVV02L
031300950531     C     KFVV          READE     FNFVV02L                               30
031400950531     C*
031500950531    2C     *IN30         IFEQ      *ON
031600950531     C                   MOVEL     MSG(2)        V1CMSG
031700950531     C                   SETON                                        4028
031800950531     C                   GOTO      ENDCT1
031900950531    2C                   ENDIF
032000950531     C*
032100950531    2C     *IN30         DOWEQ     *OFF
032200110520     C* CONTROLLO SE ESISTE IN FIDST00F
032300110523     C                   z-add     FVVNFV        dstnfv
032400110520     C     KDST          SETLL     FIDST01L                               31
032500950531     C   31              SETON                                        0730
032600950531     C*
032700950531     C  N30KFVV          READE     FNFVV02L                               30
032800950531    2C                   ENDDO
032900950531     C*
033000110520     C* 07 OFF - NON PRESENTI FOGLI IN FiDST00F PER QUELLA DATA
033100950531    2C     *IN07         IFEQ      *OFF
033200950531     C                   MOVEL     MSG(2)        V1CMSG
033300950531     C                   SETON                                        4028
033400950531     C                   GOTO      ENDCT1
033500950531    2C                   ENDIF
033600950531     C*
033700950530     C                   SETON                                        01
033800950530    1C                   ENDIF
033900950530     C*
034000950530     C****  NUMERO FOGLIO DISTINTA  ****
034100950530    1C     V1CNFD        IFEQ      *BLANKS
034200950530     C     V1CNFD        OREQ      *ZEROS
034300950530    2C     V1CNFA        IFGT      0
034400950530     C                   MOVEL     MSG(3)        V1CMSG
034500950530     C                   SETON                                        4128
034600950530     C                   GOTO      ENDCT1
034700950530    2C                   ENDIF
034800950530     C*
034900950530   X1C                   ELSE
035000950530     C* NUMERO DISTINTA > 0
035100950530     C*
035200950530     C* RICERCA
035300950530     C     '?'           SCAN      V1CNFD                                 90
035400950530    2C     *IN90         IFEQ      *ON
035500160719     C                   CLEAR                   fidg31ds
035600160719     C                   Z-ADD     V1CFGS        fdgFGS
035700160719     C                   MOVEL     'FNLRF1R'     fdgPGM
035800160719     C                   MOVEL     'S'           fdgRIC
035900160719     C                   MOVEL     '2'           fdgTFV
036000160719     C                   Z-ADD     4             fdgNPG
036100160719     C                   MOVEL     v1ctpm        fdgtdis
036200160719     C                   MOVEL     v1cfpp        fdgtusc
036300160719     C                   MOVEL     V1Ctur1       FDGTUR1
036400160719     C                   MOVEL     V1Ctur2       FDGTUR2
036500950530     C* 01 ON  - IMMESSA DATA FOGLIO DISTINTA
036600950530    3C     *IN01         IFEQ      *ON
036700160719     C                   Z-ADD     wdfv          fdgGDA
036800160719     C                   Z-ADD     wdfv          fdgGAL
036900950530   X3C                   ELSE
037000160719     C                   CLEAR                   fdgGDA
037100160719     C                   Z-ADD     311299        fdgGAL
037200950530    3C                   ENDIF
037300160719     C                   MOVEL     fidg31ds      KPJBU
037400160719     c                   clear                   $FV6
037500160719      * impostato a tutti 99999
037600160719     c                   do        600           yy                3 0
037700160719     c                   z-add     999999        $FV6(yy)
037800160719     c                   enddo
037900160719     C                   CALL      'FIDG31R'
038000950530     C                   PARM                    KPJBA
038100160719     C                   PARM                    $fv6
038200160719     C                   MOVEL     KPJBU         fidg31ds
038300160719     C                   MOVE      fdgNFV        V1CNFD
038400950530     C*
038500950530     C                   GOTO      ENDCT1
038600950530    2C                   ENDIF
038700950530     C*
038800950530     C* CONTROLLO VALIDITA' NUMERO FOGLIO DISTINTA
038900950530     C                   TESTN                   V1CNFD               35
039000950530     C*
039100950530    2C     *IN35         IFEQ      *ON
039200950530     C                   MOVE      V1CNFD        W001
039300950530     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
039400950530    3C     W001          IFLT      '0'
039500950530     C                   SETOFF                                       35
039600950530    3C                   ENDIF
039700950530    2C                   ENDIF
039800950530     C*
039900950530    2C     *IN35         IFEQ      *OFF
040000950530     C                   MOVEL     MSG(4)        V1CMSG
040100950530     C                   SETON                                        4128
040200950530     C                   GOTO      ENDCT1
040300950530    2C                   ENDIF
040400950530     C*
040500950530     C* SE NUMERO DISTINTA > 0  CONTROLLO SE ESISTE IN FNFVV00F
040600110520     C                   MOVEL     V1CNFD        WNFD
040700950530     C     KFVV2         CHAIN     FNFVV01L                           30
040800950530     C*
040900950530    2C     *IN30         IFEQ      *OFF
041000950530     C* SE NUMERO FOGLIO DISTINTA ANNULLATO: ERRORE
041100950530    3C     FVVATB        IFNE      ' '
041200950607     C     V1CNFA        ANDEQ     0
041300950530     C                   MOVEL     MSG(5)        V1CMSG
041400950530     C                   SETON                                        4128
041500950530     C                   GOTO      ENDCT1
041600950530     C*
041700950530   X3C                   ELSE
041800950530     C* SE NUMERO FOGLIO DISTINTA CHIUSO   : ERRORE
041900950530    4C     FVVFCF        IFNE      ' '
042000950607     C     V1CNFA        ANDEQ     0
042100950530     C                   MOVEL     MSG(6)        V1CMSG
042200950530     C                   SETON                                        4128
042300950530     C                   GOTO      ENDCT1
042400950530    4C                   ENDIF
042500950530     C*
042600950530     C* SE IMMESSA CONTROLLO DATA DISTINTA
042700950530    4C   01FVVDFV        IFNE      WDFV
042800950530     C                   MOVEL     MSG(7)        V1CMSG
042900950530     C                   SETON                                        4128
043000950530     C                   GOTO      ENDCT1
043100950530    4C                   ENDIF
043200160524     c* se parcel + messaggerie se scelta una sola distinta controllo che
043300160524     c* il tipo scelto sia congruente
043400160524     C                   MOVEL     FVVspn        dfvvspn
043500160524      *
043600160525     c                   if        *in10 and not *in28
043700160524     c                             and v1ctpm <> ' '
043800160524     c                   if        (v1ctpm = 'P' and §fvvtpm <>' ') or
043900160524     c                             (v1ctpm = 'M' and §fvvtpm <>'M')
044000160524     C                   SETON                                        414228
044100160525     C                   MOVEL     msg(16)       v1cmsg
044200160524     c                   end
044300160524     c                   end
044400160524     c* se turno e se scelta una sola distinta controllo che
044500160524     c* il tipo scelto sia congruente
044600160525     c                   if        *in11 and not *in28
044700160524     c                             and (v1ctur1 = ' ' or v1ctur2 = ' ')
044800160524     c                             and §fvvtpm= ' '
044900160524     c                   select
045000160524     c                   when      (v1ctur1 = '1' or
045100160524     c                              v1ctur2 = '1') and
045200160524     c                             §fvvturno<> ' '
045300160524     C                   SETON                                        28
045400160524     c                   when      (v1ctur1 = '2' or
045500160524     c                              v1ctur2 = '2') and
045600160524     c                             §fvvturno <> '2'
045700160524     C                   SETON                                        28
045800160524     c                   endsl
045900160524     c                   if        *in28
046000160524     c                   seton                                        45
046100160525     C                   MOVEL     msg(14)       v1cmsg
046200160525     C                   GOTO      ENDCT1
046300160524     c                   end
046400160524     c                   end
046500160524     c* se parcel con + distinte
046600160524     c                   if        *in11 and not *in28
046700160524     c                             and v1ctur1 = ' ' and v1ctur2 = ' '
046800160524     c                             and (v1ctpm= 'P' or v1ctpm = ' ')
046900160524     c                   seton                                        4528
047000160525     C                   MOVEL     msg(15)       v1cmsg
047100160525     C                   GOTO      ENDCT1
047200160524     c                   end
047300950530     C*
047400950530     C* SE IMMESSO CONTROLLO FLAG PRESTAZIONE PADRONCINO
047500950530    4C     V1CFPP        IFNE      ' '
047600950530     C     V1CFPP        ANDNE     FVVFPP
047700950607     C     V1CNFA        ANDEQ     0
047800950530     C                   MOVEL     MSG(8)        V1CMSG
047900950530     C                   SETON                                        4228
048000950530     C                   GOTO      ENDCT1
048100950530    4C                   ENDIF
048200950530     C*
048300110520     C* SE IN FNFVV00F TUTTO OK CONTROLLO SE ESISTE IN FIDST00F
048400110520     C     KDST          CHAIN     FiDST000                           31
048500950530    4C     *IN31         IFEQ      *ON
048600950607     C     V1CNFA        ANDEQ     0
048700950530     C                   MOVEL     MSG(9)        V1CMSG
048800950530     C                   SETON                                        4128
048900950530     C                   GOTO      ENDCT1
049000950530    4C                   ENDIF
049100950530     C*
049200950530     C* 03 ON  - IMMESSO NUMERO FOGLIO DISTINTA AL
049300950530    4C     V1CNFA        IFGT      0
049400950530     C                   SETON                                        03
049500950530     C     V1CNFA        IFLT      WNFD
049600950530     C                   MOVEL     MSG(10)       V1CMSG
049700950530     C                   SETON                                        4328
049800950530     C                   GOTO      ENDCT1
049900950530     C                   ENDIF
050000950530    4C                   ENDIF
050100950530     C*
050200950530     C* DESCRIZIONE FOGLIO DISTINTA DAL
050300950530     C  N03              MOVEL     FVVDSF        V1DNFD
050400950530    3C                   ENDIF
050500950530     C*
050600950530   X2C                   ELSE
050700950530     C* 30 ON  - NUMERO FOGLIO DISTINTA INESISTENTE
050800950607    3C     V1CNFA        IFEQ      0
050900950530     C                   MOVEL     MSG(4)        V1CMSG
051000950530     C                   SETON                                        4128
051100950530     C                   GOTO      ENDCT1
051200950607    3C                   ENDIF
051300950607    2C                   ENDIF
051400950530     C*
051500950530    1C                   ENDIF
051600950530     C*
051700950523     C****  CODICE PADRONCINO  ****
051800950523     C* RICERCA
051900950530     C     '?'           SCAN      V1CPDR                                 90
052000950523    1C     *IN90         IFEQ      *ON
052100021203     c                   clear                   fnlv24ds
052200021203     C                   Z-ADD     V1CFGS        d24FIL
052300021203     C                   MOVEL     'R'           d24FLG
052400021203     C                   MOVEL     'A'           d24tip
052500021203     C                   MOVEL(p)  fnlv24ds      KPJBU
052600950523     C                   CALL      'FNLV24R'
052700950523     C                   PARM                    KPJBA
052800021203     C                   MOVEL     KPJBU         fnlv24ds
052900950523     C*
053000950523     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
053100021203     C     d24pdr        IFNE      0
053200021203     C                   MOVEL     d24pdr        V1CPDR
053300021203     C                   MOVEL     d24rsc        V1DPDR
053400950523     C                   ENDIF
053500950523     C*
053600950523     C                   GOTO      ENDCT1
053700950523    1C                   ENDIF
053800950523     C*
053900950523     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
054000950530    1C     V1CPDR        IFNE      *BLANKS
054100950530     C     V1CPDR        ANDNE     *ZEROS
054200950530     C                   TESTN                   V1CPDR               35
054300950523     C*
054400950524    2C     *IN35         IFEQ      *ON
054500950530     C                   MOVE      V1CPDR        W001              1
054600950523     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
054700950524    3C     W001          IFLT      '0'
054800950523     C                   SETOFF                                       35
054900950524    3C                   ENDIF
055000950524    2C                   ENDIF
055100950523     C*
055200950524    2C     *IN35         IFEQ      *OFF
055300950530     C                   MOVEL     MSG(11)       V1CMSG
055400950530     C                   SETON                                        4428
055500950523     C                   GOTO      ENDCT1
055600950524    2C                   ENDIF
055700950523     C*
055800950530     C* LA FIL.PADRONCINO DEVE ESSERE CONGRUENTE CON LA FIL.IN GESTIONE
055900950530     C                   MOVEL     V1CPDR        W0030             3 0
056000950530    2C     W0030         IFNE      V1CFGS
056100950530     C                   MOVEL     MSG(12)       V1CMSG
056200950530     C                   SETON                                        4428
056300950530     C                   GOTO      ENDCT1
056400950530    2C                   ENDIF
056500950530     C*
056600950523     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
056700950530     C                   MOVE      V1CPDR        WPDR
056800021203     C     kapdw         CHAIN     fiapd000                           30
056900950523     C*
057000950523    2C  N30APDATB        IFNE      ' '
057100950523     C                   SETON                                        30
057200950523    2C                   ENDIF
057300950523     C* CODICE PADRONCINO INESISTENTE
057400950523    2C     *IN30         IFEQ      *ON
057500950530     C                   MOVEL     MSG(11)       V1CMSG
057600950530     C                   SETON                                        4428
057700950523     C                   GOTO      ENDCT1
057800950523    2C                   ENDIF
057900950530     C*
058000950531     C* SE IMMESSO CONTROLLO IL CODICE PADRONCINO
058100950530    2C     V1CNFD        IFNE      *BLANKS
058200950530     C     V1CNFD        ANDNE     *ZEROS
058300950607     C     V1CNFA        ANDEQ     0
058400950530     C     WPDR          ANDNE     DSTPDR
058500950530     C                   MOVEL     MSG(13)       V1CMSG
058600950530     C                   SETON                                        4428
058700950530     C                   GOTO      ENDCT1
058800950530    2C                   ENDIF
058900950530     C*
059000950530     C* DECODIFICA CODICE PADRONCINO
059100950530     C                   MOVEL     APDRSC        V1DPDR
059200950523    1C                   ENDIF
059300950523     C*
059400950523     C     ENDCT1        ENDSR
059500950530     C*
059600160524     C*--------------------------------------------------------------*
059700160524     C*  parcel + messaggerie
059800160524     C*--------------------------------------------------------------*
059900160524     C     srtpm         BEGSR
060000160524     c* leggo organigramma x gestire ind 10 parcel+messaggerie
060100160524     c     v1cfgs        chain     azorg01l
060200160524     c                   if        %found(azorg01l)
060300160524     c                   movel     orgdf0        og150
060400160524     c     §OGPKM        comp      'S'                                    10
060500160524     c     §OGturno      comp      'S'                                    11
060600160524     c                   end
060700160524     C                   ENDSR
060800950525     C*---------------------------------------------------------------*
060900970417**
061000950612 ** RIEPILOGO DISTINTE CONSEGNA **
061100950523**  MSG
061200950530Effettuare almeno una selezione                                               01
061300950530Data immessa errata o non presenti distinte                                   02
061400950530Immettere il numero Distinta Dal                                              03
061500970417Foglio Distinta inesistente o in gestione ad altro punto operativo            04
061600950530Foglio Distinta annullato                                                     05
061700950530Foglio Distinta chiuso                                                        06
061800950530Foglio Distinta non appartenente alla data immessa                            07
061900970417Prestazione Autotrasp. incongruente con il numero Distinta Dal immesso        08
062000950530Foglio Distinta non utilizzabile                                              09
062100950530Numero Distinta Al deve essere maggiore o uguale al numero Distinta Dal       10
062200970417Codice Autotrasportatore inesistente                                          11
062300160524P.O. Autotrasportatore incongruente con punto operativo apertura foglio       12
062400160524Codice Autotrasportatore incongruente con il numero Distinta Dal immesso      13
062500160524Distinta non congruente con il turno                                          14
062600160524Per distinte Parcel è obbligatoria la scelta del turno                        15
062700160524Distinta non congruente con il tipo distinta consegna                         16
