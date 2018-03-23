000100950523     H DECEDIT('0,') DATEDIT(*DMY.)
000200950530     H* FNLRF2R *-----------------------------------------------------*
000300950530     H*              STAMPA FOGLI DISTINTE CONSEGNA     - BATCH -     *
000400930225     H*---------------------------------------------------------------*
000500030623     FFNARB70L  IF   E           K DISK
000600030623     FFNFVV01L  IF   E           K DISK
000700950530     FFNFVV02L  IF   E           K DISK
000800950530     F                                     RENAME(FNFVV000:FNFVV002)
000900110520     FFiDST01L  IF   E           K DISK
001000110520     FFiDST04L  IF   E           K DISK
001100110520     F                                     RENAME(FiDST000:FiDST004)
001200021203     Ffiapd01L  IF   E           K DISK
001300041006     Ffiqdt01L  IF   E           K DISK
001400950612     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001500950612     F                                     USROPN
001600950531     F                                     FORMLEN(066)
001700950531     F                                     FORMOFL(063)
001800950531     FFNLV22P   O    F  132        PRINTER USROPN
001900950524     D*
002000950524     D* DEFINIZIONE SCHIERE
002100950612     D STA             S             66    DIM(13) CTDATA PERRCD(1)             SCHIERE STAMPA
002200950612     D CM              S              1    DIM(70) CTDATA PERRCD(70)            COMAND
002300950612     D CM2             S              1    DIM(70) CTDATA PERRCD(70)            COMAND
002400950523     D*
002500950530     D* PARAMETRI PASSATI DAL FILTRO                  - FNLRF1R -
002600950530     D PARAM           DS
002700950530     D* FILIALE GESTIONE
002800950530     D  PARFGS                 1      3  0
002900950530     D* DATA FOGLIO DISTINTA IN GG/MM/AAAA
003000950530     D  PARDFV                 4     11  0
003100950530     D* DATA FOGLIO DISTINTA IN AAAA/MM/GG
003200950530     D  PARDFR                12     19  0
003300950530     D* NUMERO FOGLIO DISTINTA DAL
003400950530     D  PARNFD                20     24  0
003500950530     D* NUMERO FOGLIO DISTINTA  AL
003600950530     D  PARNFA                25     29  0
003700950530     D* CODICE PADRONCINO
003800950530     D  PARPDR                30     36  0
003900950530     D* PRESTAZIONE PADRONCINO
004000950530     D  PARFPP                37     37
004100950530     D* TIPO STAMPA
004200950531     D  PARTST                38     38
004300950612     D* NUMERO COPIE
004400950612     D  PARNCP                39     41  0
004500950913     D* STAMPA DEL NUMERO BOLLE SU EITCHETTE
004600950913     D  PARSNB                42     42
004700160525     D* parcel/messaggerie/tutte
004800160525     D  PARtpm                43     43
004900160525     D* turno 1 2
005000160525     D  PARtur1               44     44
005100160525     D  PARtur2               45     45
005200950607     D* STAMPANTE E MODULO ETICHETTE
005300950607     D  PARPRE               101    110
005400950607     D  PARMDE               111    120
005500950908     D* NOME PGM PER STAMPA ETICHETTE DISTINTA
005600950908     D  PARPSD               121    130
005700021203     D* DS PER FNLV25R - STAMPA ETICHETTA PADRONCINI
005800021203     D DSLV25        E DS                  EXTNAME(FNLV25DS)
005900160525     d ddstflr       e ds
006000950524     D*
006100950525     D                 DS
006200950525     D  AA                     1      4  0
006300950525     D  MM                     5      6  0
006400950525     D  GG                     7      8  0
006500950525     D  WDATA                  1      8  0
006600950523     D*
006700900514     D KPJBA         E DS
006800950523     D CNCR80        E DS
006900950523     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007000950531     D*
007100950531     D* DEFINIZIONE COSTANTI
007200950531     D CMAT            C                   CONST('MATTINO   ')
007300950531     D CPOM            C                   CONST('POMERIGGIO')
007400950523     C*****************************************************************
007500950523     C* RIEPILOGO INDICATORI
007600950523     C*****************************************************************
007700950530     C* 03    - E' STATO IMMESSO IL NUMERO FOGLIO DISTINTA AL
007800950530     C* 05    - OFF  FOGLIO DISTINTA DA STAMPARE
007900950531     C* 20    - STAMPO TESTATA
008000950530     C* 30/32 - DI COMODO
008100950531     C* 90    - INDICATORE QCMDEXC
008200950523     C*****************************************************************
008300950531     C*
008400950531     C* 90 OFF - OVRPRTF SUL FILE FNLV22P ANDATA A BUON FINE
008500950531    0C     *IN90         IFEQ      *OFF
008600950531     C*
008700950530     C* 03 ON - E' STATO IMMESSO IL NUMERO FOGLIO DISTINTA AL
008800950530    1C     PARNFA        IFGT      0
008900950530     C     PARNFA        ANDNE     PARNFD
009000950530     C                   SETON                                        03
009100950530    1C                   ENDIF
009200950530     C*
009300950530     C****  D A T A    F O G L I O    D I S T I N T A  =  0  ****
009400950530    1C     PARDFV        IFEQ      0
009500950530     C*
009600950530     C****  N U M E R O    D I S T I N T A    D A L    =  0  ****
009700950530    2C     PARNFD        IFEQ      0
009800950530     C*
009900950530     C* E' STATO PER FORZA INSERITO IL CODICE PADRONCINO
010000110520     C     kdst4         SETLL     Fidst04L
010100110520     C     kdst4         READE     Fidst04L                               30
010200950530     C*
010300950530    3C     *IN30         DOWEQ     *OFF
010400950530     C* CONTROLLO CHE IL FOGLIO DISTINTA NON SIA ANNULLATO
010500950530    4C     DSTATB        IFEQ      ' '
010600950530     C*
010700041006      * file rientri per bancali affidati
010800041006     C                   z-add     DSTNFV        WNFV6
010801170714     C                   z-add     DSTdFV        Wdfv
010900041006     C                   z-add     DSTFGS        WFGSqdt
011000160525     c                   movel     dstflr        ddstflr
011100160525     c* controllo se parcel/messaggerie
011200160526     c                   if        partpm <> ' '
011300160526     c                   if        (dsttpm = ' ' and partpm <> 'P') or
011400160526     c                             (dsttpm = 'M' and partpm <> 'M')
011500160526     c                   seton                                        31
011600160526     c                   end
011700160526     c                   end
011800160525     c* controllo turno
011900160526     c                   if        (partur1 = ' ' or partur2 = ' ')
012000160526     c                             and dsttpm= ' '
012100160526     c                   select
012200160526     c                   when      (partur1 = '1' or
012300160526     c                              partur2 = '1') and
012400160526     c                             §dstturno <> ' '
012500160526     C                   seton                                        31
012600160526     c                   when      (partur1 = '2' or
012700160526     c                              partur2 = '2') and
012800160526     c                             §dstturno  <> '2'
012900160526     C                   seton                                        31
013000160525     c                   endsl
013100160525     c                   end
013200041006     C     Kqdt          chain     Fiqdt01l                           33
013300950530     C* CONTROLLO CHE IL FOGLIO DISTINTA NON SIA CHIUSO
013400120404     C                   MOVE      DSTNFV        WNFV
013500950530     C                   MOVEL     DSTFGS        WFGS
013600950530     C     KFVV          CHAIN     FNFVV01L                           31
013700950530    5C  N31FVVFCF        IFEQ      ' '
013800950530     C*
013900950530     C* SE IMMESSO CONTROLLO FLAG PRESTAZIONE PADRONCINO
014000950530    6C     PARFPP        IFNE      ' '
014100950530     C     PARFPP        ANDNE     FVVFPP
014200950530     C                   SETON                                        31
014300950530    6C                   ENDIF
014400950530     C*
014500950530     C* STAMPO IL FOGLIO DISTINTA
014600950530     C  N31              EXSR      STAMPA
014700950530    5C                   ENDIF
014800950530    4C                   ENDIF
014900950530     C*
015000110520     C     kdst4         READE     Fidst04L                               30
015100950530    3C                   ENDDO
015200950530     C*
015300950530     C****
015400950530   X2C                   ELSE
015500950530     C****
015600950530     C****  N U M E R O    D I S T I N T A    D A L    >  0  ****
015700950530     C*
015800950530     C                   MOVEL     PARFGS        WFGS
015900950524     C*
016000950530    3C     PARNFD        DOWLE     PARNFA
016100950530     C*
016200950531     C                   MOVEL     PARNFD        WNFV
016300950530     C     KFVV          CHAIN     FNFVV01L                           30
016400950530     C*
016500950530     C* SE IMMESSO CONTROLLO FLAG PRESTAZIONE PADRONCINO
016600950530    4C  N30PARFPP        IFNE      ' '
016700950530     C     PARFPP        ANDNE     FVVFPP
016800950530     C                   SETON                                        30
016900950530    4C                   ENDIF
017000950530     C*
017100950530     C* CONTROLLO CHE IL FOGLIO DISTINTA NON SIA ANNULLATO O CHIUSO
017200950530    4C  N30FVVATB        IFNE      ' '
017300950530     C     FVVFCF        ORNE      ' '
017400950530     C                   SETON                                        30
017500950530    4C                   ENDIF
017600041006      * file rientri per bancali affidati
017700041006     C                   z-add     fvvNFV        WNFV6
017701170714     C                   z-add     fvvdFV        Wdfv
017800041006     C                   z-add     fvvFGS        WFGSqdt
017900041006     C  n30Kqdt          chain     Fiqdt01l                           33
018000950526     C*
018100110520     C* CONTROLLO CHE IL FOGLIO DISTINTA ESISTA ANCHE IN FiDST00F
018200110520     c                   z-add     fvvnfv        dstnfv
018300110520     C  N30KDST          CHAIN     FiDST01L                           31
018400950530     C  N30
018500950530     CAN 31              SETON                                        30
018600160525     c                   movel     dstflr        ddstflr
018700160525     c* controllo se parcel/messaggerie
018800160525     c                   if        partpm <> ' '
018900160525     c                   if        (dsttpm = ' ' and partpm <> 'P') or
019000160525     c                             (dsttpm = 'M' and partpm <> 'M')
019100160525     c                   seton                                        30
019200160525     c                   end
019300160525     c                   end
019400160525     c* controllo turno
019500160525     c                   if        (partur1 = ' ' or partur2 = ' ')
019600160525     c                             and dsttpm= ' '
019700160525     c                   select
019800160525     c                   when      (partur1 = '1' or
019900160525     c                              partur2 = '1') and
020000160525     c                             §dstturno <> ' '
020100160525     C                   seton                                        30
020200160525     c                   when      (partur1 = '2' or
020300160525     c                              partur2 = '2') and
020400160525     c                             §dstturno  <> '2'
020500160525     C                   seton                                        30
020600160525     c                   endsl
020700160525     c                   end
020800950530     C*
020900950530     C* SE IMMESSO CONTROLLO CODICE PADRONCINO
021000950530    4C  N30PARPDR        IFGT      0
021100950530     C     PARPDR        ANDNE     DSTPDR
021200950530     C                   SETON                                        30
021300950530    4C                   ENDIF
021400950530     C*
021500950530     C* STAMPO IL FOGLIO DISTINTA
021600950530     C  N30              EXSR      STAMPA
021700950530     C*
021800950530     C                   ADD       1             PARNFD
021900950530    3C                   ENDDO
022000950530     C*
022100950530    2C                   ENDIF
022200950526     C*
022300950530     C****
022400950530   X1C                   ELSE
022500950530     C****
022600950530     C****  D A T A    F O G L I O    D I S T I N T A  >  0  ****
022700950530     C*
022800950530     C     KFVV2         SETLL     FNFVV02L
022900950530     C     KFVV2         READE     FNFVV02L                               30
023000950530     C*
023100950530    2C     *IN30         DOWEQ     *OFF
023200950530     C                   SETOFF                                       05
023300950530     C*
023400950530     C* CONTROLLO CHE IL FOGLIO DISTINTA NON SIA ANNULLATO O CHIUSO
023500950530    3C     FVVATB        IFEQ      ' '
023600950530     C     FVVFCF        ANDEQ     ' '
023700950530     C*
023800950530     C* 03 OFF - NUMERO FOGLIO DISTINTA DAL SENZA NUMERO AL
023900950530    4C  N03PARNFD        IFGT      0
024000950530     C     PARNFD        ANDNE     FVVNFV
024100950530     C                   SETON                                        05
024200950530    4C                   ENDIF
024300950530     C*
024400950530     C* 03 ON  - NUMERO FOGLIO DISTINTA DAL E NUMERO AL
024500950530     C  N05
024600950530    4CAN 03FVVNFV        IFLT      PARNFD
024700950530     C                   SETON                                        05
024800950530    4C                   ENDIF
024900950530     C*
025000950530     C  N05
025100950530    4CAN 03FVVNFV        IFGT      PARNFA
025200950530     C                   SETON                                        05
025300950530    4C                   ENDIF
025400950530     C*
025500950530     C* SE IMMESSO CONTROLLO FLAG PRESTAZIONE PADRONCINO
025600950530    4C  N05PARFPP        IFNE      ' '
025700950530     C     PARFPP        ANDNE     FVVFPP
025800950530     C                   SETON                                        05
025900950530    4C                   ENDIF
026000041006      * file rientri per bancali affidati
026100041006     C                   z-add     fvvNFV        WNFV6
026101170714     C                   z-add     fvvdFV        Wdfv
026200041006     C                   z-add     fvvFGS        WFGSqdt
026300041006     C  n05Kqdt          chain     Fiqdt01l                           33
026400950530     C*
026500110520     C* CONTROLLO CHE IL FOGLIO DISTINTA ESISTA ANCHE IN FiDST00F
026600110520     c                   z-add     fvvnfv        dstnfv
026700110520     C  N05KDST          CHAIN     FiDST01L                           31
026800950530     C  N05
026900950530     CAN 31              SETON                                        05
027000950530     C*
027100160525     c                   movel     dstflr        ddstflr
027200160525     c* controllo se parcel/messaggerie
027300160525     c                   if        partpm <> ' '
027400160525     c                   if        (dsttpm = ' ' and partpm <> 'P') or
027500160525     c                             (dsttpm = 'M' and partpm <> 'M')
027600160525     c                   seton                                        05
027700160525     c                   end
027800160525     c                   end
027900160525     c* controllo turno
028000160525     c                   if        (partur1 = ' ' or partur2 = ' ')
028100160525     c                             and dsttpm= ' '
028200160525     c                   select
028300160525     c                   when      (partur1 = '1' or
028400160525     c                              partur2 = '1') and
028500160525     c                             §dstturno <> ' '
028600160525     C                   seton                                        05
028700160525     c                   when      (partur1 = '2' or
028800160525     c                              partur2 = '2') and
028900160525     c                             §dstturno  <> '2'
029000160525     C                   seton                                        05
029100160525     c                   endsl
029200160525     c                   end
029300950530     C* SE IMMESSO CONTROLLO CODICE PADRONCINO
029400950530    4C  N05PARPDR        IFGT      0
029500950530     C     PARPDR        ANDNE     DSTPDR
029600950530     C                   SETON                                        05
029700950530    4C                   ENDIF
029800950530     C*
029900950530     C  N05              EXSR      STAMPA
030000950530    3C                   ENDIF
030100950530     C*
030200950530     C     KFVV2         READE     FNFVV02L                               30
030300950530    2C                   ENDDO
030400950530     C*
030500950530    1C                   ENDIF
030600950531     C*
030700950531     C* STAMPO TESTATA
030800950531    1C     *IN20         IFEQ      *OFF
030900950531     C                   EXCEPT    TES
031000950531     C                   EXCEPT    NOSTA
031100950531    1C                   ENDIF
031200950531     C* SE RICHIESTA SOLO STAMPA ETICHETTA NON STAMPO MESSAGGIO DI FINE
031300950531    1C     PARTST        IFNE      'E'
031400950531     C                   EXCEPT    FINES
031500950531    1C                   ENDIF
031600950531    0C                   ENDIF
031700950530     C*
031800950530     C                   SETON                                        LR
031900950523     C*
032000950530     C*--- INZSR INIZIALE --------------------------------------------*
032100950530     C     *INZSR        BEGSR
032200950530     C*
032300950530     C     *ENTRY        PLIST
032400950530     C                   PARM                    KPJBA
032500950530     C                   Z-ADD     1             CODUT
032600950530     C                   CALL      'X§PARUT'
032700950530     C                   PARM                    UT§DSE
032800950530     C                   MOVEL     RAGUT         RSUT             20
032900950530     C                   MOVEL     REC80         CNCR80
033000950530     C                   MOVEL     KPJBU         PARAM
033100950531     C**
033200950531     C* OVRPRTF SU FNLV22P
033300950531     C**
033400950607     C                   MOVEA     PARPRE        CM(60)
033500950612     C                   Z-ADD     70            LUNG             15 5
033600950612     C                   MOVEA     CM            COMAND           82
033700950531     C                   CALL      'QCMDEXC'                            90
033800950531     C                   PARM                    COMAND
033900950531     C                   PARM                    LUNG
034000950531     C* POSSO APRIRE IL FILE
034100950531     C  N90              OPEN      FNLV22P
034200950612     C**
034300950612     C* OVRPRTF SU FNLV22P
034400950612     C**
034500950612     C                   MOVEL     PARNCP        W003A             3
034600950612     C                   MOVEA     W003A         CM2(50)
034700950612     C                   MOVEA     CM2           COMAND           82
034800950612     C                   CALL      'QCMDEXC'                            90
034900950612     C                   PARM                    COMAND
035000950612     C                   PARM                    LUNG
035100950612     C* POSSO APRIRE IL FILE
035200950612     C  N90              OPEN      QSYSPRT
035300950530     C***
035400950530     C* LA DATA DEL GIORNO LA PRENDO DA TIME
035500950530     C***
035600950530     C                   TIME                    W0140            14 0
035700950530     C                   MOVE      W0140         UDATE8            8 0
035800950530     C                   MOVEL     W0140         UTIME             6 0
035900950530     C*
036000950530     C                   Z-ADD     4             WNPG
036100950530     C                   SETON                                        OF
036200950531     C* PRESTAZIONE PADRONCINO
036300950531     C                   CLEAR                   WFPP
036400950531     C     PARFPP        IFEQ      'M'
036500950531     C                   MOVEL     CMAT          WFPP             10
036600950531     C                   ENDIF
036700950531     C     PARFPP        IFEQ      'P'
036800950531     C                   MOVEL     CPOM          WFPP
036900950531     C                   ENDIF
037000950531     C*
037100950530     C***
037200950530     C* DEFINIZIONE CAMPI
037300950530     C***
037400950530     C     *LIKE         DEFINE    FVVNPG        WNPG
037500950530     C     *LIKE         DEFINE    FVVNFV        WNFV
037600950530     C     *LIKE         DEFINE    FVVFGS        WFGS
037700950530     C     *LIKE         DEFINE    FVVDFV        WDFV
037800950531     C     *LIKE         DEFINE    APDRSC        WDPDR2
037900030623     C     *LIKE         DEFINE    arbpkf        stppkf
038000030623     C     *LIKE         DEFINE    arbvlf        stpvlf
038100950531     C***
038200950531     C* KLIST
038300950531     C***
038400950530     C* ACCESSO FNFVV01L
038500950530     C     KFVV          KLIST
038600950530     C                   KFLD                    WNPG
038700950530     C                   KFLD                    WNFV
038800950530     C                   KFLD                    WFGS
038900021203     C* ACCESSO FIAPD01L
039000021203     C     KAPDD         KLIST
039100021203     C                   KFLD                    apdtip
039200021203     C                   KFLD                    dstpdr
039300021203     c                   movel     'A'           apdtip
039400950530     C* ACCESSO FNFVV02L
039500950530     C     KFVV2         KLIST
039600950530     C                   KFLD                    WNPG
039700950530     C                   KFLD                    PARDFR
039800950530     C                   KFLD                    PARFGS
039900110520     C* ACCESSO FiDST01L
040000950530     C     KDST          KLIST
040100110520     C                   KFLD                    wnpg
040200110520     C                   KFLD                    dstNFV
040300950530     C                   KFLD                    PARFGS
040400110520     C     KDST4         KLIST
040500110520     C                   KFLD                    dsttipa
040600110520     C                   KFLD                    PARpdr
040700110520     c                   eval      dsttipa= 'A'
040800030623     C* ACCESSO FNARB70L
040900030623     C     KARB          KLIST
041000030623     C                   KFLD                    ARBIFP
041100030623     C                   KFLD                    ARBNDC
041200030623     C                   KFLD                    FVVDFV
041300041006     C* ACCESSO Fiqdt01l
041400041006     C     Kqdt          klist
041500041006     C                   kfld                    wfgsqdt           3 0
041600041006     C                   kfld                    wnfv6             6 0
041601170714     C                   kfld                    wdfv              8 0
041700041006
041800950530     C                   ENDSR
041900950523     C*
042000950530     C*--- ROUTINE DI STAMPA -----------------------------------------*
042100950530     C     STAMPA        BEGSR
042200950531     C                   SETON                                        20
042300950530     C*
042400950530     C* GIRO DATA FOGLIO DISTINTA
042500950530     C                   Z-ADD     FVVDFV        WDATA
042600950530     C                   Z-ADD     MM            GGMM              4 0
042700950530     C                   MOVEL     GG            GGMM
042800950530     C                   Z-ADD     AA            WDFV
042900950530     C                   MOVEL     GGMM          WDFV
043000950530     C*
043100950530     C* DECODIFICA CODICE PADRONCINO
043200021203     C     kapdd         CHAIN     fiapd01L                           32
043300950530    1C     *IN32         IFEQ      *OFF
043400950612     C                   MOVEL     APDRSC        WDPDR            20
043500950531     C                   MOVEL     APDRSC        WDPDR2
043600950530   X1C                   ELSE
043700950530     C                   CLEAR                   WDPDR
043800950531     C                   CLEAR                   WDPDR2
043900950530    1C                   ENDIF
044000950530     C*
044100950530     C****  CONTROLLO IL TIPO DI STAMPA RICHIESTO  ****
044200950607     C* PARTST = "L" --> RICHIESTA LA STAMPA DEL FOGLIO DISTINTA
044300950607    1C     PARTST        IFEQ      'L'
044400950530     C     PARTST        OREQ      ' '
044500950530     C* STAMPO TESTATA
044600950530    2C     *INOF         IFEQ      *ON
044700950530     C                   EXCEPT    TES
044800950530     C                   SETOFF                                       OF
044900950530    2C                   ENDIF
045000030623     c*calcolo numero bolle, peso e volume
045100030623     c                   exsr      srbolle
045200950530     C* STAMPO DETTAGLIO
045300950530     C                   EXCEPT    DET
045400950530    1C                   ENDIF
045500950530     C*
045600950530     C* PARTST = "E" --> RICHIESTA LA STAMPA ETICHETTA
045700950530    1C     PARTST        IFEQ      'E'
045800950530     C     PARTST        OREQ      ' '
045900950530     C                   CLEAR                   DSLV25
046000950530     C                   MOVEL     DSTPDR        D25PDR
046100950531     C                   MOVEL     WDPDR2        D25RSC
046200950913     C     PARSNB        IFEQ      'S'
046300030623     C                   MOVEL     stpNBL        D25NBL
046400950913     C                   ENDIF
046500120404     C                   MOVE      DSTNFV        D25NFV
046600950530     C                   MOVEL     WDFV          D25DDC
046700080212     c                   movel     FVVfpp        D25fpp
046800950530     C                   MOVEL     DSTNOT        D25NOT
046900950908     C                   CALL      PARPSD
047000950530     C                   PARM                    DSLV25
047100950530    1C                   ENDIF
047200950530     C*
047300000619     C     NOSTAM        ENDSR
047400000619     C*--------------------------------------------------------------*
047500030623     C*  calcolo numero bolle peso e volume
047600000619     C*--------------------------------------------------------------*
047700030623     C     srbolle       BEGSR
047800110520     c                   clear                   stpnbl            5 0
047900030623     c                   clear                   stppkf
048000030623     c                   clear                   stpvlf
048100030623     C                   Z-ADD     FVVNFV        ARBNDC
048200030623     C                   Z-ADD     FVVFGS        ARBIFP
048300030623     c     karb          setll     fnarb70l
048400030623     c                   do        *hival
048500030623     c     karb          reade     fnarb70l
048600030623     c                   if        %eof(fnarb70l)
048700030623     c                   leave
048800030623     c                   end
048900030623     C                   ADD       1             STPNBL
049000030623     C                   ADD       ARBPKF        STPPKF
049100030623     C                   ADD       ARBVLF        STPVLF
049200030623     c                   enddo
049300030623     C                   ENDSR
049400950530     C*---------------------------------------------------------------*
049500950530     OQSYSPRT   E            TES              02
049600950530     O                       STA(1)              66
049700950530     O                       STA(2)             132
049800950530     O                       RSUT                20
049900950530     O                       SIMFEL              24
050000950531     O                       UDATE8             118 '  /  /    '
050100950530     O                       PAGE          Z    132
050200950530     O          E            TES         1
050300950531     O                       UTIME              116 '  :  :  '
050400950531     O          E            TES         1
050500950531     O                       STA(3)              66
050600950531     O                       PARPDR        Z     19
050700950531     O                       PARDFV              53 '  /  /    '
050800950531     O                       PARNFD        Z     72
050900950531     O                                           77 'AL'
051000950531     O                       PARNFA        Z     83
051100950531     O                       WFPP               100
051200950531     O          E            TES         2
051300950531     O                       STA(4)              66
051400950531     O                       STA(7)             132
051500950612     O          E            TES         1  1
051600950531     O                       STA(5)              66
051700950531     O                       STA(8)             132
051800950530     O*
051900030623     O          E            DET         1  1
052000950531     O                       DSTPDR               7
052100950531     O                                            8 '-'
052200950612     O                       WDPDR               28
052300950612     O                       DSTNFV        Z     35
052400110520     O                       qdtnbnaf      Z     45
052500030623     O                       stpnbl        Z     56
052600030623     O                       stppkf        4     +6
052700030623     O                       stpvlf        4     +6
052800030623     O                       DSTNOT              +6
052900950531     O          E            NOSTA            20
053000950531     O                       STA(10)             66
053100950531     O                       STA(11)            132
053200950531     O          E            FINES            65
053300950531     O                                           80 '***   FINE  STAMPA   ***'
053400950531     O*
053500950531     OFNLV22P   T    LRNLR
053600950612**
053700950612XXXXXXXXXXXXXXXXXXXX/XXX                ***   RIEPILOGO   DISTINTE   01
053800950612   CONSEGNA   ***             FNLRF2R     XX/XX/XXXX     PAG. XXXX   02
053900970417AUTOTRASP.:                  DISTINTE: DEL              NUMERO DAL   03
054000030623  AUTOTRASPORTATORE          DISTINTA  NUMERO      NUMERO
054100950612                                       BANCALI     BOLLE
054200030623XXXXXXX-XXXXXXXXXXXXXXXXXXXX  XXXXX     XXXXX      XXXXX      XXXX
054300030623PESO      VOLUME      N  O  T  E
054400030623
054500030623XX,X      XX,XXX      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
054600950531              N O N   E '   S T A T A   T R O V A T A   N E S S U    10
054700950531N A    D I S T I N T A    D A    S T A M P A R E  ! !                11
054800950607** Comandi    567890123456789012345678901234567890123456789012345678901234567890
054900950607OVRPRTF FILE(FNLV22P) USRDTA('Etic.Padr') SHARE(*YES) OUTQ(          )
055000950612** Comandi    567890123456789012345678901234567890123456789012345678901234567890
055100950612OVRPRTF FILE(QSYSPRT) USRDTA('Riep.Dist') copies(   )
