000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941109     H* FNLR71R *----------------------------------------------------*
000300920422     H*             QUADRATURA DISTINTA PADRONCINO       BATCH       *
000400000000     H*--------------------------------------------------------------*
000500021203     F*FNAPD01L  IF   E           K DISK
000600021203     FFIAPD01L  IF   E           K DISK
000700941109     FFNARB01L  IF   E           K DISK
000800020911     FFNARB78L  IF   E           K DISK
000900020911     F                                     RENAME(FNARB000:FNARB078)
001000941109     FFNART01L  IF   E           K DISK
001100941109     FFNART27L  IF   E           K DISK
001200941109     F                                     RENAME(FNART000:FNART027)
001300070122     FFNBRV01L  IF   E           K DISK
001400070122     FFNBRV09L  IF   E           K DISK
001500070122     F                                     RENAME(FNBRV000:FNBRV009)
001600941109     FFNFVV01L  IF   E           K DISK
001700941109     FFNLBL01L  IF   E           K DISK
001800941109     FFNLBL02L  IF   E           K DISK
001900941109     F                                     RENAME(FNLBL000:FNLBL002)
002000920831     FAZORG01L  IF   E           K DISK
002100070122     FFNBRVE1L  IF   E           K DISK
002200070122     F                                     RENAME(FNBRV000:FNBRVE01)
002300070122     F                                     prefix(BRE:3)
002400070122     FFNBRVE2L  IF   E           K DISK
002500070122     F                                     RENAME(FNBRV000:FNBRVE02)
002600070122     F                                     prefix(BRE:3)
002700010618     FTABEL00F  IF   E           K DISK
002800120419     FFiDST01L  IF   E           K DISK
002900941110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003000941110     F                                     USROPN
003100010712     F                                     INFDS(PRTFBK)
003200020911
003300020911     d Karbifp         s                   Like(ArbIfp)
003400021203      *
003500021203     d kpdpdr          S                   LIKE(APDpdr)
003600021203     d kpdtip          S                   LIKE(APDtip)
003700020911
003800941110     D STA             S             66    DIM(13) CTDATA PERRCD(1)             SKIERE STAMPA
003900941110     D CM              S              1    DIM(45) CTDATA PERRCD(45)            COMAND
004000010703     D COS             S              1    DIM(33) CTDATA PERRCD(33)
004100010703     D W33             S              1    DIM(33)
004200010712     D W06             S              1    DIM(6)
004300911018     D                 DS
004400941109     D  ARBAAS                 1      4  0
004500941109     D  ARBMGS                 5      8  0
004600941109     D  ARBDSP                 1      8  0
004700911018     D                 DS
004800941109     D  COMAAS                 1      4  0
004900941109     D  ARBLNP                 5      7  0
005000941109     D  ARBNRS                 8      9  0
005100941109     D  ARBNSP                10     16  0
005200941109     D  ARBSPE                 1     16
005300941109     D***
005400941109     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
005500941109     D***
005600941109     D WLBDAT          DS                  INZ
005700941109     D  G02DAT                 1      8  0
005800941109     D  G02INV                 9     16  0
005900941109     D  G02ERR                17     17
006000941109     D  G02TGI                18     22  0
006100941109     D* PASSAGGIO PARAMETRI
006200941109     D* DELLA DS SONO DA PASSARE I SEGUENTI CAMPI:
006300941109     D* DLRNPG
006400941109     D* DLRNFV
006500941109     D* DLRFGS
006600941109     D DSLR01        E DS
006700941109     D*
006800941109     D CNCR80        E DS
006900920427     D KPJBA         E DS
007000900621     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007100010613     D DSLV53        E DS                  EXTNAME(FNLV53DS)
007200070122     D DSBRVE        E DS                  EXTNAME(FNBRVE0F)
007300070122     d                                     prefix(BRE:3)
007400010618     D DS7A2         E DS
007500021029      * TRUL06DS serve per call a TRUL06R
007600021029     d Trul06Ds      e ds
007700021029     d  L1                     1     90  0 Dim(30)
007800021029
007900010618     D* DS PER XDTORA  - DIFFERENZA FRA DUE DATA/ORA
008000010618     D WXDTOR          DS
008100010618     D  WXDT1                  1      8  0
008200010618     D  WXHM1                  9     12  0
008300010618     D  WXDT2                 13     20  0
008400010618     D  WXHM2                 21     24  0
008500010618     D  WXFLG                 25     25
008600010618     D  WXMIN                 26     30  0
008700010712     D PRTFBK          DS
008800010712     D  CURLIN               367    368B 0
008900930426     C*****************************************************************
009000930426     C* RIEPILOGO INDICATORI
009100930426     C*****************************************************************
009200970327     C* 01    - STAMPA SCRITTA - NON SPUNTATO IN DATA XX/XX/XXXX
009300970327     C* 02    - STAMPA SCRITTA - SPUNTATO SU ALTRA DISTINTA N.  DEL
009400970327     C* 03    - STAMPA SCRITTA - SPUNTATO NON IN DISTINTA
009500970327     C* 04    - STAMPA SCRITTA - SPUNTATO DI DISTINTA N.   DEL
009600970327     C* 05    - STAMPA SCRITTA - MANCA REC.BOLLA LETTORE BARCODE N: XX
009700950214     C* 06    - STAMPA SCRITTA - SPUNTATO SU F.  X-XXXXX DEL XX/XX
009800920422     C* 07    - ON  CAMBIATA SPEDIZIONE
009900920422     C* 08    - ON  ESISTE SERIE
010000920423     C* 09    - SEGNACOLLI NON SEQUENZIALI
010100970327     C* 10    - STAMPA SCRITTA - SPUNTATO GIA' CONSEGNATO
010200010613     C* 11    - OVERFLOW FILE PRINTER
010300920424     C* 20    - STAMPO TESTATA
010400920422     C* 30/35 - COMODO
010500911018     C*---------------------------------------------------------------*
010600941110     C     *IN90         IFEQ      *OFF
010700920909     C*
010800920909     C* LEGGO BOLLE PER DECODIFICARE COD PADRONCINO
010900020911     C     Karb78        SETLL     FNARB78L
011000020911     C     Karb78        READE     FNARB78L                               30
011100920909     C*
011200920909     C* DECODIFICA CODICE PADRONCINO CONSEGNA
011300941109     C     *IN30         DOWEQ     '0'
011400021203     C*    ARBPDC        CHAIN     FNAPD000                           31
011500021205     C                   move      arbpdc        kpdpdr
011600021203     C                   move      'A'           kpdtip
011700021203     C     kfapd         chain     fiapd01L                           31
011800941109     C     *IN31         IFEQ      *OFF
011900941109     C                   MOVEL     APDRSC        DESCPC           25
012000941109     C                   MOVEL     ARBPDC        STACPC            8
012100941109     C                   MOVE      '-'           STACPC
012200941109     C                   SETON                                        30
012300941109     C*
012400941109     C                   ELSE
012500920909     C*
012600020911     C     Karb78        READE     FNARB78L                               30
012700941109     C                   ENDIF
012800941109     C                   ENDDO
012900920909     C*
013000920909     C* SE NON TROVATO CI METTO LA DESCRIZIONE DEL FOGLIO
013100920909     C     DESCPC        IFEQ      *BLANKS
013200920909     C                   MOVEL     *BLANKS       STACPC
013300941109     C                   MOVEL     FVVDSF        DESCPC
013400920909     C                   END
013500920909     C*
013600930308     C*
013700020911     C     Karb78        SETLL     FNARB78L
013800020911     C     Karb78        READE     FNARB78L                               30
013900920417     C*
014000941109    0C     *IN30         DOWEQ     '0'
014100930308     C*
014200930308     C* ESCLUDO I SOLI INCASSO SE BOLLA E' NELLO STORICO
014300941109     C*  HANNO GIA' LA DATA CONSEGNA MERCE
014400930308     C*
014500941109    1C     ARBDCM        IFEQ      0
014600930308     C*
014700941109     C                   SETOFF                                           12
014800941109     C     KART          SETLL     FNART01L                               33
014900930728    2C     *IN33         IFEQ      '0'
015000920622     C*
015100941109     C     KART          CHAIN     FNLBL000                           34
015200920909     C*
015300920909     C* BOLLA FIGLIA LEGATA A BOLLA MAMMA
015400930728    3C     *IN34         IFEQ      '0'
015500920622     C                   SETON                                        12
015600941109     C*
015700941109     C     KLBL2         SETLL     FNART01L
015800941109     C     KLBL2         READE     FNART01L                               35
015900920909     C*
016000930728    4C     *IN35         DOWEQ     '0'
016100941109     C*
016200941109     C* CONTROLLO SEGNACOLLO MANCANTE
016300941109     C                   EXSR      CTRART
016400920909     C*
016500941109     C     KLBL2         READE     FNART01L                               35
016600941109    4C                   ENDDO
016700930728    3C                   END
016800920622     C*
016900930728   X2C                   ELSE
017000920909     C*
017100920909     C* SE E' MAMMA NON CONSIDERO TANTO CI SARA' LA FIGLIA
017200941109     C     KART          SETLL     FNLBL002                               31
017300920909     C*
017400930728    3C     *IN31         IFEQ      '0'
017500941109     C     KART          READE     FNART01L                               31
017600920909     C*
017700930728    4C     *IN31         DOWEQ     '0'
017800941109     C*
017900941109     C* CONTROLLO SE SEGNACOLLO MANCANTE
018000941109     C                   EXSR      CTRART
018100941109     C*
018200941109     C     KART          READE     FNART01L                               31
018300941109    4C                   ENDDO
018400930728    3C                   END
018500920622     C*
018600930728    2C                   END
018700930728    1C                   END
018800930308     C*
018900020911     C     Karb78        READE     FNARB78L                               30
019000941109    0C                   ENDDO
019100941109     C**
019200941109     C* LETTURA PARTENDO DALLE SPUNTE
019300941109     C**
019400941109     C                   SETOFF                                       010212
019500950214     C                   SETOFF                                       06
019600070122     C     KBRV1         SETLL     FNBRV000
019700070122     C     KBRV1         READE     FNBRV000                               33
019800920909     C*
019900941109    0C     *IN33         DOWEQ     '0'
020000920909     C*
020100021029     C     KART2         CHAIN     FNART27L
020200021029     c                   If        %Found(Fnart27l)
020300021030     c     ArtLna        Lookup    L1                                     34
020400021029     c                   EndIf
020500941109     C*
020600021029    1C**   *IN34         IFEQ      '1'
020700021029    1c                   If        Not %Found(Fnart27l) or Not *In34
020800920422     C* MANCA BOLLA
020900920421     C                   SETON                                        05
021000920421     C                   EXSR      STAMPA
021100930427     C*
021200941109   X1C                   ELSE
021300941109     C     KARB          CHAIN     FNARB01L                           05
021400920909     C*
021500920909     C   05              EXSR      STAMPA
021600920909     C*
021700941109    2C  N05              DO
021800930427     C                   Z-ADD     0             NUM3              3 0
021900930427     C*
022000941109     C     KART          SETLL     FNLBL002
022100941109     C     KART          READE     FNLBL002                               30
022200920909     C* BOLLA LEGATA
022300941109    3C     *IN30         DOWEQ     '0'
022400941109     C* RICERCA IN FNARB
022500941109     C     KLBL          CHAIN     FNARB01L                           31
022600920909     C*
022700941109    4C  N31ARBNDC        IFNE      0
022800930427     C                   SETON                                        30
022900920622     C*
023000920622     C* IN CONSEGNA SU ALTRA DISTINTA
023100941109    5C     ARBNDC        IFNE      COMNDC
023200970327     C     ARTDCM        IFEQ      0
023300941109     C                   SETON                                        0412
023400920622     C                   EXSR      STAMPA
023500970327     C                   ELSE
023600970327     C                   SETON                                        1012
023700970327     C                   EXSR      STAMPA
023800941109    5C                   ENDIF
023900970327    5C                   ENDIF
024000930427     C*
024100941109     C                   ADD       1             NUM3
024200941109    4C                   ENDIF
024300930427     C*
024400941109     C  N30KART          READE     FNLBL002                               30
024500970327    3C                   ENDDO
024600920909     C*
024700941109     C* NON TROVATO NESSUNA BOLLA FIGLIA IN COSEGNA: ERRORE
024800941109     C*  ALTRIMENTI STAMPO: BOLLA NON IN CONSEGNA
024900941109    3C     NUM3          IFGT      0
025000941109     C*
025100941109     C     ARBNDC        IFEQ      0
025200930427     C                   SETON                                        03
025300930427     C                   EXSR      STAMPA
025400941109     C                   ENDIF
025500930427     C*
025600941109   X3C                   ELSE
025700930427     C* NUM3 = 0
025800930427     C                   SETOFF                                       12
025900930427     C*
026000941109    4C     ARBNDC        IFEQ      0
026100930427     C* NON IN CONSEGNA
026200930427     C                   SETON                                        03
026300930427     C                   EXSR      STAMPA
026400941109   X4C                   ELSE
026500930427     C*
026600930427     C* IN CONSEGNA SU ALTRA DISTINTA
026700941109    5C     ARBNDC        IFNE      COMNDC
026800970327    6C     ARTDCM        IFEQ      0
026900930427     C                   SETON                                        04
027000930427     C                   EXSR      STAMPA
027100970327     C                   ELSE
027200970327     C                   SETON                                        10
027300970327     C                   EXSR      STAMPA
027400970327    6C                   END
027500970327     C*
027600970327    5C                   END
027700941109    4C                   END
027800941109    3C                   END
027900930427     C*
028000941109    2C                   END
028100941109    1C                   END
028200920909     C*
028300920421     C                   SETOFF                                       030405
028400970327     C                   SETOFF                                       1210
028500941109     C*
028600070122     C     KBRV1         READE     FNBRV000                               33
028700970327    0C                   ENDDO
028800010619     C*
028900010619     C* ELABORAZIONE PER STAMPARE SPUNTE DOPPIE
029000070122     C     KBRVE         SETLL     FNBRVE1L
029100070122     C     KBRVE         READE     FNBRVE1L                               30
029200010619    2C     *IN30         DOWEQ     *OFF
029300010619     C                   CLEAR                   WGIR
029400010619     C                   CLEAR                   SAWNPS
029500010622     C                   CLEAR                   SAWNFV
029600010622     C                   CLEAR                   SAWFGS
029700010619     C                   SETOFF                                       19
029800070122     C     KBRVE2        SETLL     FNBRVE2L
029900070122     C     KBRVE2        READE     FNBRVE2L                               31
030000010619    3C     *IN31         DOWEQ     *OFF
030100010619     C* Considero solo se data foglio >= data distinta in quadratura
030200010703    4C     DLRFGS        IFNE      BREFGS
030300010703     C     WNFV          ORNE      BRENFV
030400010619     C                   CLEAR                   DSLV53
030500010619     C                   MOVEL     'V'           D53TFO
030600010619     C                   MOVEL     DLRNPG        D53NPG
030700010619     C                   Z-ADD     SIMFEL        D53FEL
030800010703     C                   Z-ADD     BRENFV        D53NFV
030900010703     C                   Z-ADD     BREFGS        D53FGS
031000010619     C                   CALL      'FNLV53R'
031100010619     C                   PARM                    DSLV53
031200010703     C                   Z-ADD     D53DFV        SA1DFV
031300010619     C                   ELSE
031400010703     C                   Z-ADD     SAVDFV        SA1DFV
031500010619    4C                   ENDIF
031600010703    4C     DLRFGS        IFEQ      BREFGS
031700010703     C     WNFV          ANDEQ     BRENFV
031800010619     C     D53ERR        OREQ      *BLANKS
031900010703     C     SA1DFV        ANDGE     SAVDFV
032000010703    5C     SAWNFV        IFGT      *ZEROS
032100010622     C* Stessa pistola e foglio: devo verificare se superati i minuti
032200010622     c* di tolleranza previsti fra una spunta e l'altra
032300010619     C                   CLEAR                   WPRT
032400010703    6C     SAWNPS        IFEQ      BRENPS
032500010703     C     SAWNFV        ANDEQ     BRENFV
032600010703     C     SAWFGS        ANDEQ     BREFGS
032700010619     C                   CLEAR                   WXDTOR
032800010703     C                   MOVEL     BREDFS        WXDT1
032900010703     C                   MOVEL     BREHMS        WXHM1
033000010619     C                   MOVEL     SAWDFS        WXDT2
033100010619     C                   MOVEL     SAWHMS        WXHM2
033200010619     C                   MOVEL     'D'           WXFLG
033300010619     C                   CALL      'XDTORA'
033400010619     C                   PARM                    WXDTOR
033500010619     C     WXMIN         IFGT      §7AMNP
033600010619     C                   MOVEL     '1'           WPRT
033700010619     C                   ENDIF
033800010619   x6C                   ELSE
033900010622     c* se pistole o foglio diversi stampo sempre
034000010619     C                   MOVEL     '1'           WPRT              1
034100010619    6C                   ENDIF
034200010619    6C     WPRT          IFEQ      '1'
034300010619     c* se sono al primo giro di stampa devo stampare sia la spunta pre
034400010619     c* cedente che la spunta successiva
034500010619    7C     WGIR          IFEQ      ' '
034600010619     C                   MOVEL     DSBRVE        W1BRVE
034700010619     C                   MOVEL     WSBRVE        DSBRVE
034800010619     C                   Z-ADD     SAWDFV        WDFV
034900010619     C                   EXSR      SPRDOP
035000010619     C                   MOVEL     W1BRVE        DSBRVE
035100010619     C                   MOVE      '1'           WGIR              1
035200010619    7C                   ENDIF
035300010619     C                   SETON                                        19
035400010703     C                   Z-ADD     SA1DFV        WDFV
035500010619     C                   EXSR      SPRDOP
035600010619   X6C                   ELSE
035700010619     C                   MOVEL     DSBRVE        WSBRVE
035800010619    6C                   ENDIF
035900010619   X5C                   ELSE
036000010619     C* se sono al primo giro di lettura memorizzo la spunta
036100010619     C                   MOVEL     DSBRVE        WSBRVE
036200010619    5C                   ENDIF
036300010619     C* memorizzo in ogni caso il numero pistola e data/ora fisica spun
036400010622     c* e il foglio
036500010703     C                   Z-ADD     BRENPS        SAWNPS
036600010703     C                   Z-ADD     BREFGS        SAWFGS
036700010703     C                   Z-ADD     BRENFV        SAWNFV
036800010703     C                   Z-ADD     BREDFS        SAWDFS
036900010703     C                   Z-ADD     BREHMS        SAWHMS
037000010703     C                   Z-ADD     SA1DFV        SAWDFV
037100010619    4C                   ENDIF
037200070122     C     KBRVE2        READE     FNBRVE2L                               31
037300010619    3C                   ENDDO
037400010619     C* Mi posiziono sempre su un nuovo segnacollo
037500070122     C     KBRVE1        SETGT     FNBRVE1L
037600070122     C     KBRVE         READE     FNBRVE1L                               30
037700010619    2C                   ENDDO
037800010711     C* STAMPO TESTATA
037900010711     C     *IN20         IFEQ      *OFF
038000010711     C                   EXCEPT    TES
038100010711     C                   EXCEPT    TUTOK
038200010711   -1C                   ENDIF
038300010711     C                   EXCEPT    FINES
038400010619     C*
038500010619     C* RICHIAMO PGM PER CHIUSURA FILE
038600010619     C                   MOVEL     'C'           D53TLA
038700010619     C                   CALL      'FNLV53R'
038800010619     C                   PARM                    DSLV53
038900941110   -1C                   END
039000010613     C*
039100920424     C*
039200000000     C                   SETON                                        LR
039300941109     C*
039400941109     C* OPERAZIONI INIZIALI------------------------------------------*
039500010711     C     *INZSR        BEGSR
039600941110     C**
039700941110     C     *ENTRY        PLIST
039800941110     C                   PARM                    KPJBA
039900941110     C                   MOVEL     KPJBU         DSLR01
040000000126     C                   Z-ADD     DLRNFV        WNFV
040100941110     C**
040200941110     C* OVRPRTF SU QSYSPRT
040300941110     C**
040400941110     C                   Z-ADD     45            LUNG             15 5
040500941110     C                   MOVEA     CM            COMAND           80
040600941110     C                   CALL      'QCMDEXC'                            90
040700941110     C                   PARM                    COMAND
040800941110     C                   PARM                    LUNG
040900941110     C* POSSO APRIRE IL FILE
041000941110     C     *IN90         IFEQ      *OFF
041100941110     C                   OPEN      QSYSPRT
041200941110     C                   ENDIF
041300941109     C**
041400941109     C* KLIST
041500941109     C**
041600941109     C     KART          KLIST
041700941109     C                   KFLD                    ARBAAS
041800941109     C                   KFLD                    ARBLNP
041900941109     C                   KFLD                    ARBNRS
042000941109     C                   KFLD                    ARBNSP
042100941109     C     KARB          KLIST
042200941109     C                   KFLD                    ARTAAS
042300941109     C                   KFLD                    ARTLNP
042400941109     C                   KFLD                    ARTNRS
042500941109     C                   KFLD                    ARTNSP
042600020911
042700020911     c     Karb78        Klist
042800020911     c                   Kfld                    Karbifp
042900020911     c                   Kfld                    Comndc
043000020911
043100941109     C     KBRV          KLIST
043200941109     C                   KFLD                    DLRNPG
043300000126     C                   KFLD                    WNFV
043400941109     C                   KFLD                    DLRFGS
043500941109     C                   KFLD                    ARTFLS
043600941109     C                   KFLD                    ARTLNA
043700941109     C                   KFLD                    ARTNRS
043800941109     C                   KFLD                    ARTNSC
043900000126     C     KBRV1         KLIST
044000000126     C                   KFLD                    DLRNPG
044100000126     C                   KFLD                    WNFV
044200000126     C                   KFLD                    DLRFGS
044300941109     C     KBRV3         KLIST
044400941109     C                   KFLD                    DLRNPG
044500941109     C                   KFLD                    ARTFLS
044600941109     C                   KFLD                    ARTLNA
044700941109     C                   KFLD                    ARTNRS
044800941109     C                   KFLD                    ARTNSC
044900941229     C                   KFLD                    SIMFEL
045000950214     C     KBRV4         KLIST
045100950214     C                   KFLD                    WNPG
045200950214     C                   KFLD                    ARTFLS
045300950214     C                   KFLD                    ARTLNA
045400950214     C                   KFLD                    ARTNRS
045500950214     C                   KFLD                    ARTNSC
045600950214     C                   KFLD                    SIMFEL
045700010703     C     KBRV9         KLIST
045800010703     C                   KFLD                    WNPG
045900010703     C                   KFLD                    BRELNP
046000010703     C                   KFLD                    BRELNA
046100010703     C                   KFLD                    BRENRS
046200010703     C                   KFLD                    BRENSC
046300010703     C                   KFLD                    SIMFEL
046400941109     C     KART2         KLIST
046500941109     C                   KFLD                    BRVLNP
046600941109     C                   KFLD                    BRVLNA
046700941109     C                   KFLD                    BRVNRS
046800941109     C                   KFLD                    BRVNSC
046900941109     C     KFVV          KLIST
047000941109     C                   KFLD                    DLRNPG
047100941109     C                   KFLD                    DLRNFV
047200941109     C                   KFLD                    DLRFGS
047300941109     C     KFVV2         KLIST
047400950214     C                   KFLD                    BRVNPG
047500000126     C                   KFLD                    WNFV5
047600941109     C                   KFLD                    BRVFGS
047700941109     C     KLBL          KLIST
047800941109     C                   KFLD                    LBLAAN
047900941109     C                   KFLD                    LBLLPN
048000941109     C                   KFLD                    LBLNRN
048100941109     C                   KFLD                    LBLNSN
048200941109     C     KLBL2         KLIST
048300941109     C                   KFLD                    LBLAAP
048400941109     C                   KFLD                    LBLLPP
048500941109     C                   KFLD                    LBLNRP
048600941109     C                   KFLD                    LBLNSP
048700010618     C     KBRVE         KLIST
048800010618     C                   KFLD                    DLRNPG
048900010618     C                   KFLD                    WNFV
049000010618     C                   KFLD                    DLRFGS
049100010618     C     KBRVE1        KLIST
049200010618     C                   KFLD                    DLRNPG
049300010618     C                   KFLD                    WNFV
049400010618     C                   KFLD                    DLRFGS
049500010703     C                   KFLD                    BRELNP
049600010703     C                   KFLD                    BRELNA
049700010703     C                   KFLD                    BRENRS
049800010703     C                   KFLD                    BRENSC
049900010618     C     KBRVE2        KLIST
050000010703     C                   KFLD                    BRENPG
050100010703     C                   KFLD                    BRELNP
050200010703     C                   KFLD                    BRELNA
050300010703     C                   KFLD                    BRENRS
050400010703     C                   KFLD                    BRENSC
050500010618     C* ACCESSO TABEL00F
050600010618     C     KTAB          KLIST
050700010618     C                   KFLD                    CODUT
050800010618     C                   KFLD                    COD
050900010618     C                   KFLD                    KEY
051000010618     C*
051100010618     C     KDST          KLIST
051200120419     C                   KFLD                    dstnpg
051201120419     C                   KFLD                    brenfv
051300010703     C                   KFLD                    BREFGS
051301120419     c                   eval      dstnpg = 4
051400021203
051500021203     C     Kfapd         klist
051600021203     C                   kfld                    kpdtip
051700021203     C                   kfld                    kpdpdr
051800941109     C**
051900950214     C     *LIKE         DEFINE    FVVNPG        WNPG
052000000126     C     *LIKE         DEFINE    BRVNFV        WNFV
052100000126     C     *LIKE         DEFINE    FVVNFV        WNFV5
052200010618     C     *LIKE         DEFINE    DSBRVE        WSBRVE
052300010618     C     *LIKE         DEFINE    DSBRVE        W1BRVE
052400010618     C     *LIKE         DEFINE    BRVNPS        SAWNPS
052500010622     C     *LIKE         DEFINE    BRVFGS        SAWFGS
052600010622     C     *LIKE         DEFINE    BRVNFV        SAWNFV
052700010618     C     *LIKE         DEFINE    BRVDFS        SAWDFS
052800010618     C     *LIKE         DEFINE    BRVHMS        SAWHMS
052900010618     C     *LIKE         DEFINE    D53DFV        SAWDFV
053000010618     C     *LIKE         DEFINE    D53DFV        WDFV
053100010703     C     *LIKE         DEFINE    D53DFV        SA1DFV
053200010618     C     *LIKE         DEFINE    TBLCOD        COD
053300010618     C     *LIKE         DEFINE    TBLKEY        KEY
053400010712     C     *LIKE         DEFINE    BRELNP        P1CLNP
053500010712     C     *LIKE         DEFINE    BRELNA        P1CLNA
053600010712     C     *LIKE         DEFINE    BRENRS        P1CNRS
053700010712     C     *LIKE         DEFINE    BRENSC        P1CNSC
053800010712     C     *LIKE         DEFINE    BREFGS        P1CFGS
053900010712     C     *LIKE         DEFINE    BRENFV        P1CNFV
054000010712     C     *LIKE         DEFINE    FVVDFV        P1CDFV
054100010712     C     *LIKE         DEFINE    DSTPDR        P1CPDR
054200010712     C     *LIKE         DEFINE    BREDFS        P1CDFS
054300010712     C     *LIKE         DEFINE    BREHMS        P1CHMS
054400010712     C     *LIKE         DEFINE    BRENPS        P1CNPS
054500950214     C**
054600010712     C                   MOVE      *ALL'-'       RIGTRA          132
054700941109     C*
054800941109     C**
054900941109     C                   Z-ADD     1             CODUT
055000941109     C                   CALL      'X§PARUT'
055100941109     C                   PARM                    UT§DSE
055200941109     C                   MOVEL     RAGUT         RSUT             20
055300941109     C                   MOVEL     REC80         CNCR80
055400941109     C*
055500941109     C                   TIME                    W0140            14 0
055600941109     C                   MOVE      W0140         WDTGIO            8 0
055700941109     C                   MOVEL     W0140         UTIME             6 0
055800021029
055900021029      * Carico i p.o. del Terminal
056000021029     c                   Clear                   Trul06Ds
056100021029     c                   Eval      D06Cod = '£1'
056200021029     c                   Movel     Simfel        D06Key
056300021029     c                   Eval      D06Tla = 'L'
056400021029     c                   Movel(p)  Trul06Ds      Kpjbu
056500021029     c                   Call      'TRUL06R'
056600021029     c                   Parm                    Kpjba
056700021029     c                   Movel     Kpjbu         Trul06Ds
056800941109     C**
056900941109     C* IMPOSTO CAMPI FISSI
057000941109     C**
057100020911     c                   Z-Add     Dlrfgs        Karbifp
057200941109     C                   Z-ADD     DLRNFV        COMNDC            7 0
057300941110     C                   MOVEL     *ZEROS        SAVSPE           16
057400911018     C                   SETON                                        OF
057500920409     C*
057600920421     C* CHAIN E DATA DELLA DISTINTA
057700941109     C     KFVV          CHAIN     FNFVV000                           32
057800920409     C*
057900920421     C     *IN32         IFEQ      '0'
058000920409     C                   MOVE      FVVDFV        G02INV
058100920409     C                   MOVE      *ZEROS        G02DAT
058200920409     C                   MOVEL     3             G02ERR
058300941109     C                   CALL      'XSRDA8'
058400920409     C                   PARM                    WLBDAT
058500941109     C                   Z-ADD     G02DAT        STADFV            8 0
058600950214     C*
058700950214     C                   MOVEL     G02DAT        W0040             4 0
058800950214     C                   Z-ADD     G02DAT        ST2DFV            6 0
058900950214     C                   MOVEL     W0040         ST2DFV
059000950214     C*
059100950214     C                   Z-ADD     FVVDFV        SAVDFV            8 0
059200950214     C*
059300920831     C* FILIALE DI APPARTENENZA
059400941109     C     DLRFGS        CHAIN     AZORG                              32
059500941109     C                   MOVEL     DLRFGS        APPFIL            3 0
059600920831     C  N32              MOVEL     ORGDES        DESFIL           15
059700920421     C                   END
059800010618     C* Chaino tabella variabili pgm arrivi per reperire minuti di
059900010618     C* tolleranza
060000010618     C                   MOVEL     '7A'          COD
060100010618     C                   MOVEL(P)  '2       '    KEY
060200010618     C     KTAB          CHAIN     TABEL                              32
060300010618     C  N32              MOVEL     TBLUNI        DS7A2
060400010618     C   32              Z-ADD     1             §7AMNP
060500010712     C*
060600010712     C                   Z-ADD     0             CURLIN
060700920421     C*
060800911018     C                   ENDSR
060900911015     C*
061000920421     C* STAMPO ERRORE -----------------------------------------------*
061100900621     C     STAMPA        BEGSR
061200920424     C                   SETON                                        20
061300920424     C*
061400911015     C* STAMPA TESTATA
061500911015     C   OF              DO
061600920422     C                   SETON                                        07
061700911018     C                   EXCEPT    TES
061800010711     C                   EXCEPT    TES1
061900911018     C                   SETOFF                                       OF
062000911018     C                   END
062100911018     C*
062200920422     C* 05 OFF - STAMPO DATI SPEDIZIONE
062300920422     C  N05              DO
062400920422     C*
062500911018     C                   Z-ADD     ARBAAS        COMAAS
062600911018     C     SAVSPE        IFNE      ARBSPE
062700920422     C                   SETON                                        07
062800911018     C                   MOVEL     ARBSPE        SAVSPE
062900911018     C                   END
063000911018     C* DATA SPEDIZIONE
063100920422     C   07              DO
063200911018     C                   MOVE      ARBDSP        G02INV
063300911018     C                   MOVE      *ZEROS        G02DAT
063400911018     C                   MOVEL     3             G02ERR
063500941109     C                   CALL      'XSRDA8'
063600911018     C                   PARM                    WLBDAT
063700920422     C                   MOVEL     G02DAT        STADSP            4 0
063800970327     C*
063900970327     C* ANNO SPEDIZIONE A 2 CIFRE
064000970327     C                   MOVE      ARBAAS        STAAAS            2 0
064100920409     C*
064200920619     C* RAGIONE SOCIALE DESTINATARIO
064300920619     C                   MOVEL     ARBRSD        STARSD           22
064400920619     C*
064500920423     C* SEGNACOLLI NON CONSECUTIVI
064600920423     C     ARBFNS        COMP      'S'                                    09
064700920423     C*
064800920421     C* SERIE >< 0
064900920422     C     ARBNRS        COMP      0                                  0808
065000911018     C                   END
065100920409     C*
065200920409     C* DATA ARRIVO MERCE  SEGNACOLLO
065300920409     C                   MOVE      ARTDAM        G02INV
065400920409     C                   MOVE      *ZEROS        G02DAT
065500920409     C                   MOVEL     3             G02ERR
065600941109     C                   CALL      'XSRDA8'
065700920409     C                   PARM                    WLBDAT
065800970327     C                   MOVEL     G02DAT        STADAM            4 0
065900920423     C*
066000920423     C                   END
066100920506     C*
066200920506     C* 04 ON - ECCEDENTE DI ALTRA DISTINTA  - PRENDO DATA
066300920506     C   04              DO
066400920506     C                   MOVE      ARBDDC        G02INV
066500920506     C                   MOVE      *ZEROS        G02DAT
066600920506     C                   MOVEL     3             G02ERR
066700941109     C                   CALL      'XSRDA8'
066800920506     C                   PARM                    WLBDAT
066900920506     C                   MOVEL     G02DAT        STADDC            4 0
067000941109     C                   Z-ADD     ARBNDC        STANDC            5 0
067100920506     C                   END
067200970327     C     *IN10         IFEQ      *ON
067300970327     C                   MOVE      ARTDCM        G02INV
067400970327     C                   MOVE      *ZEROS        G02DAT
067500970327     C                   MOVEL     3             G02ERR
067600970327     C                   CALL      'XSRDA8'
067700970327     C                   PARM                    WLBDAT
067800970327     C                   MOVEL     G02DAT        STADCM            4 0
067900970327     C                   END
068000920506     C*
068100920622     C* 02 ON - MANCANTE SU ALTRA DISTINTA SPUNTATO - PRENDO DATA
068200941110     C     *IN02         IFEQ      *ON
068300950214     C     *IN06         OREQ      *ON
068400920506     C                   MOVE      FVVDFV        G02INV
068500920506     C                   MOVE      *ZEROS        G02DAT
068600920506     C                   MOVEL     3             G02ERR
068700941109     C                   CALL      'XSRDA8'
068800920506     C                   PARM                    WLBDAT
068900920506     C                   MOVEL     G02DAT        STADDC            4 0
069000920506     C                   END
069100910522     C*
069200900622     C* STAMPO DETTAGLIO
069300911018     C                   EXCEPT    DET
069400920422     C                   SETOFF                                       07
069500900622     C*
069600900622     C                   ENDSR
069700941109     C*
069800941109     C* CONTROLLO SE SEGNACOLLO MANCANTE-----------------------------*
069900941109     C     CTRART        BEGSR
070000941109     C*
070100941110     C* ELABORO SOLO I COLLI NON CONSEGNATI ED ARRIVATI
070200941109    1C     ARTDCM        IFEQ      0
070300950214     C                   SETOFF                                       010206
070400941109     C*
070500070122     C     KBRV          CHAIN     FNBRV01L                           36
070600941109     C* SPUNTA NON TROVATA
070700941109    2C     *IN36         IFEQ      *ON
070800941109     C*
070900941109     C* VEDO SE E' PRESENTE UNA SPUNTA SU UN'ALTRA DISTINTA
071000070122     C     KBRV3         CHAIN     FNBRV09L                           36
071100941109     C*
071200941109     C* COLLO NON PRESENTE IN NESSUNA DISTINTA
071300941109    3C     *IN36         IFEQ      *ON
071400941109     C                   SETON                                        01
071500941109   X3C                   ELSE
071600941109     C* SE DISTINTA CHIUSA COME SE NON FOSSE PRESENTE
071700000126     C                   Z-ADD     BRVNFV        WNFV5
071800941109     C     KFVV2         CHAIN     FNFVV000                           31
071900941109    4C     *IN31         IFEQ      *ON
072000941109     C     FVVFCF        ORNE      ' '
072100941109     C                   SETON                                        01
072200941109   X4C                   ELSE
072300941109     C                   SETON                                        02
072400941109    4C                   ENDIF
072500941109    3C                   ENDIF
072600950214     C*
072700950214     C* 01 ON - VERIFICO SE SPARATO DA QUALCHE ALTRA PARTE NEL GIORNO
072800950214     C*         DELLA DISTINTA
072900950214    3C     *IN01         IFEQ      *ON
073000950214     C                   Z-ADD     3             WNPG                           INVENTARIO
073100950214     C                   EXSR      CHABRV
073200950214     C                   ENDIF
073300950214    3C     *IN01         IFEQ      *ON
073400950214     C                   Z-ADD     6             WNPG                           ARRIVI
073500950214     C                   EXSR      CHABRV
073600950214     C                   ENDIF
073700950214    3C     *IN01         IFEQ      *ON
073800950214     C                   Z-ADD     2             WNPG                           ARRIVI
073900950214     C                   EXSR      CHABRV
074000950214     C                   ENDIF
074100950214    3C     *IN01         IFEQ      *ON
074200950214     C                   Z-ADD     5             WNPG                           ENTRATA
074300950214     C                   EXSR      CHABRV
074400950214     C                   ENDIF
074500941109     C*
074600941109     C                   EXSR      STAMPA
074700941109     C*
074800941109    2C                   END
074900941109    1C                   END
075000941109     C                   ENDSR
075100950214     C*
075200950214     C* CHAIN BRV CON CATEGORIA NON 4 -------------------------------*
075300950214     C     CHABRV        BEGSR
075400950214     C*
075500070122     C     KBRV4         CHAIN     FNBRV09L                           31
075600950214     C*
075700950215    1C     *IN31         DOWEQ     *OFF
075800000126     C                   Z-ADD     BRVNFV        WNFV5
075900950215     C     KFVV2         CHAIN     FNFVV000                           32
076000950214     C*
076100950215    2C     *IN32         IFEQ      *OFF
076200950214     C     FVVDFV        ANDEQ     SAVDFV
076300950215     C                   SETON                                        0631
076400950214     C                   SETOFF                                       01
076500950214    2C                   ENDIF
076600950215     C*
076700070122     C  N31KBRV4         READE     FNBRV09L                               31
076800950215    1C                   ENDDO
076900950214     C                   ENDSR
077000010618     C*
077100010618     C* STAMPA SPUNTA DOPPIA ----------------------------------------*
077200010618     C     SPRDOP        BEGSR
077300010703     C                   CLEAR                   P1CMSG
077400010711     C* PER OVERFLOW O PRIMA VOLTA STAMPO INTESTAZIONE
077500010711     C     *INOF         IFEQ      *ON
077600010712     C     WFIRST        OREQ      *BLANKS
077700010712     C     CURLIN        ANDGT     55
077800010711     C                   EXCEPT    TES
077900010711     C                   EXCEPT    TESDOP
078000010711     C                   SETON                                        20
078100010712     C                   SETOFF                                       19OF
078200010712     C                   MOVE      '1'           WFIRST
078300010711     C                   ENDIF
078400010711     C* SE E' LA PRIMA RIGA DI STAMPA SPUNTE DOPPIE STAMPO TITOLO
078500010711     C* E DESCRIZIONE RIGHE
078600010711     C     WFIRST        IFEQ      *BLANKS
078700010711     C                   EXCEPT    TESDOP
078800010711     C                   MOVE      '1'           WFIRST            1
078900010711     C                   ENDIF
079000010618     C* SENGACOLLO SOLO PER 19 OFF
079100010618     C     *IN19         IFEQ      *OFF
079200010703     C                   Z-ADD     BRELNP        P1CLNP
079300010703     C                   Z-ADD     BRELNA        P1CLNA
079400010703     C                   Z-ADD     BRENRS        P1CNRS
079500010703     C                   Z-ADD     BRENSC        P1CNSC
079600010703     C* VEDO SE COLLO IN IMA
079700010703     C                   Z-ADD     3             WNPG
079800070122     C     KBRV9         SETGT     FNBRV09L
079900070122     C     KBRV9         READPE    FNBRV09L                               32
080000010703     C     *IN32         DOWEQ     *OFF
080100010703     C     P1CMSG        ANDEQ     *BLANKS
080200010703     C* IL foglio deve avere sottocategoria arrivi ed avere data foglio
080300010703     C* maggiore uguale alla data del foglio in fase di quadratura
080400010703     C                   CLEAR                   DSLV53
080500010703     C                   MOVEL     'V'           D53TFO
080600010703     C                   MOVEL     WNPG          D53NPG
080700070122     C***                Z-ADD     SIMFEL        D53FEL
080800010703     C                   Z-ADD     BRVNFV        D53NFV
080900010703     C                   Z-ADD     BRVFGS        D53FGS
081000010703     C                   CALL      'FNLV53R'
081100010703     C                   PARM                    DSLV53
081200010703     C     D53ERR        IFEQ      *BLANKS
081300010703     C     D53DFV        ANDGE     SAVDFV
081400010703     C     D53SPG        ANDEQ     'A'
081500010703     C                   MOVEA     COS           W33
081600010703     C                   MOVE      BRVFGS        W003A             3
081700010703     C                   MOVEA     W003A         W33(8)
081800010703     C                   MOVE      BRVNFV        W006A             6
081900010712     C                   EXSR      EDITA
082000010703     C                   MOVEA     W006A         W33(12)
082100010703     C                   MOVE      D53DFV        W0040             4 0
082200010703     C                   MOVEL     W0040         WMMDFV            2
082300010703     C                   MOVE      W0040         WGGDFV            2
082400010703     C                   MOVEA     WGGDFV        W33(23)
082500010703     C                   MOVEA     WMMDFV        W33(26)
082600010703     C                   MOVE      BRVNPS        W002A             2
082700010703     C                   MOVEA     W002A         W33(32)
082800010712     C                   MOVEA     W33           P1CMSG           33
082900010703     C                   ENDIF
083000070122     C     KBRV9         READPE    FNBRV09L                               32
083100010703     C                   ENDDO
083200010618     C                   ENDIF
083300010619     C*
083400010703     C                   Z-ADD     BREFGS        P1CFGS
083500010703     C                   Z-ADD     BRENFV        P1CNFV
083600010618     C* DATA DISTINTA
083700010618     C                   Z-ADD     WDFV          G02INV
083800010618     C                   MOVE      *ZEROS        G02DAT
083900010618     C                   MOVEL     3             G02ERR
084000010618     C                   CALL      'XSRDA8'
084100010618     C                   PARM                    WLBDAT
084200010618     C                   Z-ADD     G02DAT        P1CDFV
084300010618     C* AUTOTRASPORTATORE
084500120419     C     KDST          CHAIN     FiDST01L                           32
084600010618    1C     *IN32         IFEQ      *OFF
084700010618     C                   Z-ADD     DSTPDR        P1CPDR
084900021205     C                   move      dstpdr        kpdpdr
085000021203     C                   move      'A'           kpdtip
085100021203     C     kfapd         chain     fiapd01L                           32
085200010618    2C     *IN32         IFEQ      *OFF
085300010712     C                   MOVEL     APDRSC        P1DPDR           25
085400010618   X2C                   ELSE
085500010618     C                   CLEAR                   P1DPDR
085600010618    2C                   ENDIF
085700010618   X1C                   ELSE
085800010618     C                   CLEAR                   P1CPDR
085900010618     C                   ENDIF
086000010618     C* DATA IMMISSIONE
086100010703     C                   Z-ADD     BREDFS        G02INV
086200010618     C                   MOVE      *ZEROS        G02DAT
086300010618     C                   MOVEL     3             G02ERR
086400010618     C                   CALL      'XSRDA8'
086500010618     C                   PARM                    WLBDAT
086600010618     C                   Z-ADD     G02DAT        P1CDFS
086700010618     C*
086800010703     C                   Z-ADD     BREHMS        P1CHMS
086900010703     C                   Z-ADD     BRENPS        P1CNPS
087000010711     C                   EXCEPT    ERRDOP
087100010618     C                   ENDSR
087200010712     C*
087300010712     C* TOLGO ZERI NON SIGNIFICATIVI --------------------------------*
087400010712     C     EDITA         BEGSR
087500010712     C*
087600010712     C                   MOVE      *BLANKS       WFINE             1
087700010712     C                   MOVEA     W006A         W06
087800010712     C                   Z-ADD     1             II                1 0
087900010712     C     WFINE         DOWEQ     *BLANKS
088000010712     C     II            ANDLE     5
088100010712     C     W06(II)       IFEQ      '0'
088200010712     C                   MOVE      *BLANKS       W06(II)
088300010712     C                   ELSE
088400010712     C                   MOVE      '1'           WFINE
088500010712     C                   END
088600010712     C                   ADD       1             II
088700010712     C                   ENDDO
088800010712     C                   MOVEA     W06           W006A
088900010712     C                   ENDSR
089000920421     C*
089100911018     OQSYSPRT   E            TES              02
089200911015     O                       STA(1)              66
089300010618     O                       STA(2)             132
089400900626     O                       RSUT                20
089500920427     O                                           21 '/'
089600920427     O                       SIMFEL              24
089700941110     O                       WDTGIO             112 '  /  /    '
089800901115     O                       PAGE          Z    132
089900941109     O          E            TES         1
090000941109     O                       UTIME              110 '  :  :  '
090100941109     O          E            TES         1
090200911015     O                       STA(3)              66
090300920421     O                       STA(4)             132
090400920831     O                       APPFIL              25
090500920831     O                       DESFIL              41
090600941109     O                       DLRNFV        Z     54
090700941109     O                       STADFV              73 '  /  /    '
090800941109     O                       STACPC             101
090900920909     O                       DESCPC            +  0
091000010711     O          E            TES1        2
091100911018     O                       STA(5)              66
091200911018     O                       STA(8)             132
091300010711     O          E            TES1        1
091400911018     O                       STA(6)              66
091500911018     O                       STA(9)             132
091600010712     O          E            TES1        1
091700010712     O                       RIGTRA             132
091800010712     O          E            TESDOP         1
091900010712     O          E            TESDOP      3  2
092000010712     O                                           70 '***  SPUNTE DOPPIE ***'
092100010712     O          E            TESDOP         1
092200010712     O                                           18 '--- SEGNACOLLO ---'
092300010712     O                                           41 '------ DISTINTA -----'
092400010712     O                                           66 '------- AUTOTRASPORTATOR'
092500010712     O                                           75 'E -------'
092600010712     O                                           92 'IMMISS. SPUNTA'
092700070122     O                                           98 'N.'
092800010712     O          E            TESDOP         1
092900010712     O                                           18 'LNP LNA SR SEGNAC.'
093000010712     O                                           41 'P.O.NUMERO  DATA     '
093100010712     O                                           92 'DATA       ORA'
093200070122     O                                          100 'PIST.'
093300010712     O          E            TESDOP         1
093400010712     O                       RIGTRA             132
093500010712     O          E            ERRDOP         1
093600010712     O              N19      P1CLNP               3
093700010712     O              N19      P1CLNA               7
093800010712     O              N19      P1CNRS        Z     10
093900010712     O              N19      P1CNSC        Z     18
094000010712     O                       P1CFGS              23
094100010712     O                       P1CNFV        Z     30
094200010712     O                       P1CDFV        8     41
094300010712     O                       P1CPDR              49
094400010712     O                                           50 '-'
094500010712     O                       P1DPDR              75
094600070122     O                       P1CDFS        8     86
094700070122     O                       P1CHMS              95 '  :  :  '
094800070122     O                       P1CNPS              98
094900010712     O                       P1CMSG             132
095000911015     O*
095100920423     O          E    05      DET         1
095200920423     O                                          132 ' '
095300911015     O          E            DET         1
095400970327     O              N05 07   STAAAS        Z      2
095500970327     O              N05 07   ARBLNP        Z   +  1
095600970327     O               05      BRVLNP        Z      6
095700970327     O              N05 07 08ARBNRS        Z   +  1
095800970327     O               05      BRVNRS        Z      9
095900970327     O              N05 07 08                  +  0 '-'
096000970327     O              N05 07   ARBNSP        Z   +  0
096100970327     O              N05 07   STADSP            +  2 '  /  '
096200970327     O              N05 07   ARBLNA        Z   +  1
096300970327     O               05      BRVLNA        Z     28
096400970327     O              N05 07   STARSD            +  1
096500970327     O              N05 07N09ARBNCD        Z   +  1
096600970327     O              N05 07N09                  +  0 '/'
096700970327     O              N05 07N09ARBNCA        Z   +  0
096800970327     O              N05 07 09                    67 'NON SEQUENZIALI'
096900970327     O              N05 07   ARBZNC            +  1
097000970327     O               05      BRVZNC              70
097100970327     O              N05      STADAM            +  2 '  /  '
097200970327     O              N05      ARTNSC        Z   +  2
097300970327     O               05      BRVNSC        Z     87
097400970327     O                  12                       67 'BOLLA LEGATA'
097500941109     O* ERRORI:
097600970327     O                     01                   110 'NON SPUNTATO IN DATA '
097700970327     O                     01ST2DFV             118 '  /  /  '
097800941109     O*
097900970327     O                     02                   113 'SPUNTATO SU ALTRA DISTIN'
098000970327     O                     02                  +  0 'TA:'
098100000126     O                     02BRVNFV        Z   +  0
098200970327     O                     02                  +  1 'DEL'
098300970327     O                     02STADDC            +  1 '  /  '
098400941109     O*
098500970327     O                     03                   113 'SPUNTATO NON IN DISTINTA'
098600941109     O*
098700970327     O                     04                   113 'SPUNTATO  DI  DISTINTA  '
098800970327     O                     04                  +  0 ' N.'
098900970327     O                     04STANDC        Z   +  1
099000970327     O                     04                  +  1 'DEL'
099100970327     O                     04STADDC            +  1 '  /  '
099200970327     O*
099300970327     O                     10                   112 'SPUNTATO  GIA''  CONSEGN'
099400970327     O                     10                  +  0 'ATO'
099500970327     O                     10                  +  2 'IL'
099600970327     O                     10STADCM            +  2 '  /  '
099700941109     O*
099800970327     O                     05                   109 'MANCA  RECORD  BOLLA'
099900941109     O                     05                   130 'LETTORE BARCODE N:'
100000920619     O                     05BRVNPS             132
100100950214     O*
100200970327     O                     06                   108 'SPUNTATO SU FOGL.DI'
100300970327     O                     06FVVFGS            +  1
100400970327     O                     06                  +  1 'N.'
100500970327     O                     06FVVNPG             116
100600950214     O                     06                   117 '-'
100700950214     O                     06FVVNFV             122
100800950214     O                     06                   126 'DEL'
100900950214     O                     06STADDC             132 '  /  '
101000920423     O          E    05      DET         1
101100920423     O                                          132 ' '
101200941110     O          E            TUTOK            20
101300941110     O                       STA(11)             66
101400941110     O                       STA(12)            132
101500941110     O          E            FINES       2
101600941110     O                       STA(13)            100
101700010703**
101800970507XXXXXXXXXXXXXXXXXXXX                 ** QUADRATURA DISTINTA AUTOTR
101900970507ASPORTATORE **       FNLR71R        XX/XX/XXXX           PAG. XXXX
102000920831D I S T I N T A :  DI XXX-XXXXXXXXXXXXXXX  N.RO  XXXXX     DEL XX/
102100010712XX/XXXX       AUTOTRASPO.: XXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXX
102200970327S  P  E  D  I  Z  I  O  N  E  DESTINATARIO             SEGNACOLLI
102300970327NO LNP SER NUMERO  DATA  LNA                          DAL  /   AL
102400970327XX XXX XX XXXXXXX  XX/XX XXX XXXXXXXXXXXXXXXXXXXXXX XXXXXXX/XXXXXX
102500970327  ZONA  DATA SEGNACOLLO
102600970327  ARR. ARRIVO  ERRATO
102700970327X  XX  XX/XX  XXXXXXX  ECCEDENTE: NON IN CONSEGNA
102800941110                     N O N   E '   S T A T A   T R O V A T A   N E
102900941110 S S U N A    A N O M A L I A
103000941110              ***   FINE   STAMPA   ***
103100941110** Comandi    567890123456789012345678901234567890123456789012345678901234567890
103200941110OVRPRTF FILE(QSYSPRT)  USRDTA('Quadr.Dist')
103300010703**
103400010703In IMA XXX XXXXXX Del XX/XX PstXX
