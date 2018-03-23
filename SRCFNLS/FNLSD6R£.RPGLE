000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H* FNLSD6R *-----------------------------------------------------*
000300941219     H*    -  ELENCO SPEDIZIONI PARTITE -   ESEGUE
000400940930     H*---------------------------------------------------------------*
000500941003     FFNLSD6P   O    E             PRINTER OFLIND(*IN30)
000600941003     F                                     USROPN
000700941007     FAZORG01L  IF   E           K DISK
000800941005     FFNLBL01L  IF   E           K DISK
000900941007     FFNLBL02L  IF   E           K DISK
001000941005     F                                     RENAME(FNLBL000:FNLBL002)
001100990715     FFIGCP01L  IF   E           K DISK
001200010726     F*?*FNBLP44CUF  E           K        DISK
001300010802     FFNBLP44L  IF   E           K DISK
001400941005     FTABEL00F  IF   E           K DISK
001500990125     FFNDCD01L  IF   E           K DISK
001600040810     F**""*FNDCT02L  IF   E           K DISK
001700941005     D*
001800941005     D AAN             S              4  0 DIM(30)
001900941005     D LPN             S              3  0 DIM(30)
002000941005     D NRN             S              2  0 DIM(30)
002100941005     D NSN             S              7  0 DIM(30)
002200990125     D NSC             S              7  0 DIM(30)
002300941007     D CMD             S              1    DIM(80) CTDATA PERRCD(80)
002400941003     D*
002500010726     D*?*FNBLP000    01
002600010726     D*?*FNSTB000    02
002700941005     D* PARAMETRI DI INGRESSO AL PROGRAMMA
002800941003     D PARAM1          DS
002900941003     D**** NON SERVE ****                     1   80PA1DBR
003000941003     D**** NON SERVE ****                     9  160PA1DPS
003100941003     D  PA1SS1                17     24  0
003200941005     D  PA1AAS                17     20  0
003300941005     D  PA1MGS                21     24  0
003400941003     D  PA1SS2                25     32  0
003500941003     D**** NON SERVE ****                    33  400PA1PS1
003600941003     D**** NON SERVE ****                    41  480PA1PS2
003700941005     D  PA1STA                49     49
003800941006     D  PA1KS1                50     56  0
003900941006     D  PA1KS2                57     63  0
004000941006     D  PA1RM1                64     78
004100941006     D  PA1RM2                79     93
004200941006     D  PA1RD1                94    113
004300941006     D  PA1RD2               114    133
004400941006     D  PA1LNP               134    136  0
004500941005     D* DS PER ROUTINE CONTROLLO/INVERSIONE DATA
004600941003     D WLBDAT          DS
004700941003     D  G02DAT                 1      8  0
004800941003     D  G02INV                 9     16  0
004900941003     D  G02ERR                17     17
005000941003     D  G02TGI                18     22  0
005100941007     D* DATE DI SPEDIZIONE
005200941007     D                 DS
005300941007     D  BLPDSP                 1      8  0
005400941007     D  BLPAAS                 1      4  0
005500941007     D  BLPMGS                 5      8  0
005600010726     D*?*         DS
005700010726     D*?*                                     1   80STBDSP
005800010726     D*?*                                     1   40STBAAS
005900010726     D*?*                                     5   80STBMGS
006000990125     D                 DS
006100990125     D  DCTDCA                 1      8  0
006200990125     D  DCTAAC                 1      4  0
006300990125     D  DCTMGC                 5      8  0
006400941003     D KPJBA         E DS
006500020429     D***                                     1   3 REM
006600020429     D***                                     4   6 REMFIL
006700970707     D DS5P          E DS
006800941003     D CNCR80        E DS
006900941003     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007000040810
007100040810      * ds per il controllo della presenza di CA per la spedizione richiesta
007200040810     d FIDN12DS      e ds
007300040810     d  skKey                 26    305    dim(20)
007400040810     d  skAnn                306    325    dim(20)
007500040810     d  skDca                326    485  0 dim(20)
007600040810     d  skFca                486    545  0 dim(20)
007700040810     d  skDch                546    705  0 dim(20)
007800040810     d  skCch                706    745    dim(20)
007900040810     d*
008000040810     d dsKey           ds
008100040810     d  dsaac                         4  0
008200040810     d  dsfil                         3  0
008300040810     d  dsnca                         7  0
008400941003     C**************************************************************************
008500941003     C*
008600941003     C     *ENTRY        PLIST
008700941003     C                   PARM                    KPJBA
008800941003     C                   MOVEL     KPJBU         PARAM1
008900941010     C*
009000941010     C* MODIFICO IL CAMPO DESCR.MITTENTE FINALE (PA1RM2) RIEMPENDO
009100941010     C* GLI EVENTUALI SPAZI FINALI CON "IL MASSIMO", QUESTO PER
009200941010     C* I SUCCESSIVI CONTROLLI DI ESCLUSIONE
009300941010     C                   MOVEL     *HIVAL        WCOMOD           20
009400941010     C     PA1RM2        CAT       WCOMOD:0      PA1RM2
009500941010     C     PA1RD2        CAT       WCOMOD:0      PA1RD2
009600941003     C*
009700941003     C                   Z-ADD     1             CODUT
009800941003     C                   CALL      'X§PARUT'
009900941003     C                   PARM                    UT§DSE
010000941003     C                   MOVEL     RAGUT         P1CRSU
010100941003     C                   MOVEL     REC80         CNCR80
010200941003     C**************************************************************************
010300941003     C*            M A I N      L I N E
010400941003     C**************************************************************************
010500941003     C*
010600941003     C* GIRO LA DATA INIZIALE
010700941003     C                   Z-ADD     PA1SS1        G02INV
010800941003     C                   Z-ADD     *ZERO         G02DAT
010900941003     C                   MOVEL     '3'           G02ERR
011000941003     C                   CALL      'XSRDA8'
011100941003     C                   PARM                    WLBDAT
011200941003     C                   Z-ADD     G02DAT        P1CSS1
011300941003     C* GIRO LA DATA FINALE
011400941003     C                   Z-ADD     PA1SS2        G02INV
011500941003     C                   Z-ADD     *ZERO         G02DAT
011600941003     C                   MOVEL     '3'           G02ERR
011700941003     C                   CALL      'XSRDA8'
011800941003     C                   PARM                    WLBDAT
011900941003     C                   Z-ADD     G02DAT        P1CSS2
012000941003     C*
012100941003     C* RILEVO ORA E DATA DEL GIORNO
012200941003     C                   TIME                    WTIME            14 0
012300941003     C                   MOVEL     WTIME         ORARIO
012400941003     C                   MOVE      WTIME         WDATE
012500941006     C*
012600941007     C* DECODIFICO LA LINEA DI PARTENZA E RILEVO LA STAMPANTE
012700941006     C                   Z-ADD     PA1LNP        P1CLNP
012800950524     C     PA1LNP        CHAIN     AZORG01L                           31
012900941021     C  N31              MOVEL     ORGDES        P1DLNP
013000941021     C*
013100941021     C                   MOVEL     *BLANKS       WOUTQ            10
013200941021     C     PA1STA        IFEQ      'R'
013300941021     C                   MOVEL     'RISTAMPA'    P1CSTA
013400941021     C                   MOVEL     '*JOB'        WOUTQ
013500941021     C                   ELSE
013600941021     C                   MOVEL     *BLANKS       P1CSTA
013700950524     C* STAMPO SEMPRE AL PRIMO LIVELLO ANCHE SE STO ELABORANDO 1 SECOND
013800970707     C                   MOVEL     'T'           W004A             4
013900970707     C                   MOVE      SIMFEL        W004A
014000970707     C                   MOVEL(P)  W004A         KKEY
014100970707     C                   MOVEL     '5P'          KCOD
014200970707     C     KTAB1         CHAIN     TABEL00F                           31
014300970707     C     *IN31         IFEQ      *OFF
014400970707     C                   MOVEL     TBLUNI        DS5P
014500970707     C                   MOVEL     §5POTQ        WOUTQ
014600970707     C                   END
014700941021     C                   ENDIF
014800941021     C*
014900941021     C* METTO NEL CAMPO "DATI UTENTE" IL CODICE FIL. (SOLO 2' LVL)
015000020429     C***        REM       IFEQ 'REM'                      DALL'UTENTE
015100020429     C***                  MOVEAREMFIL    CMD,55           FILIALE
015200020429     C***                  ELSE
015300020429     C     SIMTPP        IFEQ      '2'
015400020429     C     SIMTPP        OREQ      *BLANKS
015500020429     C                   MOVE      SIMPOU        W003A             3
015600020429     C                   MOVEA     W003A         CMD(55)
015700020429     C                   ELSE
015800960807     C* PRENDO LA STAMPANTE DALLA KPJBA
015900960807     C                   MOVE      KQOTD         WOUTQ
016000941021     C                   ENDIF
016100960807     C                   MOVEA     WOUTQ         CMD(28)
016200941007     C*
016300941007     C* Eseguo OVRPRTF sul file di stampa
016400941007     C                   Z-ADD     80            LUNG             15 5
016500941007     C                   MOVEL     *BLANKS       COMMAN           80
016600941007     C                   MOVEA     CMD           COMMAN
016700941010     C                   CALL      'QCMDEXC'                            H1
016800941010     C                   PARM                    COMMAN
016900941010     C                   PARM                    LUNG
017000941005     C*
017100941005     C                   Z-ADD     *ZERO         P2CBOL
017200941005     C                   OPEN      FNLSD6P
017300941005     C                   WRITE     LSD6T01
017400941005     C*
017500950518     C*? SOLO PER RISTAMPA -   F A S E    1    ----------    ?
017600941005     C*
017700010726     C*?*        PA1STA    IFEQ 'R'
017800010726     C*?*                  EXSR FASE1
017900010726     C*?*                  ELSE
018000950518     C*
018100950518     C*? SOLO PER   STAMPA -   F A S E    2    ----------    ?
018200950518     C*
018300010726     C*?*                  EXSR FASE2
018400010726     C*?*                  ENDIF
018500941005     C*
018600950518     C*? PER ENTRANBI-------   F A S E   3    -----------    ?
018700941005     C*
018800950518     C                   EXSR      FASE3
018900941003     C*
019000941003     C* VEDO SE HO STAMPATO ALMENO UNA BOLLA
019100941005     C     P2CBOL        IFEQ      *ZERO
019200941003     C                   WRITE     LSD6Z01
019300941003     C                   ELSE
019400941003     C                   WRITE     LSD6Z02
019500021016     C**!!!              EXSR      DTAARA
019600941003     C                   ENDIF
019700941005     C*
019800941005     C                   CLOSE     FNLSD6P
019900941003     C*
020000941003     C                   SETON                                        LR
020100941003     C**************************************************************************
020200010726     C* **OBS**        ?    F   A   S   E     1    ?
020300941005     C*
020400010726     C* **OBS**  SOLO PER RISTAMPA
020500950518     C*
020600010726     C* **OBS**  VENGONO STAMPATE TUTTE LE BOLLE CON:
020700010726     C* **OBS**     .  DATA STAMPA COMPRESA NEL RANGE E DATA SPEDIZIONE
020800010726     C* **OBS**        PRECEDENTE AL RANGE
020900941003     C**************************************************************************
021000010726     C*?*        FASE1     BEGSR
021100941003     C*
021200010726     C*?*        PA1LNP    SETLLFNSTB000
021300010726     C*?*        PA1LNP    READEFNSTB000                 31
021400950523     C*
021500010726     C*?*        *IN31     DOWEQ*OFF
021600010726     C*?*        STBDSP    ANDLTPA1SS1
021700941005     C*
021800010726     C*?*        STBDDS    IFGE PA1SS1
021900010726     C*?*        STBDDS    ANDLEPA1SS2
022000941005     C*
022100010726     C*?*                  EXSR DETBOL                     STAMPA BOLLA
022200010726     C*?*                  EXSR LEGBOL                     LEGAMI BOLLA
022300010726     C*?*                  EXSR COMANO                     C.A.
022400941005     C*
022500010726     C*?*                  ENDIF
022600941005     C*
022700010726     C*?*        PA1LNP    READEFNSTB000                 31
022800010726     C*?*                  ENDDO
022900941005     C*
023000010802     C*?*                  ENDSR
023100950518     C**************************************************************************
023200010726     C* **OBS**        ?    F   A   S   E     2    ?
023300950518     C*
023400010726     C* **OBS**  SOLO PER STAMPA
023500950518     C*
023600010726     C* **OBS**  VENGONO STAMPATE TUTTE LE BOLLE CON:
023700010726     C* **OBS**     .  DATA STAMPA = 0
023800010726     C* **OBS**     .  DATA SPEDIZIONE PRECEDENTE AL RANGE
023900950518     C**************************************************************************
024000010726     C*?*        FASE2     BEGSR
024100950518     C*
024200010726     C*?*        PA1LNP    SETLLFNSTB000
024300010726     C*?*        PA1LNP    READEFNSTB000                 31
024400010726     C*?*        *IN31     DOWEQ*OFF
024500010726     C*?*        STBDSP    ANDLTPA1SS1
024600950518     C*
024700010726     C*?*        STBDDS    IFEQ 0
024800950518     C*
024900010726     C*?*                  EXSR DETBOL                     STAMPA BOLLA
025000010726     C*?*                  EXSR LEGBOL                     LEGAMI BOLLA
025100010726     C*?*                  EXSR COMANO                     C.A.
025200950518     C* AGGIORNO LA BOLLA
025300010726     C*?*                  Z-ADDPA1SS1    STBDDS           DATA STAMPA
025400010726     C*?*                  EXCPTUPDSTB
025500950518     C*
025600010726     C*?*                  ENDIF
025700950518     C*
025800010726     C*?*        PA1LNP    READEFNSTB000                 31
025900010726     C*?*                  ENDDO
026000950518     C*
026100010726     C*?*                  ENDSR
026200950518     C**************************************************************************
026300950518     C*         ?    F   A   S   E     3    ?
026400950518     C*
026500950518     C* SIA PER STAMPA CHE PER RISTAMPA
026600950518     C*
026700941005     C* VENGONO STAMPATE TUTTE LE BOLLE CON:
026800941005     C*    .  DATA SPEDIZIONE COMPRESA NEL RANGE
026900941005     C*    .  COD.CLIENTE COMPRESO NEL RANGE
027000941005     C*    .  MITTENTE COMPRESO NEL RANGE
027100941005     C*    .  DESTINATARIO COMPRESO NEL RANGE
027200941005     C**************************************************************************
027300950518     C     FASE3         BEGSR
027400941005     C*
027500941005     C* MI POSIZIONO CON LA DATA INIZIALE
027600941005     C                   Z-ADD     PA1AAS        KAAS
027700941005     C                   Z-ADD     PA1MGS        KMGS
027800941007     C                   MOVEL     PA1RM1        KRSM
027900941007     C                   MOVEL     PA1RD1        KRSD
028000010726     C*?*        KBLP1     SETLLFNBLP44C
028100010726     C     KBLP1         SETLL     FNBLP44L
028200941005     C*
028300010726     C*?*                  SETOF                     0102
028400010726     C*?*        PA1LNP    READEFNBLP44C                 31
028500010726     C     PA1LNP        READE     FNBLP44L                               31
028600941007     C*
028700941005     C     *IN31         DOWEQ     *OFF
028800941007     C*
028900010726     C*?*01      BLPDSP    IFGT PA1SS2                     DATA SPEDIZIONE
029000010726     C     BLPDSP        IFGT      PA1SS2
029100941007     C                   SETON                                        31
029200941007     C                   ENDIF
029300010726     C*?*02      STBDSP    IFGT PA1SS2                     DATA SPEDIZIONE
029400010726     C*?*                  SETON                     31
029500010726     C*?*                  ENDIF
029600941007     C*
029700941007     C* VERIFICO I LIMITI E LE ESCLUSIONI
029800941007     C  N31              EXSR      ESCLUS
029900941007     C*
030000941007     C  N31WOK           IFEQ      'S'
030100941005     C*
030200941005     C                   EXSR      DETBOL
030300941005     C                   EXSR      LEGBOL
030400990125     C                   EXSR      COMANO
030500941005     C*
030600941005     C* AGGIORNO LA BOLLA CON LA DATA STAMPA
030700941007     C* SOLO SE SONO IN STAMPA
030800010726     C*?*02      PA1STA    IFEQ 'S'
030900010726     C*?*                  Z-ADDPA1SS1    STBDDS           DATA STAMPA
031000010726     C*?*                  EXCPTUPDSTB
031100010726     C*?*                  ENDIF
031200941005     C*
031300941005     C                   ENDIF
031400941005     C*
031500010726     C*?*                  SETOF                     0102
031600010726     C*?N31      PA1LNP    READEFNBLP44C                 31
031700010726     C  N31PA1LNP        READE     FNBLP44L                               31
031800941005     C                   ENDDO
031900941005     C*
032000941005     C                   ENDSR
032100941005     C**************************************************************************
032200941007     C*  VERIFICO I LIMITI E LE ESCLUSIONI PER IL RECORD LETTO
032300941005     C**************************************************************************
032400941007     C     ESCLUS        BEGSR
032500941005     C*
032600941010     C                   MOVEL     'S'           WOK               1
032700941007     C*
032800941010     C* VERIFICO LE INCLUSIONI PER CODICE CLIENTE
032900010726     C*?*01      BLPCCM    IFNE *ZERO
033000010726     C     BLPCCM        IFNE      *ZERO
033100941010     C                   Z-ADD     BLPCCM        P1CKSC
033200941010     C                   ELSE
033300941010     C                   Z-ADD     BLPKSC        P1CKSC
033400941010     C                   ENDIF
033500010726     C*?*02                Z-ADDSTBKSC    P1CKSC           CLIENTE
033600941010     C     P1CKSC        IFLT      PA1KS1
033700941010     C     P1CKSC        ORGT      PA1KS2
033800941010     C                   MOVEL     *BLANKS       WOK
033900941010     C                   ENDIF
034000941010     C*
034100941010     C* VERIFICO LE INCLUSIONI PER MITTENTE
034200941010     C* (POTREBBE ANCHE ESSERCI UNA PARTE DI CAMPO)
034300010726     C*?*01                MOVELBLPRSM    P1CRSM           MITTENTE
034400010726     C                   MOVEL     BLPRSM        P1CRSM
034500010726     C*?*02                MOVELSTBRSM    P1CRSM           MITTENTE
034600941010     C     P1CRSM        IFLT      PA1RM1
034700941010     C     P1CRSM        ORGT      PA1RM2
034800941010     C                   MOVEL     *BLANKS       WOK
034900941010     C                   ENDIF
035000941010     C*
035100941010     C* VERIFICO LE INCLUSIONI PER DESTINATARIO
035200941010     C* (POTREBBE ANCHE ESSERCI UNA PARTE DI CAMPO)
035300010726     C*?*01                MOVELBLPRSD    P1CRSD           DESTINATARIO
035400010726     C                   MOVEL     BLPRSD        P1CRSD
035500010726     C*?*02                MOVELSTBRSD    P1CRSD           DESTINATARIO
035600941010     C     P1CRSD        IFLT      PA1RD1
035700941010     C     P1CRSD        ORGT      PA1RD2
035800941010     C                   MOVEL     *BLANKS       WOK
035900941010     C                   ENDIF
036000941007     C*
036100941007     C* IMPOSTO LA DATA SPEDIZIONE
036200010726     C*?*01                Z-ADDBLPDSP    BOLDSP
036300010726     C                   Z-ADD     BLPDSP        BOLDSP
036400010726     C*?*02                Z-ADDSTBDSP    BOLDSP
036500941007     C*
036600941007     C                   ENDSR
036700941007     C**************************************************************************
036800941007     C*  STAMPO IL DETTAGLIO BOLLA
036900941007     C**************************************************************************
037000941007     C     DETBOL        BEGSR
037100941007     C*
037200941003     C* PER OVERFLOW INTESTO NUOVA PAGINA
037300941005     C   30              DO
037400941003     C                   WRITE     LSD6T01
037500941003     C                   SETOFF                                       30
037600941003     C                   ENDDO
037700941007     C*
037800010726     C* **OBS** HO LETTO FNBLP O FNSTB ?
037900010726     C*?*        *IN01     IFEQ *ON                        FNBLP000
038000941007     C*
038100941007     C* RIEMPO I CAMPI IN STAMPA ---- FNBLP ------------------------------------
038200941007     C                   Z-ADD     BLPDSP        G02INV
038300941007     C                   Z-ADD     *ZERO         G02DAT
038400941007     C                   MOVEL     '3'           G02ERR
038500941007     C                   CALL      'XSRDA8'
038600941007     C                   PARM                    WLBDAT
038700941007     C                   Z-ADD     G02DAT        P1CDSP
038800941007     C                   Z-ADD     BLPLNP        P1CLNP
038900941007     C                   Z-ADD     BLPNRS        P1CNRS
039000941007     C                   Z-ADD     BLPNSP        P1CNSP
039100941007     C                   Z-ADD     BLPLNA        P1CLNA
039200950112     C     BLPCCM        IFNE      *ZERO
039300950112     C                   Z-ADD     BLPCCM        P1CKSC
039400950112     C                   ELSE
039500950112     C                   Z-ADD     BLPKSC        P1CKSC
039600950112     C                   ENDIF
039700950112     C                   MOVEL     BLPRSM        P1CRSM
039800950112     C                   MOVEL     BLPRSD        P1CRSD
039900941007     C                   Z-ADD     BLPRMN        P1CRMN
040000941007     C                   Z-ADD     BLPDCM        G02INV
040100941007     C                   Z-ADD     *ZERO         G02DAT
040200941007     C                   MOVEL     '3'           G02ERR
040300941007     C                   CALL      'XSRDA8'
040400941007     C                   PARM                    WLBDAT
040500941007     C                   Z-ADD     G02DAT        P1CDCM
040600941007     C                   Z-ADD     BLPNDC        P1CNDC
040700941007     C                   Z-ADD     BLPDDC        G02INV
040800941007     C                   Z-ADD     *ZERO         G02DAT
040900941007     C                   MOVEL     '3'           G02ERR
041000941007     C                   CALL      'XSRDA8'
041100941007     C                   PARM                    WLBDAT
041200941007     C                   Z-ADD     G02DAT        P1CDDC
041300941007     C                   Z-ADD     *ZERO         P1CNGC
041400941007     C                   Z-ADD     *ZERO         P1CDCG
041500990715     C* PER LA GIACENZA DEVO AGGANCIARE FIGCP01L
041600990715     C     KGCP1         CHAIN     FIGCP01L                           32
041700941007     C  N32              DO
041800941007     C                   Z-ADD     GCPNGC        P1CNGC
041900941007     C     GCPDCG        IFNE      *ZERO
042000941007     C                   Z-ADD     GCPDCG        G02INV
042100941007     C                   Z-ADD     *ZERO         G02DAT
042200941007     C                   MOVEL     '3'           G02ERR
042300941007     C                   CALL      'XSRDA8'
042400941007     C                   PARM                    WLBDAT
042500941007     C                   Z-ADD     G02DAT        P1CDCG
042600941007     C                   ENDIF
042700941007     C                   ENDDO
042800941007     C                   MOVEL     BLPCCA        P1CCCA
042900941007     C                   MOVEL     *BLANKS       P1DCCA
043000941007     C     BLPCCA        IFNE      *BLANKS
043100941007     C                   MOVEL     '7O'          KCOD
043200941007     C                   MOVEL     *BLANKS       KKEY
043300941007     C                   MOVEL     BLPCCA        KKEY
043400941007     C     KTAB1         CHAIN     TABEL00F                           99
043500941007     C  N99              MOVEL     TBLUNI        P1DCCA
043600941007     C                   ENDIF
043700941007     C*
043800010726     C*?*                  ELSE                            FNSTB000
043900941003     C*
044000010726     C* **OBS** RIEMPO I CAMPI IN STAMPA ---- FNSTB ----------------------------
044100010726     C*?*                  Z-ADDSTBDSP    G02INV           AAAA/MM/GG
044200010726     C*?*                  Z-ADD*ZERO     G02DAT
044300010726     C*?*                  MOVEL'3'       G02ERR
044400010726     C*?*                  CALL 'XSRDA8'
044500010726     C*?*                  PARM           WLBDAT
044600010726     C*?*                  Z-ADDG02DAT    P1CDSP           DATA SPEDIZIONE
044700010726     C*?*                  Z-ADDSTBLNP    P1CLNP           LNP
044800010726     C*?*                  Z-ADDSTBNRS    P1CNRS           SERIE
044900010726     C*?*                  Z-ADDSTBNSP    P1CNSP           SPEDIZIONE
045000010726     C*?*                  Z-ADDSTBKSC    P1CKSC           CLIENTE
045100010726     C*?*                  MOVELSTBRSM    P1CRSM           MITTENTE
045200010726     C*?*                  MOVELSTBRSD    P1CRSD           DESTINATARIO
045300010726     C*?*                  Z-ADDSTBLNA    P1CLNA           LNA
045400010726     C*?*                  Z-ADDSTBRMN    P1CRMN           RIF.MITTENTE
045500010726     C*?*                  Z-ADDSTBDCM    G02INV           AAAA/MM/GG
045600010726     C*?*                  Z-ADD*ZERO     G02DAT
045700010726     C*?*                  MOVEL'3'       G02ERR
045800010726     C*?*                  CALL 'XSRDA8'
045900010726     C*?*                  PARM           WLBDAT
046000010726     C*?*                  Z-ADDG02DAT    P1CDCM           DATA CONSEGNA
046100010726     C*?*                  Z-ADDSTBNDC    P1CNDC           NR.DISTINTA
046200010726     C*?*                  Z-ADDSTBDDC    G02INV           AAAA/MM/GG
046300010726     C*?*                  Z-ADD*ZERO     G02DAT
046400010726     C*?*                  MOVEL'3'       G02ERR
046500010726     C*?*                  CALL 'XSRDA8'
046600010726     C*?*                  PARM           WLBDAT
046700010726     C*?*                  Z-ADDG02DAT    P1CDDC           DATA DISTINTA
046800010726     C*?*                  Z-ADDSTBNGC    P1CNGC           NR.GIACENZA
046900010726     C*?*        STBDCG    IFNE *ZERO
047000010726     C*?*                  Z-ADDSTBDCG    G02INV           AAAA/MM/GG
047100010726     C*?*                  Z-ADD*ZERO     G02DAT
047200010726     C*?*                  MOVEL'3'       G02ERR
047300010726     C*?*                  CALL 'XSRDA8'
047400010726     C*?*                  PARM           WLBDAT
047500010726     C*?*                  Z-ADDG02DAT    P1CDCG           DATA CH.GIACENZA
047600010726     C*?*                  ELSE
047700010726     C*?*                  Z-ADD*ZERO     P1CDCG
047800010726     C*?*                  ENDIF
047900010726     C*?*                  MOVELSTBCCA    P1CCCA           CONS.ANOMALA
048000010726     C*?*                  MOVEL*BLANKS   P1DCCA           DESCR.CONS.ANOMALA
048100010726     C*?*        STBCCA    IFNE *BLANKS
048200010726     C*?*                  MOVEL'7O'      KCOD
048300010726     C*?*                  MOVEL*BLANKS   KKEY
048400010726     C*?*                  MOVELSTBCCA    KKEY
048500010726     C*?*        KTAB1     CHAINTABEL00F             99
048600010726     C*?N99                MOVELTBLUNI    P1DCCA           DESCR.CONS.ANOMALA
048700010726     C*?*                  ENDIF
048800941007     C*
048900010726     C*?*                  ENDIF                           *IN01=*ON
049000941003     C*
049100941005     C                   WRITE     LSD6P01
049200941005     C                   ADD       1             P2CBOL
049300941003     C*
049400941003     C                   ENDSR
049500941003     C**************************************************************************
049600941003     C*  RICERCO I LEGAMI BOLLA
049700941003     C**************************************************************************
049800941003     C     LEGBOL        BEGSR
049900941003     C*
050000941007     C* UTILIZZO 4 CAMPI DI COMODO PER CERCARE I LEGAMI BOLLA
050100010726     C*?*01                Z-ADDBLPAAS    BOLAAS
050200010726     C*?*01                Z-ADDBLPLNP    BOLLNP
050300010726     C*?*01                Z-ADDBLPNRS    BOLNRS
050400010726     C*?*01                Z-ADDBLPNSP    BOLNSP
050500010726     C                   Z-ADD     BLPAAS        BOLAAS
050600010726     C                   Z-ADD     BLPLNP        BOLLNP
050700010726     C                   Z-ADD     BLPNRS        BOLNRS
050800010726     C                   Z-ADD     BLPNSP        BOLNSP
050900010726     C*?*02                Z-ADDSTBAAS    BOLAAS
051000010726     C*?*02                Z-ADDSTBLNP    BOLLNP
051100010726     C*?*02                Z-ADDSTBNRS    BOLNRS
051200010726     C*?*02                Z-ADDSTBNSP    BOLNSP
051300941007     C*
051400941007     C* UTILIZZO LA BOLLA COME FIGLIA, O COME MAMMA, PER REPERIRE L'ORIGINE
051500941007     C                   Z-ADD     BOLAAS        KAAS
051600941007     C                   Z-ADD     BOLLNP        KLNP
051700941007     C                   Z-ADD     BOLNRS        KNRS
051800941007     C                   Z-ADD     BOLNSP        KNSP
051900941005     C     KLBL1         CHAIN     FNLBL01L                           99
052000941005     C   99KLBL1         CHAIN     FNLBL02L                           99
052100941005     C  N99              DO
052200941005     C*
052300941005     C                   MOVEL     *BLANKS       WLEGA             1
052400941005     C*
052500941005     C* CON L'ORIGINALE LEGGO LA PRIMA BOLLA DELLA LISTA
052600941005     C                   MOVEL     LBLAAO        KAAS
052700941005     C                   MOVEL     LBLLPO        KLNP
052800941005     C                   MOVEL     LBLNRO        KNRS
052900941005     C                   MOVEL     LBLNSO        KNSP
053000941005     C     KLBL1         CHAIN     FNLBL02L                           99
053100941005     C*
053200941005     C* PER PRIMA COSA METTO NEL SFL LA BOLLA ORIGINALE
053300941005     C* E TESTO SE E' PRESENTE IN ARCHIVIO
053400941005     C  N99              DO
053500941005     C                   MOVEL     *BLANKS       WTROVA            1
053600941005     C*
053700941005     C* VERIFICO SE E' QUELLA IN ESAME DALL'UTENTE
053800941007     C     LBLAAO        IFEQ      BOLAAS
053900941007     C     LBLLPO        ANDEQ     BOLLNP
054000941007     C     LBLNRO        ANDEQ     BOLNRS
054100941007     C     LBLNSO        ANDEQ     BOLNSP
054200941005     C* NON STAMPO
054300941005     C                   MOVEL     'S'           WTROVA
054400941005     C                   ELSE
054500941005     C* STAMPO
054600941007     C                   MOVEL     LBLAAO        G02INV
054700941007     C                   MOVE      LBLMGO        G02INV
054800941007     C                   Z-ADD     *ZERO         G02DAT
054900941007     C                   MOVEL     '3'           G02ERR
055000941007     C                   CALL      'XSRDA8'
055100941007     C                   PARM                    WLBDAT
055200941007     C                   Z-ADD     G02DAT        P2CDSP
055300941005     C                   MOVEL     LBLLPO        P2CLNP
055400941005     C                   MOVEL     LBLLAO        P2CLNA
055500941005     C                   MOVEL     LBLNRO        P2CNRS
055600941005     C                   MOVEL     LBLNSO        P2CNSP
055700941007     C                   MOVEL     *BLANKS       P2CPOS
055800941005     C     'ORIGINAL'    CAT       'E':0         P2CPOS
055900941005     C                   WRITE     LSD6P02
056000941005     C                   MOVEL     'S'           WLEGA
056100941005     C                   ENDIF
056200941005     C*
056300941005     C                   ENDDO
056400941005     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
056500941005     C* CICLO FINO ALL'ULTIMO LEGAME
056600941005     C* LAVORO SOLO SUI CAMPI "BOLLA SEGUENTE"
056700941005     C                   Z-ADD     *ZERO         WA                3 0
056800941005     C                   Z-ADD     *ZERO         WB                3 0
056900941005     C                   MOVEA     *ZERO         AAN
057000941005     C     *IN99         DOWEQ     *OFF
057100941005     C*
057200941005     C* VERIFICO SE LA SEGUENTE E' QUELLA IN ESAME DALL'UTENTE
057300941007     C     LBLAAN        IFEQ      BOLAAS
057400941007     C     LBLLPN        ANDEQ     BOLLNP
057500941007     C     LBLNRN        ANDEQ     BOLNRS
057600941007     C     LBLNSN        ANDEQ     BOLNSP
057700941005     C* NON STAMPO
057800941005     C                   MOVEL     'S'           WTROVA
057900941005     C                   ELSE
058000941005     C* STAMPO
058100941007     C                   MOVEL     LBLAAN        G02INV
058200941007     C                   MOVE      LBLMGN        G02INV
058300941007     C                   Z-ADD     *ZERO         G02DAT
058400941007     C                   MOVEL     '3'           G02ERR
058500941007     C                   CALL      'XSRDA8'
058600941007     C                   PARM                    WLBDAT
058700941007     C                   Z-ADD     G02DAT        P2CDSP
058800941005     C                   MOVEL     LBLLPN        P2CLNP
058900941005     C                   MOVEL     LBLLAN        P2CLNA
059000941005     C                   MOVEL     LBLNRN        P2CNRS
059100941005     C                   MOVEL     LBLNSN        P2CNSP
059200941005     C*
059300941005     C* SE NON HO ANCORA INCONTRATO LA MIA BOLLA ---> PRECEDENTE
059400941005     C* ALTRIMENTI ---> SUCCESSIVA
059500941007     C                   MOVEL     *BLANKS       P2CPOS
059600941005     C     WTROVA        IFEQ      *BLANKS
059700941005     C     'PRECEDEN'    CAT       'TE':0        P2CPOS
059800941005     C                   ELSE
059900941005     C     'SUCCESSI'    CAT       'VA':0        P2CPOS
060000941005     C                   ENDIF
060100941005     C*
060200941005     C* SE HO STAMPATO ALMENO UN LEGAME SALTO UNA RIGA
060300941005     C     WLEGA         IFNE      *BLANKS
060400941005     C                   WRITE     LSD6P03
060500941005     C                   ELSE
060600941005     C                   MOVEL     'S'           WLEGA
060700941005     C                   ENDIF
060800941005     C*
060900941005     C                   WRITE     LSD6P02
061000941005     C                   ENDIF
061100941005     C*
061200941005     C* AGGIORNO LA SCHIERA CON LE FIGLIE
061300941005     C                   ADD       1             WA
061400941005     C                   MOVEL     LBLAAN        AAN(WA)
061500941005     C                   MOVEL     LBLLPN        LPN(WA)
061600941005     C                   MOVEL     LBLNRN        NRN(WA)
061700941005     C                   MOVEL     LBLNSN        NSN(WA)
061800941005     C*
061900941005     C     KLBL1         READE     FNLBL02L                               99
062000941005     C*
062100941005     C* SE NON HO PIU' BOLLE CON READE
062200941005     C* USO LA FIGLIA PER LEGGERE IL LEGAME SUCCESSIVO
062300941005     C   99WB            IFLT      WA
062400941005     C                   ADD       1             WB
062500941005     C                   MOVEL     AAN(WB)       KAAS
062600941005     C                   MOVEL     LPN(WB)       KLNP
062700941005     C                   MOVEL     NRN(WB)       KNRS
062800941005     C                   MOVEL     NSN(WB)       KNSP
062900941005     C     KLBL1         SETLL     FNLBL02L
063000941005     C     KLBL1         READE     FNLBL02L                               99
063100941005     C                   ENDIF
063200941005     C*
063300941005     C                   ENDDO
063400941005     C                   ENDDO
063500941003     C*
063600941003     C                   ENDSR
063700990125     C**************************************************************************
063800990125     C*  RICERCO LE C.A. PRESENTI SULLA SPEDIZIONE
063900990125     C**************************************************************************
064000990125     C     COMANO        BEGSR
064100990125     C*
064200990125     C* UTILIZZO 4 CAMPI DI COMODO PER CERCARE I LEGAMI BOLLA
064300010726     C*?*01                Z-ADDBLPAAS    KAAS
064400010726     C*?*01                Z-ADDBLPLNP    KLNP
064500010726     C*?*01                Z-ADDBLPNRS    KNRS
064600010726     C*?*01                Z-ADDBLPNSP    KNSP
064700010726     C                   Z-ADD     BLPAAS        KAAS
064800010726     C                   Z-ADD     BLPLNP        KLNP
064900010726     C                   Z-ADD     BLPNRS        KNRS
065000010726     C                   Z-ADD     BLPNSP        KNSP
065100010726     C*?*02                Z-ADDSTBAAS    KAAS
065200010726     C*?*02                Z-ADDSTBLNP    KLNP
065300010726     C*?*02                Z-ADDSTBNRS    KNRS
065400010726     C*?*02                Z-ADDSTBNSP    KNSP
065500040810     C**""*KLBL1         CHAIN     FNDCT02L                           99
065600040810    1C**""**IN99         DOWEQ     *OFF
065700990125     C* NON ANNULLATA
065800040810    2C**""*DCTATB        IFEQ      ' '
065900990125     C* GIRO DATA APERTURA
066000040810     C**""*              Z-ADD     DCTDCA        G02INV
066100040810     C**""*              Z-ADD     *ZERO         G02DAT
066200040810     C**""*              MOVEL     '3'           G02ERR
066300040810     C**""*              CALL      'XSRDA8'
066400040810     C**""*              PARM                    WLBDAT
066500040810     C**""*              Z-ADD     G02DAT        P4CDCA
066600990125     C* GIRO DATA CHIUSURA
066700040810     C**""*              CLEAR                   P4CCHC
066800040810    3C**""*DCTDCH        IFGT      0
066900040810     C**""*              Z-ADD     DCTDCH        G02INV
067000040810     C**""*              Z-ADD     *ZERO         G02DAT
067100040810     C**""*              MOVEL     '3'           G02ERR
067200040810     C**""*              CALL      'XSRDA8'
067300040810     C**""*              PARM                    WLBDAT
067400040810     C**""*              Z-ADD     G02DAT        P4CCHC
067500040810    3C**""*              ENDIF
067600040810     C**""*              Z-ADD     DCTFIL        P4CFIL
067700040810     C**""*              Z-ADD     DCTNCA        P4CNCA
067800990125     C* VERIFICO SE C'E' DEL DETTAGLIO
067900040810     C**""*              CLEAR                   P4CFLS
068000040810     C**""*              CLEAR                   P4CNS1
068100040810     C**""*              CLEAR                   P4CNS2
068200040810     C**""*              CLEAR                   P4CNS3
068300040810     C**""*              CLEAR                   P4CNS4
068400040810     C**""*              CLEAR                   P4CNS5
068500040810     C**""*              CLEAR                   P4CNS6
068600040810     C**""*              CLEAR                   P4CNS7
068700040810     C**""*              CLEAR                   P4CNS8
068800040810     C**""*              CLEAR                   P4CNS9
068900040810     C**""*              SETON                                        05
069000040810     C**""*              CLEAR                   NSC
069100040810     C**""*              Z-ADD     1             XX                3 0
069200990125     C*
069300040810     C**""*KDCT          CHAIN     FNDCD01L                           98
069400040810    2C**""**IN98         DOWEQ     *OFF
069500040810    3C**""*DCDATB        IFEQ      ' '
069600040810     C**""*              Z-ADD     DCDFLS        P4CFLS
069700990125     C* CARICO UNA SCHIERA DI 9 STAMPO E VADO AVANTI A LEGGERE
069800040810    4C**""*XX            IFGT      9
069900040810     C**""*              Z-ADD     NSC(1)        P4CNS1
070000040810     C**""*              Z-ADD     NSC(2)        P4CNS2
070100040810     C**""*              Z-ADD     NSC(3)        P4CNS3
070200040810     C**""*              Z-ADD     NSC(4)        P4CNS4
070300040810     C**""*              Z-ADD     NSC(5)        P4CNS5
070400040810     C**""*              Z-ADD     NSC(6)        P4CNS6
070500040810     C**""*              Z-ADD     NSC(7)        P4CNS7
070600040810     C**""*              Z-ADD     NSC(8)        P4CNS8
070700040810     C**""*              Z-ADD     NSC(9)        P4CNS9
070800040810    5C**""**IN30         IFEQ      *ON
070900040810     C**""*              WRITE     LSD6T01
071000040810     C**""*              SETOFF                                       30
071100040810    5C**""*              ENDIF
071200040810     C**""*              WRITE     LSD6P04
071300990125     C**
071400040810     C**""*              CLEAR                   NSC
071500040810     C**""*              Z-ADD     1             XX
071600040810     C**""*              SETOFF                                       05
071700040810    4C**""*              ENDIF
071800990125     C**
071900040810     C**""*              Z-ADD     DCDNSC        NSC(XX)
072000040810     C**""*              ADD       1             XX
072100990125     C*
072200040810    3C**""*              ENDIF
072300040810     C**""*KDCT          READE     FNDCD01L                               98
072400040810    2C**""*              ENDDO
072500990125     C* STAMPO ULTIMA RIGA
072600040810     C**""*              Z-ADD     NSC(1)        P4CNS1
072700040810     C**""*              Z-ADD     NSC(2)        P4CNS2
072800040810     C**""*              Z-ADD     NSC(3)        P4CNS3
072900040810     C**""*              Z-ADD     NSC(4)        P4CNS4
073000040810     C**""*              Z-ADD     NSC(5)        P4CNS5
073100040810     C**""*              Z-ADD     NSC(6)        P4CNS6
073200040810     C**""*              Z-ADD     NSC(7)        P4CNS7
073300040810     C**""*              Z-ADD     NSC(8)        P4CNS8
073400040810     C**""*              Z-ADD     NSC(9)        P4CNS9
073500990126     C* SALTO PAGINA
073600040810    2C**""**IN30         IFEQ      *ON
073700040810     C**""*              WRITE     LSD6T01
073800040810     C**""*              SETOFF                                       30
073900040810    2C**""*              ENDIF
074000990126     C**
074100040810     C**""*              WRITE     LSD6P04
074200040810    2C**""*              ENDIF
074300990125     C**
074400040810     C**""*KLBL1         READE     FNDCT02L                               99
074500040810    1C**""*              ENDDO
074600040810      *
074700040810      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
074800040810      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
074900040810      * legate alla spedizione che passo
075000040810     c                   clear                   fidn12ds
075100040810     c                   eval      i12aas = KAAS
075200040810     c                   eval      i12lnp = KLNP
075300040810     c                   eval      i12nrs = KNRS
075400040810     c                   eval      i12nsp = KNSP
075500040810     c                   eval      i12fel = 'E'
075600040810     c                   eval      i12fan = 'E'
075700040810     c*
075800040810     c                   call      'FIDN12R'
075900040810     c                   parm                    fidn12ds
076000040810     c*
076100040810     c                   setoff                                       99
076200040810     c*
076300040810     c* se non ci sono errori
076400040810     c                   if        o12err <> ' '
076500040810     c                   seton                                        99
076600040810     c                   else
076700040810     c* se numero di CA trovate maggiore di zero
076800040810     c                   if        o12nca = 0
076900040810     c                   seton                                        99
077000040810     c                   else
077100040810     c                   z-add     1             ii                2 0
077200040810     c                   dow       ii <= o12nca and *in99 = *off
077300040810     c                   movea     skKey(ii)     dsKey
077400040810     C* GIRO DATA APERTURA
077500040810     C                   Z-ADD     skDca(ii)     G02INV
077600040810     C                   Z-ADD     *ZERO         G02DAT
077700040810     C                   MOVEL     '3'           G02ERR
077800040810     C                   CALL      'XSRDA8'
077900040810     C                   PARM                    WLBDAT
078000040810     C                   Z-ADD     G02DAT        P4CDCA
078100040810     C* GIRO DATA CHIUSURA
078200040810     C                   CLEAR                   P4CCHC
078300040810    3C     skDch(ii)     IFGT      0
078400040810     C                   Z-ADD     skDch(ii)     G02INV
078500040810     C                   Z-ADD     *ZERO         G02DAT
078600040810     C                   MOVEL     '3'           G02ERR
078700040810     C                   CALL      'XSRDA8'
078800040810     C                   PARM                    WLBDAT
078900040810     C                   Z-ADD     G02DAT        P4CCHC
079000040810    3C                   ENDIF
079100040810     C                   Z-ADD     dsFil         P4CFIL
079200040810     C                   Z-ADD     dsNca         P4CNCA
079300040810     C* VERIFICO SE C'E' DEL DETTAGLIO
079400040810     C                   CLEAR                   P4CFLS
079500040810     C                   CLEAR                   P4CNS1
079600040810     C                   CLEAR                   P4CNS2
079700040810     C                   CLEAR                   P4CNS3
079800040810     C                   CLEAR                   P4CNS4
079900040810     C                   CLEAR                   P4CNS5
080000040810     C                   CLEAR                   P4CNS6
080100040810     C                   CLEAR                   P4CNS7
080200040810     C                   CLEAR                   P4CNS8
080300040810     C                   CLEAR                   P4CNS9
080400040810     C                   SETON                                        05
080500040810     C                   CLEAR                   NSC
080600040810     C                   Z-ADD     1             XX                3 0
080700040810     C*
080800040810     C     KDCT          CHAIN     FNDCD01L                           98
080900040810    2C     *IN98         DOWEQ     *OFF
081000040810    3C     DCDATB        IFEQ      ' '
081100040810     C                   Z-ADD     DCDFLS        P4CFLS
081200040810     C* CARICO UNA SCHIERA DI 9 STAMPO E VADO AVANTI A LEGGERE
081300040810    4C     XX            IFGT      9
081400040810     C                   Z-ADD     NSC(1)        P4CNS1
081500040810     C                   Z-ADD     NSC(2)        P4CNS2
081600040810     C                   Z-ADD     NSC(3)        P4CNS3
081700040810     C                   Z-ADD     NSC(4)        P4CNS4
081800040810     C                   Z-ADD     NSC(5)        P4CNS5
081900040810     C                   Z-ADD     NSC(6)        P4CNS6
082000040810     C                   Z-ADD     NSC(7)        P4CNS7
082100040810     C                   Z-ADD     NSC(8)        P4CNS8
082200040810     C                   Z-ADD     NSC(9)        P4CNS9
082300040810    5C     *IN30         IFEQ      *ON
082400040810     C                   WRITE     LSD6T01
082500040810     C                   SETOFF                                       30
082600040810    5C                   ENDIF
082700040810     C                   WRITE     LSD6P04
082800040810     C**
082900040810     C                   CLEAR                   NSC
083000040810     C                   Z-ADD     1             XX
083100040810     C                   SETOFF                                       05
083200040810    4C                   ENDIF
083300040810     C**
083400040810     C                   Z-ADD     DCDNSC        NSC(XX)
083500040810     C                   ADD       1             XX
083600040810     C*
083700040810    3C                   ENDIF
083800040810     C     KDCT          READE     FNDCD01L                               98
083900040810    2C                   ENDDO
084000040810     C* STAMPO ULTIMA RIGA
084100040810     C                   Z-ADD     NSC(1)        P4CNS1
084200040810     C                   Z-ADD     NSC(2)        P4CNS2
084300040810     C                   Z-ADD     NSC(3)        P4CNS3
084400040810     C                   Z-ADD     NSC(4)        P4CNS4
084500040810     C                   Z-ADD     NSC(5)        P4CNS5
084600040810     C                   Z-ADD     NSC(6)        P4CNS6
084700040810     C                   Z-ADD     NSC(7)        P4CNS7
084800040810     C                   Z-ADD     NSC(8)        P4CNS8
084900040810     C                   Z-ADD     NSC(9)        P4CNS9
085000040810     C* SALTO PAGINA
085100040810    2C     *IN30         IFEQ      *ON
085200040810     C                   WRITE     LSD6T01
085300040810     C                   SETOFF                                       30
085400040810    2C                   ENDIF
085500040810     C**
085600040810     C                   WRITE     LSD6P04
085700040810     c*
085800040810     c                   add       1             ii
085900040810     c                   enddo
086000040810     c*
086100040810     c                   seton                                        99
086200040810     c*
086300040810     c                   endif
086400040810     c                   endif
086500040810     c**
086600990125     C*
086700990125     C                   ENDSR
086800941003     C**************************************************************************
086900941011     C*     AGGIORNAMENTO DATA AREA ULTIMA STAMPA ESEGUITA
087000941003     C**************************************************************************
087100021016     C**!!!DTAARA        BEGSR
087200941003     C*
087300941011     C* L'AGGIORNAMENTO AVVIENE SOLO IN CASO DI STAMPA
087400021016     C**!!!PA1STA        IFEQ      'S'
087500941011     C*
087600021016     C**!!!*LOCK         IN        FNLSD6
087700941011     C*
087800941011     C* E' INFERIORE ALLA DATA FINALE
087900021016     C**!!!FNLSD6        IFLT      PA1SS1
088000021016     C**!!!              Z-ADD     PA1SS2        FNLSD6
088100021016     C**!!!              OUT       FNLSD6
088200021016     C**!!!              ELSE
088300021016     C**!!!              UNLOCK    FNLSD6
088400021016     C**!!!              ENDIF
088500941011     C*
088600021016     C**!!!              ENDIF
088700941011     C*
088800021016     C**!!!              ENDSR
088900941011     C**************************************************************************
089000941011     C*     R O U T I N E    I N I Z I A L E
089100941011     C**************************************************************************
089200941011     C     *INZSR        BEGSR
089300941011     C*
089400040810     C     KTAB1         KLIST
089500941003     C                   KFLD                    CODUT
089600941003     C                   KFLD                    KCOD
089700941003     C                   KFLD                    KKEY
089800010726     C     KBLP1         KLIST
089900941007     C                   KFLD                    PA1LNP
090000941005     C                   KFLD                    KAAS
090100941005     C                   KFLD                    KMGS
090200941005     C                   KFLD                    KRSM
090300941005     C                   KFLD                    KRSD
090400941005     C     KLBL1         KLIST
090500941005     C                   KFLD                    KAAS
090600941005     C                   KFLD                    KLNP
090700941005     C                   KFLD                    KNRS
090800941005     C                   KFLD                    KNSP
090900990715     C     KGCP1         KLIST
091000941010     C                   KFLD                    BLPAAS
091100941010     C                   KFLD                    BLPLNP
091200941010     C                   KFLD                    BLPNRS
091300941010     C                   KFLD                    BLPNSP
091400990125     C     KDCT          KLIST
091500040810     C                   KFLD                    dsAac
091600040810     C                   KFLD                    dsFil
091700040810     C                   KFLD                    dsNca
091800941003     C*
091900941003     C     *LIKE         DEFINE    TBLCOD        KCOD
092000941003     C     *LIKE         DEFINE    TBLKEY        KKEY
092100010726     C*?*        *LIKE     DEFN STBAAS    KAAS             ANNO
092200010726     C*?*        *LIKE     DEFN STBMGS    KMGS             MESE+GIORNO
092300010726     C*?*        *LIKE     DEFN STBRSM    KRSM             MITTENTE
092400010726     C*?*        *LIKE     DEFN STBRSD    KRSD             DESTINATARIO
092500010726     C*?*        *LIKE     DEFN STBLNP    KLNP             LNP
092600010726     C*?*        *LIKE     DEFN STBNRS    KNRS             SERIE
092700010726     C*?*        *LIKE     DEFN STBNSP    KNSP             SPEDIZIONE
092800010726     C     *LIKE         DEFINE    BLPAAS        KAAS
092900010726     C     *LIKE         DEFINE    BLPMGS        KMGS
093000010727     C     *LIKE         DEFINE    PA1RM1        KRSM
093100010726     C     *LIKE         DEFINE    PA1RD1        KRSD
093200010726     C     *LIKE         DEFINE    BLPLNP        KLNP
093300010726     C     *LIKE         DEFINE    BLPNRS        KNRS
093400010726     C     *LIKE         DEFINE    BLPNSP        KNSP
093500941007     C     *LIKE         DEFINE    BLPDSP        BOLDSP
093600941007     C     *LIKE         DEFINE    BLPAAS        BOLAAS
093700941007     C     *LIKE         DEFINE    BLPLNP        BOLLNP
093800941007     C     *LIKE         DEFINE    BLPNRS        BOLNRS
093900941007     C     *LIKE         DEFINE    BLPNSP        BOLNSP
094000021016     C**!!!*DTAARA       DEFINE                  FNLSD6            8 0
094100941010     C*
094200941003     C                   ENDSR
094300941005     C**************************************************************************
094400010726     C* **OBS**  AGGIORNAMENTO BOLLA NEL FILE STORICO
094500010726     O*?*FNSTB000E                UPDSTB
094600010726     O*?*                         STBDDS
094700941021** 456789012345678901234567890123456789012345678901234567890123456789
094800941021OVRPRTF FILE(FNLSD6P) OUTQ(          ) USRDTA('Spe.Part. ')
