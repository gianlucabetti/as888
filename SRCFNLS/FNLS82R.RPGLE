000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS82R *----------------------------------------------------*
000300941228     H*         - STAMPA BOLLE
000400000000     H*--------------------------------------------------------------*
000500941228     FFNLS82D   CF   E             WORKSTN
000600941228     F                                     SFILE(LS82DSF:NRR)
000700951114     F                                     SFILE(LS82DS2:NRR2)
000800021203     FFIAPD01L  IF   E           K DISK
000900941228     FAZORG01L  IF   E           K DISK
001000910412     FCNACO00F  IF   E           K DISK
001100970213     FFNBLP45L  IF   E           K DISK
001200030620
001300970218     D* DEFINIZIONE SCHIERE
001400951114     D CLI             S              7  0 DIM(100)
001500970326     D L1              S              3  0 DIM(30)
001600020607     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001700020607
001800020607     d wokbolla        s              1    inz('0')
001900020607     d wpopdc          s              3  0
002000020607
002100970218     D*
002200900523     D PARAM           DS
002300941228     D  V1CLNA                 1      3  0
002400961017     D  PARKSC                 4     10  0
002500941228     D  PARPDR                11     17  0
002600941228     D  V1CSOP                18     18
002700970218     D  V1CLNP                19     21  0
002800970326     D  PARDSD                22     29  0
002900970326     D  PARDSA                30     37  0
003000970630     D  V1CNRD                38     39  0
003100970630     D  V1CNSD                40     46  0
003200970630     D  WNSD                  38     46  0
003300970630     D  V1CNRA                47     48  0
003400970630     D  V1CNSA                49     55  0
003500970630     D  WNSA                  47     55  0
003600970630     D  V1CRIS                56     56
003700000511     D  V1CDPD                57     57
003800030620     d* - parametri da TRUL90R:
003900030620     d* � pgm.   per stampa LDV
004000030620     d  wD90psl              187    196
004100030620     d* � modulo per stampa LDV su "A4"     (laser)
004200030620     d  wD90mdb4             197    206
004300030620     d* � modulo per stampa LDV su "A5"     (laser)
004400030620     d  wD90mdb5             207    216
004500030620     d* � stampante per LDV in formato "A4" (laser)
004600030620     d  wD90prb4             217    226
004700030620     d* � stampante per LDV in formato "A5" (laser)
004800030620     d  wD90prb5             227    236
004900030620      *
005000021203      * Ds esterna per ricerca codice padroncino
005100021203     D FNLV24DS      E DS
005200021203      *
005300970326     D WLBDAT          DS                  INZ
005400970326     D  G02DAT                 1      8  0
005500970326     D  G02INV                 9     16  0
005600970326     D  G02ERR                17     17
005700970326     D  G02TGI                18     22  0
005800970218     D*
005900900517     D KPJBA         E DS
006000970218     D*
006100910205     D TCUDS           DS
006200910205     D  F1                     1      1
006300910205     D  F3                     3      3
006400910205     D  F2                     2      2
006500910205     D  F4                     4      4
006600910205     D  F56                    5      6
006700970218     D*
006800961017     D                 DS
006900961017     D  V1CKLP                 1      3  0
007000961017     D  V1CCLI                 4      7  0
007100961017     D  V1CKSC                 1      7  0
007200030620      *
007300030620      * Parametri per stampa BOLLE ed ETICHETTE
007400030620     d TRUL90DS      e ds                  inz
007500030620     d   D90rse      e                     inz('N')
007600030620     d   D90rsb      e                     inz('S')
007700970326     D*
007800970326     D* DS PER TRUL06R - CARICAMENTO 즍
007900970326     D DSUL06        E DS                  EXTNAME(TRUL06DS)
008000970326     D  LIN                    1     90  0
008100970326     D                                     DIM(30)
008200941228     D*
008300970218     D CNCR80        E DS
008400970218     D UT쬎SE        E DS                  EXTNAME(UT쬎SE0F)
008500000000     D  TCU                  398    697
008600000000     D                                     DIM(50)
008700000000     D  KCU                  698    847P 0
008800000000     D                                     DIM(50)
008900000000     D                                     PACKEVEN
009000970218     D*
009100941228     D* DEFINIZIONE COSTANTI
009200941228     D CTUTTE          C                   CONST('0 = TUTTE')
009300021203
009400021203     D kpdpdr          S                   LIKE(APDpdr)
009500021203     D kpdtip          S                   LIKE(APDtip)
009600910412     C****************************************************************
009700910412     C*  RIEPILOGO INDICATORI
009800910412     C***************************************************************
009900020430     C* 05    - SONO UN SECONDO LIVELLO
010000970218     C* 06    - CAMBIO FILIALE IN GESTIONE PER CMD18
010100910412     C* 10    - RICHIESTA RISTAMPA
010200970218     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
010300910412     C* 30-32 - DI COMODO
010400910412     C* 35    - PULIZIA SUBFILES
010500020607     C* 40-48 - ERRORI NEI CAMPI
010600970326     C* 55-   - ERRORI NEI CAMPI
010700910412     C* 90    - INDICATORE GENERICO DI ERRORE
010800910412     C*****************************************************************
010900000000     C     *ENTRY        PLIST
011000000000     C                   PARM                    KPJBA
011100000000     C                   Z-ADD     1             CODUT
011200970218     C                   CALL      'X쬝ARUT'
011300970218     C                   PARM                    UT쬎SE
011400000000     C                   MOVEL     RAGUT         RSUT             20
011500970218     C                   MOVEL     REC80         CNCR80
011600910205     C*--- RICERCA CAPOCONTI------------------------------------------*
011700910205     C                   DO        50            X                 2 0
011800910205     C                   MOVE      TCU(X)        TCUDS
011900910205     C     F56           CABNE     'CG'          END1
012000910205     C     F4            COMP      '1'                                    21
012100910205     C     F4            COMP      '2'                                    22
012200910205     C     F4            COMP      '3'                                    23
012300910205     C     F4            COMP      '6'                                    27
012400910205     C** 1 CLIENTI   21
012500910205     C** 2 FORNITORI 22
012600910205     C** 3 AGENTI    23
012700910205     C     F3            COMP      '0'                                242425
012800910205     C     F3            COMP      'I'                                    26
012900910205     C** 0 ITALIA   25
013000910205     C** 1 ESTERO   24
013100910205     ** I CAPO CONTO IVA
013200910205     C   21
013300910205     CAN 24              Z-ADD     KCU(X)        KCE               4 0
013400910205     C   21
013500910205     CAN 25              Z-ADD     KCU(X)        KCI               4 0
013600910205     C   22
013700910205     CAN 24              Z-ADD     KCU(X)        KFE               4 0
013800910205     C   22
013900910205     CAN 25              Z-ADD     KCU(X)        KFI               4 0
014000910205     C   23
014100910205     CAN 24              Z-ADD     KCU(X)        KAE               4 0
014200910205     C   23
014300910205     CAN 25              Z-ADD     KCU(X)        KAI               4 0
014400910205     C   26              Z-ADD     KCU(X)        KIVA              4 0
014500910205     C   27              Z-ADD     KCU(X)        KBNA              4 0
014600910205     C     END1          TAG
014700910205     C                   END
014800900511     C*---------------------------------------------------------------*
014900910205     C* ACCESSO   CNACO                                    *
015000910205     C     KACO          KLIST
015100910205     C                   KFLD                    CODUT
015200910205     C                   KFLD                    CCC               4 0
015300910205     C                   KFLD                    KSC               7 0
015400970630     C     KBLP          KLIST
015500970630     C                   KFLD                    WFST              1
015600970630     C                   KFLD                    V1CLNA
015700970630     C     KBLP2         KLIST
015800970630     C                   KFLD                    WFST              1
015900970630     C                   KFLD                    LNA
016000021203     C     Kfapd         klist
016100021203     C                   kfld                    kpdtip
016200021203     C                   kfld                    kpdpdr
016300910205     C*---------------------------------------------------------------*
016400970630     C                   MOVEL     'N'           WFST
016500970326     C***
016600970326     C* CARICO TABELLA PUNTI OPERATIVI GESTITI �1
016700970326     C***
016800970326     C                   CLEAR                   DSUL06
016900970326     C                   MOVE      '�1'          D06COD
017000970326     C                   MOVEL     SIMFEL        D06KEY
017100970326     C                   MOVEL     'L'           D06TLA
017200970326     C                   MOVEL     DSUL06        KPJBU
017300970326     C*
017400970326     C                   CALL      'TRUL06R'
017500970326     C                   PARM                    KPJBA
017600970326     C                   MOVEL     KPJBU         DSUL06
017700970326     C                   MOVEA     LIN           L1
017800970326     C**
017900020430     C* VEDO SE SONO PRIMO O SECODNO LIVELLO
018000020430     C*                    MOVELSIMFEL    V1CLNP
018100020430     C                   Z-ADD     SIMPOU        V1CLNP
018200970218     C*
018300020430     C     SIMTPP        IFEQ      '2'
018400020430     C     SIMTPP        OREQ      *BLANKS
018500970218     C                   SETON                                        05
018600970218     C                   ENDIF
018700030620      *
018800030620      * Richiesta stampanti
018900030620     c                   reset                   TRUL90DS
019000030620     c                   call      'TRUL90R'
019100030620     c                   parm                    KPJBA
019200030620     c                   parm                    TRUL90DS
019300030620     c*
019400950104     C* F3 - FINE
019500030620     C     D90F3         IFEQ      '1'
019600950104     C                   GOTO      FINE
019700950104     C                   ENDIF
019800030620      *
019900030620      * Parametri x ovrprtf
020000030620      * � pgm. per stampa LDV ............. - ad aghi o laser
020100030620     c                   movel     D90psl        wD90psl
020200030620      * � stampante per LDV in formato "A4" - laser
020300030620     c                   movel     D90prb4       wD90prb4
020400030620      * � stampante per LDV in formato "A5" - laser
020500030620     c                   movel     D90prb5       wD90prb5
020600030620      * � modulo per stampa LDV su "A4" ... - laser
020700030620     c                   movel     D90mdb4       wD90mdb4
020800030620      * � modulo per stampa LDV su "A5" ... - laser
020900030620     c                   movel     D90mdb5       wD90mdb5
021000900517     C*
021100910205     C                   Z-ADD     KCI           CCC
021200950411     C     INIZIO        TAG
021300941228     C                   MOVEL     *BLANKS       V1CLNA
021400000511     C                   MOVEL     'D'           V1CDPD
021500941228     C                   MOVEL     *BLANKS       V1DLNA
021600941228     C                   MOVEL     CTUTTE        V1DLNA
021700941228     C                   MOVEL     *ZEROS        V1CKSC
021800941228     C                   MOVEL     *BLANKS       V1DKSC
021900941228     C                   MOVEL     *BLANKS       V1CPDR
022000941228     C                   MOVEL     *BLANKS       V1DPDR
022100970326     C                   CLEAR                   V1CDSD
022200970326     C                   CLEAR                   V1CDSA
022300970627     C                   CLEAR                   V1CNSD
022400970627     C                   CLEAR                   V1CNSA
022500970630     C                   CLEAR                   V1CNRD
022600970630     C                   CLEAR                   V1CNRA
022700970627     C                   MOVEL     'N'           V1CRIS
022800900509     C     FOR01         TAG
022900941228     C                   EXFMT     LS82D01
023000961017     C*
023100961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
023200961017     C                   CLEAR                   V1CMSG
023300961017     C                   SETOFF                                       284041
023400970326     C                   SETOFF                                       554546
023500020607     C                   SETOFF                                       5048
023600961017     C*
023700941228     C** F3 - FINE LAVORO
023800900509     C   KC              GOTO      FINE
023900970218     C*
024000970218     C** CMD18- CAMBIO FILIALE
024100970218     C   KS              SETON                                        0655
024200970218     C   KS              GOTO      FOR01
024300970218     C*
024400970218     C* 06 ON - CONTROLLO FILIALE IN GESTIONE
024500970218     C   06              EXSR      CTRFGS
024600970218     C   06
024700970218     CAN 90              GOTO      FOR01
024800970218     C*
024900910205     C* CONTROLLI FORMATO
025000910205     C                   EXSR      CONTR
025100941228     C  NKF
025200941228     COR 90              GOTO      FOR01
025300901214     C*
025400900511     C* ELABORAZIONE
025500941228     C     V1CPDR        IFGT      *ZEROS
025600941228     C                   MOVEL     V1CPDR        PARPDR
025700941228     C                   ENDIF
025800961017     C                   MOVEL     V1CKSC        PARKSC
025900961017     C*
026000900523     C                   MOVEL     PARAM         KPJBU
026100941228     C                   MOVEL     'LS83'        KCOAZ
026200030620     C***                CALL      'BCH10'
026300030715     c***                movel(p)  'FNLS83C'     Wpgm             10
026400030620     c                   movel(p)  'BCH10'       Wpgm             10
026500030620     c                   call      Wpgm
026600900523     C                   PARM                    KPJBA
026700941228     C*
026800950411     C                   GOTO      INIZIO
026900000000     C     FINE          TAG
027000000000     C                   SETON                                        LR
027100970218     C*
027200970218     C* CONTROLLO LA FILIALE IN GESTIONE ----------------------------
027300970218     C     CTRFGS        BEGSR
027400970218     C                   SETOFF                                       90
027500970218     C*
027600970218     C* POSSO ACCTTARE ANCHE 0 E SIGNIFICA TUTTE LE LINEE
027700970326     C     V1CLNP        IFGT      0
027800970326     C     V1CLNP        LOOKUP    L1                                     30
027900970326     C     *IN30         IFEQ      *OFF
028000970326     C                   MOVEL     MSG(5)        V1CMSG
028100970218     C                   SETON                                        559028
028200970218     C                   GOTO      ENDCTF
028300970218     C                   ENDIF
028400970326     C                   ENDIF
028500970218     C*
028600970218     C     ENDCTF        ENDSR
028700970218     C*
028800910306     C* CONTROLLO FORMATO -------------------------------------------
028900910205     C     CONTR         BEGSR
029000910205     C                   SETOFF                                       90
029100941228     C                   CLEAR                   PARPDR
029200941228     C                   CLEAR                   V1DPDR
029300941228     C                   CLEAR                   V1DLNA
029400941228     C                   MOVEL     CTUTTE        V1DLNA
029500941228     C*
029600951114     C* CONTROLLO SE SI VUOLE EFFETTUARE UNA RICERCA    L N A
029700941228    1C     RICLNA        IFNE      *BLANKS
029800950111     C                   CLEAR                   RICLNA
029900941228     C                   EXSR      CTRLNA
030000941228     C*
030100910205     C* SE NRR = 0 NON CI SONO SEGNACOLLI DA STAMPARE
030200941228    2C     NRR           IFEQ      0
030300910205     C                   SETON                                        4490
030400910205     C                   GOTO      ENDCTR
030500941228    2C                   END
030600910205     C* EMETTO VIDEATA
030700941228     C                   WRITE     LS82D02
030800910205     C*
030900910205     C     FORCT         TAG
031000941228     C                   EXFMT     LS82DCT
031100910205     C* CMD12 RITORNO
031200941228    2C   KL              DO
031300941228     C                   MOVEL     *BLANKS       RICLNA
031400910205     C                   SETON                                        90
031500910205     C                   GOTO      ENDCTR
031600941228    2C                   END
031700910205     C*
031800941228    2C                   DO        *HIVAL
031900941228     C                   READC     LS82DSF                                31
032000941228    3C  N31VI5SCE        IFEQ      '1'
032100941228     C                   MOVEL     VI5LNA        V1CLNA
032200941228     C                   MOVEL     *BLANKS       RICLNA
032300941228     C                   MOVEL     DE5LNA        V1DLNA
032400910205     C                   MOVEL     *BLANKS       VI5SCE
032500941228     C                   UPDATE    LS82DSF
032600910205     C                   SETON                                        90
032700941228    3C                   END
032800941228     C  N90
032900941228    2CANN31              END
033000910205     C   31              GOTO      FORCT
033100910205     C*
033200941228    1C                   END
033300951114     C*
033400951114     C* CONTROLLO SE SI VUOLE EFFETTUARE UNA RICERCA  C L I E N T E
033500951114    1C     RICCLI        IFNE      *BLANKS
033600951114     C                   CLEAR                   RICCLI
033700951114     C                   EXSR      CTRCLI
033800951114     C*
033900951114     C* SE NRR = 0 NON CI SONO SEGNACOLLI DA STAMPARE
034000951114    2C     NRR2          IFEQ      0
034100951114     C                   SETON                                        4490
034200951114     C                   GOTO      ENDCTR
034300951114    2C                   END
034400951114     C* EMETTO VIDEATA
034500951114     C                   WRITE     LS82D03
034600951114     C*
034700951114     C     FORCT2        TAG
034800951114     C                   EXFMT     LS82DC2
034900951114     C* CMD12 RITORNO
035000951114    2C   KL              DO
035100951114     C                   MOVEL     *BLANKS       RICCLI
035200951114     C                   SETON                                        90
035300951114     C                   GOTO      ENDCTR
035400951114    2C                   END
035500951114     C*
035600951114    2C                   DO        *HIVAL
035700951114     C                   READC     LS82DS2                                31
035800951114    3C  N31V2CSCE        IFEQ      '1'
035900951114     C                   MOVEL     V2CKSC        V1CKSC
036000951114     C                   MOVEL     *BLANKS       RICCLI
036100951114     C                   MOVEL     V2DKSC        V1DKSC
036200951114     C                   MOVEL     *BLANKS       V2CSCE
036300951114     C                   UPDATE    LS82DS2
036400951114     C                   SETON                                        90
036500951114    3C                   END
036600951114     C  N90
036700951114    2CANN31              END
036800951114     C   31              GOTO      FORCT2
036900951114     C*
037000951114    1C                   END
037100910205     C*
037200941228     C* CONTROLLO SE SI TRATTA DI LINEA ESISTENTE
037300941228    1C     V1CLNA        IFGT      0
037400941228     C     V1CLNA        CHAIN     AZORG                              30
037500941228    2C     *IN30         IFEQ      *ON
037600941228     C     ORGFVA        ORNE      *BLANKS
037700941228     C     ORGFAG        OREQ      'V'
037800941228     C                   SETON                                        4290
037900941228     C                   GOTO      ENDCTR
038000941228    2C                   ENDIF
038100941228     C* DECODIFICA
038200941228     C                   MOVEL     ORGDES        V1DLNA
038300941228     C*
038400941228     C* CONTROLLO SE ESISTE ALMENO 1 BOLLA DA STAMPARE
038500970630    2C     V1CRIS        IFEQ      'N'
038600970630     C     KBLP          SETLL     FNBLP45L                               30
038700970630    3C     *IN30         IFEQ      *OFF
038800941228     C                   SETON                                        4490
038900941228     C                   GOTO      ENDCTR
039000970630    3C                   ENDIF
039100020607      * se la bolla esiste deve essere:del p.o. in gestione se inserito
039200020607     c                   eval      wokbolla = *off
039300020607 b3  c                   do        *hival
039400020607     c     kblp          reade     fnblp45l
039500020607     c                   if        %eof(fnblp45l)
039600020607     c                   leave
039700020607     c                   endif
039800020607      * del p.o. in gestione se inserito
039900020607     c                   if        v1clnp <> *zeros and blplnp <> v1clnp
040000020607     c                   iter
040100020607     c                   endif
040200020607      * far parte del terminal di partenza se non indicato il p.o. gestione
040300020607 b4  c                   if        v1clnp = *zeros
040400020607     c     blplnp        lookup    l1                                     30
040500020607     c                   if        not *in30
040600020607     c                   iter
040700020607     c                   endif
040800020607 e4  c                   endif
040900020607     c                   eval      wokbolla = *on
041000020607     c                   leave
041100020607 e3  c                   enddo
041200020607      * msg di errore
041300020607 b3  c                   if        wokbolla = *off
041400020607     c                   eval      *in44 = *on
041500020607     c                   eval      *in90 = *on
041600020607     c                   goto      endctr
041700020607 e3  c                   endif
041800970630    2C                   ENDIF
041900000511   X1C                   ELSE
042000000511     C* LINEA DI ARRIVO OBBLIGATORIA SE RICHIESTA LA STAMPA DI TUTTE
042100000511     C* LE LINEE DI PARTENZA
042200000511     C     V1CDPD        IFEQ      'T'
042300000511     C                   SETON                                        4790
042400000511     C                   GOTO      ENDCTR
042500000511     C                   ENDIF
042600970630    1C                   ENDIF
042700910205     C*
042800910205     C* C O D I C E   C L I E N T E
042900910205     C*
043000961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
043100961017    1C     V1CKLP        IFGT      0
043200961017     C     V1CKLP        CHAIN     AZORG01L                           32
043300961017    2C     *IN32         IFEQ      *ON
043400961017     C                   SETON                                        284090
043500961017     C                   MOVEL     MSG(1)        V1CMSG
043600961017     C                   GOTO      ENDCTR
043700961017    2C                   ENDIF
043800961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
043900961017    2C     V1CCLI        IFEQ      0
044000961017     C     V1DKSC        ANDEQ     *BLANKS
044100961017     C                   SETON                                        284190
044200961017     C                   MOVEL     MSG(2)        V1CMSG
044300961017     C                   GOTO      ENDCTR
044400961017    2C                   ENDIF
044500961017     C*
044600961017   X1C                   ELSE
044700961017    2C     V1CCLI        IFGT      0
044800961017     C                   SETON                                        284090
044900961017     C                   MOVEL     MSG(1)        V1CMSG
045000961017     C                   GOTO      ENDCTR
045100961017    2C                   ENDIF
045200961017    1C                   ENDIF
045300961017     C*
045400910205     C* RICERCA ALFABETICA
045500961017    1C     V1CCLI        IFEQ      0
045600941228     C     V1DKSC        ANDNE     *BLANKS
045700910205     C                   MOVEL     RSUT          PARDUT           30
045800910205     C* PARSTA=9 ESCLUDO ANNULLATI
045900941228     C                   CLEAR                   DESCDS
046000941228     C                   MOVEL     V1DKSC        DESCDS
046100910205     C                   Z-ADD     9             PARSTA
046200961016     C                   Z-ADD     CCC           PARCCC
046300961017     C                   MOVEL     V1CKLP        PARFLR
046400981109     C                   CLEAR                   PARDIT
046500000906     C                   Z-ADD     1             PAXNUM
046600000906     C                   CALL      'XALFA3BR'
046700910205     C                   PARM                    PARDUT
046800910205     C                   PARM                    CODUT
046900910205     C                   PARM                    DESCDS           48
047000961016     C                   PARM                    PARCCC            4 0
047100910205     C                   PARM                    PARSTA            1 0
047200961016     C                   PARM                    PARFLR           90
047300981109     C                   PARM                    PARDIT            3
047400000906     C                   PARM                    PAXNUM            2 0
047500000906     C                   PARM                    PAXKCM           80
047600000906     C                   PARM                    PAXKSM          140
047700000906     C                   PARM                    PAXKDM           60
047800941228    2C     PARSTA        IFGT      -1
047900000906     C                   MOVEL     PAXKSM        V1CKSC
048000941228     C                   MOVEL     DESCDS        V1DKSC
048100941228    2C                   END
048200910205     C                   GOTO      ENDCTR
048300941228    1C                   END
048400941228     C*
048500941228    1C     V1CKSC        IFGT      *ZEROS
048600941228     C                   MOVEL     V1CKSC        KSC
048700910205     C     KACO          CHAIN     CNACO000                           30
048800961017     C     *IN30         IFEQ      *ON
048900961017     C                   SETON                                        284190
049000961017     C                   MOVEL     MSG(2)        V1CMSG
049100961017     C                   GOTO      ENDCTR
049200961017     C                   ENDIF
049300941228    1C                   END
049400941228     C*
049500910205     C* C O D I C E   P A D R O N C I N O
049600910205     C* RICERCA ALFABETICA
049700941228    1C     V1CPDR        IFNE      *BLANKS
049800941228     C     V1CPDR        ANDNE     *ZEROS
049900941228     C     '?'           SCAN      V1CPDR                                 90
050000941228    2C     *IN90         IFEQ      *ON
050100021203      *
050200021203      * imposto i campi della ds esterna fnlv24ds
050300021203     C                   clear                   fnlv24ds
050400021203     C                   eval      d24tip   = 'A'
050500021203     C                   eval      d24flg   = 'R'
050600021203     C                   movel     fnlv24ds      kpjbu
050700941228     C                   CALL      'FNLV24R'
050800921012     C                   PARM                    KPJBA
050900021203     C                   movel     kpjbu         fnlv24ds
051000021203     C                   move      d24pdr        V1CPDR
051100021203     C                   movel     d24rsc        V1DPDR
051200910205     C                   SETON                                        90
051300910205     C                   GOTO      ENDCTR
051400941228    2C                   END
051500941228     C* ESEGUO TESTN
051600941228     C                   TESTN                   V1CPDR               30
051700941228     C                   MOVE      V1CPDR        W001A             1
051800941228    2C     *IN30         IFEQ      *OFF
051900941228     C     W001A         ORLT      '0'
052000941228     C                   SETON                                        4390
052100941228     C                   GOTO      ENDCTR
052200941228    2C                   ENDIF
052300941228     C*
052400941228     C                   MOVE      V1CPDR        W0070             7 0
052500021203     C*    W0070         CHAIN     FNAPD000                           30
052600021203     C                   move      w0070         kpdpdr
052700021203     C                   move      'A'           kpdtip
052800021203     C     kfapd         chain     fiapd01l                           30
052900941228    2C     *IN30         IFEQ      *ON
053000941228     C                   SETON                                        4390
053100941228     C                   GOTO      ENDCTR
053200941228    2C                   ENDIF
053300020607     c                   movel     v1cpdr        wpopdc
053400020607      * se inserito il p.o. gestione deve essere in gestione al p.o.
053500020607     c                   if        v1clnp <> *zeros and wpopdc <> v1clnp
053600020607     c                   eval      *in48 = *on
053700020607     c                   eval      *in28 = *on
053800020607     c                   eval      *in90 = *on
053900020607     c                   eval      v1cmsg = msg(10)
054000020607     c                   goto      endctr
054100020607     c                   endif
054200020607      * se non inserito il p.o. gestione deve essere della �1
054300020607     c                   if        v1clnp = *zeros
054400020607     c     wpopdc        lookup    l1                                     30
054500020607     c                   if        not *in30
054600020607     c                   eval      *in48 = *on
054700020607     c                   eval      *in28 = *on
054800020607     c                   eval      *in90 = *on
054900020607     c                   eval      v1cmsg = msg(10)
055000020607     c                   goto      endctr
055100020607     c                   endif
055200020607     c                   endif
055300941228     C*
055400941228     C                   MOVEL     APDRSC        V1DPDR
055500941228    1C                   ENDIF
055600970326     C* DATA SPEDIZIONE DAL / AL
055700970326     C                   CLEAR                   PARDSD
055800970326     C                   CLEAR                   PARDSA
055900970326     C     V1CDSD        IFGT      0
056000970326     C                   Z-ADD     V1CDSD        G02DAT
056100970326     C                   MOVEL     *BLANK        G02ERR
056200970326     C                   CALL      'XSRDA8'
056300970326     C                   PARM                    WLBDAT
056400970326     C* ERRATA
056500970326    1C     G02ERR        IFEQ      '1'
056600970326     C                   SETON                                        452890
056700970326     C                   MOVEL     MSG(3)        V1CMSG
056800970326     C                   GOTO      ENDCTR
056900970326     C                   ELSE
057000970326     C                   Z-ADD     G02INV        PARDSD
057100970326     C                   Z-ADD     G02DAT        V1CDSD
057200970326     C                   ENDIF
057300970326     C                   ENDIF
057400970326     C     V1CDSA        IFGT      0
057500970326     C                   Z-ADD     V1CDSA        G02DAT
057600970326     C                   MOVEL     *BLANK        G02ERR
057700970326     C                   CALL      'XSRDA8'
057800970326     C                   PARM                    WLBDAT
057900970326     C* ERRATA
058000970326    1C     G02ERR        IFEQ      '1'
058100970326     C                   SETON                                        462890
058200970326     C                   MOVEL     MSG(3)        V1CMSG
058300970326     C                   GOTO      ENDCTR
058400970326     C                   ELSE
058500970326     C                   Z-ADD     G02INV        PARDSA
058600970326     C                   Z-ADD     G02DAT        V1CDSA
058700970326     C*
058800970326     C     PARDSA        IFLT      PARDSD
058900970326     C                   SETON                                        462890
059000970326     C                   MOVEL     MSG(4)        V1CMSG
059100970326     C                   GOTO      ENDCTR
059200970326     C                   ENDIF
059300970326     C**
059400970326     C                   ENDIF
059500970326     C                   ENDIF
059600970627     C** NUMERO SPEDIZIONE SI PUO' IMMETTERE SOLO SE RISTAMPA
059700970627     C     V1CRIS        IFEQ      'N'
059800970630     C     WNSD          IFGT      0
059900970630     C     WNSA          ORGT      0
060000970630     C                   MOVEL     MSG(6)        V1CMSG
060100970630     C                   SETON                                        509028
060200970627     C                   GOTO      ENDCTR
060300970627     C                   ENDIF
060400970627     C                   ENDIF
060500970627     C**
060600970627     C* SE RICHIESTA RISTAMPA IMMETTERE NUMERO O DATA SPEDIZIONE
060700970630    1C     V1CRIS        IFEQ      'S'
060800970630    2C     WNSD          IFEQ      0
060900970630     C     WNSA          ANDEQ     0
061000970627     C     V1CDSD        ANDEQ     0
061100970627     C     V1CDSA        ANDEQ     0
061200970630     C                   MOVEL     MSG(7)        V1CMSG
061300970630     C                   SETON                                        509028
061400970627     C                   GOTO      ENDCTR
061500970630    2C                   ENDIF
061600970630     c* numero spedizione al non puo' essere inferiore del numero dal
061700020626     c                   if        wnsa < wnsd and wnsa > *zeros
061800970630     C                   MOVEL     MSG(8)        V1CMSG
061900970630     C                   SETON                                        509028
062000970630     C                   GOTO      ENDCTR
062100970630    2C                   ENDIF
062200970630     C* SE IMMESSO NUMERO DI SPEDIZIONE IMMETTERE ANCHE PO IN GESTIONE
062300970630    2C     WNSD          IFGT      0
062400970630     C     WNSA          ORGT      0
062500970630    3C     V1CLNP        IFEQ      0
062600970630     C                   MOVEL     MSG(9)        V1CMSG
062700970630     C                   SETON                                        559028
062800970630     C                   GOTO      ENDCTR
062900970630    3C                   ENDIF
063000970630    2C                   ENDIF
063100970630    1C                   ENDIF
063200910412     C*
063300910205     C     ENDCTR        ENDSR
063400910205     C*
063500941228     C* RICERCA LINEE CON BOLLE ANCORA DA STAMPARE -------------------*
063600941228     C     CTRLNA        BEGSR
063700910205     C                   EXSR      PULSF
063800910205     C                   Z-ADD     0             NRR               4 0
063900941228     C                   Z-ADD     1             LNA               3 0
064000970630     C     KBLP2         SETLL     FNBLP45L
064100970630     C     WFST          READE     FNBLP45L                               30
064200941228     C*
064300941228     C     *IN30         DOWEQ     *OFF
064400020719      * deve essere del p.o. in gestione se inserito
064500020719     c                   if        v1clnp <> *zeros and blplnp <> v1clnp
064600020719     c                   goto      nobolla
064700020719     c                   endif
064800020719      * deve far parte del terminal di partenza se non indicato il p.o. gestione
064900020719 b4  c                   if        v1clnp = *zeros
065000020719     c     blplnp        lookup    l1                                     31
065100020719     c                   if        not *in31
065200020719     c                   goto      nobolla
065300020719     c                   endif
065400020719 e4  c                   endif
065500941228     C*
065600941228     C     BLPLNA        CHAIN     AZORG                              31
065700941228     C  N31              MOVEL     ORGDES        DE5LNA
065800941228     C   31              MOVEL     *BLANKS       DE5LNA
065900941228     C                   MOVEL     BLPLNA        VI5LNA
066000910205     C* SCRIVO SUBFILE
066100910205     C                   ADD       1             NRR
066200941228     C                   WRITE     LS82DSF
066300941228     C*
066400910412     C* MI RIPOSIZIONO SU UN CODICE PIU' ALTO
066500941228     C                   Z-ADD     BLPLNA        LNA
066600941228     C                   ADD       1             LNA
066700970630     C     KBLP2         SETLL     FNBLP45L
066800020719     c     nobolla       tag
066900910205     C*
067000970630     C     WFST          READE     FNBLP45L                               30
067100941228     C                   ENDDO
067200910205     C*
067300910205     C                   ENDSR
067400951114     C*
067500951114     C* RICERCA CLIENTI CON BOLLE ANCORA DA STAMPARE -----------------*
067600951114     C     CTRCLI        BEGSR
067700951114     C                   CLEAR                   CLI
067800951114     C*
067900951114     C                   SETON                                        35
068000951114     C                   WRITE     LS82DC2
068100951114     C                   SETOFF                                       3507
068200951114     C*
068300951114     C                   Z-ADD     1             I                 3 0
068400951114     C                   Z-ADD     0             NRR2              4 0
068500970630     C     WFST          SETLL     FNBLP45L
068600970630     C     WFST          READE     FNBLP45L                               30
068700951114     C*
068800951114     C     *IN30         DOWEQ     *OFF
068900020719      * deve essere del p.o. in gestione se inserito
069000020719     c                   if        v1clnp <> *zeros and blplnp <> v1clnp
069100020719     c                   goto      nocli
069200020719     c                   endif
069300020719      * deve far parte del terminal di partenza se non indicato il p.o. gestione
069400020719 b4  c                   if        v1clnp = *zeros
069500020719     c     blplnp        lookup    l1                                     31
069600020719     c                   if        not *in31
069700020719     c                   goto      nocli
069800020719     c                   endif
069900020719 e4  c                   endif
070000951114     C     BLPCCM        IFGT      0
070100951114     C                   MOVEL     BLPCCM        KSC
070200951114     C                   ELSE
070300951114     C                   MOVEL     BLPKSC        KSC               7 0
070400951114     C                   ENDIF
070500951114     C     KSC           LOOKUP    CLI                                    31
070600951114     C     *IN31         IFEQ      *OFF
070700951114     C                   Z-ADD     KSC           V2CKSC
070800951114     C                   Z-ADD     KSC           CLI(I)
070900951114     C                   ADD       1             I
071000951114     C     KACO          CHAIN     CNACO000                           30
071100951114     C  N30              MOVEL     ACORAG        V2DKSC
071200951114     C   30              CLEAR                   V2DKSC
071300951114     C   30              MOVEL     BLPRSM        V2DKSC
071400951114     C                   CLEAR                   V2CSCE
071500951114     C                   ADD       1             NRR2
071600951114     C                   WRITE     LS82DS2
071700951114     C                   ENDIF
071800020719     c     nocli         tag
071900951114     C*
072000951114     C     I             IFEQ      101
072100951114     C                   SETON                                        30
072200951114     C                   ELSE
072300970630     C     WFST          READE     FNBLP45L                               30
072400951114     C                   ENDIF
072500951114     C*
072600951114     C                   ENDDO
072700951114     C     I             IFEQ      101
072800951114     C                   SETON                                        07
072900951114     C                   ENDIF
073000951114     C*
073100951114     C                   ENDSR
073200910205     C*
073300910205     C* PULIZIA SUBFILE ---------------------------------------------
073400910205     C     PULSF         BEGSR
073500910205     C                   SETON                                        35
073600941228     C                   WRITE     LS82DCT
073700910205     C                   SETOFF                                       35
073800910205     C                   ENDSR
073900961017     C*---------------------------------------------------------------*
074000970630**  MSG
074100961017Immettere linea di partenza valida                                            1
074200961017Codice Cliente inesistente                                                    2
074300970630Data Errata                                                                   3
074400970630Data AL minore della data DAL                                                 4
074500970630Linea di partenza errata                                                      5
074600970630Numero spedizione dal-al si puo' immettere soltanto per ristampa              6
074700970630In ristampa effettuare selezione per data o per numero spedizione             7
074800970630Numero spedizione al non puo' essere inferiore del numero dal
074900970630Se immesso numero di spedizione immettere anche il P.O. in gestione
075000020607Autotrasportatore in gestione ad altro p.o.                                   10
