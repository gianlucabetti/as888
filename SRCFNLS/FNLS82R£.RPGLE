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
004900030620     d* � modulo per stampa bolle/fatture   (ad aghi)
005000030620     d  wD90mdb              237    246
005100030620     d* � stampante per bolle/fatture       (ad aghi)
005200030620     d  wD90prb              247    256
005300030620      *
005400021203      * Ds esterna per ricerca codice padroncino
005500021203     D FNLV24DS      E DS
005600021203      *
005700970326     D WLBDAT          DS                  INZ
005800970326     D  G02DAT                 1      8  0
005900970326     D  G02INV                 9     16  0
006000970326     D  G02ERR                17     17
006100970326     D  G02TGI                18     22  0
006200970218     D*
006300900517     D KPJBA         E DS
006400970218     D*
006500910205     D TCUDS           DS
006600910205     D  F1                     1      1
006700910205     D  F3                     3      3
006800910205     D  F2                     2      2
006900910205     D  F4                     4      4
007000910205     D  F56                    5      6
007100970218     D*
007200961017     D                 DS
007300961017     D  V1CKLP                 1      3  0
007400961017     D  V1CCLI                 4      7  0
007500961017     D  V1CKSC                 1      7  0
007600030620      *
007700030620      * Parametri per stampa BOLLE ed ETICHETTE
007800030620     d TRUL90DS      e ds                  inz
007900030620     d   D90rse      e                     inz('N')
008000030620     d   D90rsb      e                     inz('S')
008100970326     D*
008200970326     D* DS PER TRUL06R - CARICAMENTO 즍
008300970326     D DSUL06        E DS                  EXTNAME(TRUL06DS)
008400970326     D  LIN                    1     90  0
008500970326     D                                     DIM(30)
008600941228     D*
008700970218     D CNCR80        E DS
008800970218     D UT쬎SE        E DS                  EXTNAME(UT쬎SE0F)
008900000000     D  TCU                  398    697
009000000000     D                                     DIM(50)
009100000000     D  KCU                  698    847P 0
009200000000     D                                     DIM(50)
009300000000     D                                     PACKEVEN
009400970218     D*
009500941228     D* DEFINIZIONE COSTANTI
009600941228     D CTUTTE          C                   CONST('0 = TUTTE')
009700021203
009800021203     D kpdpdr          S                   LIKE(APDpdr)
009900021203     D kpdtip          S                   LIKE(APDtip)
010000910412     C****************************************************************
010100910412     C*  RIEPILOGO INDICATORI
010200910412     C***************************************************************
010300020430     C* 05    - SONO UN SECONDO LIVELLO
010400970218     C* 06    - CAMBIO FILIALE IN GESTIONE PER CMD18
010500910412     C* 10    - RICHIESTA RISTAMPA
010600970218     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
010700910412     C* 30-32 - DI COMODO
010800910412     C* 35    - PULIZIA SUBFILES
010900020607     C* 40-48 - ERRORI NEI CAMPI
011000970326     C* 55-   - ERRORI NEI CAMPI
011100910412     C* 90    - INDICATORE GENERICO DI ERRORE
011200910412     C*****************************************************************
011300000000     C     *ENTRY        PLIST
011400000000     C                   PARM                    KPJBA
011500000000     C                   Z-ADD     1             CODUT
011600970218     C                   CALL      'X쬝ARUT'
011700970218     C                   PARM                    UT쬎SE
011800000000     C                   MOVEL     RAGUT         RSUT             20
011900970218     C                   MOVEL     REC80         CNCR80
012000910205     C*--- RICERCA CAPOCONTI------------------------------------------*
012100910205     C                   DO        50            X                 2 0
012200910205     C                   MOVE      TCU(X)        TCUDS
012300910205     C     F56           CABNE     'CG'          END1
012400910205     C     F4            COMP      '1'                                    21
012500910205     C     F4            COMP      '2'                                    22
012600910205     C     F4            COMP      '3'                                    23
012700910205     C     F4            COMP      '6'                                    27
012800910205     C** 1 CLIENTI   21
012900910205     C** 2 FORNITORI 22
013000910205     C** 3 AGENTI    23
013100910205     C     F3            COMP      '0'                                242425
013200910205     C     F3            COMP      'I'                                    26
013300910205     C** 0 ITALIA   25
013400910205     C** 1 ESTERO   24
013500910205     ** I CAPO CONTO IVA
013600910205     C   21
013700910205     CAN 24              Z-ADD     KCU(X)        KCE               4 0
013800910205     C   21
013900910205     CAN 25              Z-ADD     KCU(X)        KCI               4 0
014000910205     C   22
014100910205     CAN 24              Z-ADD     KCU(X)        KFE               4 0
014200910205     C   22
014300910205     CAN 25              Z-ADD     KCU(X)        KFI               4 0
014400910205     C   23
014500910205     CAN 24              Z-ADD     KCU(X)        KAE               4 0
014600910205     C   23
014700910205     CAN 25              Z-ADD     KCU(X)        KAI               4 0
014800910205     C   26              Z-ADD     KCU(X)        KIVA              4 0
014900910205     C   27              Z-ADD     KCU(X)        KBNA              4 0
015000910205     C     END1          TAG
015100910205     C                   END
015200900511     C*---------------------------------------------------------------*
015300910205     C* ACCESSO   CNACO                                    *
015400910205     C     KACO          KLIST
015500910205     C                   KFLD                    CODUT
015600910205     C                   KFLD                    CCC               4 0
015700910205     C                   KFLD                    KSC               7 0
015800970630     C     KBLP          KLIST
015900970630     C                   KFLD                    WFST              1
016000970630     C                   KFLD                    V1CLNA
016100970630     C     KBLP2         KLIST
016200970630     C                   KFLD                    WFST              1
016300970630     C                   KFLD                    LNA
016400021203     C     Kfapd         klist
016500021203     C                   kfld                    kpdtip
016600021203     C                   kfld                    kpdpdr
016700910205     C*---------------------------------------------------------------*
016800970630     C                   MOVEL     'N'           WFST
016900970326     C***
017000970326     C* CARICO TABELLA PUNTI OPERATIVI GESTITI �1
017100970326     C***
017200970326     C                   CLEAR                   DSUL06
017300970326     C                   MOVE      '�1'          D06COD
017400970326     C                   MOVEL     SIMFEL        D06KEY
017500970326     C                   MOVEL     'L'           D06TLA
017600970326     C                   MOVEL     DSUL06        KPJBU
017700970326     C*
017800970326     C                   CALL      'TRUL06R'
017900970326     C                   PARM                    KPJBA
018000970326     C                   MOVEL     KPJBU         DSUL06
018100970326     C                   MOVEA     LIN           L1
018200970326     C**
018300020430     C* VEDO SE SONO PRIMO O SECODNO LIVELLO
018400020430     C*                    MOVELSIMFEL    V1CLNP
018500020430     C                   Z-ADD     SIMPOU        V1CLNP
018600970218     C*
018700020430     C     SIMTPP        IFEQ      '2'
018800020430     C     SIMTPP        OREQ      *BLANKS
018900970218     C                   SETON                                        05
019000970218     C                   ENDIF
019100030620      *
019200030620      * Richiesta stampanti
019300030620     c                   reset                   TRUL90DS
019400030620     c                   call      'TRUL90R'
019500030620     c                   parm                    KPJBA
019600030620     c                   parm                    TRUL90DS
019700030620     c*
019800950104     C* F3 - FINE
019900030620     C     D90F3         IFEQ      '1'
020000950104     C                   GOTO      FINE
020100950104     C                   ENDIF
020200030620      *
020300030620      * Parametri x ovrprtf
020400030620      * � pgm. per stampa LDV ............. - ad aghi o laser
020500030620     c                   movel     D90psl        wD90psl
020600030620      * � stampante per LDV in formato "A4" - laser
020700030620     c                   movel     D90prb4       wD90prb4
020800030620      * � stampante per LDV in formato "A5" - laser
020900030620     c                   movel     D90prb5       wD90prb5
021000030620      * � modulo per stampa LDV su "A4" ... - laser
021100030620     c                   movel     D90mdb4       wD90mdb4
021200030620      * � modulo per stampa LDV su "A5" ... - laser
021300030620     c                   movel     D90mdb5       wD90mdb5
021400030620      * � stampante per bolle/fatture ..... - ad aghi
021500030620     c                   movel     D90prb        wD90prb
021600030620      * � modulo per stampa bolle/fatture . - ad aghi
021700030620     c                   movel     D90mdb        wD90mdb
021800900517     C*
021900910205     C                   Z-ADD     KCI           CCC
022000950411     C     INIZIO        TAG
022100941228     C                   MOVEL     *BLANKS       V1CLNA
022200000511     C                   MOVEL     'D'           V1CDPD
022300941228     C                   MOVEL     *BLANKS       V1DLNA
022400941228     C                   MOVEL     CTUTTE        V1DLNA
022500941228     C                   MOVEL     *ZEROS        V1CKSC
022600941228     C                   MOVEL     *BLANKS       V1DKSC
022700941228     C                   MOVEL     *BLANKS       V1CPDR
022800941228     C                   MOVEL     *BLANKS       V1DPDR
022900970326     C                   CLEAR                   V1CDSD
023000970326     C                   CLEAR                   V1CDSA
023100970627     C                   CLEAR                   V1CNSD
023200970627     C                   CLEAR                   V1CNSA
023300970630     C                   CLEAR                   V1CNRD
023400970630     C                   CLEAR                   V1CNRA
023500970627     C                   MOVEL     'N'           V1CRIS
023600900509     C     FOR01         TAG
023700941228     C                   EXFMT     LS82D01
023800961017     C*
023900961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
024000961017     C                   CLEAR                   V1CMSG
024100961017     C                   SETOFF                                       284041
024200970326     C                   SETOFF                                       554546
024300020607     C                   SETOFF                                       5048
024400961017     C*
024500941228     C** F3 - FINE LAVORO
024600900509     C   KC              GOTO      FINE
024700970218     C*
024800970218     C** CMD18- CAMBIO FILIALE
024900970218     C   KS              SETON                                        0655
025000970218     C   KS              GOTO      FOR01
025100970218     C*
025200970218     C* 06 ON - CONTROLLO FILIALE IN GESTIONE
025300970218     C   06              EXSR      CTRFGS
025400970218     C   06
025500970218     CAN 90              GOTO      FOR01
025600970218     C*
025700910205     C* CONTROLLI FORMATO
025800910205     C                   EXSR      CONTR
025900941228     C  NKF
026000941228     COR 90              GOTO      FOR01
026100901214     C*
026200900511     C* ELABORAZIONE
026300941228     C     V1CPDR        IFGT      *ZEROS
026400941228     C                   MOVEL     V1CPDR        PARPDR
026500941228     C                   ENDIF
026600961017     C                   MOVEL     V1CKSC        PARKSC
026700961017     C*
026800900523     C                   MOVEL     PARAM         KPJBU
026900941228     C                   MOVEL     'LS83'        KCOAZ
027000030620     C***                CALL      'BCH10'
027100030715     c***                movel(p)  'FNLS83C'     Wpgm             10
027200030620     c                   movel(p)  'BCH10'       Wpgm             10
027300030620     c                   call      Wpgm
027400900523     C                   PARM                    KPJBA
027500941228     C*
027600950411     C                   GOTO      INIZIO
027700000000     C     FINE          TAG
027800000000     C                   SETON                                        LR
027900970218     C*
028000970218     C* CONTROLLO LA FILIALE IN GESTIONE ----------------------------
028100970218     C     CTRFGS        BEGSR
028200970218     C                   SETOFF                                       90
028300970218     C*
028400970218     C* POSSO ACCTTARE ANCHE 0 E SIGNIFICA TUTTE LE LINEE
028500970326     C     V1CLNP        IFGT      0
028600970326     C     V1CLNP        LOOKUP    L1                                     30
028700970326     C     *IN30         IFEQ      *OFF
028800970326     C                   MOVEL     MSG(5)        V1CMSG
028900970218     C                   SETON                                        559028
029000970218     C                   GOTO      ENDCTF
029100970218     C                   ENDIF
029200970326     C                   ENDIF
029300970218     C*
029400970218     C     ENDCTF        ENDSR
029500970218     C*
029600910306     C* CONTROLLO FORMATO -------------------------------------------
029700910205     C     CONTR         BEGSR
029800910205     C                   SETOFF                                       90
029900941228     C                   CLEAR                   PARPDR
030000941228     C                   CLEAR                   V1DPDR
030100941228     C                   CLEAR                   V1DLNA
030200941228     C                   MOVEL     CTUTTE        V1DLNA
030300941228     C*
030400951114     C* CONTROLLO SE SI VUOLE EFFETTUARE UNA RICERCA    L N A
030500941228    1C     RICLNA        IFNE      *BLANKS
030600950111     C                   CLEAR                   RICLNA
030700941228     C                   EXSR      CTRLNA
030800941228     C*
030900910205     C* SE NRR = 0 NON CI SONO SEGNACOLLI DA STAMPARE
031000941228    2C     NRR           IFEQ      0
031100910205     C                   SETON                                        4490
031200910205     C                   GOTO      ENDCTR
031300941228    2C                   END
031400910205     C* EMETTO VIDEATA
031500941228     C                   WRITE     LS82D02
031600910205     C*
031700910205     C     FORCT         TAG
031800941228     C                   EXFMT     LS82DCT
031900910205     C* CMD12 RITORNO
032000941228    2C   KL              DO
032100941228     C                   MOVEL     *BLANKS       RICLNA
032200910205     C                   SETON                                        90
032300910205     C                   GOTO      ENDCTR
032400941228    2C                   END
032500910205     C*
032600941228    2C                   DO        *HIVAL
032700941228     C                   READC     LS82DSF                                31
032800941228    3C  N31VI5SCE        IFEQ      '1'
032900941228     C                   MOVEL     VI5LNA        V1CLNA
033000941228     C                   MOVEL     *BLANKS       RICLNA
033100941228     C                   MOVEL     DE5LNA        V1DLNA
033200910205     C                   MOVEL     *BLANKS       VI5SCE
033300941228     C                   UPDATE    LS82DSF
033400910205     C                   SETON                                        90
033500941228    3C                   END
033600941228     C  N90
033700941228    2CANN31              END
033800910205     C   31              GOTO      FORCT
033900910205     C*
034000941228    1C                   END
034100951114     C*
034200951114     C* CONTROLLO SE SI VUOLE EFFETTUARE UNA RICERCA  C L I E N T E
034300951114    1C     RICCLI        IFNE      *BLANKS
034400951114     C                   CLEAR                   RICCLI
034500951114     C                   EXSR      CTRCLI
034600951114     C*
034700951114     C* SE NRR = 0 NON CI SONO SEGNACOLLI DA STAMPARE
034800951114    2C     NRR2          IFEQ      0
034900951114     C                   SETON                                        4490
035000951114     C                   GOTO      ENDCTR
035100951114    2C                   END
035200951114     C* EMETTO VIDEATA
035300951114     C                   WRITE     LS82D03
035400951114     C*
035500951114     C     FORCT2        TAG
035600951114     C                   EXFMT     LS82DC2
035700951114     C* CMD12 RITORNO
035800951114    2C   KL              DO
035900951114     C                   MOVEL     *BLANKS       RICCLI
036000951114     C                   SETON                                        90
036100951114     C                   GOTO      ENDCTR
036200951114    2C                   END
036300951114     C*
036400951114    2C                   DO        *HIVAL
036500951114     C                   READC     LS82DS2                                31
036600951114    3C  N31V2CSCE        IFEQ      '1'
036700951114     C                   MOVEL     V2CKSC        V1CKSC
036800951114     C                   MOVEL     *BLANKS       RICCLI
036900951114     C                   MOVEL     V2DKSC        V1DKSC
037000951114     C                   MOVEL     *BLANKS       V2CSCE
037100951114     C                   UPDATE    LS82DS2
037200951114     C                   SETON                                        90
037300951114    3C                   END
037400951114     C  N90
037500951114    2CANN31              END
037600951114     C   31              GOTO      FORCT2
037700951114     C*
037800951114    1C                   END
037900910205     C*
038000941228     C* CONTROLLO SE SI TRATTA DI LINEA ESISTENTE
038100941228    1C     V1CLNA        IFGT      0
038200941228     C     V1CLNA        CHAIN     AZORG                              30
038300941228    2C     *IN30         IFEQ      *ON
038400941228     C     ORGFVA        ORNE      *BLANKS
038500941228     C     ORGFAG        OREQ      'V'
038600941228     C                   SETON                                        4290
038700941228     C                   GOTO      ENDCTR
038800941228    2C                   ENDIF
038900941228     C* DECODIFICA
039000941228     C                   MOVEL     ORGDES        V1DLNA
039100941228     C*
039200941228     C* CONTROLLO SE ESISTE ALMENO 1 BOLLA DA STAMPARE
039300970630    2C     V1CRIS        IFEQ      'N'
039400970630     C     KBLP          SETLL     FNBLP45L                               30
039500970630    3C     *IN30         IFEQ      *OFF
039600941228     C                   SETON                                        4490
039700941228     C                   GOTO      ENDCTR
039800970630    3C                   ENDIF
039900020607      * se la bolla esiste deve essere:del p.o. in gestione se inserito
040000020607     c                   eval      wokbolla = *off
040100020607 b3  c                   do        *hival
040200020607     c     kblp          reade     fnblp45l
040300020607     c                   if        %eof(fnblp45l)
040400020607     c                   leave
040500020607     c                   endif
040600020607      * del p.o. in gestione se inserito
040700020607     c                   if        v1clnp <> *zeros and blplnp <> v1clnp
040800020607     c                   iter
040900020607     c                   endif
041000020607      * far parte del terminal di partenza se non indicato il p.o. gestione
041100020607 b4  c                   if        v1clnp = *zeros
041200020607     c     blplnp        lookup    l1                                     30
041300020607     c                   if        not *in30
041400020607     c                   iter
041500020607     c                   endif
041600020607 e4  c                   endif
041700020607     c                   eval      wokbolla = *on
041800020607     c                   leave
041900020607 e3  c                   enddo
042000020607      * msg di errore
042100020607 b3  c                   if        wokbolla = *off
042200020607     c                   eval      *in44 = *on
042300020607     c                   eval      *in90 = *on
042400020607     c                   goto      endctr
042500020607 e3  c                   endif
042600970630    2C                   ENDIF
042700000511   X1C                   ELSE
042800000511     C* LINEA DI ARRIVO OBBLIGATORIA SE RICHIESTA LA STAMPA DI TUTTE
042900000511     C* LE LINEE DI PARTENZA
043000000511     C     V1CDPD        IFEQ      'T'
043100000511     C                   SETON                                        4790
043200000511     C                   GOTO      ENDCTR
043300000511     C                   ENDIF
043400970630    1C                   ENDIF
043500910205     C*
043600910205     C* C O D I C E   C L I E N T E
043700910205     C*
043800961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
043900961017    1C     V1CKLP        IFGT      0
044000961017     C     V1CKLP        CHAIN     AZORG01L                           32
044100961017    2C     *IN32         IFEQ      *ON
044200961017     C                   SETON                                        284090
044300961017     C                   MOVEL     MSG(1)        V1CMSG
044400961017     C                   GOTO      ENDCTR
044500961017    2C                   ENDIF
044600961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
044700961017    2C     V1CCLI        IFEQ      0
044800961017     C     V1DKSC        ANDEQ     *BLANKS
044900961017     C                   SETON                                        284190
045000961017     C                   MOVEL     MSG(2)        V1CMSG
045100961017     C                   GOTO      ENDCTR
045200961017    2C                   ENDIF
045300961017     C*
045400961017   X1C                   ELSE
045500961017    2C     V1CCLI        IFGT      0
045600961017     C                   SETON                                        284090
045700961017     C                   MOVEL     MSG(1)        V1CMSG
045800961017     C                   GOTO      ENDCTR
045900961017    2C                   ENDIF
046000961017    1C                   ENDIF
046100961017     C*
046200910205     C* RICERCA ALFABETICA
046300961017    1C     V1CCLI        IFEQ      0
046400941228     C     V1DKSC        ANDNE     *BLANKS
046500910205     C                   MOVEL     RSUT          PARDUT           30
046600910205     C* PARSTA=9 ESCLUDO ANNULLATI
046700941228     C                   CLEAR                   DESCDS
046800941228     C                   MOVEL     V1DKSC        DESCDS
046900910205     C                   Z-ADD     9             PARSTA
047000961016     C                   Z-ADD     CCC           PARCCC
047100961017     C                   MOVEL     V1CKLP        PARFLR
047200981109     C                   CLEAR                   PARDIT
047300000906     C                   Z-ADD     1             PAXNUM
047400000906     C                   CALL      'XALFA3BR'
047500910205     C                   PARM                    PARDUT
047600910205     C                   PARM                    CODUT
047700910205     C                   PARM                    DESCDS           48
047800961016     C                   PARM                    PARCCC            4 0
047900910205     C                   PARM                    PARSTA            1 0
048000961016     C                   PARM                    PARFLR           90
048100981109     C                   PARM                    PARDIT            3
048200000906     C                   PARM                    PAXNUM            2 0
048300000906     C                   PARM                    PAXKCM           80
048400000906     C                   PARM                    PAXKSM          140
048500000906     C                   PARM                    PAXKDM           60
048600941228    2C     PARSTA        IFGT      -1
048700000906     C                   MOVEL     PAXKSM        V1CKSC
048800941228     C                   MOVEL     DESCDS        V1DKSC
048900941228    2C                   END
049000910205     C                   GOTO      ENDCTR
049100941228    1C                   END
049200941228     C*
049300941228    1C     V1CKSC        IFGT      *ZEROS
049400941228     C                   MOVEL     V1CKSC        KSC
049500910205     C     KACO          CHAIN     CNACO000                           30
049600961017     C     *IN30         IFEQ      *ON
049700961017     C                   SETON                                        284190
049800961017     C                   MOVEL     MSG(2)        V1CMSG
049900961017     C                   GOTO      ENDCTR
050000961017     C                   ENDIF
050100941228    1C                   END
050200941228     C*
050300910205     C* C O D I C E   P A D R O N C I N O
050400910205     C* RICERCA ALFABETICA
050500941228    1C     V1CPDR        IFNE      *BLANKS
050600941228     C     V1CPDR        ANDNE     *ZEROS
050700941228     C     '?'           SCAN      V1CPDR                                 90
050800941228    2C     *IN90         IFEQ      *ON
050900021203      *
051000021203      * imposto i campi della ds esterna fnlv24ds
051100021203     C                   clear                   fnlv24ds
051200021203     C                   eval      d24tip   = 'A'
051300021203     C                   eval      d24flg   = 'R'
051400021203     C                   movel     fnlv24ds      kpjbu
051500941228     C                   CALL      'FNLV24R'
051600921012     C                   PARM                    KPJBA
051700021203     C                   movel     kpjbu         fnlv24ds
051800021203     C                   move      d24pdr        V1CPDR
051900021203     C                   movel     d24rsc        V1DPDR
052000910205     C                   SETON                                        90
052100910205     C                   GOTO      ENDCTR
052200941228    2C                   END
052300941228     C* ESEGUO TESTN
052400941228     C                   TESTN                   V1CPDR               30
052500941228     C                   MOVE      V1CPDR        W001A             1
052600941228    2C     *IN30         IFEQ      *OFF
052700941228     C     W001A         ORLT      '0'
052800941228     C                   SETON                                        4390
052900941228     C                   GOTO      ENDCTR
053000941228    2C                   ENDIF
053100941228     C*
053200941228     C                   MOVE      V1CPDR        W0070             7 0
053300021203     C*    W0070         CHAIN     FNAPD000                           30
053400021203     C                   move      w0070         kpdpdr
053500021203     C                   move      'A'           kpdtip
053600021203     C     kfapd         chain     fiapd01l                           30
053700941228    2C     *IN30         IFEQ      *ON
053800941228     C                   SETON                                        4390
053900941228     C                   GOTO      ENDCTR
054000941228    2C                   ENDIF
054100020607     c                   movel     v1cpdr        wpopdc
054200020607      * se inserito il p.o. gestione deve essere in gestione al p.o.
054300020607     c                   if        v1clnp <> *zeros and wpopdc <> v1clnp
054400020607     c                   eval      *in48 = *on
054500020607     c                   eval      *in28 = *on
054600020607     c                   eval      *in90 = *on
054700020607     c                   eval      v1cmsg = msg(10)
054800020607     c                   goto      endctr
054900020607     c                   endif
055000020607      * se non inserito il p.o. gestione deve essere della �1
055100020607     c                   if        v1clnp = *zeros
055200020607     c     wpopdc        lookup    l1                                     30
055300020607     c                   if        not *in30
055400020607     c                   eval      *in48 = *on
055500020607     c                   eval      *in28 = *on
055600020607     c                   eval      *in90 = *on
055700020607     c                   eval      v1cmsg = msg(10)
055800020607     c                   goto      endctr
055900020607     c                   endif
056000020607     c                   endif
056100941228     C*
056200941228     C                   MOVEL     APDRSC        V1DPDR
056300941228    1C                   ENDIF
056400970326     C* DATA SPEDIZIONE DAL / AL
056500970326     C                   CLEAR                   PARDSD
056600970326     C                   CLEAR                   PARDSA
056700970326     C     V1CDSD        IFGT      0
056800970326     C                   Z-ADD     V1CDSD        G02DAT
056900970326     C                   MOVEL     *BLANK        G02ERR
057000970326     C                   CALL      'XSRDA8'
057100970326     C                   PARM                    WLBDAT
057200970326     C* ERRATA
057300970326    1C     G02ERR        IFEQ      '1'
057400970326     C                   SETON                                        452890
057500970326     C                   MOVEL     MSG(3)        V1CMSG
057600970326     C                   GOTO      ENDCTR
057700970326     C                   ELSE
057800970326     C                   Z-ADD     G02INV        PARDSD
057900970326     C                   Z-ADD     G02DAT        V1CDSD
058000970326     C                   ENDIF
058100970326     C                   ENDIF
058200970326     C     V1CDSA        IFGT      0
058300970326     C                   Z-ADD     V1CDSA        G02DAT
058400970326     C                   MOVEL     *BLANK        G02ERR
058500970326     C                   CALL      'XSRDA8'
058600970326     C                   PARM                    WLBDAT
058700970326     C* ERRATA
058800970326    1C     G02ERR        IFEQ      '1'
058900970326     C                   SETON                                        462890
059000970326     C                   MOVEL     MSG(3)        V1CMSG
059100970326     C                   GOTO      ENDCTR
059200970326     C                   ELSE
059300970326     C                   Z-ADD     G02INV        PARDSA
059400970326     C                   Z-ADD     G02DAT        V1CDSA
059500970326     C*
059600970326     C     PARDSA        IFLT      PARDSD
059700970326     C                   SETON                                        462890
059800970326     C                   MOVEL     MSG(4)        V1CMSG
059900970326     C                   GOTO      ENDCTR
060000970326     C                   ENDIF
060100970326     C**
060200970326     C                   ENDIF
060300970326     C                   ENDIF
060400970627     C** NUMERO SPEDIZIONE SI PUO' IMMETTERE SOLO SE RISTAMPA
060500970627     C     V1CRIS        IFEQ      'N'
060600970630     C     WNSD          IFGT      0
060700970630     C     WNSA          ORGT      0
060800970630     C                   MOVEL     MSG(6)        V1CMSG
060900970630     C                   SETON                                        509028
061000970627     C                   GOTO      ENDCTR
061100970627     C                   ENDIF
061200970627     C                   ENDIF
061300970627     C**
061400970627     C* SE RICHIESTA RISTAMPA IMMETTERE NUMERO O DATA SPEDIZIONE
061500970630    1C     V1CRIS        IFEQ      'S'
061600970630    2C     WNSD          IFEQ      0
061700970630     C     WNSA          ANDEQ     0
061800970627     C     V1CDSD        ANDEQ     0
061900970627     C     V1CDSA        ANDEQ     0
062000970630     C                   MOVEL     MSG(7)        V1CMSG
062100970630     C                   SETON                                        509028
062200970627     C                   GOTO      ENDCTR
062300970630    2C                   ENDIF
062400970630     c* numero spedizione al non puo' essere inferiore del numero dal
062500020626     c                   if        wnsa < wnsd and wnsa > *zeros
062600970630     C                   MOVEL     MSG(8)        V1CMSG
062700970630     C                   SETON                                        509028
062800970630     C                   GOTO      ENDCTR
062900970630    2C                   ENDIF
063000970630     C* SE IMMESSO NUMERO DI SPEDIZIONE IMMETTERE ANCHE PO IN GESTIONE
063100970630    2C     WNSD          IFGT      0
063200970630     C     WNSA          ORGT      0
063300970630    3C     V1CLNP        IFEQ      0
063400970630     C                   MOVEL     MSG(9)        V1CMSG
063500970630     C                   SETON                                        559028
063600970630     C                   GOTO      ENDCTR
063700970630    3C                   ENDIF
063800970630    2C                   ENDIF
063900970630    1C                   ENDIF
064000910412     C*
064100910205     C     ENDCTR        ENDSR
064200910205     C*
064300941228     C* RICERCA LINEE CON BOLLE ANCORA DA STAMPARE -------------------*
064400941228     C     CTRLNA        BEGSR
064500910205     C                   EXSR      PULSF
064600910205     C                   Z-ADD     0             NRR               4 0
064700941228     C                   Z-ADD     1             LNA               3 0
064800970630     C     KBLP2         SETLL     FNBLP45L
064900970630     C     WFST          READE     FNBLP45L                               30
065000941228     C*
065100941228     C     *IN30         DOWEQ     *OFF
065200020719      * deve essere del p.o. in gestione se inserito
065300020719     c                   if        v1clnp <> *zeros and blplnp <> v1clnp
065400020719     c                   goto      nobolla
065500020719     c                   endif
065600020719      * deve far parte del terminal di partenza se non indicato il p.o. gestione
065700020719 b4  c                   if        v1clnp = *zeros
065800020719     c     blplnp        lookup    l1                                     31
065900020719     c                   if        not *in31
066000020719     c                   goto      nobolla
066100020719     c                   endif
066200020719 e4  c                   endif
066300941228     C*
066400941228     C     BLPLNA        CHAIN     AZORG                              31
066500941228     C  N31              MOVEL     ORGDES        DE5LNA
066600941228     C   31              MOVEL     *BLANKS       DE5LNA
066700941228     C                   MOVEL     BLPLNA        VI5LNA
066800910205     C* SCRIVO SUBFILE
066900910205     C                   ADD       1             NRR
067000941228     C                   WRITE     LS82DSF
067100941228     C*
067200910412     C* MI RIPOSIZIONO SU UN CODICE PIU' ALTO
067300941228     C                   Z-ADD     BLPLNA        LNA
067400941228     C                   ADD       1             LNA
067500970630     C     KBLP2         SETLL     FNBLP45L
067600020719     c     nobolla       tag
067700910205     C*
067800970630     C     WFST          READE     FNBLP45L                               30
067900941228     C                   ENDDO
068000910205     C*
068100910205     C                   ENDSR
068200951114     C*
068300951114     C* RICERCA CLIENTI CON BOLLE ANCORA DA STAMPARE -----------------*
068400951114     C     CTRCLI        BEGSR
068500951114     C                   CLEAR                   CLI
068600951114     C*
068700951114     C                   SETON                                        35
068800951114     C                   WRITE     LS82DC2
068900951114     C                   SETOFF                                       3507
069000951114     C*
069100951114     C                   Z-ADD     1             I                 3 0
069200951114     C                   Z-ADD     0             NRR2              4 0
069300970630     C     WFST          SETLL     FNBLP45L
069400970630     C     WFST          READE     FNBLP45L                               30
069500951114     C*
069600951114     C     *IN30         DOWEQ     *OFF
069700020719      * deve essere del p.o. in gestione se inserito
069800020719     c                   if        v1clnp <> *zeros and blplnp <> v1clnp
069900020719     c                   goto      nocli
070000020719     c                   endif
070100020719      * deve far parte del terminal di partenza se non indicato il p.o. gestione
070200020719 b4  c                   if        v1clnp = *zeros
070300020719     c     blplnp        lookup    l1                                     31
070400020719     c                   if        not *in31
070500020719     c                   goto      nocli
070600020719     c                   endif
070700020719 e4  c                   endif
070800951114     C     BLPCCM        IFGT      0
070900951114     C                   MOVEL     BLPCCM        KSC
071000951114     C                   ELSE
071100951114     C                   MOVEL     BLPKSC        KSC               7 0
071200951114     C                   ENDIF
071300951114     C     KSC           LOOKUP    CLI                                    31
071400951114     C     *IN31         IFEQ      *OFF
071500951114     C                   Z-ADD     KSC           V2CKSC
071600951114     C                   Z-ADD     KSC           CLI(I)
071700951114     C                   ADD       1             I
071800951114     C     KACO          CHAIN     CNACO000                           30
071900951114     C  N30              MOVEL     ACORAG        V2DKSC
072000951114     C   30              CLEAR                   V2DKSC
072100951114     C   30              MOVEL     BLPRSM        V2DKSC
072200951114     C                   CLEAR                   V2CSCE
072300951114     C                   ADD       1             NRR2
072400951114     C                   WRITE     LS82DS2
072500951114     C                   ENDIF
072600020719     c     nocli         tag
072700951114     C*
072800951114     C     I             IFEQ      101
072900951114     C                   SETON                                        30
073000951114     C                   ELSE
073100970630     C     WFST          READE     FNBLP45L                               30
073200951114     C                   ENDIF
073300951114     C*
073400951114     C                   ENDDO
073500951114     C     I             IFEQ      101
073600951114     C                   SETON                                        07
073700951114     C                   ENDIF
073800951114     C*
073900951114     C                   ENDSR
074000910205     C*
074100910205     C* PULIZIA SUBFILE ---------------------------------------------
074200910205     C     PULSF         BEGSR
074300910205     C                   SETON                                        35
074400941228     C                   WRITE     LS82DCT
074500910205     C                   SETOFF                                       35
074600910205     C                   ENDSR
074700961017     C*---------------------------------------------------------------*
074800970630**  MSG
074900961017Immettere linea di partenza valida                                            1
075000961017Codice Cliente inesistente                                                    2
075100970630Data Errata                                                                   3
075200970630Data AL minore della data DAL                                                 4
075300970630Linea di partenza errata                                                      5
075400970630Numero spedizione dal-al si puo' immettere soltanto per ristampa              6
075500970630In ristampa effettuare selezione per data o per numero spedizione             7
075600970630Numero spedizione al non puo' essere inferiore del numero dal
075700970630Se immesso numero di spedizione immettere anche il P.O. in gestione
075800020607Autotrasportatore in gestione ad altro p.o.                                   10
