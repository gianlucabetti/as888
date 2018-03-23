000100060207     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200941220      * TNTA14R *----------------------------------------------------*
000300060207      *         - GESTIONE TARIFFE / OFFERTE GIACENZA                *
000400900515      *--------------------------------------------------------------*
000500060207     FTNTA14D   CF   E             WORKSTN
000600090916     FTiVIS05L  IF   E           K DISK    USROPN
000700060207     FCNACO00F  IF   E           K DISK
000800910617     FCNIND00F  IF   E           K DISK
000900910617     FTFIND00F  IF   E           K DISK    USROPN
001000910617     F                                     RENAME(CNIND000:TFIND000)
001100060207     FFNCLS01L  IF   E           K DISK
001200060207     Ftfcls01L  IF   E           K DISK    usropn
001300060207     F                                     RENAME(fncls000:tfcls000)
001400990615     FTITGC01L  UF A E           K DISK
001500060207     FTIOGC01L  uf a E           K DISK    USROPN
001600990615     F                                     RENAME(TITGC000:TIOGC000)
001700900523     FTABEL00F  IF   E           K DISK
001800940316     FTFNTC01L  IF   E           K DISK
001900030716     FTITAV00F  O  A E             DISK
002000900515      *
002100910617     D TES             S             32    DIM(2) CTDATA PERRCD(1)
002200900517      *
002300900514     D KPJBA         E DS
002400900516      *
002500940901     D                 DS
002600940901     D  VIDFL1                 1      1
002700940901     D  VIDFL0                10     10
002800940901     D  FLG                    1     10
002900940901     D                                     DIM(10)
003000960910      *
003100960910     D                 DS
003200960910     D  WGG                    1      2
003300960910     D  WMM                    3      4
003400960910     D  WAA                    5      8
003500960910     D  WDATA                  1      8
003600990615     D WLBDAT          DS
003700990615     D  G02DAT                 1      8  0
003800990615     D  G02INV                 9     16  0
003900990615     D  G02ERR                17     17
004000990615     D  G02TGI                18     22  0
004100940901      *
004200990615     D TGCDS         E DS                  EXTNAME(TITGC00F)
004300961205     D DS2G          E DS
004400940315     D PARAM           DS
004500091210      * richiamato da tariffa/offerta
004600091210     d  parast                 9      9
004700091210      * codice cliente/visita/trattativa
004800091210     d  parcli                10     16  0
004900941221     D* DECODIFICA CODICE TARIFFA PASSATO DALLA GESTIONE TARIFFE
005000941221     D  PARDCT                17     26
005100941221     D* DESCRIZIONE TARIFFA       PASSATO DALLA GESTIONE TARIFFE
005200941221     D  PARDCV                27     41
005300961206     D* FLAG ITALIA/ESTERO        PASSATO DALLA GESTIONE TARIFFE
005400961206     D  PARFIE                42     42
005500990824     D* CODICE DIVISA
005600990824     D  VICDIV                43     45
005700990622     D* FLAG GESTIONE DEI DECIMALI
005800990622     D  PARFDC                73     73
005900020204      * NETWORK DPD/FEDEX
006000020204     D  PARNET                74     74
006100090916      * flag per richiamo da trattativa = 'T'
006200090916     d  partrat               75     75
006300091210      * codice tariffa e progressivo
006400091210     d  parctr               125    127  0
006500091210     d  parprg               128    130  0
006600030714     D* PARMAN --> SE RESTITUITO PIENO OCCORRE AGGIORNARE LO STORICO
006700030714     D*            VARIAZIONE
006800030714     D  PARMAN               256    256
006900960202RE   D* Ds esterna x richiamo pgm controllo fax
007000960202RE   D TRUL42        E DS                  EXTNAME(TRUL42DS)
007100960315RE   D* Ds esterna x reperimento luogo invio comunicaz. giac.
007200960315RE   D TRUL43        E DS                  EXTNAME(TRUL43DS)
007300960202RE   D* Ds esterna x scomposizione TBLUNI tabella 2H
007400960202RE   D DS2H          E DS
007500960202RE   D* Ds esterna x scomposizione TBLUNI tabella 2F
007600960202RE   D DS2F          E DS
007700020520     D*
007800020520     D tibs34ds      E DS                  inz
007900020520     D ddatiute      e ds
008000020520     D azuteds       e ds                  extname(AZUTE00F)
008100020520     D codut           S                   LIKE(TBLkut) inz(1)
008200030716
008300030716     d first           s              1    inz
008400040729     d woknota88       s              1    inz('0')
008500040729
008600040729     d kcapl           s                   like(NtcApl)
008700040729     d kcnk1           s                   like(NtcNk1)
008800040729     d kcnk2           s                   like(NtcNk2) inz
008900040729     d kctnt           s                   like(NtcTnt) inz('88')
009000030716
009100030716     D* Reperimento dati del PGM
009200030716     D PGMSTATUS      SDS
009300030716     D JOB_NAME              244    253
009400030716     D USER                  254    263
009500090916
009600090916     itivis000
009700090916     i              visesi                      wisesi
009800030716
009900900515      *---------------------------------------------------------------*
010000000000     C     *ENTRY        PLIST
010100000000     C                   PARM                    KPJBA
010200900522     C                   MOVEL     KPJBU         PARAM
010300020520     c*
010400020520     c     *dtaara       define    §azute        azuteds
010500020520     c     *dtaara       define    §datiute      ddatiute
010600020520     C                   in(E)     *dtaara
010700020520     C                   IF        %error  or RSUT = *blanks
010800020520     C                   CLEAR                   tibs34ds
010900020520     C                   CALL      'TIBS34R'
011000020520     C                   PARM                    tibs34ds
011100020520     C                   in        *dtaara
011200020520     c                   endif
011300020520     c* N.B.:Dal momento che la call al tbs34r mi serve solo per reperire il
011400020520     c* capoconto clienti italia e la ragione sociale utente non testo la
011500020520     c* presenza di eventuali errori
011600020520     C                   MOVEL(P)  RSUT          PAXDUT           30
011700020520     C                   MOVEL(P)  RSUT          RSUTP            39
011800991104      *
011900020919     C                   MOVEL     SIMFEL        simfelA           3
012000101220      * SEMPRE       richiamato da trattativa
012100101220     c**                 if        partrat = 'T'
012200090916     c                   eval      *in38 = *on
012300101220     c**                 else
012400101220     c**                 eval      *in38 = *off
012500101220     c**                 endif
012600101220     c
012700030702      * se programma richiamato dall'interrogazione il campo PARFDC
012800030702      * moneta con decimali non è valorizzato lo valorizzo adesso
012900030702      * controllando la moneta
013000030702     c                   IF        (VICDIV <> *blanks or VICDIV <> 'ITL')
013100030702     c                             and PARFDC = *blank
013200030702     c                   eval      parfdc ='S'
013300030702     c                   endif
013400900515      *---------------------------------------------------------------*
013500960321      * INDICATORI                                                    *
013600900515      *---------------------------------------------------------------*
013700960321      * 13    - CMD DI RIATTIVAZIONE
013800960321      * 16    - CMD DI ANNULLAMENTO
013900960509      * 18    - SI TRATTA DI CLIENTE ESTERO
014000990622      * 20/27 - ERRORE VIDEO
014100960321      * 31/33 - DI COMODO
014200960321      * 37    - ON - OFFERTA     OFF - TARIFFA
014300090916      * 38    - Trattativa
014400960321      * 40/49 - ERRORI
014500960321      * 50    - TARIFFA ANNULLATA
014600960321      * 51/59 - ERRORI
014700960321      * 60/61 - USATI PER LA STAMPA DELLA LETTERA
014800010806      * 62/63 - ERRORE
014900960321      * 72/73 - ERRORI
015000960321      * 80    - ON - IMMISSIONE  OFF - VARIAZIONE
015100960321      * 90    - INDICATORE DI ERRORE GENERICO
015200991018      * 95    - CIFRE CON DECIMALI
015300960321      * 98    - SONO IN INTERROGAZIONE
015400960321      * 99    - PROTEGGE TUTTI I CAMPI PER INTERROGAZIONE O ANNULLAM
015500960321      *---------------------------------------------------------------*
015600960321      * CHIAVI                                                        *
015700960321      *---------------------------------------------------------------*
015800900515     C     KACO          KLIST
015900900515     C                   KFLD                    CODUT
016000020520     C                   KFLD                    dutkci
016100900517     C                   KFLD                    CODCLI            7 0
016200900523      *
016300900523     C     KTAB          KLIST
016400900523     C                   KFLD                    CODUT
016500900523     C                   KFLD                    CODTAB            2
016600900523     C                   KFLD                    KEYTAB            8
016700040729      * chiave per nota 88 (così non sporco la chiave che si imposta
016800040729      *                     per il responsabile)
016900040729     c     kntc88        klist
017000040729     c                   kfld                    kcapl
017100040729     c                   kfld                    kcnk1
017200040729     c                   kfld                    kcnk2
017300040729     c                   kfld                    kctnt
017400040729
017500941122     C     KTGC          KLIST
017600941122     C                   KFLD                    CODCLI
017700091210     C                   KFLD                    parCTR
017800091210     C                   KFLD                    parPRG
017900900515      *---------------------------------------------------------------*
018000900515      * FLUSSO PRINCIPALE                                             *
018100900515      *---------------------------------------------------------------*
018200940316     C                   EXSR      CTRIN
018300900515      *          =============
018400900509     C     INIZIO        TAG
018500900515      *          =============
018600900515     C                   EXSR      PULIZ
018700981211     C* CLIENTE
018800091210     C     parAST        IFEQ      'C'
018900091210     C                   MOVE      parcli        VIDCLI
019000091210     C                   MOVEL     parcli        CODCLI
019100981211     C     KACO          CHAIN     CNIND00F                           36
019200981211     C   36              CLEAR                   INDTLF
019300981211     C                   MOVEL     INDTLF        WTLF
019400981211     C     *LIKE         DEFINE    INDTLF        WTLF
019500060207      * ragione sociale cliente
019600060207     C     KACO          CHAIN     CNaco00F
019700060207     c                   If        %Found(cnaco00f)
019800060207     c                   eval      descli = acorag
019900060207     c                   endif
020000981211     C                   ELSE
020100101220     C* trattativa
020200091210     C                   MOVE      parcli        VIDNVST
020300091210     C                   MOVE      parcli        VIDCLI
020400101220     C     parcli        CHAIN     TiVIS05L                           31
020500060207     c  n31              eval      descli = visrag
020600981211     C* VEDO SE ESISTE TFIND O CNIND
020700981211     C                   MOVEL     VISNRV        CODCLI
020800060207     C     KACO          CHAIN     TFIND00F                           36
020900060207     c                   If        *In36 and visksc > 0
021000060207     C                   MOVEL     VISKSC        CODCLI
021100981211     C     KACO          CHAIN     CNIND00F                           36
021200060207     c                   endif
021300981211     C   36              CLEAR                   INDTLF
021400981211     C                   MOVEL     INDTLF        WTLF
021500981211     C                   ENDIF
021600060207      * ragione sociale cliente
021700060207     C                   MOVEL     VISNRV        CODCLI
021800940315      *
021900091210     C                   MOVE      parCTR        VIDCTR
022000091210     C                   MOVE      parPRG        VIDPRG
022100981113     C                   MOVE      PARDCT        DESCTR
022200990621     C                   MOVE      PARDCV        VIDDCV
022300940315     C* AGGANCIO IL FILE GIACENZE
022400940826     C                   EXSR      AGGANC
022500900517      *
022600900515      *          =============
022700060207     C     FMT01         TAG
022800900515      *          =============
022900060207     C                   EXFMT     TA14D01
023000940315     C* CMD12 - RITORNO
023100060207     C   kl              GOTO      FINE
023200910617      *
023300940315     C* CONTROLLO
023400060207     C                   EXSR      CTR1
023500060207     C   90              GOTO      FMT01
023600900516      *
023700960321      * F13 - RIATTIVA
023800900517      *
023900900517     C   13              DO
024000900517     C                   MOVE      ' '           TGCATB
024100990615     C* VALORIZZO ULTIMA DATA VARIAZIONE
024200990615     C                   Z-ADD     DATEU8        TGCDUV
024300040916     C                   Z-ADD     DATEU8        TGCDTR
024400060207     C  n37              UPDATE    TITGC000
024500060207     C   37              UPDATE    TIoGC000
024600030717      * scrivo lo storico variazioni se non si tratta di offerte
024700030717     c                   if        not *in37
024800030716     c                   eval      TAVcta = 'I'
024900030716     c                   exsr      SUB_storico
025000030717     c                   endif
025100030716      *
025200900516      *
025300900516      * RIFACCIO LA CHAIN PER L'EVENTUALE AGGIORNAMENTO SEGUENTE
025400900516      *
025500060207     C  n37KTGC          CHAIN     TITGC01L                           80
025600060207     C   37KTGC          CHAIN     TIoGC01L                           80
025700910617     C                   SETOFF                                       5099
025800060207     C                   GOTO      FMT01
025900900516     C                   END
026000900516      *
026100940315      * CMD16 - ANNULLA
026200900517      *
026300900516     C   16              DO
026400950116     C*
026500910617     C* REGISTRO TARIFFA SE NON C'E
026600910617     C                   EXSR      REGIS
026700910617     C* RIFACCIO CHAIN PER ANNULLARE
026800060207     C  n37KTGC          CHAIN     TITGC01L                           80
026900060207     C   37KTGC          CHAIN     TIoGC01L                           80
027000910624     C*
027100910624     C* DA OFFERTA
027200091210     C     parAST        IFEQ      'V'
027300060207     C   37              DELETE    TIoGC000
027400950117     C                   GOTO      FINE
027500910617     C                   ELSE
027600910624     C* DA TARIFFA
027700990615     C* VALORIZZO ULTIMA DATA VARIAZIONE
027800990615     C                   Z-ADD     DATEU8        TGCDUV
027900950117     C                   MOVE      'A'           TGCATB
028000950117     C                   CLEAR                   TGCFTR
028100040916     C                   Z-ADD     DATEU8        TGCDTR
028200060207     C  n37              UPDATE    TITGC000
028300030717      * scrivo lo storico variazioni se non sono in gestione offerta
028400030717     c                   if        not *in37
028500030716     c                   eval      TAVcta = 'A'
028600030716     c                   exsr      SUB_storico
028700030717     c                   endif
028800030716      *
028900950117     C                   GOTO      FINE
029000910617     C                   END
029100900516     C                   END
029200900515      *
029300060207     C* CMD6 - REGISTRA
029400960321     C     *INKF         IFEQ      *ON
029500940315     C                   EXSR      REGIS
029600910617     C                   GOTO      FINE
029700940315     C                   END
029800900517      *
029900060207     C                   GOTO      FMT01
030000900517      *
030100000000     C     FINE          TAG
030200950116     C                   MOVEL     PARAM         KPJBU
030300950116     C*
030400000000     C                   SETON                                        LR
030500900515      *-----------------------------------------------------***********
030600900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
030700900515      *-----------------------------------------------------***********
030800900515     C     PULIZ         BEGSR
030900900515      *
031000900517     C                   MOVE      *BLANKS       VIDCLI
031100900517     C                   MOVEL     *BLANKS       DESCLI
031200940831     C                   Z-ADD     0             VIDCTR
031300940831     C                   MOVEL     *BLANKS       DESCTR
031400940831     C                   MOVEL     *BLANKS       VIDDCV
031500990908     C                   Z-ADD     0             VIDDUV
031600940831     C                   Z-ADD     0             VIDPRG
031700900517     C                   Z-ADD     0             VIDSGV
031800940826     C                   Z-ADD     0             VIDSGR
031900900517     C                   Z-ADD     0             VIDSGP
032000900517     C                   Z-ADD     0             VIDSGD
032100900517     C                   Z-ADD     0             VIDSG1
032200900517     C                   Z-ADD     0             VIDSG2
032300900517     C                   Z-ADD     0             VIDSG3
032400900517     C                   Z-ADD     0             VIDST1
032500900517     C                   Z-ADD     0             VIDST2
032600900517     C                   Z-ADD     0             VIDST3
032700900517     C                   Z-ADD     0             VIDSTM
032800900517     C                   Z-ADD     0             VIDRPV
032900900517     C                   Z-ADD     0             VIDSGF
033000911120     C                   Z-ADD     0             VIDGGR
033100940831     C                   MOVEL     ' '           VIDFAF
033200950121     C                   MOVEL     ' '           VIDTCM
033300950121     C                   MOVEL     ' '           VIDTFG
033400940901     C                   CLEAR                   DESTCM
033500940901     C                   CLEAR                   DESTFG
033600950116     C*
033700950116     C                   MOVEL     *BLANKS       COMTGC           62
033800940316     C                   ENDSR
033900940316      *-----------------------------------------------------***********
034000940316      * OPERAZIONI INIZIALI       O                         *  CTRIN  *
034100940316      *-----------------------------------------------------***********
034200940316     C     CTRIN         BEGSR
034300941219     C* PRENDO LA DATA DEL GIORNO DA TIME
034400941219     C                   TIME                    W0140            14 0
034500941219     C                   MOVE      W0140         UDATE8            8 0
034600990615     C*
034700990615     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
034800990615     C*
034900990615     C                   Z-ADD     UDATE8        G02DAT
035000990615     C                   MOVEL     *BLANK        G02ERR
035100990615     C                   CALL      'XSRDA8'
035200990615     C                   PARM                    WLBDAT
035300990615     C* UDATE A 8 IN AAAA/MM/GG
035400990615     C                   Z-ADD     G02INV        DATEU8            8 0
035500900515      *
035600941220     C                   MOVEL     'TNTA14R'     VIDPGM
035700991018     C* CONTROLLO SE TARIFFA CON DECIMALI
035800991018     C     PARFDC        COMP      'S'                                    95
035900900524      *
036000091113     C* VEDO SE OFFERTA
036100091210     C     parAST        IFEQ      'V'
036200910617     C                   MOVE      TES(2)        VIDTES
036300910617     C                   SETON                                        37
036400910617     C                   OPEN      TFIND00F
036500101220     C                   OPEN      TiVIS05L
036600990615     C                   OPEN      TIOGC01L
036700060206     C                   OPEN      tfcls01L
036800091113     C* O TARIFFA
036900910617     C                   ELSE
037000910617     C                   MOVE      TES(1)        VIDTES
037100910617     C                   END
037200940315     C*
037300961205     C* AGGANCIO LA TABELLA "2G" PER REPERIRE IL NUMERO MASSIMO DEI
037400961205     C*   GIRONI DI RIENTRO PER LE TARIFFE ESTERE
037500961205     C                   MOVE      '2G'          CODTAB
037600961205     C                   MOVEL     '1       '    KEYTAB
037700961205     C     KTAB          CHAIN     TABEL                              33
037800961205     C     *IN33         IFEQ      *OFF
037900961205     C                   MOVEL     TBLUNI        DS2G
038000961205     C                   ELSE
038100961205     C                   CLEAR                   DS2G
038200961205     C                   Z-ADD     99            §2GGRE
038300010806     C                   Z-ADD     99            §2GGDP
038400020212     C                   Z-ADD     99            §2GGFE
038500961205     C                   ENDIF
038600961205     C*
038700900514     C                   ENDSR
038800900517      *-----------------------------------------------------***********
038900060207      * CONTROLLI FORMATO 1                                 *  CTR1   *
039000900517      *-----------------------------------------------------***********
039100060207     C     CTR1          BEGSR
039200900517      *
039300900517     C                   SETOFF                                       90
039400990622      * CONTROLLO I DECIMALI DI TUTTI GLI IMPORTI IN BASE ALLA DIVISA
039500990622     C     VIDSGV        IFGT      0
039600990622     C                   MOVE      VIDSGV        W0030             3 0
039700990622     C     W0030         IFGT      0
039800991022     C     *IN95         ANDNE     *ON
039900990622     C                   SETON                                        2090
040000060207     C                   GOTO      FICTR1
040100990622     C                   END
040200990622     C                   END
040300900523      *--------------------
040400990622     C     VIDSGR        IFGT      0
040500990622     C                   MOVE      VIDSGR        W0030             3 0
040600990622     C     W0030         IFGT      0
040700991022     C     *IN95         ANDNE     *ON
040800990622     C                   SETON                                        2190
040900060207     C                   GOTO      FICTR1
041000990622     C                   END
041100990622     C                   END
041200990622      *--------------------
041300990622     C     VIDSGD        IFGT      0
041400990622     C                   MOVE      VIDSGD        W0030             3 0
041500990622     C     W0030         IFGT      0
041600991022     C     *IN95         ANDNE     *ON
041700990622     C                   SETON                                        2290
041800060207     C                   GOTO      FICTR1
041900990622     C                   END
042000990622     C                   END
042100990622      *--------------------
042200990622     C     VIDSGP        IFGT      0
042300990622     C                   MOVE      VIDSGP        W0030             3 0
042400990622     C     W0030         IFGT      0
042500991022     C     *IN95         ANDNE     *ON
042600990622     C                   SETON                                        2390
042700060207     C                   GOTO      FICTR1
042800990622     C                   END
042900990622     C                   END
043000990622      *--------------------
043100990622     C     VIDST1        IFGT      0
043200990622     C                   MOVE      VIDST1        W0030             3 0
043300990622     C     W0030         IFGT      0
043400991022     C     *IN95         ANDNE     *ON
043500990622     C                   SETON                                        2490
043600060207     C                   GOTO      FICTR1
043700990622     C                   END
043800990622     C                   END
043900990622      *--------------------
044000900523     C     VIDSG2        IFNE      0
044100900523     C     VIDSG1        ANDEQ     0
044200900523     C                   SETON                                        72  90
044300900523     C                   END
044400060207     C   90              GOTO      FICTR1
044500900523      *--------------------
044600900523     C     VIDSG3        IFNE      0
044700900523     C     VIDSG2        ANDEQ     0
044800900523     C                   SETON                                        73  90
044900900523     C                   END
045000060207     C   90              GOTO      FICTR1
045100900523      *--------------------
045200900517     C     VIDSG1        IFNE      999
045300900517     C     VIDSG2        ANDNE     999
045400900517     C     VIDSG3        ANDNE     999
045500960321     C                   SETON                                        40  90
045600900517     C                   END
045700060207     C   90              GOTO      FICTR1
045800900517      *--------------------
045900900517     C     VIDSG1        IFEQ      0
046000900517     C     VIDST1        ANDNE     0
046100960321     C                   SETON                                        57  90
046200900517     C                   END
046300060207     C   90              GOTO      FICTR1
046400900517      *
046500990622      *--------------------
046600990622     C     VIDST2        IFGT      0
046700990622     C                   MOVE      VIDST2        W0030             3 0
046800990622     C     W0030         IFGT      0
046900991022     C     *IN95         ANDNE     *ON
047000990622     C                   SETON                                        2590
047100060207     C                   GOTO      FICTR1
047200990622     C                   END
047300990622     C                   END
047400900517      *--------------------
047500900517     C     VIDSG2        IFEQ      0
047600900517     C     VIDST2        ANDNE     0
047700960321     C                   SETON                                        59  90
047800900517     C                   END
047900060207     C   90              GOTO      FICTR1
048000900517      *
048100900517     C     VIDST2        IFEQ      0
048200900517     C     VIDSG2        ANDNE     0
048300900517     C                   SETON                                        42  90
048400900517     C                   END
048500060207     C   90              GOTO      FICTR1
048600990622      *--------------------
048700990622     C     VIDST3        IFGT      0
048800990622     C                   MOVE      VIDST3        W0030             3 0
048900990622     C     W0030         IFGT      0
049000991022     C     *IN95         ANDNE     *ON
049100990622     C                   SETON                                        2690
049200060207     C                   GOTO      FICTR1
049300990622     C                   END
049400990622     C                   END
049500900517      *--------------------
049600900517     C     VIDSG3        IFEQ      0
049700900517     C     VIDST3        ANDNE     0
049800960321     C                   SETON                                        58  90
049900900517     C                   END
050000060207     C   90              GOTO      FICTR1
050100900517      *
050200900517     C     VIDST3        IFEQ      0
050300900517     C     VIDSG3        ANDNE     0
050400900517     C                   SETON                                        43  90
050500900517     C                   END
050600060207     C   90              GOTO      FICTR1
050700900517      *--------------------
050800900522      * GG2 < GG1
050900900522     C     VIDSG2        IFNE      0
051000900522     C     VIDSG2        IFLT      VIDSG1
051100900522     C                   SETON                                        44  90
051200900522     C                   END
051300900522     C                   END
051400060207     C   90              GOTO      FICTR1
051500900522      *--------------------
051600900522      * GG3 < GG2
051700900522     C     VIDSG3        IFNE      0
051800900522     C     VIDSG3        IFLT      VIDSG2
051900900522     C                   SETON                                        45  90
052000900522     C                   END
052100900522     C                   END
052200060207     C   90              GOTO      FICTR1
052300900522      *--------------------
052400900522      * TR2 < TR1
052500900522     C     VIDST2        IFNE      0
052600900522     C     VIDST2        IFLT      VIDST1
052700900522     C                   SETON                                        46  90
052800900522     C                   END
052900900522     C                   END
053000060207     C   90              GOTO      FICTR1
053100900522      *--------------------
053200900522      * TR3 < TR2
053300900522     C     VIDST3        IFNE      0
053400900522     C     VIDST3        IFLT      VIDST2
053500900522     C                   SETON                                        47  90
053600900522     C                   END
053700900522     C                   END
053800060207     C   90              GOTO      FICTR1
053900990622      *--------------------
054000990622     C     VIDSTM        IFGT      0
054100990622     C                   MOVE      VIDSTM        W0030             3 0
054200990622     C     W0030         IFGT      0
054300991022     C     *IN95         ANDNE     *ON
054400990622     C                   SETON                                        2790
054500060207     C                   GOTO      FICTR1
054600990622     C                   END
054700990622     C                   END
054800900522      *--------------------
054900950220     C*
055000950220     C****  FLAG APPLICAZIONE FRANCHIGIA  ****
055100950220     C     VIDFAF        IFEQ      ' '
055200950220     C     VIDSGF        IFGT      0
055300950220     C                   SETON                                        5190
055400060207     C                   GOTO      FICTR1
055500950220     C                   ENDIF
055600950220     C*
055700950220     C                   ELSE
055800950220     C     VIDSGF        IFEQ      0
055900950220     C                   SETON                                        5290
056000060207     C                   GOTO      FICTR1
056100950220     C                   ENDIF
056200950220     C                   ENDIF
056300950220     C*
056400961205     C****  GIRONI DI RIENTRO  ****
056500961205     C* SE I GIORNI DI RIENTRO SUPERANO IL LIMITE INDICATO IN TABELLA
056600020212     C*   E SI TRATTA DI TARIFFA ESTERA
056700020212     C*   E NON E' NETWORK FEDEX        --> ERRORE
056800961205     C     VIDGGR        IFGT      §2GGRE
056900961206     C     PARFIE        ANDEQ     'E'
057000020212     C     PARNET        ANDNE     'F'
057100961205     C                   SETON                                        62  90
057200060207     C                   GOTO      FICTR1
057300961205     C                   ENDIF
057400020212     C* SE I GIORNI DI RIENTRO SUPERANO IL LIMITE INDICATO IN TABELLA
057500020212     C*   E SI TRATTA DI TARIFFA ESTERA
057600020212     C*   E NETWORK FEDEX        --> ERRORE
057700020212     C     VIDGGR        IFGT      §2GGFE
057800020212     C     PARFIE        ANDEQ     'E'
057900020212     C     PARNET        ANDEQ     'F'
058000020212     C                   SETON                                        64  90
058100060207     C                   GOTO      FICTR1
058200020212     C                   ENDIF
058300010806     C* SE I GIORNI DI RIENTRO SUPERANO IL LIMITE INDICATO IN TABELLA
058400010806     C*   E SI TRATTA DI TARIFFA DPD    --> ERRORE
058500010806     C     VIDGGR        IFGT      §2GGDP
058600020204     C     PARNET        ANDEQ     'D'
058700010806     C                   SETON                                        63  90
058800060207     C                   GOTO      FICTR1
058900010806     C                   ENDIF
059000961205     C*
059100940831     C****  TIPO COMUNICAZIONE AL MITTENTE  ****
059200940901     C     VIDTCM        IFEQ      '?'
059300940901     C                   MOVEL     '2F'          CODTAB
059400940901     C                   MOVEL     ' '           VIDTCM
059500940901     C                   MOVEL     *BLANKS       DESTAB           25
059600940831     C                   CALL      'X§TABER'
059700940831     C                   PARM                    CODUT
059800940901     C                   PARM                    CODTAB
059900940901     C                   PARM                    KEYTAB
060000940901     C                   PARM                    DESTAB
060100940901     C                   MOVEL     KEYTAB        VIDTCM
060200940901     C                   MOVEL     DESTAB        DESTCM
060300940831     C                   SETON                                        90
060400060207     C                   GOTO      FICTR1
060500940831     C                   ENDIF
060600940831     C*
060700940901     C* SE IMMESSO CONTROLLO VALIDITA' TIPO COMUNICAZIONE AL MITTENTE
060800940901     C     VIDTCM        IFNE      ' '
060900960202RE   C                   MOVEL     *BLANKS       WCTFAX            1
061000940901     C                   MOVEL     '2F'          CODTAB
061100940901     C                   MOVEL     *BLANKS       KEYTAB
061200940901     C                   MOVEL     VIDTCM        KEYTAB
061300940901     C     KTAB          CHAIN     TABEL                              35
061400940901     C  N35TBLFLG        IFNE      ' '
061500940901     C                   SETON                                        35
061600940831     C                   ENDIF
061700940901     C* TIPO COMUNICAZIONE ERRATO
061800940901     C     *IN35         IFEQ      *ON
061900940901     C                   SETON                                        4890
062000060207     C                   GOTO      FICTR1
062100940831     C                   ENDIF
062200940831     C*
062300940901     C* DECODIFICA TIPO COMUNICAZIONE AL MITTENTE
062400960202RE   C*------------------------------------------------------*
062500960202RE   C*  Se tipo comunicazione apertura giacenza è presente  *
062600960202RE   C*  in tabella e tipo invio è tramite FAX ne verifico   *
062700960202RE   C*  l'attendibilità                                     *
062800960202RE   C                   MOVEL     TBLUNI        DS2F
062900960202RE   C                   MOVEL     §2FDES        DESTCM
063000960202RE   C     §2FFTP        IFEQ      'F'
063100960202RE   C                   MOVEL     'S'           WCTFAX            1
063200960202RE   C                   MOVE      VIDCLI        §QUATR            4
063300960202RE   C     §QUATR        IFEQ      *ALL'8'
063400960202RE   C     §QUATR        OREQ      *ALL'9'
063500960202RE   C                   SETON                                        5390
063600060207RE   C                   GOTO      FICTR1
063700960202RE   C                   ELSE
063800960202RE   C                   CLEAR                   TRUL42
063900960315RE   C                   CLEAR                   TRUL43
064000981211RE   C                   MOVEL     WTLF          D42FAX
064100960315RE   C                   MOVE      VIDCLI        D43KSC
064200960315RE   C                   CALL      'TRUL43R'
064300960315RE   C                   PARM                    TRUL43
064400960315RE   C     D43TRV        IFEQ      'S'
064500960315RE   C                   MOVEL     D43FAX        D42FAX
064600960315RE   C                   END
064700960202RE   C                   CALL      'TRUL42R'
064800960202RE   C                   PARM                    TRUL42
064900960202RE   C     D42ERR        IFEQ      '1'
065000960202RE   C                   SETON                                        5490
065100060207RE   C                   GOTO      FICTR1
065200960202RE   C                   END
065300960202RE   C                   END
065400960202RE   C                   END
065500960202RE   C*------------------------------------------------------*
065600040729      * se il tipo comunicazione è tramite MAIL
065700040729if  2c                   If        §2fFtp = 'M'
065800040729     c                   Move      VidCli        §QUATR
065900040729if  3c                   If        §quatr = *ALL'8' or §quatr = *ALL'9'
066000040729     c                   Eval      *In53 = *On
066100040729     c                   Eval      *In90 = *On
066200060207     c                   Goto      fictr1
066300040729   x3c                   else
066400040729     c                   Clear                   Trul43
066500091113      * tariffa
066600040729     c  n37              Move      VidCli        D43Ksc
066700091113      * offerta
066800040729     c                   If        *In37 and VisKsc > *Zeros
066900040729     c                   Move      VisKsc        D43Ksc
067000040729     c                   EndIf
067100040729     c                   If        *In37 and VisKsc = *Zeros
067200040729     c                   Move      VidCli        D43Ksc
067300040729     c                   EndIf
067400091113
067500040729     c                   Call      'TRUL43R'
067600040729     c                   Parm                    Trul43
067700040729if  4c                   If        D43Trv = 'S'
067800040729if  5c                   If        D43Trvm = 'N'
067900040729     c                   Eval      *In65 = *On
068000040729     c                   Eval      *In90 = *On
068100060207     c                   Goto      fictr1
068200040729e   5c                   EndIf
068300040729   x4c                   Else
068400040729      * cerco la nota 88 se manca il luogo di invio MAIL
068500091113     c                   select
068600091113      * tariffa
068700091113     c                   when      not *in37
068800091113     c                   Movel     'C'           kcApl
068900091113      * offerta da trattativa
069000101220     c                   when      *in37
069100091113     c                   Movel     'T'           kcApl
069200091113     c                   endsl
069300091113
069400040729     c                   Movel     dutkci        kcNk1
069500040729     c                   Move      VidCli        kcNk1
069600040729     c                   Eval      woknota88 = *Off
069700091113     c     kNtc88        chain     Tfntc01l
069800091113     c                   If        %Found(tfntc01l)
069900040729     c                   Eval      woknota88 = *On
070000091113     c                   EndIf
070100091113      * non ho trovato la nota e sono da offerta da trattativa su cliente
070200091113      * cerco con il cliente
070300091113     c                   if        woknota88 = *Off and *in38
070400091113     c                             and visksc > 0
070500091113     c                   eval      kcapl = 'C'
070600091113     c                   move      visksc        kcnk1
070700091113     c     kNtc88        chain     Tfntc01l
070800091113     c                   If        %Found(tfntc01l)
070900091113     c                   Eval      woknota88 = *On
071000091113     c                   EndIf
071100091113     c                   endif
071200040729     c                   If        woknota88 = *Off
071300040729     c                   Eval      *In65 = *On
071400040729     c                   Eval      *In90 = *On
071500060207     c                   Goto      fictr1
071600040729     c                   EndIf
071700040729e   4c                   EndIf
071800040729e   3c                   EndIf
071900040729e   2c                   EndIf
072000040729
072100940901     C                   ELSE
072200940901     C                   CLEAR                   DESTCM
072300940901     C                   ENDIF
072400940831     C*
072500940831     C****  TIPO COMUNICAZIONE FINE GIACENZA  ****
072600940901     C     VIDTFG        IFEQ      '?'
072700940901     C                   MOVEL     '2H'          CODTAB
072800940901     C                   MOVEL     ' '           VIDTFG
072900940901     C                   MOVEL     *BLANKS       DESTAB           25
073000940901     C                   CALL      'X§TABER'
073100940901     C                   PARM                    CODUT
073200940901     C                   PARM                    CODTAB
073300940901     C                   PARM                    KEYTAB
073400940901     C                   PARM                    DESTAB
073500940901     C                   MOVEL     KEYTAB        VIDTFG
073600940901     C                   MOVEL     DESTAB        DESTFG
073700940901     C                   SETON                                        90
073800060207     C                   GOTO      FICTR1
073900940901     C                   ENDIF
074000940901     C*
074100940901     C* SE IMMESSO CONTROLLO VALIDITA' TIPO COMUNICAZIONE FINE GIACENZA
074200940901     C     VIDTFG        IFNE      ' '
074300940901     C                   MOVEL     '2H'          CODTAB
074400940901     C                   MOVEL     *BLANKS       KEYTAB
074500940901     C                   MOVEL     VIDTFG        KEYTAB
074600940901     C     KTAB          CHAIN     TABEL                              35
074700940901     C  N35TBLFLG        IFNE      ' '
074800940901     C                   SETON                                        35
074900940901     C                   ENDIF
075000940901     C* TIPO COMUNICAZIONE ERRATO
075100940901     C     *IN35         IFEQ      *ON
075200940901     C                   SETON                                        4990
075300060207     C                   GOTO      FICTR1
075400940901     C                   ENDIF
075500940901     C*
075600940901     C* DECODIFICA TIPO COMUNICAZIONE FINE GIACENZA
075700960202RE   C*------------------------------------------------------*
075800960202RE   C*  Se tipo comunicazione chiusura giacenza è presente  *
075900960202RE   C*  in tabella e tipo invio è tramite FAX ne verifico   *
076000960202RE   C*  l'attendibilità                                     *
076100960202RE   C                   MOVEL     TBLUNI        DS2H
076200960202RE   C                   MOVEL     §2HDES        DESTFG
076300960202RE   C     §2HFAX        IFEQ      'F'
076400960202RE   C     WCTFAX        ANDNE     'S'
076500960202RE   C                   MOVE      VIDCLI        §QUATR            4
076600960202RE   C     §QUATR        IFEQ      *ALL'8'
076700960202RE   C     §QUATR        OREQ      *ALL'9'
076800960202RE   C                   SETON                                        5590
076900060207RE   C                   GOTO      FICTR1
077000960202RE   C                   ELSE
077100960202RE   C                   CLEAR                   TRUL42
077200960315RE   C                   CLEAR                   TRUL43
077300981211RE   C                   MOVEL     WTLF          D42FAX
077400960315RE   C                   MOVE      VIDCLI        D43KSC
077500960315RE   C                   CALL      'TRUL43R'
077600960315RE   C                   PARM                    TRUL43
077700960315RE   C     D43TRV        IFEQ      'S'
077800960315RE   C                   MOVEL     D43FAX        D42FAX
077900960315RE   C                   END
078000960202RE   C                   CALL      'TRUL42R'
078100960202RE   C                   PARM                    TRUL42
078200960202RE   C     D42ERR        IFEQ      '1'
078300960202RE   C                   SETON                                        5690
078400060207RE   C                   GOTO      FICTR1
078500960202RE   C                   END
078600960202RE   C                   END
078700960202RE   C                   END
078800960202RE   C*------------------------------------------------------*
078900940901     C                   ELSE
079000940901     C                   CLEAR                   DESTFG
079100940901     C                   ENDIF
079200940901     C*
079300060207     C     FICTR1        ENDSR
079400940901     C*
079500940315      *-----------------------------------------------------***********
079600940315     C* AGGANCIO IL FILE GIACENZE                              AGGANC
079700940315      *-----------------------------------------------------***********
079800940315     C     AGGANC        BEGSR
079900940315     C                   MOVEL     VIDCLI        CODCLI
080000060207     C  n37KTGC          CHAIN     TITGC000                           80
080100060207     C   37KTGC          CHAIN     TIoGC000                           80
080200940315     C*
080300940315     C* RIEMPO I CAMPI
080400940315     C*
080500980623     C     *IN80         IFEQ      *OFF
080600940315     C                   EXSR      RIEMPI
080700940315     C                   ENDIF
080800940315     C*
080900940901     C*
081000940831     C* SE IL TIPO COMUNICAZIONE AL MITTENTE E IL TIPO COMUNICAZIONE
081100940831     C*   FINE GICENZA SONO VUOTI, LI PROPONGO PRENDENDOLI DAL P.D.C.
081200060206      * se da visita prima controllo se c'è l'anagrafica provvisoria
081300060206     c                   If        *In37
081400060206     c     codcli        Chain     Tfcls01l                           35
081500060206      * Non trovo anagrafica provvisoria controllo con il cliente su
081600060206      * anagrafico P.d.c.
081700060206     c                   If        *In35 and visksc > 0
081800060206     c     visksc        Chain     Fncls01l                           35
081900060207     c                   endif
082000060206      * se è da tariffa diretta sul p.d.c.
082100060206     c                   Else
082200060206     c     codcli        Chain     fncls01l                           35
082300060206     c                   endif
082400060206
082500060206     c                   if        not *in35
082600940901     C                   MOVEA     CLSFLO        FLG
082700041029      * valorizzo  il campo di comodo
082800041029     c                   eval      anatcm = '(in anagrafica = ' + vidfl0 +
082900041029     c                             ' )'
083000041029      * valorizzo  il campo di comodo
083100041029     c                   eval      anatfg = '(in anagrafica = ' + vidfl1 +
083200041029     c                             ' )'
083300940901     C                   ENDIF
083400950121     C*
083500940901     C* DECODIFICA TIPO COMUNICAZIONE AL MITTENTE
083600940901     C                   MOVEL     '2F'          CODTAB
083700940901     C                   MOVEL     *BLANKS       KEYTAB
083800940901     C                   MOVEL     VIDTCM        KEYTAB
083900940901     C     KTAB          CHAIN     TABEL                              35
084000940901     C  N35              MOVEL     TBLUNI        DESTCM
084100940901     C*
084200940901     C* DECODIFICA TIPO COMUNICAZIONE FINE GIACENZA
084300940901     C                   MOVEL     '2H'          CODTAB
084400940901     C                   MOVEL     *BLANKS       KEYTAB
084500940901     C                   MOVEL     VIDTFG        KEYTAB
084600940901     C     KTAB          CHAIN     TABEL                              35
084700940901     C  N35              MOVEL     TBLUNI        DESTFG
084800940901     C*
084900940315     C                   ENDSR
085000900515      *-----------------------------------------------------***********
085100900515      * REGISTRAZIONE RECORD                                *  REGIS  *
085200900515      *-----------------------------------------------------***********
085300900515     C     REGIS         BEGSR
085400900515      *
085500900517     C                   MOVEL     *BLANK        TGCATB
085600900517     C                   MOVE      VIDCLI        TGCKSC
085700940831     C                   MOVE      VIDCTR        TGCCTR
085800940831     C                   MOVE      VIDPRG        TGCPRG
085900900517     C                   Z-ADD     VIDSGV        TGCSGV
086000940826     C                   Z-ADD     VIDSGR        TGCSGR
086100900517     C                   Z-ADD     VIDSGP        TGCSGP
086200900517     C                   Z-ADD     VIDSGD        TGCSGD
086300900517     C                   Z-ADD     VIDSG1        TGCSG1
086400900517     C                   Z-ADD     VIDSG2        TGCSG2
086500900517     C                   Z-ADD     VIDSG3        TGCSG3
086600900517     C                   Z-ADD     VIDST1        TGCST1
086700900517     C                   Z-ADD     VIDST2        TGCST2
086800900517     C                   Z-ADD     VIDST3        TGCST3
086900900517     C                   Z-ADD     VIDSTM        TGCSTM
087000900517     C                   Z-ADD     VIDRPV        TGCRPV
087100900517     C                   Z-ADD     VIDSGF        TGCSGF
087200911120     C                   Z-ADD     VIDGGR        TGCGGR
087300940831     C                   MOVEL     VIDFAF        TGCFAF
087400940903     C*
087500940903     C* IL TIPO COMUNICAZIONE AL MITTENTE E IL TIPO COMUNICAZIONE
087600940914     C*   FINE GIACENZA LI MEMORIZZO SOLO SE VARIANO RISPETTO A
087700940903     C*   QUELLO CHE C'E' SUL P.D.C.
087800940903     C     VIDTCM        IFNE      VIDFL0
087900940901     C                   MOVEL     VIDTCM        TGCTCM
088000950106     C                   ELSE
088100950106     C                   CLEAR                   TGCTCM
088200940903     C                   ENDIF
088300950106     C*
088400940903     C     VIDTFG        IFNE      VIDFL1
088500940831     C                   MOVEL     VIDTFG        TGCTFG
088600950106     C                   ELSE
088700950106     C                   CLEAR                   TGCTFG
088800940903     C                   ENDIF
088900940903     C*
089000990615     C* SE VIENE VARIATO TITGC ABBLENCO FLAG E DATA TRASMISSIONE
089100950116    1C     COMTGC        IFNE      TGCDS
089200940826     C                   MOVE      ' '           TGCFTR
089300940826     C                   Z-ADD     0             TGCDTR
089400950116    1C                   ENDIF
089500900516      *
089600990615     C* VALORIZZO ULTIMA DATA VARIAZIONE
089700990615     C                   Z-ADD     DATEU8        TGCDUV
089800040916     C* VALORIZZO DATA trasmissione
089900040916     C                   Z-ADD     DATEU8        TGCDTR
090000990615      *
090100060207     c                   if        not *in80
090200060207     C  n37              UPDATE    TITGC000
090300060207     C   37              UPDATE    TIogC000
090400060207     c                   endif
090500030714      * VALORIZZO IL FLAG PER LA SCRITTURA DEL RECORD STORICO
090600030717      * se non si tratta di annullamento e non si tratta di offerta
090700030717     c                   if        not *in16 and not *in80 and not *in37
090800030716     C                   EVAL      PARMAN = 'S'
090900030716     c                   endif
091000030714      *
091100060207     c                   If        *in80
091200060207     C  n37              WRITE     TITGC000
091300060207     C   37              WRITE     TIoGC000
091400060207     c                   endif
091500950116     C*
091600900515     C                   ENDSR
091700900515      *-----------------------------------------------------***********
091800900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
091900900515      *-----------------------------------------------------***********
092000900515     C     RIEMPI        BEGSR
092100990908     C*
092200990908     C* GIRO DATA ULTIMA VARIAZIONE
092300990908     C*
092400990908     C                   Z-ADD     TGCDUV        G02INV
092500990908     C                   MOVEL     '3'           G02ERR
092600990908     C                   CALL      'XSRDA8'
092700990908     C                   PARM                    WLBDAT
092800990908     C                   Z-ADD     G02DAT        VIDDUV
092900950116     C*
093000950116     C                   MOVEL     TGCDS         COMTGC
093100950116     C*
093200900517     C                   Z-ADD     TGCSGV        VIDSGV
093300940826     C                   Z-ADD     TGCSGR        VIDSGR
093400900517     C                   Z-ADD     TGCSGP        VIDSGP
093500900517     C                   Z-ADD     TGCSGD        VIDSGD
093600900517     C                   Z-ADD     TGCSG1        VIDSG1
093700900517     C                   Z-ADD     TGCSG2        VIDSG2
093800900517     C                   Z-ADD     TGCSG3        VIDSG3
093900900517     C                   Z-ADD     TGCST1        VIDST1
094000900517     C                   Z-ADD     TGCST2        VIDST2
094100900517     C                   Z-ADD     TGCST3        VIDST3
094200900517     C                   Z-ADD     TGCSTM        VIDSTM
094300900517     C                   Z-ADD     TGCRPV        VIDRPV
094400900517     C                   Z-ADD     TGCSGF        VIDSGF
094500911120     C                   Z-ADD     TGCGGR        VIDGGR
094600940831     C                   MOVEL     TGCFAF        VIDFAF
094700940831     C                   MOVEL     TGCTCM        VIDTCM
094800940831     C                   MOVEL     TGCTFG        VIDTFG
094900900517      *
095000950105     C     TGCATB        IFNE      ' '
095100980623     C                   SETON                                        50  99
095200900517     C                   ELSE
095300900517     C                   SETOFF                                       50  99
095400900517     C                   END
095500900515      *
095600900517     C                   ENDSR
095700030716     C*---------------------------------------------------------------*
095800030716     C* ROUTINE DI SCRITTURA FILE STORICO TITAV00F
095900030716     C*---------------------------------------------------------------*
096000030716     C     SUB_STORICO   BEGSR
096100030716      *la prima volta inizializzo i campi del file che vanno sempre bene
096200030716     c                   if        first = ' '
096300030716     C                   eval      TAVksc= tgcksc
096400030716     C                   eval      TAVctr= tgcctr
096500030716     C                   eval      TAVprg= tgcprg
096600030716      * data variazione
096700030716     C                   eval      TAVdav= dateu8
096800030716     C                   eval      TAVnoj= job_name
096900030716     C                   eval      TAVpru= user
097000030716     C                   eval      first = 'N'
097100030716     c                   endif
097200030716      * recupero ora di variazione
097300030716     c                   time                    w0140
097400030716     c                   movel     w0140         TAVorv
097500030716     c                   eval      TAVtrd= 'GIA'
097600030716     c                   clear                   TAVftc
097700041005     c                   z-add     dateu8        TAVdtr
097800030716      *
097900030716     c                   write     titav000
098000030716     C*
098100030716     C                   ENDSR
098200030716     C***
098300910617**
098400910617 ** GESTIONE TARIFFE GIACENZA **
098500910617 ** GESTIONE OFFERTE GIACENZA **
