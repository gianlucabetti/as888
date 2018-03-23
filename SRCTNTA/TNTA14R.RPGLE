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
002400130125
002500130125       // -?abilitazioni profilo utente
002600130125     d dUTE01        e ds
002700900516      *
002800940901     D                 DS
002900940901     D  VIDFL1                 1      1
003000940901     D  VIDFL0                10     10
003100940901     D  FLG                    1     10
003200940901     D                                     DIM(10)
003300960910      *
003400960910     D                 DS
003500960910     D  WGG                    1      2
003600960910     D  WMM                    3      4
003700960910     D  WAA                    5      8
003800960910     D  WDATA                  1      8
003900990615     D WLBDAT          DS
004000990615     D  G02DAT                 1      8  0
004100990615     D  G02INV                 9     16  0
004200990615     D  G02ERR                17     17
004300990615     D  G02TGI                18     22  0
004400940901      *
004500990615     D TGCDS         E DS                  EXTNAME(TITGC00F)
004600961205     D DS2G          E DS
004700110916     d ds1G          e ds
004800940315     D PARAM           DS
004900091210      * richiamato da tariffa/offerta
005000091210     d  parast                 9      9
005100091210      * codice cliente/visita/trattativa
005200091210     d  parcli                10     16  0
005300941221     D* DECODIFICA CODICE TARIFFA PASSATO DALLA GESTIONE TARIFFE
005400941221     D  PARDCT                17     26
005500941221     D* DESCRIZIONE TARIFFA       PASSATO DALLA GESTIONE TARIFFE
005600941221     D  PARDCV                27     41
005700961206     D* FLAG ITALIA/ESTERO        PASSATO DALLA GESTIONE TARIFFE
005800961206     D  PARFIE                42     42
005900990824     D* CODICE DIVISA
006000990824     D  VICDIV                43     45
006100990622     D* FLAG GESTIONE DEI DECIMALI
006200990622     D  PARFDC                73     73
006300020204      * NETWORK DPD/FEDEX
006400020204     D  PARNET                74     74
006500090916      * flag per richiamo da trattativa = 'T'
006600090916     d  partrat               75     75
006700091210      * codice tariffa e progressivo
006800091210     d  parctr               125    127  0
006900091210     d  parprg               128    130  0
007000030714     D* PARMAN --> SE RESTITUITO PIENO OCCORRE AGGIORNARE LO STORICO
007100030714     D*            VARIAZIONE
007200030714     D  PARMAN               256    256
007300960202RE   D* Ds esterna x richiamo pgm controllo fax
007400960202RE   D TRUL42        E DS                  EXTNAME(TRUL42DS)
007500960315RE   D* Ds esterna x reperimento luogo invio comunicaz. giac.
007600960315RE   D TRUL43        E DS                  EXTNAME(TRUL43DS)
007700960202RE   D* Ds esterna x scomposizione TBLUNI tabella 2H
007800960202RE   D DS2H          E DS
007900960202RE   D* Ds esterna x scomposizione TBLUNI tabella 2F
008000960202RE   D DS2F          E DS
008100020520     D*
008200020520     D tibs34ds      E DS                  inz
008300020520     D ddatiute      e ds
008400020520     D azuteds       e ds                  extname(AZUTE00F)
008500020520     D codut           S                   LIKE(TBLkut) inz(1)
008600030716
008700030716     d first           s              1    inz
008800040729     d woknota88       s              1    inz('0')
008900040729
009000040729     d kcapl           s                   like(NtcApl)
009100040729     d kcnk1           s                   like(NtcNk1)
009200040729     d kcnk2           s                   like(NtcNk2) inz
009300040729     d kctnt           s                   like(NtcTnt) inz('88')
009400110916
009500110916     d savrpv          s                   like(TGCrpv) inz
009600030716
009700030716     D* Reperimento dati del PGM
009800030716     D PGMSTATUS      SDS
009900030716     D JOB_NAME              244    253
010000030716     D USER                  254    263
010100090916
010200090916     itivis000
010300090916     i              visesi                      wisesi
010400030716
010500900515      *---------------------------------------------------------------*
010600000000     C     *ENTRY        PLIST
010700000000     C                   PARM                    KPJBA
010800900522     C                   MOVEL     KPJBU         PARAM
010900020520     c*
011000020520     c     *dtaara       define    §azute        azuteds
011100020520     c     *dtaara       define    §datiute      ddatiute
011200020520     C                   in(E)     *dtaara
011300020520     C                   IF        %error  or RSUT = *blanks
011400020520     C                   CLEAR                   tibs34ds
011500020520     C                   CALL      'TIBS34R'
011600020520     C                   PARM                    tibs34ds
011700020520     C                   in        *dtaara
011800020520     c                   endif
011900020520     c* N.B.:Dal momento che la call al tbs34r mi serve solo per reperire il
012000020520     c* capoconto clienti italia e la ragione sociale utente non testo la
012100020520     c* presenza di eventuali errori
012200020520     C                   MOVEL(P)  RSUT          PAXDUT           30
012300020520     C                   MOVEL(P)  RSUT          RSUTP            39
012400991104      *
012500020919     C                   MOVEL     SIMFEL        simfelA           3
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
013900130125      * 17    - Protezione campi in modifica tariffe
014000960509      * 18    - SI TRATTA DI CLIENTE ESTERO
014100990622      * 20/27 - ERRORE VIDEO
014200960321      * 31/33 - DI COMODO
014300960321      * 37    - ON - OFFERTA     OFF - TARIFFA
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
032900110916     C                   Z-ADD     0             SAVrpv
033000900517     C                   Z-ADD     0             VIDSGF
033100911120     C                   Z-ADD     0             VIDGGR
033200940831     C                   MOVEL     ' '           VIDFAF
033300950121     C                   MOVEL     ' '           VIDTCM
033400950121     C                   MOVEL     ' '           VIDTFG
033500940901     C                   CLEAR                   DESTCM
033600940901     C                   CLEAR                   DESTFG
033700950116     C*
033800950116     C                   MOVEL     *BLANKS       COMTGC           62
033900940316     C                   ENDSR
034000940316      *-----------------------------------------------------***********
034100940316      * OPERAZIONI INIZIALI       O                         *  CTRIN  *
034200940316      *-----------------------------------------------------***********
034300940316     C     CTRIN         BEGSR
034400941219     C* PRENDO LA DATA DEL GIORNO DA TIME
034500941219     C                   TIME                    W0140            14 0
034600941219     C                   MOVE      W0140         UDATE8            8 0
034700990615     C*
034800990615     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
034900990615     C*
035000990615     C                   Z-ADD     UDATE8        G02DAT
035100990615     C                   MOVEL     *BLANK        G02ERR
035200990615     C                   CALL      'XSRDA8'
035300990615     C                   PARM                    WLBDAT
035400990615     C* UDATE A 8 IN AAAA/MM/GG
035500990615     C                   Z-ADD     G02INV        DATEU8            8 0
035600900515      *
035700941220     C                   MOVEL     'TNTA14R'     VIDPGM
035800991018     C* CONTROLLO SE TARIFFA CON DECIMALI
035900991018     C     PARFDC        COMP      'S'                                    95
036000900524      *
036100091113     C* VEDO SE OFFERTA
036200091210     C     parAST        IFEQ      'V'
036300910617     C                   MOVE      TES(2)        VIDTES
036400910617     C                   SETON                                        37
036500910617     C                   OPEN      TFIND00F
036600101220     C                   OPEN      TiVIS05L
036700990615     C                   OPEN      TIOGC01L
036800060206     C                   OPEN      tfcls01L
036900091113     C* O TARIFFA
037000910617     C                   ELSE
037100910617     C                   MOVE      TES(1)        VIDTES
037200910617     C                   END
037300940315     C*
037400961205     C* AGGANCIO LA TABELLA "2G" PER REPERIRE IL NUMERO MASSIMO DEI
037500961205     C*   GIRONI DI RIENTRO PER LE TARIFFE ESTERE
037600961205     C                   MOVE      '2G'          CODTAB
037700961205     C                   MOVEL     '1       '    KEYTAB
037800961205     C     KTAB          CHAIN     TABEL                              33
037900961205     C     *IN33         IFEQ      *OFF
038000961205     C                   MOVEL     TBLUNI        DS2G
038100961205     C                   ELSE
038200961205     C                   CLEAR                   DS2G
038300961205     C                   Z-ADD     99            §2GGRE
038400010806     C                   Z-ADD     99            §2GGDP
038500020212     C                   Z-ADD     99            §2GGFE
038600961205     C                   ENDIF
038700110916
038800110916      * Aggancio la tabella 1G per rapporto peso/volume
038900110916     c                   eval      codtab = '1G'
039000110916     c                   eval      keytab = '1       '
039100110916     c     ktab          chain     tabel00f
039200110916     c                   IF        %found(tabel00f) and TBLflg = *blanks
039300110916     c                   eval      ds1G = TBLuni
039400110916     c                   ELSE
039500110916     c                   clear                   ds1G
039600110916     c                   ENDIF
039700130125
039800130125       //?Imposto indicatore per protezione campi in modifica tariffe
039900130125       //?ma solo TARIFFE e NO CARTELLO
040000130125     c                   eval      dUTE01 = UTEfaf
040100130125     c                   IF        not *in37 and
040200130125     c                             %subst(%editc(parcli:'X'):1:4)
040300130125     c                             <> '8888'
040400130125     c                   eval      *in17 = (§UTEmodtar = *blanks)
040500130125     c                   ENDIF
040600961205     C*
040700900514     C                   ENDSR
040800900517      *-----------------------------------------------------***********
040900060207      * CONTROLLI FORMATO 1                                 *  CTR1   *
041000900517      *-----------------------------------------------------***********
041100060207     C     CTR1          BEGSR
041200900517      *
041300900517     C                   SETOFF                                       90
041400990622      * CONTROLLO I DECIMALI DI TUTTI GLI IMPORTI IN BASE ALLA DIVISA
041500990622     C     VIDSGV        IFGT      0
041600990622     C                   MOVE      VIDSGV        W0030             3 0
041700990622     C     W0030         IFGT      0
041800991022     C     *IN95         ANDNE     *ON
041900990622     C                   SETON                                        2090
042000060207     C                   GOTO      FICTR1
042100990622     C                   END
042200990622     C                   END
042300900523      *--------------------
042400990622     C     VIDSGR        IFGT      0
042500990622     C                   MOVE      VIDSGR        W0030             3 0
042600990622     C     W0030         IFGT      0
042700991022     C     *IN95         ANDNE     *ON
042800990622     C                   SETON                                        2190
042900060207     C                   GOTO      FICTR1
043000990622     C                   END
043100990622     C                   END
043200990622      *--------------------
043300990622     C     VIDSGD        IFGT      0
043400990622     C                   MOVE      VIDSGD        W0030             3 0
043500990622     C     W0030         IFGT      0
043600991022     C     *IN95         ANDNE     *ON
043700990622     C                   SETON                                        2290
043800060207     C                   GOTO      FICTR1
043900990622     C                   END
044000990622     C                   END
044100990622      *--------------------
044200990622     C     VIDSGP        IFGT      0
044300990622     C                   MOVE      VIDSGP        W0030             3 0
044400990622     C     W0030         IFGT      0
044500991022     C     *IN95         ANDNE     *ON
044600990622     C                   SETON                                        2390
044700060207     C                   GOTO      FICTR1
044800990622     C                   END
044900990622     C                   END
045000990622      *--------------------
045100990622     C     VIDST1        IFGT      0
045200990622     C                   MOVE      VIDST1        W0030             3 0
045300990622     C     W0030         IFGT      0
045400991022     C     *IN95         ANDNE     *ON
045500990622     C                   SETON                                        2490
045600060207     C                   GOTO      FICTR1
045700990622     C                   END
045800990622     C                   END
045900990622      *--------------------
046000900523     C     VIDSG2        IFNE      0
046100900523     C     VIDSG1        ANDEQ     0
046200900523     C                   SETON                                        72  90
046300900523     C                   END
046400060207     C   90              GOTO      FICTR1
046500900523      *--------------------
046600900523     C     VIDSG3        IFNE      0
046700900523     C     VIDSG2        ANDEQ     0
046800900523     C                   SETON                                        73  90
046900900523     C                   END
047000060207     C   90              GOTO      FICTR1
047100900523      *--------------------
047200900517     C     VIDSG1        IFNE      999
047300900517     C     VIDSG2        ANDNE     999
047400900517     C     VIDSG3        ANDNE     999
047500960321     C                   SETON                                        40  90
047600900517     C                   END
047700060207     C   90              GOTO      FICTR1
047800900517      *--------------------
047900900517     C     VIDSG1        IFEQ      0
048000900517     C     VIDST1        ANDNE     0
048100960321     C                   SETON                                        57  90
048200900517     C                   END
048300060207     C   90              GOTO      FICTR1
048400900517      *
048500990622      *--------------------
048600990622     C     VIDST2        IFGT      0
048700990622     C                   MOVE      VIDST2        W0030             3 0
048800990622     C     W0030         IFGT      0
048900991022     C     *IN95         ANDNE     *ON
049000990622     C                   SETON                                        2590
049100060207     C                   GOTO      FICTR1
049200990622     C                   END
049300990622     C                   END
049400900517      *--------------------
049500900517     C     VIDSG2        IFEQ      0
049600900517     C     VIDST2        ANDNE     0
049700960321     C                   SETON                                        59  90
049800900517     C                   END
049900060207     C   90              GOTO      FICTR1
050000900517      *
050100900517     C     VIDST2        IFEQ      0
050200900517     C     VIDSG2        ANDNE     0
050300900517     C                   SETON                                        42  90
050400900517     C                   END
050500060207     C   90              GOTO      FICTR1
050600990622      *--------------------
050700990622     C     VIDST3        IFGT      0
050800990622     C                   MOVE      VIDST3        W0030             3 0
050900990622     C     W0030         IFGT      0
051000991022     C     *IN95         ANDNE     *ON
051100990622     C                   SETON                                        2690
051200060207     C                   GOTO      FICTR1
051300990622     C                   END
051400990622     C                   END
051500900517      *--------------------
051600900517     C     VIDSG3        IFEQ      0
051700900517     C     VIDST3        ANDNE     0
051800960321     C                   SETON                                        58  90
051900900517     C                   END
052000060207     C   90              GOTO      FICTR1
052100900517      *
052200900517     C     VIDST3        IFEQ      0
052300900517     C     VIDSG3        ANDNE     0
052400900517     C                   SETON                                        43  90
052500900517     C                   END
052600060207     C   90              GOTO      FICTR1
052700900517      *--------------------
052800900522      * GG2 < GG1
052900900522     C     VIDSG2        IFNE      0
053000900522     C     VIDSG2        IFLT      VIDSG1
053100900522     C                   SETON                                        44  90
053200900522     C                   END
053300900522     C                   END
053400060207     C   90              GOTO      FICTR1
053500900522      *--------------------
053600900522      * GG3 < GG2
053700900522     C     VIDSG3        IFNE      0
053800900522     C     VIDSG3        IFLT      VIDSG2
053900900522     C                   SETON                                        45  90
054000900522     C                   END
054100900522     C                   END
054200060207     C   90              GOTO      FICTR1
054300900522      *--------------------
054400900522      * TR2 < TR1
054500900522     C     VIDST2        IFNE      0
054600900522     C     VIDST2        IFLT      VIDST1
054700900522     C                   SETON                                        46  90
054800900522     C                   END
054900900522     C                   END
055000060207     C   90              GOTO      FICTR1
055100900522      *--------------------
055200900522      * TR3 < TR2
055300900522     C     VIDST3        IFNE      0
055400900522     C     VIDST3        IFLT      VIDST2
055500900522     C                   SETON                                        47  90
055600900522     C                   END
055700900522     C                   END
055800060207     C   90              GOTO      FICTR1
055900990622      *--------------------
056000990622     C     VIDSTM        IFGT      0
056100990622     C                   MOVE      VIDSTM        W0030             3 0
056200990622     C     W0030         IFGT      0
056300991022     C     *IN95         ANDNE     *ON
056400990622     C                   SETON                                        2790
056500060207     C                   GOTO      FICTR1
056600990622     C                   END
056700990622     C                   END
056800900522      *--------------------
056900110916
057000110916      * Rapporto peso volume controllo con tabella 1G
057100110916     c                   IF        VIDrpv <> SAVrpv and
057200110916     c                             VIDrpv > §1Grpv
057300110916     c                   eval      *in60 = *on
057400110916     c                   eval      *in90 = *on
057500110916     c                   eval      SAVrpv = VIDrpv
057600110916     c                   leavesr
057700110916     c                   ENDIF
057800950220     C*
057900950220     C****  FLAG APPLICAZIONE FRANCHIGIA  ****
058000950220     C     VIDFAF        IFEQ      ' '
058100950220     C     VIDSGF        IFGT      0
058200950220     C                   SETON                                        5190
058300060207     C                   GOTO      FICTR1
058400950220     C                   ENDIF
058500950220     C*
058600950220     C                   ELSE
058700950220     C     VIDSGF        IFEQ      0
058800950220     C                   SETON                                        5290
058900060207     C                   GOTO      FICTR1
059000950220     C                   ENDIF
059100950220     C                   ENDIF
059200950220     C*
059300961205     C****  GIRONI DI RIENTRO  ****
059400961205     C* SE I GIORNI DI RIENTRO SUPERANO IL LIMITE INDICATO IN TABELLA
059500020212     C*   E SI TRATTA DI TARIFFA ESTERA
059600020212     C*   E NON E' NETWORK FEDEX        --> ERRORE
059700961205     C     VIDGGR        IFGT      §2GGRE
059800961206     C     PARFIE        ANDEQ     'E'
059900020212     C     PARNET        ANDNE     'F'
060000961205     C                   SETON                                        62  90
060100060207     C                   GOTO      FICTR1
060200961205     C                   ENDIF
060300020212     C* SE I GIORNI DI RIENTRO SUPERANO IL LIMITE INDICATO IN TABELLA
060400020212     C*   E SI TRATTA DI TARIFFA ESTERA
060500020212     C*   E NETWORK FEDEX        --> ERRORE
060600020212     C     VIDGGR        IFGT      §2GGFE
060700020212     C     PARFIE        ANDEQ     'E'
060800020212     C     PARNET        ANDEQ     'F'
060900020212     C                   SETON                                        64  90
061000060207     C                   GOTO      FICTR1
061100020212     C                   ENDIF
061200010806     C* SE I GIORNI DI RIENTRO SUPERANO IL LIMITE INDICATO IN TABELLA
061300010806     C*   E SI TRATTA DI TARIFFA DPD    --> ERRORE
061400010806     C     VIDGGR        IFGT      §2GGDP
061500020204     C     PARNET        ANDEQ     'D'
061600010806     C                   SETON                                        63  90
061700060207     C                   GOTO      FICTR1
061800010806     C                   ENDIF
061900961205     C*
062000940831     C****  TIPO COMUNICAZIONE AL MITTENTE  ****
062100940901     C     VIDTCM        IFEQ      '?'
062200940901     C                   MOVEL     '2F'          CODTAB
062300940901     C                   MOVEL     ' '           VIDTCM
062400940901     C                   MOVEL     *BLANKS       DESTAB           25
062500940831     C                   CALL      'X§TABER'
062600940831     C                   PARM                    CODUT
062700940901     C                   PARM                    CODTAB
062800940901     C                   PARM                    KEYTAB
062900940901     C                   PARM                    DESTAB
063000940901     C                   MOVEL     KEYTAB        VIDTCM
063100940901     C                   MOVEL     DESTAB        DESTCM
063200940831     C                   SETON                                        90
063300060207     C                   GOTO      FICTR1
063400940831     C                   ENDIF
063500940831     C*
063600940901     C* SE IMMESSO CONTROLLO VALIDITA' TIPO COMUNICAZIONE AL MITTENTE
063700940901     C     VIDTCM        IFNE      ' '
063800960202RE   C                   MOVEL     *BLANKS       WCTFAX            1
063900940901     C                   MOVEL     '2F'          CODTAB
064000940901     C                   MOVEL     *BLANKS       KEYTAB
064100940901     C                   MOVEL     VIDTCM        KEYTAB
064200940901     C     KTAB          CHAIN     TABEL                              35
064300940901     C  N35TBLFLG        IFNE      ' '
064400940901     C                   SETON                                        35
064500940831     C                   ENDIF
064600940901     C* TIPO COMUNICAZIONE ERRATO
064700940901     C     *IN35         IFEQ      *ON
064800940901     C                   SETON                                        4890
064900060207     C                   GOTO      FICTR1
065000940831     C                   ENDIF
065100940831     C*
065200940901     C* DECODIFICA TIPO COMUNICAZIONE AL MITTENTE
065300960202RE   C*------------------------------------------------------*
065400960202RE   C*  Se tipo comunicazione apertura giacenza è presente  *
065500960202RE   C*  in tabella e tipo invio è tramite FAX ne verifico   *
065600960202RE   C*  l'attendibilità                                     *
065700960202RE   C                   MOVEL     TBLUNI        DS2F
065800960202RE   C                   MOVEL     §2FDES        DESTCM
065900960202RE   C     §2FFTP        IFEQ      'F'
066000960202RE   C                   MOVEL     'S'           WCTFAX            1
066100960202RE   C                   MOVE      VIDCLI        §QUATR            4
066200960202RE   C     §QUATR        IFEQ      *ALL'8'
066300960202RE   C     §QUATR        OREQ      *ALL'9'
066400960202RE   C                   SETON                                        5390
066500060207RE   C                   GOTO      FICTR1
066600960202RE   C                   ELSE
066700960202RE   C                   CLEAR                   TRUL42
066800960315RE   C                   CLEAR                   TRUL43
066900981211RE   C                   MOVEL     WTLF          D42FAX
067000960315RE   C                   MOVE      VIDCLI        D43KSC
067100960315RE   C                   CALL      'TRUL43R'
067200960315RE   C                   PARM                    TRUL43
067300960315RE   C     D43TRV        IFEQ      'S'
067400960315RE   C                   MOVEL     D43FAX        D42FAX
067500960315RE   C                   END
067600960202RE   C                   CALL      'TRUL42R'
067700960202RE   C                   PARM                    TRUL42
067800960202RE   C     D42ERR        IFEQ      '1'
067900960202RE   C                   SETON                                        5490
068000060207RE   C                   GOTO      FICTR1
068100960202RE   C                   END
068200960202RE   C                   END
068300960202RE   C                   END
068400960202RE   C*------------------------------------------------------*
068500040729      * se il tipo comunicazione è tramite MAIL
068600040729if  2c                   If        §2fFtp = 'M'
068700040729     c                   Move      VidCli        §QUATR
068800040729if  3c                   If        §quatr = *ALL'8' or §quatr = *ALL'9'
068900040729     c                   Eval      *In53 = *On
069000040729     c                   Eval      *In90 = *On
069100060207     c                   Goto      fictr1
069200040729   x3c                   else
069300040729     c                   Clear                   Trul43
069400091113      * tariffa
069500040729     c  n37              Move      VidCli        D43Ksc
069600091113      * offerta
069700040729     c                   If        *In37 and VisKsc > *Zeros
069800040729     c                   Move      VisKsc        D43Ksc
069900040729     c                   EndIf
070000040729     c                   If        *In37 and VisKsc = *Zeros
070100040729     c                   Move      VidCli        D43Ksc
070200040729     c                   EndIf
070300091113
070400040729     c                   Call      'TRUL43R'
070500040729     c                   Parm                    Trul43
070600040729if  4c                   If        D43Trv = 'S'
070700040729if  5c                   If        D43Trvm = 'N'
070800040729     c                   Eval      *In65 = *On
070900040729     c                   Eval      *In90 = *On
071000060207     c                   Goto      fictr1
071100040729e   5c                   EndIf
071200040729   x4c                   Else
071300040729      * cerco la nota 88 se manca il luogo di invio MAIL
071400091113     c                   select
071500091113      * tariffa
071600091113     c                   when      not *in37
071700091113     c                   Movel     'C'           kcApl
071800091113      * offerta da trattativa
071900101220     c                   when      *in37
072000091113     c                   Movel     'T'           kcApl
072100091113     c                   endsl
072200091113
072300040729     c                   Movel     dutkci        kcNk1
072400040729     c                   Move      VidCli        kcNk1
072500040729     c                   Eval      woknota88 = *Off
072600091113     c     kNtc88        chain     Tfntc01l
072700091113     c                   If        %Found(tfntc01l)
072800040729     c                   Eval      woknota88 = *On
072900091113     c                   EndIf
073000130125      * non ho trovato la nota e trattativa su cliente
073100130125      * cerco con il cliente della trattativa
073200130125     c                   if        woknota88 = *Off and *in37
073300091113     c                             and visksc > 0
073400091113     c                   eval      kcapl = 'C'
073500091113     c                   move      visksc        kcnk1
073600091113     c     kNtc88        chain     Tfntc01l
073700091113     c                   If        %Found(tfntc01l)
073800091113     c                   Eval      woknota88 = *On
073900091113     c                   EndIf
074000091113     c                   endif
074100040729     c                   If        woknota88 = *Off
074200040729     c                   Eval      *In65 = *On
074300040729     c                   Eval      *In90 = *On
074400060207     c                   Goto      fictr1
074500040729     c                   EndIf
074600040729e   4c                   EndIf
074700040729e   3c                   EndIf
074800040729e   2c                   EndIf
074900040729
075000940901     C                   ELSE
075100940901     C                   CLEAR                   DESTCM
075200940901     C                   ENDIF
075300940831     C*
075400940831     C****  TIPO COMUNICAZIONE FINE GIACENZA  ****
075500940901     C     VIDTFG        IFEQ      '?'
075600940901     C                   MOVEL     '2H'          CODTAB
075700940901     C                   MOVEL     ' '           VIDTFG
075800940901     C                   MOVEL     *BLANKS       DESTAB           25
075900940901     C                   CALL      'X§TABER'
076000940901     C                   PARM                    CODUT
076100940901     C                   PARM                    CODTAB
076200940901     C                   PARM                    KEYTAB
076300940901     C                   PARM                    DESTAB
076400940901     C                   MOVEL     KEYTAB        VIDTFG
076500940901     C                   MOVEL     DESTAB        DESTFG
076600940901     C                   SETON                                        90
076700060207     C                   GOTO      FICTR1
076800940901     C                   ENDIF
076900940901     C*
077000940901     C* SE IMMESSO CONTROLLO VALIDITA' TIPO COMUNICAZIONE FINE GIACENZA
077100940901     C     VIDTFG        IFNE      ' '
077200940901     C                   MOVEL     '2H'          CODTAB
077300940901     C                   MOVEL     *BLANKS       KEYTAB
077400940901     C                   MOVEL     VIDTFG        KEYTAB
077500940901     C     KTAB          CHAIN     TABEL                              35
077600940901     C  N35TBLFLG        IFNE      ' '
077700940901     C                   SETON                                        35
077800940901     C                   ENDIF
077900940901     C* TIPO COMUNICAZIONE ERRATO
078000940901     C     *IN35         IFEQ      *ON
078100940901     C                   SETON                                        4990
078200060207     C                   GOTO      FICTR1
078300940901     C                   ENDIF
078400940901     C*
078500940901     C* DECODIFICA TIPO COMUNICAZIONE FINE GIACENZA
078600960202RE   C*------------------------------------------------------*
078700960202RE   C*  Se tipo comunicazione chiusura giacenza è presente  *
078800960202RE   C*  in tabella e tipo invio è tramite FAX ne verifico   *
078900960202RE   C*  l'attendibilità                                     *
079000960202RE   C                   MOVEL     TBLUNI        DS2H
079100960202RE   C                   MOVEL     §2HDES        DESTFG
079200960202RE   C     §2HFAX        IFEQ      'F'
079300960202RE   C     WCTFAX        ANDNE     'S'
079400960202RE   C                   MOVE      VIDCLI        §QUATR            4
079500960202RE   C     §QUATR        IFEQ      *ALL'8'
079600960202RE   C     §QUATR        OREQ      *ALL'9'
079700960202RE   C                   SETON                                        5590
079800060207RE   C                   GOTO      FICTR1
079900960202RE   C                   ELSE
080000960202RE   C                   CLEAR                   TRUL42
080100960315RE   C                   CLEAR                   TRUL43
080200981211RE   C                   MOVEL     WTLF          D42FAX
080300960315RE   C                   MOVE      VIDCLI        D43KSC
080400960315RE   C                   CALL      'TRUL43R'
080500960315RE   C                   PARM                    TRUL43
080600960315RE   C     D43TRV        IFEQ      'S'
080700960315RE   C                   MOVEL     D43FAX        D42FAX
080800960315RE   C                   END
080900960202RE   C                   CALL      'TRUL42R'
081000960202RE   C                   PARM                    TRUL42
081100960202RE   C     D42ERR        IFEQ      '1'
081200960202RE   C                   SETON                                        5690
081300060207RE   C                   GOTO      FICTR1
081400960202RE   C                   END
081500960202RE   C                   END
081600960202RE   C                   END
081700960202RE   C*------------------------------------------------------*
081800940901     C                   ELSE
081900940901     C                   CLEAR                   DESTFG
082000940901     C                   ENDIF
082100940901     C*
082200060207     C     FICTR1        ENDSR
082300940901     C*
082400940315      *-----------------------------------------------------***********
082500940315     C* AGGANCIO IL FILE GIACENZE                              AGGANC
082600940315      *-----------------------------------------------------***********
082700940315     C     AGGANC        BEGSR
082800940315     C                   MOVEL     VIDCLI        CODCLI
082900060207     C  n37KTGC          CHAIN     TITGC000                           80
083000060207     C   37KTGC          CHAIN     TIoGC000                           80
083100940315     C*
083200940315     C* RIEMPO I CAMPI
083300940315     C*
083400980623     C     *IN80         IFEQ      *OFF
083500940315     C                   EXSR      RIEMPI
083600940315     C                   ENDIF
083700940315     C*
083800940901     C*
083900940831     C* SE IL TIPO COMUNICAZIONE AL MITTENTE E IL TIPO COMUNICAZIONE
084000940831     C*   FINE GICENZA SONO VUOTI, LI PROPONGO PRENDENDOLI DAL P.D.C.
084100060206      * se da visita prima controllo se c'è l'anagrafica provvisoria
084200060206     c                   If        *In37
084300060206     c     codcli        Chain     Tfcls01l                           35
084400060206      * Non trovo anagrafica provvisoria controllo con il cliente su
084500060206      * anagrafico P.d.c.
084600060206     c                   If        *In35 and visksc > 0
084700060206     c     visksc        Chain     Fncls01l                           35
084800060207     c                   endif
084900060206      * se è da tariffa diretta sul p.d.c.
085000060206     c                   Else
085100060206     c     codcli        Chain     fncls01l                           35
085200060206     c                   endif
085300060206
085400060206     c                   if        not *in35
085500940901     C                   MOVEA     CLSFLO        FLG
085600041029      * valorizzo  il campo di comodo
085700041029     c                   eval      anatcm = '(in anagrafica = ' + vidfl0 +
085800041029     c                             ' )'
085900041029      * valorizzo  il campo di comodo
086000041029     c                   eval      anatfg = '(in anagrafica = ' + vidfl1 +
086100041029     c                             ' )'
086200940901     C                   ENDIF
086300950121     C*
086400940901     C* DECODIFICA TIPO COMUNICAZIONE AL MITTENTE
086500940901     C                   MOVEL     '2F'          CODTAB
086600940901     C                   MOVEL     *BLANKS       KEYTAB
086700940901     C                   MOVEL     VIDTCM        KEYTAB
086800940901     C     KTAB          CHAIN     TABEL                              35
086900940901     C  N35              MOVEL     TBLUNI        DESTCM
087000940901     C*
087100940901     C* DECODIFICA TIPO COMUNICAZIONE FINE GIACENZA
087200940901     C                   MOVEL     '2H'          CODTAB
087300940901     C                   MOVEL     *BLANKS       KEYTAB
087400940901     C                   MOVEL     VIDTFG        KEYTAB
087500940901     C     KTAB          CHAIN     TABEL                              35
087600940901     C  N35              MOVEL     TBLUNI        DESTFG
087700940901     C*
087800940315     C                   ENDSR
087900900515      *-----------------------------------------------------***********
088000900515      * REGISTRAZIONE RECORD                                *  REGIS  *
088100900515      *-----------------------------------------------------***********
088200900515     C     REGIS         BEGSR
088300900515      *
088400900517     C                   MOVEL     *BLANK        TGCATB
088500900517     C                   MOVE      VIDCLI        TGCKSC
088600940831     C                   MOVE      VIDCTR        TGCCTR
088700940831     C                   MOVE      VIDPRG        TGCPRG
088800900517     C                   Z-ADD     VIDSGV        TGCSGV
088900940826     C                   Z-ADD     VIDSGR        TGCSGR
089000900517     C                   Z-ADD     VIDSGP        TGCSGP
089100900517     C                   Z-ADD     VIDSGD        TGCSGD
089200900517     C                   Z-ADD     VIDSG1        TGCSG1
089300900517     C                   Z-ADD     VIDSG2        TGCSG2
089400900517     C                   Z-ADD     VIDSG3        TGCSG3
089500900517     C                   Z-ADD     VIDST1        TGCST1
089600900517     C                   Z-ADD     VIDST2        TGCST2
089700900517     C                   Z-ADD     VIDST3        TGCST3
089800900517     C                   Z-ADD     VIDSTM        TGCSTM
089900900517     C                   Z-ADD     VIDRPV        TGCRPV
090000900517     C                   Z-ADD     VIDSGF        TGCSGF
090100911120     C                   Z-ADD     VIDGGR        TGCGGR
090200940831     C                   MOVEL     VIDFAF        TGCFAF
090300940903     C*
090400940903     C* IL TIPO COMUNICAZIONE AL MITTENTE E IL TIPO COMUNICAZIONE
090500940914     C*   FINE GIACENZA LI MEMORIZZO SOLO SE VARIANO RISPETTO A
090600940903     C*   QUELLO CHE C'E' SUL P.D.C.
090700940903     C     VIDTCM        IFNE      VIDFL0
090800940901     C                   MOVEL     VIDTCM        TGCTCM
090900950106     C                   ELSE
091000950106     C                   CLEAR                   TGCTCM
091100940903     C                   ENDIF
091200950106     C*
091300940903     C     VIDTFG        IFNE      VIDFL1
091400940831     C                   MOVEL     VIDTFG        TGCTFG
091500950106     C                   ELSE
091600950106     C                   CLEAR                   TGCTFG
091700940903     C                   ENDIF
091800940903     C*
091900990615     C* SE VIENE VARIATO TITGC ABBLENCO FLAG E DATA TRASMISSIONE
092000950116    1C     COMTGC        IFNE      TGCDS
092100940826     C                   MOVE      ' '           TGCFTR
092200940826     C                   Z-ADD     0             TGCDTR
092300950116    1C                   ENDIF
092400900516      *
092500990615     C* VALORIZZO ULTIMA DATA VARIAZIONE
092600990615     C                   Z-ADD     DATEU8        TGCDUV
092700040916     C* VALORIZZO DATA trasmissione
092800040916     C                   Z-ADD     DATEU8        TGCDTR
092900990615      *
093000060207     c                   if        not *in80
093100060207     C  n37              UPDATE    TITGC000
093200060207     C   37              UPDATE    TIogC000
093300060207     c                   endif
093400030714      * VALORIZZO IL FLAG PER LA SCRITTURA DEL RECORD STORICO
093500030717      * se non si tratta di annullamento e non si tratta di offerta
093600030717     c                   if        not *in16 and not *in80 and not *in37
093700030716     C                   EVAL      PARMAN = 'S'
093800030716     c                   endif
093900030714      *
094000060207     c                   If        *in80
094100060207     C  n37              WRITE     TITGC000
094200060207     C   37              WRITE     TIoGC000
094300060207     c                   endif
094400950116     C*
094500900515     C                   ENDSR
094600900515      *-----------------------------------------------------***********
094700900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
094800900515      *-----------------------------------------------------***********
094900900515     C     RIEMPI        BEGSR
095000990908     C*
095100990908     C* GIRO DATA ULTIMA VARIAZIONE
095200990908     C*
095300990908     C                   Z-ADD     TGCDUV        G02INV
095400990908     C                   MOVEL     '3'           G02ERR
095500990908     C                   CALL      'XSRDA8'
095600990908     C                   PARM                    WLBDAT
095700990908     C                   Z-ADD     G02DAT        VIDDUV
095800950116     C*
095900950116     C                   MOVEL     TGCDS         COMTGC
096000950116     C*
096100900517     C                   Z-ADD     TGCSGV        VIDSGV
096200940826     C                   Z-ADD     TGCSGR        VIDSGR
096300900517     C                   Z-ADD     TGCSGP        VIDSGP
096400900517     C                   Z-ADD     TGCSGD        VIDSGD
096500900517     C                   Z-ADD     TGCSG1        VIDSG1
096600900517     C                   Z-ADD     TGCSG2        VIDSG2
096700900517     C                   Z-ADD     TGCSG3        VIDSG3
096800900517     C                   Z-ADD     TGCST1        VIDST1
096900900517     C                   Z-ADD     TGCST2        VIDST2
097000900517     C                   Z-ADD     TGCST3        VIDST3
097100900517     C                   Z-ADD     TGCSTM        VIDSTM
097200900517     C                   Z-ADD     TGCRPV        VIDRPV
097300110916     C                   Z-ADD     TGCRPV        SAVrpv
097400900517     C                   Z-ADD     TGCSGF        VIDSGF
097500911120     C                   Z-ADD     TGCGGR        VIDGGR
097600940831     C                   MOVEL     TGCFAF        VIDFAF
097700940831     C                   MOVEL     TGCTCM        VIDTCM
097800940831     C                   MOVEL     TGCTFG        VIDTFG
097900900517      *
098000950105     C     TGCATB        IFNE      ' '
098100980623     C                   SETON                                        50  99
098200900517     C                   ELSE
098300900517     C                   SETOFF                                       50  99
098400900517     C                   END
098500900515      *
098600900517     C                   ENDSR
098700030716     C*---------------------------------------------------------------*
098800030716     C* ROUTINE DI SCRITTURA FILE STORICO TITAV00F
098900030716     C*---------------------------------------------------------------*
099000030716     C     SUB_STORICO   BEGSR
099100030716      *la prima volta inizializzo i campi del file che vanno sempre bene
099200030716     c                   if        first = ' '
099300030716     C                   eval      TAVksc= tgcksc
099400030716     C                   eval      TAVctr= tgcctr
099500030716     C                   eval      TAVprg= tgcprg
099600030716      * data variazione
099700030716     C                   eval      TAVdav= dateu8
099800030716     C                   eval      TAVnoj= job_name
099900030716     C                   eval      TAVpru= user
100000030716     C                   eval      first = 'N'
100100030716     c                   endif
100200030716      * recupero ora di variazione
100300030716     c                   time                    w0140
100400030716     c                   movel     w0140         TAVorv
100500030716     c                   eval      TAVtrd= 'GIA'
100600030716     c                   clear                   TAVftc
100700041005     c                   z-add     dateu8        TAVdtr
100800030716      *
100900030716     c                   write     titav000
101000030716     C*
101100030716     C                   ENDSR
101200030716     C***
101300910617**
101400910617 ** GESTIONE TARIFFE GIACENZA **
101500910617 ** GESTIONE OFFERTE GIACENZA **
