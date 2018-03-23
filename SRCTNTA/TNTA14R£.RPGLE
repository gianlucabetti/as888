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
004400110916     d ds1G          e ds
004500940315     D PARAM           DS
004600091210      * richiamato da tariffa/offerta
004700091210     d  parast                 9      9
004800091210      * codice cliente/visita/trattativa
004900091210     d  parcli                10     16  0
005000941221     D* DECODIFICA CODICE TARIFFA PASSATO DALLA GESTIONE TARIFFE
005100941221     D  PARDCT                17     26
005200941221     D* DESCRIZIONE TARIFFA       PASSATO DALLA GESTIONE TARIFFE
005300941221     D  PARDCV                27     41
005400961206     D* FLAG ITALIA/ESTERO        PASSATO DALLA GESTIONE TARIFFE
005500961206     D  PARFIE                42     42
005600990824     D* CODICE DIVISA
005700990824     D  VICDIV                43     45
005800990622     D* FLAG GESTIONE DEI DECIMALI
005900990622     D  PARFDC                73     73
006000020204      * NETWORK DPD/FEDEX
006100020204     D  PARNET                74     74
006200090916      * flag per richiamo da trattativa = 'T'
006300090916     d  partrat               75     75
006400091210      * codice tariffa e progressivo
006500091210     d  parctr               125    127  0
006600091210     d  parprg               128    130  0
006700030714     D* PARMAN --> SE RESTITUITO PIENO OCCORRE AGGIORNARE LO STORICO
006800030714     D*            VARIAZIONE
006900030714     D  PARMAN               256    256
007000960202RE   D* Ds esterna x richiamo pgm controllo fax
007100960202RE   D TRUL42        E DS                  EXTNAME(TRUL42DS)
007200960315RE   D* Ds esterna x reperimento luogo invio comunicaz. giac.
007300960315RE   D TRUL43        E DS                  EXTNAME(TRUL43DS)
007400960202RE   D* Ds esterna x scomposizione TBLUNI tabella 2H
007500960202RE   D DS2H          E DS
007600960202RE   D* Ds esterna x scomposizione TBLUNI tabella 2F
007700960202RE   D DS2F          E DS
007800020520     D*
007900020520     D tibs34ds      E DS                  inz
008000020520     D ddatiute      e ds
008100020520     D azuteds       e ds                  extname(AZUTE00F)
008200020520     D codut           S                   LIKE(TBLkut) inz(1)
008300030716
008400030716     d first           s              1    inz
008500040729     d woknota88       s              1    inz('0')
008600040729
008700040729     d kcapl           s                   like(NtcApl)
008800040729     d kcnk1           s                   like(NtcNk1)
008900040729     d kcnk2           s                   like(NtcNk2) inz
009000040729     d kctnt           s                   like(NtcTnt) inz('88')
009100110916
009200110916     d savrpv          s                   like(TGCrpv) inz
009300030716
009400030716     D* Reperimento dati del PGM
009500030716     D PGMSTATUS      SDS
009600030716     D JOB_NAME              244    253
009700030716     D USER                  254    263
009800090916
009900090916     itivis000
010000090916     i              visesi                      wisesi
010100030716
010200900515      *---------------------------------------------------------------*
010300000000     C     *ENTRY        PLIST
010400000000     C                   PARM                    KPJBA
010500900522     C                   MOVEL     KPJBU         PARAM
010600020520     c*
010700020520     c     *dtaara       define    §azute        azuteds
010800020520     c     *dtaara       define    §datiute      ddatiute
010900020520     C                   in(E)     *dtaara
011000020520     C                   IF        %error  or RSUT = *blanks
011100020520     C                   CLEAR                   tibs34ds
011200020520     C                   CALL      'TIBS34R'
011300020520     C                   PARM                    tibs34ds
011400020520     C                   in        *dtaara
011500020520     c                   endif
011600020520     c* N.B.:Dal momento che la call al tbs34r mi serve solo per reperire il
011700020520     c* capoconto clienti italia e la ragione sociale utente non testo la
011800020520     c* presenza di eventuali errori
011900020520     C                   MOVEL(P)  RSUT          PAXDUT           30
012000020520     C                   MOVEL(P)  RSUT          RSUTP            39
012100991104      *
012200020919     C                   MOVEL     SIMFEL        simfelA           3
012300101220      * SEMPRE       richiamato da trattativa
012400101220     c**                 if        partrat = 'T'
012500090916     c                   eval      *in38 = *on
012600101220     c**                 else
012700101220     c**                 eval      *in38 = *off
012800101220     c**                 endif
012900101220     c
013000030702      * se programma richiamato dall'interrogazione il campo PARFDC
013100030702      * moneta con decimali non è valorizzato lo valorizzo adesso
013200030702      * controllando la moneta
013300030702     c                   IF        (VICDIV <> *blanks or VICDIV <> 'ITL')
013400030702     c                             and PARFDC = *blank
013500030702     c                   eval      parfdc ='S'
013600030702     c                   endif
013700900515      *---------------------------------------------------------------*
013800960321      * INDICATORI                                                    *
013900900515      *---------------------------------------------------------------*
014000960321      * 13    - CMD DI RIATTIVAZIONE
014100960321      * 16    - CMD DI ANNULLAMENTO
014200960509      * 18    - SI TRATTA DI CLIENTE ESTERO
014300990622      * 20/27 - ERRORE VIDEO
014400960321      * 31/33 - DI COMODO
014500960321      * 37    - ON - OFFERTA     OFF - TARIFFA
014600090916      * 38    - Trattativa
014700960321      * 40/49 - ERRORI
014800960321      * 50    - TARIFFA ANNULLATA
014900960321      * 51/59 - ERRORI
015000960321      * 60/61 - USATI PER LA STAMPA DELLA LETTERA
015100010806      * 62/63 - ERRORE
015200960321      * 72/73 - ERRORI
015300960321      * 80    - ON - IMMISSIONE  OFF - VARIAZIONE
015400960321      * 90    - INDICATORE DI ERRORE GENERICO
015500991018      * 95    - CIFRE CON DECIMALI
015600960321      * 98    - SONO IN INTERROGAZIONE
015700960321      * 99    - PROTEGGE TUTTI I CAMPI PER INTERROGAZIONE O ANNULLAM
015800960321      *---------------------------------------------------------------*
015900960321      * CHIAVI                                                        *
016000960321      *---------------------------------------------------------------*
016100900515     C     KACO          KLIST
016200900515     C                   KFLD                    CODUT
016300020520     C                   KFLD                    dutkci
016400900517     C                   KFLD                    CODCLI            7 0
016500900523      *
016600900523     C     KTAB          KLIST
016700900523     C                   KFLD                    CODUT
016800900523     C                   KFLD                    CODTAB            2
016900900523     C                   KFLD                    KEYTAB            8
017000040729      * chiave per nota 88 (così non sporco la chiave che si imposta
017100040729      *                     per il responsabile)
017200040729     c     kntc88        klist
017300040729     c                   kfld                    kcapl
017400040729     c                   kfld                    kcnk1
017500040729     c                   kfld                    kcnk2
017600040729     c                   kfld                    kctnt
017700040729
017800941122     C     KTGC          KLIST
017900941122     C                   KFLD                    CODCLI
018000091210     C                   KFLD                    parCTR
018100091210     C                   KFLD                    parPRG
018200900515      *---------------------------------------------------------------*
018300900515      * FLUSSO PRINCIPALE                                             *
018400900515      *---------------------------------------------------------------*
018500940316     C                   EXSR      CTRIN
018600900515      *          =============
018700900509     C     INIZIO        TAG
018800900515      *          =============
018900900515     C                   EXSR      PULIZ
019000981211     C* CLIENTE
019100091210     C     parAST        IFEQ      'C'
019200091210     C                   MOVE      parcli        VIDCLI
019300091210     C                   MOVEL     parcli        CODCLI
019400981211     C     KACO          CHAIN     CNIND00F                           36
019500981211     C   36              CLEAR                   INDTLF
019600981211     C                   MOVEL     INDTLF        WTLF
019700981211     C     *LIKE         DEFINE    INDTLF        WTLF
019800060207      * ragione sociale cliente
019900060207     C     KACO          CHAIN     CNaco00F
020000060207     c                   If        %Found(cnaco00f)
020100060207     c                   eval      descli = acorag
020200060207     c                   endif
020300981211     C                   ELSE
020400101220     C* trattativa
020500091210     C                   MOVE      parcli        VIDNVST
020600091210     C                   MOVE      parcli        VIDCLI
020700101220     C     parcli        CHAIN     TiVIS05L                           31
020800060207     c  n31              eval      descli = visrag
020900981211     C* VEDO SE ESISTE TFIND O CNIND
021000981211     C                   MOVEL     VISNRV        CODCLI
021100060207     C     KACO          CHAIN     TFIND00F                           36
021200060207     c                   If        *In36 and visksc > 0
021300060207     C                   MOVEL     VISKSC        CODCLI
021400981211     C     KACO          CHAIN     CNIND00F                           36
021500060207     c                   endif
021600981211     C   36              CLEAR                   INDTLF
021700981211     C                   MOVEL     INDTLF        WTLF
021800981211     C                   ENDIF
021900060207      * ragione sociale cliente
022000060207     C                   MOVEL     VISNRV        CODCLI
022100940315      *
022200091210     C                   MOVE      parCTR        VIDCTR
022300091210     C                   MOVE      parPRG        VIDPRG
022400981113     C                   MOVE      PARDCT        DESCTR
022500990621     C                   MOVE      PARDCV        VIDDCV
022600940315     C* AGGANCIO IL FILE GIACENZE
022700940826     C                   EXSR      AGGANC
022800900517      *
022900900515      *          =============
023000060207     C     FMT01         TAG
023100900515      *          =============
023200060207     C                   EXFMT     TA14D01
023300940315     C* CMD12 - RITORNO
023400060207     C   kl              GOTO      FINE
023500910617      *
023600940315     C* CONTROLLO
023700060207     C                   EXSR      CTR1
023800060207     C   90              GOTO      FMT01
023900900516      *
024000960321      * F13 - RIATTIVA
024100900517      *
024200900517     C   13              DO
024300900517     C                   MOVE      ' '           TGCATB
024400990615     C* VALORIZZO ULTIMA DATA VARIAZIONE
024500990615     C                   Z-ADD     DATEU8        TGCDUV
024600040916     C                   Z-ADD     DATEU8        TGCDTR
024700060207     C  n37              UPDATE    TITGC000
024800060207     C   37              UPDATE    TIoGC000
024900030717      * scrivo lo storico variazioni se non si tratta di offerte
025000030717     c                   if        not *in37
025100030716     c                   eval      TAVcta = 'I'
025200030716     c                   exsr      SUB_storico
025300030717     c                   endif
025400030716      *
025500900516      *
025600900516      * RIFACCIO LA CHAIN PER L'EVENTUALE AGGIORNAMENTO SEGUENTE
025700900516      *
025800060207     C  n37KTGC          CHAIN     TITGC01L                           80
025900060207     C   37KTGC          CHAIN     TIoGC01L                           80
026000910617     C                   SETOFF                                       5099
026100060207     C                   GOTO      FMT01
026200900516     C                   END
026300900516      *
026400940315      * CMD16 - ANNULLA
026500900517      *
026600900516     C   16              DO
026700950116     C*
026800910617     C* REGISTRO TARIFFA SE NON C'E
026900910617     C                   EXSR      REGIS
027000910617     C* RIFACCIO CHAIN PER ANNULLARE
027100060207     C  n37KTGC          CHAIN     TITGC01L                           80
027200060207     C   37KTGC          CHAIN     TIoGC01L                           80
027300910624     C*
027400910624     C* DA OFFERTA
027500091210     C     parAST        IFEQ      'V'
027600060207     C   37              DELETE    TIoGC000
027700950117     C                   GOTO      FINE
027800910617     C                   ELSE
027900910624     C* DA TARIFFA
028000990615     C* VALORIZZO ULTIMA DATA VARIAZIONE
028100990615     C                   Z-ADD     DATEU8        TGCDUV
028200950117     C                   MOVE      'A'           TGCATB
028300950117     C                   CLEAR                   TGCFTR
028400040916     C                   Z-ADD     DATEU8        TGCDTR
028500060207     C  n37              UPDATE    TITGC000
028600030717      * scrivo lo storico variazioni se non sono in gestione offerta
028700030717     c                   if        not *in37
028800030716     c                   eval      TAVcta = 'A'
028900030716     c                   exsr      SUB_storico
029000030717     c                   endif
029100030716      *
029200950117     C                   GOTO      FINE
029300910617     C                   END
029400900516     C                   END
029500900515      *
029600060207     C* CMD6 - REGISTRA
029700960321     C     *INKF         IFEQ      *ON
029800940315     C                   EXSR      REGIS
029900910617     C                   GOTO      FINE
030000940315     C                   END
030100900517      *
030200060207     C                   GOTO      FMT01
030300900517      *
030400000000     C     FINE          TAG
030500950116     C                   MOVEL     PARAM         KPJBU
030600950116     C*
030700000000     C                   SETON                                        LR
030800900515      *-----------------------------------------------------***********
030900900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
031000900515      *-----------------------------------------------------***********
031100900515     C     PULIZ         BEGSR
031200900515      *
031300900517     C                   MOVE      *BLANKS       VIDCLI
031400900517     C                   MOVEL     *BLANKS       DESCLI
031500940831     C                   Z-ADD     0             VIDCTR
031600940831     C                   MOVEL     *BLANKS       DESCTR
031700940831     C                   MOVEL     *BLANKS       VIDDCV
031800990908     C                   Z-ADD     0             VIDDUV
031900940831     C                   Z-ADD     0             VIDPRG
032000900517     C                   Z-ADD     0             VIDSGV
032100940826     C                   Z-ADD     0             VIDSGR
032200900517     C                   Z-ADD     0             VIDSGP
032300900517     C                   Z-ADD     0             VIDSGD
032400900517     C                   Z-ADD     0             VIDSG1
032500900517     C                   Z-ADD     0             VIDSG2
032600900517     C                   Z-ADD     0             VIDSG3
032700900517     C                   Z-ADD     0             VIDST1
032800900517     C                   Z-ADD     0             VIDST2
032900900517     C                   Z-ADD     0             VIDST3
033000900517     C                   Z-ADD     0             VIDSTM
033100900517     C                   Z-ADD     0             VIDRPV
033200110916     C                   Z-ADD     0             SAVrpv
033300900517     C                   Z-ADD     0             VIDSGF
033400911120     C                   Z-ADD     0             VIDGGR
033500940831     C                   MOVEL     ' '           VIDFAF
033600950121     C                   MOVEL     ' '           VIDTCM
033700950121     C                   MOVEL     ' '           VIDTFG
033800940901     C                   CLEAR                   DESTCM
033900940901     C                   CLEAR                   DESTFG
034000950116     C*
034100950116     C                   MOVEL     *BLANKS       COMTGC           62
034200940316     C                   ENDSR
034300940316      *-----------------------------------------------------***********
034400940316      * OPERAZIONI INIZIALI       O                         *  CTRIN  *
034500940316      *-----------------------------------------------------***********
034600940316     C     CTRIN         BEGSR
034700941219     C* PRENDO LA DATA DEL GIORNO DA TIME
034800941219     C                   TIME                    W0140            14 0
034900941219     C                   MOVE      W0140         UDATE8            8 0
035000990615     C*
035100990615     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
035200990615     C*
035300990615     C                   Z-ADD     UDATE8        G02DAT
035400990615     C                   MOVEL     *BLANK        G02ERR
035500990615     C                   CALL      'XSRDA8'
035600990615     C                   PARM                    WLBDAT
035700990615     C* UDATE A 8 IN AAAA/MM/GG
035800990615     C                   Z-ADD     G02INV        DATEU8            8 0
035900900515      *
036000941220     C                   MOVEL     'TNTA14R'     VIDPGM
036100991018     C* CONTROLLO SE TARIFFA CON DECIMALI
036200991018     C     PARFDC        COMP      'S'                                    95
036300900524      *
036400091113     C* VEDO SE OFFERTA
036500091210     C     parAST        IFEQ      'V'
036600910617     C                   MOVE      TES(2)        VIDTES
036700910617     C                   SETON                                        37
036800910617     C                   OPEN      TFIND00F
036900101220     C                   OPEN      TiVIS05L
037000990615     C                   OPEN      TIOGC01L
037100060206     C                   OPEN      tfcls01L
037200091113     C* O TARIFFA
037300910617     C                   ELSE
037400910617     C                   MOVE      TES(1)        VIDTES
037500910617     C                   END
037600940315     C*
037700961205     C* AGGANCIO LA TABELLA "2G" PER REPERIRE IL NUMERO MASSIMO DEI
037800961205     C*   GIRONI DI RIENTRO PER LE TARIFFE ESTERE
037900961205     C                   MOVE      '2G'          CODTAB
038000961205     C                   MOVEL     '1       '    KEYTAB
038100961205     C     KTAB          CHAIN     TABEL                              33
038200961205     C     *IN33         IFEQ      *OFF
038300961205     C                   MOVEL     TBLUNI        DS2G
038400961205     C                   ELSE
038500961205     C                   CLEAR                   DS2G
038600961205     C                   Z-ADD     99            §2GGRE
038700010806     C                   Z-ADD     99            §2GGDP
038800020212     C                   Z-ADD     99            §2GGFE
038900961205     C                   ENDIF
039000110916
039100110916      * Aggancio la tabella 1G per rapporto peso/volume
039200110916     c                   eval      codtab = '1G'
039300110916     c                   eval      keytab = '1       '
039400110916     c     ktab          chain     tabel00f
039500110916     c                   IF        %found(tabel00f) and TBLflg = *blanks
039600110916     c                   eval      ds1G = TBLuni
039700110916     c                   ELSE
039800110916     c                   clear                   ds1G
039900110916     c                   ENDIF
040000961205     C*
040100900514     C                   ENDSR
040200900517      *-----------------------------------------------------***********
040300060207      * CONTROLLI FORMATO 1                                 *  CTR1   *
040400900517      *-----------------------------------------------------***********
040500060207     C     CTR1          BEGSR
040600900517      *
040700900517     C                   SETOFF                                       90
040800990622      * CONTROLLO I DECIMALI DI TUTTI GLI IMPORTI IN BASE ALLA DIVISA
040900990622     C     VIDSGV        IFGT      0
041000990622     C                   MOVE      VIDSGV        W0030             3 0
041100990622     C     W0030         IFGT      0
041200991022     C     *IN95         ANDNE     *ON
041300990622     C                   SETON                                        2090
041400060207     C                   GOTO      FICTR1
041500990622     C                   END
041600990622     C                   END
041700900523      *--------------------
041800990622     C     VIDSGR        IFGT      0
041900990622     C                   MOVE      VIDSGR        W0030             3 0
042000990622     C     W0030         IFGT      0
042100991022     C     *IN95         ANDNE     *ON
042200990622     C                   SETON                                        2190
042300060207     C                   GOTO      FICTR1
042400990622     C                   END
042500990622     C                   END
042600990622      *--------------------
042700990622     C     VIDSGD        IFGT      0
042800990622     C                   MOVE      VIDSGD        W0030             3 0
042900990622     C     W0030         IFGT      0
043000991022     C     *IN95         ANDNE     *ON
043100990622     C                   SETON                                        2290
043200060207     C                   GOTO      FICTR1
043300990622     C                   END
043400990622     C                   END
043500990622      *--------------------
043600990622     C     VIDSGP        IFGT      0
043700990622     C                   MOVE      VIDSGP        W0030             3 0
043800990622     C     W0030         IFGT      0
043900991022     C     *IN95         ANDNE     *ON
044000990622     C                   SETON                                        2390
044100060207     C                   GOTO      FICTR1
044200990622     C                   END
044300990622     C                   END
044400990622      *--------------------
044500990622     C     VIDST1        IFGT      0
044600990622     C                   MOVE      VIDST1        W0030             3 0
044700990622     C     W0030         IFGT      0
044800991022     C     *IN95         ANDNE     *ON
044900990622     C                   SETON                                        2490
045000060207     C                   GOTO      FICTR1
045100990622     C                   END
045200990622     C                   END
045300990622      *--------------------
045400900523     C     VIDSG2        IFNE      0
045500900523     C     VIDSG1        ANDEQ     0
045600900523     C                   SETON                                        72  90
045700900523     C                   END
045800060207     C   90              GOTO      FICTR1
045900900523      *--------------------
046000900523     C     VIDSG3        IFNE      0
046100900523     C     VIDSG2        ANDEQ     0
046200900523     C                   SETON                                        73  90
046300900523     C                   END
046400060207     C   90              GOTO      FICTR1
046500900523      *--------------------
046600900517     C     VIDSG1        IFNE      999
046700900517     C     VIDSG2        ANDNE     999
046800900517     C     VIDSG3        ANDNE     999
046900960321     C                   SETON                                        40  90
047000900517     C                   END
047100060207     C   90              GOTO      FICTR1
047200900517      *--------------------
047300900517     C     VIDSG1        IFEQ      0
047400900517     C     VIDST1        ANDNE     0
047500960321     C                   SETON                                        57  90
047600900517     C                   END
047700060207     C   90              GOTO      FICTR1
047800900517      *
047900990622      *--------------------
048000990622     C     VIDST2        IFGT      0
048100990622     C                   MOVE      VIDST2        W0030             3 0
048200990622     C     W0030         IFGT      0
048300991022     C     *IN95         ANDNE     *ON
048400990622     C                   SETON                                        2590
048500060207     C                   GOTO      FICTR1
048600990622     C                   END
048700990622     C                   END
048800900517      *--------------------
048900900517     C     VIDSG2        IFEQ      0
049000900517     C     VIDST2        ANDNE     0
049100960321     C                   SETON                                        59  90
049200900517     C                   END
049300060207     C   90              GOTO      FICTR1
049400900517      *
049500900517     C     VIDST2        IFEQ      0
049600900517     C     VIDSG2        ANDNE     0
049700900517     C                   SETON                                        42  90
049800900517     C                   END
049900060207     C   90              GOTO      FICTR1
050000990622      *--------------------
050100990622     C     VIDST3        IFGT      0
050200990622     C                   MOVE      VIDST3        W0030             3 0
050300990622     C     W0030         IFGT      0
050400991022     C     *IN95         ANDNE     *ON
050500990622     C                   SETON                                        2690
050600060207     C                   GOTO      FICTR1
050700990622     C                   END
050800990622     C                   END
050900900517      *--------------------
051000900517     C     VIDSG3        IFEQ      0
051100900517     C     VIDST3        ANDNE     0
051200960321     C                   SETON                                        58  90
051300900517     C                   END
051400060207     C   90              GOTO      FICTR1
051500900517      *
051600900517     C     VIDST3        IFEQ      0
051700900517     C     VIDSG3        ANDNE     0
051800900517     C                   SETON                                        43  90
051900900517     C                   END
052000060207     C   90              GOTO      FICTR1
052100900517      *--------------------
052200900522      * GG2 < GG1
052300900522     C     VIDSG2        IFNE      0
052400900522     C     VIDSG2        IFLT      VIDSG1
052500900522     C                   SETON                                        44  90
052600900522     C                   END
052700900522     C                   END
052800060207     C   90              GOTO      FICTR1
052900900522      *--------------------
053000900522      * GG3 < GG2
053100900522     C     VIDSG3        IFNE      0
053200900522     C     VIDSG3        IFLT      VIDSG2
053300900522     C                   SETON                                        45  90
053400900522     C                   END
053500900522     C                   END
053600060207     C   90              GOTO      FICTR1
053700900522      *--------------------
053800900522      * TR2 < TR1
053900900522     C     VIDST2        IFNE      0
054000900522     C     VIDST2        IFLT      VIDST1
054100900522     C                   SETON                                        46  90
054200900522     C                   END
054300900522     C                   END
054400060207     C   90              GOTO      FICTR1
054500900522      *--------------------
054600900522      * TR3 < TR2
054700900522     C     VIDST3        IFNE      0
054800900522     C     VIDST3        IFLT      VIDST2
054900900522     C                   SETON                                        47  90
055000900522     C                   END
055100900522     C                   END
055200060207     C   90              GOTO      FICTR1
055300990622      *--------------------
055400990622     C     VIDSTM        IFGT      0
055500990622     C                   MOVE      VIDSTM        W0030             3 0
055600990622     C     W0030         IFGT      0
055700991022     C     *IN95         ANDNE     *ON
055800990622     C                   SETON                                        2790
055900060207     C                   GOTO      FICTR1
056000990622     C                   END
056100990622     C                   END
056200900522      *--------------------
056300110916
056400110916      * Rapporto peso volume controllo con tabella 1G
056500110916     c                   IF        VIDrpv <> SAVrpv and
056600110916     c                             VIDrpv > §1Grpv
056700110916     c                   eval      *in60 = *on
056800110916     c                   eval      *in90 = *on
056900110916     c                   eval      SAVrpv = VIDrpv
057000110916     c                   leavesr
057100110916     c                   ENDIF
057200950220     C*
057300950220     C****  FLAG APPLICAZIONE FRANCHIGIA  ****
057400950220     C     VIDFAF        IFEQ      ' '
057500950220     C     VIDSGF        IFGT      0
057600950220     C                   SETON                                        5190
057700060207     C                   GOTO      FICTR1
057800950220     C                   ENDIF
057900950220     C*
058000950220     C                   ELSE
058100950220     C     VIDSGF        IFEQ      0
058200950220     C                   SETON                                        5290
058300060207     C                   GOTO      FICTR1
058400950220     C                   ENDIF
058500950220     C                   ENDIF
058600950220     C*
058700961205     C****  GIRONI DI RIENTRO  ****
058800961205     C* SE I GIORNI DI RIENTRO SUPERANO IL LIMITE INDICATO IN TABELLA
058900020212     C*   E SI TRATTA DI TARIFFA ESTERA
059000020212     C*   E NON E' NETWORK FEDEX        --> ERRORE
059100961205     C     VIDGGR        IFGT      §2GGRE
059200961206     C     PARFIE        ANDEQ     'E'
059300020212     C     PARNET        ANDNE     'F'
059400961205     C                   SETON                                        62  90
059500060207     C                   GOTO      FICTR1
059600961205     C                   ENDIF
059700020212     C* SE I GIORNI DI RIENTRO SUPERANO IL LIMITE INDICATO IN TABELLA
059800020212     C*   E SI TRATTA DI TARIFFA ESTERA
059900020212     C*   E NETWORK FEDEX        --> ERRORE
060000020212     C     VIDGGR        IFGT      §2GGFE
060100020212     C     PARFIE        ANDEQ     'E'
060200020212     C     PARNET        ANDEQ     'F'
060300020212     C                   SETON                                        64  90
060400060207     C                   GOTO      FICTR1
060500020212     C                   ENDIF
060600010806     C* SE I GIORNI DI RIENTRO SUPERANO IL LIMITE INDICATO IN TABELLA
060700010806     C*   E SI TRATTA DI TARIFFA DPD    --> ERRORE
060800010806     C     VIDGGR        IFGT      §2GGDP
060900020204     C     PARNET        ANDEQ     'D'
061000010806     C                   SETON                                        63  90
061100060207     C                   GOTO      FICTR1
061200010806     C                   ENDIF
061300961205     C*
061400940831     C****  TIPO COMUNICAZIONE AL MITTENTE  ****
061500940901     C     VIDTCM        IFEQ      '?'
061600940901     C                   MOVEL     '2F'          CODTAB
061700940901     C                   MOVEL     ' '           VIDTCM
061800940901     C                   MOVEL     *BLANKS       DESTAB           25
061900940831     C                   CALL      'X§TABER'
062000940831     C                   PARM                    CODUT
062100940901     C                   PARM                    CODTAB
062200940901     C                   PARM                    KEYTAB
062300940901     C                   PARM                    DESTAB
062400940901     C                   MOVEL     KEYTAB        VIDTCM
062500940901     C                   MOVEL     DESTAB        DESTCM
062600940831     C                   SETON                                        90
062700060207     C                   GOTO      FICTR1
062800940831     C                   ENDIF
062900940831     C*
063000940901     C* SE IMMESSO CONTROLLO VALIDITA' TIPO COMUNICAZIONE AL MITTENTE
063100940901     C     VIDTCM        IFNE      ' '
063200960202RE   C                   MOVEL     *BLANKS       WCTFAX            1
063300940901     C                   MOVEL     '2F'          CODTAB
063400940901     C                   MOVEL     *BLANKS       KEYTAB
063500940901     C                   MOVEL     VIDTCM        KEYTAB
063600940901     C     KTAB          CHAIN     TABEL                              35
063700940901     C  N35TBLFLG        IFNE      ' '
063800940901     C                   SETON                                        35
063900940831     C                   ENDIF
064000940901     C* TIPO COMUNICAZIONE ERRATO
064100940901     C     *IN35         IFEQ      *ON
064200940901     C                   SETON                                        4890
064300060207     C                   GOTO      FICTR1
064400940831     C                   ENDIF
064500940831     C*
064600940901     C* DECODIFICA TIPO COMUNICAZIONE AL MITTENTE
064700960202RE   C*------------------------------------------------------*
064800960202RE   C*  Se tipo comunicazione apertura giacenza è presente  *
064900960202RE   C*  in tabella e tipo invio è tramite FAX ne verifico   *
065000960202RE   C*  l'attendibilità                                     *
065100960202RE   C                   MOVEL     TBLUNI        DS2F
065200960202RE   C                   MOVEL     §2FDES        DESTCM
065300960202RE   C     §2FFTP        IFEQ      'F'
065400960202RE   C                   MOVEL     'S'           WCTFAX            1
065500960202RE   C                   MOVE      VIDCLI        §QUATR            4
065600960202RE   C     §QUATR        IFEQ      *ALL'8'
065700960202RE   C     §QUATR        OREQ      *ALL'9'
065800960202RE   C                   SETON                                        5390
065900060207RE   C                   GOTO      FICTR1
066000960202RE   C                   ELSE
066100960202RE   C                   CLEAR                   TRUL42
066200960315RE   C                   CLEAR                   TRUL43
066300981211RE   C                   MOVEL     WTLF          D42FAX
066400960315RE   C                   MOVE      VIDCLI        D43KSC
066500960315RE   C                   CALL      'TRUL43R'
066600960315RE   C                   PARM                    TRUL43
066700960315RE   C     D43TRV        IFEQ      'S'
066800960315RE   C                   MOVEL     D43FAX        D42FAX
066900960315RE   C                   END
067000960202RE   C                   CALL      'TRUL42R'
067100960202RE   C                   PARM                    TRUL42
067200960202RE   C     D42ERR        IFEQ      '1'
067300960202RE   C                   SETON                                        5490
067400060207RE   C                   GOTO      FICTR1
067500960202RE   C                   END
067600960202RE   C                   END
067700960202RE   C                   END
067800960202RE   C*------------------------------------------------------*
067900040729      * se il tipo comunicazione è tramite MAIL
068000040729if  2c                   If        §2fFtp = 'M'
068100040729     c                   Move      VidCli        §QUATR
068200040729if  3c                   If        §quatr = *ALL'8' or §quatr = *ALL'9'
068300040729     c                   Eval      *In53 = *On
068400040729     c                   Eval      *In90 = *On
068500060207     c                   Goto      fictr1
068600040729   x3c                   else
068700040729     c                   Clear                   Trul43
068800091113      * tariffa
068900040729     c  n37              Move      VidCli        D43Ksc
069000091113      * offerta
069100040729     c                   If        *In37 and VisKsc > *Zeros
069200040729     c                   Move      VisKsc        D43Ksc
069300040729     c                   EndIf
069400040729     c                   If        *In37 and VisKsc = *Zeros
069500040729     c                   Move      VidCli        D43Ksc
069600040729     c                   EndIf
069700091113
069800040729     c                   Call      'TRUL43R'
069900040729     c                   Parm                    Trul43
070000040729if  4c                   If        D43Trv = 'S'
070100040729if  5c                   If        D43Trvm = 'N'
070200040729     c                   Eval      *In65 = *On
070300040729     c                   Eval      *In90 = *On
070400060207     c                   Goto      fictr1
070500040729e   5c                   EndIf
070600040729   x4c                   Else
070700040729      * cerco la nota 88 se manca il luogo di invio MAIL
070800091113     c                   select
070900091113      * tariffa
071000091113     c                   when      not *in37
071100091113     c                   Movel     'C'           kcApl
071200091113      * offerta da trattativa
071300101220     c                   when      *in37
071400091113     c                   Movel     'T'           kcApl
071500091113     c                   endsl
071600091113
071700040729     c                   Movel     dutkci        kcNk1
071800040729     c                   Move      VidCli        kcNk1
071900040729     c                   Eval      woknota88 = *Off
072000091113     c     kNtc88        chain     Tfntc01l
072100091113     c                   If        %Found(tfntc01l)
072200040729     c                   Eval      woknota88 = *On
072300091113     c                   EndIf
072400091113      * non ho trovato la nota e sono da offerta da trattativa su cliente
072500091113      * cerco con il cliente
072600091113     c                   if        woknota88 = *Off and *in38
072700091113     c                             and visksc > 0
072800091113     c                   eval      kcapl = 'C'
072900091113     c                   move      visksc        kcnk1
073000091113     c     kNtc88        chain     Tfntc01l
073100091113     c                   If        %Found(tfntc01l)
073200091113     c                   Eval      woknota88 = *On
073300091113     c                   EndIf
073400091113     c                   endif
073500040729     c                   If        woknota88 = *Off
073600040729     c                   Eval      *In65 = *On
073700040729     c                   Eval      *In90 = *On
073800060207     c                   Goto      fictr1
073900040729     c                   EndIf
074000040729e   4c                   EndIf
074100040729e   3c                   EndIf
074200040729e   2c                   EndIf
074300040729
074400940901     C                   ELSE
074500940901     C                   CLEAR                   DESTCM
074600940901     C                   ENDIF
074700940831     C*
074800940831     C****  TIPO COMUNICAZIONE FINE GIACENZA  ****
074900940901     C     VIDTFG        IFEQ      '?'
075000940901     C                   MOVEL     '2H'          CODTAB
075100940901     C                   MOVEL     ' '           VIDTFG
075200940901     C                   MOVEL     *BLANKS       DESTAB           25
075300940901     C                   CALL      'X§TABER'
075400940901     C                   PARM                    CODUT
075500940901     C                   PARM                    CODTAB
075600940901     C                   PARM                    KEYTAB
075700940901     C                   PARM                    DESTAB
075800940901     C                   MOVEL     KEYTAB        VIDTFG
075900940901     C                   MOVEL     DESTAB        DESTFG
076000940901     C                   SETON                                        90
076100060207     C                   GOTO      FICTR1
076200940901     C                   ENDIF
076300940901     C*
076400940901     C* SE IMMESSO CONTROLLO VALIDITA' TIPO COMUNICAZIONE FINE GIACENZA
076500940901     C     VIDTFG        IFNE      ' '
076600940901     C                   MOVEL     '2H'          CODTAB
076700940901     C                   MOVEL     *BLANKS       KEYTAB
076800940901     C                   MOVEL     VIDTFG        KEYTAB
076900940901     C     KTAB          CHAIN     TABEL                              35
077000940901     C  N35TBLFLG        IFNE      ' '
077100940901     C                   SETON                                        35
077200940901     C                   ENDIF
077300940901     C* TIPO COMUNICAZIONE ERRATO
077400940901     C     *IN35         IFEQ      *ON
077500940901     C                   SETON                                        4990
077600060207     C                   GOTO      FICTR1
077700940901     C                   ENDIF
077800940901     C*
077900940901     C* DECODIFICA TIPO COMUNICAZIONE FINE GIACENZA
078000960202RE   C*------------------------------------------------------*
078100960202RE   C*  Se tipo comunicazione chiusura giacenza è presente  *
078200960202RE   C*  in tabella e tipo invio è tramite FAX ne verifico   *
078300960202RE   C*  l'attendibilità                                     *
078400960202RE   C                   MOVEL     TBLUNI        DS2H
078500960202RE   C                   MOVEL     §2HDES        DESTFG
078600960202RE   C     §2HFAX        IFEQ      'F'
078700960202RE   C     WCTFAX        ANDNE     'S'
078800960202RE   C                   MOVE      VIDCLI        §QUATR            4
078900960202RE   C     §QUATR        IFEQ      *ALL'8'
079000960202RE   C     §QUATR        OREQ      *ALL'9'
079100960202RE   C                   SETON                                        5590
079200060207RE   C                   GOTO      FICTR1
079300960202RE   C                   ELSE
079400960202RE   C                   CLEAR                   TRUL42
079500960315RE   C                   CLEAR                   TRUL43
079600981211RE   C                   MOVEL     WTLF          D42FAX
079700960315RE   C                   MOVE      VIDCLI        D43KSC
079800960315RE   C                   CALL      'TRUL43R'
079900960315RE   C                   PARM                    TRUL43
080000960315RE   C     D43TRV        IFEQ      'S'
080100960315RE   C                   MOVEL     D43FAX        D42FAX
080200960315RE   C                   END
080300960202RE   C                   CALL      'TRUL42R'
080400960202RE   C                   PARM                    TRUL42
080500960202RE   C     D42ERR        IFEQ      '1'
080600960202RE   C                   SETON                                        5690
080700060207RE   C                   GOTO      FICTR1
080800960202RE   C                   END
080900960202RE   C                   END
081000960202RE   C                   END
081100960202RE   C*------------------------------------------------------*
081200940901     C                   ELSE
081300940901     C                   CLEAR                   DESTFG
081400940901     C                   ENDIF
081500940901     C*
081600060207     C     FICTR1        ENDSR
081700940901     C*
081800940315      *-----------------------------------------------------***********
081900940315     C* AGGANCIO IL FILE GIACENZE                              AGGANC
082000940315      *-----------------------------------------------------***********
082100940315     C     AGGANC        BEGSR
082200940315     C                   MOVEL     VIDCLI        CODCLI
082300060207     C  n37KTGC          CHAIN     TITGC000                           80
082400060207     C   37KTGC          CHAIN     TIoGC000                           80
082500940315     C*
082600940315     C* RIEMPO I CAMPI
082700940315     C*
082800980623     C     *IN80         IFEQ      *OFF
082900940315     C                   EXSR      RIEMPI
083000940315     C                   ENDIF
083100940315     C*
083200940901     C*
083300940831     C* SE IL TIPO COMUNICAZIONE AL MITTENTE E IL TIPO COMUNICAZIONE
083400940831     C*   FINE GICENZA SONO VUOTI, LI PROPONGO PRENDENDOLI DAL P.D.C.
083500060206      * se da visita prima controllo se c'è l'anagrafica provvisoria
083600060206     c                   If        *In37
083700060206     c     codcli        Chain     Tfcls01l                           35
083800060206      * Non trovo anagrafica provvisoria controllo con il cliente su
083900060206      * anagrafico P.d.c.
084000060206     c                   If        *In35 and visksc > 0
084100060206     c     visksc        Chain     Fncls01l                           35
084200060207     c                   endif
084300060206      * se è da tariffa diretta sul p.d.c.
084400060206     c                   Else
084500060206     c     codcli        Chain     fncls01l                           35
084600060206     c                   endif
084700060206
084800060206     c                   if        not *in35
084900940901     C                   MOVEA     CLSFLO        FLG
085000041029      * valorizzo  il campo di comodo
085100041029     c                   eval      anatcm = '(in anagrafica = ' + vidfl0 +
085200041029     c                             ' )'
085300041029      * valorizzo  il campo di comodo
085400041029     c                   eval      anatfg = '(in anagrafica = ' + vidfl1 +
085500041029     c                             ' )'
085600940901     C                   ENDIF
085700950121     C*
085800940901     C* DECODIFICA TIPO COMUNICAZIONE AL MITTENTE
085900940901     C                   MOVEL     '2F'          CODTAB
086000940901     C                   MOVEL     *BLANKS       KEYTAB
086100940901     C                   MOVEL     VIDTCM        KEYTAB
086200940901     C     KTAB          CHAIN     TABEL                              35
086300940901     C  N35              MOVEL     TBLUNI        DESTCM
086400940901     C*
086500940901     C* DECODIFICA TIPO COMUNICAZIONE FINE GIACENZA
086600940901     C                   MOVEL     '2H'          CODTAB
086700940901     C                   MOVEL     *BLANKS       KEYTAB
086800940901     C                   MOVEL     VIDTFG        KEYTAB
086900940901     C     KTAB          CHAIN     TABEL                              35
087000940901     C  N35              MOVEL     TBLUNI        DESTFG
087100940901     C*
087200940315     C                   ENDSR
087300900515      *-----------------------------------------------------***********
087400900515      * REGISTRAZIONE RECORD                                *  REGIS  *
087500900515      *-----------------------------------------------------***********
087600900515     C     REGIS         BEGSR
087700900515      *
087800900517     C                   MOVEL     *BLANK        TGCATB
087900900517     C                   MOVE      VIDCLI        TGCKSC
088000940831     C                   MOVE      VIDCTR        TGCCTR
088100940831     C                   MOVE      VIDPRG        TGCPRG
088200900517     C                   Z-ADD     VIDSGV        TGCSGV
088300940826     C                   Z-ADD     VIDSGR        TGCSGR
088400900517     C                   Z-ADD     VIDSGP        TGCSGP
088500900517     C                   Z-ADD     VIDSGD        TGCSGD
088600900517     C                   Z-ADD     VIDSG1        TGCSG1
088700900517     C                   Z-ADD     VIDSG2        TGCSG2
088800900517     C                   Z-ADD     VIDSG3        TGCSG3
088900900517     C                   Z-ADD     VIDST1        TGCST1
089000900517     C                   Z-ADD     VIDST2        TGCST2
089100900517     C                   Z-ADD     VIDST3        TGCST3
089200900517     C                   Z-ADD     VIDSTM        TGCSTM
089300900517     C                   Z-ADD     VIDRPV        TGCRPV
089400900517     C                   Z-ADD     VIDSGF        TGCSGF
089500911120     C                   Z-ADD     VIDGGR        TGCGGR
089600940831     C                   MOVEL     VIDFAF        TGCFAF
089700940903     C*
089800940903     C* IL TIPO COMUNICAZIONE AL MITTENTE E IL TIPO COMUNICAZIONE
089900940914     C*   FINE GIACENZA LI MEMORIZZO SOLO SE VARIANO RISPETTO A
090000940903     C*   QUELLO CHE C'E' SUL P.D.C.
090100940903     C     VIDTCM        IFNE      VIDFL0
090200940901     C                   MOVEL     VIDTCM        TGCTCM
090300950106     C                   ELSE
090400950106     C                   CLEAR                   TGCTCM
090500940903     C                   ENDIF
090600950106     C*
090700940903     C     VIDTFG        IFNE      VIDFL1
090800940831     C                   MOVEL     VIDTFG        TGCTFG
090900950106     C                   ELSE
091000950106     C                   CLEAR                   TGCTFG
091100940903     C                   ENDIF
091200940903     C*
091300990615     C* SE VIENE VARIATO TITGC ABBLENCO FLAG E DATA TRASMISSIONE
091400950116    1C     COMTGC        IFNE      TGCDS
091500940826     C                   MOVE      ' '           TGCFTR
091600940826     C                   Z-ADD     0             TGCDTR
091700950116    1C                   ENDIF
091800900516      *
091900990615     C* VALORIZZO ULTIMA DATA VARIAZIONE
092000990615     C                   Z-ADD     DATEU8        TGCDUV
092100040916     C* VALORIZZO DATA trasmissione
092200040916     C                   Z-ADD     DATEU8        TGCDTR
092300990615      *
092400060207     c                   if        not *in80
092500060207     C  n37              UPDATE    TITGC000
092600060207     C   37              UPDATE    TIogC000
092700060207     c                   endif
092800030714      * VALORIZZO IL FLAG PER LA SCRITTURA DEL RECORD STORICO
092900030717      * se non si tratta di annullamento e non si tratta di offerta
093000030717     c                   if        not *in16 and not *in80 and not *in37
093100030716     C                   EVAL      PARMAN = 'S'
093200030716     c                   endif
093300030714      *
093400060207     c                   If        *in80
093500060207     C  n37              WRITE     TITGC000
093600060207     C   37              WRITE     TIoGC000
093700060207     c                   endif
093800950116     C*
093900900515     C                   ENDSR
094000900515      *-----------------------------------------------------***********
094100900515      * COPIA CAMPI RECORD IN CAMPI FILE VIDEO              *  RIEMPI *
094200900515      *-----------------------------------------------------***********
094300900515     C     RIEMPI        BEGSR
094400990908     C*
094500990908     C* GIRO DATA ULTIMA VARIAZIONE
094600990908     C*
094700990908     C                   Z-ADD     TGCDUV        G02INV
094800990908     C                   MOVEL     '3'           G02ERR
094900990908     C                   CALL      'XSRDA8'
095000990908     C                   PARM                    WLBDAT
095100990908     C                   Z-ADD     G02DAT        VIDDUV
095200950116     C*
095300950116     C                   MOVEL     TGCDS         COMTGC
095400950116     C*
095500900517     C                   Z-ADD     TGCSGV        VIDSGV
095600940826     C                   Z-ADD     TGCSGR        VIDSGR
095700900517     C                   Z-ADD     TGCSGP        VIDSGP
095800900517     C                   Z-ADD     TGCSGD        VIDSGD
095900900517     C                   Z-ADD     TGCSG1        VIDSG1
096000900517     C                   Z-ADD     TGCSG2        VIDSG2
096100900517     C                   Z-ADD     TGCSG3        VIDSG3
096200900517     C                   Z-ADD     TGCST1        VIDST1
096300900517     C                   Z-ADD     TGCST2        VIDST2
096400900517     C                   Z-ADD     TGCST3        VIDST3
096500900517     C                   Z-ADD     TGCSTM        VIDSTM
096600900517     C                   Z-ADD     TGCRPV        VIDRPV
096700110916     C                   Z-ADD     TGCRPV        SAVrpv
096800900517     C                   Z-ADD     TGCSGF        VIDSGF
096900911120     C                   Z-ADD     TGCGGR        VIDGGR
097000940831     C                   MOVEL     TGCFAF        VIDFAF
097100940831     C                   MOVEL     TGCTCM        VIDTCM
097200940831     C                   MOVEL     TGCTFG        VIDTFG
097300900517      *
097400950105     C     TGCATB        IFNE      ' '
097500980623     C                   SETON                                        50  99
097600900517     C                   ELSE
097700900517     C                   SETOFF                                       50  99
097800900517     C                   END
097900900515      *
098000900517     C                   ENDSR
098100030716     C*---------------------------------------------------------------*
098200030716     C* ROUTINE DI SCRITTURA FILE STORICO TITAV00F
098300030716     C*---------------------------------------------------------------*
098400030716     C     SUB_STORICO   BEGSR
098500030716      *la prima volta inizializzo i campi del file che vanno sempre bene
098600030716     c                   if        first = ' '
098700030716     C                   eval      TAVksc= tgcksc
098800030716     C                   eval      TAVctr= tgcctr
098900030716     C                   eval      TAVprg= tgcprg
099000030716      * data variazione
099100030716     C                   eval      TAVdav= dateu8
099200030716     C                   eval      TAVnoj= job_name
099300030716     C                   eval      TAVpru= user
099400030716     C                   eval      first = 'N'
099500030716     c                   endif
099600030716      * recupero ora di variazione
099700030716     c                   time                    w0140
099800030716     c                   movel     w0140         TAVorv
099900030716     c                   eval      TAVtrd= 'GIA'
100000030716     c                   clear                   TAVftc
100100041005     c                   z-add     dateu8        TAVdtr
100200030716      *
100300030716     c                   write     titav000
100400030716     C*
100500030716     C                   ENDSR
100600030716     C***
100700910617**
100800910617 ** GESTIONE TARIFFE GIACENZA **
100900910617 ** GESTIONE OFFERTE GIACENZA **
