000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960412     H* FNLR33R *----------------------------------------------------*
000300960412     H*        - CHIUSURA ANOMALIA                                   *
000400000000     H*--------------------------------------------------------------*
000500990511     FFNANM00F  UF   E             DISK
000600990511     FFNANM02L  IF   E           K DISK
000700990511     F                                     RENAME(FNANM000:FNANM02)
000800990511     F                                     INFDS(FNANM2)
000900990511     FFNANM04L  UF   E             DISK
001000990511     F                                     RENAME(FNANM000:FNANM04)
001100960412     FTABEL00F  IF   E           K DISK
001200960412     D DSLR33        E DS                  EXTNAME(FNLR33DS)
001300960412     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400990511     D DSANM         E DS                  EXTNAME(FNANM00F)
001500961025     D CNCR80        E DS
001600960412     D DS7C          E DS
001700961025     D* DS PER TRUL06R - CARICAMENTO £X
001800040119     D*DSUL06    E DSTRUL06DS
001900040119     D*                                        1  900LIN
002000990511     D FNANM2          DS
002100970603     D  ANMNR2               397    400B 0
002200971128     D WLBDAT          DS                  INZ
002300971128     D  G02DAT                 1      8  0
002400971128     D  G02INV                 9     16  0
002500971128     D  G02ERR                17     17
002600971128     D  G02TGI                18     22  0
002700921124     I*---------------------------------------------------------------*
002800921124     I*   RIEPILOGO INDICATORI
002900921124     I*---------------------------------------------------------------*
003000960412     I* 30    - COMODO
003100960412     I* 31    - COMODO
003200040116     I* 36    -
003300921124     I*---------------------------------------------------------------*
003400900622     C     *ENTRY        PLIST
003500960412     C                   PARM                    DSLR33
003600960412     C*
003700960412    1C     D33TLA        IFNE      'C'
003800960412     C*
003900990511     C     D33NRR        CHAIN     FNANM00F                           30
004000960412    2C     *IN30         IFEQ      *OFF
004100960719     C*
004200960719     C* A cambio causale leggo tabella 7C
004300960719    3C     ANMCAA        IFNE      SAVCAA
004400960719     C                   MOVE      ANMCAA        SAVCAA
004500960719     C                   MOVEL     '7C'          KCOD
004600960719     C                   MOVEL(P)  ANMCAA        KKEY
004700960719     C     KTAB          CHAIN     TABEL00F                           31
004800960719     C  N31              MOVEL     TBLUNI        DS7C
004900960719     C   31              CLEAR                   DS7C
005000960719    3C                   END
005100961106     C*
005200961106     C* Se per questa causale non è ammessa la chiusura con data < dao
005300001110     C* (§7CCHS <> 'S') e data chiusura < dao --> NON FACCIO NIENTE
005400961106     C     §7CCHS        IFNE      'S'
005500990511     C     D33DCH        ANDLT     ANMDAO
005600990511     C                   UNLOCK    FNANM00F
005700961106     C                   GOTO      FINE
005800961106     C                   END
005900001110     C* Se anomalia non chiudibile da categoria spunta IMI e fase chius
006000001110     C* ura ="I"(da spunta Imi) ----------------> NON FACCIO NIENTE
006100110929     C**   §7CCCG        IFNE      'S'
006200110929     C**   D33FSC        ANDEQ     'I'
006300110929     C**                 UNLOCK    FNANM00F
006400110929     C**                 GOTO      FINE
006500110929     C**                 END
006600960510     C**
006700961118     C***** TRATTAMENTO ANOMALIE DISCORDANZE EXPORT (ANMAIE = 'X')
006800960510     C**
006900961118     C** CANCELLO SEMPRE L'ANOMALIA
007000110929   2AC     ANMAIE        ifeq      'X'
007100990511     C                   DELETE    FNANM000
007200961118     C**
007300110929     C***** TRATTAMENTO altre anomalie "I"  e "E"
007400110929   2AC**** ANMAIE        WHENNE    'R'
007500110929     c
007600110929     c                   else
007700110929     c
007800960510     C* ANOMALIA AUTOCHIUSA:
007900960510     C* Se interna non faccio niente, se esterna e flag modifica ester-
008000960514     C* ne autochiuse = 'S' --> deleto se non trasmessa
008100970217     C* Deleto anche se trattasi di anomalia 300-ritiri non effettuati
008200970217     C* non trasmessa
008300960416   3AC     §7CCHA        IFEQ      'S'
008400960510     C     ANMAIE        IFNE      'I'
008500960510     C     D33MAC        ANDEQ     'S'
008600960510     C     ANMFT1        ANDEQ     *BLANKS
008700960416     C     ANMFT2        ANDEQ     *BLANKS
008800970217     C     ANMCAA        OREQ      300
008900970217     C     ANMFT1        ANDEQ     *BLANKS
009000970217     C     ANMFT2        ANDEQ     *BLANKS
009100990511     C                   DELETE    FNANM000
009200960416     C                   ELSE
009300990511     C                   UNLOCK    FNANM00F
009400960416     C                   END
009500110929     c
009600960416  X3AC                   ELSE
009700960510     C* ANOMALIA NON AUTOCHIUSA:
009800960412     C* Elaboro solo se e' aperta
009900960412    3C     ANMDCH        IFEQ      0
010000960412     C* o e' chiusa con pratica IDD
010100960412     C     ANMDCH        ORGT      0
010200960412     C     ANMCCH        ANDEQ     'PR'
010300980216     C     ANMDCH        ORGT      0
010400980216     C     ANMCCH        ANDEQ     'AN'
010500960412     C**
010600960510     C* ANOMALIA ESTERNA
010700960412    4C     ANMAIE        IFEQ      'E'
010800110929     c
010900990324     C* Se causale consegna anomala=5 (IDD3) --> non faccio niente
011000990324     C     D33CCA        IFEQ      '5'
011100990511     C                   UNLOCK    FNANM00F
011200990324     C                   GOTO      FINE
011300990324     C                   END
011400990324     C*
011500970603     C* Eseguo controlli particolari per l'anomalia 5-collo partito
011600970603     C* non arrivato
011700970603     C                   EXSR      ANM005
011800960412     C*
011900960514     C* Se già trasmessa o chiusura manuale, chiudo e ritrasmetto
012000970603     C* chiudo e ritrasmetto anche se wcan5='N' (per anom. 5 a liv.0)
012100960412    5C     ANMFT1        IFNE      *BLANKS
012200960412     C     ANMFT2        ORNE      *BLANKS
012300960412     C     ANMLID        ORGT      §7CLID
012400960514     C     D33FSC        OREQ      'M'
012500970603     C     WCAN5         OREQ      'N'
012600050308     C* 8/3/05-SPECIFICA DA AGGIUNGEREPER CHI FA L'IDD IL GIORNO
012700050308     C*        DOPO,ALTRIMENTI L'ANOMALIA SI CANCELLA PRIMA DI FARE IDD
012800050308     C     ANMDAO        ORNE      D33DCH
012900990412     C*
013000990412     C* LA DATA LA IMPOSTO SOLO SE LA CAUSALE CHIUSURA <> DA 'PR'
013100990412     C     ANMCCH        IFNE      'PR'
013200990511     C                   Z-ADD     D33DCH        ANMDCH
013300990412     C                   ENDIF
013400990412     C*
013500960412     C                   MOVEL     D33FSC        ANMFSC
013600960412     C                   MOVEL     D33CCH        ANMCCH
013700960419     C     D33FTR        IFEQ      *BLANKS
013800960412     C                   MOVEL     *BLANKS       ANMFT1
013900960412     C                   MOVEL     *BLANKS       ANMFT2
014000041125     C                   ELSE
014100041125     C* SE ANNULLAMENTO, E NON DEVO FLAGGARE, FLAGGO IL RECORD
014200041125     C* SU AS UNICO NON DEVO PIÙ RITRASMETTERLO
014300041125     C     D33CCH        IFEQ      'AN'
014400041125     C                   MOVEL     DATEU         ANMDTR
014500041125     C                   MOVEL     'T'           ANMFT1
014600041125     C     ANMFL2        IFGT      0
014700041125     C                   MOVEL     'T'           ANMFT2
014800960419     C                   END
014900041125     C                   END
015000041125     C                   END
015100041125     C*
015200990511     C                   UPDATE    FNANM000
015300960412     C*
015400960412   X5C                   ELSE
015500960412     C* Se non trasmessa deleto
015600990511     C                   DELETE    FNANM000
015700960412    5C                   END
015800960510     C* ANOMALIA INTERNA
015900960412   X4C                   ELSE
016000111109     c
016100111230     c* Se c'e' consegna anomala e l'anomalia è la 115 -->
016200111230     c*  non la devo deletare mai ma chiuderla come annullata
016300111230     c**                 if        anmcaa=115 and d33cca<>' '
016400111230     C**                 GOTO      FINE
016500111230     C**                 END
016600961122     C*
016700990511    5C     ANMDAO        IFEQ      D33DCH
016800960514     C     D33FSC        ANDNE     'M'
016900110929     c     §7cktr        andne     'S'
017000990511     C                   DELETE    FNANM000
017100960415   X5C                   ELSE
017200990511     C                   Z-ADD     D33DCH        ANMDCH
017300960412     C                   MOVEL     D33FSC        ANMFSC
017400960412     C                   MOVEL     D33CCH        ANMCCH
017500111230     c
017600111230     c* se anomalia 115 forzo l'annullamento
017700111230     c                   if        anmcaa=115 and d33cca<>' '
017800111230     C                   MOVEL     'M'           ANMFSC
017900111230     C                   MOVEL     'AN'          ANMCCH
018000111230     c                   endif
018100961122     C**
018200000811     C* LE ANOMALIE 145 E 146 DEVONO ESSERE FLAGGATE CON 'T'
018300000811     C* PER EVITARNE LA TRASMISSIONE
018400961122     C     ANMCAA        IFEQ      145
018500961122     C     ANMCAA        OREQ      146
018600961122     C                   MOVEL     'T'           ANMFT1
018700961122     C                   ENDIF
018800110929     c*
018900110929     C* le anomalie inviabili ai clienti le devo sflaggare
019000110929     c                   if        §7cktr='S'
019100110929     c                   clear                   anmft1
019200110929     c                   clear                   anmft2
019300110929     c                   endif
019400110929     c
019500990511     C                   UPDATE    FNANM000
019600110929     c
019700960415    5C                   END
019800960412    4C                   END
019900110929     c
020000960429   X3C                   ELSE
020100990511     C                   UNLOCK    FNANM00F
020200960412    3C                   END
020300960416   3AC                   END
020400110929   2AC                   endif
020500110929     c
020600960412    2C                   END
020700960412    1C                   END
020800960424     C*
020900960424     C     FINE          TAG
021000911015     C*
021100960412    1C     D33TLA        IFEQ      ' '
021200960412     C                   RETURN
021300971128     C*
021400960412   X1C                   ELSE
021500971128     C*
021600000000     C                   SETON                                        LR
021700960412    1C                   END
021800970603     C**************************************************************************
021900970603     C* VERIFICHE PARTICOLARI PER ANOMALIA 5
022000970603     C**************************************************************************
022100970603     C     ANM005        BEGSR
022200970603     C                   CLEAR                   WCAN5
022300970603     C* SE ANOMALIA 5 A LIVELLO 0 VERIFICO SE CE N'è UN'ALTRA
022400970603    1C     ANMCAA        IFEQ      5
022500970603     C     ANMLID        ANDEQ     0
022600990511     C* Salvo record di fnanm00f per non sporcare i campi nel cercare
022700970603     C* l'esistenza di un'altra anomalia 5
022800970603     C                   MOVEL     DSANM         DSANMX
022900990511     C     KANM2         SETLL     FNANM02L
023000990511     C     KANM2         READE     FNANM02L                               30
023100970603    2C     *IN30         DOWEQ     *OFF
023200970603     C     ANMNR2        ANDEQ     D33NRR
023300970603     C* Scarto record = a quello che sto elaborando e do per scontato
023400970603     C* che posso avere per lo stesso segnacollo al massimo 2 anomalie
023500970603     C* 005
023600990511     C     KANM2         READE     FNANM02L                               30
023700970603    2C                   ENDDO
023800970603     C* 30 OFF --> ESISTE UN'ALTRA ANOMALIA 5 PER IL SEGNACOLLO
023900970603    2C     *IN30         IFEQ      *OFF
024000970603     C     ANMLID        ANDGT     0
024100990511     C     ANMNR2        CHAIN     FNANM04L                           30
024200970603     C                   CLEAR                   WAGG5
024300970603    3C                   SELECT
024400970603     C     ANMFL1        WHENEQ    *ZEROS
024500970603     C                   MOVE      '1'           WAGG5             1
024600970603     C     D33FTR        IFEQ      *BLANKS
024700970603     C                   CLEAR                   ANMFT1
024800970603     C                   END
024900970603    4C     ANMLNP        IFGT      0
025000970603     C                   MOVEL     ANMLNP        ANMFL1
025100970603   X4C                   ELSE
025200970603     C                   MOVEL     ANMFLS        ANMFL1
025300970603    4C                   ENDIF
025400970603     C     ANMFL2        WHENEQ    *ZEROS
025500970603     C                   MOVE      '1'           WAGG5             1
025600970603     C     D33FTR        IFEQ      *BLANKS
025700970603     C                   CLEAR                   ANMFT2
025800970603     C                   END
025900970603    4C     ANMLNP        IFGT      0
026000970603     C                   MOVEL     ANMLNP        ANMFL2
026100970603   X4C                   ELSE
026200970603     C                   MOVEL     ANMFLS        ANMFL2
026300970603    4C                   ENDIF
026400970603    3C                   ENDSL
026500970603     C* SE HO IMPOSTATO FL1 O FL2 AGGIORNO LA SECONDA ANOMALIA
026600970603     C* (LASCIO INVECE WCAN5 = BLANKS PER PERMETTERE LA CANCELLAZIONE
026700970603     C* DELLA PRIMA ANOMALIA)
026800970603    3C     WAGG5         IFEQ      '1'
026900990511     C                   UPDATE    FNANM04
027000970603   X3C                   ELSE
027100970603     C* ALTRIMENTI RILASCIO IL RECORD E IMPOSTO WCAN5='N' PER IMPEDIRE
027200970603     C* LA CANCELLAZIONE DELLA PRIMA ANOMALIA NONOSTANTE SIA A LIV.0
027300970603     C* (LO FACCIO PER POTER TRASMETTERE LA CHIUSURA ALLA PARTENZA)
027400990511     C                   UNLOCK    FNANM04L
027500970603     C                   MOVE      'N'           WCAN5             1
027600970603    3C                   END
027700970603    2C                   END
027800970603     C                   MOVEL     DSANMX        DSANM
027900970603    1C                   END
028000970603     C                   ENDSR
028100960412     C****************  OPERAZIONI INIZIALI   *************************
028200960412     C     *INZSR        BEGSR
028300960412     C*
028400960412     C                   Z-ADD     1             CODUT
028500960412     C                   CALL      'X§PARUT'
028600960412     C                   PARM                    UT§DSE
028700961025     C                   MOVEL     REC80         CNCR80
028800960412     C*
028900960412     C     *LIKE         DEFINE    ANMCAA        SAVCAA
029000960412     C     *LIKE         DEFINE    TBLCOD        KCOD
029100960412     C     *LIKE         DEFINE    TBLKEY        KKEY
029200970603     C     *LIKE         DEFINE    DSANM         DSANMX
029300960412     C*
029400960412     C                   CLEAR                   SAVCAA
029500960412     C*
029600960412     C     KTAB          KLIST
029700960412     C                   KFLD                    CODUT
029800960412     C                   KFLD                    KCOD
029900960412     C                   KFLD                    KKEY
030000970603     C     KANM2         KLIST
030100970603     C                   KFLD                    ANMFLS
030200970603     C                   KFLD                    ANMLNA
030300970603     C                   KFLD                    ANMNRS
030400970603     C                   KFLD                    ANMSCN
030500970603     C                   KFLD                    ANMCAA
030600040116     C*
030700971128     C*
030800971128     C* DETERMINO DATA DEL GIORNO
030900971128     C                   TIME                    W0140            14 0
031000971128     C                   MOVE      W0140         WDTGIO            8 0
031100971128     C                   Z-ADD     WDTGIO        G02DAT
031200971128     C                   MOVEL     *BLANK        G02ERR
031300971128     C                   CALL      'XSRDA8'
031400971128     C                   PARM                    WLBDAT
031500971128     C                   MOVEL     G02INV        DATEU             8 0
031600960412     C*
031700960412     C                   ENDSR
