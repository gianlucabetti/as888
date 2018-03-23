000100050510     H DECEDIT('0,') DATEDIT(*yMd.)
000200970925     H* FNLR27R *-----------------------------------------------------*
000300930505     H*         - ABILITAZIONE BOLLE ARRIVI ALLA CONSEGNA -           *
000400930505     H*---------------------------------------------------------------*
000500930505     H* INDICATORI USATI                                              *
000600930505     H*---------------------------------------------------------------*
000700950405     H* 01 - MANCA DATA ARRIVO: NON ABILITARE IN BASE ALL'ARRIVO
000800930505     H* 02 - SPUNTA DI FOGLIO ABBINATO
000900950404     H* 05 - SE ON NON E' RIUSCITA L'ALLOCAZIONE DEI FILES DI TRASMISS.
001000960906     H* 08 - OFF => TIPO BOLLA DEVE PERVENIRE IN SEDE
001100930505     H* 10 - ERRORE IN LETTURA FILE BOLLE
001200930505     H* 15 - DEVO ABILITARE MERCE ALLA CONSEGNA
001300940613     H* 17 - INDICA SE COLLO DI FOGLIO ABBINATO(17 ON) OPPURE NO(17OFF)
001400941031     H* 20 - DEVO CALCOLARE IL VOLUME CML
001500990920     H* 21 - DEVO CALCOLARE IL PESO   CML
001600941031     H* 30 - LETTURA DETTAGLIO BOLLA
001700930505     H* 31 - LETTURA BOLLA
001800930505     H* 33 - RICERCA FILIALI IN GESTIONE
001900930517     H* 34 - NUMERO FOGLIO VIAGGIO NON PRESENTE NELLA SCHIERA
002000070123     H* 35 - LETTURA DI FNBRV09L
002100960304     H* 38 - COMOD0
002200930505     F*---------------------------------------------------------------*
002300930505     F* DATA BASE                                                     *
002400930505     F*---------------------------------------------------------------*
002500960304     FTABEL00F  IF   E           K DISK
002600001010     FAZORG01L  IF   E           K DISK
002700960304     FFNARB03L  UF   E           K DISK
002800950404     FFNARB01L  UF   E           K DISK
002900950404     F                                     RENAME(FNARB000:FNARB1)
003000991116     FFIAR601L  IF   E           K DISK
003100060216     FFiar401L  IF   E           K DISK
003200941031     FFNART01L  IF   E           K DISK
003300941031     FFNFVA02L  IF   E           K DISK
003400941102     F                                     RENAME(FNFVA000:FNFVA02)
003500020917     FFNFVV02L  IF   E           K DISK
003600050510     FFNFVV08L  IF   E           K DISK
003700050510     F                                     RENAME(FNFVv000:FNFVv08)
003800070123     FFNBRV09L  IF   E           K DISK
003900041222     FFnanm02l  IF   E           K DISK
004000930430     D*---------------------------------------------------------------*
004100930430     D* SCHIERE                                                       *
004200930430     D*---------------------------------------------------------------*
004300930517     D* FOGLI VIAGGIO DELLA BOLLA
004400930517     D NFV             S              5  0 DIM(100)
004500930517     D* SEGNACOLLI DEL FOGLI VIAGGIO
004600930517     D NSC             S              7  0 DIM(100)
004700050304     D* data uscita transito della bolla
004800050304     D DUT             S              8  0 DIM(100)
004900950403     D*
005000971120     D COM             S             80    DIM(4) CTDATA PERRCD(1)              COMANDI CL
005100950403     D CMD             S              1    DIM(80)                              COMANDO QCMDEXC
005200930430     D*---------------------------------------------------------------*
005300911030     D KPJBA         E DS
005400930430     D*------------------
005500930430     D* DS RICERCA RAG. SOCIALE
005600930430     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005700040513     D CNCR80        E DS
005800941031     D*------------------
005900941031     D* DS PER FNLV20R - DS DEI CAMPI PER CALCOLO VOLUME DA FATTURARE
006000941031     D DSLV20        E DS                  EXTNAME(FNLV20DS)
006100990806     D DSLV39        E DS                  EXTNAME(FNLV39DS)
006200970730     D DSLV53        E DS                  EXTNAME(FNLV53DS)
006300970924     D DSLV57        E DS                  EXTNAME(FNLV57DS)
006400020927     d* membro per FNARBC0T
006500950403     D                 DS
006600950403     D  EMME                   1      1
006700020927     D  xSIMFE                 2      4
006800020927     D  $SIMFE                 5      7  0
006900950403     D  $MBR                   1     10
007000050913     D                 DS
007100050913     D  arbaas                 1      4  0
007200050913     D  arbmgs                 5      8  0
007300050913     D  arbdsp                 1      8  0
007400020927     d* membro per FIARBC46
007500020927     d blank           s             10
007600001010     D OG143         E DS
007700041216     D OG150         E DS
007800001010     D DS3A          E DS
007900041217     D DSbl4a        E DS
008000971120     D WLBDAT          DS                  INZ
008100971120     D  G02DAT                 1      8  0
008200971120     D  G02INV                 9     16  0
008300971120     D  G02ERR                17     17
008400971120     D  G02TGI                18     22  0
008500040505     d* Passaggio dati a fnlsa3r
008600040505     D PARAM3          DS
008700040505     D  PA3NFV                 1      5
008800070502     D  PA3tfp                 6      8
008900160616     d
009000160616     D PARAM7a         DS
009100160616     D  nmi7a                  1      7
009200160616     D  fgs7a                  8     10
009300160616     d
009400160616     D PARAM7b         DS
009500160616     D  pa7nsped               1     16
009600160616     D  pa7rich               17     17
009700160616     D  pa7err                18     18
009800020917
009900040505     d minuti          s              5  0
010000040505     d Kfgs            s                   Like(FvvFgs)
010100050510     d Kdam            s                   Like(savdam)
010200050510     d SAVFL2          s                   Like(artfl2)
010300050510     d*TfaOgas         s                   Like(§ogas)
010400050510     d*LnaOgas         s                   Like(§ogas)
010500041217     d ktrc            s                   Like(ar4trc) inz('A')
010600041222     d kCAA            s                   Like(anmcaa) inz(010)
010700020918     d Wgiro           s              1    Inz('0')
010800100915     d soloarbc0t      s              1    Inz(' ')
010900040505     d worap           s               z
011000040505     d worad           s               z
011100050510     d Dataiso         s               d   datfmt(*iso)
011200020917
011300930430     C*---------------------------------------------------------------*
011400930430     C*                    MAIN                                       *
011500930430     C* PROGRAMMA DI ABILITAZIONE BOLLE ARRIVI ALLA CONSEGNA          *
011600930430     C*---------------------------------------------------------------*
011700040401     C     *ENTRY        PLIST
011800040401     C                   PARM                    KPJBA
011900040401     C                   PARM                    chiudi            1
012000111109     C                   PARM                    TrasmiVP          1
012100040513     c
012200040513     c                   if        knsif=*blanks
012300040513     c                   eval      knsif='*LIBL'
012400040513     c                   endif
012500040505     c* Per pgm gufo, ogni 2 ore chiudo pgm trasmetto variazioni peso volume
012600040505     c*  e riapro il pgm
012700040505     c
012800930430     C*
012900040401     C     *loval        setll     FNARB000                             1031
013000040401     C                   READ      FNARB000                             1031
013100941031     C*
013200941031DO  1C  N10*IN31         DOWEQ     *OFF
013300930503     C*
013400990806     C                   SETOFF                                       152021
013500950404     C                   MOVE      *BLANKS       W001A
013600041222     C                   MOVE      *BLANKS       Wctrarbnfv        1
013700001010     C* VERIFICO SE SI TRATTA DI UNA BOLLA POSTE
013800001010     C     ARBLNP        IFNE      SAVLNP
013900001010     C     ARBLNP        CHAIN     AZORG01L                           38
014000001010     C  N38              MOVEL     ORGDE3        OG143
014100001010     C   38              CLEAR                   OG143
014200001010     C                   MOVEL     ARBLNP        SAVLNP
014300001010     C                   ENDIF
014400041216     C* VERIFICO SE LA LINEA DI ARRIVO FA IL PIKING
014500041216     C     ARBLNA        IFNE      SAVLNA
014600041216     C     ARBLNA        CHAIN     AZORG01L                           38
014700041216     C  N38              MOVEL     ORGDF0        OG150
014800041216     C   38              CLEAR                   OG150
014900041217     C                   MOVEL     ARBLNa        SAVLNA
015000041216     C                   ENDIF
015100041216     C
015200930517     C*
015300921231     C* 15 ON - DEVO ABILITARE LA MERCE ALLA CONSEGNA
015400930603     C     ARBFBS        IFEQ      'S'
015500970924     C     ARBFBS        OREQ      'E'
015600930603     C                   SETON                                        15
015700930603     C* PULISCO DATA E ORA DI COMODO
015800941102     C                   Z-ADD     *ZEROS        SAVAED
015900941102     C                   Z-ADD     *ZEROS        SAVAEH
016000930607     C                   Z-ADD     *ZEROS        SAVDAM
016100050510     c                   clear                   savfl2
016200940613     C                   CLEAR                   NFV
016300940613     C                   CLEAR                   NSC
016400050304     C                   CLEAR                   dut
016500930607     C                   Z-ADD     1             X
016600950629     C                   ENDIF
016700921231     C*
016800921231     C* 20 ON - DEVO CALCOLARE IL VOLUME CML SU BOLLA
016900970924IF  2C****       ARBNCR    IFEQ 99999
017000970924     C****       ARBNCR    OREQ 99998
017100970924     C     ARBFBS        IFEQ      'V'
017200970924     C     ARBFBS        OREQ      'E'
017300930504     C                   SETON                                            20
017400941102     C                   Z-ADD     *ZEROS        TOTVUC            8 6
017500950323     C* SALVO VOLUME DA FATTURARE, VOLUME CML E NUMERO COLLI RILEVATI
017600950323     C                   Z-ADD     ARBVLF        SAVVLF
017700950323     C                   Z-ADD     ARBVLC        SAVVLC
017800950323     C                   Z-ADD     ARBNCR        SAVNCR
017900070704     c                   movel     arbfvf        SAVFVF
018000950323     C*
018100941102     C                   CLEAR                   DSLV20
018200930517E   2C                   ENDIF
018300990806     C* 21 ON - DEVO CALCOLARE IL PESO CML SU BOLLA
018400990806     C     ARBFBS        IFEQ      'P'
018500990806     C     ARBFBS        OREQ      'E'
018600990806     C                   SETON                                            21
018700990806     C                   Z-ADD     *ZEROS        TOTPUC            9 3
018800990806     C* SALVO VOLUME DA FATTURARE, VOLUME CML E NUMERO COLLI RILEVATI
018900990806     C                   Z-ADD     ARBPKF        SAVPKF
019000990806     C                   Z-ADD     ARBPKC        SAVPKC
019100990806     C                   Z-ADD     ARBNCP        SAVNCP
019200990806     C*
019300990806     C                   CLEAR                   DSLV39
019400990806E   2C                   ENDIF
019500911011     C*
019600930603     C* ELABORO SE:
019700930603     C* 1) DEVO ABILITARE BOLLA ALLA CONSEGNA E LIVELLO ABILITAZIONE <2
019800930603     C* 2)  "       "       "     "     "     "   "         "       >"2
019900930603     C*     MA NON HO ANCORA CALCOLATO LA DATA E L'ORA DI ARRIVO EFFETT
020000930603     C* 3) DEVO CALCOLARE IL VOLUME CML
020100990920     C* 4) DEVO CALCOLARE IL PESO   CML
020200930603     C*
020300950629IF  2C     *IN15         IFEQ      *ON
020400950629     C     ARBAMA        ANDNE     2
020500001010     C* LA DATA ARRIVO ULTIMO COLLO NON LA CALCOLO PER BOLLE POSTE
020600950629     C     *IN15         OREQ      *ON
020700930603     C     ARBDTI        ANDEQ     0
020800020918     c     §OgNtw        andne     'PPT'
020900001010     C* VOLUME O PESO
021000950629     C     *IN20         OREQ      *ON
021100990920     C     *IN21         OREQ      *ON
021200940613     C                   SETOFF                                       0102
021300940613     C                   SETON                                        17
021400950619     C*  CONTROLLO SE IL FOGLIO SU CUI E' PARTITA LA BOLLA E' ABBINATO
021500950619     C*  PERCHE' COME DATA ARRIVO ULTIMO COLLO DEVE METTERE LA > TRA
021600041217     C*  QUELLA DEI COLLI  E QUELLA DELLA BOLLA solo per DDT SI senza
021700041217     c*  piking
021800041217     C                   CLEAR                   BOLAED
021900041217     C                   CLEAR                   BOLAEH
022000950619    3C     *IN15         IFEQ      *ON
022100041217    4c     §OgNtw        Ifne      'PPT'
022200041216     C* CONSIDERO L'ABBINAMENTO DEL FOGLIO DI BORDERIZZAZIONE SOLO PER
022300041216     C* DDT  SI  E CON LNA CHE NON FA IL PICKING
022400041222     c* considero anche se si trata di manca record in arrivo
022500041217    5c                   if        §ogpck=' '
022600060216     c     kar4a         chain     fiar401l
022700060216    6c                   if         %found(fiar401l)
022800041217     c                   movel     ar4not        dsbl4a
022900041217     c                   else
023000041217     c                   clear                   dsbl4a
023100041217    6c                   endif
023200041217     c
023300120113    6c                   if        §b4abm='S' or §b4abm='C' or
023400120113    6c                             §b4abm='P' or §b4abm='Q'
023500041222     c                   EXSR      ctrarbnfv
023600041217    6C                   ENDIF
023700041217    5C                   ENDIF
023800001010     C* BOLLA POSTE: NON CALCOLO DATA ARRIVO ULTIMO COLLO
023900041217  X 4C                   ELSE
024000001010     C                   SETOFF                                       17
024100041217    4C                   ENDIF
024200001010    3C                   ENDIF
024300911011     C*
024400990921     C                   CLEAR                   WFVC
024500990921     C                   CLEAR                   WFPC
024600041222     c
024700930518     C* LEGGO I COLLI DELLA BOLLA
024800941031     C     KARB          SETLL     FNART000
024900941031     C     KARB          READE     FNART000                               30
025000041222     c                   clear                   wesiste010        1
025100911011     C*
025200930504DO  3C     *IN30         DOWEQ     '0'
025300041222     c* Memorizzo se presente un manca record: in questo caso devo
025400041222     c*  considerare l'abbinamento del foglio della bolla
025500041222     c                   if        wctrarbnfv=' '
025600041222     c     kanm          setll     fnanm02l                               34
025700050308     c                   if        *in34  or artdam<=arbdbr  and
025800050308     c                             artdam>0
025900041222     c                   exsr      CTRARBNFV
026000041222     c                   endif
026100041222     c                   endif
026200041222     c
026300921231     C*
026400930603    4C     *IN15         IFEQ      '1'
026500930603     C***
026600930603     C*** A B I L I T A Z I O N E   B O L L E   A L L A   C O N S E G.
026700930603     C***
026800940208     C*
026900940613     C* ROUTINE CHE CONTROLLA COLLO PER COLLO SE SONO PARTITI SU FV
027000980114     C*  ABBINATI: SE NE TROVO ALMENO 1 NON ABBINATO NON  POSSO
027100980114     C*  CALCOLARE LA DATA E ORA ARRIVO EFFETTIVO ULTIMO COLLO (17 OFF)
027200940613     C   17              EXSR      RIC2
027300940208     C*
027400950629IF  5C     ARBAMA        IFNE      2
027500921123     C*
027600980114     C* PER L'ABILITAZIONE, SE C'E LA DATA ARRIVO E' COME SE :
027700921123     C* 1) FOSSE PARTITO IL COLLO
027800921123     C* 2) FOSSE ABBINATO IL RELATIVO FOGLIO VIAGGIO
027900921123     C*
028000930914     C* MANCA DATA ARRIVO O COLLO NON ABILITATO:
028100930914     C*                    NON ABILITO IN BASE ALL'ARRIVO
028200930603IF  6C     ARTDAM        IFEQ      0
028300941221     C     ARTDAB        OREQ      'S'
028400921123     C                   SETON                                        02
028500921123     C*
028600921123     C* MANCA ABBINAMENTO: NON ABILITO IN BASE ALLA PARTENZA
028700941221     C* ARTABN= "S" : SIGNIFICA SPUNTA DI FOGLIO ABBINATO
028800941221IF  7C     ARTABN        IFNE      'S'
028900921123     C                   SETON                                        01
029000930603E   7C                   ENDIF
029100930603E   6C                   ENDIF
029200921123     C*
029300921123     C* NON E' POSSIBILE ABBINARE
029400990920     C   01
029500990920     CAN 02
029600990920     CANN20
029700990920     CANN21              SETON                                        30
029800930603E   5C                   ENDIF
029900930603     C***
030000930603     C*** D A T A   A R R I V O   E F F E T T I V O  ULTIMO COLLO BOLLA
030100930603     C***
030200930603     C* SE DATA CONSEGNA EFFETTIVA DELLA BOLLA E' VUOTA
030300940613IF  5C   17ARBDTI        IFEQ      0
030400930603     C*
030500930603     C* SE LA DATA DEL COLLO E' MAGGIORE DELLA DATA SALVATA
030600940613IF  6C     ARTDAM        IFGT      SAVDAM
030700930603     C*
030800930603     C* PULISCO SCHIERE DEI FOGLI VIAGGIO E DEI SEGNACOLLI
030900940613     C                   CLEAR                   NFV
031000940613     C                   CLEAR                   NSC
031100050304     C                   CLEAR                   dut
031200930603     C*
031300930603     C* MEMORIZZO DATA, NUMERO FOGLIO E SEGNACOLLO
031400941102     C                   Z-ADD     ARTDAM        SAVDAM
031500050510     c* Memrizzo se aggiornato con spunta processo ope partenza o arrivo
031600050510     c                   eval      savfl2=artfl2
031700930603     C                   Z-ADD     ARTNFV        NFV(1)
031800941102     C                   Z-ADD     ARTNSC        NSC(1)
031900050304     C                   Z-ADD     ARTdut        dut(1)
032000930603     C                   Z-ADD     1             X                 3 0
032100930603     C*
032200930603     C* DATA ARRIVO MINORE O UGUALE
032300940613X   6C                   ELSE
032400050304IF  7C     ARTDAM        IFEQ      SAVDAM
032500050510     c*
032600050510     c                   if        artfl2='P'
032700050510     c                   eval      savfl2=artfl2
032800050510     c                   endif
032900050510     c
033000050304     c                   z-add     1             yy                4 0
033100050304     C     ARTNFV        LOOKUP    NFV(yy)                                34
033200930603     C*
033300930603     C* F.V. NON MEMORIZZATO (NE CARICO AL MASSIMO 100)
033400950331IF  8C     *IN34         IFEQ      '0'
033500050304    9C     X             iflt      100
033600930603     C                   ADD       1             X
033700930603     C                   Z-ADD     ARTNFV        NFV(X)
033800941102     C                   Z-ADD     ARTNSC        NSC(X)
033900050304     C                   Z-ADD     ARTdut        DUT(X)
034000050304    9c                   endif
034100050304     c*
034200050304   x8c                   else
034300050304     c* Se foglio già presente memorizzo segnacolo con DUT >
034400050304    9c                   if        artdut>dut(yy)
034500050304     C                   Z-ADD     ARTNFV        NFV(yy)
034600050304     C                   Z-ADD     ARTNSC        NSC(yy)
034700050304     C                   Z-ADD     ARTdut        DUT(yy)
034800050304E   9C                   ENDIF
034900050304E   8C                   ENDIF
035000950331E   7C                   ENDIF
035100940613E   6C                   ENDIF
035200930603     C*
035300930603E   5C                   ENDIF
035400930603E   4C                   ENDIF
035500930603     C***
035600930603     C*** V O L U M E   C M L
035700930603     C***
035800950629IF  4C     *IN20         IFEQ      *ON
035900941031     C     ARTVUC        ANDGT     0
036000930503     C*
036100930503     C* CALCOLO IL VOLUME TOTALE DELLA BOLLA E I COLLI RILEVATI
036200941031     C                   ADD       ARTVUC        TOTVUC
036300941031     C                   ADD       1             D20NCR
036400990921     C     ARTFVC        IFNE      'P'
036500990921     C                   MOVEL     ARTFVC        WFVC
036600990921     C                   ENDIF
036700930504E   4C                   ENDIF
036800990806     C*** P E S O   C M L
036900990806     C***
037000990806IF  4C     *IN21         IFEQ      *ON
037100990806     C     ARTPUC        ANDGT     0
037200990806     C*
037300990806     C* CALCOLO IL PESO TOTALE DELLA BOLLA E I COLLI RILEVATI
037400990913     C                   ADD       ARTPUC        TOTPUC
037500990806     C                   ADD       1             D39NCP
037600990921     C     ARTFPC        IFNE      'P'
037700990921     C                   MOVEL     ARTFPC        WFPC
037800990921     C                   ENDIF
037900990806E   4C                   ENDIF
038000930517     C*
038100941031     C  N30KARB          READE     FNART000                               30
038200930504E   3C                   ENDDO
038300930603     C*
038400930603     C*
038500950629    3C     *IN15         IFEQ      *ON
038600930603     C***
038700930603     C*** A B I L I T A Z I O N E   B O L L E   A L L A   C O N S E G.
038800930603     C***
038900911011     C*
039000921123     C* 01 OFF - SEGNACOLLI PARTITI E ABBINATI ==> ABILITO  AMP=1 AMA=1
039100921123     C*          SE AMP=1 AMA SI PUO' PORTARE SOLO A 2
039200930517     C*
039300921123     C* 02 OFF - SEGNACOLLI ARRIVATI           ==> ABILITO  AMP=1 AMA=2
039400911011     C*
039500950629    4C     ARBAMA        IFNE      2
039600930603     C  N01
039700930603     CANN02              MOVEL     1             ARBAMP
039800930603     C  N01
039900930603     CANN02              MOVEL     2             ARBAMA
040000911011     C*
040100950629     C  N01
040200950629IF  5CAN 02ARBAMP        IFEQ      0
040300911011     C                   MOVEL     1             ARBAMP
040400911011     C                   MOVEL     1             ARBAMA
040500950629E   5C                   ENDIF
040600950629E   4C                   ENDIF
040700930603     C***
040800930603     C*** D A T A   A R R I V O   E F F E T T I V O  ULTIMO COLLO BOLLA
040900930603     C***
041000940613    4C   17ARBDTI        IFEQ      0
041100930603     C*
041200930517     C                   Z-ADD     1             X
041300950404     C*
041400950404     C* MEMORIZZO PER POI RICHIAMARE IL RICALCOLO TEMPI DI CONSEGNA
041500950404     C                   MOVE      '1'           W001A             1
041600930517     C*
041700930603DO  5C     NSC(X)        DOWGT     *ZEROS
041800930517     C                   Z-ADD     ARBFLS        KLNP
041900930517     C                   Z-ADD     ARBLNA        KLNA
042000930517     C                   Z-ADD     ARBNRS        KNRS
042100930517     C                   Z-ADD     NSC(X)        KNSC
042200930517     C*
042300930517     C* LEGGO SEGNACOLLO PER REPERIRE LA LNP DEL FOGLIO
042400070123     C     KBRV          SETLL     FNBRV09L
042500070123     C     KBRV          READE     FNBRV09L                               35
042600930517     C*
042700930517     C* F.V. DELLA LNA DELLA BOLLA
042800941102    6C     *IN35         DOWEQ     *OFF
042900970730     C                   CLEAR                   DSLV53
043000970730     C                   MOVEL     'A'           D53TFO
043100040331     c* campo non più usato
043200040331     C***                Z-ADD     SIMFEL        D53FEL
043300970730     C                   Z-ADD     BRVFGS        D53FGS
043400970730     C                   Z-ADD     BRVNFV        D53NFV
043500970730     C                   MOVEL     '1'           D53NPG
043600970730     C                   MOVEL     BRVFLE        D53FLF
043700970730     C                   MOVEL     'A'           D53ABB
043800970730     C                   MOVEL     ARBLNA        D53LNA
043900050603     c* non lo passo perchè potrebbe essere cambiato (ad esempio se collo
044000050603     c* impato o transitato)
044100050603     C***                MOVEL     ARBTFA        D53TFA
044200970730     C                   CALL      'FNLV53R'
044300970730     C                   PARM                    DSLV53
044400930517     C*
044500970730IF  7C     D53ERR        IFEQ      ' '
044600930517     C*
044700940613     C* MEMORIZZO DATA E ORA + RECENTI SOLTANTO SE:
044800940613     C*  IL FV PARTENZA E' STATO ABBINATO A UN FOGLIO ARRIVI
044900950427     C*  CON DATA >= ALLA DATA DI ARRIVO DEL COLLO
045000940613     C*
045100050510     c* Se data arrivo aggiornata con spunta partenza, data foglio arrivi
045200050510     c*  deve essere > (e non uguale) alla stessa data
045300050510     c**
045400050510   7AC**   D53DFA        IFGE      SAVDAM
045500050510     c**
045600050510     c                   if        (savfl2=' ' and d53dfa >=SAVDAM) or
045700050510     c                             (savfl2<>' ' and d53dfa>SAVDAM)
045800940613     C*
045900930517     C* MEMORIZZO SOLO SE LA DATA E L'ORA SONO PIU RECENTI
046000970730IF  8C     D53AED        IFGE      SAVAED
046100970730IF  9C     D53AED        IFGT      SAVAED
046200970730     C                   Z-ADD     D53AED        SAVAED
046300970730     C                   Z-ADD     D53AEH        SAVAEH
046400930607X   9C                   ELSE
046500970730IF 10C     D53AEH        IFGT      SAVAEH
046600970730     C                   Z-ADD     D53AEH        SAVAEH
046700930607E  10C                   ENDIF
046800930607E   9C                   ENDIF
046900930603E   8C                   ENDIF
047000940613     C*
047100940613E  7AC                   ENDIF
047200940613E   7C                   ENDIF
047300930528     C*
047400070123     C     KBRV          READE     FNBRV09L                               35
047500941102E   6C                   ENDDO
047600930517     C*
047700930528     C                   ADD       1             X
047800930603E   5C                   ENDDO
047900940613     C*
048000930603     C* SE DATA ARRIVO PRESENTE IN TUTTI I COLLI DELLA BOLLA
048100950627    5C     SAVAED        IFGT      0
048200950627    6C     BOLAED        IFLT      SAVAED
048300950627     C     BOLAED        OREQ      SAVAED
048400950619     C     BOLAEH        ANDLT     SAVAEH
048500930603     C                   Z-ADD     SAVAED        ARBDTI
048600930603     C                   Z-ADD     SAVAEH        ARBHTI
048700950627   X6C                   ELSE
048800950619     C                   Z-ADD     BOLAED        ARBDTI
048900950619     C                   Z-ADD     BOLAEH        ARBHTI
049000950627E   6C                   ENDIF
049100950627E   5C                   ENDIF
049200950619E   4C                   ENDIF
049300930517     C*
049400931213     C*
049500940613     C* SE  ARBDTI = 0  CERCO NEI FV PART ABBINATI AL FOGLIO ARRIVI
049600940613     C*                 CON DATA = ARTDAM
049700940613    4C   17ARBDTI        IFEQ      0
049800931213     C*
049900931213     C                   CLEAR                   COMDTI
050000931213     C                   CLEAR                   COMHTI
050100020918      * cerco l'As del terminal di arrivo e della linea di arrivo della bolla
050200050510     c* Ora stesso AS: non serve più
050300050510     c***                Exsr      RicAs
050400940613     C*
050500931213     C                   EXSR      RICFV
050600931213     C*
050700931213     C* MEMORIZZO I VALORI TROVATI IN  ARBDTI  E  ARBHTI
050800950619     C* CONFRONTANDO CON LA DATA DI ARRIVO DEL FOGLIO DELLA BOLLA
050900950619     C*
051000950619    5C     BOLAED        IFLT      COMDTI
051100950619    5C     BOLAED        OREQ      COMDTI
051200950619     C     BOLAEH        ANDLT     COMHTI
051300950619     C                   Z-ADD     COMDTI        ARBDTI
051400950619     C                   Z-ADD     COMHTI        ARBHTI
051500950619   X5C                   ELSE
051600950619     C                   Z-ADD     BOLAED        ARBDTI
051700950619     C                   Z-ADD     BOLAEH        ARBHTI
051800950619E   5C                   ENDIF
051900940613    4C                   ENDIF
052000931213     C*
052100930603E   3C                   ENDIF
052200930603E   2C                   ENDIF
052300930603     C***
052400930603     C*** V O L U M E   C M L
052500930603     C***
052600930504IF  2C     *IN20         IFEQ      '1'
052700941031     C     TOTVUC        ANDGT     0
052800941102     C*
052900941102     C* IMPOSTO IL VOLUME CML
053000941102     C                   Z-ADD(H)  TOTVUC        D20VLU
053100950110     C     D20VLU        IFEQ      0
053200950110     C                   Z-ADD     0,001         D20VLU
053300950110     C                   ENDIF
053400930503     C*
053500941031     C                   MOVEL     'A'           D20TBO
053600990921     C* NON DEVO TRASMETTERE LA VARIAZIONE DI VOLUME CML SE
053700990921     C* I VOLUMI SONO DELLA PARTENZA PERCHE' IN QUESTO CASO
053800990921     C* LA TRASMISSIONE ALLA SEDE E' GIA' STATA FATTA DALLA PARTENZA
053900990921     C* (V. TABELLA 7T TRASMISSIONE VOLUMI 'T' E 'Z' DA PARTENZA A
054000990921     C* SEDE --> TALI FLAG SONO IMPOSTATI AD 'S')
054100990921     C* ATTENZIONE QUINDI CHE I FLAG DELLA 7T DIVENTADO FISSO (NON
054200990921     C* MODIFICABILI) ALTRIMENTI NON FUNZIONA PIU' QUESTO GIRO
054300990921     C* COL RISCHIO DI PERDERE DEI VOLUMI CML IN SEDE
054400990921     C     WFVC          IFNE      *BLANKS
054500970901     C                   MOVEL     'S'           D20FTR
054600990921     C                   ELSE
054700990921     C                   MOVEL     ' '           D20FTR
054800990921     C                   ENDIF
054900941031     C                   MOVEL     'C'           D20TVL
055000941031     C                   MOVEL     ARBFVF        D20FVF
055100941031     C                   MOVEL     ARBVLF        D20VLF
055200941031     C                   MOVEL     ARBFVB        D20FVB
055300941031     C                   MOVEL     ARBVLB        D20VLB
055400941031     C                   MOVEL     ARBAAS        D20AAS
055500941031     C                   MOVEL     ARBLNP        D20LNP
055600941031     C                   MOVEL     ARBNRS        D20NRS
055700941031     C                   MOVEL     ARBNSP        D20NSP
055800941031     C                   MOVEL     ARBCBO        D20CBO
055900941031     C                   MOVEL     ARBNCL        D20NCL
056000991116     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
056100991116     C                   MOVEL     '1'           WTRC
056200991116     C     KAR6          CHAIN     FIAR6000                           30
056300941102     C  N30              MOVEL     AR6DFT        D20DFT
056400941031     C                   MOVEL     ARBLNA        D20LNA
056500960927     C*
056600960927     C                   MOVE      ARBDBR        D20DTE
056700941031     C                   MOVEL     DSLV20        KPJBU
056800040510     C* Passo parametri in più se pgm "gufo"
056900111109     c                   if        %parms>=2
057000990806     C                   CALL      'FNLV20R'
057100990806     C                   PARM                    KPJBA
057200040510     c                   parm      'S'           pwstor            1
057300040510     c                   else
057400040510     C                   CALL      'FNLV20R'
057500040510     C                   PARM                    KPJBA
057600040510     c                   endif
057700990806     C*
057800990806     C                   MOVEL     KPJBU         DSLV20
057900990806     C*
058000990806     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
058100990806    4C     D20ERR        IFNE      '1'
058200990806     C                   MOVEL     D20NCR        ARBNCR
058300990806     C                   MOVEL     D20VLU        ARBVLC
058400990806     C                   MOVEL     D20FVF        ARBFVF
058500990806     C                   MOVEL     D20VLF        ARBVLF
058600990806     C**
058700990806   X4C                   ELSE
058800990806     C                   MOVEL     'V'           WFBS              1
058900990806E   4C                   ENDIF
059000990806E   2C                   ENDIF
059100990806     C*
059200990806      * DS PER PESO
059300990806     C*** P E S O    C M L
059400990806     C***
059500990806IF  2C     *IN21         IFEQ      '1'
059600990806     C     TOTPUC        ANDGT     0
059700990806     C*
059800990806     C* IMPOSTO IL VOLUME CML
059900990806     C                   Z-ADD(H)  TOTPUC        D39PLU
060000990806     C     D39PLU        IFEQ      0
060100990806     C                   Z-ADD     0,001         D39PLU
060200990806     C                   ENDIF
060300990806     C                   MOVEL     'A'           D39TBO
060400990921     C* PER EVITARE DI TRASMETTERE SEMPRE IL PESO CML ALLA SEDE
060500990921     C* ABBIAMO IMPOSTATO AD 'S' IL FLAG DI TRASMISSIONE PESO CML
060600990921     C* DA PARTENZA A SEDE  (V.TABELLA 7Y) E IN ARRIVO LA TRASMISSIONE
060700990921     C* VERRA' EFFETTUATA SOLO SE IL PESO NON E' DELLA PARTENZA
060800990921     C* ATTENZIONE QUINDI CHE IL FLAG DELLA 7Y DIVENTA FISSO (NON
060900990921     C* MODIFICABILE) ALTRIMENTI NON FUNZIONA PIU' QUESTO GIRO
061000990921     C* COL RISCHIO DI PERDERE DEI PESI CML IN SEDE
061100990921     C     WFPC          IFNE      *BLANKS
061200990921     C                   MOVEL     'S'           D39FTR
061300990921     C                   ELSE
061400990921     C                   MOVEL     ' '           D39FTR
061500990921     C                   ENDIF
061600990806     C                   MOVEL     'C'           D39TPP
061700990806     C                   MOVEL     ARBPKF        D39PLF
061800990806     C                   MOVEL     ARBPKB        D39PLB
061900990806     C                   MOVEL     ARBAAS        D39AAS
062000990806     C                   MOVEL     ARBLNP        D39LNP
062100990806     C                   MOVEL     ARBNRS        D39NRS
062200990806     C                   MOVEL     ARBNSP        D39NSP
062300990806     C                   MOVEL     ARBCBO        D39CBO
062400990806     C                   MOVEL     ARBNCL        D39NCL
062500931213     C*
062600990806     C  N30              MOVEL     AR6DFT        D39DFT
062700990806     C                   MOVEL     ARBLNA        D39LNA
062800990806     C                   MOVE      ARBDBR        D39DTE
062900990806     C*
063000990806     C                   MOVEL     DSLV39        KPJBU
063100040510     C                   CALL      'FNLV39R'
063200040510     C                   PARM                    KPJBA
063300941031     C*
063400990806     C                   MOVEL     KPJBU         DSLV39
063500941031     C*
063600970616     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
063700990806    4C     D39ERR        IFNE      '1'
063800990806     C                   MOVEL     D39NCP        ARBNCP
063900990806     C                   MOVEL     D39PLU        ARBPKC
064000990806     C                   MOVEL     D39PLF        ARBPKF
064100970924     C**
064200970924   X4C                   ELSE
064300070704     c                   if        wfbs=' '
064400990806     C                   MOVEL     'P'           WFBS              1
064500070704     c                   else
064600070704     C                   MOVEL     'E'           WFBS              1
064700070704     c                   endif
064800070704     c
064900941102E   4C                   ENDIF
065000990806     C                   ENDIF
065100950323     C*
065200950323     C* AGGIORNO FILE TASSAZIONE PADRONCINI
065300990806     C     *IN20         IFEQ      *ON
065400040205     C     *IN21         oreq      *ON
065500950323     C                   EXSR      AGGTAS
065600070703     c* se non sono riuscita ad aggiornare i padroncini, reimposto
065700070703     c*  per elaborare
065800070703     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
065900070704    4C     o57err        IFNE      ' '
066000070704     c* Se modificato il peso
066100070704    5c                   if        *in21
066200070703     C                   MOVEL     savNCP        ARBNCP
066300070703     C                   MOVEL     savPKC        ARBPKC
066400070703     C                   MOVEL     savPKF        ARBPKF
066500070703     C                   MOVEL     'P'           WFBS              1
066600070704E   5C                   ENDIF
066700070704     c* Se modificato il volume
066800070704    5c                   if        *in20
066900070704     C                   MOVEL     savNCR        ARBNCR
067000070704     C                   MOVEL     savvlc        ARBVLC
067100070704     C                   MOVEL     savFVF        ARBFVF
067200070704     C                   MOVEL     savVLF        ARBVLF
067300070704    6c                   if        wfbs=' '
067400070704     C                   MOVEL     'V'           WFBS              1
067500070704     c                   else
067600070704     C                   MOVEL     'E'           WFBS              1
067700070704    6c                   endif
067800070704E   5C                   ENDIF
067900070704     c
068000070704E   4C                   ENDIF
068100070704    3C                   END
068200930503     C*
068300070703     C* Aggiorno bolla
068400970924     C                   MOVEL     WFBS          ARBFBS
068500970924     C                   CLEAR                   WFBS
068600930503     C*
068700941031     C                   UPDATE    FNARB000
068800950404     C*
068900950404     C     W001A         IFNE      *BLANKS
069000950404     C     ARBDCM        ANDNE     *ZEROS
069100950404     C     ARBDCE        ANDNE     *ZEROS
069200950404     C                   EXSR      SRTCON
069300950404     C                   END
069400111114     C*
069500941031     C                   READ      FNARB000                             1031
069600930504     C   10              SETON                                        31
069700930504E   1C                   ENDDO
069800040401     c*
069900111109     c* chiudo sempre in LR
070000111109     c                   seton                                        LR
070100111109     c
070200040505     c* Se passato un parametro solo chiudo con LR
070300040505    1c                   if        %PARMs<2
070400040505     C                   SETON                                        LR
070500111109     c                   clear                   Sitrasm           1
070600040505   x1c                   else
070700111109
070800040505     C* CONTROLLA SE E' STATA RICHIESTA LA CHIUSURA DEL SOTTOSISTEMA
070900040505     C                   SHTDN                                        98
071000040505     c
071100040505    2c                   if        *in98
071200040505     c                   eval      chiudi='S'
071300040505     C                   SETON                                        LR
071400111114     c*****              eval      SItrasm='S'
071500040505   x2c                   else
071600111109     c
071700111109     c* Imposto quello passato
071800111109     c                   movel     TrasmiVP      Sitrasm
071900040505     c
072000040505     c* Se non sono passate 2 ore accendo RT altrimenti LR
072100040505     c*  e sottometto trasmissione
072200111109     c**                 time                    worad
072300111109     c**   worad         subdur    worap         minuti:*MN
072400111109    3c**                 if        minuti>120
072500111109     C**                 SETON                                        LR
072600111109     c**                 eval      SItrasm='S'
072700111109   x3c**                 else
072800111006     C*******            SETON                                        RT
072900111109     c*
073000111109     c* chiudo il LR ma non trasmetto pesi e volumi
073100111109     C**                 SETON                                        LR
073200111109    3c**+                endif
073300111109     c
073400041104    2c                   endif
073500041104     c
073600111109     c*
073700041104    2c                   if        *inlr
073800041104     c* chiudo i pgm del peso e volume
073900041104     C                   CLEAR                   DSLV39
074000041104     C                   MOVEL     'C'           D39TLA
074100041104     C                   MOVEL     DSLV39        KPJBU
074200041104     C                   CALL      'FNLV39R'
074300041104     C                   PARM                    KPJBA
074400041108     c*
074500041108     C                   CLEAR                   DSLV20
074600041108     C                   MOVEL     'C'           D20TLA
074700041108     C                   MOVEL     DSLV20        KPJBU
074800041108     C                   CALL      'FNLV20R'
074900041108     C                   PARM                    KPJBA
075000041104     c
075100111006     c                   if        SItrasm='S'
075200040505     c                   eval      pa3nfv='99999'
075300070502     c                   eval      pa3tfp='   '
075400040505     c                   movel     param3        kpjbu
075500040505     c                   eval      trasmpesvol='S'
075600040505     c                   call      'FNLSA3R'
075700040505     c                   parm                    kpjba
075800040505     c                   parm                    trasmpesvol       1
075900111114     c
076000111114     c                   clear                   Sitrasm           1
076100160617     c
076200160617     c* Eseguo trasmissione date di consegna del membro M946046 se ce ne sno
076300160617     c                   clear                   param7a
076400160617     c                   eval      nmi7a='8888888'
076500160617     c                   eval      fgs7a='888'
076600160617     c                   movel     param7a       kpjbu
076700160617     c                   call      'FNLR90R'
076800160617     c                   parm                    kpjba
076900040505    2c                   endif
077000111006     c
077100111006    2c                   endif
077200040505    1c                   endif
077300040505     C**
077400040505     c                   if        *inlr
077500040505     C                   CLEAR                   DSLV57
077600040505     C                   MOVEL     'C'           I57TLA
077700040505     C                   CALL      'FNLV57R'
077800040505     C                   PARM                    KPJBA
077900040505     C                   PARM                    DSLV57
078000111010     C*
078100111010     C                   MOVEL     'C'           D53TLA
078200111010     C                   CALL      'FNLV53R'
078300111010     C                   PARM                    DSLV53
078400040505    1c                   endif
078500931210     C*---------------------------------------------------------------*
078600931210     C*                    RICFV                                      *
078700931210     C* ROUTINE DI RICERCA FOGLIO VIAGGIO PARTENZA                    *
078800931210     C*---------------------------------------------------------------*
078900931210     C     RICFV         BEGSR
079000050510     c                   eval      Wgiro = *off
079100050510     C                   Z-ADD     2             COMNPG
079200050510
079300050510      * Devo leggere fnfvv con terminal di arrivo bolla nel caso in cui
079400050510      * il terminal di arrivo della bolla e la linea di arrivo della bolla sono sullo
079500050510      * stesso As (ora sempre)
079600050510     c**                 If        TfaOgas = LnaOgas
079700050510     c                   Eval      Kfgs = ArbTfa
079800050510     c**                 Else
079900050510      * se non sono sullo stesso As leggo fnfvv con linea di arrivo della bolla
080000050510     c**                 Eval      Kfgs = ArbLna
080100050510     c**                 EndIf
080200050510     c
080300050510     c* Se bolla aggioranta con spunte processo ope partenza, devo
080400050510     c*  cercare un foglio arrivi con data > di SAVDAM
080500050510     c                   if        savfl2=' '
080600050510     c                   eval      kdam=savdam
080700050510     c                   else
080800050510     c*
080900050510     C     KFVV08        SETgt     FNFVV08
081000050510     C     KFVV08b       READE     FNFVV08                                32
081100050510     c                   if        not *in32
081200050510     c                   eval      kdam=fvvdfv
081300050510     c                   else
081400050510     c
081500050510     c* Foglio arrivi non trovato
081600050510     c* imposto SAVDAM+1
081700050510     c     *iso          movel     savdam        dataiso
081800050510     c                   adddur    1:*d          dataiso
081900050510     c                   move      dataiso       comdti
082000050510     C                   Z-ADD     0101          COMHTI
082100050510     c                   goto      endric
082200050510     c                   endif
082300050510     c
082400050510     c                   endif
082500020919
082600020919      * imposto un flag di comodo x fare il secondo tentativo con lo stesso npg
082700020919     c     rileggi       tag
082800940613     C*
082900940613     C* NEL 1 GIRO
083000050510     C* CERCO IL FOGLIO ARRIVI CON  NPG=2  E  DFV=SAVDAM o >savdam
083100940613     C* NEL 2 GIRO
083200050510     C* CERCO IL FOGLIO ARRIVI CON  NPG=6  E  DFV=SAVDAM o >savdam
083300050914     C* SE NON L'HO TROVATO AL 1 GIRO (non più)
083400050914     C****               Z-ADD     1             Y                 2 0
083500940613     C*
083600050914   -1C**   Y             DOWLE     2
083700050914     C**   COMDTI        ANDEQ     0
083800940613     C*
083900941102     C     KFVV          SETLL     FNFVV000
084000941102     C     KFVV          READE     FNFVV000                               32
084100940613     C*
084200940613    0C     *IN32         DOWEQ     *OFF
084300940613     C*
084400940613     C* CERCO IL F.VIAGGIO PARTENZA (ABBINATO SUL FOGLIO ARRIVI TROVATO
084500940613     C*   CHE CONTIENE LA LINEA DEL COLLO CON ARTDAM PIU'  RECENTE
084600931215     C*
084700940613     C* LEGGO FLFVA02L CON         NFA=FVVNFV
084800941102     C     KFVA2         SETLL     FNFVA02
084900941102     C     KFVA2         READE     FNFVA02                                33
085000931210     C*
085100931213    1C     *IN33         DOWEQ     *OFF
085200970708     C                   CLEAR                   DSLV53
085300970708     C                   MOVEL     'A'           D53TFO
085400970708     C                   MOVEL     FVANFV        D53NFV
085500970708     C                   MOVEL     1             D53NPG
085600970708     C                   MOVEL     FVALNP        D53FGS
085700970708     C                   MOVEL     FVALAI        D53LAI
085800031212     C                   MOVEL     'B'           D53ABB
085900970708     C                   MOVEL     ARBLNA        D53LNA
086000050603     C****               MOVEL     ARBTFA        D53TFA
086100970708     C                   CALL      'FNLV53R'
086200970708     C                   PARM                    DSLV53
086300970730     C**
086400970730     C* LEGGO QUESTO FNFVA SOLO SE IL PGM HA TROVATO CHE E' UN
086500970730     C*  FOGLIO DELLA LINEA DI ARRIVO
086600970730    2C     D53ERR        IFEQ      ' '
086700050914     C* SE FVALNP = TERMINAL PART. BOLLA
086800970730     C     FVALNP        ANDEQ     ARBTFP
086900050914     c                   if        comdti=0 or (comdti<fvaaed) or
087000050914     c                             (comdti=fvaaed and fvaaeh>comhti)
087100931214     C                   Z-ADD     FVAAEH        COMHTI
087200940613     C                   Z-ADD     FVAAED        COMDTI
087300050914     c                   endif
087400050913     c
087500050913   X2C***                else
087600050913    2C                   ENDIF
087700970730     C* RILEGGO
087800050914     C     KFVA2         READE     FNFVA02                                33
087900050913    2C***                ENDIF
088000931213    1C                   ENDDO
088100940613     C*
088200941102     C     KFVV          READE     FNFVV000                               32
088300940613     C*
088400940613     C* SE GIA' TROVATO NON LEGGO PIU'
088500050914     c*  non lo faccio più perchè tengo la data + alta quindi devo leggere
088600050914     c*  tutto
088700050914     C**   COMDTI        IFGT      0
088800050914     C**                 SETON                                        32
088900050914     C**                 ENDIF
089000940613    0C                   ENDDO
089100940613     C*
089200050914     c* tolgo la cat. 6 non più usata
089300050914     C*****              ADD       1             Y
089400050914     C*****              Z-ADD     6             COMNPG
089500050914   -1C*****              ENDDO
089600020919
089700050913      * Se non ho trovato e terminal di arrivo e linea di arrivo della bolla
089800020919      * sono su stesso AS devo fare un altro tentativo con la linea di arrivo della bolla
089900050510     c***                If        Comdti = *zeros and TfaOgas = LnaOgas
090000050510     c                   If        Comdti = *zeros
090100020919     c                             and Wgiro = *off and
090200020919     c                             arbtfa<>arblna
090300020919     c                   Eval      Kfgs = ArbLna
090400020919     c                   Eval      Wgiro = *on
090500020919     c                   GoTo      Rileggi
090600020919     c                   EndIf
090700931210     C*
090800940613     C     COMDTI        IFEQ      0
090900931210     C                   Z-ADD     0101          COMHTI
091000050510     C                   Z-ADD     kdam          COMDTI
091100931210     C                   ENDIF
091200931210     C*
091300050510     C     endric        ENDSR
091400940208     C*---------------------------------------------------------------*
091500980114     C* CONTROLLO SE CALCOLARE LA DATA ARRIVO ULTIMO COLLO
091600940208     C*---------------------------------------------------------------*
091700940208     C     RIC2          BEGSR
091800940613     C* SE COLLO NON ARRIVATO NON POSSO CALCOLARE LA DATA E ORA
091900940613     C*  ARRIVO ULTIMO COLLO
092000940613    1C     ARTDAM        IFEQ      0
092100940613     C                   SETOFF                                       17
092200941102   X1C                   ELSE
092300940208     C*
092400950123     C* ARTABN = "S" COLLO DI FV PARTENZA ABBINATO
092500111229     c* se la bolla è consegnata anche se manca ARTABN calcolo lo stesso
092600950123    2C     ARTABN        IFNE      'S'
092700111229     c     artdcm        andeq     0
092800111229     c
092900980114     C* NON LO CALCOLO SE C'E' LA DATA DI PARTENZA E QUINDI LA SPUNTA
093000111229     C*  NON ANCORA ABBINATA.
093100980114    3C     ARTDFV        IFGT      0
093200980114     C     ARTFLP        ANDEQ     0
093300980114     C     ARTDUT        ORGT      0
093400980114     C     ARTFLP        ANDGT     0
093500980114     C                   SETOFF                                       17
093600980114    3C                   ENDIF
093700980114    2C                   ENDIF
093800980114    1C                   ENDIF
093900940208     C*
094000940208     C                   ENDSR
094100950323     C*---------------------------------------------------------------*
094200950323     C*                    AGGTAS                                     *
094300950323     C* AGGIORNO FILE TASSAZIONE PADRONCINI                           *
094400950323     C*---------------------------------------------------------------*
094500950323     C     AGGTAS        BEGSR
094600950323     C*
094700970616     C* MODIFICATO VOLUME CML: SISTEMO IL VOLUME ANCHE NEI PADRONCINI
094800950323    1C     ARBVLC        IFNE      SAVVLC
094900040205    1C     ARBPKC        ORNE      SAVPKC
095000970924     C                   CLEAR                   DSLV57
095100970924     C                   MOVEL     'A'           I57TBO
095200970924     C                   MOVEL     ARBAAS        I57AAS
095300970924     C                   MOVEL     ARBLNP        I57LNP
095400970924     C                   MOVEL     ARBNRS        I57NRS
095500970924     C                   MOVEL     ARBNSP        I57NSP
095600970924     C                   MOVEL     ARBPDC        I57PDR
095700970924     C                   MOVEL     ARBNCL        I57NCL
095800970924     C                   MOVEL     'C'           I57TVL
095900970924     C                   MOVEL     SAVNCR        I57NCR
096000970924     C                   MOVEL     SAVVLC        I57VLC
096100990920     C                   MOVEL     SAVVLF        I57VLF
096200970924     C                   MOVEL     ARBVLC        I57VCN
096300970924     C                   MOVEL     ARBNCR        I57NCN
096400970924     C                   MOVEL     ARBVLF        I57VFN
096500040205     C                   MOVEL     SAVNCp        I57nCp
096600040205     C                   MOVEL     SAVpkC        I57pkC
096700040205     C                   MOVEL     SAVpkF        I57pkF
096800040205     C                   MOVEL     arbNCp        I57npn
096900040205     C                   MOVEL     arbpkC        I57pcn
097000040205     C                   MOVEL     arbpkF        I57pfn
097100970924     C                   CALL      'FNLV57R'
097200970924     C                   PARM                    KPJBA
097300970924     C                   PARM                    DSLV57
097400970924     C                   ENDIF
097500970616     C*
097600950323     C                   ENDSR
097700950404     C*---------------------------------------------------------------*
097800950404     C*                    SRTCON                                     *
097900950404     C* RICALCOLO TEMPI DI CONSEGNA                                   *
098000950404     C*---------------------------------------------------------------*
098100950404     C     SRTCON        BEGSR
098200950404     C* SE SPEDIZIONE GIà CONSEGNATA E GIA' CALCOLATI I TEMPI DI
098300950404     C* CONSEGNA LI RICALCOLO: I TEMPI POTEVANO ESSERE STATI CALCOLATI
098400950404     C* QUANDO ANCORA NON ERANO STATI ABBINATI I F.V. E QUINDI OCCORRE
098500950404     C* RICALCOLARLI PER AVERE I TEMPI GIUSTI.
098600160616     c
098700160616     c* il ricalcolo lo affido al pgm che già ora effettua i calcoli e
098800160616     c*  manda le date di consegna in sede e un partenza FNLR90R
098900160616     c                   clear                   param7a
099000160616     c                   clear                   param7b
099100160616     c                   eval      pa7rich='C'
099200160616     c                   eval      %subst(pa7nsped:1:4)=%editc(arbaas:'X')
099300160616     c                   eval      %subst(pa7nsped:5:3)=%editc(arblnp:'X')
099400160616     c                   eval      %subst(pa7nsped:8:2)=%editc(arbnrs:'X')
099500160616     c                   eval      %subst(pa7nsped:10:7)=%editc(arbnsp:'X')
099600160616     c                   movel     param7a       kpjbu
099700160616     c                   call      'FNLR90R'
099800160616     c                   parm                    kpjba
099900160616     c                   parm                    param7b
100000160616     c
100100160616     c                   if        pa7err<>' '
100200160616     C     KARB          CHAIN     FNARB1                             31
100300160616     C                   MOVEL     'S'           ARBFBS
100400160616     C                   MOVE      *ZEROS        ARBDTI
100500160616     C                   MOVE      *ZEROS        ARBHTI
100600160616     C                   UPDATE    FNARB1
100700160616     c                   endif
100800950404     C*
100900950404     C                   ENDSR
101000041222     C*---------------------------------------------------------------*
101100041222     C* controllo abbinamento foglio bolla
101200041222     C*---------------------------------------------------------------*
101300041222     c     CTRARBNFV     begsr
101400041222     C                   CLEAR                   DSLV53
101500041222     C                   MOVEL     'A'           D53TFO
101600111130     C                   Z-ADD     ARBtfp        D53FGS
101700041222     C                   Z-ADD     ARBNFV        D53NFV
101800041222     C                   MOVEL     '1'           D53NPG
101900041222     C                   MOVEL     'B'           D53ABB
102000041222     C                   MOVEL     ARBLNA        D53LNA
102100041222     C                   MOVEL     ARBTFA        D53TFA
102200041222     C                   CALL      'FNLV53R'
102300041222     C                   PARM                    DSLV53
102400041222     C* SE NON C'E' ERRORE VUOL DIRE CHE HO TROVATO IL FOGLIO
102500041222    1C     D53ERR        IFEQ      ' '
102600041222    2C     D53AED        IFEQ      0
102700041222     C                   SETOFF                                       17
102800041222   X2C                   ELSE
102900041222     C                   MOVEL     D53AED        BOLAED
103000041222     C                   MOVEL     D53AEH        BOLAEH
103100041222    2C                   ENDIF
103200041222   X1C                   ELSE
103300041222     C* SE NON TROVATO FOGLIO --> NON METTO LA DATA ARRIVO ULTIMO COLLO
103400041222     C                   SETOFF                                       17
103500041222    1C                   ENDIF
103600041222     c*
103700041222     c                   eval      wctrarbnfv='S'
103800041222     c                   ENDSR
103900930430     C*---------------------------------------------------------------*
104000930430     C*                    DEFVAR                                     *
104100930430     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
104200930430     C*---------------------------------------------------------------*
104300040401     C     *inzsr        BEGSR
104400930430     C*
104500930430     C*
104600930430     C                   Z-ADD     1             CODUT             1 0
104700040513     C                   CALL      'X§PARUT'
104800040513     C                   PARM                    UT§DSE
104900040513     C                   MOVEL     REC80         CNCR80
105000930505     C*
105100941102     C                   Z-ADD     1             KNPG
105200971120     C*
105300971120     C* DETERMINO DATA DEL GIORNO
105400971120     C                   TIME                    W0140            14 0
105500971120     C                   MOVE      W0140         WDTGIO            8 0
105600971120     C                   CLEAR                   WLBDAT
105700971120     C                   Z-ADD     WDTGIO        G02DAT
105800971120     C                   CALL      'XSRDA8'
105900971120     C                   PARM                    WLBDAT
106000971120     C                   MOVEL     G02INV        DATEU             8 0
106100040505     c* Prendo ora di apertura pgm
106200040505     C                   time                    worap
106300930503     C*
106400931210     C* DEFINIZIONE DI ALCUNI CAMPI
106500001010     C     *LIKE         DEFINE    ARBLNP        SAVLNP
106600041216     C     *LIKE         DEFINE    ARBLNP        SAVLNA
106700001010     C     *LIKE         DEFINE    ARBLNA        WLNA
106800961129     C     *LIKE         DEFINE    ARBDTI        COMDTI
106900931210     C     *LIKE         DEFINE    ARBHTI        COMHTI
107000931210     C     *LIKE         DEFINE    FVANPG        COMNPG
107100941102     C     *LIKE         DEFINE    FVANPG        KNPG
107200941102     C     *LIKE         DEFINE    FVAAED        SAVAED
107300941102     C     *LIKE         DEFINE    FVAAEH        SAVAEH
107400950619     C     *LIKE         DEFINE    FVAAED        BOLAED
107500950619     C     *LIKE         DEFINE    FVAAEH        BOLAEH
107600941102     C     *LIKE         DEFINE    FVVDFV        SAVDAM
107700950323     C     *LIKE         DEFINE    ARBNCR        SAVNCR
107800950323     C     *LIKE         DEFINE    ARBVLC        SAVVLC
107900950323     C     *LIKE         DEFINE    ARBVLF        SAVVLF
108000070704     C     *LIKE         DEFINE    ARBfvF        SAVfvF
108100990806     C     *LIKE         DEFINE    ARBPKF        SAVPKF
108200990806     C     *LIKE         DEFINE    ARBPKC        SAVPKC
108300990806     C     *LIKE         DEFINE    ARBNCP        SAVNCP
108400950323     C     *LIKE         DEFINE    ARBVLF        WADDV
108500950323     C     *LIKE         DEFINE    ARBVLF        WSOTV
108600960906     C     *LIKE         DEFINE    TBLKEY        KEY
108700960906     C     *LIKE         DEFINE    TBLCOD        COD
108800990921     C     *LIKE         DEFINE    ARTFVC        WFVC
108900990921     C     *LIKE         DEFINE    ARTFPC        WFPC
109000991116     C     *LIKE         DEFINE    AR6TRC        WTRC
109100931210     C*
109200941031     C* LETTURA FNARB
109300930430     C     KARB          KLIST
109400930430     C                   KFLD                    ARBAAS
109500930430     C                   KFLD                    ARBLNP
109600930430     C                   KFLD                    ARBNRS
109700930430     C                   KFLD                    ARBNSP
109800991116     C*
109900991116     C     KAR6          KLIST
110000991116     C                   KFLD                    ARBAAS
110100991116     C                   KFLD                    ARBLNP
110200991116     C                   KFLD                    ARBNRS
110300991116     C                   KFLD                    ARBNSP
110400991116     C                   KFLD                    WTRC
110500041217     C     KAR4a         KLIST
110600041217     C                   KFLD                    ARBAAS
110700041217     C                   KFLD                    ARBLNP
110800041217     C                   KFLD                    ARBNRS
110900041217     C                   KFLD                    ARBNSP
111000041217     C                   KFLD                    ktrc
111100041222     C     KAnm          KLIST
111200041222     C                   KFLD                    ARtfls
111300041222     C                   KFLD                    ARtlna
111400041222     C                   KFLD                    ARtNRS
111500041222     C                   KFLD                    ARtNSc
111600041222     C                   KFLD                    kcaa
111700930517     C*
111800070123     C* LETTURA FNBRV09L
111900941102     C     KBRV          KLIST
112000941102     C                   KFLD                    KNPG
112100941102     C                   KFLD                    KLNP              3 0
112200960304     C                   KFLD                    KLNA              3 0
112300930517     C                   KFLD                    KNRS              2 0
112400930517     C                   KFLD                    KNSC              7 0
112500930517     C*
112600941102     C* LETTURA FNFVV02L
112700931210     C     KFVV          KLIST
112800931210     C                   KFLD                    COMNPG
112900050510     C                   KFLD                    kdam
113000020917     C                   KFLD                    Kfgs
113100050510     C     KFVV08        KLIST
113200050510     C                   KFLD                    Kfgs
113300050510     C                   KFLD                    COMNPG
113400050510     C                   KFLD                    savdam
113500050510     C     KFVV08b       KLIST
113600050510     C                   KFLD                    Kfgs
113700050510     C                   KFLD                    COMNPG
113800931210     C* LETTURA FLFVA02L
113900931210     C     KFVA2         KLIST
114000931210     C                   KFLD                    COMNPG
114100931210     C                   KFLD                    FVVNFV
114200020917     c                   Kfld                    FvvFgs
114300931210     C*
114400960304     C* LETTURA TABEL00F
114500960304     C     KTAB2         KLIST
114600960304     C                   KFLD                    CODUT
114700960906     C                   KFLD                    COD
114800960906     C     KTAB          KLIST
114900960906     C                   KFLD                    CODUT
115000960906     C                   KFLD                    COD
115100960906     C                   KFLD                    KEY
115200930430     C                   ENDSR
115300950404**  COM  -  COMANDI CL
115400950403OVRDBF FILE(          ) MBR(          )
115500950403ALCOBJ OBJ((           *FILE *SHRUPD           ))
115600950403DLCOBJ OBJ((           *FILE *SHRUPD           ))
115700950404DLTOVR FILE(FNARBC0T  )
