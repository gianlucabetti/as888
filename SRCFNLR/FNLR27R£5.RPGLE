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
004000950403     FFNARBC0T  O  A E             DISK    USROPN
004100010103     FFIARBC46  O  A E             DISK    USROPN
004200930430     D*---------------------------------------------------------------*
004300930430     D* SCHIERE                                                       *
004400930430     D*---------------------------------------------------------------*
004500930517     D* FOGLI VIAGGIO DELLA BOLLA
004600930517     D NFV             S              5  0 DIM(100)
004700930517     D* SEGNACOLLI DEL FOGLI VIAGGIO
004800930517     D NSC             S              7  0 DIM(100)
004900050304     D* data uscita transito della bolla
005000050304     D DUT             S              8  0 DIM(100)
005100950403     D*
005200971120     D COM             S             80    DIM(4) CTDATA PERRCD(1)              COMANDI CL
005300950403     D CMD             S              1    DIM(80)                              COMANDO QCMDEXC
005400930430     D*---------------------------------------------------------------*
005500911030     D KPJBA         E DS
005600930430     D*------------------
005700930430     D* DS RICERCA RAG. SOCIALE
005800930430     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005900040513     D CNCR80        E DS
006000941031     D*------------------
006100941031     D* DS PER FNLV20R - DS DEI CAMPI PER CALCOLO VOLUME DA FATTURARE
006200941031     D DSLV20        E DS                  EXTNAME(FNLV20DS)
006300990806     D DSLV39        E DS                  EXTNAME(FNLV39DS)
006400970730     D DSLV53        E DS                  EXTNAME(FNLV53DS)
006500970924     D DSLV57        E DS                  EXTNAME(FNLV57DS)
006600970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
006700971120     D DSLV55        E DS                  EXTNAME(FNLV55DS) INZ
006800020927     d* membro per FNARBC0T
006900950403     D                 DS
007000950403     D  EMME                   1      1
007100020927     D  xSIMFE                 2      4
007200020927     D  $SIMFE                 5      7  0
007300950403     D  $MBR                   1     10
007400050913     D                 DS
007500050913     D  arbaas                 1      4  0
007600050913     D  arbmgs                 5      8  0
007700050913     D  arbdsp                 1      8  0
007800020927     d* membro per FIARBC46
007900020927     d blank           s             10
008000001010     D OG143         E DS
008100041216     D OG150         E DS
008200001010     D DS3A          E DS
008300041217     D DSbl4a        E DS
008400971120     D WLBDAT          DS                  INZ
008500971120     D  G02DAT                 1      8  0
008600971120     D  G02INV                 9     16  0
008700971120     D  G02ERR                17     17
008800971120     D  G02TGI                18     22  0
008900040505     d* Passaggio dati a fnlsa3r
009000040505     D PARAM3          DS
009100040505     D  PA3NFV                 1      5
009200070502     D  PA3tfp                 6      8
009300020917
009400040505     d minuti          s              5  0
009500040505     d Kfgs            s                   Like(FvvFgs)
009600050510     d Kdam            s                   Like(savdam)
009700050510     d SAVFL2          s                   Like(artfl2)
009800050510     d*TfaOgas         s                   Like(§ogas)
009900050510     d*LnaOgas         s                   Like(§ogas)
010000041217     d ktrc            s                   Like(ar4trc) inz('A')
010100041222     d kCAA            s                   Like(anmcaa) inz(010)
010200020918     d Wgiro           s              1    Inz('0')
010300100915     d soloarbc0t      s              1    Inz(' ')
010400040505     d worap           s               z
010500040505     d worad           s               z
010600050510     d Dataiso         s               d   datfmt(*iso)
010700020917
010800930430     C*---------------------------------------------------------------*
010900930430     C*                    MAIN                                       *
011000930430     C* PROGRAMMA DI ABILITAZIONE BOLLE ARRIVI ALLA CONSEGNA          *
011100930430     C*---------------------------------------------------------------*
011200040401     C     *ENTRY        PLIST
011300040401     C                   PARM                    KPJBA
011400040401     C                   PARM                    chiudi            1
011500111109     C                   PARM                    TrasmiVP          1
011600040513     c
011700040513     c                   if        knsif=*blanks
011800040513     c                   eval      knsif='*LIBL'
011900040513     c                   endif
012000040505     c* Per pgm gufo, ogni 2 ore chiudo pgm trasmetto variazioni peso volume
012100040505     c*  e riapro il pgm
012200040505     c
012300930430     C*
012400040401     C     *loval        setll     FNARB000                             1031
012500040401     C                   READ      FNARB000                             1031
012600941031     C*
012700941031DO  1C  N10*IN31         DOWEQ     *OFF
012800930503     C*
012900990806     C                   SETOFF                                       152021
013000950404     C                   MOVE      *BLANKS       W001A
013100041222     C                   MOVE      *BLANKS       Wctrarbnfv        1
013200001010     C* VERIFICO SE SI TRATTA DI UNA BOLLA POSTE
013300001010     C     ARBLNP        IFNE      SAVLNP
013400001010     C     ARBLNP        CHAIN     AZORG01L                           38
013500001010     C  N38              MOVEL     ORGDE3        OG143
013600001010     C   38              CLEAR                   OG143
013700001010     C                   MOVEL     ARBLNP        SAVLNP
013800001010     C                   ENDIF
013900041216     C* VERIFICO SE LA LINEA DI ARRIVO FA IL PIKING
014000041216     C     ARBLNA        IFNE      SAVLNA
014100041216     C     ARBLNA        CHAIN     AZORG01L                           38
014200041216     C  N38              MOVEL     ORGDF0        OG150
014300041216     C   38              CLEAR                   OG150
014400041217     C                   MOVEL     ARBLNa        SAVLNA
014500041216     C                   ENDIF
014600041216     C
014700930517     C*
014800921231     C* 15 ON - DEVO ABILITARE LA MERCE ALLA CONSEGNA
014900930603     C     ARBFBS        IFEQ      'S'
015000970924     C     ARBFBS        OREQ      'E'
015100930603     C                   SETON                                        15
015200930603     C* PULISCO DATA E ORA DI COMODO
015300941102     C                   Z-ADD     *ZEROS        SAVAED
015400941102     C                   Z-ADD     *ZEROS        SAVAEH
015500930607     C                   Z-ADD     *ZEROS        SAVDAM
015600050510     c                   clear                   savfl2
015700940613     C                   CLEAR                   NFV
015800940613     C                   CLEAR                   NSC
015900050304     C                   CLEAR                   dut
016000930607     C                   Z-ADD     1             X
016100950629     C                   ENDIF
016200921231     C*
016300921231     C* 20 ON - DEVO CALCOLARE IL VOLUME CML SU BOLLA
016400970924IF  2C****       ARBNCR    IFEQ 99999
016500970924     C****       ARBNCR    OREQ 99998
016600970924     C     ARBFBS        IFEQ      'V'
016700970924     C     ARBFBS        OREQ      'E'
016800930504     C                   SETON                                            20
016900941102     C                   Z-ADD     *ZEROS        TOTVUC            8 6
017000950323     C* SALVO VOLUME DA FATTURARE, VOLUME CML E NUMERO COLLI RILEVATI
017100950323     C                   Z-ADD     ARBVLF        SAVVLF
017200950323     C                   Z-ADD     ARBVLC        SAVVLC
017300950323     C                   Z-ADD     ARBNCR        SAVNCR
017400070704     c                   movel     arbfvf        SAVFVF
017500950323     C*
017600941102     C                   CLEAR                   DSLV20
017700930517E   2C                   ENDIF
017800990806     C* 21 ON - DEVO CALCOLARE IL PESO CML SU BOLLA
017900990806     C     ARBFBS        IFEQ      'P'
018000990806     C     ARBFBS        OREQ      'E'
018100990806     C                   SETON                                            21
018200990806     C                   Z-ADD     *ZEROS        TOTPUC            9 3
018300990806     C* SALVO VOLUME DA FATTURARE, VOLUME CML E NUMERO COLLI RILEVATI
018400990806     C                   Z-ADD     ARBPKF        SAVPKF
018500990806     C                   Z-ADD     ARBPKC        SAVPKC
018600990806     C                   Z-ADD     ARBNCP        SAVNCP
018700990806     C*
018800990806     C                   CLEAR                   DSLV39
018900990806E   2C                   ENDIF
019000911011     C*
019100930603     C* ELABORO SE:
019200930603     C* 1) DEVO ABILITARE BOLLA ALLA CONSEGNA E LIVELLO ABILITAZIONE <2
019300930603     C* 2)  "       "       "     "     "     "   "         "       >"2
019400930603     C*     MA NON HO ANCORA CALCOLATO LA DATA E L'ORA DI ARRIVO EFFETT
019500930603     C* 3) DEVO CALCOLARE IL VOLUME CML
019600990920     C* 4) DEVO CALCOLARE IL PESO   CML
019700930603     C*
019800950629IF  2C     *IN15         IFEQ      *ON
019900950629     C     ARBAMA        ANDNE     2
020000001010     C* LA DATA ARRIVO ULTIMO COLLO NON LA CALCOLO PER BOLLE POSTE
020100950629     C     *IN15         OREQ      *ON
020200930603     C     ARBDTI        ANDEQ     0
020300020918     c     §OgNtw        andne     'PPT'
020400001010     C* VOLUME O PESO
020500950629     C     *IN20         OREQ      *ON
020600990920     C     *IN21         OREQ      *ON
020700940613     C                   SETOFF                                       0102
020800940613     C                   SETON                                        17
020900950619     C*  CONTROLLO SE IL FOGLIO SU CUI E' PARTITA LA BOLLA E' ABBINATO
021000950619     C*  PERCHE' COME DATA ARRIVO ULTIMO COLLO DEVE METTERE LA > TRA
021100041217     C*  QUELLA DEI COLLI  E QUELLA DELLA BOLLA solo per DDT SI senza
021200041217     c*  piking
021300041217     C                   CLEAR                   BOLAED
021400041217     C                   CLEAR                   BOLAEH
021500950619    3C     *IN15         IFEQ      *ON
021600041217    4c     §OgNtw        Ifne      'PPT'
021700041216     C* CONSIDERO L'ABBINAMENTO DEL FOGLIO DI BORDERIZZAZIONE SOLO PER
021800041216     C* DDT  SI  E CON LNA CHE NON FA IL PICKING
021900041222     c* considero anche se si trata di manca record in arrivo
022000041217    5c                   if        §ogpck=' '
022100060216     c     kar4a         chain     fiar401l
022200060216    6c                   if         %found(fiar401l)
022300041217     c                   movel     ar4not        dsbl4a
022400041217     c                   else
022500041217     c                   clear                   dsbl4a
022600041217    6c                   endif
022700041217     c
022800041217    6c                   if        §b4abm='S' or §b4abm='C'
022900041222     c                   EXSR      ctrarbnfv
023000041217    6C                   ENDIF
023100041217    5C                   ENDIF
023200001010     C* BOLLA POSTE: NON CALCOLO DATA ARRIVO ULTIMO COLLO
023300041217  X 4C                   ELSE
023400001010     C                   SETOFF                                       17
023500041217    4C                   ENDIF
023600001010    3C                   ENDIF
023700911011     C*
023800990921     C                   CLEAR                   WFVC
023900990921     C                   CLEAR                   WFPC
024000041222     c
024100930518     C* LEGGO I COLLI DELLA BOLLA
024200941031     C     KARB          SETLL     FNART000
024300941031     C     KARB          READE     FNART000                               30
024400041222     c                   clear                   wesiste010        1
024500911011     C*
024600930504DO  3C     *IN30         DOWEQ     '0'
024700041222     c* Memorizzo se presente un manca record: in questo caso devo
024800041222     c*  considerare l'abbinamento del foglio della bolla
024900041222     c                   if        wctrarbnfv=' '
025000041222     c     kanm          setll     fnanm02l                               34
025100050308     c                   if        *in34  or artdam<=arbdbr  and
025200050308     c                             artdam>0
025300041222     c                   exsr      CTRARBNFV
025400041222     c                   endif
025500041222     c                   endif
025600041222     c
025700921231     C*
025800930603    4C     *IN15         IFEQ      '1'
025900930603     C***
026000930603     C*** A B I L I T A Z I O N E   B O L L E   A L L A   C O N S E G.
026100930603     C***
026200940208     C*
026300940613     C* ROUTINE CHE CONTROLLA COLLO PER COLLO SE SONO PARTITI SU FV
026400980114     C*  ABBINATI: SE NE TROVO ALMENO 1 NON ABBINATO NON  POSSO
026500980114     C*  CALCOLARE LA DATA E ORA ARRIVO EFFETTIVO ULTIMO COLLO (17 OFF)
026600940613     C   17              EXSR      RIC2
026700940208     C*
026800950629IF  5C     ARBAMA        IFNE      2
026900921123     C*
027000980114     C* PER L'ABILITAZIONE, SE C'E LA DATA ARRIVO E' COME SE :
027100921123     C* 1) FOSSE PARTITO IL COLLO
027200921123     C* 2) FOSSE ABBINATO IL RELATIVO FOGLIO VIAGGIO
027300921123     C*
027400930914     C* MANCA DATA ARRIVO O COLLO NON ABILITATO:
027500930914     C*                    NON ABILITO IN BASE ALL'ARRIVO
027600930603IF  6C     ARTDAM        IFEQ      0
027700941221     C     ARTDAB        OREQ      'S'
027800921123     C                   SETON                                        02
027900921123     C*
028000921123     C* MANCA ABBINAMENTO: NON ABILITO IN BASE ALLA PARTENZA
028100941221     C* ARTABN= "S" : SIGNIFICA SPUNTA DI FOGLIO ABBINATO
028200941221IF  7C     ARTABN        IFNE      'S'
028300921123     C                   SETON                                        01
028400930603E   7C                   ENDIF
028500930603E   6C                   ENDIF
028600921123     C*
028700921123     C* NON E' POSSIBILE ABBINARE
028800990920     C   01
028900990920     CAN 02
029000990920     CANN20
029100990920     CANN21              SETON                                        30
029200930603E   5C                   ENDIF
029300930603     C***
029400930603     C*** D A T A   A R R I V O   E F F E T T I V O  ULTIMO COLLO BOLLA
029500930603     C***
029600930603     C* SE DATA CONSEGNA EFFETTIVA DELLA BOLLA E' VUOTA
029700940613IF  5C   17ARBDTI        IFEQ      0
029800930603     C*
029900930603     C* SE LA DATA DEL COLLO E' MAGGIORE DELLA DATA SALVATA
030000940613IF  6C     ARTDAM        IFGT      SAVDAM
030100930603     C*
030200930603     C* PULISCO SCHIERE DEI FOGLI VIAGGIO E DEI SEGNACOLLI
030300940613     C                   CLEAR                   NFV
030400940613     C                   CLEAR                   NSC
030500050304     C                   CLEAR                   dut
030600930603     C*
030700930603     C* MEMORIZZO DATA, NUMERO FOGLIO E SEGNACOLLO
030800941102     C                   Z-ADD     ARTDAM        SAVDAM
030900050510     c* Memrizzo se aggiornato con spunta processo ope partenza o arrivo
031000050510     c                   eval      savfl2=artfl2
031100930603     C                   Z-ADD     ARTNFV        NFV(1)
031200941102     C                   Z-ADD     ARTNSC        NSC(1)
031300050304     C                   Z-ADD     ARTdut        dut(1)
031400930603     C                   Z-ADD     1             X                 3 0
031500930603     C*
031600930603     C* DATA ARRIVO MINORE O UGUALE
031700940613X   6C                   ELSE
031800050304IF  7C     ARTDAM        IFEQ      SAVDAM
031900050510     c*
032000050510     c                   if        artfl2='P'
032100050510     c                   eval      savfl2=artfl2
032200050510     c                   endif
032300050510     c
032400050304     c                   z-add     1             yy                4 0
032500050304     C     ARTNFV        LOOKUP    NFV(yy)                                34
032600930603     C*
032700930603     C* F.V. NON MEMORIZZATO (NE CARICO AL MASSIMO 100)
032800950331IF  8C     *IN34         IFEQ      '0'
032900050304    9C     X             iflt      100
033000930603     C                   ADD       1             X
033100930603     C                   Z-ADD     ARTNFV        NFV(X)
033200941102     C                   Z-ADD     ARTNSC        NSC(X)
033300050304     C                   Z-ADD     ARTdut        DUT(X)
033400050304    9c                   endif
033500050304     c*
033600050304   x8c                   else
033700050304     c* Se foglio già presente memorizzo segnacolo con DUT >
033800050304    9c                   if        artdut>dut(yy)
033900050304     C                   Z-ADD     ARTNFV        NFV(yy)
034000050304     C                   Z-ADD     ARTNSC        NSC(yy)
034100050304     C                   Z-ADD     ARTdut        DUT(yy)
034200050304E   9C                   ENDIF
034300050304E   8C                   ENDIF
034400950331E   7C                   ENDIF
034500940613E   6C                   ENDIF
034600930603     C*
034700930603E   5C                   ENDIF
034800930603E   4C                   ENDIF
034900930603     C***
035000930603     C*** V O L U M E   C M L
035100930603     C***
035200950629IF  4C     *IN20         IFEQ      *ON
035300941031     C     ARTVUC        ANDGT     0
035400930503     C*
035500930503     C* CALCOLO IL VOLUME TOTALE DELLA BOLLA E I COLLI RILEVATI
035600941031     C                   ADD       ARTVUC        TOTVUC
035700941031     C                   ADD       1             D20NCR
035800990921     C     ARTFVC        IFNE      'P'
035900990921     C                   MOVEL     ARTFVC        WFVC
036000990921     C                   ENDIF
036100930504E   4C                   ENDIF
036200990806     C*** P E S O   C M L
036300990806     C***
036400990806IF  4C     *IN21         IFEQ      *ON
036500990806     C     ARTPUC        ANDGT     0
036600990806     C*
036700990806     C* CALCOLO IL PESO TOTALE DELLA BOLLA E I COLLI RILEVATI
036800990913     C                   ADD       ARTPUC        TOTPUC
036900990806     C                   ADD       1             D39NCP
037000990921     C     ARTFPC        IFNE      'P'
037100990921     C                   MOVEL     ARTFPC        WFPC
037200990921     C                   ENDIF
037300990806E   4C                   ENDIF
037400930517     C*
037500941031     C  N30KARB          READE     FNART000                               30
037600930504E   3C                   ENDDO
037700930603     C*
037800930603     C*
037900950629    3C     *IN15         IFEQ      *ON
038000930603     C***
038100930603     C*** A B I L I T A Z I O N E   B O L L E   A L L A   C O N S E G.
038200930603     C***
038300911011     C*
038400921123     C* 01 OFF - SEGNACOLLI PARTITI E ABBINATI ==> ABILITO  AMP=1 AMA=1
038500921123     C*          SE AMP=1 AMA SI PUO' PORTARE SOLO A 2
038600930517     C*
038700921123     C* 02 OFF - SEGNACOLLI ARRIVATI           ==> ABILITO  AMP=1 AMA=2
038800911011     C*
038900950629    4C     ARBAMA        IFNE      2
039000930603     C  N01
039100930603     CANN02              MOVEL     1             ARBAMP
039200930603     C  N01
039300930603     CANN02              MOVEL     2             ARBAMA
039400911011     C*
039500950629     C  N01
039600950629IF  5CAN 02ARBAMP        IFEQ      0
039700911011     C                   MOVEL     1             ARBAMP
039800911011     C                   MOVEL     1             ARBAMA
039900950629E   5C                   ENDIF
040000950629E   4C                   ENDIF
040100930603     C***
040200930603     C*** D A T A   A R R I V O   E F F E T T I V O  ULTIMO COLLO BOLLA
040300930603     C***
040400940613    4C   17ARBDTI        IFEQ      0
040500930603     C*
040600930517     C                   Z-ADD     1             X
040700950404     C*
040800950404     C* MEMORIZZO PER POI RICHIAMARE IL RICALCOLO TEMPI DI CONSEGNA
040900950404     C                   MOVE      '1'           W001A             1
041000930517     C*
041100930603DO  5C     NSC(X)        DOWGT     *ZEROS
041200930517     C                   Z-ADD     ARBFLS        KLNP
041300930517     C                   Z-ADD     ARBLNA        KLNA
041400930517     C                   Z-ADD     ARBNRS        KNRS
041500930517     C                   Z-ADD     NSC(X)        KNSC
041600930517     C*
041700930517     C* LEGGO SEGNACOLLO PER REPERIRE LA LNP DEL FOGLIO
041800070123     C     KBRV          SETLL     FNBRV09L
041900070123     C     KBRV          READE     FNBRV09L                               35
042000930517     C*
042100930517     C* F.V. DELLA LNA DELLA BOLLA
042200941102    6C     *IN35         DOWEQ     *OFF
042300970730     C                   CLEAR                   DSLV53
042400970730     C                   MOVEL     'A'           D53TFO
042500040331     c* campo non più usato
042600040331     C***                Z-ADD     SIMFEL        D53FEL
042700970730     C                   Z-ADD     BRVFGS        D53FGS
042800970730     C                   Z-ADD     BRVNFV        D53NFV
042900970730     C                   MOVEL     '1'           D53NPG
043000970730     C                   MOVEL     BRVFLE        D53FLF
043100970730     C                   MOVEL     'A'           D53ABB
043200970730     C                   MOVEL     ARBLNA        D53LNA
043300050603     c* non lo passo perchè potrebbe essere cambiato (ad esempio se collo
043400050603     c* impato o transitato)
043500050603     C***                MOVEL     ARBTFA        D53TFA
043600970730     C                   CALL      'FNLV53R'
043700970730     C                   PARM                    DSLV53
043800930517     C*
043900970730IF  7C     D53ERR        IFEQ      ' '
044000930517     C*
044100940613     C* MEMORIZZO DATA E ORA + RECENTI SOLTANTO SE:
044200940613     C*  IL FV PARTENZA E' STATO ABBINATO A UN FOGLIO ARRIVI
044300950427     C*  CON DATA >= ALLA DATA DI ARRIVO DEL COLLO
044400940613     C*
044500050510     c* Se data arrivo aggiornata con spunta partenza, data foglio arrivi
044600050510     c*  deve essere > (e non uguale) alla stessa data
044700050510     c**
044800050510   7AC**   D53DFA        IFGE      SAVDAM
044900050510     c**
045000050510     c                   if        (savfl2=' ' and d53dfa >=SAVDAM) or
045100050510     c                             (savfl2<>' ' and d53dfa>SAVDAM)
045200940613     C*
045300930517     C* MEMORIZZO SOLO SE LA DATA E L'ORA SONO PIU RECENTI
045400970730IF  8C     D53AED        IFGE      SAVAED
045500970730IF  9C     D53AED        IFGT      SAVAED
045600970730     C                   Z-ADD     D53AED        SAVAED
045700970730     C                   Z-ADD     D53AEH        SAVAEH
045800930607X   9C                   ELSE
045900970730IF 10C     D53AEH        IFGT      SAVAEH
046000970730     C                   Z-ADD     D53AEH        SAVAEH
046100930607E  10C                   ENDIF
046200930607E   9C                   ENDIF
046300930603E   8C                   ENDIF
046400940613     C*
046500940613E  7AC                   ENDIF
046600940613E   7C                   ENDIF
046700930528     C*
046800070123     C     KBRV          READE     FNBRV09L                               35
046900941102E   6C                   ENDDO
047000930517     C*
047100930528     C                   ADD       1             X
047200930603E   5C                   ENDDO
047300940613     C*
047400930603     C* SE DATA ARRIVO PRESENTE IN TUTTI I COLLI DELLA BOLLA
047500950627    5C     SAVAED        IFGT      0
047600950627    6C     BOLAED        IFLT      SAVAED
047700950627     C     BOLAED        OREQ      SAVAED
047800950619     C     BOLAEH        ANDLT     SAVAEH
047900930603     C                   Z-ADD     SAVAED        ARBDTI
048000930603     C                   Z-ADD     SAVAEH        ARBHTI
048100950627   X6C                   ELSE
048200950619     C                   Z-ADD     BOLAED        ARBDTI
048300950619     C                   Z-ADD     BOLAEH        ARBHTI
048400950627E   6C                   ENDIF
048500950627E   5C                   ENDIF
048600950619E   4C                   ENDIF
048700930517     C*
048800931213     C*
048900940613     C* SE  ARBDTI = 0  CERCO NEI FV PART ABBINATI AL FOGLIO ARRIVI
049000940613     C*                 CON DATA = ARTDAM
049100940613    4C   17ARBDTI        IFEQ      0
049200931213     C*
049300931213     C                   CLEAR                   COMDTI
049400931213     C                   CLEAR                   COMHTI
049500020918      * cerco l'As del terminal di arrivo e della linea di arrivo della bolla
049600050510     c* Ora stesso AS: non serve più
049700050510     c***                Exsr      RicAs
049800940613     C*
049900931213     C                   EXSR      RICFV
050000931213     C*
050100931213     C* MEMORIZZO I VALORI TROVATI IN  ARBDTI  E  ARBHTI
050200950619     C* CONFRONTANDO CON LA DATA DI ARRIVO DEL FOGLIO DELLA BOLLA
050300950619     C*
050400950619    5C     BOLAED        IFLT      COMDTI
050500950619    5C     BOLAED        OREQ      COMDTI
050600950619     C     BOLAEH        ANDLT     COMHTI
050700950619     C                   Z-ADD     COMDTI        ARBDTI
050800950619     C                   Z-ADD     COMHTI        ARBHTI
050900950619   X5C                   ELSE
051000950619     C                   Z-ADD     BOLAED        ARBDTI
051100950619     C                   Z-ADD     BOLAEH        ARBHTI
051200950619E   5C                   ENDIF
051300940613    4C                   ENDIF
051400931213     C*
051500930603E   3C                   ENDIF
051600930603E   2C                   ENDIF
051700930603     C***
051800930603     C*** V O L U M E   C M L
051900930603     C***
052000930504IF  2C     *IN20         IFEQ      '1'
052100941031     C     TOTVUC        ANDGT     0
052200941102     C*
052300941102     C* IMPOSTO IL VOLUME CML
052400941102     C                   Z-ADD(H)  TOTVUC        D20VLU
052500950110     C     D20VLU        IFEQ      0
052600950110     C                   Z-ADD     0,001         D20VLU
052700950110     C                   ENDIF
052800930503     C*
052900941031     C                   MOVEL     'A'           D20TBO
053000990921     C* NON DEVO TRASMETTERE LA VARIAZIONE DI VOLUME CML SE
053100990921     C* I VOLUMI SONO DELLA PARTENZA PERCHE' IN QUESTO CASO
053200990921     C* LA TRASMISSIONE ALLA SEDE E' GIA' STATA FATTA DALLA PARTENZA
053300990921     C* (V. TABELLA 7T TRASMISSIONE VOLUMI 'T' E 'Z' DA PARTENZA A
053400990921     C* SEDE --> TALI FLAG SONO IMPOSTATI AD 'S')
053500990921     C* ATTENZIONE QUINDI CHE I FLAG DELLA 7T DIVENTADO FISSO (NON
053600990921     C* MODIFICABILI) ALTRIMENTI NON FUNZIONA PIU' QUESTO GIRO
053700990921     C* COL RISCHIO DI PERDERE DEI VOLUMI CML IN SEDE
053800990921     C     WFVC          IFNE      *BLANKS
053900970901     C                   MOVEL     'S'           D20FTR
054000990921     C                   ELSE
054100990921     C                   MOVEL     ' '           D20FTR
054200990921     C                   ENDIF
054300941031     C                   MOVEL     'C'           D20TVL
054400941031     C                   MOVEL     ARBFVF        D20FVF
054500941031     C                   MOVEL     ARBVLF        D20VLF
054600941031     C                   MOVEL     ARBFVB        D20FVB
054700941031     C                   MOVEL     ARBVLB        D20VLB
054800941031     C                   MOVEL     ARBAAS        D20AAS
054900941031     C                   MOVEL     ARBLNP        D20LNP
055000941031     C                   MOVEL     ARBNRS        D20NRS
055100941031     C                   MOVEL     ARBNSP        D20NSP
055200941031     C                   MOVEL     ARBCBO        D20CBO
055300941031     C                   MOVEL     ARBNCL        D20NCL
055400991116     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
055500991116     C                   MOVEL     '1'           WTRC
055600991116     C     KAR6          CHAIN     FIAR6000                           30
055700941102     C  N30              MOVEL     AR6DFT        D20DFT
055800941031     C                   MOVEL     ARBLNA        D20LNA
055900960927     C*
056000960927     C                   MOVE      ARBDBR        D20DTE
056100941031     C                   MOVEL     DSLV20        KPJBU
056200040510     C* Passo parametri in più se pgm "gufo"
056300111109     c                   if        %parms>=2
056400990806     C                   CALL      'FNLV20R'
056500990806     C                   PARM                    KPJBA
056600040510     c                   parm      'S'           pwstor            1
056700040510     c                   else
056800040510     C                   CALL      'FNLV20R'
056900040510     C                   PARM                    KPJBA
057000040510     c                   endif
057100990806     C*
057200990806     C                   MOVEL     KPJBU         DSLV20
057300990806     C*
057400990806     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
057500990806    4C     D20ERR        IFNE      '1'
057600990806     C                   MOVEL     D20NCR        ARBNCR
057700990806     C                   MOVEL     D20VLU        ARBVLC
057800990806     C                   MOVEL     D20FVF        ARBFVF
057900990806     C                   MOVEL     D20VLF        ARBVLF
058000990806     C**
058100990806   X4C                   ELSE
058200990806     C                   MOVEL     'V'           WFBS              1
058300990806E   4C                   ENDIF
058400990806E   2C                   ENDIF
058500990806     C*
058600990806      * DS PER PESO
058700990806     C*** P E S O    C M L
058800990806     C***
058900990806IF  2C     *IN21         IFEQ      '1'
059000990806     C     TOTPUC        ANDGT     0
059100990806     C*
059200990806     C* IMPOSTO IL VOLUME CML
059300990806     C                   Z-ADD(H)  TOTPUC        D39PLU
059400990806     C     D39PLU        IFEQ      0
059500990806     C                   Z-ADD     0,001         D39PLU
059600990806     C                   ENDIF
059700990806     C                   MOVEL     'A'           D39TBO
059800990921     C* PER EVITARE DI TRASMETTERE SEMPRE IL PESO CML ALLA SEDE
059900990921     C* ABBIAMO IMPOSTATO AD 'S' IL FLAG DI TRASMISSIONE PESO CML
060000990921     C* DA PARTENZA A SEDE  (V.TABELLA 7Y) E IN ARRIVO LA TRASMISSIONE
060100990921     C* VERRA' EFFETTUATA SOLO SE IL PESO NON E' DELLA PARTENZA
060200990921     C* ATTENZIONE QUINDI CHE IL FLAG DELLA 7Y DIVENTA FISSO (NON
060300990921     C* MODIFICABILE) ALTRIMENTI NON FUNZIONA PIU' QUESTO GIRO
060400990921     C* COL RISCHIO DI PERDERE DEI PESI CML IN SEDE
060500990921     C     WFPC          IFNE      *BLANKS
060600990921     C                   MOVEL     'S'           D39FTR
060700990921     C                   ELSE
060800990921     C                   MOVEL     ' '           D39FTR
060900990921     C                   ENDIF
061000990806     C                   MOVEL     'C'           D39TPP
061100990806     C                   MOVEL     ARBPKF        D39PLF
061200990806     C                   MOVEL     ARBPKB        D39PLB
061300990806     C                   MOVEL     ARBAAS        D39AAS
061400990806     C                   MOVEL     ARBLNP        D39LNP
061500990806     C                   MOVEL     ARBNRS        D39NRS
061600990806     C                   MOVEL     ARBNSP        D39NSP
061700990806     C                   MOVEL     ARBCBO        D39CBO
061800990806     C                   MOVEL     ARBNCL        D39NCL
061900931213     C*
062000990806     C  N30              MOVEL     AR6DFT        D39DFT
062100990806     C                   MOVEL     ARBLNA        D39LNA
062200990806     C                   MOVE      ARBDBR        D39DTE
062300990806     C*
062400990806     C                   MOVEL     DSLV39        KPJBU
062500040510     C                   CALL      'FNLV39R'
062600040510     C                   PARM                    KPJBA
062700941031     C*
062800990806     C                   MOVEL     KPJBU         DSLV39
062900941031     C*
063000970616     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
063100990806    4C     D39ERR        IFNE      '1'
063200990806     C                   MOVEL     D39NCP        ARBNCP
063300990806     C                   MOVEL     D39PLU        ARBPKC
063400990806     C                   MOVEL     D39PLF        ARBPKF
063500970924     C**
063600970924   X4C                   ELSE
063700070704     c                   if        wfbs=' '
063800990806     C                   MOVEL     'P'           WFBS              1
063900070704     c                   else
064000070704     C                   MOVEL     'E'           WFBS              1
064100070704     c                   endif
064200070704     c
064300941102E   4C                   ENDIF
064400990806     C                   ENDIF
064500950323     C*
064600950323     C* AGGIORNO FILE TASSAZIONE PADRONCINI
064700990806     C     *IN20         IFEQ      *ON
064800040205     C     *IN21         oreq      *ON
064900950323     C                   EXSR      AGGTAS
065000070703     c* se non sono riuscita ad aggiornare i padroncini, reimposto
065100070703     c*  per elaborare
065200070703     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
065300070704    4C     o57err        IFNE      ' '
065400070704     c* Se modificato il peso
065500070704    5c                   if        *in21
065600070703     C                   MOVEL     savNCP        ARBNCP
065700070703     C                   MOVEL     savPKC        ARBPKC
065800070703     C                   MOVEL     savPKF        ARBPKF
065900070703     C                   MOVEL     'P'           WFBS              1
066000070704E   5C                   ENDIF
066100070704     c* Se modificato il volume
066200070704    5c                   if        *in20
066300070704     C                   MOVEL     savNCR        ARBNCR
066400070704     C                   MOVEL     savvlc        ARBVLC
066500070704     C                   MOVEL     savFVF        ARBFVF
066600070704     C                   MOVEL     savVLF        ARBVLF
066700070704    6c                   if        wfbs=' '
066800070704     C                   MOVEL     'V'           WFBS              1
066900070704     c                   else
067000070704     C                   MOVEL     'E'           WFBS              1
067100070704    6c                   endif
067200070704E   5C                   ENDIF
067300070704     c
067400070704E   4C                   ENDIF
067500070704    3C                   END
067600930503     C*
067700070703     C* Aggiorno bolla
067800970924     C                   MOVEL     WFBS          ARBFBS
067900970924     C                   CLEAR                   WFBS
068000930503     C*
068100941031     C                   UPDATE    FNARB000
068200950404     C*
068300950404     C     W001A         IFNE      *BLANKS
068400950404     C     ARBDCM        ANDNE     *ZEROS
068500950404     C     ARBDCE        ANDNE     *ZEROS
068600950404     C                   EXSR      SRTCON
068700950404     C                   END
068800111114     C*
068900941031     C                   READ      FNARB000                             1031
069000930504     C   10              SETON                                        31
069100930504E   1C                   ENDDO
069200040401     c*
069300111109     c* chiudo sempre in LR
069400111109     c                   seton                                        LR
069500111109     c
069600040505     c* Se passato un parametro solo chiudo con LR
069700040505    1c                   if        %PARMs<2
069800040505     C                   SETON                                        LR
069900111109     c                   clear                   Sitrasm           1
070000040505   x1c                   else
070100111109
070200040505     C* CONTROLLA SE E' STATA RICHIESTA LA CHIUSURA DEL SOTTOSISTEMA
070300040505     C                   SHTDN                                        98
070400040505     c
070500040505    2c                   if        *in98
070600040505     c                   eval      chiudi='S'
070700040505     C                   SETON                                        LR
070800111114     c*****              eval      SItrasm='S'
070900040505   x2c                   else
071000111109     c
071100111109     c* Imposto quello passato
071200111109     c                   movel     TrasmiVP      Sitrasm
071300040505     c
071400040505     c* Se non sono passate 2 ore accendo RT altrimenti LR
071500040505     c*  e sottometto trasmissione
071600111109     c**                 time                    worad
071700111109     c**   worad         subdur    worap         minuti:*MN
071800111109    3c**                 if        minuti>120
071900111109     C**                 SETON                                        LR
072000111109     c**                 eval      SItrasm='S'
072100111109   x3c**                 else
072200111006     C*******            SETON                                        RT
072300111109     c*
072400111109     c* chiudo il LR ma non trasmetto pesi e volumi
072500111109     C**                 SETON                                        LR
072600111109    3c**+                endif
072700111109     c
072800041104    2c                   endif
072900041104     c
073000111109     c*
073100041104    2c                   if        *inlr
073200041104     c* chiudo i pgm del peso e volume
073300041104     C                   CLEAR                   DSLV39
073400041104     C                   MOVEL     'C'           D39TLA
073500041104     C                   MOVEL     DSLV39        KPJBU
073600041104     C                   CALL      'FNLV39R'
073700041104     C                   PARM                    KPJBA
073800041108     c*
073900041108     C                   CLEAR                   DSLV20
074000041108     C                   MOVEL     'C'           D20TLA
074100041108     C                   MOVEL     DSLV20        KPJBU
074200041108     C                   CALL      'FNLV20R'
074300041108     C                   PARM                    KPJBA
074400041104     c
074500111006     c                   if        SItrasm='S'
074600040505     c                   eval      pa3nfv='99999'
074700070502     c                   eval      pa3tfp='   '
074800040505     c                   movel     param3        kpjbu
074900040505     c                   eval      trasmpesvol='S'
075000040505     c                   call      'FNLSA3R'
075100040505     c                   parm                    kpjba
075200040505     c                   parm                    trasmpesvol       1
075300111114     c
075400111114     c                   clear                   Sitrasm           1
075500040505    2c                   endif
075600111006     c
075700111006    2c                   endif
075800040505    1c                   endif
075900040505     C**
076000040505     c                   if        *inlr
076100040505     C                   CLEAR                   DSLV57
076200040505     C                   MOVEL     'C'           I57TLA
076300040505     C                   CALL      'FNLV57R'
076400040505     C                   PARM                    KPJBA
076500040505     C                   PARM                    DSLV57
076600040505     C*
076700040505     C                   MOVEL     'C'           D55TLA
076800040505     C                   CALL      'FNLV55R'
076900040505     C                   PARM                    DSLV55
077000111010     C*
077100111010     C                   MOVEL     'C'           D53TLA
077200111010     C                   CALL      'FNLV53R'
077300111010     C                   PARM                    DSLV53
077400040505    1c                   endif
077500931210     C*---------------------------------------------------------------*
077600931210     C*                    RICFV                                      *
077700931210     C* ROUTINE DI RICERCA FOGLIO VIAGGIO PARTENZA                    *
077800931210     C*---------------------------------------------------------------*
077900931210     C     RICFV         BEGSR
078000050510     c                   eval      Wgiro = *off
078100050510     C                   Z-ADD     2             COMNPG
078200050510
078300050510      * Devo leggere fnfvv con terminal di arrivo bolla nel caso in cui
078400050510      * il terminal di arrivo della bolla e la linea di arrivo della bolla sono sullo
078500050510      * stesso As (ora sempre)
078600050510     c**                 If        TfaOgas = LnaOgas
078700050510     c                   Eval      Kfgs = ArbTfa
078800050510     c**                 Else
078900050510      * se non sono sullo stesso As leggo fnfvv con linea di arrivo della bolla
079000050510     c**                 Eval      Kfgs = ArbLna
079100050510     c**                 EndIf
079200050510     c
079300050510     c* Se bolla aggioranta con spunte processo ope partenza, devo
079400050510     c*  cercare un foglio arrivi con data > di SAVDAM
079500050510     c                   if        savfl2=' '
079600050510     c                   eval      kdam=savdam
079700050510     c                   else
079800050510     c*
079900050510     C     KFVV08        SETgt     FNFVV08
080000050510     C     KFVV08b       READE     FNFVV08                                32
080100050510     c                   if        not *in32
080200050510     c                   eval      kdam=fvvdfv
080300050510     c                   else
080400050510     c
080500050510     c* Foglio arrivi non trovato
080600050510     c* imposto SAVDAM+1
080700050510     c     *iso          movel     savdam        dataiso
080800050510     c                   adddur    1:*d          dataiso
080900050510     c                   move      dataiso       comdti
081000050510     C                   Z-ADD     0101          COMHTI
081100050510     c                   goto      endric
081200050510     c                   endif
081300050510     c
081400050510     c                   endif
081500020919
081600020919      * imposto un flag di comodo x fare il secondo tentativo con lo stesso npg
081700020919     c     rileggi       tag
081800940613     C*
081900940613     C* NEL 1 GIRO
082000050510     C* CERCO IL FOGLIO ARRIVI CON  NPG=2  E  DFV=SAVDAM o >savdam
082100940613     C* NEL 2 GIRO
082200050510     C* CERCO IL FOGLIO ARRIVI CON  NPG=6  E  DFV=SAVDAM o >savdam
082300050914     C* SE NON L'HO TROVATO AL 1 GIRO (non più)
082400050914     C****               Z-ADD     1             Y                 2 0
082500940613     C*
082600050914   -1C**   Y             DOWLE     2
082700050914     C**   COMDTI        ANDEQ     0
082800940613     C*
082900941102     C     KFVV          SETLL     FNFVV000
083000941102     C     KFVV          READE     FNFVV000                               32
083100940613     C*
083200940613    0C     *IN32         DOWEQ     *OFF
083300940613     C*
083400940613     C* CERCO IL F.VIAGGIO PARTENZA (ABBINATO SUL FOGLIO ARRIVI TROVATO
083500940613     C*   CHE CONTIENE LA LINEA DEL COLLO CON ARTDAM PIU'  RECENTE
083600931215     C*
083700940613     C* LEGGO FLFVA02L CON         NFA=FVVNFV
083800941102     C     KFVA2         SETLL     FNFVA02
083900941102     C     KFVA2         READE     FNFVA02                                33
084000931210     C*
084100931213    1C     *IN33         DOWEQ     *OFF
084200970708     C                   CLEAR                   DSLV53
084300970708     C                   MOVEL     'A'           D53TFO
084400970708     C                   MOVEL     FVANFV        D53NFV
084500970708     C                   MOVEL     1             D53NPG
084600970708     C                   MOVEL     FVALNP        D53FGS
084700970708     C                   MOVEL     FVALAI        D53LAI
084800031212     C                   MOVEL     'B'           D53ABB
084900970708     C                   MOVEL     ARBLNA        D53LNA
085000050603     C****               MOVEL     ARBTFA        D53TFA
085100970708     C                   CALL      'FNLV53R'
085200970708     C                   PARM                    DSLV53
085300970730     C**
085400970730     C* LEGGO QUESTO FNFVA SOLO SE IL PGM HA TROVATO CHE E' UN
085500970730     C*  FOGLIO DELLA LINEA DI ARRIVO
085600970730    2C     D53ERR        IFEQ      ' '
085700050914     C* SE FVALNP = TERMINAL PART. BOLLA
085800970730     C     FVALNP        ANDEQ     ARBTFP
085900050914     c                   if        comdti=0 or (comdti<fvaaed) or
086000050914     c                             (comdti=fvaaed and fvaaeh>comhti)
086100931214     C                   Z-ADD     FVAAEH        COMHTI
086200940613     C                   Z-ADD     FVAAED        COMDTI
086300050914     c                   endif
086400050913     c
086500050913   X2C***                else
086600050913    2C                   ENDIF
086700970730     C* RILEGGO
086800050914     C     KFVA2         READE     FNFVA02                                33
086900050913    2C***                ENDIF
087000931213    1C                   ENDDO
087100940613     C*
087200941102     C     KFVV          READE     FNFVV000                               32
087300940613     C*
087400940613     C* SE GIA' TROVATO NON LEGGO PIU'
087500050914     c*  non lo faccio più perchè tengo la data + alta quindi devo leggere
087600050914     c*  tutto
087700050914     C**   COMDTI        IFGT      0
087800050914     C**                 SETON                                        32
087900050914     C**                 ENDIF
088000940613    0C                   ENDDO
088100940613     C*
088200050914     c* tolgo la cat. 6 non più usata
088300050914     C*****              ADD       1             Y
088400050914     C*****              Z-ADD     6             COMNPG
088500050914   -1C*****              ENDDO
088600020919
088700050913      * Se non ho trovato e terminal di arrivo e linea di arrivo della bolla
088800020919      * sono su stesso AS devo fare un altro tentativo con la linea di arrivo della bolla
088900050510     c***                If        Comdti = *zeros and TfaOgas = LnaOgas
089000050510     c                   If        Comdti = *zeros
089100020919     c                             and Wgiro = *off and
089200020919     c                             arbtfa<>arblna
089300020919     c                   Eval      Kfgs = ArbLna
089400020919     c                   Eval      Wgiro = *on
089500020919     c                   GoTo      Rileggi
089600020919     c                   EndIf
089700931210     C*
089800940613     C     COMDTI        IFEQ      0
089900931210     C                   Z-ADD     0101          COMHTI
090000050510     C                   Z-ADD     kdam          COMDTI
090100931210     C                   ENDIF
090200931210     C*
090300050510     C     endric        ENDSR
090400940208     C*---------------------------------------------------------------*
090500980114     C* CONTROLLO SE CALCOLARE LA DATA ARRIVO ULTIMO COLLO
090600940208     C*---------------------------------------------------------------*
090700940208     C     RIC2          BEGSR
090800940613     C* SE COLLO NON ARRIVATO NON POSSO CALCOLARE LA DATA E ORA
090900940613     C*  ARRIVO ULTIMO COLLO
091000940613    1C     ARTDAM        IFEQ      0
091100940613     C                   SETOFF                                       17
091200941102   X1C                   ELSE
091300940208     C*
091400950123     C* ARTABN = "S" COLLO DI FV PARTENZA ABBINATO
091500111229     c* se la bolla è consegnata anche se manca ARTABN calcolo lo stesso
091600950123    2C     ARTABN        IFNE      'S'
091700111229     c     artdcm        andeq     0
091800111229     c
091900980114     C* NON LO CALCOLO SE C'E' LA DATA DI PARTENZA E QUINDI LA SPUNTA
092000111229     C*  NON ANCORA ABBINATA.
092100980114    3C     ARTDFV        IFGT      0
092200980114     C     ARTFLP        ANDEQ     0
092300980114     C     ARTDUT        ORGT      0
092400980114     C     ARTFLP        ANDGT     0
092500980114     C                   SETOFF                                       17
092600980114    3C                   ENDIF
092700980114    2C                   ENDIF
092800980114    1C                   ENDIF
092900940208     C*
093000940208     C                   ENDSR
093100950323     C*---------------------------------------------------------------*
093200950323     C*                    AGGTAS                                     *
093300950323     C* AGGIORNO FILE TASSAZIONE PADRONCINI                           *
093400950323     C*---------------------------------------------------------------*
093500950323     C     AGGTAS        BEGSR
093600950323     C*
093700970616     C* MODIFICATO VOLUME CML: SISTEMO IL VOLUME ANCHE NEI PADRONCINI
093800950323    1C     ARBVLC        IFNE      SAVVLC
093900040205    1C     ARBPKC        ORNE      SAVPKC
094000970924     C                   CLEAR                   DSLV57
094100970924     C                   MOVEL     'A'           I57TBO
094200970924     C                   MOVEL     ARBAAS        I57AAS
094300970924     C                   MOVEL     ARBLNP        I57LNP
094400970924     C                   MOVEL     ARBNRS        I57NRS
094500970924     C                   MOVEL     ARBNSP        I57NSP
094600970924     C                   MOVEL     ARBPDC        I57PDR
094700970924     C                   MOVEL     ARBNCL        I57NCL
094800970924     C                   MOVEL     'C'           I57TVL
094900970924     C                   MOVEL     SAVNCR        I57NCR
095000970924     C                   MOVEL     SAVVLC        I57VLC
095100990920     C                   MOVEL     SAVVLF        I57VLF
095200970924     C                   MOVEL     ARBVLC        I57VCN
095300970924     C                   MOVEL     ARBNCR        I57NCN
095400970924     C                   MOVEL     ARBVLF        I57VFN
095500040205     C                   MOVEL     SAVNCp        I57nCp
095600040205     C                   MOVEL     SAVpkC        I57pkC
095700040205     C                   MOVEL     SAVpkF        I57pkF
095800040205     C                   MOVEL     arbNCp        I57npn
095900040205     C                   MOVEL     arbpkC        I57pcn
096000040205     C                   MOVEL     arbpkF        I57pfn
096100970924     C                   CALL      'FNLV57R'
096200970924     C                   PARM                    KPJBA
096300970924     C                   PARM                    DSLV57
096400970924     C                   ENDIF
096500970616     C*
096600950323     C                   ENDSR
096700950404     C*---------------------------------------------------------------*
096800950404     C*                    SRTCON                                     *
096900950404     C* RICALCOLO TEMPI DI CONSEGNA                                   *
097000950404     C*---------------------------------------------------------------*
097100950404     C     SRTCON        BEGSR
097200950404     C* SE SPEDIZIONE GIà CONSEGNATA E GIA' CALCOLATI I TEMPI DI
097300950404     C* CONSEGNA LI RICALCOLO: I TEMPI POTEVANO ESSERE STATI CALCOLATI
097400950404     C* QUANDO ANCORA NON ERANO STATI ABBINATI I F.V. E QUINDI OCCORRE
097500950404     C* RICALCOLARLI PER AVERE I TEMPI GIUSTI.
097600970925     C                   CLEAR                   DS99
097700950404     C                   MOVEL     ' '           D98TLA
097800950404     C                   MOVEL     'A'           D98TBO
097900950404     C                   Z-ADD     ARBAAS        D98AAS
098000950404     C                   Z-ADD     ARBLNP        D98LNP
098100950404     C                   Z-ADD     ARBNRS        D98NRS
098200950404     C                   Z-ADD     ARBNSP        D98NSP
098300950404     C*
098400970925     C                   CALL      'TNSD99R'
098500970925     C                   PARM                    DS99
098600950404     C* SE TEMPI DIVERSI
098700950404     C     D98DCE        IFNE      ARBDCE
098800950404     C     D98NPI        ORNE      ARBNPI
098900950404     C     D98NCI        ORNE      ARBNCI
099000950404     C     D98NRC        ORNE      ARBNRC
099100950404     C     KARB          CHAIN     FNARB1                             31
099200960906     C* Determino se tipo bolla deve pervenire in sede (*IN08 OFF)
099300960906     C                   SETOFF                                       08
099400960906     C                   MOVEL     '3A'          COD
099500960906     C                   MOVEL(P)  ARBCBO        KEY
099600960906     C     KTAB          CHAIN     TABEL00F                           38
099700960906     C     *IN38         IFEQ      *OFF
099800960906    6C     TBLFLG        ANDEQ     ' '
099900960906     C                   MOVEL     TBLUNI        DS3A
100000960906     C     §3ABSD        COMP      ' '                                0808
100100960906     C                   END
100200950404     C* Richiamo routine di Addpfm/Allocazione/Ovrdbf
100300950404     C                   EXSR      SRARBC
100400950404     C     *IN05         IFEQ      *OFF
100500971120     C                   OPEN      FNARBC0T
100600010103     C  N08              OPEN      FIARBC46
100700950404     C                   Z-ADD     D98DCE        ARBDCE
100800950404     C                   Z-ADD     D98NPI        ARBNPI
100900950404     C                   Z-ADD     D98NCI        ARBNCI
101000950404     C                   Z-ADD     D98NRC        ARBNRC
101100950404     C                   MOVEL     'C'           ARBFAG
101200971120     C                   WRITE     FNARBC00
101300010103     C  N08              WRITE     FIARBC00
101400971120     C                   CLOSE     FNARBC0T
101500010103     C  N08              CLOSE     FIARBC46
101600950404     C* Richiamo routine di Disallocazione/Dltovr
101700100915     c                   eval      soloarbc0t=' '
101800950404     C                   EXSR      DISALL
101900100915
102000950404     C                   ELSE
102100960125     C                   MOVEL     'S'           ARBFBS
102200950404     C                   MOVE      *ZEROS        ARBDTI
102300950404     C                   MOVE      *ZEROS        ARBHTI
102400950404     C                   END
102500950404     C*
102600950404     C                   UPDATE    FNARB1
102700950405     C*
102800950405     C                   END
102900950404     C*
103000950404     C                   ENDSR
103100950403     C*---------------------------------------------------------------*
103200950403     C*                    SRARBC                                     *
103300950403     C* ADDPFM/ALLOCAZIONE/OPEN FILE DI TRASMISSIONE                  *
103400950403     C*---------------------------------------------------------------*
103500950403     C     SRARBC        BEGSR
103600950403     C*
103700950403     C                   MOVE      *BLANKS       $MBR
103800950403     C                   MOVE      'M'           EMME                            DESTINAZIONE
103900040401     C***                move      simfel        xsimfe
104000040513     c                   if        simfel=946
104100040401     C                   MOVE      '026'         XSIMFE
104200040513     c                   else
104300040513     C                   move      simfel        xsimfe
104400040513     c                   endif
104500971120     C*
104600971120     C                   CLEAR                   DSLV55
104700971120     C                   MOVEL     'P'           D55TPT
104800971120     C                   MOVE      ARBLNP        D55LIN
104900971120     C                   MOVE      DATEU         D55DRF
105000971120     C                   CALL      'FNLV55R'
105100971120     C                   PARM                    DSLV55
105200971120     C                   MOVE      D55TFP        $SIMFE
105300970224     C*
105400950403      * Richiamo a pgm per addpfm
105500950403     C                   MOVE      *BLANKS       FILE
105600971120     C                   MOVEL     'FNARBC0T'    FILE
105700950403     C                   CALL      'TRUL50C'
105800950403     C                   PARM                    FILE             10
105900950403     C                   PARM                    $MBR
106000950403     C                   PARM                    KNSIF
106100950403     C                   PARM                    LOGICO           10
106200950403     C                   PARM                    LOGMBR           10
106300950403     C                   PARM                    FLG               1
106400950403      * Tento l'allocazione del file di trasmissione
106500950403     C                   EXSR      ALLOCA
106600950403      * Proseguo solo se riuscita l'allocazione
106700950404     C  N05              DO
106800950403      * OVRDBF e apertura del file
106900950403     C                   MOVE      COM(1)        $COMOD                         DI COMODO
107000950403     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
107100950403     C                   MOVEA     FILE          CMD(13)
107200950403     C                   MOVEA     $MBR          CMD(29)
107300950403     C                   MOVEA     CMD           COMMAN
107400950403     C                   Z-ADD     80            LUNG
107500950403     C                   CALL      'QCMDEXC'
107600950403     C                   PARM                    COMMAN
107700950403     C                   PARM                    LUNG
107800980720     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
107900980720     C                   MOVEA     FILE2         CMD(13)
108000980720     C                   MOVEA     BLANK         CMD(29)
108100980720     C                   MOVEA     CMD           COMMAN
108200980720     C                   Z-ADD     80            LUNG
108300980720     C                   CALL      'QCMDEXC'
108400980720     C                   PARM                    COMMAN
108500980720     C                   PARM                    LUNG
108600950403     C                   ENDDO
108700950403     C*
108800950403     C                   ENDSR
108900950403      **************************************************************************
109000950403      * Allocazione di un archivio (FILE)
109100950403      **************************************************************************
109200950403     C     ALLOCA        BEGSR
109300950403     C*
109400971120     C* ALLOCAZIONE FNARBC0T
109500950403     C                   MOVE      COM(2)        $COMOD           80            DI COMODO
109600950403     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
109700950403     C                   MOVEA     FILE          CMD(13)
109800950403     C                   MOVEA     $MBR          CMD(38)
109900950403     C                   MOVEA     CMD           COMMAN
110000950403     C                   Z-ADD     80            LUNG
110100950404     C                   CALL      'QCMDEXC'                            05
110200950403     C                   PARM                    COMMAN           80
110300950403     C                   PARM                    LUNG             15 5
110400010103     C* ALLOCAZIONE FIARBC46
110500950404     C     *IN05         IFEQ      *OFF
110600960906     C     *IN08         ANDEQ     *OFF
110700950405     C                   MOVE      *BLANKS       FILE2            10
110800010103     C                   MOVEL     'FIARBC46'    FILE2
110900020927     C                   eval      BLANK = 'M' + xsimfe + '046'
111000950403     C                   MOVEA     FILE2         CMD(13)
111100950411     C                   MOVEA     BLANK         CMD(38)
111200950403     C                   MOVEA     CMD           COMMAN
111300950404     C                   CALL      'QCMDEXC'                            05
111400950403     C                   PARM                    COMMAN           80
111500950403     C                   PARM                    LUNG             15 5
111600100915     c* Se allocazione non riuscita disalloco fnarbc0t
111700100915     c                   eval      soloarbc0t='S'
111800100915     c                   exsr      disall
111900100915     c                   eval      soloarbc0t=' '
112000950403     C                   END
112100950403      *
112200950403     C                   ENDSR
112300950404      **************************************************************************
112400950404      * Dssallocazione di un archivio (FILE) con membro ($MBR)
112500950404      **************************************************************************
112600950404     C     DISALL        BEGSR
112700950404      *
112800950404     C                   MOVE      COM(3)        $COMOD                         DI COMODO
112900950404     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
113000950404     C                   MOVEA     FILE          CMD(13)
113100950404     C                   MOVEA     $MBR          CMD(38)
113200950404     C                   MOVEA     CMD           COMMAN
113300950404     C                   Z-ADD     80            LUNG
113400950404     C                   CALL      'QCMDEXC'                            99
113500950404     C                   PARM                    COMMAN
113600950404     C                   PARM                    LUNG
113700950404     C*
113800100915     c                   if        not *in08 and soloarbc0t =' '
113900100915     C     *IN08         IFEQ      *OFF
114000950404     C                   MOVE      COM(3)        $COMOD                         DI COMODO
114100950404     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
114200950404     C                   MOVEA     FILE2         CMD(13)
114300950411     C                   MOVEA     BLANK         CMD(38)
114400950404     C                   MOVEA     CMD           COMMAN
114500950404     C                   Z-ADD     80            LUNG
114600950404     C                   CALL      'QCMDEXC'                            99
114700950404     C                   PARM                    COMMAN
114800950404     C                   PARM                    LUNG
114900960906     C                   END
115000971120      * DLTOVR di FNARBC0T
115100971120     C                   MOVEL(P)  COM(4)        COMMAN
115200950404     C                   Z-ADD     80            LUNG
115300100915     C                   CALL      'QCMDEXC'                            99
115400950404     C                   PARM                    COMMAN
115500950404     C                   PARM                    LUNG
115600100915     c                   endif
115700950404      *
115800950404     C                   ENDSR
115900041222     C*---------------------------------------------------------------*
116000041222     C* controllo abbinamento foglio bolla
116100041222     C*---------------------------------------------------------------*
116200041222     c     CTRARBNFV     begsr
116300041222     C                   CLEAR                   DSLV53
116400041222     C                   MOVEL     'A'           D53TFO
116500111130     C                   Z-ADD     ARBtfp        D53FGS
116600041222     C                   Z-ADD     ARBNFV        D53NFV
116700041222     C                   MOVEL     '1'           D53NPG
116800041222     C                   MOVEL     'B'           D53ABB
116900041222     C                   MOVEL     ARBLNA        D53LNA
117000041222     C                   MOVEL     ARBTFA        D53TFA
117100041222     C                   CALL      'FNLV53R'
117200041222     C                   PARM                    DSLV53
117300041222     C* SE NON C'E' ERRORE VUOL DIRE CHE HO TROVATO IL FOGLIO
117400041222    1C     D53ERR        IFEQ      ' '
117500041222    2C     D53AED        IFEQ      0
117600041222     C                   SETOFF                                       17
117700041222   X2C                   ELSE
117800041222     C                   MOVEL     D53AED        BOLAED
117900041222     C                   MOVEL     D53AEH        BOLAEH
118000041222    2C                   ENDIF
118100041222   X1C                   ELSE
118200041222     C* SE NON TROVATO FOGLIO --> NON METTO LA DATA ARRIVO ULTIMO COLLO
118300041222     C                   SETOFF                                       17
118400041222    1C                   ENDIF
118500041222     c*
118600041222     c                   eval      wctrarbnfv='S'
118700041222     c                   ENDSR
118800930430     C*---------------------------------------------------------------*
118900930430     C*                    DEFVAR                                     *
119000930430     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
119100930430     C*---------------------------------------------------------------*
119200040401     C     *inzsr        BEGSR
119300930430     C*
119400930430     C*
119500930430     C                   Z-ADD     1             CODUT             1 0
119600040513     C                   CALL      'X§PARUT'
119700040513     C                   PARM                    UT§DSE
119800040513     C                   MOVEL     REC80         CNCR80
119900930505     C*
120000941102     C                   Z-ADD     1             KNPG
120100971120     C*
120200971120     C* DETERMINO DATA DEL GIORNO
120300971120     C                   TIME                    W0140            14 0
120400971120     C                   MOVE      W0140         WDTGIO            8 0
120500971120     C                   CLEAR                   WLBDAT
120600971120     C                   Z-ADD     WDTGIO        G02DAT
120700971120     C                   CALL      'XSRDA8'
120800971120     C                   PARM                    WLBDAT
120900971120     C                   MOVEL     G02INV        DATEU             8 0
121000040505     c* Prendo ora di apertura pgm
121100040505     C                   time                    worap
121200930503     C*
121300931210     C* DEFINIZIONE DI ALCUNI CAMPI
121400001010     C     *LIKE         DEFINE    ARBLNP        SAVLNP
121500041216     C     *LIKE         DEFINE    ARBLNP        SAVLNA
121600001010     C     *LIKE         DEFINE    ARBLNA        WLNA
121700961129     C     *LIKE         DEFINE    ARBDTI        COMDTI
121800931210     C     *LIKE         DEFINE    ARBHTI        COMHTI
121900931210     C     *LIKE         DEFINE    FVANPG        COMNPG
122000941102     C     *LIKE         DEFINE    FVANPG        KNPG
122100941102     C     *LIKE         DEFINE    FVAAED        SAVAED
122200941102     C     *LIKE         DEFINE    FVAAEH        SAVAEH
122300950619     C     *LIKE         DEFINE    FVAAED        BOLAED
122400950619     C     *LIKE         DEFINE    FVAAEH        BOLAEH
122500941102     C     *LIKE         DEFINE    FVVDFV        SAVDAM
122600950323     C     *LIKE         DEFINE    ARBNCR        SAVNCR
122700950323     C     *LIKE         DEFINE    ARBVLC        SAVVLC
122800950323     C     *LIKE         DEFINE    ARBVLF        SAVVLF
122900070704     C     *LIKE         DEFINE    ARBfvF        SAVfvF
123000990806     C     *LIKE         DEFINE    ARBPKF        SAVPKF
123100990806     C     *LIKE         DEFINE    ARBPKC        SAVPKC
123200990806     C     *LIKE         DEFINE    ARBNCP        SAVNCP
123300950323     C     *LIKE         DEFINE    ARBVLF        WADDV
123400950323     C     *LIKE         DEFINE    ARBVLF        WSOTV
123500960906     C     *LIKE         DEFINE    TBLKEY        KEY
123600960906     C     *LIKE         DEFINE    TBLCOD        COD
123700990921     C     *LIKE         DEFINE    ARTFVC        WFVC
123800990921     C     *LIKE         DEFINE    ARTFPC        WFPC
123900991116     C     *LIKE         DEFINE    AR6TRC        WTRC
124000931210     C*
124100941031     C* LETTURA FNARB
124200930430     C     KARB          KLIST
124300930430     C                   KFLD                    ARBAAS
124400930430     C                   KFLD                    ARBLNP
124500930430     C                   KFLD                    ARBNRS
124600930430     C                   KFLD                    ARBNSP
124700991116     C*
124800991116     C     KAR6          KLIST
124900991116     C                   KFLD                    ARBAAS
125000991116     C                   KFLD                    ARBLNP
125100991116     C                   KFLD                    ARBNRS
125200991116     C                   KFLD                    ARBNSP
125300991116     C                   KFLD                    WTRC
125400041217     C     KAR4a         KLIST
125500041217     C                   KFLD                    ARBAAS
125600041217     C                   KFLD                    ARBLNP
125700041217     C                   KFLD                    ARBNRS
125800041217     C                   KFLD                    ARBNSP
125900041217     C                   KFLD                    ktrc
126000041222     C     KAnm          KLIST
126100041222     C                   KFLD                    ARtfls
126200041222     C                   KFLD                    ARtlna
126300041222     C                   KFLD                    ARtNRS
126400041222     C                   KFLD                    ARtNSc
126500041222     C                   KFLD                    kcaa
126600930517     C*
126700070123     C* LETTURA FNBRV09L
126800941102     C     KBRV          KLIST
126900941102     C                   KFLD                    KNPG
127000941102     C                   KFLD                    KLNP              3 0
127100960304     C                   KFLD                    KLNA              3 0
127200930517     C                   KFLD                    KNRS              2 0
127300930517     C                   KFLD                    KNSC              7 0
127400930517     C*
127500941102     C* LETTURA FNFVV02L
127600931210     C     KFVV          KLIST
127700931210     C                   KFLD                    COMNPG
127800050510     C                   KFLD                    kdam
127900020917     C                   KFLD                    Kfgs
128000050510     C     KFVV08        KLIST
128100050510     C                   KFLD                    Kfgs
128200050510     C                   KFLD                    COMNPG
128300050510     C                   KFLD                    savdam
128400050510     C     KFVV08b       KLIST
128500050510     C                   KFLD                    Kfgs
128600050510     C                   KFLD                    COMNPG
128700931210     C* LETTURA FLFVA02L
128800931210     C     KFVA2         KLIST
128900931210     C                   KFLD                    COMNPG
129000931210     C                   KFLD                    FVVNFV
129100020917     c                   Kfld                    FvvFgs
129200931210     C*
129300960304     C* LETTURA TABEL00F
129400960304     C     KTAB2         KLIST
129500960304     C                   KFLD                    CODUT
129600960906     C                   KFLD                    COD
129700960906     C     KTAB          KLIST
129800960906     C                   KFLD                    CODUT
129900960906     C                   KFLD                    COD
130000960906     C                   KFLD                    KEY
130100930430     C                   ENDSR
130200950404**  COM  -  COMANDI CL
130300950403OVRDBF FILE(          ) MBR(          )
130400950403ALCOBJ OBJ((           *FILE *SHRUPD           ))
130500950403DLCOBJ OBJ((           *FILE *SHRUPD           ))
130600950404DLTOVR FILE(FNARBC0T  )
