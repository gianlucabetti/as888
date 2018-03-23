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
011500040513     c
011600040513     c                   if        knsif=*blanks
011700040513     c                   eval      knsif='*LIBL'
011800040513     c                   endif
011900040505     c* Per pgm gufo, ogni 2 ore chiudo pgm trasmetto variazioni peso volume
012000040505     c*  e riapro il pgm
012100040505     c
012200930430     C*
012300040401     C     *loval        setll     FNARB000                             1031
012400040401     C                   READ      FNARB000                             1031
012500941031     C*
012600941031DO  1C  N10*IN31         DOWEQ     *OFF
012700930503     C*
012800990806     C                   SETOFF                                       152021
012900950404     C                   MOVE      *BLANKS       W001A
013000041222     C                   MOVE      *BLANKS       Wctrarbnfv        1
013100001010     C* VERIFICO SE SI TRATTA DI UNA BOLLA POSTE
013200001010     C     ARBLNP        IFNE      SAVLNP
013300001010     C     ARBLNP        CHAIN     AZORG01L                           38
013400001010     C  N38              MOVEL     ORGDE3        OG143
013500001010     C   38              CLEAR                   OG143
013600001010     C                   MOVEL     ARBLNP        SAVLNP
013700001010     C                   ENDIF
013800041216     C* VERIFICO SE LA LINEA DI ARRIVO FA IL PIKING
013900041216     C     ARBLNA        IFNE      SAVLNA
014000041216     C     ARBLNA        CHAIN     AZORG01L                           38
014100041216     C  N38              MOVEL     ORGDF0        OG150
014200041216     C   38              CLEAR                   OG150
014300041217     C                   MOVEL     ARBLNa        SAVLNA
014400041216     C                   ENDIF
014500041216     C
014600930517     C*
014700921231     C* 15 ON - DEVO ABILITARE LA MERCE ALLA CONSEGNA
014800930603     C     ARBFBS        IFEQ      'S'
014900970924     C     ARBFBS        OREQ      'E'
015000930603     C                   SETON                                        15
015100930603     C* PULISCO DATA E ORA DI COMODO
015200941102     C                   Z-ADD     *ZEROS        SAVAED
015300941102     C                   Z-ADD     *ZEROS        SAVAEH
015400930607     C                   Z-ADD     *ZEROS        SAVDAM
015500050510     c                   clear                   savfl2
015600940613     C                   CLEAR                   NFV
015700940613     C                   CLEAR                   NSC
015800050304     C                   CLEAR                   dut
015900930607     C                   Z-ADD     1             X
016000950629     C                   ENDIF
016100921231     C*
016200921231     C* 20 ON - DEVO CALCOLARE IL VOLUME CML SU BOLLA
016300970924IF  2C****       ARBNCR    IFEQ 99999
016400970924     C****       ARBNCR    OREQ 99998
016500970924     C     ARBFBS        IFEQ      'V'
016600970924     C     ARBFBS        OREQ      'E'
016700930504     C                   SETON                                            20
016800941102     C                   Z-ADD     *ZEROS        TOTVUC            8 6
016900950323     C* SALVO VOLUME DA FATTURARE, VOLUME CML E NUMERO COLLI RILEVATI
017000950323     C                   Z-ADD     ARBVLF        SAVVLF
017100950323     C                   Z-ADD     ARBVLC        SAVVLC
017200950323     C                   Z-ADD     ARBNCR        SAVNCR
017300070704     c                   movel     arbfvf        SAVFVF
017400950323     C*
017500941102     C                   CLEAR                   DSLV20
017600930517E   2C                   ENDIF
017700990806     C* 21 ON - DEVO CALCOLARE IL PESO CML SU BOLLA
017800990806     C     ARBFBS        IFEQ      'P'
017900990806     C     ARBFBS        OREQ      'E'
018000990806     C                   SETON                                            21
018100990806     C                   Z-ADD     *ZEROS        TOTPUC            9 3
018200990806     C* SALVO VOLUME DA FATTURARE, VOLUME CML E NUMERO COLLI RILEVATI
018300990806     C                   Z-ADD     ARBPKF        SAVPKF
018400990806     C                   Z-ADD     ARBPKC        SAVPKC
018500990806     C                   Z-ADD     ARBNCP        SAVNCP
018600990806     C*
018700990806     C                   CLEAR                   DSLV39
018800990806E   2C                   ENDIF
018900911011     C*
019000930603     C* ELABORO SE:
019100930603     C* 1) DEVO ABILITARE BOLLA ALLA CONSEGNA E LIVELLO ABILITAZIONE <2
019200930603     C* 2)  "       "       "     "     "     "   "         "       >"2
019300930603     C*     MA NON HO ANCORA CALCOLATO LA DATA E L'ORA DI ARRIVO EFFETT
019400930603     C* 3) DEVO CALCOLARE IL VOLUME CML
019500990920     C* 4) DEVO CALCOLARE IL PESO   CML
019600930603     C*
019700950629IF  2C     *IN15         IFEQ      *ON
019800950629     C     ARBAMA        ANDNE     2
019900001010     C* LA DATA ARRIVO ULTIMO COLLO NON LA CALCOLO PER BOLLE POSTE
020000950629     C     *IN15         OREQ      *ON
020100930603     C     ARBDTI        ANDEQ     0
020200020918     c     §OgNtw        andne     'PPT'
020300001010     C* VOLUME O PESO
020400950629     C     *IN20         OREQ      *ON
020500990920     C     *IN21         OREQ      *ON
020600940613     C                   SETOFF                                       0102
020700940613     C                   SETON                                        17
020800950619     C*  CONTROLLO SE IL FOGLIO SU CUI E' PARTITA LA BOLLA E' ABBINATO
020900950619     C*  PERCHE' COME DATA ARRIVO ULTIMO COLLO DEVE METTERE LA > TRA
021000041217     C*  QUELLA DEI COLLI  E QUELLA DELLA BOLLA solo per DDT SI senza
021100041217     c*  piking
021200041217     C                   CLEAR                   BOLAED
021300041217     C                   CLEAR                   BOLAEH
021400950619    3C     *IN15         IFEQ      *ON
021500041217    4c     §OgNtw        Ifne      'PPT'
021600041216     C* CONSIDERO L'ABBINAMENTO DEL FOGLIO DI BORDERIZZAZIONE SOLO PER
021700041216     C* DDT  SI  E CON LNA CHE NON FA IL PICKING
021800041222     c* considero anche se si trata di manca record in arrivo
021900041217    5c                   if        §ogpck=' '
022000060216     c     kar4a         chain     fiar401l
022100060216    6c                   if         %found(fiar401l)
022200041217     c                   movel     ar4not        dsbl4a
022300041217     c                   else
022400041217     c                   clear                   dsbl4a
022500041217    6c                   endif
022600041217     c
022700041217    6c                   if        §b4abm='S' or §b4abm='C'
022800041222     c                   EXSR      ctrarbnfv
022900041217    6C                   ENDIF
023000041217    5C                   ENDIF
023100001010     C* BOLLA POSTE: NON CALCOLO DATA ARRIVO ULTIMO COLLO
023200041217  X 4C                   ELSE
023300001010     C                   SETOFF                                       17
023400041217    4C                   ENDIF
023500001010    3C                   ENDIF
023600911011     C*
023700990921     C                   CLEAR                   WFVC
023800990921     C                   CLEAR                   WFPC
023900041222     c
024000930518     C* LEGGO I COLLI DELLA BOLLA
024100941031     C     KARB          SETLL     FNART000
024200941031     C     KARB          READE     FNART000                               30
024300041222     c                   clear                   wesiste010        1
024400911011     C*
024500930504DO  3C     *IN30         DOWEQ     '0'
024600041222     c* Memorizzo se presente un manca record: in questo caso devo
024700041222     c*  considerare l'abbinamento del foglio della bolla
024800041222     c                   if        wctrarbnfv=' '
024900041222     c     kanm          setll     fnanm02l                               34
025000050308     c                   if        *in34  or artdam<=arbdbr  and
025100050308     c                             artdam>0
025200041222     c                   exsr      CTRARBNFV
025300041222     c                   endif
025400041222     c                   endif
025500041222     c
025600921231     C*
025700930603    4C     *IN15         IFEQ      '1'
025800930603     C***
025900930603     C*** A B I L I T A Z I O N E   B O L L E   A L L A   C O N S E G.
026000930603     C***
026100940208     C*
026200940613     C* ROUTINE CHE CONTROLLA COLLO PER COLLO SE SONO PARTITI SU FV
026300980114     C*  ABBINATI: SE NE TROVO ALMENO 1 NON ABBINATO NON  POSSO
026400980114     C*  CALCOLARE LA DATA E ORA ARRIVO EFFETTIVO ULTIMO COLLO (17 OFF)
026500940613     C   17              EXSR      RIC2
026600940208     C*
026700950629IF  5C     ARBAMA        IFNE      2
026800921123     C*
026900980114     C* PER L'ABILITAZIONE, SE C'E LA DATA ARRIVO E' COME SE :
027000921123     C* 1) FOSSE PARTITO IL COLLO
027100921123     C* 2) FOSSE ABBINATO IL RELATIVO FOGLIO VIAGGIO
027200921123     C*
027300930914     C* MANCA DATA ARRIVO O COLLO NON ABILITATO:
027400930914     C*                    NON ABILITO IN BASE ALL'ARRIVO
027500930603IF  6C     ARTDAM        IFEQ      0
027600941221     C     ARTDAB        OREQ      'S'
027700921123     C                   SETON                                        02
027800921123     C*
027900921123     C* MANCA ABBINAMENTO: NON ABILITO IN BASE ALLA PARTENZA
028000941221     C* ARTABN= "S" : SIGNIFICA SPUNTA DI FOGLIO ABBINATO
028100941221IF  7C     ARTABN        IFNE      'S'
028200921123     C                   SETON                                        01
028300930603E   7C                   ENDIF
028400930603E   6C                   ENDIF
028500921123     C*
028600921123     C* NON E' POSSIBILE ABBINARE
028700990920     C   01
028800990920     CAN 02
028900990920     CANN20
029000990920     CANN21              SETON                                        30
029100930603E   5C                   ENDIF
029200930603     C***
029300930603     C*** D A T A   A R R I V O   E F F E T T I V O  ULTIMO COLLO BOLLA
029400930603     C***
029500930603     C* SE DATA CONSEGNA EFFETTIVA DELLA BOLLA E' VUOTA
029600940613IF  5C   17ARBDTI        IFEQ      0
029700930603     C*
029800930603     C* SE LA DATA DEL COLLO E' MAGGIORE DELLA DATA SALVATA
029900940613IF  6C     ARTDAM        IFGT      SAVDAM
030000930603     C*
030100930603     C* PULISCO SCHIERE DEI FOGLI VIAGGIO E DEI SEGNACOLLI
030200940613     C                   CLEAR                   NFV
030300940613     C                   CLEAR                   NSC
030400050304     C                   CLEAR                   dut
030500930603     C*
030600930603     C* MEMORIZZO DATA, NUMERO FOGLIO E SEGNACOLLO
030700941102     C                   Z-ADD     ARTDAM        SAVDAM
030800050510     c* Memrizzo se aggiornato con spunta processo ope partenza o arrivo
030900050510     c                   eval      savfl2=artfl2
031000930603     C                   Z-ADD     ARTNFV        NFV(1)
031100941102     C                   Z-ADD     ARTNSC        NSC(1)
031200050304     C                   Z-ADD     ARTdut        dut(1)
031300930603     C                   Z-ADD     1             X                 3 0
031400930603     C*
031500930603     C* DATA ARRIVO MINORE O UGUALE
031600940613X   6C                   ELSE
031700050304IF  7C     ARTDAM        IFEQ      SAVDAM
031800050510     c*
031900050510     c                   if        artfl2='P'
032000050510     c                   eval      savfl2=artfl2
032100050510     c                   endif
032200050510     c
032300050304     c                   z-add     1             yy                4 0
032400050304     C     ARTNFV        LOOKUP    NFV(yy)                                34
032500930603     C*
032600930603     C* F.V. NON MEMORIZZATO (NE CARICO AL MASSIMO 100)
032700950331IF  8C     *IN34         IFEQ      '0'
032800050304    9C     X             iflt      100
032900930603     C                   ADD       1             X
033000930603     C                   Z-ADD     ARTNFV        NFV(X)
033100941102     C                   Z-ADD     ARTNSC        NSC(X)
033200050304     C                   Z-ADD     ARTdut        DUT(X)
033300050304    9c                   endif
033400050304     c*
033500050304   x8c                   else
033600050304     c* Se foglio già presente memorizzo segnacolo con DUT >
033700050304    9c                   if        artdut>dut(yy)
033800050304     C                   Z-ADD     ARTNFV        NFV(yy)
033900050304     C                   Z-ADD     ARTNSC        NSC(yy)
034000050304     C                   Z-ADD     ARTdut        DUT(yy)
034100050304E   9C                   ENDIF
034200050304E   8C                   ENDIF
034300950331E   7C                   ENDIF
034400940613E   6C                   ENDIF
034500930603     C*
034600930603E   5C                   ENDIF
034700930603E   4C                   ENDIF
034800930603     C***
034900930603     C*** V O L U M E   C M L
035000930603     C***
035100950629IF  4C     *IN20         IFEQ      *ON
035200941031     C     ARTVUC        ANDGT     0
035300930503     C*
035400930503     C* CALCOLO IL VOLUME TOTALE DELLA BOLLA E I COLLI RILEVATI
035500941031     C                   ADD       ARTVUC        TOTVUC
035600941031     C                   ADD       1             D20NCR
035700990921     C     ARTFVC        IFNE      'P'
035800990921     C                   MOVEL     ARTFVC        WFVC
035900990921     C                   ENDIF
036000930504E   4C                   ENDIF
036100990806     C*** P E S O   C M L
036200990806     C***
036300990806IF  4C     *IN21         IFEQ      *ON
036400990806     C     ARTPUC        ANDGT     0
036500990806     C*
036600990806     C* CALCOLO IL PESO TOTALE DELLA BOLLA E I COLLI RILEVATI
036700990913     C                   ADD       ARTPUC        TOTPUC
036800990806     C                   ADD       1             D39NCP
036900990921     C     ARTFPC        IFNE      'P'
037000990921     C                   MOVEL     ARTFPC        WFPC
037100990921     C                   ENDIF
037200990806E   4C                   ENDIF
037300930517     C*
037400941031     C  N30KARB          READE     FNART000                               30
037500930504E   3C                   ENDDO
037600930603     C*
037700930603     C*
037800950629    3C     *IN15         IFEQ      *ON
037900930603     C***
038000930603     C*** A B I L I T A Z I O N E   B O L L E   A L L A   C O N S E G.
038100930603     C***
038200911011     C*
038300921123     C* 01 OFF - SEGNACOLLI PARTITI E ABBINATI ==> ABILITO  AMP=1 AMA=1
038400921123     C*          SE AMP=1 AMA SI PUO' PORTARE SOLO A 2
038500930517     C*
038600921123     C* 02 OFF - SEGNACOLLI ARRIVATI           ==> ABILITO  AMP=1 AMA=2
038700911011     C*
038800950629    4C     ARBAMA        IFNE      2
038900930603     C  N01
039000930603     CANN02              MOVEL     1             ARBAMP
039100930603     C  N01
039200930603     CANN02              MOVEL     2             ARBAMA
039300911011     C*
039400950629     C  N01
039500950629IF  5CAN 02ARBAMP        IFEQ      0
039600911011     C                   MOVEL     1             ARBAMP
039700911011     C                   MOVEL     1             ARBAMA
039800950629E   5C                   ENDIF
039900950629E   4C                   ENDIF
040000930603     C***
040100930603     C*** D A T A   A R R I V O   E F F E T T I V O  ULTIMO COLLO BOLLA
040200930603     C***
040300940613    4C   17ARBDTI        IFEQ      0
040400930603     C*
040500930517     C                   Z-ADD     1             X
040600950404     C*
040700950404     C* MEMORIZZO PER POI RICHIAMARE IL RICALCOLO TEMPI DI CONSEGNA
040800950404     C                   MOVE      '1'           W001A             1
040900930517     C*
041000930603DO  5C     NSC(X)        DOWGT     *ZEROS
041100930517     C                   Z-ADD     ARBFLS        KLNP
041200930517     C                   Z-ADD     ARBLNA        KLNA
041300930517     C                   Z-ADD     ARBNRS        KNRS
041400930517     C                   Z-ADD     NSC(X)        KNSC
041500930517     C*
041600930517     C* LEGGO SEGNACOLLO PER REPERIRE LA LNP DEL FOGLIO
041700070123     C     KBRV          SETLL     FNBRV09L
041800070123     C     KBRV          READE     FNBRV09L                               35
041900930517     C*
042000930517     C* F.V. DELLA LNA DELLA BOLLA
042100941102    6C     *IN35         DOWEQ     *OFF
042200970730     C                   CLEAR                   DSLV53
042300970730     C                   MOVEL     'A'           D53TFO
042400040331     c* campo non più usato
042500040331     C***                Z-ADD     SIMFEL        D53FEL
042600970730     C                   Z-ADD     BRVFGS        D53FGS
042700970730     C                   Z-ADD     BRVNFV        D53NFV
042800970730     C                   MOVEL     '1'           D53NPG
042900970730     C                   MOVEL     BRVFLE        D53FLF
043000970730     C                   MOVEL     'A'           D53ABB
043100970730     C                   MOVEL     ARBLNA        D53LNA
043200050603     c* non lo passo perchè potrebbe essere cambiato (ad esempio se collo
043300050603     c* impato o transitato)
043400050603     C***                MOVEL     ARBTFA        D53TFA
043500970730     C                   CALL      'FNLV53R'
043600970730     C                   PARM                    DSLV53
043700930517     C*
043800970730IF  7C     D53ERR        IFEQ      ' '
043900930517     C*
044000940613     C* MEMORIZZO DATA E ORA + RECENTI SOLTANTO SE:
044100940613     C*  IL FV PARTENZA E' STATO ABBINATO A UN FOGLIO ARRIVI
044200950427     C*  CON DATA >= ALLA DATA DI ARRIVO DEL COLLO
044300940613     C*
044400050510     c* Se data arrivo aggiornata con spunta partenza, data foglio arrivi
044500050510     c*  deve essere > (e non uguale) alla stessa data
044600050510     c**
044700050510   7AC**   D53DFA        IFGE      SAVDAM
044800050510     c**
044900050510     c                   if        (savfl2=' ' and d53dfa >=SAVDAM) or
045000050510     c                             (savfl2<>' ' and d53dfa>SAVDAM)
045100940613     C*
045200930517     C* MEMORIZZO SOLO SE LA DATA E L'ORA SONO PIU RECENTI
045300970730IF  8C     D53AED        IFGE      SAVAED
045400970730IF  9C     D53AED        IFGT      SAVAED
045500970730     C                   Z-ADD     D53AED        SAVAED
045600970730     C                   Z-ADD     D53AEH        SAVAEH
045700930607X   9C                   ELSE
045800970730IF 10C     D53AEH        IFGT      SAVAEH
045900970730     C                   Z-ADD     D53AEH        SAVAEH
046000930607E  10C                   ENDIF
046100930607E   9C                   ENDIF
046200930603E   8C                   ENDIF
046300940613     C*
046400940613E  7AC                   ENDIF
046500940613E   7C                   ENDIF
046600930528     C*
046700070123     C     KBRV          READE     FNBRV09L                               35
046800941102E   6C                   ENDDO
046900930517     C*
047000930528     C                   ADD       1             X
047100930603E   5C                   ENDDO
047200940613     C*
047300930603     C* SE DATA ARRIVO PRESENTE IN TUTTI I COLLI DELLA BOLLA
047400950627    5C     SAVAED        IFGT      0
047500950627    6C     BOLAED        IFLT      SAVAED
047600950627     C     BOLAED        OREQ      SAVAED
047700950619     C     BOLAEH        ANDLT     SAVAEH
047800930603     C                   Z-ADD     SAVAED        ARBDTI
047900930603     C                   Z-ADD     SAVAEH        ARBHTI
048000950627   X6C                   ELSE
048100950619     C                   Z-ADD     BOLAED        ARBDTI
048200950619     C                   Z-ADD     BOLAEH        ARBHTI
048300950627E   6C                   ENDIF
048400950627E   5C                   ENDIF
048500950619E   4C                   ENDIF
048600930517     C*
048700931213     C*
048800940613     C* SE  ARBDTI = 0  CERCO NEI FV PART ABBINATI AL FOGLIO ARRIVI
048900940613     C*                 CON DATA = ARTDAM
049000940613    4C   17ARBDTI        IFEQ      0
049100931213     C*
049200931213     C                   CLEAR                   COMDTI
049300931213     C                   CLEAR                   COMHTI
049400020918      * cerco l'As del terminal di arrivo e della linea di arrivo della bolla
049500050510     c* Ora stesso AS: non serve più
049600050510     c***                Exsr      RicAs
049700940613     C*
049800931213     C                   EXSR      RICFV
049900931213     C*
050000931213     C* MEMORIZZO I VALORI TROVATI IN  ARBDTI  E  ARBHTI
050100950619     C* CONFRONTANDO CON LA DATA DI ARRIVO DEL FOGLIO DELLA BOLLA
050200950619     C*
050300950619    5C     BOLAED        IFLT      COMDTI
050400950619    5C     BOLAED        OREQ      COMDTI
050500950619     C     BOLAEH        ANDLT     COMHTI
050600950619     C                   Z-ADD     COMDTI        ARBDTI
050700950619     C                   Z-ADD     COMHTI        ARBHTI
050800950619   X5C                   ELSE
050900950619     C                   Z-ADD     BOLAED        ARBDTI
051000950619     C                   Z-ADD     BOLAEH        ARBHTI
051100950619E   5C                   ENDIF
051200940613    4C                   ENDIF
051300931213     C*
051400930603E   3C                   ENDIF
051500930603E   2C                   ENDIF
051600930603     C***
051700930603     C*** V O L U M E   C M L
051800930603     C***
051900930504IF  2C     *IN20         IFEQ      '1'
052000941031     C     TOTVUC        ANDGT     0
052100941102     C*
052200941102     C* IMPOSTO IL VOLUME CML
052300941102     C                   Z-ADD(H)  TOTVUC        D20VLU
052400950110     C     D20VLU        IFEQ      0
052500950110     C                   Z-ADD     0,001         D20VLU
052600950110     C                   ENDIF
052700930503     C*
052800941031     C                   MOVEL     'A'           D20TBO
052900990921     C* NON DEVO TRASMETTERE LA VARIAZIONE DI VOLUME CML SE
053000990921     C* I VOLUMI SONO DELLA PARTENZA PERCHE' IN QUESTO CASO
053100990921     C* LA TRASMISSIONE ALLA SEDE E' GIA' STATA FATTA DALLA PARTENZA
053200990921     C* (V. TABELLA 7T TRASMISSIONE VOLUMI 'T' E 'Z' DA PARTENZA A
053300990921     C* SEDE --> TALI FLAG SONO IMPOSTATI AD 'S')
053400990921     C* ATTENZIONE QUINDI CHE I FLAG DELLA 7T DIVENTADO FISSO (NON
053500990921     C* MODIFICABILI) ALTRIMENTI NON FUNZIONA PIU' QUESTO GIRO
053600990921     C* COL RISCHIO DI PERDERE DEI VOLUMI CML IN SEDE
053700990921     C     WFVC          IFNE      *BLANKS
053800970901     C                   MOVEL     'S'           D20FTR
053900990921     C                   ELSE
054000990921     C                   MOVEL     ' '           D20FTR
054100990921     C                   ENDIF
054200941031     C                   MOVEL     'C'           D20TVL
054300941031     C                   MOVEL     ARBFVF        D20FVF
054400941031     C                   MOVEL     ARBVLF        D20VLF
054500941031     C                   MOVEL     ARBFVB        D20FVB
054600941031     C                   MOVEL     ARBVLB        D20VLB
054700941031     C                   MOVEL     ARBAAS        D20AAS
054800941031     C                   MOVEL     ARBLNP        D20LNP
054900941031     C                   MOVEL     ARBNRS        D20NRS
055000941031     C                   MOVEL     ARBNSP        D20NSP
055100941031     C                   MOVEL     ARBCBO        D20CBO
055200941031     C                   MOVEL     ARBNCL        D20NCL
055300991116     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
055400991116     C                   MOVEL     '1'           WTRC
055500991116     C     KAR6          CHAIN     FIAR6000                           30
055600941102     C  N30              MOVEL     AR6DFT        D20DFT
055700941031     C                   MOVEL     ARBLNA        D20LNA
055800960927     C*
055900960927     C                   MOVE      ARBDBR        D20DTE
056000941031     C                   MOVEL     DSLV20        KPJBU
056100040510     C* Passo parametri in più se pgm "gufo"
056200040510     c                   if        %parms=2
056300990806     C                   CALL      'FNLV20R'
056400990806     C                   PARM                    KPJBA
056500040510     c                   parm      'S'           pwstor            1
056600040510     c                   else
056700040510     C                   CALL      'FNLV20R'
056800040510     C                   PARM                    KPJBA
056900040510     c                   endif
057000990806     C*
057100990806     C                   MOVEL     KPJBU         DSLV20
057200990806     C*
057300990806     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
057400990806    4C     D20ERR        IFNE      '1'
057500990806     C                   MOVEL     D20NCR        ARBNCR
057600990806     C                   MOVEL     D20VLU        ARBVLC
057700990806     C                   MOVEL     D20FVF        ARBFVF
057800990806     C                   MOVEL     D20VLF        ARBVLF
057900990806     C**
058000990806   X4C                   ELSE
058100990806     C                   MOVEL     'V'           WFBS              1
058200990806E   4C                   ENDIF
058300990806E   2C                   ENDIF
058400990806     C*
058500990806      * DS PER PESO
058600990806     C*** P E S O    C M L
058700990806     C***
058800990806IF  2C     *IN21         IFEQ      '1'
058900990806     C     TOTPUC        ANDGT     0
059000990806     C*
059100990806     C* IMPOSTO IL VOLUME CML
059200990806     C                   Z-ADD(H)  TOTPUC        D39PLU
059300990806     C     D39PLU        IFEQ      0
059400990806     C                   Z-ADD     0,001         D39PLU
059500990806     C                   ENDIF
059600990806     C                   MOVEL     'A'           D39TBO
059700990921     C* PER EVITARE DI TRASMETTERE SEMPRE IL PESO CML ALLA SEDE
059800990921     C* ABBIAMO IMPOSTATO AD 'S' IL FLAG DI TRASMISSIONE PESO CML
059900990921     C* DA PARTENZA A SEDE  (V.TABELLA 7Y) E IN ARRIVO LA TRASMISSIONE
060000990921     C* VERRA' EFFETTUATA SOLO SE IL PESO NON E' DELLA PARTENZA
060100990921     C* ATTENZIONE QUINDI CHE IL FLAG DELLA 7Y DIVENTA FISSO (NON
060200990921     C* MODIFICABILE) ALTRIMENTI NON FUNZIONA PIU' QUESTO GIRO
060300990921     C* COL RISCHIO DI PERDERE DEI PESI CML IN SEDE
060400990921     C     WFPC          IFNE      *BLANKS
060500990921     C                   MOVEL     'S'           D39FTR
060600990921     C                   ELSE
060700990921     C                   MOVEL     ' '           D39FTR
060800990921     C                   ENDIF
060900990806     C                   MOVEL     'C'           D39TPP
061000990806     C                   MOVEL     ARBPKF        D39PLF
061100990806     C                   MOVEL     ARBPKB        D39PLB
061200990806     C                   MOVEL     ARBAAS        D39AAS
061300990806     C                   MOVEL     ARBLNP        D39LNP
061400990806     C                   MOVEL     ARBNRS        D39NRS
061500990806     C                   MOVEL     ARBNSP        D39NSP
061600990806     C                   MOVEL     ARBCBO        D39CBO
061700990806     C                   MOVEL     ARBNCL        D39NCL
061800931213     C*
061900990806     C  N30              MOVEL     AR6DFT        D39DFT
062000990806     C                   MOVEL     ARBLNA        D39LNA
062100990806     C                   MOVE      ARBDBR        D39DTE
062200990806     C*
062300990806     C                   MOVEL     DSLV39        KPJBU
062400040510     C                   CALL      'FNLV39R'
062500040510     C                   PARM                    KPJBA
062600941031     C*
062700990806     C                   MOVEL     KPJBU         DSLV39
062800941031     C*
062900970616     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
063000990806    4C     D39ERR        IFNE      '1'
063100990806     C                   MOVEL     D39NCP        ARBNCP
063200990806     C                   MOVEL     D39PLU        ARBPKC
063300990806     C                   MOVEL     D39PLF        ARBPKF
063400970924     C**
063500970924   X4C                   ELSE
063600070704     c                   if        wfbs=' '
063700990806     C                   MOVEL     'P'           WFBS              1
063800070704     c                   else
063900070704     C                   MOVEL     'E'           WFBS              1
064000070704     c                   endif
064100070704     c
064200941102E   4C                   ENDIF
064300990806     C                   ENDIF
064400950323     C*
064500950323     C* AGGIORNO FILE TASSAZIONE PADRONCINI
064600990806     C     *IN20         IFEQ      *ON
064700040205     C     *IN21         oreq      *ON
064800950323     C                   EXSR      AGGTAS
064900070703     c* se non sono riuscita ad aggiornare i padroncini, reimposto
065000070703     c*  per elaborare
065100070703     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
065200070704    4C     o57err        IFNE      ' '
065300070704     c* Se modificato il peso
065400070704    5c                   if        *in21
065500070703     C                   MOVEL     savNCP        ARBNCP
065600070703     C                   MOVEL     savPKC        ARBPKC
065700070703     C                   MOVEL     savPKF        ARBPKF
065800070703     C                   MOVEL     'P'           WFBS              1
065900070704E   5C                   ENDIF
066000070704     c* Se modificato il volume
066100070704    5c                   if        *in20
066200070704     C                   MOVEL     savNCR        ARBNCR
066300070704     C                   MOVEL     savvlc        ARBVLC
066400070704     C                   MOVEL     savFVF        ARBFVF
066500070704     C                   MOVEL     savVLF        ARBVLF
066600070704    6c                   if        wfbs=' '
066700070704     C                   MOVEL     'V'           WFBS              1
066800070704     c                   else
066900070704     C                   MOVEL     'E'           WFBS              1
067000070704    6c                   endif
067100070704E   5C                   ENDIF
067200070704     c
067300070704E   4C                   ENDIF
067400070704    3C                   END
067500930503     C*
067600070703     C* Aggiorno bolla
067700970924     C                   MOVEL     WFBS          ARBFBS
067800970924     C                   CLEAR                   WFBS
067900930503     C*
068000941031     C                   UPDATE    FNARB000
068100950404     C*
068200950404     C     W001A         IFNE      *BLANKS
068300950404     C     ARBDCM        ANDNE     *ZEROS
068400950404     C     ARBDCE        ANDNE     *ZEROS
068500950404     C                   EXSR      SRTCON
068600950404     C                   END
068700911011     C*
068800941031     C                   READ      FNARB000                             1031
068900930504     C   10              SETON                                        31
069000930504E   1C                   ENDDO
069100111006     c
069200111006     c* sottometto trasmissione solo ogni due ore o per chiusura sistema
069300111006     c                   clear                   SItrasm           1
069400040401     c*
069500040505     c* Se passato un parametro solo chiudo con LR
069600040505    1c                   if        %PARMs<2
069700040505     C                   SETON                                        LR
069800040505   x1c                   else
069900040505     C* CONTROLLA SE E' STATA RICHIESTA LA CHIUSURA DEL SOTTOSISTEMA
070000040505     C                   SHTDN                                        98
070100040505     c
070200040505    2c                   if        *in98
070300040505     c                   eval      chiudi='S'
070400040505     C                   SETON                                        LR
070500111006     c                   eval      SItrasm='S'
070600040505   x2c                   else
070700040505     c
070800040505     c* Se non sono passate 2 ore accendo RT altrimenti LR
070900040505     c*  e sottometto trasmissione
071000040505     c                   time                    worad
071100040505     c     worad         subdur    worap         minuti:*MN
071200041109    3c                   if        minuti>120
071300040505     C                   SETON                                        LR
071400111006     c                   eval      SItrasm='S'
071500041104   x3c                   else
071600111006     C*******            SETON                                        RT
071700111006     c* chiudo il LR ma non trasametto pesi e volumi
071800111006     C                   SETON                                        LR
071900041104    3c                   endif
072000041104    2c                   endif
072100041104     c
072200040505     c* Sottometto lsa3 per trasmissione
072300041104    2c                   if        *inlr
072400041104     c* chiudo i pgm del peso e volume
072500041104     C                   CLEAR                   DSLV39
072600041104     C                   MOVEL     'C'           D39TLA
072700041104     C                   MOVEL     DSLV39        KPJBU
072800041104     C                   CALL      'FNLV39R'
072900041104     C                   PARM                    KPJBA
073000041108     c*
073100041108     C                   CLEAR                   DSLV20
073200041108     C                   MOVEL     'C'           D20TLA
073300041108     C                   MOVEL     DSLV20        KPJBU
073400041108     C                   CALL      'FNLV20R'
073500041108     C                   PARM                    KPJBA
073600041104     c
073700111006     c                   if        SItrasm='S'
073800040505     c                   eval      pa3nfv='99999'
073900070502     c                   eval      pa3tfp='   '
074000040505     c                   movel     param3        kpjbu
074100040505     c                   eval      trasmpesvol='S'
074200040505     c                   call      'FNLSA3R'
074300040505     c                   parm                    kpjba
074400040505     c                   parm                    trasmpesvol       1
074500040505    2c                   endif
074600111006     c
074700111006    2c                   endif
074800040505    1c                   endif
074900040505     C**
075000040505     c                   if        *inlr
075100040505     C                   CLEAR                   DSLV57
075200040505     C                   MOVEL     'C'           I57TLA
075300040505     C                   CALL      'FNLV57R'
075400040505     C                   PARM                    KPJBA
075500040505     C                   PARM                    DSLV57
075600040505     C*
075700040505     C                   MOVEL     'C'           D55TLA
075800040505     C                   CALL      'FNLV55R'
075900040505     C                   PARM                    DSLV55
076000111010     C*
076100111010     C                   MOVEL     'C'           D53TLA
076200111010     C                   CALL      'FNLV53R'
076300111010     C                   PARM                    DSLV53
076400040505    1c                   endif
076500931210     C*---------------------------------------------------------------*
076600931210     C*                    RICFV                                      *
076700931210     C* ROUTINE DI RICERCA FOGLIO VIAGGIO PARTENZA                    *
076800931210     C*---------------------------------------------------------------*
076900931210     C     RICFV         BEGSR
077000050510     c                   eval      Wgiro = *off
077100050510     C                   Z-ADD     2             COMNPG
077200050510
077300050510      * Devo leggere fnfvv con terminal di arrivo bolla nel caso in cui
077400050510      * il terminal di arrivo della bolla e la linea di arrivo della bolla sono sullo
077500050510      * stesso As (ora sempre)
077600050510     c**                 If        TfaOgas = LnaOgas
077700050510     c                   Eval      Kfgs = ArbTfa
077800050510     c**                 Else
077900050510      * se non sono sullo stesso As leggo fnfvv con linea di arrivo della bolla
078000050510     c**                 Eval      Kfgs = ArbLna
078100050510     c**                 EndIf
078200050510     c
078300050510     c* Se bolla aggioranta con spunte processo ope partenza, devo
078400050510     c*  cercare un foglio arrivi con data > di SAVDAM
078500050510     c                   if        savfl2=' '
078600050510     c                   eval      kdam=savdam
078700050510     c                   else
078800050510     c*
078900050510     C     KFVV08        SETgt     FNFVV08
079000050510     C     KFVV08b       READE     FNFVV08                                32
079100050510     c                   if        not *in32
079200050510     c                   eval      kdam=fvvdfv
079300050510     c                   else
079400050510     c
079500050510     c* Foglio arrivi non trovato
079600050510     c* imposto SAVDAM+1
079700050510     c     *iso          movel     savdam        dataiso
079800050510     c                   adddur    1:*d          dataiso
079900050510     c                   move      dataiso       comdti
080000050510     C                   Z-ADD     0101          COMHTI
080100050510     c                   goto      endric
080200050510     c                   endif
080300050510     c
080400050510     c                   endif
080500020919
080600020919      * imposto un flag di comodo x fare il secondo tentativo con lo stesso npg
080700020919     c     rileggi       tag
080800940613     C*
080900940613     C* NEL 1 GIRO
081000050510     C* CERCO IL FOGLIO ARRIVI CON  NPG=2  E  DFV=SAVDAM o >savdam
081100940613     C* NEL 2 GIRO
081200050510     C* CERCO IL FOGLIO ARRIVI CON  NPG=6  E  DFV=SAVDAM o >savdam
081300050914     C* SE NON L'HO TROVATO AL 1 GIRO (non più)
081400050914     C****               Z-ADD     1             Y                 2 0
081500940613     C*
081600050914   -1C**   Y             DOWLE     2
081700050914     C**   COMDTI        ANDEQ     0
081800940613     C*
081900941102     C     KFVV          SETLL     FNFVV000
082000941102     C     KFVV          READE     FNFVV000                               32
082100940613     C*
082200940613    0C     *IN32         DOWEQ     *OFF
082300940613     C*
082400940613     C* CERCO IL F.VIAGGIO PARTENZA (ABBINATO SUL FOGLIO ARRIVI TROVATO
082500940613     C*   CHE CONTIENE LA LINEA DEL COLLO CON ARTDAM PIU'  RECENTE
082600931215     C*
082700940613     C* LEGGO FLFVA02L CON         NFA=FVVNFV
082800941102     C     KFVA2         SETLL     FNFVA02
082900941102     C     KFVA2         READE     FNFVA02                                33
083000931210     C*
083100931213    1C     *IN33         DOWEQ     *OFF
083200970708     C                   CLEAR                   DSLV53
083300970708     C                   MOVEL     'A'           D53TFO
083400040331     c* campo non più usato
083500040331     C***                MOVEL     SIMFEL        D53FEL
083600970708     C                   MOVEL     FVANFV        D53NFV
083700970708     C                   MOVEL     1             D53NPG
083800970708     C                   MOVEL     FVALNP        D53FGS
083900970708     C                   MOVEL     FVALNP        D53FLF
084000970708     C                   MOVEL     FVALAI        D53LAI
084100031212     C                   MOVEL     'B'           D53ABB
084200970708     C                   MOVEL     ARBLNA        D53LNA
084300050603     C****               MOVEL     ARBTFA        D53TFA
084400970708     C                   CALL      'FNLV53R'
084500970708     C                   PARM                    DSLV53
084600970730     C**
084700970730     C* LEGGO QUESTO FNFVA SOLO SE IL PGM HA TROVATO CHE E' UN
084800970730     C*  FOGLIO DELLA LINEA DI ARRIVO
084900970730    2C     D53ERR        IFEQ      ' '
085000050914     C* SE FVALNP = TERMINAL PART. BOLLA
085100970730     C     FVALNP        ANDEQ     ARBTFP
085200050914     c                   if        comdti=0 or (comdti<fvaaed) or
085300050914     c                             (comdti=fvaaed and fvaaeh>comhti)
085400931214     C                   Z-ADD     FVAAEH        COMHTI
085500940613     C                   Z-ADD     FVAAED        COMDTI
085600050914     c                   endif
085700050913     c
085800050913   X2C***                else
085900050913    2C                   ENDIF
086000970730     C* RILEGGO
086100050914     C     KFVA2         READE     FNFVA02                                33
086200050913    2C***                ENDIF
086300931213    1C                   ENDDO
086400940613     C*
086500941102     C     KFVV          READE     FNFVV000                               32
086600940613     C*
086700940613     C* SE GIA' TROVATO NON LEGGO PIU'
086800050914     c*  non lo faccio più perchè tengo la data + alta quindi devo leggere
086900050914     c*  tutto
087000050914     C**   COMDTI        IFGT      0
087100050914     C**                 SETON                                        32
087200050914     C**                 ENDIF
087300940613    0C                   ENDDO
087400940613     C*
087500050914     c* tolgo la cat. 6 non più usata
087600050914     C*****              ADD       1             Y
087700050914     C*****              Z-ADD     6             COMNPG
087800050914   -1C*****              ENDDO
087900020919
088000050913      * Se non ho trovato e terminal di arrivo e linea di arrivo della bolla
088100020919      * sono su stesso AS devo fare un altro tentativo con la linea di arrivo della bolla
088200050510     c***                If        Comdti = *zeros and TfaOgas = LnaOgas
088300050510     c                   If        Comdti = *zeros
088400020919     c                             and Wgiro = *off and
088500020919     c                             arbtfa<>arblna
088600020919     c                   Eval      Kfgs = ArbLna
088700020919     c                   Eval      Wgiro = *on
088800020919     c                   GoTo      Rileggi
088900020919     c                   EndIf
089000931210     C*
089100940613     C     COMDTI        IFEQ      0
089200931210     C                   Z-ADD     0101          COMHTI
089300050510     C                   Z-ADD     kdam          COMDTI
089400931210     C                   ENDIF
089500931210     C*
089600050510     C     endric        ENDSR
089700940208     C*---------------------------------------------------------------*
089800980114     C* CONTROLLO SE CALCOLARE LA DATA ARRIVO ULTIMO COLLO
089900940208     C*---------------------------------------------------------------*
090000940208     C     RIC2          BEGSR
090100940613     C* SE COLLO NON ARRIVATO NON POSSO CALCOLARE LA DATA E ORA
090200940613     C*  ARRIVO ULTIMO COLLO
090300940613    1C     ARTDAM        IFEQ      0
090400940613     C                   SETOFF                                       17
090500941102   X1C                   ELSE
090600940208     C*
090700950123     C* ARTABN = "S" COLLO DI FV PARTENZA ABBINATO
090800950123    2C     ARTABN        IFNE      'S'
090900980114     C* NON LO CALCOLO SE C'E' LA DATA DI PARTENZA E QUINDI LA SPUNTA
091000980114     C*  NON ANCORA ABBINATA. IL PULIZIA SPUNTE NON CANCELLO SE IL
091100980114     C*  RELATIVO FOGLIO NON E' ABBINATO ANCHE SE CONSEGNARA
091200980114    3C     ARTDFV        IFGT      0
091300980114     C     ARTFLP        ANDEQ     0
091400980114     C     ARTDUT        ORGT      0
091500980114     C     ARTFLP        ANDGT     0
091600980114     C                   SETOFF                                       17
091700980114    3C                   ENDIF
091800980114    2C                   ENDIF
091900980114    1C                   ENDIF
092000940208     C*
092100940208     C                   ENDSR
092200950323     C*---------------------------------------------------------------*
092300950323     C*                    AGGTAS                                     *
092400950323     C* AGGIORNO FILE TASSAZIONE PADRONCINI                           *
092500950323     C*---------------------------------------------------------------*
092600950323     C     AGGTAS        BEGSR
092700950323     C*
092800970616     C* MODIFICATO VOLUME CML: SISTEMO IL VOLUME ANCHE NEI PADRONCINI
092900950323    1C     ARBVLC        IFNE      SAVVLC
093000040205    1C     ARBPKC        ORNE      SAVPKC
093100970924     C                   CLEAR                   DSLV57
093200970924     C                   MOVEL     'A'           I57TBO
093300970924     C                   MOVEL     ARBAAS        I57AAS
093400970924     C                   MOVEL     ARBLNP        I57LNP
093500970924     C                   MOVEL     ARBNRS        I57NRS
093600970924     C                   MOVEL     ARBNSP        I57NSP
093700970924     C                   MOVEL     ARBPDC        I57PDR
093800970924     C                   MOVEL     ARBNCL        I57NCL
093900970924     C                   MOVEL     'C'           I57TVL
094000970924     C                   MOVEL     SAVNCR        I57NCR
094100970924     C                   MOVEL     SAVVLC        I57VLC
094200990920     C                   MOVEL     SAVVLF        I57VLF
094300970924     C                   MOVEL     ARBVLC        I57VCN
094400970924     C                   MOVEL     ARBNCR        I57NCN
094500970924     C                   MOVEL     ARBVLF        I57VFN
094600040205     C                   MOVEL     SAVNCp        I57nCp
094700040205     C                   MOVEL     SAVpkC        I57pkC
094800040205     C                   MOVEL     SAVpkF        I57pkF
094900040205     C                   MOVEL     arbNCp        I57npn
095000040205     C                   MOVEL     arbpkC        I57pcn
095100040205     C                   MOVEL     arbpkF        I57pfn
095200970924     C                   CALL      'FNLV57R'
095300970924     C                   PARM                    KPJBA
095400970924     C                   PARM                    DSLV57
095500970924     C                   ENDIF
095600970616     C*
095700950323     C                   ENDSR
095800950404     C*---------------------------------------------------------------*
095900950404     C*                    SRTCON                                     *
096000950404     C* RICALCOLO TEMPI DI CONSEGNA                                   *
096100950404     C*---------------------------------------------------------------*
096200950404     C     SRTCON        BEGSR
096300950404     C* SE SPEDIZIONE GIà CONSEGNATA E GIA' CALCOLATI I TEMPI DI
096400950404     C* CONSEGNA LI RICALCOLO: I TEMPI POTEVANO ESSERE STATI CALCOLATI
096500950404     C* QUANDO ANCORA NON ERANO STATI ABBINATI I F.V. E QUINDI OCCORRE
096600950404     C* RICALCOLARLI PER AVERE I TEMPI GIUSTI.
096700970925     C                   CLEAR                   DS99
096800950404     C                   MOVEL     ' '           D98TLA
096900950404     C                   MOVEL     'A'           D98TBO
097000950404     C                   Z-ADD     ARBAAS        D98AAS
097100950404     C                   Z-ADD     ARBLNP        D98LNP
097200950404     C                   Z-ADD     ARBNRS        D98NRS
097300950404     C                   Z-ADD     ARBNSP        D98NSP
097400950404     C*
097500970925     C                   CALL      'TNSD99R'
097600970925     C                   PARM                    DS99
097700950404     C* SE TEMPI DIVERSI
097800950404     C     D98DCE        IFNE      ARBDCE
097900950404     C     D98NPI        ORNE      ARBNPI
098000950404     C     D98NCI        ORNE      ARBNCI
098100950404     C     D98NRC        ORNE      ARBNRC
098200950404     C     KARB          CHAIN     FNARB1                             31
098300960906     C* Determino se tipo bolla deve pervenire in sede (*IN08 OFF)
098400960906     C                   SETOFF                                       08
098500960906     C                   MOVEL     '3A'          COD
098600960906     C                   MOVEL(P)  ARBCBO        KEY
098700960906     C     KTAB          CHAIN     TABEL00F                           38
098800960906     C     *IN38         IFEQ      *OFF
098900960906    6C     TBLFLG        ANDEQ     ' '
099000960906     C                   MOVEL     TBLUNI        DS3A
099100960906     C     §3ABSD        COMP      ' '                                0808
099200960906     C                   END
099300950404     C* Richiamo routine di Addpfm/Allocazione/Ovrdbf
099400950404     C                   EXSR      SRARBC
099500950404     C     *IN05         IFEQ      *OFF
099600971120     C                   OPEN      FNARBC0T
099700010103     C  N08              OPEN      FIARBC46
099800950404     C                   Z-ADD     D98DCE        ARBDCE
099900950404     C                   Z-ADD     D98NPI        ARBNPI
100000950404     C                   Z-ADD     D98NCI        ARBNCI
100100950404     C                   Z-ADD     D98NRC        ARBNRC
100200950404     C                   MOVEL     'C'           ARBFAG
100300971120     C                   WRITE     FNARBC00
100400010103     C  N08              WRITE     FIARBC00
100500971120     C                   CLOSE     FNARBC0T
100600010103     C  N08              CLOSE     FIARBC46
100700950404     C* Richiamo routine di Disallocazione/Dltovr
100800100915     c                   eval      soloarbc0t=' '
100900950404     C                   EXSR      DISALL
101000100915
101100950404     C                   ELSE
101200960125     C                   MOVEL     'S'           ARBFBS
101300950404     C                   MOVE      *ZEROS        ARBDTI
101400950404     C                   MOVE      *ZEROS        ARBHTI
101500950404     C                   END
101600950404     C*
101700950404     C                   UPDATE    FNARB1
101800950405     C*
101900950405     C                   END
102000950404     C*
102100950404     C                   ENDSR
102200950403     C*---------------------------------------------------------------*
102300950403     C*                    SRARBC                                     *
102400950403     C* ADDPFM/ALLOCAZIONE/OPEN FILE DI TRASMISSIONE                  *
102500950403     C*---------------------------------------------------------------*
102600950403     C     SRARBC        BEGSR
102700950403     C*
102800950403     C                   MOVE      *BLANKS       $MBR
102900950403     C                   MOVE      'M'           EMME                            DESTINAZIONE
103000040401     C***                move      simfel        xsimfe
103100040513     c                   if        simfel=946
103200040401     C                   MOVE      '026'         XSIMFE
103300040513     c                   else
103400040513     C                   move      simfel        xsimfe
103500040513     c                   endif
103600971120     C*
103700971120     C                   CLEAR                   DSLV55
103800971120     C                   MOVEL     'P'           D55TPT
103900971120     C                   MOVE      ARBLNP        D55LIN
104000971120     C                   MOVE      DATEU         D55DRF
104100971120     C                   CALL      'FNLV55R'
104200971120     C                   PARM                    DSLV55
104300971120     C                   MOVE      D55TFP        $SIMFE
104400970224     C*
104500950403      * Richiamo a pgm per addpfm
104600950403     C                   MOVE      *BLANKS       FILE
104700971120     C                   MOVEL     'FNARBC0T'    FILE
104800950403     C                   CALL      'TRUL50C'
104900950403     C                   PARM                    FILE             10
105000950403     C                   PARM                    $MBR
105100950403     C                   PARM                    KNSIF
105200950403     C                   PARM                    LOGICO           10
105300950403     C                   PARM                    LOGMBR           10
105400950403     C                   PARM                    FLG               1
105500950403      * Tento l'allocazione del file di trasmissione
105600950403     C                   EXSR      ALLOCA
105700950403      * Proseguo solo se riuscita l'allocazione
105800950404     C  N05              DO
105900950403      * OVRDBF e apertura del file
106000950403     C                   MOVE      COM(1)        $COMOD                         DI COMODO
106100950403     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
106200950403     C                   MOVEA     FILE          CMD(13)
106300950403     C                   MOVEA     $MBR          CMD(29)
106400950403     C                   MOVEA     CMD           COMMAN
106500950403     C                   Z-ADD     80            LUNG
106600950403     C                   CALL      'QCMDEXC'
106700950403     C                   PARM                    COMMAN
106800950403     C                   PARM                    LUNG
106900980720     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
107000980720     C                   MOVEA     FILE2         CMD(13)
107100980720     C                   MOVEA     BLANK         CMD(29)
107200980720     C                   MOVEA     CMD           COMMAN
107300980720     C                   Z-ADD     80            LUNG
107400980720     C                   CALL      'QCMDEXC'
107500980720     C                   PARM                    COMMAN
107600980720     C                   PARM                    LUNG
107700950403     C                   ENDDO
107800950403     C*
107900950403     C                   ENDSR
108000950403      **************************************************************************
108100950403      * Allocazione di un archivio (FILE)
108200950403      **************************************************************************
108300950403     C     ALLOCA        BEGSR
108400950403     C*
108500971120     C* ALLOCAZIONE FNARBC0T
108600950403     C                   MOVE      COM(2)        $COMOD           80            DI COMODO
108700950403     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
108800950403     C                   MOVEA     FILE          CMD(13)
108900950403     C                   MOVEA     $MBR          CMD(38)
109000950403     C                   MOVEA     CMD           COMMAN
109100950403     C                   Z-ADD     80            LUNG
109200950404     C                   CALL      'QCMDEXC'                            05
109300950403     C                   PARM                    COMMAN           80
109400950403     C                   PARM                    LUNG             15 5
109500010103     C* ALLOCAZIONE FIARBC46
109600950404     C     *IN05         IFEQ      *OFF
109700960906     C     *IN08         ANDEQ     *OFF
109800950405     C                   MOVE      *BLANKS       FILE2            10
109900010103     C                   MOVEL     'FIARBC46'    FILE2
110000020927     C                   eval      BLANK = 'M' + xsimfe + '046'
110100950403     C                   MOVEA     FILE2         CMD(13)
110200950411     C                   MOVEA     BLANK         CMD(38)
110300950403     C                   MOVEA     CMD           COMMAN
110400950404     C                   CALL      'QCMDEXC'                            05
110500950403     C                   PARM                    COMMAN           80
110600950403     C                   PARM                    LUNG             15 5
110700100915     c* Se allocazione non riuscita disalloco fnarbc0t
110800100915     c                   eval      soloarbc0t='S'
110900100915     c                   exsr      disall
111000100915     c                   eval      soloarbc0t=' '
111100950403     C                   END
111200950403      *
111300950403     C                   ENDSR
111400950404      **************************************************************************
111500950404      * Dssallocazione di un archivio (FILE) con membro ($MBR)
111600950404      **************************************************************************
111700950404     C     DISALL        BEGSR
111800950404      *
111900950404     C                   MOVE      COM(3)        $COMOD                         DI COMODO
112000950404     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
112100950404     C                   MOVEA     FILE          CMD(13)
112200950404     C                   MOVEA     $MBR          CMD(38)
112300950404     C                   MOVEA     CMD           COMMAN
112400950404     C                   Z-ADD     80            LUNG
112500950404     C                   CALL      'QCMDEXC'                            99
112600950404     C                   PARM                    COMMAN
112700950404     C                   PARM                    LUNG
112800950404     C*
112900100915     c                   if        not *in08 and soloarbc0t =' '
113000100915     C     *IN08         IFEQ      *OFF
113100950404     C                   MOVE      COM(3)        $COMOD                         DI COMODO
113200950404     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
113300950404     C                   MOVEA     FILE2         CMD(13)
113400950411     C                   MOVEA     BLANK         CMD(38)
113500950404     C                   MOVEA     CMD           COMMAN
113600950404     C                   Z-ADD     80            LUNG
113700950404     C                   CALL      'QCMDEXC'                            99
113800950404     C                   PARM                    COMMAN
113900950404     C                   PARM                    LUNG
114000960906     C                   END
114100971120      * DLTOVR di FNARBC0T
114200971120     C                   MOVEL(P)  COM(4)        COMMAN
114300950404     C                   Z-ADD     80            LUNG
114400100915     C                   CALL      'QCMDEXC'                            99
114500950404     C                   PARM                    COMMAN
114600950404     C                   PARM                    LUNG
114700100915     c                   endif
114800950404      *
114900950404     C                   ENDSR
115000041222     C*---------------------------------------------------------------*
115100041222     C* controllo abbinamento foglio bolla
115200041222     C*---------------------------------------------------------------*
115300041222     c     CTRARBNFV     begsr
115400041222     C                   CLEAR                   DSLV53
115500041222     C                   MOVEL     'A'           D53TFO
115600041222     c* Campo non più usato
115700041222     C***                Z-ADD     SIMFEL        D53FEL
115800041222     C                   Z-ADD     ARBFLE        D53FGS
115900041222     C                   Z-ADD     ARBNFV        D53NFV
116000041222     C                   MOVEL     '1'           D53NPG
116100041222     C                   MOVEL     ARBFLE        D53FLF
116200041222     C                   MOVEL     'B'           D53ABB
116300041222     C                   MOVEL     ARBLNA        D53LNA
116400041222     C                   MOVEL     ARBTFA        D53TFA
116500041222     C                   CALL      'FNLV53R'
116600041222     C                   PARM                    DSLV53
116700041222     C* SE NON C'E' ERRORE VUOL DIRE CHE HO TROVATO IL FOGLIO
116800041222    1C     D53ERR        IFEQ      ' '
116900041222    2C     D53AED        IFEQ      0
117000041222     C                   SETOFF                                       17
117100041222   X2C                   ELSE
117200041222     C                   MOVEL     D53AED        BOLAED
117300041222     C                   MOVEL     D53AEH        BOLAEH
117400041222    2C                   ENDIF
117500041222   X1C                   ELSE
117600041222     C* SE NON TROVATO FOGLIO --> NON METTO LA DATA ARRIVO ULTIMO COLLO
117700041222     C                   SETOFF                                       17
117800041222    1C                   ENDIF
117900041222     c*
118000041222     c                   eval      wctrarbnfv='S'
118100041222     c                   ENDSR
118200930430     C*---------------------------------------------------------------*
118300930430     C*                    DEFVAR                                     *
118400930430     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
118500930430     C*---------------------------------------------------------------*
118600040401     C     *inzsr        BEGSR
118700930430     C*
118800930430     C*
118900930430     C                   Z-ADD     1             CODUT             1 0
119000040513     C                   CALL      'X§PARUT'
119100040513     C                   PARM                    UT§DSE
119200040513     C                   MOVEL     REC80         CNCR80
119300930505     C*
119400941102     C                   Z-ADD     1             KNPG
119500971120     C*
119600971120     C* DETERMINO DATA DEL GIORNO
119700971120     C                   TIME                    W0140            14 0
119800971120     C                   MOVE      W0140         WDTGIO            8 0
119900971120     C                   CLEAR                   WLBDAT
120000971120     C                   Z-ADD     WDTGIO        G02DAT
120100971120     C                   CALL      'XSRDA8'
120200971120     C                   PARM                    WLBDAT
120300971120     C                   MOVEL     G02INV        DATEU             8 0
120400040505     c* Prendo ora di apertura pgm
120500040505     C                   time                    worap
120600930503     C*
120700931210     C* DEFINIZIONE DI ALCUNI CAMPI
120800001010     C     *LIKE         DEFINE    ARBLNP        SAVLNP
120900041216     C     *LIKE         DEFINE    ARBLNP        SAVLNA
121000001010     C     *LIKE         DEFINE    ARBLNA        WLNA
121100961129     C     *LIKE         DEFINE    ARBDTI        COMDTI
121200931210     C     *LIKE         DEFINE    ARBHTI        COMHTI
121300931210     C     *LIKE         DEFINE    FVANPG        COMNPG
121400941102     C     *LIKE         DEFINE    FVANPG        KNPG
121500941102     C     *LIKE         DEFINE    FVAAED        SAVAED
121600941102     C     *LIKE         DEFINE    FVAAEH        SAVAEH
121700950619     C     *LIKE         DEFINE    FVAAED        BOLAED
121800950619     C     *LIKE         DEFINE    FVAAEH        BOLAEH
121900941102     C     *LIKE         DEFINE    FVVDFV        SAVDAM
122000950323     C     *LIKE         DEFINE    ARBNCR        SAVNCR
122100950323     C     *LIKE         DEFINE    ARBVLC        SAVVLC
122200950323     C     *LIKE         DEFINE    ARBVLF        SAVVLF
122300070704     C     *LIKE         DEFINE    ARBfvF        SAVfvF
122400990806     C     *LIKE         DEFINE    ARBPKF        SAVPKF
122500990806     C     *LIKE         DEFINE    ARBPKC        SAVPKC
122600990806     C     *LIKE         DEFINE    ARBNCP        SAVNCP
122700950323     C     *LIKE         DEFINE    ARBVLF        WADDV
122800950323     C     *LIKE         DEFINE    ARBVLF        WSOTV
122900960906     C     *LIKE         DEFINE    TBLKEY        KEY
123000960906     C     *LIKE         DEFINE    TBLCOD        COD
123100990921     C     *LIKE         DEFINE    ARTFVC        WFVC
123200990921     C     *LIKE         DEFINE    ARTFPC        WFPC
123300991116     C     *LIKE         DEFINE    AR6TRC        WTRC
123400931210     C*
123500941031     C* LETTURA FNARB
123600930430     C     KARB          KLIST
123700930430     C                   KFLD                    ARBAAS
123800930430     C                   KFLD                    ARBLNP
123900930430     C                   KFLD                    ARBNRS
124000930430     C                   KFLD                    ARBNSP
124100991116     C*
124200991116     C     KAR6          KLIST
124300991116     C                   KFLD                    ARBAAS
124400991116     C                   KFLD                    ARBLNP
124500991116     C                   KFLD                    ARBNRS
124600991116     C                   KFLD                    ARBNSP
124700991116     C                   KFLD                    WTRC
124800041217     C     KAR4a         KLIST
124900041217     C                   KFLD                    ARBAAS
125000041217     C                   KFLD                    ARBLNP
125100041217     C                   KFLD                    ARBNRS
125200041217     C                   KFLD                    ARBNSP
125300041217     C                   KFLD                    ktrc
125400041222     C     KAnm          KLIST
125500041222     C                   KFLD                    ARtfls
125600041222     C                   KFLD                    ARtlna
125700041222     C                   KFLD                    ARtNRS
125800041222     C                   KFLD                    ARtNSc
125900041222     C                   KFLD                    kcaa
126000930517     C*
126100070123     C* LETTURA FNBRV09L
126200941102     C     KBRV          KLIST
126300941102     C                   KFLD                    KNPG
126400941102     C                   KFLD                    KLNP              3 0
126500960304     C                   KFLD                    KLNA              3 0
126600930517     C                   KFLD                    KNRS              2 0
126700930517     C                   KFLD                    KNSC              7 0
126800930517     C*
126900941102     C* LETTURA FNFVV02L
127000931210     C     KFVV          KLIST
127100931210     C                   KFLD                    COMNPG
127200050510     C                   KFLD                    kdam
127300020917     C                   KFLD                    Kfgs
127400050510     C     KFVV08        KLIST
127500050510     C                   KFLD                    Kfgs
127600050510     C                   KFLD                    COMNPG
127700050510     C                   KFLD                    savdam
127800050510     C     KFVV08b       KLIST
127900050510     C                   KFLD                    Kfgs
128000050510     C                   KFLD                    COMNPG
128100931210     C* LETTURA FLFVA02L
128200931210     C     KFVA2         KLIST
128300931210     C                   KFLD                    COMNPG
128400931210     C                   KFLD                    FVVNFV
128500020917     c                   Kfld                    FvvFgs
128600931210     C*
128700960304     C* LETTURA TABEL00F
128800960304     C     KTAB2         KLIST
128900960304     C                   KFLD                    CODUT
129000960906     C                   KFLD                    COD
129100960906     C     KTAB          KLIST
129200960906     C                   KFLD                    CODUT
129300960906     C                   KFLD                    COD
129400960906     C                   KFLD                    KEY
129500930430     C                   ENDSR
129600950404**  COM  -  COMANDI CL
129700950403OVRDBF FILE(          ) MBR(          )
129800950403ALCOBJ OBJ((           *FILE *SHRUPD           ))
129900950403DLCOBJ OBJ((           *FILE *SHRUPD           ))
130000950404DLTOVR FILE(FNARBC0T  )
