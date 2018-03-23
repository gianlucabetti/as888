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
010700111109       // -?Costante per controllo "caratteri solo numerici"?
010800111109     d c_Digits        c                   const('1234567890')
010900020917
011000930430     C*---------------------------------------------------------------*
011100930430     C*                    MAIN                                       *
011200930430     C* PROGRAMMA DI ABILITAZIONE BOLLE ARRIVI ALLA CONSEGNA          *
011300930430     C*---------------------------------------------------------------*
011400040401     C     *ENTRY        PLIST
011500040401     C                   PARM                    KPJBA
011600040401     C                   PARM                    chiudi            1
011700111109     C                   PARM                    numela            3
011800040513     c
011900040513     c                   if        knsif=*blanks
012000040513     c                   eval      knsif='*LIBL'
012100040513     c                   endif
012200040505     c* Per pgm gufo, ogni 2 ore chiudo pgm trasmetto variazioni peso volume
012300040505     c*  e riapro il pgm
012400040505     c
012500930430     C*
012600040401     C     *loval        setll     FNARB000                             1031
012700040401     C                   READ      FNARB000                             1031
012800941031     C*
012900941031DO  1C  N10*IN31         DOWEQ     *OFF
013000930503     C*
013100990806     C                   SETOFF                                       152021
013200950404     C                   MOVE      *BLANKS       W001A
013300041222     C                   MOVE      *BLANKS       Wctrarbnfv        1
013400001010     C* VERIFICO SE SI TRATTA DI UNA BOLLA POSTE
013500001010     C     ARBLNP        IFNE      SAVLNP
013600001010     C     ARBLNP        CHAIN     AZORG01L                           38
013700001010     C  N38              MOVEL     ORGDE3        OG143
013800001010     C   38              CLEAR                   OG143
013900001010     C                   MOVEL     ARBLNP        SAVLNP
014000001010     C                   ENDIF
014100041216     C* VERIFICO SE LA LINEA DI ARRIVO FA IL PIKING
014200041216     C     ARBLNA        IFNE      SAVLNA
014300041216     C     ARBLNA        CHAIN     AZORG01L                           38
014400041216     C  N38              MOVEL     ORGDF0        OG150
014500041216     C   38              CLEAR                   OG150
014600041217     C                   MOVEL     ARBLNa        SAVLNA
014700041216     C                   ENDIF
014800041216     C
014900930517     C*
015000921231     C* 15 ON - DEVO ABILITARE LA MERCE ALLA CONSEGNA
015100930603     C     ARBFBS        IFEQ      'S'
015200970924     C     ARBFBS        OREQ      'E'
015300930603     C                   SETON                                        15
015400930603     C* PULISCO DATA E ORA DI COMODO
015500941102     C                   Z-ADD     *ZEROS        SAVAED
015600941102     C                   Z-ADD     *ZEROS        SAVAEH
015700930607     C                   Z-ADD     *ZEROS        SAVDAM
015800050510     c                   clear                   savfl2
015900940613     C                   CLEAR                   NFV
016000940613     C                   CLEAR                   NSC
016100050304     C                   CLEAR                   dut
016200930607     C                   Z-ADD     1             X
016300950629     C                   ENDIF
016400921231     C*
016500921231     C* 20 ON - DEVO CALCOLARE IL VOLUME CML SU BOLLA
016600970924IF  2C****       ARBNCR    IFEQ 99999
016700970924     C****       ARBNCR    OREQ 99998
016800970924     C     ARBFBS        IFEQ      'V'
016900970924     C     ARBFBS        OREQ      'E'
017000930504     C                   SETON                                            20
017100941102     C                   Z-ADD     *ZEROS        TOTVUC            8 6
017200950323     C* SALVO VOLUME DA FATTURARE, VOLUME CML E NUMERO COLLI RILEVATI
017300950323     C                   Z-ADD     ARBVLF        SAVVLF
017400950323     C                   Z-ADD     ARBVLC        SAVVLC
017500950323     C                   Z-ADD     ARBNCR        SAVNCR
017600070704     c                   movel     arbfvf        SAVFVF
017700950323     C*
017800941102     C                   CLEAR                   DSLV20
017900930517E   2C                   ENDIF
018000990806     C* 21 ON - DEVO CALCOLARE IL PESO CML SU BOLLA
018100990806     C     ARBFBS        IFEQ      'P'
018200990806     C     ARBFBS        OREQ      'E'
018300990806     C                   SETON                                            21
018400990806     C                   Z-ADD     *ZEROS        TOTPUC            9 3
018500990806     C* SALVO VOLUME DA FATTURARE, VOLUME CML E NUMERO COLLI RILEVATI
018600990806     C                   Z-ADD     ARBPKF        SAVPKF
018700990806     C                   Z-ADD     ARBPKC        SAVPKC
018800990806     C                   Z-ADD     ARBNCP        SAVNCP
018900990806     C*
019000990806     C                   CLEAR                   DSLV39
019100990806E   2C                   ENDIF
019200911011     C*
019300930603     C* ELABORO SE:
019400930603     C* 1) DEVO ABILITARE BOLLA ALLA CONSEGNA E LIVELLO ABILITAZIONE <2
019500930603     C* 2)  "       "       "     "     "     "   "         "       >"2
019600930603     C*     MA NON HO ANCORA CALCOLATO LA DATA E L'ORA DI ARRIVO EFFETT
019700930603     C* 3) DEVO CALCOLARE IL VOLUME CML
019800990920     C* 4) DEVO CALCOLARE IL PESO   CML
019900930603     C*
020000950629IF  2C     *IN15         IFEQ      *ON
020100950629     C     ARBAMA        ANDNE     2
020200001010     C* LA DATA ARRIVO ULTIMO COLLO NON LA CALCOLO PER BOLLE POSTE
020300950629     C     *IN15         OREQ      *ON
020400930603     C     ARBDTI        ANDEQ     0
020500020918     c     §OgNtw        andne     'PPT'
020600001010     C* VOLUME O PESO
020700950629     C     *IN20         OREQ      *ON
020800990920     C     *IN21         OREQ      *ON
020900940613     C                   SETOFF                                       0102
021000940613     C                   SETON                                        17
021100950619     C*  CONTROLLO SE IL FOGLIO SU CUI E' PARTITA LA BOLLA E' ABBINATO
021200950619     C*  PERCHE' COME DATA ARRIVO ULTIMO COLLO DEVE METTERE LA > TRA
021300041217     C*  QUELLA DEI COLLI  E QUELLA DELLA BOLLA solo per DDT SI senza
021400041217     c*  piking
021500041217     C                   CLEAR                   BOLAED
021600041217     C                   CLEAR                   BOLAEH
021700950619    3C     *IN15         IFEQ      *ON
021800041217    4c     §OgNtw        Ifne      'PPT'
021900041216     C* CONSIDERO L'ABBINAMENTO DEL FOGLIO DI BORDERIZZAZIONE SOLO PER
022000041216     C* DDT  SI  E CON LNA CHE NON FA IL PICKING
022100041222     c* considero anche se si trata di manca record in arrivo
022200041217    5c                   if        §ogpck=' '
022300060216     c     kar4a         chain     fiar401l
022400060216    6c                   if         %found(fiar401l)
022500041217     c                   movel     ar4not        dsbl4a
022600041217     c                   else
022700041217     c                   clear                   dsbl4a
022800041217    6c                   endif
022900041217     c
023000041217    6c                   if        §b4abm='S' or §b4abm='C'
023100041222     c                   EXSR      ctrarbnfv
023200041217    6C                   ENDIF
023300041217    5C                   ENDIF
023400001010     C* BOLLA POSTE: NON CALCOLO DATA ARRIVO ULTIMO COLLO
023500041217  X 4C                   ELSE
023600001010     C                   SETOFF                                       17
023700041217    4C                   ENDIF
023800001010    3C                   ENDIF
023900911011     C*
024000990921     C                   CLEAR                   WFVC
024100990921     C                   CLEAR                   WFPC
024200041222     c
024300930518     C* LEGGO I COLLI DELLA BOLLA
024400941031     C     KARB          SETLL     FNART000
024500941031     C     KARB          READE     FNART000                               30
024600041222     c                   clear                   wesiste010        1
024700911011     C*
024800930504DO  3C     *IN30         DOWEQ     '0'
024900041222     c* Memorizzo se presente un manca record: in questo caso devo
025000041222     c*  considerare l'abbinamento del foglio della bolla
025100041222     c                   if        wctrarbnfv=' '
025200041222     c     kanm          setll     fnanm02l                               34
025300050308     c                   if        *in34  or artdam<=arbdbr  and
025400050308     c                             artdam>0
025500041222     c                   exsr      CTRARBNFV
025600041222     c                   endif
025700041222     c                   endif
025800041222     c
025900921231     C*
026000930603    4C     *IN15         IFEQ      '1'
026100930603     C***
026200930603     C*** A B I L I T A Z I O N E   B O L L E   A L L A   C O N S E G.
026300930603     C***
026400940208     C*
026500940613     C* ROUTINE CHE CONTROLLA COLLO PER COLLO SE SONO PARTITI SU FV
026600980114     C*  ABBINATI: SE NE TROVO ALMENO 1 NON ABBINATO NON  POSSO
026700980114     C*  CALCOLARE LA DATA E ORA ARRIVO EFFETTIVO ULTIMO COLLO (17 OFF)
026800940613     C   17              EXSR      RIC2
026900940208     C*
027000950629IF  5C     ARBAMA        IFNE      2
027100921123     C*
027200980114     C* PER L'ABILITAZIONE, SE C'E LA DATA ARRIVO E' COME SE :
027300921123     C* 1) FOSSE PARTITO IL COLLO
027400921123     C* 2) FOSSE ABBINATO IL RELATIVO FOGLIO VIAGGIO
027500921123     C*
027600930914     C* MANCA DATA ARRIVO O COLLO NON ABILITATO:
027700930914     C*                    NON ABILITO IN BASE ALL'ARRIVO
027800930603IF  6C     ARTDAM        IFEQ      0
027900941221     C     ARTDAB        OREQ      'S'
028000921123     C                   SETON                                        02
028100921123     C*
028200921123     C* MANCA ABBINAMENTO: NON ABILITO IN BASE ALLA PARTENZA
028300941221     C* ARTABN= "S" : SIGNIFICA SPUNTA DI FOGLIO ABBINATO
028400941221IF  7C     ARTABN        IFNE      'S'
028500921123     C                   SETON                                        01
028600930603E   7C                   ENDIF
028700930603E   6C                   ENDIF
028800921123     C*
028900921123     C* NON E' POSSIBILE ABBINARE
029000990920     C   01
029100990920     CAN 02
029200990920     CANN20
029300990920     CANN21              SETON                                        30
029400930603E   5C                   ENDIF
029500930603     C***
029600930603     C*** D A T A   A R R I V O   E F F E T T I V O  ULTIMO COLLO BOLLA
029700930603     C***
029800930603     C* SE DATA CONSEGNA EFFETTIVA DELLA BOLLA E' VUOTA
029900940613IF  5C   17ARBDTI        IFEQ      0
030000930603     C*
030100930603     C* SE LA DATA DEL COLLO E' MAGGIORE DELLA DATA SALVATA
030200940613IF  6C     ARTDAM        IFGT      SAVDAM
030300930603     C*
030400930603     C* PULISCO SCHIERE DEI FOGLI VIAGGIO E DEI SEGNACOLLI
030500940613     C                   CLEAR                   NFV
030600940613     C                   CLEAR                   NSC
030700050304     C                   CLEAR                   dut
030800930603     C*
030900930603     C* MEMORIZZO DATA, NUMERO FOGLIO E SEGNACOLLO
031000941102     C                   Z-ADD     ARTDAM        SAVDAM
031100050510     c* Memrizzo se aggiornato con spunta processo ope partenza o arrivo
031200050510     c                   eval      savfl2=artfl2
031300930603     C                   Z-ADD     ARTNFV        NFV(1)
031400941102     C                   Z-ADD     ARTNSC        NSC(1)
031500050304     C                   Z-ADD     ARTdut        dut(1)
031600930603     C                   Z-ADD     1             X                 3 0
031700930603     C*
031800930603     C* DATA ARRIVO MINORE O UGUALE
031900940613X   6C                   ELSE
032000050304IF  7C     ARTDAM        IFEQ      SAVDAM
032100050510     c*
032200050510     c                   if        artfl2='P'
032300050510     c                   eval      savfl2=artfl2
032400050510     c                   endif
032500050510     c
032600050304     c                   z-add     1             yy                4 0
032700050304     C     ARTNFV        LOOKUP    NFV(yy)                                34
032800930603     C*
032900930603     C* F.V. NON MEMORIZZATO (NE CARICO AL MASSIMO 100)
033000950331IF  8C     *IN34         IFEQ      '0'
033100050304    9C     X             iflt      100
033200930603     C                   ADD       1             X
033300930603     C                   Z-ADD     ARTNFV        NFV(X)
033400941102     C                   Z-ADD     ARTNSC        NSC(X)
033500050304     C                   Z-ADD     ARTdut        DUT(X)
033600050304    9c                   endif
033700050304     c*
033800050304   x8c                   else
033900050304     c* Se foglio già presente memorizzo segnacolo con DUT >
034000050304    9c                   if        artdut>dut(yy)
034100050304     C                   Z-ADD     ARTNFV        NFV(yy)
034200050304     C                   Z-ADD     ARTNSC        NSC(yy)
034300050304     C                   Z-ADD     ARTdut        DUT(yy)
034400050304E   9C                   ENDIF
034500050304E   8C                   ENDIF
034600950331E   7C                   ENDIF
034700940613E   6C                   ENDIF
034800930603     C*
034900930603E   5C                   ENDIF
035000930603E   4C                   ENDIF
035100930603     C***
035200930603     C*** V O L U M E   C M L
035300930603     C***
035400950629IF  4C     *IN20         IFEQ      *ON
035500941031     C     ARTVUC        ANDGT     0
035600930503     C*
035700930503     C* CALCOLO IL VOLUME TOTALE DELLA BOLLA E I COLLI RILEVATI
035800941031     C                   ADD       ARTVUC        TOTVUC
035900941031     C                   ADD       1             D20NCR
036000990921     C     ARTFVC        IFNE      'P'
036100990921     C                   MOVEL     ARTFVC        WFVC
036200990921     C                   ENDIF
036300930504E   4C                   ENDIF
036400990806     C*** P E S O   C M L
036500990806     C***
036600990806IF  4C     *IN21         IFEQ      *ON
036700990806     C     ARTPUC        ANDGT     0
036800990806     C*
036900990806     C* CALCOLO IL PESO TOTALE DELLA BOLLA E I COLLI RILEVATI
037000990913     C                   ADD       ARTPUC        TOTPUC
037100990806     C                   ADD       1             D39NCP
037200990921     C     ARTFPC        IFNE      'P'
037300990921     C                   MOVEL     ARTFPC        WFPC
037400990921     C                   ENDIF
037500990806E   4C                   ENDIF
037600930517     C*
037700941031     C  N30KARB          READE     FNART000                               30
037800930504E   3C                   ENDDO
037900930603     C*
038000930603     C*
038100950629    3C     *IN15         IFEQ      *ON
038200930603     C***
038300930603     C*** A B I L I T A Z I O N E   B O L L E   A L L A   C O N S E G.
038400930603     C***
038500911011     C*
038600921123     C* 01 OFF - SEGNACOLLI PARTITI E ABBINATI ==> ABILITO  AMP=1 AMA=1
038700921123     C*          SE AMP=1 AMA SI PUO' PORTARE SOLO A 2
038800930517     C*
038900921123     C* 02 OFF - SEGNACOLLI ARRIVATI           ==> ABILITO  AMP=1 AMA=2
039000911011     C*
039100950629    4C     ARBAMA        IFNE      2
039200930603     C  N01
039300930603     CANN02              MOVEL     1             ARBAMP
039400930603     C  N01
039500930603     CANN02              MOVEL     2             ARBAMA
039600911011     C*
039700950629     C  N01
039800950629IF  5CAN 02ARBAMP        IFEQ      0
039900911011     C                   MOVEL     1             ARBAMP
040000911011     C                   MOVEL     1             ARBAMA
040100950629E   5C                   ENDIF
040200950629E   4C                   ENDIF
040300930603     C***
040400930603     C*** D A T A   A R R I V O   E F F E T T I V O  ULTIMO COLLO BOLLA
040500930603     C***
040600940613    4C   17ARBDTI        IFEQ      0
040700930603     C*
040800930517     C                   Z-ADD     1             X
040900950404     C*
041000950404     C* MEMORIZZO PER POI RICHIAMARE IL RICALCOLO TEMPI DI CONSEGNA
041100950404     C                   MOVE      '1'           W001A             1
041200930517     C*
041300930603DO  5C     NSC(X)        DOWGT     *ZEROS
041400930517     C                   Z-ADD     ARBFLS        KLNP
041500930517     C                   Z-ADD     ARBLNA        KLNA
041600930517     C                   Z-ADD     ARBNRS        KNRS
041700930517     C                   Z-ADD     NSC(X)        KNSC
041800930517     C*
041900930517     C* LEGGO SEGNACOLLO PER REPERIRE LA LNP DEL FOGLIO
042000070123     C     KBRV          SETLL     FNBRV09L
042100070123     C     KBRV          READE     FNBRV09L                               35
042200930517     C*
042300930517     C* F.V. DELLA LNA DELLA BOLLA
042400941102    6C     *IN35         DOWEQ     *OFF
042500970730     C                   CLEAR                   DSLV53
042600970730     C                   MOVEL     'A'           D53TFO
042700040331     c* campo non più usato
042800040331     C***                Z-ADD     SIMFEL        D53FEL
042900970730     C                   Z-ADD     BRVFGS        D53FGS
043000970730     C                   Z-ADD     BRVNFV        D53NFV
043100970730     C                   MOVEL     '1'           D53NPG
043200970730     C                   MOVEL     BRVFLE        D53FLF
043300970730     C                   MOVEL     'A'           D53ABB
043400970730     C                   MOVEL     ARBLNA        D53LNA
043500050603     c* non lo passo perchè potrebbe essere cambiato (ad esempio se collo
043600050603     c* impato o transitato)
043700050603     C***                MOVEL     ARBTFA        D53TFA
043800970730     C                   CALL      'FNLV53R'
043900970730     C                   PARM                    DSLV53
044000930517     C*
044100970730IF  7C     D53ERR        IFEQ      ' '
044200930517     C*
044300940613     C* MEMORIZZO DATA E ORA + RECENTI SOLTANTO SE:
044400940613     C*  IL FV PARTENZA E' STATO ABBINATO A UN FOGLIO ARRIVI
044500950427     C*  CON DATA >= ALLA DATA DI ARRIVO DEL COLLO
044600940613     C*
044700050510     c* Se data arrivo aggiornata con spunta partenza, data foglio arrivi
044800050510     c*  deve essere > (e non uguale) alla stessa data
044900050510     c**
045000050510   7AC**   D53DFA        IFGE      SAVDAM
045100050510     c**
045200050510     c                   if        (savfl2=' ' and d53dfa >=SAVDAM) or
045300050510     c                             (savfl2<>' ' and d53dfa>SAVDAM)
045400940613     C*
045500930517     C* MEMORIZZO SOLO SE LA DATA E L'ORA SONO PIU RECENTI
045600970730IF  8C     D53AED        IFGE      SAVAED
045700970730IF  9C     D53AED        IFGT      SAVAED
045800970730     C                   Z-ADD     D53AED        SAVAED
045900970730     C                   Z-ADD     D53AEH        SAVAEH
046000930607X   9C                   ELSE
046100970730IF 10C     D53AEH        IFGT      SAVAEH
046200970730     C                   Z-ADD     D53AEH        SAVAEH
046300930607E  10C                   ENDIF
046400930607E   9C                   ENDIF
046500930603E   8C                   ENDIF
046600940613     C*
046700940613E  7AC                   ENDIF
046800940613E   7C                   ENDIF
046900930528     C*
047000070123     C     KBRV          READE     FNBRV09L                               35
047100941102E   6C                   ENDDO
047200930517     C*
047300930528     C                   ADD       1             X
047400930603E   5C                   ENDDO
047500940613     C*
047600930603     C* SE DATA ARRIVO PRESENTE IN TUTTI I COLLI DELLA BOLLA
047700950627    5C     SAVAED        IFGT      0
047800950627    6C     BOLAED        IFLT      SAVAED
047900950627     C     BOLAED        OREQ      SAVAED
048000950619     C     BOLAEH        ANDLT     SAVAEH
048100930603     C                   Z-ADD     SAVAED        ARBDTI
048200930603     C                   Z-ADD     SAVAEH        ARBHTI
048300950627   X6C                   ELSE
048400950619     C                   Z-ADD     BOLAED        ARBDTI
048500950619     C                   Z-ADD     BOLAEH        ARBHTI
048600950627E   6C                   ENDIF
048700950627E   5C                   ENDIF
048800950619E   4C                   ENDIF
048900930517     C*
049000931213     C*
049100940613     C* SE  ARBDTI = 0  CERCO NEI FV PART ABBINATI AL FOGLIO ARRIVI
049200940613     C*                 CON DATA = ARTDAM
049300940613    4C   17ARBDTI        IFEQ      0
049400931213     C*
049500931213     C                   CLEAR                   COMDTI
049600931213     C                   CLEAR                   COMHTI
049700020918      * cerco l'As del terminal di arrivo e della linea di arrivo della bolla
049800050510     c* Ora stesso AS: non serve più
049900050510     c***                Exsr      RicAs
050000940613     C*
050100931213     C                   EXSR      RICFV
050200931213     C*
050300931213     C* MEMORIZZO I VALORI TROVATI IN  ARBDTI  E  ARBHTI
050400950619     C* CONFRONTANDO CON LA DATA DI ARRIVO DEL FOGLIO DELLA BOLLA
050500950619     C*
050600950619    5C     BOLAED        IFLT      COMDTI
050700950619    5C     BOLAED        OREQ      COMDTI
050800950619     C     BOLAEH        ANDLT     COMHTI
050900950619     C                   Z-ADD     COMDTI        ARBDTI
051000950619     C                   Z-ADD     COMHTI        ARBHTI
051100950619   X5C                   ELSE
051200950619     C                   Z-ADD     BOLAED        ARBDTI
051300950619     C                   Z-ADD     BOLAEH        ARBHTI
051400950619E   5C                   ENDIF
051500940613    4C                   ENDIF
051600931213     C*
051700930603E   3C                   ENDIF
051800930603E   2C                   ENDIF
051900930603     C***
052000930603     C*** V O L U M E   C M L
052100930603     C***
052200930504IF  2C     *IN20         IFEQ      '1'
052300941031     C     TOTVUC        ANDGT     0
052400941102     C*
052500941102     C* IMPOSTO IL VOLUME CML
052600941102     C                   Z-ADD(H)  TOTVUC        D20VLU
052700950110     C     D20VLU        IFEQ      0
052800950110     C                   Z-ADD     0,001         D20VLU
052900950110     C                   ENDIF
053000930503     C*
053100941031     C                   MOVEL     'A'           D20TBO
053200990921     C* NON DEVO TRASMETTERE LA VARIAZIONE DI VOLUME CML SE
053300990921     C* I VOLUMI SONO DELLA PARTENZA PERCHE' IN QUESTO CASO
053400990921     C* LA TRASMISSIONE ALLA SEDE E' GIA' STATA FATTA DALLA PARTENZA
053500990921     C* (V. TABELLA 7T TRASMISSIONE VOLUMI 'T' E 'Z' DA PARTENZA A
053600990921     C* SEDE --> TALI FLAG SONO IMPOSTATI AD 'S')
053700990921     C* ATTENZIONE QUINDI CHE I FLAG DELLA 7T DIVENTADO FISSO (NON
053800990921     C* MODIFICABILI) ALTRIMENTI NON FUNZIONA PIU' QUESTO GIRO
053900990921     C* COL RISCHIO DI PERDERE DEI VOLUMI CML IN SEDE
054000990921     C     WFVC          IFNE      *BLANKS
054100970901     C                   MOVEL     'S'           D20FTR
054200990921     C                   ELSE
054300990921     C                   MOVEL     ' '           D20FTR
054400990921     C                   ENDIF
054500941031     C                   MOVEL     'C'           D20TVL
054600941031     C                   MOVEL     ARBFVF        D20FVF
054700941031     C                   MOVEL     ARBVLF        D20VLF
054800941031     C                   MOVEL     ARBFVB        D20FVB
054900941031     C                   MOVEL     ARBVLB        D20VLB
055000941031     C                   MOVEL     ARBAAS        D20AAS
055100941031     C                   MOVEL     ARBLNP        D20LNP
055200941031     C                   MOVEL     ARBNRS        D20NRS
055300941031     C                   MOVEL     ARBNSP        D20NSP
055400941031     C                   MOVEL     ARBCBO        D20CBO
055500941031     C                   MOVEL     ARBNCL        D20NCL
055600991116     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
055700991116     C                   MOVEL     '1'           WTRC
055800991116     C     KAR6          CHAIN     FIAR6000                           30
055900941102     C  N30              MOVEL     AR6DFT        D20DFT
056000941031     C                   MOVEL     ARBLNA        D20LNA
056100960927     C*
056200960927     C                   MOVE      ARBDBR        D20DTE
056300941031     C                   MOVEL     DSLV20        KPJBU
056400040510     C* Passo parametri in più se pgm "gufo"
056500111109     c                   if        %parms>=2
056600990806     C                   CALL      'FNLV20R'
056700990806     C                   PARM                    KPJBA
056800040510     c                   parm      'S'           pwstor            1
056900040510     c                   else
057000040510     C                   CALL      'FNLV20R'
057100040510     C                   PARM                    KPJBA
057200040510     c                   endif
057300990806     C*
057400990806     C                   MOVEL     KPJBU         DSLV20
057500990806     C*
057600990806     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
057700990806    4C     D20ERR        IFNE      '1'
057800990806     C                   MOVEL     D20NCR        ARBNCR
057900990806     C                   MOVEL     D20VLU        ARBVLC
058000990806     C                   MOVEL     D20FVF        ARBFVF
058100990806     C                   MOVEL     D20VLF        ARBVLF
058200990806     C**
058300990806   X4C                   ELSE
058400990806     C                   MOVEL     'V'           WFBS              1
058500990806E   4C                   ENDIF
058600990806E   2C                   ENDIF
058700990806     C*
058800990806      * DS PER PESO
058900990806     C*** P E S O    C M L
059000990806     C***
059100990806IF  2C     *IN21         IFEQ      '1'
059200990806     C     TOTPUC        ANDGT     0
059300990806     C*
059400990806     C* IMPOSTO IL VOLUME CML
059500990806     C                   Z-ADD(H)  TOTPUC        D39PLU
059600990806     C     D39PLU        IFEQ      0
059700990806     C                   Z-ADD     0,001         D39PLU
059800990806     C                   ENDIF
059900990806     C                   MOVEL     'A'           D39TBO
060000990921     C* PER EVITARE DI TRASMETTERE SEMPRE IL PESO CML ALLA SEDE
060100990921     C* ABBIAMO IMPOSTATO AD 'S' IL FLAG DI TRASMISSIONE PESO CML
060200990921     C* DA PARTENZA A SEDE  (V.TABELLA 7Y) E IN ARRIVO LA TRASMISSIONE
060300990921     C* VERRA' EFFETTUATA SOLO SE IL PESO NON E' DELLA PARTENZA
060400990921     C* ATTENZIONE QUINDI CHE IL FLAG DELLA 7Y DIVENTA FISSO (NON
060500990921     C* MODIFICABILE) ALTRIMENTI NON FUNZIONA PIU' QUESTO GIRO
060600990921     C* COL RISCHIO DI PERDERE DEI PESI CML IN SEDE
060700990921     C     WFPC          IFNE      *BLANKS
060800990921     C                   MOVEL     'S'           D39FTR
060900990921     C                   ELSE
061000990921     C                   MOVEL     ' '           D39FTR
061100990921     C                   ENDIF
061200990806     C                   MOVEL     'C'           D39TPP
061300990806     C                   MOVEL     ARBPKF        D39PLF
061400990806     C                   MOVEL     ARBPKB        D39PLB
061500990806     C                   MOVEL     ARBAAS        D39AAS
061600990806     C                   MOVEL     ARBLNP        D39LNP
061700990806     C                   MOVEL     ARBNRS        D39NRS
061800990806     C                   MOVEL     ARBNSP        D39NSP
061900990806     C                   MOVEL     ARBCBO        D39CBO
062000990806     C                   MOVEL     ARBNCL        D39NCL
062100931213     C*
062200990806     C  N30              MOVEL     AR6DFT        D39DFT
062300990806     C                   MOVEL     ARBLNA        D39LNA
062400990806     C                   MOVE      ARBDBR        D39DTE
062500990806     C*
062600990806     C                   MOVEL     DSLV39        KPJBU
062700040510     C                   CALL      'FNLV39R'
062800040510     C                   PARM                    KPJBA
062900941031     C*
063000990806     C                   MOVEL     KPJBU         DSLV39
063100941031     C*
063200970616     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
063300990806    4C     D39ERR        IFNE      '1'
063400990806     C                   MOVEL     D39NCP        ARBNCP
063500990806     C                   MOVEL     D39PLU        ARBPKC
063600990806     C                   MOVEL     D39PLF        ARBPKF
063700970924     C**
063800970924   X4C                   ELSE
063900070704     c                   if        wfbs=' '
064000990806     C                   MOVEL     'P'           WFBS              1
064100070704     c                   else
064200070704     C                   MOVEL     'E'           WFBS              1
064300070704     c                   endif
064400070704     c
064500941102E   4C                   ENDIF
064600990806     C                   ENDIF
064700950323     C*
064800950323     C* AGGIORNO FILE TASSAZIONE PADRONCINI
064900990806     C     *IN20         IFEQ      *ON
065000040205     C     *IN21         oreq      *ON
065100950323     C                   EXSR      AGGTAS
065200070703     c* se non sono riuscita ad aggiornare i padroncini, reimposto
065300070703     c*  per elaborare
065400070703     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
065500070704    4C     o57err        IFNE      ' '
065600070704     c* Se modificato il peso
065700070704    5c                   if        *in21
065800070703     C                   MOVEL     savNCP        ARBNCP
065900070703     C                   MOVEL     savPKC        ARBPKC
066000070703     C                   MOVEL     savPKF        ARBPKF
066100070703     C                   MOVEL     'P'           WFBS              1
066200070704E   5C                   ENDIF
066300070704     c* Se modificato il volume
066400070704    5c                   if        *in20
066500070704     C                   MOVEL     savNCR        ARBNCR
066600070704     C                   MOVEL     savvlc        ARBVLC
066700070704     C                   MOVEL     savFVF        ARBFVF
066800070704     C                   MOVEL     savVLF        ARBVLF
066900070704    6c                   if        wfbs=' '
067000070704     C                   MOVEL     'V'           WFBS              1
067100070704     c                   else
067200070704     C                   MOVEL     'E'           WFBS              1
067300070704    6c                   endif
067400070704E   5C                   ENDIF
067500070704     c
067600070704E   4C                   ENDIF
067700070704    3C                   END
067800930503     C*
067900070703     C* Aggiorno bolla
068000970924     C                   MOVEL     WFBS          ARBFBS
068100970924     C                   CLEAR                   WFBS
068200930503     C*
068300941031     C                   UPDATE    FNARB000
068400950404     C*
068500950404     C     W001A         IFNE      *BLANKS
068600950404     C     ARBDCM        ANDNE     *ZEROS
068700950404     C     ARBDCE        ANDNE     *ZEROS
068800950404     C                   EXSR      SRTCON
068900950404     C                   END
069000911011     C*
069100941031     C                   READ      FNARB000                             1031
069200930504     C   10              SETON                                        31
069300930504E   1C                   ENDDO
069400111006     c
069500111109     c*
069600111006     c                   clear                   SItrasm           1
069700040401     c*
069800040505     c* Se passato un parametro solo chiudo con LR
069900040505    1c                   if        %PARMs<2
070000040505     C                   SETON                                        LR
070100040505   x1c                   else
070200111109     c
070300040505     C* CONTROLLA SE E' STATA RICHIESTA LA CHIUSURA DEL SOTTOSISTEMA
070400040505     C                   SHTDN                                        98
070500040505     c
070600040505    2c                   if        *in98
070700040505     c                   eval      chiudi='S'
070800040505     C                   SETON                                        LR
070900111006     c                   eval      SItrasm='S'
071000040505   x2c                   else
071100040505     c
071200040505     c* Se non sono passate 2 ore accendo RT altrimenti LR
071300040505     c*  e sottometto trasmissione
071400111109     c**                 time                    worad
071500111109     c**   worad         subdur    worap         minuti:*MN
071600111109    3c**                 if        minuti>120
071700111109     C*+                 SETON                                        LR
071800111109     c**                 eval      SItrasm='S'
071900111109   x3c**                 else
072000111006     C*******            SETON                                        RT
072100111109     c
072200111109     c* Visto che non posso trasmettere ogni due ore perchè chiudo il LR
072300111109     c*  conto i passaggi e ogni  240 volte trasmetto
072400111109     c
072500111109     c                   eval      *in32 = *off
072600111109     c     c_Digits      checkr    numela                                 32
072700111109     c
072800111109if  3c                   if        *in32
072900111109     c                   eval      numela='001'
073000111109   x3c                   else
073100111109     c                   movel     numela        w0030             3 0
073200111109     c                   add       1             w0030
073300111109     c                   movel     w0030         numela
073400111109    4c                   if        w0030>= 2
073500111109     c                   eval      SItrasm='S'
073600111109    4c                   endif
073700111109     c
073800111109     c*
073900111006     C                   SETON                                        LR
074000041104    3c                   endif
074100041104    2c                   endif
074200041104     c
074300040505     c* Sottometto lsa3 per trasmissione
074400041104    2c                   if        *inlr
074500041104     c* chiudo i pgm del peso e volume
074600041104     C                   CLEAR                   DSLV39
074700041104     C                   MOVEL     'C'           D39TLA
074800041104     C                   MOVEL     DSLV39        KPJBU
074900041104     C                   CALL      'FNLV39R'
075000041104     C                   PARM                    KPJBA
075100041108     c*
075200041108     C                   CLEAR                   DSLV20
075300041108     C                   MOVEL     'C'           D20TLA
075400041108     C                   MOVEL     DSLV20        KPJBU
075500041108     C                   CALL      'FNLV20R'
075600041108     C                   PARM                    KPJBA
075700041104     c
075800111006     c                   if        SItrasm='S'
075900040505     c                   eval      pa3nfv='99999'
076000070502     c                   eval      pa3tfp='   '
076100040505     c                   movel     param3        kpjbu
076200040505     c                   eval      trasmpesvol='S'
076300040505     c                   call      'FNLSA3R'
076400040505     c                   parm                    kpjba
076500040505     c                   parm                    trasmpesvol       1
076600040505    2c                   endif
076700111006     c
076800111006    2c                   endif
076900040505    1c                   endif
077000040505     C**
077100040505     c                   if        *inlr
077200040505     C                   CLEAR                   DSLV57
077300040505     C                   MOVEL     'C'           I57TLA
077400040505     C                   CALL      'FNLV57R'
077500040505     C                   PARM                    KPJBA
077600040505     C                   PARM                    DSLV57
077700040505     C*
077800040505     C                   MOVEL     'C'           D55TLA
077900040505     C                   CALL      'FNLV55R'
078000040505     C                   PARM                    DSLV55
078100111010     C*
078200111010     C                   MOVEL     'C'           D53TLA
078300111010     C                   CALL      'FNLV53R'
078400111010     C                   PARM                    DSLV53
078500040505    1c                   endif
078600931210     C*---------------------------------------------------------------*
078700931210     C*                    RICFV                                      *
078800931210     C* ROUTINE DI RICERCA FOGLIO VIAGGIO PARTENZA                    *
078900931210     C*---------------------------------------------------------------*
079000931210     C     RICFV         BEGSR
079100050510     c                   eval      Wgiro = *off
079200050510     C                   Z-ADD     2             COMNPG
079300050510
079400050510      * Devo leggere fnfvv con terminal di arrivo bolla nel caso in cui
079500050510      * il terminal di arrivo della bolla e la linea di arrivo della bolla sono sullo
079600050510      * stesso As (ora sempre)
079700050510     c**                 If        TfaOgas = LnaOgas
079800050510     c                   Eval      Kfgs = ArbTfa
079900050510     c**                 Else
080000050510      * se non sono sullo stesso As leggo fnfvv con linea di arrivo della bolla
080100050510     c**                 Eval      Kfgs = ArbLna
080200050510     c**                 EndIf
080300050510     c
080400050510     c* Se bolla aggioranta con spunte processo ope partenza, devo
080500050510     c*  cercare un foglio arrivi con data > di SAVDAM
080600050510     c                   if        savfl2=' '
080700050510     c                   eval      kdam=savdam
080800050510     c                   else
080900050510     c*
081000050510     C     KFVV08        SETgt     FNFVV08
081100050510     C     KFVV08b       READE     FNFVV08                                32
081200050510     c                   if        not *in32
081300050510     c                   eval      kdam=fvvdfv
081400050510     c                   else
081500050510     c
081600050510     c* Foglio arrivi non trovato
081700050510     c* imposto SAVDAM+1
081800050510     c     *iso          movel     savdam        dataiso
081900050510     c                   adddur    1:*d          dataiso
082000050510     c                   move      dataiso       comdti
082100050510     C                   Z-ADD     0101          COMHTI
082200050510     c                   goto      endric
082300050510     c                   endif
082400050510     c
082500050510     c                   endif
082600020919
082700020919      * imposto un flag di comodo x fare il secondo tentativo con lo stesso npg
082800020919     c     rileggi       tag
082900940613     C*
083000940613     C* NEL 1 GIRO
083100050510     C* CERCO IL FOGLIO ARRIVI CON  NPG=2  E  DFV=SAVDAM o >savdam
083200940613     C* NEL 2 GIRO
083300050510     C* CERCO IL FOGLIO ARRIVI CON  NPG=6  E  DFV=SAVDAM o >savdam
083400050914     C* SE NON L'HO TROVATO AL 1 GIRO (non più)
083500050914     C****               Z-ADD     1             Y                 2 0
083600940613     C*
083700050914   -1C**   Y             DOWLE     2
083800050914     C**   COMDTI        ANDEQ     0
083900940613     C*
084000941102     C     KFVV          SETLL     FNFVV000
084100941102     C     KFVV          READE     FNFVV000                               32
084200940613     C*
084300940613    0C     *IN32         DOWEQ     *OFF
084400940613     C*
084500940613     C* CERCO IL F.VIAGGIO PARTENZA (ABBINATO SUL FOGLIO ARRIVI TROVATO
084600940613     C*   CHE CONTIENE LA LINEA DEL COLLO CON ARTDAM PIU'  RECENTE
084700931215     C*
084800940613     C* LEGGO FLFVA02L CON         NFA=FVVNFV
084900941102     C     KFVA2         SETLL     FNFVA02
085000941102     C     KFVA2         READE     FNFVA02                                33
085100931210     C*
085200931213    1C     *IN33         DOWEQ     *OFF
085300970708     C                   CLEAR                   DSLV53
085400970708     C                   MOVEL     'A'           D53TFO
085500040331     c* campo non più usato
085600040331     C***                MOVEL     SIMFEL        D53FEL
085700970708     C                   MOVEL     FVANFV        D53NFV
085800970708     C                   MOVEL     1             D53NPG
085900970708     C                   MOVEL     FVALNP        D53FGS
086000970708     C                   MOVEL     FVALNP        D53FLF
086100970708     C                   MOVEL     FVALAI        D53LAI
086200031212     C                   MOVEL     'B'           D53ABB
086300970708     C                   MOVEL     ARBLNA        D53LNA
086400050603     C****               MOVEL     ARBTFA        D53TFA
086500970708     C                   CALL      'FNLV53R'
086600970708     C                   PARM                    DSLV53
086700970730     C**
086800970730     C* LEGGO QUESTO FNFVA SOLO SE IL PGM HA TROVATO CHE E' UN
086900970730     C*  FOGLIO DELLA LINEA DI ARRIVO
087000970730    2C     D53ERR        IFEQ      ' '
087100050914     C* SE FVALNP = TERMINAL PART. BOLLA
087200970730     C     FVALNP        ANDEQ     ARBTFP
087300050914     c                   if        comdti=0 or (comdti<fvaaed) or
087400050914     c                             (comdti=fvaaed and fvaaeh>comhti)
087500931214     C                   Z-ADD     FVAAEH        COMHTI
087600940613     C                   Z-ADD     FVAAED        COMDTI
087700050914     c                   endif
087800050913     c
087900050913   X2C***                else
088000050913    2C                   ENDIF
088100970730     C* RILEGGO
088200050914     C     KFVA2         READE     FNFVA02                                33
088300050913    2C***                ENDIF
088400931213    1C                   ENDDO
088500940613     C*
088600941102     C     KFVV          READE     FNFVV000                               32
088700940613     C*
088800940613     C* SE GIA' TROVATO NON LEGGO PIU'
088900050914     c*  non lo faccio più perchè tengo la data + alta quindi devo leggere
089000050914     c*  tutto
089100050914     C**   COMDTI        IFGT      0
089200050914     C**                 SETON                                        32
089300050914     C**                 ENDIF
089400940613    0C                   ENDDO
089500940613     C*
089600050914     c* tolgo la cat. 6 non più usata
089700050914     C*****              ADD       1             Y
089800050914     C*****              Z-ADD     6             COMNPG
089900050914   -1C*****              ENDDO
090000020919
090100050913      * Se non ho trovato e terminal di arrivo e linea di arrivo della bolla
090200020919      * sono su stesso AS devo fare un altro tentativo con la linea di arrivo della bolla
090300050510     c***                If        Comdti = *zeros and TfaOgas = LnaOgas
090400050510     c                   If        Comdti = *zeros
090500020919     c                             and Wgiro = *off and
090600020919     c                             arbtfa<>arblna
090700020919     c                   Eval      Kfgs = ArbLna
090800020919     c                   Eval      Wgiro = *on
090900020919     c                   GoTo      Rileggi
091000020919     c                   EndIf
091100931210     C*
091200940613     C     COMDTI        IFEQ      0
091300931210     C                   Z-ADD     0101          COMHTI
091400050510     C                   Z-ADD     kdam          COMDTI
091500931210     C                   ENDIF
091600931210     C*
091700050510     C     endric        ENDSR
091800940208     C*---------------------------------------------------------------*
091900980114     C* CONTROLLO SE CALCOLARE LA DATA ARRIVO ULTIMO COLLO
092000940208     C*---------------------------------------------------------------*
092100940208     C     RIC2          BEGSR
092200940613     C* SE COLLO NON ARRIVATO NON POSSO CALCOLARE LA DATA E ORA
092300940613     C*  ARRIVO ULTIMO COLLO
092400940613    1C     ARTDAM        IFEQ      0
092500940613     C                   SETOFF                                       17
092600941102   X1C                   ELSE
092700940208     C*
092800950123     C* ARTABN = "S" COLLO DI FV PARTENZA ABBINATO
092900950123    2C     ARTABN        IFNE      'S'
093000980114     C* NON LO CALCOLO SE C'E' LA DATA DI PARTENZA E QUINDI LA SPUNTA
093100980114     C*  NON ANCORA ABBINATA. IL PULIZIA SPUNTE NON CANCELLO SE IL
093200980114     C*  RELATIVO FOGLIO NON E' ABBINATO ANCHE SE CONSEGNARA
093300980114    3C     ARTDFV        IFGT      0
093400980114     C     ARTFLP        ANDEQ     0
093500980114     C     ARTDUT        ORGT      0
093600980114     C     ARTFLP        ANDGT     0
093700980114     C                   SETOFF                                       17
093800980114    3C                   ENDIF
093900980114    2C                   ENDIF
094000980114    1C                   ENDIF
094100940208     C*
094200940208     C                   ENDSR
094300950323     C*---------------------------------------------------------------*
094400950323     C*                    AGGTAS                                     *
094500950323     C* AGGIORNO FILE TASSAZIONE PADRONCINI                           *
094600950323     C*---------------------------------------------------------------*
094700950323     C     AGGTAS        BEGSR
094800950323     C*
094900970616     C* MODIFICATO VOLUME CML: SISTEMO IL VOLUME ANCHE NEI PADRONCINI
095000950323    1C     ARBVLC        IFNE      SAVVLC
095100040205    1C     ARBPKC        ORNE      SAVPKC
095200970924     C                   CLEAR                   DSLV57
095300970924     C                   MOVEL     'A'           I57TBO
095400970924     C                   MOVEL     ARBAAS        I57AAS
095500970924     C                   MOVEL     ARBLNP        I57LNP
095600970924     C                   MOVEL     ARBNRS        I57NRS
095700970924     C                   MOVEL     ARBNSP        I57NSP
095800970924     C                   MOVEL     ARBPDC        I57PDR
095900970924     C                   MOVEL     ARBNCL        I57NCL
096000970924     C                   MOVEL     'C'           I57TVL
096100970924     C                   MOVEL     SAVNCR        I57NCR
096200970924     C                   MOVEL     SAVVLC        I57VLC
096300990920     C                   MOVEL     SAVVLF        I57VLF
096400970924     C                   MOVEL     ARBVLC        I57VCN
096500970924     C                   MOVEL     ARBNCR        I57NCN
096600970924     C                   MOVEL     ARBVLF        I57VFN
096700040205     C                   MOVEL     SAVNCp        I57nCp
096800040205     C                   MOVEL     SAVpkC        I57pkC
096900040205     C                   MOVEL     SAVpkF        I57pkF
097000040205     C                   MOVEL     arbNCp        I57npn
097100040205     C                   MOVEL     arbpkC        I57pcn
097200040205     C                   MOVEL     arbpkF        I57pfn
097300970924     C                   CALL      'FNLV57R'
097400970924     C                   PARM                    KPJBA
097500970924     C                   PARM                    DSLV57
097600970924     C                   ENDIF
097700970616     C*
097800950323     C                   ENDSR
097900950404     C*---------------------------------------------------------------*
098000950404     C*                    SRTCON                                     *
098100950404     C* RICALCOLO TEMPI DI CONSEGNA                                   *
098200950404     C*---------------------------------------------------------------*
098300950404     C     SRTCON        BEGSR
098400950404     C* SE SPEDIZIONE GIà CONSEGNATA E GIA' CALCOLATI I TEMPI DI
098500950404     C* CONSEGNA LI RICALCOLO: I TEMPI POTEVANO ESSERE STATI CALCOLATI
098600950404     C* QUANDO ANCORA NON ERANO STATI ABBINATI I F.V. E QUINDI OCCORRE
098700950404     C* RICALCOLARLI PER AVERE I TEMPI GIUSTI.
098800970925     C                   CLEAR                   DS99
098900950404     C                   MOVEL     ' '           D98TLA
099000950404     C                   MOVEL     'A'           D98TBO
099100950404     C                   Z-ADD     ARBAAS        D98AAS
099200950404     C                   Z-ADD     ARBLNP        D98LNP
099300950404     C                   Z-ADD     ARBNRS        D98NRS
099400950404     C                   Z-ADD     ARBNSP        D98NSP
099500950404     C*
099600970925     C                   CALL      'TNSD99R'
099700970925     C                   PARM                    DS99
099800950404     C* SE TEMPI DIVERSI
099900950404     C     D98DCE        IFNE      ARBDCE
100000950404     C     D98NPI        ORNE      ARBNPI
100100950404     C     D98NCI        ORNE      ARBNCI
100200950404     C     D98NRC        ORNE      ARBNRC
100300950404     C     KARB          CHAIN     FNARB1                             31
100400960906     C* Determino se tipo bolla deve pervenire in sede (*IN08 OFF)
100500960906     C                   SETOFF                                       08
100600960906     C                   MOVEL     '3A'          COD
100700960906     C                   MOVEL(P)  ARBCBO        KEY
100800960906     C     KTAB          CHAIN     TABEL00F                           38
100900960906     C     *IN38         IFEQ      *OFF
101000960906    6C     TBLFLG        ANDEQ     ' '
101100960906     C                   MOVEL     TBLUNI        DS3A
101200960906     C     §3ABSD        COMP      ' '                                0808
101300960906     C                   END
101400950404     C* Richiamo routine di Addpfm/Allocazione/Ovrdbf
101500950404     C                   EXSR      SRARBC
101600950404     C     *IN05         IFEQ      *OFF
101700971120     C                   OPEN      FNARBC0T
101800010103     C  N08              OPEN      FIARBC46
101900950404     C                   Z-ADD     D98DCE        ARBDCE
102000950404     C                   Z-ADD     D98NPI        ARBNPI
102100950404     C                   Z-ADD     D98NCI        ARBNCI
102200950404     C                   Z-ADD     D98NRC        ARBNRC
102300950404     C                   MOVEL     'C'           ARBFAG
102400971120     C                   WRITE     FNARBC00
102500010103     C  N08              WRITE     FIARBC00
102600971120     C                   CLOSE     FNARBC0T
102700010103     C  N08              CLOSE     FIARBC46
102800950404     C* Richiamo routine di Disallocazione/Dltovr
102900100915     c                   eval      soloarbc0t=' '
103000950404     C                   EXSR      DISALL
103100100915
103200950404     C                   ELSE
103300960125     C                   MOVEL     'S'           ARBFBS
103400950404     C                   MOVE      *ZEROS        ARBDTI
103500950404     C                   MOVE      *ZEROS        ARBHTI
103600950404     C                   END
103700950404     C*
103800950404     C                   UPDATE    FNARB1
103900950405     C*
104000950405     C                   END
104100950404     C*
104200950404     C                   ENDSR
104300950403     C*---------------------------------------------------------------*
104400950403     C*                    SRARBC                                     *
104500950403     C* ADDPFM/ALLOCAZIONE/OPEN FILE DI TRASMISSIONE                  *
104600950403     C*---------------------------------------------------------------*
104700950403     C     SRARBC        BEGSR
104800950403     C*
104900950403     C                   MOVE      *BLANKS       $MBR
105000950403     C                   MOVE      'M'           EMME                            DESTINAZIONE
105100040401     C***                move      simfel        xsimfe
105200040513     c                   if        simfel=946
105300040401     C                   MOVE      '026'         XSIMFE
105400040513     c                   else
105500040513     C                   move      simfel        xsimfe
105600040513     c                   endif
105700971120     C*
105800971120     C                   CLEAR                   DSLV55
105900971120     C                   MOVEL     'P'           D55TPT
106000971120     C                   MOVE      ARBLNP        D55LIN
106100971120     C                   MOVE      DATEU         D55DRF
106200971120     C                   CALL      'FNLV55R'
106300971120     C                   PARM                    DSLV55
106400971120     C                   MOVE      D55TFP        $SIMFE
106500970224     C*
106600950403      * Richiamo a pgm per addpfm
106700950403     C                   MOVE      *BLANKS       FILE
106800971120     C                   MOVEL     'FNARBC0T'    FILE
106900950403     C                   CALL      'TRUL50C'
107000950403     C                   PARM                    FILE             10
107100950403     C                   PARM                    $MBR
107200950403     C                   PARM                    KNSIF
107300950403     C                   PARM                    LOGICO           10
107400950403     C                   PARM                    LOGMBR           10
107500950403     C                   PARM                    FLG               1
107600950403      * Tento l'allocazione del file di trasmissione
107700950403     C                   EXSR      ALLOCA
107800950403      * Proseguo solo se riuscita l'allocazione
107900950404     C  N05              DO
108000950403      * OVRDBF e apertura del file
108100950403     C                   MOVE      COM(1)        $COMOD                         DI COMODO
108200950403     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
108300950403     C                   MOVEA     FILE          CMD(13)
108400950403     C                   MOVEA     $MBR          CMD(29)
108500950403     C                   MOVEA     CMD           COMMAN
108600950403     C                   Z-ADD     80            LUNG
108700950403     C                   CALL      'QCMDEXC'
108800950403     C                   PARM                    COMMAN
108900950403     C                   PARM                    LUNG
109000980720     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
109100980720     C                   MOVEA     FILE2         CMD(13)
109200980720     C                   MOVEA     BLANK         CMD(29)
109300980720     C                   MOVEA     CMD           COMMAN
109400980720     C                   Z-ADD     80            LUNG
109500980720     C                   CALL      'QCMDEXC'
109600980720     C                   PARM                    COMMAN
109700980720     C                   PARM                    LUNG
109800950403     C                   ENDDO
109900950403     C*
110000950403     C                   ENDSR
110100950403      **************************************************************************
110200950403      * Allocazione di un archivio (FILE)
110300950403      **************************************************************************
110400950403     C     ALLOCA        BEGSR
110500950403     C*
110600971120     C* ALLOCAZIONE FNARBC0T
110700950403     C                   MOVE      COM(2)        $COMOD           80            DI COMODO
110800950403     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
110900950403     C                   MOVEA     FILE          CMD(13)
111000950403     C                   MOVEA     $MBR          CMD(38)
111100950403     C                   MOVEA     CMD           COMMAN
111200950403     C                   Z-ADD     80            LUNG
111300950404     C                   CALL      'QCMDEXC'                            05
111400950403     C                   PARM                    COMMAN           80
111500950403     C                   PARM                    LUNG             15 5
111600010103     C* ALLOCAZIONE FIARBC46
111700950404     C     *IN05         IFEQ      *OFF
111800960906     C     *IN08         ANDEQ     *OFF
111900950405     C                   MOVE      *BLANKS       FILE2            10
112000010103     C                   MOVEL     'FIARBC46'    FILE2
112100020927     C                   eval      BLANK = 'M' + xsimfe + '046'
112200950403     C                   MOVEA     FILE2         CMD(13)
112300950411     C                   MOVEA     BLANK         CMD(38)
112400950403     C                   MOVEA     CMD           COMMAN
112500950404     C                   CALL      'QCMDEXC'                            05
112600950403     C                   PARM                    COMMAN           80
112700950403     C                   PARM                    LUNG             15 5
112800100915     c* Se allocazione non riuscita disalloco fnarbc0t
112900100915     c                   eval      soloarbc0t='S'
113000100915     c                   exsr      disall
113100100915     c                   eval      soloarbc0t=' '
113200950403     C                   END
113300950403      *
113400950403     C                   ENDSR
113500950404      **************************************************************************
113600950404      * Dssallocazione di un archivio (FILE) con membro ($MBR)
113700950404      **************************************************************************
113800950404     C     DISALL        BEGSR
113900950404      *
114000950404     C                   MOVE      COM(3)        $COMOD                         DI COMODO
114100950404     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
114200950404     C                   MOVEA     FILE          CMD(13)
114300950404     C                   MOVEA     $MBR          CMD(38)
114400950404     C                   MOVEA     CMD           COMMAN
114500950404     C                   Z-ADD     80            LUNG
114600950404     C                   CALL      'QCMDEXC'                            99
114700950404     C                   PARM                    COMMAN
114800950404     C                   PARM                    LUNG
114900950404     C*
115000100915     c                   if        not *in08 and soloarbc0t =' '
115100100915     C     *IN08         IFEQ      *OFF
115200950404     C                   MOVE      COM(3)        $COMOD                         DI COMODO
115300950404     C                   MOVEA     $COMOD        CMD                            NELLA SCHIERA
115400950404     C                   MOVEA     FILE2         CMD(13)
115500950411     C                   MOVEA     BLANK         CMD(38)
115600950404     C                   MOVEA     CMD           COMMAN
115700950404     C                   Z-ADD     80            LUNG
115800950404     C                   CALL      'QCMDEXC'                            99
115900950404     C                   PARM                    COMMAN
116000950404     C                   PARM                    LUNG
116100960906     C                   END
116200971120      * DLTOVR di FNARBC0T
116300971120     C                   MOVEL(P)  COM(4)        COMMAN
116400950404     C                   Z-ADD     80            LUNG
116500100915     C                   CALL      'QCMDEXC'                            99
116600950404     C                   PARM                    COMMAN
116700950404     C                   PARM                    LUNG
116800100915     c                   endif
116900950404      *
117000950404     C                   ENDSR
117100041222     C*---------------------------------------------------------------*
117200041222     C* controllo abbinamento foglio bolla
117300041222     C*---------------------------------------------------------------*
117400041222     c     CTRARBNFV     begsr
117500041222     C                   CLEAR                   DSLV53
117600041222     C                   MOVEL     'A'           D53TFO
117700041222     c* Campo non più usato
117800041222     C***                Z-ADD     SIMFEL        D53FEL
117900041222     C                   Z-ADD     ARBFLE        D53FGS
118000041222     C                   Z-ADD     ARBNFV        D53NFV
118100041222     C                   MOVEL     '1'           D53NPG
118200041222     C                   MOVEL     ARBFLE        D53FLF
118300041222     C                   MOVEL     'B'           D53ABB
118400041222     C                   MOVEL     ARBLNA        D53LNA
118500041222     C                   MOVEL     ARBTFA        D53TFA
118600041222     C                   CALL      'FNLV53R'
118700041222     C                   PARM                    DSLV53
118800041222     C* SE NON C'E' ERRORE VUOL DIRE CHE HO TROVATO IL FOGLIO
118900041222    1C     D53ERR        IFEQ      ' '
119000041222    2C     D53AED        IFEQ      0
119100041222     C                   SETOFF                                       17
119200041222   X2C                   ELSE
119300041222     C                   MOVEL     D53AED        BOLAED
119400041222     C                   MOVEL     D53AEH        BOLAEH
119500041222    2C                   ENDIF
119600041222   X1C                   ELSE
119700041222     C* SE NON TROVATO FOGLIO --> NON METTO LA DATA ARRIVO ULTIMO COLLO
119800041222     C                   SETOFF                                       17
119900041222    1C                   ENDIF
120000041222     c*
120100041222     c                   eval      wctrarbnfv='S'
120200041222     c                   ENDSR
120300930430     C*---------------------------------------------------------------*
120400930430     C*                    DEFVAR                                     *
120500930430     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
120600930430     C*---------------------------------------------------------------*
120700040401     C     *inzsr        BEGSR
120800930430     C*
120900930430     C*
121000930430     C                   Z-ADD     1             CODUT             1 0
121100040513     C                   CALL      'X§PARUT'
121200040513     C                   PARM                    UT§DSE
121300040513     C                   MOVEL     REC80         CNCR80
121400930505     C*
121500941102     C                   Z-ADD     1             KNPG
121600971120     C*
121700971120     C* DETERMINO DATA DEL GIORNO
121800971120     C                   TIME                    W0140            14 0
121900971120     C                   MOVE      W0140         WDTGIO            8 0
122000971120     C                   CLEAR                   WLBDAT
122100971120     C                   Z-ADD     WDTGIO        G02DAT
122200971120     C                   CALL      'XSRDA8'
122300971120     C                   PARM                    WLBDAT
122400971120     C                   MOVEL     G02INV        DATEU             8 0
122500040505     c* Prendo ora di apertura pgm
122600040505     C                   time                    worap
122700930503     C*
122800931210     C* DEFINIZIONE DI ALCUNI CAMPI
122900001010     C     *LIKE         DEFINE    ARBLNP        SAVLNP
123000041216     C     *LIKE         DEFINE    ARBLNP        SAVLNA
123100001010     C     *LIKE         DEFINE    ARBLNA        WLNA
123200961129     C     *LIKE         DEFINE    ARBDTI        COMDTI
123300931210     C     *LIKE         DEFINE    ARBHTI        COMHTI
123400931210     C     *LIKE         DEFINE    FVANPG        COMNPG
123500941102     C     *LIKE         DEFINE    FVANPG        KNPG
123600941102     C     *LIKE         DEFINE    FVAAED        SAVAED
123700941102     C     *LIKE         DEFINE    FVAAEH        SAVAEH
123800950619     C     *LIKE         DEFINE    FVAAED        BOLAED
123900950619     C     *LIKE         DEFINE    FVAAEH        BOLAEH
124000941102     C     *LIKE         DEFINE    FVVDFV        SAVDAM
124100950323     C     *LIKE         DEFINE    ARBNCR        SAVNCR
124200950323     C     *LIKE         DEFINE    ARBVLC        SAVVLC
124300950323     C     *LIKE         DEFINE    ARBVLF        SAVVLF
124400070704     C     *LIKE         DEFINE    ARBfvF        SAVfvF
124500990806     C     *LIKE         DEFINE    ARBPKF        SAVPKF
124600990806     C     *LIKE         DEFINE    ARBPKC        SAVPKC
124700990806     C     *LIKE         DEFINE    ARBNCP        SAVNCP
124800950323     C     *LIKE         DEFINE    ARBVLF        WADDV
124900950323     C     *LIKE         DEFINE    ARBVLF        WSOTV
125000960906     C     *LIKE         DEFINE    TBLKEY        KEY
125100960906     C     *LIKE         DEFINE    TBLCOD        COD
125200990921     C     *LIKE         DEFINE    ARTFVC        WFVC
125300990921     C     *LIKE         DEFINE    ARTFPC        WFPC
125400991116     C     *LIKE         DEFINE    AR6TRC        WTRC
125500931210     C*
125600941031     C* LETTURA FNARB
125700930430     C     KARB          KLIST
125800930430     C                   KFLD                    ARBAAS
125900930430     C                   KFLD                    ARBLNP
126000930430     C                   KFLD                    ARBNRS
126100930430     C                   KFLD                    ARBNSP
126200991116     C*
126300991116     C     KAR6          KLIST
126400991116     C                   KFLD                    ARBAAS
126500991116     C                   KFLD                    ARBLNP
126600991116     C                   KFLD                    ARBNRS
126700991116     C                   KFLD                    ARBNSP
126800991116     C                   KFLD                    WTRC
126900041217     C     KAR4a         KLIST
127000041217     C                   KFLD                    ARBAAS
127100041217     C                   KFLD                    ARBLNP
127200041217     C                   KFLD                    ARBNRS
127300041217     C                   KFLD                    ARBNSP
127400041217     C                   KFLD                    ktrc
127500041222     C     KAnm          KLIST
127600041222     C                   KFLD                    ARtfls
127700041222     C                   KFLD                    ARtlna
127800041222     C                   KFLD                    ARtNRS
127900041222     C                   KFLD                    ARtNSc
128000041222     C                   KFLD                    kcaa
128100930517     C*
128200070123     C* LETTURA FNBRV09L
128300941102     C     KBRV          KLIST
128400941102     C                   KFLD                    KNPG
128500941102     C                   KFLD                    KLNP              3 0
128600960304     C                   KFLD                    KLNA              3 0
128700930517     C                   KFLD                    KNRS              2 0
128800930517     C                   KFLD                    KNSC              7 0
128900930517     C*
129000941102     C* LETTURA FNFVV02L
129100931210     C     KFVV          KLIST
129200931210     C                   KFLD                    COMNPG
129300050510     C                   KFLD                    kdam
129400020917     C                   KFLD                    Kfgs
129500050510     C     KFVV08        KLIST
129600050510     C                   KFLD                    Kfgs
129700050510     C                   KFLD                    COMNPG
129800050510     C                   KFLD                    savdam
129900050510     C     KFVV08b       KLIST
130000050510     C                   KFLD                    Kfgs
130100050510     C                   KFLD                    COMNPG
130200931210     C* LETTURA FLFVA02L
130300931210     C     KFVA2         KLIST
130400931210     C                   KFLD                    COMNPG
130500931210     C                   KFLD                    FVVNFV
130600020917     c                   Kfld                    FvvFgs
130700931210     C*
130800960304     C* LETTURA TABEL00F
130900960304     C     KTAB2         KLIST
131000960304     C                   KFLD                    CODUT
131100960906     C                   KFLD                    COD
131200960906     C     KTAB          KLIST
131300960906     C                   KFLD                    CODUT
131400960906     C                   KFLD                    COD
131500960906     C                   KFLD                    KEY
131600930430     C                   ENDSR
131700950404**  COM  -  COMANDI CL
131800950403OVRDBF FILE(          ) MBR(          )
131900950403ALCOBJ OBJ((           *FILE *SHRUPD           ))
132000950403DLCOBJ OBJ((           *FILE *SHRUPD           ))
132100950404DLTOVR FILE(FNARBC0T  )
