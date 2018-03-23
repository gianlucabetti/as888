000100020422     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H**************************************************************************
000300970819     H*  Nome programma:  FNLR21R
000400970819     H*  Descrizione   :  Caricamento spunte partenza
000500000000     H**************************************************************************
000600000000     FTABEL00F  IF   E           K DISK
000700990510     FFNANM02L  UF   E           K DISK
000800990510     F                                     INFDS(FNANM2)
000900000000     FFNART01L  IF   E           K DISK
001000000000     F                                     RENAME(FNART000:FNART001)
001100000000     FFNART27L  UF   E           K DISK
001200000000     FFNARB01L  UF   E           K DISK
001300020610     FFNBTT37L  UF   E           K DISK
001400020610     FFNBTP11L  UF   E           K DISK
001500020610     FAZORG01L  IF   E           K DISK
001600000000     D*
001700021029     D*L1              S              3  0 DIM(30)                              FIL GESTITE
001800970818     D C7J             S              2  0 DIM(101)                             CODICI PISTOLE
001900970818     D D7J             S              4    DIM(101)                             DS7J DELLE PISTOLE
002000990510     D FNANM2          DS
002100960527     D  ANMNR2               397    400B 0
002200000000     D                 DS
002300000000     D* CHIAVE COMPLETA SPEDIZIONE ARRIVI
002400000000     D  ARTAAS                 1      4  0
002500000000     D  ARTLNP                 5      7  0
002600000000     D  ARTNRS                 8      9  0
002700000000     D  ARTNSP                10     16  0
002800000000     D  ARTSPE                 1     16  0
002900950122     D                 DS                  INZ
003000950122     D  WSAAAS                 1      4  0
003100950122     D  WSALNP                 5      7  0
003200950122     D  WSANRS                 8      9  0
003300950122     D  WSANSP                10     16  0
003400950122     D  WSASPE                 1     16  0
003500950130     D                 DS
003600970508     D  WSPLNP                 1      3  0
003700970508     D  WSPLNA                 4      6  0
003800991231     D  WSPNFV                 7     12  0
003900991231     D  WSPFGS                13     15  0
004000991231     D  SPPFV                  1     15  0
004100000000     D WLBDAT          DS
004200000000     D  G02DAT                 1      8  0
004300000000     D  G02INV                 9     16  0
004400000000     D  G02ERR                17     17
004500000000     D  G02TGI                18     22  0
004600000000     D WGIDAT          DS
004700000000     D  GIODAT                 1      8  0
004800000000     D  GIOINV                 9     16  0
004900000000     D  GIOTGI                17     21  0
005000000000     D KPJBA         E DS
005100000000     D  LEGFIL               483    492
005200000000     D  MBRFIL               493    502
005300000000     D                 DS
005400000000     D* ESTRATTO DELLA DS7J
005500000000     D  $7JTPS                 1      1
005600970818     D  $7JORD                 2      4  0
005700970818     D  $DS7J                  1      4
005800970508     D PARAM           DS
005900970508     D  D21TLA                 1      1
006000991231     D  D21REC                 2     68
006100991231     D  D21ERR                69     69
006200970819     D* CAMPO DI OUTPUT: S= NON CARICARE LA SPUNTA
006300991231     D  D21NOS                70     70
006400970825     D* CAMPO DI OUTPUT: S= LANCIARE PGM DI ABILITAZIONE BOLLE
006500991231     D  D21ABI                71     71
006600991231     D DSSPP         E DS                  EXTNAME(FISPP00R)
006700020610     D*DSLV55        E DS                  EXTNAME(FNLV55DS)
006800970819     D DSLV53        E DS                  EXTNAME(FNLV53DS)
006900970508     D CNCR80        E DS
007000000000     D DS7J          E DS
007100000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007200020610     D OG143         E DS
007300020422      * ds per trul06r
007400021029     d*trul06ds      e ds
007500021029     d* lin                    1     90  0 dim(30)
007600021029
007700021029     d Fel_§Ogas       s                   Like(§Ogas)
007800040212     d sav_dslv53      s                   like(dslv53)
007900000000     C**************************************************************************
008000000000     C*  L'INDICATORE ... QUESTO SCONOSCIUTO.
008100000000     C*
008200950122     C*  02  ON   ESEGUO UNA VOLTA UNA OPERAZIONE
008300000000     C*  07  ON   ULTIMO AGGIORNAMENTO SU FNARB
008400950122     C*  10       MEMRO TRASMI E' TERMINAL ARRIVO DI SPPLNA
008500000000     C*  20       DI COMODO
008600000000     C*  30
008700000000     C*  31       CICLO PRINCIPALE DI LETTURA ARCHIVI
008800000000     C*  32       DI COMODO
008900000000     C*  33
009000960527     C*  34       DI COMODO
009100950122     C*  38       SIMFEL E' TRAMINAL ARRIVO DI SPPLNA
009200021029     C*  39       SPUNTA DELL'AREA (stesso §OGAS)
009300000000     C*  99       DI COMODO, UTILIZZABILE A PIACERE
009400000000     C**************************************************************************
009500000000     C     *ENTRY        PLIST
009600000000     C                   PARM                    KPJBA
009700970508     C                   MOVEL     KPJBU         PARAM
009800970508     C                   MOVEL     D21REC        DSSPP
009900020423
010000021029     c**!!!              exsr      carl1
010100020423
010200000000     C*---------------------------------------------------------------*
010300970508     C     D21TLA        IFEQ      'C'
010400000000     C* AGGIORNO FNARB CON L'ULTIMA SPEDIZIONE LETTA
010500950122     C   02              EXSR      AGGAR2
010600970818     C**
010700020610     C***                MOVEL     'C'           D55TLA
010800020610     C***                CALL      'FNLV55R'
010900020610     C***                PARM                    DSLV55
011000970819     C                   MOVEL     'C'           D53TLA
011100970819     C                   CALL      'FNLV53R'
011200970819     C                   PARM                    DSLV53
011300970508     C**
011400040505     C**   *IN11         IFEQ      *ON
011500040505     C**                 MOVEL(P)  'KSDIFFE'     KQIEB
011600040505     C**                 MOVEL     'LR27'        KCOAZ
011700040505     C**                 CALL      'BCH10'
011800040505     C**                 PARM                    KPJBA
011900040505     C**                 ENDIF
012000040505     C**                 CLEAR                   KQIEB
012100970508     C**
012200970508     C                   SETON                                        LR
012300970508     C**
012400970508     C                   ELSE
012500970509     C                   CLEAR                   D21ERR
012600970509     C** APPOGGIO DATI PARTENZA IN BOLLA
012700970508     C                   EXSR      APPBOL
012800970509     C                   MOVEL     PARAM         KPJBU
012900970509     C**
013000970508     C                   ENDIF
013100000000     C*
013200970509     C                   RETURN
013300000000     C**************************************************************************
013400970508     C*  APPOGGIO SU BOLLE ARRIVI O TRANSITO LE SPUNTE PARTENZA
013500000000     C**************************************************************************
013600000000     C     APPBOL        BEGSR
013700050510     c                   clear                   wttr              1
013800950122     C**
013900950122     C* C A M B I A T A   L A   L I N E A   D I   A R R I V O
014000950122     C**
014100970508    1C     SPPLNA        IFNE      SAVLNA
014200020610     C***                SETOFF                                       1038
014300000000     C*
014400020610     C**                 CLEAR                   DSLV55
014500020610     C**                 MOVEL     ' '           D55TPT
014600020610     C**                 MOVEL     SPPLNA        D55LIN
014700020610     C**                 MOVEL     SPPDFV        D55DRF
014800020610     C**                 CALL      'FNLV55R'
014900020610     C**                 PARM                    DSLV55
015000970818     C**
015100020610     C**   D55ERR        IFEQ      ' '
015200020610     C**                 MOVEL     D55TFP        WTFP
015300020610     C**                 ELSE
015400020610     C**                 MOVEL     SPPLNA        WTFP
015500020610     C**                 MOVEL     SPPLNA        D55TFA
015600020610     C**                 ENDIF
015700000000     C*
015800000000     C* VEDO SE E' UNA MIA LINEA
015900021029     C**!!!SPPLNA        LOOKUP    L1                                     39
016000020610     c* reperisco il network
016100020610     c     spplna        chain     azorg01l
016200020610     c                   if        %found(azorg01l)
016300020610     c                   movel     orgde3        og143
016400020610     c                   else
016500020610     c                   clear                   og143
016600020610     c                   endif
016700021029
016800021029      * Controllo se è stesso AS
016900021029     c     §Ogas         Comp      Fel_§Ogas                              39
017000020610     c
017100950122     C*
017200950122     C* 10 ON -VIENE DA MIO TERMINAL
017300970508     C* DO PER SCONTATO CHE LA FIL GESTIONE SIA SEMPRE UN 1 LIVELLO
017400020610    2C** 39SPPFGS        IFEQ      D55TFA
017500020610     C**                 SETON                                        10
017600020610    2C**                 ENDIF
017700000000     C*
017800000000     C* SIMFEL E' IL TERMINAL DELLA LINEA DI ARRIVO
017900020610    2C***  SIMFEL        IFEQ      D55TFA
018000020610     C***                SETON                                        38
018100020610    2C***                END
018200000000     C*
018300950122     C                   MOVEL     SPPLNA        SAVLNA
018400950122    1C                   ENDIF
018500970818     C**
018600970818     C                   Z-ADD     1             WA
018700970818     C     SPPNPS        LOOKUP    C7J(WA)                                30     ON=TROVATO
018800970818     C   30              MOVEL     D7J(WA)       $DS7J
018900001020     C  N30              CLEAR                   $7JTPS
019000001020     C  N30              CLEAR                   $7JORD
019100970818     C* SALVO I CAMPI
019200970818     C                   MOVEL     $7JORD        SPPORD
019300950122     C**
019400000000     C* B O L L A   D I   S I M F E L
019500950122     C**
019600020610    1C***  *IN39         IFEQ      *ON
019700020610     c* Se spunta con SPPATR = "A" o "T" o "D" si tratta sicuramente di
019800020610     c* una spunta che aggiorna la bolla arrivi
019900020610     c* Se spunta con sppatr = blanks aggiorna la bolla arrivi se sppflp = 0
020000020610     c* mentre aggiorna la bolla transito se sppflp <> 0. In questo caso
020100020610     c* sppflp contine il codice del p.o. di transito da utilizzare per la
020200020610     c* ricerca della bolla transito
020300020610     c     sppatr        ifeq      'A'
020400020610     c     sppatr        oreq      'T'
020500020610     c     sppatr        oreq      'D'
020600020610     c     sppatr        oreq      *blanks
020700020610     c     sppflp        andeq     *zeros
020800000000     C     KSPP          CHAIN     FNART27L                           31
020900970509     C**
021000970509     C*  A --> NON ESISTE BOLLA ARRIVI
021100970509    2C     *IN31         IFEQ      *ON
021200020610     c* Se la spunta è per il ter-arr della lna su altro As la bolla arrivi
021300020610     c* non ci sarà mai e quindi non devo ritornare d21err = "A"
021400021029     c     *in39         ifeq      *on
021500970509     C                   MOVEL     'A'           D21ERR
021600020610     c                   endif
021700970509   X2C                   ELSE
021800040212     c* se spunta di disguido che transita prima di arrivare all'arrivo fina
021900040212     c* le la spunta non deve aggiornare la bolla arrivi
022000040212     c     sppatr        ifeq      'D'
022100040212     c* prima salvo dslv53 per non sporcarla in quanto usata in routine
022200040212     c* abbin. In realtà così come è strutturato il pgm non sarebbe
022300040212     c* necessario salvarla ma per sicurezza la salvo lo stesso
022400040212     c                   move      dslv53        sav_dslv53
022500040212     c                   clear                   wnoagg
022600040212     c*
022700040212     c                   clear                   dslv53
022800040512     c* cerco nei fogli partenza perchè il foglio di fva è ancora nel
022900040512     c* membro di ricezione. Se spunta  di lan che transita, non aggiorno
023000040512     c*  fnart con dut  (se transitq il pgm restituisce errore)
023100040512     C                   MOVEL     'G'           D53TFO
023200040212     C                   Z-ADD     SPPNFV        D53NFV
023300040212     C                   MOVEL     1             D53NPG
023400040212     C                   MOVEL     SPPFGS        D53FGS
023500040212     C                   MOVEL     SPPFGS        D53FLF
023600040212     C                   MOVEL     'A'           D53ABB
023700040212     C                   MOVEL     SPPLNA        D53LNA
023800040407     C*****              MOVEL     ARBTFA        D53TFA
023900040212     c                   call      'FNLV53R'
024000040212     c                   parm                    dslv53
024100040212     c     d53err        ifne      *blanks
024200040212     c                   movel     '1'           Wnoagg            1
024300040212     c                   endif
024400050510     c                   eval      wttr=d53ttr
024500040212     c*
024600040212     c                   move      sav_dslv53    dslv53
024700040212     c                   endif
024800040212     C*
024900040212   3Ac     sppatr        ifne      'D'
025000040212     c     wnoagg        oreq      *blanks
025100050510     c* non aggiorno nemmeno se si tratta di disguido su foglio locale
025200050510     c*  (quindi collo non mio in partenza ma mio in arrivo)
025300050510     c     wttr          andne     'L'
025400950817     C* LA PRIMA VOLTA NON AGGIORNO LA BOLLA MA SOLO A SUCCESSIVO CAMBI
025500950122    3C     *IN02         IFEQ      *OFF
025600950122     C                   MOVEL     ARTSPE        WSASPE
025700950122     C                   SETON                                        02
025800950122    3C                   ENDIF
025900000000     C*
026000950122     C* AGGIORNO ARTVUC = VOLUME DA MACCHINA SMISTACOLLI
026100950122     C                   EXSR      VOLCAN
026200990809     C* AGGIORNO ARTPUC = PESO DA MACCHINA SMISTACOLLI
026300990809     C                   EXSR      PESCAN
026400040212     C* Aggiorno cod anomalia spunta su fnart --> ARTCAN
026500040212     c                   if        sppcan<>' '
026600040212     c                   eval      artcan=sppcan
026700040212     c                   endif
026800950122     C*
026900020610     C* AGGIORNO SOLTANTO SE lna non è estera
027000020610     C***  ARTLNA        IFLT      300
027100020610     c     §ogntw        ifne      'FED'
027200020610     c     §ogntw        andne     'DPD'
027300020610     c     §ogntw        andne     'EEX'
027400020610     c     §ogntw        andne     'EUP'
027500000000     C                   MOVEL     DATEU         ARTDTR                         DATA TRASMISS.
027600961112     C                   ENDIF
027700961112     C**
027800950130     C                   MOVEL     ' '           ARTABN                         FLG ABBINAM
027900950328     C* SE CAMBIATA BOLLA CLEARO LA FILIALE DI TRANSITO SULLA BOLLA
028000950328     C     ARTSPE        IFNE      WSASPE
028100950328     C                   CLEAR                   WFLP
028200950328     C                   ENDIF
028300950328     C***
028400970818     C* S P U N T A   D A L   T R A N S I T O
028500950328     C***
028600000000    3C     SPPATR        IFNE      ' '
028700000000     C     SPPATR        ANDNE     'A'
028800950122     C*
028900950122     C* SALVO LA FILIALE DI TRANSITO DA METTERE SULLA BOLLA
029000020610    4C**N10SPPATR        IFEQ      'T'
029100020610    4C     SPPATR        IFEQ      'T'
029200960527     C                   MOVEL     SPPFGS        WFLP
029300950122    4C                   ENDIF
029400950122     C*
029500970818     C* SE CAMBIA LA FIL DI TRANSITO AGGIORNO SOLO SE LA DATA E' >=
029600970818     C*
029700960527    4C     SPPFGS        IFNE      ARTFLP
029800970818    5C     SPPDFV        IFGE      ARTDUT
029900950122     C*
030000000000     C* AGGIORNO LA FIL.TRANSITO SOLO SE SPUNTA NON DEL MIO TERMINAL
030100020610     C**N10              MOVEL     SPPFGS        ARTFLP
030200020610     C                   MOVEL     SPPFGS        ARTFLP
030300000000     C                   CLEAR                   ARTDET
030400000000     C                   CLEAR                   ARTPET
030500000000     C                   Z-ADD     SPPDFV        ARTDUT
030600000000     C                   MOVEL     SPPNPS        ARTPUT
030700970818    5C                   ENDIF
030800970818   X4C                   ELSE
030900970818     C**
031000970818     C*  SE LA FIL DI TRANSITO NON CAMBIA ALLORA FACCIO LE CONSIDERAZIO
031100970818     C*  SULLA PISTOLA E SULLA DATA PER VEDERE SE SOSTITUIRE O MENO
031200970818     C* SE FILIALE DI TRANSITO >< PULISCO I CAMPI DI ENTRATA AL TRANSIT
031300970818     C**
031400970818    5C     ARTDUT        IFGT      0
031500970818     C                   Z-ADD     1             WA
031600971125     C     ARTPUT        LOOKUP    C7J(WA)                                32
031700971125     C   32              MOVEL     D7J(WA)       $DS7J
031800001020     C  N32              CLEAR                   $7JTPS
031900001020     C  N32              CLEAR                   $7JORD
032000970818    5C                   ENDIF
032100970818     C**
032200970818    5C     SPPDFV        IFGT      ARTDUT
032300970818     C     SPPDFV        OREQ      ARTDUT
032400970818     C     SPPORD        ANDLE     $7JORD
032500970818     C                   Z-ADD     SPPDFV        ARTDUT
032600970818     C                   MOVEL     SPPNPS        ARTPUT
032700970819   X5C                   ELSE
032800970819     C* SE A PARTITA DI DATA NON AGGIORNO LA BOLLA NON CARICO NEMMENO
032900970819     C*  LA SPUNTA
033000970819    6C     SPPDFV        IFEQ      ARTDUT
033100970819     C                   MOVEL     'S'           D21NOS
033200970819    6C                   END
033300970818    5C                   ENDIF
033400970818    4C                   ENDIF
033500000000     C*
033600000000   X3C                   ELSE
033700000000     C*
033800970818     C* S P U N T A   D A L L A   P A R T E N Z A
033900000000     C*
034000950621     C* Se spunta in partenza dal mio terminal di arrivo
034100950621     C* aggiorno la data partenza, pistola,n.foglio,
034200950621     C* solo se data partenza = 0
034300020610    4C***  *IN10         IFEQ      '0'
034400020610     C***  *IN10         OREQ      '1'
034500020610     C***  ARTDFV        ANDEQ     *ZEROS
034600970818    5C     ARTDFV        IFGT      0
034700970818     C                   Z-ADD     1             WA
034800971125     C     ARTNPP        LOOKUP    C7J(WA)                                32
034900971125     C   32              MOVEL     D7J(WA)       $DS7J
035000001020     C  N32              CLEAR                   $7JTPS
035100001020     C  N32              CLEAR                   $7JORD
035200970818    5C                   ENDIF
035300970818     C**
035400970818     C* SE LA DATA C'E' LA AGGIORNO SOLO SE E' MAGGIORE
035500970818     C*  A PARTITA' DI DATA AGGIORNO QUELLA CON ORDINE INFERIORE
035600970818     A*  A PARTITA' DI ORDINE AGGIORNO L'ULTIMA
035700970818    5C     SPPDFV        IFGT      ARTDFV
035800970818     C     SPPDFV        OREQ      ARTDFV
035900970818     C     SPPORD        ANDLE     $7JORD
036000991231     C                   Z-ADD     SPPNFV        ARTNFV
036100000000     C                   Z-ADD     SPPDFV        ARTDFV
036200000000     C                   MOVEL     SPPNPS        ARTNPP
036300950215     C*
036400970818     C* SE SPUNTA "A" METTO LA FILIALE DI TRANSITO
036500970818    6C     SPPATR        IFEQ      'A'
036600950215     C                   MOVEL     SPPFLP        ARTFLP
036700970818    6C                   END
036800970819   X5C                   ELSE
036900970819     C* SE A PARTITA DI DATA NON AGGIORNO LA BOLLA NON CARICO NEMMENO
037000970819     C*  LA SPUNTA
037100970819    6C     SPPDFV        IFEQ      ARTDFV
037200970819     C                   MOVEL     'S'           D21NOS
037300970819    6C                   END
037400970819    5C                   END
037500020610    4C***                END
037600961204    3C                   END
037700950130     C*
037800950130     C* SE LA SPUNTA E' DI FV GIA' ABBINATO, AGGIORNO COME L'ABBINAM
037900970115     C     SPPATR        IFNE      'A'
038000950130     C                   EXSR      ABBIN
038100961204     C                   ENDIF
038200000000     C*
038300000000     C                   UPDATE    FNART000
038400950122     C*
038500950122     C* SE CAMBIATA SPED. AGGIORNO BOLLA
038600950122    3C     ARTSPE        IFNE      WSASPE
038700950122     C                   MOVEL     ARTSPE        COMSPE
038800950122     C                   EXSR      AGGAR2
038900950122     C                   MOVEL     COMSPE        WSASPE
039000950122     C                   MOVEL     WFVC          WSAFVC
039100950122     C                   MOVEL     WFLP          WSAFLP
039200040402     C                   MOVEL     WFPC          WSAFPC
039300950122    3C                   END
039400950122     C*
039500950122     C* WFVC = "S" AGGIORNATO COLLO CON VOLUNE CML
039600950122    3C     WFVC          IFEQ      'S'
039700950122     C                   MOVEL     WFVC          WSAFVC
039800950122    3C                   ENDIF
039900990809     C* WFPC = "S" AGGIORNATO COLLO CON PESO CML
040000990809    3C     WFPC          IFEQ      'S'
040100990809     C                   MOVEL     WFPC          WSAFPC
040200990809    3C                   ENDIF
040300950122     C* FIL.TRANSITO
040400950122    3C     WFLP          IFGT      0
040500950122     C                   MOVEL     WFLP          WSAFLP
040600950122    3C                   ENDIF
040700000000     C*
040800040407   3ac                   else
040900040407     c* non disallocava e dava errore fnlr22r se SPPATR=T
041000040407     c                   unlock    fnart27l
041100040212   3Ac                   endif
041200000000    2C                   END
041300000000     C*
041400960527     C* ANOMALIA: MANCA RECORD BOLLA ARRIVO
041500950817     C*
041600960527     C* NON LA FACCIAMO PIU' CREARE IN BASE ALLE SPUNTE PARTENZA RICEVU
041700960527     C*  TE. L'UNICO PROBLEMA E' CHE NON VENGONO CARICARE LE RELATIVE
041800950817     C*  SPUNTE PARTENZA QUANDO SI RICEVE IL RECORD BOLLA, SE ANCORA
041900950817     C*  NON SONO STATI SPARATI IN ARRIVO I COLLI, MA AL MASSIMO LA
042000950817     C*  SUCCESSIVA RICEZIONE SPUNTE LE FA CARICARE
042100000000     C*
042200000000   X1C                   ELSE
042300000000     C*
042400000000     C* B O L L A   T R A N S I T O
042500020610    2C***  *IN38         IFEQ      '0'
042600020610     C     Kbtt          CHAIN     FNBTT37L                           31
042700970509     C*
042800970509     C* T --> NON TROVATA BOLLA TRANSITO
042900970509    3C     *IN31         IFEQ      *ON
043000970509     C                   MOVEL     'T'           D21ERR
043100970509   X3C                   ELSE
043200970818     C*
043300970818     C* AGGIORNO ARTVUC = VOLUME DA MACCHINA SMISTACOLLI
043400970818     C                   Z-ADD     BTTVUC        WELVUC
043500970818     C                   MOVEL     BTTFVC        WELFVC
043600970818     C                   EXSR      AGGVUC
043700970818     C                   Z-ADD     WELVUC        BTTVUC
043800970818     C                   MOVEL     WELFVC        BTTFVC
043900990809     C* AGGIORNO ARTPUC = PESO DA MACCHINA SMISTACOLLI
044000990809     C                   Z-ADD     BTTPUC        WELPUC
044100990809     C                   MOVEL     BTTFPC        WELFPC
044200990809     C                   EXSR      AGGPUC
044300990809     C                   Z-ADD     WELPUC        BTTPUC
044400990809     C                   MOVEL     WELFPC        BTTFPC
044500950122     C*
044600990209     C**** NON AGGIORNO PIU' BTTCAN
044700990209     C****                 MOVELWCAN      BTTCAN           COD.ANOMALIA
044800970818     C*
044900970818     C                   MOVEL     'R'           BTTFTR                         'R'=RICEZIONE
045000970818     C                   MOVEL     DATEU         BTTDTR                         DATA RICEZIONE
045100970818     C**
045200000110    4C     BTTDFV        IFGT      0
045300970818     C                   Z-ADD     1             WA
045400971125     C     BTTNPP        LOOKUP    C7J(WA)                                32
045500971125     C   32              MOVEL     D7J(WA)       $DS7J
045600001020     C  N32              CLEAR                   $7JTPS
045700001020     C  N32              CLEAR                   $7JORD
045800970818    4C                   ENDIF
045900970818     C**
046000970818     C* SE LA DATA C'E' LA AGGIORNO SOLO SE E' MAGGIORE
046100970818     C*  A PARTITA' DI DATA AGGIORNO QUELLA CON ORDINE INFERIORE
046200970818     A*  A PARTITA' DI ORDINE AGGIORNO L'ULTIMA
046300970818    4C     SPPDFV        IFGT      BTTDFV
046400970818     C     SPPDFV        OREQ      BTTDFV
046500970818     C     SPPORD        ANDLE     $7JORD
046600970818     C                   Z-ADD     SPPDFV        BTTDFV
046700991231     C                   Z-ADD     SPPNFV        BTTNFV
046800970818     C                   MOVEL     SPPNPS        BTTNPP
046900950122     C**
047000020610     C     KBTP          CHAIN     FNBTP11L                           31
047100970509     C**
047200970818    5C     *IN31         IFEQ      *ON
047300970509     C                   MOVEL     'T'           D21ERR
047400970818   X5C                   ELSE
047500000000     C*
047600000000     C* IMPOSTO LA DATA PARTENZA PRIMO COLLO (SE NON C'E' ANCORA)
047700970818    6C     BTPDPC        IFGT      SPPDFV
047800000000     C     BTPDPC        OREQ      *ZERO
047900000000     C                   MOVEL     SPPDFV        BTPDPC
048000970818    6C                   END
048100950122     C* VISTO CHE NON C'E' CALCOLO DEI TEMPI DI CONSEGNA
048200950122     C*  SULLE BOLLE TRANSITO  DUC LO IMPOSTO MAN MANO
048300970818    6C     SPPDFV        IFGT      BTPDUC
048400950122     C                   MOVEL     SPPDFV        BTPDUC
048500970818    6C                   END
048600950122     C*
048700000000     C                   EXCEPT    UPDBTP
048800970818    5C                   ENDIF
048900970819   X4C                   ELSE
049000970819     C* SE A PARTITA DI DATA NON AGGIORNO LA BOLLA NON CARICO NEMMENO
049100970819     C*  LA SPUNTA
049200970819    5C     SPPDFV        IFEQ      BTTDFV
049300970819     C                   MOVEL     'S'           D21NOS
049400970819    5C                   END
049500970818    4C                   ENDIF
049600000000     C*
049700000000     C                   UPDATE    FNBTT000
049800970509    3C                   ENDIF
049900950817     C*
050000020610    2C***                END
050100950122    1C                   END
050200000000     C*
050300000000     C* SE C'E' ANOMALIA 50 ARRIVATO NON PARTITO --> LA CHIUDO
050400960527     C*  NON POSSO RICHIAMARE IL PGM FNLR33R PERCHE' DEVE FARE IL
050500960527     C*  TEST ANCHE SULLA DATA APERTURA ANOMALIA
050600000000     C                   Z-ADD     50            COMCAA
050700990510     C     KANM          CHAIN     FNANM000                           99
050800960415     C     *IN99         IFEQ      *OFF
050900960415     C     ANMFT1        ANDEQ     *BLANKS
051000960415     C     ANMFT2        ANDEQ     *BLANKS
051100990510     C     SPPDFV        ANDEQ     ANMDAO
051200990510     C                   DELETE    FNANM000
051300960415     C                   ELSE
051400990510     C  N99              UNLOCK    FNANM02L
051500960415     C                   END
051600000000     C*
051700000000     C                   ENDSR
051800000000     C**************************************************************************
051900000000     C*  AGGIORNO VOLUME DA MACCHINA SMISTACOLLI
052000000000     C**************************************************************************
052100000000     C     AGGVUC        BEGSR
052200000000     C*
052300950122     C                   MOVEL     *BLANKS       WFVC              1            DI COMODO
052400000000     C*
052500000000    1C     SPPVUC        IFNE      0
052600000000     C* VERIFICO LA PISTOLA PER SAPERE SE CML O NO
052700970424     C*
052800970424     C* DETERMINO CHE FLAG UTILIZZERO' SE DOVRO' AGGIORNARE FVC
052900970829    2C*****      SPPTPS    IFEQ 'C'
053000970424    3C     SPPFLG        IFEQ      *BLANKS
053100970424    4C     SPPATR        IFNE      ' '
053200970424     C     SPPATR        ANDNE     'A'
053300970424     C                   MOVEL     'T'           WWFVC             1            CML DA TRANS
053400970424   X4C                   ELSE
053500970424     C                   MOVEL     'P'           WWFVC                          CML DA PART.
053600970424    4C                   END
053700970424   X3C                   ELSE
053800970424     C                   MOVEL     'T'           WWFVC                          CML DA TRANS
053900970424    3C                   END
054000970829   X2C*****                ELSE
054100970829     C*****                MOVEL'M'       WWFVC             'M'=MANUALE
054200970829    2C*****                END
054300000000     C*
054400000000     C* SE ARTVUC = 0   AGGIORNO IL CAMPO CON SPPVUC
054500950122    2C     WELVUC        IFEQ      0
054600950122     C                   Z-ADD     SPPVUC        WELVUC                          VOLUME
054700950122     C                   MOVEL     'S'           WFVC                            RIEMPITO ARTVUC
054800970424     C                   MOVEL     WWFVC         WELFVC
054900000000     C*
055000000000   X2C                   ELSE
055100970423     C* SE ARTVUC <> 0 AGGIORNO IL CAMPO CON SPPVUC SOLO NEI SEGUENTI
055200970423     C* CASI:
055300970423     C* - IL VOLUME ERA STATO INSERITO MANUALMENTE
055400970423     C* - IL VOLUME ERA QUELLO RILEVATO IN ARRIVO
055500030604     c*   solo se non è lna FedEx
055600970424     C* - IL VOLUME ERA STATO RILEVATO DAL TRANSITO E STO RICEVENDO
055700970424     C*   IL VOLUME DALLA PARTENZA
055800970424     C*
055900970423    3C     WELFVC        IFEQ      'M'
056000970423     C     WELFVC        OREQ      'A'
056100970424     C     WWFVC         ANDNE     'M'
056200030604     c     §ogntw        andne     'FED'
056300970423     C     WELFVC        OREQ      'T'
056400970424     C     WWFVC         ANDEQ     'P'
056500950122     C                   Z-ADD     SPPVUC        WELVUC
056600950122     C                   MOVEL     'S'           WFVC                            RIEMPITO ARTVUC
056700000000     C*
056800970424     C                   MOVEL     WWFVC         WELFVC
056900000000    3C                   ENDIF
057000000000     C*
057100000000    2C                   ENDIF
057200000000    1C                   ENDIF
057300000000     C*
057400000000     C                   ENDSR
057500990809     C**************************************************************************
057600990809     C*  AGGIORNO PESO DA MACCHINA SMISTACOLLI
057700990809     C**************************************************************************
057800990809     C     AGGPUC        BEGSR
057900990809     C*
058000990809     C                   MOVEL     *BLANKS       WFPC              1            DI COMODO
058100990809     C*
058200990809    1C     SPPPUC        IFNE      0
058300990809     C* VERIFICO LA PISTOLA PER SAPERE SE CML O NO
058400990809     C*
058500990809     C* DETERMINO CHE FLAG UTILIZZERO' SE DOVRO' AGGIORNARE FVC
058600990809    3C     SPPFLG        IFEQ      *BLANKS
058700990809    4C     SPPATR        IFNE      ' '
058800990809     C     SPPATR        ANDNE     'A'
058900990809     C                   MOVEL     'T'           WWFPC             1            CML DA TRANS
059000990809   X4C                   ELSE
059100990809     C                   MOVEL     'P'           WWFPC                          CML DA PART.
059200990809    4C                   END
059300990809   X3C                   ELSE
059400990809     C                   MOVEL     'T'           WWFPC                          CML DA TRANS
059500990809    3C                   END
059600990809     C*
059700990809     C* SE ARTPUC = 0   AGGIORNO IL CAMPO CON SPPPUC
059800990809    2C     WELPUC        IFEQ      0
059900990809     C                   Z-ADD     SPPPUC        WELPUC                          PESO
060000990809     C                   MOVEL     'S'           WFPC                            RIEMPITO ARTPUC
060100990809     C                   MOVEL     WWFPC         WELFPC
060200990809     C*
060300990809   X2C                   ELSE
060400990809     C* SE ARTPUC <> 0 AGGIORNO IL CAMPO CON SPPPUC SOLO NEI SEGUENTI
060500990809     C* CASI:
060600990809     C* - IL PESO ERA STATO INSERITO MANUALMENTE
060700990809     C* - IL PESO ERA QUELLO RILEVATO IN ARRIVO
060800990809     C* - IL PESO ERA STATO RILEVATO DAL TRANSITO E STO RICEVENDO
060900990809     C*   IL PESO DALLA PARTENZA
061000990809     C*
061100990809    3C     WELFPC        IFEQ      'M'
061200990809     C     WELFPC        OREQ      'A'
061300990809     C     WWFPC         ANDNE     'M'
061400990809     C     WELFPC        OREQ      'T'
061500990809     C     WWFPC         ANDEQ     'P'
061600990809     C                   Z-ADD     SPPPUC        WELPUC
061700990809     C                   MOVEL     'S'           WFPC                            RIEMPITO ARTPUC
061800990809     C*
061900990809     C                   MOVEL     WWFPC         WELFPC
062000990809    3C                   ENDIF
062100990809     C*
062200990809    2C                   ENDIF
062300990809    1C                   ENDIF
062400990809     C*
062500990809     C                   ENDSR
062600950122     C**************************************************************************
062700950122     C*   AGGIORNO FNARB DA SPUNTE DALLA PARTENZA
062800950122     C**************************************************************************
062900950122     C     AGGAR2        BEGSR
063000950122     C*
063100950122     C                   Z-ADD     *ZERO         DUT
063200950122     C                   Z-ADD     *HIVAL        DPC
063300950122     C                   Z-ADD     *ZERO         DUC
063400950122     C                   CLEAR                   WFAN
063500950122     C*
063600950122     C* LEGGO FNART PER PRENDERE LA DATA ENTRATA E LA DATA USCITA
063700950122     C*   DAL TRANSITO PIU' VECCHIE
063800950122     C     KSAV2         SETLL     FNART01L
063900950122     C     KSAV2         READE     FNART01L                               34
064000950122     C*
064100950122     C     *IN34         DOWEQ     '0'
064200950122     C*
064300950122     C     ARTDFV        IFGT      0
064400950122     C* DATA PARTENZA 1 COLLO
064500950122     C     ARTDFV        IFLT      DPC
064600950122     C                   MOVEL     ARTDFV        DPC
064700950122     C                   END
064800950122     C* DATA PARTENZA ULTIMO COLLO
064900950122     C     ARTDFV        IFGT      DUC
065000950122     C                   MOVEL     ARTDFV        DUC
065100950122     C                   END
065200950122     C*
065300950122     C                   ELSE
065400950122     C                   Z-ADD     *HIVAL        DUC
065500950122     C                   END
065600950122     C*
065700950122     C* DATA USCITA DAL TRANSITO
065800950122     C     ARTDUT        IFGT      0
065900950122     C     ARTDUT        ANDGT     DUT
066000950122     C                   MOVEL     ARTDUT        DUT
066100950122     C                   END
066200950122     C* CODICE ANOMALIA
066300950122     C     ARTCAN        IFNE      *BLANKS
066400950122     C                   MOVEL     'S'           WFAN
066500950122     C                   ENDIF
066600950122     C*
066700950122     C     KSAV2         READE     FNART01L                               34
066800950122     C                   END
066900950122     C*
067000950122     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
067100950122     C*   TRANSITO E FILIALE TRANSITO
067200950122     C     KSAV2         CHAIN     FNARB01L                           30
067300950122     C*
067400950122     C     *IN30         IFEQ      '0'
067500950122     C* DATA PARTENZA ULTIMO COLLO
067600950122     C     DUC           IFNE      *HIVAL
067700950122     C                   MOVEL     DUC           ARBDUC
067800950122     C                   ELSE
067900950122     C                   MOVEL     *ZEROS        ARBDUC
068000950122     C                   END
068100950122     C* DATA PARTENZA PRIMO COLLO
068200950122     C     DPC           IFNE      *HIVAL
068300950122     C                   MOVEL     DPC           ARBDPC
068400950122     C                   ELSE
068500950122     C                   MOVEL     *ZEROS        ARBDPC
068600950122     C                   END
068700950122     C* DATA USCITA DAL TRANSITO
068800950122     C                   Z-ADD     DUT           ARBDUT
068900950122     C*
069000990913     C* SE HO AGGIORNATO ARTVUC METTO 99999 IN "NR.COLLI CML"
069100990913     C     WSAFVC        IFEQ      'S'                                          'S'=SI ARTVUC
069200990913     C     ARBFBS        IFEQ      ' '
069300990913     C                   MOVEL     'V'           ARBFBS
069400990913     C                   ELSE
069500990913     C     ARBFBS        IFEQ      'S'
069600990913     C     ARBFBS        OREQ      'P'
069700990913     C                   MOVEL     'E'           ARBFBS
069800990913     C                   ENDIF
069900990913     C                   ENDIF
070000040505     C**                 SETON                                        11        LANCIA ABILITAZIONE
070100990913     C                   ENDIF
070200990809     C* SE HO AGGIORNATO ARTPUC METTO 99999 IN "NR.COLLI CML"
070300990809     C     WSAFPC        IFEQ      'S'                                          'S'=SI ARTPUC
070400970924     C     ARBFBS        IFEQ      ' '
070500990809     C                   MOVEL     'P'           ARBFBS
070600970924     C                   ELSE
070700970924     C     ARBFBS        IFEQ      'S'
070800990913     C     ARBFBS        OREQ      'V'
070900970924     C                   MOVEL     'E'           ARBFBS
071000970924     C                   ENDIF
071100970924     C                   ENDIF
071200040505     C***                SETON                                        11        LANCIA ABILITAZIONE
071300950122     C                   ENDIF
071400950122     C*
071500950122     C* SE TROVATO COD.ANOMALIA SUI COLLI IMPOSTO IL FLAG SULLA BOLLA
071600950122     C     WFAN          IFEQ      'S'
071700950122     C                   MOVEL     'S'           ARBFAN
071800950122     C                   ENDIF
071900950122     C* AGGIORNO FILIALE TRANSITO
072000950122     C                   Z-ADD     WSAFLP        ARBFLP
072100950328     C* AZZERO SEMPRE LA DATA ARRIVO ULTIMO COLLO CHE E' DA RICALCOLARE
072200950328     C* SE PER CASO CI FOSSE GIA' PERCHE' HO RICEVUTO UNA NUOVA SPUNTA
072300950328     C*  PARTENZA E QUINDI SE CE NE ERA UNA PRECEDENTE E' FASULLA
072400950328     C                   CLEAR                   ARBDTI
072500950328     C                   CLEAR                   ARBHTI
072600950122     C*
072700950122     C                   UPDATE    FNARB000
072800950122     C                   END
072900000000     C*
073000000000     C                   ENDSR
073100950122     C**************************************************************************
073200950122     C* AGGIORNO VOLUME E CODICE ANOMALIA PER BOLLA IN ARB
073300950122     C**************************************************************************
073400950122     C     VOLCAN        BEGSR
073500950122     C                   Z-ADD     ARTVUC        WELVUC                         VOLUME
073600950122     C                   MOVEL     ARTFVC        WELFVC
073700950122     C                   EXSR      AGGVUC
073800950122     C                   Z-ADD     WELVUC        ARTVUC
073900950122     C                   MOVEL     WELFVC        ARTFVC
074000950122     C*
074100950122     C                   ENDSR
074200990809     C**************************************************************************
074300990809     C* AGGIORNO PESO E CODICE ANOMALIA PER BOLLA IN ARB
074400990809     C**************************************************************************
074500990809     C     PESCAN        BEGSR
074600990809     C                   Z-ADD     ARTPUC        WELPUC                         PESO
074700990809     C                   MOVEL     ARTFPC        WELFPC
074800990809     C                   EXSR      AGGPUC
074900990809     C                   Z-ADD     WELPUC        ARTPUC
075000990809     C                   MOVEL     WELFPC        ARTFPC
075100990809     C*
075200990809     C                   ENDSR
075300000000     C**************************************************************************
075400000000     C* R O U T I N E    I N I Z I A L E
075500000000     C**************************************************************************
075600000000     C     *INZSR        BEGSR
075700000000     C*
075800000000     C     KSPP          KLIST
075900000000     C                   KFLD                    SPPLNP
076000000000     C                   KFLD                    SPPLNA
076100000000     C                   KFLD                    SPPNRS
076200000000     C                   KFLD                    SPPNSC
076300020610     C     Kbtt          KLIST
076400020610     C                   KFLD                    SPPFLP
076500020610     C                   KFLD                    SPPLNP
076600020610     C                   KFLD                    SPPLNA
076700020610     C                   KFLD                    SPPNRS
076800020610     C                   KFLD                    SPPNSC
076900000000     C     KART          KLIST
077000000000     C                   KFLD                    ARTAAS
077100000000     C                   KFLD                    ARTLNP
077200000000     C                   KFLD                    ARTNRS
077300000000     C                   KFLD                    ARTNSP
077400950122     C     KSAV2         KLIST
077500950122     C                   KFLD                    WSAAAS
077600950122     C                   KFLD                    WSALNP
077700950122     C                   KFLD                    WSANRS
077800950122     C                   KFLD                    WSANSP
077900000000     C     KBTP          KLIST
078000020610     c                   KFLD                    BTTFLP
078100000000     C                   KFLD                    BTTAAS
078200000000     C                   KFLD                    BTTLNP
078300000000     C                   KFLD                    BTTNRS
078400000000     C                   KFLD                    BTTNSP
078500000000     C     KTAB          KLIST
078600000000     C                   KFLD                    CODUT
078700000000     C                   KFLD                    COD
078800000000     C                   KFLD                    KEY
078900000000     C     KTAB2         KLIST
079000000000     C                   KFLD                    CODUT
079100000000     C                   KFLD                    COD
079200000000     C     KANM          KLIST                                                  *
079300000000     C                   KFLD                    SPPLNP                         *
079400000000     C                   KFLD                    SPPLNA                         *
079500000000     C                   KFLD                    SPPNRS                         *
079600000000     C                   KFLD                    SPPNSC
079700000000     C                   KFLD                    COMCAA
079800000000     C*
079900970818     C     *LIKE         DEFINE    §7JORD        SPPORD
080000020610     C**   *LIKE         DEFINE    ARBTFP        WTFP
080100950130     C     *LIKE         DEFINE    SPPLNA        SAVLNA
080200950122     C     *LIKE         DEFINE    ARBDET        DET
080300950122     C     *LIKE         DEFINE    ARBDUT        DUT
080400950122     C     *LIKE         DEFINE    TBLCOD        COD
080500000000     C     *LIKE         DEFINE    TBLKEY        KEY
080600000000     C     *LIKE         DEFINE    ANMCAA        COMCAA                         CAUSALE ANOMALIA
080700970508     C     *LIKE         DEFINE    WSASPE        COMSPE
080800950122     C     *LIKE         DEFINE    ARTVUC        WELVUC
080900950122     C     *LIKE         DEFINE    ARTFVC        WELFVC
081000950122     C     *LIKE         DEFINE    WFVC          WSAFVC
081100990809     C     *LIKE         DEFINE    ARTPUC        WELPUC
081200990809     C     *LIKE         DEFINE    ARTFPC        WELFPC
081300990809     C     *LIKE         DEFINE    WFPC          WSAFPC
081400960527     C     *LIKE         DEFINE    ARBFLP        WSAFLP
081500960527     C     *LIKE         DEFINE    ARBFLP        WFLP
081600960527     C     *LIKE         DEFINE    ARBDPC        DPC
081700960527     C     *LIKE         DEFINE    ARBDUC        DUC
081800960527     C     *LIKE         DEFINE    ARBFAN        WFAN
081900960527     C     *LIKE         DEFINE    SPPFV         SAVFV
082000970508     C*
082100970508     C                   Z-ADD     0             WSASPE
082200970508     C                   CLEAR                   WSAFVC                         VOL.CML SU FNART
082300970508     C                   CLEAR                   SAVLNA
082400970509     C* DATI UTENTE
082500970509     C                   Z-ADD     1             CODUT
082600970509     C                   CALL      'X§PARUT'
082700970509     C                   PARM                    UT§DSE
082800970509     C                   MOVEL     REC80         CNCR80
082900970508     C*
083000970508     C* GIRO DATA DEL GIORNO
083100970508     C                   TIME                    WTIME            14 0          ORA & DATA
083200970508     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
083300970508     C                   Z-ADD     WDATE         G02DAT
083400970508     C                   MOVEL     *BLANK        G02ERR
083500970508     C                   CALL      'XSRDA8'
083600970508     C                   PARM                    WLBDAT
083700970508     C                   Z-ADD     G02INV        DATEU             8 0          AAAA/MM/GG
083800970508     C*
083900970508     C* CARICO TUTTA LA TABELLA 7J -->  TIPI PISTOLA
084000970508     C                   Z-ADD     *ZERO         WA                3 0          CONTATORE
084100970508     C                   MOVEL     '7J'          COD
084200970508     C     KTAB2         SETLL     TABEL00F
084300970508     C     KTAB2         READE     TABEL00F                               30
084400970508     C     *IN30         DOWEQ     *OFF
084500970508     C     TBLFLG        IFEQ      *BLANKS
084600970508     C                   ADD       1             WA
084700970508     C                   MOVEL     TBLUNI        DS7J
084800970508     C                   MOVEL     §7JTPS        $7JTPS
084900970818     C                   MOVEL     §7JORD        $7JORD
085000970508     C                   MOVEL     TBLKEY        C7J(WA)
085100970508     C                   MOVEL     $DS7J         D7J(WA)
085200970508     C                   ENDIF
085300970508     C     KTAB2         READE     TABEL00F                               30
085400970508     C                   ENDDO
085500021029
085600021029      * Ricerco OGAS di Simfel
085700021029     c     Simfel        Chain     Azorg01l
085800021029     c                   If        %Found(Azorg01l)
085900021029     c                   Movel     OrgDe3        Og143
086000021029     c                   Else
086100021029     c                   Clear                   Og143
086200021029     c                   EndIf
086300021029     c                   Eval      Fel_§Ogas = §Ogas
086400000000     C*
086500000000     C                   ENDSR
086600950130     C**************************************************************************
086700950130     C* SE FV ABBINATO, AGGIORNO COME L'ABBINAMENTO
086800950130     C**************************************************************************
086900950130     C     ABBIN         BEGSR
087000970508     C                   MOVEL     SPPLNP        WSPLNP
087100970508     C                   MOVEL     SPPLNA        WSPLNA
087200991231     C                   Z-ADD     SPPNFV        WSPNFV
087300970508     C                   MOVEL     SPPFGS        WSPFGS
087400950130     C* SE CAMBIATO FOGLIO O LNP O LNA SPUNTA --> RICONTROLLO FOGLIO
087500950130    1C     SPPFV         IFNE      SAVFV
087600970819     C                   CLEAR                   DSLV53
087700970819     C                   MOVEL     'A'           D53TFO
087800970819     C                   MOVEL     SIMFEL        D53FEL
087900991231     C                   Z-ADD     SPPNFV        D53NFV
088000970819     C                   MOVEL     SPPFGS        D53FGS
088100970819     C                   MOVEL     SPPFGS        D53FLF
088200970819     C                   MOVEL     1             D53NPG
088300970819     C                   MOVEL     'A'           D53ABB
088400970819     C                   MOVEL     SPPLNA        D53LNA
088500970819     C     KART          CHAIN(N)  FNARB01L                           31
088600970819     C  N31              MOVEL     ARBTFA        D53TFA
088700970819     C                   CALL      'FNLV53R'
088800970819     C                   PARM                    DSLV53
088900970819     C*
089000970819     C                   MOVEL     SPPFV         SAVFV
089100970819    1C                   ENDIF
089200950130     C*
089300950130     C* SOLO SE  A B B I N A T O  GIA' METTO FLAG TOCCO SPUNTA
089400970819    1C     D53ERR        IFEQ      ' '
089500970819     C     D53NFA        ANDGT     0
089600961204     C     KART          CHAIN     FNARB01L                           31
089700950130     C*
089800970924     C     ARBFBS        IFEQ      ' '
089900970924     C                   MOVEL     'S'           ARBFBS                         SOLO ABILIT
090000970924     C                   ELSE
090100970924     C     ARBFBS        IFEQ      'V'
090200990809     C     ARBFBS        OREQ      'P'
090300970924     C                   MOVEL     'E'           ARBFBS                         ENTRAMBI
090400970924     C                   ENDIF
090500970924     C                   ENDIF
090600970924     C**
090700950130     C                   MOVEL     'S'           ARTABN
090800950130     C                   SETOFF                                       12
090900971218     C*****                MOVEL'S'       D21ABI
091000040505     C***                SETON                                        11
091100950130     C*
091200950130     C* SE IL FOGLIO VIENE DAL TERMINAL AGGIORNO ANCHE LA DATA
091300950328     C*  DI ARRIVO CON LA DATA DI ARRIVO EFFETTIVA DEL MEZZO
091400950130     C*  12 ON - MESSA DATA DI ARRIVO
091500970825    2C***        D53DEF    IFEQ 'S'
091600970825     C***        *IN10     ANDEQ*ON
091700970825     C***
091800970825    3C***        ARTDAM    IFEQ 0
091900970825     C***        ARTDAM    ORGT 0
092000970825     C***        ARTNAP    ANDEQ80
092100970825     C***        ARTDAM    ANDLTD53AED
092200970825     C***                  Z-ADDD53AED    ARTDAM
092300970825     C***                  MOVEL80        ARTNAP
092400970825     C***                  SETON                     12
092500950328     C* SE 1 COLLO SOLO RISCRIVO ANCHE ARBDAM
092600970825    4C***        ARBNCL    IFEQ 1
092700970825     C***        ARBXCO    OREQ 'X'
092800970825     C***                  CLEARARBDAM
092900970825    4C***                  END
093000970825    3C***                  END
093100950130     C*
093200950130     C* SE E' SPUNTA DA TERMINAL E HO AGGIORNATO IL DETTAGLIO COLLI
093300950130     C*   AGGIORNO ANCHE LA BOLLA
093400970825    3C***        *IN12     IFEQ *ON
093500970825    4C***        ARBDAM    IFEQ 0
093600970825     C***                  MOVELD53AED    ARBDAM
093700970825    4C***                  END
093800970825     C***                  SETON                     11
093900970825    3C***                  END
094000950328     C*
094100970825    2C***                  END
094200950130     C*
094300950130     C* COMUNQUE UPDATO LA BOLLA PER SVINCOLARE IL RECORD
094400961204     C  N31              UPDATE    FNARB000
094500950130    1C                   END
094600950130     C*
094700950130     C                   ENDSR
094800000000     C**************************************************************************
094900000000     OFNBTP000  E            UPDBTP
095000000000     O                       BTPDPC
095100950122     O                       BTPDUC
