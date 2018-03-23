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
019300160512     C                   MOVEL     $7JTPS        SPPTPS
019400950122     C**
019500000000     C* B O L L A   D I   S I M F E L
019600950122     C**
019700020610    1C***  *IN39         IFEQ      *ON
019800020610     c* Se spunta con SPPATR = "A" o "T" o "D" si tratta sicuramente di
019900020610     c* una spunta che aggiorna la bolla arrivi
020000020610     c* Se spunta con sppatr = blanks aggiorna la bolla arrivi se sppflp = 0
020100020610     c* mentre aggiorna la bolla transito se sppflp <> 0. In questo caso
020200020610     c* sppflp contine il codice del p.o. di transito da utilizzare per la
020300020610     c* ricerca della bolla transito
020400020610     c     sppatr        ifeq      'A'
020500020610     c     sppatr        oreq      'T'
020600020610     c     sppatr        oreq      'D'
020700020610     c     sppatr        oreq      *blanks
020800020610     c     sppflp        andeq     *zeros
020900000000     C     KSPP          CHAIN     FNART27L                           31
021000970509     C**
021100970509     C*  A --> NON ESISTE BOLLA ARRIVI
021200970509    2C     *IN31         IFEQ      *ON
021300020610     c* Se la spunta è per il ter-arr della lna su altro As la bolla arrivi
021400020610     c* non ci sarà mai e quindi non devo ritornare d21err = "A"
021500021029     c     *in39         ifeq      *on
021600970509     C                   MOVEL     'A'           D21ERR
021700020610     c                   endif
021800970509   X2C                   ELSE
021900040212     c* se spunta di disguido che transita prima di arrivare all'arrivo fina
022000040212     c* le la spunta non deve aggiornare la bolla arrivi
022100040212     c     sppatr        ifeq      'D'
022200040212     c* prima salvo dslv53 per non sporcarla in quanto usata in routine
022300040212     c* abbin. In realtà così come è strutturato il pgm non sarebbe
022400040212     c* necessario salvarla ma per sicurezza la salvo lo stesso
022500040212     c                   move      dslv53        sav_dslv53
022600040212     c                   clear                   wnoagg
022700040212     c*
022800040212     c                   clear                   dslv53
022900040512     c* cerco nei fogli partenza perchè il foglio di fva è ancora nel
023000040512     c* membro di ricezione. Se spunta  di lan che transita, non aggiorno
023100040512     c*  fnart con dut  (se transitq il pgm restituisce errore)
023200040512     C                   MOVEL     'G'           D53TFO
023300040212     C                   Z-ADD     SPPNFV        D53NFV
023400040212     C                   MOVEL     1             D53NPG
023500040212     C                   MOVEL     SPPFGS        D53FGS
023600040212     C                   MOVEL     SPPFGS        D53FLF
023700040212     C                   MOVEL     'A'           D53ABB
023800040212     C                   MOVEL     SPPLNA        D53LNA
023900040407     C*****              MOVEL     ARBTFA        D53TFA
024000040212     c                   call      'FNLV53R'
024100040212     c                   parm                    dslv53
024200040212     c     d53err        ifne      *blanks
024300040212     c                   movel     '1'           Wnoagg            1
024400040212     c                   endif
024500050510     c                   eval      wttr=d53ttr
024600040212     c*
024700040212     c                   move      sav_dslv53    dslv53
024800040212     c                   endif
024900040212     C*
025000040212   3Ac     sppatr        ifne      'D'
025100040212     c     wnoagg        oreq      *blanks
025200050510     c* non aggiorno nemmeno se si tratta di disguido su foglio locale
025300050510     c*  (quindi collo non mio in partenza ma mio in arrivo)
025400050510     c     wttr          andne     'L'
025500950817     C* LA PRIMA VOLTA NON AGGIORNO LA BOLLA MA SOLO A SUCCESSIVO CAMBI
025600950122    3C     *IN02         IFEQ      *OFF
025700950122     C                   MOVEL     ARTSPE        WSASPE
025800950122     C                   SETON                                        02
025900950122    3C                   ENDIF
026000000000     C*
026100950122     C* AGGIORNO ARTVUC = VOLUME DA MACCHINA SMISTACOLLI
026200950122     C                   EXSR      VOLCAN
026300990809     C* AGGIORNO ARTPUC = PESO DA MACCHINA SMISTACOLLI
026400990809     C                   EXSR      PESCAN
026500040212     C* Aggiorno cod anomalia spunta su fnart --> ARTCAN
026600040212     c                   if        sppcan<>' '
026700040212     c                   eval      artcan=sppcan
026800040212     c                   endif
026900950122     C*
027000020610     C* AGGIORNO SOLTANTO SE lna non è estera
027100020610     C***  ARTLNA        IFLT      300
027200020610     c     §ogntw        ifne      'FED'
027300020610     c     §ogntw        andne     'DPD'
027400020610     c     §ogntw        andne     'EEX'
027500020610     c     §ogntw        andne     'EUP'
027600000000     C                   MOVEL     DATEU         ARTDTR                         DATA TRASMISS.
027700961112     C                   ENDIF
027800961112     C**
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
030700111223     C                   MOVEL     ' '           ARTABN                         FLG ABBINAM
030800970818    5C                   ENDIF
030900970818   X4C                   ELSE
031000970818     C**
031100970818     C*  SE LA FIL DI TRANSITO NON CAMBIA ALLORA FACCIO LE CONSIDERAZIO
031200970818     C*  SULLA PISTOLA E SULLA DATA PER VEDERE SE SOSTITUIRE O MENO
031300970818     C* SE FILIALE DI TRANSITO >< PULISCO I CAMPI DI ENTRATA AL TRANSIT
031400970818     C**
031500970818    5C     ARTDUT        IFGT      0
031600970818     C                   Z-ADD     1             WA
031700971125     C     ARTPUT        LOOKUP    C7J(WA)                                32
031800971125     C   32              MOVEL     D7J(WA)       $DS7J
031900001020     C  N32              CLEAR                   $7JTPS
032000001020     C  N32              CLEAR                   $7JORD
032100970818    5C                   ENDIF
032200970818     C**
032300970818    5C     SPPDFV        IFGT      ARTDUT
032400970818     C     SPPDFV        OREQ      ARTDUT
032500970818     C     SPPORD        ANDLE     $7JORD
032600970818     C                   Z-ADD     SPPDFV        ARTDUT
032700970818     C                   MOVEL     SPPNPS        ARTPUT
032800111223     C                   MOVEL     ' '           ARTABN                         FLG ABBINAM
032900970819   X5C                   ELSE
033000970819     C* SE A PARTITA DI DATA NON AGGIORNO LA BOLLA NON CARICO NEMMENO
033100970819     C*  LA SPUNTA
033200970819    6C     SPPDFV        IFEQ      ARTDUT
033300970819     C                   MOVEL     'S'           D21NOS
033400970819    6C                   END
033500970818    5C                   ENDIF
033600970818    4C                   ENDIF
033700000000     C*
033800000000   X3C                   ELSE
033900000000     C*
034000970818     C* S P U N T A   D A L L A   P A R T E N Z A
034100000000     C*
034200950621     C* Se spunta in partenza dal mio terminal di arrivo
034300950621     C* aggiorno la data partenza, pistola,n.foglio,
034400950621     C* solo se data partenza = 0
034500020610    4C***  *IN10         IFEQ      '0'
034600020610     C***  *IN10         OREQ      '1'
034700020610     C***  ARTDFV        ANDEQ     *ZEROS
034800970818    5C     ARTDFV        IFGT      0
034900970818     C                   Z-ADD     1             WA
035000971125     C     ARTNPP        LOOKUP    C7J(WA)                                32
035100971125     C   32              MOVEL     D7J(WA)       $DS7J
035200001020     C  N32              CLEAR                   $7JTPS
035300001020     C  N32              CLEAR                   $7JORD
035400970818    5C                   ENDIF
035500970818     C**
035600970818     C* SE LA DATA C'E' LA AGGIORNO SOLO SE E' MAGGIORE
035700970818     C*  A PARTITA' DI DATA AGGIORNO QUELLA CON ORDINE INFERIORE
035800970818     A*  A PARTITA' DI ORDINE AGGIORNO L'ULTIMA
035900970818    5C     SPPDFV        IFGT      ARTDFV
036000970818     C     SPPDFV        OREQ      ARTDFV
036100970818     C     SPPORD        ANDLE     $7JORD
036200991231     C                   Z-ADD     SPPNFV        ARTNFV
036300000000     C                   Z-ADD     SPPDFV        ARTDFV
036400000000     C                   MOVEL     SPPNPS        ARTNPP
036500111223     C                   MOVEL     ' '           ARTABN                         FLG ABBINAM
036600950215     C*
036700970818     C* SE SPUNTA "A" METTO LA FILIALE DI TRANSITO
036800970818    6C     SPPATR        IFEQ      'A'
036900950215     C                   MOVEL     SPPFLP        ARTFLP
037000970818    6C                   END
037100970819   X5C                   ELSE
037200970819     C* SE A PARTITA DI DATA NON AGGIORNO LA BOLLA NON CARICO NEMMENO
037300970819     C*  LA SPUNTA
037400970819    6C     SPPDFV        IFEQ      ARTDFV
037500970819     C                   MOVEL     'S'           D21NOS
037600970819    6C                   END
037700970819    5C                   END
037800020610    4C***                END
037900961204    3C                   END
038000950130     C*
038100950130     C* SE LA SPUNTA E' DI FV GIA' ABBINATO, AGGIORNO COME L'ABBINAM
038200970115     C     SPPATR        IFNE      'A'
038300950130     C                   EXSR      ABBIN
038400961204     C                   ENDIF
038500000000     C*
038600000000     C                   UPDATE    FNART000
038700950122     C*
038800950122     C* SE CAMBIATA SPED. AGGIORNO BOLLA
038900950122    3C     ARTSPE        IFNE      WSASPE
039000950122     C                   MOVEL     ARTSPE        COMSPE
039100950122     C                   EXSR      AGGAR2
039200950122     C                   MOVEL     COMSPE        WSASPE
039300950122     C                   MOVEL     WFVC          WSAFVC
039400950122     C                   MOVEL     WFLP          WSAFLP
039500040402     C                   MOVEL     WFPC          WSAFPC
039600950122    3C                   END
039700160512     c* Mi salvo il campo ZONA se non è autogenerata
039800160512     c                   if        spptps='L' or spptps='C'
039900160512     c                             or  sppnps=96 or sppnps=98
040000160512     c* devo considerare anche le 98 e 96 perchè figlie di spunte entrata reali
040100160512     c                   movel     sppznc        wsaznc            2
040200160512     c                   endif
040300950122     C*
040400950122     C* WFVC = "S" AGGIORNATO COLLO CON VOLUNE CML
040500950122    3C     WFVC          IFEQ      'S'
040600950122     C                   MOVEL     WFVC          WSAFVC
040700950122    3C                   ENDIF
040800990809     C* WFPC = "S" AGGIORNATO COLLO CON PESO CML
040900990809    3C     WFPC          IFEQ      'S'
041000990809     C                   MOVEL     WFPC          WSAFPC
041100990809    3C                   ENDIF
041200950122     C* FIL.TRANSITO
041300950122    3C     WFLP          IFGT      0
041400950122     C                   MOVEL     WFLP          WSAFLP
041500950122    3C                   ENDIF
041600000000     C*
041700040407   3ac                   else
041800040407     c* non disallocava e dava errore fnlr22r se SPPATR=T
041900040407     c                   unlock    fnart27l
042000040212   3Ac                   endif
042100000000    2C                   END
042200000000     C*
042300960527     C* ANOMALIA: MANCA RECORD BOLLA ARRIVO
042400950817     C*
042500960527     C* NON LA FACCIAMO PIU' CREARE IN BASE ALLE SPUNTE PARTENZA RICEVU
042600960527     C*  TE. L'UNICO PROBLEMA E' CHE NON VENGONO CARICARE LE RELATIVE
042700950817     C*  SPUNTE PARTENZA QUANDO SI RICEVE IL RECORD BOLLA, SE ANCORA
042800950817     C*  NON SONO STATI SPARATI IN ARRIVO I COLLI, MA AL MASSIMO LA
042900950817     C*  SUCCESSIVA RICEZIONE SPUNTE LE FA CARICARE
043000000000     C*
043100000000   X1C                   ELSE
043200000000     C*
043300000000     C* B O L L A   T R A N S I T O
043400020610    2C***  *IN38         IFEQ      '0'
043500020610     C     Kbtt          CHAIN     FNBTT37L                           31
043600970509     C*
043700970509     C* T --> NON TROVATA BOLLA TRANSITO
043800970509    3C     *IN31         IFEQ      *ON
043900970509     C                   MOVEL     'T'           D21ERR
044000970509   X3C                   ELSE
044100970818     C*
044200970818     C* AGGIORNO ARTVUC = VOLUME DA MACCHINA SMISTACOLLI
044300970818     C                   Z-ADD     BTTVUC        WELVUC
044400970818     C                   MOVEL     BTTFVC        WELFVC
044500970818     C                   EXSR      AGGVUC
044600970818     C                   Z-ADD     WELVUC        BTTVUC
044700970818     C                   MOVEL     WELFVC        BTTFVC
044800990809     C* AGGIORNO ARTPUC = PESO DA MACCHINA SMISTACOLLI
044900990809     C                   Z-ADD     BTTPUC        WELPUC
045000990809     C                   MOVEL     BTTFPC        WELFPC
045100990809     C                   EXSR      AGGPUC
045200990809     C                   Z-ADD     WELPUC        BTTPUC
045300990809     C                   MOVEL     WELFPC        BTTFPC
045400950122     C*
045500990209     C**** NON AGGIORNO PIU' BTTCAN
045600990209     C****                 MOVELWCAN      BTTCAN           COD.ANOMALIA
045700970818     C*
045800970818     C                   MOVEL     'R'           BTTFTR                         'R'=RICEZIONE
045900970818     C                   MOVEL     DATEU         BTTDTR                         DATA RICEZIONE
046000970818     C**
046100000110    4C     BTTDFV        IFGT      0
046200970818     C                   Z-ADD     1             WA
046300971125     C     BTTNPP        LOOKUP    C7J(WA)                                32
046400971125     C   32              MOVEL     D7J(WA)       $DS7J
046500001020     C  N32              CLEAR                   $7JTPS
046600001020     C  N32              CLEAR                   $7JORD
046700970818    4C                   ENDIF
046800970818     C**
046900970818     C* SE LA DATA C'E' LA AGGIORNO SOLO SE E' MAGGIORE
047000970818     C*  A PARTITA' DI DATA AGGIORNO QUELLA CON ORDINE INFERIORE
047100970818     A*  A PARTITA' DI ORDINE AGGIORNO L'ULTIMA
047200970818    4C     SPPDFV        IFGT      BTTDFV
047300970818     C     SPPDFV        OREQ      BTTDFV
047400970818     C     SPPORD        ANDLE     $7JORD
047500970818     C                   Z-ADD     SPPDFV        BTTDFV
047600991231     C                   Z-ADD     SPPNFV        BTTNFV
047700970818     C                   MOVEL     SPPNPS        BTTNPP
047800950122     C**
047900020610     C     KBTP          CHAIN     FNBTP11L                           31
048000970509     C**
048100970818    5C     *IN31         IFEQ      *ON
048200970509     C                   MOVEL     'T'           D21ERR
048300970818   X5C                   ELSE
048400000000     C*
048500000000     C* IMPOSTO LA DATA PARTENZA PRIMO COLLO (SE NON C'E' ANCORA)
048600970818    6C     BTPDPC        IFGT      SPPDFV
048700000000     C     BTPDPC        OREQ      *ZERO
048800000000     C                   MOVEL     SPPDFV        BTPDPC
048900970818    6C                   END
049000950122     C* VISTO CHE NON C'E' CALCOLO DEI TEMPI DI CONSEGNA
049100950122     C*  SULLE BOLLE TRANSITO  DUC LO IMPOSTO MAN MANO
049200970818    6C     SPPDFV        IFGT      BTPDUC
049300950122     C                   MOVEL     SPPDFV        BTPDUC
049400970818    6C                   END
049500950122     C*
049600000000     C                   EXCEPT    UPDBTP
049700970818    5C                   ENDIF
049800970819   X4C                   ELSE
049900970819     C* SE A PARTITA DI DATA NON AGGIORNO LA BOLLA NON CARICO NEMMENO
050000970819     C*  LA SPUNTA
050100970819    5C     SPPDFV        IFEQ      BTTDFV
050200970819     C                   MOVEL     'S'           D21NOS
050300970819    5C                   END
050400970818    4C                   ENDIF
050500000000     C*
050600000000     C                   UPDATE    FNBTT000
050700970509    3C                   ENDIF
050800950817     C*
050900020610    2C***                END
051000950122    1C                   END
051100000000     C*
051200000000     C* SE C'E' ANOMALIA 50 ARRIVATO NON PARTITO --> LA CHIUDO
051300960527     C*  NON POSSO RICHIAMARE IL PGM FNLR33R PERCHE' DEVE FARE IL
051400960527     C*  TEST ANCHE SULLA DATA APERTURA ANOMALIA
051500000000     C                   Z-ADD     50            COMCAA
051600990510     C     KANM          CHAIN     FNANM000                           99
051700960415     C     *IN99         IFEQ      *OFF
051800960415     C     ANMFT1        ANDEQ     *BLANKS
051900960415     C     ANMFT2        ANDEQ     *BLANKS
052000990510     C     SPPDFV        ANDEQ     ANMDAO
052100990510     C                   DELETE    FNANM000
052200960415     C                   ELSE
052300990510     C  N99              UNLOCK    FNANM02L
052400960415     C                   END
052500000000     C*
052600000000     C                   ENDSR
052700000000     C**************************************************************************
052800000000     C*  AGGIORNO VOLUME DA MACCHINA SMISTACOLLI
052900000000     C**************************************************************************
053000000000     C     AGGVUC        BEGSR
053100000000     C*
053200950122     C                   MOVEL     *BLANKS       WFVC              1            DI COMODO
053300000000     C*
053400000000    1C     SPPVUC        IFNE      0
053500000000     C* VERIFICO LA PISTOLA PER SAPERE SE CML O NO
053600970424     C*
053700970424     C* DETERMINO CHE FLAG UTILIZZERO' SE DOVRO' AGGIORNARE FVC
053800970829    2C*****      SPPTPS    IFEQ 'C'
053900970424    3C     SPPFLG        IFEQ      *BLANKS
054000970424    4C     SPPATR        IFNE      ' '
054100970424     C     SPPATR        ANDNE     'A'
054200970424     C                   MOVEL     'T'           WWFVC             1            CML DA TRANS
054300970424   X4C                   ELSE
054400970424     C                   MOVEL     'P'           WWFVC                          CML DA PART.
054500970424    4C                   END
054600970424   X3C                   ELSE
054700970424     C                   MOVEL     'T'           WWFVC                          CML DA TRANS
054800970424    3C                   END
054900970829   X2C*****                ELSE
055000970829     C*****                MOVEL'M'       WWFVC             'M'=MANUALE
055100970829    2C*****                END
055200000000     C*
055300000000     C* SE ARTVUC = 0   AGGIORNO IL CAMPO CON SPPVUC
055400950122    2C     WELVUC        IFEQ      0
055500950122     C                   Z-ADD     SPPVUC        WELVUC                          VOLUME
055600950122     C                   MOVEL     'S'           WFVC                            RIEMPITO ARTVUC
055700970424     C                   MOVEL     WWFVC         WELFVC
055800000000     C*
055900000000   X2C                   ELSE
056000970423     C* SE ARTVUC <> 0 AGGIORNO IL CAMPO CON SPPVUC SOLO NEI SEGUENTI
056100970423     C* CASI:
056200970423     C* - IL VOLUME ERA STATO INSERITO MANUALMENTE
056300970423     C* - IL VOLUME ERA QUELLO RILEVATO IN ARRIVO
056400030604     c*   solo se non è lna FedEx
056500970424     C* - IL VOLUME ERA STATO RILEVATO DAL TRANSITO E STO RICEVENDO
056600970424     C*   IL VOLUME DALLA PARTENZA
056700970424     C*
056800970423    3C     WELFVC        IFEQ      'M'
056900970423     C     WELFVC        OREQ      'A'
057000970424     C     WWFVC         ANDNE     'M'
057100030604     c     §ogntw        andne     'FED'
057200970423     C     WELFVC        OREQ      'T'
057300970424     C     WWFVC         ANDEQ     'P'
057400950122     C                   Z-ADD     SPPVUC        WELVUC
057500950122     C                   MOVEL     'S'           WFVC                            RIEMPITO ARTVUC
057600000000     C*
057700970424     C                   MOVEL     WWFVC         WELFVC
057800000000    3C                   ENDIF
057900000000     C*
058000000000    2C                   ENDIF
058100000000    1C                   ENDIF
058200000000     C*
058300000000     C                   ENDSR
058400990809     C**************************************************************************
058500990809     C*  AGGIORNO PESO DA MACCHINA SMISTACOLLI
058600990809     C**************************************************************************
058700990809     C     AGGPUC        BEGSR
058800990809     C*
058900990809     C                   MOVEL     *BLANKS       WFPC              1            DI COMODO
059000990809     C*
059100990809    1C     SPPPUC        IFNE      0
059200990809     C* VERIFICO LA PISTOLA PER SAPERE SE CML O NO
059300990809     C*
059400990809     C* DETERMINO CHE FLAG UTILIZZERO' SE DOVRO' AGGIORNARE FVC
059500990809    3C     SPPFLG        IFEQ      *BLANKS
059600990809    4C     SPPATR        IFNE      ' '
059700990809     C     SPPATR        ANDNE     'A'
059800990809     C                   MOVEL     'T'           WWFPC             1            CML DA TRANS
059900990809   X4C                   ELSE
060000990809     C                   MOVEL     'P'           WWFPC                          CML DA PART.
060100990809    4C                   END
060200990809   X3C                   ELSE
060300990809     C                   MOVEL     'T'           WWFPC                          CML DA TRANS
060400990809    3C                   END
060500990809     C*
060600990809     C* SE ARTPUC = 0   AGGIORNO IL CAMPO CON SPPPUC
060700990809    2C     WELPUC        IFEQ      0
060800990809     C                   Z-ADD     SPPPUC        WELPUC                          PESO
060900990809     C                   MOVEL     'S'           WFPC                            RIEMPITO ARTPUC
061000990809     C                   MOVEL     WWFPC         WELFPC
061100990809     C*
061200990809   X2C                   ELSE
061300990809     C* SE ARTPUC <> 0 AGGIORNO IL CAMPO CON SPPPUC SOLO NEI SEGUENTI
061400990809     C* CASI:
061500990809     C* - IL PESO ERA STATO INSERITO MANUALMENTE
061600990809     C* - IL PESO ERA QUELLO RILEVATO IN ARRIVO
061700990809     C* - IL PESO ERA STATO RILEVATO DAL TRANSITO E STO RICEVENDO
061800990809     C*   IL PESO DALLA PARTENZA
061900990809     C*
062000990809    3C     WELFPC        IFEQ      'M'
062100990809     C     WELFPC        OREQ      'A'
062200990809     C     WWFPC         ANDNE     'M'
062300990809     C     WELFPC        OREQ      'T'
062400990809     C     WWFPC         ANDEQ     'P'
062500990809     C                   Z-ADD     SPPPUC        WELPUC
062600990809     C                   MOVEL     'S'           WFPC                            RIEMPITO ARTPUC
062700990809     C*
062800990809     C                   MOVEL     WWFPC         WELFPC
062900990809    3C                   ENDIF
063000990809     C*
063100990809    2C                   ENDIF
063200990809    1C                   ENDIF
063300990809     C*
063400990809     C                   ENDSR
063500950122     C**************************************************************************
063600950122     C*   AGGIORNO FNARB DA SPUNTE DALLA PARTENZA
063700950122     C**************************************************************************
063800950122     C     AGGAR2        BEGSR
063900950122     C*
064000950122     C                   Z-ADD     *ZERO         DUT
064100950122     C                   Z-ADD     *HIVAL        DPC
064200950122     C                   Z-ADD     *ZERO         DUC
064300950122     C                   CLEAR                   WFAN
064400950122     C*
064500950122     C* LEGGO FNART PER PRENDERE LA DATA ENTRATA E LA DATA USCITA
064600950122     C*   DAL TRANSITO PIU' VECCHIE
064700950122     C     KSAV2         SETLL     FNART01L
064800950122     C     KSAV2         READE     FNART01L                               34
064900950122     C*
065000950122     C     *IN34         DOWEQ     '0'
065100950122     C*
065200950122     C     ARTDFV        IFGT      0
065300950122     C* DATA PARTENZA 1 COLLO
065400950122     C     ARTDFV        IFLT      DPC
065500950122     C                   MOVEL     ARTDFV        DPC
065600950122     C                   END
065700950122     C* DATA PARTENZA ULTIMO COLLO
065800950122     C     ARTDFV        IFGT      DUC
065900950122     C                   MOVEL     ARTDFV        DUC
066000950122     C                   END
066100950122     C*
066200950122     C                   ELSE
066300950122     C                   Z-ADD     *HIVAL        DUC
066400950122     C                   END
066500950122     C*
066600950122     C* DATA USCITA DAL TRANSITO
066700950122     C     ARTDUT        IFGT      0
066800950122     C     ARTDUT        ANDGT     DUT
066900950122     C                   MOVEL     ARTDUT        DUT
067000950122     C                   END
067100950122     C* CODICE ANOMALIA
067200950122     C     ARTCAN        IFNE      *BLANKS
067300950122     C                   MOVEL     'S'           WFAN
067400950122     C                   ENDIF
067500950122     C*
067600950122     C     KSAV2         READE     FNART01L                               34
067700950122     C                   END
067800950122     C*
067900950122     C* CHAIN BOLLA E AGGIORNO DATA ENTRATA TRANSITO, DATA USCITA
068000950122     C*   TRANSITO E FILIALE TRANSITO
068100950122     C     KSAV2         CHAIN     FNARB01L                           30
068200950122     C*
068300950122     C     *IN30         IFEQ      '0'
068400160512     c* se zona diversa la aggiorno
068500160512     c                   if        wsaznc<>'  ' and wsaznc<>%editc(arbznc:'X')
068600160512     c                   movel     wsaznc        arbznc
068700160512     c                   endif
068800160512     c                   clear                   wsaznc
068900160512     c
069000950122     C* DATA PARTENZA ULTIMO COLLO
069100950122     C     DUC           IFNE      *HIVAL
069200950122     C                   MOVEL     DUC           ARBDUC
069300950122     C                   ELSE
069400950122     C                   MOVEL     *ZEROS        ARBDUC
069500950122     C                   END
069600950122     C* DATA PARTENZA PRIMO COLLO
069700950122     C     DPC           IFNE      *HIVAL
069800950122     C                   MOVEL     DPC           ARBDPC
069900950122     C                   ELSE
070000950122     C                   MOVEL     *ZEROS        ARBDPC
070100950122     C                   END
070200950122     C* DATA USCITA DAL TRANSITO
070300950122     C                   Z-ADD     DUT           ARBDUT
070400950122     C*
070500990913     C* SE HO AGGIORNATO ARTVUC METTO 99999 IN "NR.COLLI CML"
070600990913     C     WSAFVC        IFEQ      'S'                                          'S'=SI ARTVUC
070700990913     C     ARBFBS        IFEQ      ' '
070800990913     C                   MOVEL     'V'           ARBFBS
070900990913     C                   ELSE
071000990913     C     ARBFBS        IFEQ      'S'
071100990913     C     ARBFBS        OREQ      'P'
071200990913     C                   MOVEL     'E'           ARBFBS
071300990913     C                   ENDIF
071400990913     C                   ENDIF
071500040505     C**                 SETON                                        11        LANCIA ABILITAZIONE
071600990913     C                   ENDIF
071700990809     C* SE HO AGGIORNATO ARTPUC METTO 99999 IN "NR.COLLI CML"
071800990809     C     WSAFPC        IFEQ      'S'                                          'S'=SI ARTPUC
071900970924     C     ARBFBS        IFEQ      ' '
072000990809     C                   MOVEL     'P'           ARBFBS
072100970924     C                   ELSE
072200970924     C     ARBFBS        IFEQ      'S'
072300990913     C     ARBFBS        OREQ      'V'
072400970924     C                   MOVEL     'E'           ARBFBS
072500970924     C                   ENDIF
072600970924     C                   ENDIF
072700040505     C***                SETON                                        11        LANCIA ABILITAZIONE
072800950122     C                   ENDIF
072900950122     C*
073000950122     C* SE TROVATO COD.ANOMALIA SUI COLLI IMPOSTO IL FLAG SULLA BOLLA
073100950122     C     WFAN          IFEQ      'S'
073200950122     C                   MOVEL     'S'           ARBFAN
073300950122     C                   ENDIF
073400950122     C* AGGIORNO FILIALE TRANSITO
073500950122     C                   Z-ADD     WSAFLP        ARBFLP
073600950328     C* AZZERO SEMPRE LA DATA ARRIVO ULTIMO COLLO CHE E' DA RICALCOLARE
073700950328     C* SE PER CASO CI FOSSE GIA' PERCHE' HO RICEVUTO UNA NUOVA SPUNTA
073800950328     C*  PARTENZA E QUINDI SE CE NE ERA UNA PRECEDENTE E' FASULLA
073900950328     C                   CLEAR                   ARBDTI
074000950328     C                   CLEAR                   ARBHTI
074100950122     C*
074200950122     C                   UPDATE    FNARB000
074300950122     C                   END
074400000000     C*
074500000000     C                   ENDSR
074600950122     C**************************************************************************
074700950122     C* AGGIORNO VOLUME E CODICE ANOMALIA PER BOLLA IN ARB
074800950122     C**************************************************************************
074900950122     C     VOLCAN        BEGSR
075000950122     C                   Z-ADD     ARTVUC        WELVUC                         VOLUME
075100950122     C                   MOVEL     ARTFVC        WELFVC
075200950122     C                   EXSR      AGGVUC
075300950122     C                   Z-ADD     WELVUC        ARTVUC
075400950122     C                   MOVEL     WELFVC        ARTFVC
075500950122     C*
075600950122     C                   ENDSR
075700990809     C**************************************************************************
075800990809     C* AGGIORNO PESO E CODICE ANOMALIA PER BOLLA IN ARB
075900990809     C**************************************************************************
076000990809     C     PESCAN        BEGSR
076100990809     C                   Z-ADD     ARTPUC        WELPUC                         PESO
076200990809     C                   MOVEL     ARTFPC        WELFPC
076300990809     C                   EXSR      AGGPUC
076400990809     C                   Z-ADD     WELPUC        ARTPUC
076500990809     C                   MOVEL     WELFPC        ARTFPC
076600990809     C*
076700990809     C                   ENDSR
076800000000     C**************************************************************************
076900000000     C* R O U T I N E    I N I Z I A L E
077000000000     C**************************************************************************
077100000000     C     *INZSR        BEGSR
077200000000     C*
077300000000     C     KSPP          KLIST
077400000000     C                   KFLD                    SPPLNP
077500000000     C                   KFLD                    SPPLNA
077600000000     C                   KFLD                    SPPNRS
077700000000     C                   KFLD                    SPPNSC
077800020610     C     Kbtt          KLIST
077900020610     C                   KFLD                    SPPFLP
078000020610     C                   KFLD                    SPPLNP
078100020610     C                   KFLD                    SPPLNA
078200020610     C                   KFLD                    SPPNRS
078300020610     C                   KFLD                    SPPNSC
078400000000     C     KART          KLIST
078500000000     C                   KFLD                    ARTAAS
078600000000     C                   KFLD                    ARTLNP
078700000000     C                   KFLD                    ARTNRS
078800000000     C                   KFLD                    ARTNSP
078900950122     C     KSAV2         KLIST
079000950122     C                   KFLD                    WSAAAS
079100950122     C                   KFLD                    WSALNP
079200950122     C                   KFLD                    WSANRS
079300950122     C                   KFLD                    WSANSP
079400000000     C     KBTP          KLIST
079500020610     c                   KFLD                    BTTFLP
079600000000     C                   KFLD                    BTTAAS
079700000000     C                   KFLD                    BTTLNP
079800000000     C                   KFLD                    BTTNRS
079900000000     C                   KFLD                    BTTNSP
080000000000     C     KTAB          KLIST
080100000000     C                   KFLD                    CODUT
080200000000     C                   KFLD                    COD
080300000000     C                   KFLD                    KEY
080400000000     C     KTAB2         KLIST
080500000000     C                   KFLD                    CODUT
080600000000     C                   KFLD                    COD
080700000000     C     KANM          KLIST                                                  *
080800000000     C                   KFLD                    SPPLNP                         *
080900000000     C                   KFLD                    SPPLNA                         *
081000000000     C                   KFLD                    SPPNRS                         *
081100000000     C                   KFLD                    SPPNSC
081200000000     C                   KFLD                    COMCAA
081300000000     C*
081400970818     C     *LIKE         DEFINE    §7JORD        SPPORD
081500160512     C     *LIKE         DEFINE    §7Jtps        SPPtps
081600020610     C**   *LIKE         DEFINE    ARBTFP        WTFP
081700950130     C     *LIKE         DEFINE    SPPLNA        SAVLNA
081800950122     C     *LIKE         DEFINE    ARBDET        DET
081900950122     C     *LIKE         DEFINE    ARBDUT        DUT
082000950122     C     *LIKE         DEFINE    TBLCOD        COD
082100000000     C     *LIKE         DEFINE    TBLKEY        KEY
082200000000     C     *LIKE         DEFINE    ANMCAA        COMCAA                         CAUSALE ANOMALIA
082300970508     C     *LIKE         DEFINE    WSASPE        COMSPE
082400950122     C     *LIKE         DEFINE    ARTVUC        WELVUC
082500950122     C     *LIKE         DEFINE    ARTFVC        WELFVC
082600950122     C     *LIKE         DEFINE    WFVC          WSAFVC
082700990809     C     *LIKE         DEFINE    ARTPUC        WELPUC
082800990809     C     *LIKE         DEFINE    ARTFPC        WELFPC
082900990809     C     *LIKE         DEFINE    WFPC          WSAFPC
083000960527     C     *LIKE         DEFINE    ARBFLP        WSAFLP
083100960527     C     *LIKE         DEFINE    ARBFLP        WFLP
083200960527     C     *LIKE         DEFINE    ARBDPC        DPC
083300960527     C     *LIKE         DEFINE    ARBDUC        DUC
083400960527     C     *LIKE         DEFINE    ARBFAN        WFAN
083500960527     C     *LIKE         DEFINE    SPPFV         SAVFV
083600970508     C*
083700970508     C                   Z-ADD     0             WSASPE
083800970508     C                   CLEAR                   WSAFVC                         VOL.CML SU FNART
083900970508     C                   CLEAR                   SAVLNA
084000970509     C* DATI UTENTE
084100970509     C                   Z-ADD     1             CODUT
084200970509     C                   CALL      'X§PARUT'
084300970509     C                   PARM                    UT§DSE
084400970509     C                   MOVEL     REC80         CNCR80
084500970508     C*
084600970508     C* GIRO DATA DEL GIORNO
084700970508     C                   TIME                    WTIME            14 0          ORA & DATA
084800970508     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
084900970508     C                   Z-ADD     WDATE         G02DAT
085000970508     C                   MOVEL     *BLANK        G02ERR
085100970508     C                   CALL      'XSRDA8'
085200970508     C                   PARM                    WLBDAT
085300970508     C                   Z-ADD     G02INV        DATEU             8 0          AAAA/MM/GG
085400970508     C*
085500970508     C* CARICO TUTTA LA TABELLA 7J -->  TIPI PISTOLA
085600970508     C                   Z-ADD     *ZERO         WA                3 0          CONTATORE
085700970508     C                   MOVEL     '7J'          COD
085800970508     C     KTAB2         SETLL     TABEL00F
085900970508     C     KTAB2         READE     TABEL00F                               30
086000970508     C     *IN30         DOWEQ     *OFF
086100970508     C     TBLFLG        IFEQ      *BLANKS
086200970508     C                   ADD       1             WA
086300970508     C                   MOVEL     TBLUNI        DS7J
086400970508     C                   MOVEL     §7JTPS        $7JTPS
086500970818     C                   MOVEL     §7JORD        $7JORD
086600970508     C                   MOVEL     TBLKEY        C7J(WA)
086700970508     C                   MOVEL     $DS7J         D7J(WA)
086800970508     C                   ENDIF
086900970508     C     KTAB2         READE     TABEL00F                               30
087000970508     C                   ENDDO
087100021029
087200021029      * Ricerco OGAS di Simfel
087300021029     c     Simfel        Chain     Azorg01l
087400021029     c                   If        %Found(Azorg01l)
087500021029     c                   Movel     OrgDe3        Og143
087600021029     c                   Else
087700021029     c                   Clear                   Og143
087800021029     c                   EndIf
087900021029     c                   Eval      Fel_§Ogas = §Ogas
088000000000     C*
088100000000     C                   ENDSR
088200950130     C**************************************************************************
088300950130     C* SE FV ABBINATO, AGGIORNO COME L'ABBINAMENTO
088400950130     C**************************************************************************
088500950130     C     ABBIN         BEGSR
088600970508     C                   MOVEL     SPPLNP        WSPLNP
088700970508     C                   MOVEL     SPPLNA        WSPLNA
088800991231     C                   Z-ADD     SPPNFV        WSPNFV
088900970508     C                   MOVEL     SPPFGS        WSPFGS
089000950130     C* SE CAMBIATO FOGLIO O LNP O LNA SPUNTA --> RICONTROLLO FOGLIO
089100950130    1C     SPPFV         IFNE      SAVFV
089200970819     C                   CLEAR                   DSLV53
089300970819     C                   MOVEL     'A'           D53TFO
089400970819     C                   MOVEL     SIMFEL        D53FEL
089500991231     C                   Z-ADD     SPPNFV        D53NFV
089600970819     C                   MOVEL     SPPFGS        D53FGS
089700970819     C                   MOVEL     SPPFGS        D53FLF
089800970819     C                   MOVEL     1             D53NPG
089900970819     C                   MOVEL     'A'           D53ABB
090000970819     C                   MOVEL     SPPLNA        D53LNA
090100970819     C     KART          CHAIN(N)  FNARB01L                           31
090200970819     C  N31              MOVEL     ARBTFA        D53TFA
090300970819     C                   CALL      'FNLV53R'
090400970819     C                   PARM                    DSLV53
090500970819     C*
090600970819     C                   MOVEL     SPPFV         SAVFV
090700970819    1C                   ENDIF
090800950130     C*
090900950130     C* SOLO SE  A B B I N A T O  GIA' METTO FLAG TOCCO SPUNTA
091000970819    1C     D53ERR        IFEQ      ' '
091100970819     C     D53NFA        ANDGT     0
091200961204     C     KART          CHAIN     FNARB01L                           31
091300950130     C*
091400970924     C     ARBFBS        IFEQ      ' '
091500970924     C                   MOVEL     'S'           ARBFBS                         SOLO ABILIT
091600970924     C                   ELSE
091700970924     C     ARBFBS        IFEQ      'V'
091800990809     C     ARBFBS        OREQ      'P'
091900970924     C                   MOVEL     'E'           ARBFBS                         ENTRAMBI
092000970924     C                   ENDIF
092100970924     C                   ENDIF
092200970924     C**
092300950130     C                   MOVEL     'S'           ARTABN
092400950130     C                   SETOFF                                       12
092500971218     C*****                MOVEL'S'       D21ABI
092600040505     C***                SETON                                        11
092700950130     C*
092800950130     C* SE IL FOGLIO VIENE DAL TERMINAL AGGIORNO ANCHE LA DATA
092900950328     C*  DI ARRIVO CON LA DATA DI ARRIVO EFFETTIVA DEL MEZZO
093000950130     C*  12 ON - MESSA DATA DI ARRIVO
093100970825    2C***        D53DEF    IFEQ 'S'
093200970825     C***        *IN10     ANDEQ*ON
093300970825     C***
093400970825    3C***        ARTDAM    IFEQ 0
093500970825     C***        ARTDAM    ORGT 0
093600970825     C***        ARTNAP    ANDEQ80
093700970825     C***        ARTDAM    ANDLTD53AED
093800970825     C***                  Z-ADDD53AED    ARTDAM
093900970825     C***                  MOVEL80        ARTNAP
094000970825     C***                  SETON                     12
094100950328     C* SE 1 COLLO SOLO RISCRIVO ANCHE ARBDAM
094200970825    4C***        ARBNCL    IFEQ 1
094300970825     C***        ARBXCO    OREQ 'X'
094400970825     C***                  CLEARARBDAM
094500970825    4C***                  END
094600970825    3C***                  END
094700950130     C*
094800950130     C* SE E' SPUNTA DA TERMINAL E HO AGGIORNATO IL DETTAGLIO COLLI
094900950130     C*   AGGIORNO ANCHE LA BOLLA
095000970825    3C***        *IN12     IFEQ *ON
095100970825    4C***        ARBDAM    IFEQ 0
095200970825     C***                  MOVELD53AED    ARBDAM
095300970825    4C***                  END
095400970825     C***                  SETON                     11
095500970825    3C***                  END
095600950328     C*
095700970825    2C***                  END
095800950130     C*
095900950130     C* COMUNQUE UPDATO LA BOLLA PER SVINCOLARE IL RECORD
096000961204     C  N31              UPDATE    FNARB000
096100950130    1C                   END
096200950130     C*
096300950130     C                   ENDSR
096400000000     C**************************************************************************
096500000000     OFNBTP000  E            UPDBTP
096600000000     O                       BTPDPC
096700950122     O                       BTPDUC
