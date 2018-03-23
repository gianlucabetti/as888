000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200941018     H* FNLG17R *----------------------------------------------------*
000300900418     H*                                                              *
000400900503     H*             - STAMPA COMUNICAZIONE DI FINE GIACENCA -        *
000500900418     H*--------------------------------------------------------------*
000600941018     FFNBLP01L  IF   E           K DISK
000700060213     FFiar401L  IF   E           K DISK
000800990929     FFIAR601L  IF   E           K DISK
000900050216     Ftignp01L  IF   E           K DISK
001000050321     Ftigcp07L  UF   E           K DISK    commit
001100900418     FTABEL00F  IF   E           K DISK
001200900503     FAZORG01L  IF   E           K DISK
001300900503     FCNACO00F  IF   E           K DISK
001400900503     FCNIND00F  IF   E           K DISK
001500050216     Ftigap01L  IF   E           K DISK
001600950227     FFNCLS01L  IF   E           K DISK
001700990730     FTITGC01L  IF   E           K DISK
001800041028     FLETGIC    O    F  132        PRINTER usropn
001900941019     F*--------------------------------------------------------------*
002000911125     D LT              S             70    DIM(21) CTDATA PERRCD(1)
002100011217     D CNZ             S              3    DIM(500)
002200011217     D ITA             S              1    DIM(500)
002300960213     D C2H             S              1    DIM(100)
002400960213     D D2H             S             69    DIM(100)
002500911112     D FIL£7           S              3    DIM(29)
002600041028     E*  QCAEXEC
002700041028     D DLT1            S             49    DIM(1) CTDATA PERRCD(1)
002800941019     D*--------------------------------------------------------------*
002900950227     D DS15          E DS
003000950321     D DS2A          E DS
003100900608     D DS2C          E DS
003200921013     D DS3K          E DS
003300960213     D DS2H          E DS
003400050608     d fnlg08ds      e ds
003500020821     d trul06ds      e ds
003600020821     d  lin                    1     90  0 dim(30)
003700960213     D TRUL42        E DS                  EXTNAME(TRUL42DS)
0038009603140    D TRUL43        E DS                  EXTNAME(TRUL43DS)
003900041028     D trul44ds      E DS
004000050902     D trtcm1ds      E DS
004100041029     d data8           s              8  0
004200041029     d dataiso         s               d   datfmt(*iso)
004300041029     d dataeur         s               d   datfmt(*eur)
004400941018     D FNLG74        E DS                  EXTNAME(FNLG74DS)
004500960123     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
004600160310     D TR27DS        E DS                  EXTNAME(TRUL27DS1)
004700900504     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004800900418     D  TCU                  398    697
004900900418     D                                     DIM(50)
005000900418     D  KCU                  698    847P 0
005100900418     D                                     DIM(50)
005200900418     D                                     PACKEVEN
005300900418     D TCUDS           DS
005400900418     D  F3                     3      3
005500900418     D  F4                     4      4
005600900418     D  F56                    5      6
005700960213     D*-----DS PER RICERCA NUOVO TIPO MODULO
005800960213     D                 DS
005900960213     D  WTPM                   1      1
006000960213     D  WFAX                   2      2
006100000913     D  WVAG                   3      3
006200960213     D  UNI                    1     69
006300000202     D OG143         E DS
006400000202     D CNCR42        E DS
006500911112     D CNCR80        E DS
006501171002      *ds per richiamo monitor variazioni
006502171002     d tigcp         e ds                  extname(tigcp00f)
006503171002     d fnlg33ds      e ds                  inz
006504171002     d tigcpa        e ds                  extname(tigcp00f)
006505171002     d                                     prefix(a)
006506171002     d tigcpb        e ds                  extname(tigcp00f)
006507171002     d                                     prefix(b)
006508171002      *
006600900418     D KPJBA         E DS
006700020930     d savkpjbu        s                   like(kpjbu)
006800041028      **
006900041029     d savNGC          s                   like(gcpNGC)
007000041029      **
007100041028     d Aperto_PRTF     s              1
007200041028      **
007300941018     D WLBDA8          DS
007400941018     D  G02DAT                 1      8  0
007500941018     D  G02INV                 9     16  0
007600941018     D  G02ERR                17     17
007700941018     D  G02TGI                18     22  0
007800150514     D fnlv59ds      E DS
007900961001     D*
008000961001     D                 DS
008100961001     D  WGG                    1      2
008200961001     D  WMM                    3      4
008300961001     D  WAA                    5      8
008400961001     D  WDATA                  1      8
008500041110      *
008600041110     d out70           s             71
008700041110     d out71           s             71
008800041110     d out72           s             71
008900041111      *
009000041111     D FNLV55DS      E DS
009100041110      *
009200041029     D chkc            C                   const('''/§')
009300041029     D chkcs           C                   const('   ')
009400061214      *
009500061214     D TxtInOut        S           2048
009600061214     D ElencoChar      S            256
009700061214     D TipoElenco      S              1
009800061214     D CharSost        S              1
009900061214     D UpperCase       S              1
010000061214     D ChkNull         S              1
010100061214     D CharNull        S              1
010200061214     D EsitoBon        S              1
010300941018     C*---------------------------------------------------------------*
010400020917     c                   clear                   trul06ds
010500020821     c                   if        simpou = simfel
010600020821     C* Cerco le filiali gestite nell'area
010700020821     c                   clear                   trul06ds
010800020821     c                   eval      d06cod = '£1'
010900020821     c                   movel     simfel        d06key
011000020930     c                   movel     kpjbu         savkpjbu
011100020821     c                   movel     trul06ds      kpjbu
011200020821     c                   call      'TRUL06R'
011300020821     c                   parm                    kpjba
011400020821     c                   movel     kpjbu         trul06ds
011500020930     c                   movel     savkpjbu      kpjbu
011600020821     c                   else
011700020821      *se è un secondo livello forzo solo quello in skiera
011800020821     c                   move      simpou        lin(1)
011900020821     c                   endif
012000020821
012100020821     c     kgcp          klist
012200020821     c                   kfld                    ksimpou           3 0
012300020821     c                   kfld                    d74cod
012400041028
012500020821      *devo eseguire il loop per ogni filiale se sono il primo livello a
012600020821      * causa dei fax che vengono inviati solo dal primo
012700060621      **
0128000606210    c                   do        30            i                 3 0
012900041028
013000020821     c                   if        lin(i) > 0
013100020821     c                   z-add     lin(i)        ksimpou
013200020821     c                   else
013300020821     c                   leave
013400020821     c                   end
013500020821
013600060217     c                   if        d74cod = 0
013700060317     c     ksimpou       setll     tigcp07l
013800060217     c                   else
013900060621     C     kgcp          SETLL     tigcp07L
014000060217     c                   end
014100060621      **
014200060621 1   C                   DO        *hival
014300041028      **
014400060621 2   C     D74COD        IFEQ      0
014500050216     C     ksimpou       READe     tigcp000                             1510
014600060621     c                   if        *in15
014700060621     c     ksimpou       reade(n)  tigcp000                             1510
014800060621     c                   iter
014900060621     c                   endif
015000060621 x2  C                   ELSE
015100050216     C     kgcp          READE     tigcp000                             1510
015200060621     c                   if        *in15
015300060621     C     kgcp          reade(n)  tigcp000                             1510
015400060621     c                   iter
015500060621     c                   endif
015600060621 e2  C                   END
015601171002     c                   eval      tigcpb = tigcp
015700060621      **
015800060621     C   10              leave
015900041028      **
016000971112     C                   SETOFF                                       30
016100971112     C** AGGANCIO AZORG CON GCPLNP
016200971119     C                   MOVEL     *BLANKS       ORGFL1
016300971112     C     GCPLNP        CHAIN     AZORG                              24
016400041028      **
016500960213     C** Se il tipo comunicazione fine giacenza è a blanks lo valorizzo
016600960213     C     GCPTFG        IFEQ      ' '
016700960213     C     GCPFAS        ANDEQ     50
016800960213     C                   EXSR      REPTFG
016900960213     C                   END
017000041028      **
017100041028     C** Reperisco informazioni sul tipo di comunicazione di chiusura
017200960213     C                   Z-ADD     1             §F
017300960213     C                   CLEAR                   DS2H
017400960213     C     GCPTFG        LOOKUP    C2H(§F)                                32
017500960213     C     *IN32         IFEQ      '1'
017600960213     C                   MOVE      D2H(§F)       UNI
017700960213     C                   MOVE      WTPM          §2HTPM
017800041028      * sul invio con Fax -> 'F' adesso può essere anche E-mail -> 'M'
017900960213     C                   MOVE      WFAX          §2HFAX
018000000913     C                   MOVE      WVAG          §2HVAG
018100960213     C                   END
018200041028      *
018300041028      *  Attenzione: se stiamo trattando solo le E-mail o solo la
018400041028      *   trasmissione dei FAX deve controllare la congruenza
018500041028     c                   move      'SI'          new_giacenza      2
018600041028      *
018700041028      *  in base ai 2 parametri
018800041028      *  Quando deve saltare a nuova giacenza :  S=Salta --> (Iter)
018900041028      *                                          N=OK continua
019000041028      *
019100041028      *                       tipologia comunicazione"2H"
019200041028      *                                 §2Hfax
019300041028      *                           | blk |  F  |  M  |
019400041028      *                D74FAX === +=====+=====+=====+
019500041028      *                  E        |  N  |  N  |  N  |
019600041028      *    da DS       ---------- *-----+-----+-----+
019700041028      *    tipo          N        |  N  |  S  |  S  |
019800041028      *    chiamata    ---------- *-----+-----+-----+
019900041028      *    pgm           F        |  S  |  N  |  S  |
020000041028      *                ---------- *-----+-----+-----+
020100041028      *                  M        |  S  |  S  |  N  |
020200041028      *                ---------- *-----+-----+-----+
020300041028      *
020400041028      * Se il record letto corrisponde ai parametri di richiesta
020500041028      *   non deve saltare ad un'altra giacenza.
020600041028     c                   if        D74fax = 'E' or
020700041029     c                             D74fax = 'N' and §2Hfax = ' ' or
020800041029     c                             D74fax = 'F' and §2Hfax = 'F' or
020900041029     c                             D74fax = 'M' and §2Hfax = 'M'
021000041028     c                   move      'NO'          new_giacenza
021100041028     c                   end
021200041028     C*
021300041028     C*   Ma anche se sto trattando la E-Mail di un cliente generico
021400041028     C*    NON posso inviare l'E-mail ad un 8888 o un 9999.
021500041028     c                   if        D74fax = 'M'
021600041028     C                   move      GCPSCM        codmit            4 0
021700041028     c                   if        codmit = 8888 or
021800041028     c                             codmit = 9999
021900041028     c                   move      'SI'          new_giacenza
022000041028     c                   end
022100041028     c                   end
022200041028     C*
022300041028     C* Deve saltare a leggere una nuova giacenza poichè quella appena
022400041028     C*  letta NON corrisponde ai parametri di filtro passati.
022500041028     c                   if        new_giacenza = 'SI'
022600041028     C*              *----------*
022700041028     c                   iter
022800041028     C*              *----------*
022900041028     c                   end
023000041028     C*
023100960314     C*  Controllo se esiste un luogo di invio comunicazione di giac.
023200960314     C                   CLEAR                   TRUL43
023300960314     C                   Z-ADD     GCPSCM        D43KSC
023400960314     C                   CALL      'TRUL43R'
023500960314     C                   PARM                    TRUL43
023600960618     C     D43PER        COMP      *BLANKS                            0404
023700970819     C                   MOVEL     D43PER        ATTPER           27
023800041028      **
023900041028      *  Attenzione a come reperire il Luogo x Indirizzo E-mail
024000041028      *   e considerare anche le note (88) !!!!!
024100041028      **
024200041028     C     §2HFAX        IFEQ      'M'
024300041028     C     D74FAX        ANDeq     'M'
024400041028     C                   EXSR      CTRMail
024500041028     C                   END
024600041028     C*
024700960213     C*  CONTROLLO IL FAX
024800960213     C                   SETOFF                                       14
024900960213     C     §2HFAX        IFEQ      'F'
025000960213     C     D74FAX        ANDNE     'N'
025100041029     C     D74FAX        andNE     'M'
025200960213     C                   EXSR      CTRFAX
025300960213     C                   END
025400041028     C*
025500911112     C** SE SI è IN FASE DI RISTAMPA SI ELABORANO SOLO LE GIACENZE
025600921015     C** CON FASE 50 E DATA COMUNICAZIONE <> 0 O SE RICHIESTA PER
025700930215     C** DATA TUTTE LE CHIUSURE STAMPATE IN QUELLA DATA.
025800950125     C   80GCPFAS        IFGE      50
025900911112     C     GCPFRG        ANDEQ     0
026000941018     C     GCPDLM        ANDGE     D74DT1
026100941018     C     GCPDLM        ANDLE     D74DT2
026200960213     C     §2HTPM        ANDEQ     'D'
026300911112     C                   ELSE
026400911112     C                   SETON                                        30
026500911112     C                   END
026600960213     C*
026700960213     C  N30§2HFAX        IFEQ      *BLANKS
026800960213     C     D74FAX        ANDEQ     'F'
026900960213     C     §2HFAX        OREQ      'F'
027000960213     C     D74FAX        ANDEQ     'N'
027100041028     C* Adesso occorre considerare anche l'invio con E-mail
027200041028     C     §2hfax        OREQ      'M'
027300041028     C     d74fax        ANDEQ     'N'
027400960213     C                   SETON                                        30
027500960213     C                   END
027600911112     C*
027700971112     C* CONTROLLO £7 X RISTAMPA
027800971112     C   80
027900060621     CANN30              DO
028000971112     C                   MOVE      GCPLNP        §LNP              3
028100971112     C     §LNP          LOOKUP    FIL£7                                  36
028200041028      **
028300060621     C     *IN36         IFEQ      '0'
028400971112     C     §2HFAX        ANDEQ     'F'
028500971112     C     D74FIL        ANDEQ     SIMFEL
028600041028      **
028700041028     C     *IN36         orEQ      '0'
028800041028     C     §2HFAX        andEQ     'M'
028900041028     C     d74FAX        andEQ     'M'
029000041028      **
029100971112     C                   SETON                                        36
029200971112     C                   END
029300041028      **
029400971112     C  N36              SETON                                        30
029500971112     C                   END
029600971112     C*
029700960213     C  N80
0298000606212    CANN30              DO
029900911112     C                   MOVE      GCPLNP        §LNP              3
030000911112     C     §LNP          LOOKUP    FIL£7                                  36
030100041028      **
0302000606213    C     *IN36         IFEQ      '0'
030300970109     C     §2HFAX        ANDEQ     'F'
030400970109     C     D74FIL        ANDEQ     SIMFEL
030500041028      **
030600041028     C     *IN36         orEQ      '0'
030700041028     C     §2HFAX        andEQ     'M'
030800041028     C     d74FAX        andEQ     'M'
030900041028      **
031000960213     C                   SETON                                        36
031100060621 e3  C                   END
031200041029      **
031300911112     C  N36              SETON                                        30
031400950227     C*
031500941018     C  N30GCPDLM        COMP      0                                  3030
031600941018     C  N30GCPFAS        COMP      50                                 3030
031700941018     C  N30GCPFRG        COMP      0                                  3030
031800960213     C  N30§2HTPM        COMP      'D'                                3030
031900950103     C*
032000060621 3   C     §2HTPM        IFEQ      'N'
032100950314     C     GCPDLM        ANDEQ     0
032200950314     C     GCPFAS        ANDEQ     50
032300950103     C                   Z-ADD     DATEU         GCPDLM
032400950227     C                   Z-ADD     60            GCPFAS
032500050422      *se = 0 aggiorno anche data stampa porta/preleva da magazzino giacenze
032600050422     c                   if        gcpdlma = 0
032700050422     c                   z-add     99999999      gcpdlma
032800050422     c                   endif
032900050422     c                   if        gcpduma = 0
033000141107     c*mm                z-add     99999999      gcpduma
033100050422     c                   endif
033200950227     C                   EXCEPT    AGGIA
033201171002     c                   exsr      RegVariazioni
033300050321     c                   if        gescomm = 'S'
033400050321     c                   commit
033500050321     c                   endif
033600970109     C* VERIFICO SE PER IL CLIENTE MITTENTE E' PREVISTO LO SCARICO
033700970109     C                   MOVEL     '3K'          COD
033800970109     C                   MOVEL     *BLANKS       KEY
033900970109     C                   MOVEL     GCPSCM        KEY
034000970109     C     KTAB          CHAIN     TABEL                              67
0341000606214    C  N67              DO
034200970109     C                   MOVEL     TBLUNI        DS3K
034300970109     C     §3KGQI        IFEQ      'C'
034400970109     C     §3KGQI        OREQ      'E'
034500050621     c                   clear                   fnlg08ds
034600050621     c                   eval      D08commit= 'S'
034700970109     C                   EXSR      SCAR
034800970109     C                   END
034900060621 4   C                   END
035000060621 3   C                   END
035100950227     C*
035200060621 2   C                   END
035300941019     C** AGGANCIO BOLLE
035400941019     C*
035500060621     C* SE TUTTO O.K. PROSEGUO
035600060621 2   C  N30              DO
035700041028      **
035800901220     C** AGGANCIO BOLLA DI ADDEBITO X STAMPARE L'EVENT. NR.FATTURA
035900901220     C                   SETOFF                                       41
036000901220     C                   MOVE      *ZEROS        DATDFT
036100901220     C                   MOVE      *ZEROS        §NFT
036200060621 3   C     GCPNBW        IFGT      *ZEROS
036300990929     C                   MOVEL     '1'           KTRC
036400990929     C     KBAD          CHAIN     FIAR601L                           25
036500060621 4   C  N25AR6NFT        IFGT      *ZEROS
036600901220     C                   SETON                                        41
036700990929     C                   MOVE      AR6NFT        §NFT
036800901220     C** DATA FATTURA
036900990929     C                   Z-ADD     AR6DFT        G02INV
037000941019     C                   Z-ADD     0             G02DAT
037100941019     C                   MOVEL     '3'           G02ERR
037200941019     C                   CALL      'XSRDA8'
037300941019     C                   PARM                    WLBDA8
037400941019     C                   Z-ADD     G02DAT        DATDFT            8 0
037500060621 e4  C                   END
037600060621 e3  C                   END
037700041028      **
037800950103     C** AGGANCIO STORICO BOLLE
037900950103     C     KGAP          CHAIN     FNBLP01L                           25
038000040325     C                   MOVEL     'D'           KTRC
038100060213     C                   MOVEL     *BLANKS       ar4NOT
038200060213     C     Kar4          CHAIN     Fiar401L                           31
038300960123     C                   CLEAR                   FNLSN0
038400960123     C                   Z-ADD     BLPAAS        DN0AAS
038500960123     C                   Z-ADD     BLPLNP        DN0LNP
038600960123     C                   Z-ADD     BLPNRS        DN0NRS
038700960123     C                   Z-ADD     BLPNSP        DN0NSP
038800960123     C                   MOVEL     *BLANKS       DN0TRC
038900960125     C                   Z-ADD     GCPDUR        DN0DAG
039000960123     C                   MOVEL     KPJBU         WSVKPJ          256
039100960123     C                   MOVEL     FNLSN0        KPJBU
039200960123     C                   CALL      'FNLSN0R'
039300960123     C                   PARM                    KPJBA
039400960123     C                   MOVEL     KPJBU         FNLSN0
039500960123     C                   MOVEL     WSVKPJ        KPJBU
039600960123     C* Destinatario
039700060621 3   C     DN0TRV        IFEQ      'S'
039800960123     C                   MOVEL     DN0RSD        STPRSD
039900960123     C                   MOVEL     DN0RS2        STPRS2
040000960123     C                   MOVEL     DN0IND        STPIND
040100960123     C                   MOVEL     DN0LOD        STPLOD
040200960123     C                   MOVEL     DN0PRD        STPPRD
040300960123     C                   MOVEL     DN0CAD        STPCAD
040400960123     C                   MOVEL     DN0NZD        STPNZD
040500960123     C                   ELSE
040600960123     C* DESTINATARIO
040700960123     C                   MOVEL     BLPRSD        STPRSD
040800060213     C                   MOVEL     ar4NOT        STPRS2
040900960123     C                   MOVEL     BLPIND        STPIND
041000960123     C                   MOVEL     BLPLOD        STPLOD
041100960123     C                   MOVEL     BLPPRD        STPPRD
041200960123     C                   MOVEL     BLPCAD        STPCAD
041300960123     C                   MOVEL     BLPNZD        STPNZD
041400060621 e3  C                   END
041500960123     C*
041600950103     C                   Z-ADD     BLPAAS        KAAS
041700950103     C                   Z-ADD     BLPLNP        KLNP
041800950103     C                   Z-ADD     BLPNRS        KNRS
041900950103     C                   Z-ADD     BLPNSP        KNSP
042000900507     C** DECODIFICA DELLA FILIALE DI PARTENZA
042100960618     C                   MOVEL     *BLANKS       DESLNP           20
042200960618     C                   MOVEL     *BLANKS       INDLNP           20
042300960618     C                   Z-ADD     0             CAPLNP            5 0
042400960618     C                   MOVEL     *BLANKS       LOCLNP           20
042500960618     C                   MOVEL     *BLANKS       PROLNP            2
042600960618     C                   MOVEL     *BLANKS       FAXLNP           13
042700960618     C                   MOVEL     *BLANKS       TELLNP           13
042800971112     C     *IN24         IFEQ      '0'
042900960618     C                   MOVEL     ORGDES        DESLNP
043000960618     C                   MOVEL     ORGIND        INDLNP
043100960618     C                   Z-ADD     ORGCPF        CAPLNP
043200960618     C                   MOVEL     ORGLOC        LOCLNP
043300960618     C                   MOVEL     ORGPRO        PROLNP
043400960618     C                   MOVEL     ORGTEL        TELLNP
043500960618     C                   MOVEL     ORGFAX        FAXLNP
043600960618     C                   END
043700960618     C                   SETON                                        13
043800020717     C                   MOVEL     ORGDE3        OG143
043900020717     C     §OgNTW        ifeq      'FED'
044000020717     C     §OgNTW        oreq      'EEX'
044100020717     C     §OgNTW        oreq      'EUP'
044200960618     C                   SETOFF                                       13
044300041111     c                   exsr      Hub_Estero
044400960618     C                   END
044500960618     C**
044600961128     C                   MOVE      GCPSCM        W0040             4 0
044700041028     C**
0448000410295    C**  Rottura x Cliente Mittente
044900960618     C     GCPSCM        IFNE      WSVSCM
045000961128     C     W0040         OREQ      8888
045100961128     C     W0040         OREQ      9999
045200960618     C                   Z-ADD     GCPSCM        WSVSCM            7 0
045300960618     C                   Z-ADD     0             STPPAG            4 0
045400960618     C                   END
045500041029     C**
045600041029      ** solo x E-mail
0457000410295    C**  Rottura x singola Giacenza
045800041029     C                   IF        gcpNGC <> savNGC and D74FAX = 'M'
045900041029     C                   Z-ADD     0             STPPAG            4 0
046000041029     C                   Z-ADD     gcpNGC        savNGC
046100041029     **
046200041029     ** se si tratta di E-mail esegue chiusura e apertura
046300041029     **  con OVRPRTF specifica
046400041029     c                   exsr      OVRPRTF_x_Mail
046500041029      ***
046600041029     C                   END
046700041029      ***
046800900503     C** SE IL CODICE è CODIFICATO PRENDO I DATI DELLA RAG.SOC.
046900900503     C** DAL PDC SE è CODICE 88 O 99 PRENDO I DATI O DALL'ANAGRAFICA
047000900503     C** D'APPOGGIO SE ESISTE O DALLO STORICO BOLLE
047100960314     C                   MOVEL     *BLANKS       RAG
047200960314     C                   MOVEL     *BLANKS       VIA
047300960314     C                   MOVEL     *BLANKS       CIT
047400960314     C                   MOVEL     *BLANKS       PRV
047500960314     C                   MOVEL     *BLANKS       CAP9
047600960314     C                   MOVEL     *BLANKS       STA
047700900503     C                   MOVE      GCPSCM        CODMIT            4 0
0478000410295    C*
047900060621 3   C     D43TRV        IFNE      'S'
048000960314     C*
048100060621 4   C     CODMIT        IFNE      8888
048200900503     C     CODMIT        ANDNE     9999
048300900503     C     KACO          CHAIN     CNACO000                           25
048400900503     C     KACO          CHAIN     CNIND000                           25
048500060621 5   C     *IN25         IFEQ      '0'
048600941019     C                   MOVEL     *BLANKS       CAP9              9
048700941019     C                   MOVEL     INDCAP        CAP9
048800060621 6   C     INDCAE        IFNE      *BLANKS
048900941019     C     INDCAE        ANDNE     *ZEROS
049000941019     C                   MOVEL     INDCAE        CAP9
049100060621 e6  C                   END
049200941019     C                   MOVEL     ACORAG        RAG              38
049300941019     C                   MOVEL     INDVIA        VIA              38
049400941019     C                   MOVEL     INDCIT        CIT              21
049500941019     C                   MOVEL     INDPRV        PRV               2
049600941019     C                   MOVEL     INDSTA        STA               3
049700060621 e5  C                   END
049800900503     C                   ELSE
049900050216     C     KGAP          CHAIN     tigap000                           25
050000060621 5   C     *IN25         IFEQ      '0'
050100941019     C                   MOVEL     GAPRSM        RAG
050200941019     C                   MOVEL     GAPINM        VIA
050300941019     C                   MOVEL     GAPLOM        CIT
050400941019     C                   MOVEL     GAPPRM        PRV
050500941019     C                   MOVEL     GAPNZM        STA
050600941019     C                   MOVEL     GAPCAM        CAP9
050700941019     C                   ELSE
050800941019     C                   MOVEL     BLPRSM        RAG
050900941019     C                   MOVEL     BLPINM        VIA
051000941019     C                   MOVEL     BLPLOM        CIT
051100941019     C                   MOVEL     BLPPRM        PRV
051200941019     C                   MOVEL     BLPNZM        STA
051300941019     C                   MOVEL     BLPCAM        CAP9
051400060621e5   C                   END
051500060621e4   C                   END
0516000410295x   C*  Controllo se devo reimpostare i dati dal luogo
051700960314     C                   ELSE
051800960314     C                   MOVEL     D43RAG        RAG
051900960314     C                   MOVEL     D43IND        VIA
052000960314     C                   MOVEL     D43LOC        CIT
052100960314     C                   MOVEL     D43PRO        PRV
052200960314     C                   MOVEL     D43CAP        CAP9
052300960314     C                   MOVEL     D43NAZ        STA
052400060621e3   C                   END
052500041029      **
052600961001     C** COSTRUISCO LOCALITA' + DATA
052700961001     C                   CLEAR                   LOCDAT
052800961001     C                   MOVEL     OGGI          WDATA
052900961001     C     ORGDES        CAT       ',':0         LOCDAT           32
053000961001     C     LOCDAT        CAT       WGG:1         LOCDAT
053100961001     C     LOCDAT        CAT       '/':0         LOCDAT
053200961001     C     LOCDAT        CAT       WMM:0         LOCDAT
053300961001     C     LOCDAT        CAT       '/':0         LOCDAT
053400961001     C     LOCDAT        CAT       WAA:0         LOCDAT
053500961001     C*
053600900503     C** RIBALTAMENTO DATE
053700900507     C                   EXSR      RIBDAT
053800041029     C*
053900900503     C** DECODIFICA CODICE DI FINE GIACENZA IN TABEL
054000900503     C                   MOVEL     '2C'          COD
054100900503     C                   MOVEL     *BLANKS       KEY
054200900503     C                   MOVEL     GCPCFG        KEY
054300900503     C     KTAB          CHAIN     TABEL                              25
054400900608     C  N25              MOVEL     TBLUNI        DS2C
054500041029      **
054600911203     C** DECODIFICA CODICE DI RIENTRO SE DIVERSO DA BLANKS
054700941019     C     GCPDIS        IFNE      *BLANKS
054800911203     C                   MOVEL     '2D'          COD
054900911203     C                   MOVEL     *BLANKS       KEY
055000941018     C                   MOVEL     GCPDIS        KEY
055100911203     C     KTAB          CHAIN     TABEL                              25
055200911203     C  N25              MOVEL     TBLUNI        RIENTR           20
055300911203     C   25              MOVEL     *BLANKS       RIENTR
055400911205     C                   ELSE
055500911205     C                   MOVEL     *BLANKS       RIENTR
055600911203     C                   END
055700041029      **
055800900507     C** CALCOLO TOT.GENERALE
055900990628     C                   Z-ADD     0             TOTGEN           13 3
056000900507     C                   ADD       GCPSGS        TOTGEN
056100900503     C                   ADD       GCPSGR        TOTGEN
056200900503     C                   ADD       GCPSGD        TOTGEN
056300911125     C                   ADD       GCPSGA        TOTGEN
056400900503     C**
056500900524     C* SE NON ADDEBITO SPESE STAMPO LETTERA DIVERSA
056600920120     C                   SETOFF                                       42
056700941018     C     GCPDIS        IFEQ      'R'
056800920120     C                   SETON                                        42
056900920120     C                   END
057000041029     C**
057100920120     C     GCPTFT        COMP      9                                      40
057200911213     C  N40GCPPSG        COMP      'D'                                    40
057300960618     C                   ADD       1             STPPAG
057400041029     C**
057500990706     C* SE LA VALUTA E' UGUALE A BLANKS IMPOSTO LE LIRE
057600990706     C     GCPDIV        IFEQ      *BLANKS
057700990716     C                   MOVEL     'ITL'         STPDIV            3
057800990706     C                   ELSE
057900990706     C                   MOVEL     GCPDIV        STPDIV
058000990706     C                   END
058100041029     C**
058200041029     c                   if        d74fax='M'
058300041029     C*               *--------------------*
058400041029     C                   EXCEPT    E_MAIL
058500041029     C*               *--------------------*
058600041029     c                   else
058700041110     c                   exsr      REPMITT
058800041028     C*               *--------------------*
058900900503     C                   EXCEPT    STAMPA
059000041028     C*               *--------------------*
059100041029     c                   end
0592000410295    C*
059300930215     C** SE IL FLAG = R RISTAMPA NON AGGIORNO
059400941018     C     D74RIS        IFNE      'R'
059500930215     C     GCPFAS        ANDEQ     50
059600930215     C     GCPDLM        ANDEQ     0
059700930215     C* VERIFICO SE PER IL CLIENTE MITTENTE E' PREVISTO LO SCARICO
059800930215     C                   MOVEL     '3K'          COD
059900930215     C                   MOVEL     *BLANKS       KEY
060000930215     C                   MOVEL     GCPSCM        KEY
060100930215     C     KTAB          CHAIN     TABEL                              67
060200930215     C  N67              DO
060300930215     C                   MOVEL     TBLUNI        DS3K
060400930215     C     §3KGQI        IFEQ      'C'
060500930215     C     §3KGQI        OREQ      'E'
060600050621     c                   clear                   fnlg08ds
060700930215     C                   EXSR      SCAR
060800050621     c                   eval      D08commit= 'N'
060900930215     C                   END
061000930215     C                   END
061100050621     C*
061200050621     C                   EXSR      AGGIOR
0613000506215-   C*
061400921124     C                   END
061500921124     C*
061600940608     C* SPOSTATO END : AGGIORN.E SCARICAVA X CLIENTE ANCHE SE TIPO INVIO
061700940608     C* ERA = A "RIEPILOGO "
061800060621     C*
061900060621e2   C                   END
062000060621     C*
062100060621e1   C                   END
062200060621     C*
062300060621e0   c                   enddo
062400041111      *
062500041111     C                   CLEAR                   FNLV55DS
062600041111     C                   MOVEL     'C'           D55TLA
062700041111     C                   CALL      'FNLV55R'
062800041111     C                   PARM                    FNLV55DS
062900160310     c
063000160310     C                   CLEAR                   tr27ds
063100160310     C                   MOVEL     'C'           I27Tla
063200160310     C                   CALL      'TRUL27R1'
063300160310     C                   PARM                    tr27ds
063400160310      *
063500160310     C                   CLEAR                   fnlv59ds
063600160310     C                   MOVEL     'C'           Ilv59tla
063700160310     C                   CALL      'FNLV59R'
063800160310     C                   PARM                    fnlv59ds
063900160310      *
064000041111      ***
064100041029     C*
064200900503     C                   SETON                                        LR
064300960213     C*------------------------------------------------------*
064400960213     C*  Posizionam. su anagrafica indirizzi x fax
064500960213     C*------------------------------------------------------*
064600960213     C     CTRFAX        BEGSR
064700960213     C*
064800961128    1C     D74RIS        IFNE      'R'
064900960213     C                   MOVE      GCPSCM        CODMIT            4 0
065000961128    2C     CODMIT        IFNE      8888
065100960213     C     CODMIT        ANDNE     9999
065200960213     C     KACO          CHAIN     CNIND000                           26
065300960213      *  Se invio tramite fax richiamo pgm x controllo validità
065400961128    3C     §2HFAX        IFEQ      'F'
065500960213     C                   CLEAR                   TRUL42
065600960213     C                   MOVEL     INDTLF        D42FAX
065700961128    4C     D43TRV        IFEQ      'S'
065800960314     C                   MOVEL     D43FAX        D42FAX
065900961128    4C                   END
066000960213     C                   CALL      'TRUL42R'
066100960213     C                   PARM                    TRUL42
066200961128    4C     D42ERR        IFEQ      '1'
066300960213     C                   MOVEL     *BLANKS       §2HFAX            1
066400960213     C                   Z-ADD     1             §F
066500960213     C                   MOVE      §2HTPM        WTPM
066600960213     C                   MOVE      §2HFAX        WFAX
066700960213     C     UNI           LOOKUP    D2H(§F)                                32
066800961128    5C     *IN32         IFEQ      '1'
066900960213     C                   MOVEL     C2H(§F)       GCPTFG
067000960213     C                   EXCEPT    AGG1
067001171002     c                   exsr      RegVariazioni
067100050321     c                   if        gescomm = 'S'
067200050321     c                   commit
067300050321     c                   endif
067400961128    5C                   END
067500961128   X4C                   ELSE
067600960213     C                   SETON                                        14
067700960213     C                   MOVEL     INDTLF        STPFAX           16
067800961128    5C     D43TRV        IFEQ      'S'
067900960314     C                   MOVEL     D43FAX        STPFAX
068000961128    5C                   END
068100961128    4C                   END
068200961128    3C                   END
068300961128   X2C                   ELSE
068400961128    3C     §2HFAX        IFEQ      'F'
068500960213     C                   MOVEL     *BLANKS       §2HFAX
068600960213     C                   Z-ADD     1             §F
068700960213     C                   MOVE      §2HTPM        WTPM
068800960213     C                   MOVE      §2HFAX        WFAX
068900960213     C     UNI           LOOKUP    D2H(§F)                                32
069000961128    4C     *IN32         IFEQ      '1'
069100960213     C                   MOVEL     C2H(§F)       GCPTFG
069200960213     C                   EXCEPT    AGG1
069201171002     c                   exsr      RegVariazioni
069300050321     c                   if        gescomm = 'S'
069400050321     c                   commit
069500050321     c                   endif
069600961128    4C                   END
069700961128    3C                   END
069800961128    2C                   END
069900960213     C*
070000961128   X1C                   ELSE
070100960213     C                   MOVE      GCPSCM        CODMIT            4 0
070200961128    2C     CODMIT        IFNE      8888
070300960213     C     CODMIT        ANDNE     9999
070400960213     C     §2HFAX        ANDEQ     'F'
070500960213     C     KACO          CHAIN     CNIND000                           26
070600961128     C                   SETOFF                                       14
070700960213     C                   MOVEL     INDTLF        STPFAX           16
070800961128    3C     D43TRV        IFEQ      'S'
070900960314     C                   MOVEL     D43FAX        STPFAX
071000961128    3C                   END
071100961128    2C                   END
071200961128    1C                   END
071300960213     C*
071400960213     C                   ENDSR
071401171002     C*__________________________________________________________
071402171002     c     RegVariazioni begsr
071403171002     C*__________________________________________________________
071404171002     c                   eval      ILG33CTA = 'M'
071405171002     c                   eval      ILG33PRU = knmus
071406171002     c                   eval      ILG33NOJ = knmeb
071407171002     c                   eval      ILG33PGM = 'FNLG17R'
071408171002     c                   eval      tigcpa = tigcp
071409171002     c                   call      'FNLG33R'
071410171002     c                   parm                    fnlg33ds
071411171002     c                   parm                    tigcpb
071412171002     c                   parm                    tigcpa
071413171002     c                   endsr
071500041028     C*----------------------------------------------------------------*
071600041028     C*  Controllo dati x mail
071700041028     C*----------------------------------------------------------------*
071800041028     C     CTRMail       BEGSR
071900041028     C*
072000041028     C*
072100041028     C*
072200041028     C                   ENDSR
072300041028     C*----------------------------------------------------------------*
072400041028     C*  Chiusura PRTF  x mail
072500041028     C*----------------------------------------------------------------*
072600041028     C     Close_PRTF    BEGSR
072700041028     C*
072800041028     C                   close     LETGIC
072900041028     c                   eval      Aperto_PRTF = 'N'
073000041028     C*  DLTOVR
073100050609     c                   z-add     50            lung
073200050608     C                   MOVEL     *BLANKS       COMMAN           80
073300041028     C                   MOVEA     dlt1(1)       COMMAN
073400041029     C                   CALL      'QCMDEXC'                            99
073500041028     C                   PARM                    COMMAN
073600050608     C                   PARM                    LUNG             15 5
073700041028     C*
073800041028     C                   ENDSR
073900041028     C*----------------------------------------------------------------*
074000041028     C*  Esegue OVRPRTF x lancio E-mail
074100041028     C*----------------------------------------------------------------*
074200041028     C     OVRPRTF_x_mailBEGSR
074300041028     C*
074400041028     C* Solo x E-mail
074500041028     c                   if        d74fax = 'M'
074600041028     C*
074700041028     C*   Prima di tutto chiude il PRTF
074800041028     c                   if        Aperto_PRTF = 'S'
074900041028     c                   exsr      Close_PRTF
075000041028     c                   endIF
075100041028     C*
075200041028     C*  Quindi riapre eseguendo la giusta OVRPRTF x invio coda E-mail
075300041028     C*   con tutti i parametri tramite il TRUL44R
075400041028     C*
075500041028     c                   if        Aperto_PRTF = 'N'
075600041029     c                   movel     gcpagc        data8
075700041029     c                   move      gcpmgc        data8
075800041029     c                   move      data8         dataiso
075900041029     c                   move      dataiso       dataeur
076000041029     c                   move      dataeur       data8
076100041029     c                   movel     blprsd        rsd20            20
076200041029     c     chkc:chkcs    xlate     rsd20         rsd20
076300041028     c                   clear                   trul44ds
076400050902     c                   clear                   trtcm1ds
076500050902     c                   eval      d44pgm = 'FNLG75R'
076600041028     c                   eval      d44cl  = '005'
076700041028     c                   move      gcpSCM        d44ksc
076800041028     c                   eval      d44apl = 'C'
076900041118     c                   eval      d44nk1 ='0151'+ %editc(gcpSCM:'X')
077000041028     c                   eval      d44tnt = '88'
077100041028     c                   eval      d44scope='*JOB'
077200041028     c                   eval      d44ovr = 'S'
077300041028     c                   eval      d44obj = '*OBJM*Comunicazione Chiusura Giac-
077400041118     c                             enza Nr.' + %editc(gcpfgc:'X') + '-' +
077500041029     c                             %trim(%editc(gcpngc:'Z')) + ' ' +
077600041029     c                             %editw(data8:'  -  -    ') +
077700041029     c                             ' Cli: '+  %editc(gcpscm:'Z') + ' ' +
077800041029     c                             rsd20 + '-' + blpprd
077900061214     C* Imposto i parametri x il *pgm d "bonifica"
078000061214     C                   EVAL      ElencoChar = *blanks
078100061214     C                   EVAL      TipoElenco = *blanks
078200061214     C                   EVAL      CharSost   = *blanks
078300061214     C                   EVAL      UpperCase  = ' '
078400061214     C                   EVAL      ChkNull    = '1'
078500061214     C                   EVAL      CharNull   = *blanks
078600061214     C*
078700061214     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
078800061214     C                   EVAL      TxtInOut   = d44obj
078900061214     C                   EVAL      EsitoBon   = *blanks
079000061214     C*
079100061214     C                   CALL      'XCHKCHAR'
079200061214     C                   PARM                    TxtInOut
079300061214     C                   PARM                    ElencoChar
079400061214     C                   PARM                    TipoElenco
079500061214     C                   PARM                    CharSost
079600061214     C                   PARM                    UpperCase
079700061214     C                   PARM                    ChkNull
079800061214     C                   PARM                    CharNull
079900061214     C                   PARM                    EsitoBon
080000061214     C*
080100061214     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
080200061214     C                   IF        EsitoBon = '1'
080300061214     C                   EVAL      d44obj  = TxtInOut
080400061214     C                   ENDIF
080500061214
080600041028     c                   eval      d44prtf = 'LETGIC'
080700041028     c                   call      'TRUL44R'
080800041028     c                   parm                    trul44ds
080900050902     c                   parm                    trtcm1ds
081000041028      **  Se c'è stato qualche problema
081100041028     c     d44err        ifne      *blank
081200041028     c                   end
081300041028     C**
081400041028     C*   Apre il file dopo OVRPRTF
081500041028     c                   open      LETGIC
081600041028     c                   eval      Aperto_PRTF = 'S'
081700041028     c                   endIF
081800041028     C*
081900041028     c                   endIF
082000041028     C*
082100041028     C                   ENDSR
082200041110     C*------------------------------------------------------*
082300041110     C*  Prepara stringa mittente per invio riferimento per FAX
082400041110     C*------------------------------------------------------*
082500041110     C     repmitt       begsr
082600041110      **
082700041110      **  x impostare fine stampa x invito a utilizzare E-mail
082800041110     c                   setoff                                       17
082900041110     c                   clear                   out70
083000041110     c                   clear                   out71
083100041110     c                   clear                   out72
083200041110      **
083300041110     c                   IF        d74fax = 'F' or d74fax = 'E'
083400041110      **
083500041110      ** Prende i parametri con il TRUL44R
083600041110     c                   clear                   mitt_x_fax       21
083700041110     c                   clear                   trul44ds
083800050902     c                   clear                   trtcm1ds
083900050902     c                   eval      d44pgm = 'FNLG75R'
084000041110     c                   eval      d44cl  = '005'
084100041110     c                   move      gcpSCM        d44ksc
084200041110     c                   eval      d44apl = 'C'
084300041118     c                   eval      d44nk1 ='0151'+ %editc(gcpSCM:'X')
084400041110     c                   eval      d44tnt = '88'
084500041110     c                   eval      d44prtf = 'LETGIC'
084600041110     c                   call      'TRUL44R'
084700041110     c                   parm                    trul44ds
084800050902     c                   parm                    trtcm1ds
084900041110      **  Se c'è stato qualche problema
085000041110     c     d44err        cabeq     'E'           salto_mit
085100041110      **
085200041110     c                   seton                                        17
085300041110     c                   eval      mitt_x_fax = D44mit
085400041110      **
085500041110     c                   eval      out70 = 'Per ricevere le prossime comunicazi-
085600041110     c                             oni via e-mail, Vi invitiamo a darne'
085700041110     c                   eval      out71 = 'segnalazione a: '+
085800041110     c                             %trim(mitt_x_fax) +
085900041110     c                             ' indicando i seguenti dati: '
086000041110     c                   eval      out72 = 'Codice cliente ' +
086100041110     c                             %editc(D44ksc:'Z') +
086200041110     c                             ' e-mail: __________________________________'
086300041110     c                   endIF
086400041110      *
086500041110     C     salto_mit     endsr
086600950227     C*---------------------------------------------------------------*
086700950227     C*   R E P T F G    Reperisco codice fine giacenza               *
086800950227     C*---------------------------------------------------------------*
086900950227     C     REPTFG        BEGSR
087000950227     C*
087100950227     C                   MOVEL     'R'           GCPTFG
087200950227     C     KGAP          CHAIN     FNBLP01L                           16
087300950227     C                   MOVEL     ' '           WTROVA            1
087400950227     C                   MOVE      GCPSCM        KKSC              7 0
087500950321     C                   MOVE      KKSC          §QUATR            4 0
087600950227     C                   Z-ADD     1             Y                 3 0
087700950228     C     BLPNZD        LOOKUP    CNZ(Y)                                 32
087800020222      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
087900020222     C                   CLEAR                   TR27DS
088000021118     C                   MOVE      BLPctr        I27ctb
088100021118     C                   MOVE      BLPTSP        I27TSP
088200020222     C                   MOVE      BLPLNA        I27LNA
088300020222     C                   MOVE      BLPLNP        I27LNP
088400020222     C                   MOVE      KKSC          I27CLI
088500160310     C                   CALL      'TRUL27R1'
088600020222     C                   PARM                    TR27DS
088700950227     C  N16§QUATR        IFNE      *ALL'8'
088800950227     C     §QUATR        ANDNE     *ALL'9'
088900950227     C     §QUATR        ANDNE     *ZEROS
089000950227     C                   Z-ADD     BLPMGS        DATDSP            8 0
089100950227     C                   MOVEL     BLPAAS        DATDSP
089200150514     C                   CLEAR                   fnlv59ds
089300150514     C                   MOVEL     ' '           ilv59tbo
089400150514     C                   Z-ADD     DATDSP        ilv59dsp
089500150514     C                   MOVE      BLPCTR        ilv59CTR
089600150514     C                   MOVE      KCI           ilv59kci
089700150514     C                   MOVE      KKSC          ilv59KSC
089800000202     C                   MOVEL     KKSC          W0030             3 0
089900150514     C                   MOVEL     O27FIE        ilv59FIE
090000150514     C                   MOVEL     BLPTSP        ilv59tsp
090100150514     C                   MOVEL     'S'           ilv59CON
090200150514     c                   eval      ilv59ta2='B'
090300150514     c                   eval      ilv59blo='S'
090400150514     C                   CALL      'FNLV59R'
090500150514     C                   PARM                    fnlv59ds
090600950227     C*
090700150514     C     olv59ERR      IFEQ      *BLANKS
090800150514     C     olv59tks      ANDEQ     'S'
090900950227     C*  Se ho trovato tariffa in vigore mi posiziono sul file
091000950227     C*  delle tariffe giacenze con il proggressivo giusto
091100150514     C                   MOVE      olv59ctr      KCTR
091200150514     C                   MOVE      olv59prg      KPRG
091300150514     C                   MOVE      ilv59ksc      KKSC
091400990730     C     KTGC          CHAIN     TITGC01L                           31
091500950227     C     *IN31         IFEQ      '0'
091600950227     C     TGCTFG        ANDNE     *BLANKS
091700950227     C                   MOVE      TGCTFG        GCPTFG
091800950227     C                   MOVEL     'S'           WTROVA
091900950227     C                   END
092000950227     C                   END
092100950227     C                   END
092200950227     C*  Se non ho trovato tipo comunicazione mitt. su tariffe
092300950227     C*  lo cerco sull'anagrafica cliente
092400950227     C     WTROVA        IFNE      'S'
092500950227     C                   MOVEL     *BLANKS       CLSFLO
092600950227     C     KKSC          CHAIN     FNCLS01L                           31
092700950227     C     CLSFLO        IFNE      *BLANKS
092800950227     C                   MOVEL     CLSFLO        GCPTFG
092900950227     C                   END
093000950227     C                   END
093100950227     C*
093200950227     C                   ENDSR
093300921013     C*------------------------------------------------------------------------*
093400900507     C     RIBDAT        BEGSR
093500921013     C*----------
093600921013     C* RIBALTAMENTO DATE
093700921013     C*----------
093800900507     C** DATA APERTURA GIACENZA
093900941019     C                   MOVEL     GCPAGC        G02INV
094000941019     C                   MOVE      GCPMGC        G02INV
094100941019     C                   Z-ADD     0             G02DAT
094200941019     C                   MOVEL     '3'           G02ERR
094300941019     C                   CALL      'XSRDA8'
094400941019     C                   PARM                    WLBDA8
094500941019     C                   Z-ADD     G02DAT        DATGIA            8 0
094600900507     C** DATA SPEDIZIONE
094700941019     C                   MOVEL     BLPAAS        G02INV
094800941019     C                   MOVE      BLPMGS        G02INV
094900941019     C                   Z-ADD     0             G02DAT
095000941019     C                   MOVEL     '3'           G02ERR
095100941019     C                   CALL      'XSRDA8'
095200941019     C                   PARM                    WLBDA8
095300941019     C                   Z-ADD     G02DAT        DATSPE            8 0
095400900507     C** DATA CHIUSURA GIACENZA
095500941019     C                   Z-ADD     GCPDCG        G02INV
095600941019     C                   Z-ADD     0             G02DAT
095700941019     C                   MOVEL     '3'           G02ERR
095800941019     C                   CALL      'XSRDA8'
095900941019     C                   PARM                    WLBDA8
096000941019     C                   Z-ADD     G02DAT        DATDCG            8 0
096100900507     C** DATA SOSTA DAL
096200050216     C                   Z-ADD     GCPDur        G02INV
096300941019     C                   Z-ADD     0             G02DAT
096400941019     C                   MOVEL     '3'           G02ERR
096500941019     C                   CALL      'XSRDA8'
096600941019     C                   PARM                    WLBDA8
096700941019     C                   Z-ADD     G02DAT        DATDSD            8 0
096800900507     C** DATA SOSTA AL
096900941019     C                   Z-ADD     GCPDSA        G02INV
097000941019     C                   Z-ADD     0             G02DAT
097100941019     C                   MOVEL     '3'           G02ERR
097200941019     C                   CALL      'XSRDA8'
097300941019     C                   PARM                    WLBDA8
097400941019     C                   Z-ADD     G02DAT        DATDSA            8 0
097500900507     C** DATA CREAZ.BOLLA GIAC.
097600941019     C                   Z-ADD     GCPDBG        G02INV
097700941019     C                   Z-ADD     0             G02DAT
097800941019     C                   MOVEL     '3'           G02ERR
097900941019     C                   CALL      'XSRDA8'
098000941019     C                   PARM                    WLBDA8
098100941019     C                   Z-ADD     G02DAT        DATDBG            8 0
098200941019     C*
098300900507     C                   ENDSR
098400900504     C*---------------------------------------------------------------*
098500941018     C*   OPERAZIONI INIZIALI                                         *
098600900504     C*---------------------------------------------------------------*
098700941018     C     *INZSR        BEGSR
098800041028      **
098900900504     C     *ENTRY        PLIST
099000900504     C                   PARM                    KPJBA
099100041028     C*
099200041028     C* Apre sempre il PRTF
099300041028     c                   open      LETGIC
099400041028     c                   eval      Aperto_PRTF = 'S'
099500041028      **
099600900504     C                   Z-ADD     1             CODUT
099700900504     C                   CALL      'X§PARUT'
099800900504     C                   PARM                    UTEDSE
099900911112     C*          CODUT     CABEQ-1        FINE
100000900504     C                   MOVEL     RAGUT         RSUT             20
100100900504     C                   MOVEL     RAGUT         RSUT2            25
100200900504     C*--- RICERCA CAPOCONTI------------------------------------------*
100300900504     C                   DO        50            X                 2 0
100400900504     C                   MOVE      TCU(X)        TCUDS
100500941018     C     F56           IFEQ      'CG'
100600941018     C     F4            ANDEQ     '1'
100700941018     C     F3            ANDEQ     '0'
100800941018     C                   Z-ADD     KCU(X)        KCI               4 0
100900941018     C                   END
101000900504     C                   END
101100900504     C*-                                                   *
101200900504     C                   MOVEL     *BLANKS       DITRGS
101300900504     C                   MOVEL     REC42         CNCR42
101400900504     C                   MOVEL     SIMRGS        DITRGS           20
101500911112     C                   MOVEL     REC80         CNCR80
101600930215     C* GIRO DATA DEL GIORNO
101700941018     C                   CLEAR                   WLBDA8
101800941018     C                   TIME                    WHHDAT           14 0
101900941019     C                   MOVE      WHHDAT        OGGI              8 0
102000941018     C                   MOVE      WHHDAT        G02DAT
102100941018     C                   CALL      'XSRDA8'
102200941018     C                   PARM                    WLBDA8
102300941018     C                   Z-ADD     G02INV        DATEU             8 0
102400900504     C*
102500941018     C                   MOVEL     KPJBU         FNLG74
102600050321      * flag gestione commit
102700050321     c                   move      d74comm       gescomm           1
102800941018     C*---------------------------------------------------------------*
102900941018     C     KACO          KLIST
103000941018     C                   KFLD                    CODUT
103100941018     C                   KFLD                    KCI
103200941018     C                   KFLD                    GCPSCM
103300941019     C*
103400060213     C     Kar4          KLIST
103500941019     C                   KFLD                    KAAS
103600941019     C                   KFLD                    KLNP
103700941019     C                   KFLD                    KNRS
103800941019     C                   KFLD                    KNSP
103900941019     C                   KFLD                    KTRC
104000941019     C*
104100941018     C     KGAP          KLIST
104200941018     C                   KFLD                    GCPAAS
104300941018     C                   KFLD                    GCPLNP
104400941018     C                   KFLD                    GCPNRS
104500941018     C                   KFLD                    GCPNSP
104600941018     C     KBAD          KLIST
104700941018     C                   KFLD                    GCPASW
104800941018     C                   KFLD                    GCPLPW
104900941018     C                   KFLD                    GCPNSW
105000941018     C                   KFLD                    GCPNBW
105100990929     C                   KFLD                    KTRC
105200941018     C     KTAB          KLIST
105300941018     C                   KFLD                    CODUT
105400941018     C                   KFLD                    COD               2
105500941018     C                   KFLD                    KEY               8
105600941018     C*
105700941018     C     KGNP          KLIST
105800941018     C                   KFLD                    KAGC
105900941018     C                   KFLD                    KFGC
106000941018     C                   KFLD                    KNGC
106100941018     C                   KFLD                    KFRG
106200941018     C                   KFLD                    KFAS
106300941018     C                   KFLD                    KNPR
106400950227     C***
106500950227     C     KTGC          KLIST
106600950227     C                   KFLD                    KKSC
106700950227     C                   KFLD                    KCTR
106800950227     C                   KFLD                    KPRG
106900950227     C*
107000950227     C     KTABK         KLIST
107100950227     C                   KFLD                    CODUT
107200950227     C                   KFLD                    COD               2
107300941018     C*---------------------------------------------------------------*
107400941018     C*  LIKE DEFN
107500960123     C     *LIKE         DEFINE    BLPRSD        STPRSD
107600060213     C     *LIKE         DEFINE    ar4NOT        STPRS2
107700960123     C     *LIKE         DEFINE    BLPIND        STPIND
107800960123     C     *LIKE         DEFINE    BLPLOD        STPLOD
107900960123     C     *LIKE         DEFINE    BLPPRD        STPPRD
108000960123     C     *LIKE         DEFINE    BLPCAD        STPCAD
108100960123     C     *LIKE         DEFINE    BLPNZD        STPNZD
108200941019     C     *LIKE         DEFINE    BLPAAS        KAAS
108300941019     C     *LIKE         DEFINE    BLPLNP        KLNP
108400941019     C     *LIKE         DEFINE    BLPNRS        KNRS
108500941019     C     *LIKE         DEFINE    BLPNSP        KNSP
108600060213     C     *LIKE         DEFINE    ar4TRC        KTRC
108700941018     C     *LIKE         DEFINE    GNPAGC        KAGC
108800941018     C     *LIKE         DEFINE    GNPFGC        KFGC
108900941018     C     *LIKE         DEFINE    GNPNGC        KNGC
109000941018     C     *LIKE         DEFINE    GNPFRG        KFRG
109100941018     C     *LIKE         DEFINE    GNPFAS        KFAS
109200941018     C     *LIKE         DEFINE    GNPNPR        KNPR
109300990929     C     *LIKE         DEFINE    AR6NFT        §NFT
109400950227     C     *LIKE         DEFINE    TGCCTR        KCTR
109500950227     C     *LIKE         DEFINE    TGCPRG        KPRG
109600941018     C*---------------------------------------------------------------*
109700950227     C* Carico CAMBI/DIVISE
109800950227     C                   Z-ADD     0             §F                3 0
109900950227     C                   MOVEL     '15'          COD
110000950227     C     KTABK         CHAIN     TABEL                              21
110100950227     C     *IN21         DOWEQ     '0'
110200011221     C     §F            ANDLE     500
110300950227     C     TBLFLG        IFEQ      *BLANK
110400950227     C                   ADD       1             §F
110500950227     C                   MOVEL     TBLUNI        DS15
110600950227     C                   MOVEL     TBLKEY        CNZ(§F)
110700950227     C                   MOVEL     §15ITA        ITA(§F)
110800950227     C                   END
110900950227     C     KTABK         READE     TABEL                                  21
111000950227     C                   END
111100960213     C* Carico tipi comunicazione fine giacenza
111200960213     C                   Z-ADD     0             §F                3 0
111300960213     C                   MOVEL     '2H'          COD
111400960213     C     KTABK         CHAIN     TABEL                              21
111500960213     C     *IN21         DOWEQ     '0'
111600960213     C     §F            ANDLE     100
111700960213     C     TBLFLG        IFEQ      *BLANK
111800960213     C                   ADD       1             §F
111900960213     C                   MOVEL     TBLKEY        C2H(§F)
112000960213     C                   MOVE      TBLUNI        D2H(§F)
112100960213     C                   END
112200960213     C     KTABK         READE     TABEL                                  21
112300960213     C                   END
112400941018     C                   Z-ADD     66            CONRIG            3 0
112500941018     C* GUARDO CHE UTENTE HA FATTO LA RICHIESTA
112600941018     C                   MOVEL     KNMUS         §SEI              6
112700020730     C     SimTpP        COMP      '2'                                    01
112800020730     C                   MOVE      SimPOU        FLE               3
112900941018     C*
113000941018     C     D74RIS        COMP      'R'                                    80
113100941018     C* VERIFICO SE LA FILIALE COLLEGATA NE GESTISCE ALTRE
113200941018     C                   MOVEA     *ZEROS        FIL£7
113300941018     C                   MOVEL     *BLANKS       KEY
113400970109     C                   MOVEL     D74FIL        KEY
113500941018     C                   MOVEL     '£7'          COD
113600941018     C     KTAB          CHAIN     TABEL                              15
113700941018     C  N15              MOVEA     TBLUNI        FIL£7
113800970109     C   15              MOVE      D74FIL        FIL£7
113900041110      **
114000041110      ** riga di separazione in stampa
114100041110     c                   move      *all'-'       lin75            75
114200041110      **
114300900504     C                   ENDSR
114400900504     C**----------------------
114500900504     C** AGGIORNAMENTO DATA STAMPA
114600900504     C** LETT.MITT. CHIUSURA GIACENZA
114700900504     C**----------------------
114800900507     C     AGGIOR        BEGSR
114900930215     C                   Z-ADD     DATEU         GCPDLM
115000941019     C                   Z-ADD     60            GCPFAS
115100050422      *se = 0 aggiorno anche data stampa porta/preleva da magazzino giacenze
115200050422     c                   if        gcpdlma = 0
115300050422     c                   z-add     99999999      gcpdlma
115400050422     c                   endif
115500050422     c                   if        gcpduma = 0
115600141107     c*mm                z-add     99999999      gcpduma
115700050422     c                   endif
115800941019     C                   EXCEPT    AGGIA
115801171002     c                   exsr      RegVariazioni
115900050321     c                   if        gescomm = 'S'
116000050321     c                   commit
116100050321     c                   endif
116200900504     C                   ENDSR
116300050621     C*-----------------------------------------------------
116400921013     C     SCAR          BEGSR
116500050621     C**----------------------------------------------------
116600050608     c                   eval      D08AGC= gcpagc
116700050608     c                   eval      D08FGC= gcpfgc
116800050608     c                   eval      D08NGC= gcpngc
116900050608     c                   eval      D08uni= §3kgks
117000050608     c                   eval      D08GSD= §3kGSD
117100050608     c                   eval      D08pgm= 'FNLG17R'
117200050609     c                   eval      D08tpr= 'C'
117300050608     c                   movel(p)  fnlg08ds      kpjbu
117400050608     c                   call      'FNLG08R'                            28
117500050608     c                   parm                    kpjba
117600050608     c                   movel     kpjbu         fnlg08ds
117700050608     C*
117800921013     C                   ENDSR
117900041111     C***------------------------------------------------***
118000041111      *  reperisce informazioni Hub di partenza Estero
118100041111     C***------------------------------------------------***
118200041111     C     Hub_Estero    Begsr
118300041111      *
118400041111     C                   CLEAR                   FNLV55DS
118500041111     C                   MOVEL     'P'           D55TPT
118600041111     C                   MOVE      gcpLNP        D55LIN
118700041111     C                   MOVE      gcpLNP        D55LNP
118800041111     C                   MOVE      dateu         d55drf
118900041111     C                   CALL      'FNLV55R'
119000041111     C                   PARM                    FNLV55DS
119100041111     c                   if        d55err = *blanks
119200041111     c     d55tfp        chain     azorg
119300041111     c                   if        %found(azorg01l)
119400041111     c                   movel     orgdes        desLNP
119500041111     c                   movel     orgfax        faxLNP
119600041111     c                   movel     orgtel        telLNP
119700041111     c                   endif
119800041111     c                   endif
119900041111      *
120000041111     C                   ENDSR
120100921013     C***------------------------------------------------***
120200050216     Otigcp000  E            AGGIA
120300950227     O                       GCPTFG
120400950227     O                       GCPDLM
120500941019     O                       GCPFAS
120600050422     O                       GCPDlma
120700141107     O*mm                    GCPDuma
120800050216     Otigcp000  E            AGG1
120900960213     O                       GCPTFG
121000941019     C***------------------------------------------------***
121100120328     Oletgic    E   N14      STAMPA           04
121200120328     o                       gcplnp               4
121300120328     O          E    14      STAMPA           05
121400960213     O                                           50 'FAX:'
121500960213     O                       STPFAX              67
121600961114     O          E    14      STAMPA           06
121700960618     O                                           75 'PAG.:'
121800960618     O                       STPPAG        Z     80
121900961114     O          E    14      STAMPA           08
122000071018     O******                                   +  6 'Punto Operativo di'
122100071018     O                                         +  6 'Filiale di'
122200961114     O          E    14      STAMPA           09
122300961001     O                       DESLNP            +  6
122400961114     O          E   N14      STAMPA           10
122500071018     O******                                   +  6 'Punto Operativo di'
122600120330     O                                         +  0 'Filiale di'
122700961114     O                                           75 'PAG.:'
122800961114     O                       STPPAG        Z     80
122900961114     O          E    14      STAMPA           10
123000960618     O               13      INDLNP            +  6
123100961114     O          E   N14      STAMPA           11
123200120330     O                       DESLNP            +  0
123300961114     O                                           53 'Spettabile'
123400961114     O               80                           3 'R'
123500961114     O          E    14      STAMPA           11
123600960618     O               13      CAPLNP            +  6
123700960618     O               13      LOCLNP            +  1
123800960618     O               13      PROLNP            +  1
123900960618     O                                           53 'Spettabile'
124000960618     O               80                           3 'R'
124100961114     O          E   N14      STAMPA           12
124200120330     O               13      INDLNP            +  0
124300961114     O          E   N14      STAMPA           13
124400120330     O               13      CAPLNP            +  0
124500961114     O               13      LOCLNP            +  1
124600120328     O*              13      PROLNP            +  1
124700961114     O                       RAG                 81
124800961114     O          E    14      STAMPA           13
124900960618     O                                         +  6 'FAX'
125000960618     O                       FAXLNP            +  1
125100960618     O                       RAG                 81
125200961114     O          E   N14      STAMPA           14
125300120330     O                                         +  0 'FAX'
125400961114     O                       FAXLNP            +  1
125500961114     O                       VIA                 81
125600961114     O          E    14      STAMPA           14
125700960618     O                                         +  6 'TEL'
125800960618     O                       TELLNP            +  1
125900960618     O                       VIA                 81
126000961114     O          E            STAMPA           15
126100120330     O              N14                        +  0 'TEL'
126200961114     O              N14      TELLNP            +  1
126300960618     O                       CAP9                52
126400960618     O                       CIT                 74
126500960618     O                       PRV                 77
126600960618     O                       STA                 81
126700961001     O          E            STAMPA           16
126800120330     O                       LOCDAT              32
126900960618     O               04                          53 'Att.ne di '
127000970819     O               04      ATTPER              81
127100120328     O          E            STAMPA           22
127200900627     O                                           14 'OGGETTO:'
127300900627     O                                           26 'Giacenza n.'
127400950125     O                       GCPFGC        Z     30
127500950125     O                       GCPNGC        Z     38
127600950125     O                                           42 'del'
127700950125     O                       DATGIA              53 '  /  /    '
127800120328     O          E            STAMPA           24
127900900627     O                                           19 'Spedizione n.'
128000950125     O                       GCPLNP        Z     23
128100950125     O                       GCPNRS        Z     26
128200950125     O                       GCPNSP        Z     34
128300950125     O                                           38 'del'
128400950125     O                       DATSPE              49 '  /  /    '
128500950125     O                                           60 'Vs. rifer.'
128600950125     O                       BLPRMN        Z     69
128700120328     O          E            STAMPA           26
128800900627     O                                           18 'Destinatario'
128900960123     O                       STPRSD              54
129000120328     O          E            STAMPA           27
129100960123     O                       STPRS2              54
129200120328     O          E            STAMPA           28
129300960123     O                       STPIND              54
129400120328     O          E            STAMPA           29
129500960123     O                       STPCAD              28
129600960123     O                       STPLOD              64
129700960123     O                       STPPRD              67
129800960123     O                       STPNZD              71
129900120328     O          E            STAMPA           32
130000900627     O                       LT(1)             +  5
130100120328     O          E            STAMPA           33
130200900627     O                       LT(2)             +  5
130300960618     O                       DATDCG              24 '  /  /    '
130400960618     O                       RIENTR              49
130500120328     O          E            STAMPA           35
130600900627     O                       §2CDES              66
130700120328     O          E         N40STAMPA           37
130800900627     O                       LT(3)             +  5
130900120328     O          E         N40STAMPA           38
131000900627     O                       LT(4)             +  5
131100120328     O          E         N40STAMPA           40
131200900627     O                       LT(5)             +  5
131300941019     O                       DATDSD              27 '  /  /    '
131400941019     O                       DATDSA              41 '  /  /    '
131500941019     O                       GCPGGS        Z     49
131600990706     O                       STPDIV              57
131700941019     O                       GCPSGS        2     75
131800120328     O          E         N40STAMPA           42
131900900627     O                       LT(6)             +  5
132000990706     O                       STPDIV              57
132100900627     O                       GCPSGR        2     75
132200120328     O          E         N40STAMPA           44
132300900627     O                       LT(7)             +  5
132400990706     O                       STPDIV              57
132500900627     O                       GCPSGD        2     75
132600120328     O          E         N40STAMPA           48
132700911203     O                       LT(17)            +  5
132800990706     O                       STPDIV              57
132900911125     O                       GCPSGA        2     75
133000120328     O          E         N40STAMPA           49
133100911125     O                       LT(9)             +  5
133200961001     O          E         N40STAMPA           49
133300900627     O                       LT(10)            +  5
133400990706     O                       STPDIV              57
133500900627     O                       TOTGEN        2     75
133600120328     O          E         N40STAMPA           53
133700900627     O                       LT(11)            +  5
133800120328     O          E         N40STAMPA           55
133900040123     O                       LT(12)            +  5
134000120328     O          E    41   N40STAMPA           56
134100040123     O                       LT(16)            +  5
134200040123     O                       GCPlpW        Z     15
134300040123     O                       GCPNSW        Z     18
134400040123     O                       GCPNBW        Z     26
134500040123     O                       DATDBG              41 '  /  /    '
134600040123     O                       §NFT          Z     61
134700040123     O                       DATDFT              75 '  /  /    '
134800120328     O          E   n41   N40STAMPA           56
134900040123     O                       LT(13)            +  5
135000040123     O*MANCA L'ANNO
135100040123     O                       GCPlpW        Z     15
135200040123     O                       GCPNSW        Z     18
135300040123     O                       GCPNBW        Z     26
135400040123     O                       DATDBG              41 '  /  /    '
135500120328     O          E         N40STAMPA           58
135600900627     O                       LT(14)            +  5
135700120328     O          E       42 40STAMPA           39
135800920120     O                       LT(14)            +  5
135900120328     O          E      N42 40STAMPA           39
136000900627     O                       LT(15)            +  5
136100900524     O          E            STAMPA      2
136200110506     O                       DITRGS              57
136300900524     O          E            STAMPA      1
136400071018     O******                                     61 'Punto Operativo di'
136500071018     O                                           47 'Filiale di'
136600071018     O******                 ORGDES              82
136700071018     O                       ORGDES              68
136800041110      ** invito a trasmettere via e-mail
136900041110     O          E    17      STAMPA      2
137000041110     O                       lin75               78
137100041110     O          E    17      STAMPA      1
137200041110     O                       out70               76
137300041110     O          E    17      STAMPA      1
137400041110     O                       out71               76
137500041110     O          E    17      STAMPA      1
137600041110     O                       out72               76
137700041110     O          E    17      STAMPA      1
137800041110     O                       lin75               78
137900041029      **---------------------------------------------------------
138000041029      ** x E-mail
138100041029     O          E    14      E_MAIL           01
138200041029     O                                           50 'FAX:'
138300041029     O                       STPFAX              67
138400041029     O          E    14      E_MAIL           02
138500041029     O                                           75 'PAG.:'
138600041029     O                       STPPAG        Z     80
138700041029     O          E    14      E_MAIL           04
138800071018     O*******                                  +  6 'Punto Operativo di'
138900071018     O                                         +  6 'Filiale di'
139000041029     O          E    14      E_MAIL           05
139100041029     O                       DESLNP            +  6
139200041029     O          E   N14      E_MAIL           06
139300071018     O*******                                  +  6 'Punto Operativo di'
139400071018     O                                         +  6 'Filiale di'
139500041029     O                                           75 'PAG.:'
139600041029     O                       STPPAG        Z     80
139700041029     O          E    14      E_MAIL           06
139800041029     O               13      INDLNP            +  6
139900041029     O          E   N14      E_MAIL           07
140000041029     O                       DESLNP            +  6
140100041029     O                                           53 'Spettabile'
140200041029     O               80                           3 'R'
140300041029     O          E    14      E_MAIL           07
140400041029     O               13      CAPLNP            +  6
140500041029     O               13      LOCLNP            +  1
140600041029     O               13      PROLNP            +  1
140700041029     O                                           53 'Spettabile'
140800041029     O               80                           3 'R'
140900041029     O          E   N14      E_MAIL           08
141000041029     O               13      INDLNP            +  6
141100041029     O          E   N14      E_MAIL           09
141200041029     O               13      CAPLNP            +  6
141300041029     O               13      LOCLNP            +  1
141400041029     O               13      PROLNP            +  1
141500041029     O                       RAG                 81
141600041029     O          E    14      E_MAIL           09
141700041029     O                                         +  6 'FAX'
141800041029     O                       FAXLNP            +  1
141900041029     O                       RAG                 81
142000041029     O          E   N14      E_MAIL           10
142100041029     O                                         +  6 'FAX'
142200041029     O                       FAXLNP            +  1
142300041029     O                       VIA                 81
142400041029     O          E    14      E_MAIL           10
142500041029     O                                         +  6 'TEL'
142600041029     O                       TELLNP            +  1
142700041029     O                       VIA                 81
142800041029     O          E            E_MAIL           11
142900041029     O              N14                        +  6 'TEL'
143000041029     O              N14      TELLNP            +  1
143100041029     O                       CAP9                52
143200041029     O                       CIT                 74
143300041029     O                       PRV                 77
143400041029     O                       STA                 81
143500041029     O          E            E_MAIL           12
143600041029     O                       LOCDAT              38
143700041029     O               04                          53 'Att.ne di '
143800041029     O               04      ATTPER              81
143900041029     O          E            E_MAIL           16
144000041029     O                                           14 'OGGETTO:'
144100041029     O                                           26 'Giacenza n.'
144200041029     O                       GCPFGC        Z     30
144300041029     O                       GCPNGC        Z     38
144400041029     O                                           42 'del'
144500041029     O                       DATGIA              53 '  /  /    '
144600041029     O          E            E_MAIL           18
144700041029     O                                           19 'Spedizione n.'
144800041029     O                       GCPLNP        Z     23
144900041029     O                       GCPNRS        Z     26
145000041029     O                       GCPNSP        Z     34
145100041029     O                                           38 'del'
145200041029     O                       DATSPE              49 '  /  /    '
145300041029     O                                           60 'Vs. rifer.'
145400041029     O                       BLPRMN        Z     69
145500041029     O          E            E_MAIL           20
145600041029     O                                           18 'Destinatario'
145700041029     O                       STPRSD              54
145800041029     O          E            E_MAIL           21
145900041029     O                       STPRS2              54
146000041029     O          E            E_MAIL           22
146100041029     O                       STPIND              54
146200041029     O          E            E_MAIL           23
146300041029     O                       STPCAD              28
146400041029     O                       STPLOD              64
146500041029     O                       STPPRD              67
146600041029     O                       STPNZD              71
146700041029     O          E            E_MAIL           26
146800041029     O                       LT(1)             +  5
146900041029     O          E            E_MAIL           27
147000041029     O                       LT(2)             +  5
147100041029     O                       DATDCG              24 '  /  /    '
147200041029     O                       RIENTR              49
147300041029     O          E            E_MAIL           29
147400041029     O                       §2CDES              66
147500041029     O          E         N40E_MAIL           31
147600041029     O                       LT(3)             +  5
147700041029     O          E         N40E_MAIL           32
147800041029     O                       LT(4)             +  5
147900041029     O          E         N40E_MAIL           34
148000041029     O                       LT(5)             +  5
148100041029     O                       DATDSD              27 '  /  /    '
148200041029     O                       DATDSA              41 '  /  /    '
148300041029     O                       GCPGGS        Z     49
148400041029     O                       STPDIV              57
148500041029     O                       GCPSGS        2     75
148600041029     O          E         N40E_MAIL           36
148700041029     O                       LT(6)             +  5
148800041029     O                       STPDIV              57
148900041029     O                       GCPSGR        2     75
149000041029     O          E         N40E_MAIL           38
149100041029     O                       LT(7)             +  5
149200041029     O                       STPDIV              57
149300041029     O                       GCPSGD        2     75
149400041029     O          E         N40E_MAIL           42
149500041029     O                       LT(17)            +  5
149600041029     O                       STPDIV              57
149700041029     O                       GCPSGA        2     75
149800041029     O          E         N40E_MAIL           43
149900041029     O                       LT(9)             +  5
150000041029     O          E         N40E_MAIL           45
150100041029     O                       LT(10)            +  5
150200041029     O                       STPDIV              57
150300041029     O                       TOTGEN        2     75
150400041029     O          E         N40E_MAIL           47
150500041029     O                       LT(11)            +  5
150600041029     O          E         N40E_MAIL           49
150700041029     O                       LT(12)            +  5
150800041029     O          E    41   N40E_MAIL           50
150900041029     O                       LT(16)            +  5
151000041029     O                       GCPlpW        Z     15
151100041029     O                       GCPNSW        Z     18
151200041029     O                       GCPNBW        Z     26
151300041029     O                       DATDBG              41 '  /  /    '
151400041029     O                       §NFT          Z     61
151500041029     O                       DATDFT              75 '  /  /    '
151600041029     O          E   n41   N40E_MAIL           50
151700041029     O                       LT(13)            +  5
151800041029     O*MANCA L'ANNO
151900041029     O                       GCPlpW        Z     15
152000041029     O                       GCPNSW        Z     18
152100041029     O                       GCPNBW        Z     26
152200041029     O                       DATDBG              41 '  /  /    '
152300041029     O          E         N40E_MAIL           52
152400041029     O                       LT(14)            +  5
152500041029     O          E       42 40E_MAIL           33
152600041029     O                       LT(14)            +  5
152700041029     O          E      N42 40E_MAIL           33
152800041029     O                       LT(15)            +  5
152900041029     O          E            E_MAIL      2
153000110506     O                       DITRGS              58
153100041029     O          E            E_MAIL      1
153200071018     O*******                                    61 'Punto Operativo di'
153300071018     O                                           47 'Filiale di'
153400071018     O*******                ORGDES              82
153500071018     O                       ORGDES              68
153600950316**   LT
153700960619      Vi comunichiamo che la giacenza di cui all'oggetto si e' risolta     1
153800960618 in data            per                                                    2
153900960618      Vi anticipiamo con la presente il dettaglio degli oneri relativi     3
154000990628 a quanto sopra, come da accordi gia' esistenti.                           4
154100990628 SOSTA: dal            al            GG                                    5
154200990628 SPESE DI RICONSEGNA E/O VARIE . . . . . . . .                             6
154300990628 SPESE DOSSIER (Diritto Fisso + Spese Postali)                             7
154400990628 ANTICIPATA. . . . . . . . . . . . . . . . . .                             8
154500920525                                                      ----------------     9
154600990628 PER UN TOTALE DI. . . . . . . . . . . . . . .                             10
154700920525                                                      ----------------     11
154800040123 che Vi addebiteremo, oltre alle eventuali addizionali concordate, con     12
154900040123 sped. xxx/xx/xxxxxxx del            sulla prossima fattura.               13
155000920525 Distinti saluti.                                                          14
155100920525 Nulla addebitandoVi, con l'occasione porgiamo distinti saluti.            15
155200040123 sped. xxx/xx/xxxxxxx del            fattura nr.         del  /  /         16
155300990628 SPESE AGGIUNTIVE . . . . . . . . . . . . . . .                            17
155400041028**         dlt1
155500041028DLTOVR FILE(LETGIC)
