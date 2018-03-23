000100000810     H DECEDIT('0,') DATEDIT(*DMY.)
000200950313      *---------*----------------------------------------------------*
000300000728      * FNLR63R1*                                                    *
000400950313      *---------*                                                    *
000500950313      *         - GESTIONE CHIUSURA DISTINTA ESTERO -                *
000600950315      *           -- DATI DISTINTA + CARICAMENTO --                  *
000700950313      *--------------------------------------------------------------*
000800950315     FFNLR63D   CF   E             WORKSTN
000900950317     F                                     SFILE(LR63S05:NRR5)
001000950315     FFNLR53D   O    E             WORKSTN
001100950315     F                                     SFILE(LR53S02:NRR2)
001200950315     F                                     SFILE(LR53S03:NRR3)
001300020902     FFNCDE01L  UF   E           K DISK    USROPN
001400020902     F                                     COMMIT
001500950315     FFNARB01L  UF   E           K DISK    USROPN
001600950313     F                                     COMMIT
001700990519     FFNANM01L  IF   E           K DISK
001800990519     F                                     INFDS(FNANM1)
001900990923     FFIARI01L  IF   E           K DISK
002000990923     FFIAR601L  IF   E           K DISK
002100051115     FFIAR901L  IF   E           K DISK    USROPN
002200950313     F                                     COMMIT
002300950313     FFNART01L  IF   E           K DISK    USROPN
002400950313     F                                     COMMIT
002500950320     FFNFVV01L  UF A E           K DISK    USROPN
002600950313     F                                     COMMIT
002700070515     FFidst01L  UF A E           K DISK    USROPN
002800070515     F                                     COMMIT
002900021204     FFIAPD01L  IF   E           K DISK
003000050406     Ftigcp21L  IF   E           K DISK
003100950606     FFNLBL02L  IF   E           K DISK
003200961114     FFNLBL01L  IF   E           K DISK
003300961114     F                                     RENAME(FNLBL000:FNLBL001)
003400950315     FTABEL00F  IF   E           K DISK
003500980323      *
003600950315     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
003700980323      *
003800980323     FFNCDS03L  UF   E           K DISK    USROPN
003900980323     F                                     COMMIT
004000980323     F                                     RENAME(FNCDS000:FNCDS003)
004100991012     FTNTBE01L  IF   E           K DISK
004200950313      *----------------------------------------------------------------*
004300950313      *    s c h i e r e                                               *
004400950313      *----------------------------------------------------------------*
004500950314     D G15             S              8  0 DIM(5)                               DATA DISTINTA - 15 G
004600950314     D D3A             S             71    DIM(100)                             SKI TAB. 3A
004700950314     D C3A             S              2    DIM(100)                             SKI CHIAVI 3A
004800990923     D D1A             S             61    DIM(100)                             SKI TAB. 1A
004900990923     D C1A             S              2    DIM(100)                             SKI CHIAVI 1A
005000991012     D D7R             S             48    DIM(100)                             SKI TAB. 7R
005100991012     D C7R             S              2    DIM(100)                             SKI CHIAVI 7R
005200001019     D ERR             S             70    DIM(26) CTDATA PERRCD(1)             ERRORI
005300950315      *----------------------------------------------------------------
005400950315      *  DS
005500950315      *----------------------------------------------------------------
005600950313     D FNLR63        E DS                  EXTNAME(FNLR63DS)
005700950313      *
005800950313     D FNLR53        E DS                  EXTNAME(FNLR53DS)
005900950314     D  NDC                    1     35  0
006000950314     D                                     DIM(5)                               NUM.DISTINTA
006100950314     D  DTD                   36     75  0
006200950314     D                                     DIM(5)                               DATA DISTINTA
006300950314     D  DDC                   76    115  0
006400950314     D                                     DIM(5)                               DATA CHIUSURA
006500950314     D  HRC                  116    135  0
006600950314     D                                     DIM(5)                               ORA CHIUSURA
006700950314     D  FPP                  136    140
006800950314     D                                     DIM(5)                               TIPO USCITA
006900950314     D  PEP                  141    145
007000950314     D                                     DIM(5)                               PICKING S/N
007100950314     D  SET                  146    180  0
007200950314     D                                     DIM(5)                               ORA CHIUSURA
007300030204     D  CAR                  247    251
007400030204     D                                     DIM(5)                               Carico S/N
007500950314      *
007600000222     D Wdpd            s              1
007700020902     D dataiso         s               d
007800000222      *
007900950313     D KPJBA         E DS
008000950313     D  LIBSYS                92    100
008100950313     D  MSGERR               453    502
008200030718     D trul90ds      E DS                  inz
008300950313      *
008400020121     D OG143         E DS
008500070515     D OG146         E DS
008600950315     D DS7R          E DS
008700950315     D DS3A          E DS
008800990923     D DS1A          E DS
008900991012     D DSCV          E DS
009000950313     D DS4G          E DS
009100991012     D DGED          E DS
009200051215     D fnlv80ds      e ds
009300021204     D fnlv24ds      e ds
009400021204     D fnlv55ds      e ds
009500020830     D ddatiute      e ds
009600020828     D azuteds       e ds                  extname(AZUTE00F)
009700020828     D tibs34ds      E DS                  inz
009800961114     D DSLR33        E DS                  EXTNAME(FNLR33DS)
009900991012     D YEURDS        E DS                  EXTNAME(YEURCODS)
010000990519     D FNANM1          DS
010100961114     D  ANMNR1               397    400B 0
010200950315      *
010300950315      *  DS per calcolo data da nr. giorni
010400950315     D WLBGIO          DS
010500950315     D  GIODAT                 1      8  0
010600950315     D  GIOINV                 9     16  0
010700950315     D  GIOTGI                17     21  0
010800950313      *  DS per inverisone data
010900950313     D WLBDA8          DS
011000950313     D  G02DAT                 1      8  0
011100950313     D  G02INV                 9     16  0
011200950313     D  G02ERR                17     17
011300950313     D  G02TGI                18     22  0
011400950314     D                 DS
011500950314     D  V2CHCO                 1      2  0
011600950314     D  V2CMCO                 3      4  0
011700950314     D  V2CHMC                 1      4  0
011800950314     D                 DS
011900950315     D  V2CFPD                 1      3  0
012000950315     D  V2CCPC                 4      7
012100950315     D  WCDCPC                 1      7
012200950315      * X  CHKOBJ FNFTDS0T
012300950315     D                 DS
012400950315     D  WFILC                 43     45  0
012500950315     D  WCMDC                  1     60
012600950315      * X  ALCOBJ FNFTDS0T
012700950315     D                 DS
012800950315     D  WFILA                 37     39  0
012900950315     D  WCMDA                  1     60
013000950315     D                SDS
013100950315     D  NOMPGM                 1     10
013200030318      *----------------------------------------------------------------
013300030318     D  Msg_alert1     c                   Const('Ricevuti più di 999 POD da el-
013400030318     D                                     aborare. <Proseguire -> ')
013500030318     D  Msg_alert2     c                   Const('ed in seguito effettuare una -
013600030318     D                                     ulteriore chiusura.>    ')
013700030318     D  Riesegui       c                   Const('Ci sono altre spediz.da chiud-
013800030318     D                                     ere: rieseguire il pgm')
013900950313      *----------------------------------------------------------------
014000950315      *  Definzione interna record AZORG
014100950315      *----------------------------------------------------------------
014200950315     IAZORG01L  AA
014300950315     I                             P    4    5 0ORGFIL
014400950315     I                                  6    6  ORGFVA
014500950315     I                                  7    7  ORGFAG
014600950315     I                                 14   33  ORGDES
014700000222     I                               3776 3800  ORGDE3
014800070515     i                               3851 3875  ORGDe6
014900950313      *---------------------------------------------------------------*
015000950313      * FLUSSO PRINCIPALE                                             *
015100950313      *---------------------------------------------------------------*
015200950313      *  Operazioni iniziali
015300950313     C     *ENTRY        PLIST
015400950313     C                   PARM                    KPJBA
015500950313     C                   MOVEL     KPJBU         FNLR63
015600020830     c* controllo se il po che mi arriva è gestito da qualcun'altro
015700020830     c                   exsr      srfgs
015800020830     c*
015900020830     C                   Z-ADD     D55tfa        KFGS
016000950322     C                   MOVEL     ' '           WFINE
016100950313      *  Se pgm richiamato per l'ultima volta chiudo in LR
016200950313     C     D63TRI        IFNE      'U'
016300950313      *  Impostazione campi distinta
016400950313     C                   EXSR      INZD02
016500950313      *  Loop gestione videate
016600950313     C     WFINE         DOUEQ     'S'
016700950313     C     WTPVID        CASEQ     '2'           GESD02
016800950313     C                   END
016900950313     C                   END
017000950313     C*
017100950313     C     FINE          TAG
017200950317     C                   MOVEL     FNLR63        KPJBU
017300950313     C                   SETON                                        RT
017400950313     C*
017500950313     C                   ELSE
017600950320     C     WCHIUS        IFEQ      'S'
017700950315     C                   EXSR      CALL53
017800950320     C                   END
017900950320     C     WSBLOC        IFEQ      'S'
018000950320     C                   MOVEL     FNLR63        KPJBU
018100950320     C                   CALL      'FNLR63R3'
018200950320     C                   PARM                    KPJBA
018300950320     C                   END
018400950426     C     WPGMER        IFEQ      'S'
018500950320     C                   MOVEL     FNLR63        KPJBU
018600950320     C                   CALL      'FNLR63R2'
018700950320     C                   PARM                    KPJBA
018800950320     C                   END
018900950320     C*
019000950320     C                   COMMIT
019100950313     C                   SETON                                        LR
019200950313     C                   END
019300950313      *---------------------------------------------------------*
019400950313      *  Inizializzo campi della prima videata                  *
019500950313      *---------------------------------------------------------*
019600950313     C     INZD02        BEGSR
019700950313      *
019800950313     C                   SETOFF                                       404142
019900950313     C                   SETOFF                                       2801
020000020904     C                   Z-ADD     D55tfa        V2CLNA
020100950313     C                   MOVEL     D63PDR        V2CFPD
020200950315     C                   MOVE      D63PDR        WCDCPC
020300950313     C                   Z-ADD     WOGGI         V2CDDC
020400950313     C                   Z-ADD     D63HMC        V2CHMC
020500950313     C                   MOVEL     D63FPP        V2CFPP
020600950313     C                   MOVEL     D63PEP        V2CPEP
020700950315     C                   MOVEL     D63NDT        V2CNDC
020800000426     C*--------------------------------------------------------------*
020900950313     C*  Controllo se ci sono record di FNCDE con il numero distinta
021000020830     C*  sia per il po richiesto che per il po di gestione:
021100000426     C*  in tal caso vuole dire che c'è stata una precedente elaboraz.
021200000426     C*  di chiusura distinte che si è interrotta quindi per poter
021300000426     C*  proseguire devo eseguire lo sblocco. (la chiusura distinta
021400020830     C*  estero prima imposta su FNCDE il nr. distinta poi una
021500000426     C*  volta confermati i dati e aggiornato FNARB cancella i record
021600000426     C*  di FNCDE aventi il nr. distinta = a quello del documento
021700020830     C*  appena confermato. Se viene richiesto di uscire
021800000426     C*  dal giro prima di aver completato la chiusura un apposito
021900000426     C*  pgm provvede a togliere da FNCDE i dati della distinta.)
022000000426     C*--------------------------------------------------------------*
022100020830     C                   Z-ADD     0             KNDC
022200020830     C                   Z-ADD     D63LNA        KLNA
022300950315     C     KCDE          SETGT     FNCDE01L
022400020830     C     klna          READE     FNCDE01L                               31
022500000426      * Se ce ne sono ...
022600950313     C     *IN31         IFEQ      '0'
022700000426      * ... libero CDE e
022800950320     C                   EXCEPT    NOAGG
022900000426      * ... accendo indicatore msg errore che avvisa di eseguire lo sblocco
023000950313     C                   SETON                                            01
023100950313     C                   END
023200950313     C*
023300950313     C                   ENDSR
023400950313      *---------------------------------------------------------*
023500950313      *  Gestione della seconda videata                         *
023600950313      *---------------------------------------------------------*
023700950313     C     GESD02        BEGSR
023800950313      *
023900001019     C*  Controllo numero righe fncde se 9999 mando messaggio
024000001019     C     d63nrr        IFEQ      9999
024100001019     C                   SETON                                        28
024200001019     C                   MOVEL     ERR(26)       $MSG
024300001019     C                   END
024400001019      *
024500950313     C                   EXFMT     LR63D02
024600950320     C                   SETOFF                                       404142
024700950320     C                   SETOFF                                       28
024800950313     C*  Fine Lavoro
024900950313     C     *INKC         IFEQ      '1'
025000950313     C                   MOVEL     '1'           D63F03
025100950313     C                   MOVEL     'S'           WFINE
025200950313     C                   GOTO      FINVD2
025300950313     C                   END
025400950313     C*  Ritorno
025500950313     C     *INKL         IFEQ      '1'
025600950320     C                   MOVEL     'S'           WFINE
025700950320     C                   GOTO      FINVD2
025800950313     C                   END
025900950313     C*  Sblocco
026000950313     C     *INKK         IFEQ      '1'
026100000426      * se richiesto richiamo pgm che va a togliere il nr. distinta
026200000426      * da FNCDE. Prima di richiedere lo sblocco bisogna appurare
026300000426      * che non ci sia un altro utente che sta chiudendo distinte
026400000426      * per la stessa lna
026500950317     C                   MOVEL     FNLR63        KPJBU
026600950313     C                   CALL      'FNLR63R3'
026700950313     C                   PARM                    KPJBA
026800950320     C                   MOVEL     KPJBU         FNLR63
026900950320     C                   MOVEL     'S'           WSBLOC
027000950317     C                   SETOFF                                       01
027100950313     C                   GOTO      FINVD2
027200950313     C                   END
027300950313     C*  Controlli
027400950313     C                   EXSR      CTR02
027500950313     C*
027600950313     C  N01*IN28         IFEQ      '0'
027700950314     C*  Esamino record di FNCDE per impostare nr.distinta
027800950314     C                   EXSR      UPDCDE
027900950314     C*
028000950313     C     WERROR        IFEQ      'S'
028100950313     C*  Se ho trovato degli errori chiamo pgm x la loro gestione
028200950313     C                   EXSR      GESERR
028300950313     C                   END
028400950314     C*  Carico dati per ditinta di consegna
028500950315     C     WFINE         IFNE      'S'
028600950320     C     D63NRR        IFEQ      0
028700950320     C                   MOVEL     'S'           WFINE
028800950320     C                   ELSE
028900950314     C                   EXSR      CARDIS
029000950313     C*  Richiamo pgm FNLR53R
029100961230     C     NRR2          IFNE      0
029200950313     C                   EXSR      CALL53
029300030319      *-
029400030319      * se aveva più di 999 P.O.D. deve uscire e rientrare per chiudere
029500030319      * una nuova distinta con i records residui.
029600030319      * Altrimenti tornerebbe sulla videata della distinta appena elaborata.
029700030319     c                   if        conta_cde = 999
029800030319     C                   movel     'S'           WFINE
029900030319     C                   goto      FINVD2
030000030319     c                   endiF
030100030319      *-
030200961230     C                   ELSE
030300961230     C                   SETON                                        28
030400961230     C                   MOVEL     ERR(23)       $MSG
030500961230     C                   END
030600950320     C                   END
030700950313     C                   END
030800950315     C*
030900950315     C                   END
031000950313     C*
031100950315     C     FINVD2        ENDSR
031200950314     C*-----------------------------------------------------*
031300950314     C*  Controlli su seconda videata
031400950314     C*-----------------------------------------------------*
031500950314     C     CTR02         BEGSR
031600000222      *
031700000222      * Verifico se LNA è DPD
031800021203     C     d63lna        CHAIN     azorg01l
031900020121      *
032000020121      * imposta i campi della DS
032100020121     c                   If        %found
032200020121     C                   movel     ORGDE3        OG143
032300070515     C                   movel     ORGDE6        OG146
032400020121     c                   endif
032500020121      *
032600020717      *  sostituito utilizzando il campo del network
032700020717     c                   EVAl      WDPD = *blank
032800020717     c                   if        §ogNTW = 'DPD'
032900020717     c                   EVAl      WDPD = 'S'
033000020717     c                   end
033100000222      *
033200000222      *   Controllo se richiesta ricerca codice padroncino
033300950314     C                   SETOFF                                           32
033400950315     C     '?'           SCAN      V2CCPC                                 32
033500950314     C   32              DO
033600021204     C                   CLEAR                   FNLV24DS
033700021204     C                   MOVEL     'R'           D24FLG
033800021204     C                   MOVEL     'A'           D24TIP
033900021204     C                   MOVEL     V2Cfpd        D24FIL
034000950315     C                   MOVE      *BLANKS       V2CCPC
034100021204     C                   MOVEL(P)  FNLV24DS      KPJBU
034200950314     C                   CALL      'FNLV24R'
034300950314     C                   PARM                    KPJBA
034400021204     C                   MOVEL     KPJBU         FNLV24DS
034500021204     C     D24PDR        IFNE      *ZEROS
034600021204     C                   MOVEL     D24PDR        WCDCPC
034700950314     C                   END
034800950315     C                   END
034900950314     C* Controllo se esiste nell'anagrafica padroncini
035000950314     C                   TESTN                   WCDCPC               32
035100950317     C     *IN32         IFEQ      '0'
035200950314     C                   SETON                                        2841
035300950314     C                   MOVEL     ERR(1)        $MSG
035400950314     C                   GOTO      FINCT2
035500950314     C                   END
035600950314     C*
035700950314     C                   MOVE      WCDCPC        APDPDR
035800021204     C     KAPD          CHAIN     FIAPD01L                           31
035900950314     C     *IN31         IFEQ      '1'
036000950314     C                   SETON                                        2841
036100950314     C                   MOVEL     ERR(2)        $MSG
036200950314     C                   GOTO      FINCT2
036300950314     C                   END
036400950316     C*
036500950316     C                   MOVE      'N'           D53WRT
036600971104     C     APDPDD        IFNE      'S'
036700971104     C*   e padroncino non è dipedente controllo se in filiale
036800971104     C*  di appartenza è attivata la gestione conteggi
036900971104     C                   MOVEL     APDPDR        ORGFIL
037000971104     C     ORGFIL        CHAIN     AZORG01L                           31
037100971104     C     *IN31         IFEQ      '0'
037200971104     C                   MOVE      'S'           D53WRT
037300971104     C                   END
037400971104     C                   END
037500971104     C*----------------------------------------------------*
037600971104     C*  Data distinta
037700971104     C     V2CDDC        IFEQ      0
037800971104     C                   SETON                                        2840
037900950314     C                   MOVEL     ERR(3)        $MSG
038000950314     C                   GOTO      FINCT2
038100950314     C                   END
038200950314     C*
038300950314     C                   CLEAR                   WLBDA8
038400950314     C                   MOVE      V2CDDC        G02DAT
038500950314     C                   CALL      'XSRDA8'
038600950314     C                   PARM                    WLBDA8
038700950314     C     G02ERR        IFEQ      '1'
038800950314     C                   SETON                                        2840
038900950314     C                   MOVEL     ERR(4)        $MSG
039000950314     C                   GOTO      FINCT2
039100950510     C                   ELSE
039200961114     C                   MOVEL     G02INV        §DDC
039300950510     C                   MOVE      G02DAT        V2CDDC
039400950314     C                   END
039500950314     C*
039600950314     C*  Controllo ora di consegna
039700950314     C     V2CHMC        IFEQ      0
039800950314     C                   SETON                                        4228
039900950314     C                   MOVEL     ERR(5)        $MSG
040000950314     C                   GOTO      FINCT2
040100950314     C                   END
040200950314     C*
040300950314     C     V2CHCO        IFGT      21
040400950314     C     V2CHCO        ORLT      7
040500950314     C     V2CHCO        ANDGT     0
040600950314     C     V2CMCO        ORNE      0
040700950314     C     V2CMCO        ANDGT     59
040800950314     C                   SETON                                        4228
040900950314     C                   MOVEL     ERR(6)        $MSG
041000950314     C                   END
041100950314     C*
041200950314     C     FINCT2        ENDSR
041300961114     C*--------------------------------------------------------------*
041400961114     C*  Sblocco spedizione se possibile
041500961114     C*--------------------------------------------------------------*
041600961114     C     SBLOCC        BEGSR
041700961114     C*
041800961114     C     ARBFBC        IFEQ      'A'
041900961114     C     ARBFBC        OREQ      'B'
042000961114     C                   MOVEL     *BLANKS       ARBFBC
042100961114     C                   MOVEL     *BLANKS       ARBCMC
042200961114     C                   ELSE
042300961114     C     ARBFBC        IFEQ      'G'
042400961114     C*  Controllo se la bolla ha una mamma
042500961114     C     KARB          CHAIN     FNLBL01L                           33
042600961114     C     *IN33         IFEQ      '0'
042700970521     C     LBLLAP        ANDEQ     LBLLAN
042800961114     C                   Z-ADD     LBLAAP        KAAS1
042900961114     C                   Z-ADD     LBLLPP        KLNP1
043000961114     C                   Z-ADD     LBLNRP        KNRS1
043100961114     C                   Z-ADD     LBLNSP        KNSP1
043200961114     C                   MOVEL     'S'           WFIGL
043300961114     C                   ELSE
043400961114     C                   Z-ADD     CDEAAS        KAAS1
043500961114     C                   Z-ADD     CDELNP        KLNP1
043600961114     C                   Z-ADD     CDENRS        KNRS1
043700961114     C                   Z-ADD     CDENSP        KNSP1
043800961114     C                   MOVEL     'N'           WFIGL             1
043900961114     C                   END
044000961114     C*  Verifico se posso sbloccare la spedizione
044100961114     C     WFIGL         IFEQ      'S'
044200050406     C     KGCA          CHAIN     tigcp21L                           35
044300961114     C     *IN35         IFEQ      '0'
044400050406     C     GCPFAS        ANDNE     40
044500961114     C                   GOTO      FINSBL
044600961114     C                   END
044700961114     C                   ELSE
044800050406     C     KGCA          CHAIN     tigcp21L                           35
044900961114     C     *IN35         IFEQ      '0'
045000961114     C     GCPFAS        ANDLT     35
045100060403      * adesso anche x fase 36
045200060403      *  ossia x disposizioni immesse in Partenza
045300060403     C     *IN35         oreq      '0'
045400060403     C     GCPFAS        ANDeq     36
045500961114     C                   GOTO      FINSBL
045600961114     C                   END
045700961114     C                   END
045800961114     C                   MOVEL     *BLANKS       ARBFBC
045900961114     C                   MOVEL     *BLANKS       ARBCMC
046000961114     C                   END
046100961114     C*
046200961114     C                   END
046300961114     C*
046400961114     C*  Se ho trovato i segnacolli vado a chiudere le anomalie
046500961114     C                   CLEAR                   DSLR33
046600961114     C                   MOVE      '4'           D33FSC
046700961114     C                   Z-ADD     §DDC          D33DCH
046800961114     C                   MOVEL     'DI'          D33CCH
046900961114     C*
047000961114     C                   Z-ADD     140           KCAA
047100990519     C     KANM          CHAIN     FNANM01L                           37
047200961114     C     *IN37         DOWEQ     '0'
047300961114     C                   Z-ADD     ANMNR1        D33NRR
047400961114     C                   CALL      'FNLR33R'
047500961114     C                   PARM                    DSLR33
047600990519     C     KANM          READE     FNANM01L                               37
047700961114     C                   END
047800961114     C*
047900961114     C     FINSBL        ENDSR
048000950314     C*-----------------------------------------------------*
048100950314     C*  Aggiorno archivio FNCDE con il numero distinta
048200950314     C*-----------------------------------------------------*
048300950314     C     UPDCDE        BEGSR
048400980323      *
048500950320     C                   Z-ADD     0             D63NRR
048600030318     C                   Z-ADD     0             conta_cde         5 0
048700950322     C                   Z-ADD     0             KNDC
048800980324     C                   Z-ADD     *ZEROS        WDSE              1 0
048900980324     C                   MOVEL     'N'           WERROR            1
049000950317     C     KCDE          CHAIN     FNCDE01L                           31
049100030318      *
049200030318      * Al massimo il programma può gestire 999 records (STOP) per distinta
049300030318      * quindi occorre eseguire più volte il pgm creando più distinte
049400030318      * quando sono presenti più di 999 spedizioni da chiudere.
049500950317     C     *IN31         DOWEQ     '0'
049600030318     c     conta_cde     andlt     999
049700030318      *
049800030318      * Il SFL può contenere al massimo 999 STOP,quindi,occorre fare
049900030318      *  più volte la distinta estero bloccando i records a gruppi
050000030318      *   di 999 per volta.
050100030318     c                   add       1             conta_cde
050200030318      *
050300961220     C                   MOVEL     'S'           WERRBL
050400961122     C                   Z-ADD     0             WNRERR            5 0
050500950315     C                   Z-ADD     CDEAAS        KAAS
050600950315     C                   Z-ADD     CDELNP        KLNP
050700950315     C                   Z-ADD     CDENRS        KNRS
050800950315     C                   Z-ADD     CDENSP        KNSP
050900980323      *  Bolla non trovata su FNARB
051000950314     C     KARB          CHAIN     FNARB01L                           32
051100950314     C     *IN32         IFEQ      '1'
051200980323     C                   Z-ADD     7             I
051300950314     C                   EXSR      WRTERR
051400950314     C                   GOTO      AGGREC
051500950314     C                   END
051600980323      *  Linea di arrivo archivio P.C. diversa da bolle arrivi
051700950315     C     ARBLNA        IFNE      CDELNA
051800980323     C                   Z-ADD     8             I
051900950314     C                   EXSR      WRTERR
052000950314     C                   GOTO      AGGREC
052100950314     C                   END
052200000222      *  Rif.numerico mittente diverso da bolle arrivi  NO x DPD
052300000222     C                   if        ARBRMN <> CDERMN and Wdpd <> 'S'
052400980323     C                   Z-ADD     9             I
052500950314     C                   EXSR      WRTERR
052600950314     C                   GOTO      AGGREC
052700950314     C                   END
052800980323      *  Spedizione già consegnata
052900950314     C     ARBDCM        IFNE      0
053000980323     C                   Z-ADD     11            I
053100950314     C                   EXSR      WRTERR
053200950314     C                   GOTO      AGGREC
053300950314     C                   END
053400980323      *  Spedizione in consegna in altra distinta
053500950314     C     ARBNDC        IFNE      0
053600020829     C     ARBNDC        ifne      CDENDC
053700980323     C                   Z-ADD     10            I
053800950314     C                   EXSR      WRTERR
053900950314     C                   GOTO      AGGREC
054000950314     C                   END
054100020829     C                   END
054200980323      *  Spedizione CON FIGLIA
054300950606     C     KARB          CHAIN     FNLBL02L                           33
054400950606     C     *IN33         IFEQ      '0'
054500980323     C                   Z-ADD     18            I
054600950606     C                   EXSR      WRTERR
054700950606     C                   GOTO      AGGREC
054800950606     C                   END
054900980323      *  Spedizione bloccata/giacente/lasc.avviso
055000950322     C     ARBFBC        IFNE      *BLANKS
055100000426      *  Verifico se è possibile sbloccare in automatico la sped.
055200961114     C                   EXSR      SBLOCC
055300000426      *  ... se ci riesco la sblocco, altrimenti se dopo l'esecuzione
055400000426      *  della subroutine la spedizione è ancora bloccata do errore
055500961114     C     ARBFBC        IFNE      *BLANKS
055600980323     C                   Z-ADD     16            I
055700950322     C                   EXSR      WRTERR
055800950322     C                   GOTO      AGGREC
055900950322     C                   END
056000961114     C                   END
056100980323      *  Spedizione non abilitata all'incasso del C/Ass.
056200950322     C                   Z-ADD     1             X
056300950322     C                   CLEAR                   DS3A
056400950322     C     ARBCBO        LOOKUP    C3A(X)                                 32
056500950322     C     *IN32         IFEQ      '1'
056600950322     C                   MOVEL     D3A(X)        DS3A
056700950322     C                   END
056800950321     C     ARBACC        IFNE      'S'
056900950322     C     §3AFCA        ANDEQ     0
057000950322     C     ARBICC        ANDEQ     ' '
057100980323     C                   Z-ADD     14            I
057200950321     C                   EXSR      WRTERR
057300950321     C                   GOTO      AGGREC
057400950321     C                   END
057500980323      *  Spedizione non abilitata all'incasso del Assegnato
057600950322     C                   MOVEL     §3ATB1        WPORTO            1
057700950322     C     ARBACA        IFNE      'S'
057800950322     C     WPORTO        IFEQ      'A'
057900950322     C     §3ATB2        ORNE      *BLANKS
058000980323     C                   Z-ADD     15            I
058100950321     C                   EXSR      WRTERR
058200961118     C                   GOTO      AGGREC
058300950321     C                   END
058400950322     C                   END
058500980323      *  Errore bloccante
058600961129     C     CDEFL1        IFEQ      'B'
058700980323     C                   Z-ADD     22            I
058800961129     C                   EXSR      WRTERR
058900961129     C                   GOTO      AGGREC
059000961129     C                   END
059100980323      *  Se c'è la data di consegna e quella di giacenza
059200961127     C     CDEDCM        IFNE      0
059300961127     C     CDEDAG        ANDNE     0
059400050406     C     KARB          CHAIN     tigcp21L                           33
059500961127     C                   MOVEL     'N'           WERRBL
059600980323      *  Se esiste giacenza con fase 35 prevale consegna
059700961127     C     *IN33         IFEQ      '0'
059800961127     C     GCPFAS        ANDEQ     35
059900961127     C                   Z-ADD     0             CDEDAG
060000961127     C                   ELSE
060100980323     C                   Z-ADD     19            I
060200961127     C                   EXSR      WRTERR
060300961127     C                   END
060400961127     C                   END
060500980323      *  Se c'è la data di consegna e (giacenza o lasciato avviso)
060600980323      *  oppure lasc.avviso e giacenza decide l'utente
060700961127     C     CDEDCM        IFNE      0
060800961127     C     CDEDLA        ANDNE     0
060900961127     C     CDEDLA        ORNE      0
061000961127     C     CDEDAG        ANDNE     0
061100970107     C                   MOVEL     'N'           WERRBL
061200980323     C                   Z-ADD     19            I
061300961127     C                   EXSR      WRTERR
061400961127     C                   END
061500980323      *  Motivo di consegna non gestibile
061600961118     C     CDEERR        IFEQ      'S'
061700980323     C                   Z-ADD     20            I
061800961118     C                   MOVEL     'N'           WERRBL
061900961118     C                   EXSR      WRTERR
062000961118     C                   END
062100000925      *contrassegno non annullato (distinta automatica) opzione 2 poste
062200000925     C     CDEERR        IFEQ      'C'
062300000925     C                   Z-ADD     25            I
062400000925     C                   MOVEL     'N'           WERRBL
062500000925     C                   EXSR      WRTERR
062600000925     C                   END
062700980324      *---------------------------------------
062800980324      *  * Se date a zero e non esiste dettaglio segnacolli
062900980402      *     - scrivo messaggio di impostare almeno una data.
063000980324      *  * Se date a zero ed esiste dettaglio segnacolli
063100980402      *     - scrivo messaggio di analizzare i segnacolli  V5CDSE = 2
063200980402      *-------------------------------------------------------------------------
063300980402      *  - scrivo msg di analizzare i segnacolli se eventi <>  V5CDSE = 2
063400980402      *  - non scrivo msg se segnacolli di eventi = (codice,data,ora) V5CDSE = 1
063500980402      *     SE RIPRISTINATA IMPLEMENTARE SCRIVENDO DATE E MSG DI FNCDS
063600980402      *-------------------------------------------------------------------------
063700980402      *
063800961120     C     CDEDCM        IFEQ      0
063900961120     C     CDEDLA        ANDEQ     0
064000961120     C     CDEDAG        ANDEQ     0
064100961120     C     CDEDRC        ANDEQ     0
064200980323      *
064300980323     C                   Z-ADD     CDEAAS        KAAS
064400980323     C                   Z-ADD     CDELNP        KLNP
064500980323     C                   Z-ADD     CDENRS        KNRS
064600980323     C                   Z-ADD     CDENSP        KNSP
064700980324     C     KARB          CHAIN     FNCDS003                           33
064800980324      *
064900980324     C     *IN33         IFEQ      '1'
065000980324     C                   Z-ADD     12            I
065100980324     C                   ELSE
065200980402      *
065300980402     C                   Z-ADD     24            I                              se ripristini
065400980402     C                   Z-ADD     2             WDSE                           specifiche *?*
065500980402     C                   END                                                    elimina queste
065600980324      *
065700970107     C                   MOVEL     'N'           WERRBL
065800961120     C                   EXSR      WRTERR
065900961120     C                   END
066000980324      *---------------------------------------
066100000210      *  Impossibile modificare codice cons.particolare ** FORZABILE **
066200000210      *   scrivo la consegna particolare nel msg e la levo dal sbf
066300000210     C                   SELECT
066400000210     C                   WHEN      CDEtc1 = *blanks
066500000210     C                   WHEN      ARBtc1 = CDEtc1  or  ARBtc2 = CDEtc1
066600000210     C                   WHEN      ARBtc1 = *blanks  or  ARBtc2 = *blanks
066700000210     C                   OTHER
066800000210     C                   movel     'N'           WERRBL
066900000210     C                   z-add     17            I
067000000210     C                   exsr      WRTERR
067100000210     C                   ENDSL
067200980406      *
067300980406      *  Se 2° evento mancanza scrivo messaggio
067400961120     C     CDEFL1        IFEQ      'S'
067500980323     C                   Z-ADD     22            I                 4 0
067600961120     C                   MOVEL     'N'           WERRBL            1
067700961120     C                   EXSR      WRTERR
067800961120     C                   END
067900980323      *
068000980323      *  Aggiorno record su FNARB
068100950320     C                   ADD       1             D63NRR
068200020830     C                   z-add     d55tfa        arbifp
068300020829     C                   Z-ADD     V2CNDC        ARBNDC
068400950314     C                   EXCEPT    AGGARB
068500980323      *
068600961120     C     AGGREC        TAG
068700980323      *
068800980323      *  Aggiorno FNCDE
068900950315     C                   Z-ADD     V2CNDC        CDENDC
069000950315     C                   EXCEPT    AGGCDE
069100980323      *
069200980323      *  Aggiorno FNCDS se esiste
069300980323     C                   Z-ADD     CDEAAS        KAAS
069400980323     C                   Z-ADD     CDELNP        KLNP
069500980323     C                   Z-ADD     CDENRS        KNRS
069600980323     C                   Z-ADD     CDENSP        KNSP
069700980323     C     KARB          SETLL     FNCDS003                               33
069800980323     C                   DO        *HIVAL
069900980323     C     KARB          READE     FNCDS003                               33
070000980323     C  N33              Z-ADD     V2CNDC        CDSNDC
070100980323     C  N33              EXCEPT    AGGCDS
070200980323     C  N33              END
070300980323      *
070400980323      *  Leggo prossimo record di FNCDE con numero distinta a 0
070500950317     C     KCDE          READE     FNCDE01L                               31
070600950315     C                   END
070700950314     C*
070800950317     C                   COMMIT
070900950317     C*
071000030318     C*  Se presenti più di 999 spedizioni manda un msg di Avviso
071100030318     C*  che non ha potuto elaborare tutto sotto la stessa distinta
071200030318     C*  e quindi occorre rieseguire il programma.
071300030318     c                   if        conta_cde = 999
071400030318     C*    INVIO MSG
071500030318     C                   movel     msg_alert1    WINMSG
071600030318     c                   exfmt     LR63W08
071700030318     C                   clear                   WINMSG
071800030318     C                   movel     msg_alert2    WINMSG
071900030318     c                   exfmt     LR63W08
072000030318     C                   clear                   WINMSG
072100030318     c                   endiF
072200030318     C*
072300950314     C                   ENDSR
072400950314     C*-----------------------------------------------------*
072500000426     C*  SCRITTURA ERRORI: la rountine gestisce la scrittura
072600000426     C*  di un subfile nascosto. Il programma di gestione
072700000426     C*  errori carica il sfl per la loro visualizzazione
072800000426     C*  da quest'ulitmo. L'utente esegue le operazioni per
072900000426     C*  eliminarli. Queste rimangono sospese fino a che non
073000000426     C*  viene data la conferma. Il programma procederà a
073100000426     C*  rileggere il sfl precedentemente aggiornato in   .
073200000426     C*  base alle scelte effettuate, a modificare altri
073300000426     C*  eventuali file.
073400000426     C*  Si caricherà poi gli eventuali errori rimasti a
073500000426     C*  video.
073600950314     C*-----------------------------------------------------*
073700950314     C     WRTERR        BEGSR
073800950314     C*
073900950314     C*  Se è la prima volta azzero subfile degli errori
074000950314     C     WERROR        IFEQ      'N'
074100980323     C                   Z-ADD     0             NRR5              4 0
074200950314     C                   MOVEL     'S'           WERROR
074300950315     C                   SETOFF                                       2021
074400950317     C                   WRITE     LR63C05
074500950314     C                   SETON                                        2021
074600950314     C                   END
074700961122     C*
074800961122     C*  Aggiorno numeratore errori
074900961122     C                   ADD       1             WNRERR
075000950314     C*  Se la data di consegna non c'è ma esiste la data
075100950314     C*  di riserva valorizzo la prima con la seconda
075200950315     C     CDEDRC        IFNE      0
075300950315     C     CDEDCM        ANDEQ     0
075400950315     C                   Z-ADD     CDEDRC        CDEDCM
075500950314     C                   END
075600950314     C*
075700950317     C                   ADD       1             NRR5
075800961122     C     WNRERR        IFGT      1
075900961122     C                   MOVEL     '1'           V5CIN3
076000961122     C                   ELSE
076100961122     C                   MOVEL     '0'           V5CIN3
076200961122     C                   END
076300961122     C                   MOVEL     *BLANKS       V5CSCE
076400950606     C                   MOVEL     *BLANKS       V5CTPD
076500980324     C                   MOVEL     WDSE          V5CDSE
076600000426     C                   MOVEL     WERRBL        V5CBLO                         errore bloc.
076700950317     C                   Z-ADD     CDEAAS        V5CAAS
076800950317     C                   Z-ADD     CDELNP        V5CLNP
076900950317     C                   Z-ADD     CDENRS        V5CNRS
077000950317     C                   Z-ADD     CDENSP        V5CNSP
077100950317     C                   Z-ADD     CDENSP        V5CNSP
077200950320     C                   Z-ADD     CDERMN        V5CRMN
077300961118     C                   MOVEL     CDECCA        V5CCCA
077400000210     C                   MOVEL     CDETC1        V5CTC1
077500961118     C                   MOVEL     CDESTS        V5CSTS
077600961120     C                   MOVEL     CDECOD        V5CCOD
077700961120     C                   MOVEL     CDEMOT        V5DCOD
077800961120     C                   MOVEL     CDEFTX        V5CFTX
077900961120     C                   MOVEL     CDEFL1        V5CFL1
078000950320     C                   CLEAR                   WLBDA8
078100950320     C                   Z-ADD     CDEDCM        G02INV
078200950320     C                   MOVEL     '3'           G02ERR
078300950320     C                   CALL      'XSRDA8'
078400950320     C                   PARM                    WLBDA8
078500950320     C                   Z-ADD     G02DAT        V5CDCM
078600950320     C                   Z-ADD     CDEDRC        G02INV
078700950320     C                   MOVEL     '3'           G02ERR
078800950320     C                   CALL      'XSRDA8'
078900950320     C                   PARM                    WLBDA8
079000950320     C                   Z-ADD     G02DAT        V5CDRC
079100950320     C                   Z-ADD     CDEDLA        G02INV
079200950320     C                   MOVEL     '3'           G02ERR
079300950320     C                   CALL      'XSRDA8'
079400950320     C                   PARM                    WLBDA8
079500950320     C                   Z-ADD     G02DAT        V5CDLA
079600950320     C                   Z-ADD     CDEDAG        G02INV
079700950320     C                   MOVEL     '3'           G02ERR
079800950320     C                   CALL      'XSRDA8'
079900950320     C                   PARM                    WLBDA8
080000950320     C                   Z-ADD     G02DAT        V5CGIA
080100000210      * MESSAGGIO DI ERRORE
080200000210     C                   SELECT
080300000210     C                   WHEN      I = 17
080400000210     C                   movel     ERR(I)        V5CERR
080500000210     c                   eval      %subst(V5Cerr:38:1) = CDEtc1
080600000210     C                   clear                   V5Ctc1
080700000210     C                   WHEN      I = 20
080800000210     C                   movel(P)  CDEMOT        V5CERR
080900000210     C                   OTHER
081000000210     C                   movel     ERR(I)        V5CERR
081100000210     C                   ENDSL
081200000426      * write riga sfl errori
081300950317     C                   WRITE     LR63S05
081400000210      *
081500950314     C                   ENDSR
081600950315     C*-----------------------------------------------------*
081700950315     C*  Gestione degli errori
081800950315     C*-----------------------------------------------------*
081900950315     C     GESERR        BEGSR
082000950315     C*
082100950315     C*  Richiamo pgm subfile gestione errori
082200950315     C                   MOVEL     FNLR63        KPJBU
082300950315     C                   CALL      'FNLR63R2'
082400950315     C                   PARM                    KPJBA
082500950320     C                   MOVEL     KPJBU         FNLR63
082600950426     C                   MOVEL     'S'           WPGMER
082700980323      *  Se non ho corretto tutti gli errori e sono uscita con
082800980323      *  F3 richiamo il programma per sbloccare le distinte
082900950315     C     D63F03        IFEQ      '1'
083000950315     C                   MOVEL     FNLR63        KPJBU
083100950315     C                   CALL      'FNLR63R3'
083200950315     C                   PARM                    KPJBA
083300950320     C                   MOVEL     KPJBU         FNLR63
083400950315     C                   MOVEL     'S'           WFINE
083500950320     C                   MOVEL     'S'           WSBLOC
083600950315     C                   END
083700980323      *
083800950315     C                   ENDSR
083900950314      *---------------------------------------------------------*
084000950314      *  Caricamento singole distinte                           *
084100950314      *---------------------------------------------------------*
084200950314     C     CARDIS        BEGSR
084300950314     C*
084400950315     C*  Impostazione dati in schiere
084500950315     C                   EXSR      INZSCH
084600950314     C*
084700950317     C                   Z-ADD     D63NDT        KNDC
084800950315     C     KCDE          CHAIN     FNCDE01L                           31
084900950320     C*  Aggiorno FNARB + archivio dei fogli viaggio
085000950320     C     *IN31         IFEQ      '0'
085100950320     C                   EXSR      UPDDIS
085200950320     C*
085300950314     C     *IN31         DOWEQ     '0'
085400950315     C*  Scrivo record di consegna e incassi
085500950315     C                   EXSR      WRTSBF
085600950314     C*
085700950320     C     KCDE          READE     FNCDE01L                               31
085800950314     C                   END
085900950320     C                   END
086000950317     C*
086100950317     C                   COMMIT
086200950315     C*
086300950315     C                   ENDSR
086400950315      *---------------------------------------------------------*
086500950315      *  Richiamo pgm. FNLR53R                                  *
086600950315      *---------------------------------------------------------*
086700950315     C     CALL53        BEGSR
086800950315     C*
086900950315     C                   MOVEL     D63TRI        D53TRI
087000950315     C                   Z-ADD     D63LNA        D53LNA
087100950315     C                   MOVEL     'E'           D53ITA
087200950315     C                   MOVEL     APDRSC        D53DSF
087300050516     C                   move      APDpdr        D53dsf
087400950530     C                   MOVEL     APDFCM        D53CMG
087500950315     C                   MOVEL     ' '           D53F03
087600950315     C                   MOVEL     'N'           D53WRT
087700950315     C                   MOVEL     FNLR53        KPJBU
087800950315     C                   CALL      'FNLR53R'
087900950315     C                   PARM                    KPJBA
088000030718     C                   PARM                    trul90ds
088100950315     C                   MOVEL     KPJBU         FNLR53
088200950320     C                   MOVEL     'S'           WCHIUS
088300950315     C*
088400020830     C*  Se non ho confermato la distinta richiamo pgm x sblocco
088500950323     C     D53F03        IFNE      ' '
088600950321     C                   MOVEL     FNLR63        KPJBU
088700950315     C                   CALL      'FNLR63R3'
088800950315     C                   PARM                    KPJBA
088900950320     C                   MOVEL     'S'           WSBLOC
089000950323     C     D53F03        IFEQ      '2'
089100950323     C                   MOVEL     'S'           WFINE
089200950323     C                   END
089300950322     C*  Ho confermato la distinta
089400950315     C                   ELSE
089500950322     C     D63TRI        IFNE      'U'
089600950322     C                   EXSR      CONFER
089700030318     C*  se confermato e ci sono altre bolle ancora da chiudere
089800030318     C*  viene lanciato un msg di avvertimento.
089900030318     c                   if        conta_cde = 999
090000030318     C*    INVIO MSG
090100030318     C                   movel     Riesegui      WINMSG
090200030318     c                   exfmt     LR63W08
090300030318     C                   clear                   WINMSG
090400030318     c                   endiF
090500950315     C                   END
090600950322     C*
090700950322     C                   END
090800950321     C                   CLEAR                   FNLR53
090900950317     C*
091000950317     C                   COMMIT
091100950315     C*
091200950315     C                   ENDSR
091300950315     C*-----------------------------------------------------*
091400950315     C*  Inizializzazione delle variabili nelle schiere
091500950315     C*-----------------------------------------------------*
091600950315     C     INZSCH        BEGSR
091700950315     C*
091800950315     C                   Z-ADD     0             NDC
091900950315     C                   Z-ADD     0             DTD
092000950315     C                   Z-ADD     0             DDC
092100950315     C                   Z-ADD     0             HRC
092200950315     C                   Z-ADD     0             SET
092300950315     C                   MOVEA     *BLANKS       FPP
092400950315     C                   MOVEA     *BLANKS       PEP
092500030204     C                   MOVEA     *BLANKS       CAR
092600950315     C*
092700950315     C*  Preimposto i dati nelle schiere in base al video
092800950315     C                   Z-ADD     V2CNDC        NDC(1)
092900950315     C                   Z-ADD     V2CDDC        DDC(1)
093000950315     C                   Z-ADD     V2CDDC        DTD(1)
093100950315     C                   Z-ADD     V2CHMC        HRC(1)
093200950315     C                   MOVEL     V2CFPP        FPP(1)
093300950315     C                   MOVEL     V2CPEP        PEP(1)
093400950315     C*  Converto la data della distinta nel formato AAAAMMGG
093500950315     C                   CLEAR                   WLBDA8
093600950315     C                   MOVE      V2CDDC        G02DAT
093700950315     C                   CALL      'XSRDA8'
093800950315     C                   PARM                    WLBDA8
093900950315     C                   Z-ADD     G02INV        §DDC
094000950315     C     G02TGI        SUB       15            GIOTGI
094100950315     C                   MOVE      *ZEROS        GIODAT
094200950315     C                   MOVE      *ZEROS        GIOINV
094300950315     C                   CALL      'XSRGI8'
094400950315     C                   PARM                    WLBGIO
094500950315     C                   MOVE      GIOINV        §G15              8 0
094600950315     C                   Z-ADD     §G15          G15(1)
094700950315     C*
094800950315     C                   SETON                                        62
094900950315     C                   WRITE     LR53C02
095000950315     C                   WRITE     LR53C03
095100950315     C                   SETOFF                                       6228
095200950315     C                   Z-ADD     0             SET
095300950315     C*
095400950315     C                   Z-ADD     0             NRR2              4 0
095500950315     C                   Z-ADD     0             NRR3              4 0
095600020611     C*
095700020611     C* inzializza il numeratore dello Stop se non è presente su ARB
095800020626     C                   Z-ADD     1             num_stop          5 0
095900950315     C*
096000950315     C                   ENDSR
096100950315      *---------------------------------------------------------*
096200950315      *  Aggiorno FNARB + FNFVV                                 *
096300950315      *---------------------------------------------------------*
096400950315     C     UPDDIS        BEGSR
096500950320     C*
096600950321     C                   Z-ADD     V2CNDC        KNFV
096700950321     C     KFVV          CHAIN     FNFVV01L                           96
096800950315     C* Scrivo il foglio viaggio bloccato
096900950315     C                   CLEAR                   FNFVV000
097000950317     C                   MOVEL     APDRSC        FVVDSF
097100020830     C                   MOVEL     d55tfa        FVVFGS
097200020916     C                   MOVEL     simFEL        FVVFLE
097300950315     C                   Z-ADD     §DDC          FVVDFV
097400950317     C                   Z-ADD     V2CNDC        FVVNFV
097500950315     C                   Z-ADD     4             FVVNPG
097600950315     C                   MOVEL     'S'           FVVFSC
097700950315     C                   MOVEL     V2CFPP        FVVFPP
097800950317     C                   MOVEL     V2CPEP        FVVPEP
097900950315     C                   MOVEL     'F'           FVVFCF
098000950321     C  N96              UPDATE    FNFVV000
098100950321     C   96              WRITE     FNFVV000
098200070515      *scrive nuovo file distinte FIDST se partita nuova gestione automatica
098300070515     c                   if        §ogdda <> *blank
098400070515     C                   Z-ADD     V2CNDC        KNFV6
098500070515     C     Kdst          CHAIN     Fidst01L                           96
098600070515     C* Scrivo il foglio viaggio bloccato
098700070515     c                   if        not %found(fidst01l)
098800070515     C                   clear                   Fidst000
098900070515     C                   MOVEL     APDRSC        dstnot
099000070515     C                   z-add     APDpdr        dstpdr
099100070515     C                   MOVEL     FVVFGS        dstfgs
099200070515     C                   Z-ADD     FVVDFV        dstdfv
099300070515     C                   Z-ADD     FVVNFV        dstnfv
099400070515     C                   Z-ADD     FVVNPG        dstnpg
099500070515     C                   MOVEL     FVVFSC        dstfca
099600070515     C                   MOVEL     FVVFPP        dstfpp
099700070515     C                   MOVEL     FVVPEP        dstpep
099800070515     C                   MOVE      'A'           dsttipa
099900070515     C                   MOVE      'N'           dstpda
100000070515     C                   WRITE     Fidst000
100100070515     c                   endif
100200070515     c                   endif
100300950315     C*
100400950315     C                   ENDSR
100500950315      *---------------------------------------------------------*
100600950315      *  Scrivo record subfile                                  *
100700950315      *---------------------------------------------------------*
100800950315     C     WRTSBF        BEGSR
100900950320     C*
101000950320     C* Eseguo chain su archivio bolle arrivo e lo fleggo
101100950320     C                   Z-ADD     CDEAAS        KAAS
101200950320     C                   Z-ADD     CDELNP        KLNP
101300950320     C                   Z-ADD     CDENRS        KNRS
101400950320     C                   Z-ADD     CDENSP        KNSP
101500950320     C     KARB          CHAIN     FNARB01L                           32
101600950320     C     *IN32         IFEQ      '0'
101700950320     C                   Z-ADD     §DDC          ARBDDC
101800950320     C                   Z-ADD     0             ARBNMI
101900950320     C                   MOVE      WCDCPC        ARBPDC
102000950321     C* Se merce disabilitata la abilito
102100950321     C     ARBAMA        IFEQ      0
102200950321     C                   MOVEL     3             ARBAMA
102300950321     C                   END
102400950320     C                   EXCEPT    AGGDIS
102500950320     C                   END
102600950315     C* Pulisco campi subfile
102700991012     C                   SETOFF                                       06
102800950315     C                   SETOFF                                       404142
102900950315     C                   SETOFF                                       434445
103000950315     C                   SETOFF                                       464748
103100991012     C                   SETOFF                                       080522
103200950315     C                   SETOFF                                       790703
103300950315     C                   CLEAR                   LR53S02
103400950315     C                   CLEAR                   LR53S03
103500950315     C                   Z-ADD     0             WIMA
103600970416     C                   Z-ADD     0             WCAS
103700970416     C                   MOVEL     *BLANKS       WVCA
103800990923     C                   MOVEL     *BLANKS       WDIV
103900970416     C                   MOVEL     *BLANKS       WTIC
104000950323     C                   Z-ADD     0             AR9CAS
104100950323     C                   MOVEL     *BLANKS       AR9VCA
104200950315     C* Incremento totale spedizioni x distinta
104300950315     C                   ADD       1             SET(1)
104400950315     C                   Z-ADD     V2CNDC        VNMDOC
104500950315     C                   Z-ADD     G15(1)        VH2G15
104600950315     C                   Z-ADD     §DDC          VH2DDC
104700950315     C                   Z-ADD     §DDC          VH2DDT
104800950315     C                   MOVEL     'NO'          V2CCON
104900950315     C*
105000950315     C                   MOVEL     ARBRSM        V3CMIT
105100950315     C     ARBRMO        IFNE      *BLANKS
105200950315     C                   MOVEL     ARBRMO        V3CMIT
105300950315     C                   END
105400950315     C                   MOVEL     ARBRSD        V02DES
105500950315     C                   Z-ADD     ARBNTC        V2CNTC
105600950315     C                   Z-ADD     ARBAAS        V2CAAS
105700950315     C                   Z-ADD     ARBLNP        V2CLNP
105800950315     C                   Z-ADD     ARBNRS        V2CNRS
105900950315     C                   Z-ADD     ARBNSP        V2CNSP
106000950315     C                   MOVEL     ARBCBO        V2CCBO
106100950315     C                   MOVEL     ARBGMA        V2CGMA
106200950315     C     V2CGMA        COMP      *BLANKS                                08
106300950315     C                   MOVEL     ARBMGS        MM                2 0
106400950315     C                   MOVE      ARBMGS        GG                2 0
106500950315     C                   MOVEL     GG            V2CGMS
106600950315     C                   MOVE      MM            V2CGMS
106700950315     C                   MOVE      ARBSTP        V02STP
106800020611      *
106900020611      * Non è possibile avere lo stop a (0) quindi imposta il numero
107000020611      *  di stop tramite un numeratore
107100030318      *   ma attenzione si possono gestire al massimo 999 stop e non di più
107200030318      *   in tal caso si riinizia da (1).
107300020611     c                   if        v02stp = 0
107400020611     c                   eval      v02stp = num_stop
107500030318     c                   if        num_stop <> 999
107600020611     c                   eval      num_stop = num_stop + 1
107700030318     c                   else
107800030318     c                   eval      num_stop = 1
107900030318     c                   end
108000020611     c                   end
108100020611      *
108200970416     C* Controllo se esiste bolla mamma
108300970417     C     KARB          CHAIN     FNLBL01L                           76
108400970416     C     *IN76         IFEQ      '0'
108500970416     C     LBLLAN        ANDEQ     LBLLAP
108600980430     C                   Z-ADD     LBLAAP        KAAS
108700980430     C                   Z-ADD     LBLLPP        KLNP
108800980430     C                   Z-ADD     LBLNRP        KNRS
108900980430     C                   Z-ADD     LBLNSP        KNSP
109000980430     C     KARB          CHAIN     FNARB01L                           76
109100000426     C* Se bolla trovata e non chiusa con causale di reso ....
109200980430     C     *IN76         IFEQ      '0'
109300980430     C     ARBCCA        ANDNE     '2'
109400980430     C     ARBCCA        ANDNE     '6'
109500970416     C* Memorizzo il legame esistente per i successivi accessi su FNARB
109600970416     C                   Z-ADD     LBLAAP        VH2AAP
109700970416     C                   Z-ADD     LBLLPP        VH2LPP
109800970416     C                   Z-ADD     LBLNRP        VH2NRP
109900970416     C                   Z-ADD     LBLNSP        VH2NSP
110000970416     C                   END
110100000426     C* Riaggancio la figlia
110200980430     C                   Z-ADD     LBLAAN        KAAS
110300980430     C                   Z-ADD     LBLLPN        KLNP
110400980430     C                   Z-ADD     LBLNRN        KNRS
110500980430     C                   Z-ADD     LBLNSN        KNSP
110600980430     C     KARB          CHAIN     FNARB01L                           76
110700980430     C                   END
110800950315     C*  controllo se c'è il contrassegno
110900950315     C                   Z-ADD     1             X
111000950315     C                   CLEAR                   DS3A
111100950315     C     ARBCBO        LOOKUP    C3A(X)                                 32
111200950315     C     *IN32         IFEQ      '1'
111300950315     C                   MOVEL     D3A(X)        DS3A
111400950315     C                   END
111500950315     C*  Controllo se c'è assegnato
111600950315     C     ARBICA        IFEQ      ' '
111700990923     C     ARBICA        OREQ      'R'
111800990923     C     ARBFIP        OREQ      'I'
111900950315     C                   MOVEL     §3ATB1        WPORTO            1
112000950315     C     WPORTO        IFEQ      'A'
112100990923     C                   MOVEL     '1'           KTRC
112200990923     C                   ELSE
112300990923     C                   MOVEL     '2'           KTRC
112400990923     C                   END
112500001128     C     KAR6          CHAIN     FIAR601L                           32
112600990923     C     *IN32         IFEQ      '0'
112700001128     C     AR6DFT        ANDNE     *ZEROS
112800990923     C                   Z-ADD     AR6IFT        WIMA
112900990923     C                   MOVEL     AR6DIV        WDIV
113000990923     C* Loop su file incassi parziali x determinare importo da inc.
113100990923     C                   MOVEL     'A'           KTIP
113200990923     C     KARI          CHAIN     FIARI01L                           95
113300990923     C     *IN95         DOWEQ     '0'
113400991012     C     AR6DIV        IFEQ      ARIDIV
113500990923     C                   SUB       ARIIPP        WIMA
113600991012     C                   ELSE
113700991012     C* Se la divisa di incasso è diversa converto importo già
113800991012     C* incassato nella divisa della bolla
113900991012     C                   CLEAR                   YEURDS
114000991012     C                   MOVEL     ARIDIV        YECDVI
114100991012     C                   Z-ADD     ARIIPP        YECIMI
114200991012     C                   MOVEL     AR6DIV        YECDVO
114300991012     C                   MOVEL     'H'           YECTAR
114400991012     C                   CALL      'YEURCO'
114500991012     C                   PARM                    YEURDS
114600991012     C                   SUB       YECIMO        WIMA
114700991012     C                   END
114800990923     C     KARI          READE     FIARI01L                               95
114900990923     C                   END
115000990923     C                   Z-ADD     AR6DFT        V3CDFT
115100970416     C                   END
115200950315     C*
115300950315     C                   ELSE
115400950315     C                   MOVE      'S'           V2CICA
115500950315     C                   END
115600950315     C* Metto la 'V' in v02sel se per la bolla è prevista la gestione
115700950315     C* della firma del destinatario
115800950315     C     ARBGMA        IFNE      *BLANKS
115900950315     C                   Z-ADD     1             X
116000950315     C                   CLEAR                   DS7R
116100950315     C     ARBGMA        LOOKUP    C7R(X)                                 32
116200950315     C     *IN32         IFEQ      '0'
116300950315     C                   SETON                                        08
116400950315     C                   MOVEL     TBLUNI        DS7R
116500950315     C                   END
116600950315     C     §7R2FI        IFEQ      'S'
116700970107     C                   MOVEL     'V'           V02SEL
116800950315     C                   END
116900950315     C                   END
117000961121     C* Metto E se prevista visualizzazione note EDI
117100961121     C     CDEFTX        IFEQ      'S'
117200961211     C                   MOVEL     '*'           V02SEL
117300961125     C                   ELSE
117400961125     C     CDENO1        IFNE      *BLANKS
117500961125     C     CDENO2        ORNE      *BLANKS
117600970107     C                   MOVEL     '*'           V02SEL
117700961121     C                   END
117800961125     C                   END
117900950315     C*
118000950315     C                   MOVE      ARBTCR        V2CTCR
118100950315     C                   MOVE      ARBDCR        V2CDCR
118200950315     C                   MOVE      ARBHCR        V2CHCR
118300051215      * richiamo pgm per informazione relativamente alla data cons. rich.
118400051215      * se TASSATIVA
118500051215     c                   clear                   v2ctat
118600051215     c                   clear                   fnlv80ds
118700051215     c                   z-add     arbAAS        ILV80AAS
118800051215     c                   z-add     arbLNP        ILV80LNP
118900051215     c                   z-add     arbNRS        ILV80NRS
119000051215     c                   z-add     arbNSP        ILV80NSP
119100051215     c                   z-add     arbMGS        ILV80MGS
119200051215     c                   move      arbTCR        ILV80TCR
119300051215     c                   z-add     arbDCR        ILV80DCR
119400051215     c                   z-add     arbHCR        ILV80HCR
119500051215     c                   move      arbTC1        ILV80TC1
119600051215     c                   move      arbTC2        ILV80TC2
119700051215     c                   call      'FNLV80R'
119800051215     c                   parm                    kpjba
119900051215     c                   parm                    fnlv80ds
120000051215     c                   move      olv80tat      v2ctat
120100051215      *
120200950315     C*
120300950321     C     §3AFCA        IFGT      0
120400950921     C     ARBICC        IFEQ      'R'
120500950921     C     ARBICC        OREQ      ' '
120600950921     C* Controllo se C/Assegno sulla BOLLA
120700051115     C     KARB          CHAIN     FIAR901L                           32
120800970417     C     *IN32         IFEQ      '1'
120900970416     C* Controllo se C/Assegno sulla mamma
121000970416     C     VH2AAP        IFNE      0
121100970416     C                   Z-ADD     LBLAAP        KAAP
121200970416     C                   Z-ADD     LBLLPP        KLPP
121300970416     C                   Z-ADD     LBLNRP        KNRP
121400970417     C                   Z-ADD     LBLNSP        KNSPP
121500970416     C     KARB1         CHAIN     FNARB01L                           32
121600051115     C  N32KARB1         CHAIN     FIAR901L                           32
121700970416     C                   END
121800970416     C                   END                                                    --- 02 ---
121900970416     C     *IN32         IFEQ      '0'                                          --- 02 ---
122000970416     C                   Z-ADD     AR9CAS        WCAS
122100970416     C                   MOVEL     AR9VCA        WVCA
122200970416     C                   MOVEL     AR9TIC        WTIC
122300970416     C                   END                                                    --- 02 ---
122400950315     C* Carico cliente x controllo tabella "TM"
122500950315     C     ARBCCM        IFGT      *ZEROS
122600950315     C                   MOVE      ARBCCM        §CLITM
122700950315     C                   ELSE
122800950315     C                   MOVE      ARBKSC        §CLITM
122900950315     C                   END
123000950315     C                   ELSE
123100950315     C                   MOVE      'S'           V2CICC
123200950315     C                   END                                                    --- 01 ---
123300950315     C                   END                                                    --- 01 ---
123400950315     C* Conto i colli ancora da consegnare
123500970416     C*  Colli su figlia
123600950320     C     KARB          CHAIN     FNART01L                           32
123700950320     C     *IN32         IFEQ      '0'
123800950320     C     *IN32         DOWEQ     '0'
123900950315     C     ARTDCM        IFEQ      0
124000950315     C                   ADD       1             V02NCL
124100950315     C                   END
124200950320     C     KARB          READE     FNART01L                               32
124300950315     C                   END
124400970416     C*
124500970416     C                   ELSE
124600970416     C*  Colli su mamma
124700970416     C     VH2AAP        IFNE      0
124800970416     C                   Z-ADD     LBLAAP        KAAP
124900970416     C                   Z-ADD     LBLLPP        KLPP
125000970416     C                   Z-ADD     LBLNRP        KNRP
125100970417     C                   Z-ADD     LBLNSP        KNSPP
125200970416     C     KARB1         CHAIN     FNART01L                           95
125300970416     C     *IN95         DOWEQ     '0'
125400970416     C     ARTDCM        IFEQ      0
125500970416     C                   ADD       1             V02NCL
125600970416     C                   END
125700970416     C     KARB1         READE     FNART01L                               95
125800970416     C                   END
125900970416     C                   END
126000970416     C*
126100950315     C                   END
126200950315     C*
126300950315     C     V02NCL        IFNE      0
126400950315     C                   Z-ADD     ARTAAS        HARAAS
126500950315     C                   Z-ADD     ARTLNP        HARLNP
126600950315     C                   Z-ADD     ARTNRS        HARNRS
126700950315     C                   Z-ADD     ARTNSP        HARNSP
126800950315     C                   Z-ADD     ARTFLS        V2CFLS
126900950315     C                   END
127000950315     C                   ADD       1             NRR2
127100950315     C*  Routine per determinazione volume da utilizzare
127200950315     C                   EXSR      DETVOL
127300950315     C* Determino peso e volume ancora da consegnare
127400950315     C     ARBPKF        MULT      V02NCL        APPOG            18 3
127500950315     C     APPOG         DIV(H)    ARBNCL        V02PKG
127600950315     C     WVOL          MULT      V02NCL        APPOG            18 3
127700950315     C     APPOG         DIV(H)    ARBNCL        V02VOL
127800950315     C*  Scrivo record importi
127900950315     C     WIMA          IFNE      0
128000970416     C     WCAS          ORNE      0
128100950315     C                   Z-ADD     V2CAAS        V3CAAS
128200950315     C                   Z-ADD     V2CLNP        V3CLNP
128300950315     C                   Z-ADD     V2CNRS        V3CNRS
128400950315     C                   Z-ADD     V2CNSP        V3CNSP
128500950315     C                   MOVEL     V2CCBO        V3CCBO
128600950315     C                   MOVEL     V2CCON        V3CCON
128700950315     C                   MOVEL     V02DES        V3CDES
128800950315     C     NRR3          ADD       1             V2CNRD
128900950315     C                   MOVEL     *BLANKS       V3CTOT
129000950315     C                   Z-ADD     NRR2          V3CNRI
129100950315     C*  Controllo se esiste C/Assegno
129200970416     C     WCAS          IFGT      0
129300950315     C                   ADD       1             NRR3
129400950315     C                   MOVEL     'N'           V02SN1
129500950315     C                   MOVEL     'C/A'         V3DTPI
129600970416     C                   Z-ADD     WCAS          V3CIMP
129700970416     C                   MOVEL     WVCA          V3CVCA
129800990924     C                   MOVEL     WVCA          VH3DIV
129900991012     C* Gestisco riga di totale solo se esiste anche assegnato
130000950315     C     WIMA          IFGT      0
130100991012     C* .. & la divisa di incasso assegnato è quella corrente...
130200011009     C     WDIV          IFEQ      §GEDCr
130300991012     C* .. o quella alternativa ..
130400991012     C     WDIV          OREQ      DIVEUR
130500991012     C* .. & la divisa di incasso C/Assegno è quella corrente...
130600011009     C     WVCA          IFEQ      §GEDCr
130700991012     C* .. o quella alternativa ..
130800991012     C     WVCA          OREQ      DIVEUR
130900950315     C                   MOVEL     'NO'          V3CTOT
131000950315     C                   END
131100991012     C                   END
131200991012     C                   END
131300970416     C                   MOVEL     WTIC          V3CTIC
131400990923     C                   Z-ADD     1             X
131500990923     C     WTIC          LOOKUP    C1A(X)                                 95
131600990923     C                   MOVEL     D1A(X)        DS1A
131700991123     C     §1AFMB        COMP      'M'                                    06
131800950315     C                   WRITE     LR53S03
131900950315     C                   SETON                                        03
132000950315     C                   END
132100950315     C*  Controllo se esiste Assegnato
132200950315     C     WIMA          IFGT      0
132300950315     C                   ADD       1             NRR3
132400950315     C                   MOVEL     'N'           V02SN1
132500950315     C                   MOVEL     'Ass'         V3DTPI
132600950315     C                   Z-ADD     WIMA          V3CIMP
132700990924     C                   MOVEL     WDIV          VH3DIV
132800950315     C                   MOVEL     ARBMIF        V3CTIC
132900950315     C                   WRITE     LR53S03
133000950315     C                   MOVEL     *BLANKS       V3CDES
133100950315     C                   END
133200950315     C*  Se esitono entrambi  Controllo se esiste C/Assegno
133300970416     C     WCAS          IFGT      0
133400990923     C     WVCA          ANDEQ     WDIV
133500950315     C     WIMA          ANDGT     0
133600950315     C                   SETON                                        05
133700950315     C                   ADD       1             NRR3
133800950315     C                   MOVEL     'N'           V02SN1
133900950315     C                   MOVEL     'Tot'         V3DTPI
134000970416     C     WCAS          ADD       WIMA          V3CIMP
134100990923     C                   MOVEL     WDIV          V3CVCA
134200990924     C                   MOVEL     WDIV          VH3DIV
134300950315     C                   MOVEL     *BLANKS       V3CTIC
134400950315     C                   WRITE     LR53S03
134500950315     C                   END
134600950315     C*
134700991012     C*  Se esitono entrambi verifico se scrivere riga totale:
134800991012     C     WCAS          IFGT      0
134900991012     C     WIMA          ANDGT     0
135000991012     C* .. Divisa di incasso assegnato è quella corrente...
135100011009     C     WDIV          IFEQ      §GEDCr
135200991012     C* .. o quella alternativa ..
135300991012     C     WDIV          OREQ      DIVEUR
135400991012     C* .. & la divisa di incasso C/Assegno è quella corrente...
135500011009     C     WVCA          IFEQ      §GEDCr
135600991012     C* .. o quella alternativa ..
135700991012     C     WVCA          OREQ      DIVEUR
135800991012     C                   SETON                                        05
135900991012     C                   ADD       1             NRR3
136000011009     C                   MOVEL     §GEDCr        V3DTPI
136100991012     C* Se l'assegnato non è in moneta di conto converto e sommo
136200991012     C* il risultato nel totale
136300011009     C     WDIV          IFNE      §GEDCr
136400991012     C                   CLEAR                   YEURDS
136500991012     C                   MOVEL     WDIV          YECDVI
136600991012     C                   Z-ADD     WIMA          YECIMI
136700011009     C                   MOVEL     §GEDCr        YECDVO
136800991012     C                   MOVEL     'H'           YECTAR
136900991012     C                   CALL      'YEURCO'
137000991012     C                   PARM                    YEURDS
137100991012     C                   Z-ADD     YECIMO        V3CIMP
137200991012     C                   ELSE
137300991012     C                   Z-ADD     WIMA          V3CIMP
137400991012     C                   END
137500991012     C* Se il C/Assegno non è in moneta di conto converto e sommo
137600991012     C* il risultato nel totale
137700011009     C     WVCA          IFNE      §GEDCr
137800991012     C                   CLEAR                   YEURDS
137900991012     C                   MOVEL     WVCA          YECDVI
138000991012     C                   Z-ADD     WCAS          YECIMI
138100011009     C                   MOVEL     §GEDCr        YECDVO
138200991012     C                   MOVEL     'H'           YECTAR
138300991012     C                   CALL      'YEURCO'
138400991012     C                   PARM                    YEURDS
138500991012     C                   ADD       YECIMO        V3CIMP
138600991012     C                   ELSE
138700991012     C                   ADD       WCAS          V3CIMP
138800991012     C                   END
138900011009     C                   MOVEL     §GEDCr        V3CVCA
139000011009     C                   MOVEL     §GEDCr        VH3DIV
139100991012     C                   MOVEL     *BLANKS       V3CTIC
139200991012     C                   WRITE     LR53S03
139300991012     C                   END
139400991012     C                   END
139500991012     C                   END
139600991012     C*
139700950315     C                   Z-ADD     NRR3          V2CNRA
139800950315     C                   END
139900950315     C*
140000950315     C*  Imposto i dati da archivio  P.C.
140100950315     C                   MOVEL     CDENO2        V2CFIR
140200950315     C                   MOVEL     CDENO1        V2CNT1
140300970107     C     CDEHMC        IFNE      9999
140400950321     C                   MOVEL     CDEHMC        V02HCO
140500950321     C                   MOVE      CDEHMC        V02MCO
140600970107     C                   END
140700950321     C                   MOVEL     ARBGC1        V021GC
140800950321     C                   MOVEL     ARBGC2        V022GC
140900950321     C*
141000950321     C     ARBGC1        IFNE      CDEGC1
141100950321     C     ARBGC2        ANDNE     CDEGC2
141200950322     C     ARBGC1        OREQ      CDEGC1
141300950322     C     ARBGC1        ANDEQ     *BLANKS
141400950322     C     ARBGC2        OREQ      CDEGC2
141500950322     C     ARBGC1        ANDEQ     *BLANKS
141600950321     C                   MOVEL     CDEGC1        V2CGC1
141700950321     C                   MOVEL     CDEGC2        V2CGC2
141800950321     C                   END
141900961118     C*  Gestisco consegne anomala
142000961118     C                   MOVEL     CDECCA        V02CCA
142100950321     C*  Gestisco consegne particolari
142200950321     C                   MOVEL     ARBTC1        V2CCP1
142300950321     C                   MOVEL     ARBTC1        V2HCP1
142400950321     C                   MOVEL     ARBTC2        V2CCP2
142500950321     C                   MOVEL     ARBTC2        V2HCP2
142600000210      *
142700000210     C                   SELECT
142800000210     C                   WHEN      CDEtc1 = *blanks
142900000210     C                   WHEN      ARBtc1 <> *blanks  and  ARBtc2 <> *blanks
143000000210     C                   WHEN      ARBtc1 = CDEtc1  or  ARBtc2 = CDEtc1
143100000210     C                   WHEN      ARBtc1 = *blanks
143200000210     C                   movel     CDETC1        V2CCP1
143300000210     C                   WHEN      ARBtc2 = *blanks
143400000210     C                   movel     CDETC1        V2CCP2
143500000210     C                   ENDSL
143600000210      *
143700950321     C                   CLEAR                   WLBDA8
143800950315     C     CDEDRC        IFNE      0
143900950315     C     CDEDCM        IFEQ      0
144000950315     C                   Z-ADD     CDEDRC        CDEDCM
144100950315     C                   END
144200950315     C                   MOVEL     'R  '         V02RIS
144300950315     C                   END
144400950315     C*  Se c'è la data di consegna o la data di riserva non considero
144500961128     C                   MOVEL     CDECMC        V02CCN
144600950315     C     CDEDCM        IFNE      0
144700950601     C                   Z-ADD     CDEDCM        G02INV
144800950315     C                   ELSE
144900950315     C     CDEDLA        IFGT      CDEDAG
145000990716     C     CDECMC        IFNE      'AV9'
145100950315     C                   MOVEL     'AVV'         V02CCN
145200990716     C                   ELSE
145300990716     C                   MOVEL     'AV9'         V02CCN
145400990716     C                   END
145500950315     C                   Z-ADD     CDEDLA        G02INV
145600950315     C                   ELSE
145700950315     C     CDECMC        IFNE      *BLANKS
145800950315     C                   MOVEL     CDECMC        V02CCN
145900950315     C                   ELSE
146000950315     C                   MOVEL     'G   '        V02CCN
146100950315     C                   END
146200950315     C                   Z-ADD     CDEDAG        G02INV
146300950315     C                   END
146400950315     C                   END
146500950315     C*
146600950315     C                   MOVEL     '3'           G02ERR
146700950315     C                   CALL      'XSRDA8'
146800950315     C                   PARM                    WLBDA8
146900950315     C                   MOVEL     G02DAT        V02DTC
147000950315     C*
147100950315     C                   WRITE     LR53S02
147200950315     C*
147300950315     C                   ENDSR
147400950315     C*-----------------------------------------------------*
147500950315     C     DETVOL        BEGSR
147600950315     C*-----------------------------------------------------*
147700950315     C* VOLUME : - SE ARBFVR   ("T"), VOLUME ARBVOL
147800950315     C*            ALTRIMENTI
147900950315     C*               - SE TOTALE   ("T"), VOLUME ARBVLS
148000950315     C*               - SE PARZIALE ("Z"), VOLUME ARBVLS (SE MAGGIORE)
148100950315     C*                               ALTRIMENTI SOMMO ARBVOL
148200950315     C     ARBFVF        IFEQ      'T'
148300950315     C                   Z-ADD     ARBVLF        WVOL
148400950315     C                   ELSE
148500050208     C     ARBNCR        ifeq      ARBNCL
148600950315     C                   Z-ADD     ARBVLC        WVOL
148700950315     C*
148800950315     C                   ELSE
148900950315     C     ARBVLC        IFGE      ARBVLF
149000950315     C                   Z-ADD     ARBVLC        WVOL
149100950315     C                   ELSE
149200950315     C                   Z-ADD     ARBVLF        WVOL
149300950315     C                   ENDIF
149400950315     C                   ENDIF
149500950315     C                   ENDIF
149600950315     C*
149700950315     C                   ENDSR
149800950322     C*-----------------------------------------------------*
149900950322     C* Ho confermato la chiusura distinta
150000950322     C*-----------------------------------------------------*
150100950322     C     CONFER        BEGSR
150200950322     C*
150300950322     C                   Z-ADD     V2CNDC        KNFV
150400950322     C     KFVV          CHAIN     FNFVV01L                           96
150500950322     C* Controllo se il flag chiusura è uguale a 'F' lo pulisco
150600950322     C  N96FVVFCF        IFEQ      'F'
150700950322     C                   MOVE      ' '           FVVFCF
150800950322     C                   EXCEPT    AGGFVV
150900950322     C                   END
151000950322     C                   MOVEL     'S'           WFINE
151100980323      *
151200980323      *  Cancello record da FNCDE e/o FNCDS.
151300980323      *
151400950322     C                   Z-ADD     D63NDT        KNDC
151500961127     C     KCDE          CHAIN     FNCDE01L                           31
151600020902      *
151700980324     C     *IN31         DOWEQ     '0'                                               -- 1
151800980323      *
151900980323     C                   Z-ADD     CDEAAS        KAAS
152000980323     C                   Z-ADD     CDELNP        KLNP
152100980323     C                   Z-ADD     CDENRS        KNRS
152200980323     C                   Z-ADD     CDENSP        KNSP
152300980323     C     KARB          SETLL     FNCDS003                               33
152400980323      *
152500980324      *  Cancello record da FNCDS se il numero distinta è impostato.
152600980324     C                   DO        *HIVAL                                            -- 2
152700980323     C     KARB          READE     FNCDS003                               33
152800980324     C  N33CDSNDC        IFEQ      CDENDC                                            -- 3
152900980323     C                   DELETE    FNCDS003
153000980324     C                   END                                                         3 --
153100980324     C  N33              END                                                         2 --
153200980324      * Controllo se restano RCD di FNCDS
153300980324     C     KARB          CHAIN     FNCDS003                           33
153400980324      *  Cancello FNCDE se non esistono record di FNCDS e se non è il secondo
153500980324      *    evento 20 - 117 con bolla senza il primo già confermato.
153600980324     C     CDEFL2        IFEQ      'S'                                               -- 2
153700961127     C                   MOVEL     *BLANKS       CDEFL2
153800961127     C                   MOVEL     *BLANKS       CDEERR
153900970225     C                   Z-ADD     0             CDENDC
154000980401     C                   MOVEL     *BLANKS       CDEFL3
154100961127     C                   EXCEPT    AGGFL2
154200980324     C                   ELSE                                                        -- 2 --
154300980324      *
154400980324     C     *IN33         IFEQ      '1'                                               -- 3
154500980324     C                   DELETE    FNCDE01L
154600980324     C                   ELSE                                                        -- 3 --
154700980324      *  Se resta solo un rcd di FNCDS a totale aggiorno FNCDE e cancello FNCDS
154800980324     C     CDSSEE        IFEQ      *BLANKS                                           -- 4
154900980324     C     KARB          READE     FNCDS003                               33
155000980324     C     *IN33         IFEQ      '0'                                               -- 5
155100980324     C                   EXSR      MUOVI
155200980324     C                   UPDATE    FNCDE000
155300980324     C                   DELETE    FNCDS003
155400980324     C                   END                                                         5 --
155500980401      *
155600980401      *  Se restano FNCDS a dettaglio ed CDEFL3 = 1 normalizzo FNCDE
155700980401     C                   ELSE                                                        -- 4 --
155800980401     C     CDEFL3        IFEQ      '1'                                               -- 5
155900980401     C                   EXSR      MUOVI0
156000980401     C                   UPDATE    FNCDE000
156100980401     C                   END                                                         5 --
156200980324     C                   END                                                         4 --
156300980324      *
156400980324     C                   END                                                         3 --
156500980324     C                   END                                                         2 --
156600980323      *
156700961127     C     KCDE          READE     FNCDE01L                               31
156800980324     C                   END                                                         1 --
156900980323      *
157000950322     C                   ENDSR
157100980324      *-----------------------------------------------------*
157200980324      *  Aggiono FNCDE con valori di FNCDS
157300980324      *-----------------------------------------------------*
157400980324     C     MUOVI         BEGSR
157500980324      *
157600980324     C                   Z-ADD     CDSDCM        CDEDCM
157700980324     C                   Z-ADD     CDSHMC        CDEHMC
157800980324     C                   Z-ADD     CDSDLA        CDEDLA
157900980324     C                   Z-ADD     CDSDAG        CDEDAG
158000980324     C                   Z-ADD     CDSDRC        CDEDRC
158100980324     C                   MOVEL     CDSNO1        CDENO1
158200980324     C                   MOVEL     CDSNO2        CDENO2
158300980324     C                   MOVEL     CDSCMC        CDECMC
158400980324     C                   MOVEL     CDSCCA        CDECCA
158500000210     C                   MOVEL     CDSTC1        CDETC1
158600980324     C                   MOVEL     CDSERR        CDEERR
158700980324     C                   MOVEL     CDSSTS        CDESTS
158800980324     C                   MOVEL     CDSCOD        CDECOD
158900980324     C                   MOVEL     CDSMOT        CDEMOT
159000980324     C                   MOVEL     CDSFL1        CDEFL1
159100980401     C                   MOVEL     *BLANKS       CDEFL3
159200980324      *
159300980324     C                   ENDSR
159400980401      *-----------------------------------------------------*
159500980401      *  Azzero FNCDE
159600980401      *-----------------------------------------------------*
159700980401     C     MUOVI0        BEGSR
159800980401      *
159900980401     C                   Z-ADD     *ZEROS        CDEDCM
160000980401     C                   Z-ADD     *ZEROS        CDEHMC
160100980401     C                   Z-ADD     *ZEROS        CDEDLA
160200980401     C                   Z-ADD     *ZEROS        CDEDAG
160300980401     C                   Z-ADD     *ZEROS        CDEDRC
160400980401     C                   MOVEL     *BLANKS       CDENO1
160500980401     C                   MOVEL     *BLANKS       CDENO2
160600980401     C                   MOVEL     *BLANKS       CDECMC
160700980401     C                   MOVEL     *BLANKS       CDECCA
160800000210     C                   MOVEL     *BLANKS       CDETC1
160900980401     C                   MOVEL     *BLANKS       CDEERR
161000980401     C                   MOVEL     *BLANKS       CDESTS
161100980401     C                   MOVEL     *BLANKS       CDECOD
161200980401     C                   MOVEL     *BLANKS       CDEMOT
161300980401     C                   MOVEL     *BLANKS       CDEFL1
161400980401     C                   MOVEL     *BLANKS       CDEFL2
161500980401     C                   MOVEL     *BLANKS       CDEFL3
161600980401      *
161700980401     C                   ENDSR
161800980324     C*-----------------------------------------------------*
161900980324     C*  Operazioni iniziali
162000980324     C*-----------------------------------------------------*
162100980324     C     *INZSR        BEGSR
162200980324      *
162300980324      * Apertura controllata dei files
162400950315     C                   OPEN      FNCDE01L                             16
162500950320     C  N16              OPEN      FNFVV01L                             16
162600070515     C  N16              OPEN      Fidst01L                             16
162700950313     C  N16              OPEN      FNARB01L                             16
162800051115     C  N16              OPEN      FIAR901L                             16
162900950313     C  N16              OPEN      FNART01L                             16
163000980324     C  N16              OPEN      FNCDS03L                             16
163100950313     C   16              GOTO      FINE                                         OPEN vado a fine
163200980324      *
163300950313     C*  Definizione chiavi
163400950315     C     KARB          KLIST
163500950313     C                   KFLD                    KAAS
163600950313     C                   KFLD                    KLNP
163700950313     C                   KFLD                    KNRS
163800950313     C                   KFLD                    KNSP
163900990923     C     KAR6          KLIST
164000990923     C                   KFLD                    KAAS
164100990923     C                   KFLD                    KLNP
164200990923     C                   KFLD                    KNRS
164300990923     C                   KFLD                    KNSP
164400990923     C                   KFLD                    KTRC
164500990923     C     KARI          KLIST
164600990923     C                   KFLD                    KAAS
164700990923     C                   KFLD                    KLNP
164800990923     C                   KFLD                    KNRS
164900990923     C                   KFLD                    KNSP
165000990923     C                   KFLD                    KTIP
165100961114     C*
165200961114     C     KGCA          KLIST
165300961114     C                   KFLD                    KAAS1
165400961114     C                   KFLD                    KLNP1
165500961114     C                   KFLD                    KNRS1
165600961114     C                   KFLD                    KNSP1
165700961114     C     KANM          KLIST
165800990519     C                   KFLD                    KAAS1
165900961114     C                   KFLD                    KLNP1
166000961114     C                   KFLD                    KNRS1
166100961114     C                   KFLD                    KNSP1
166200961114     C                   KFLD                    KCAA
166300950313     C*
166400950313     C     KFVV          KLIST
166500950313     C                   KFLD                    KNPG
166600950313     C                   KFLD                    KNFV
166700950313     C                   KFLD                    KFGS
166800070515     C*
166900070515     C     Kdst          KLIST
167000070515     C                   KFLD                    KNPG
167100070515     C                   KFLD                    KNFV6             6 0
167200070515     C                   KFLD                    KFGS
167300950313     C*
167400950315     C     KTAB          KLIST
167500950313     C                   KFLD                    KKUT
167600950313     C                   KFLD                    KCOD
167700991012     C     KTBE          KLIST
167800991012     C                   KFLD                    KCODE
167900991012     C                   KFLD                    KKEYE
168000950315     C*
168100950315     C     KCDE          KLIST
168200950315     C                   KFLD                    KLNA
168300950315     C                   KFLD                    KNDC
168400970417     C*
168500970417     C     KARB1         KLIST
168600970417     C                   KFLD                    KAAP
168700970417     C                   KFLD                    KLPP
168800970417     C                   KFLD                    KNRP
168900970417     C                   KFLD                    KNSPP
169000021204     C*
169100021204     C     KAPD          KLIST
169200021204     C                   KFLD                    APDTIP
169300021204     C                   KFLD                    APDPDR
169400021204     C                   MOVEL     'A'           APDTIP
169500950313     C*----------------------------------------------------*
169600950313     C*  Definizione delle variabili
169700950313     C     *LIKE         DEFINE    ARBAAS        KAAS
169800950313     C     *LIKE         DEFINE    ARBLNP        KLNP
169900950313     C     *LIKE         DEFINE    ARBNRS        KNRS
170000950313     C     *LIKE         DEFINE    ARBNSP        KNSP
170100990923     C     *LIKE         DEFINE    ARITIP        KTIP
170200950313     C     *LIKE         DEFINE    FVVNPG        KNPG
170300950313     C     *LIKE         DEFINE    FVVNFV        KNFV
170400950313     C     *LIKE         DEFINE    FVVFGS        KFGS
170500950313     C     *LIKE         DEFINE    TBLKUT        KKUT
170600950313     C     *LIKE         DEFINE    TBLCOD        KCOD
170700950315     C     *LIKE         DEFINE    ARBDDC        §DDC
170800950315     C     *LIKE         DEFINE    CDELNA        KLNA
170900950315     C     *LIKE         DEFINE    CDENDC        KNDC
171000950315     C     *LIKE         DEFINE    AR6IFT        WIMA
171100950315     C     *LIKE         DEFINE    ARBVLF        WVOL
171200970417     C     *LIKE         DEFINE    AR9CAS        WCAS
171300970417     C     *LIKE         DEFINE    AR9VCA        WVCA
171400990923     C     *LIKE         DEFINE    AR6DIV        WDIV
171500970417     C     *LIKE         DEFINE    AR9TIC        WTIC
171600990923     C     *LIKE         DEFINE    AR6TRC        KTRC
171700961114     C     *LIKE         DEFINE    LBLAAP        KAAS1
171800961114     C     *LIKE         DEFINE    ANMLNP        KLNP1
171900961114     C     *LIKE         DEFINE    ANMNRS        KNRS1
172000961114     C     *LIKE         DEFINE    ANMNSP        KNSP1
172100961114     C     *LIKE         DEFINE    ANMCAA        KCAA
172200970417     C     *LIKE         DEFINE    LBLAAP        KAAP
172300970417     C     *LIKE         DEFINE    LBLLPP        KLPP
172400970417     C     *LIKE         DEFINE    LBLNRP        KNRP
172500970417     C     *LIKE         DEFINE    LBLNSP        KNSPP
172600991012     C     *LIKE         DEFINE    TBECOD        KCODE
172700991012     C     *LIKE         DEFINE    TBEKE1        KKEYE
172800950313     C*----------------------------------------------------*
172900950313     C* Giro data del giorno
173000950313     C                   TIME                    WHHDAT           14 0
173100950317     C                   MOVE      WHHDAT        WOGGI             8 0
173200020828     C* Decodifica Elaboratore
173300020828     c     *dtaara       define    §azute        azuteds
173400020828     c     *dtaara       define    §datiute      ddatiute
173500020828     C                   in(E)     *dtaara
173600020828     C                   IF        %Error  or  RSUT = *blanks
173700020828     C                   call      'TIBS34R'
173800020828     C                   parm                    Tibs34Ds
173900020828     C                   in        *dtaara
174000020828     c                   ENDIF
174100950315     C*----------------------------------------------------*
174200950315     C*  Caricamento Tabella 7R
174300950315     C                   Z-ADD     0             X                 3 0
174400950317     C                   Z-ADD     1             KKUT
174500950315     C                   MOVEL     '7R'          KCOD
174600950315     C     KTAB          CHAIN     TABEL00F                           96
174700950315     C     *IN96         DOWEQ     '0'
174800950315     C     X             ANDLT     100
174900950315     C     TBLFLG        IFEQ      ' '
175000950315     C                   ADD       1             X
175100950315     C                   MOVEL     TBLKEY        C7R(X)
175200950315     C                   MOVEL     TBLUNI        D7R(X)
175300950315     C                   END
175400950315     C     KTAB          READE     TABEL00F                               96
175500950315     C                   END
175600950315     C*  Caricamento Tabella 3A
175700950315     C                   Z-ADD     0             X                 3 0
175800950315     C                   MOVEL     '3A'          KCOD
175900950315     C     KTAB          CHAIN     TABEL00F                           96
176000950315     C     *IN96         DOWEQ     '0'
176100950315     C     X             ANDLT     100
176200950315     C     TBLFLG        IFEQ      ' '
176300950315     C                   ADD       1             X
176400950315     C                   MOVEL     TBLKEY        C3A(X)
176500950315     C                   MOVEL     TBLUNI        D3A(X)
176600950315     C                   END
176700950315     C     KTAB          READE     TABEL00F                               96
176800950315     C                   END
176900990923     C* Caricamento tipo incasso
177000990923     C                   Z-ADD     0             X                 3 0
177100990923     C                   MOVEL     '1A'          KCOD
177200990923     C     KTAB          CHAIN     TABEL00F                           96
177300990923     C     *IN96         DOWEQ     '0'
177400990923     C     X             ANDLT     100
177500990923     C     TBLFLG        IFEQ      ' '
177600990923     C                   ADD       1             X
177700990923     C                   MOVEL     TBLKEY        C1A(X)
177800990923     C                   MOVEL     TBLUNI        D1A(X)
177900990923     C                   END
178000990923     C     KTAB          READE     TABEL00F                               96
178100990923     C                   END
178200991012     C*  Aggancio tabella 'GED' x reperire la divisa corrente
178300991012     C                   MOVEL     'GED'         KCODE
178400991012     C                   MOVEL     *BLANKS       KKEYE
178500991012     C                   MOVEL     '1'           KKEYE
178600991012     C     KTBE          CHAIN     TNTBE01L                           13
178700991012     C     *IN13         IFEQ      '0'
178800991012     C                   MOVEL     TBEUNI        DGED
178900991012     C                   END
179000991012     C*
179100991012     C*  CARICO DATI TABELLA CV
179200991012     C                   MOVEL     *BLANKS       DIVEUR
179300991012     C                   MOVEL     'CV'          KCOD
179400991012     C     KTAB          CHAIN     TABEL00F                           13
179500991012     C     *IN13         DOWEQ     '0'
179600991012     C                   MOVEL     TBLKEY        DIVCV             3
179700991012     C                   MOVEL     TBLUNI        DSCV
179800991012     C* Se divisa diversa dalla moneta di conto reperisco data
179900991012     C* validità x trovare altra divisa da utilizzare x incassi
180000011009     C     §GEDCR        IFNE      DIVCV
180100991012     C     DIVEUR        ANDEQ     *BLANKS
180200991012     C     §CVDVA        ANDGE     'S'
180300991012     C                   MOVEL     DIVCV         DIVEUR            3
180400991012     C                   END
180500991012     C     KTAB          READE     TABEL00F                               13
180600991012     C                   END
180700950313     C*----------------------------------------------------*
180800950315     C                   CLEAR                   FNLR53
180900950315     C                   Z-ADD     4             KNPG
181000950313     C                   MOVEL     ' '           WFINE             1
181100950313     C                   MOVEL     '2'           WTPVID            1
181200950320     C                   MOVEL     'N'           WCHIUS            1
181300950426     C                   MOVEL     'N'           WPGMER            1
181400950320     C                   MOVEL     'N'           WSBLOC            1
181500950313     C*----------------------------------------------------*
181600950313     C                   ENDSR
181700020830      *---------------------------------------------------------*
181800020830      *  reperisce il po di gestione                            *
181900020830      *---------------------------------------------------------*
182000020830     C     srfgs         BEGSR
182100020830     C*
182200020830     c                   clear                   fnlv55ds
182300020830     c                   eval      d55tpt = '6'
182400020830     c                   movel     d63lna        d55lin
182500020830     c                   movel     *date         dataiso
182600020830     c                   movel     dataiso       d55drf
182700020830     c                   call      'FNLV55R'
182800020902     c                   parm                    fnlv55ds
182900020830     C*
183000020830     C                   ENDSR
183100950315     C*----------------------------------------------------*
183200950315     OFNCDE000  E            AGGCDE
183300950315     O                       CDENDC
183400950606     O                       CDEDAG
183500950606     O                       CDEDCM
183600961127     OFNCDE000  E            AGGFL2
183700961127     O                       CDEFL2
183800961127     O                       CDEERR
183900970225     O                       CDENDC
184000950320     OFNCDE000  E            NOAGG
184100950315     OFNARB000  E            AGGARB
184200020829     O                       ARBifp
184300020829     O                       ARBNDC
184400961114     O                       ARBFBC
184500961114     O                       ARBCMC
184600950315     OFNARB000  E            AGGDIS
184700950315     O                       ARBPDC
184800950315     O                       ARBDDC
184900950315     O                       ARBNMI
185000950323     O                       ARBAMA
185100950315     OFNFVV000  E            AGGFVV
185200950315     O                       FVVFCF
185300980323      *
185400980323     OFNCDS003  E            AGGCDS
185500980323     O                       CDSNDC
185600980323     O                       CDSDAG
185700980323     O                       CDSDCM
185800980323      *----------------------------------------------------*
185900980323**
186000950314Codice padroncino non valido                                           01
186100950314Codice padroncino inesistente                                          02
186200950314Immettere data di consegna                                             03
186300950314Data di consegna errata                                                04
186400950314Immettere ora di consegna                                              05
186500950314Ora di consegna errata                                                 06
186600950314Numero di spedizione inesitente                                        07
186700950314Linea di arrivo diversa dalla bolla                                    08
186800950314Riferimento mittente numerico diverso                                  09
186900950314Bolla con numero distinta già impostato                                10
187000950314Bolla già consegnata                                                   11
187100950314Deve essere impostata almeno una data                                  12
187200950315Non chiudere dist. di questo padr.x conflitti! Riprovare fra 2 min.    13
187300950321Sped. non abilitata all'incasso del C/A                                14
187400950321Spedizione da tassare                                                  15
187500961119Spediz.con giacenza in fase non congrua                                16
187600000210Impossibile aggiungere Cons.Partic. - -                                17
187700950606Bolla con figlia !!!                                                   18
187800961121Immetti G=sped.giac./C=Conse./A=L.Avv.                                 19
187900961120Evento da analizzare !!                                                20
188000961118Immetti E se vuoi visualizzare note EDI                                21
188100961120Ricevuto secondo evento colli mancanti                                 22
188200961230Non ci sono spedizioni valide da chiudere in distinta                  23
188300980323Analizzare dettaglio segnacolli !!!                                    24
188400000925C/A ancora presente da annullare !!                                    25
188500001019Esistono altre bolle da estrarre dopo la chiusura ripetere l'operazione26
