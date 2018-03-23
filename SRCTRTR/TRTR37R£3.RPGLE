000100910926     H DECEDIT('0,') DATEDIT(*DMY.)
000200071029     H* TRTR39R *----------------------------------------------------*
000300911114     H*  RICEZIONE IN SEDE VARIAZIONE BOLLE                          *
000400000000     H*--------------------------------------------------------------*
000500070119     Ffnblp01l  IF   E           K DISK    usropn
000600070119     FTNTAM01L  IF   E           K DISK
000700990719     FTITPT01L  IF   E           K DISK
000800990719     FTITPD01L  IF   E           K DISK
000900981001     F*  ARCHIVIO BOLLA SEDE MITTENTE DESTINATARIO
001000981001     FTNTMD01L  UF A E           K DISK
001100981001     F*
001200151008     FFiarbr3c  UF   E           K DISK    USROPN infds(infdsarbr)
001201151008     Ffnarbg0r  iF   E             disk    rename(fnarbg00:fnarbg)
001202151008     f                                     usropn
001203151008     Ffnarbd0r  iF   E             disk    rename(fnarbd00:fnarbd)
001204151008     f                                     usropn
001205151008     Ffnarbk0r  iF   E             disk    rename(fnarbk00:fnarbk)
001206151008     f                                     usropn
001207151008     Ffiarbt0r  iF   E             disk    rename(fiarbt00:fiarbt)
001208151008     f                                     usropn
001300950201     FFNARBM1R  UF   E           K DISK    USROPN
001400950201     F                                     RENAME(FNARBM00:FNARBMRR)
001500950201     FFNARBD1R  UF   E           K DISK    USROPN
001600950201     F                                     RENAME(FNARBD00:FNARBDRR)
001700991014     FFiARBU1R  UF   E           K DISK    USROPN
001800991014     F                                     RENAME(FIARBU00:FIARBURR)
001900991014     FFIARBE2C  IF A E           K DISK
002000950201     F                                     RENAME(FNARBD00:FNARBDEE)
002100991014     F                                     RENAME(FIARBT00:FIARBTEE)
002200950201     F                                     RENAME(FNARBK00:FNARBKEE)
002300950201     F                                     RENAME(FNARBM00:FNARBMEE)
002400950201     F                                     RENAME(FNARBG00:FNARBGEE)
002500991014     FFIARBU1E  O  A E           K DISK
002600991014     F                                     RENAME(FIARBU00:FIARBUEE)
002700950201     F*
002800990624     FTITAS30C  UF A E           K DISK
002900990629     FTITAA30C  UF A E           K DISK
003000990624     FTITA730C  UF A E           K DISK
003100010126     FTITA430C  UF A E           K DISK
003200000000     FTABEL00F  IF   E           K DISK
003300900426     FAZORG01L  IF   E           K DISK
003400941121     FFNLBL01L  IF   E           K DISK
003500941121     FTNCSB03L  UF A E           K DISK
003600941121     FTNCSA01L  UF A E           K DISK
003700941121     FTNCSV00F  O  A E             DISK
003800981118     Fndppa01L  IF   E           K DISK
003900990518     FNdcpa01L  uF   E           K DISK
004000941130     FTNBLA01L  IF   E           K DISK
004006121107
004100020827     D CMD1            S             51    DIM(1) CTDATA PERRCD(1)
004200020827     D CM2             S              1    DIM(51) CTDATA PERRCD(51)
004300020827     D CM3             S              1    DIM(51) CTDATA PERRCD(51)
004400020827     D CM5             S              1    DIM(213) CTDATA PERRCD(66)
004500020827     D C6              S              1    DIM(213)
004600920113     D*
004700991019     D ERR             S             35    DIM(42) CTDATA PERRCD(1)             COSTANTI ERRORA
004800950202     D K35             S              1    DIM(35)                              COMODO        A
004900070119     d cmdt            s             60    dim(01) ctdata perrcd(1)
005000011218
005100011218     d sksv            s              1    dim(20)                              sk sigla varie
005200011218     d skva            s             11  3 dim(20)                              sk imp.varie
005300011218
005301151008     d infdsarbr       ds
005302151008     d   recname             261    270
005303151008     D   nrri                397    400i 0
005800970916     D WLBDAT          DS                  INZ
005900970916     D  G02DAT                 1      8  0
006000970916     D  G02INV                 9     16  0
006100970916     D  G02ERR                17     17
006200970916     D  G02TGI                18     22  0
006300970916     D WGIDAT          DS                  INZ
006400970916     D  GIODAT                 1      8  0
006500970916     D  GIOINV                 9     16  0
006600970916     D  GIOTGI                17     21  0
006700941130     D CSBDS         E DS                  EXTNAME(TNCSB00F)
006800941130     D               E DS                  EXTNAME(TNCSV00F)
006900941130     D  CSVDS                 16    229
007000920113     D KPJBA         E DS
007100920113     D  LIBSYS                92    101
007200981118     D  wsoc                  98    100
007300920113     D  LEGFIL               483    492
007400920113     D  MBRFIL               493    502
007500920113     D  ALFFIL               494    496
007501151008     d fnlvv1ds      e ds
007502151008      * ATTENZIONE
007503151008     d* Se dovesse cambiare la lunghezza record di una delle seguenti DS
007504151008     d* riverificare la definizione del campo parvar
007508151008     d dsarbg          ds                  qualified
007509151008     d   fnarbg                            likerec(fnarbg) overlay(dsarbg)
007510151008     d dsarbk          ds                  qualified
007511151008     d   fnarbk                            likerec(fnarbk) overlay(dsarbk)
007512151008     d dsarbd          ds                  qualified
007513151008     d   fnarbd                            likerec(fnarbd) overlay(dsarbd)
007514151008     d dsarbt          ds                  qualified
007515151008     d   fiarbt                            likerec(fiarbt) overlay(dsarbt)
007516151008      * Definizione del seguente campo in base alla dsarbd che è la più lunga
007517151008      * di tutte.
007518151008     dparvar           s            224
007519151008     dpartitas         s                   like(titasds)
007520151008     dparcsb           s                   like(dscsb)
007600970714     D* DS PER TISI95R - CONTROLLO CAP
007700970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
007800021014     D ddatiute      e ds
007900021014     D azuteds       e ds                  extname(AZUTE00F)
008000020604     D tibs34ds      E DS
008100021112     D tibs36ds      E DS
008200070201     D tibs55ds      E DS
008300930514     D DSTB          E DS
008400930514     D DS3A          E DS
008500920113     D DS7L          E DS
008501150108     D DS7R          E DS
008600950202     D DS1A          E DS
008700010126     D og143         E DS
008800010126     D dsbl4j        E DS
008900060317     D dta4a         E DS
009000971215     D DSLV55        E DS                  EXTNAME(FNLV55DS)
009100981001     D DSBS69        E DS                  EXTNAME(TIBS69DS)
009200981001     D DSACO         E DS                  EXTNAME(CNACO00F)
009300981001     D DSIND         E DS                  EXTNAME(CNIND00F)
009400981001     D DSCLP         E DS                  EXTNAME(CNCLP00F)
009500981001     D DSCLS         E DS                  EXTNAME(FNCLS00F)
009502121115     D dstitas       E DS                  EXTNAME(titas00f) qualified
009503121116     D titasds       E DS                  EXTNAME(titas00f)
009504121115     D dscsb         E DS                  EXTNAME(tncsb00f) qualified
009600080408     D SAVDS3a       E DS                  extname(ds3a) prefix(S_)
009700120510     d*dvpoDECO      e ds
009800121114     D Tibs02DS      E DS
009900071102     d
010000990505     DWDELARBU         s              1
010100990505     dwctb             s                   like(ppactb) inz('CG')
010200981118     dwkcc             s                   like(ppakcc) inz('000151')
010300981118     dwksc             s                   like(ppaksc) inz('00000000')
010400981118     dwdft             s                   like(ppadtpar)
010500981118     dwnft             s                   like(ppanrpar)
010600981118     dwfiv             s                   like(ppaserpar) inz
010700981118     dsavfiv           s                   like(tasfiv)
010800981118     dsavnft           s                   like(tasnft)
010900981118     dsavdft           s                   like(tasdft)
011000981118     dsavksc           s                   like(tasksc)
011001151012     dtasdsp           s                   like(tamddt)
011100990505     dWCAS             s                   like(arbcas)
011200990625     d§trc             s                   like(ta4trc)
011300991102     dwcsbvca          s                   like(csbvca)
011400011218
011500011218     d flgriorg        s              1
011600011218     d xx              s              2  0                                      indice sk varie
011700070119     d commt           s             80
011800070119     d lenght          s             15  5 inz(80)
011801120424     D k               s              5  0                                      indice
011804121205     D wanncas         s              1
011805151008     D                SDS
011806151008     D  NOMPGM                 1     10
011900941130     I*
012000950201     IFNARBD00      11
012100950201     IFNARBK00      12
012200991014     IFIARBT00      13
012300950201     IFNARBG00      15
012400950201     IFNARBM00      16
012500941121     I*
012600990624     ITITAS000      02
012700990624     ITITAS010      03
012800990624     ITITASP00      04
012900010126     I*
013000010126     ITITA4000      05
013100010126     ITITA4P00      06
013200071029     I*
013300071029     ITITAA000      07
013400071029     ITITAAP00      08
013500920113     I/SPACE 3
013600920113     C****************************************************************
013700920113     C*  RIEPILOGO INDICATORI
013800920113     C***************************************************************
013900950201     C* 01    - LETTURA SUL FILE DEGLI ERRORI DI FNARBE
014000010126     C* 02/06 - INDICATORI DI TIPO RECORD
014100941130     C* 17    - STAMPO ERRORE VARIAZIONE CONS PARTICOLARE
014200920228     C* 20    - NON ALLOCATI DEGLI AGGETTI ESCO DA PROGRAMMA
014300930514     C* 23    - OFF SPEDIZIONE STORNATA
014400930513     C* 24    - NON AGGIORNO TUTTI I CAMPI SE 2BOLL GIA' CONTABILIZZATA
014500930513     C* 25    - NON ESISTE ANAGRAFICA C/A
014600981118     C* 26    - NON ESISTE RECORD IN partita
014700930513     C* 27    - NON AGGIORNO TUTTI I CAMPI SE 1BOLL GIA' CONTABILIZZATA
014800930512     C* 29    - ON  33 ON  - NON ESISTE NEMMENO        SU BLTAS10
014900930702     C*       - ON  33 OFF - SPEDIZIONE CONTABILIZZATA SU BLTAS
015000950201     C* 30    - LETTURA FNARBR3C
015100920212     C* 31    - FILE CONTRASSEGNI NON TROVATO O GIA' INCASATO
015200920212     C* 32    - COMODO
015300920228     C* 33    - ON SPEDIZIONE ESISTENTE SU BLTAS10
015400920212     C* 34    - COMODO
015500950201     C* 35    - MANCA RECORD SU FNARBM0R
015600920214     C* 36    - SCRIVO FLRBM0E QUANDO SERVE
015700920213     C* 37    - RECORD DA NON CANCELLARE DOPO WRITE ERRORE
015800920226     C* 38    - SPEDIZIONE GIA' TASSATA
015900920227     C* 39    - DI COMODO
016000920303     C* 40    - SPEDIZIONE ANNULLATA
016100941130     C* 41    - ESISTENTE TNCSB000
016200930303     C* 42    - ESISTENTE LEGAME BOLLA (C/ASSEGNO SULLA MAMMA)
016300981001     C* 48    - NON ESISTE RECORD SU TNTMD
016400920113     C*****************************************************************
016500920113     C*
016600920113     C* ALLOCO MEMBRI
016700920113     C                   EXSR      CAEX
016800920113     C   20              GOTO      FINE
016900920113     C*
017000991014     C                   READ      FIARBR3C                               30
017100920113     C*
017200930512    1C     *IN30         DOWEQ     '0'
017300990414     C                   SETOFF                                       242736
017400930622     C*
017500981001     C                   MOVEL     ' '           DMDD              1
017600930514     C                   MOVEL     ' '           ST1
017700981001     C* SOLO NEL CASO IN CUI SI TRATTI DI LETTURA DI VARIAZIONI MITTENTE E
017800981001     C* DESTINATARIO ( *IN11 *IN16 ON ) CONTROLLO SE SI TRATTA DI VARIAZIONI
017900981001     C* DM / DD PERCHE' IN QUESTO CASO DEVO SCRIVERE IL FILE DATI ANAGRAFICI
018000981001     C* SPEDIZIONI CHE VIENE UTILIZZATO PER STAMPE RELATIVE AI DANNI
018100981001     C* INOLTRE SEMPRE PER 11 E 16 ACCESO PER LE VARIAZIONI I0 / MI (VARIAZIONI
018200981001     C* DESTINATARIO E MITTENTE) DEVO AGGIORNARE QUESTO FILE INDIPENDENTEMENTE
018300981001     C* DAL FATTO CHE SIA RELATIVO AD UNA BOLLA GIA' CONTABILIZZATA
018400981001     C     *IN11         IFEQ      '1'
018500981001     C     *IN16         OREQ      '1'
018600981001     C                   EXSR      AGTMD
018700981001     C* SE HO LETTO UNA VARIAZIONE DM O DD LEGGO IL RECORD SUCCESSIVO
018800981001     C* ALTRIMENTI PROSEGUO
018900981001     C     DMDD          IFNE      ' '
019000981001     C                   GOTO      NEXT
019100981001     C                   ENDIF
019200981001     C*
019300981001     C                   ENDIF
019400930513     C*
019500930513     C* PRENDO  C O D I C E   B O L L A
019600920113     C                   MOVEL     '3A'          COD
019700920113     C                   MOVEL     *BLANKS       KEY
019800991014     C  N12              MOVEL     ARBCBO        KEY
019900950203     C   12              MOVEL     ARBCBP        KEY
020000920113     C     KTAB          CHAIN     TABEL                              33
020100920114     C*
020200920212     C* SE NON ESISTE TIPO BOLLA: ERRORE E NON CANCELLO
020300930513    2C     *IN33         IFEQ      '1'
020400991014     C                   MOVEL     ERR(2)        ARBDER
020500930513     C*
020600000703     C                   Z-ADD     102           COMFEV
020700930513     C                   SETON                                        37
020800920212     C*
020900930513     C                   EXSR      WTRERR
021000930513     C*
021100930513   X2C                   ELSE
021200930513     C*
021300930513     C                   MOVEL     TBLUNI        DS3A
021400950203     C* IMPOSTO IL TIPO BOLLA PER EVENTUALE ERRORE
021500950203     C                   MOVEL     §3ATB1        TBLERR            1
021600990519     C                   MOVEL     §3ARBL        TBLRBL            1
021700930513     C*
021800930513     C* PRENDO  C A U S A L E   V A R I A Z I O N E
021900991014     C                   MOVEL     ARBCVB        COMCVB            2
022000930513     C*
022100930513     C                   MOVEL     '7L'          COD
022200930513     C                   MOVEL     *BLANKS       KEY
022300930513     C                   MOVEL     COMCVB        KEY
022400930513     C     KTAB          CHAIN     TABEL                              33
022500930513     C*
022600930513     C* 33 ON - NON ESISTE CAUSALE VARIAZIONE: STAMPA E NON ANNULLO
022700930513    3C     *IN33         IFEQ      '1'
022800991014     C                   MOVEL     ERR(3)        ARBDER
022900930513     C*
023000930513     C                   Z-ADD     102           COMFEV
023100930513     C                   SETON                                        37
023200930513     C*
023300930513     C                   EXSR      WTRERR
023400930513    3C                   END
023500930513     C*
023600930513     C  N33              MOVEL     TBLUNI        DS7L
023700930513    2C                   END
023800920130     C*
023900930513    2C     *IN33         IFEQ      '0'
024000920226     C                   MOVEL     COMCVB        FLGCVB            1
024100920226     C*
024200990701     C                   CLEAR                   KSC42BO
024300990701     c*
024400920226     C* CONTROLLO ESISTENZA 2   BOLLA
024500950202    3C     FLGCVB        IFNE      'C'
024600950202     C     §3ATB2        ANDNE     '  '
024700970514     C* NON CONTROLLO LA 2 BOLLA SE TASSAZIONE IN PARTENZA PERCHE' NON
024800970514     C*  LA VA MAI AD AGGIORNARE
024900970514     C     COMCVB        ANDNE     'TP'
025000920226     C                   MOVEL     §3ATB2        COMTBL
025100920226     C                   EXSR      CTRSPE
025200941130     C*
025300941130     C* SE NON TROVATI ERRORI --> SALVO CAMPI CHE MI SERVONO
025400941130    4C     *IN29         IFEQ      *OFF
025500941130     C                   MOVE      TASKSC        SAVKS4            4 0
025600981118     C                   MOVE      TASKSC        SAVKsc
025700941130     C                   MOVEL     TASFIV        SAVFIV
025800941130     C                   MOVE      TASDFT        SAVDFT
025900941130     C                   MOVEL     TASNFT        SAVNFT
026000960625     C                   MOVEL     TASTBL        SAVTBL
026100941130     C*
026200941130     C* 24 ON - 2 BOLLA GIA' CONTABILIZZATA
026300941130    5C     *IN02         IFEQ      *ON
026400941130     C     TASFIC        ANDNE     ' '
026500941130     C     *IN02         OREQ      *OFF
026600941130     C                   SETON                                        24
026700941130    5C                   ENDIF
026800941130    4C                   ENDIF
026900930513    3C                   END
027000920226     C*
027100920226     C* SE TROVATA 2 BOLLA CONTROLLO LA 1
027200930513    3C  N29FLGCVB        IFNE      '2'
027300920226     C                   MOVEL     §3ATB1        COMTBL
027400920226     C                   EXSR      CTRSPE
027500941130     C*
027600941130    4C     *IN29         IFEQ      *OFF
027700981118     C* 27 ON - 1 BOLLA GIA' CONTABILIZZATA --> AGGIORNO ANCHE yncpa
027800941130    5C     *IN02         IFEQ      *ON
027900941130     C     TASFIC        ANDNE     ' '
028000941130     C     *IN02         OREQ      *OFF
028100941130     C                   SETON                                        27
028200941130    5C                   END
028300941130    4C                   END
028400930513    3C                   END
028500990414     C*
028600941130     C*
028700941130     C* 29 OFF - AGGIORNO BOLLA SE NON SONO STORNATE ENTRAMBI LE BOLLE
028800930514     C  N29ST1           IFEQ      ' '
028900991021     c     flgcvb        oreq      'C'
029000930514     C                   EXSR      AGGBOL
029100930514     C                   END
029200941130     C*
029300930514    2C                   END
029400920113     C*
029500950203    2C  N37              DO
029600920212     C   11              ADD       1             NUMRBD            7 0
029700920113     C   12              ADD       1             NUMRBK            7 0
029800920113     C   13              ADD       1             NUMRBT            7 0
029900950201     C   15              ADD       1             NUMRBG            7 0
030000950201     C   16              ADD       1             NUMRBM            7 0
030100920219     C* CANCELLO RECORD
030200950201     C   11              DELETE    FNARBD00
030300950201     C   12              DELETE    FNARBK00
030400950202     C   12KARBM         DELETE    FNARBMRR                           32
030500991014     C   13              DELETE    FIARBT00
030600100303     c   13              if        st1='1'
030700100303     C     KARBU         SETLL     FIARBU1R
030800100303     C     KARBU         READE     FIARBU1R                               30
030900100303     C*
031000100303     C     *IN30         DOWEQ     *OFF
031100100303     C                   DELETE    FIARBURR
031200100303     C*
031300100303     C     KARBU         READE     FIARBU1R                               30
031400100303     C                   ENDDO
031500100303     c
031600100303     c                   endif
031700100303     c
031800950201     C   15              DELETE    FNARBG00
031900950201     C   16              DELETE    FNARBM00
032000930513    2C                   END
032100920113     C*
032200981001     C     NEXT          TAG
032300981001     C*
032400920113     C                   SETOFF                                       111213
032500991014     C                   SETOFF                                         1516
032600950201     C                   SETOFF                                       353738
032700941130     C                   SETOFF                                       2933
032800991014     C                   READ      FIARBR3C                               30
032900930512    1C                   END
033000970714     C*
033100970714     C                   CLEAR                   DSSI95
033200970714     C                   MOVEL     'C'           I95TLA
033300970714     C                   CALL      'TISI95R'
033400970714     C                   PARM                    DSSI95
033500971215     C*
033600971215     C                   MOVEL     'C'           D55TLA
033700971215     C                   CALL      'FNLV55R'
033800971215     C                   PARM                    DSLV55
033900981001     C*
034000981001     C                   CLEAR                   DSBS69
034100981001     C                   MOVEL     'C'           I69TLA
034200981001     C                   CALL      'TIBS69R'
034300981001     C                   PARM                    DSBS69
034400981001     C                   PARM                    DSACO
034500981001     C                   PARM                    DSIND
034600981001     C                   PARM                    DSCLP
034700981001     C                   PARM                    DSCLS
034800020604     c*
034900021112     C                   CLEAR                   tibs36ds
035000021112     C                   MOVEL     'C'           I36TLA
035100021112     C                   CALL      'TIBS36R'
035200021112     C                   PARM                    TIBS36DS
035201120424
035209121114
035210121114     C                   CLEAR                   tibs02ds
035211121114     C                   MOVEL     'C'           t02tla
035212121114     C                   CALL      'TIBS02R'
035213121114     C                   PARM                    kpjba
035214121114     C                   PARM                    tibs02ds
035215151009
035216151009     C                   MOVEL     'C'           iv1tla
035217151009     c                   movel     fnlvv1ds      kpjbu
035218151009     C                   CALL      'FNLVV1R'
035219151009     C                   PARM                    kpjba
035300920113     C*
035400920113     C     FINE          TAG
035500920113     C*
035600920113     C                   SETON                                        LR
035700920113     C**---------------------------------------------------------
035800920113     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
035900920113     C**---------------------------------------------------------
036000920113     C     CAEX          BEGSR
036100020827     C                   MOVE      ')'           VAR11            11
036200020827     C                   MOVEL     MBRfil        VAR11
036300020827     C                   Z-ADD     51            LUNG             15 5
036400020827     C                   Z-ADD     213           LUNG2            15 5
036500920113     C* CHK E ADD SU TUTTI I FILE FISICI
036600950202     C                   MOVEL     'FNARBD0R'    FISICO           10
036700930212     C                   CALL      'TRUL50C'
036800930212     C                   PARM                    FISICO
036900930212     C                   PARM                    MBRFIL
037000930212     C                   PARM                    LIBSYS
037100930212     C                   PARM                    LOGICO           10
037200930212     C                   PARM                    MBRLOG           10
037300930212     C                   PARM                    ULFLG             1
037400950202     C                   MOVEL     'FNARBD1R'    LOGICO
037500950202     C                   MOVEL     MBRFIL        MBRLOG
037600950202     C                   CALL      'TRUL50C'
037700950202     C                   PARM                    FISICO
037800950202     C                   PARM                    MBRFIL
037900950202     C                   PARM                    LIBSYS
038000950202     C                   PARM                    LOGICO           10
038100950202     C                   PARM                    MBRLOG           10
038200950202     C                   PARM                    ULFLG             1
038300950202     C*
038400950202     C                   CLEAR                   LOGICO
038500950202     C                   CLEAR                   MBRLOG
038600991015     C                   MOVEL     'FIARBT0R'    FISICO
038700930212     C                   CALL      'TRUL50C'
038800930212     C                   PARM                    FISICO
038900930212     C                   PARM                    MBRFIL
039000930212     C                   PARM                    LIBSYS
039100930212     C                   PARM                    LOGICO           10
039200930212     C                   PARM                    MBRLOG           10
039300930212     C                   PARM                    ULFLG             1
039400950201     C                   MOVEL     'FNARBK0R'    FISICO
039500930212     C                   CALL      'TRUL50C'
039600930212     C                   PARM                    FISICO
039700930212     C                   PARM                    MBRFIL
039800930212     C                   PARM                    LIBSYS
039900930212     C                   PARM                    LOGICO           10
040000930212     C                   PARM                    MBRLOG           10
040100930212     C                   PARM                    ULFLG             1
040200950202     C*
040300950201     C                   MOVEL     'FNARBM0R'    FISICO
040400930212     C                   CALL      'TRUL50C'
040500930212     C                   PARM                    FISICO
040600930212     C                   PARM                    MBRFIL
040700930212     C                   PARM                    LIBSYS
040800930212     C                   PARM                    LOGICO           10
040900930212     C                   PARM                    MBRLOG           10
041000930212     C                   PARM                    ULFLG             1
041100950202     C                   MOVEL     'FNARBM1R'    LOGICO
041200950202     C                   MOVEL     MBRFIL        MBRLOG
041300950202     C                   CALL      'TRUL50C'
041400950202     C                   PARM                    FISICO
041500950202     C                   PARM                    MBRFIL
041600950202     C                   PARM                    LIBSYS
041700950202     C                   PARM                    LOGICO           10
041800950202     C                   PARM                    MBRLOG           10
041900950202     C                   PARM                    ULFLG             1
042000950202     C*
042100950202     C                   CLEAR                   LOGICO
042200950202     C                   CLEAR                   MBRLOG
042300991015     C                   MOVEL     'FIARBU0R'    FISICO
042400950202     C                   CALL      'TRUL50C'
042500950202     C                   PARM                    FISICO
042600950202     C                   PARM                    MBRFIL
042700950202     C                   PARM                    LIBSYS
042800950202     C                   PARM                    LOGICO           10
042900950202     C                   PARM                    MBRLOG           10
043000950202     C                   PARM                    ULFLG             1
043100991015     C                   MOVEL     'FIARBU1R'    LOGICO
043200950202     C                   MOVEL     MBRFIL        MBRLOG
043300950202     C                   CALL      'TRUL50C'
043400950202     C                   PARM                    FISICO
043500950202     C                   PARM                    MBRFIL
043600950202     C                   PARM                    LIBSYS
043700950202     C                   PARM                    LOGICO           10
043800950202     C                   PARM                    MBRLOG           10
043900950202     C                   PARM                    ULFLG             1
044000950202     C*
044100950202     C                   CLEAR                   LOGICO
044200950202     C                   CLEAR                   MBRLOG
044300950202     C                   MOVEL     'FNARBG0R'    FISICO
044400950202     C                   CALL      'TRUL50C'
044500950202     C                   PARM                    FISICO
044600950202     C                   PARM                    MBRFIL
044700950202     C                   PARM                    LIBSYS
044800950202     C                   PARM                    LOGICO           10
044900950202     C                   PARM                    MBRLOG           10
045000950202     C                   PARM                    ULFLG             1
045100920113     C*
045200920113     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE FACCIO ADDLFM
045300920113     C                   SETOFF                                       20
045400920212     C                   MOVE      VAR11         CMD1
045500920113     C                   MOVEL     *BLANKS       COMMAN
045600920113     C                   MOVEA     CMD1(1)       COMMAN           80
045700920113     C                   CALL      'QCMDEXC'                            20
045800920113     C                   PARM                    COMMAN
045900920113     C                   PARM                    LUNG
046000920113     C*
046100920113     C   20              DO
046200950202     C                   MOVEA     CM5           C6
046300920113     C*
046400020827     C                   MOVEA     MBRFIL        C6(27)
046500020827     C                   MOVEA     MBRFIL        C6(69)
046600020827     C                   MOVEA     MBRFIL        C6(102)
046700020827     C                   MOVEA     MBRFIL        C6(135)
046800020827     C                   MOVEA     MBRFIL        C6(168)
046900020827     C                   MOVEA     MBRFIL        C6(201)
047000920113     C                   SETOFF                                       20
047100920113     C                   MOVEL     *BLANKS       COMMA2
047200020827     C                   MOVEA     C6            COMMA2          213
047300920113     C                   CALL      'QCMDEXC'                            H1
047400920113     C                   PARM                    COMMA2
047500920113     C                   PARM                    LUNG2
047600920113     C   20              GOTO      ENDCAE
047700920113     C                   END
047800020827
047900020827     C                   MOVE      '))'          VAR12            12
048000020827     C                   MOVEL     MBRfil        VAR12
048100920113     C*
048200920114     C* ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
048300041006     C**                 SETOFF                                       20
048400041006     C**                 MOVEa     VAR12         CM2(40)
048500041006     C**                 MOVEA     'FIARBR3C'    CM2(13)
048600041006     C**                 MOVEL     *BLANKS       COMMAN
048700041006     C**                 MOVEA     CM2           COMMAN
048800041006     C**                 CALL      'QCMDEXC'                            20
048900041006     C**                 PARM                    COMMAN
049000041006     C**                 PARM                    LUNG
049100920113     C*
049200041006     C** 20              GOTO      ENDCAE
049300041006     C**
049400930212     C* ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
049500041006     C**                 SETOFF                                       20
049600041006     C**                 MOVEa     VAR12         CM2(40)
049700041006     C**                 MOVEA     'FNARBM1R'    CM2(13)
049800041006     C**                 MOVEL     *BLANKS       COMMAN
049900041006     C**                 MOVEA     CM2           COMMAN
050000041006     C**                 CALL      'QCMDEXC'                            20
050100041006     C**                 PARM                    COMMAN
050200041006     C**                 PARM                    LUNG
050300041006     C**
050400041006     C** 20              GOTO      ENDCAE
050500041006     C**
050600041006     C**                 SETOFF                                       20
050700041006     C**                 MOVEa     VAR12         CM2(40)
050800041006     C**                 MOVEA     'FNARBD1R'    CM2(13)
050900041006     C**                 MOVEL     *BLANKS       COMMAN
051000041006     C**                 MOVEA     CM2           COMMAN
051100041006     C**                 CALL      'QCMDEXC'                            20
051200041006     C**                 PARM                    COMMAN
051300041006     C**                 PARM                    LUNG
051400041006     C**
051500041006     C** 20              GOTO      ENDCAE
051600041006     C**
051700041006     C**ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
051800041006     C**                 SETOFF                                       20
051900041006     C**                 MOVEa     VAR12         CM2(40)
052000041006     C**                 MOVEA     'FIARBU1R'    CM2(13)
052100041006     C**                 MOVEL     *BLANKS       COMMAN
052200041006     C**                 MOVEA     CM2           COMMAN
052300041006     C**                 CALL      'QCMDEXC'                            20
052400041006     C**                 PARM                    COMMAN
052500041006     C**                 PARM                    LUNG
052600041006     C**
052700041006     C** 20              GOTO      ENDCAE
052800041006     C**OVRDBF
052900041006     C                   SETOFF                                       20
053000020828     C                   MOVEa     VAR11         CM3(41)
053100950202     C                   MOVEA     'FNARBM1R'    CM3(13)
053200920113     C                   MOVEL     *BLANKS       COMMAN
053300920212     C                   MOVEA     CM3(1)        COMMAN
053400920113     C                   CALL      'QCMDEXC'                            20
053500920113     C                   PARM                    COMMAN
053600920113     C                   PARM                    LUNG
053700920212     C*
053800920212     C   20              GOTO      ENDCAE
053900950202     C                   SETOFF                                       20
054000020828     C                   MOVEa     VAR11         CM3(41)
054100991015     C                   MOVEA     'FIARBU1R'    CM3(13)
054200950202     C                   MOVEL     *BLANKS       COMMAN
054300950202     C                   MOVEA     CM3(1)        COMMAN
054400950202     C                   CALL      'QCMDEXC'                            20
054500950202     C                   PARM                    COMMAN
054600950202     C                   PARM                    LUNG
054700950202     C*
054800950202     C   20              GOTO      ENDCAE
054900920214     C* OVRDBF
055000920214     C                   SETOFF                                       20
055100020828     C                   MOVEa     VAR11         CM3(41)
055200950201     C                   MOVEA     'FNARBD1R'    CM3(13)
055300920214     C                   MOVEL     *BLANKS       COMMAN
055400920214     C                   MOVEA     CM3(1)        COMMAN
055500920214     C                   CALL      'QCMDEXC'                            20
055600920214     C                   PARM                    COMMAN
055700920214     C                   PARM                    LUNG
055800920214     C*
055900920214     C   20              GOTO      ENDCAE
056000920212     C*
056100991015     C                   MOVEA     'FIARBR3C'    CM3(13)
056200920212     C                   MOVEL     *BLANKS       COMMAN
056300920212     C                   MOVEA     CM3(1)        COMMAN
056400920212     C                   CALL      'QCMDEXC'                            20
056500920212     C                   PARM                    COMMAN
056600920212     C                   PARM                    LUNG
056601151008     C   20              GOTO      ENDCAE
056602151008      *
056603151008     C                   MOVEA     'FNARBD0R'    CM3(13)
056604151008     C                   MOVEL     *BLANKS       COMMAN
056605151008     C                   MOVEA     CM3(1)        COMMAN
056606151008     C                   CALL      'QCMDEXC'                            20
056607151008     C                   PARM                    COMMAN
056608151008     C                   PARM                    LUNG
056609151008     C   20              GOTO      ENDCAE
056610151008     C                   MOVEA     'FNARBK0R'    CM3(13)
056611151008     C                   MOVEL     *BLANKS       COMMAN
056612151008     C                   MOVEA     CM3(1)        COMMAN
056613151008     C                   CALL      'QCMDEXC'                            20
056614151008     C                   PARM                    COMMAN
056615151008     C                   PARM                    LUNG
056616151008     C   20              GOTO      ENDCAE
056617151008     C                   MOVEA     'FNARBG0R'    CM3(13)
056618151008     C                   MOVEL     *BLANKS       COMMAN
056619151008     C                   MOVEA     CM3(1)        COMMAN
056620151008     C                   CALL      'QCMDEXC'                            20
056621151008     C                   PARM                    COMMAN
056622151008     C                   PARM                    LUNG
056623151008     C   20              GOTO      ENDCAE
056624151008     C                   MOVEA     'FIARBT0R'    CM3(13)
056625151008     C                   MOVEL     *BLANKS       COMMAN
056626151008     C                   MOVEA     CM3(1)        COMMAN
056627151008     C                   CALL      'QCMDEXC'                            20
056628151008     C                   PARM                    COMMAN
056629151008     C                   PARM                    LUNG
056630151008     C   20              GOTO      ENDCAE
056700920113     C*
056800991015     C  N20              OPEN      FIARBR3C
056900950202     C  N20              OPEN      FNARBM1R
057000950201     C  N20              OPEN      FNARBD1R
057100991015     C  N20              OPEN      FIARBU1R
057101151008     C  N20              OPEN      FNARBD0R
057102151008     C  N20              OPEN      FNARBK0R
057103151008     C  N20              OPEN      FNARBG0R
057104151008     C  N20              OPEN      FIARBT0R
057200920113     C*
057300920113     C     ENDCAE        ENDSR
057400920113     C**---------------------------------------------------------
057500920113     C** AGGIORNO BOLLA PARTENZE
057600920113     C**---------------------------------------------------------
057700920113     C     AGGBOL        BEGSR
057800920130     C*
057900941130     C                   SELECT
058000061110     C     FLGCVB        WHENEQ    'F'
058100061110     C                   EXSR      AGARBF
058200061110     C*
058300941130     C     FLGCVB        WHENEQ    'I'
058400920113     C                   EXSR      AGARBI
058500941130     C*
058600941130     C     FLGCVB        WHENEQ    'K'
058700920127     C                   EXSR      AGARBK
058800920228     C*
058900941130     C     FLGCVB        WHENEQ    'T'
059000920113     C                   EXSR      AGARBT
059100920113     C*
059200941130     C     FLGCVB        WHENEQ    '2'
059300920113     C                   EXSR      AGAR2T
059400950202     C*
059500950202     C     FLGCVB        WHENEQ    'M'
059600950202     C                   EXSR      AGARBM
059700950202     C*
059800950202     C     FLGCVB        WHENEQ    'C'
059900950202     C                   EXSR      AGARBC
060000060317     C*
060100060317     C     FLGCVB        WHENEQ    'R'
060200060317     C                   EXSR      AGARBR
060300941130     C                   ENDSL
060400920113     C*
060500930513     C                   ENDSR
060600061110     C**---------------------------------------------------------
060700061110     C** AGGIORNO CODICE VARIAZIONI FX - codice fiscale
060800061110     C**---------------------------------------------------------
060900061110     C     AGARBF        BEGSR
061000061110     c
061100061110     C                   SETOFF                                       3126
061200061110     C                   SETOFF                                       2225
061300061110     C                   MOVE      TASKSC        KSC4              4 0
061400071029     c* Vedo quale bolla aggiornare
061500071029     c                   clear                   bolla             1
061600071029     c* Destinatario
061700071029    1c                   if        arbrd2='D'
061800071029     C* chain su YNCPA01L per aggiornare codice fiscale franco o assegnato
061900071029    2C     *IN27         IFEQ      '1'
062000071029     C     KSC4          ANDEQ     9999
062100071029     C     TASNFT        ANDGT     0
062200071029     c                   eval      bolla ='1'
062300071029    2c                   endif
062400071029     c
062500071029    2C     *IN24         ifeq      '1'
062600071029     C     SAVKS4        ANDEQ     9999
062700071029     C     SAVNFT        ANDGT     0
062800071029     c                   eval      bolla ='2'
062900071029    2c                   endif
063000071029     c
063100071029   x1c                   else
063200071029     c* Mittente
063300071029    2C     *IN27         ifeq      '1'
063400071029     C     KSC4          ANDEQ     8888
063500071029     C     TASNFT        ANDGT     0
063600071029     c                   eval      bolla ='1'
063700071029    2c                   endif
063800071029    1c                   endif
063900071029     c*
064000061110     C*
064100061110     C* chain su YNCPA01L per aggiornare codice fiscale franco o assegnato
064200071029    1c                   if        bolla<>' '
064300061110     C*
064400071029     C* SE SI TRATTA DI storno ignoro
064500071029    2c                   if        bolla='2'
064600061110     C                   MOVEL(P)  SAVTBL        KEY
064700061110     C                   ELSE
064800061110     C                   MOVEL(P)  TASTBL        KEY
064900061113    2C                   ENDIF
065000061110     C*
065100061110     C                   MOVEL     'TB'          COD
065200061110     C     KTAB          CHAIN     TABEL                              32
065300061110     C  N32              MOVEL     TBLUNI        DSTB
065400061110     C   32              MOVEL     *ALL'1'       DSTB
065500071029     c
065600061110     C*  FCB="1" ASSEGNATO BUONO
065700061113    2C     §TBFCB        IFEQ      '1'
065800061110     C*
065900061110     c                   clear                   wdft
066000061113    3c                   select
066100071029     c***                when      *in27
066200071029     c                   when      bolla='1'
066300061110     c                   move      tasnft        wnft
066400061110     c                   if        tasdft > 0
066500061110     c                   move      tasdft        wdft
066600061110     c                   end
066700061110     c                   move      tasksc        wksc
066800071029     c***                when      *in24
066900071029     c                   when      bolla='2'
067000061110     c                   move      savnft        wnft
067100061110     c                   if        savdft > 0
067200061110     c                   move      savdft        wdft
067300061110     c                   end
067400061110     c                   move      savksc        wksc
067500061113    3c                   endsl
067600061110     c
067700061110     C     Kppa          CHAIN     ndppa01l                           26
067800061113    3C     *IN26         IFEQ      *OFF
067900061110     c     kycpa         chain     ndcpa01l                           26
068000061113    4C     *IN26         IFEQ      *OFF
068100071102     c
068200061110     C                   MOVEL     ARBCPI        cpacdfisc
068300071102     c
068400071102     c* Solo se reperita o partita la procedura aggiorno
068500120510     c*****              if        arbrsd<>*blanks or arbdtv>=§vpopiv
068600071029     C                   MOVEL     ARBRSD        cpapartiva
068700120510     c*****              endif
068800071102     c
068900061110     C                   UPDATE    ndcpa000
069000061113    4C                   ENDIF
069100061113    3C                   ENDIF
069200061110     C*
069300061113    2C                   END
069400061113    1C                   END
069500061110     c
069600061110     C* Aggiorna record Q di TITA4
069700061110     C                   movel     'Q'           §trc
069800061110     c                   setoff                                       0506
069900061110     C     ktita         chain     tita430c                           32
070000061113     c* se non c'e' lo scrivo
070100061113    1c                   if        *in32
070200061113     c                   movel     arbaas        ta4aas
070300061113     c                   movel     arblnp        ta4lnp
070400061113     c                   movel     arbnrs        ta4nrs
070500061113     c                   movel     arbnsp        ta4nsp
070600061113     c                   movel     'Q'           ta4trc
070700061113     c                   clear                   ta4not
070800061113    1c                   endif
070900061113     c
071000061110     c* SE mittente a SX , destinatario a DX
071100071029    1c                   if        arbrd2='M'
071200061110     c                   movel     arbcpi        ta4not
071300061110     c                   else
071400061110     c                   move      arbcpi        ta4not
071500061113    1c                   endif
071600061110     c*
071700061113    1c                   if        not *in32
071800061113    2C                   IF        TA4NOT<>*BLANKS
071900061110     C   05              update    tita4000
072000061110     C   06              update    tita4p00
072100061113     C                   ELSE
072200061113     C*SE eliminata e non c'e' più nulla, cancello il record
072300061113     c   05              delete    tita4000
072400061113     c   06              delete    tita4p00
072500061113    2c                   endif
072600061113     c
072700061113   x1c                   else
072800061113    2c                   if        ta4not<>*blanks
072900061113     C  N04              write     tita4000
073000061113     C   04              write     tita4p00
073100061113    2c                   endif
073200061113     c*
073300061113    1c                   endif
073400071029     c
073500071029     c* Aggiorno partita iva
073600071102     c*                      solo se presente o partita la procedura
073700120510    0c******             if        arbrsd<>*blanks or arbdtv>=§vpopiv
073800071029     c* Destinatario --> in titas
073900071029    1c                   if        arbrd2='D'
074000071029     c                   movel     arbrsd        tascpd
074100071029     C* AGGIORNO TITAS
074200071029     C   02              UPDATE    TITAS000
074300071029     C   03              UPDATE    TITAS010
074400071029     C   04              UPDATE    TITASP00
074500071102   x1c                   else
074600071029     c* Mittente --> in titaa
074700071029     c* Aggancio TITAA e aggiorno la partita del mittente
074800071029     c                   movel     'M'           §trc
074900071029     c                   setoff                                       0708
075000071029     c     ktita         chain     titaa30c                           30
075100071102IF  2C                   if        %found(titaa30c)
075200071029     C                   MOVEL     ARBrsd        TAacpi
075300071029     c   07              update    titaa000
075400071029     c   08              update    titaap00
075500071102    2c                   endif
075600071102    1c                   endif
075700071029     C*
075800071029     C* S E C O N D A   B O L L A
075900071029     C*
076000071029     c* solo per destinatario
076100071029    1c                   if        arbrd2='D'  and §3atb2<>'  '
076200071029     C                   MOVEL     §3ATB2        COMTBL
076300071029     C                   SETOFF                                       020304
076400071029     C     KBLP          CHAIN     TiTAS30C                           39
076500071029     c* Aggiorno partita iva
076600071029     c                   movel     arbrsd        tascpd
076700071029     C* AGGIORNO TITAS
076800071029     C   02              UPDATE    TITAS000
076900071029     C   03              UPDATE    TITAS010
077000071029     C   04              UPDATE    TITASP00
077100071102    1C                   END
077200120510    0C*****              END
077300061110     C*
077400061110     C                   ENDSR
077500920113     C**---------------------------------------------------------
077600920113     C** AGGIORNO CODICE VARIAZIONI IX
077700920113     C**---------------------------------------------------------
077800920211     C     AGARBI        BEGSR
077900941130     C                   SETOFF                                       3126
078000941130     C                   SETOFF                                       2225
078100930513     C                   MOVE      TASKSC        KSC4              4 0
078200930513     C*
078300981118     C* chain su YNCPA01L per aggiornare la PIVA e dati destinatario
078400930513    1C     *IN27         IFEQ      '1'
078500930513     C     KSC4          ANDEQ     9999
078600960625     C     TASNFT        ANDGT     0
078700930513     C     *IN24         OREQ      '1'
078800930513     C     SAVKS4        ANDEQ     9999
078900960625     C     SAVNFT        ANDGT     0
079000960625     C*
079100960625     C* SE SI TRATTA DI ASSEGNATO STORNO IGNORO
079200960625     C     *IN24         IFEQ      '1'
079300960625     C     SAVKS4        ANDEQ     9999
079400960625     C                   MOVEL(P)  SAVTBL        KEY
079500960625     C                   ELSE
079600960625     C                   MOVEL(P)  TASTBL        KEY
079700960625     C                   ENDIF
079800960625     C*
079900960625     C                   MOVEL     'TB'          COD
080000960625     C     KTAB          CHAIN     TABEL                              32
080100960625     C  N32              MOVEL     TBLUNI        DSTB
080200960625     C   32              MOVEL     *ALL'1'       DSTB
080300960625     C*  FCB="1" ASSEGNATO BUONO
080400960625   1AC     §TBFCB        IFEQ      '1'
080500930513     C*
080600981118     c                   clear                   wdft
080700981118     c                   select
080800981118     c                   when      *in27
080900981118     c                   move      tasnft        wnft
081000981118     c                   if        tasdft > 0
081100981118     c                   move      tasdft        wdft
081200981118     c                   end
081300981118     c***                move      tasfiv        wfiv
081400981118     c                   move      tasksc        wksc
081500981118     c                   when      *in24
081600981118     c                   move      savnft        wnft
081700981118     c                   if        savdft > 0
081800981118     c                   move      savdft        wdft
081900981118     c                   end
082000981118     c***                move      savfiv        wfiv
082100981118     c                   move      savksc        wksc
082200981118     c                   endsl
082300981118     C     Kppa          CHAIN     ndppa01l                           26
082400960625    2C     *IN26         IFEQ      *OFF
082500990518     c     kycpa         chain     ndcpa01l                           26
082600981118    2C     *IN26         IFEQ      *OFF
082700990518     C                   MOVEL     ARBRSD        cpadescr
082800990518     C                   MOVEL     ARBIND        cpaindiriz
082900981118     C                   MOVEL     ARBCAD        cpacap
083000990518     C                   MOVEL     ARBLOD        cpalocalit
083100981118     C                   MOVEL     ARBPRD        cpaprov
083200981118     c                   movel     'IT  '        cpastato
083300981118     c     arbnzd        ifne      *blank
083400981118     c     arbnzd        andne     'I  '
083500981118     c     arbnzd        andne     'IT '
083600981118     c                   movel     arbnzd        cpastato
083700981118     c                   endif
083800981118     C                   MOVEL     ARBCPI        cpapartiva
083900930513     C*
084000990518     C                   UPDATE    ndcpa000
084100950302    2C                   ENDIF
084200981118    2C                   ENDIF
084300930514     C*
084400960625    1C                   END
084500981118    1C                   END
084600930513     C*
084700950203     C                   MOVEL     §3ATB1        UNOTB1            1
084800920601     C*
084900941130     C* SE CAUSALE VARIAZIONE I0 --> AGGANCIO ARCHIVIO C/ASSEGNI
085000140407    2C*    COMCVB        IFEQ      'I0'
085100140407     C*    §3AFCA        ANDNE     0
085101140407     c                   if        (comcvb='I0' or comcvb='I7' or comcvb='I8'
085102140407     c                             or comcvb='I9') and §3afca<>0
085200930513     C*
085300920212     C                   EXSR      CONCSB
085400941130     C* ERRORE --> NON TROVATO RECORD IN CSB E NON TROVATO LEGAME
085500941130     C   25              GOTO      ENDAGI
085600950227     C* SE IL C/A E' SOLO INCASSATO, POSSO EFFETTUARE LA VARIAIZONE DI
085700950227     C*  DESTINATARIO
085800950227     C  N42CSBBNA        IFEQ      0
085900950227     C                   SETOFF                                       31
086000950227     C                   ENDIF
086100920130     C*
086200950227     C* CONTRASSEGNO ESISTENTE GIA' PAGATO
086300941130    3C     *IN31         IFEQ      '1'
086400930303     C*
086500930303     C* SE ESISTE LEGAME BOLLA (42 ON) NON AGGIORNO ANAGRAFICA C/ASS
086600930303     C* MA AGGIORNO LA BOLLA
086700930303     C*
086800930513    4C     *IN42         IFEQ      '1'
086900930303     C                   SETOFF                                       31
087000930513   X4C                   ELSE
087100941202     C* CONTROLLO SE LA RAGIONE SOCIALE DESTINATARIO E' MODIFICATA
087200941202     C*  --> ERRORE SE C/A GIA' INCASSATO-PAGATO
087300941202     C                   EXSR      CTRDES
087400930513     C*
087500930513    4C                   END
087600920601     C* STAMPO ERRORE
087700941130     C     *IN31         IFEQ      *ON
087800941130     C     *IN42         ANDEQ     *OFF
087900110214     c* ES-14 02 11 -da oggi non stampo più C/Ass pagato per la 102 ma lo stampo
088000110214     c*  per la linea di arrivo se ci fossero problemi o contestazioni la colpa...è sua
088100110214     c* FNARBM non mi serve non accendo il 36
088200130326     c****               movel     taslna        COMFEV
088300110214     C****               SETON                                        36
088400110214     C****               Z-ADD     102           COMFEV
088401130326     c                   z-add     46            COMFEV
088500941130     C                   MOVEL     ERR(10)       ARBDER
088600941130     C                   EXSR      WTRERR
088700930513    3C                   END
088800941130    3C                   END
088900920601     C*
089000941130    3C  N42*IN31         IFEQ      '0'
089100040721     C**  N27
089200040721     C**ANN24              MOVEL     ARBFIN        CSBFIN
089300920115     C                   MOVEL     ARBLOD        CSBPAR
089400920115     C                   MOVEL     ARBPRD        CSBPRO
089500920115     C                   MOVEL     ARBRSD        CSBRSD
089600941130     C                   UPDATE    TNCSB000
089700930513    3C                   END
089800930513    2C                   END
089900920115     C*
090000990625     c                   exsr      agarbi1
090100920113     C*
090200920113     C* S E C O N D A   B O L L A
090300920113     C*
090400930513    3C     §3ATB2        IFNE      '  '
090500920113     C                   MOVEL     §3ATB2        COMTBL
090600941130     C                   SETOFF                                       020304
090700990624     C     KBLP          CHAIN     TiTAS30C                           39
090701121108     c                   exsr      memo_dstitas
090800990628     c                   move      tasksc        ksc4
090900990625     c                   exsr      agarbi1
091000930513    3C                   END
091100941130     C*
091200941130     C     ENDAGI        TAG
091300920113     C*
091400920113     C                   ENDSR
091500990625     C**---------------------------------------------------------
091600990625     C** AGGIORNO CODICE VARIAZIONI IX
091700990625     C**---------------------------------------------------------
091800990630     C     AGARBI1       BEGSR
091900990628     C* I SEGUENTI CAMPI LI AGGIORNO SOLO SE 1 BOLLA NON CONTABILIZZATA
092000990628    1C     *in27         IFEQ      *off
092100990628     C                   Z-ADD     ARBPKb        TASPKB
092200990628     C                   Z-ADD     ARBPKF        TASPKf
092300990628     C                   movel     'R'           TASFPF
092400990625     C*
092500990628    2C     TASFVF        IFNE      'T'
092600990625     C     TASVLF        ANDNE     ARBVLF
092700990628    3C     TASFVF        IFEQ      'Z'
092800990625     C     TASVLF        ANDLT     ARBVLF
092900990625     C     TASFVF        ORNE      'Z'
093000990625     C                   Z-ADD     ARBVLF        TASVLF
093100990625     C                   MOVEL     ARBFVF        TASFVF
093200990625     C                   Z-ADD     ARBVLB        TASVLB
093300990625     C                   MOVEL     ARBFVB        TASFVB
093400990628    3C                   END
093500990628    2C                   END
093600990625     C*
093700990630    2c     ksc42bo       ifne      9999
093800990625     C* DETERMINO IL CODICE DI TASSAZIONE
093900990630    3C     COMCVB        ifeq      'I0'
093901140407    3C     COMCVB        oreq      'I7'
093902140407    3C     COMCVB        oreq      'I8'
093903140407    3C     COMCVB        oreq      'I9'
094000990628     c* la call al tisi mi basta farla solo la prima volta e cioe quando
094100990625     c* sono sulla prima bolla
094200990630    4c     comtbl        ifeq      §3atb1
094201120322     c                   clear                   savcts
094202120322     c                   exsr      c_tisi95
096800120322     C* SE ERRORE da tisi95R LO SCRIVO NEL FILE DEGLI ERRORI E NON LO ANDRO'
096900990625     C* A SOSTITUIRE
097000990630    5C     O95ERR        IFNE      *BLANKS
097100990625     C                   MOVEL     ERR(30)       ARBDER
097200990625     C                   Z-ADD     102           COMFEV
097300990625     C                   EXSR      WTRERR
097400990630   X5C                   ELSE
097500990630     C                   MOVEL     O95CTS        SAVCTS
097600990630    5C                   END
097700990630    4c                   END
097800990630    4c     SAVCTS        IFNE      *BLANKS
097900990630     C                   MOVEL     SAVCTS        TASCTS
098000990630    4C                   END
098100990630    3C                   END
098200990625
098300990625     C                   MOVEL     ARBFIN        TASFIN
098400990625     C                   MOVEL     ARBCAD        TASCAD
098500990625     C                   MOVEL     ARBNZD        TASNZD
098600990630    2C                   END
098700990630    1C                   END
098800990625     C*
098900990625RM*  C                   MOVEL     ARBFFD        TASFFD
099000990625     C*
099100990628     c* memorizzo se sono variati inoltro,cap o nazione
099200990625     c                   clear                   flgmod
099300990628    1c     arbfin        ifne      tasfin
099400990625     c     arbcad        orne      tascad
099500990625     c     arbnzd        orne      tasnzd
099600990625     c                   move      '1'           flgmod            1
099700990628    1c                   endif
099800990625     c
099900990628     c* Se ho la seconda bolla contabilizzata 9999 non aggiornerò mai i
100000990628     c* i seguenti campi nemmeno sulla prima bolla.
100100990630     c* Dopodichè posso aggiornarli solo se la 1bolla non è contabilizzata
100200990628     c* o se non sono stati modificati inoltro cap e nazione
100300990628    1c     ksc42bo       ifne      9999
100400990628    2c     *in27         ifeq      *off
100500990628     c     flgmod        oreq      *blank
100600990625     C                   MOVEL(P)  ARBRSD        TASRSD
100700990625     C                   MOVEL(P)  ARBIND        TASIND
100800990625     C                   MOVEL(P)  ARBLOD        TASLOD
100900990625     C                   MOVEL(P)  ARBPRD        TASPRD
101000990701     c* Dalla filiale posso ricevere la partita iva del destinatario
101100990701     c* solo se la bolla è assegnato o solo in caso di doppia bolla
101200990701     c* In ogni caso vado SEMPRE in sostituzione di tascpd
101300990625     C                   MOVEL     ARBCPI        TASCPD
101400140219    2c                   endif
101401140219    1c                   endif
101500990625     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
101600130325     C***                MOVEL     ERR(9)        ARBDER
101700130325     c***                exsr      wtrerr
101701140219     c* La scrittura/aggiornamento del tivrb viene fatta sempre se la
101702140219     c* prima bolla è contabilizzata.
101703140219     c* Viene fatta a prescindere dal contenuto di FLGMOD perchè
101704140219     c* deve receprire anche eventuali variazioni che riportano
101705140219     c* i dati alla situazione originale ciè quella di titas
101706140219     c* Es: se prima vario inoltor da "P" a "I" e poi rimetto
101707140219     c* "P" devo receprie sul tivrb anche quest'ultima variazione
101708140219    1c                   if        *in27=*on
101710120321     c                   exsr      ctr_vrb
101800140219    1c                   END
101900140219   x1c*                  else
102000990628     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
102100130325     C***                MOVEL     ERR(9)        ARBDER
102200130325     c***                exsr      wtrerr
102205140219     c*                  exsr      ctr_vrb
102300140219    1c*                  END
102400990625     C* AGGIORNO TITAS
102500990625     C   02              UPDATE    TITAS000
102600990625     C   03              UPDATE    TITAS010
102700990625     C   04              UPDATE    TITASP00
102800990625     C*
102900990625     c                   endsr
102901120322     C**---------------------------------------------------------
102902120322     C** Richiamo tisi95 per determinazione codice tassazione
102903120322     C**---------------------------------------------------------
102904120322     C     C_tisi95      BEGSR
102906120322     C                   CLEAR                   DSSI95
102907120322     C                   MOVEL     ARBCAD        I95CAP
102908120322     C                   MOVEL     ARBNZD        I95NAR
102909120322     C                   MOVEL     ARBLOD        I95LOC
102910120322     C                   MOVEL     ARBPRD        I95PRV
102911121116     c                   movel     tasaas        i95dat
102912121116     c                   move      tasmgs        i95dat
102913121116     C****               MOVEL     TASDSP        I95DAT
102915120322     C                   MOVEL     '7'           I95TCN
102916120322      * Se network FedEx passo flag apposito per dire di prendere i dati
102917120322      * dalla tabella della nazione
102918120322     c     taslna        chain     azorg01l
102919120322     c                   if        %found(azorg01l) and orgfva = *blanks
102920120322     c                   eval      og143 = orgde3
102921120322     c     §ogntw        ifeq      'FED'
102922120322     c                   eval      i95fi1 = 'S'
102923120322     c                   endif
102924120322     c                   endif
102925120322
102926120322     C                   MOVEL     §3ATB1        I95TPO
102927120322    5C     §3AFCA        IFNE      0
102928120322     C                   MOVEL     'S'           I95FCA
102929120322    5C                   ENDIF
102930120322     C**
102931120322     C                   CALL      'TISI95R'
102932120322     C                   PARM                    DSSI95
102933120322     c                   endsr
103000950202     C**---------------------------------------------------------
103100950202     C** AGGIORNO CODICE VARIAZIONI MX
103200950202     C**---------------------------------------------------------
103300950202     C     AGARBM        BEGSR
103400950203     C* VEDO SE ESISTE CODICE
103500950202     C                   MOVE      ARBCCM        KSC4
103600950202     C*
103700950202     C* SE E' CODIFICATO CHAIN SU ANAGRAFICA
103800950202     C     KSC4          IFNE      8888
103900981001     C                   CLEAR                   DSBS69
104000981001     C                   MOVEL     KNSIF         I69SIF
104100981001     C                   MOVEL     KCI           I69KCC
104200981001     C                   MOVEL     ARBCCM        I69KAC
104300981001     C                   MOVEL     ARBCCM        I69KIN
104400981001     C                   CALL      'TIBS69R'
104500981001     C                   PARM                    DSBS69
104600981001     C                   PARM                    DSACO
104700981001     C                   PARM                    DSIND
104800981001     C                   PARM                    DSCLP
104900981001     C                   PARM                    DSCLS
105000950202     C*
105100950203     C* DO ERRORE SOLO SE SI TRATTA DI CLIENTE DI ALTRA FILIALE
105200981001     C     O69ERR        IFNE      ' '
105300950203     C* CONFRONTO LE 2 FILIALI ELABORATORE
105400971215     C**         ARBLNP    CHAINAZORG                30
105500971215     C**      N30          MOVELORGFEL    LNPFEL  30
105600971215     C**       30          MOVELARBLNP    LNPFEL
105700971215     C**
105800971215     C**                   MOVELARBCCM    W0030   30
105900971215     C**         W0030     CHAINAZORG                30
106000971215     C**      N30          MOVELORGFEL    KSCFEL  30
106100971215     C**       30          MOVELW0030     KSCFEL
106200971215     C*
106300971215     C**         KSCFEL    IFNE LNPFEL
106400971215     C**                   MOVELERR,27    ARBDER
106500971215     C**                   EXSR WTRERR
106600950203     C*
106700971215     C**                   ELSE
106800950203     C* SE STESSA FIL ELABORATORE: NON CANCELLO LA VARIAZIONE
106900950203     C*  E ATTENDO L'ARRIVO DEL PDC
107000950203     C                   SETON                                        37
107100971215     C**                   ENDIF
107200950203     C*
107300950202     C                   GOTO      ENDGBM
107400960919     C                   ELSE
107500960919     C* SE CLIENTE BLOCCATO CARICO LO STESSO MA SEGNALO
107600100303     C***  ACOABL        IFEQ      '8'
107700100303     C***                MOVEL     ERR(29)       ARBDER
107800100303     C***                EXSR      WTRERR
107900100303     C***                END
108000950202     C                   ENDIF
108100950202     C                   ENDIF
108200950203     C*
108300950203     C* VERIFICO ESISTENZA NAZIONE E PROVINCIA MITTENTE
108400970714     C                   CLEAR                   DSSI95
108500970714     C                   MOVEL     ARBCAM        I95CAP
108600121116     C                   MOVEL     ARBNZM        I95NAR
108601121116     c                   movel     tasaas        i95dat
108602121116     c                   move      tasmgs        i95dat
108700121116     C****               MOVEL     TASDSP        I95DAT
108800970714     C                   MOVEL     '3'           I95TCN
108900020311      * Se network FedEx passo flag apposito per dire di prendere i dati
109000020311      * dalla tabella della nazione
109100020311     c     arblnp        chain     azorg01l
109200020311     c                   if        %found(azorg01l) and orgfva = *blanks
109300020311     c                   eval      og143 = orgde3
109400020311     c     §ogntw        ifeq      'FED'
109500020311     c                   eval      i95fi1 = 'S'
109600020311     c                   endif
109700020311     c                   endif
109800020311
109900970714     C                   CALL      'TISI95R'
110000970714     C                   PARM                    DSSI95
110100950203     C*
110200970714     C     O95ERR        IFNE      *BLANKS
110201130326     C                   Z-ADD     102           COMFEV
110300950203     C                   MOVEL     ERR(26)       ARBDER
110400950203     C                   EXSR      WTRERR
110500950203     C                   GOTO      ENDGBM
110600950203     C                   ENDIF
110700950203     C*
110800950203     C* VERIFICO SE ESISTEN L'ANAGRAFICA DEI C/A E SE GIA' INCASSATO
110900950222    1C     §3AFCA        IFNE      0
111000950202     C                   EXSR      CONCSB
111100950222     C* INESISTENTE  --> NON AGGIORNO
111200950222     C   25              GOTO      ENDGBM
111300950203     C***
111400950203     C*** A G G I O R N O   C / A S S E G N O
111500950222     C* AGGIORNO SOLO SE NON E' INCASSATO
111600950222   1AC     *IN31         IFEQ      *OFF
111700950202     C     KANN          CHAIN     TNCSA000                           30
111800950202     C*
111900950202     C* NON E' 8888 :SE CREATA CANCELLO ANAGRAFICA C/A
112000950202IF  2C     KSC4          IFNE      8888
112100970916IF  2C     KSC4          ANDNE     9999
112200950202     C  N30              DELETE    TNCSA000
112300950202X   2C                   ELSE
112400950202     C                   CLEAR                   TNCSA000
112500950202     C*
112600950202     C                   MOVEL     CSBAAS        CSAAAS
112700950202     C                   MOVEL     CSBLNP        CSALNP
112800950202     C                   MOVEL     CSBNRS        CSANRS
112900950202     C                   MOVEL     CSBNSP        CSANSP
113000950202     C                   MOVEL     ARBRSM        CSARSD
113100950202     C                   MOVEL     ARBINM        CSAVID
113200950202     C                   MOVEL     ARBCAM        CSACAD
113300950202     C                   MOVEL     ARBLOM        CSACID
113400950202     C                   MOVEL     ARBPRM        CSAPRM
113500950202     C                   MOVEL     ARBNZM        CSANZM
113600950202     C*
113700950202     C  N30              UPDATE    TNCSA000
113800950202     C   30              WRITE     TNCSA000
113900950202E   2C                   ENDIF
114000950202     C*
114100950202     C                   MOVEL     ARBCCM        CSBCDI
114200950202     C*
114300950202     C* SE C/A SU 1 BOLLA IMPOSTO CODICE BOLLA AMCHE
114400950202    2C     §3AFCA        IFEQ      1
114500950202     C                   MOVEL     ARBCCM        CSBKSC
114600950202    2C                   ENDIF
114700950202     C*
114800950202     C                   UPDATE    TNCSB000
114900950222   1AC                   ENDIF
115000950222    1C                   ENDIF
115100950202     C*
115200950203     C* A G G I O R N O   L A   1  B O L L A
115300970714     C                   MOVEL     O95CTS        TASMCT
115400950202     C                   MOVEL     ARBCAM        TASCAM
115500950202     C                   MOVEL     ARBNZM        TASNZM
115600950202     C                   MOVEL     ARBFAP        TASFAP
115700950202     C                   MOVEL     ARBCCM        TASKSC
115800990628     C                   MOVEL     ARBCCM        TASccm
115900950202     C*
116000950202     C*
116100990628     c                   movel     'M'           §trc
116200990629     c     ktita         chain     titaa000                           30
116300950202IF  3C     KSC4          IFEQ      8888
116400990628     c   30              z-add     tasaas        taaaas
116500990628     c   30              z-add     taslnp        taalnp
116600990628     c   30              z-add     tasnrs        taanrs
116700990628     c   30              z-add     tasnsp        taansp
116800990628     c   30              movel     §trc          taatrc
116900990628     C                   MOVEL     ARBRSM        TAaRSc
117000990628     C                   MOVEL     ARBINM        TAaIND
117100990628     C                   MOVEL     ARBCAM        TAaCAP
117200990628     C                   MOVEL     ARBLOM        TAaLOc
117300990628     C                   MOVEL     ARBPRM        TAaPRV
117400990628     C                   MOVEL     ARBNZM        TAaNAZ
117500990628     C                   MOVEL     ARBCPI        TAacpi
117600990628     c  n30              update    titaa000
117700990628     c   30              write     titaa000
117800990628     c                   else
117900990628     c* se prima della variazione era un 8888 lo vado a cancellare da
118000990628     c* titaa00f
118100990628     c  n30              delete    titaa000
118200990628     c
118300950202E   3C                   ENDIF
118400950202     C*
118500990628     C   02              UPDATE    TiTAS000
118600071029     C***
118700950203     C*** A G G I O R N O   2   B O L L A
118800950202    3C     §3ATB2        IFNE      '  '
118900950202     C                   MOVEL     §3ATB2        COMTBL
119000950202     C                   SETOFF                                       020304
119100990628     C     KBLP          CHAIN     TiTAS30C                           39
119200950202     C*
119300950203     C     *IN39         IFEQ      *OFF
119400950202     C                   MOVEL     ARBCCM        TASCCM
119500950202     C*
119600950202     C                   MOVEL     ARBCAM        TASCAM
119700950202     C                   MOVEL     ARBNZM        TASNZM
119800950202     C                   MOVEL     ARBFAP        TASFAP
119900970714     C                   MOVEL     O95CTS        TASMCT
120000950202     C*
120100990628     C   02              UPDATE    TiTAS000
120200990628     C   03              UPDATE    TiTAS010
120300990628     C   04              UPDATE    TiTASP00
120400950202     C                   ENDIF
120500950202     C                   ENDIF
120600950202     C*
120700950202     C     ENDGBM        ENDSR
120800950202     C**---------------------------------------------------------
120900950202     C** AGGIORNO CODICE VARIAZIONI CX
121000950202     C**---------------------------------------------------------
121100950202     C     AGARBC        BEGSR
121200010126     C     arblnp        ifne      SAVLNP
121300010126     C     arblnp        chain     azorg01l                           32
121400010126     C  n32              movel     orgde3        og143
121500010126     C   32              clear                   og143
121600010126     C                   z-add     arblnp        savlnp
121700010126     C                   endif
121800010126     C* SE BOLLA POSTE ED AGGIUNTO O TOLTO IL FUORI MISURA BISOGNA
121900010126     C* AGGIORNARE tita4 TIPO RECORD "J"
122000010126     C     §ogpt         ifeq      'S'
122100010126     C                   clear                   wtcp
122200010126     C                   clear                   wsvtcp
122300010126    2C     arbtc1        ifeq      'F'
122400010126     C     arbtc2        oreq      'F'
122500010126     C                   movel     'F'           wtcp
122600010126    2C                   endif
122700010126    2C     tasftc        ifeq      'F'
122800010126     C     tastc2        oreq      'F'
122900010126     C                   movel     'F'           wsvtcp
123000010126    2C                   endif
123100010126    2C                   select
123200010126     C* SE NON E' STATA NE' AGGIUNTA NE' TOLTA LA "F" NON FACCIO NIENTE
123300010126     C     arbtc1        whenne    'F'
123400010126     C     arbtc2        andne     'F'
123500010126     C     tasftc        andne     'F'
123600010126     C     tastc2        andne     'F'
123700010126     C     wtcp          oreq      'F'
123800010126     C     wsvtcp        andeq     'F'
123900010126     C                   other
124000010126     C                   movel     'J'           §trc
124100010126     c                   setoff                                       0506
124200010126     C     ktita         chain     tita430c                           32
124300010126    3C     *in32         ifeq      *off
124400010126     C                   movel     ta4not        dsbl4j
124500010126     C* TOLTO LA 'F'
124600010126    4C     wtcp          ifeq      *blanks
124700010126     C     wsvtcp        andeq     'F'
124800010126     C                   clear                   §b4bai
124900010126   X4C                   else
125000010126     C* AGGIUNTA 'F'
125100010126    5C     wtcp          ifeq      'F'
125200010126     C     wsvtcp        andeq     *blanks
125300010126     C                   movel     'S'           §b4bai
125400010126    5C                   endif
125500010126    4C                   endif
125600010126     C                   movel     dsbl4j        ta4not
125700010126     C   05              update    tita4000
125800010126     C   06              update    tita4p00
125900010126   X3C                   else
126000010126     C* AGGIUNTA 'F' --> CREO RECORD "J"
126100010126    4C     wtcp          ifeq      'F'
126200010126     C     wsvtcp        andeq     *blanks
126300010126     C                   clear                   tita4000
126400010126     C                   z-add     tasaas        ta4AAS
126500010126     C                   z-add     taslnp        ta4LNP
126600010126     C                   z-add     tasnrs        ta4NRS
126700010126     C                   z-add     tasnsp        ta4NSP
126800010126     C                   movel     'J'           ta4TRC
126900010126     C                   clear                   dsbl4j
127000010126     C                   clear                   ta4not
127100010126     C                   movel     'S'           §B4BAI
127200010126     C                   movel     dsbl4j        ta4NOT
127300010126     C  N04              write     tita4000
127400010126     C   04              write     tita4p00
127500010126    4C                   endif
127600010126    3C                   endif
127700010126    2C                   endsl
127800010126     C                   endif
127900950202     C* AGGIORNO SEMPRE  LE CONSEGNE PARTICOLARI SOLO SU  1 BOLLA
128000991018     c* idem per gg chiusura e data consegna richiesta
128100950202     C                   MOVEL     ARBTC1        TASFTC
128200950202     C                   MOVEL     ARBTC2        TASTC2
128300991018     c                   movel     arbtcr        tastcr
128400991018     c                   z-add     arbdcr        tasdcr
128500991018     c                   z-add     arbhcr        tashcr
128600991018     C                   MOVEL     arbgC1        tasgc1
128700991018     C                   MOVEL     arbgC2        tasgc2
128800990628     C   02              UPDATE    TiTAS000
128900990628     C   03              UPDATE    TiTAS010
129000990628     C   04              UPDATE    TiTASP00
129100950202     C                   ENDSR
129200950202     C*
129300920113     C**---------------------------------------------------------
129400920113     C** AGGIORNO CODICE VARIAZIONI TX
129500920113     C**---------------------------------------------------------
129600920113     C     AGARBT        BEGSR
129700920113     C*
129800920226     C                   EXSR      CTR2
129900920226     C*
130000941130    1C  N29              DO
130100950202     C*
130200991102     c
130300990419     c* controllo l'importo da assicurare se la spedizione e' tassata
130400990419     c*  o con tot.imponibile
130500011029     c                   if        tasias > 0  and tasvas = *blanks
130600011029     c                   movel     'ITL'         tasvas
130700011029     c                   endif
130800990419     c                   if        arbias <> tasias or
130900990419     c                             arbvas <> tasvas
131000990419     c*
131100990419     c                   exsr      AGGIAS
131200990419     c                   endif
131300950202     C*
131400950202     C                   Z-ADD     ARBIAS        TASIAS
131500950202     C                   MOVEL     ARBVAS        TASVAS
131600990824     c                   clear                   TASFCM
131700920113     C*
131800941130    2C     §3ATB2        IFEQ      '  '
131900970514     C     COMCVB        OREQ      'TP'
132000950222     C* AGGIORNO KSC SOLO SE VARIAZIONE DA ARRIVO E NON DA PARTENZA
132100950222     C*  PERCHE' SI TRATTA DI VARIAZIONE DI MITTENTE E NON DI TASSAZION
132200950222     C     ARBCVB        IFNE      'TP'
132300920113     C                   Z-ADD     ARBKSC        TASKSC
132400950222     C                   ENDIF
132500051219     c* Non vado sopra al codice tariffa se bolla in assegnato tassata con
132600051219     c* tariffa mittente
132700051219     c     tasftm        ifeq      *blanks
132800051219     C     comcvb        oreq      'TP'
132900950222     C                   MOVEL     ARBCTR        TASCTR
133000051219     c                   endif
133100950202     C                   Z-ADD     ARBQFT        TASQFT
133200991015     c                   MOVEL     ARBDIV        TASDIV
133300920113     C                   Z-ADD     ARBPOR        TASPOR
133400920113     C                   MOVEL     ARBSV1        TASSV1
133500920113     C                   Z-ADD     ARBVA1        TASVA1
133600920113     C                   MOVEL     ARBSV2        TASSV2
133700941130     C                   Z-ADD     ARBVA2        TASVA2
133800950202     C                   MOVEL     ARBSV3        TASSV3
133900950202     C                   Z-ADD     ARBVA3        TASVA3
134000941130     C*
134100990628     C* SE ESITONO GIA' DELLE VARIE IN TITA700F LE CANCELLO
134200990628     C     KBLP          SETLL     TITA7000
134300990628     C     KBLP          READE     TITA7000                               30
134400941130     C*
134500941130    3C     *IN30         DOWEQ     *OFF
134600991015     C                   DELETE    TITA7000
134700991015     C     KBLP          READE     TITA7000                               30
134800941130    3C                   ENDDO
134900991015     C     KARBU         SETLL     FIARBU1R
135000991015     C     KARBU         READE     FIARBU1R                               30
135100950202     C*
135200950202     C     *IN30         DOWEQ     *OFF
135300990628     C                   CLEAR                   TiTA7000
135400950202     C                   Z-ADD     ARBAAS        TA7AAS
135500950202     C                   Z-ADD     ARBLNP        TA7LNP
135600950202     C                   Z-ADD     ARBNRS        TA7NRS
135700950202     C                   Z-ADD     ARBNSP        TA7NSP
135800950202     C                   MOVEL     TASTBL        TA7TBL
135900950202     C                   MOVEL     ARBSVN        TA7SVN
136000950202     C                   Z-ADD     ARBVAN        TA7VAN
136100990629     C                   WRITE     TITA7000
136200950202     C*
136300950202     C                   ADD       1             NUMRBU            7 0
136400991015     C                   DELETE    FIARBURR
136500950202     C*
136600991015     C     KARBU         READE     FIARBU1R                               30
136700950202     C                   ENDDO
136800990628     c
136900941130     C*
137000920113     C                   Z-ADD     ARBDFT        TASDFT
137100920113     C                   Z-ADD     ARBNFT        TASNFT
137200920113     C                   Z-ADD     ARBFIV        TASFIV
137300941130     C                   MOVEL     ARBCEI        TASFEI
137400920113     C*
137500990624     C                   z-add     arbimv        TASimv
137600941130    3C     ARBIFT        IFGT      0
137700941130     C                   MOVEL     '1'           TASFTS
137800920113     C                   ELSE
137900941130     C                   MOVEL     ' '           TASFTS
138000941130    3C                   END
138100941130    2C                   END
138200920113     C* AGGIORNO
138300990628     C   02              UPDATE    TiTAS000
138400941130    1C                   ENDDO
138500920113     C*
138600920113     C                   ENDSR
138700920113     C**---------------------------------------------------------
138800920113     C** AGGIORNO CODICE VARIAZIONI 2T
138900920113     C**---------------------------------------------------------
139000920113     C     AGAR2T        BEGSR
139100920113     C*
139200920226     C* SECONDA BOLLA
139300920113     C     §3ATB2        IFNE      '  '
139400920226     C                   EXSR      CTR2
139500920114     C*
139600920226     C  N29              DO
139700991015     C                   Z-ADD     ARBKSC        TASKSC
139800991015     C                   MOVEL     ARBCTR        TASCTR
139900991015     C                   movel     arbdiv        tasdiv
140000991015     C                   MOVEL     ARBSV1        TASSV1
140100991015     C                   Z-ADD     ARBVA1        TASVA1
140200991015     C                   MOVEL     ARBSV2        TASSV2
140300991015     C                   Z-ADD     ARBVA2        TASVA2
140400040114     c                   Movel     ArbSv3        TasSv3
140500040114     c                   Z-Add     ArbVa3        TasVa3
140600040114      * Se esistono già della varie su Tita7 le devo eliminare
140700040114     c     Kblp          Setll     Tita7000
140800040114     c                   Do        *Hival
140900040114     c     Kblp          Reade     Tita7000
141000040114     c                   If        %Eof
141100040114     c                   Leave
141200040114     c                   EndIf
141300040114     c                   Delete    Tita7000
141400040114     c                   EndDo
141500040114      * Scrivo il nuovo Tita7 e cancello il file di ricezione
141600040114     c     Karbu         Setll     Fiarbu1r
141700040114     c                   Do        *Hival
141800040114     c     Karbu         Reade     Fiarbu1r
141900040114     c                   If        %Eof(Fiarbu1r)
142000040114     c                   Leave
142100040114     c                   EndIf
142200040114     c                   Clear                   Tita7000
142300040114     c                   Z-add     ArbAas        Ta7Aas
142400040114     c                   Z-add     ArbLnp        Ta7Lnp
142500040114     c                   Z-add     ArbNrs        Ta7Nrs
142600040114     c                   Z-add     ArbNsp        Ta7Nsp
142700040114     c                   Movel     TasTbl        Ta7Tbl
142800040114     c                   Movel     ArbSvn        Ta7Svn
142900040114     c                   Z-add     ArbVan        Ta7Van
143000040114     c                   Write     Tita7000
143100040114     c                   Add       1             NumRbu
143200040114     c                   Delete    Fiarburr
143300040114     c                   EndDo
143400040114
143500991015     C                   Z-ADD     ARBDFT        TASDFT
143600991015     C                   Z-ADD     ARBNFT        TASNFT
143700991015     C                   Z-ADD     ARBFIV        TASFIV
143800991015     C                   MOVEL     ARBCEI        TASFEI
143900040114     C**!!!              z-add     arbva1        TASimv
144000040114     c                   Z-add     ArbImv        TasImv
144100991015     C     ARBIFT        IFGT      0
144200941130     C                   MOVEL     '1'           TASFTS
144300920114     C                   ELSE
144400941130     C                   MOVEL     ' '           TASFTS
144500920114     C                   END
144600920113     C*
144700040114      * Aggiorno Titas
144800990628     C   02              UPDATE    TiTAS000
144900920113     C                   END
145000920130     C                   END
145100920113     C*
145200920113     C                   ENDSR
145300920127     C**---------------------------------------------------------
145400920127     C** AGGIORNO CODICE VARIAZIONI KX
145500920127     C**---------------------------------------------------------
145600920127     C     AGARBK        BEGSR
145601121109     c                   if        *in11='0' and *in16='0'
145602121109     c                   exsr      memo_csb
145603121109     c                   endif
145604121205     c                   clear                   wanncas
145700920212     C                   SETOFF                                       31
145800941130     C                   CLEAR                   TNCSB000
145900950202     C* NON AGGIORNO SE HO LETTO
146000950202    1C     *IN11         IFEQ      *OFF
146100950202     C     *IN16         ANDEQ     *OFF
146200920127     C* VARIA CONTRASSEGNO
146300941130    2C     ARBCBP        IFEQ      ARBCBN
146400920127     C* VEDO CHE TIPO BOLLA AGGANCIARE DAL FLAG C/ASSEGNO
146500920131     C* RIAGGANCIO SOLO LA SECONDA PERCHE' LA PRIMA L'HO GIA' AGGANCIAT
146600941130    3C     §3AFCA        IFEQ      2
146700920127     C                   MOVEL     §3ATB2        COMTBL
146800941130     C                   SETOFF                                       020304
146900990625     C     KBLP          CHAIN     TITAS30C                           39
146901121108     c                   exsr      memo_dstitas
147000941130    3C                   END
147100920228     C*
147200920228     C* CONTROLLO SPEDIZIONE SE OK
147300990414     C                   EXSR      CTR3
147400920210     C*
147500920212     C* CONTROLLO ARCHIVIO C/ASSEGNO SE ERA PREVISTO
147600941130    3C     §3AFCA        IFNE      0
147700920212     C                   EXSR      CONCSB
147800941130    3C                   END
147900920210     C* SE NO ERRORE
148000950324    3C  N31CSBDDC        IFEQ      0
148100950324     C     CSBBNA        ANDEQ     0
148200941130     C                   MOVEL     *BLANKS       ARBDER
148300941130     C*
148400920228     C*** AGGIORNO CONTRASSEGNO
148500920228     C*** PRIMA     ESISTEVA ORA     ESISTE: V A R I A Z I O N E
148600941130    4C     CSBCAS        IFNE      0
148700950202     C     ARBCAS        ANDNE     0
148800950202     C* SCRIVO SOLO SE IMPORTO DIVERSO
148900950202    5C     ARBCAS        IFNE      CSBCAS
149000991103    5C     ARBVCA        ORNE      WCSBVCA
149100000703     c*
149200000703     c     csbsta        oreq      9
149300920212     C* SCRIVO STORICO
149400920212     C                   MOVEL     'VACA'        CSVCAV
149500920212     C                   EXSR      CSVWTR
149600950202     C*
149700950202     C                   EXSR      AGGCSB
149800950202     C*
149900941130     C                   UPDATE    TNCSB000
150000920228     C*
150100120420     C* E R R O R E : NON MODIFICABILE IMPORTO VARIA G
150200130325     C***29              MOVEL     ERR(22)       ARBDER
150202120322     C   29              exsr      ctr_vrb
150300941130    5C                   END
150400941130    4C                   END
150500920228     C***
150600920228     C*** PRIMA     ESISTEVA ORA NON ESISTE: A N N U L L O
150700941130    4C     CSBCAS        IFNE      0
150800950202     C     ARBCAS        ANDEQ     0
150900920212     C                   EXSR      ANNCSB
151000920226     C*
151100920228     C* E R R O R E : NON ELIMINABILE IMPORTO VARIA G
151200920316     C                   MOVEL     *BLANKS       ARBDER
151300990505    5C   29              DO
151400941130     C* CONTROLLO SE ESISTE VARIA "G"
151500130326     C****               EXSR      ESVARG
151600130325    6C***  WSIVAR        IFEQ      'S'
151700130325     C***                MOVEL     ERR(14)       ARBDER
151800130325    6C***                END
151802121205     c                   eval      wanncas='S'
151803121203     c                   exsr      ctr_vrb
151900941130    5C                   END
152000941130    4C                   END
152100920228     C***
152200920228     C*** PRIMA NON ESISTEVA ORA     ESISTE: C R E O  RECORD C/ASSEGNO
152300941130    4C     CSBCAS        IFEQ      0
152400950202     C     ARBCAS        ANDNE     0
152500920212     C                   MOVEL     §3ATB1        TBLCA
152600941130     C                   MOVEL     §3ATB1        FLGTB1
152700941130     C*
152800941130     C                   MOVEL     TASLOD        DESPAR
152900990625     C                   MOVEL     TASPRD        DESPRV
153000950913     C*
153100920212     C                   EXSR      CRTCSB
153200920228     C* E R R O R E : NON POSSO AGGIUNGERE IMPORTO VARIA G
153300130325     C***29              MOVEL     ERR(18)       ARBDER
153400130325     c***29              setoff                                       36
153402120322     C   29              exsr      ctr_vrb
153500941130    4C                   END
153600920228     C***
153700920228     C*** AGGIORNO SPEDIZIONE
153800941130     C                   MOVEL     'S'           TASFCA
153900920228     C* SE GIA' TASSATA METTO A POSTO PER VARIA G
154000110203     c                   if        *in38
154100110203     c                   Z-ADD     arbcas        wcas
154200110203     C                   EXSR      VARIAG
154300110203     c
154400110203      * riorganizzo le varie se è necessario
154500110203     c                   if        flgriorg = '1'
154600110203     c                   exsr      sr_riorg
154700110203     c                   endif
154800110203     c                   endif
154900110203
155000950202     C* SCRIVO ERRORE SE GIA' CONTABILIZZATA/STORICIZZATA
155100941130    4C     ARBDER        IFNE      *BLANKS
155200920309     C                   EXSR      WTRERR
155300941130    4C                   END
155400920228     C*
155500990625     C   02              UPDATE    TITAS000
155600990625     C   03              UPDATE    TITAS010
155700990625     C   04              UPDATE    TITASP00
155800941130    3C                   END
155900920127     C*
156000941130   X2C                   ELSE
156100920228     C******
156200920228     C****** M O D I F I C A T O   T I P O   B O L L A
156300920228     C******
156400920212     C                   SETOFF                                       31
156500920127     C                   MOVEL     §3ATB1        SAVTB1            2
156600920127     C                   MOVEL     §3ATB2        SAVTB2            2
156700920227     C                   MOVEL     §3AFCA        SAVFCA            1 0
156800920227     C                   MOVEL     §3AFTD        SAVFTD            1 0
156900941130    3C     §3ABCM        IFNE      ' '
157000920324     C                   MOVEL     0             SAVFCA
157100941130    3C                   END
157200920127     C* NUOVO TIPO BOLLA
157300920127     C                   MOVEL     '3A'          COD
157400941130     C                   MOVEL(P)  ARBCBN        KEY
157500920212     C     KTAB          CHAIN     TABEL                              34
157600941130     C*
157700941130     C* COD BOLLA NUOVO NON TROVATO
157800941130    3C     *IN34         IFEQ      *ON
157900941130     C                   MOVEL     ERR(6)        ARBDER
158000920212     C*
158100941130     C                   SETON                                        37
158200941130     C                   Z-ADD     102           COMFEV
158300941130     C                   EXSR      WTRERR
158400941130     C**
158500941130   X3C                   ELSE
158600941130     C**
158700941130     C* COD BOLLA NUOVO TROVATO
158800920131     C                   MOVEL     TBLUNI        DS3A
158900941130    4C     §3ABCM        IFNE      ' '
159000920324     C                   Z-ADD     0             §3AFCA
159100941130    4C                   END
159200920131     C*
159300920212     C* ANNULLAMENTO CONTRASSEGNO
159400941130    4C     SAVFCA        IFNE      0
159500920212     C     §3AFCA        ANDEQ     0
159600920212     C                   EXSR      CONCSB
159700920212     C*
159800941209    5C     *IN31         IFEQ      *OFF
159900941209     C* IMPOSTO IL NUOVO TIPO BOLLA
160000941209    6C     SAVFCA        IFEQ      2
160100941209     C     §3ATB2        ANDNE     *BLANKS
160200941209     C                   MOVEL     §3ATB2        CSBTBL
160300941209     C                   ELSE
160400941209     C                   MOVEL     §3ATB1        CSBTBL
160500941209    6C                   ENDIF
160600941209     C*
160700941209     C                   EXSR      ANNCSB
160800941209    5C                   END
160900941209    4C                   END
161000950202     C***************
161100920212     C* SE C/A PASSA A  "F"  A  "A"  O VICEVERSA POSSO FARLO ANCHE SE
161200920212     C*  INCASSATO - BASTA CHE NON SIA MODIFICATO L'IMPORTO
161300941130    4C     SAVFCA        IFEQ      1
161400920212     C     §3AFCA        ANDEQ     2
161500920212     C                   EXSR      CONCSB
161600950630     C* Se da bolla singola a bolla doppia e C/A sulla 2 bolla non
161700950630     C* aggiorno adesso TNCSB ma solo dopo scrittura della 2 bolla su
161800950630     C* TNTAS
161900950630    5C     SAVTB2        IFEQ      '  '
162000950630     C     §3ATB2        ANDNE     '  '
162100950630     C                   UNLOCK    TNCSB03L                             99
162200950630     C                   ELSE
162300941130     C* ANAGRAFICA C/A TROVATA
162400950630    6C     *IN31         IFEQ      *OFF
162500941130     C* CREO STORICO SE MODIFICATO IMPORTO
162600950630    7C     CSBCAS        IFNE      ARBCAS
162700991103    7C     WCSBVCA       ORNE      ARBVCA
162800920212     C                   MOVEL     'VACA'        CSVCAV
162900920212     C                   EXSR      CSVWTR
163000950630    7C                   END
163100920212     C*
163200941130     C                   Z-ADD     9999          CSBKSC
163300941130     C                   MOVEL     TASLNA        CSBKSC
163400950202     C                   MOVEL     §3ATB2        CSBTBL
163500950202     C*
163600950202     C                   EXSR      AGGCSB
163700941130     C                   UPDATE    TNCSB000
163800950630    6C                   END
163900950630    5C                   END
164000941130    4C                   END
164100950202     C***************
164200941130    4C     SAVFCA        IFEQ      2
164300920212     C     §3AFCA        ANDEQ     1
164400920228     C                   EXSR      CHTB2
164500920212     C*
164600920228     C                   EXSR      CONCSB
164700941130     C*
164800941130    5C     *IN31         IFEQ      *OFF
164900920212     C* CREO STORICO SE MODIFICATO IMPORTO
165000950202    6C     ARBCAS        IFNE      CSBCAS
165100991102    6C     ARBVCA        ORNE      WCSBVCA
165200920212     C                   MOVEL     'VACA'        CSVCAV
165300920212     C                   EXSR      CSVWTR
165400941130    6C                   END
165500920212     C*
165600941130     C                   MOVEL     CSBCDI        CSBKSC
165700941130     C                   MOVEL     §3ATB1        CSBTBL
165800950202     C                   EXSR      AGGCSB
165900941130     C                   UPDATE    TNCSB000
166000941130    5C                   END
166100941130    4C                   END
166200950202     C***************
166300950202     C* MODIFICATO IMPORTO C/ASSEGNO
166400941130    4C     SAVFCA        IFEQ      1
166500920227     C     §3AFCA        ANDEQ     1
166600941130     C* CONTROLLO IL C/A
166700920227     C                   EXSR      CONCSB
166800941130     C*
166900950202    5C  N31CSBCAS        IFNE      ARBCAS
167000991103    5C     WCSBVCA       ORNE      ARBVCA
167100920227     C                   MOVEL     'VACA'        CSVCAV
167200920227     C                   EXSR      CSVWTR
167300941209    5C                   END
167400950202     C*
167500950202     C                   EXSR      AGGCSB
167600950202     C*
167700941209     C* IMPOSTO IL NUOVO TIPO BOLLA
167800941209     C                   MOVEL     §3ATB1        CSBTBL
167900941209     C  N31              UPDATE    TNCSB000
168000941130    4C                   END
168100950202     C***************
168200941130    4C     SAVFCA        IFEQ      2
168300920227     C     §3AFCA        ANDEQ     2
168400920228     C                   EXSR      CHTB2
168500920227     C*
168600920227     C                   EXSR      CONCSB
168700941130     C*
168800950202    5C  N31CSBCAS        IFNE      ARBCAS
168900991103    5C     WCSBVCA       ORNE      ARBVCA
169000920227     C                   MOVEL     'VACA'        CSVCAV
169100920227     C                   EXSR      CSVWTR
169200941209    5C                   END
169300950202     C*
169400950202     C                   EXSR      AGGCSB
169500950202     C*
169600941209     C* IMPOSTO IL NUOVO TIPO BOLLA
169700941209     C                   MOVEL     §3ATB2        CSBTBL
169800941209     C  N31              UPDATE    TNCSB000
169900941130    4C                   END
170000920212     C***
170100920227     C*** A  G  G  I  O  R  N  O       B  O  L  L  A
170200920212     C***
170300080408     c  N31              if        VariataSede=' '
170400941130     C                   CLEAR                   DESPAR
170500970916     C                   CLEAR                   DESPRV
170600920228     C***
170700920228     C*** DA BOLLA SINGOLA A BOLLA SINGOLA
170800941130    5C     SAVTB2        IFEQ      '  '
170900920227     C     §3ATB2        ANDEQ     '  '
171000110315     c
171100110315     c* Non lo faccio se devo aggiungere solo il C/Ass perchè la bolla
171200110315     c*  non la trovo col cod bolla vecchio ma col cod bolla nuovo
171300110315    6c                   if        SoloCAss=' '
171400110315     c
171500941130     C                   MOVEL     'N'           VARFCA            1
171600920228     C                   EXSR      AGGTB1
171700941130     C*
171800941130     C* IMPOSTO IL PAESE DI ARRIVO
171900941130     C                   MOVEL     TASLOD        DESPAR
172000990625     C                   MOVEL     TASPRD        DESPRV
172100110315    6C                   END
172200110315    5C                   END
172300920228     C***
172400920228     C*** DA BOLLA DOPPIA A BOLLA DOPPIA
172500941130    5C     SAVTB2        IFNE      '  '
172600920227     C     §3ATB2        ANDNE     '  '
172700920228     C* AGGIORNO 1 BOLLA
172800941130     C                   MOVEL     'N'           VARFCA            1
172900920228     C                   EXSR      AGGTB1
173000941130     C* AGGANCIO 2 BOLLA
173100941130     C                   EXSR      CHTB2
173200941130     C* IMPOSTO IL PAESE DI ARRIVO
173300941130     C                   MOVEL     TASLOD        DESPAR
173400990625     C                   MOVEL     TASPRD        DESPRV
173500920227     C*
173600941130     C   02
173700941130    6CANN29              DO
173800920227     C*
173900941130    7C   38SAVFCA        IFEQ      2
174000920228     C     §3AFCA        OREQ      2
174100990505     c* Se il C/A si trova sulla 2 bolla, li imposto, altrimenti lo azzero
174200990505     c*  per il controllo della varia G
174300990505     c                   if        §3afca=2
174400990505     c                   eval      wcas=arbcas
174500990505     c                   else
174600990505     c                   clear                   wcas
174700990505     c                   endif
174800990505     c
174900920228     C                   EXSR      VARIAG
175000941130    7C                   END
175100920227     C*
175200920227     C                   MOVEL     §3ATB2        TASTBL
175300990629     C                   MOVEL     arbcbn        TASCBO
175400011218
175500011218      * riorganizzo le varie se è necessario
175600011218     c                   if        flgriorg = '1'
175700011218     c                   exsr      sr_riorg
175800011218     c                   endif
175900011218
176000920227     C*
176100941201     C* ALTRIMENTI CAMBIO IL TIPO BOLAA ANCHE NEL TA7
176200990625     C     KBLP          SETLL     TiTA7000
176300990625     C     KBLP          READE     TiTA7000                               30
176400941201    8C     *IN30         DOWEQ     *OFF
176500941201     C                   MOVEL     §3ATB2        TA7TBL
176600990625     C                   UPDATE    TITA7000
176700990625     C     KBLP          READE     TITA7000                               30
176800941201    8C                   ENDDO
176900941130    6C                   END
177000920227     C*
177100990625     C   02              UPDATE    TITAS000
177200990625     C   03              UPDATE    TITAS010
177300990625     C   04              UPDATE    TITASP00
177400920227     C*
177500920227     C* ERRORI
177600941130     C  N02
177700941130     COR 02
177800941130    6CAN 29              DO
177900941130    7C     SAVTB2        IFNE      §3ATB2
177901130410     c* solo se tipo bolla nuovo non è uno storno
177902130410     c                   movel     'TB'          cod
177903130410     c                   movel(p)  §3atb2        key
177904130410     c                   clear                   dstb
177905130410     C     KTAB          CHAIN     TABEL
177906130410    8c                   if        %found(tabel00f)
177907130410     c                   movel     tbluni        dstb
177908130410    8c                   endif
177909130410    8c                   if        §tbfcb='1'
177910130326     c                   z-add     46            comfev
178000930513     C                   MOVEL     ERR(16)       ARBDER                          MODIF BOLLA
178001130410    8c                   endif
178100941130   X7C                   ELSE
178200920228     C*
178201130326     c                   z-add     46            comfev
178300930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF A3
178400941130    7C                   END
178500920228     C*
178600920227     C                   EXSR      WTRERR
178700941130    6C                   END
178800941130    5C                   END
178900920228     C***
179000920228     C*** DA BOLLA DOPPIA A BOLLA SINGOLA
179100941130    5C     SAVTB2        IFNE      '  '
179200920227     C     §3ATB2        ANDEQ     '  '
179300950630     C*
179400920228     C* AGGANCIO 2 BOLLA
179500920228     C                   EXSR      CHTB2
179600941130     C* IMPOSTO IL PAESE DI ARRIVO
179700941130     C                   MOVEL     TASLOD        DESPAR
179800990625     C                   MOVEL     TASPRD        DESPRV
179900920227     C*
180000941130     C   02
180100941130    6CANN29              DO
180200920227     C                   MOVEL     'AP'          TASTBL
180300991021     c                   movel     arbcbn        tascbo
180400941130     C*
180500941130     C* TOLGO FLAG C/A SOLO SE DALLA 2 BOLLA E' PASSATO ALLA 1
180600941130     C     SAVFCA        IFEQ      2
180700941130     C     §3AFCA        ANDEQ     1
180800941130     C                   MOVEL     ' '           TASFCA
180900941130     C                   ENDIF
181000941130     C*
181100990625     C                   UPDATE    TITAS000
181200941130    6C                   END
181300920227     C*
181400920227     C* ERRORI
181500990505    6C   29              DO
181501130326     c                   z-add     46            comfev
181600930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF BOLLA
181700920227     C                   EXSR      WTRERR
181800941130    6C                   END
181900920227     C**
182000920228     C** AGGIORNO 1 BOLLA
182100941130     C                   MOVEL     'S'           VARFCA
182200920228     C                   EXSR      AGGTB1
182300941130    5C                   END
182400920228     C***
182500920228     C*** DA BOLLA SINGOLA A BOLLA DOPPIA
182600941130    5C     SAVTB2        IFEQ      '  '
182700920227     C     §3ATB2        ANDNE     '  '
182800950630     C*
182900920228     C* AGGIORNO PRIMA BOLLA
183000941130     C                   MOVEL     'S'           VARFCA
183100920228     C                   EXSR      AGGTB1
183200920227     C*
183300000816     c* Prima di creare la seconda bolla cancello eventualmente bolla tipo
183400000816     c* "AP" che potrei avere in seguito ad un precedente cambio da bolla
183500000816     c* doppia a bolla singola
183600000816     c                   movel     'AP'          comtbl
183700000816     c                   setoff                                       020304
183800000816     c     kblp          chain     titas30c                           39
183900000816     c     *in39         ifeq      *off
184000000816     c     *in02         andeq     *on
184100000816     c     tasdft        ifeq      *zeros
184200000816     c     tasfic        oreq      ' '
184300000816     c                   delete    titas000
184400000816     c                   endif
184500000816     c                   endif
184600920227     C* CREO SECONDA BOLLA
184700920227     C*
184800950201     C* AGGANCIO FILE FNARBD1R
184900920227     C     ARBORV        ADD       1             COMORV
185000950201     C     KARBD         CHAIN     FNARBDRR                           32
185100920304     C  N32              ADD       1             NUMRBD
185200920227     C* 32 ON - INESISTENTE: NON SCRIVO DESTINATARIO E SEGNALO ERRORE
185300941130    6C   32              DO
185400920227     C                   MOVEL     *BLANKS       ARBRSD
185500950202     C                   MOVEL     *BLANKS       ARBRD2
185600920227     C                   MOVEL     *BLANKS       ARBIND
185700920227     C                   MOVEL     *BLANKS       ARBLOD
185800950202     C                   MOVEL     *BLANKS       ARBCAD
185900941130     C                   MOVEL     *BLANKS       ARBPRD
186000950202     C                   MOVEL     *BLANKS       ARBNZD
186100920227     C*
186200920227     C                   MOVEL     ERR(12)       ARBDER
186300920227     C                   Z-ADD     102           COMFEV
186400920227     C                   EXSR      WTRERR
186500941130    6C                   END
186600920227     C*
186700920227     C*
186800941130     C                   MOVEL     §3ATB2        TASTBL
186900990629     C                   MOVEL     ARBCBN        TASCBO
187000941130     C                   MOVE      TASKSC        KSC4
187100941130     C*
187200941130     C                   CLEAR                   TASRSD
187300941130     C                   CLEAR                   TASIND
187400990625     C                   CLEAR                   TASCAd
187500941130     C                   CLEAR                   TASLOD
187600990625     C                   CLEAR                   TASPRd
187700990625     C                   CLEAR                   TAScpd
187800941130     C*
187900941130     C* MUOVO L'INIDIRIZZO COMPLETO DEL DESTINATARIO
188000941130     C                   MOVEL     ARBRSD        TASRSD
188100941130     C                   MOVEL     ARBIND        TASIND
188200990625     C                   MOVEL     ARBCAD        TASCAD
188300941130     C                   MOVEL     ARBLOD        TASLOD
188400941130     C                   MOVEL     ARBLOD        DESPAR
188500970916     C                   MOVEL     ARBPRD        DESPRV
188600990625     C                   MOVEL     ARBPRD        TASPRD
188700990625     C                   MOVEL     ARBNZD        TASNZD
188800990701     c* Muovo comunque la p.iva in tascpd anche se in questo caso
188900990701     c* sarà sempre vuota perchè visto che prima della variazione
189000990701     c* la bolla era franco non poteva esserci in filiale la p.iva
189100990701     c* del destinatario memoziazzata su fnarb
189200990701     C                   MOVEL     ARBCPI        TASCPD
189300941130     C*
189400941130     C                   CLEAR                   TASPVL
189500941130     C                   CLEAR                   TASFTC
189600950202     C                   CLEAR                   TASTC2
189700941130     C                   CLEAR                   TASIAS
189800990824     C                   CLEAR                   TASFCM
189900950202     C                   CLEAR                   TASVAS
190000941130     C                   MOVEL     *ZEROS        TASCTR
190100930604     C                   MOVEL     *BLANKS       TASFTM
190200991018     C                   MOVEL     *BLANKS       TASDIV
190300920227     C                   MOVE      *ZERO         TASPOR
190400920227     C                   MOVE      *BLANK        TASSV1
190500920227     C                   MOVE      *ZERO         TASVA1
190600920227     C                   MOVE      *BLANK        TASSV2
190700920227     C                   MOVE      *ZERO         TASVA2
190800930212     C                   MOVE      *BLANK        TASSV3
190900930212     C                   MOVE      *ZERO         TASVA3
191000920227     C                   Z-ADD     0             TASDFT
191100920227     C                   Z-ADD     0             TASNFT
191200920227     C                   Z-ADD     0             TASFIV
191300930212     C                   MOVE      ' '           TASFA2
191400920227     C                   Z-ADD     9999          TASKSC
191500920227     C                   MOVEL     TASLNA        TASKSC
191600920227     C* CONTRASSEGNO A SECONDA DEL FLAG
191700941130    6C     §3AFCA        IFEQ      2
191800941130     C                   MOVEL     'S'           TASFCA
191900920227     C                   MOVEL     §3ATB2        TBLCA             2
192000920227     C                   ELSE
192100941130     C                   MOVEL     ' '           TASFCA
192200941130    6C                   END
192300920227     C*
192400990624     C                   MOVE      *zeros        TASimv
192500941130     C                   MOVEL     ' '           TASFEI
192600941130     C                   MOVEL     ' '           TASFTS
192700941130     C                   MOVEL     ' '           TASFIC
192800920227     C*
192900990625     C                   WRITE     TITAS000                             99
193000950630     C*
193100950630     C     SAVFCA        IFEQ      1
193200950630     C     §3AFCA        ANDEQ     2
193300950630     C     KANN          CHAIN     TNCSB000                           31
193400991103     c                   move      csbvca        wcsbvca
193500991103     c     csbcas        ifgt      0
193600991103     c     wcsbvca       andeq     *blanks
193700991103     c                   movel     'ITL'         wcsbvca
193800991103     c                   endif
193900950630     C     CSBCAS        IFNE      ARBCAS
194000991103     C     wcsbvca       ORNE      ARBVCA
194100950630     C                   MOVEL     'VACA'        CSVCAV
194200950630     C                   EXSR      CSVWTR
194300950630     C                   END
194400950630     C*
194500950630     C                   Z-ADD     9999          CSBKSC
194600950630     C                   MOVEL     TASLNA        CSBKSC
194700950630     C                   MOVEL     §3ATB2        CSBTBL
194800950630     C*
194900950630     C                   EXSR      AGGCSB
195000950630     C                   UPDATE    TNCSB000
195100950630     C                   END
195200920227     C*
195300941130     C* REIMPOSTO ORA DI VARIAZIONE DI ARBK: ORA DI ARBD - 1
195400920227     C  N32              SUB       1             ARBORV
195500950201     C  N32              DELETE    FNARBDRR
195600941130    5C                   END
195700920227     C*
195800920227     C* CREO SE NECESSARIO RECORD IN FILE CONTRASSEGNI
195900941130    5C     SAVFCA        IFEQ      0
196000941130    6C     §3AFCA        IFEQ      1
196100920227     C                   MOVEL     §3ATB1        TBLCA
196200941130     C* RICHAINO LA BOLLA
196300941130     C                   MOVEL     §3ATB1        COMTBL
196400941130     C                   SETOFF                                       020304
196500990625     C     KBLP          CHAIN     TITAS30C                           30
196600110315     c* Se solo C/Ass--> aggiungo il flag
196700110315     c                   if        SoloCAss='S'
196800110315     c                   eval      TASFCA='S'
196900110315     C                   MOVEL     TASLOD        DESPAR
197000110315     C                   MOVEL     TASPRD        DESPRV
197100110315     C   02              UPDATE    TITAS000
197200110315     C   03              UPDATE    TITAS010
197300110315     C   04              UPDATE    TITASP00
197400110315     c                   endif
197500950913     C*
197600920227     C                   EXSR      CRTCSB
197700941130     C*
197800920227     C                   ELSE
197900941130     C*
198000941130    7C     §3AFCA        IFEQ      2
198100920227     C                   MOVEL     §3ATB2        TBLCA
198200941130     C* RICHAINO LA BOLLA
198300941130     C                   MOVEL     §3ATB2        COMTBL
198400941130     C                   SETOFF                                       020304
198500990625     C     KBLP          CHAIN     TITAS30C                           30
198600920227     C                   EXSR      CRTCSB
198700941130    7C                   END
198800941130    6C                   END
198900941130    5C                   END
199000920227     C*
199100941130    4C                   END
199200941130    3C                   END
199300941130    2C                   END
199400930513     C*
199500930513     C* 11 ON - RECORD DESTINATARIO PER CREARE 2 BOLLA
199600941130   X1C                   ELSE
199700950202     C* NON CANCELLO IL RECORD : IN ARBM LO LASCIO LI'
199800950202     C                   SETON                                        37
199900950202     C*
200000950202     C     *IN11         IFEQ      *ON
200100930513     C                   MOVEL     102           COMFEV
200200930513     C                   MOVEL     ERR(15)       ARBDER
200300930513     C                   EXSR      WTRERR
200400950202     C                   ENDIF
200500950202    1C                   ENDIF
200600920227     C*
200700920227     C                   ENDSR
200800060317     C**---------------------------------------------------------
200900060317     C** AGGIORNO CODICE VARIAZIONE RM
201000060317     C**---------------------------------------------------------
201100060317     C     AGARBR        BEGSR
201200060317     c* rma in tita4 record "A"
201300060317     c                   setoff                                       0506
201400060317     C                   movel     'A'           §trc
201500060317     C     ktita         chain     tita430c                           32
201600060317    3C     *in32         ifeq      *off
201700060317     C                   movel     ta4not        dta4a
201800060317     c                   movel     arbrsd        §TA4ARMA
201900060317     C                   movel     dta4a         ta4not
202000060317     C   05              update    tita4000
202100060317     C   06              update    tita4p00
202200060317     c                   endif
202300060317     c*
202400060317     C                   MOVEL     ARBrd2        TASnas
202500060317     C   02              UPDATE    TiTAS000
202600060317     C   03              UPDATE    TiTAS010
202700060317     C   04              UPDATE    TiTASP00
202800060317     c*
202900060317     C                   ENDSR
203000920228     C**---------------------------------------------------------
203100920228     C** AGGIORNO PRIMA BOLLA
203200920228     C**---------------------------------------------------------
203300920228     C     AGGTB1        BEGSR
203400920228     C* AGGANCIO 1 BOLLA
203500920228     C                   EXSR      CHTB1
203600920228     C*
203700941201     C* SE E' DA AGGIUNGERE LO AGGIUNGO
203800920228     C     §3AFCA        IFEQ      1
203900941130     C                   MOVEL     'S'           TASFCA
204000941201     C*
204100920228     C                   ELSE
204200941201     C* LO TOLGO SOLO SE FALG IMPOSTATO = "S"
204300941201     C*
204400941201     C     VARFCA        IFEQ      'S'
204500941130     C                   MOVEL     ' '           TASFCA
204600920228     C                   END
204700941130     C                   END
204800920228     C*
204900941130     C   02
205000941130     CANN29              DO
205100920228     C   38SAVFCA        IFEQ      1
205200920228     C     §3AFCA        OREQ      1
205300990505     c* Devo impostare l'importo C/A nella routine varia G :
205400990505     c*  se il C/A e' stato tolto dalla 1 bolla, richiamo con importo=0
205500990505     c                   if        §3afca=1
205600990505     c                   eval      wcas=arbcas
205700990505     c                   else
205800990505     c                   clear                   wcas
205900990505     c                   endif
206000990505     c
206100920228     C                   EXSR      VARIAG
206200920228     C                   END
206300920228     C*
206400920228     C                   MOVEL     §3ATB1        TASTBL
206500990629     C                   MOVEL     ARBCBN        TASCBO
206600011219
206700011219      * riorganizzo le varie se è necessario
206800011219     c                   if        flgriorg = '1'
206900011219     c                   exsr      sr_riorg
207000011219     c                   endif
207100011219
207200000209     C* AGGIORNO TITA7000 SE ESISTE
207300000209     C     KBLP          SETLL     TITA7000
207400000209     C     KBLP          READE     TITA7000                               30
207500000209     C*
207600000209     C     *IN30         DOWEQ     *OFF
207700000209     C                   MOVEL     §3ATB1        TA7TBL
207800000209     C                   UPDATE    TITA7000
207900000209     C     KBLP          READE     TITA7000                               30
208000000209     C                   ENDDO
208100920228     C                   END
208200941130     C*
208300920228     C*
208400990625     C   02              UPDATE    TITAS000
208500990625     C   03              UPDATE    TITAS010
208600990625     C   04              UPDATE    TITASP00
208700920228     C*
208800990505     C* spedizione gia' contabilizzata
208900990505     C   29              DO
209000920228     C*
209100920312     C                   MOVEL     *BLANKS       ARBDER
209200920228     C     §3ATB1        IFNE      SAVTB1
209201130410     c* solo se tipo bolla nuovo non è uno storno
209202130410     c                   movel     'TB'          cod
209203130410     c                   movel(p)  §3atb1        key
209204130410     c                   clear                   dstb
209205130410     C     KTAB          CHAIN     TABEL
209206130410    3c                   if        %found(tabel00f)
209207130410     c                   movel     tbluni        dstb
209208130410    3c                   endif
209209130410    3c                   if        §tbfcb='1'
209210130326     c                   z-add     46            comfev
209300930513     C                   MOVEL     ERR(16)       ARBDER                         MODIF BOLLA
209301130410    3c                   endif
209400920228     C                   ELSE
209500920228     C*
209600920228     C     SAVFCA        IFNE      1
209700920228     C     §3AFCA        ANDEQ     1
209800130325     C***                MOVEL     ERR(18)       ARBDER                         AGGIUN VARIA
209900130325     c***                setoff                                       36
209902120423     c                   exsr      ctr_vrb
210000920228     C                   END
210100920228     C*
210200920228     C     SAVFCA        IFEQ      1
210300920228     C     §3AFCA        IFNE      1
210400941130     C*
210500941130     C* CONTROLLO SE ESISTE VARIA "G"
210600130326     C***                EXSR      ESVARG
210700130325     C***  WSIVAR        IFEQ      'S'
210800130325     C***                MOVEL     ERR(14)       ARBDER                         TOLTA VARIA
210900130325     c***                setoff                                       36
211000130325     C***                END
211002121205     c                   eval      wanncas='S'
211003121203     c                   exsr      ctr_vrb
211100941130     C*
211200920228     C                   ELSE
211300130325     C****               MOVEL     ERR(22)       ARBDER                         MODIF VARIA
211400130325     c****               setoff                                       36
211402120423     c                   exsr      ctr_vrb
211500920228     C                   END
211600920228     C                   END
211700920228     C                   END
211800920228     C*
211900920312     C     ARBDER        IFNE      *BLANKS
212000920228     C                   EXSR      WTRERR
212100920228     C                   END
212200920312     C                   END
212300920228     C                   ENDSR
212400920227     C**---------------------------------------------------------
212500920227     C** CALCOLO O ANNULLO VARIA  G
212600120322     C**---------------------------------------------------------
212700920227     C     VARIAG        BEGSR
212800920228     C                   MOVE      TASKSC        KSC4
212900990505     c                   clear                   ARBDER
213000011218
213100011218     c                   clear                   flgriorg
213200941130     C*
213300990414     c* Se data fattura a zero e cliente codificato vado a togliere la varia G
213400990505     C     TASDFT        IFEQ      *ZEROS
213500990414    1C     KSC4          ANDNE     9999
213600920303     C     KSC4          ANDNE     8888
213700920228     C*
213800990505     c* C/A tolto
213900990505     c                   if        wcas=0
214000941130     C                   SELECT
214100941130     C     TASSV1        WHENEQ    'G'
214200011218     c                   eval      flgriorg = '1'
214300991019     c     tasimv        ifeq      tasva1
214400991019     c                   clear                   tasdiv
214500991019     c                   endif
214600920227     C                   MOVEL     ' '           TASSV1
214700920227     C                   Z-ADD     0             TASVA1
214800990624     C                   MOVE      *zeros        TASimv
214900941130     C     TASSV2        WHENEQ    'G'
215000011218     c                   eval      flgriorg = '1'
215100991019     c     tasimv        ifeq      tasva2
215200991019     c                   clear                   tasdiv
215300991019     c                   endif
215400920227     C                   MOVEL     ' '           TASSV2
215500920227     C                   Z-ADD     0             TASVA2
215600990624     C                   MOVE      *zeros        TASimv
215700941130     C     TASSV3        WHENEQ    'G'
215800011218     c                   eval      flgriorg = '1'
215900991019     c     tasimv        ifeq      tasva3
216000991019     c                   clear                   tasdiv
216100991019     c                   endif
216200930212     C                   MOVEL     ' '           TASSV3
216300930212     C                   Z-ADD     0             TASVA3
216400990624     C                   MOVE      *zeros        TASimv
216500941130     C*
216600941130     C                   OTHER
216700941130     C*
216800941130     C                   MOVEL     'G'           WSV1
216900990625     C     KBLPV         DELETE    TITA7000                           30
217000990624     C  n30              MOVE      *zeros        TASimv
217100941130     C                   ENDSL
217200990419     c                   else
217300990419     c* C/A aggiunto o midificato
217400990419     c                   EXSR      ESVARG
217500990419     c* Se la varia non c'e' tolto totale imponibile cosi' la calcola
217600990419     c                   if        wsivar=' '
217700990624     c                   clear                   TASimv
217800990419     c                   else
217900990419     c* Se gia' esiste stampo errore --> no modificata varia G
218000130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
218100130326     C***                EXSR      WTRERR
218101130326     c***                eval      wcntab=' '
218102130326     c***                exsr      ctr_vrb
218200990419     c                   endif
218300990419     c                   endif
218400920228     C*
218500941130   X1C                   ELSE
218600990414     C* CLIENTE NON CODIFICATO O CON DATA FATTURA VALORIZZATA
218700991019     C* COMUNQUE SE ANNULLATO C/A ELIMINO VARIA G  E ADEGUO TOT.IMPONIBILE
218800990414     C* Se c'è importo del contrassegno e la data fattura è uguale a zero posso
218900990414     c* togliere la varia "G"
219000990505    2C     WCAS          IFEQ      0
219100990505     C     TASDFT        ANDEQ     0
219200990414     c*
219300941130    3C                   SELECT
219400941130     C     TASSV1        WHENEQ    'G'
219500011218     c                   eval      flgriorg = '1'
219600991019     c                   sub       tasva1        tasimv
219700991019     c     tasimv        ifeq      0
219800991019     c                   clear                   tasdiv
219900991019     c                   endif
220000920228     C                   MOVEL     ' '           TASSV1
220100920228     C                   Z-ADD     0             TASVA1
220200941130     C     TASSV2        WHENEQ    'G'
220300011218     c                   eval      flgriorg = '1'
220400991019     c                   sub       tasva2        tasimv
220500991019     c     tasimv        ifeq      0
220600991019     c                   clear                   tasdiv
220700991019     c                   endif
220800920228     C                   MOVEL     ' '           TASSV2
220900920228     C                   Z-ADD     0             TASVA2
221000941130     C     TASSV3        WHENEQ    'G'
221100011218     c                   eval      flgriorg = '1'
221200991019     c                   sub       tasva3        tasimv
221300991019     c     tasimv        ifeq      0
221400991019     c                   clear                   tasdiv
221500991019     c                   endif
221600930212     C                   MOVEL     ' '           TASSV3
221700930212     C                   Z-ADD     0             TASVA3
221800941130     C                   OTHER
221900941130     C                   MOVEL     'G'           WSV1
222000991019     C     KBLPV         chain     TITA7000                           30
222100991019     c  n30              sub       ta7van        tasimv
222200991019     c  n30              delete    tita7000
222300941130    3C                   ENDSL
222400920228     C*
222500941130   X2C                   ELSE
222600990414     c*
222700990414     c* Se è già stato fatturato oppure arbcas > 0
222800920228     C*
222900941130     C* VEDO SE LA VARIA "G" C'ERA
223000130326     C*****              EXSR      ESVARG
223100941130     C*
223200991019     c* se c'è  l'importo del contrassegno per non codificati
223300991019     c*                                      o data fattura maggiore di zero
223400130326    3c***  wcas          ifgt      *zeros
223500990414     c*
223600130326    4C***  WSIVAR        IFEQ      ' '
223700130326     C***                MOVEL     ERR(24)       ARBDER                         AGGIUN VARIA
223800130326     C***                EXSR      WTRERR
223801130326     c***                eval      wcntab=' '
223802130326     c***                exsr      ctr_vrb
223900130326     C***                ELSE
224000130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
224100130326     C***                EXSR      WTRERR
224101130326     c***                eval      wcntab=' '
224102130326     c***                exsr      ctr_vrb
224200130326    4C***                END
224300130326    3c****               endif
224400990505     c*
224500990414     c* se l'importo è uguale a zero e prima c'era la varia "G"
224600130326    3c***  wcas          ifeq      *zeros
224700130326     c***  wsivar        andne     ' '
224800130326     C***                MOVEL     ERR(23)       ARBDER                          no tolta varia
224900130326     C***                EXSR      WTRERR
225000130326    3C***                END
225100941130    2C                   END
225200941130    1C                   END
225300920227     C*
225400920227     C                   ENDSR
225500011218      *----------------------------------------------------------
225600011218      * Riorganizzo le varie sul titas
225700011218      *----------------------------------------------------------
225800011218     c     sr_riorg      begsr
225900011218
226000011218     c                   clear                   sksv
226100011218     c                   clear                   skva
226200011218     c                   eval      xx = 1
226300011218
226400011219      * carico la sk delle varie dal titas
226500011218     c                   if        tassv1 <> *blanks
226600011218     c                   eval      sksv(xx) = tassv1
226700011218     c                   eval      skva(xx) = tasva1
226800020107     c                   clear                   tassv1
226900020107     c                   clear                   tasva1
227000011218     c                   add       1             xx
227100011218     c                   endif
227200011218     c                   if        tassv2 <> *blanks
227300011218     c                   eval      sksv(xx) = tassv2
227400011218     c                   eval      skva(xx) = tasva2
227500020107     c                   clear                   tassv2
227600020107     c                   clear                   tasva2
227700011218     c                   add       1             xx
227800011218     c                   endif
227900011218     c                   if        tassv3 <> *blanks
228000011218     c                   eval      sksv(xx) = tassv3
228100011218     c                   eval      skva(xx) = tasva3
228200020107     c                   clear                   tassv3
228300020107     c                   clear                   tasva3
228400011218     c                   add       1             xx
228500011218     c                   endif
228600011218
228700011219      * carico la sk delle varie dal tita7
228800011219     c     kblp          setll     tita7000
228900011218     c                   do        *hival
229000011219     c     kblp          reade     tita7000
229100011219     c                   if        %eof
229200011218     c                   leave
229300011219     c                   endif
229400011218     c                   eval      skva(xx) = ta7van
229500011218     c                   eval      sksv(xx) = ta7svn
229600011218     c                   add       1             xx
229700011219     c                   delete    tita7000
229800011218     c                   enddo
229900011219
230000011219      * scrivo le varie su titas
230100011219     c                   if        sksv(1) <> *blanks
230200011219     c                   eval      tassv1 = sksv(1)
230300011219     c                   eval      tasva1 = skva(1)
230400011219     c                   endif
230500011219     c                   if        sksv(2) <> *blanks
230600011219     c                   eval      tassv2 = sksv(2)
230700011219     c                   eval      tasva2 = skva(2)
230800011219     c                   endif
230900011219     c                   if        sksv(3) <> *blanks
231000011219     c                   eval      tassv3 = sksv(3)
231100011219     c                   eval      tasva3 = skva(3)
231200011219     c                   endif
231300011219
231400011219      * scrivo le varie su tita7
231500011219     c     4             do        20            xx
231600011219     c                   if        sksv(xx) <> *blanks
231700011219     c                   eval      ta7aas = arbaas
231800011219     c                   eval      ta7lnp = arblnp
231900011219     c                   eval      ta7nrs = arbnrs
232000011219     c                   eval      ta7nsp = arbnsp
232100011219     c                   eval      ta7tbl = comtbl
232200011219     c                   eval      ta7svn = sksv(xx)
232300011219     c                   eval      ta7van = skva(xx)
232400011219     c                   write     tita7000
232500011219     c                   endif
232600011219     c                   enddo
232700011218
232800011218     c                   endsr
232900011218
233000920210     C**---------------------------------------------------------
233100920210     C** CREO RECORD CONTRASSEGNO
233200920210     C**---------------------------------------------------------
233300920210     C     CRTCSB        BEGSR
233400941130     C                   MOVEL     ARBDER        SAVDER
233500920303     C*
233600920303     C* SE ESISTE ANNULLATO LO DELETO E RISCRIVO
233700941130     C     KANN          CHAIN     TNCSB000                           41
233800941130    1C     *IN41         IFEQ      *ON
233900941130     C                   CLEAR                   TNCSB000
234000941130     C                   ELSE
234100941130     C     KANN          CHAIN     TNCSA000                           39
234200941130     C  N39              DELETE    TNCSA000
234300941130    1C                   ENDIF
234400920303     C*
234500950201     C* AGGANCIO FILE FNARBM0R
234600950202     C     KARBM         CHAIN     FNARBMRR                           35
234700920304     C  N35              ADD       1             NUMRBM            7 0
234800920212     C* INESISTENTE: STAMPO MA CREO UGUALMENTE ARCHIVIO (DA COMPLETARE)
234900941130    1C   35              DO
235000920212     C                   MOVEL     ERR(11)       ARBDER
235100920212     C                   Z-ADD     102           COMFEV
235200920212     C                   EXSR      WTRERR
235300920212     C* AZZERO CAMPI MITTENTE
235400920212     C                   MOVEL     *BLANKS       ARBRSM
235500920212     C                   MOVEL     *BLANKS       ARBINM
235600920212     C                   MOVEL     *BLANKS       ARBCAM
235700920212     C                   MOVEL     *BLANKS       ARBLOM
235800941130     C                   MOVEL     *BLANKS       ARBPRD
235900920212     C                   MOVEL     *ZEROS        ARBCCM
236000941130    1C                   END
236100920212     C*
236200920210     C                   MOVEL     TASLNP        CSALNP
236300920210     C                   MOVEL     TASNRS        CSANRS
236400920210     C                   MOVEL     TASAAS        CSAAAS
236500920210     C                   MOVEL     TASNSP        CSANSP
236600920210     C*
236700920210     C                   MOVEL     TBLCA         CSBTBL
236800920210     C                   Z-ADD     TASLNP        CSBLNP
236900920210     C                   Z-ADD     TASAAS        CSBAAS
237000920210     C                   Z-ADD     TASNRS        CSBNRS
237100920210     C                   Z-ADD     TASNSP        CSBNSP
237200941130     C                   Z-ADD     TASMGS        CSBMGS
237300990629     C                   Z-ADD     TASTFP        CSBFLE
237400941130     C                   MOVEL     DESPAR        CSBPAR                         PAESE ARRIVO
237500970916     C                   MOVEL     DESPRV        CSBPRO                         PROV.DEST.
237600030310     C                   MOVE      *ZERO         CSBSTA
237700950202     C                   EXSR      AGGCSB
237800920304     C*
237900941130     C                   MOVEL     TBLCA         FLGTB1
238000920304     C*
238100920304     C* SE BLANK E CODIFICATO DESTINATARIO CHIAN
238200941130    2C     TASRSD        IFEQ      *BLANKS
238300990625     c* N.B.la call al tibs69 sarà da togliere quando tutte le bolla avranno
238400990625     c* il destinatario pieno
238500990625     c     flgtb1        ifeq      'A'
238600981001     C                   CLEAR                   DSBS69
238700981001     C                   MOVEL     KCI           I69KCC
238800981001     C                   MOVEL     KNSIF         I69SIF
238900981001     C                   MOVEL     TASKSC        I69KAC
239000981001     C                   CALL      'TIBS69R'
239100981001     C                   PARM                    DSBS69
239200981001     C                   PARM                    DSACO
239300981001     C                   PARM                    DSIND
239400981001     C                   PARM                    DSCLP
239500981001     C                   PARM                    DSCLS
239600981001     C**
239700981001     C     O69ERR        IFEQ      ' '
239800981001     C                   MOVEL     ACORAG        CSBRSD
239900981001     C                   ELSE
240000981001     C                   MOVEL     *BLANKS       CSBRSD
240100981001     C                   ENDIF
240200990625     C                   ENDIF
240300981001     C                   ELSE
240400981001     C                   MOVEL     TASRSD        CSBRSD
240500941130    2C                   END
240600920304     C*
240700920304     C*
240800920210     C                   Z-ADD     TASRMN        CSBRMN
240900990625     c* prendo il riferimento mittente alfabetico da estensione (tita4)
241000990625     c                   movel     'A'           §trc
241100010126     c     ktita         chain(N)  tita430c                           39
241200990625     c  n39              movel     ta4not        csbrma
241300990625     c   39              clear                   csbrma
241400941207     C                   Z-ADD     TASLNA        CSBLNA
241500040721     C**                   MOVEL     TASFIN        CSBFIN
241600920210     C                   Z-ADD     TASKSC        CSBKSC
241700920210     C                   MOVE      CSBKSC        CSBCDI
241800931108     C   41              MOVEL     'R'           CSBFTR
241900931108     C   41              MOVE      DATEU         CSBDTR
242000920303     C*
242100941130    1C     FLGTB1        IFEQ      'A'
242200941130     C                   Z-ADD     99999         CSBKSC
242300941130     C                   MOVEL     TASLNA        CSBKSC
242400941206     C                   MOVE      CSBKSC        CSBCDI                         WB/LB
242500941206    1C                   END                                                    WB/LB
242600941130     C*
242700941130     C                   MOVEL     TASCCM        CSBCDI
242800941206    1C*************        END                             WB/LB
242900941130     C*
243000920210     C** RAGGRUPPAMENTO C/ASSEGNI
243100920210     C     TASLNA        CHAIN     AZORG                              51
243200920210     C  N51              Z-ADD     ORGFC5        CSBRGF                         FIL RAGGR.C/A
243300920210     C   51              Z-ADD     TASLNA        CSBRGF
243400941130     C*
243500941130     C* VEDO SE CSBCDI E' INTESTAZIONE GENERICA
243600941130     C                   MOVE      CSBCDI        W0040             4 0
243700920210     C*
243800920210     C** DATI ANAGRAFICI PER CODICI GENERICI E ASSEGNATI
243900941130     C     W0040         IFEQ      8888
244000941130     C     W0040         OREQ      9999
244100920212     C                   MOVEL     ARBRSM        CSARSD                         MITTENTE
244200920212     C                   MOVEL     ARBINM        CSAVID                            '
244300920212     C                   MOVEL     ARBCAM        CSACAD                            '
244400920212     C                   MOVEL     ARBLOM        CSACID                            '
244500970916     C                   MOVEL     ARBPRM        CSAPRM
244600970916     C                   MOVEL     ARBNZM        CSANZM
244700920212     C*
244800941201     C                   WRITE     TNCSA000                             99
244900920212     C                   END
245000920212     C**
245100941201     C   41              WRITE     TNCSB000                             99
245200941130     C  N41              UPDATE    TNCSB000
245300950914     C*
245400950914     C* CREO RECORD DI INSERIMENTO C/ASSEGNO NELLE VARIAZIONI
245500950914     C                   MOVEL     'IMCA'        CSVCAV
245600950914     C                   EXSR      CSVWTR
245700941130     C*
245800941130     C** SE ERA IMPOSTATO LO RIMETTO COME ERA PRIMA
245900941130     C                   MOVEL     SAVDER        ARBDER
246000920210     C                   ENDSR
246100920210     C**---------------------------------------------------------
246200920210     C** CONTROLLO ESISTENZA ARCHIVIO CONTRASSEGNI
246300920210     C**---------------------------------------------------------
246400920210     C     CONCSB        BEGSR
246500930513     C                   SETOFF                                           42    25
246600920212     C* VARIO  CONTRASSEGNO SOLO SE NON PAGATO-INCASSATO
246700941130     C     KANN          CHAIN     TNCSB000                           31
246800930303     C*
246900941130     C   31KANN          SETLL     FNLBL01L                               42
247000920212     C*
247100930513     C* PER CASUALE VARIAZIONE "K" SEMPRE ERRORE
247200930303     C   31FLGCVB        IFEQ      'K'
247300930513     C                   SETON                                        25
247400930303     C                   MOVEL     ERR(5)        ARBDER
247500930303     C                   Z-ADD     102           COMFEV
247600930303     C                   EXSR      WTRERR
247700930513     C*
247800930303     C                   ELSE
247900930513     C*
248000930513     C* ALTRIMENTI SOLO SE NON C'E' IL LEGAME BOLLA
248100930513     C     *IN42         IFEQ      '0'
248200930513     C                   SETON                                        25
248300930513     C                   MOVEL     ERR(5)        ARBDER
248400930513     C                   Z-ADD     102           COMFEV
248500930513     C                   EXSR      WTRERR
248600930303     C                   END
248700930513     C                   END
248800991103     c*
248900991103     c     *in31         ifeq      *off
249000991103     c                   movel     csbvca        wcsbvca
249100991103     c     csbcas        ifgt      0
249200991103     c     csbvca        andeq     *blanks
249300991103     c                   movel     'ITL'         wcsbvca
249400991103     c                   endif
249500991103     c                   endif
249600930513     C*
249700920302     C* GIA' INCASSATO O PAGATO
249800950202    1C  N31CSBDDC        IFNE      0
249900920210     C     CSBBNA        ORNE      0
250000920212     C*
250100950324     C* SE NON MODIFICATO L'IMPORTO DEL C/A OK, SE VARIAZIONE "K"
250200950324     C     FLGCVB        IFEQ      'K'
250300950202    3C     ARBCAS        IFNE      CSBCAS
250400991102     C     ARBVCA        ORNE      WCSBVCA
250500920212     C                   SETON                                        31
250600110214     C                   Z-ADD     102           COMFEV
250700950202    2C                   END
250800110214     c
250900950324     C* ALTRIMENTI SEMPRE ERRORE
251000950324     C                   ELSE
251100950324     C                   SETON                                        31
251200110214     C                   Z-ADD     102           COMFEV
251300950324    2C                   END
251400920601     C*
251500950202    2C   31FLGCVB        IFNE      'I'
251600950222    2C     FLGCVB        ANDNE     'M'
251700920601     C                   SETON                                        36
251800920601     C                   MOVEL     ERR(10)       ARBDER
251900920601     C                   EXSR      WTRERR
252000950324     C                   ELSE
252100950324     C                   CLEAR                   COMFEV
252200950202    2C                   END
252300950202    1C                   END
252400920210     C                   ENDSR
252500950202     C*
252600950202     C**---------------------------------------------------------
252700950202     C** AGGIORNO RECORD CONTRASSEGNO
252800950202     C**---------------------------------------------------------
252900950202     C     AGGCSB        BEGSR
253000950202     C                   Z-ADD     ARBCAS        CSBCAS
253100950202     C                   MOVEL     ARBVCA        CSBVCA
253200950202     C                   MOVEL     ARBGCA        CSBGCA
253300950203     C                   Z-ADD     ARBCMB        CSBCMB
253400030310     c                   if        csbsta <> 3
253500000703     c                   movel     0             csbsta
253600030310     c                   end
253700950202     C* TIPO ASSEGNO: MITTENTE / BARTOLINI
253800950202     C                   MOVEL     '1A'          COD
253900950202     C                   MOVEL(P)  ARBTIC        KEY
254000950202     C     KTAB          CHAIN     TABEL                              30
254100950202     C  N30              MOVEL     TBLUNI        DS1A
254200950202     C  N30              MOVEL     §1AFMB        CSBFUS
254300950202     C   30              CLEAR                   CSBFUS
254400950202     C                   ENDSR
254500920212     C**---------------------------------------------------------
254600920212     C** ANNULLO RECORD CONTRASSEGNO
254700920212     C**---------------------------------------------------------
254800920212     C     ANNCSB        BEGSR
254900941209     C* CREO STORICO VARIAZIONE
255000920602     C     CSBSTA        IFNE      9
255100920212     C                   MOVEL     'ANCA'        CSVCAV
255200920212     C                   EXSR      CSVWTR
255300920212     C* ANNULLO
255400920212     C                   MOVEL     9             CSBSTA
255500941130     C                   UPDATE    TNCSB000
255600920212     C                   END
255700920212     C                   ENDSR
255800920212     C**---------------------------------------------------------
255900920212     C** ESEGUE WRITE BLCSV00F                                  -
256000920212     C**---------------------------------------------------------
256100920212     C     CSVWTR        BEGSR
256200950202     C* COME DATA E ORA VARIAZIONE METTO QUELLA DELLA FILIALE
256300941130     C                   Z-ADD     ARBDTV        CSVDTV
256400941130     C                   Z-ADD     ARBORV        CSVORV
256500020604     c*
256600021112     c                   clear                   tibs36ds
256700021112     c                   movel     arbpru        i36ute
256701140407     c                   eval      %subst(i36ute:10:1)=' '
256800021112     C                   CALL      'TIBS36R'
256900021112     C                   PARM                    tibs36ds
257000020604     c* non tovato profilo su azute
257100070420   1 c     o36err        ifne      *blanks
257200020604     c* richiamo fnlv55r per sapere se lna è gestita da altro p.o. e da chi
257300020604     c                   clear                   dslv55
257400020604     c                   movel     '6'           d55tpt
257500020604     c                   z-add     taslna        d55lin
257600020604     c                   z-add     csvdtv        d55drf
257700020604     c                   call      'FNLV55R'
257800020604     c                   parm                    dslv55
257900020604     C                   MOVEL     d55tfa        CSVFEV
258000070420   x1c                   else
258100020604     c* trovato profilo su azute
258200021112     c                   movel     o36pou        csvfev
258300070420    1c                   endif
258400020604     c*
258500920212     C                   MOVEL     CSBDS         CSVDS
258600941130     C* COME DATA TRSMISSIONE QUANDO HO CARICATO
258700941130     C                   Z-ADD     DATEU         CSVDTR
258800941130     C                   WRITE     TNCSV000
258900920212     C                   ENDSR
259000920130     C**---------------------------------------------------------
259100920130     C** CONTROLLO ESISTENZA BOLLA
259200920130     C**---------------------------------------------------------
259300990628     C     CTRSPE        BEGSR
259400920302     C                   SETOFF                                       332940
259500941130     C                   SETOFF                                       4217
259600941121     C                   SETOFF                                       020304
259700930514     C                   SETON                                        23
259800080408     c                   clear                   VariataSede       1
259900110315     c                   clear                   SoloCASS          1
260000941121     C*
260100991015     C     KBLP          CHAIN     TITAS30C                           29
260200920220     C*
260300941121     C* 29 ON  - S P E D I Z I O N E   I N E S I S T E N T E
260400941121    1C     *IN29         IFEQ      *ON
260500941121     C*
260600941121     C     KBLP          CHAIN     TNBLA000                           40
260700941121     C   40KANN          CHAIN     TNBLA000                           40
260800920602     C*
260900930804     C* 40 OFF - S P E D I Z I O N E   A N N U L L A T A
261000991015     C  N40              MOVEL     ERR(28)       ARBDER
261100070119     c
261200070119     c* Verifico se lnp estera, se record bolla chiuso in partenza
261300070119     c*  come merce mai affidata
261400070119    2c                   if        *in40
261500070119     c
261600070119     c     ArbLnp        Chain     Azorg01l
261700070119    3c                   If        %Found(Azorg01l)
261800070119     c                   Eval      Og143 = OrgDe3
261900070119    4c                   If        §OgNtw = 'DPD' or §ogntw='EEX' or
262000070119     c                             §ogntw='FED'
262100070119     c* controllo bolla se presente in fnblp
262200070119     c     kann          chain     fnblp01l
262300070119    5c                   if        %found(fnblp01l) and blpft1='N'
262400070119     c                             and blpcca='7'
262500070119     c                   setoff                                       23
262600070119    5c                   endif
262700070119    4c                   endif
262800070119    3c                   endif
262900070119    2c                   endif
263000920302     C*
263100920602     C* 40 ON - INESISTENTE: VEDO SE GIA' TRASFORMATO COD BOLLA
263200070119     c   23
263300070119    2Can 40FLGCVB        IFEQ      'K'
263400941121    3C     COMTBL        IFEQ      §3ATB1
263500930108     C                   MOVEL     ERR(1)        ARBDER
263600930108     C                   ELSE
263700930108     C                   MOVEL     ERR(7)        ARBDER
263800941121    3C                   END
263900930108     C*
264000920602     C                   MOVEL     §3ATB1        SAVTB1            2
264100920602     C                   MOVEL     §3ATB2        SAVTB2            2
264200080408     c                   movel     ds3a          savds3a
264300920602     C*
264400920602     C                   MOVEL     '3A'          COD
264500070119     c                   MOVEL(P)  ARBCBN        KEY
264600920602     C     KTAB          CHAIN     TABEL                              32
264700920602     C  N32              MOVEL     TBLUNI        DS3A
264800080408     c   32              clear                   ds3a
264900080408     c                   movel     §3afca        newfca            1 0
265000920602     C*
265100941121     C* 1BOLLA MODIFICATA
265200941121    3C  N32SAVTB1        IFNE      §3ATB1
265300920602     C                   MOVEL     §3ATB1        COMTBL
265400990624     C     KBLP          SETLL     TITAS30C                               42
265500920602     C  N42              MOVEL     ERR(1)        ARBDER
265600941121    3C                   END
265700920602     C*
265800941121     C* 2 BOLLA MODIFICATA
265900920602     C* SOLO SE ESISTEVA CONTROLLO
266000941121    3C  N32SAVTB2        IFNE      §3ATB2
266100920602     C     SAVTB2        ANDNE     '  '
266200920602     C*
266300941121    4C     §3ATB2        IFEQ      '  '
266400920602     C                   MOVEL     'AP'          COMTBL
266500941121     C                   ELSE
266600920602     C                   MOVEL     §3ATB2        COMTBL
266700941121    4C                   END
266800990624     C     KBLP          SETLL     TITAS30C                               42
266900070119     C  N42              MOVEL     ERR(7)        ARBDER
267000941121    3C                   END
267100080408     C***                MOVEL     SAVTB1        §3ATB1
267200080408     C***                MOVEL     SAVTB2        §3ATB2
267300080408     c                   movel     savds3a       ds3a
267400070119     c
267500070119     c* Se codice bolla vecchio = codice bolla nuovo, anche se
267600070119     c*  inesistente è inutile ristamparlo: già fatto precedentemente
267700080408    3c  n42              if        arbcbp=arbcbn and §3afca=0
267800070119     c                   setoff                                       23
267900080408    3c                   endif
268000080408     c* Se si tratta di tipo bolla con C/ASS prima e senza dopo
268100080408     c*  o una KB che toglie solo in C/ASS,
268200080408     c*  se bolla modificata in C/S --> vado avanti per il C/ASS
268300090504    3c***                if        not *in42 and *in23  and §3atb2=*blanks
268400090504    3c                   if        *in23  and §3atb2=*blanks
268500080408    4c                   if        (newfca=0 and §3afca=1) or
268600080408     c                             (arbcbp=arbcbn and §3afca=1 and arbcas=0)
268700080408     C                   MOVEL     'F7'          COMTBL
268800080408     C     KBLP          chain     TITAS30C
268900080408    5c                   if        %found(titas30c)
269000080408     c                   setoff                                       2923
269100080408     c                   eval      VariataSede='S'
269200080408    5c                   endif
269300080408    4c                   endif
269400080408    3c                   endif
269500110315     c
269600110315     c* se ho trovato la bolla con cod bolla nuovo ma era stato annullato il C/Ass  o
269700110315     c*   non c'era e ora c'e' --> lo aggiungo o riabilito
269800110315    3c                   if        *in23  and *in42 and §3atb2=*blanks  and
269900110315    4c                              newfca=1 and §3afca=0
270000110315     c                   setoff                                       2923
270100110315     c                   eval       soloCAss='S'
270200110315     c                   endif
270300920602     C*
270400941121   X2C                   ELSE
270500920602     C*
270600941130     C* CONTROLLO SE LA SPEDIZIONE E' STORNATA
270700930514     C                   EXSR      STORNO
270800991019     c* se causale variazione 'Cx' e la spedizione e' stornata
270900991019     c* spengo in29 per caricare ugualmente la variazione
271000991021    3c     flgcvb        ifeq      'C'
271100060317    3c     flgcvb        oreq      'R'
271200991021    4c     *in23         ifeq      *off
271300991021     c                   setoff                                       29
271400991021   x4c                   else
271500991021     c* altrimenti cerco la spedizione
271600991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
271700991021     c* (cioè il franco) faccio setgt e readpe invece che chain
271800991021     c     kann          setgt     titas30c
271900991021     c     kann          readpe    titas30c                               23
272000991019     c  n23              setoff                                       29
272100991021    4c                   endif
272200991021    3c                   endif
272300080408     c
272400080408     c* per "I0" e codice bolla "Z" cerco la bolla come A2 e poi
272500080408     c*  aggiorno secondo i modi già previsti in AGARBI
272600140407    3c                   if        (comcvb='I0' or
272601140407     c                              comcvb='I7' or
272602140407     c                              comcvb='I8' or
272603140407     c                              comcvb='I9' )
272604140407    3c                              and arbcbo='Z'
272700080408     C                   MOVEL     'A2'          COMTBL
272800080408     C     KBLP          chain     TITAS30C
272900080408    4c                   if        %found(titas30c)
273000080408     c                   setoff                                       2923
273100080408    4c                   endif
273200080408    3c                   endif
273300110214     c* per "I0" e codice bolla "A" cerco la bolla come F1 e poi
273400110214     c*  aggiorno secondo i modi già previsti in AGARBI
273500140407    3c***                if        comcvb='I0' and (arbcbo='A' or arbcbo='Q')
273501140407    3c                   if        (comcvb='I0' or
273502140407     c                              comcvb='I7' or
273503140407     c                              comcvb='I8' or
273504140407     c                              comcvb='I9' )
273505140407     c                             and (arbcbo='A' or arbcbo='Q')
273600110214     C                   MOVEL     'F1'          COMTBL
273700110214     C     KBLP          chain     TITAS30C
273800110214    4c                   if        %found(titas30c)
273900110214     c                   setoff                                       2923
274000110214    4c                   endif
274100110214    3c                   endif
274200930514     C*
274300941121    3C   23COMTBL        IFEQ      §3ATB1
274400991015     C                   MOVEL     ERR(1)        ARBDER
274500920130     C                   ELSE
274600991015     C                   MOVEL     ERR(7)        ARBDER
274700941121    3C                   END
274800941121    2C                   END
274900941130     C**
275000941130     C* 23 ON --> STAMPO ERRORE
275100941130    2C     *IN23         IFEQ      *ON
275200920602     C  N42
275300920602     CAN 40              SETON                                        37
275400920212     C*        PER C.E.D.
275500920603     C  N42
275600920603     CAN 40
275700920603     CORN40              Z-ADD     102           COMFEV
275800920602     C  N42
275900920602     CAN 40
276000920602     CORN40              EXSR      WTRERR
276100941121    2C                   END
276200941121     C*
276300991019    1C                   Endif
276400941121     C*
276500991019    1c     *in29         ifeq      *off
276600941121     C* 29 OFF - S P E D I Z I O N E       E S I S T E N T E
276601121108     c* Salvo contenuto del tracciato record di titas in ds apposita:
276602121108     c* serve per la scrittura dell'immagine di titas prima della prima variazione
276603121108     c* nel file TIVRB00F
276604121108     c                   exsr      memo_dstitas
276700941121     C*
276800941121     C* 02 OFF             --> GIA' CONTABILIZZATA IN BLTAS10 O BLTASP
276900941130     C* 02 ON  MA TASFIC=1 --> GIA' CONTABILIZZATA IN BLTAS00
277000930512     C*
277100941130    2C     *IN02         IFEQ      *OFF
277200941121     C     *IN02         OREQ      *ON
277300941130     C     TASFIC        ANDNE     ' '
277400941130     C**
277500950202     C* VARIAZIONE "Ix" --> indirizzo/ peso/ volume
277600941130     C**
277700941130    3C     FLGCVB        IFEQ      'I'
277800941130     C*
277900950202     C* SE VARIO PESO/VOLUME      E NON E' LA 2 BOLLA --> ERRORE
278000960416    4C     COMCVB        IFNE      'I0'
278001140407    4C     COMCVB        andne     'I7'
278002140407    4C     COMCVB        andne     'I8'
278003140407    4C     COMCVB        andne     'I9'
278100941130     C*
278200991021    5C     §3ATB2        IFNE      tastbl
278300930513     C                   SETON                                        29
278400930804     C*
278500140219    6c***  TASPKB        IFNE      ARBPKF
278600130325     C***                MOVEL     ERR(9)        ARBDER
278700130325     C***                EXSR      WTRERR
278703120313     c                   exsr      ctr_vrb
278800140219   X6C***                ELSE
278900930603     C*
279000140219    7C***  TASFVF        IFNE      'T'
279100140219     C***  TASVLF        ANDNE     ARBVLF
279200140219    8C***  TASFVF        IFEQ      'Z'
279300140219     C***  TASVLF        ANDLT     ARBVLF
279400140219     C***  TASFVF        ORNE      'Z'
279500130325     C***                MOVEL     ERR(9)        ARBDER
279600130325     C***                EXSR      WTRERR
279601140219     c***                eval      wcntab='S'
279604140219     c***                exsr      ctr_vrb
279700140219    8C***                END
279800140219    7C***                END
279900930603     C*
280000140219    6C***                END
280100930603     C*
280200941130    5C                   ENDIF
280300941130     C*
280400930512   X4C                   ELSE
280500930512     C*
280600950308RM*  C*§SE VARIATO FERMO DEPOSITO E NON E' LA 2 BOLLA --> ERRORE
280700950308RM* 5C*§         TASFFD    IFNE ARBFFD
280800950308RM*  C*§         §3ATB2    ANDNECOMTBL
280900950308RM*  C*§                   MOVELERR,9     ARBDER
281000950308RM*  C*§                   EXSR WTRERR
281100950308RM* 5C*§                   END
281200990625     C**
281300990625     C* MEMORIZZO ULTIME 4 CIFRE CODICE FATTURAZIONE 2 BOLLA
281400991021    4C     §3ATB2        IFEQ      tastbl
281500990625     C                   MOVE      TASKSC        KSC42BO           4 0
281600930512    4C                   END
281700990625     C*
281800990625    4C                   END
281900930512    3C                   END
282000950202     C**
282100950202     C* VARIAZIONE "Cx" --> consegne particolari
282200950202     C**
282300950202    3C     FLGCVB        IFEQ      'C'
282400950202     C* no errore se 2 bolla
282500991021    4C     §3ATB2        IFNE      tastbl
282600991019     c
282700991019     C                   MOVEL     'TB'          COD
282800991021     C                   MOVEL(P)  tastbl        KEY
282900991019     C     KTAB          CHAIN     TABEL                              32
283000991019     C  N32              MOVEL     TBLUNI        DSTB
283100991019     C   32              MOVEL     *ALL'1'       DSTB
283200991019   4aC     §TBFCB        IFEQ      '1'
283300950202     C*
283400950202     C* STAMPO SOLO SE UNO DEI TRE VARIATO
283500950202    5C     TASFTC        IFEQ      ARBTC1
283600950202     C     TASTC2        ANDEQ     ARBTC2
283700950202     C     TASFTC        OREQ      ARBTC2
283800950202     C     TASTC2        ANDEQ     ARBTC1
283900950202     C*
284000950202   X5C                   ELSE
284100950202     C* CONTROLLO SE LA TARIFFA TASSA LE CONSEGNE PARTICOLARI
284200140219     C***                EXSR      CONPAR
284300950202     C*
284400140219    6C**   *IN17         IFEQ      *ON
284500130325     C****               MOVEL     ERR(9)        ARBDER
284600130325     C****               EXSR      WTRERR
284605120320     c                   exsr      ctr_vrb
284700140219    6C**                ENDIF
284800950202    5C                   ENDIF
284900950202     C*
285000991019   4ac                   ENDIF
285100950202    4C                   ENDIF
285200950202    3C                   ENDIF
285300941130     C**
285400941130     C* VARIAZIONE "Tx" --> tassazione 1 bolla
285500941130     C**
285600941130    3C     FLGCVB        IFEQ      'T'
285700950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
285800130326     C***  ARBFIV        IFGT      0
285900130326     C***  TASFIV        ANDNE     ARBFIV
286000950324     C                   Z-ADD     46            COMFEV
286100130326     C***                ENDIF
286200950324     C*
286300930513     C                   MOVEL     ERR(9)        ARBDER
286400990505     c                   eval      wdelarbu='S'
286500920220     C                   EXSR      WTRERR
286600920226     C                   SETON                                        29
286700011029     c                   if        tasias > 0  and tasvas = *blanks
286800011029     c                   movel     'ITL'         tasvas
286900011029     c                   endif
287000990419     c* Controllo se variato l'importo da assicurare se contabilizzata     da assicurare
287100990419     c                   if        *in13 and (arbias <> tasias or
287200990419     c                             arbvas <> tasvas)
287300990419     c*
287400990419     c                   exsr      AGGIAS
287500990419     c*
287600990419     c                   endif
287700941130    3C                   END
287800941130     C**
287900941130     C* VARIAZIONE "2x" --> tassazione 2 bolla
288000941130     C**
288100941130    3C     FLGCVB        IFEQ      '2'
288200991021     C     tastbl        ANDEQ     §3ATB2
288300950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
288400130326     C**   ARBFIV        IFGT      0
288500130326     C**   TASFIV        ANDNE     ARBFIV
288600950324     C                   Z-ADD     46            COMFEV
288700130326     C**                 ENDIF
288800950324     C*
288900950203     C                   MOVEL     §3ATB2        TBLERR
289000930513     C                   MOVEL     ERR(9)        ARBDER
289100920226     C                   EXSR      WTRERR
289200920226     C                   SETON                                        29
289300941130    3C                   END
289400950202     C**
289500950202     C* VARIAZIONE "Mx" --> mittente
289600990628     c*                     la variazione del mittente dipende dalla conab
289700990628     c*                     della prima bolla, a prescindere da come è messa
289800990628     c*                     la seconda: se aggiono la  aggiorno anche la 2
289900950202     C**
290000950202    3C     FLGCVB        IFEQ      'M'
290100991021    5C     §3ATB2        ANDNE     tastbl
290101130326     C                   Z-ADD     46            COMFEV
290200950202     C                   MOVEL     ERR(9)        ARBDER
290300950202     C                   EXSR      WTRERR
290400950202     C                   SETON                                        29
290500950202    3C                   END
290600941130    2C                   END
290700930512    1C                   END
290800941130     C*
290900920226     C                   ENDSR
291000920228     C**---------------------------------------------------------
291100920228     C** CHAIN SU PRIMA BOLLA
291200920228     C**---------------------------------------------------------
291300920228     C     CHTB1         BEGSR
291400920228     C                   MOVEL     SAVTB1        COMTBL
291500941130     C                   SETOFF                                       020304
291600990625     C     KBLP          CHAIN     TITAS30C                           39
291601121108     c                   exsr      memo_dstitas
291700920228     C*
291800990414     C                   EXSR      CTR3
291900920228     C                   ENDSR
292000920228     C**---------------------------------------------------------
292100920228     C** CHAIN SU SECONDA BOLLA
292200920228     C**---------------------------------------------------------
292300920228     C     CHTB2         BEGSR
292400920228     C                   MOVEL     SAVTB2        COMTBL
292500941130     C                   SETOFF                                       020304
292600990625     C     KBLP          CHAIN     TITAS30C                           39
292601121108     c                   exsr      memo_dstitas
292700920228     C*
292800990414     C                   EXSR      CTR3
292900920228     C                   ENDSR
293000920226     C**---------------------------------------------------------
293100920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
293200920226     C**---------------------------------------------------------
293300920226     C     CTR2          BEGSR
293400920302     C                   MOVEL     COMTBL        FLGTB1            1
293500950202     C* SPEDIZIONE 1) GIA' IMPONIBILE     SE NO FATTURA E NO ASSEGNATO
293600950202     C*               SE VARIAZOINE DA ARRIVO
293700950202     C*            2) GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
293800950202     C*            --> NON POSSO VARIARE
293900950202    1C     TASDFT        IFGT      0
294000950202    2C     TASFIC        IFNE      ' '
294100920226     C                   SETON                                        29
294101130326     c                   z-add     46            comfev
294200991015     C                   MOVEL     ERR(9)        ARBDER
294300920226     C                   EXSR      WTRERR
294400950202    2C                   END
294500920226     C*
294600950202   X1C                   ELSE
294700920226     C*
294800990624    2C     TASimv        IFNE      *zeros
294900950202     C     COMCVB        ANDNE     'TP'
295000950202    3C     FLGTB1        ANDNE     'A'
295100920302     C                   SETON                                        29
295200920226     C*
295300991015     C
295400991015     C   29              MOVEL     ERR(8)        ARBDER
295401130326     c   29              z-add     46            comfev
295500920226     C   29              EXSR      WTRERR
295600950202    2C                   END
295700920302     C*
295800950202    1C                   END
295900920226     C                   ENDSR
296000920226     C**---------------------------------------------------------
296100920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
296200941130     C** PER LE VAIRAZIONI C/ASSEGNO O TIPI BOLLA
296300920226     C**---------------------------------------------------------
296400920226     C     CTR3          BEGSR
296500920303     C                   SETOFF                                       2938
296600920226     C* SPEDIZIONE GIA' IMPONIBILE     SE NO FATTURA
296700920226     C*            GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
296800920226     C*            NON POSSO VARIARE
296900920226     C*
297000990414     C  N02              SETON                                        2936
297100941202     C   02TASDFT        IFGT      0
297200990414     C     TASFIC        ANDNE     ' '
297300920227     C                   SETON                                        2936
297400920226     C*
297500920226     C                   ELSE
297600920226     C*
297700990624     C     TASimv        IFNE      *zeros
297800920226     C                   SETON                                        38
297900920226     C                   END
298000920226     C*
298100920226     C                   END
298200920226     C                   ENDSR
298300920211     C**---------------------------------------------------------
298400920211     C** SCRIVO FILE ERRORI
298500920211     C**---------------------------------------------------------
298600920211     C     WTRERR        BEGSR
298700110208     c                   clear                   arbdat
298800920211     C*
298900970916     C* CREO IL RECORD DI ERRORE SOLO SE LA DATA VARIAZIONE E' INFERIOR
299000970916     C* DI 3 GIORNI A OGGI PER LE STAMPA DI SEDE
299100000703    0C     COMFEV        IFEQ      102
299200110208     C     ARBDTV        ANDLE     DATAM3
299300000703     c     *in37         andeq     *on
299400100225
299500110208     c* Se si tratta di codice TP con importo da assicurare, stampo subito
299600100225    0C     COMFEV        oreq      102
299700100225     c     *in13         andeq     *on
299800100225     c     arbias        andgt     0
299900970916     C*é*
300000970916     C     COMFEV        ORNE      102
300100000703     C*é*
300200000703     C     *in37         ORNE      *on
300300970916     C**
300400991015     C     KERR          SETLL     FIARBE2C                               01
300500930702     C* SE NON TROVATO ERRORE
300600930706    1C     *IN01         IFEQ      *OFF
300700110208     c
300800110208     c* Se l'errore è bolla inesistente, aspetto 3 gg prima di ricrearlo
300900110208    2c                   if        arbder=err(1)
301000110208     c                   eval      arbdat=dateu
301100110208     C     KERR          SETLL     FIARBE2C                               01
301200110208    3c                   if        not  *in01
301300110208     c                   eval      arbdat=datam1
301400110208     C     KERR          SETLL     FIARBE2C                               01
301500110208    4c                   if        not  *in01
301600110208     c                   eval      arbdat=datam2
301700110208     C     KERR          SETLL     FIARBE2C                               01
301800110208    4c                   endif
301900110208    3c                   endif
302000110208    2c                   endif
302100110208    1c                   endif
302200110208     c
302300110208    1C     *IN01         IFEQ      *OFF
302400110209     c                   clear                   arbdat
302500930702     C* RISCRIVO ERRORE
302600950203     C* SE NON L'HO GIA' IMPOSTATO CERCO FILIALE A CUI INVIARE STAMPA
302700930706    2C     COMFEV        IFEQ      0
302800920212     C*
302900950203    3C     TBLERR        IFEQ      'F'
303000991015     C                   MOVEL     ARBLNP        ARBFEV
303100930706   X3C                   ELSE
303200991015     C                   MOVEL     TASLNA        ARBFEV
303300930706    3C                   END
303400971215     C* PRENDO TERMINAL DI PARTENZA
303500971215     C                   CLEAR                   DSLV55
303600971215     C                   MOVEL     'P'           D55TPT
303700991015     C                   MOVE      ARBFEV        D55LIN
303800971215     C                   Z-ADD     DATEU         D55DRF
303900971215     C                   CALL      'FNLV55R'
304000971215     C                   PARM                    DSLV55
304100971215     C     D55ERR        IFEQ      *BLANKS
304200991015     C                   MOVE      D55TFP        ARBFEV
304300971215     C                   END
304400920212     C*
304500930706   X2C                   ELSE
304600920212     C*
304700991015     C                   Z-ADD     COMFEV        ARBFEV
304800930706    2C                   END
304900991015     C     ARBFEV        IFEQ      102
305000070420     C*********          MOVE      MBR           ARBLNA
305100950202     C                   ELSE
305200950202     C                   MOVEL     TASLNA        ARBLNA
305300950202     C                   ENDIF
305400920211     C*
305500950201     C   11              WRITE     FNARBDEE
305600920212     C*
305700950201     C   12              WRITE     FNARBKEE
305800950201     C* SCRIVO ANCHE FNARBM0E SOLO SE RICHIESTO
305900930706    2C   12*IN36         IFEQ      '1'
306000950202     C     KARBM         CHAIN     FNARBMRR                           32
306100950201     C  N32              WRITE     FNARBMEE
306200920214     C                   SETOFF                                       36
306300930706    2C                   END
306400920212     C*
306500950203     C     *IN13         IFEQ      *ON
306600991015     C                   WRITE     FIARBTEE
306700950203     C*
306800991015     c* se devo cancellare fiarbu, lo faccio
306900991018     c     arbtrc        ifeq      '1'
307000991015     C     KARBU         SETLL     FIARBU1R
307100991015     C     KARBU         READE     FIARBU1R                               30
307200950203     C*
307300950203     C     *IN30         DOWEQ     *OFF
307400991015     C                   WRITE     FIARBUEE                             30
307500950203     C* SE POSSO LI DELETO
307600990505     c  n37              if        wdelarbu='S'
307700990505     C                   ADD       1             NUMRBU
307800991015     C                   DELETE    FIARBURR
307900990505     c                   endif
308000950203     C*
308100991015     C     KARBU         READE     FIARBU1R                               30
308200950203     C                   ENDDO
308300991018     c                   endif
308400950203     C                   ENDIF
308500950203     C*
308600950202     C   15              WRITE     FNARBGEE
308700950202     C   16              WRITE     FNARBMEE
308800991214   X1c                   else
308900991214     c* se devo cancellare fiarbu, lo faccio
309000991214    2c     *in37         ifeq      *off
309100991214     c     wdelarbu      andeq     'S'
309200991214     c     arbtrc        andeq     '1'
309300991214     C     KARBU         SETLL     FIARBU1R
309400991214     C     KARBU         READE     FIARBU1R                               30
309500991214     C*
309600991214    3C     *IN30         DOWEQ     *OFF
309700991214     C                   ADD       1             NUMRBU
309800991214     C                   DELETE    FIARBURR
309900991214     C     KARBU         READE     FIARBU1R                               30
310000991214    3C                   ENDDO
310100991214    2c                   endif
310200930706    1C                   ENDIF
310300970916    0C                   ENDIF
310400920212     C*
310500920212     C                   Z-ADD     0             COMFEV            3 0
310600950203     C                   MOVEL     §3ATB1        TBLERR
310700920211     C                   ENDSR
310800930514     C**---------------------------------------------------------
310900930514     C** CONTROLLO SE LA SPEDIZIONE INESISTENTE E' STORNATA
311000930514     C**---------------------------------------------------------
311100930514     C     STORNO        BEGSR
311200930514     C*
311300930514     C* SE E' SINGOLA BOLLA
311400930514     C     §3ATB2        IFEQ      '  '
311500930514     C*
311600991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
311700991021     c* (cioè il franco) faccio setgt e readpe invece che chain
311800991021     C     KANN          setgt     TITAS30C
311900991021     C     KANN          readpe    TITAS30C                               23
312000931119     C*
312100941130     C     *IN23         IFEQ      *OFF
312200930514     C                   MOVEL     'TB'          COD
312300941130     C                   MOVEL(P)  TASTBL        KEY
312400930514     C     KTAB          CHAIN     TABEL                              32
312500930514     C  N32              MOVEL     TBLUNI        DSTB
312600930514     C   32              MOVEL     *ALL'1'       DSTB
312700930514     C*
312800930514     C* SE NON E' STORNO STAMPO ERRORE
312900930514     C     §TBFCB        IFEQ      '1'
313000930514     C                   SETON                                        23
313100930514     C                   ELSE
313200930514     C                   MOVEL     '1'           ST1               1
313300930514     C                   END
313400930514     C                   END
313500930514     C*
313600930514     C                   END
313700930514     C                   ENDSR
313800930804     C*
313900950202     C**---------------------------------------------------------
314000950202     C* VEDO SE IN TARIFFE C'E' CONSEGNA PARTICOLARE
314100950202     C**---------------------------------------------------------
314200930804     C     CONPAR        BEGSR
314300930804     C                   SETOFF                                       17
314400930804     C*
314500930804     C                   MOVE      TASKSC        KSC4              4 0
314600930804     C*
314700930804     C* SE E' 8888 O 9999 ERRORE SUBITO PERCHE' UTILIZZO TARIFFE CART
314800930804    2C     KSC4          IFEQ      9999
314900930804     C     KSC4          OREQ      8888
315000930804     C                   SETON                                        17
315100930804     C*
315200930804   X2C                   ELSE
315300930804     C*
315400930804     C                   Z-ADD     999           TARPRG
315500930804     C                   Z-ADD     999           PREPRG
315600991021     C                   MOVEL     tastbl        UNOTB1
315700930804     C*
315800930805     C* CIO' CHE NON DEVE ESSERE LA TARIFFA (SICURAMENTE UN FRANCO
315900930805     C*  NON PUO' AVERE IN TARIFFA A DI ARRIVO)
316000930804    3C     UNOTB1        IFEQ      'F'
316100930805     C                   MOVEL     'A'           TFP               1
316200930804     C                   ELSE
316300930805     C                   MOVEL     'F'           TFP
316400930804    3C                   END
316500930804     C*
316600930804     C* SE C'E' A ZERO NON DO' L'ERRORE
316700941130     C     KTAM          SETLL     TNTAM000
316800941130     C     KTAM          READE     TNTAM000                               35
316900930804     C*
317000930804    3C     *IN35         DOWEQ     *OFF
317100930804     C*
317200930804     C* ESCLUDO SE ANNULLATO  BLOCCATA O PER ARRIVI
317300930804    4C     TAMATB        IFNE      'A'
317400930804     C*
317500941205    5C     TAMBAP        IFNE      'B'
317600941205     C     TAMBAP        ANDNE     TFP
317700930804     C*
317800930804     C                   MOVE      TAMPRG        TARPRG            3 0
317900930804     C*
318000930804     C* VEDO SE E' TARIFFA VALIDA
318001121116     c                   movel     tasaas        tasdsp
318002121116     c                   move      tasmgs        tasdsp
318100120430    6C     TASDSP        IFGE      TAMDDT
318200120430     C     TASDSP        ANDLE     TAMDST
318300120430     C                   SETON                                        35
318400120430    6C                   END
318500930804     C*
318600930804   X5C                   ELSE
318700930804     C                   MOVE      TAMPRG        PREPRG            3 0
318800930804    5C                   END
318900930804    4C                   END
319000930804     C*
319100941130     C  N35KTAM          READE     TNTAM000                               35
319200930804    3C                   END
319300930804     C*
319400930804     C* NON TROVATA TARIFFA: STAMPO ERRORE
319500930804    3C     TARPRG        IFEQ      999
319600930804     C     PREPRG        ANDEQ     999
319700930804     C                   SETON                                        17
319800930804   X3C                   ELSE
319900930804     C*
320000930804    4C     TARPRG        IFNE      999
320100930804     C                   MOVEL     TARPRG        PRG
320200930804   X4C                   ELSE
320300930804     C                   MOVEL     PREPRG        PRG
320400930804    4C                   ENDIF
320500930804     C*
320600930804     C* VEDO SE TARIFFA PARTICOLARE A 0
320700950202     C     ARBTC1        IFNE      ' '
320800990719     C                   MOVEL(P)  ARBTC1        WFTC
320900950202     C                   EXSR      PARTIC
321000950202     C                   ENDIF
321100950202     C  N17ARBTC2        IFNE      ' '
321200990719     C                   MOVEL(P)  ARBTC2        WFTC
321300950202     C                   EXSR      PARTIC
321400950202     C                   ENDIF
321500950202     C*
321600930804    3C                   END
321700930804    2C                   END
321800930804     C*
321900930804     C                   ENDSR
322000950202     C**---------------------------------------------------------
322100950202     C* CONTROLLO IN TPT
322200950202     C**---------------------------------------------------------
322300950202     C     PARTIC        BEGSR
322400950202     C*
322500990719     C     KTAM2         CHAIN     TITPT000                           17
322600950202     C* 17 ON - NON ESISTE TARIFFA
322700950202    4C     *IN17         IFEQ      *OFF
322800950202     C*         NON ESISTE PERCHE' ANNULLATA
322900950202    5C     TPTATB        IFEQ      'A'
323000950202     C                   SETON                                        17
323100950202   X5C                   ELSE
323200950202     C* VEDO SE ESISTE DETTAGLIO TARIFFA PARTICOLARE
323300990719     C     KTAM2         SETLL     TITPD000
323400990719     C     KTAM2         READE     TITPD000                               35
323500950202     C*
323600950202    6C     *IN35         DOWEQ     *OFF
323700950202     C* NO ANNULLATO
323800950202    7C     TPDATB        IFEQ      ' '
323900950202     C* SE > 0 ERRORE
324000950202    8C     TPDITR        IFNE      0
324100950202     C     TPDMIN        ORNE      0
324200950202     C     TPDMAX        ORNE      0
324300950202     C                   SETON                                        1735
324400950202    8C                   END
324500950202    7C                   END
324600950202     C*
324700990719     C  N35KTAM2         READE     TITPD000                               35
324800950202    6C                   END
324900950202    5C                   END
325000950202    4C                   END
325100950202     C                   ENDSR
325200941130     C**---------------------------------------------------------
325300941130     C* CONTROLLO SE ESISTE VARIA "G"
325400941130     C**---------------------------------------------------------
325500941130     C     ESVARG        BEGSR
325600941130     C                   CLEAR                   WSIVAR
325700941130     C*
325800941130     C                   SETON                                        30
325900941130    3C     TASSV1        IFNE      'G'
326000941130     C     TASSV2        ANDNE     'G'
326100941130     C     TASSV3        ANDNE     'G'
326200941130     C                   MOVEL     'G'           WSV1
326300990625     C     KBLPV         SETLL     TITA730C                               30
326400941130    3C                   ENDIF
326500941130     C*
326600941130     C     *IN30         IFEQ      *ON
326700941130     C                   MOVEL     'S'           WSIVAR            1
326800941130     C                   ENDIF
326900941130     C                   ENDSR
327000941202     C**---------------------------------------------------------
327100941202     C* CONTROLLO SE HO MODIFICATO IL DESTINATARIO -------------------*
327200941202     C**---------------------------------------------------------
327300941202     C     CTRDES        BEGSR
327400941202     C*
327500950202     C                   MOVEL     CSBRSD        ALF35            35
327600941202     C* CONTROLLO LA 1 POSIZIONE <> ' '
327700950202     C     ' '           CHECKR    ALF35         C                 3 0    30
327800950202     C                   MOVE      ALF35         W001A             1
327900941202     C*
328000941202     C* CONFRONTO CON IL CAMPO CHE HO RICEVUTO
328100941202     C     *IN30         IFEQ      *ON
328200941215     C     W001A         ANDEQ     ' '
328300950202     C                   MOVEA     ARBRSD        K35
328400941202     C                   ADD       1             C
328500950202     C                   MOVEA     *BLANKS       K35(C)
328600950202     C                   MOVEA     K35           W035A            35
328700941202     C                   ELSE
328800950202     C                   MOVEL     ARBRSD        W035A
328900941202     C                   ENDIF
329000941202     C*
329100950202     C     W035A         IFEQ      ALF35
329200941202     C                   SETOFF                                       31
329300941202     C                   END
329400941202     C*
329500941202     C                   ENDSR
329600990415     C**------------------------------------------------------------------------------
329700990415     C** VARIAZIONE IMPORTO D'ASSICURARE PER SPEDIZIONE GIA' TASSATA / CONTABILIZZATA
329800990415     C**------------------------------------------------------------------------------
329900990415     c*
330000990419     c     AGGIAS        begsr
330100990415     c*
330200990415     c* controllo se la spedizione è già stata fatturata / contabilizzata oppure
330300990415     c* se è stato calcolato l'imponibile
330400990415     c*
330500990415     c                   exsr      ctr3
330600990415     c* se è acceso il  38 vuol dire che non è stata contabilizzata ma è stato calcolato
330700991018     c* fino all'imponibile e quindi devo andare a controllare se è già stata
330800990415     c* calcolata la varia "R"
330900990519     c                   select
331000990519    1c                   when      *in38
331100990421     c*
331200990421     c                   z-add     tasias        arbiap
331300990421     c                   movel     tasvas        arbvap
331400990421     c                   movel     'R'           arbvar
331500991019     c*
331600991019     c* verifico se devo registrare messagio per ufficio assicurazione
331700991019     C* relativo a spedizione avente tipo bolla non assicurabile per la
331800991019     c* quale è stato tolto, modificato o aggiunto l'importo da assicurare
331900991019     c* Escludo le bolle di recupero
332000991019     c     tblrbl        ifne      'R'
332100991019     c                   exsr      msgass
332200991019     c                   endif
332201131129
332202131129     c                   clear                   arbiap
332203131129     c                   clear                   arbvap
332204131129     c                   clear                   arbvar
332300990415     c*
332400990824     c* Non faccio niente se spedizione:
332500990824     c* - contabilizzata
332600990824     c* - tasias> 0
332700990824     c* - tasfcm="F" (tasias immesso dalla sede (dalla fatturazione))
332800990824     c* - arbias= 0
332900990824     c                   when      *in29 and tasias > 0 and tasfcm = 'F'
333000990824     c                                   and arbias = 0
333100990824     c*
333200990519     c* se è acceso il 29  vuol dire che è stata CONTABILIZZATA
333300990519    1c                   when      *in29
333400990505     c                   eval      wdelarbu='S'
333500990415     c
333600990415     c* verifico l'esistenza della varia "R"
333700990415     c                   exsr      esvarr
333800990419     c*
333900990419     c                   z-add     tasias        arbiap
334000990419     c                   movel     tasvas        arbvap
334100990419     c                   movel     'R'           arbvar
334200990415     c*
334300990519    2c                   SELECT
334400990415     c*
334500990415     c* importo in TAS = SI
334600990415     c* importo in ARB = NO
334700990415     c* varia   "R"    = SI
334800990415     c*                            errore no tolta varia "R"
334900991019     c                   when      tasias > 0 and arbias = 0
335000130325     c***                if        Wvariar = 'S'
335100130325     c***                movel     err(34)       arbder
335200130325     c***                exsr      WTRERR
335300130325     c***                endif
335302121203     c                   exsr      ctr_vrb
335400990415     c* messaggio all'Ufficio Assicurazione
335500990519     c* escludo le bolle di recupero
335600990519     c                   if        tblrbl<>'R'
335700990415     c                   z-add     603           comfev
335800990415     c                   movel     err(32)       arbder
335900990419     c                   exsr      wtrerr
336000990519     c                   endif
336100990415     c*
336200990415     c* importo in TAS = NO
336300990415     c* importo in ARB = SI
336400990415     c* varia   "R"    = NO
336500990415     c*                            errore no aggiunta varia "R"
336600991019     c                   when      tasias = 0 and arbias > 0
336700130325     c                   if        wvariar = ' '
336800130325     c***                movel     err(35)       arbder
336900130325     c***                exsr      wtrerr
336902120423     c                   exsr      ctr_vrb
337000991019     c                   endif
337100990415     c* messaggio all'Ufficio Assicurazione
337200990519     c                   if        tblrbl<>'R'
337300990415     c                   z-add     603           comfev
337400990415     c                   movel     err(31)       arbder
337500990419     c                   exsr      wtrerr
337600990519     c                   endif
337700990415     c*
337800990415     c* importo in TAS = SI
337900990415     c* importo in ARB = SI  ma diversa da tas
338000990415     c* varia   "R"    = SI
338100990415     c*                            errore no modificata varia "R"
338200991019     c                   when      tasias <> arbias
338300991019     c                   if        wvariar = 'S'
338400130325     c***                movel     err(36)       arbder
338500130325     c***                exsr      wtrerr
338502120423     c                   exsr      ctr_vrb
338600991019     c                   endif
338700990415     c* messaggio all'Ufficio Assicurazione
338800990519     c                   if        tblrbl<>'R'
338900990415     c                   z-add     603           comfev
339000990415     c                   movel     err(33)       arbder
339100990419     c                   exsr      wtrerr
339200990519     c                   endif
339300990415     c*
339400990519    2c                   endsl
339500990415     c*
339600990624     c* aggiorno in TITAS il valore dell'importo d'assicurare
339700990415     c                   z-add     arbias        tasias
339800990415     c                   movel     arbvas        tasvas
339900990824     c                   clear                   tasfcm
340000990415     c*
340100990624     c   02              update    titas000
340200990624     c   03              update    titas010
340300990624     c   04              update    titasp00
340400990419     c*
340500990419     c                   clear                   arbiap
340600990419     c                   clear                   arbvap
340700990419     c                   clear                   arbvar
340800990419     c**
340900990415     c* riaccendo il 29 e spengo il 36
341000990415     c                   seton                                        29
341100990415     c                   setoff                                       36
341200991019     c* Se e' bolla normale, ma non compare nel tabulato per la
341300991019     c*  Consuldanni, stampo solo per l'ufficio assicurazione
341400990519     c                   other
341500990519     c                   if        tblrbl<>'R'
341600000410     c                   z-add     tasias        arbiap
341700000410     c                   movel     tasvas        arbvap
341800000410     c                   movel     'R'           arbvar
341900991019     c                   exsr      msgass
342000000410     c                   clear                   arbiap
342100000410     c                   clear                   arbvap
342200000410     c                   clear                   arbvar
342300990519     c                   endif
342400990519    1c                   ENDSL
342500990415     c*
342600990415     c                   endsr
342700990415     c*
342800991019     C**-------------------------------------------------------------
342900991019     C** messaggi per ufficio Assicurazione per tipo bolla non assic.
343000991019     C**-------------------------------------------------------------
343100991019     C     msgass        BEGSR
343200991019     C                   MOVEL     'TB'          COD
343300991019     c                   movel(P)  tastbl        key
343400991019     C     KTAB          CHAIN     TABEL                              32
343500991019     C  N32              MOVEL     TBLUNI        DSTB
343600991019     C   32              clear                   DSTB
343700991019     c* se bolla non assicurabile e aggiunto, tolto o modificato importo
343800991019     c* da assicurare devo stampare messaggio per ufficio Assicurazione
343900991019    1c     §tbfas        ifeq      '0'
344000991019    2c                   select
344100991019     c* tolto importo da assicurare
344200991019   w1c                   when      tasias > 0 and arbias = 0
344300991019     c                   z-add     603           comfev
344400991019     c                   movel     err(41)       arbder
344500991019     c                   exsr      wtrerr
344600991019     c* aggiunto importo da assicurare
344700991019   w2c                   when      tasias = 0 and arbias > 0
344800991019     c                   z-add     603           comfev
344900991019     c                   movel     err(40)       arbder
345000991019     c                   exsr      wtrerr
345100991019     c* modificato importo da assicurare
345200991019   w3c                   when      tasias <> 0
345300991019     c                   z-add     603           comfev
345400991019     c                   movel     err(42)       arbder
345500991019     c                   exsr      wtrerr
345600991019    2c                   endsl
345700991019    1c                   endif
345800991019     c                   endsr
345900990415     C**---------------------------------------------------------
346000990415     C* CONTROLLO SE ESISTE VARIA "R"
346100990415     C**---------------------------------------------------------
346200990415     C     ESVARR        BEGSR
346300990415     C                   CLEAR                   WVARIAR           1
346400990415     C*
346500990415     C                   SETON                                        30
346600990415    3C     TASSV1        IFNE      'R'
346700990415     C     TASSV2        ANDNE     'R'
346800990415     C     TASSV3        ANDNE     'R'
346900990415     C                   MOVEL     'R'           WSV1
347000990624     C     KBLPV         SETLL     TITA730C                               30
347100990415    3C                   ENDIF
347200990415     C*
347300990415     C     *IN30         IFEQ      *ON
347400990415     C                   MOVEL     'S'           WVARIAR
347500990415     C                   ENDIF
347600990415     C                   ENDSR
347700941130     C**---------------------------------------------------------
347800941130     C* ROUTINE INIZIALE
347900941130     C**---------------------------------------------------------
348000941130     C     *INZSR        BEGSR
348100021014
348200941130     C     *ENTRY        PLIST
348300941130     C                   PARM                    KPJBA
348400021014
348500941130     C                   Z-ADD     1             CODUT             1 0
348600021014
348700021014     c     *dtaara       define    §azute        azuteds
348800021014     c     *dtaara       define    §datiute      ddatiute
348900021014     C                   in(E)     *dtaara
349000021014     C                   IF        %Error  or  RSUT = *blanks
349100021014     C                   CLEAR                   TIBS34DS
349200021014     C                   call      'TIBS34R'
349300021014     C                   parm                    Tibs34Ds
349400021014     C                   in        *dtaara
349500021014     c                   ENDIF
349600021014
349700021014     C                   Z-ADD     DUTKCI        KCI               4 0
349800021014
349900941130     C*---------------------------------------------------------------*
350000991018     C     KBLP          KLIST
350100950201     C                   KFLD                    ARBAAS
350200941130     C                   KFLD                    ARBLNP
350300941130     C                   KFLD                    ARBNRS
350400941130     C                   KFLD                    ARBNSP
350500941130     C                   KFLD                    COMTBL            2
350600990629     C     KTITA         KLIST
350700990629     C                   KFLD                    TASAAS
350800990629     C                   KFLD                    TASLNP
350900990629     C                   KFLD                    TASNRS
351000990629     C                   KFLD                    TASNSP
351100990629     C                   KFLD                    §TRC
351200941130     C     KBLPV         KLIST
351300950201     C                   KFLD                    ARBAAS
351400941130     C                   KFLD                    ARBLNP
351500941130     C                   KFLD                    ARBNRS
351600941130     C                   KFLD                    ARBNSP
351700941130     C                   KFLD                    COMTBL            2
351800941130     C                   KFLD                    WSV1
351900941130     C     KANN          KLIST
352000950201     C                   KFLD                    ARBAAS
352100941130     C                   KFLD                    ARBLNP
352200941130     C                   KFLD                    ARBNRS
352300941130     C                   KFLD                    ARBNSP
352400950202     C     KARBU         KLIST
352500941130     C                   KFLD                    ARBAAS
352600941130     C                   KFLD                    ARBLNP
352700941130     C                   KFLD                    ARBNRS
352800941130     C                   KFLD                    ARBNSP
352900991015     C                   KFLD                    ARBTRC
353000941130     C                   KFLD                    ARBDTV
353100941130     C                   KFLD                    ARBORV
353200950202     C     KARBM         KLIST
353300950202     C                   KFLD                    ARBAAS
353400950202     C                   KFLD                    ARBLNP
353500950202     C                   KFLD                    ARBNRS
353600950202     C                   KFLD                    ARBNSP
353700950202     C                   KFLD                    ARBCVB
353800950202     C                   KFLD                    ARBDTV
353900950202     C                   KFLD                    ARBORV
354000941130     C     KARBD         KLIST
354100941130     C                   KFLD                    ARBAAS
354200941130     C                   KFLD                    ARBLNP
354300941130     C                   KFLD                    ARBNRS
354400941130     C                   KFLD                    ARBNSP
354500941130     C                   KFLD                    ARBCVB
354600941130     C                   KFLD                    ARBDTV
354700941130     C                   KFLD                    COMORV            6 0
354800941130     C     KTAB          KLIST
354900941130     C                   KFLD                    CODUT
355000941130     C                   KFLD                    COD               2
355100941130     C                   KFLD                    KEY               8
355200941130     C     KACO          KLIST
355300941130     C                   KFLD                    CODUT
355400941130     C                   KFLD                    KCI
355500941130     C                   KFLD                    TASKSC
355600950202     C     KACO2         KLIST
355700950202     C                   KFLD                    CODUT
355800950202     C                   KFLD                    KCI
355900950202     C                   KFLD                    ARBCCM
356000981118     C     Kppa          KLIST
356100981118     C                   KFLD                    wsoc
356200981118     C                   KFLD                    wctb
356300981118     C                   KFLD                    wkcc
356400981118     C                   KFLD                    wksc
356500981118     C                   KFLD                    wdft
356600981118     C                   KFLD                    wnft
356700981118     C                   KFLD                    wfiv
356800981118     C     Kycpa         KLIST
356900981118     C                   KFLD                    ppasys
357000981118     C                   KFLD                    ppanrasint
357100941130     C     KERR          KLIST
357200941130     C                   KFLD                    ARBAAS
357300941130     C                   KFLD                    ARBLNP
357400941130     C                   KFLD                    ARBNRS
357500941130     C                   KFLD                    ARBNSP
357600941130     C                   KFLD                    ARBDER
357700941130     C                   KFLD                    ARBDAT
357800991214     c                   kfld                    ARBDTV
357900991214     c                   kfld                    ARBORV
358000941130     C     KTAM          KLIST
358100941130     C                   KFLD                    TASKSC
358200941130     C                   KFLD                    TASCTR
358300941130     C     KTAM2         KLIST
358400941130     C                   KFLD                    TASKSC
358500941130     C                   KFLD                    TASCTR
358600941130     C                   KFLD                    PRG               3 0
358700950202     C                   KFLD                    WFTC
358737121108
358738121108
358739121108
358800941130     C**
358900941130     C* DEFINIZIONE CAMPI
359000941130     C**
359100960625     C     *LIKE         DEFINE    TASTBL        SAVTBL
359200990719     C     *LIKE         DEFINE    tptftc        WFTC
359300950202     C     *LIKE         DEFINE    TASVLF        SAVVLF
359400950202     C     *LIKE         DEFINE    TASVLF        SAVVLB
359500950202     C     *LIKE         DEFINE    TASFVF        SAVFVF
359600950202     C     *LIKE         DEFINE    TASFVF        SAVFVB
359700990625     C     *LIKE         DEFINE    TAScts        SAVcts
359800941130     C     *LIKE         DEFINE    TASSV1        WSV1
359900941130     C     *LIKE         DEFINE    TASVA1        WVA1
360000941130     C     *LIKE         DEFINE    ARBDER        SAVDER
360100941130     C     *LIKE         DEFINE    CSBPAR        DESPAR
360200970916     C     *LIKE         DEFINE    CSBPAR        DESPRV
360300010126     C     *LIKE         DEFINE    arblnp        savlnp
360400010126     C     *like         define    tasftc        WTCP
360500010126     C     *like         define    tasftc        WsvTCP
360600941130     C*---------------------------------------------------------------*
360700970916     C                   TIME                    W0140            14 0
360800970916     C                   MOVEL     W0140         UTIME             6 0
360900970916     C                   MOVE      W0140         WDTGIO            8 0
361000970916     C* UDATE IN AAAAMMGG
361100970916     C                   Z-ADD     WDTGIO        G02DAT
361200970916     C                   MOVEL     *BLANK        G02ERR
361300970916     C                   CALL      'XSRDA8'
361400970916     C                   PARM                    WLBDAT
361500970916     C                   MOVEL     G02INV        DATEU             8 0
361600110208     C* SOTTRAGGO  1
361700110208     C     G02TGI        SUB       1             GIOTGI
361800110208     C                   CALL      'XSRGI8'
361900110208     C                   PARM                    WGIDAT
362000110208     C                   Z-ADD     GIOINV        DATAM1            8 0
362100110208     C* SOTTRAGGO  2
362200110208     C     G02TGI        SUB       2             GIOTGI
362300110208     C                   CALL      'XSRGI8'
362400110208     C                   PARM                    WGIDAT
362500110208     C                   Z-ADD     GIOINV        DATAM2            8 0
362600970916     C* SOTTRAGGO  3
362700970916     C     G02TGI        SUB       3             GIOTGI
362800970916     C                   CALL      'XSRGI8'
362900970916     C                   PARM                    WGIDAT
363000110208     C                   Z-ADD     GIOINV        DATAM3            8 0
363100941130     C*
363200941130     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
363300941130     C* ASSUMO IL PRIMO MEMBRO DEL FILE
363400941130     C     MBRFIL        IFEQ      *BLANK
363500020827     C                   MOVEL     'M888888888'  MBRFIL
363600941130     C                   END
363700941130     C* MEMBRO LUNGO 3
363800941130     C                   MOVEL     ALFFIL        MBR               3 0
363900941130     C* PRENDO ANNO IN CORSO
364000941130     C                   MOVE      UDATE         AAS               2 0
364100070119     c* Apro file bolle di filiale
364200070119     c                   clear                   tibs55ds
364300070119     c                   eval      i50apo=001
364400070119     c                   call      'TIBS55R'
364500070119     c                   parm                    tibs55ds
364600070119     c*
364700070119     c                   Clear                   commt
364800070119     c                   Movel(p)  cmdt(1)       commt
364900070119     c                   Eval      %Subst(commt:30:10) = o50ala
365000070119     c                   Eval      commt =
365100070119     c                             %trim(commt) + '/FNBLP01L)'
365200070119     c                   Call      'QCMDEXC'
365300070119     c                   Parm                    commt
365400070119     c                   Parm                    lenght
365500070119     c                   Open      FNBLP01L
365600070119     c
365700070119     c
365800071102      * Aggancio tabella VPO record DECO
365900120510     c**********         Clear                   Dvpodeco
366000120510     c***********        Clear                   Tibs02ds
366100120510     c***********        Eval      T02tla = 'L'
366200120510     c***********        Eval      T02Mod = 'C'
366300120510     c*************      Eval      T02Sif = knsif
366400120510     c*************      Eval      T02Cod = 'VPO'
366500120510     c***********        Movel(p)  'DECO'        T02Ke1
366600120510     c*******            Call      'TIBS02R'
366700120510     c*********          Parm                    Kpjba
366800120510     c*********          Parm                    Tibs02ds
366900120510     c*********          If        T02Err = *Blanks
367000120510     c*********          Movel     T02Uni        Dvpodeco
367100120510     c**********         EndIf
367200071102
367300941130     C                   ENDSR
367400930804     C*
367500981001     C**-------------------------------------------------------------
367600981001     C** AGGIORNO CON CODICI DI VARIAZIONE DM DD BOLLA SEDE MITT/DEST
367700981001     C**-------------------------------------------------------------
367800981001     C     AGTMD         BEGSR
367900981001     C* CONTROLLO CHE TIPO DI VARIAZIONE STO LEGGENDO
368000981001     C* DM CREAZIONE MITTENTE BOLLA SEDE
368100981001     C* DD CREAZIONE DESTINATARIO BOLLA SEDE
368200981001     C* I0 VARIAZIONE DESTINATARIO
368300981001     C* MI VARIAZIONE MITTENTE
368400981001     C*
368500981001    1C     ARBCVB        IFNE      'DM'
368600981001     C     ARBCVB        ANDNE     'DD'
368700981001     C     ARBCVB        ANDNE     'I0'
368701140407     C     ARBCVB        ANDNE     'I7'
368702140407     C     ARBCVB        ANDNE     'I8'
368703140407     C     ARBCVB        ANDNE     'I9'
368800981001     C     ARBCVB        ANDNE     'MI'
368900981001     C*
369000981001   X1C                   ELSE
369100981001     C* CERCO LA SPEDIZIONE NEL FILE TNTMD
369200981001     C     KANN          CHAIN     TNTMD01L                           48
369300981001     C* IMMISSIONE O CAMBIO MITTENTE
369400981001     C* CAUSALI DI VARIAZIONI
369500981001    2C     ARBCVB        IFEQ      'DM'
369600981001     C     ARBCVB        OREQ      'MI'
369700981001     C*
369800981001     C                   MOVEL     ARBRSM        TMDRSM
369900981001     C                   MOVEL     ARBINM        TMDINM
370000981001     C                   MOVEL     ARBCAM        TMDCAM
370100981001     C                   MOVEL     ARBLOM        TMDLOM
370200981001     C                   MOVEL     ARBPRM        TMDPRM
370300981001     C                   MOVEL     ARBNZM        TMDNZM
370400981001     C*
370500981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
370600981001    3C     *IN48         IFEQ      '1'
370700981005     C     ARBCVB        ANDEQ     'DM'
370800981001     C                   Z-ADD     ARBAAS        TMDAAS
370900981001     C                   Z-ADD     ARBLNP        TMDLNP
371000981001     C                   Z-ADD     ARBNRS        TMDNRS
371100981001     C                   Z-ADD     ARBNSP        TMDNSP
371200981001     C                   WRITE     TNTMD000
371300981001     C*
371400981005    3C                   ENDIF
371500981005     C* PER 48 SPENTO AGGIORNO IL RECORD
371600981005     C  N48              UPDATE    TNTMD000
371700981001     C*
371800981001    2C                   ENDIF
371900981001     C*
372000981001     C* IMMISSIONE O CAMBIO DESTINATARIO
372100981001     C* CAUSALI DI VARIAZIONI
372200981001    2C     ARBCVB        IFEQ      'DD'
372300981001     C     ARBCVB        OREQ      'I0'
372301140407     C     ARBCVB        OREQ      'I7'
372302140407     C     ARBCVB        OREQ      'I8'
372303140407     C     ARBCVB        OREQ      'I9'
372400981001     C*
372500981001     C                   MOVEL     ARBRSD        TMDRSD
372600981001     C                   MOVEL     ARBIND        TMDIND
372700981001     C                   MOVEL     ARBCAD        TMDCAD
372800981001     C                   MOVEL     ARBLOD        TMDLOD
372900981001     C                   MOVEL     ARBPRD        TMDPRD
373000981001     C                   MOVEL     ARBNZD        TMDNZD
373100981001     C*
373200981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
373300981001    3C     *IN48         IFEQ      '1'
373400981005     C     ARBCVB        ANDEQ     'DD'
373500981001     C                   Z-ADD     ARBAAS        TMDAAS
373600981001     C                   Z-ADD     ARBLNP        TMDLNP
373700981001     C                   Z-ADD     ARBNRS        TMDNRS
373800981001     C                   Z-ADD     ARBNSP        TMDNSP
373900981001     C                   WRITE     TNTMD000
374000981005    3C                   ENDIF
374100981005     C*
374200981001     C* PER 48 SPENTO AGGIORNO IL RECORD
374300981005     C  N48              UPDATE    TNTMD000
374400981001     C*
374500981001    2C                   ENDIF
374600981001     C*
374700981001     C* SE LA VARIAZIONE ARB E' DD O DM DELETO IL RECORD E NON PROSEGUO CON
374800981001     C* LA LETTURA
374900981001    2C     ARBCVB        IFEQ      'DM'
375000981001     C     ARBCVB        OREQ      'DD'
375100981001     C   11              DELETE    FNARBD00
375200981001     C   16              DELETE    FNARBM00
375300981001     C                   MOVE      'Y'           DMDD
375400981001    2C                   ENDIF
375500981001     C*
375600981001    1C                   ENDIF
375700981001     C*
375800981001     C                   ENDSR
375801120312     C**-------------------------------------------------------------
375802120515     C** RITASSAZIONE DELLA SPEDIZIONE CON I NUOVI DATI RICEVUTI
375805120312     C**-------------------------------------------------------------
375806120313     C     ctr_vrb       BEGSR
375974151008     c* Richiamo pgm memorizzazione variazioni post-fatturazione
375975151008     c                   clear                   fnlvv1ds
375976151008     c                   clear                   parvar
375977151008     c                   clear                   partitas
375978151008     c                   clear                   parcsb
375979151008     c                   eval      iv1cvb=comcvb
375980151008     c                   eval      iv1pgm=nompgm
375981151008     c                   select
375982151008     c                   when      %subst(iv1cvb:1:1)='I'
375984151008     c     nrri          chain     fnarbd0r      dsarbd
375985151008     c                   if        %found(fnarbd0r)
375986151008     c                   eval      parvar =dsarbd
375987151008     c                   endif
375989151008     c                   when      %subst(iv1cvb:1:1)='K'
375990151008     c     nrri          chain     fnarbk0r      dsarbk
375991151008     c                   if        %found(fnarbk0r)
375992151008     c                   eval      parvar =dsarbk
375993151008     c                   endif
375994151008     c                   if        wanncas='S'
375995151008     c                   eval      iv1can='S'
375996151008     c                   endif
375997151008     c                   eval      parcsb  = dscsb
375998151008     c                   when      %subst(iv1cvb:1:1)='T'
375999151008     c     nrri          chain     fiarbt0r      dsarbt
376000151008     c                   if        %found(fiarbt0r)
376001151008     c                   eval      parvar =dsarbt
376002151008     c                   endif
376003151008     c                   when      %subst(iv1cvb:1:1)='C'
376004151008     c     nrri          chain     fnarbg0r      dsarbg
376005151008     c                   if        %found(fnarbg0r)
376006151008     c                   eval      parvar =dsarbg
376007151008     c                   endif
376008151008     c                   endsl
376009151008     c                   eval      kpjbu=fnlvv1ds
376010151009     c                   eval      partitas=dstitas
376011151008     c                   call      'FNLVV1R'
376012151008     c                   parm                    kpjba
376013151008     c                   parm                    parvar
376014151008     c                   parm                    partitas
376015151008     c                   parm                    parcsb
376016120312     c                   endsr
376988121108      ****************************************************
376989121108      ** Memorizzazione tracciato della bolla di sede prima
376990121108      ** della variazione
376991121108      ****************************************************
376992121205     c     memo_dstitas  Begsr
376993121116     c                   eval      dstitas=titasds
376995121108     c                   endsr
376996121109      ****************************************************
376997121109      ** Memorizzazione dati del contrassegno prima
376998121109      ** della variazione
376999121109      ****************************************************
377000121109     c     memo_csb      Begsr
377001121109     c                   clear                   dscsb
377002121109     C     KANN          CHAIN(N)  TNCSB000
377003121109     c                   if        %found(tncsb03l)
377004121115     c                   eval      dscsb=csbds
377005121109     c                   endif
377006121109     c                   endsr
377024981001     C*
377025970814**         CMD1
377026020827CHKOBJ OBJ(FIARBR3C) OBJTYPE(*FILE) MBR(M234567890)
377027920207**         CM2
377028020827ALCOBJ OBJ((FLXXXX0R *FILE *EXCL       M234567890))
377029920212**         CM3
377030020827OVRDBF FILE(FLXXXXXX)               MBR(M234567890)
377031920113**         CM5
377032020827ADDLFM FILE(FIARBR3C) MBR(M234567890)   DTAMBRS((*CURRENT/FNARBK0R
377033020827 (M234567890)) (*CURRENT/FNARBD0R (M234567890)) (*CURRENT/FIARBT0R
377034020827 (M234567890)) (*CURRENT/FNARBG0R (M234567890)) (*CURRENT/FNARBM0R
377035020827 (M234567890)))
377100920113**
3772009201301 BOLLA  INESISTENTE                                                1
377300920130TIPO BOLLA INESISTENTE                                              2
377400920130CAUSALE VARIAZIONE INESISTENTE                                      3
377500960514NON ESISTE RECORD BOLLA IN TNAC200F                                 4
377600920130NON ESISTE ANAGRAFICA CONTRASSEGNI                                  5
377700920130NUOVO CODICE BOLLA INESISTENTE                                      6
3778009201302 BOLLA  INESISTENTE                                                7
377900920220SPEDIZIONE GIA' TASSATA                                             8
378000920212SPEDIZIONE GIA' CONTABILIZZATA                                      9
378100920210C/ASSEGNO GIA' INCASSATO - PAGATO                                   10
378200950201MANCA RECORD FILE FNARBM0R                                          11
378300950201MANCA X CREAZ. ASSEGNATO FNARBD1R                                   12
378400930513X CREAR 2BOLLA MA NO VARIAZ CD.BOLL                                 15
378500920601GIA' CONTABIL.:NO TOLTA VARIA G                                     14
378600930513
378700920601GIA' CONTABIL.:NO CAMBIO TIPO BOLLA                                 16
378800930513
378900920601GIA' CONTABIL.:NO AGGIUNTA VARIA G                                  18
379000930513
379100920601GIA' CONTABIL.:NO VARIATO L'A3                                      20
379200930513
379300920601GIA' CONTABIL.:NO MODIFIC. VARIA G                                  22
379400990414GIA' TASSATA  :NO TOLTA    VARIA G                                  23
379500920228GIA' TASSATA  :NO AGGIUNTA VARIA G                                  24
379600920228GIA' TASSATA  :NO MODIFIC. VARIA G                                  25
379700950203CAP E NAZIONE MITTENTE INESISTENTE                                  26
379800950203COD.MITTENTE INESISTENTE IN ANAGRAF                                 27
379900920302SPEDIZIONE ANNULLATA                                                28
380000960919COD.MITTENTE BLOCCATO                                               29
380100970814ERRORE IN DETERMINAZIONE COD.TASSAZ                                 30
380200990414AGGIUNTO   IMPORTO DA ASSICURARE                                    31
380300990414TOLTO      IMPORTO DA ASSICURARE                                    32
380400990414MODIFICATO IMPORTO DA ASSICURARE                                    33
380500990414GIA' CONTABIL.:NO TOLTA    VARIA R                                  34
380600990414GIA' CONTABIL.:NO AGGIUNTA VARIA R                                  35
380700990414GIA' CONTABIL.:NO MODIFIC. VARIA R                                  36
380800991022
380900991022
381000991022
381100991022AGGIUNTO IM.ASS. TP.BOLLA NON ASSIC                                 40
381200991022TOLTO    IM.ASS. TP.BOLLA NON ASSIC                                 41
381300991022MODIFIC. IM.ASS. TP.BOLLA NON ASSIC                                 42
381400070119** cmdt
381500070119OVRDBF FILE(FNBLP01L) TOFILE(
