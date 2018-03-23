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
001300151008     Ffnarbg0r  iF   E             disk    rename(fnarbg00:fnarbg)
001400151008     f                                     usropn
001500151008     Ffnarbd0r  iF   E             disk    rename(fnarbd00:fnarbd)
001600151008     f                                     usropn
001700151008     Ffnarbk0r  iF   E             disk    rename(fnarbk00:fnarbk)
001800151008     f                                     usropn
001900151008     Ffiarbt0r  iF   E             disk    rename(fiarbt00:fiarbt)
002000151008     f                                     usropn
002100950201     FFNARBM1R  UF   E           K DISK    USROPN
002200950201     F                                     RENAME(FNARBM00:FNARBMRR)
002300950201     FFNARBD1R  UF   E           K DISK    USROPN
002400950201     F                                     RENAME(FNARBD00:FNARBDRR)
002500991014     FFiARBU1R  UF   E           K DISK    USROPN
002600991014     F                                     RENAME(FIARBU00:FIARBURR)
002700991014     FFIARBE2C  IF A E           K DISK
002800950201     F                                     RENAME(FNARBD00:FNARBDEE)
002900991014     F                                     RENAME(FIARBT00:FIARBTEE)
003000950201     F                                     RENAME(FNARBK00:FNARBKEE)
003100950201     F                                     RENAME(FNARBM00:FNARBMEE)
003200950201     F                                     RENAME(FNARBG00:FNARBGEE)
003300991014     FFIARBU1E  O  A E           K DISK
003400991014     F                                     RENAME(FIARBU00:FIARBUEE)
003500950201     F*
003600990624     FTITAS30C  UF A E           K DISK
003700990629     FTITAA30C  UF A E           K DISK
003800990624     FTITA730C  UF A E           K DISK
003900010126     FTITA430C  UF A E           K DISK
003901151120     Ffiar531c  uF a E           K DISK
004000000000     FTABEL00F  IF   E           K DISK
004100900426     FAZORG01L  IF   E           K DISK
004200941121     FFNLBL01L  IF   E           K DISK
004300941121     FTNCSB03L  UF A E           K DISK
004400941121     FTNCSA01L  UF A E           K DISK
004500941121     FTNCSV00F  O  A E             DISK
004600981118     Fndppa01L  IF   E           K DISK
004700990518     FNdcpa01L  uF   E           K DISK
004800941130     FTNBLA01L  IF   E           K DISK
004900121107
005000020827     D CMD1            S             51    DIM(1) CTDATA PERRCD(1)
005100020827     D CM2             S              1    DIM(51) CTDATA PERRCD(51)
005200020827     D CM3             S              1    DIM(51) CTDATA PERRCD(51)
005300020827     D CM5             S              1    DIM(213) CTDATA PERRCD(66)
005400020827     D C6              S              1    DIM(213)
005500920113     D*
005600991019     D ERR             S             35    DIM(42) CTDATA PERRCD(1)             COSTANTI ERRORA
005700950202     D K35             S              1    DIM(35)                              COMODO        A
005800070119     d cmdt            s             60    dim(01) ctdata perrcd(1)
005900011218
006000011218     d sksv            s              1    dim(20)                              sk sigla varie
006100011218     d skva            s             11  3 dim(20)                              sk imp.varie
006200011218
006300151008     d infdsarbr       ds
006400151008     d   recname             261    270
006500151008     D   nrri                397    400i 0
006600970916     D WLBDAT          DS                  INZ
006700970916     D  G02DAT                 1      8  0
006800970916     D  G02INV                 9     16  0
006900970916     D  G02ERR                17     17
007000970916     D  G02TGI                18     22  0
007100970916     D WGIDAT          DS                  INZ
007200970916     D  GIODAT                 1      8  0
007300970916     D  GIOINV                 9     16  0
007400970916     D  GIOTGI                17     21  0
007500941130     D CSBDS         E DS                  EXTNAME(TNCSB00F)
007600941130     D               E DS                  EXTNAME(TNCSV00F)
007700941130     D  CSVDS                 16    229
007800920113     D KPJBA         E DS
007900920113     D  LIBSYS                92    101
008000981118     D  wsoc                  98    100
008100920113     D  LEGFIL               483    492
008200920113     D  MBRFIL               493    502
008300920113     D  ALFFIL               494    496
008400151008     d fnlvv1ds      e ds
008500151008      * ATTENZIONE
008600151008     d* Se dovesse cambiare la lunghezza record di una delle seguenti DS
008700151008     d* riverificare la definizione del campo parvar
008800151008     d dsarbg          ds                  qualified
008900151008     d   fnarbg                            likerec(fnarbg) overlay(dsarbg)
009000151008     d dsarbk          ds                  qualified
009100151008     d   fnarbk                            likerec(fnarbk) overlay(dsarbk)
009200151008     d dsarbd          ds                  qualified
009300151008     d   fnarbd                            likerec(fnarbd) overlay(dsarbd)
009400151008     d dsarbt          ds                  qualified
009500151008     d   fiarbt                            likerec(fiarbt) overlay(dsarbt)
009600151008      * Definizione del seguente campo in base alla dsarbd che è la più lunga
009700151008      * di tutte.
009800151008     dparvar           s            224
009900151008     dpartitas         s                   like(titasds)
010000151008     dparcsb           s                   like(dscsb)
010100970714     D* DS PER TISI95R - CONTROLLO CAP
010200970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
010300021014     D ddatiute      e ds
010400021014     D azuteds       e ds                  extname(AZUTE00F)
010500020604     D tibs34ds      E DS
010600021112     D tibs36ds      E DS
010700070201     D tibs55ds      E DS
010800930514     D DSTB          E DS
010900930514     D DS3A          E DS
011000920113     D DS7L          E DS
011100150108     D DS7R          E DS
011200950202     D DS1A          E DS
011300010126     D og143         E DS
011400010126     D dsbl4j        E DS
011500060317     D dta4a         E DS
011600971215     D DSLV55        E DS                  EXTNAME(FNLV55DS)
011700981001     D DSBS69        E DS                  EXTNAME(TIBS69DS)
011800981001     D DSACO         E DS                  EXTNAME(CNACO00F)
011900981001     D DSIND         E DS                  EXTNAME(CNIND00F)
012000981001     D DSCLP         E DS                  EXTNAME(CNCLP00F)
012100981001     D DSCLS         E DS                  EXTNAME(FNCLS00F)
012200121115     D dstitas       E DS                  EXTNAME(titas00f) qualified
012300121116     D titasds       E DS                  EXTNAME(titas00f)
012400121115     D dscsb         E DS                  EXTNAME(tncsb00f) qualified
012500080408     D SAVDS3a       E DS                  extname(ds3a) prefix(S_)
012600120510     d*dvpoDECO      e ds
012700121114     D Tibs02DS      E DS
012701151119     D dar5fat       E DS
012800071102     d
012900990505     DWDELARBU         s              1
013000990505     dwctb             s                   like(ppactb) inz('CG')
013100981118     dwkcc             s                   like(ppakcc) inz('000151')
013200981118     dwksc             s                   like(ppaksc) inz('00000000')
013300981118     dwdft             s                   like(ppadtpar)
013400981118     dwnft             s                   like(ppanrpar)
013500981118     dwfiv             s                   like(ppaserpar) inz
013600981118     dsavfiv           s                   like(tasfiv)
013700981118     dsavnft           s                   like(tasnft)
013800981118     dsavdft           s                   like(tasdft)
013900981118     dsavksc           s                   like(tasksc)
014000151012     dtasdsp           s                   like(tamddt)
014100990505     dWCAS             s                   like(arbcas)
014200990625     d§trc             s                   like(ta4trc)
014300991102     dwcsbvca          s                   like(csbvca)
014301151119     dkar5trd          s                   like(ar5trd)
014400011218
014500011218     d flgriorg        s              1
014600011218     d xx              s              2  0                                      indice sk varie
014700070119     d commt           s             80
014800070119     d lenght          s             15  5 inz(80)
014900120424     D k               s              5  0                                      indice
015000121205     D wanncas         s              1
015100151008     D                SDS
015200151008     D  NOMPGM                 1     10
015300941130     I*
015400950201     IFNARBD00      11
015500950201     IFNARBK00      12
015600991014     IFIARBT00      13
015700950201     IFNARBG00      15
015800950201     IFNARBM00      16
015900941121     I*
016000990624     ITITAS000      02
016100990624     ITITAS010      03
016200990624     ITITASP00      04
016300010126     I*
016400010126     ITITA4000      05
016500010126     ITITA4P00      06
016600071029     I*
016700071029     ITITAA000      07
016800071029     ITITAAP00      08
016900920113     I/SPACE 3
017000920113     C****************************************************************
017100920113     C*  RIEPILOGO INDICATORI
017200920113     C***************************************************************
017300950201     C* 01    - LETTURA SUL FILE DEGLI ERRORI DI FNARBE
017400010126     C* 02/06 - INDICATORI DI TIPO RECORD
017500941130     C* 17    - STAMPO ERRORE VARIAZIONE CONS PARTICOLARE
017600920228     C* 20    - NON ALLOCATI DEGLI AGGETTI ESCO DA PROGRAMMA
017700930514     C* 23    - OFF SPEDIZIONE STORNATA
017800930513     C* 24    - NON AGGIORNO TUTTI I CAMPI SE 2BOLL GIA' CONTABILIZZATA
017900930513     C* 25    - NON ESISTE ANAGRAFICA C/A
018000981118     C* 26    - NON ESISTE RECORD IN partita
018100930513     C* 27    - NON AGGIORNO TUTTI I CAMPI SE 1BOLL GIA' CONTABILIZZATA
018200930512     C* 29    - ON  33 ON  - NON ESISTE NEMMENO        SU BLTAS10
018300930702     C*       - ON  33 OFF - SPEDIZIONE CONTABILIZZATA SU BLTAS
018400950201     C* 30    - LETTURA FNARBR3C
018500920212     C* 31    - FILE CONTRASSEGNI NON TROVATO O GIA' INCASATO
018600920212     C* 32    - COMODO
018700920228     C* 33    - ON SPEDIZIONE ESISTENTE SU BLTAS10
018800920212     C* 34    - COMODO
018900950201     C* 35    - MANCA RECORD SU FNARBM0R
019000920214     C* 36    - SCRIVO FLRBM0E QUANDO SERVE
019100920213     C* 37    - RECORD DA NON CANCELLARE DOPO WRITE ERRORE
019200920226     C* 38    - SPEDIZIONE GIA' TASSATA
019300920227     C* 39    - DI COMODO
019400920303     C* 40    - SPEDIZIONE ANNULLATA
019500941130     C* 41    - ESISTENTE TNCSB000
019600930303     C* 42    - ESISTENTE LEGAME BOLLA (C/ASSEGNO SULLA MAMMA)
019700981001     C* 48    - NON ESISTE RECORD SU TNTMD
019800920113     C*****************************************************************
019900920113     C*
020000920113     C* ALLOCO MEMBRI
020100920113     C                   EXSR      CAEX
020200920113     C   20              GOTO      FINE
020300920113     C*
020400991014     C                   READ      FIARBR3C                               30
020500920113     C*
020600930512    1C     *IN30         DOWEQ     '0'
020700990414     C                   SETOFF                                       242736
020800930622     C*
020900981001     C                   MOVEL     ' '           DMDD              1
021000930514     C                   MOVEL     ' '           ST1
021100981001     C* SOLO NEL CASO IN CUI SI TRATTI DI LETTURA DI VARIAZIONI MITTENTE E
021200981001     C* DESTINATARIO ( *IN11 *IN16 ON ) CONTROLLO SE SI TRATTA DI VARIAZIONI
021300981001     C* DM / DD PERCHE' IN QUESTO CASO DEVO SCRIVERE IL FILE DATI ANAGRAFICI
021400981001     C* SPEDIZIONI CHE VIENE UTILIZZATO PER STAMPE RELATIVE AI DANNI
021500981001     C* INOLTRE SEMPRE PER 11 E 16 ACCESO PER LE VARIAZIONI I0 / MI (VARIAZIONI
021600981001     C* DESTINATARIO E MITTENTE) DEVO AGGIORNARE QUESTO FILE INDIPENDENTEMENTE
021700981001     C* DAL FATTO CHE SIA RELATIVO AD UNA BOLLA GIA' CONTABILIZZATA
021800981001     C     *IN11         IFEQ      '1'
021900981001     C     *IN16         OREQ      '1'
022000981001     C                   EXSR      AGTMD
022100981001     C* SE HO LETTO UNA VARIAZIONE DM O DD LEGGO IL RECORD SUCCESSIVO
022200981001     C* ALTRIMENTI PROSEGUO
022300981001     C     DMDD          IFNE      ' '
022400981001     C                   GOTO      NEXT
022500981001     C                   ENDIF
022600981001     C*
022700981001     C                   ENDIF
022800930513     C*
022900930513     C* PRENDO  C O D I C E   B O L L A
023000920113     C                   MOVEL     '3A'          COD
023100920113     C                   MOVEL     *BLANKS       KEY
023200991014     C  N12              MOVEL     ARBCBO        KEY
023300950203     C   12              MOVEL     ARBCBP        KEY
023400920113     C     KTAB          CHAIN     TABEL                              33
023500920114     C*
023600920212     C* SE NON ESISTE TIPO BOLLA: ERRORE E NON CANCELLO
023700930513    2C     *IN33         IFEQ      '1'
023800991014     C                   MOVEL     ERR(2)        ARBDER
023900930513     C*
024000000703     C                   Z-ADD     102           COMFEV
024100930513     C                   SETON                                        37
024200920212     C*
024300930513     C                   EXSR      WTRERR
024400930513     C*
024500930513   X2C                   ELSE
024600930513     C*
024700930513     C                   MOVEL     TBLUNI        DS3A
024800950203     C* IMPOSTO IL TIPO BOLLA PER EVENTUALE ERRORE
024900950203     C                   MOVEL     §3ATB1        TBLERR            1
025000990519     C                   MOVEL     §3ARBL        TBLRBL            1
025100930513     C*
025200930513     C* PRENDO  C A U S A L E   V A R I A Z I O N E
025300991014     C                   MOVEL     ARBCVB        COMCVB            2
025400930513     C*
025500930513     C                   MOVEL     '7L'          COD
025600930513     C                   MOVEL     *BLANKS       KEY
025700930513     C                   MOVEL     COMCVB        KEY
025800930513     C     KTAB          CHAIN     TABEL                              33
025900930513     C*
026000930513     C* 33 ON - NON ESISTE CAUSALE VARIAZIONE: STAMPA E NON ANNULLO
026100930513    3C     *IN33         IFEQ      '1'
026200991014     C                   MOVEL     ERR(3)        ARBDER
026300930513     C*
026400930513     C                   Z-ADD     102           COMFEV
026500930513     C                   SETON                                        37
026600930513     C*
026700930513     C                   EXSR      WTRERR
026800930513    3C                   END
026900930513     C*
027000930513     C  N33              MOVEL     TBLUNI        DS7L
027100930513    2C                   END
027200920130     C*
027300930513    2C     *IN33         IFEQ      '0'
027400920226     C                   MOVEL     COMCVB        FLGCVB            1
027500920226     C*
027600990701     C                   CLEAR                   KSC42BO
027700990701     c*
027800920226     C* CONTROLLO ESISTENZA 2   BOLLA
027900950202    3C     FLGCVB        IFNE      'C'
028000950202     C     §3ATB2        ANDNE     '  '
028100970514     C* NON CONTROLLO LA 2 BOLLA SE TASSAZIONE IN PARTENZA PERCHE' NON
028200970514     C*  LA VA MAI AD AGGIORNARE
028300970514     C     COMCVB        ANDNE     'TP'
028400920226     C                   MOVEL     §3ATB2        COMTBL
028500920226     C                   EXSR      CTRSPE
028600941130     C*
028700941130     C* SE NON TROVATI ERRORI --> SALVO CAMPI CHE MI SERVONO
028800941130    4C     *IN29         IFEQ      *OFF
028900941130     C                   MOVE      TASKSC        SAVKS4            4 0
029000981118     C                   MOVE      TASKSC        SAVKsc
029100941130     C                   MOVEL     TASFIV        SAVFIV
029200941130     C                   MOVE      TASDFT        SAVDFT
029300941130     C                   MOVEL     TASNFT        SAVNFT
029400960625     C                   MOVEL     TASTBL        SAVTBL
029500941130     C*
029600941130     C* 24 ON - 2 BOLLA GIA' CONTABILIZZATA
029700941130    5C     *IN02         IFEQ      *ON
029800941130     C     TASFIC        ANDNE     ' '
029900941130     C     *IN02         OREQ      *OFF
030000941130     C                   SETON                                        24
030100941130    5C                   ENDIF
030200941130    4C                   ENDIF
030300930513    3C                   END
030400920226     C*
030500920226     C* SE TROVATA 2 BOLLA CONTROLLO LA 1
030600930513    3C  N29FLGCVB        IFNE      '2'
030700920226     C                   MOVEL     §3ATB1        COMTBL
030800920226     C                   EXSR      CTRSPE
030900941130     C*
031000941130    4C     *IN29         IFEQ      *OFF
031100981118     C* 27 ON - 1 BOLLA GIA' CONTABILIZZATA --> AGGIORNO ANCHE yncpa
031200941130    5C     *IN02         IFEQ      *ON
031300941130     C     TASFIC        ANDNE     ' '
031400941130     C     *IN02         OREQ      *OFF
031500941130     C                   SETON                                        27
031600941130    5C                   END
031700941130    4C                   END
031800930513    3C                   END
031900990414     C*
032000941130     C*
032100941130     C* 29 OFF - AGGIORNO BOLLA SE NON SONO STORNATE ENTRAMBI LE BOLLE
032200930514     C  N29ST1           IFEQ      ' '
032300991021     c     flgcvb        oreq      'C'
032400930514     C                   EXSR      AGGBOL
032500930514     C                   END
032600941130     C*
032700930514    2C                   END
032800920113     C*
032900950203    2C  N37              DO
033000920212     C   11              ADD       1             NUMRBD            7 0
033100920113     C   12              ADD       1             NUMRBK            7 0
033200920113     C   13              ADD       1             NUMRBT            7 0
033300950201     C   15              ADD       1             NUMRBG            7 0
033400950201     C   16              ADD       1             NUMRBM            7 0
033500920219     C* CANCELLO RECORD
033600950201     C   11              DELETE    FNARBD00
033700950201     C   12              DELETE    FNARBK00
033800950202     C   12KARBM         DELETE    FNARBMRR                           32
033900991014     C   13              DELETE    FIARBT00
034000100303     c   13              if        st1='1'
034100100303     C     KARBU         SETLL     FIARBU1R
034200100303     C     KARBU         READE     FIARBU1R                               30
034300100303     C*
034400100303     C     *IN30         DOWEQ     *OFF
034500100303     C                   DELETE    FIARBURR
034600100303     C*
034700100303     C     KARBU         READE     FIARBU1R                               30
034800100303     C                   ENDDO
034900100303     c
035000100303     c                   endif
035100100303     c
035200950201     C   15              DELETE    FNARBG00
035300950201     C   16              DELETE    FNARBM00
035400930513    2C                   END
035500920113     C*
035600981001     C     NEXT          TAG
035700981001     C*
035800920113     C                   SETOFF                                       111213
035900991014     C                   SETOFF                                         1516
036000950201     C                   SETOFF                                       353738
036100941130     C                   SETOFF                                       2933
036200991014     C                   READ      FIARBR3C                               30
036300930512    1C                   END
036400970714     C*
036500970714     C                   CLEAR                   DSSI95
036600970714     C                   MOVEL     'C'           I95TLA
036700970714     C                   CALL      'TISI95R'
036800970714     C                   PARM                    DSSI95
036900971215     C*
037000971215     C                   MOVEL     'C'           D55TLA
037100971215     C                   CALL      'FNLV55R'
037200971215     C                   PARM                    DSLV55
037300981001     C*
037400981001     C                   CLEAR                   DSBS69
037500981001     C                   MOVEL     'C'           I69TLA
037600981001     C                   CALL      'TIBS69R'
037700981001     C                   PARM                    DSBS69
037800981001     C                   PARM                    DSACO
037900981001     C                   PARM                    DSIND
038000981001     C                   PARM                    DSCLP
038100981001     C                   PARM                    DSCLS
038200020604     c*
038300021112     C                   CLEAR                   tibs36ds
038400021112     C                   MOVEL     'C'           I36TLA
038500021112     C                   CALL      'TIBS36R'
038600021112     C                   PARM                    TIBS36DS
038700120424
038800121114
038900121114     C                   CLEAR                   tibs02ds
039000121114     C                   MOVEL     'C'           t02tla
039100121114     C                   CALL      'TIBS02R'
039200121114     C                   PARM                    kpjba
039300121114     C                   PARM                    tibs02ds
039400151009
039500151009     C                   MOVEL     'C'           iv1tla
039600151009     c                   movel     fnlvv1ds      kpjbu
039700151009     C                   CALL      'FNLVV1R'
039800151009     C                   PARM                    kpjba
039900920113     C*
040000920113     C     FINE          TAG
040100920113     C*
040200920113     C                   SETON                                        LR
040300920113     C**---------------------------------------------------------
040400920113     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
040500920113     C**---------------------------------------------------------
040600920113     C     CAEX          BEGSR
040700020827     C                   MOVE      ')'           VAR11            11
040800020827     C                   MOVEL     MBRfil        VAR11
040900020827     C                   Z-ADD     51            LUNG             15 5
041000020827     C                   Z-ADD     213           LUNG2            15 5
041100920113     C* CHK E ADD SU TUTTI I FILE FISICI
041200950202     C                   MOVEL     'FNARBD0R'    FISICO           10
041300930212     C                   CALL      'TRUL50C'
041400930212     C                   PARM                    FISICO
041500930212     C                   PARM                    MBRFIL
041600930212     C                   PARM                    LIBSYS
041700930212     C                   PARM                    LOGICO           10
041800930212     C                   PARM                    MBRLOG           10
041900930212     C                   PARM                    ULFLG             1
042000950202     C                   MOVEL     'FNARBD1R'    LOGICO
042100950202     C                   MOVEL     MBRFIL        MBRLOG
042200950202     C                   CALL      'TRUL50C'
042300950202     C                   PARM                    FISICO
042400950202     C                   PARM                    MBRFIL
042500950202     C                   PARM                    LIBSYS
042600950202     C                   PARM                    LOGICO           10
042700950202     C                   PARM                    MBRLOG           10
042800950202     C                   PARM                    ULFLG             1
042900950202     C*
043000950202     C                   CLEAR                   LOGICO
043100950202     C                   CLEAR                   MBRLOG
043200991015     C                   MOVEL     'FIARBT0R'    FISICO
043300930212     C                   CALL      'TRUL50C'
043400930212     C                   PARM                    FISICO
043500930212     C                   PARM                    MBRFIL
043600930212     C                   PARM                    LIBSYS
043700930212     C                   PARM                    LOGICO           10
043800930212     C                   PARM                    MBRLOG           10
043900930212     C                   PARM                    ULFLG             1
044000950201     C                   MOVEL     'FNARBK0R'    FISICO
044100930212     C                   CALL      'TRUL50C'
044200930212     C                   PARM                    FISICO
044300930212     C                   PARM                    MBRFIL
044400930212     C                   PARM                    LIBSYS
044500930212     C                   PARM                    LOGICO           10
044600930212     C                   PARM                    MBRLOG           10
044700930212     C                   PARM                    ULFLG             1
044800950202     C*
044900950201     C                   MOVEL     'FNARBM0R'    FISICO
045000930212     C                   CALL      'TRUL50C'
045100930212     C                   PARM                    FISICO
045200930212     C                   PARM                    MBRFIL
045300930212     C                   PARM                    LIBSYS
045400930212     C                   PARM                    LOGICO           10
045500930212     C                   PARM                    MBRLOG           10
045600930212     C                   PARM                    ULFLG             1
045700950202     C                   MOVEL     'FNARBM1R'    LOGICO
045800950202     C                   MOVEL     MBRFIL        MBRLOG
045900950202     C                   CALL      'TRUL50C'
046000950202     C                   PARM                    FISICO
046100950202     C                   PARM                    MBRFIL
046200950202     C                   PARM                    LIBSYS
046300950202     C                   PARM                    LOGICO           10
046400950202     C                   PARM                    MBRLOG           10
046500950202     C                   PARM                    ULFLG             1
046600950202     C*
046700950202     C                   CLEAR                   LOGICO
046800950202     C                   CLEAR                   MBRLOG
046900991015     C                   MOVEL     'FIARBU0R'    FISICO
047000950202     C                   CALL      'TRUL50C'
047100950202     C                   PARM                    FISICO
047200950202     C                   PARM                    MBRFIL
047300950202     C                   PARM                    LIBSYS
047400950202     C                   PARM                    LOGICO           10
047500950202     C                   PARM                    MBRLOG           10
047600950202     C                   PARM                    ULFLG             1
047700991015     C                   MOVEL     'FIARBU1R'    LOGICO
047800950202     C                   MOVEL     MBRFIL        MBRLOG
047900950202     C                   CALL      'TRUL50C'
048000950202     C                   PARM                    FISICO
048100950202     C                   PARM                    MBRFIL
048200950202     C                   PARM                    LIBSYS
048300950202     C                   PARM                    LOGICO           10
048400950202     C                   PARM                    MBRLOG           10
048500950202     C                   PARM                    ULFLG             1
048600950202     C*
048700950202     C                   CLEAR                   LOGICO
048800950202     C                   CLEAR                   MBRLOG
048900950202     C                   MOVEL     'FNARBG0R'    FISICO
049000950202     C                   CALL      'TRUL50C'
049100950202     C                   PARM                    FISICO
049200950202     C                   PARM                    MBRFIL
049300950202     C                   PARM                    LIBSYS
049400950202     C                   PARM                    LOGICO           10
049500950202     C                   PARM                    MBRLOG           10
049600950202     C                   PARM                    ULFLG             1
049700920113     C*
049800920113     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE FACCIO ADDLFM
049900920113     C                   SETOFF                                       20
050000920212     C                   MOVE      VAR11         CMD1
050100920113     C                   MOVEL     *BLANKS       COMMAN
050200920113     C                   MOVEA     CMD1(1)       COMMAN           80
050300920113     C                   CALL      'QCMDEXC'                            20
050400920113     C                   PARM                    COMMAN
050500920113     C                   PARM                    LUNG
050600920113     C*
050700920113     C   20              DO
050800950202     C                   MOVEA     CM5           C6
050900920113     C*
051000020827     C                   MOVEA     MBRFIL        C6(27)
051100020827     C                   MOVEA     MBRFIL        C6(69)
051200020827     C                   MOVEA     MBRFIL        C6(102)
051300020827     C                   MOVEA     MBRFIL        C6(135)
051400020827     C                   MOVEA     MBRFIL        C6(168)
051500020827     C                   MOVEA     MBRFIL        C6(201)
051600920113     C                   SETOFF                                       20
051700920113     C                   MOVEL     *BLANKS       COMMA2
051800020827     C                   MOVEA     C6            COMMA2          213
051900920113     C                   CALL      'QCMDEXC'                            H1
052000920113     C                   PARM                    COMMA2
052100920113     C                   PARM                    LUNG2
052200920113     C   20              GOTO      ENDCAE
052300920113     C                   END
052400020827
052500020827     C                   MOVE      '))'          VAR12            12
052600020827     C                   MOVEL     MBRfil        VAR12
052700920113     C*
052800920114     C* ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
052900041006     C**                 SETOFF                                       20
053000041006     C**                 MOVEa     VAR12         CM2(40)
053100041006     C**                 MOVEA     'FIARBR3C'    CM2(13)
053200041006     C**                 MOVEL     *BLANKS       COMMAN
053300041006     C**                 MOVEA     CM2           COMMAN
053400041006     C**                 CALL      'QCMDEXC'                            20
053500041006     C**                 PARM                    COMMAN
053600041006     C**                 PARM                    LUNG
053700920113     C*
053800041006     C** 20              GOTO      ENDCAE
053900041006     C**
054000930212     C* ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
054100041006     C**                 SETOFF                                       20
054200041006     C**                 MOVEa     VAR12         CM2(40)
054300041006     C**                 MOVEA     'FNARBM1R'    CM2(13)
054400041006     C**                 MOVEL     *BLANKS       COMMAN
054500041006     C**                 MOVEA     CM2           COMMAN
054600041006     C**                 CALL      'QCMDEXC'                            20
054700041006     C**                 PARM                    COMMAN
054800041006     C**                 PARM                    LUNG
054900041006     C**
055000041006     C** 20              GOTO      ENDCAE
055100041006     C**
055200041006     C**                 SETOFF                                       20
055300041006     C**                 MOVEa     VAR12         CM2(40)
055400041006     C**                 MOVEA     'FNARBD1R'    CM2(13)
055500041006     C**                 MOVEL     *BLANKS       COMMAN
055600041006     C**                 MOVEA     CM2           COMMAN
055700041006     C**                 CALL      'QCMDEXC'                            20
055800041006     C**                 PARM                    COMMAN
055900041006     C**                 PARM                    LUNG
056000041006     C**
056100041006     C** 20              GOTO      ENDCAE
056200041006     C**
056300041006     C**ALLOCO MEMBRI: SE NON RIESCO ESCO DA PROGRAMMA
056400041006     C**                 SETOFF                                       20
056500041006     C**                 MOVEa     VAR12         CM2(40)
056600041006     C**                 MOVEA     'FIARBU1R'    CM2(13)
056700041006     C**                 MOVEL     *BLANKS       COMMAN
056800041006     C**                 MOVEA     CM2           COMMAN
056900041006     C**                 CALL      'QCMDEXC'                            20
057000041006     C**                 PARM                    COMMAN
057100041006     C**                 PARM                    LUNG
057200041006     C**
057300041006     C** 20              GOTO      ENDCAE
057400041006     C**OVRDBF
057500041006     C                   SETOFF                                       20
057600020828     C                   MOVEa     VAR11         CM3(41)
057700950202     C                   MOVEA     'FNARBM1R'    CM3(13)
057800920113     C                   MOVEL     *BLANKS       COMMAN
057900920212     C                   MOVEA     CM3(1)        COMMAN
058000920113     C                   CALL      'QCMDEXC'                            20
058100920113     C                   PARM                    COMMAN
058200920113     C                   PARM                    LUNG
058300920212     C*
058400920212     C   20              GOTO      ENDCAE
058500950202     C                   SETOFF                                       20
058600020828     C                   MOVEa     VAR11         CM3(41)
058700991015     C                   MOVEA     'FIARBU1R'    CM3(13)
058800950202     C                   MOVEL     *BLANKS       COMMAN
058900950202     C                   MOVEA     CM3(1)        COMMAN
059000950202     C                   CALL      'QCMDEXC'                            20
059100950202     C                   PARM                    COMMAN
059200950202     C                   PARM                    LUNG
059300950202     C*
059400950202     C   20              GOTO      ENDCAE
059500920214     C* OVRDBF
059600920214     C                   SETOFF                                       20
059700020828     C                   MOVEa     VAR11         CM3(41)
059800950201     C                   MOVEA     'FNARBD1R'    CM3(13)
059900920214     C                   MOVEL     *BLANKS       COMMAN
060000920214     C                   MOVEA     CM3(1)        COMMAN
060100920214     C                   CALL      'QCMDEXC'                            20
060200920214     C                   PARM                    COMMAN
060300920214     C                   PARM                    LUNG
060400920214     C*
060500920214     C   20              GOTO      ENDCAE
060600920212     C*
060700991015     C                   MOVEA     'FIARBR3C'    CM3(13)
060800920212     C                   MOVEL     *BLANKS       COMMAN
060900920212     C                   MOVEA     CM3(1)        COMMAN
061000920212     C                   CALL      'QCMDEXC'                            20
061100920212     C                   PARM                    COMMAN
061200920212     C                   PARM                    LUNG
061300151008     C   20              GOTO      ENDCAE
061400151008      *
061500151008     C                   MOVEA     'FNARBD0R'    CM3(13)
061600151008     C                   MOVEL     *BLANKS       COMMAN
061700151008     C                   MOVEA     CM3(1)        COMMAN
061800151008     C                   CALL      'QCMDEXC'                            20
061900151008     C                   PARM                    COMMAN
062000151008     C                   PARM                    LUNG
062100151008     C   20              GOTO      ENDCAE
062200151008     C                   MOVEA     'FNARBK0R'    CM3(13)
062300151008     C                   MOVEL     *BLANKS       COMMAN
062400151008     C                   MOVEA     CM3(1)        COMMAN
062500151008     C                   CALL      'QCMDEXC'                            20
062600151008     C                   PARM                    COMMAN
062700151008     C                   PARM                    LUNG
062800151008     C   20              GOTO      ENDCAE
062900151008     C                   MOVEA     'FNARBG0R'    CM3(13)
063000151008     C                   MOVEL     *BLANKS       COMMAN
063100151008     C                   MOVEA     CM3(1)        COMMAN
063200151008     C                   CALL      'QCMDEXC'                            20
063300151008     C                   PARM                    COMMAN
063400151008     C                   PARM                    LUNG
063500151008     C   20              GOTO      ENDCAE
063600151008     C                   MOVEA     'FIARBT0R'    CM3(13)
063700151008     C                   MOVEL     *BLANKS       COMMAN
063800151008     C                   MOVEA     CM3(1)        COMMAN
063900151008     C                   CALL      'QCMDEXC'                            20
064000151008     C                   PARM                    COMMAN
064100151008     C                   PARM                    LUNG
064200151008     C   20              GOTO      ENDCAE
064300920113     C*
064400991015     C  N20              OPEN      FIARBR3C
064500950202     C  N20              OPEN      FNARBM1R
064600950201     C  N20              OPEN      FNARBD1R
064700991015     C  N20              OPEN      FIARBU1R
064800151008     C  N20              OPEN      FNARBD0R
064900151008     C  N20              OPEN      FNARBK0R
065000151008     C  N20              OPEN      FNARBG0R
065100151008     C  N20              OPEN      FIARBT0R
065200920113     C*
065300920113     C     ENDCAE        ENDSR
065400920113     C**---------------------------------------------------------
065500920113     C** AGGIORNO BOLLA PARTENZE
065600920113     C**---------------------------------------------------------
065700920113     C     AGGBOL        BEGSR
065800920130     C*
065900941130     C                   SELECT
066000061110     C     FLGCVB        WHENEQ    'F'
066100061110     C                   EXSR      AGARBF
066200061110     C*
066300941130     C     FLGCVB        WHENEQ    'I'
066400920113     C                   EXSR      AGARBI
066500941130     C*
066600941130     C     FLGCVB        WHENEQ    'K'
066700920127     C                   EXSR      AGARBK
066800920228     C*
066900941130     C     FLGCVB        WHENEQ    'T'
067000920113     C                   EXSR      AGARBT
067100920113     C*
067200941130     C     FLGCVB        WHENEQ    '2'
067300920113     C                   EXSR      AGAR2T
067400950202     C*
067500950202     C     FLGCVB        WHENEQ    'M'
067600950202     C                   EXSR      AGARBM
067700950202     C*
067800950202     C     FLGCVB        WHENEQ    'C'
067900950202     C                   EXSR      AGARBC
068000060317     C*
068100060317     C     FLGCVB        WHENEQ    'R'
068200060317     C                   EXSR      AGARBR
068300941130     C                   ENDSL
068400920113     C*
068500930513     C                   ENDSR
068600061110     C**---------------------------------------------------------
068700061110     C** AGGIORNO CODICE VARIAZIONI FX - codice fiscale
068800061110     C**---------------------------------------------------------
068900061110     C     AGARBF        BEGSR
069000061110     c
069100061110     C                   SETOFF                                       3126
069200061110     C                   SETOFF                                       2225
069300061110     C                   MOVE      TASKSC        KSC4              4 0
069400071029     c* Vedo quale bolla aggiornare
069500071029     c                   clear                   bolla             1
069600071029     c* Destinatario
069700071029    1c                   if        arbrd2='D'
069800071029     C* chain su YNCPA01L per aggiornare codice fiscale franco o assegnato
069900071029    2C     *IN27         IFEQ      '1'
070000071029     C     KSC4          ANDEQ     9999
070100071029     C     TASNFT        ANDGT     0
070200071029     c                   eval      bolla ='1'
070300071029    2c                   endif
070400071029     c
070500071029    2C     *IN24         ifeq      '1'
070600071029     C     SAVKS4        ANDEQ     9999
070700071029     C     SAVNFT        ANDGT     0
070800071029     c                   eval      bolla ='2'
070900071029    2c                   endif
071000071029     c
071100071029   x1c                   else
071200071029     c* Mittente
071300071029    2C     *IN27         ifeq      '1'
071400071029     C     KSC4          ANDEQ     8888
071500071029     C     TASNFT        ANDGT     0
071600071029     c                   eval      bolla ='1'
071700071029    2c                   endif
071800071029    1c                   endif
071900071029     c*
072000061110     C*
072100061110     C* chain su YNCPA01L per aggiornare codice fiscale franco o assegnato
072200071029    1c                   if        bolla<>' '
072300061110     C*
072400071029     C* SE SI TRATTA DI storno ignoro
072500071029    2c                   if        bolla='2'
072600061110     C                   MOVEL(P)  SAVTBL        KEY
072700061110     C                   ELSE
072800061110     C                   MOVEL(P)  TASTBL        KEY
072900061113    2C                   ENDIF
073000061110     C*
073100061110     C                   MOVEL     'TB'          COD
073200061110     C     KTAB          CHAIN     TABEL                              32
073300061110     C  N32              MOVEL     TBLUNI        DSTB
073400061110     C   32              MOVEL     *ALL'1'       DSTB
073500071029     c
073600061110     C*  FCB="1" ASSEGNATO BUONO
073700061113    2C     §TBFCB        IFEQ      '1'
073800061110     C*
073900061110     c                   clear                   wdft
074000061113    3c                   select
074100071029     c***                when      *in27
074200071029     c                   when      bolla='1'
074300061110     c                   move      tasnft        wnft
074400061110     c                   if        tasdft > 0
074500061110     c                   move      tasdft        wdft
074600061110     c                   end
074700061110     c                   move      tasksc        wksc
074800071029     c***                when      *in24
074900071029     c                   when      bolla='2'
075000061110     c                   move      savnft        wnft
075100061110     c                   if        savdft > 0
075200061110     c                   move      savdft        wdft
075300061110     c                   end
075400061110     c                   move      savksc        wksc
075500061113    3c                   endsl
075600061110     c
075700061110     C     Kppa          CHAIN     ndppa01l                           26
075800061113    3C     *IN26         IFEQ      *OFF
075900061110     c     kycpa         chain     ndcpa01l                           26
076000061113    4C     *IN26         IFEQ      *OFF
076100071102     c
076200061110     C                   MOVEL     ARBCPI        cpacdfisc
076300071102     c
076400071102     c* Solo se reperita o partita la procedura aggiorno
076500120510     c*****              if        arbrsd<>*blanks or arbdtv>=§vpopiv
076600071029     C                   MOVEL     ARBRSD        cpapartiva
076700120510     c*****              endif
076800071102     c
076900061110     C                   UPDATE    ndcpa000
077000061113    4C                   ENDIF
077100061113    3C                   ENDIF
077200061110     C*
077300061113    2C                   END
077400061113    1C                   END
077500061110     c
077600061110     C* Aggiorna record Q di TITA4
077700061110     C                   movel     'Q'           §trc
077800061110     c                   setoff                                       0506
077900061110     C     ktita         chain     tita430c                           32
078000061113     c* se non c'e' lo scrivo
078100061113    1c                   if        *in32
078200061113     c                   movel     arbaas        ta4aas
078300061113     c                   movel     arblnp        ta4lnp
078400061113     c                   movel     arbnrs        ta4nrs
078500061113     c                   movel     arbnsp        ta4nsp
078600061113     c                   movel     'Q'           ta4trc
078700061113     c                   clear                   ta4not
078800061113    1c                   endif
078900061113     c
079000061110     c* SE mittente a SX , destinatario a DX
079100071029    1c                   if        arbrd2='M'
079200061110     c                   movel     arbcpi        ta4not
079300061110     c                   else
079400061110     c                   move      arbcpi        ta4not
079500061113    1c                   endif
079600061110     c*
079700061113    1c                   if        not *in32
079800061113    2C                   IF        TA4NOT<>*BLANKS
079900061110     C   05              update    tita4000
080000061110     C   06              update    tita4p00
080100061113     C                   ELSE
080200061113     C*SE eliminata e non c'e' più nulla, cancello il record
080300061113     c   05              delete    tita4000
080400061113     c   06              delete    tita4p00
080500061113    2c                   endif
080600061113     c
080700061113   x1c                   else
080800061113    2c                   if        ta4not<>*blanks
080900061113     C  N04              write     tita4000
081000061113     C   04              write     tita4p00
081100061113    2c                   endif
081200061113     c*
081300061113    1c                   endif
081400071029     c
081500071029     c* Aggiorno partita iva
081600071102     c*                      solo se presente o partita la procedura
081700120510    0c******             if        arbrsd<>*blanks or arbdtv>=§vpopiv
081800071029     c* Destinatario --> in titas
081900071029    1c                   if        arbrd2='D'
082000071029     c                   movel     arbrsd        tascpd
082100071029     C* AGGIORNO TITAS
082200071029     C   02              UPDATE    TITAS000
082300071029     C   03              UPDATE    TITAS010
082400071029     C   04              UPDATE    TITASP00
082500071102   x1c                   else
082600071029     c* Mittente --> in titaa
082700071029     c* Aggancio TITAA e aggiorno la partita del mittente
082800071029     c                   movel     'M'           §trc
082900071029     c                   setoff                                       0708
083000071029     c     ktita         chain     titaa30c                           30
083100071102IF  2C                   if        %found(titaa30c)
083200071029     C                   MOVEL     ARBrsd        TAacpi
083300071029     c   07              update    titaa000
083400071029     c   08              update    titaap00
083500071102    2c                   endif
083600071102    1c                   endif
083700071029     C*
083800071029     C* S E C O N D A   B O L L A
083900071029     C*
084000071029     c* solo per destinatario
084100071029    1c                   if        arbrd2='D'  and §3atb2<>'  '
084200071029     C                   MOVEL     §3ATB2        COMTBL
084300071029     C                   SETOFF                                       020304
084400071029     C     KBLP          CHAIN     TiTAS30C                           39
084500071029     c* Aggiorno partita iva
084600071029     c                   movel     arbrsd        tascpd
084700071029     C* AGGIORNO TITAS
084800071029     C   02              UPDATE    TITAS000
084900071029     C   03              UPDATE    TITAS010
085000071029     C   04              UPDATE    TITASP00
085100071102    1C                   END
085200120510    0C*****              END
085300061110     C*
085400061110     C                   ENDSR
085500920113     C**---------------------------------------------------------
085600920113     C** AGGIORNO CODICE VARIAZIONI IX
085700920113     C**---------------------------------------------------------
085800920211     C     AGARBI        BEGSR
085900941130     C                   SETOFF                                       3126
086000941130     C                   SETOFF                                       2225
086100930513     C                   MOVE      TASKSC        KSC4              4 0
086200930513     C*
086300981118     C* chain su YNCPA01L per aggiornare la PIVA e dati destinatario
086400930513    1C     *IN27         IFEQ      '1'
086500930513     C     KSC4          ANDEQ     9999
086600960625     C     TASNFT        ANDGT     0
086700930513     C     *IN24         OREQ      '1'
086800930513     C     SAVKS4        ANDEQ     9999
086900960625     C     SAVNFT        ANDGT     0
087000960625     C*
087100960625     C* SE SI TRATTA DI ASSEGNATO STORNO IGNORO
087200960625     C     *IN24         IFEQ      '1'
087300960625     C     SAVKS4        ANDEQ     9999
087400960625     C                   MOVEL(P)  SAVTBL        KEY
087500960625     C                   ELSE
087600960625     C                   MOVEL(P)  TASTBL        KEY
087700960625     C                   ENDIF
087800960625     C*
087900960625     C                   MOVEL     'TB'          COD
088000960625     C     KTAB          CHAIN     TABEL                              32
088100960625     C  N32              MOVEL     TBLUNI        DSTB
088200960625     C   32              MOVEL     *ALL'1'       DSTB
088300960625     C*  FCB="1" ASSEGNATO BUONO
088400960625   1AC     §TBFCB        IFEQ      '1'
088500930513     C*
088600981118     c                   clear                   wdft
088700981118     c                   select
088800981118     c                   when      *in27
088900981118     c                   move      tasnft        wnft
089000981118     c                   if        tasdft > 0
089100981118     c                   move      tasdft        wdft
089200981118     c                   end
089300981118     c***                move      tasfiv        wfiv
089400981118     c                   move      tasksc        wksc
089500981118     c                   when      *in24
089600981118     c                   move      savnft        wnft
089700981118     c                   if        savdft > 0
089800981118     c                   move      savdft        wdft
089900981118     c                   end
090000981118     c***                move      savfiv        wfiv
090100981118     c                   move      savksc        wksc
090200981118     c                   endsl
090300981118     C     Kppa          CHAIN     ndppa01l                           26
090400960625    2C     *IN26         IFEQ      *OFF
090500990518     c     kycpa         chain     ndcpa01l                           26
090600981118    2C     *IN26         IFEQ      *OFF
090700990518     C                   MOVEL     ARBRSD        cpadescr
090800990518     C                   MOVEL     ARBIND        cpaindiriz
090900981118     C                   MOVEL     ARBCAD        cpacap
091000990518     C                   MOVEL     ARBLOD        cpalocalit
091100981118     C                   MOVEL     ARBPRD        cpaprov
091200981118     c                   movel     'IT  '        cpastato
091300981118     c     arbnzd        ifne      *blank
091400981118     c     arbnzd        andne     'I  '
091500981118     c     arbnzd        andne     'IT '
091600981118     c                   movel     arbnzd        cpastato
091700981118     c                   endif
091800981118     C                   MOVEL     ARBCPI        cpapartiva
091900930513     C*
092000990518     C                   UPDATE    ndcpa000
092100950302    2C                   ENDIF
092200981118    2C                   ENDIF
092300930514     C*
092400960625    1C                   END
092500981118    1C                   END
092600930513     C*
092700950203     C                   MOVEL     §3ATB1        UNOTB1            1
092800920601     C*
092900941130     C* SE CAUSALE VARIAZIONE I0 --> AGGANCIO ARCHIVIO C/ASSEGNI
093000140407    2C*    COMCVB        IFEQ      'I0'
093100140407     C*    §3AFCA        ANDNE     0
093200140407     c                   if        (comcvb='I0' or comcvb='I7' or comcvb='I8'
093300140407     c                             or comcvb='I9') and §3afca<>0
093400930513     C*
093500920212     C                   EXSR      CONCSB
093600941130     C* ERRORE --> NON TROVATO RECORD IN CSB E NON TROVATO LEGAME
093700941130     C   25              GOTO      ENDAGI
093800950227     C* SE IL C/A E' SOLO INCASSATO, POSSO EFFETTUARE LA VARIAIZONE DI
093900950227     C*  DESTINATARIO
094000950227     C  N42CSBBNA        IFEQ      0
094100950227     C                   SETOFF                                       31
094200950227     C                   ENDIF
094300920130     C*
094400950227     C* CONTRASSEGNO ESISTENTE GIA' PAGATO
094500941130    3C     *IN31         IFEQ      '1'
094600930303     C*
094700930303     C* SE ESISTE LEGAME BOLLA (42 ON) NON AGGIORNO ANAGRAFICA C/ASS
094800930303     C* MA AGGIORNO LA BOLLA
094900930303     C*
095000930513    4C     *IN42         IFEQ      '1'
095100930303     C                   SETOFF                                       31
095200930513   X4C                   ELSE
095300941202     C* CONTROLLO SE LA RAGIONE SOCIALE DESTINATARIO E' MODIFICATA
095400941202     C*  --> ERRORE SE C/A GIA' INCASSATO-PAGATO
095500941202     C                   EXSR      CTRDES
095600930513     C*
095700930513    4C                   END
095800920601     C* STAMPO ERRORE
095900941130     C     *IN31         IFEQ      *ON
096000941130     C     *IN42         ANDEQ     *OFF
096100110214     c* ES-14 02 11 -da oggi non stampo più C/Ass pagato per la 102 ma lo stampo
096200110214     c*  per la linea di arrivo se ci fossero problemi o contestazioni la colpa...è sua
096300110214     c* FNARBM non mi serve non accendo il 36
096400130326     c****               movel     taslna        COMFEV
096500110214     C****               SETON                                        36
096600110214     C****               Z-ADD     102           COMFEV
096700130326     c                   z-add     46            COMFEV
096800941130     C                   MOVEL     ERR(10)       ARBDER
096900941130     C                   EXSR      WTRERR
097000930513    3C                   END
097100941130    3C                   END
097200920601     C*
097300941130    3C  N42*IN31         IFEQ      '0'
097400040721     C**  N27
097500040721     C**ANN24              MOVEL     ARBFIN        CSBFIN
097600920115     C                   MOVEL     ARBLOD        CSBPAR
097700920115     C                   MOVEL     ARBPRD        CSBPRO
097800920115     C                   MOVEL     ARBRSD        CSBRSD
097900941130     C                   UPDATE    TNCSB000
098000930513    3C                   END
098100930513    2C                   END
098200920115     C*
098300990625     c                   exsr      agarbi1
098400920113     C*
098500920113     C* S E C O N D A   B O L L A
098600920113     C*
098700930513    3C     §3ATB2        IFNE      '  '
098800920113     C                   MOVEL     §3ATB2        COMTBL
098900941130     C                   SETOFF                                       020304
099000990624     C     KBLP          CHAIN     TiTAS30C                           39
099100121108     c                   exsr      memo_dstitas
099200990628     c                   move      tasksc        ksc4
099300990625     c                   exsr      agarbi1
099400930513    3C                   END
099500941130     C*
099600941130     C     ENDAGI        TAG
099700920113     C*
099800920113     C                   ENDSR
099900990625     C**---------------------------------------------------------
100000990625     C** AGGIORNO CODICE VARIAZIONI IX
100100990625     C**---------------------------------------------------------
100200990630     C     AGARBI1       BEGSR
100300990628     C* I SEGUENTI CAMPI LI AGGIORNO SOLO SE 1 BOLLA NON CONTABILIZZATA
100400990628    1C     *in27         IFEQ      *off
100500990628     C                   Z-ADD     ARBPKb        TASPKB
100600990628     C                   Z-ADD     ARBPKF        TASPKf
100700151103     c* non devo mai aggiornare questo flag perchè quando si tassa fino all'imponibile
100800151103     c*  viene impostato sia qui che nel record  FAT e non deve più essere modificato
100900151103     C********           movel     'R'           TASFPF
101000990625     C*
101100990628    2C     TASFVF        IFNE      'T'
101200990625     C     TASVLF        ANDNE     ARBVLF
101300990628    3C     TASFVF        IFEQ      'Z'
101400990625     C     TASVLF        ANDLT     ARBVLF
101500990625     C     TASFVF        ORNE      'Z'
101600990625     C                   Z-ADD     ARBVLF        TASVLF
101700990625     C                   MOVEL     ARBFVF        TASFVF
101800990625     C                   Z-ADD     ARBVLB        TASVLB
101900990625     C                   MOVEL     ARBFVB        TASFVB
102000990628    3C                   END
102100990628    2C                   END
102200990625     C*
102300990630    2c     ksc42bo       ifne      9999
102400990625     C* DETERMINO IL CODICE DI TASSAZIONE
102500990630    3C     COMCVB        ifeq      'I0'
102600140407    3C     COMCVB        oreq      'I7'
102700140407    3C     COMCVB        oreq      'I8'
102800140407    3C     COMCVB        oreq      'I9'
102900990628     c* la call al tisi mi basta farla solo la prima volta e cioe quando
103000990625     c* sono sulla prima bolla
103100990630    4c     comtbl        ifeq      §3atb1
103200120322     c                   clear                   savcts
103300120322     c                   exsr      c_tisi95
103400120322     C* SE ERRORE da tisi95R LO SCRIVO NEL FILE DEGLI ERRORI E NON LO ANDRO'
103500990625     C* A SOSTITUIRE
103600990630    5C     O95ERR        IFNE      *BLANKS
103700990625     C                   MOVEL     ERR(30)       ARBDER
103800990625     C                   Z-ADD     102           COMFEV
103900990625     C                   EXSR      WTRERR
104000990630   X5C                   ELSE
104100990630     C                   MOVEL     O95CTS        SAVCTS
104200990630    5C                   END
104300990630    4c                   END
104400990630    4c     SAVCTS        IFNE      *BLANKS
104500990630     C                   MOVEL     SAVCTS        TASCTS
104600990630    4C                   END
104700990630    3C                   END
104800990625
104900990625     C                   MOVEL     ARBFIN        TASFIN
105000990625     C                   MOVEL     ARBCAD        TASCAD
105100990625     C                   MOVEL     ARBNZD        TASNZD
105200990630    2C                   END
105300990630    1C                   END
105400990625     C*
105500990625RM*  C                   MOVEL     ARBFFD        TASFFD
105600990625     C*
105700990628     c* memorizzo se sono variati inoltro,cap o nazione
105800990625     c                   clear                   flgmod
105900990628    1c     arbfin        ifne      tasfin
106000990625     c     arbcad        orne      tascad
106100990625     c     arbnzd        orne      tasnzd
106200990625     c                   move      '1'           flgmod            1
106300990628    1c                   endif
106400990625     c
106500990628     c* Se ho la seconda bolla contabilizzata 9999 non aggiornerò mai i
106600990628     c* i seguenti campi nemmeno sulla prima bolla.
106700990630     c* Dopodichè posso aggiornarli solo se la 1bolla non è contabilizzata
106800990628     c* o se non sono stati modificati inoltro cap e nazione
106900990628    1c     ksc42bo       ifne      9999
107000990628    2c     *in27         ifeq      *off
107100990628     c     flgmod        oreq      *blank
107200990625     C                   MOVEL(P)  ARBRSD        TASRSD
107300990625     C                   MOVEL(P)  ARBIND        TASIND
107400990625     C                   MOVEL(P)  ARBLOD        TASLOD
107500990625     C                   MOVEL(P)  ARBPRD        TASPRD
107600990701     c* Dalla filiale posso ricevere la partita iva del destinatario
107700990701     c* solo se la bolla è assegnato o solo in caso di doppia bolla
107800990701     c* In ogni caso vado SEMPRE in sostituzione di tascpd
107900990625     C                   MOVEL     ARBCPI        TASCPD
108000140219    2c                   endif
108100140219    1c                   endif
108200990625     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
108300130325     C***                MOVEL     ERR(9)        ARBDER
108400130325     c***                exsr      wtrerr
108500140219     c* La scrittura/aggiornamento del tivrb viene fatta sempre se la
108600140219     c* prima bolla è contabilizzata.
108700140219     c* Viene fatta a prescindere dal contenuto di FLGMOD perchè
108800140219     c* deve receprire anche eventuali variazioni che riportano
108900140219     c* i dati alla situazione originale ciè quella di titas
109000140219     c* Es: se prima vario inoltor da "P" a "I" e poi rimetto
109100140219     c* "P" devo receprie sul tivrb anche quest'ultima variazione
109200140219    1c                   if        *in27=*on
109300120321     c                   exsr      ctr_vrb
109400140219    1c                   END
109500140219   x1c*                  else
109600990628     c* se bolla contabilizzata e variati fin,cad,nzd scrivo errore
109700130325     C***                MOVEL     ERR(9)        ARBDER
109800130325     c***                exsr      wtrerr
109900140219     c*                  exsr      ctr_vrb
110000140219    1c*                  END
110100990625     C* AGGIORNO TITAS
110200990625     C   02              UPDATE    TITAS000
110300990625     C   03              UPDATE    TITAS010
110400990625     C   04              UPDATE    TITASP00
110500990625     C*
110600990625     c                   endsr
110700120322     C**---------------------------------------------------------
110800120322     C** Richiamo tisi95 per determinazione codice tassazione
110900120322     C**---------------------------------------------------------
111000120322     C     C_tisi95      BEGSR
111100120322     C                   CLEAR                   DSSI95
111200120322     C                   MOVEL     ARBCAD        I95CAP
111300120322     C                   MOVEL     ARBNZD        I95NAR
111400120322     C                   MOVEL     ARBLOD        I95LOC
111500120322     C                   MOVEL     ARBPRD        I95PRV
111600121116     c                   movel     tasaas        i95dat
111700121116     c                   move      tasmgs        i95dat
111800121116     C****               MOVEL     TASDSP        I95DAT
111900120322     C                   MOVEL     '7'           I95TCN
112000120322      * Se network FedEx passo flag apposito per dire di prendere i dati
112100120322      * dalla tabella della nazione
112200120322     c     taslna        chain     azorg01l
112300120322     c                   if        %found(azorg01l) and orgfva = *blanks
112400120322     c                   eval      og143 = orgde3
112500120322     c     §ogntw        ifeq      'FED'
112600120322     c                   eval      i95fi1 = 'S'
112700120322     c                   endif
112800120322     c                   endif
112900120322
113000120322     C                   MOVEL     §3ATB1        I95TPO
113100120322    5C     §3AFCA        IFNE      0
113200120322     C                   MOVEL     'S'           I95FCA
113300120322    5C                   ENDIF
113400120322     C**
113500120322     C                   CALL      'TISI95R'
113600120322     C                   PARM                    DSSI95
113700120322     c                   endsr
113800950202     C**---------------------------------------------------------
113900950202     C** AGGIORNO CODICE VARIAZIONI MX
114000950202     C**---------------------------------------------------------
114100950202     C     AGARBM        BEGSR
114200950203     C* VEDO SE ESISTE CODICE
114300950202     C                   MOVE      ARBCCM        KSC4
114400950202     C*
114500950202     C* SE E' CODIFICATO CHAIN SU ANAGRAFICA
114600950202     C     KSC4          IFNE      8888
114700981001     C                   CLEAR                   DSBS69
114800981001     C                   MOVEL     KNSIF         I69SIF
114900981001     C                   MOVEL     KCI           I69KCC
115000981001     C                   MOVEL     ARBCCM        I69KAC
115100981001     C                   MOVEL     ARBCCM        I69KIN
115200981001     C                   CALL      'TIBS69R'
115300981001     C                   PARM                    DSBS69
115400981001     C                   PARM                    DSACO
115500981001     C                   PARM                    DSIND
115600981001     C                   PARM                    DSCLP
115700981001     C                   PARM                    DSCLS
115800950202     C*
115900950203     C* DO ERRORE SOLO SE SI TRATTA DI CLIENTE DI ALTRA FILIALE
116000981001     C     O69ERR        IFNE      ' '
116100950203     C* CONFRONTO LE 2 FILIALI ELABORATORE
116200971215     C**         ARBLNP    CHAINAZORG                30
116300971215     C**      N30          MOVELORGFEL    LNPFEL  30
116400971215     C**       30          MOVELARBLNP    LNPFEL
116500971215     C**
116600971215     C**                   MOVELARBCCM    W0030   30
116700971215     C**         W0030     CHAINAZORG                30
116800971215     C**      N30          MOVELORGFEL    KSCFEL  30
116900971215     C**       30          MOVELW0030     KSCFEL
117000971215     C*
117100971215     C**         KSCFEL    IFNE LNPFEL
117200971215     C**                   MOVELERR,27    ARBDER
117300971215     C**                   EXSR WTRERR
117400950203     C*
117500971215     C**                   ELSE
117600950203     C* SE STESSA FIL ELABORATORE: NON CANCELLO LA VARIAZIONE
117700950203     C*  E ATTENDO L'ARRIVO DEL PDC
117800950203     C                   SETON                                        37
117900971215     C**                   ENDIF
118000950203     C*
118100950202     C                   GOTO      ENDGBM
118200960919     C                   ELSE
118300960919     C* SE CLIENTE BLOCCATO CARICO LO STESSO MA SEGNALO
118400100303     C***  ACOABL        IFEQ      '8'
118500100303     C***                MOVEL     ERR(29)       ARBDER
118600100303     C***                EXSR      WTRERR
118700100303     C***                END
118800950202     C                   ENDIF
118900950202     C                   ENDIF
119000950203     C*
119100950203     C* VERIFICO ESISTENZA NAZIONE E PROVINCIA MITTENTE
119200970714     C                   CLEAR                   DSSI95
119300970714     C                   MOVEL     ARBCAM        I95CAP
119400121116     C                   MOVEL     ARBNZM        I95NAR
119500121116     c                   movel     tasaas        i95dat
119600121116     c                   move      tasmgs        i95dat
119700121116     C****               MOVEL     TASDSP        I95DAT
119800970714     C                   MOVEL     '3'           I95TCN
119900020311      * Se network FedEx passo flag apposito per dire di prendere i dati
120000020311      * dalla tabella della nazione
120100020311     c     arblnp        chain     azorg01l
120200020311     c                   if        %found(azorg01l) and orgfva = *blanks
120300020311     c                   eval      og143 = orgde3
120400020311     c     §ogntw        ifeq      'FED'
120500020311     c                   eval      i95fi1 = 'S'
120600020311     c                   endif
120700020311     c                   endif
120800020311
120900970714     C                   CALL      'TISI95R'
121000970714     C                   PARM                    DSSI95
121100950203     C*
121200970714     C     O95ERR        IFNE      *BLANKS
121300130326     C                   Z-ADD     102           COMFEV
121400950203     C                   MOVEL     ERR(26)       ARBDER
121500950203     C                   EXSR      WTRERR
121600950203     C                   GOTO      ENDGBM
121700950203     C                   ENDIF
121800950203     C*
121900950203     C* VERIFICO SE ESISTEN L'ANAGRAFICA DEI C/A E SE GIA' INCASSATO
122000950222    1C     §3AFCA        IFNE      0
122100950202     C                   EXSR      CONCSB
122200950222     C* INESISTENTE  --> NON AGGIORNO
122300950222     C   25              GOTO      ENDGBM
122400950203     C***
122500950203     C*** A G G I O R N O   C / A S S E G N O
122600950222     C* AGGIORNO SOLO SE NON E' INCASSATO
122700950222   1AC     *IN31         IFEQ      *OFF
122800950202     C     KANN          CHAIN     TNCSA000                           30
122900950202     C*
123000950202     C* NON E' 8888 :SE CREATA CANCELLO ANAGRAFICA C/A
123100950202IF  2C     KSC4          IFNE      8888
123200970916IF  2C     KSC4          ANDNE     9999
123300950202     C  N30              DELETE    TNCSA000
123400950202X   2C                   ELSE
123500950202     C                   CLEAR                   TNCSA000
123600950202     C*
123700950202     C                   MOVEL     CSBAAS        CSAAAS
123800950202     C                   MOVEL     CSBLNP        CSALNP
123900950202     C                   MOVEL     CSBNRS        CSANRS
124000950202     C                   MOVEL     CSBNSP        CSANSP
124100950202     C                   MOVEL     ARBRSM        CSARSD
124200950202     C                   MOVEL     ARBINM        CSAVID
124300950202     C                   MOVEL     ARBCAM        CSACAD
124400950202     C                   MOVEL     ARBLOM        CSACID
124500950202     C                   MOVEL     ARBPRM        CSAPRM
124600950202     C                   MOVEL     ARBNZM        CSANZM
124700950202     C*
124800950202     C  N30              UPDATE    TNCSA000
124900950202     C   30              WRITE     TNCSA000
125000950202E   2C                   ENDIF
125100950202     C*
125200950202     C                   MOVEL     ARBCCM        CSBCDI
125300950202     C*
125400950202     C* SE C/A SU 1 BOLLA IMPOSTO CODICE BOLLA AMCHE
125500950202    2C     §3AFCA        IFEQ      1
125600950202     C                   MOVEL     ARBCCM        CSBKSC
125700950202    2C                   ENDIF
125800950202     C*
125900950202     C                   UPDATE    TNCSB000
126000950222   1AC                   ENDIF
126100950222    1C                   ENDIF
126200950202     C*
126300950203     C* A G G I O R N O   L A   1  B O L L A
126400970714     C                   MOVEL     O95CTS        TASMCT
126500950202     C                   MOVEL     ARBCAM        TASCAM
126600950202     C                   MOVEL     ARBNZM        TASNZM
126700950202     C                   MOVEL     ARBFAP        TASFAP
126800950202     C                   MOVEL     ARBCCM        TASKSC
126900990628     C                   MOVEL     ARBCCM        TASccm
127000950202     C*
127100950202     C*
127200990628     c                   movel     'M'           §trc
127300990629     c     ktita         chain     titaa000                           30
127400950202IF  3C     KSC4          IFEQ      8888
127500990628     c   30              z-add     tasaas        taaaas
127600990628     c   30              z-add     taslnp        taalnp
127700990628     c   30              z-add     tasnrs        taanrs
127800990628     c   30              z-add     tasnsp        taansp
127900990628     c   30              movel     §trc          taatrc
128000990628     C                   MOVEL     ARBRSM        TAaRSc
128100990628     C                   MOVEL     ARBINM        TAaIND
128200990628     C                   MOVEL     ARBCAM        TAaCAP
128300990628     C                   MOVEL     ARBLOM        TAaLOc
128400990628     C                   MOVEL     ARBPRM        TAaPRV
128500990628     C                   MOVEL     ARBNZM        TAaNAZ
128600990628     C                   MOVEL     ARBCPI        TAacpi
128700990628     c  n30              update    titaa000
128800990628     c   30              write     titaa000
128900990628     c                   else
129000990628     c* se prima della variazione era un 8888 lo vado a cancellare da
129100990628     c* titaa00f
129200990628     c  n30              delete    titaa000
129300990628     c
129400950202E   3C                   ENDIF
129500950202     C*
129600990628     C   02              UPDATE    TiTAS000
129700071029     C***
129800950203     C*** A G G I O R N O   2   B O L L A
129900950202    3C     §3ATB2        IFNE      '  '
130000950202     C                   MOVEL     §3ATB2        COMTBL
130100950202     C                   SETOFF                                       020304
130200990628     C     KBLP          CHAIN     TiTAS30C                           39
130300950202     C*
130400950203     C     *IN39         IFEQ      *OFF
130500950202     C                   MOVEL     ARBCCM        TASCCM
130600950202     C*
130700950202     C                   MOVEL     ARBCAM        TASCAM
130800950202     C                   MOVEL     ARBNZM        TASNZM
130900950202     C                   MOVEL     ARBFAP        TASFAP
131000970714     C                   MOVEL     O95CTS        TASMCT
131100950202     C*
131200990628     C   02              UPDATE    TiTAS000
131300990628     C   03              UPDATE    TiTAS010
131400990628     C   04              UPDATE    TiTASP00
131500950202     C                   ENDIF
131600950202     C                   ENDIF
131700950202     C*
131800950202     C     ENDGBM        ENDSR
131900950202     C**---------------------------------------------------------
132000950202     C** AGGIORNO CODICE VARIAZIONI CX
132100950202     C**---------------------------------------------------------
132200950202     C     AGARBC        BEGSR
132300010126     C     arblnp        ifne      SAVLNP
132400010126     C     arblnp        chain     azorg01l                           32
132500010126     C  n32              movel     orgde3        og143
132600010126     C   32              clear                   og143
132700010126     C                   z-add     arblnp        savlnp
132800010126     C                   endif
132900010126     C* SE BOLLA POSTE ED AGGIUNTO O TOLTO IL FUORI MISURA BISOGNA
133000010126     C* AGGIORNARE tita4 TIPO RECORD "J"
133100010126     C     §ogpt         ifeq      'S'
133200010126     C                   clear                   wtcp
133300010126     C                   clear                   wsvtcp
133400010126    2C     arbtc1        ifeq      'F'
133500010126     C     arbtc2        oreq      'F'
133600010126     C                   movel     'F'           wtcp
133700010126    2C                   endif
133800010126    2C     tasftc        ifeq      'F'
133900010126     C     tastc2        oreq      'F'
134000010126     C                   movel     'F'           wsvtcp
134100010126    2C                   endif
134200010126    2C                   select
134300010126     C* SE NON E' STATA NE' AGGIUNTA NE' TOLTA LA "F" NON FACCIO NIENTE
134400010126     C     arbtc1        whenne    'F'
134500010126     C     arbtc2        andne     'F'
134600010126     C     tasftc        andne     'F'
134700010126     C     tastc2        andne     'F'
134800010126     C     wtcp          oreq      'F'
134900010126     C     wsvtcp        andeq     'F'
135000010126     C                   other
135100010126     C                   movel     'J'           §trc
135200010126     c                   setoff                                       0506
135300010126     C     ktita         chain     tita430c                           32
135400010126    3C     *in32         ifeq      *off
135500010126     C                   movel     ta4not        dsbl4j
135600010126     C* TOLTO LA 'F'
135700010126    4C     wtcp          ifeq      *blanks
135800010126     C     wsvtcp        andeq     'F'
135900010126     C                   clear                   §b4bai
136000010126   X4C                   else
136100010126     C* AGGIUNTA 'F'
136200010126    5C     wtcp          ifeq      'F'
136300010126     C     wsvtcp        andeq     *blanks
136400010126     C                   movel     'S'           §b4bai
136500010126    5C                   endif
136600010126    4C                   endif
136700010126     C                   movel     dsbl4j        ta4not
136800010126     C   05              update    tita4000
136900010126     C   06              update    tita4p00
137000010126   X3C                   else
137100010126     C* AGGIUNTA 'F' --> CREO RECORD "J"
137200010126    4C     wtcp          ifeq      'F'
137300010126     C     wsvtcp        andeq     *blanks
137400010126     C                   clear                   tita4000
137500010126     C                   z-add     tasaas        ta4AAS
137600010126     C                   z-add     taslnp        ta4LNP
137700010126     C                   z-add     tasnrs        ta4NRS
137800010126     C                   z-add     tasnsp        ta4NSP
137900010126     C                   movel     'J'           ta4TRC
138000010126     C                   clear                   dsbl4j
138100010126     C                   clear                   ta4not
138200010126     C                   movel     'S'           §B4BAI
138300010126     C                   movel     dsbl4j        ta4NOT
138400010126     C  N04              write     tita4000
138500010126     C   04              write     tita4p00
138600010126    4C                   endif
138700010126    3C                   endif
138800010126    2C                   endsl
138900010126     C                   endif
139000950202     C* AGGIORNO SEMPRE  LE CONSEGNE PARTICOLARI SOLO SU  1 BOLLA
139100991018     c* idem per gg chiusura e data consegna richiesta
139200950202     C                   MOVEL     ARBTC1        TASFTC
139300950202     C                   MOVEL     ARBTC2        TASTC2
139400991018     c                   movel     arbtcr        tastcr
139500991018     c                   z-add     arbdcr        tasdcr
139600991018     c                   z-add     arbhcr        tashcr
139700991018     C                   MOVEL     arbgC1        tasgc1
139800991018     C                   MOVEL     arbgC2        tasgc2
139900990628     C   02              UPDATE    TiTAS000
140000990628     C   03              UPDATE    TiTAS010
140100990628     C   04              UPDATE    TiTASP00
140200950202     C                   ENDSR
140300950202     C*
140400920113     C**---------------------------------------------------------
140500920113     C** AGGIORNO CODICE VARIAZIONI TX
140600920113     C**---------------------------------------------------------
140700920113     C     AGARBT        BEGSR
140800920113     C*
140900920226     C                   EXSR      CTR2
141000920226     C*
141100941130    1C  N29              DO
141200950202     C*
141300991102     c
141400990419     c* controllo l'importo da assicurare se la spedizione e' tassata
141500990419     c*  o con tot.imponibile
141600011029     c                   if        tasias > 0  and tasvas = *blanks
141700011029     c                   movel     'ITL'         tasvas
141800011029     c                   endif
141900990419     c                   if        arbias <> tasias or
142000990419     c                             arbvas <> tasvas
142100990419     c*
142200990419     c                   exsr      AGGIAS
142300990419     c                   endif
142400950202     C*
142500950202     C                   Z-ADD     ARBIAS        TASIAS
142600950202     C                   MOVEL     ARBVAS        TASVAS
142700990824     c                   clear                   TASFCM
142800920113     C*
142900941130    2C     §3ATB2        IFEQ      '  '
143000970514     C     COMCVB        OREQ      'TP'
143100950222     C* AGGIORNO KSC SOLO SE VARIAZIONE DA ARRIVO E NON DA PARTENZA
143200950222     C*  PERCHE' SI TRATTA DI VARIAZIONE DI MITTENTE E NON DI TASSAZION
143300950222     C     ARBCVB        IFNE      'TP'
143400920113     C                   Z-ADD     ARBKSC        TASKSC
143500950222     C                   ENDIF
143600051219     c* Non vado sopra al codice tariffa se bolla in assegnato tassata con
143700051219     c* tariffa mittente
143800051219     c     tasftm        ifeq      *blanks
143900051219     C     comcvb        oreq      'TP'
144000950222     C                   MOVEL     ARBCTR        TASCTR
144100051219     c                   endif
144200950202     C                   Z-ADD     ARBQFT        TASQFT
144300991015     c                   MOVEL     ARBDIV        TASDIV
144400920113     C                   Z-ADD     ARBPOR        TASPOR
144500920113     C                   MOVEL     ARBSV1        TASSV1
144600920113     C                   Z-ADD     ARBVA1        TASVA1
144700920113     C                   MOVEL     ARBSV2        TASSV2
144800941130     C                   Z-ADD     ARBVA2        TASVA2
144900950202     C                   MOVEL     ARBSV3        TASSV3
145000950202     C                   Z-ADD     ARBVA3        TASVA3
145100941130     C*
145200990628     C* SE ESITONO GIA' DELLE VARIE IN TITA700F LE CANCELLO
145300990628     C     KBLP          SETLL     TITA7000
145400990628     C     KBLP          READE     TITA7000                               30
145500941130     C*
145600941130    3C     *IN30         DOWEQ     *OFF
145700991015     C                   DELETE    TITA7000
145800991015     C     KBLP          READE     TITA7000                               30
145900941130    3C                   ENDDO
146000991015     C     KARBU         SETLL     FIARBU1R
146100991015     C     KARBU         READE     FIARBU1R                               30
146200950202     C*
146300950202     C     *IN30         DOWEQ     *OFF
146400990628     C                   CLEAR                   TiTA7000
146500950202     C                   Z-ADD     ARBAAS        TA7AAS
146600950202     C                   Z-ADD     ARBLNP        TA7LNP
146700950202     C                   Z-ADD     ARBNRS        TA7NRS
146800950202     C                   Z-ADD     ARBNSP        TA7NSP
146900950202     C                   MOVEL     TASTBL        TA7TBL
147000950202     C                   MOVEL     ARBSVN        TA7SVN
147100950202     C                   Z-ADD     ARBVAN        TA7VAN
147200990629     C                   WRITE     TITA7000
147300950202     C*
147400950202     C                   ADD       1             NUMRBU            7 0
147500991015     C                   DELETE    FIARBURR
147600950202     C*
147700991015     C     KARBU         READE     FIARBU1R                               30
147800950202     C                   ENDDO
147900990628     c
148000941130     C*
148100920113     C                   Z-ADD     ARBDFT        TASDFT
148200920113     C                   Z-ADD     ARBNFT        TASNFT
148300920113     C                   Z-ADD     ARBFIV        TASFIV
148400941130     C                   MOVEL     ARBCEI        TASFEI
148500920113     C*
148600990624     C                   z-add     arbimv        TASimv
148601151120     c* Totale imponibile = 0 --> Cancello record FAT se presente e imposto fpf='R'
148602151120    3c                   if        tasimv=0
148603151120     c                   Eval      kar5trd='FAT'
148604151120     c     kar5          Chain     Fiar531c
148605151120     c                   if        %found(fiar531c)
148606151120     c   02              delete    fiar5000
148607151120     c                   endif
148608151118     c                   eval      tasfpf='R'
148609151120   x3c                   else
148610151120     c* Totale imponibile > 0 --> Scrivo record FAT se manca imposto in base a record FAT
148611151120     c*                           Imposto TASFPF in base al record FAT
148612151118     c                   clear                   dar5fat
148613151118     c                   Eval      kar5trd='FAT'
148614151118     c     kar5          Chain     Fiar531c
148615151120    4c                   If        %Found(Fiar531c)
148616151118     c                   Movel     ar5uni        dar5fat
148617151120   x4c                   else
148618151120     c                   Eval      Ar5Aas = ArbAas
148619151120     c                   Eval      Ar5Lnp = ArbLnp
148620151120     c                   Eval      Ar5Nrs = ArbNrs
148621151120     c                   Eval      Ar5Nsp = ArbNsp
148622151120     c                   Eval      Ar5Trd = 'FAT'
148624151120     c                   movel     tasvlf        §ar5vltas
148625151120     c                   movel     tasfvf        §ar5fvtas
148627151120     c                   movel     taspkf        §ar5pktas
148628151120     c                   movel     'R'           §ar5fptas
148629151120     c                   eval      §AR5DATAS=%dec(%date())
148630151120     c                   eval      §AR5HMTAS=%dec(%time())
148631151120     c                   eval      §AR5PRTAS=knmus
148632151120     c                   eval      §AR5NJTAS=knmtd
148633151120     c                   Movel(p)  dar5fat       Ar5uni
148634151120     c                   eval      ar5dac=%dec(%date())
148635151120     c                   eval      ar5orc=%dec(%time())
148636151120     c                   Eval      AR5FT1='T'
148637151120     c                   Eval      AR5DT1=99999999
148638151120     c                   Eval      AR5FT2='T'
148639151120     c                   Eval      AR5DT2=99999999
148640151120     c                   Eval      AR5FT3='T'
148641151120     c                   Eval      AR5DT3=99999999
148642151120     c   02              write     fiar5000
148643151120    4c                   endif
148644151120     c* Imposto TASFPF
148645151118     c                   select
148646151118     c                   when      §AR5FPTAS='R'
148647151118     c                   eval      tasfpf='R'
148648151118     c                   when      §AR5FPTAS='D'
148649151118     c                   eval      tasfpf='D'
148650151118     c                   other
148651151118     c                   eval      tasfpf='V'
148652151118     c                   endsl
148653151120    3c                   endif
148700941130    3C     ARBIFT        IFGT      0
148800941130     C                   MOVEL     '1'           TASFTS
148900920113     C                   ELSE
149000941130     C                   MOVEL     ' '           TASFTS
149100941130    3C                   END
149200941130    2C                   END
149300920113     C* AGGIORNO
149400990628     C   02              UPDATE    TiTAS000
149500941130    1C                   ENDDO
149600920113     C*
149700920113     C                   ENDSR
149800920113     C**---------------------------------------------------------
149900920113     C** AGGIORNO CODICE VARIAZIONI 2T
150000920113     C**---------------------------------------------------------
150100920113     C     AGAR2T        BEGSR
150200920113     C*
150300920226     C* SECONDA BOLLA
150400920113     C     §3ATB2        IFNE      '  '
150500920226     C                   EXSR      CTR2
150600920114     C*
150700920226     C  N29              DO
150800991015     C                   Z-ADD     ARBKSC        TASKSC
150900991015     C                   MOVEL     ARBCTR        TASCTR
151000991015     C                   movel     arbdiv        tasdiv
151100991015     C                   MOVEL     ARBSV1        TASSV1
151200991015     C                   Z-ADD     ARBVA1        TASVA1
151300991015     C                   MOVEL     ARBSV2        TASSV2
151400991015     C                   Z-ADD     ARBVA2        TASVA2
151500040114     c                   Movel     ArbSv3        TasSv3
151600040114     c                   Z-Add     ArbVa3        TasVa3
151700040114      * Se esistono già della varie su Tita7 le devo eliminare
151800040114     c     Kblp          Setll     Tita7000
151900040114     c                   Do        *Hival
152000040114     c     Kblp          Reade     Tita7000
152100040114     c                   If        %Eof
152200040114     c                   Leave
152300040114     c                   EndIf
152400040114     c                   Delete    Tita7000
152500040114     c                   EndDo
152600040114      * Scrivo il nuovo Tita7 e cancello il file di ricezione
152700040114     c     Karbu         Setll     Fiarbu1r
152800040114     c                   Do        *Hival
152900040114     c     Karbu         Reade     Fiarbu1r
153000040114     c                   If        %Eof(Fiarbu1r)
153100040114     c                   Leave
153200040114     c                   EndIf
153300040114     c                   Clear                   Tita7000
153400040114     c                   Z-add     ArbAas        Ta7Aas
153500040114     c                   Z-add     ArbLnp        Ta7Lnp
153600040114     c                   Z-add     ArbNrs        Ta7Nrs
153700040114     c                   Z-add     ArbNsp        Ta7Nsp
153800040114     c                   Movel     TasTbl        Ta7Tbl
153900040114     c                   Movel     ArbSvn        Ta7Svn
154000040114     c                   Z-add     ArbVan        Ta7Van
154100040114     c                   Write     Tita7000
154200040114     c                   Add       1             NumRbu
154300040114     c                   Delete    Fiarburr
154400040114     c                   EndDo
154500040114
154600991015     C                   Z-ADD     ARBDFT        TASDFT
154700991015     C                   Z-ADD     ARBNFT        TASNFT
154800991015     C                   Z-ADD     ARBFIV        TASFIV
154900991015     C                   MOVEL     ARBCEI        TASFEI
155000040114     C**!!!              z-add     arbva1        TASimv
155100040114     c                   Z-add     ArbImv        TasImv
155200991015     C     ARBIFT        IFGT      0
155300941130     C                   MOVEL     '1'           TASFTS
155400920114     C                   ELSE
155500941130     C                   MOVEL     ' '           TASFTS
155600920114     C                   END
155700920113     C*
155800040114      * Aggiorno Titas
155900990628     C   02              UPDATE    TiTAS000
156000920113     C                   END
156100920130     C                   END
156200920113     C*
156300920113     C                   ENDSR
156400920127     C**---------------------------------------------------------
156500920127     C** AGGIORNO CODICE VARIAZIONI KX
156600920127     C**---------------------------------------------------------
156700920127     C     AGARBK        BEGSR
156800121109     c                   if        *in11='0' and *in16='0'
156900121109     c                   exsr      memo_csb
157000121109     c                   endif
157100121205     c                   clear                   wanncas
157200920212     C                   SETOFF                                       31
157300941130     C                   CLEAR                   TNCSB000
157400950202     C* NON AGGIORNO SE HO LETTO
157500950202    1C     *IN11         IFEQ      *OFF
157600950202     C     *IN16         ANDEQ     *OFF
157700920127     C* VARIA CONTRASSEGNO
157800941130    2C     ARBCBP        IFEQ      ARBCBN
157900920127     C* VEDO CHE TIPO BOLLA AGGANCIARE DAL FLAG C/ASSEGNO
158000920131     C* RIAGGANCIO SOLO LA SECONDA PERCHE' LA PRIMA L'HO GIA' AGGANCIAT
158100941130    3C     §3AFCA        IFEQ      2
158200920127     C                   MOVEL     §3ATB2        COMTBL
158300941130     C                   SETOFF                                       020304
158400990625     C     KBLP          CHAIN     TITAS30C                           39
158500121108     c                   exsr      memo_dstitas
158600941130    3C                   END
158700920228     C*
158800920228     C* CONTROLLO SPEDIZIONE SE OK
158900990414     C                   EXSR      CTR3
159000920210     C*
159100920212     C* CONTROLLO ARCHIVIO C/ASSEGNO SE ERA PREVISTO
159200941130    3C     §3AFCA        IFNE      0
159300920212     C                   EXSR      CONCSB
159400941130    3C                   END
159500920210     C* SE NO ERRORE
159600950324    3C  N31CSBDDC        IFEQ      0
159700950324     C     CSBBNA        ANDEQ     0
159800941130     C                   MOVEL     *BLANKS       ARBDER
159900941130     C*
160000920228     C*** AGGIORNO CONTRASSEGNO
160100920228     C*** PRIMA     ESISTEVA ORA     ESISTE: V A R I A Z I O N E
160200941130    4C     CSBCAS        IFNE      0
160300950202     C     ARBCAS        ANDNE     0
160400950202     C* SCRIVO SOLO SE IMPORTO DIVERSO
160500950202    5C     ARBCAS        IFNE      CSBCAS
160600991103    5C     ARBVCA        ORNE      WCSBVCA
160700000703     c*
160800000703     c     csbsta        oreq      9
160900920212     C* SCRIVO STORICO
161000920212     C                   MOVEL     'VACA'        CSVCAV
161100920212     C                   EXSR      CSVWTR
161200950202     C*
161300950202     C                   EXSR      AGGCSB
161400950202     C*
161500941130     C                   UPDATE    TNCSB000
161600920228     C*
161700120420     C* E R R O R E : NON MODIFICABILE IMPORTO VARIA G
161800130325     C***29              MOVEL     ERR(22)       ARBDER
161900120322     C   29              exsr      ctr_vrb
162000941130    5C                   END
162100941130    4C                   END
162200920228     C***
162300920228     C*** PRIMA     ESISTEVA ORA NON ESISTE: A N N U L L O
162400941130    4C     CSBCAS        IFNE      0
162500950202     C     ARBCAS        ANDEQ     0
162600920212     C                   EXSR      ANNCSB
162700920226     C*
162800920228     C* E R R O R E : NON ELIMINABILE IMPORTO VARIA G
162900920316     C                   MOVEL     *BLANKS       ARBDER
163000990505    5C   29              DO
163100941130     C* CONTROLLO SE ESISTE VARIA "G"
163200130326     C****               EXSR      ESVARG
163300130325    6C***  WSIVAR        IFEQ      'S'
163400130325     C***                MOVEL     ERR(14)       ARBDER
163500130325    6C***                END
163600121205     c                   eval      wanncas='S'
163700121203     c                   exsr      ctr_vrb
163800941130    5C                   END
163900941130    4C                   END
164000920228     C***
164100920228     C*** PRIMA NON ESISTEVA ORA     ESISTE: C R E O  RECORD C/ASSEGNO
164200941130    4C     CSBCAS        IFEQ      0
164300950202     C     ARBCAS        ANDNE     0
164400920212     C                   MOVEL     §3ATB1        TBLCA
164500941130     C                   MOVEL     §3ATB1        FLGTB1
164600941130     C*
164700941130     C                   MOVEL     TASLOD        DESPAR
164800990625     C                   MOVEL     TASPRD        DESPRV
164900950913     C*
165000920212     C                   EXSR      CRTCSB
165100920228     C* E R R O R E : NON POSSO AGGIUNGERE IMPORTO VARIA G
165200130325     C***29              MOVEL     ERR(18)       ARBDER
165300130325     c***29              setoff                                       36
165400120322     C   29              exsr      ctr_vrb
165500941130    4C                   END
165600920228     C***
165700920228     C*** AGGIORNO SPEDIZIONE
165800941130     C                   MOVEL     'S'           TASFCA
165900920228     C* SE GIA' TASSATA METTO A POSTO PER VARIA G
166000110203     c                   if        *in38
166100110203     c                   Z-ADD     arbcas        wcas
166200110203     C                   EXSR      VARIAG
166300110203     c
166400110203      * riorganizzo le varie se è necessario
166500110203     c                   if        flgriorg = '1'
166600110203     c                   exsr      sr_riorg
166700110203     c                   endif
166800110203     c                   endif
166900110203
167000950202     C* SCRIVO ERRORE SE GIA' CONTABILIZZATA/STORICIZZATA
167100941130    4C     ARBDER        IFNE      *BLANKS
167200920309     C                   EXSR      WTRERR
167300941130    4C                   END
167400920228     C*
167500990625     C   02              UPDATE    TITAS000
167600990625     C   03              UPDATE    TITAS010
167700990625     C   04              UPDATE    TITASP00
167800941130    3C                   END
167900920127     C*
168000941130   X2C                   ELSE
168100920228     C******
168200920228     C****** M O D I F I C A T O   T I P O   B O L L A
168300920228     C******
168400920212     C                   SETOFF                                       31
168500920127     C                   MOVEL     §3ATB1        SAVTB1            2
168600920127     C                   MOVEL     §3ATB2        SAVTB2            2
168700920227     C                   MOVEL     §3AFCA        SAVFCA            1 0
168800920227     C                   MOVEL     §3AFTD        SAVFTD            1 0
168900941130    3C     §3ABCM        IFNE      ' '
169000920324     C                   MOVEL     0             SAVFCA
169100941130    3C                   END
169200920127     C* NUOVO TIPO BOLLA
169300920127     C                   MOVEL     '3A'          COD
169400941130     C                   MOVEL(P)  ARBCBN        KEY
169500920212     C     KTAB          CHAIN     TABEL                              34
169600941130     C*
169700941130     C* COD BOLLA NUOVO NON TROVATO
169800941130    3C     *IN34         IFEQ      *ON
169900941130     C                   MOVEL     ERR(6)        ARBDER
170000920212     C*
170100941130     C                   SETON                                        37
170200941130     C                   Z-ADD     102           COMFEV
170300941130     C                   EXSR      WTRERR
170400941130     C**
170500941130   X3C                   ELSE
170600941130     C**
170700941130     C* COD BOLLA NUOVO TROVATO
170800920131     C                   MOVEL     TBLUNI        DS3A
170900941130    4C     §3ABCM        IFNE      ' '
171000920324     C                   Z-ADD     0             §3AFCA
171100941130    4C                   END
171200920131     C*
171300920212     C* ANNULLAMENTO CONTRASSEGNO
171400941130    4C     SAVFCA        IFNE      0
171500920212     C     §3AFCA        ANDEQ     0
171600920212     C                   EXSR      CONCSB
171700920212     C*
171800941209    5C     *IN31         IFEQ      *OFF
171900941209     C* IMPOSTO IL NUOVO TIPO BOLLA
172000941209    6C     SAVFCA        IFEQ      2
172100941209     C     §3ATB2        ANDNE     *BLANKS
172200941209     C                   MOVEL     §3ATB2        CSBTBL
172300941209     C                   ELSE
172400941209     C                   MOVEL     §3ATB1        CSBTBL
172500941209    6C                   ENDIF
172600941209     C*
172700941209     C                   EXSR      ANNCSB
172800941209    5C                   END
172900941209    4C                   END
173000950202     C***************
173100920212     C* SE C/A PASSA A  "F"  A  "A"  O VICEVERSA POSSO FARLO ANCHE SE
173200920212     C*  INCASSATO - BASTA CHE NON SIA MODIFICATO L'IMPORTO
173300941130    4C     SAVFCA        IFEQ      1
173400920212     C     §3AFCA        ANDEQ     2
173500920212     C                   EXSR      CONCSB
173600950630     C* Se da bolla singola a bolla doppia e C/A sulla 2 bolla non
173700950630     C* aggiorno adesso TNCSB ma solo dopo scrittura della 2 bolla su
173800950630     C* TNTAS
173900950630    5C     SAVTB2        IFEQ      '  '
174000950630     C     §3ATB2        ANDNE     '  '
174100950630     C                   UNLOCK    TNCSB03L                             99
174200950630     C                   ELSE
174300941130     C* ANAGRAFICA C/A TROVATA
174400950630    6C     *IN31         IFEQ      *OFF
174500941130     C* CREO STORICO SE MODIFICATO IMPORTO
174600950630    7C     CSBCAS        IFNE      ARBCAS
174700991103    7C     WCSBVCA       ORNE      ARBVCA
174800920212     C                   MOVEL     'VACA'        CSVCAV
174900920212     C                   EXSR      CSVWTR
175000950630    7C                   END
175100920212     C*
175200941130     C                   Z-ADD     9999          CSBKSC
175300941130     C                   MOVEL     TASLNA        CSBKSC
175400950202     C                   MOVEL     §3ATB2        CSBTBL
175500950202     C*
175600950202     C                   EXSR      AGGCSB
175700941130     C                   UPDATE    TNCSB000
175800950630    6C                   END
175900950630    5C                   END
176000941130    4C                   END
176100950202     C***************
176200941130    4C     SAVFCA        IFEQ      2
176300920212     C     §3AFCA        ANDEQ     1
176400920228     C                   EXSR      CHTB2
176500920212     C*
176600920228     C                   EXSR      CONCSB
176700941130     C*
176800941130    5C     *IN31         IFEQ      *OFF
176900920212     C* CREO STORICO SE MODIFICATO IMPORTO
177000950202    6C     ARBCAS        IFNE      CSBCAS
177100991102    6C     ARBVCA        ORNE      WCSBVCA
177200920212     C                   MOVEL     'VACA'        CSVCAV
177300920212     C                   EXSR      CSVWTR
177400941130    6C                   END
177500920212     C*
177600941130     C                   MOVEL     CSBCDI        CSBKSC
177700941130     C                   MOVEL     §3ATB1        CSBTBL
177800950202     C                   EXSR      AGGCSB
177900941130     C                   UPDATE    TNCSB000
178000941130    5C                   END
178100941130    4C                   END
178200950202     C***************
178300950202     C* MODIFICATO IMPORTO C/ASSEGNO
178400941130    4C     SAVFCA        IFEQ      1
178500920227     C     §3AFCA        ANDEQ     1
178600941130     C* CONTROLLO IL C/A
178700920227     C                   EXSR      CONCSB
178800941130     C*
178900950202    5C  N31CSBCAS        IFNE      ARBCAS
179000991103    5C     WCSBVCA       ORNE      ARBVCA
179100920227     C                   MOVEL     'VACA'        CSVCAV
179200920227     C                   EXSR      CSVWTR
179300941209    5C                   END
179400950202     C*
179500950202     C                   EXSR      AGGCSB
179600950202     C*
179700941209     C* IMPOSTO IL NUOVO TIPO BOLLA
179800941209     C                   MOVEL     §3ATB1        CSBTBL
179900941209     C  N31              UPDATE    TNCSB000
180000941130    4C                   END
180100950202     C***************
180200941130    4C     SAVFCA        IFEQ      2
180300920227     C     §3AFCA        ANDEQ     2
180400920228     C                   EXSR      CHTB2
180500920227     C*
180600920227     C                   EXSR      CONCSB
180700941130     C*
180800950202    5C  N31CSBCAS        IFNE      ARBCAS
180900991103    5C     WCSBVCA       ORNE      ARBVCA
181000920227     C                   MOVEL     'VACA'        CSVCAV
181100920227     C                   EXSR      CSVWTR
181200941209    5C                   END
181300950202     C*
181400950202     C                   EXSR      AGGCSB
181500950202     C*
181600941209     C* IMPOSTO IL NUOVO TIPO BOLLA
181700941209     C                   MOVEL     §3ATB2        CSBTBL
181800941209     C  N31              UPDATE    TNCSB000
181900941130    4C                   END
182000920212     C***
182100920227     C*** A  G  G  I  O  R  N  O       B  O  L  L  A
182200920212     C***
182300080408     c  N31              if        VariataSede=' '
182400941130     C                   CLEAR                   DESPAR
182500970916     C                   CLEAR                   DESPRV
182600920228     C***
182700920228     C*** DA BOLLA SINGOLA A BOLLA SINGOLA
182800941130    5C     SAVTB2        IFEQ      '  '
182900920227     C     §3ATB2        ANDEQ     '  '
183000110315     c
183100110315     c* Non lo faccio se devo aggiungere solo il C/Ass perchè la bolla
183200110315     c*  non la trovo col cod bolla vecchio ma col cod bolla nuovo
183300110315    6c                   if        SoloCAss=' '
183400110315     c
183500941130     C                   MOVEL     'N'           VARFCA            1
183600920228     C                   EXSR      AGGTB1
183700941130     C*
183800941130     C* IMPOSTO IL PAESE DI ARRIVO
183900941130     C                   MOVEL     TASLOD        DESPAR
184000990625     C                   MOVEL     TASPRD        DESPRV
184100110315    6C                   END
184200110315    5C                   END
184300920228     C***
184400920228     C*** DA BOLLA DOPPIA A BOLLA DOPPIA
184500941130    5C     SAVTB2        IFNE      '  '
184600920227     C     §3ATB2        ANDNE     '  '
184700920228     C* AGGIORNO 1 BOLLA
184800941130     C                   MOVEL     'N'           VARFCA            1
184900920228     C                   EXSR      AGGTB1
185000941130     C* AGGANCIO 2 BOLLA
185100941130     C                   EXSR      CHTB2
185200941130     C* IMPOSTO IL PAESE DI ARRIVO
185300941130     C                   MOVEL     TASLOD        DESPAR
185400990625     C                   MOVEL     TASPRD        DESPRV
185500920227     C*
185600941130     C   02
185700941130    6CANN29              DO
185800920227     C*
185900941130    7C   38SAVFCA        IFEQ      2
186000920228     C     §3AFCA        OREQ      2
186100990505     c* Se il C/A si trova sulla 2 bolla, li imposto, altrimenti lo azzero
186200990505     c*  per il controllo della varia G
186300990505     c                   if        §3afca=2
186400990505     c                   eval      wcas=arbcas
186500990505     c                   else
186600990505     c                   clear                   wcas
186700990505     c                   endif
186800990505     c
186900920228     C                   EXSR      VARIAG
187000941130    7C                   END
187100920227     C*
187200920227     C                   MOVEL     §3ATB2        TASTBL
187300990629     C                   MOVEL     arbcbn        TASCBO
187400011218
187500011218      * riorganizzo le varie se è necessario
187600011218     c                   if        flgriorg = '1'
187700011218     c                   exsr      sr_riorg
187800011218     c                   endif
187900011218
188000920227     C*
188100941201     C* ALTRIMENTI CAMBIO IL TIPO BOLAA ANCHE NEL TA7
188200990625     C     KBLP          SETLL     TiTA7000
188300990625     C     KBLP          READE     TiTA7000                               30
188400941201    8C     *IN30         DOWEQ     *OFF
188500941201     C                   MOVEL     §3ATB2        TA7TBL
188600990625     C                   UPDATE    TITA7000
188700990625     C     KBLP          READE     TITA7000                               30
188800941201    8C                   ENDDO
188900941130    6C                   END
189000920227     C*
189100990625     C   02              UPDATE    TITAS000
189200990625     C   03              UPDATE    TITAS010
189300990625     C   04              UPDATE    TITASP00
189400920227     C*
189500920227     C* ERRORI
189600941130     C  N02
189700941130     COR 02
189800941130    6CAN 29              DO
189900941130    7C     SAVTB2        IFNE      §3ATB2
190000130410     c* solo se tipo bolla nuovo non è uno storno
190100130410     c                   movel     'TB'          cod
190200130410     c                   movel(p)  §3atb2        key
190300130410     c                   clear                   dstb
190400130410     C     KTAB          CHAIN     TABEL
190500130410    8c                   if        %found(tabel00f)
190600130410     c                   movel     tbluni        dstb
190700130410    8c                   endif
190800130410    8c                   if        §tbfcb='1'
190900130326     c                   z-add     46            comfev
191000930513     C                   MOVEL     ERR(16)       ARBDER                          MODIF BOLLA
191100130410    8c                   endif
191200941130   X7C                   ELSE
191300920228     C*
191400130326     c                   z-add     46            comfev
191500930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF A3
191600941130    7C                   END
191700920228     C*
191800920227     C                   EXSR      WTRERR
191900941130    6C                   END
192000941130    5C                   END
192100920228     C***
192200920228     C*** DA BOLLA DOPPIA A BOLLA SINGOLA
192300941130    5C     SAVTB2        IFNE      '  '
192400920227     C     §3ATB2        ANDEQ     '  '
192500950630     C*
192600920228     C* AGGANCIO 2 BOLLA
192700920228     C                   EXSR      CHTB2
192800941130     C* IMPOSTO IL PAESE DI ARRIVO
192900941130     C                   MOVEL     TASLOD        DESPAR
193000990625     C                   MOVEL     TASPRD        DESPRV
193100920227     C*
193200941130     C   02
193300941130    6CANN29              DO
193400920227     C                   MOVEL     'AP'          TASTBL
193500991021     c                   movel     arbcbn        tascbo
193600941130     C*
193700941130     C* TOLGO FLAG C/A SOLO SE DALLA 2 BOLLA E' PASSATO ALLA 1
193800941130     C     SAVFCA        IFEQ      2
193900941130     C     §3AFCA        ANDEQ     1
194000941130     C                   MOVEL     ' '           TASFCA
194100941130     C                   ENDIF
194200941130     C*
194300990625     C                   UPDATE    TITAS000
194400941130    6C                   END
194500920227     C*
194600920227     C* ERRORI
194700990505    6C   29              DO
194800130326     c                   z-add     46            comfev
194900930513     C                   MOVEL     ERR(20)       ARBDER                          MODIF BOLLA
195000920227     C                   EXSR      WTRERR
195100941130    6C                   END
195200920227     C**
195300920228     C** AGGIORNO 1 BOLLA
195400941130     C                   MOVEL     'S'           VARFCA
195500920228     C                   EXSR      AGGTB1
195600941130    5C                   END
195700920228     C***
195800920228     C*** DA BOLLA SINGOLA A BOLLA DOPPIA
195900941130    5C     SAVTB2        IFEQ      '  '
196000920227     C     §3ATB2        ANDNE     '  '
196100950630     C*
196200920228     C* AGGIORNO PRIMA BOLLA
196300941130     C                   MOVEL     'S'           VARFCA
196400920228     C                   EXSR      AGGTB1
196500920227     C*
196600000816     c* Prima di creare la seconda bolla cancello eventualmente bolla tipo
196700000816     c* "AP" che potrei avere in seguito ad un precedente cambio da bolla
196800000816     c* doppia a bolla singola
196900000816     c                   movel     'AP'          comtbl
197000000816     c                   setoff                                       020304
197100000816     c     kblp          chain     titas30c                           39
197200000816     c     *in39         ifeq      *off
197300000816     c     *in02         andeq     *on
197400000816     c     tasdft        ifeq      *zeros
197500000816     c     tasfic        oreq      ' '
197600000816     c                   delete    titas000
197700000816     c                   endif
197800000816     c                   endif
197900920227     C* CREO SECONDA BOLLA
198000920227     C*
198100950201     C* AGGANCIO FILE FNARBD1R
198200920227     C     ARBORV        ADD       1             COMORV
198300950201     C     KARBD         CHAIN     FNARBDRR                           32
198400920304     C  N32              ADD       1             NUMRBD
198500920227     C* 32 ON - INESISTENTE: NON SCRIVO DESTINATARIO E SEGNALO ERRORE
198600941130    6C   32              DO
198700920227     C                   MOVEL     *BLANKS       ARBRSD
198800950202     C                   MOVEL     *BLANKS       ARBRD2
198900920227     C                   MOVEL     *BLANKS       ARBIND
199000920227     C                   MOVEL     *BLANKS       ARBLOD
199100950202     C                   MOVEL     *BLANKS       ARBCAD
199200941130     C                   MOVEL     *BLANKS       ARBPRD
199300950202     C                   MOVEL     *BLANKS       ARBNZD
199400920227     C*
199500920227     C                   MOVEL     ERR(12)       ARBDER
199600920227     C                   Z-ADD     102           COMFEV
199700920227     C                   EXSR      WTRERR
199800941130    6C                   END
199900920227     C*
200000920227     C*
200100941130     C                   MOVEL     §3ATB2        TASTBL
200200990629     C                   MOVEL     ARBCBN        TASCBO
200300941130     C                   MOVE      TASKSC        KSC4
200400941130     C*
200500941130     C                   CLEAR                   TASRSD
200600941130     C                   CLEAR                   TASIND
200700990625     C                   CLEAR                   TASCAd
200800941130     C                   CLEAR                   TASLOD
200900990625     C                   CLEAR                   TASPRd
201000990625     C                   CLEAR                   TAScpd
201100941130     C*
201200941130     C* MUOVO L'INIDIRIZZO COMPLETO DEL DESTINATARIO
201300941130     C                   MOVEL     ARBRSD        TASRSD
201400941130     C                   MOVEL     ARBIND        TASIND
201500990625     C                   MOVEL     ARBCAD        TASCAD
201600941130     C                   MOVEL     ARBLOD        TASLOD
201700941130     C                   MOVEL     ARBLOD        DESPAR
201800970916     C                   MOVEL     ARBPRD        DESPRV
201900990625     C                   MOVEL     ARBPRD        TASPRD
202000990625     C                   MOVEL     ARBNZD        TASNZD
202100990701     c* Muovo comunque la p.iva in tascpd anche se in questo caso
202200990701     c* sarà sempre vuota perchè visto che prima della variazione
202300990701     c* la bolla era franco non poteva esserci in filiale la p.iva
202400990701     c* del destinatario memoziazzata su fnarb
202500990701     C                   MOVEL     ARBCPI        TASCPD
202600941130     C*
202700941130     C                   CLEAR                   TASPVL
202800941130     C                   CLEAR                   TASFTC
202900950202     C                   CLEAR                   TASTC2
203000941130     C                   CLEAR                   TASIAS
203100990824     C                   CLEAR                   TASFCM
203200950202     C                   CLEAR                   TASVAS
203300941130     C                   MOVEL     *ZEROS        TASCTR
203400930604     C                   MOVEL     *BLANKS       TASFTM
203500991018     C                   MOVEL     *BLANKS       TASDIV
203600920227     C                   MOVE      *ZERO         TASPOR
203700920227     C                   MOVE      *BLANK        TASSV1
203800920227     C                   MOVE      *ZERO         TASVA1
203900920227     C                   MOVE      *BLANK        TASSV2
204000920227     C                   MOVE      *ZERO         TASVA2
204100930212     C                   MOVE      *BLANK        TASSV3
204200930212     C                   MOVE      *ZERO         TASVA3
204300920227     C                   Z-ADD     0             TASDFT
204400920227     C                   Z-ADD     0             TASNFT
204500920227     C                   Z-ADD     0             TASFIV
204600930212     C                   MOVE      ' '           TASFA2
204700920227     C                   Z-ADD     9999          TASKSC
204800920227     C                   MOVEL     TASLNA        TASKSC
204900920227     C* CONTRASSEGNO A SECONDA DEL FLAG
205000941130    6C     §3AFCA        IFEQ      2
205100941130     C                   MOVEL     'S'           TASFCA
205200920227     C                   MOVEL     §3ATB2        TBLCA             2
205300920227     C                   ELSE
205400941130     C                   MOVEL     ' '           TASFCA
205500941130    6C                   END
205600920227     C*
205700990624     C                   MOVE      *zeros        TASimv
205800941130     C                   MOVEL     ' '           TASFEI
205900941130     C                   MOVEL     ' '           TASFTS
206000941130     C                   MOVEL     ' '           TASFIC
206100920227     C*
206200990625     C                   WRITE     TITAS000                             99
206300950630     C*
206400950630     C     SAVFCA        IFEQ      1
206500950630     C     §3AFCA        ANDEQ     2
206600950630     C     KANN          CHAIN     TNCSB000                           31
206700991103     c                   move      csbvca        wcsbvca
206800991103     c     csbcas        ifgt      0
206900991103     c     wcsbvca       andeq     *blanks
207000991103     c                   movel     'ITL'         wcsbvca
207100991103     c                   endif
207200950630     C     CSBCAS        IFNE      ARBCAS
207300991103     C     wcsbvca       ORNE      ARBVCA
207400950630     C                   MOVEL     'VACA'        CSVCAV
207500950630     C                   EXSR      CSVWTR
207600950630     C                   END
207700950630     C*
207800950630     C                   Z-ADD     9999          CSBKSC
207900950630     C                   MOVEL     TASLNA        CSBKSC
208000950630     C                   MOVEL     §3ATB2        CSBTBL
208100950630     C*
208200950630     C                   EXSR      AGGCSB
208300950630     C                   UPDATE    TNCSB000
208400950630     C                   END
208500920227     C*
208600941130     C* REIMPOSTO ORA DI VARIAZIONE DI ARBK: ORA DI ARBD - 1
208700920227     C  N32              SUB       1             ARBORV
208800950201     C  N32              DELETE    FNARBDRR
208900941130    5C                   END
209000920227     C*
209100920227     C* CREO SE NECESSARIO RECORD IN FILE CONTRASSEGNI
209200941130    5C     SAVFCA        IFEQ      0
209300941130    6C     §3AFCA        IFEQ      1
209400920227     C                   MOVEL     §3ATB1        TBLCA
209500941130     C* RICHAINO LA BOLLA
209600941130     C                   MOVEL     §3ATB1        COMTBL
209700941130     C                   SETOFF                                       020304
209800990625     C     KBLP          CHAIN     TITAS30C                           30
209900110315     c* Se solo C/Ass--> aggiungo il flag
210000110315     c                   if        SoloCAss='S'
210100110315     c                   eval      TASFCA='S'
210200110315     C                   MOVEL     TASLOD        DESPAR
210300110315     C                   MOVEL     TASPRD        DESPRV
210400110315     C   02              UPDATE    TITAS000
210500110315     C   03              UPDATE    TITAS010
210600110315     C   04              UPDATE    TITASP00
210700110315     c                   endif
210800950913     C*
210900920227     C                   EXSR      CRTCSB
211000941130     C*
211100920227     C                   ELSE
211200941130     C*
211300941130    7C     §3AFCA        IFEQ      2
211400920227     C                   MOVEL     §3ATB2        TBLCA
211500941130     C* RICHAINO LA BOLLA
211600941130     C                   MOVEL     §3ATB2        COMTBL
211700941130     C                   SETOFF                                       020304
211800990625     C     KBLP          CHAIN     TITAS30C                           30
211900920227     C                   EXSR      CRTCSB
212000941130    7C                   END
212100941130    6C                   END
212200941130    5C                   END
212300920227     C*
212400941130    4C                   END
212500941130    3C                   END
212600941130    2C                   END
212700930513     C*
212800930513     C* 11 ON - RECORD DESTINATARIO PER CREARE 2 BOLLA
212900941130   X1C                   ELSE
213000950202     C* NON CANCELLO IL RECORD : IN ARBM LO LASCIO LI'
213100950202     C                   SETON                                        37
213200950202     C*
213300950202     C     *IN11         IFEQ      *ON
213400930513     C                   MOVEL     102           COMFEV
213500930513     C                   MOVEL     ERR(15)       ARBDER
213600930513     C                   EXSR      WTRERR
213700950202     C                   ENDIF
213800950202    1C                   ENDIF
213900920227     C*
214000920227     C                   ENDSR
214100060317     C**---------------------------------------------------------
214200060317     C** AGGIORNO CODICE VARIAZIONE RM
214300060317     C**---------------------------------------------------------
214400060317     C     AGARBR        BEGSR
214500060317     c* rma in tita4 record "A"
214600060317     c                   setoff                                       0506
214700060317     C                   movel     'A'           §trc
214800060317     C     ktita         chain     tita430c                           32
214900060317    3C     *in32         ifeq      *off
215000060317     C                   movel     ta4not        dta4a
215100060317     c                   movel     arbrsd        §TA4ARMA
215200060317     C                   movel     dta4a         ta4not
215300060317     C   05              update    tita4000
215400060317     C   06              update    tita4p00
215500060317     c                   endif
215600060317     c*
215700060317     C                   MOVEL     ARBrd2        TASnas
215800060317     C   02              UPDATE    TiTAS000
215900060317     C   03              UPDATE    TiTAS010
216000060317     C   04              UPDATE    TiTASP00
216100060317     c*
216200060317     C                   ENDSR
216300920228     C**---------------------------------------------------------
216400920228     C** AGGIORNO PRIMA BOLLA
216500920228     C**---------------------------------------------------------
216600920228     C     AGGTB1        BEGSR
216700920228     C* AGGANCIO 1 BOLLA
216800920228     C                   EXSR      CHTB1
216900920228     C*
217000941201     C* SE E' DA AGGIUNGERE LO AGGIUNGO
217100920228     C     §3AFCA        IFEQ      1
217200941130     C                   MOVEL     'S'           TASFCA
217300941201     C*
217400920228     C                   ELSE
217500941201     C* LO TOLGO SOLO SE FALG IMPOSTATO = "S"
217600941201     C*
217700941201     C     VARFCA        IFEQ      'S'
217800941130     C                   MOVEL     ' '           TASFCA
217900920228     C                   END
218000941130     C                   END
218100920228     C*
218200941130     C   02
218300941130     CANN29              DO
218400920228     C   38SAVFCA        IFEQ      1
218500920228     C     §3AFCA        OREQ      1
218600990505     c* Devo impostare l'importo C/A nella routine varia G :
218700990505     c*  se il C/A e' stato tolto dalla 1 bolla, richiamo con importo=0
218800990505     c                   if        §3afca=1
218900990505     c                   eval      wcas=arbcas
219000990505     c                   else
219100990505     c                   clear                   wcas
219200990505     c                   endif
219300990505     c
219400920228     C                   EXSR      VARIAG
219500920228     C                   END
219600920228     C*
219700920228     C                   MOVEL     §3ATB1        TASTBL
219800990629     C                   MOVEL     ARBCBN        TASCBO
219900011219
220000011219      * riorganizzo le varie se è necessario
220100011219     c                   if        flgriorg = '1'
220200011219     c                   exsr      sr_riorg
220300011219     c                   endif
220400011219
220500000209     C* AGGIORNO TITA7000 SE ESISTE
220600000209     C     KBLP          SETLL     TITA7000
220700000209     C     KBLP          READE     TITA7000                               30
220800000209     C*
220900000209     C     *IN30         DOWEQ     *OFF
221000000209     C                   MOVEL     §3ATB1        TA7TBL
221100000209     C                   UPDATE    TITA7000
221200000209     C     KBLP          READE     TITA7000                               30
221300000209     C                   ENDDO
221400920228     C                   END
221500941130     C*
221600920228     C*
221700990625     C   02              UPDATE    TITAS000
221800990625     C   03              UPDATE    TITAS010
221900990625     C   04              UPDATE    TITASP00
222000920228     C*
222100990505     C* spedizione gia' contabilizzata
222200990505     C   29              DO
222300920228     C*
222400920312     C                   MOVEL     *BLANKS       ARBDER
222500920228     C     §3ATB1        IFNE      SAVTB1
222600130410     c* solo se tipo bolla nuovo non è uno storno
222700130410     c                   movel     'TB'          cod
222800130410     c                   movel(p)  §3atb1        key
222900130410     c                   clear                   dstb
223000130410     C     KTAB          CHAIN     TABEL
223100130410    3c                   if        %found(tabel00f)
223200130410     c                   movel     tbluni        dstb
223300130410    3c                   endif
223400130410    3c                   if        §tbfcb='1'
223500130326     c                   z-add     46            comfev
223600930513     C                   MOVEL     ERR(16)       ARBDER                         MODIF BOLLA
223700130410    3c                   endif
223800920228     C                   ELSE
223900920228     C*
224000920228     C     SAVFCA        IFNE      1
224100920228     C     §3AFCA        ANDEQ     1
224200130325     C***                MOVEL     ERR(18)       ARBDER                         AGGIUN VARIA
224300130325     c***                setoff                                       36
224400120423     c                   exsr      ctr_vrb
224500920228     C                   END
224600920228     C*
224700920228     C     SAVFCA        IFEQ      1
224800920228     C     §3AFCA        IFNE      1
224900941130     C*
225000941130     C* CONTROLLO SE ESISTE VARIA "G"
225100130326     C***                EXSR      ESVARG
225200130325     C***  WSIVAR        IFEQ      'S'
225300130325     C***                MOVEL     ERR(14)       ARBDER                         TOLTA VARIA
225400130325     c***                setoff                                       36
225500130325     C***                END
225600121205     c                   eval      wanncas='S'
225700121203     c                   exsr      ctr_vrb
225800941130     C*
225900920228     C                   ELSE
226000130325     C****               MOVEL     ERR(22)       ARBDER                         MODIF VARIA
226100130325     c****               setoff                                       36
226200120423     c                   exsr      ctr_vrb
226300920228     C                   END
226400920228     C                   END
226500920228     C                   END
226600920228     C*
226700920312     C     ARBDER        IFNE      *BLANKS
226800920228     C                   EXSR      WTRERR
226900920228     C                   END
227000920312     C                   END
227100920228     C                   ENDSR
227200920227     C**---------------------------------------------------------
227300920227     C** CALCOLO O ANNULLO VARIA  G
227400120322     C**---------------------------------------------------------
227500920227     C     VARIAG        BEGSR
227600920228     C                   MOVE      TASKSC        KSC4
227700990505     c                   clear                   ARBDER
227800011218
227900011218     c                   clear                   flgriorg
228000941130     C*
228100990414     c* Se data fattura a zero e cliente codificato vado a togliere la varia G
228200990505     C     TASDFT        IFEQ      *ZEROS
228300990414    1C     KSC4          ANDNE     9999
228400920303     C     KSC4          ANDNE     8888
228500920228     C*
228600990505     c* C/A tolto
228700990505     c                   if        wcas=0
228800941130     C                   SELECT
228900941130     C     TASSV1        WHENEQ    'G'
229000011218     c                   eval      flgriorg = '1'
229100991019     c     tasimv        ifeq      tasva1
229200991019     c                   clear                   tasdiv
229300991019     c                   endif
229400920227     C                   MOVEL     ' '           TASSV1
229500920227     C                   Z-ADD     0             TASVA1
229600990624     C                   MOVE      *zeros        TASimv
229700941130     C     TASSV2        WHENEQ    'G'
229800011218     c                   eval      flgriorg = '1'
229900991019     c     tasimv        ifeq      tasva2
230000991019     c                   clear                   tasdiv
230100991019     c                   endif
230200920227     C                   MOVEL     ' '           TASSV2
230300920227     C                   Z-ADD     0             TASVA2
230400990624     C                   MOVE      *zeros        TASimv
230500941130     C     TASSV3        WHENEQ    'G'
230600011218     c                   eval      flgriorg = '1'
230700991019     c     tasimv        ifeq      tasva3
230800991019     c                   clear                   tasdiv
230900991019     c                   endif
231000930212     C                   MOVEL     ' '           TASSV3
231100930212     C                   Z-ADD     0             TASVA3
231200990624     C                   MOVE      *zeros        TASimv
231300941130     C*
231400941130     C                   OTHER
231500941130     C*
231600941130     C                   MOVEL     'G'           WSV1
231700990625     C     KBLPV         DELETE    TITA7000                           30
231800990624     C  n30              MOVE      *zeros        TASimv
231900941130     C                   ENDSL
232000990419     c                   else
232100990419     c* C/A aggiunto o midificato
232200990419     c                   EXSR      ESVARG
232300990419     c* Se la varia non c'e' tolto totale imponibile cosi' la calcola
232400990419     c                   if        wsivar=' '
232500990624     c                   clear                   TASimv
232600990419     c                   else
232700990419     c* Se gia' esiste stampo errore --> no modificata varia G
232800130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
232900130326     C***                EXSR      WTRERR
233000130326     c***                eval      wcntab=' '
233100130326     c***                exsr      ctr_vrb
233200990419     c                   endif
233300990419     c                   endif
233400920228     C*
233500941130   X1C                   ELSE
233600990414     C* CLIENTE NON CODIFICATO O CON DATA FATTURA VALORIZZATA
233700991019     C* COMUNQUE SE ANNULLATO C/A ELIMINO VARIA G  E ADEGUO TOT.IMPONIBILE
233800990414     C* Se c'è importo del contrassegno e la data fattura è uguale a zero posso
233900990414     c* togliere la varia "G"
234000990505    2C     WCAS          IFEQ      0
234100990505     C     TASDFT        ANDEQ     0
234200990414     c*
234300941130    3C                   SELECT
234400941130     C     TASSV1        WHENEQ    'G'
234500011218     c                   eval      flgriorg = '1'
234600991019     c                   sub       tasva1        tasimv
234700991019     c     tasimv        ifeq      0
234800991019     c                   clear                   tasdiv
234900991019     c                   endif
235000920228     C                   MOVEL     ' '           TASSV1
235100920228     C                   Z-ADD     0             TASVA1
235200941130     C     TASSV2        WHENEQ    'G'
235300011218     c                   eval      flgriorg = '1'
235400991019     c                   sub       tasva2        tasimv
235500991019     c     tasimv        ifeq      0
235600991019     c                   clear                   tasdiv
235700991019     c                   endif
235800920228     C                   MOVEL     ' '           TASSV2
235900920228     C                   Z-ADD     0             TASVA2
236000941130     C     TASSV3        WHENEQ    'G'
236100011218     c                   eval      flgriorg = '1'
236200991019     c                   sub       tasva3        tasimv
236300991019     c     tasimv        ifeq      0
236400991019     c                   clear                   tasdiv
236500991019     c                   endif
236600930212     C                   MOVEL     ' '           TASSV3
236700930212     C                   Z-ADD     0             TASVA3
236800941130     C                   OTHER
236900941130     C                   MOVEL     'G'           WSV1
237000991019     C     KBLPV         chain     TITA7000                           30
237100991019     c  n30              sub       ta7van        tasimv
237200991019     c  n30              delete    tita7000
237300941130    3C                   ENDSL
237400920228     C*
237500941130   X2C                   ELSE
237600990414     c*
237700990414     c* Se è già stato fatturato oppure arbcas > 0
237800920228     C*
237900941130     C* VEDO SE LA VARIA "G" C'ERA
238000130326     C*****              EXSR      ESVARG
238100941130     C*
238200991019     c* se c'è  l'importo del contrassegno per non codificati
238300991019     c*                                      o data fattura maggiore di zero
238400130326    3c***  wcas          ifgt      *zeros
238500990414     c*
238600130326    4C***  WSIVAR        IFEQ      ' '
238700130326     C***                MOVEL     ERR(24)       ARBDER                         AGGIUN VARIA
238800130326     C***                EXSR      WTRERR
238900130326     c***                eval      wcntab=' '
239000130326     c***                exsr      ctr_vrb
239100130326     C***                ELSE
239200130326     C***                MOVEL     ERR(25)       ARBDER                          MODIF VARIA
239300130326     C***                EXSR      WTRERR
239400130326     c***                eval      wcntab=' '
239500130326     c***                exsr      ctr_vrb
239600130326    4C***                END
239700130326    3c****               endif
239800990505     c*
239900990414     c* se l'importo è uguale a zero e prima c'era la varia "G"
240000130326    3c***  wcas          ifeq      *zeros
240100130326     c***  wsivar        andne     ' '
240200130326     C***                MOVEL     ERR(23)       ARBDER                          no tolta varia
240300130326     C***                EXSR      WTRERR
240400130326    3C***                END
240500941130    2C                   END
240600941130    1C                   END
240700920227     C*
240800920227     C                   ENDSR
240900011218      *----------------------------------------------------------
241000011218      * Riorganizzo le varie sul titas
241100011218      *----------------------------------------------------------
241200011218     c     sr_riorg      begsr
241300011218
241400011218     c                   clear                   sksv
241500011218     c                   clear                   skva
241600011218     c                   eval      xx = 1
241700011218
241800011219      * carico la sk delle varie dal titas
241900011218     c                   if        tassv1 <> *blanks
242000011218     c                   eval      sksv(xx) = tassv1
242100011218     c                   eval      skva(xx) = tasva1
242200020107     c                   clear                   tassv1
242300020107     c                   clear                   tasva1
242400011218     c                   add       1             xx
242500011218     c                   endif
242600011218     c                   if        tassv2 <> *blanks
242700011218     c                   eval      sksv(xx) = tassv2
242800011218     c                   eval      skva(xx) = tasva2
242900020107     c                   clear                   tassv2
243000020107     c                   clear                   tasva2
243100011218     c                   add       1             xx
243200011218     c                   endif
243300011218     c                   if        tassv3 <> *blanks
243400011218     c                   eval      sksv(xx) = tassv3
243500011218     c                   eval      skva(xx) = tasva3
243600020107     c                   clear                   tassv3
243700020107     c                   clear                   tasva3
243800011218     c                   add       1             xx
243900011218     c                   endif
244000011218
244100011219      * carico la sk delle varie dal tita7
244200011219     c     kblp          setll     tita7000
244300011218     c                   do        *hival
244400011219     c     kblp          reade     tita7000
244500011219     c                   if        %eof
244600011218     c                   leave
244700011219     c                   endif
244800011218     c                   eval      skva(xx) = ta7van
244900011218     c                   eval      sksv(xx) = ta7svn
245000011218     c                   add       1             xx
245100011219     c                   delete    tita7000
245200011218     c                   enddo
245300011219
245400011219      * scrivo le varie su titas
245500011219     c                   if        sksv(1) <> *blanks
245600011219     c                   eval      tassv1 = sksv(1)
245700011219     c                   eval      tasva1 = skva(1)
245800011219     c                   endif
245900011219     c                   if        sksv(2) <> *blanks
246000011219     c                   eval      tassv2 = sksv(2)
246100011219     c                   eval      tasva2 = skva(2)
246200011219     c                   endif
246300011219     c                   if        sksv(3) <> *blanks
246400011219     c                   eval      tassv3 = sksv(3)
246500011219     c                   eval      tasva3 = skva(3)
246600011219     c                   endif
246700011219
246800011219      * scrivo le varie su tita7
246900011219     c     4             do        20            xx
247000011219     c                   if        sksv(xx) <> *blanks
247100011219     c                   eval      ta7aas = arbaas
247200011219     c                   eval      ta7lnp = arblnp
247300011219     c                   eval      ta7nrs = arbnrs
247400011219     c                   eval      ta7nsp = arbnsp
247500011219     c                   eval      ta7tbl = comtbl
247600011219     c                   eval      ta7svn = sksv(xx)
247700011219     c                   eval      ta7van = skva(xx)
247800011219     c                   write     tita7000
247900011219     c                   endif
248000011219     c                   enddo
248100011218
248200011218     c                   endsr
248300011218
248400920210     C**---------------------------------------------------------
248500920210     C** CREO RECORD CONTRASSEGNO
248600920210     C**---------------------------------------------------------
248700920210     C     CRTCSB        BEGSR
248800941130     C                   MOVEL     ARBDER        SAVDER
248900920303     C*
249000920303     C* SE ESISTE ANNULLATO LO DELETO E RISCRIVO
249100941130     C     KANN          CHAIN     TNCSB000                           41
249200941130    1C     *IN41         IFEQ      *ON
249300941130     C                   CLEAR                   TNCSB000
249400941130     C                   ELSE
249500941130     C     KANN          CHAIN     TNCSA000                           39
249600941130     C  N39              DELETE    TNCSA000
249700941130    1C                   ENDIF
249800920303     C*
249900950201     C* AGGANCIO FILE FNARBM0R
250000950202     C     KARBM         CHAIN     FNARBMRR                           35
250100920304     C  N35              ADD       1             NUMRBM            7 0
250200920212     C* INESISTENTE: STAMPO MA CREO UGUALMENTE ARCHIVIO (DA COMPLETARE)
250300941130    1C   35              DO
250400920212     C                   MOVEL     ERR(11)       ARBDER
250500920212     C                   Z-ADD     102           COMFEV
250600920212     C                   EXSR      WTRERR
250700920212     C* AZZERO CAMPI MITTENTE
250800920212     C                   MOVEL     *BLANKS       ARBRSM
250900920212     C                   MOVEL     *BLANKS       ARBINM
251000920212     C                   MOVEL     *BLANKS       ARBCAM
251100920212     C                   MOVEL     *BLANKS       ARBLOM
251200941130     C                   MOVEL     *BLANKS       ARBPRD
251300920212     C                   MOVEL     *ZEROS        ARBCCM
251400941130    1C                   END
251500920212     C*
251600920210     C                   MOVEL     TASLNP        CSALNP
251700920210     C                   MOVEL     TASNRS        CSANRS
251800920210     C                   MOVEL     TASAAS        CSAAAS
251900920210     C                   MOVEL     TASNSP        CSANSP
252000920210     C*
252100920210     C                   MOVEL     TBLCA         CSBTBL
252200920210     C                   Z-ADD     TASLNP        CSBLNP
252300920210     C                   Z-ADD     TASAAS        CSBAAS
252400920210     C                   Z-ADD     TASNRS        CSBNRS
252500920210     C                   Z-ADD     TASNSP        CSBNSP
252600941130     C                   Z-ADD     TASMGS        CSBMGS
252700990629     C                   Z-ADD     TASTFP        CSBFLE
252800941130     C                   MOVEL     DESPAR        CSBPAR                         PAESE ARRIVO
252900970916     C                   MOVEL     DESPRV        CSBPRO                         PROV.DEST.
253000030310     C                   MOVE      *ZERO         CSBSTA
253100950202     C                   EXSR      AGGCSB
253200920304     C*
253300941130     C                   MOVEL     TBLCA         FLGTB1
253400920304     C*
253500920304     C* SE BLANK E CODIFICATO DESTINATARIO CHIAN
253600941130    2C     TASRSD        IFEQ      *BLANKS
253700990625     c* N.B.la call al tibs69 sarà da togliere quando tutte le bolla avranno
253800990625     c* il destinatario pieno
253900990625     c     flgtb1        ifeq      'A'
254000981001     C                   CLEAR                   DSBS69
254100981001     C                   MOVEL     KCI           I69KCC
254200981001     C                   MOVEL     KNSIF         I69SIF
254300981001     C                   MOVEL     TASKSC        I69KAC
254400981001     C                   CALL      'TIBS69R'
254500981001     C                   PARM                    DSBS69
254600981001     C                   PARM                    DSACO
254700981001     C                   PARM                    DSIND
254800981001     C                   PARM                    DSCLP
254900981001     C                   PARM                    DSCLS
255000981001     C**
255100981001     C     O69ERR        IFEQ      ' '
255200981001     C                   MOVEL     ACORAG        CSBRSD
255300981001     C                   ELSE
255400981001     C                   MOVEL     *BLANKS       CSBRSD
255500981001     C                   ENDIF
255600990625     C                   ENDIF
255700981001     C                   ELSE
255800981001     C                   MOVEL     TASRSD        CSBRSD
255900941130    2C                   END
256000920304     C*
256100920304     C*
256200920210     C                   Z-ADD     TASRMN        CSBRMN
256300990625     c* prendo il riferimento mittente alfabetico da estensione (tita4)
256400990625     c                   movel     'A'           §trc
256500010126     c     ktita         chain(N)  tita430c                           39
256600990625     c  n39              movel     ta4not        csbrma
256700990625     c   39              clear                   csbrma
256800941207     C                   Z-ADD     TASLNA        CSBLNA
256900040721     C**                   MOVEL     TASFIN        CSBFIN
257000920210     C                   Z-ADD     TASKSC        CSBKSC
257100920210     C                   MOVE      CSBKSC        CSBCDI
257200931108     C   41              MOVEL     'R'           CSBFTR
257300931108     C   41              MOVE      DATEU         CSBDTR
257400920303     C*
257500941130    1C     FLGTB1        IFEQ      'A'
257600941130     C                   Z-ADD     99999         CSBKSC
257700941130     C                   MOVEL     TASLNA        CSBKSC
257800941206     C                   MOVE      CSBKSC        CSBCDI                         WB/LB
257900941206    1C                   END                                                    WB/LB
258000941130     C*
258100941130     C                   MOVEL     TASCCM        CSBCDI
258200941206    1C*************        END                             WB/LB
258300941130     C*
258400920210     C** RAGGRUPPAMENTO C/ASSEGNI
258500920210     C     TASLNA        CHAIN     AZORG                              51
258600920210     C  N51              Z-ADD     ORGFC5        CSBRGF                         FIL RAGGR.C/A
258700920210     C   51              Z-ADD     TASLNA        CSBRGF
258800941130     C*
258900941130     C* VEDO SE CSBCDI E' INTESTAZIONE GENERICA
259000941130     C                   MOVE      CSBCDI        W0040             4 0
259100920210     C*
259200920210     C** DATI ANAGRAFICI PER CODICI GENERICI E ASSEGNATI
259300941130     C     W0040         IFEQ      8888
259400941130     C     W0040         OREQ      9999
259500920212     C                   MOVEL     ARBRSM        CSARSD                         MITTENTE
259600920212     C                   MOVEL     ARBINM        CSAVID                            '
259700920212     C                   MOVEL     ARBCAM        CSACAD                            '
259800920212     C                   MOVEL     ARBLOM        CSACID                            '
259900970916     C                   MOVEL     ARBPRM        CSAPRM
260000970916     C                   MOVEL     ARBNZM        CSANZM
260100920212     C*
260200941201     C                   WRITE     TNCSA000                             99
260300920212     C                   END
260400920212     C**
260500941201     C   41              WRITE     TNCSB000                             99
260600941130     C  N41              UPDATE    TNCSB000
260700950914     C*
260800950914     C* CREO RECORD DI INSERIMENTO C/ASSEGNO NELLE VARIAZIONI
260900950914     C                   MOVEL     'IMCA'        CSVCAV
261000950914     C                   EXSR      CSVWTR
261100941130     C*
261200941130     C** SE ERA IMPOSTATO LO RIMETTO COME ERA PRIMA
261300941130     C                   MOVEL     SAVDER        ARBDER
261400920210     C                   ENDSR
261500920210     C**---------------------------------------------------------
261600920210     C** CONTROLLO ESISTENZA ARCHIVIO CONTRASSEGNI
261700920210     C**---------------------------------------------------------
261800920210     C     CONCSB        BEGSR
261900930513     C                   SETOFF                                           42    25
262000920212     C* VARIO  CONTRASSEGNO SOLO SE NON PAGATO-INCASSATO
262100941130     C     KANN          CHAIN     TNCSB000                           31
262200930303     C*
262300941130     C   31KANN          SETLL     FNLBL01L                               42
262400920212     C*
262500930513     C* PER CASUALE VARIAZIONE "K" SEMPRE ERRORE
262600930303     C   31FLGCVB        IFEQ      'K'
262700930513     C                   SETON                                        25
262800930303     C                   MOVEL     ERR(5)        ARBDER
262900930303     C                   Z-ADD     102           COMFEV
263000930303     C                   EXSR      WTRERR
263100930513     C*
263200930303     C                   ELSE
263300930513     C*
263400930513     C* ALTRIMENTI SOLO SE NON C'E' IL LEGAME BOLLA
263500930513     C     *IN42         IFEQ      '0'
263600930513     C                   SETON                                        25
263700930513     C                   MOVEL     ERR(5)        ARBDER
263800930513     C                   Z-ADD     102           COMFEV
263900930513     C                   EXSR      WTRERR
264000930303     C                   END
264100930513     C                   END
264200991103     c*
264300991103     c     *in31         ifeq      *off
264400991103     c                   movel     csbvca        wcsbvca
264500991103     c     csbcas        ifgt      0
264600991103     c     csbvca        andeq     *blanks
264700991103     c                   movel     'ITL'         wcsbvca
264800991103     c                   endif
264900991103     c                   endif
265000930513     C*
265100920302     C* GIA' INCASSATO O PAGATO
265200950202    1C  N31CSBDDC        IFNE      0
265300920210     C     CSBBNA        ORNE      0
265400920212     C*
265500950324     C* SE NON MODIFICATO L'IMPORTO DEL C/A OK, SE VARIAZIONE "K"
265600950324     C     FLGCVB        IFEQ      'K'
265700950202    3C     ARBCAS        IFNE      CSBCAS
265800991102     C     ARBVCA        ORNE      WCSBVCA
265900920212     C                   SETON                                        31
266000110214     C                   Z-ADD     102           COMFEV
266100950202    2C                   END
266200110214     c
266300950324     C* ALTRIMENTI SEMPRE ERRORE
266400950324     C                   ELSE
266500950324     C                   SETON                                        31
266600110214     C                   Z-ADD     102           COMFEV
266700950324    2C                   END
266800920601     C*
266900950202    2C   31FLGCVB        IFNE      'I'
267000950222    2C     FLGCVB        ANDNE     'M'
267100920601     C                   SETON                                        36
267200920601     C                   MOVEL     ERR(10)       ARBDER
267300920601     C                   EXSR      WTRERR
267400950324     C                   ELSE
267500950324     C                   CLEAR                   COMFEV
267600950202    2C                   END
267700950202    1C                   END
267800920210     C                   ENDSR
267900950202     C*
268000950202     C**---------------------------------------------------------
268100950202     C** AGGIORNO RECORD CONTRASSEGNO
268200950202     C**---------------------------------------------------------
268300950202     C     AGGCSB        BEGSR
268400950202     C                   Z-ADD     ARBCAS        CSBCAS
268500950202     C                   MOVEL     ARBVCA        CSBVCA
268600950202     C                   MOVEL     ARBGCA        CSBGCA
268700950203     C                   Z-ADD     ARBCMB        CSBCMB
268800030310     c                   if        csbsta <> 3
268900000703     c                   movel     0             csbsta
269000030310     c                   end
269100950202     C* TIPO ASSEGNO: MITTENTE / BARTOLINI
269200950202     C                   MOVEL     '1A'          COD
269300950202     C                   MOVEL(P)  ARBTIC        KEY
269400950202     C     KTAB          CHAIN     TABEL                              30
269500950202     C  N30              MOVEL     TBLUNI        DS1A
269600950202     C  N30              MOVEL     §1AFMB        CSBFUS
269700950202     C   30              CLEAR                   CSBFUS
269800950202     C                   ENDSR
269900920212     C**---------------------------------------------------------
270000920212     C** ANNULLO RECORD CONTRASSEGNO
270100920212     C**---------------------------------------------------------
270200920212     C     ANNCSB        BEGSR
270300941209     C* CREO STORICO VARIAZIONE
270400920602     C     CSBSTA        IFNE      9
270500920212     C                   MOVEL     'ANCA'        CSVCAV
270600920212     C                   EXSR      CSVWTR
270700920212     C* ANNULLO
270800920212     C                   MOVEL     9             CSBSTA
270900941130     C                   UPDATE    TNCSB000
271000920212     C                   END
271100920212     C                   ENDSR
271200920212     C**---------------------------------------------------------
271300920212     C** ESEGUE WRITE BLCSV00F                                  -
271400920212     C**---------------------------------------------------------
271500920212     C     CSVWTR        BEGSR
271600950202     C* COME DATA E ORA VARIAZIONE METTO QUELLA DELLA FILIALE
271700941130     C                   Z-ADD     ARBDTV        CSVDTV
271800941130     C                   Z-ADD     ARBORV        CSVORV
271900020604     c*
272000021112     c                   clear                   tibs36ds
272100021112     c                   movel     arbpru        i36ute
272200140407     c                   eval      %subst(i36ute:10:1)=' '
272300021112     C                   CALL      'TIBS36R'
272400021112     C                   PARM                    tibs36ds
272500020604     c* non tovato profilo su azute
272600070420   1 c     o36err        ifne      *blanks
272700020604     c* richiamo fnlv55r per sapere se lna è gestita da altro p.o. e da chi
272800020604     c                   clear                   dslv55
272900020604     c                   movel     '6'           d55tpt
273000020604     c                   z-add     taslna        d55lin
273100020604     c                   z-add     csvdtv        d55drf
273200020604     c                   call      'FNLV55R'
273300020604     c                   parm                    dslv55
273400020604     C                   MOVEL     d55tfa        CSVFEV
273500070420   x1c                   else
273600020604     c* trovato profilo su azute
273700021112     c                   movel     o36pou        csvfev
273800070420    1c                   endif
273900020604     c*
274000920212     C                   MOVEL     CSBDS         CSVDS
274100941130     C* COME DATA TRSMISSIONE QUANDO HO CARICATO
274200941130     C                   Z-ADD     DATEU         CSVDTR
274300941130     C                   WRITE     TNCSV000
274400920212     C                   ENDSR
274500920130     C**---------------------------------------------------------
274600920130     C** CONTROLLO ESISTENZA BOLLA
274700920130     C**---------------------------------------------------------
274800990628     C     CTRSPE        BEGSR
274900920302     C                   SETOFF                                       332940
275000941130     C                   SETOFF                                       4217
275100941121     C                   SETOFF                                       020304
275200930514     C                   SETON                                        23
275300080408     c                   clear                   VariataSede       1
275400110315     c                   clear                   SoloCASS          1
275500941121     C*
275600991015     C     KBLP          CHAIN     TITAS30C                           29
275700920220     C*
275800941121     C* 29 ON  - S P E D I Z I O N E   I N E S I S T E N T E
275900941121    1C     *IN29         IFEQ      *ON
276000941121     C*
276100941121     C     KBLP          CHAIN     TNBLA000                           40
276200941121     C   40KANN          CHAIN     TNBLA000                           40
276300920602     C*
276400930804     C* 40 OFF - S P E D I Z I O N E   A N N U L L A T A
276500991015     C  N40              MOVEL     ERR(28)       ARBDER
276600070119     c
276700070119     c* Verifico se lnp estera, se record bolla chiuso in partenza
276800070119     c*  come merce mai affidata
276900070119    2c                   if        *in40
277000070119     c
277100070119     c     ArbLnp        Chain     Azorg01l
277200070119    3c                   If        %Found(Azorg01l)
277300070119     c                   Eval      Og143 = OrgDe3
277400070119    4c                   If        §OgNtw = 'DPD' or §ogntw='EEX' or
277500070119     c                             §ogntw='FED'
277600070119     c* controllo bolla se presente in fnblp
277700070119     c     kann          chain     fnblp01l
277800070119    5c                   if        %found(fnblp01l) and blpft1='N'
277900070119     c                             and blpcca='7'
278000070119     c                   setoff                                       23
278100070119    5c                   endif
278200070119    4c                   endif
278300070119    3c                   endif
278400070119    2c                   endif
278500920302     C*
278600920602     C* 40 ON - INESISTENTE: VEDO SE GIA' TRASFORMATO COD BOLLA
278700070119     c   23
278800070119    2Can 40FLGCVB        IFEQ      'K'
278900941121    3C     COMTBL        IFEQ      §3ATB1
279000930108     C                   MOVEL     ERR(1)        ARBDER
279100930108     C                   ELSE
279200930108     C                   MOVEL     ERR(7)        ARBDER
279300941121    3C                   END
279400930108     C*
279500920602     C                   MOVEL     §3ATB1        SAVTB1            2
279600920602     C                   MOVEL     §3ATB2        SAVTB2            2
279700080408     c                   movel     ds3a          savds3a
279800920602     C*
279900920602     C                   MOVEL     '3A'          COD
280000070119     c                   MOVEL(P)  ARBCBN        KEY
280100920602     C     KTAB          CHAIN     TABEL                              32
280200920602     C  N32              MOVEL     TBLUNI        DS3A
280300080408     c   32              clear                   ds3a
280400080408     c                   movel     §3afca        newfca            1 0
280500920602     C*
280600941121     C* 1BOLLA MODIFICATA
280700941121    3C  N32SAVTB1        IFNE      §3ATB1
280800920602     C                   MOVEL     §3ATB1        COMTBL
280900990624     C     KBLP          SETLL     TITAS30C                               42
281000920602     C  N42              MOVEL     ERR(1)        ARBDER
281100941121    3C                   END
281200920602     C*
281300941121     C* 2 BOLLA MODIFICATA
281400920602     C* SOLO SE ESISTEVA CONTROLLO
281500941121    3C  N32SAVTB2        IFNE      §3ATB2
281600920602     C     SAVTB2        ANDNE     '  '
281700920602     C*
281800941121    4C     §3ATB2        IFEQ      '  '
281900920602     C                   MOVEL     'AP'          COMTBL
282000941121     C                   ELSE
282100920602     C                   MOVEL     §3ATB2        COMTBL
282200941121    4C                   END
282300990624     C     KBLP          SETLL     TITAS30C                               42
282400070119     C  N42              MOVEL     ERR(7)        ARBDER
282500941121    3C                   END
282600080408     C***                MOVEL     SAVTB1        §3ATB1
282700080408     C***                MOVEL     SAVTB2        §3ATB2
282800080408     c                   movel     savds3a       ds3a
282900070119     c
283000070119     c* Se codice bolla vecchio = codice bolla nuovo, anche se
283100070119     c*  inesistente è inutile ristamparlo: già fatto precedentemente
283200080408    3c  n42              if        arbcbp=arbcbn and §3afca=0
283300070119     c                   setoff                                       23
283400080408    3c                   endif
283500080408     c* Se si tratta di tipo bolla con C/ASS prima e senza dopo
283600080408     c*  o una KB che toglie solo in C/ASS,
283700080408     c*  se bolla modificata in C/S --> vado avanti per il C/ASS
283800090504    3c***                if        not *in42 and *in23  and §3atb2=*blanks
283900090504    3c                   if        *in23  and §3atb2=*blanks
284000080408    4c                   if        (newfca=0 and §3afca=1) or
284100080408     c                             (arbcbp=arbcbn and §3afca=1 and arbcas=0)
284200080408     C                   MOVEL     'F7'          COMTBL
284300080408     C     KBLP          chain     TITAS30C
284400080408    5c                   if        %found(titas30c)
284500080408     c                   setoff                                       2923
284600080408     c                   eval      VariataSede='S'
284700080408    5c                   endif
284800080408    4c                   endif
284900080408    3c                   endif
285000110315     c
285100110315     c* se ho trovato la bolla con cod bolla nuovo ma era stato annullato il C/Ass  o
285200110315     c*   non c'era e ora c'e' --> lo aggiungo o riabilito
285300110315    3c                   if        *in23  and *in42 and §3atb2=*blanks  and
285400110315    4c                              newfca=1 and §3afca=0
285500110315     c                   setoff                                       2923
285600110315     c                   eval       soloCAss='S'
285700110315     c                   endif
285800920602     C*
285900941121   X2C                   ELSE
286000920602     C*
286100941130     C* CONTROLLO SE LA SPEDIZIONE E' STORNATA
286200930514     C                   EXSR      STORNO
286300991019     c* se causale variazione 'Cx' e la spedizione e' stornata
286400991019     c* spengo in29 per caricare ugualmente la variazione
286500991021    3c     flgcvb        ifeq      'C'
286600060317    3c     flgcvb        oreq      'R'
286700991021    4c     *in23         ifeq      *off
286800991021     c                   setoff                                       29
286900991021   x4c                   else
287000991021     c* altrimenti cerco la spedizione
287100991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
287200991021     c* (cioè il franco) faccio setgt e readpe invece che chain
287300991021     c     kann          setgt     titas30c
287400991021     c     kann          readpe    titas30c                               23
287500991019     c  n23              setoff                                       29
287600991021    4c                   endif
287700991021    3c                   endif
287800080408     c
287900080408     c* per "I0" e codice bolla "Z" cerco la bolla come A2 e poi
288000080408     c*  aggiorno secondo i modi già previsti in AGARBI
288100140407    3c                   if        (comcvb='I0' or
288200140407     c                              comcvb='I7' or
288300140407     c                              comcvb='I8' or
288400140407     c                              comcvb='I9' )
288500140407    3c                              and arbcbo='Z'
288600080408     C                   MOVEL     'A2'          COMTBL
288700080408     C     KBLP          chain     TITAS30C
288800080408    4c                   if        %found(titas30c)
288900080408     c                   setoff                                       2923
289000080408    4c                   endif
289100080408    3c                   endif
289200110214     c* per "I0" e codice bolla "A" cerco la bolla come F1 e poi
289300110214     c*  aggiorno secondo i modi già previsti in AGARBI
289400140407    3c***                if        comcvb='I0' and (arbcbo='A' or arbcbo='Q')
289500140407    3c                   if        (comcvb='I0' or
289600140407     c                              comcvb='I7' or
289700140407     c                              comcvb='I8' or
289800140407     c                              comcvb='I9' )
289900140407     c                             and (arbcbo='A' or arbcbo='Q')
290000110214     C                   MOVEL     'F1'          COMTBL
290100110214     C     KBLP          chain     TITAS30C
290200110214    4c                   if        %found(titas30c)
290300110214     c                   setoff                                       2923
290400110214    4c                   endif
290500110214    3c                   endif
290600930514     C*
290700941121    3C   23COMTBL        IFEQ      §3ATB1
290800991015     C                   MOVEL     ERR(1)        ARBDER
290900920130     C                   ELSE
291000991015     C                   MOVEL     ERR(7)        ARBDER
291100941121    3C                   END
291200941121    2C                   END
291300941130     C**
291400941130     C* 23 ON --> STAMPO ERRORE
291500941130    2C     *IN23         IFEQ      *ON
291600920602     C  N42
291700920602     CAN 40              SETON                                        37
291800920212     C*        PER C.E.D.
291900920603     C  N42
292000920603     CAN 40
292100920603     CORN40              Z-ADD     102           COMFEV
292200920602     C  N42
292300920602     CAN 40
292400920602     CORN40              EXSR      WTRERR
292500941121    2C                   END
292600941121     C*
292700991019    1C                   Endif
292800941121     C*
292900991019    1c     *in29         ifeq      *off
293000941121     C* 29 OFF - S P E D I Z I O N E       E S I S T E N T E
293100121108     c* Salvo contenuto del tracciato record di titas in ds apposita:
293200121108     c* serve per la scrittura dell'immagine di titas prima della prima variazione
293300121108     c* nel file TIVRB00F
293400121108     c                   exsr      memo_dstitas
293500941121     C*
293600941121     C* 02 OFF             --> GIA' CONTABILIZZATA IN BLTAS10 O BLTASP
293700941130     C* 02 ON  MA TASFIC=1 --> GIA' CONTABILIZZATA IN BLTAS00
293800930512     C*
293900941130    2C     *IN02         IFEQ      *OFF
294000941121     C     *IN02         OREQ      *ON
294100941130     C     TASFIC        ANDNE     ' '
294200941130     C**
294300950202     C* VARIAZIONE "Ix" --> indirizzo/ peso/ volume
294400941130     C**
294500941130    3C     FLGCVB        IFEQ      'I'
294600941130     C*
294700950202     C* SE VARIO PESO/VOLUME      E NON E' LA 2 BOLLA --> ERRORE
294800960416    4C     COMCVB        IFNE      'I0'
294900140407    4C     COMCVB        andne     'I7'
295000140407    4C     COMCVB        andne     'I8'
295100140407    4C     COMCVB        andne     'I9'
295200941130     C*
295300991021    5C     §3ATB2        IFNE      tastbl
295400930513     C                   SETON                                        29
295500930804     C*
295600140219    6c***  TASPKB        IFNE      ARBPKF
295700130325     C***                MOVEL     ERR(9)        ARBDER
295800130325     C***                EXSR      WTRERR
295900120313     c                   exsr      ctr_vrb
296000140219   X6C***                ELSE
296100930603     C*
296200140219    7C***  TASFVF        IFNE      'T'
296300140219     C***  TASVLF        ANDNE     ARBVLF
296400140219    8C***  TASFVF        IFEQ      'Z'
296500140219     C***  TASVLF        ANDLT     ARBVLF
296600140219     C***  TASFVF        ORNE      'Z'
296700130325     C***                MOVEL     ERR(9)        ARBDER
296800130325     C***                EXSR      WTRERR
296900140219     c***                eval      wcntab='S'
297000140219     c***                exsr      ctr_vrb
297100140219    8C***                END
297200140219    7C***                END
297300930603     C*
297400140219    6C***                END
297500930603     C*
297600941130    5C                   ENDIF
297700941130     C*
297800930512   X4C                   ELSE
297900930512     C*
298000950308RM*  C*§SE VARIATO FERMO DEPOSITO E NON E' LA 2 BOLLA --> ERRORE
298100950308RM* 5C*§         TASFFD    IFNE ARBFFD
298200950308RM*  C*§         §3ATB2    ANDNECOMTBL
298300950308RM*  C*§                   MOVELERR,9     ARBDER
298400950308RM*  C*§                   EXSR WTRERR
298500950308RM* 5C*§                   END
298600990625     C**
298700990625     C* MEMORIZZO ULTIME 4 CIFRE CODICE FATTURAZIONE 2 BOLLA
298800991021    4C     §3ATB2        IFEQ      tastbl
298900990625     C                   MOVE      TASKSC        KSC42BO           4 0
299000930512    4C                   END
299100990625     C*
299200990625    4C                   END
299300930512    3C                   END
299400950202     C**
299500950202     C* VARIAZIONE "Cx" --> consegne particolari
299600950202     C**
299700950202    3C     FLGCVB        IFEQ      'C'
299800950202     C* no errore se 2 bolla
299900991021    4C     §3ATB2        IFNE      tastbl
300000991019     c
300100991019     C                   MOVEL     'TB'          COD
300200991021     C                   MOVEL(P)  tastbl        KEY
300300991019     C     KTAB          CHAIN     TABEL                              32
300400991019     C  N32              MOVEL     TBLUNI        DSTB
300500991019     C   32              MOVEL     *ALL'1'       DSTB
300600991019   4aC     §TBFCB        IFEQ      '1'
300700950202     C*
300800950202     C* STAMPO SOLO SE UNO DEI TRE VARIATO
300900950202    5C     TASFTC        IFEQ      ARBTC1
301000950202     C     TASTC2        ANDEQ     ARBTC2
301100950202     C     TASFTC        OREQ      ARBTC2
301200950202     C     TASTC2        ANDEQ     ARBTC1
301300950202     C*
301400950202   X5C                   ELSE
301500950202     C* CONTROLLO SE LA TARIFFA TASSA LE CONSEGNE PARTICOLARI
301600140219     C***                EXSR      CONPAR
301700950202     C*
301800140219    6C**   *IN17         IFEQ      *ON
301900130325     C****               MOVEL     ERR(9)        ARBDER
302000130325     C****               EXSR      WTRERR
302100120320     c                   exsr      ctr_vrb
302200140219    6C**                ENDIF
302300950202    5C                   ENDIF
302400950202     C*
302500991019   4ac                   ENDIF
302600950202    4C                   ENDIF
302700950202    3C                   ENDIF
302800941130     C**
302900941130     C* VARIAZIONE "Tx" --> tassazione 1 bolla
303000941130     C**
303100941130    3C     FLGCVB        IFEQ      'T'
303200950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
303300130326     C***  ARBFIV        IFGT      0
303400130326     C***  TASFIV        ANDNE     ARBFIV
303500950324     C                   Z-ADD     46            COMFEV
303600130326     C***                ENDIF
303700950324     C*
303800930513     C                   MOVEL     ERR(9)        ARBDER
303900990505     c                   eval      wdelarbu='S'
304000920220     C                   EXSR      WTRERR
304100920226     C                   SETON                                        29
304200011029     c                   if        tasias > 0  and tasvas = *blanks
304300011029     c                   movel     'ITL'         tasvas
304400011029     c                   endif
304500990419     c* Controllo se variato l'importo da assicurare se contabilizzata     da assicurare
304600990419     c                   if        *in13 and (arbias <> tasias or
304700990419     c                             arbvas <> tasvas)
304800990419     c*
304900990419     c                   exsr      AGGIAS
305000990419     c*
305100990419     c                   endif
305200941130    3C                   END
305300941130     C**
305400941130     C* VARIAZIONE "2x" --> tassazione 2 bolla
305500941130     C**
305600941130    3C     FLGCVB        IFEQ      '2'
305700991021     C     tastbl        ANDEQ     §3ATB2
305800950324     C* SE E' VARIATO LA FILIALE IVA-> ERRORE PER LA SEDE
305900130326     C**   ARBFIV        IFGT      0
306000130326     C**   TASFIV        ANDNE     ARBFIV
306100950324     C                   Z-ADD     46            COMFEV
306200130326     C**                 ENDIF
306300950324     C*
306400950203     C                   MOVEL     §3ATB2        TBLERR
306500930513     C                   MOVEL     ERR(9)        ARBDER
306600920226     C                   EXSR      WTRERR
306700920226     C                   SETON                                        29
306800941130    3C                   END
306900950202     C**
307000950202     C* VARIAZIONE "Mx" --> mittente
307100990628     c*                     la variazione del mittente dipende dalla conab
307200990628     c*                     della prima bolla, a prescindere da come è messa
307300990628     c*                     la seconda: se aggiono la  aggiorno anche la 2
307400950202     C**
307500950202    3C     FLGCVB        IFEQ      'M'
307600991021    5C     §3ATB2        ANDNE     tastbl
307700130326     C                   Z-ADD     46            COMFEV
307800950202     C                   MOVEL     ERR(9)        ARBDER
307900950202     C                   EXSR      WTRERR
308000950202     C                   SETON                                        29
308100950202    3C                   END
308200941130    2C                   END
308300930512    1C                   END
308400941130     C*
308500920226     C                   ENDSR
308600920228     C**---------------------------------------------------------
308700920228     C** CHAIN SU PRIMA BOLLA
308800920228     C**---------------------------------------------------------
308900920228     C     CHTB1         BEGSR
309000920228     C                   MOVEL     SAVTB1        COMTBL
309100941130     C                   SETOFF                                       020304
309200990625     C     KBLP          CHAIN     TITAS30C                           39
309300121108     c                   exsr      memo_dstitas
309400920228     C*
309500990414     C                   EXSR      CTR3
309600920228     C                   ENDSR
309700920228     C**---------------------------------------------------------
309800920228     C** CHAIN SU SECONDA BOLLA
309900920228     C**---------------------------------------------------------
310000920228     C     CHTB2         BEGSR
310100920228     C                   MOVEL     SAVTB2        COMTBL
310200941130     C                   SETOFF                                       020304
310300990625     C     KBLP          CHAIN     TITAS30C                           39
310400121108     c                   exsr      memo_dstitas
310500920228     C*
310600990414     C                   EXSR      CTR3
310700920228     C                   ENDSR
310800920226     C**---------------------------------------------------------
310900920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
311000920226     C**---------------------------------------------------------
311100920226     C     CTR2          BEGSR
311200920302     C                   MOVEL     COMTBL        FLGTB1            1
311300950202     C* SPEDIZIONE 1) GIA' IMPONIBILE     SE NO FATTURA E NO ASSEGNATO
311400950202     C*               SE VARIAZOINE DA ARRIVO
311500950202     C*            2) GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
311600950202     C*            --> NON POSSO VARIARE
311700950202    1C     TASDFT        IFGT      0
311800950202    2C     TASFIC        IFNE      ' '
311900920226     C                   SETON                                        29
312000130326     c                   z-add     46            comfev
312100991015     C                   MOVEL     ERR(9)        ARBDER
312200920226     C                   EXSR      WTRERR
312300950202    2C                   END
312400920226     C*
312500950202   X1C                   ELSE
312600920226     C*
312700990624    2C     TASimv        IFNE      *zeros
312800950202     C     COMCVB        ANDNE     'TP'
312900950202    3C     FLGTB1        ANDNE     'A'
313000920302     C                   SETON                                        29
313100920226     C*
313200991015     C
313300991015     C   29              MOVEL     ERR(8)        ARBDER
313400130326     c   29              z-add     46            comfev
313500920226     C   29              EXSR      WTRERR
313600950202    2C                   END
313700920302     C*
313800950202    1C                   END
313900920226     C                   ENDSR
314000920226     C**---------------------------------------------------------
314100920226     C** CONTROLLO SE GIA' CONTABILIZZATA O TASSATA PER F
314200941130     C** PER LE VAIRAZIONI C/ASSEGNO O TIPI BOLLA
314300920226     C**---------------------------------------------------------
314400920226     C     CTR3          BEGSR
314500920303     C                   SETOFF                                       2938
314600920226     C* SPEDIZIONE GIA' IMPONIBILE     SE NO FATTURA
314700920226     C*            GIA' CONTABILIZZATA SE FATTURA IMMEDIATA
314800920226     C*            NON POSSO VARIARE
314900920226     C*
315000990414     C  N02              SETON                                        2936
315100941202     C   02TASDFT        IFGT      0
315200990414     C     TASFIC        ANDNE     ' '
315300920227     C                   SETON                                        2936
315400920226     C*
315500920226     C                   ELSE
315600920226     C*
315700990624     C     TASimv        IFNE      *zeros
315800920226     C                   SETON                                        38
315900920226     C                   END
316000920226     C*
316100920226     C                   END
316200920226     C                   ENDSR
316300920211     C**---------------------------------------------------------
316400920211     C** SCRIVO FILE ERRORI
316500920211     C**---------------------------------------------------------
316600920211     C     WTRERR        BEGSR
316700110208     c                   clear                   arbdat
316800920211     C*
316900970916     C* CREO IL RECORD DI ERRORE SOLO SE LA DATA VARIAZIONE E' INFERIOR
317000970916     C* DI 3 GIORNI A OGGI PER LE STAMPA DI SEDE
317100000703    0C     COMFEV        IFEQ      102
317200110208     C     ARBDTV        ANDLE     DATAM3
317300000703     c     *in37         andeq     *on
317400100225
317500110208     c* Se si tratta di codice TP con importo da assicurare, stampo subito
317600100225    0C     COMFEV        oreq      102
317700100225     c     *in13         andeq     *on
317800100225     c     arbias        andgt     0
317900970916     C*é*
318000970916     C     COMFEV        ORNE      102
318100000703     C*é*
318200000703     C     *in37         ORNE      *on
318300970916     C**
318400991015     C     KERR          SETLL     FIARBE2C                               01
318500930702     C* SE NON TROVATO ERRORE
318600930706    1C     *IN01         IFEQ      *OFF
318700110208     c
318800110208     c* Se l'errore è bolla inesistente, aspetto 3 gg prima di ricrearlo
318900110208    2c                   if        arbder=err(1)
319000110208     c                   eval      arbdat=dateu
319100110208     C     KERR          SETLL     FIARBE2C                               01
319200110208    3c                   if        not  *in01
319300110208     c                   eval      arbdat=datam1
319400110208     C     KERR          SETLL     FIARBE2C                               01
319500110208    4c                   if        not  *in01
319600110208     c                   eval      arbdat=datam2
319700110208     C     KERR          SETLL     FIARBE2C                               01
319800110208    4c                   endif
319900110208    3c                   endif
320000110208    2c                   endif
320100110208    1c                   endif
320200110208     c
320300110208    1C     *IN01         IFEQ      *OFF
320400110209     c                   clear                   arbdat
320500930702     C* RISCRIVO ERRORE
320600950203     C* SE NON L'HO GIA' IMPOSTATO CERCO FILIALE A CUI INVIARE STAMPA
320700930706    2C     COMFEV        IFEQ      0
320800920212     C*
320900950203    3C     TBLERR        IFEQ      'F'
321000991015     C                   MOVEL     ARBLNP        ARBFEV
321100930706   X3C                   ELSE
321200991015     C                   MOVEL     TASLNA        ARBFEV
321300930706    3C                   END
321400971215     C* PRENDO TERMINAL DI PARTENZA
321500971215     C                   CLEAR                   DSLV55
321600971215     C                   MOVEL     'P'           D55TPT
321700991015     C                   MOVE      ARBFEV        D55LIN
321800971215     C                   Z-ADD     DATEU         D55DRF
321900971215     C                   CALL      'FNLV55R'
322000971215     C                   PARM                    DSLV55
322100971215     C     D55ERR        IFEQ      *BLANKS
322200991015     C                   MOVE      D55TFP        ARBFEV
322300971215     C                   END
322400920212     C*
322500930706   X2C                   ELSE
322600920212     C*
322700991015     C                   Z-ADD     COMFEV        ARBFEV
322800930706    2C                   END
322900991015     C     ARBFEV        IFEQ      102
323000070420     C*********          MOVE      MBR           ARBLNA
323100950202     C                   ELSE
323200950202     C                   MOVEL     TASLNA        ARBLNA
323300950202     C                   ENDIF
323400920211     C*
323500950201     C   11              WRITE     FNARBDEE
323600920212     C*
323700950201     C   12              WRITE     FNARBKEE
323800950201     C* SCRIVO ANCHE FNARBM0E SOLO SE RICHIESTO
323900930706    2C   12*IN36         IFEQ      '1'
324000950202     C     KARBM         CHAIN     FNARBMRR                           32
324100950201     C  N32              WRITE     FNARBMEE
324200920214     C                   SETOFF                                       36
324300930706    2C                   END
324400920212     C*
324500950203     C     *IN13         IFEQ      *ON
324600991015     C                   WRITE     FIARBTEE
324700950203     C*
324800991015     c* se devo cancellare fiarbu, lo faccio
324900991018     c     arbtrc        ifeq      '1'
325000991015     C     KARBU         SETLL     FIARBU1R
325100991015     C     KARBU         READE     FIARBU1R                               30
325200950203     C*
325300950203     C     *IN30         DOWEQ     *OFF
325400991015     C                   WRITE     FIARBUEE                             30
325500950203     C* SE POSSO LI DELETO
325600990505     c  n37              if        wdelarbu='S'
325700990505     C                   ADD       1             NUMRBU
325800991015     C                   DELETE    FIARBURR
325900990505     c                   endif
326000950203     C*
326100991015     C     KARBU         READE     FIARBU1R                               30
326200950203     C                   ENDDO
326300991018     c                   endif
326400950203     C                   ENDIF
326500950203     C*
326600950202     C   15              WRITE     FNARBGEE
326700950202     C   16              WRITE     FNARBMEE
326800991214   X1c                   else
326900991214     c* se devo cancellare fiarbu, lo faccio
327000991214    2c     *in37         ifeq      *off
327100991214     c     wdelarbu      andeq     'S'
327200991214     c     arbtrc        andeq     '1'
327300991214     C     KARBU         SETLL     FIARBU1R
327400991214     C     KARBU         READE     FIARBU1R                               30
327500991214     C*
327600991214    3C     *IN30         DOWEQ     *OFF
327700991214     C                   ADD       1             NUMRBU
327800991214     C                   DELETE    FIARBURR
327900991214     C     KARBU         READE     FIARBU1R                               30
328000991214    3C                   ENDDO
328100991214    2c                   endif
328200930706    1C                   ENDIF
328300970916    0C                   ENDIF
328400920212     C*
328500920212     C                   Z-ADD     0             COMFEV            3 0
328600950203     C                   MOVEL     §3ATB1        TBLERR
328700920211     C                   ENDSR
328800930514     C**---------------------------------------------------------
328900930514     C** CONTROLLO SE LA SPEDIZIONE INESISTENTE E' STORNATA
329000930514     C**---------------------------------------------------------
329100930514     C     STORNO        BEGSR
329200930514     C*
329300930514     C* SE E' SINGOLA BOLLA
329400930514     C     §3ATB2        IFEQ      '  '
329500930514     C*
329600991021     c* Per essere sicura in caso di bolla doppia di chainare la prima bolla
329700991021     c* (cioè il franco) faccio setgt e readpe invece che chain
329800991021     C     KANN          setgt     TITAS30C
329900991021     C     KANN          readpe    TITAS30C                               23
330000931119     C*
330100941130     C     *IN23         IFEQ      *OFF
330200930514     C                   MOVEL     'TB'          COD
330300941130     C                   MOVEL(P)  TASTBL        KEY
330400930514     C     KTAB          CHAIN     TABEL                              32
330500930514     C  N32              MOVEL     TBLUNI        DSTB
330600930514     C   32              MOVEL     *ALL'1'       DSTB
330700930514     C*
330800930514     C* SE NON E' STORNO STAMPO ERRORE
330900930514     C     §TBFCB        IFEQ      '1'
331000930514     C                   SETON                                        23
331100930514     C                   ELSE
331200930514     C                   MOVEL     '1'           ST1               1
331300930514     C                   END
331400930514     C                   END
331500930514     C*
331600930514     C                   END
331700930514     C                   ENDSR
331800930804     C*
331900950202     C**---------------------------------------------------------
332000950202     C* VEDO SE IN TARIFFE C'E' CONSEGNA PARTICOLARE
332100950202     C**---------------------------------------------------------
332200930804     C     CONPAR        BEGSR
332300930804     C                   SETOFF                                       17
332400930804     C*
332500930804     C                   MOVE      TASKSC        KSC4              4 0
332600930804     C*
332700930804     C* SE E' 8888 O 9999 ERRORE SUBITO PERCHE' UTILIZZO TARIFFE CART
332800930804    2C     KSC4          IFEQ      9999
332900930804     C     KSC4          OREQ      8888
333000930804     C                   SETON                                        17
333100930804     C*
333200930804   X2C                   ELSE
333300930804     C*
333400930804     C                   Z-ADD     999           TARPRG
333500930804     C                   Z-ADD     999           PREPRG
333600991021     C                   MOVEL     tastbl        UNOTB1
333700930804     C*
333800930805     C* CIO' CHE NON DEVE ESSERE LA TARIFFA (SICURAMENTE UN FRANCO
333900930805     C*  NON PUO' AVERE IN TARIFFA A DI ARRIVO)
334000930804    3C     UNOTB1        IFEQ      'F'
334100930805     C                   MOVEL     'A'           TFP               1
334200930804     C                   ELSE
334300930805     C                   MOVEL     'F'           TFP
334400930804    3C                   END
334500930804     C*
334600930804     C* SE C'E' A ZERO NON DO' L'ERRORE
334700941130     C     KTAM          SETLL     TNTAM000
334800941130     C     KTAM          READE     TNTAM000                               35
334900930804     C*
335000930804    3C     *IN35         DOWEQ     *OFF
335100930804     C*
335200930804     C* ESCLUDO SE ANNULLATO  BLOCCATA O PER ARRIVI
335300930804    4C     TAMATB        IFNE      'A'
335400930804     C*
335500941205    5C     TAMBAP        IFNE      'B'
335600941205     C     TAMBAP        ANDNE     TFP
335700930804     C*
335800930804     C                   MOVE      TAMPRG        TARPRG            3 0
335900930804     C*
336000930804     C* VEDO SE E' TARIFFA VALIDA
336100121116     c                   movel     tasaas        tasdsp
336200121116     c                   move      tasmgs        tasdsp
336300120430    6C     TASDSP        IFGE      TAMDDT
336400120430     C     TASDSP        ANDLE     TAMDST
336500120430     C                   SETON                                        35
336600120430    6C                   END
336700930804     C*
336800930804   X5C                   ELSE
336900930804     C                   MOVE      TAMPRG        PREPRG            3 0
337000930804    5C                   END
337100930804    4C                   END
337200930804     C*
337300941130     C  N35KTAM          READE     TNTAM000                               35
337400930804    3C                   END
337500930804     C*
337600930804     C* NON TROVATA TARIFFA: STAMPO ERRORE
337700930804    3C     TARPRG        IFEQ      999
337800930804     C     PREPRG        ANDEQ     999
337900930804     C                   SETON                                        17
338000930804   X3C                   ELSE
338100930804     C*
338200930804    4C     TARPRG        IFNE      999
338300930804     C                   MOVEL     TARPRG        PRG
338400930804   X4C                   ELSE
338500930804     C                   MOVEL     PREPRG        PRG
338600930804    4C                   ENDIF
338700930804     C*
338800930804     C* VEDO SE TARIFFA PARTICOLARE A 0
338900950202     C     ARBTC1        IFNE      ' '
339000990719     C                   MOVEL(P)  ARBTC1        WFTC
339100950202     C                   EXSR      PARTIC
339200950202     C                   ENDIF
339300950202     C  N17ARBTC2        IFNE      ' '
339400990719     C                   MOVEL(P)  ARBTC2        WFTC
339500950202     C                   EXSR      PARTIC
339600950202     C                   ENDIF
339700950202     C*
339800930804    3C                   END
339900930804    2C                   END
340000930804     C*
340100930804     C                   ENDSR
340200950202     C**---------------------------------------------------------
340300950202     C* CONTROLLO IN TPT
340400950202     C**---------------------------------------------------------
340500950202     C     PARTIC        BEGSR
340600950202     C*
340700990719     C     KTAM2         CHAIN     TITPT000                           17
340800950202     C* 17 ON - NON ESISTE TARIFFA
340900950202    4C     *IN17         IFEQ      *OFF
341000950202     C*         NON ESISTE PERCHE' ANNULLATA
341100950202    5C     TPTATB        IFEQ      'A'
341200950202     C                   SETON                                        17
341300950202   X5C                   ELSE
341400950202     C* VEDO SE ESISTE DETTAGLIO TARIFFA PARTICOLARE
341500990719     C     KTAM2         SETLL     TITPD000
341600990719     C     KTAM2         READE     TITPD000                               35
341700950202     C*
341800950202    6C     *IN35         DOWEQ     *OFF
341900950202     C* NO ANNULLATO
342000950202    7C     TPDATB        IFEQ      ' '
342100950202     C* SE > 0 ERRORE
342200950202    8C     TPDITR        IFNE      0
342300950202     C     TPDMIN        ORNE      0
342400950202     C     TPDMAX        ORNE      0
342500950202     C                   SETON                                        1735
342600950202    8C                   END
342700950202    7C                   END
342800950202     C*
342900990719     C  N35KTAM2         READE     TITPD000                               35
343000950202    6C                   END
343100950202    5C                   END
343200950202    4C                   END
343300950202     C                   ENDSR
343400941130     C**---------------------------------------------------------
343500941130     C* CONTROLLO SE ESISTE VARIA "G"
343600941130     C**---------------------------------------------------------
343700941130     C     ESVARG        BEGSR
343800941130     C                   CLEAR                   WSIVAR
343900941130     C*
344000941130     C                   SETON                                        30
344100941130    3C     TASSV1        IFNE      'G'
344200941130     C     TASSV2        ANDNE     'G'
344300941130     C     TASSV3        ANDNE     'G'
344400941130     C                   MOVEL     'G'           WSV1
344500990625     C     KBLPV         SETLL     TITA730C                               30
344600941130    3C                   ENDIF
344700941130     C*
344800941130     C     *IN30         IFEQ      *ON
344900941130     C                   MOVEL     'S'           WSIVAR            1
345000941130     C                   ENDIF
345100941130     C                   ENDSR
345200941202     C**---------------------------------------------------------
345300941202     C* CONTROLLO SE HO MODIFICATO IL DESTINATARIO -------------------*
345400941202     C**---------------------------------------------------------
345500941202     C     CTRDES        BEGSR
345600941202     C*
345700950202     C                   MOVEL     CSBRSD        ALF35            35
345800941202     C* CONTROLLO LA 1 POSIZIONE <> ' '
345900950202     C     ' '           CHECKR    ALF35         C                 3 0    30
346000950202     C                   MOVE      ALF35         W001A             1
346100941202     C*
346200941202     C* CONFRONTO CON IL CAMPO CHE HO RICEVUTO
346300941202     C     *IN30         IFEQ      *ON
346400941215     C     W001A         ANDEQ     ' '
346500950202     C                   MOVEA     ARBRSD        K35
346600941202     C                   ADD       1             C
346700950202     C                   MOVEA     *BLANKS       K35(C)
346800950202     C                   MOVEA     K35           W035A            35
346900941202     C                   ELSE
347000950202     C                   MOVEL     ARBRSD        W035A
347100941202     C                   ENDIF
347200941202     C*
347300950202     C     W035A         IFEQ      ALF35
347400941202     C                   SETOFF                                       31
347500941202     C                   END
347600941202     C*
347700941202     C                   ENDSR
347800990415     C**------------------------------------------------------------------------------
347900990415     C** VARIAZIONE IMPORTO D'ASSICURARE PER SPEDIZIONE GIA' TASSATA / CONTABILIZZATA
348000990415     C**------------------------------------------------------------------------------
348100990415     c*
348200990419     c     AGGIAS        begsr
348300990415     c*
348400990415     c* controllo se la spedizione è già stata fatturata / contabilizzata oppure
348500990415     c* se è stato calcolato l'imponibile
348600990415     c*
348700990415     c                   exsr      ctr3
348800990415     c* se è acceso il  38 vuol dire che non è stata contabilizzata ma è stato calcolato
348900991018     c* fino all'imponibile e quindi devo andare a controllare se è già stata
349000990415     c* calcolata la varia "R"
349100990519     c                   select
349200990519    1c                   when      *in38
349300990421     c*
349400990421     c                   z-add     tasias        arbiap
349500990421     c                   movel     tasvas        arbvap
349600990421     c                   movel     'R'           arbvar
349700991019     c*
349800991019     c* verifico se devo registrare messagio per ufficio assicurazione
349900991019     C* relativo a spedizione avente tipo bolla non assicurabile per la
350000991019     c* quale è stato tolto, modificato o aggiunto l'importo da assicurare
350100991019     c* Escludo le bolle di recupero
350200991019     c     tblrbl        ifne      'R'
350300991019     c                   exsr      msgass
350400991019     c                   endif
350500131129
350600131129     c                   clear                   arbiap
350700131129     c                   clear                   arbvap
350800131129     c                   clear                   arbvar
350900990415     c*
351000990824     c* Non faccio niente se spedizione:
351100990824     c* - contabilizzata
351200990824     c* - tasias> 0
351300990824     c* - tasfcm="F" (tasias immesso dalla sede (dalla fatturazione))
351400990824     c* - arbias= 0
351500990824     c                   when      *in29 and tasias > 0 and tasfcm = 'F'
351600990824     c                                   and arbias = 0
351700990824     c*
351800990519     c* se è acceso il 29  vuol dire che è stata CONTABILIZZATA
351900990519    1c                   when      *in29
352000990505     c                   eval      wdelarbu='S'
352100990415     c
352200990415     c* verifico l'esistenza della varia "R"
352300990415     c                   exsr      esvarr
352400990419     c*
352500990419     c                   z-add     tasias        arbiap
352600990419     c                   movel     tasvas        arbvap
352700990419     c                   movel     'R'           arbvar
352800990415     c*
352900990519    2c                   SELECT
353000990415     c*
353100990415     c* importo in TAS = SI
353200990415     c* importo in ARB = NO
353300990415     c* varia   "R"    = SI
353400990415     c*                            errore no tolta varia "R"
353500991019     c                   when      tasias > 0 and arbias = 0
353600130325     c***                if        Wvariar = 'S'
353700130325     c***                movel     err(34)       arbder
353800130325     c***                exsr      WTRERR
353900130325     c***                endif
354000121203     c                   exsr      ctr_vrb
354100990415     c* messaggio all'Ufficio Assicurazione
354200990519     c* escludo le bolle di recupero
354300990519     c                   if        tblrbl<>'R'
354400990415     c                   z-add     603           comfev
354500990415     c                   movel     err(32)       arbder
354600990419     c                   exsr      wtrerr
354700990519     c                   endif
354800990415     c*
354900990415     c* importo in TAS = NO
355000990415     c* importo in ARB = SI
355100990415     c* varia   "R"    = NO
355200990415     c*                            errore no aggiunta varia "R"
355300991019     c                   when      tasias = 0 and arbias > 0
355400130325     c                   if        wvariar = ' '
355500130325     c***                movel     err(35)       arbder
355600130325     c***                exsr      wtrerr
355700120423     c                   exsr      ctr_vrb
355800991019     c                   endif
355900990415     c* messaggio all'Ufficio Assicurazione
356000990519     c                   if        tblrbl<>'R'
356100990415     c                   z-add     603           comfev
356200990415     c                   movel     err(31)       arbder
356300990419     c                   exsr      wtrerr
356400990519     c                   endif
356500990415     c*
356600990415     c* importo in TAS = SI
356700990415     c* importo in ARB = SI  ma diversa da tas
356800990415     c* varia   "R"    = SI
356900990415     c*                            errore no modificata varia "R"
357000991019     c                   when      tasias <> arbias
357100991019     c                   if        wvariar = 'S'
357200130325     c***                movel     err(36)       arbder
357300130325     c***                exsr      wtrerr
357400120423     c                   exsr      ctr_vrb
357500991019     c                   endif
357600990415     c* messaggio all'Ufficio Assicurazione
357700990519     c                   if        tblrbl<>'R'
357800990415     c                   z-add     603           comfev
357900990415     c                   movel     err(33)       arbder
358000990419     c                   exsr      wtrerr
358100990519     c                   endif
358200990415     c*
358300990519    2c                   endsl
358400990415     c*
358500990624     c* aggiorno in TITAS il valore dell'importo d'assicurare
358600990415     c                   z-add     arbias        tasias
358700990415     c                   movel     arbvas        tasvas
358800990824     c                   clear                   tasfcm
358900990415     c*
359000990624     c   02              update    titas000
359100990624     c   03              update    titas010
359200990624     c   04              update    titasp00
359300990419     c*
359400990419     c                   clear                   arbiap
359500990419     c                   clear                   arbvap
359600990419     c                   clear                   arbvar
359700990419     c**
359800990415     c* riaccendo il 29 e spengo il 36
359900990415     c                   seton                                        29
360000990415     c                   setoff                                       36
360100991019     c* Se e' bolla normale, ma non compare nel tabulato per la
360200991019     c*  Consuldanni, stampo solo per l'ufficio assicurazione
360300990519     c                   other
360400990519     c                   if        tblrbl<>'R'
360500000410     c                   z-add     tasias        arbiap
360600000410     c                   movel     tasvas        arbvap
360700000410     c                   movel     'R'           arbvar
360800991019     c                   exsr      msgass
360900000410     c                   clear                   arbiap
361000000410     c                   clear                   arbvap
361100000410     c                   clear                   arbvar
361200990519     c                   endif
361300990519    1c                   ENDSL
361400990415     c*
361500990415     c                   endsr
361600990415     c*
361700991019     C**-------------------------------------------------------------
361800991019     C** messaggi per ufficio Assicurazione per tipo bolla non assic.
361900991019     C**-------------------------------------------------------------
362000991019     C     msgass        BEGSR
362100991019     C                   MOVEL     'TB'          COD
362200991019     c                   movel(P)  tastbl        key
362300991019     C     KTAB          CHAIN     TABEL                              32
362400991019     C  N32              MOVEL     TBLUNI        DSTB
362500991019     C   32              clear                   DSTB
362600991019     c* se bolla non assicurabile e aggiunto, tolto o modificato importo
362700991019     c* da assicurare devo stampare messaggio per ufficio Assicurazione
362800991019    1c     §tbfas        ifeq      '0'
362900991019    2c                   select
363000991019     c* tolto importo da assicurare
363100991019   w1c                   when      tasias > 0 and arbias = 0
363200991019     c                   z-add     603           comfev
363300991019     c                   movel     err(41)       arbder
363400991019     c                   exsr      wtrerr
363500991019     c* aggiunto importo da assicurare
363600991019   w2c                   when      tasias = 0 and arbias > 0
363700991019     c                   z-add     603           comfev
363800991019     c                   movel     err(40)       arbder
363900991019     c                   exsr      wtrerr
364000991019     c* modificato importo da assicurare
364100991019   w3c                   when      tasias <> 0
364200991019     c                   z-add     603           comfev
364300991019     c                   movel     err(42)       arbder
364400991019     c                   exsr      wtrerr
364500991019    2c                   endsl
364600991019    1c                   endif
364700991019     c                   endsr
364800990415     C**---------------------------------------------------------
364900990415     C* CONTROLLO SE ESISTE VARIA "R"
365000990415     C**---------------------------------------------------------
365100990415     C     ESVARR        BEGSR
365200990415     C                   CLEAR                   WVARIAR           1
365300990415     C*
365400990415     C                   SETON                                        30
365500990415    3C     TASSV1        IFNE      'R'
365600990415     C     TASSV2        ANDNE     'R'
365700990415     C     TASSV3        ANDNE     'R'
365800990415     C                   MOVEL     'R'           WSV1
365900990624     C     KBLPV         SETLL     TITA730C                               30
366000990415    3C                   ENDIF
366100990415     C*
366200990415     C     *IN30         IFEQ      *ON
366300990415     C                   MOVEL     'S'           WVARIAR
366400990415     C                   ENDIF
366500990415     C                   ENDSR
366600941130     C**---------------------------------------------------------
366700941130     C* ROUTINE INIZIALE
366800941130     C**---------------------------------------------------------
366900941130     C     *INZSR        BEGSR
367000021014
367100941130     C     *ENTRY        PLIST
367200941130     C                   PARM                    KPJBA
367300021014
367400941130     C                   Z-ADD     1             CODUT             1 0
367500021014
367600021014     c     *dtaara       define    §azute        azuteds
367700021014     c     *dtaara       define    §datiute      ddatiute
367800021014     C                   in(E)     *dtaara
367900021014     C                   IF        %Error  or  RSUT = *blanks
368000021014     C                   CLEAR                   TIBS34DS
368100021014     C                   call      'TIBS34R'
368200021014     C                   parm                    Tibs34Ds
368300021014     C                   in        *dtaara
368400021014     c                   ENDIF
368500021014
368600021014     C                   Z-ADD     DUTKCI        KCI               4 0
368700021014
368800941130     C*---------------------------------------------------------------*
368900991018     C     KBLP          KLIST
369000950201     C                   KFLD                    ARBAAS
369100941130     C                   KFLD                    ARBLNP
369200941130     C                   KFLD                    ARBNRS
369300941130     C                   KFLD                    ARBNSP
369400941130     C                   KFLD                    COMTBL            2
369500990629     C     KTITA         KLIST
369600990629     C                   KFLD                    TASAAS
369700990629     C                   KFLD                    TASLNP
369800990629     C                   KFLD                    TASNRS
369900990629     C                   KFLD                    TASNSP
370000990629     C                   KFLD                    §TRC
370100941130     C     KBLPV         KLIST
370200950201     C                   KFLD                    ARBAAS
370300941130     C                   KFLD                    ARBLNP
370400941130     C                   KFLD                    ARBNRS
370500941130     C                   KFLD                    ARBNSP
370600941130     C                   KFLD                    COMTBL            2
370700941130     C                   KFLD                    WSV1
370800941130     C     KANN          KLIST
370900950201     C                   KFLD                    ARBAAS
371000941130     C                   KFLD                    ARBLNP
371100941130     C                   KFLD                    ARBNRS
371200941130     C                   KFLD                    ARBNSP
371300950202     C     KARBU         KLIST
371400941130     C                   KFLD                    ARBAAS
371500941130     C                   KFLD                    ARBLNP
371600941130     C                   KFLD                    ARBNRS
371700941130     C                   KFLD                    ARBNSP
371800991015     C                   KFLD                    ARBTRC
371900941130     C                   KFLD                    ARBDTV
372000941130     C                   KFLD                    ARBORV
372100950202     C     KARBM         KLIST
372200950202     C                   KFLD                    ARBAAS
372300950202     C                   KFLD                    ARBLNP
372400950202     C                   KFLD                    ARBNRS
372500950202     C                   KFLD                    ARBNSP
372600950202     C                   KFLD                    ARBCVB
372700950202     C                   KFLD                    ARBDTV
372800950202     C                   KFLD                    ARBORV
372900941130     C     KARBD         KLIST
373000941130     C                   KFLD                    ARBAAS
373100941130     C                   KFLD                    ARBLNP
373200941130     C                   KFLD                    ARBNRS
373300941130     C                   KFLD                    ARBNSP
373400941130     C                   KFLD                    ARBCVB
373500941130     C                   KFLD                    ARBDTV
373600941130     C                   KFLD                    COMORV            6 0
373700941130     C     KTAB          KLIST
373800941130     C                   KFLD                    CODUT
373900941130     C                   KFLD                    COD               2
374000941130     C                   KFLD                    KEY               8
374100941130     C     KACO          KLIST
374200941130     C                   KFLD                    CODUT
374300941130     C                   KFLD                    KCI
374400941130     C                   KFLD                    TASKSC
374500950202     C     KACO2         KLIST
374600950202     C                   KFLD                    CODUT
374700950202     C                   KFLD                    KCI
374800950202     C                   KFLD                    ARBCCM
374900981118     C     Kppa          KLIST
375000981118     C                   KFLD                    wsoc
375100981118     C                   KFLD                    wctb
375200981118     C                   KFLD                    wkcc
375300981118     C                   KFLD                    wksc
375400981118     C                   KFLD                    wdft
375500981118     C                   KFLD                    wnft
375600981118     C                   KFLD                    wfiv
375700981118     C     Kycpa         KLIST
375800981118     C                   KFLD                    ppasys
375900981118     C                   KFLD                    ppanrasint
376000941130     C     KERR          KLIST
376100941130     C                   KFLD                    ARBAAS
376200941130     C                   KFLD                    ARBLNP
376300941130     C                   KFLD                    ARBNRS
376400941130     C                   KFLD                    ARBNSP
376500941130     C                   KFLD                    ARBDER
376600941130     C                   KFLD                    ARBDAT
376700991214     c                   kfld                    ARBDTV
376800991214     c                   kfld                    ARBORV
376900941130     C     KTAM          KLIST
377000941130     C                   KFLD                    TASKSC
377100941130     C                   KFLD                    TASCTR
377200941130     C     KTAM2         KLIST
377300941130     C                   KFLD                    TASKSC
377400941130     C                   KFLD                    TASCTR
377500941130     C                   KFLD                    PRG               3 0
377600950202     C                   KFLD                    WFTC
377601151119     C     Kar5          KLIST
377602151119     C                   KFLD                    ARBAAS
377603151119     C                   KFLD                    ARBLNP
377604151119     C                   KFLD                    ARBNRS
377605151119     C                   KFLD                    ARBNSP
377606151119     C                   KFLD                    kar5trd
377700121108
377800121108
377900121108
378000941130     C**
378100941130     C* DEFINIZIONE CAMPI
378200941130     C**
378300960625     C     *LIKE         DEFINE    TASTBL        SAVTBL
378400990719     C     *LIKE         DEFINE    tptftc        WFTC
378500950202     C     *LIKE         DEFINE    TASVLF        SAVVLF
378600950202     C     *LIKE         DEFINE    TASVLF        SAVVLB
378700950202     C     *LIKE         DEFINE    TASFVF        SAVFVF
378800950202     C     *LIKE         DEFINE    TASFVF        SAVFVB
378900990625     C     *LIKE         DEFINE    TAScts        SAVcts
379000941130     C     *LIKE         DEFINE    TASSV1        WSV1
379100941130     C     *LIKE         DEFINE    TASVA1        WVA1
379200941130     C     *LIKE         DEFINE    ARBDER        SAVDER
379300941130     C     *LIKE         DEFINE    CSBPAR        DESPAR
379400970916     C     *LIKE         DEFINE    CSBPAR        DESPRV
379500010126     C     *LIKE         DEFINE    arblnp        savlnp
379600010126     C     *like         define    tasftc        WTCP
379700010126     C     *like         define    tasftc        WsvTCP
379800941130     C*---------------------------------------------------------------*
379900970916     C                   TIME                    W0140            14 0
380000970916     C                   MOVEL     W0140         UTIME             6 0
380100970916     C                   MOVE      W0140         WDTGIO            8 0
380200970916     C* UDATE IN AAAAMMGG
380300970916     C                   Z-ADD     WDTGIO        G02DAT
380400970916     C                   MOVEL     *BLANK        G02ERR
380500970916     C                   CALL      'XSRDA8'
380600970916     C                   PARM                    WLBDAT
380700970916     C                   MOVEL     G02INV        DATEU             8 0
380800110208     C* SOTTRAGGO  1
380900110208     C     G02TGI        SUB       1             GIOTGI
381000110208     C                   CALL      'XSRGI8'
381100110208     C                   PARM                    WGIDAT
381200110208     C                   Z-ADD     GIOINV        DATAM1            8 0
381300110208     C* SOTTRAGGO  2
381400110208     C     G02TGI        SUB       2             GIOTGI
381500110208     C                   CALL      'XSRGI8'
381600110208     C                   PARM                    WGIDAT
381700110208     C                   Z-ADD     GIOINV        DATAM2            8 0
381800970916     C* SOTTRAGGO  3
381900970916     C     G02TGI        SUB       3             GIOTGI
382000970916     C                   CALL      'XSRGI8'
382100970916     C                   PARM                    WGIDAT
382200110208     C                   Z-ADD     GIOINV        DATAM3            8 0
382300941130     C*
382400941130     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
382500941130     C* ASSUMO IL PRIMO MEMBRO DEL FILE
382600941130     C     MBRFIL        IFEQ      *BLANK
382700020827     C                   MOVEL     'M888888888'  MBRFIL
382800941130     C                   END
382900941130     C* MEMBRO LUNGO 3
383000941130     C                   MOVEL     ALFFIL        MBR               3 0
383100941130     C* PRENDO ANNO IN CORSO
383200941130     C                   MOVE      UDATE         AAS               2 0
383300070119     c* Apro file bolle di filiale
383400070119     c                   clear                   tibs55ds
383500070119     c                   eval      i50apo=001
383600070119     c                   call      'TIBS55R'
383700070119     c                   parm                    tibs55ds
383800070119     c*
383900070119     c                   Clear                   commt
384000070119     c                   Movel(p)  cmdt(1)       commt
384100070119     c                   Eval      %Subst(commt:30:10) = o50ala
384200070119     c                   Eval      commt =
384300070119     c                             %trim(commt) + '/FNBLP01L)'
384400070119     c                   Call      'QCMDEXC'
384500070119     c                   Parm                    commt
384600070119     c                   Parm                    lenght
384700070119     c                   Open      FNBLP01L
384800070119     c
384900070119     c
385000071102      * Aggancio tabella VPO record DECO
385100120510     c**********         Clear                   Dvpodeco
385200120510     c***********        Clear                   Tibs02ds
385300120510     c***********        Eval      T02tla = 'L'
385400120510     c***********        Eval      T02Mod = 'C'
385500120510     c*************      Eval      T02Sif = knsif
385600120510     c*************      Eval      T02Cod = 'VPO'
385700120510     c***********        Movel(p)  'DECO'        T02Ke1
385800120510     c*******            Call      'TIBS02R'
385900120510     c*********          Parm                    Kpjba
386000120510     c*********          Parm                    Tibs02ds
386100120510     c*********          If        T02Err = *Blanks
386200120510     c*********          Movel     T02Uni        Dvpodeco
386300120510     c**********         EndIf
386400071102
386500941130     C                   ENDSR
386600930804     C*
386700981001     C**-------------------------------------------------------------
386800981001     C** AGGIORNO CON CODICI DI VARIAZIONE DM DD BOLLA SEDE MITT/DEST
386900981001     C**-------------------------------------------------------------
387000981001     C     AGTMD         BEGSR
387100981001     C* CONTROLLO CHE TIPO DI VARIAZIONE STO LEGGENDO
387200981001     C* DM CREAZIONE MITTENTE BOLLA SEDE
387300981001     C* DD CREAZIONE DESTINATARIO BOLLA SEDE
387400981001     C* I0 VARIAZIONE DESTINATARIO
387500981001     C* MI VARIAZIONE MITTENTE
387600981001     C*
387700981001    1C     ARBCVB        IFNE      'DM'
387800981001     C     ARBCVB        ANDNE     'DD'
387900981001     C     ARBCVB        ANDNE     'I0'
388000140407     C     ARBCVB        ANDNE     'I7'
388100140407     C     ARBCVB        ANDNE     'I8'
388200140407     C     ARBCVB        ANDNE     'I9'
388300981001     C     ARBCVB        ANDNE     'MI'
388400981001     C*
388500981001   X1C                   ELSE
388600981001     C* CERCO LA SPEDIZIONE NEL FILE TNTMD
388700981001     C     KANN          CHAIN     TNTMD01L                           48
388800981001     C* IMMISSIONE O CAMBIO MITTENTE
388900981001     C* CAUSALI DI VARIAZIONI
389000981001    2C     ARBCVB        IFEQ      'DM'
389100981001     C     ARBCVB        OREQ      'MI'
389200981001     C*
389300981001     C                   MOVEL     ARBRSM        TMDRSM
389400981001     C                   MOVEL     ARBINM        TMDINM
389500981001     C                   MOVEL     ARBCAM        TMDCAM
389600981001     C                   MOVEL     ARBLOM        TMDLOM
389700981001     C                   MOVEL     ARBPRM        TMDPRM
389800981001     C                   MOVEL     ARBNZM        TMDNZM
389900981001     C*
390000981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
390100981001    3C     *IN48         IFEQ      '1'
390200981005     C     ARBCVB        ANDEQ     'DM'
390300981001     C                   Z-ADD     ARBAAS        TMDAAS
390400981001     C                   Z-ADD     ARBLNP        TMDLNP
390500981001     C                   Z-ADD     ARBNRS        TMDNRS
390600981001     C                   Z-ADD     ARBNSP        TMDNSP
390700981001     C                   WRITE     TNTMD000
390800981001     C*
390900981005    3C                   ENDIF
391000981005     C* PER 48 SPENTO AGGIORNO IL RECORD
391100981005     C  N48              UPDATE    TNTMD000
391200981001     C*
391300981001    2C                   ENDIF
391400981001     C*
391500981001     C* IMMISSIONE O CAMBIO DESTINATARIO
391600981001     C* CAUSALI DI VARIAZIONI
391700981001    2C     ARBCVB        IFEQ      'DD'
391800981001     C     ARBCVB        OREQ      'I0'
391900140407     C     ARBCVB        OREQ      'I7'
392000140407     C     ARBCVB        OREQ      'I8'
392100140407     C     ARBCVB        OREQ      'I9'
392200981001     C*
392300981001     C                   MOVEL     ARBRSD        TMDRSD
392400981001     C                   MOVEL     ARBIND        TMDIND
392500981001     C                   MOVEL     ARBCAD        TMDCAD
392600981001     C                   MOVEL     ARBLOD        TMDLOD
392700981001     C                   MOVEL     ARBPRD        TMDPRD
392800981001     C                   MOVEL     ARBNZD        TMDNZD
392900981001     C*
393000981001     C* PER 48 ACCESO VALORIZZO I DATI CHIAVE SPEDIZIONE E SCRIVO UN NUOVO RECOR
393100981001    3C     *IN48         IFEQ      '1'
393200981005     C     ARBCVB        ANDEQ     'DD'
393300981001     C                   Z-ADD     ARBAAS        TMDAAS
393400981001     C                   Z-ADD     ARBLNP        TMDLNP
393500981001     C                   Z-ADD     ARBNRS        TMDNRS
393600981001     C                   Z-ADD     ARBNSP        TMDNSP
393700981001     C                   WRITE     TNTMD000
393800981005    3C                   ENDIF
393900981005     C*
394000981001     C* PER 48 SPENTO AGGIORNO IL RECORD
394100981005     C  N48              UPDATE    TNTMD000
394200981001     C*
394300981001    2C                   ENDIF
394400981001     C*
394500981001     C* SE LA VARIAZIONE ARB E' DD O DM DELETO IL RECORD E NON PROSEGUO CON
394600981001     C* LA LETTURA
394700981001    2C     ARBCVB        IFEQ      'DM'
394800981001     C     ARBCVB        OREQ      'DD'
394900981001     C   11              DELETE    FNARBD00
395000981001     C   16              DELETE    FNARBM00
395100981001     C                   MOVE      'Y'           DMDD
395200981001    2C                   ENDIF
395300981001     C*
395400981001    1C                   ENDIF
395500981001     C*
395600981001     C                   ENDSR
395700120312     C**-------------------------------------------------------------
395800120515     C** RITASSAZIONE DELLA SPEDIZIONE CON I NUOVI DATI RICEVUTI
395900120312     C**-------------------------------------------------------------
396000120313     C     ctr_vrb       BEGSR
396100151008     c* Richiamo pgm memorizzazione variazioni post-fatturazione
396200151008     c                   clear                   fnlvv1ds
396300151008     c                   clear                   parvar
396400151008     c                   clear                   partitas
396500151008     c                   clear                   parcsb
396600151008     c                   eval      iv1cvb=comcvb
396700151008     c                   eval      iv1pgm=nompgm
396800151008     c                   select
396900151008     c                   when      %subst(iv1cvb:1:1)='I'
397000151008     c     nrri          chain     fnarbd0r      dsarbd
397100151008     c                   if        %found(fnarbd0r)
397200151008     c                   eval      parvar =dsarbd
397300151008     c                   endif
397400151008     c                   when      %subst(iv1cvb:1:1)='K'
397500151008     c     nrri          chain     fnarbk0r      dsarbk
397600151008     c                   if        %found(fnarbk0r)
397700151008     c                   eval      parvar =dsarbk
397800151008     c                   endif
397900151008     c                   if        wanncas='S'
398000151008     c                   eval      iv1can='S'
398100151008     c                   endif
398200151008     c                   eval      parcsb  = dscsb
398300151008     c                   when      %subst(iv1cvb:1:1)='T'
398400151008     c     nrri          chain     fiarbt0r      dsarbt
398500151008     c                   if        %found(fiarbt0r)
398600151008     c                   eval      parvar =dsarbt
398700151008     c                   endif
398800151008     c                   when      %subst(iv1cvb:1:1)='C'
398900151008     c     nrri          chain     fnarbg0r      dsarbg
399000151008     c                   if        %found(fnarbg0r)
399100151008     c                   eval      parvar =dsarbg
399200151008     c                   endif
399300151008     c                   endsl
399400151008     c                   eval      kpjbu=fnlvv1ds
399500151009     c                   eval      partitas=dstitas
399600151008     c                   call      'FNLVV1R'
399700151008     c                   parm                    kpjba
399800151008     c                   parm                    parvar
399900151008     c                   parm                    partitas
400000151008     c                   parm                    parcsb
400100120312     c                   endsr
400200121108      ****************************************************
400300121108      ** Memorizzazione tracciato della bolla di sede prima
400400121108      ** della variazione
400500121108      ****************************************************
400600121205     c     memo_dstitas  Begsr
400700121116     c                   eval      dstitas=titasds
400800121108     c                   endsr
400900121109      ****************************************************
401000121109      ** Memorizzazione dati del contrassegno prima
401100121109      ** della variazione
401200121109      ****************************************************
401300121109     c     memo_csb      Begsr
401400121109     c                   clear                   dscsb
401500121109     C     KANN          CHAIN(N)  TNCSB000
401600121109     c                   if        %found(tncsb03l)
401700121115     c                   eval      dscsb=csbds
401800121109     c                   endif
401900121109     c                   endsr
402000981001     C*
402100970814**         CMD1
402200020827CHKOBJ OBJ(FIARBR3C) OBJTYPE(*FILE) MBR(M234567890)
402300920207**         CM2
402400020827ALCOBJ OBJ((FLXXXX0R *FILE *EXCL       M234567890))
402500920212**         CM3
402600020827OVRDBF FILE(FLXXXXXX)               MBR(M234567890)
402700920113**         CM5
402800020827ADDLFM FILE(FIARBR3C) MBR(M234567890)   DTAMBRS((*CURRENT/FNARBK0R
402900020827 (M234567890)) (*CURRENT/FNARBD0R (M234567890)) (*CURRENT/FIARBT0R
403000020827 (M234567890)) (*CURRENT/FNARBG0R (M234567890)) (*CURRENT/FNARBM0R
403100020827 (M234567890)))
403200920113**
4033009201301 BOLLA  INESISTENTE                                                1
403400920130TIPO BOLLA INESISTENTE                                              2
403500920130CAUSALE VARIAZIONE INESISTENTE                                      3
403600960514NON ESISTE RECORD BOLLA IN TNAC200F                                 4
403700920130NON ESISTE ANAGRAFICA CONTRASSEGNI                                  5
403800920130NUOVO CODICE BOLLA INESISTENTE                                      6
4039009201302 BOLLA  INESISTENTE                                                7
404000920220SPEDIZIONE GIA' TASSATA                                             8
404100920212SPEDIZIONE GIA' CONTABILIZZATA                                      9
404200920210C/ASSEGNO GIA' INCASSATO - PAGATO                                   10
404300950201MANCA RECORD FILE FNARBM0R                                          11
404400950201MANCA X CREAZ. ASSEGNATO FNARBD1R                                   12
404500930513X CREAR 2BOLLA MA NO VARIAZ CD.BOLL                                 15
404600920601GIA' CONTABIL.:NO TOLTA VARIA G                                     14
404700930513
404800920601GIA' CONTABIL.:NO CAMBIO TIPO BOLLA                                 16
404900930513
405000920601GIA' CONTABIL.:NO AGGIUNTA VARIA G                                  18
405100930513
405200920601GIA' CONTABIL.:NO VARIATO L'A3                                      20
405300930513
405400920601GIA' CONTABIL.:NO MODIFIC. VARIA G                                  22
405500990414GIA' TASSATA  :NO TOLTA    VARIA G                                  23
405600920228GIA' TASSATA  :NO AGGIUNTA VARIA G                                  24
405700920228GIA' TASSATA  :NO MODIFIC. VARIA G                                  25
405800950203CAP E NAZIONE MITTENTE INESISTENTE                                  26
405900950203COD.MITTENTE INESISTENTE IN ANAGRAF                                 27
406000920302SPEDIZIONE ANNULLATA                                                28
406100960919COD.MITTENTE BLOCCATO                                               29
406200970814ERRORE IN DETERMINAZIONE COD.TASSAZ                                 30
406300990414AGGIUNTO   IMPORTO DA ASSICURARE                                    31
406400990414TOLTO      IMPORTO DA ASSICURARE                                    32
406500990414MODIFICATO IMPORTO DA ASSICURARE                                    33
406600990414GIA' CONTABIL.:NO TOLTA    VARIA R                                  34
406700990414GIA' CONTABIL.:NO AGGIUNTA VARIA R                                  35
406800990414GIA' CONTABIL.:NO MODIFIC. VARIA R                                  36
406900991022
407000991022
407100991022
407200991022AGGIUNTO IM.ASS. TP.BOLLA NON ASSIC                                 40
407300991022TOLTO    IM.ASS. TP.BOLLA NON ASSIC                                 41
407400991022MODIFIC. IM.ASS. TP.BOLLA NON ASSIC                                 42
407500070119** cmdt
407600070119OVRDBF FILE(FNBLP01L) TOFILE(
